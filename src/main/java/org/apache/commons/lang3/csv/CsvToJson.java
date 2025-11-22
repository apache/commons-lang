/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang3.csv;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Utility class for converting CSV data to JSON format.
 * 
 * <p>This class provides a simple and efficient way to convert CSV data from various sources
 * to JSON format while maintaining the original order of columns and providing flexible
 * configuration options.</p>
 * 
 * <p>Features:</p>
 * <ul>
 *   <li>Support for multiple input sources (File, String, byte[])</li>
 *   <li>Configurable delimiter (default: comma)</li>
 *   <li>Configurable quote character (default: double quote)</li>
 *   <li>Support for custom headers</li>
 *   <li>Configurable type conversion</li>
 *   <li>Support for different character encodings</li>
 *   <li>Preserves column order</li>
 * </ul>
 * 
 * <p>Usage example:</p>
 * <pre>{@code
 * String csv = "name,age,city\nJohn,25,New York\nJane,30,Los Angeles";
 * String json = CsvToJson.fromString(csv).toJson();
 * }</pre>
 * 
 * @since 3.20.0
 */
public final class CsvToJson {
    
    private final Reader reader;
    private final CsvConfig config;
    
    private CsvToJson(Reader reader, CsvConfig config) {
        this.reader = Objects.requireNonNull(reader, "reader");
        this.config = Objects.requireNonNull(config, "config");
    }
    
    /**
     * Creates a new CsvToJson instance from a file.
     * 
     * @param file the CSV file to read from
     * @return a new CsvToJson instance
     * @throws NullPointerException if file is null
     * @throws IllegalArgumentException if file does not exist
     */
    public static CsvToJson fromFile(File file) {
        return fromFile(file, StandardCharsets.UTF_8);
    }
    
    /**
     * Creates a new CsvToJson instance from a file with specified charset.
     * 
     * @param file the CSV file to read from
     * @param charset the charset to use
     * @return a new CsvToJson instance
     * @throws NullPointerException if file or charset is null
     * @throws IllegalArgumentException if file does not exist
     */
    public static CsvToJson fromFile(File file, Charset charset) {
        Objects.requireNonNull(file, "file");
        Objects.requireNonNull(charset, "charset");
        
        if (!file.exists()) {
            throw new IllegalArgumentException("File does not exist: " + file.getAbsolutePath());
        }
        
        try {
            return new CsvToJson(new InputStreamReader(new FileInputStream(file), charset), new CsvConfig());
        } catch (IOException e) {
            throw new IllegalArgumentException("Failed to read file: " + file.getAbsolutePath(), e);
        }
    }
    
    /**
     * Creates a new CsvToJson instance from a string.
     * 
     * @param csv the CSV data as a string
     * @return a new CsvToJson instance
     * @throws NullPointerException if csv is null
     */
    public static CsvToJson fromString(String csv) {
        Objects.requireNonNull(csv, "csv");
        return new CsvToJson(new StringReader(csv), new CsvConfig());
    }
    
    /**
     * Creates a new CsvToJson instance from a byte array.
     * 
     * @param bytes the CSV data as a byte array
     * @return a new CsvToJson instance
     * @throws NullPointerException if bytes is null
     */
    public static CsvToJson fromBytes(byte[] bytes) {
        return fromBytes(bytes, StandardCharsets.UTF_8);
    }
    
    /**
     * Creates a new CsvToJson instance from a byte array with specified charset.
     * 
     * @param bytes the CSV data as a byte array
     * @param charset the charset to use
     * @return a new CsvToJson instance
     * @throws NullPointerException if bytes or charset is null
     */
    public static CsvToJson fromBytes(byte[] bytes, Charset charset) {
        Objects.requireNonNull(bytes, "bytes");
        Objects.requireNonNull(charset, "charset");
        return new CsvToJson(new InputStreamReader(new java.io.ByteArrayInputStream(bytes), charset), new CsvConfig());
    }
    
    /**
     * Creates a new CsvToJson instance from a reader.
     * 
     * @param reader the reader to read CSV data from
     * @return a new CsvToJson instance
     * @throws NullPointerException if reader is null
     */
    public static CsvToJson fromReader(Reader reader) {
        Objects.requireNonNull(reader, "reader");
        return new CsvToJson(reader, new CsvConfig());
    }
    
    /**
     * Returns a builder to configure the CSV to JSON conversion.
     * 
     * @return a builder instance
     */
    public static Builder builder() {
        return new Builder();
    }
    
    /**
     * Converts the CSV data to JSON format.
     * 
     * @return the JSON representation of the CSV data
     * @throws IOException if an I/O error occurs
     */
    public String toJson() throws IOException {
        return toJson(new JsonConfig());
    }
    
    /**
     * Converts the CSV data to JSON format with specified configuration.
     * 
     * @param jsonConfig the JSON configuration
     * @return the JSON representation of the CSV data
     * @throws IOException if an I/O error occurs
     */
    public String toJson(JsonConfig jsonConfig) throws IOException {
        Objects.requireNonNull(jsonConfig, "jsonConfig");
        
        List<String[]> rows = parseCsv();
        if (rows.isEmpty()) {
            return "[]";
        }
        
        String[] headers = config.hasHeaders ? rows.get(0) : config.customHeaders;
        int startIndex = config.hasHeaders ? 1 : 0;
        
        if (headers == null) {
            throw new IllegalArgumentException("Headers must be provided when hasHeaders is false");
        }
        
        StringBuilder json = new StringBuilder();
        json.append("[");
        
        for (int i = startIndex; i < rows.size(); i++) {
            if (i > startIndex) {
                json.append(",");
            }
            
            String[] values = rows.get(i);
            json.append("{");
            
            for (int j = 0; j < Math.min(headers.length, values.length); j++) {
                if (j > 0) {
                    json.append(",");
                }
                
                json.append(quoteString(headers[j])).append(":");
                
                String value = values[j];
                if (jsonConfig.shouldConvertTypes) {
                    Object converted = convertValue(value);
                    json.append(formatValue(converted));
                } else {
                    json.append(quoteString(value));
                }
            }
            
            // Handle extra columns
            for (int j = headers.length; j < values.length; j++) {
                if (j > 0) {
                    json.append(",");
                }
                json.append(quoteString("column_" + (j + 1))).append(":");
                json.append(quoteString(values[j]));
            }
            
            json.append("}");
        }
        
        json.append("]");
        return json.toString();
    }
    
    private List<String[]> parseCsv() throws IOException {
        List<String[]> rows = new ArrayList<>();
        BufferedReader br = new BufferedReader(reader);
        String line;
        
        while ((line = br.readLine()) != null) {
            if (line.isEmpty() && config.skipEmptyLines) {
                continue;
            }
            
            String[] values = parseLine(line);
            if (values.length > 0) {
                rows.add(values);
            }
        }
        
        return rows;
    }
    
    private String[] parseLine(String line) {
        if (line == null || line.isEmpty()) {
            return new String[0];
        }
        
        List<String> values = new ArrayList<>();
        StringBuilder current = new StringBuilder();
        boolean inQuotes = false;
        int i = 0;
        int len = line.length();
        
        while (i < len) {
            char c = line.charAt(i);
            
            if (c == config.quoteChar && inQuotes) {
                // Check for escaped quote
                if (i + 1 < len && line.charAt(i + 1) == config.quoteChar) {
                    current.append(config.quoteChar);
                    i += 2;
                    continue;
                } else {
                    inQuotes = false;
                    i++;
                    continue;
                }
            } else if (c == config.quoteChar && !inQuotes) {
                inQuotes = true;
                i++;
                continue;
            } else if (c == config.delimiter && !inQuotes) {
                values.add(current.toString());
                current.setLength(0);
                i++;
                continue;
            }
            
            current.append(c);
            i++;
        }
        
        values.add(current.toString());
        return values.toArray(new String[0]);
    }
    
    private String quoteString(String value) {
        if (value == null) {
            return "null";
        }
        
        StringBuilder sb = new StringBuilder();
        sb.append('"');
        for (char c : value.toCharArray()) {
            switch (c) {
                case '"':
                    sb.append("\\\"");
                    break;
                case '\\':
                    sb.append("\\\\");
                    break;
                case '\b':
                    sb.append("\\b");
                    break;
                case '\f':
                    sb.append("\\f");
                    break;
                case '\n':
                    sb.append("\\n");
                    break;
                case '\r':
                    sb.append("\\r");
                    break;
                case '\t':
                    sb.append("\\t");
                    break;
                default:
                    if (c < ' ' || c > '~') {
                        sb.append(String.format("\\u%04x", (int) c));
                    } else {
                        sb.append(c);
                    }
                    break;
            }
        }
        sb.append('"');
        return sb.toString();
    }
    
    private Object convertValue(String value) {
        if (value == null || value.isEmpty()) {
            return value;
        }
        
        String str = value;
        
        // Check for boolean
        if ("true".equalsIgnoreCase(str)) {
            return Boolean.TRUE;
        }
        if ("false".equalsIgnoreCase(str)) {
            return Boolean.FALSE;
        }
        
        // Check for null
        if ("null".equalsIgnoreCase(str)) {
            return null;
        }
        
        // Check for number
        try {
            if (str.contains(".")) {
                return Double.parseDouble(str);
            } else {
                return Long.parseLong(str);
            }
        } catch (NumberFormatException e) {
            return value;
        }
    }
    
    private String formatValue(Object value) {
        if (value == null) {
            return "null";
        } else if (value instanceof String) {
            return quoteString((String) value);
        } else {
            return value.toString();
        }
    }
    
    /**
     * Builder for configuring CSV to JSON conversion.
     */
    public static class Builder {
        private final CsvConfig config = new CsvConfig();
        
        /**
         * Sets the delimiter character.
         * 
         * @param delimiter the delimiter character
         * @return this builder
         */
        public Builder withDelimiter(char delimiter) {
            config.delimiter = delimiter;
            return this;
        }
        
        /**
         * Sets the quote character.
         * 
         * @param quoteChar the quote character
         * @return this builder
         */
        public Builder withQuoteChar(char quoteChar) {
            config.quoteChar = quoteChar;
            return this;
        }
        
        /**
         * Sets whether the CSV has headers.
         * 
         * @param hasHeaders true if the CSV has headers
         * @return this builder
         */
        public Builder withHeaders(boolean hasHeaders) {
            config.hasHeaders = hasHeaders;
            return this;
        }
        
        /**
         * Sets custom headers.
         * 
         * @param headers the custom headers
         * @return this builder
         */
        public Builder withCustomHeaders(String... headers) {
            config.customHeaders = headers.clone();
            config.hasHeaders = false;
            return this;
        }
        
        /**
         * Sets whether to skip empty lines.
         * 
         * @param skipEmptyLines true to skip empty lines
         * @return this builder
         */
        public Builder skipEmptyLines(boolean skipEmptyLines) {
            config.skipEmptyLines = skipEmptyLines;
            return this;
        }
        
        /**
         * Builds a new CsvToJson instance from a file.
         * 
         * @param file the file to read from
         * @return a new CsvToJson instance
         */
        public CsvToJson fromFile(File file) {
            return CsvToJson.fromFile(file).withConfig(config);
        }
        
        /**
         * Builds a new CsvToJson instance from a file with specified charset.
         * 
         * @param file the file to read from
         * @param charset the charset to use
         * @return a new CsvToJson instance
         */
        public CsvToJson fromFile(File file, Charset charset) {
            return CsvToJson.fromFile(file, charset).withConfig(config);
        }
        
        /**
         * Builds a new CsvToJson instance from a string.
         * 
         * @param csv the CSV data
         * @return a new CsvToJson instance
         */
        public CsvToJson fromString(String csv) {
            return CsvToJson.fromString(csv).withConfig(config);
        }
        
        /**
         * Builds a new CsvToJson instance from a byte array.
         * 
         * @param bytes the CSV data
         * @return a new CsvToJson instance
         */
        public CsvToJson fromBytes(byte[] bytes) {
            return CsvToJson.fromBytes(bytes).withConfig(config);
        }
        
        /**
         * Builds a new CsvToJson instance from a byte array with specified charset.
         * 
         * @param bytes the CSV data
         * @param charset the charset to use
         * @return a new CsvToJson instance
         */
        public CsvToJson fromBytes(byte[] bytes, Charset charset) {
            return CsvToJson.fromBytes(bytes, charset).withConfig(config);
        }
        
        /**
         * Builds a new CsvToJson instance from a reader.
         * 
         * @param reader the reader to read from
         * @return a new CsvToJson instance
         */
        public CsvToJson fromReader(Reader reader) {
            return CsvToJson.fromReader(reader).withConfig(config);
        }
    }
    
    private CsvToJson withConfig(CsvConfig config) {
        return new CsvToJson(this.reader, config);
    }
    
    /**
     * Configuration for CSV parsing.
     */
    private static class CsvConfig {
        char delimiter = ',';
        char quoteChar = '"';
        boolean hasHeaders = true;
        String[] customHeaders = null;
        boolean skipEmptyLines = true;
    }
    
    /**
     * Configuration for JSON output.
     */
    public static class JsonConfig {
        boolean shouldConvertTypes = false;
        
        /**
         * Sets whether to perform automatic type conversion.
         * 
         * @param shouldConvertTypes true to enable type conversion
         * @return this config
         */
        public JsonConfig withTypeConversion(boolean shouldConvertTypes) {
            this.shouldConvertTypes = shouldConvertTypes;
            return this;
        }
    }
}