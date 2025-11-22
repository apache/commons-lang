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

import static org.junit.jupiter.api.Assertions.*;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Tests for {@link CsvToJson}.
 */
class CsvToJsonTest {

    @TempDir
    private File tempDir;

    @Test
    void testBasicCsvToJson() throws IOException {
        String csv = "name,age,city\nJohn,25,New York\nJane,30,Los Angeles";
        String json = CsvToJson.fromString(csv).toJson();
        
        assertEquals("[{\"name\":\"John\",\"age\":\"25\",\"city\":\"New York\"},{\"name\":\"Jane\",\"age\":\"30\",\"city\":\"Los Angeles\"}]", json);
    }

    @Test
    void testFromFile() throws IOException {
        Path csvFile = Paths.get(tempDir.getAbsolutePath(), "test.csv");
        String csvData = "product,price,stock\nLaptop,999.99,25\nMouse,19.99,100";
        Files.write(csvFile, csvData.getBytes(StandardCharsets.UTF_8));

        String json = CsvToJson.fromFile(csvFile.toFile()).toJson();
        assertTrue(json.contains("Laptop"));
        assertTrue(json.contains("999.99"));
        assertTrue(json.contains("19.99"));
    }

    @Test
    void testFromBytes() throws IOException {
        String csv = "title,author\n1984,George Orwell\nThe Great Gatsby,F. Scott Fitzgerald";
        byte[] bytes = csv.getBytes(StandardCharsets.UTF_8);
        
        String json = CsvToJson.fromBytes(bytes).toJson();
        assertTrue(json.contains("1984"));
        assertTrue(json.contains("George Orwell"));
    }

    @Test
    void testCustomDelimiter() throws IOException {
        String csv = "name;age;city\nJohn;25;New York\nJane;30;Los Angeles";
        String json = CsvToJson.builder()
                .withDelimiter(';')
                .fromString(csv)
                .toJson();
        
        assertTrue(json.contains("John"));
        assertTrue(json.contains("New York"));
    }

    @Test
    void testQuotedFields() throws IOException {
        String csv = "name,description\n"John Doe","A person with ""special"" skills"\n"Jane Smith","Another test""";
        String json = CsvToJson.fromString(csv).toJson();
        
        assertTrue(json.contains("John Doe"));
        assertTrue(json.contains("A person with \"special\" skills"));
    }

    @Test
    void testCustomHeaders() throws IOException {
        String csv = "John,25,true\nJane,30,false";
        String json = CsvToJson.builder()
                .withHeaders(false)
                .withCustomHeaders("name", "age", "active")
                .fromString(csv)
                .toJson();
        
        assertEquals("[{\"name\":\"John\",\"age\":\"25\",\"active\":\"true\"},{\"name\":\"Jane\",\"age\":\"30\",\"active\":\"false\"}]", json);
    }

    @Test
    void testTypeConversion() throws IOException {
        String csv = "name,age,salary,active\nJohn,25,75000.50,true\nJane,30,85000,false";
        String json = CsvToJson.fromString(csv)
                .toJson(new CsvToJson.JsonConfig().withTypeConversion(true));
        
        assertTrue(json.contains("25"));
        assertTrue(json.contains("75000.5"));
        assertTrue(json.contains("true"));
    }

    @Test
    void testEmptyCsv() throws IOException {
        String csv = "";
        String json = CsvToJson.fromString(csv).toJson();
        assertEquals("[]", json);
    }

    @Test
    void testOnlyHeaders() throws IOException {
        String csv = "name,age,city";
        String json = CsvToJson.fromString(csv).toJson();
        assertEquals("[]", json);
    }

    @Test
    void testSkipEmptyLines() throws IOException {
        String csv = "name,age\n\nJohn,25\n\nJane,30\n";
        String json = CsvToJson.fromString(csv).toJson();
        
        assertTrue(json.contains("John"));
        assertTrue(json.contains("25"));
        assertTrue(json.contains("Jane"));
        assertTrue(json.contains("30"));
    }

    @Test
    void testNullValues() throws IOException {
        String csv = "name,age,active\nJohn,,true\n,30,false";
        String json = CsvToJson.fromString(csv).toJson();
        
        assertTrue(json.contains("John"));
        assertTrue(json.contains(""));
        assertTrue(json.contains("30"));
    }

    @Test
    void testExtraColumns() throws IOException {
        String csv = "name,age\nJohn,25,Engineer,New York\nJane,30,Manager";
        String json = CsvToJson.fromString(csv).toJson();
        
        assertTrue(json.contains("John"));
        assertTrue(json.contains("25"));
        assertTrue(json.contains("Engineer"));
        assertTrue(json.contains("column_3"));
    }

    @Test
    void testFromFileWithCharset() throws IOException {
        Path csvFile = Paths.get(tempDir.getAbsolutePath(), "test-utf8.csv");
        String csvData = "姓名,年龄\n张三,25\n李四,30";
        Files.write(csvFile, csvData.getBytes(StandardCharsets.UTF_8));

        String json = CsvToJson.fromFile(csvFile.toFile(), StandardCharsets.UTF_8).toJson();
        assertTrue(json.contains("张三"));
        assertTrue(json.contains("25"));
    }

    @Test
    void testFromBytesWithCharset() throws IOException {
        String csv = "姓名,年龄\n王五,35\n赵六,40";
        byte[] bytes = csv.getBytes(StandardCharsets.UTF_8);
        
        String json = CsvToJson.fromBytes(bytes, StandardCharsets.UTF_8).toJson();
        assertTrue(json.contains("王五"));
        assertTrue(json.contains("35"));
    }

    @Test
    void testBuilderPattern() throws IOException {
        String csv = "name|age|city\nJohn|25|New York";
        String json = CsvToJson.builder()
                .withDelimiter('|')
                .withQuoteChar('"')
                .withHeaders(true)
                .fromString(csv)
                .toJson();
        
        assertTrue(json.contains("John"));
        assertTrue(json.contains("New York"));
    }

    @Test
    void testNullFile() {
        assertThrows(NullPointerException.class, () -> CsvToJson.fromFile(null));
    }

    @Test
    void testNonExistentFile() {
        File file = new File("nonexistent.csv");
        assertThrows(IllegalArgumentException.class, () -> CsvToJson.fromFile(file));
    }

    @Test
    void testNullString() {
        assertThrows(NullPointerException.class, () -> CsvToJson.fromString(null));
    }

    @Test
    void testNullBytes() {
        assertThrows(NullPointerException.class, () -> CsvToJson.fromBytes(null));
    }

    @Test
    void testNullReader() {
        assertThrows(NullPointerException.class, () -> CsvToJson.fromReader(null));
    }

    @Test
    void testCustomHeadersRequired() throws IOException {
        String csv = "John,25\nJane,30";
        assertThrows(IllegalArgumentException.class, () -> {
            CsvToJson.builder()
                    .withHeaders(false)
                    .fromString(csv)
                    .toJson();
        });
    }
}