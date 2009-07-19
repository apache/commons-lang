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
package org.apache.commons.lang.text.translate;

import java.io.IOException;
import java.io.Writer;

// CsvEscaper
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.CharUtils;

/**
 * Helper class defining various standard language escape functions. 
 * @since 3.0
 */
public class EscapeUtils {

    public static final CharSequenceTranslator ESCAPE_JAVA = 
          new LookupTranslator(
            new String[][] { 
              {"\"", "\\\""},
              {"\\", "\\\\"},
          }).with(
            new LookupTranslator(EntityArrays.JAVA_CTRL_CHARS_ESCAPE())
          ).with(
            UnicodeEscaper.outsideOf(32, 0x7f) 
        );

    public static final String escapeJava(String input) {
        return ESCAPE_JAVA.translate(input);
    }
                
    public static final CharSequenceTranslator ESCAPE_ECMASCRIPT = 
        new AggregateTranslator(
            new LookupTranslator(
                      new String[][] { 
                            {"'", "\\'"},
                            {"\"", "\\\""},
                            {"\\", "\\\\"},
                            {"/", "\\/"}
                      }),
            new LookupTranslator(EntityArrays.JAVA_CTRL_CHARS_ESCAPE()),
            UnicodeEscaper.outsideOf(32, 0x7f) 
        );
            
    public static final String escapeEcmaScript(String input) {
        return ESCAPE_ECMASCRIPT.translate(input);
    }
                
    public static final CharSequenceTranslator ESCAPE_XML = 
        new AggregateTranslator(
            new LookupTranslator(EntityArrays.BASIC_ESCAPE()),
            new LookupTranslator(EntityArrays.APOS_ESCAPE()),
            NumericEntityEscaper.above(0x7f)
        );

    public static final String escapeXml(String input) {
        return ESCAPE_XML.translate(input);
    }
                
    public static final CharSequenceTranslator ESCAPE_HTML3 = 
        new AggregateTranslator(
            new LookupTranslator(EntityArrays.BASIC_ESCAPE()),
            new LookupTranslator(EntityArrays.ISO8859_1_ESCAPE()),
            NumericEntityEscaper.above(0x7f)
        );

    public static final String escapeHtml3(String input) {
        return ESCAPE_HTML3.translate(input);
    }
                
    public static final CharSequenceTranslator ESCAPE_HTML4 = 
        new AggregateTranslator(
            new LookupTranslator(EntityArrays.BASIC_ESCAPE()),
            new LookupTranslator(EntityArrays.ISO8859_1_ESCAPE()),
            new LookupTranslator(EntityArrays.HTML40_EXTENDED_ESCAPE()),
            NumericEntityEscaper.above(0x7f)
        );

    public static final String escapeHtml4(String input) {
        return ESCAPE_HTML4.translate(input);
    }
                
    public static final CharSequenceTranslator ESCAPE_CSV = new CsvEscaper();

    public static final String escapeCsv(String input) {
        return ESCAPE_CSV.translate(input);
    }
                
    // TODO: Create a parent class - 'SinglePassTranslator' ?
    // TODO: It would handle the index checking, and length returning, and 
    // TODO: could also have an optimization check method.
    static class CsvEscaper extends CharSequenceTranslator {

        private static final char CSV_DELIMITER = ',';
        private static final char CSV_QUOTE = '"';
        private static final String CSV_QUOTE_STR = String.valueOf(CSV_QUOTE);
        private static final char[] CSV_SEARCH_CHARS = new char[] {CSV_DELIMITER, CSV_QUOTE, CharUtils.CR, CharUtils.LF};

        // TODO: Replace with a RegexTranslator. That should consume the number of characters the regex uses up?
        public int translate(CharSequence input, int index, Writer out) throws IOException {

            if(index != 0) {
                throw new IllegalStateException("CsvEscaper should never reach the [1] index");
            }

            if (StringUtils.containsNone(input.toString(), CSV_SEARCH_CHARS)) {
                out.write(input.toString());
            } else {
                out.write(CSV_QUOTE);
                out.write(StringUtils.replace(input.toString(), CSV_QUOTE_STR, CSV_QUOTE_STR + CSV_QUOTE_STR));
                out.write(CSV_QUOTE);
            }
            return input.length();
        }
    }

}
