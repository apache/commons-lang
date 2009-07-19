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

// CsvUnescaper
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.CharUtils;

/**
 * Helper class defining various standard language unescape functions. 
 * @since 3.0
 */
public class UnescapeUtils {

    // throw "illegal character: \92" as an Exception if a \ on the end of the Java (as per the compiler)?
    public static final CharSequenceTranslator UNESCAPE_JAVA = 
        new AggregateTranslator(
            new UnicodeUnescaper(),
            new LookupTranslator(EntityArrays.JAVA_CTRL_CHARS_UNESCAPE()),
            new LookupTranslator(
                      new String[][] { 
                            {"\\\\", "\\"},
                            {"\\\"", "\""},
                            {"\\'", "'"},
                            {"\\", ""}
                      })
        );

    public static final String unescapeJava(String input) {
        return UNESCAPE_JAVA.translate(input);
    }
                
    public static final CharSequenceTranslator UNESCAPE_ECMASCRIPT = UNESCAPE_JAVA;

    public static final String unescapeEcmaScript(String input) {
        return UNESCAPE_ECMASCRIPT.translate(input);
    }
                
    public static final CharSequenceTranslator UNESCAPE_HTML3 = 
        new AggregateTranslator(
            new LookupTranslator(EntityArrays.BASIC_UNESCAPE()),
            new LookupTranslator(EntityArrays.ISO8859_1_UNESCAPE()),
            new NumericEntityUnescaper()
        );

    public static final String unescapeHtml3(String input) {
        return UNESCAPE_HTML3.translate(input);
    }
                
    public static final CharSequenceTranslator UNESCAPE_HTML4 = 
        new AggregateTranslator(
            new LookupTranslator(EntityArrays.BASIC_UNESCAPE()),
            new LookupTranslator(EntityArrays.ISO8859_1_UNESCAPE()),
            new LookupTranslator(EntityArrays.HTML40_EXTENDED_UNESCAPE()),
            new NumericEntityUnescaper()
        );

    public static final String unescapeHtml4(String input) {
        return UNESCAPE_HTML4.translate(input);
    }
                
    public static final CharSequenceTranslator UNESCAPE_XML = 
        new AggregateTranslator(
            new LookupTranslator(EntityArrays.BASIC_UNESCAPE()),
            new LookupTranslator(EntityArrays.APOS_UNESCAPE()),
            new NumericEntityUnescaper()
        );

    public static final String unescapeXml(String input) {
        return UNESCAPE_XML.translate(input);
    }
                
    public static final CharSequenceTranslator UNESCAPE_CSV = new CsvUnescaper();

    public static final String unescapeCsv(String input) {
        return UNESCAPE_CSV.translate(input);
    }
                
    static class CsvUnescaper extends CharSequenceTranslator {

        private static final char CSV_DELIMITER = ',';
        private static final char CSV_QUOTE = '"';
        private static final String CSV_QUOTE_STR = String.valueOf(CSV_QUOTE);
        private static final char[] CSV_SEARCH_CHARS = new char[] {CSV_DELIMITER, CSV_QUOTE, CharUtils.CR, CharUtils.LF};

        // TODO: Replace with a RegexTranslator. That should consume the number of characters the regex uses up?
        public int translate(CharSequence input, int index, Writer out) throws IOException {

            if(index != 0) {
                throw new IllegalStateException("CsvUnescaper should never reach the [1] index");
            }

            if ( input.charAt(0) != CSV_QUOTE || input.charAt(input.length() - 1) != CSV_QUOTE ) {
                out.write(input.toString());
                return input.length();
            }

            // strip quotes
            String quoteless = input.subSequence(1, input.length() - 1).toString();

            if ( StringUtils.containsAny(quoteless, CSV_SEARCH_CHARS) ) {
                // deal with escaped quotes; ie) ""
                out.write(StringUtils.replace(quoteless, CSV_QUOTE_STR + CSV_QUOTE_STR, CSV_QUOTE_STR));
            } else {
                out.write(input.toString());
            }
            return input.length();
        }
    }

}
