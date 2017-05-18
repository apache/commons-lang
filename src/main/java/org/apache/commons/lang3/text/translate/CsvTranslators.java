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
package org.apache.commons.lang3.text.translate;

import org.apache.commons.lang3.CharUtils;
import org.apache.commons.lang3.StringUtils;

import java.io.IOException;
import java.io.Writer;

/**
 * This class holds inner classes for escaping/unescaping Comma Separated Values.
 */
public class CsvTranslators {

    private static final char CSV_DELIMITER = ',';
    private static final char CSV_QUOTE = '"';
    private static final String CSV_QUOTE_STR = String.valueOf(CSV_QUOTE);
    private static final String CSV_ESCAPED_QUOTE_STR = CSV_QUOTE_STR + CSV_QUOTE_STR;
    private static final char[] CSV_SEARCH_CHARS =
            new char[] {CSV_DELIMITER, CSV_QUOTE, CharUtils.CR, CharUtils.LF};

    private CsvTranslators() { }

    /**
     * Translator for escaping Comma Separated Values.
     */
    public static class CsvEscaper extends SinglePassTranslator {

        @Override
        void translateWhole(final CharSequence input, final Writer out) throws IOException {
            final String inputSting = input.toString();
            if (StringUtils.containsNone(inputSting, CSV_SEARCH_CHARS)) {
                out.write(inputSting);
            } else {
                // input needs quoting
                out.write(CSV_QUOTE);
                out.write(StringUtils.replace(inputSting, CSV_QUOTE_STR, CSV_ESCAPED_QUOTE_STR));
                out.write(CSV_QUOTE);
            }
        }
    }

    /**
     * Translator for unescaping escaped Comma Separated Value entries.
     */
    public static class CsvUnescaper extends SinglePassTranslator {

        @Override
        void translateWhole(final CharSequence input, final Writer out) throws IOException {
            // is input not quoted?
            if (input.charAt(0) != CSV_QUOTE || input.charAt(input.length() - 1) != CSV_QUOTE) {
                out.write(input.toString());
                return;
            }

            // strip quotes
            final String quoteless = input.subSequence(1, input.length() - 1).toString();

            if (StringUtils.containsAny(quoteless, CSV_SEARCH_CHARS)) {
                // deal with escaped quotes; ie) ""
                out.write(StringUtils.replace(quoteless, CSV_ESCAPED_QUOTE_STR, CSV_QUOTE_STR));
            } else {
                out.write(input.toString());
            }
        }
    }
}