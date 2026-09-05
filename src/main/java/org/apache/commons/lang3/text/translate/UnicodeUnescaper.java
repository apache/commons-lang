/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.commons.lang3.text.translate;

import java.io.IOException;
import java.io.Writer;

import org.apache.commons.lang3.CharUtils;

/**
 * Translates escaped Unicode values of the form \\u+\d\d\d\d back to Unicode. It supports multiple 'u' characters and will work with or without the +.
 * <p>
 * Only ASCII hexadecimal digits ({@code [0-9a-fA-F]}) are accepted in the four-digit value. Malformed sequences - non-ASCII-hex digits, sign characters,
 * or an escape truncated by the end of the input - are not translated and pass through unchanged.
 * </p>
 *
 * @since 3.0
 * @deprecated As of <a href="https://commons.apache.org/proper/commons-lang/changes-report.html#a3.6">3.6</a>, use Apache Commons Text
 *             <a href="https://commons.apache.org/proper/commons-text/javadocs/api-release/org/apache/commons/text/translate/UnicodeUnescaper.html">
 *             UnicodeUnescaper</a>.
 */
@Deprecated
public class UnicodeUnescaper extends CharSequenceTranslator {

    /**
     * Constructs a new instance.
     */
    public UnicodeUnescaper() {
        // empty
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int translate(final CharSequence input, final int index, final Writer out) throws IOException {
        if (input.charAt(index) == '\\' && index + 1 < input.length() && input.charAt(index + 1) == 'u') {
            // consume optional additional 'u' chars
            int i = 2;
            while (index + i < input.length() && input.charAt(index + i) == 'u') {
                i++;
            }
            if (index + i < input.length() && input.charAt(index + i) == '+') {
                i++;
            }
            if (index + i + 4 <= input.length()) {
                // Get 4 hex digits
                final CharSequence unicode = input.subSequence(index + i, index + i + 4);
                // Pre-validate that all four characters are ASCII hexadecimal digits, mirroring
                // NumericEntityUnescaper's CharUtils.isHex discipline. Integer.parseInt is looser than
                // the \\uXXXX format: it accepts a leading sign and, via Character.digit, decimal digits
                // from any Unicode script and fullwidth Latin hex letters. Anything that is not a
                // well-formed escape is not translated and passes through verbatim.
                for (int j = 0; j < 4; j++) {
                    if (!CharUtils.isHex(unicode.charAt(j))) {
                        return 0;
                    }
                }
                out.write((char) Integer.parseInt(unicode.toString(), 16));
                return i + 4;
            }
            // Truncated escape at the end of the input: not a well-formed escape, pass through verbatim.
            return 0;
        }
        return 0;
    }
}
