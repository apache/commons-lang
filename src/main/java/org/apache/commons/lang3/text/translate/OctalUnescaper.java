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
 * Translate escaped octal Strings back to their octal values.
 *
 * For example, "\45" should go back to being the specific value (a %).
 *
 * Note that this currently only supports the viable range of octal for Java; namely
 * 1 to 377. This is because parsing Java is the main use case.
 *
 * @since 3.0
 * @deprecated As of <a href="https://commons.apache.org/proper/commons-lang/changes-report.html#a3.6">3.6</a>, use Apache Commons Text
 * <a href="https://commons.apache.org/proper/commons-text/javadocs/api-release/org/apache/commons/text/translate/OctalUnescaper.html">
 * OctalUnescaper</a>.
 */
@Deprecated
public class OctalUnescaper extends CharSequenceTranslator {

    /**
     * Constructs a new instance.
     */
    public OctalUnescaper() {
        // empty
    }

    /**
     * Checks if the given char is the character representation of one of the digit from 0 to 3.
     * @param ch the char to check
     * @return true if the given char is the character representation of one of the digits from 0 to 3
     */
    private boolean isZeroToThree(final char ch) {
        return ch >= '0' && ch <= '3';
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int translate(final CharSequence input, final int index, final Writer out) throws IOException {
        final int remaining = input.length() - index - 1; // how many characters left, ignoring the first \
        final StringBuilder builder = new StringBuilder();
        if (input.charAt(index) == '\\' && remaining > 0 && CharUtils.isOctal(input.charAt(index + 1))) {
            final int next = index + 1;
            final int next2 = index + 2;
            final int next3 = index + 3;
            // we know this is good as we checked it in the if block above
            builder.append(input.charAt(next));
            if (remaining > 1 && CharUtils.isOctal(input.charAt(next2))) {
                builder.append(input.charAt(next2));
                if (remaining > 2 && isZeroToThree(input.charAt(next)) && CharUtils.isOctal(input.charAt(next3))) {
                    builder.append(input.charAt(next3));
                }
            }
            out.write(Integer.parseInt(builder.toString(), 8));
            return 1 + builder.length();
        }
        return 0;
    }
}
