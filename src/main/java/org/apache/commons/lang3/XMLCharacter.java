/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the  "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
/*
 * From Apache Xalan XMLCharacterRecognizer.
 */
package org.apache.commons.lang3;

/**
 * Verifies whether specified primitives and objects conforms to the XML 1.0 definition of whitespace.
 * 
 * <p>
 * Copied and tweaked from Apache Xalan {@code XMLCharacterRecognizer}
 * </p>
 *
 * @since 3.5
 */
public class XMLCharacter {

    /**
     * Returns whether the specified {@code ch} conforms to the XML 1.0 definition of whitespace. Refer to
     * <a href="http://www.w3.org/TR/1998/REC-xml-19980210#NT-S"> the definition of <CODE>S</CODE></a> for details.
     *
     * @param ch
     *            Character to check as XML whitespace.
     * @return true if {@code ch} is XML whitespace; otherwise false.
     */
    public static boolean isWhitespace(final char ch) {
        return ch == 0x20 || ch == 0x09 || ch == 0xD || ch == 0xA;
    }

    /**
     * Detects if the string is whitespace.
     *
     * @param ch
     *            Character array to check as XML whitespace.
     * @param start
     *            Start index of characters in the array
     * @param length
     *            Number of characters in the array
     * @return true if the characters in the array are XML whitespace; otherwise, false.
     */
    public static boolean isWhitespace(final char ch[], final int start, final int length) {
        final int end = start + length;
        for (int s = start; s < end; s++) {
            if (!isWhitespace(ch[s])) {
                return false;
            }
        }
        return length > 0;
    }

    /**
     * Detects if the string is whitespace.
     *
     * @param charSequence
     *            StringBuffer to check as XML whitespace.
     * @return True if characters in buffer are XML whitespace, false otherwise
     */
    public static boolean isWhitespace(final CharSequence charSequence) {
        final int length = charSequence.length();
        for (int i = 0; i < length; i++) {
            if (!isWhitespace(charSequence.charAt(i))) {
                return false;
            }
        }
        return length > 0;
    }

}
