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
package org.apache.commons.lang3;

import org.junit.Assert;
import org.junit.Test;

/**
 * Tests {@link XMLCharacter}.
 */
public class XMLCharacterTest {

    private static char[] XML_WHITESPACE_CHARS = { ' ', '\t', '\r', '\n' };

    private static char[] JAVA_EXTRA_WHITESPACE_CHARS = { '\u000B', '\u001C', '\u001D', '\u001E', '\u001F' };

    /**
     * @see Character#isWhitespace(char)
     */
    @Test
    public void testIsWhitespace_char() {
        for (final char c : XML_WHITESPACE_CHARS) {
            Assert.assertTrue(XMLCharacter.isWhitespace(c));
            Assert.assertTrue(Character.isWhitespace(c));
        }
        for (final char c : JAVA_EXTRA_WHITESPACE_CHARS) {
            Assert.assertFalse(XMLCharacter.isWhitespace(c));
            Assert.assertTrue(Character.isWhitespace(c));
        }
        //
        Assert.assertFalse(XMLCharacter.isWhitespace('a'));
    }

    @Test
    public void testIsWhitespace_char_arrary() {
        Assert.assertTrue(XMLCharacter.isWhitespace(XML_WHITESPACE_CHARS, 0, XML_WHITESPACE_CHARS.length));
        Assert.assertFalse(
                XMLCharacter.isWhitespace(JAVA_EXTRA_WHITESPACE_CHARS, 0, JAVA_EXTRA_WHITESPACE_CHARS.length));
    }

    @Test
    public void testIsWhitespace_CharSequence() {
        Assert.assertFalse(XMLCharacter.isWhitespace(StringUtils.EMPTY));
    }

    @Test
    public void testIsWhitespace_EmptyArray() {
        Assert.assertFalse(XMLCharacter.isWhitespace(new char[] {}, 0, 0));
    }

    @Test
    public void testIsWhitespace_String_firstChar() {
        for (final char c : XML_WHITESPACE_CHARS) {
            Assert.assertTrue(XMLCharacter.isWhitespace(Character.toString(c) + Character.toString(c)));
            Assert.assertFalse(XMLCharacter.isWhitespace(Character.toString(c) + "X"));
        }
        for (final char c : JAVA_EXTRA_WHITESPACE_CHARS) {
            Assert.assertFalse(XMLCharacter.isWhitespace(Character.toString(c) + "X"));
        }
        //
        Assert.assertFalse(XMLCharacter.isWhitespace('a'));
    }

    @Test
    public void testIsWhitespace_String_lastChar() {
        for (final char c : XML_WHITESPACE_CHARS) {
            Assert.assertTrue(XMLCharacter.isWhitespace(Character.toString(c) + Character.toString(c)));
            Assert.assertFalse(XMLCharacter.isWhitespace("X" + Character.toString(c)));
        }
        for (final char c : JAVA_EXTRA_WHITESPACE_CHARS) {
            Assert.assertFalse(XMLCharacter.isWhitespace("X" + Character.toString(c)));
        }
        //
        Assert.assertFalse(XMLCharacter.isWhitespace('a'));
    }

    @Test
    public void testIsWhitespace_String_singleChar() {
        for (final char c : XML_WHITESPACE_CHARS) {
            Assert.assertTrue(XMLCharacter.isWhitespace(Character.toString(c)));
        }
        for (final char c : JAVA_EXTRA_WHITESPACE_CHARS) {
            Assert.assertFalse(XMLCharacter.isWhitespace(Character.toString(c)));
        }
        //
        Assert.assertFalse(XMLCharacter.isWhitespace('a'));
    }

}
