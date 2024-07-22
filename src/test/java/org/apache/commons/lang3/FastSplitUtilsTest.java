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
package org.apache.commons.lang3;

import org.apache.commons.lang3.FastSplitUtils.SplitBuffer;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;


/**
 * Unit tests {@link org.apache.commons.lang3.FastSplitUtils}.
 */
public class FastSplitUtilsTest {

    private static final String WHITESPACE;
    private static final String NON_WHITESPACE;

    static {
        StringBuilder ws = new StringBuilder();
        StringBuilder nws = new StringBuilder();

        for (int i = 0; i < Character.MAX_VALUE; i++) {
            if (Character.isWhitespace((char) i)) {
                ws.append((char) i);
            } else if (i < 40) {
                nws.append((char) i);
            }
        }
        WHITESPACE = ws.toString();
        NON_WHITESPACE = nws.toString();
    }

    private void innerTestSplit(final char separator, final String sepStr, final char noMatch) {
        final String msg = "Failed on separator hex(" + Integer.toHexString(separator) +
                "), noMatch hex(" + Integer.toHexString(noMatch) + "), sepStr(" + sepStr + ")";

        final String str = "a" + separator + "b" + separator + separator + noMatch + "c";
        String[] res;
        // (str, sepStr)
        res = FastSplitUtils.split(str, sepStr);
        assertEquals(3, res.length, msg);
        assertEquals("a", res[0]);
        assertEquals("b", res[1]);
        assertEquals(noMatch + "c", res[2]);

        final String str2 = separator + "a" + separator;
        res = FastSplitUtils.split(str2, sepStr);
        assertEquals(1, res.length, msg);
        assertEquals("a", res[0], msg);

        res = FastSplitUtils.split(str, sepStr, -1);
        assertEquals(3, res.length, msg);
        assertEquals("a", res[0], msg);
        assertEquals("b", res[1], msg);
        assertEquals(noMatch + "c", res[2], msg);

        res = FastSplitUtils.split(str, sepStr, 0);
        assertEquals(3, res.length, msg);
        assertEquals("a", res[0], msg);
        assertEquals("b", res[1], msg);
        assertEquals(noMatch + "c", res[2], msg);

        res = FastSplitUtils.split(str, sepStr, 1);
        assertEquals(1, res.length, msg);
        assertEquals(str, res[0], msg);

        res = FastSplitUtils.split(str, sepStr, 2);
        assertEquals(2, res.length, msg);
        assertEquals("a", res[0], msg);
        assertEquals(str.substring(2), res[1], msg);
    }

    private void innerTestSplitPreserveAllTokens(final char separator, final String sepStr, final char noMatch) {
        final String msg = "Failed on separator hex(" + Integer.toHexString(separator) +
                "), noMatch hex(" + Integer.toHexString(noMatch) + "), sepStr(" + sepStr + ")";

        final String str = "a" + separator + "b" + separator + separator + noMatch + "c";
        String[] res;
        // (str, sepStr)
        res = FastSplitUtils.splitPreserveAllTokens(str, sepStr);
        assertEquals(4, res.length, msg);
        assertEquals("a", res[0], msg);
        assertEquals("b", res[1], msg);
        assertEquals("", res[2], msg);
        assertEquals(noMatch + "c", res[3], msg);

        final String str2 = separator + "a" + separator;
        res = FastSplitUtils.splitPreserveAllTokens(str2, sepStr);
        assertEquals(3, res.length, msg);
        assertEquals("", res[0], msg);
        assertEquals("a", res[1], msg);
        assertEquals("", res[2], msg);

        res = FastSplitUtils.splitPreserveAllTokens(str, sepStr, -1);
        assertEquals(4, res.length, msg);
        assertEquals("a", res[0], msg);
        assertEquals("b", res[1], msg);
        assertEquals("", res[2], msg);
        assertEquals(noMatch + "c", res[3], msg);

        res = FastSplitUtils.splitPreserveAllTokens(str, sepStr, 0);
        assertEquals(4, res.length, msg);
        assertEquals("a", res[0], msg);
        assertEquals("b", res[1], msg);
        assertEquals("", res[2], msg);
        assertEquals(noMatch + "c", res[3], msg);

        res = FastSplitUtils.splitPreserveAllTokens(str, sepStr, 1);
        assertEquals(1, res.length, msg);
        assertEquals(str, res[0], msg);

        res = FastSplitUtils.splitPreserveAllTokens(str, sepStr, 2);
        assertEquals(2, res.length, msg);
        assertEquals("a", res[0], msg);
        assertEquals(str.substring(2), res[1], msg);
    }


    @Test
    public void testSplit_String() {
        assertNull(FastSplitUtils.split(null));
        assertEquals(0, FastSplitUtils.split("").length);

        String str = "a b  .c";
        String[] res = FastSplitUtils.split(str);
        assertEquals(3, res.length);
        assertEquals("a", res[0]);
        assertEquals("b", res[1]);
        assertEquals(".c", res[2]);

        str = " a ";
        res = FastSplitUtils.split(str);
        assertEquals(1, res.length);
        assertEquals("a", res[0]);

        str = "a" + WHITESPACE + "b" + NON_WHITESPACE + "c";
        res = FastSplitUtils.split(str);
        assertEquals(2, res.length);
        assertEquals("a", res[0]);
        assertEquals("b" + NON_WHITESPACE + "c", res[1]);
    }

    @Test
    public void testSplit_StringChar() {
        assertNull(FastSplitUtils.split(null, '.'));
        assertEquals(0, FastSplitUtils.split("", '.').length);

        String str = "a.b.. c";
        String[] res = FastSplitUtils.split(str, '.');
        assertEquals(3, res.length);
        assertEquals("a", res[0]);
        assertEquals("b", res[1]);
        assertEquals(" c", res[2]);

        str = ".a.";
        res = FastSplitUtils.split(str, '.');
        assertEquals(1, res.length);
        assertEquals("a", res[0]);

        str = "a b c";
        res = FastSplitUtils.split(str, ' ');
        assertEquals(3, res.length);
        assertEquals("a", res[0]);
        assertEquals("b", res[1]);
        assertEquals("c", res[2]);
    }

    @Test
    public void testSplit_StringString_StringStringInt() {
        assertNull(FastSplitUtils.split(null, "."));
        assertNull(FastSplitUtils.split(null, ".", 3));

        assertEquals(0, FastSplitUtils.split("", ".").length);
        assertEquals(0, FastSplitUtils.split("", ".", 3).length);

        innerTestSplit('.', ".", ' ');
        innerTestSplit('.', ".", ',');
        innerTestSplit('.', ".,", 'x');
        for (int i = 0; i < WHITESPACE.length(); i++) {
            for (int j = 0; j < NON_WHITESPACE.length(); j++) {
                innerTestSplit(WHITESPACE.charAt(i), null, NON_WHITESPACE.charAt(j));
                innerTestSplit(WHITESPACE.charAt(i), String.valueOf(WHITESPACE.charAt(i)), NON_WHITESPACE.charAt(j));
            }
        }

        String[] results;
        final String[] expectedResults = {"ab", "de fg"};
        results = FastSplitUtils.split("ab   de fg", null, 2);
        assertEquals(expectedResults.length, results.length);
        for (int i = 0; i < expectedResults.length; i++) {
            assertEquals(expectedResults[i], results[i]);
        }

        final String[] expectedResults2 = {"ab", "cd:ef"};
        results = FastSplitUtils.split("ab:cd:ef", ":", 2);
        assertEquals(expectedResults2.length, results.length);
        for (int i = 0; i < expectedResults2.length; i++) {
            assertEquals(expectedResults2[i], results[i]);
        }
    }

    @Test
    public void testSplitByCharacterType() {
        assertNull(FastSplitUtils.splitByCharacterType(null));
        assertEquals(0, FastSplitUtils.splitByCharacterType("").length);

        assertTrue(Objects.deepEquals(new String[]{"ab", " ", "de", " ",
                "fg"}, FastSplitUtils.splitByCharacterType("ab de fg")));

        assertTrue(Objects.deepEquals(new String[]{"ab", "   ", "de", " ",
                "fg"}, FastSplitUtils.splitByCharacterType("ab   de fg")));

        assertTrue(Objects.deepEquals(new String[]{"ab", ":", "cd", ":",
                "ef"}, FastSplitUtils.splitByCharacterType("ab:cd:ef")));

        assertTrue(Objects.deepEquals(new String[]{"number", "5"},
                FastSplitUtils.splitByCharacterType("number5")));

        assertTrue(Objects.deepEquals(new String[]{"foo", "B", "ar"},
                FastSplitUtils.splitByCharacterType("fooBar")));

        assertTrue(Objects.deepEquals(new String[]{"foo", "200", "B", "ar"},
                FastSplitUtils.splitByCharacterType("foo200Bar")));

        assertTrue(Objects.deepEquals(new String[]{"ASFR", "ules"},
                FastSplitUtils.splitByCharacterType("ASFRules")));
    }

    @Test
    public void testSplitByCharacterTypeCamelCase() {
        assertNull(FastSplitUtils.splitByCharacterTypeCamelCase(null));
        assertEquals(0, FastSplitUtils.splitByCharacterTypeCamelCase("").length);

        assertTrue(Objects.deepEquals(new String[]{"ab", " ", "de", " ",
                "fg"}, FastSplitUtils.splitByCharacterTypeCamelCase("ab de fg")));

        assertTrue(Objects.deepEquals(new String[]{"ab", "   ", "de", " ",
                "fg"}, FastSplitUtils.splitByCharacterTypeCamelCase("ab   de fg")));

        assertTrue(Objects.deepEquals(new String[]{"ab", ":", "cd", ":",
                "ef"}, FastSplitUtils.splitByCharacterTypeCamelCase("ab:cd:ef")));

        assertTrue(Objects.deepEquals(new String[]{"number", "5"},
                FastSplitUtils.splitByCharacterTypeCamelCase("number5")));

        assertTrue(Objects.deepEquals(new String[]{"foo", "Bar"},
                FastSplitUtils.splitByCharacterTypeCamelCase("fooBar")));

        assertTrue(Objects.deepEquals(new String[]{"foo", "200", "Bar"},
                FastSplitUtils.splitByCharacterTypeCamelCase("foo200Bar")));

        assertTrue(Objects.deepEquals(new String[]{"ASF", "Rules"},
                FastSplitUtils.splitByCharacterTypeCamelCase("ASFRules")));
    }

    @Test
    public void testSplitByWholeSeparatorPreserveAllTokens_StringString() {
        assertArrayEquals(null, FastSplitUtils.splitByWholeSeparatorPreserveAllTokens(null, "."));

        assertEquals(0, FastSplitUtils.splitByWholeSeparatorPreserveAllTokens("", ".").length);

        // test whitespace
        String input = "ab   de fg";
        String[] expected = new String[]{"ab", "", "", "de", "fg"};

        String[] actual = FastSplitUtils.splitByWholeSeparatorPreserveAllTokens(input, null);
        assertEquals(expected.length, actual.length);
        for (int i = 0; i < actual.length; i += 1) {
            assertEquals(expected[i], actual[i]);
        }

        // test delimiter singlechar
        input = "1::2:::3::::4";
        expected = new String[]{"1", "", "2", "", "", "3", "", "", "", "4"};

        actual = FastSplitUtils.splitByWholeSeparatorPreserveAllTokens(input, ":");
        assertEquals(expected.length, actual.length);
        for (int i = 0; i < actual.length; i += 1) {
            assertEquals(expected[i], actual[i]);
        }

        // test delimiter multichar
        input = "1::2:::3::::4";
        expected = new String[]{"1", "2", ":3", "", "4"};

        actual = FastSplitUtils.splitByWholeSeparatorPreserveAllTokens(input, "::");
        assertEquals(expected.length, actual.length);
        for (int i = 0; i < actual.length; i += 1) {
            assertEquals(expected[i], actual[i]);
        }
    }

    @Test
    public void testSplitByWholeSeparatorPreserveAllTokens_StringStringInt() {
        assertArrayEquals(null, FastSplitUtils.splitByWholeSeparatorPreserveAllTokens(null, ".", -1));

        assertEquals(0, FastSplitUtils.splitByWholeSeparatorPreserveAllTokens("", ".", -1).length);

        // test whitespace
        String input = "ab   de fg";
        String[] expected = new String[]{"ab", "", "", "de", "fg"};

        String[] actual = FastSplitUtils.splitByWholeSeparatorPreserveAllTokens(input, null, -1);
        assertEquals(expected.length, actual.length);
        for (int i = 0; i < actual.length; i += 1) {
            assertEquals(expected[i], actual[i]);
        }

        // test delimiter singlechar
        input = "1::2:::3::::4";
        expected = new String[]{"1", "", "2", "", "", "3", "", "", "", "4"};

        actual = FastSplitUtils.splitByWholeSeparatorPreserveAllTokens(input, ":", -1);
        assertEquals(expected.length, actual.length);
        for (int i = 0; i < actual.length; i += 1) {
            assertEquals(expected[i], actual[i]);
        }

        // test delimiter multichar
        input = "1::2:::3::::4";
        expected = new String[]{"1", "2", ":3", "", "4"};

        actual = FastSplitUtils.splitByWholeSeparatorPreserveAllTokens(input, "::", -1);
        assertEquals(expected.length, actual.length);
        for (int i = 0; i < actual.length; i += 1) {
            assertEquals(expected[i], actual[i]);
        }

        // test delimiter char with max
        input = "1::2::3:4";
        expected = new String[]{"1", "", "2", ":3:4"};

        actual = FastSplitUtils.splitByWholeSeparatorPreserveAllTokens(input, ":", 4);
        assertEquals(expected.length, actual.length);
        for (int i = 0; i < actual.length; i += 1) {
            assertEquals(expected[i], actual[i]);
        }
    }

    @Test
    public void testSplitByWholeString_StringStringBoolean() {
        assertArrayEquals(null, FastSplitUtils.splitByWholeSeparator(null, "."));

        assertEquals(0, FastSplitUtils.splitByWholeSeparator("", ".").length);

        final String stringToSplitOnNulls = "ab   de fg";
        final String[] splitOnNullExpectedResults = {"ab", "de", "fg"};

        final String[] splitOnNullResults = FastSplitUtils.splitByWholeSeparator(stringToSplitOnNulls, null);
        assertEquals(splitOnNullExpectedResults.length, splitOnNullResults.length);
        for (int i = 0; i < splitOnNullExpectedResults.length; i += 1) {
            assertEquals(splitOnNullExpectedResults[i], splitOnNullResults[i]);
        }

        final String stringToSplitOnCharactersAndString = "abstemiouslyaeiouyabstemiously";

        final String[] splitOnStringExpectedResults = {"abstemiously", "abstemiously"};
        final String[] splitOnStringResults = FastSplitUtils.splitByWholeSeparator(stringToSplitOnCharactersAndString, "aeiouy");
        assertEquals(splitOnStringExpectedResults.length, splitOnStringResults.length);
        for (int i = 0; i < splitOnStringExpectedResults.length; i += 1) {
            assertEquals(splitOnStringExpectedResults[i], splitOnStringResults[i]);
        }

        final String[] splitWithMultipleSeparatorExpectedResults = {"ab", "cd", "ef"};
        final String[] splitWithMultipleSeparator = FastSplitUtils.splitByWholeSeparator("ab:cd::ef", ":");
        assertEquals(splitWithMultipleSeparatorExpectedResults.length, splitWithMultipleSeparator.length);
        for (int i = 0; i < splitWithMultipleSeparatorExpectedResults.length; i++) {
            assertEquals(splitWithMultipleSeparatorExpectedResults[i], splitWithMultipleSeparator[i]);
        }
    }

    @Test
    public void testSplitByWholeString_StringStringBooleanInt() {
        assertArrayEquals(null, FastSplitUtils.splitByWholeSeparator(null, ".", 3));

        assertEquals(0, FastSplitUtils.splitByWholeSeparator("", ".", 3).length);

        final String stringToSplitOnNulls = "ab   de fg";
        final String[] splitOnNullExpectedResults = {"ab", "de fg"};
        //String[] splitOnNullExpectedResults = { "ab", "de" } ;

        final String[] splitOnNullResults = FastSplitUtils.splitByWholeSeparator(stringToSplitOnNulls, null, 2);
        assertEquals(splitOnNullExpectedResults.length, splitOnNullResults.length);
        for (int i = 0; i < splitOnNullExpectedResults.length; i += 1) {
            assertEquals(splitOnNullExpectedResults[i], splitOnNullResults[i]);
        }

        final String stringToSplitOnCharactersAndString = "abstemiouslyaeiouyabstemiouslyaeiouyabstemiously";

        final String[] splitOnStringExpectedResults = {"abstemiously", "abstemiouslyaeiouyabstemiously"};
        //String[] splitOnStringExpectedResults = { "abstemiously", "abstemiously" } ;
        final String[] splitOnStringResults = FastSplitUtils.splitByWholeSeparator(stringToSplitOnCharactersAndString, "aeiouy", 2);
        assertEquals(splitOnStringExpectedResults.length, splitOnStringResults.length);
        for (int i = 0; i < splitOnStringExpectedResults.length; i++) {
            assertEquals(splitOnStringExpectedResults[i], splitOnStringResults[i]);
        }
    }

    @Test
    public void testSplitPreserveAllTokens_String() {
        assertNull(FastSplitUtils.splitPreserveAllTokens(null));
        assertEquals(0, FastSplitUtils.splitPreserveAllTokens("").length);

        String str = "abc def";
        String[] res = FastSplitUtils.splitPreserveAllTokens(str);
        assertEquals(2, res.length);
        assertEquals("abc", res[0]);
        assertEquals("def", res[1]);

        str = "abc  def";
        res = FastSplitUtils.splitPreserveAllTokens(str);
        assertEquals(3, res.length);
        assertEquals("abc", res[0]);
        assertEquals("", res[1]);
        assertEquals("def", res[2]);

        str = " abc ";
        res = FastSplitUtils.splitPreserveAllTokens(str);
        assertEquals(3, res.length);
        assertEquals("", res[0]);
        assertEquals("abc", res[1]);
        assertEquals("", res[2]);

        str = "a b .c";
        res = FastSplitUtils.splitPreserveAllTokens(str);
        assertEquals(3, res.length);
        assertEquals("a", res[0]);
        assertEquals("b", res[1]);
        assertEquals(".c", res[2]);

        str = " a b .c";
        res = FastSplitUtils.splitPreserveAllTokens(str);
        assertEquals(4, res.length);
        assertEquals("", res[0]);
        assertEquals("a", res[1]);
        assertEquals("b", res[2]);
        assertEquals(".c", res[3]);

        str = "a  b  .c";
        res = FastSplitUtils.splitPreserveAllTokens(str);
        assertEquals(5, res.length);
        assertEquals("a", res[0]);
        assertEquals("", res[1]);
        assertEquals("b", res[2]);
        assertEquals("", res[3]);
        assertEquals(".c", res[4]);

        str = " a  ";
        res = FastSplitUtils.splitPreserveAllTokens(str);
        assertEquals(4, res.length);
        assertEquals("", res[0]);
        assertEquals("a", res[1]);
        assertEquals("", res[2]);
        assertEquals("", res[3]);

        str = " a  b";
        res = FastSplitUtils.splitPreserveAllTokens(str);
        assertEquals(4, res.length);
        assertEquals("", res[0]);
        assertEquals("a", res[1]);
        assertEquals("", res[2]);
        assertEquals("b", res[3]);

        str = "a" + WHITESPACE + "b" + NON_WHITESPACE + "c";
        res = FastSplitUtils.splitPreserveAllTokens(str);
        assertEquals(WHITESPACE.length() + 1, res.length);
        assertEquals("a", res[0]);
        for (int i = 1; i < WHITESPACE.length() - 1; i++) {
            assertEquals("", res[i]);
        }
        assertEquals("b" + NON_WHITESPACE + "c", res[WHITESPACE.length()]);
    }

    @Test
    public void testSplitPreserveAllTokens_StringChar() {
        assertNull(FastSplitUtils.splitPreserveAllTokens(null, '.'));
        assertEquals(0, FastSplitUtils.splitPreserveAllTokens("", '.').length);

        String str = "a.b. c";
        String[] res = FastSplitUtils.splitPreserveAllTokens(str, '.');
        assertEquals(3, res.length);
        assertEquals("a", res[0]);
        assertEquals("b", res[1]);
        assertEquals(" c", res[2]);

        str = "a.b.. c";
        res = FastSplitUtils.splitPreserveAllTokens(str, '.');
        assertEquals(4, res.length);
        assertEquals("a", res[0]);
        assertEquals("b", res[1]);
        assertEquals("", res[2]);
        assertEquals(" c", res[3]);

        str = ".a.";
        res = FastSplitUtils.splitPreserveAllTokens(str, '.');
        assertEquals(3, res.length);
        assertEquals("", res[0]);
        assertEquals("a", res[1]);
        assertEquals("", res[2]);

        str = ".a..";
        res = FastSplitUtils.splitPreserveAllTokens(str, '.');
        assertEquals(4, res.length);
        assertEquals("", res[0]);
        assertEquals("a", res[1]);
        assertEquals("", res[2]);
        assertEquals("", res[3]);

        str = "..a.";
        res = FastSplitUtils.splitPreserveAllTokens(str, '.');
        assertEquals(4, res.length);
        assertEquals("", res[0]);
        assertEquals("", res[1]);
        assertEquals("a", res[2]);
        assertEquals("", res[3]);

        str = "..a";
        res = FastSplitUtils.splitPreserveAllTokens(str, '.');
        assertEquals(3, res.length);
        assertEquals("", res[0]);
        assertEquals("", res[1]);
        assertEquals("a", res[2]);

        str = "a b c";
        res = FastSplitUtils.splitPreserveAllTokens(str, ' ');
        assertEquals(3, res.length);
        assertEquals("a", res[0]);
        assertEquals("b", res[1]);
        assertEquals("c", res[2]);

        str = "a  b  c";
        res = FastSplitUtils.splitPreserveAllTokens(str, ' ');
        assertEquals(5, res.length);
        assertEquals("a", res[0]);
        assertEquals("", res[1]);
        assertEquals("b", res[2]);
        assertEquals("", res[3]);
        assertEquals("c", res[4]);

        str = " a b c";
        res = FastSplitUtils.splitPreserveAllTokens(str, ' ');
        assertEquals(4, res.length);
        assertEquals("", res[0]);
        assertEquals("a", res[1]);
        assertEquals("b", res[2]);
        assertEquals("c", res[3]);

        str = "  a b c";
        res = FastSplitUtils.splitPreserveAllTokens(str, ' ');
        assertEquals(5, res.length);
        assertEquals("", res[0]);
        assertEquals("", res[1]);
        assertEquals("a", res[2]);
        assertEquals("b", res[3]);
        assertEquals("c", res[4]);

        str = "a b c ";
        res = FastSplitUtils.splitPreserveAllTokens(str, ' ');
        assertEquals(4, res.length);
        assertEquals("a", res[0]);
        assertEquals("b", res[1]);
        assertEquals("c", res[2]);
        assertEquals("", res[3]);

        str = "a b c  ";
        res = FastSplitUtils.splitPreserveAllTokens(str, ' ');
        assertEquals(5, res.length);
        assertEquals("a", res[0]);
        assertEquals("b", res[1]);
        assertEquals("c", res[2]);
        assertEquals("", res[3]);
        assertEquals("", res[3]);

        // Match example in javadoc
        {
            String[] results;
            final String[] expectedResults = {"a", "", "b", "c"};
            results = FastSplitUtils.splitPreserveAllTokens("a..b.c", '.');
            assertEquals(expectedResults.length, results.length);
            for (int i = 0; i < expectedResults.length; i++) {
                assertEquals(expectedResults[i], results[i]);
            }
        }
    }

    @Test
    public void testSplitPreserveAllTokens_StringString_StringStringInt() {
        assertNull(FastSplitUtils.splitPreserveAllTokens(null, "."));
        assertNull(FastSplitUtils.splitPreserveAllTokens(null, ".", 3));

        assertEquals(0, FastSplitUtils.splitPreserveAllTokens("", ".").length);
        assertEquals(0, FastSplitUtils.splitPreserveAllTokens("", ".", 3).length);

        innerTestSplitPreserveAllTokens('.', ".", ' ');
        innerTestSplitPreserveAllTokens('.', ".", ',');
        innerTestSplitPreserveAllTokens('.', ".,", 'x');
        for (int i = 0; i < WHITESPACE.length(); i++) {
            for (int j = 0; j < NON_WHITESPACE.length(); j++) {
                innerTestSplitPreserveAllTokens(WHITESPACE.charAt(i), null, NON_WHITESPACE.charAt(j));
                innerTestSplitPreserveAllTokens(WHITESPACE.charAt(i), String.valueOf(WHITESPACE.charAt(i)), NON_WHITESPACE.charAt(j));
            }
        }

        {
            String[] results;
            final String[] expectedResults = {"ab", "de fg"};
            results = FastSplitUtils.splitPreserveAllTokens("ab de fg", null, 2);
            assertEquals(expectedResults.length, results.length);
            for (int i = 0; i < expectedResults.length; i++) {
                assertEquals(expectedResults[i], results[i]);
            }
        }

        {
            String[] results;
            final String[] expectedResults = {"ab", "  de fg"};
            results = FastSplitUtils.splitPreserveAllTokens("ab   de fg", null, 2);
            assertEquals(expectedResults.length, results.length);
            for (int i = 0; i < expectedResults.length; i++) {
                assertEquals(expectedResults[i], results[i]);
            }
        }

        {
            String[] results;
            final String[] expectedResults = {"ab", "::de:fg"};
            results = FastSplitUtils.splitPreserveAllTokens("ab:::de:fg", ":", 2);
            assertEquals(expectedResults.length, results.length);
            for (int i = 0; i < expectedResults.length; i++) {
                assertEquals(expectedResults[i], results[i]);
            }
        }

        {
            String[] results;
            final String[] expectedResults = {"ab", "", " de fg"};
            results = FastSplitUtils.splitPreserveAllTokens("ab   de fg", null, 3);
            assertEquals(expectedResults.length, results.length);
            for (int i = 0; i < expectedResults.length; i++) {
                assertEquals(expectedResults[i], results[i]);
            }
        }

        {
            String[] results;
            final String[] expectedResults = {"ab", "", "", "de fg"};
            results = FastSplitUtils.splitPreserveAllTokens("ab   de fg", null, 4);
            assertEquals(expectedResults.length, results.length);
            for (int i = 0; i < expectedResults.length; i++) {
                assertEquals(expectedResults[i], results[i]);
            }
        }

        {
            final String[] expectedResults = {"ab", "cd:ef"};
            String[] results;
            results = FastSplitUtils.splitPreserveAllTokens("ab:cd:ef", ":", 2);
            assertEquals(expectedResults.length, results.length);
            for (int i = 0; i < expectedResults.length; i++) {
                assertEquals(expectedResults[i], results[i]);
            }
        }

        {
            String[] results;
            final String[] expectedResults = {"ab", ":cd:ef"};
            results = FastSplitUtils.splitPreserveAllTokens("ab::cd:ef", ":", 2);
            assertEquals(expectedResults.length, results.length);
            for (int i = 0; i < expectedResults.length; i++) {
                assertEquals(expectedResults[i], results[i]);
            }
        }

        {
            String[] results;
            final String[] expectedResults = {"ab", "", ":cd:ef"};
            results = FastSplitUtils.splitPreserveAllTokens("ab:::cd:ef", ":", 3);
            assertEquals(expectedResults.length, results.length);
            for (int i = 0; i < expectedResults.length; i++) {
                assertEquals(expectedResults[i], results[i]);
            }
        }

        {
            String[] results;
            final String[] expectedResults = {"ab", "", "", "cd:ef"};
            results = FastSplitUtils.splitPreserveAllTokens("ab:::cd:ef", ":", 4);
            assertEquals(expectedResults.length, results.length);
            for (int i = 0; i < expectedResults.length; i++) {
                assertEquals(expectedResults[i], results[i]);
            }
        }

        {
            String[] results;
            final String[] expectedResults = {"", "ab", "", "", "cd:ef"};
            results = FastSplitUtils.splitPreserveAllTokens(":ab:::cd:ef", ":", 5);
            assertEquals(expectedResults.length, results.length);
            for (int i = 0; i < expectedResults.length; i++) {
                assertEquals(expectedResults[i], results[i]);
            }
        }

        {
            String[] results;
            final String[] expectedResults = {"", "", "ab", "", "", "cd:ef"};
            results = FastSplitUtils.splitPreserveAllTokens("::ab:::cd:ef", ":", 6);
            assertEquals(expectedResults.length, results.length);
            for (int i = 0; i < expectedResults.length; i++) {
                assertEquals(expectedResults[i], results[i]);
            }
        }

    }

    @Test
    public void testSplitBuffer() throws Exception {
        SplitBuffer buffer = new SplitBuffer();
        Field field = buffer.getClass().getDeclaredField("elementData");
        field.setAccessible(true);

        String[] elementData = ((String[]) field.get(buffer));
        assertEquals(0, elementData.length);

        for (int i = 1; i <= 5; i++) {
            buffer.add(i + "");
        }
        elementData = ((String[]) field.get(buffer));
        assertArrayEquals(new String[]{"1", "2", "3", "4", "5"}, buffer.toArray());
        assertEquals(10, elementData.length);
        assertEquals("3", buffer.get(2));

        for (int i = 6; i <= 11; i++) {
            buffer.add(i + "");
        }
        elementData = ((String[]) field.get(buffer));
        assertArrayEquals(new String[]{"1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"}, buffer.toArray());
        assertEquals(15, elementData.length);
        assertEquals("7", buffer.get(6));

        buffer.reset();
        elementData = ((String[]) field.get(buffer));
        assertArrayEquals(new String[]{}, buffer.toArray());
        assertEquals(15, elementData.length);
    }

}
