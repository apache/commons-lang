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

package org.apache.commons.lang3.text;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.nio.CharBuffer;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.ArrayUtils;
import org.junit.jupiter.api.Test;

/**
 * Unit tests for {@link org.apache.commons.lang3.text.StrBuilder}.
 */
@Deprecated
public class StrBuilderTest extends AbstractLangTest {

    @Test
    public void testConstructors() {
        final StrBuilder sb0 = new StrBuilder();
        assertEquals(32, sb0.capacity());
        assertEquals(0, sb0.length());
        assertEquals(0, sb0.size());

        final StrBuilder sb1 = new StrBuilder(32);
        assertEquals(32, sb1.capacity());
        assertEquals(0, sb1.length());
        assertEquals(0, sb1.size());

        final StrBuilder sb2 = new StrBuilder(0);
        assertEquals(32, sb2.capacity());
        assertEquals(0, sb2.length());
        assertEquals(0, sb2.size());

        final StrBuilder sb3 = new StrBuilder(-1);
        assertEquals(32, sb3.capacity());
        assertEquals(0, sb3.length());
        assertEquals(0, sb3.size());

        final StrBuilder sb4 = new StrBuilder(1);
        assertEquals(1, sb4.capacity());
        assertEquals(0, sb4.length());
        assertEquals(0, sb4.size());

        final StrBuilder sb5 = new StrBuilder(null);
        assertEquals(32, sb5.capacity());
        assertEquals(0, sb5.length());
        assertEquals(0, sb5.size());

        final StrBuilder sb6 = new StrBuilder("");
        assertEquals(32, sb6.capacity());
        assertEquals(0, sb6.length());
        assertEquals(0, sb6.size());

        final StrBuilder sb7 = new StrBuilder("foo");
        assertEquals(35, sb7.capacity());
        assertEquals(3, sb7.length());
        assertEquals(3, sb7.size());
    }

    @Test
    public void testChaining() {
        final StrBuilder sb = new StrBuilder();
        assertSame(sb, sb.setNewLineText(null));
        assertSame(sb, sb.setNullText(null));
        assertSame(sb, sb.setLength(1));
        assertSame(sb, sb.setCharAt(0, 'a'));
        assertSame(sb, sb.ensureCapacity(0));
        assertSame(sb, sb.minimizeCapacity());
        assertSame(sb, sb.clear());
        assertSame(sb, sb.reverse());
        assertSame(sb, sb.trim());
    }

    @Test
    public void testReadFromReader() throws Exception {
        String s = "";
        for (int i = 0; i < 100; ++i) {
            final StrBuilder sb = new StrBuilder();
            final int len = sb.readFrom(new StringReader(s));

            assertEquals(s.length(), len);
            assertEquals(s, sb.toString());

            s += Integer.toString(i);
        }
    }

    @Test
    public void testReadFromReaderAppendsToEnd() throws Exception {
        final StrBuilder sb = new StrBuilder("Test");
        sb.readFrom(new StringReader(" 123"));
        assertEquals("Test 123", sb.toString());
    }

    @Test
    public void testReadFromCharBuffer() throws Exception {
        String s = "";
        for (int i = 0; i < 100; ++i) {
            final StrBuilder sb = new StrBuilder();
            final int len = sb.readFrom(CharBuffer.wrap(s));

            assertEquals(s.length(), len);
            assertEquals(s, sb.toString());

            s += Integer.toString(i);
        }
    }

    @Test
    public void testReadFromCharBufferAppendsToEnd() throws Exception {
        final StrBuilder sb = new StrBuilder("Test");
        sb.readFrom(CharBuffer.wrap(" 123"));
        assertEquals("Test 123", sb.toString());
    }

    @Test
    public void testReadFromReadable() throws Exception {
        String s = "";
        for (int i = 0; i < 100; ++i) {
            final StrBuilder sb = new StrBuilder();
            final int len = sb.readFrom(new MockReadable(s));

            assertEquals(s.length(), len);
            assertEquals(s, sb.toString());

            s += Integer.toString(i);
        }
    }

    @Test
    public void testReadFromReadableAppendsToEnd() throws Exception {
        final StrBuilder sb = new StrBuilder("Test");
        sb.readFrom(new MockReadable(" 123"));
        assertEquals("Test 123", sb.toString());
    }

    private static class MockReadable implements Readable {

        private final CharBuffer src;

        MockReadable(final String src) {
            this.src = CharBuffer.wrap(src);
        }

        @Override
        public int read(final CharBuffer cb) throws IOException {
            return src.read(cb);
        }
    }

    @Test
    public void testGetSetNewLineText() {
        final StrBuilder sb = new StrBuilder();
        assertNull(sb.getNewLineText());

        sb.setNewLineText("#");
        assertEquals("#", sb.getNewLineText());

        sb.setNewLineText("");
        assertEquals("", sb.getNewLineText());

        sb.setNewLineText(null);
        assertNull(sb.getNewLineText());
    }

    @Test
    public void testGetSetNullText() {
        final StrBuilder sb = new StrBuilder();
        assertNull(sb.getNullText());

        sb.setNullText("null");
        assertEquals("null", sb.getNullText());

        sb.setNullText("");
        assertNull(sb.getNullText());

        sb.setNullText("NULL");
        assertEquals("NULL", sb.getNullText());

        sb.setNullText(null);
        assertNull(sb.getNullText());
    }

    @Test
    public void testCapacityAndLength() {
        final StrBuilder sb = new StrBuilder();
        assertEquals(32, sb.capacity());
        assertEquals(0, sb.length());
        assertEquals(0, sb.size());
        assertTrue(sb.isEmpty());

        sb.minimizeCapacity();
        assertEquals(0, sb.capacity());
        assertEquals(0, sb.length());
        assertEquals(0, sb.size());
        assertTrue(sb.isEmpty());

        sb.ensureCapacity(32);
        assertTrue(sb.capacity() >= 32);
        assertEquals(0, sb.length());
        assertEquals(0, sb.size());
        assertTrue(sb.isEmpty());

        sb.append("foo");
        assertTrue(sb.capacity() >= 32);
        assertEquals(3, sb.length());
        assertEquals(3, sb.size());
        assertFalse(sb.isEmpty());

        sb.clear();
        assertTrue(sb.capacity() >= 32);
        assertEquals(0, sb.length());
        assertEquals(0, sb.size());
        assertTrue(sb.isEmpty());

        sb.append("123456789012345678901234567890123");
        assertTrue(sb.capacity() > 32);
        assertEquals(33, sb.length());
        assertEquals(33, sb.size());
        assertFalse(sb.isEmpty());

        sb.ensureCapacity(16);
        assertTrue(sb.capacity() > 16);
        assertEquals(33, sb.length());
        assertEquals(33, sb.size());
        assertFalse(sb.isEmpty());

        sb.minimizeCapacity();
        assertEquals(33, sb.capacity());
        assertEquals(33, sb.length());
        assertEquals(33, sb.size());
        assertFalse(sb.isEmpty());

        assertThrows(
                IndexOutOfBoundsException.class,
                () -> sb.setLength(-1),
                "setLength(-1) expected StringIndexOutOfBoundsException");

        sb.setLength(33);
        assertEquals(33, sb.capacity());
        assertEquals(33, sb.length());
        assertEquals(33, sb.size());
        assertFalse(sb.isEmpty());

        sb.setLength(16);
        assertTrue(sb.capacity() >= 16);
        assertEquals(16, sb.length());
        assertEquals(16, sb.size());
        assertEquals("1234567890123456", sb.toString());
        assertFalse(sb.isEmpty());

        sb.setLength(32);
        assertTrue(sb.capacity() >= 32);
        assertEquals(32, sb.length());
        assertEquals(32, sb.size());
        assertEquals("1234567890123456\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0", sb.toString());
        assertFalse(sb.isEmpty());

        sb.setLength(0);
        assertTrue(sb.capacity() >= 32);
        assertEquals(0, sb.length());
        assertEquals(0, sb.size());
        assertTrue(sb.isEmpty());
    }

    @Test
    public void testLength() {
        final StrBuilder sb = new StrBuilder();
        assertEquals(0, sb.length());

        sb.append("Hello");
        assertEquals(5, sb.length());
    }

    @Test
    public void testSetLength() {
        final StrBuilder sb = new StrBuilder();
        sb.append("Hello");
        sb.setLength(2);  // shorten
        assertEquals("He", sb.toString());
        sb.setLength(2);  // no change
        assertEquals("He", sb.toString());
        sb.setLength(3);  // lengthen
        assertEquals("He\0", sb.toString());

        assertThrows(
                IndexOutOfBoundsException.class,
                () -> sb.setLength(-1),
                "setLength(-1) expected StringIndexOutOfBoundsException");
    }

    @Test
    public void testCapacity() {
        final StrBuilder sb = new StrBuilder();
        assertEquals(sb.buffer.length, sb.capacity());

        sb.append("HelloWorldHelloWorldHelloWorldHelloWorld");
        assertEquals(sb.buffer.length, sb.capacity());
    }

    @Test
    public void testEnsureCapacity() {
        final StrBuilder sb = new StrBuilder();
        sb.ensureCapacity(2);
        assertTrue(sb.capacity() >= 2);

        sb.ensureCapacity(-1);
        assertTrue(sb.capacity() >= 0);

        sb.append("HelloWorld");
        sb.ensureCapacity(40);
        assertTrue(sb.capacity() >= 40);
    }

    @Test
    public void testMinimizeCapacity() {
        final StrBuilder sb = new StrBuilder();
        sb.minimizeCapacity();
        assertEquals(0, sb.capacity());

        sb.append("HelloWorld");
        sb.minimizeCapacity();
        assertEquals(10, sb.capacity());
    }

    @Test
    public void testSize() {
        final StrBuilder sb = new StrBuilder();
        assertEquals(0, sb.size());

        sb.append("Hello");
        assertEquals(5, sb.size());
    }

    @Test
    public void testIsEmpty() {
        final StrBuilder sb = new StrBuilder();
        assertTrue(sb.isEmpty());

        sb.append("Hello");
        assertFalse(sb.isEmpty());

        sb.clear();
        assertTrue(sb.isEmpty());
    }

    @Test
    public void testClear() {
        final StrBuilder sb = new StrBuilder();
        sb.append("Hello");
        sb.clear();
        assertEquals(0, sb.length());
        assertTrue(sb.buffer.length >= 5);
    }

    @Test
    public void testCharAt() {
        final StrBuilder sb = new StrBuilder();
        assertThrows(
                IndexOutOfBoundsException.class, () -> sb.charAt(0), "charAt(0) expected IndexOutOfBoundsException");
        assertThrows(
                IndexOutOfBoundsException.class, () -> sb.charAt(-1), "charAt(-1) expected IndexOutOfBoundsException");
        sb.append("foo");
        assertEquals('f', sb.charAt(0));
        assertEquals('o', sb.charAt(1));
        assertEquals('o', sb.charAt(2));
        assertThrows(
                IndexOutOfBoundsException.class, () -> sb.charAt(-1), "charAt(-1) expected IndexOutOfBoundsException");
        assertThrows(
                IndexOutOfBoundsException.class, () -> sb.charAt(3), "charAt(3) expected IndexOutOfBoundsException");
    }

    @Test
    public void testSetCharAt() {
        final StrBuilder sb = new StrBuilder();
        assertThrows(
                IndexOutOfBoundsException.class,
                () -> sb.setCharAt(0, 'f'),
                "setCharAt(0, ) expected IndexOutOfBoundsException");
        assertThrows(
                IndexOutOfBoundsException.class,
                () -> sb.setCharAt(-1, 'f'),
                "setCharAt(-1, ) expected IndexOutOfBoundsException");
        sb.append("foo");
        sb.setCharAt(0, 'b');
        sb.setCharAt(1, 'a');
        sb.setCharAt(2, 'r');
        assertThrows(
                IndexOutOfBoundsException.class,
                () -> sb.setCharAt(3, '!'),
                "setCharAt(3, ) expected IndexOutOfBoundsException");
        assertEquals("bar", sb.toString());
    }

    @Test
    public void testDeleteCharAt() {
        final StrBuilder sb = new StrBuilder("abc");
        sb.deleteCharAt(0);
        assertEquals("bc", sb.toString());

        assertThrows(IndexOutOfBoundsException.class, () -> sb.deleteCharAt(1000));
    }

    @Test
    public void testToCharArray() {
        final StrBuilder sb = new StrBuilder();
        assertEquals(ArrayUtils.EMPTY_CHAR_ARRAY, sb.toCharArray());

        char[] a = sb.toCharArray();
        assertNotNull(a, "toCharArray() result is null");
        assertEquals(0, a.length, "toCharArray() result is too large");

        sb.append("junit");
        a = sb.toCharArray();
        assertEquals(5, a.length, "toCharArray() result incorrect length");
        assertArrayEquals("junit".toCharArray(), a, "toCharArray() result does not match");
    }

    @Test
    public void testToCharArrayIntInt() {
        final StrBuilder sb = new StrBuilder();
        assertEquals(ArrayUtils.EMPTY_CHAR_ARRAY, sb.toCharArray(0, 0));

        sb.append("junit");
        char[] a = sb.toCharArray(0, 20); // too large test
        assertEquals(5, a.length, "toCharArray(int, int) result incorrect length");
        assertArrayEquals("junit".toCharArray(), a, "toCharArray(int, int) result does not match");

        a = sb.toCharArray(0, 4);
        assertEquals(4, a.length, "toCharArray(int, int) result incorrect length");
        assertArrayEquals("juni".toCharArray(), a, "toCharArray(int, int) result does not match");

        a = sb.toCharArray(0, 4);
        assertEquals(4, a.length, "toCharArray(int, int) result incorrect length");
        assertArrayEquals("juni".toCharArray(), a, "toCharArray(int, int) result does not match");

        a = sb.toCharArray(0, 1);
        assertNotNull(a, "toCharArray(int, int) result is null");

        assertThrows(
                IndexOutOfBoundsException.class, () -> sb.toCharArray(-1, 5), "no string index out of bound on -1");

        assertThrows(
                IndexOutOfBoundsException.class, () -> sb.toCharArray(6, 5), "no string index out of bound on -1");
    }

    @Test
    public void testGetChars ( ) {
        final StrBuilder sb = new StrBuilder();

        char[] input = new char[10];
        char[] a = sb.getChars(input);
        assertSame (input, a);
        assertArrayEquals(new char[10], a);

        sb.append("junit");
        a = sb.getChars(input);
        assertSame(input, a);
        assertArrayEquals(new char[]{'j', 'u', 'n', 'i', 't', 0, 0, 0, 0, 0}, a);

        a = sb.getChars(null);
        assertNotSame(input, a);
        assertEquals(5, a.length);
        assertArrayEquals("junit".toCharArray(), a);

        input = new char[5];
        a = sb.getChars(input);
        assertSame(input, a);

        input = new char[4];
        a = sb.getChars(input);
        assertNotSame(input, a);
    }

    @Test
    public void testGetCharsIntIntCharArrayInt( ) {
        final StrBuilder sb = new StrBuilder();

        sb.append("junit");
        final char[] a = new char[5];
        sb.getChars(0, 5, a, 0);
        assertArrayEquals(new char[]{'j', 'u', 'n', 'i', 't'}, a);

        final char[] b = new char[5];
        sb.getChars(0, 2, b, 3);
        assertArrayEquals(new char[]{0, 0, 0, 'j', 'u'}, b);

        assertThrows(IndexOutOfBoundsException.class, () -> sb.getChars(-1, 0, b, 0));
        assertThrows(IndexOutOfBoundsException.class, () -> sb.getChars(0, -1, b, 0));
        assertThrows(IndexOutOfBoundsException.class, () -> sb.getChars(0, 20, b, 0));
        assertThrows(IndexOutOfBoundsException.class, () -> sb.getChars(4, 2, b, 0));
    }

    @Test
    public void testDeleteIntInt() {
        final StrBuilder sb = new StrBuilder("abc");
        sb.delete(0, 1);
        assertEquals("bc", sb.toString());
        sb.delete(1, 2);
        assertEquals("b", sb.toString());
        sb.delete(0, 1);
        assertEquals("", sb.toString());
        sb.delete(0, 1000);
        assertEquals("", sb.toString());

        assertThrows(IndexOutOfBoundsException.class, () -> sb.delete(1, 2));
        assertThrows(IndexOutOfBoundsException.class, () -> sb.delete(-1, 1));
        assertThrows(IndexOutOfBoundsException.class, () -> new StrBuilder("anything").delete(2, 1));
    }

    @Test
    public void testDeleteAll_char() {
        StrBuilder sb = new StrBuilder("abcbccba");
        sb.deleteAll('X');
        assertEquals("abcbccba", sb.toString());
        sb.deleteAll('a');
        assertEquals("bcbccb", sb.toString());
        sb.deleteAll('c');
        assertEquals("bbb", sb.toString());
        sb.deleteAll('b');
        assertEquals("", sb.toString());

        sb = new StrBuilder("");
        sb.deleteAll('b');
        assertEquals("", sb.toString());
    }

    @Test
    public void testDeleteFirst_char() {
        StrBuilder sb = new StrBuilder("abcba");
        sb.deleteFirst('X');
        assertEquals("abcba", sb.toString());
        sb.deleteFirst('a');
        assertEquals("bcba", sb.toString());
        sb.deleteFirst('c');
        assertEquals("bba", sb.toString());
        sb.deleteFirst('b');
        assertEquals("ba", sb.toString());

        sb = new StrBuilder("");
        sb.deleteFirst('b');
        assertEquals("", sb.toString());
    }

    @Test
    public void testDeleteAll_String() {
        StrBuilder sb = new StrBuilder("abcbccba");
        sb.deleteAll((String) null);
        assertEquals("abcbccba", sb.toString());
        sb.deleteAll("");
        assertEquals("abcbccba", sb.toString());

        sb.deleteAll("X");
        assertEquals("abcbccba", sb.toString());
        sb.deleteAll("a");
        assertEquals("bcbccb", sb.toString());
        sb.deleteAll("c");
        assertEquals("bbb", sb.toString());
        sb.deleteAll("b");
        assertEquals("", sb.toString());

        sb = new StrBuilder("abcbccba");
        sb.deleteAll("bc");
        assertEquals("acba", sb.toString());

        sb = new StrBuilder("");
        sb.deleteAll("bc");
        assertEquals("", sb.toString());
    }

    @Test
    public void testDeleteFirst_String() {
        StrBuilder sb = new StrBuilder("abcbccba");
        sb.deleteFirst((String) null);
        assertEquals("abcbccba", sb.toString());
        sb.deleteFirst("");
        assertEquals("abcbccba", sb.toString());

        sb.deleteFirst("X");
        assertEquals("abcbccba", sb.toString());
        sb.deleteFirst("a");
        assertEquals("bcbccba", sb.toString());
        sb.deleteFirst("c");
        assertEquals("bbccba", sb.toString());
        sb.deleteFirst("b");
        assertEquals("bccba", sb.toString());

        sb = new StrBuilder("abcbccba");
        sb.deleteFirst("bc");
        assertEquals("abccba", sb.toString());

        sb = new StrBuilder("");
        sb.deleteFirst("bc");
        assertEquals("", sb.toString());
    }

    @Test
    public void testDeleteAll_StrMatcher() {
        StrBuilder sb = new StrBuilder("A0xA1A2yA3");
        sb.deleteAll((StrMatcher) null);
        assertEquals("A0xA1A2yA3", sb.toString());
        sb.deleteAll(A_NUMBER_MATCHER);
        assertEquals("xy", sb.toString());

        sb = new StrBuilder("Ax1");
        sb.deleteAll(A_NUMBER_MATCHER);
        assertEquals("Ax1", sb.toString());

        sb = new StrBuilder("");
        sb.deleteAll(A_NUMBER_MATCHER);
        assertEquals("", sb.toString());
    }

    @Test
    public void testDeleteFirst_StrMatcher() {
        StrBuilder sb = new StrBuilder("A0xA1A2yA3");
        sb.deleteFirst((StrMatcher) null);
        assertEquals("A0xA1A2yA3", sb.toString());
        sb.deleteFirst(A_NUMBER_MATCHER);
        assertEquals("xA1A2yA3", sb.toString());

        sb = new StrBuilder("Ax1");
        sb.deleteFirst(A_NUMBER_MATCHER);
        assertEquals("Ax1", sb.toString());

        sb = new StrBuilder("");
        sb.deleteFirst(A_NUMBER_MATCHER);
        assertEquals("", sb.toString());
    }

    @Test
    public void testReplace_int_int_String() {
        final StrBuilder sb = new StrBuilder("abc");
        sb.replace(0, 1, "d");
        assertEquals("dbc", sb.toString());
        sb.replace(0, 1, "aaa");
        assertEquals("aaabc", sb.toString());
        sb.replace(0, 3, "");
        assertEquals("bc", sb.toString());
        sb.replace(1, 2, null);
        assertEquals("b", sb.toString());
        sb.replace(1, 1000, "text");
        assertEquals("btext", sb.toString());
        sb.replace(0, 1000, "text");
        assertEquals("text", sb.toString());

        final StrBuilder sb1 = new StrBuilder("atext");
        sb1.replace(1, 1, "ny");
        assertEquals("anytext", sb1.toString());
        assertThrows(IndexOutOfBoundsException.class, () -> sb1.replace(2, 1, "anything"));

        final StrBuilder sb2 = new StrBuilder();
        assertThrows(IndexOutOfBoundsException.class, () -> sb2.replace(1, 2, "anything"));
        assertThrows(IndexOutOfBoundsException.class, () -> sb2.replace(-1, 1, "anything"));
    }

    @Test
    public void testReplaceAll_char_char() {
        final StrBuilder sb = new StrBuilder("abcbccba");
        sb.replaceAll('x', 'y');
        assertEquals("abcbccba", sb.toString());
        sb.replaceAll('a', 'd');
        assertEquals("dbcbccbd", sb.toString());
        sb.replaceAll('b', 'e');
        assertEquals("dececced", sb.toString());
        sb.replaceAll('c', 'f');
        assertEquals("defeffed", sb.toString());
        sb.replaceAll('d', 'd');
        assertEquals("defeffed", sb.toString());
    }

    @Test
    public void testReplaceFirst_char_char() {
        final StrBuilder sb = new StrBuilder("abcbccba");
        sb.replaceFirst('x', 'y');
        assertEquals("abcbccba", sb.toString());
        sb.replaceFirst('a', 'd');
        assertEquals("dbcbccba", sb.toString());
        sb.replaceFirst('b', 'e');
        assertEquals("decbccba", sb.toString());
        sb.replaceFirst('c', 'f');
        assertEquals("defbccba", sb.toString());
        sb.replaceFirst('d', 'd');
        assertEquals("defbccba", sb.toString());
    }

    @Test
    public void testReplaceAll_String_String() {
        StrBuilder sb = new StrBuilder("abcbccba");
        sb.replaceAll((String) null, null);
        assertEquals("abcbccba", sb.toString());
        sb.replaceAll((String) null, "anything");
        assertEquals("abcbccba", sb.toString());
        sb.replaceAll("", null);
        assertEquals("abcbccba", sb.toString());
        sb.replaceAll("", "anything");
        assertEquals("abcbccba", sb.toString());

        sb.replaceAll("x", "y");
        assertEquals("abcbccba", sb.toString());
        sb.replaceAll("a", "d");
        assertEquals("dbcbccbd", sb.toString());
        sb.replaceAll("d", null);
        assertEquals("bcbccb", sb.toString());
        sb.replaceAll("cb", "-");
        assertEquals("b-c-", sb.toString());

        sb = new StrBuilder("abcba");
        sb.replaceAll("b", "xbx");
        assertEquals("axbxcxbxa", sb.toString());

        sb = new StrBuilder("bb");
        sb.replaceAll("b", "xbx");
        assertEquals("xbxxbx", sb.toString());
    }

    @Test
    public void testReplaceFirst_String_String() {
        StrBuilder sb = new StrBuilder("abcbccba");
        sb.replaceFirst((String) null, null);
        assertEquals("abcbccba", sb.toString());
        sb.replaceFirst((String) null, "anything");
        assertEquals("abcbccba", sb.toString());
        sb.replaceFirst("", null);
        assertEquals("abcbccba", sb.toString());
        sb.replaceFirst("", "anything");
        assertEquals("abcbccba", sb.toString());

        sb.replaceFirst("x", "y");
        assertEquals("abcbccba", sb.toString());
        sb.replaceFirst("a", "d");
        assertEquals("dbcbccba", sb.toString());
        sb.replaceFirst("d", null);
        assertEquals("bcbccba", sb.toString());
        sb.replaceFirst("cb", "-");
        assertEquals("b-ccba", sb.toString());

        sb = new StrBuilder("abcba");
        sb.replaceFirst("b", "xbx");
        assertEquals("axbxcba", sb.toString());

        sb = new StrBuilder("bb");
        sb.replaceFirst("b", "xbx");
        assertEquals("xbxb", sb.toString());
    }

    @Test
    public void testReplaceAll_StrMatcher_String() {
        StrBuilder sb = new StrBuilder("abcbccba");
        sb.replaceAll((StrMatcher) null, null);
        assertEquals("abcbccba", sb.toString());
        sb.replaceAll((StrMatcher) null, "anything");
        assertEquals("abcbccba", sb.toString());
        sb.replaceAll(StrMatcher.noneMatcher(), null);
        assertEquals("abcbccba", sb.toString());
        sb.replaceAll(StrMatcher.noneMatcher(), "anything");
        assertEquals("abcbccba", sb.toString());

        sb.replaceAll(StrMatcher.charMatcher('x'), "y");
        assertEquals("abcbccba", sb.toString());
        sb.replaceAll(StrMatcher.charMatcher('a'), "d");
        assertEquals("dbcbccbd", sb.toString());
        sb.replaceAll(StrMatcher.charMatcher('d'), null);
        assertEquals("bcbccb", sb.toString());
        sb.replaceAll(StrMatcher.stringMatcher("cb"), "-");
        assertEquals("b-c-", sb.toString());

        sb = new StrBuilder("abcba");
        sb.replaceAll(StrMatcher.charMatcher('b'), "xbx");
        assertEquals("axbxcxbxa", sb.toString());

        sb = new StrBuilder("bb");
        sb.replaceAll(StrMatcher.charMatcher('b'), "xbx");
        assertEquals("xbxxbx", sb.toString());

        sb = new StrBuilder("A1-A2A3-A4");
        sb.replaceAll(A_NUMBER_MATCHER, "***");
        assertEquals("***-******-***", sb.toString());

        sb = new StrBuilder("Dear X, hello X.");
        sb.replaceAll(StrMatcher.stringMatcher("X"), "012345678901234567");
        assertEquals("Dear 012345678901234567, hello 012345678901234567.", sb.toString());
    }

    @Test
    public void testReplaceFirst_StrMatcher_String() {
        StrBuilder sb = new StrBuilder("abcbccba");
        sb.replaceFirst((StrMatcher) null, null);
        assertEquals("abcbccba", sb.toString());
        sb.replaceFirst((StrMatcher) null, "anything");
        assertEquals("abcbccba", sb.toString());
        sb.replaceFirst(StrMatcher.noneMatcher(), null);
        assertEquals("abcbccba", sb.toString());
        sb.replaceFirst(StrMatcher.noneMatcher(), "anything");
        assertEquals("abcbccba", sb.toString());

        sb.replaceFirst(StrMatcher.charMatcher('x'), "y");
        assertEquals("abcbccba", sb.toString());
        sb.replaceFirst(StrMatcher.charMatcher('a'), "d");
        assertEquals("dbcbccba", sb.toString());
        sb.replaceFirst(StrMatcher.charMatcher('d'), null);
        assertEquals("bcbccba", sb.toString());
        sb.replaceFirst(StrMatcher.stringMatcher("cb"), "-");
        assertEquals("b-ccba", sb.toString());

        sb = new StrBuilder("abcba");
        sb.replaceFirst(StrMatcher.charMatcher('b'), "xbx");
        assertEquals("axbxcba", sb.toString());

        sb = new StrBuilder("bb");
        sb.replaceFirst(StrMatcher.charMatcher('b'), "xbx");
        assertEquals("xbxb", sb.toString());

        sb = new StrBuilder("A1-A2A3-A4");
        sb.replaceFirst(A_NUMBER_MATCHER, "***");
        assertEquals("***-A2A3-A4", sb.toString());
    }

    @Test
    public void testReplace_StrMatcher_String_int_int_int_VaryMatcher() {
        StrBuilder sb = new StrBuilder("abcbccba");
        sb.replace(null, "x", 0, sb.length(), -1);
        assertEquals("abcbccba", sb.toString());

        sb.replace(StrMatcher.charMatcher('a'), "x", 0, sb.length(), -1);
        assertEquals("xbcbccbx", sb.toString());

        sb.replace(StrMatcher.stringMatcher("cb"), "x", 0, sb.length(), -1);
        assertEquals("xbxcxx", sb.toString());

        sb = new StrBuilder("A1-A2A3-A4");
        sb.replace(A_NUMBER_MATCHER, "***", 0, sb.length(), -1);
        assertEquals("***-******-***", sb.toString());

        sb = new StrBuilder();
        sb.replace(A_NUMBER_MATCHER, "***", 0, sb.length(), -1);
        assertEquals("", sb.toString());
    }

    @Test
    public void testReplace_StrMatcher_String_int_int_int_VaryReplace() {
        StrBuilder sb = new StrBuilder("abcbccba");
        sb.replace(StrMatcher.stringMatcher("cb"), "cb", 0, sb.length(), -1);
        assertEquals("abcbccba", sb.toString());

        sb = new StrBuilder("abcbccba");
        sb.replace(StrMatcher.stringMatcher("cb"), "-", 0, sb.length(), -1);
        assertEquals("ab-c-a", sb.toString());

        sb = new StrBuilder("abcbccba");
        sb.replace(StrMatcher.stringMatcher("cb"), "+++", 0, sb.length(), -1);
        assertEquals("ab+++c+++a", sb.toString());

        sb = new StrBuilder("abcbccba");
        sb.replace(StrMatcher.stringMatcher("cb"), "", 0, sb.length(), -1);
        assertEquals("abca", sb.toString());

        sb = new StrBuilder("abcbccba");
        sb.replace(StrMatcher.stringMatcher("cb"), null, 0, sb.length(), -1);
        assertEquals("abca", sb.toString());
    }

    @Test
    public void testReplace_StrMatcher_String_int_int_int_VaryStartIndex() {
        StrBuilder sb = new StrBuilder("aaxaaaayaa");
        sb.replace(StrMatcher.stringMatcher("aa"), "-", 0, sb.length(), -1);
        assertEquals("-x--y-", sb.toString());

        sb = new StrBuilder("aaxaaaayaa");
        sb.replace(StrMatcher.stringMatcher("aa"), "-", 1, sb.length(), -1);
        assertEquals("aax--y-", sb.toString());

        sb = new StrBuilder("aaxaaaayaa");
        sb.replace(StrMatcher.stringMatcher("aa"), "-", 2, sb.length(), -1);
        assertEquals("aax--y-", sb.toString());

        sb = new StrBuilder("aaxaaaayaa");
        sb.replace(StrMatcher.stringMatcher("aa"), "-", 3, sb.length(), -1);
        assertEquals("aax--y-", sb.toString());

        sb = new StrBuilder("aaxaaaayaa");
        sb.replace(StrMatcher.stringMatcher("aa"), "-", 4, sb.length(), -1);
        assertEquals("aaxa-ay-", sb.toString());

        sb = new StrBuilder("aaxaaaayaa");
        sb.replace(StrMatcher.stringMatcher("aa"), "-", 5, sb.length(), -1);
        assertEquals("aaxaa-y-", sb.toString());

        sb = new StrBuilder("aaxaaaayaa");
        sb.replace(StrMatcher.stringMatcher("aa"), "-", 6, sb.length(), -1);
        assertEquals("aaxaaaay-", sb.toString());

        sb = new StrBuilder("aaxaaaayaa");
        sb.replace(StrMatcher.stringMatcher("aa"), "-", 7, sb.length(), -1);
        assertEquals("aaxaaaay-", sb.toString());

        sb = new StrBuilder("aaxaaaayaa");
        sb.replace(StrMatcher.stringMatcher("aa"), "-", 8, sb.length(), -1);
        assertEquals("aaxaaaay-", sb.toString());

        sb = new StrBuilder("aaxaaaayaa");
        sb.replace(StrMatcher.stringMatcher("aa"), "-", 9, sb.length(), -1);
        assertEquals("aaxaaaayaa", sb.toString());

        sb = new StrBuilder("aaxaaaayaa");
        sb.replace(StrMatcher.stringMatcher("aa"), "-", 10, sb.length(), -1);
        assertEquals("aaxaaaayaa", sb.toString());

        final StrBuilder sb1 = new StrBuilder("aaxaaaayaa");
        assertThrows(
                IndexOutOfBoundsException.class,
                () -> sb1.replace(StrMatcher.stringMatcher("aa"), "-", 11, sb1.length(), -1));
        assertEquals("aaxaaaayaa", sb1.toString());

        final StrBuilder sb2 = new StrBuilder("aaxaaaayaa");
        assertThrows(
                IndexOutOfBoundsException.class,
                () -> sb2.replace(StrMatcher.stringMatcher("aa"), "-", -1, sb2.length(), -1));
        assertEquals("aaxaaaayaa", sb2.toString());
    }

    @Test
    public void testReplace_StrMatcher_String_int_int_int_VaryEndIndex() {
        StrBuilder sb = new StrBuilder("aaxaaaayaa");
        sb.replace(StrMatcher.stringMatcher("aa"), "-", 0, 0, -1);
        assertEquals("aaxaaaayaa", sb.toString());

        sb = new StrBuilder("aaxaaaayaa");
        sb.replace(StrMatcher.stringMatcher("aa"), "-", 0, 2, -1);
        assertEquals("-xaaaayaa", sb.toString());

        sb = new StrBuilder("aaxaaaayaa");
        sb.replace(StrMatcher.stringMatcher("aa"), "-", 0, 3, -1);
        assertEquals("-xaaaayaa", sb.toString());

        sb = new StrBuilder("aaxaaaayaa");
        sb.replace(StrMatcher.stringMatcher("aa"), "-", 0, 4, -1);
        assertEquals("-xaaaayaa", sb.toString());

        sb = new StrBuilder("aaxaaaayaa");
        sb.replace(StrMatcher.stringMatcher("aa"), "-", 0, 5, -1);
        assertEquals("-x-aayaa", sb.toString());

        sb = new StrBuilder("aaxaaaayaa");
        sb.replace(StrMatcher.stringMatcher("aa"), "-", 0, 6, -1);
        assertEquals("-x-aayaa", sb.toString());

        sb = new StrBuilder("aaxaaaayaa");
        sb.replace(StrMatcher.stringMatcher("aa"), "-", 0, 7, -1);
        assertEquals("-x--yaa", sb.toString());

        sb = new StrBuilder("aaxaaaayaa");
        sb.replace(StrMatcher.stringMatcher("aa"), "-", 0, 8, -1);
        assertEquals("-x--yaa", sb.toString());

        sb = new StrBuilder("aaxaaaayaa");
        sb.replace(StrMatcher.stringMatcher("aa"), "-", 0, 9, -1);
        assertEquals("-x--yaa", sb.toString());

        sb = new StrBuilder("aaxaaaayaa");
        sb.replace(StrMatcher.stringMatcher("aa"), "-", 0, 10, -1);
        assertEquals("-x--y-", sb.toString());

        sb = new StrBuilder("aaxaaaayaa");
        sb.replace(StrMatcher.stringMatcher("aa"), "-", 0, 1000, -1);
        assertEquals("-x--y-", sb.toString());

        final StrBuilder sb1 = new StrBuilder("aaxaaaayaa");
        assertThrows(
                IndexOutOfBoundsException.class,
                () -> sb1.replace(StrMatcher.stringMatcher("aa"), "-", 2, 1, -1));
        assertEquals("aaxaaaayaa", sb1.toString());
    }

    @Test
    public void testReplace_StrMatcher_String_int_int_int_VaryCount() {
        StrBuilder sb = new StrBuilder("aaxaaaayaa");
        sb.replace(StrMatcher.stringMatcher("aa"), "-", 0, 10, -1);
        assertEquals("-x--y-", sb.toString());

        sb = new StrBuilder("aaxaaaayaa");
        sb.replace(StrMatcher.stringMatcher("aa"), "-", 0, 10, 0);
        assertEquals("aaxaaaayaa", sb.toString());

        sb = new StrBuilder("aaxaaaayaa");
        sb.replace(StrMatcher.stringMatcher("aa"), "-", 0, 10, 1);
        assertEquals("-xaaaayaa", sb.toString());

        sb = new StrBuilder("aaxaaaayaa");
        sb.replace(StrMatcher.stringMatcher("aa"), "-", 0, 10, 2);
        assertEquals("-x-aayaa", sb.toString());

        sb = new StrBuilder("aaxaaaayaa");
        sb.replace(StrMatcher.stringMatcher("aa"), "-", 0, 10, 3);
        assertEquals("-x--yaa", sb.toString());

        sb = new StrBuilder("aaxaaaayaa");
        sb.replace(StrMatcher.stringMatcher("aa"), "-", 0, 10, 4);
        assertEquals("-x--y-", sb.toString());

        sb = new StrBuilder("aaxaaaayaa");
        sb.replace(StrMatcher.stringMatcher("aa"), "-", 0, 10, 5);
        assertEquals("-x--y-", sb.toString());
    }

    @Test
    public void testReverse() {
        final StrBuilder sb = new StrBuilder();
        assertEquals("", sb.reverse().toString());

        sb.clear().append(true);
        assertEquals("eurt", sb.reverse().toString());
        assertEquals("true", sb.reverse().toString());
    }

    @Test
    public void testTrim() {
        final StrBuilder sb = new StrBuilder();
        assertEquals("", sb.reverse().toString());

        sb.clear().append(" \u0000 ");
        assertEquals("", sb.trim().toString());

        sb.clear().append(" \u0000 a b c");
        assertEquals("a b c", sb.trim().toString());

        sb.clear().append("a b c \u0000 ");
        assertEquals("a b c", sb.trim().toString());

        sb.clear().append(" \u0000 a b c \u0000 ");
        assertEquals("a b c", sb.trim().toString());

        sb.clear().append("a b c");
        assertEquals("a b c", sb.trim().toString());
    }

    @Test
    public void testStartsWith() {
        final StrBuilder sb = new StrBuilder();
        assertFalse(sb.startsWith("a"));
        assertFalse(sb.startsWith(null));
        assertTrue(sb.startsWith(""));
        sb.append("abc");
        assertTrue(sb.startsWith("a"));
        assertTrue(sb.startsWith("ab"));
        assertTrue(sb.startsWith("abc"));
        assertFalse(sb.startsWith("cba"));
    }

    @Test
    public void testEndsWith() {
        final StrBuilder sb = new StrBuilder();
        assertFalse(sb.endsWith("a"));
        assertFalse(sb.endsWith("c"));
        assertTrue(sb.endsWith(""));
        assertFalse(sb.endsWith(null));
        sb.append("abc");
        assertTrue(sb.endsWith("c"));
        assertTrue(sb.endsWith("bc"));
        assertTrue(sb.endsWith("abc"));
        assertFalse(sb.endsWith("cba"));
        assertFalse(sb.endsWith("abcd"));
        assertFalse(sb.endsWith(" abc"));
        assertFalse(sb.endsWith("abc "));
    }

    @Test
    public void testSubSequenceIntInt() {
       final StrBuilder sb = new StrBuilder ("hello goodbye");
       // Start index is negative
        assertThrows(IndexOutOfBoundsException.class, () -> sb.subSequence(-1, 5));

        // End index is negative
        assertThrows(IndexOutOfBoundsException.class, () -> sb.subSequence(2, -1));

        // End index greater than length()
        assertThrows(IndexOutOfBoundsException.class, () -> sb.subSequence(2, sb.length() + 1));

        // Start index greater then end index
        assertThrows(IndexOutOfBoundsException.class, () -> sb.subSequence(3, 2));

        // Normal cases
        assertEquals ("hello", sb.subSequence(0, 5));
        assertEquals ("hello goodbye".subSequence(0, 6), sb.subSequence(0, 6));
        assertEquals ("goodbye", sb.subSequence(6, 13));
        assertEquals ("hello goodbye".subSequence(6, 13), sb.subSequence(6, 13));
    }

    @Test
    public void testSubstringInt() {
        final StrBuilder sb = new StrBuilder ("hello goodbye");
        assertEquals ("goodbye", sb.substring(6));
        assertEquals ("hello goodbye".substring(6), sb.substring(6));
        assertEquals ("hello goodbye", sb.substring(0));
        assertEquals ("hello goodbye".substring(0), sb.substring(0));
        assertThrows(IndexOutOfBoundsException.class, () -> sb.substring(-1));

        assertThrows(IndexOutOfBoundsException.class, () -> sb.substring(15));
    }

    @Test
    public void testSubstringIntInt() {
        final StrBuilder sb = new StrBuilder ("hello goodbye");
        assertEquals ("hello", sb.substring(0, 5));
        assertEquals ("hello goodbye".substring(0, 6), sb.substring(0, 6));

        assertEquals ("goodbye", sb.substring(6, 13));
        assertEquals ("hello goodbye".substring(6, 13), sb.substring(6, 13));

        assertEquals ("goodbye", sb.substring(6, 20));

        assertThrows(IndexOutOfBoundsException.class, () -> sb.substring(-1, 5));
        assertThrows(IndexOutOfBoundsException.class, () -> sb.substring(15, 20));
    }

    @Test
    public void testMidString() {
        final StrBuilder sb = new StrBuilder("hello goodbye hello");
        assertEquals("goodbye", sb.midString(6, 7));
        assertEquals("hello", sb.midString(0, 5));
        assertEquals("hello", sb.midString(-5, 5));
        assertEquals("", sb.midString(0, -1));
        assertEquals("", sb.midString(20, 2));
        assertEquals("hello", sb.midString(14, 22));
    }

    @Test
    public void testRightString() {
        final StrBuilder sb = new StrBuilder("left right");
        assertEquals("right", sb.rightString(5));
        assertEquals("", sb.rightString(0));
        assertEquals("", sb.rightString(-5));
        assertEquals("left right", sb.rightString(15));
    }

    @Test
    public void testLeftString() {
        final StrBuilder sb = new StrBuilder("left right");
        assertEquals("left", sb.leftString(4));
        assertEquals("", sb.leftString(0));
        assertEquals("", sb.leftString(-5));
        assertEquals("left right", sb.leftString(15));
    }

    @Test
    public void testContains_char() {
        final StrBuilder sb = new StrBuilder("abcdefghijklmnopqrstuvwxyz");
        assertTrue(sb.contains('a'));
        assertTrue(sb.contains('o'));
        assertTrue(sb.contains('z'));
        assertFalse(sb.contains('1'));
    }

    @Test
    public void testContains_String() {
        final StrBuilder sb = new StrBuilder("abcdefghijklmnopqrstuvwxyz");
        assertTrue(sb.contains("a"));
        assertTrue(sb.contains("pq"));
        assertTrue(sb.contains("z"));
        assertFalse(sb.contains("zyx"));
        assertFalse(sb.contains((String) null));
    }

    @Test
    public void testContains_StrMatcher() {
        StrBuilder sb = new StrBuilder("abcdefghijklmnopqrstuvwxyz");
        assertTrue(sb.contains(StrMatcher.charMatcher('a')));
        assertTrue(sb.contains(StrMatcher.stringMatcher("pq")));
        assertTrue(sb.contains(StrMatcher.charMatcher('z')));
        assertFalse(sb.contains(StrMatcher.stringMatcher("zy")));
        assertFalse(sb.contains((StrMatcher) null));

        sb = new StrBuilder();
        assertFalse(sb.contains(A_NUMBER_MATCHER));
        sb.append("B A1 C");
        assertTrue(sb.contains(A_NUMBER_MATCHER));
    }

    @Test
    public void testIndexOf_char() {
        final StrBuilder sb = new StrBuilder("abab");
        assertEquals(0, sb.indexOf('a'));

        // should work like String#indexOf
        assertEquals("abab".indexOf('a'), sb.indexOf('a'));

        assertEquals(1, sb.indexOf('b'));
        assertEquals("abab".indexOf('b'), sb.indexOf('b'));

        assertEquals(-1, sb.indexOf('z'));
    }

    @Test
    public void testIndexOf_char_int() {
        StrBuilder sb = new StrBuilder("abab");
        assertEquals(0, sb.indexOf('a', -1));
        assertEquals(0, sb.indexOf('a', 0));
        assertEquals(2, sb.indexOf('a', 1));
        assertEquals(-1, sb.indexOf('a', 4));
        assertEquals(-1, sb.indexOf('a', 5));

        // should work like String#indexOf
        assertEquals("abab".indexOf('a', 1), sb.indexOf('a', 1));

        assertEquals(3, sb.indexOf('b', 2));
        assertEquals("abab".indexOf('b', 2), sb.indexOf('b', 2));

        assertEquals(-1, sb.indexOf('z', 2));

        sb = new StrBuilder("xyzabc");
        assertEquals(2, sb.indexOf('z', 0));
        assertEquals(-1, sb.indexOf('z', 3));
    }

    @Test
    public void testLastIndexOf_char() {
        final StrBuilder sb = new StrBuilder("abab");

        assertEquals (2, sb.lastIndexOf('a'));
        //should work like String#lastIndexOf
        assertEquals ("abab".lastIndexOf('a'), sb.lastIndexOf('a'));

        assertEquals(3, sb.lastIndexOf('b'));
        assertEquals ("abab".lastIndexOf('b'), sb.lastIndexOf('b'));

        assertEquals (-1, sb.lastIndexOf('z'));
    }

    @Test
    public void testLastIndexOf_char_int() {
        StrBuilder sb = new StrBuilder("abab");
        assertEquals(-1, sb.lastIndexOf('a', -1));
        assertEquals(0, sb.lastIndexOf('a', 0));
        assertEquals(0, sb.lastIndexOf('a', 1));

        // should work like String#lastIndexOf
        assertEquals("abab".lastIndexOf('a', 1), sb.lastIndexOf('a', 1));

        assertEquals(1, sb.lastIndexOf('b', 2));
        assertEquals("abab".lastIndexOf('b', 2), sb.lastIndexOf('b', 2));

        assertEquals(-1, sb.lastIndexOf('z', 2));

        sb = new StrBuilder("xyzabc");
        assertEquals(2, sb.lastIndexOf('z', sb.length()));
        assertEquals(-1, sb.lastIndexOf('z', 1));
    }

    @Test
    public void testIndexOf_String() {
        final StrBuilder sb = new StrBuilder("abab");

        assertEquals(0, sb.indexOf("a"));
        //should work like String#indexOf
        assertEquals("abab".indexOf("a"), sb.indexOf("a"));

        assertEquals(0, sb.indexOf("ab"));
        //should work like String#indexOf
        assertEquals("abab".indexOf("ab"), sb.indexOf("ab"));

        assertEquals(1, sb.indexOf("b"));
        assertEquals("abab".indexOf("b"), sb.indexOf("b"));

        assertEquals(1, sb.indexOf("ba"));
        assertEquals("abab".indexOf("ba"), sb.indexOf("ba"));

        assertEquals(-1, sb.indexOf("z"));

        assertEquals(-1, sb.indexOf((String) null));
    }

    @Test
    public void testIndexOf_String_int() {
        StrBuilder sb = new StrBuilder("abab");
        assertEquals(0, sb.indexOf("a", -1));
        assertEquals(0, sb.indexOf("a", 0));
        assertEquals(2, sb.indexOf("a", 1));
        assertEquals(2, sb.indexOf("a", 2));
        assertEquals(-1, sb.indexOf("a", 3));
        assertEquals(-1, sb.indexOf("a", 4));
        assertEquals(-1, sb.indexOf("a", 5));

        assertEquals(-1, sb.indexOf("abcdef", 0));
        assertEquals(0, sb.indexOf("", 0));
        assertEquals(1, sb.indexOf("", 1));

        //should work like String#indexOf
        assertEquals ("abab".indexOf("a", 1), sb.indexOf("a", 1));

        assertEquals(2, sb.indexOf("ab", 1));
        //should work like String#indexOf
        assertEquals("abab".indexOf("ab", 1), sb.indexOf("ab", 1));

        assertEquals(3, sb.indexOf("b", 2));
        assertEquals("abab".indexOf("b", 2), sb.indexOf("b", 2));

        assertEquals(1, sb.indexOf("ba", 1));
        assertEquals("abab".indexOf("ba", 2), sb.indexOf("ba", 2));

        assertEquals(-1, sb.indexOf("z", 2));

        sb = new StrBuilder("xyzabc");
        assertEquals(2, sb.indexOf("za", 0));
        assertEquals(-1, sb.indexOf("za", 3));

        assertEquals(-1, sb.indexOf((String) null, 2));
    }

    @Test
    public void testLastIndexOf_String() {
        final StrBuilder sb = new StrBuilder("abab");

        assertEquals(2, sb.lastIndexOf("a"));
        //should work like String#lastIndexOf
        assertEquals("abab".lastIndexOf("a"), sb.lastIndexOf("a"));

        assertEquals(2, sb.lastIndexOf("ab"));
        //should work like String#lastIndexOf
        assertEquals("abab".lastIndexOf("ab"), sb.lastIndexOf("ab"));

        assertEquals(3, sb.lastIndexOf("b"));
        assertEquals("abab".lastIndexOf("b"), sb.lastIndexOf("b"));

        assertEquals(1, sb.lastIndexOf("ba"));
        assertEquals("abab".lastIndexOf("ba"), sb.lastIndexOf("ba"));

        assertEquals(-1, sb.lastIndexOf("z"));

        assertEquals(-1, sb.lastIndexOf((String) null));
    }

    @Test
    public void testLastIndexOf_String_int() {
        StrBuilder sb = new StrBuilder("abab");
        assertEquals(-1, sb.lastIndexOf("a", -1));
        assertEquals(0, sb.lastIndexOf("a", 0));
        assertEquals(0, sb.lastIndexOf("a", 1));
        assertEquals(2, sb.lastIndexOf("a", 2));
        assertEquals(2, sb.lastIndexOf("a", 3));
        assertEquals(2, sb.lastIndexOf("a", 4));
        assertEquals(2, sb.lastIndexOf("a", 5));

        assertEquals(-1, sb.lastIndexOf("abcdef", 3));
        assertEquals("abab".lastIndexOf("", 3), sb.lastIndexOf("", 3));
        assertEquals("abab".lastIndexOf("", 1), sb.lastIndexOf("", 1));

        //should work like String#lastIndexOf
        assertEquals("abab".lastIndexOf("a", 1), sb.lastIndexOf("a", 1));

        assertEquals(0, sb.lastIndexOf("ab", 1));
        //should work like String#lastIndexOf
        assertEquals("abab".lastIndexOf("ab", 1), sb.lastIndexOf("ab", 1));

        assertEquals(1, sb.lastIndexOf("b", 2));
        assertEquals("abab".lastIndexOf("b", 2), sb.lastIndexOf("b", 2));

        assertEquals(1, sb.lastIndexOf("ba", 2));
        assertEquals("abab".lastIndexOf("ba", 2), sb.lastIndexOf("ba", 2));

        assertEquals(-1, sb.lastIndexOf("z", 2));

        sb = new StrBuilder("xyzabc");
        assertEquals(2, sb.lastIndexOf("za", sb.length()));
        assertEquals(-1, sb.lastIndexOf("za", 1));

        assertEquals(-1, sb.lastIndexOf((String) null, 2));
    }

    @Test
    public void testIndexOf_StrMatcher() {
        final StrBuilder sb = new StrBuilder();
        assertEquals(-1, sb.indexOf((StrMatcher) null));
        assertEquals(-1, sb.indexOf(StrMatcher.charMatcher('a')));

        sb.append("ab bd");
        assertEquals(0, sb.indexOf(StrMatcher.charMatcher('a')));
        assertEquals(1, sb.indexOf(StrMatcher.charMatcher('b')));
        assertEquals(2, sb.indexOf(StrMatcher.spaceMatcher()));
        assertEquals(4, sb.indexOf(StrMatcher.charMatcher('d')));
        assertEquals(-1, sb.indexOf(StrMatcher.noneMatcher()));
        assertEquals(-1, sb.indexOf((StrMatcher) null));

        sb.append(" A1 junction");
        assertEquals(6, sb.indexOf(A_NUMBER_MATCHER));
    }

    @Test
    public void testIndexOf_StrMatcher_int() {
        final StrBuilder sb = new StrBuilder();
        assertEquals(-1, sb.indexOf((StrMatcher) null, 2));
        assertEquals(-1, sb.indexOf(StrMatcher.charMatcher('a'), 2));
        assertEquals(-1, sb.indexOf(StrMatcher.charMatcher('a'), 0));

        sb.append("ab bd");
        assertEquals(0, sb.indexOf(StrMatcher.charMatcher('a'), -2));
        assertEquals(0, sb.indexOf(StrMatcher.charMatcher('a'), 0));
        assertEquals(-1, sb.indexOf(StrMatcher.charMatcher('a'), 2));
        assertEquals(-1, sb.indexOf(StrMatcher.charMatcher('a'), 20));

        assertEquals(1, sb.indexOf(StrMatcher.charMatcher('b'), -1));
        assertEquals(1, sb.indexOf(StrMatcher.charMatcher('b'), 0));
        assertEquals(1, sb.indexOf(StrMatcher.charMatcher('b'), 1));
        assertEquals(3, sb.indexOf(StrMatcher.charMatcher('b'), 2));
        assertEquals(3, sb.indexOf(StrMatcher.charMatcher('b'), 3));
        assertEquals(-1, sb.indexOf(StrMatcher.charMatcher('b'), 4));
        assertEquals(-1, sb.indexOf(StrMatcher.charMatcher('b'), 5));
        assertEquals(-1, sb.indexOf(StrMatcher.charMatcher('b'), 6));

        assertEquals(2, sb.indexOf(StrMatcher.spaceMatcher(), -2));
        assertEquals(2, sb.indexOf(StrMatcher.spaceMatcher(), 0));
        assertEquals(2, sb.indexOf(StrMatcher.spaceMatcher(), 2));
        assertEquals(-1, sb.indexOf(StrMatcher.spaceMatcher(), 4));
        assertEquals(-1, sb.indexOf(StrMatcher.spaceMatcher(), 20));

        assertEquals(-1, sb.indexOf(StrMatcher.noneMatcher(), 0));
        assertEquals(-1, sb.indexOf((StrMatcher) null, 0));

        sb.append(" A1 junction with A2");
        assertEquals(6, sb.indexOf(A_NUMBER_MATCHER, 5));
        assertEquals(6, sb.indexOf(A_NUMBER_MATCHER, 6));
        assertEquals(23, sb.indexOf(A_NUMBER_MATCHER, 7));
        assertEquals(23, sb.indexOf(A_NUMBER_MATCHER, 22));
        assertEquals(23, sb.indexOf(A_NUMBER_MATCHER, 23));
        assertEquals(-1, sb.indexOf(A_NUMBER_MATCHER, 24));
    }

    @Test
    public void testLastIndexOf_StrMatcher() {
        final StrBuilder sb = new StrBuilder();
        assertEquals(-1, sb.lastIndexOf((StrMatcher) null));
        assertEquals(-1, sb.lastIndexOf(StrMatcher.charMatcher('a')));

        sb.append("ab bd");
        assertEquals(0, sb.lastIndexOf(StrMatcher.charMatcher('a')));
        assertEquals(3, sb.lastIndexOf(StrMatcher.charMatcher('b')));
        assertEquals(2, sb.lastIndexOf(StrMatcher.spaceMatcher()));
        assertEquals(4, sb.lastIndexOf(StrMatcher.charMatcher('d')));
        assertEquals(-1, sb.lastIndexOf(StrMatcher.noneMatcher()));
        assertEquals(-1, sb.lastIndexOf((StrMatcher) null));

        sb.append(" A1 junction");
        assertEquals(6, sb.lastIndexOf(A_NUMBER_MATCHER));
    }

    @Test
    public void testLastIndexOf_StrMatcher_int() {
        final StrBuilder sb = new StrBuilder();
        assertEquals(-1, sb.lastIndexOf((StrMatcher) null, 2));
        assertEquals(-1, sb.lastIndexOf(StrMatcher.charMatcher('a'), 2));
        assertEquals(-1, sb.lastIndexOf(StrMatcher.charMatcher('a'), 0));
        assertEquals(-1, sb.lastIndexOf(StrMatcher.charMatcher('a'), -1));

        sb.append("ab bd");
        assertEquals(-1, sb.lastIndexOf(StrMatcher.charMatcher('a'), -2));
        assertEquals(0, sb.lastIndexOf(StrMatcher.charMatcher('a'), 0));
        assertEquals(0, sb.lastIndexOf(StrMatcher.charMatcher('a'), 2));
        assertEquals(0, sb.lastIndexOf(StrMatcher.charMatcher('a'), 20));

        assertEquals(-1, sb.lastIndexOf(StrMatcher.charMatcher('b'), -1));
        assertEquals(-1, sb.lastIndexOf(StrMatcher.charMatcher('b'), 0));
        assertEquals(1, sb.lastIndexOf(StrMatcher.charMatcher('b'), 1));
        assertEquals(1, sb.lastIndexOf(StrMatcher.charMatcher('b'), 2));
        assertEquals(3, sb.lastIndexOf(StrMatcher.charMatcher('b'), 3));
        assertEquals(3, sb.lastIndexOf(StrMatcher.charMatcher('b'), 4));
        assertEquals(3, sb.lastIndexOf(StrMatcher.charMatcher('b'), 5));
        assertEquals(3, sb.lastIndexOf(StrMatcher.charMatcher('b'), 6));

        assertEquals(-1, sb.lastIndexOf(StrMatcher.spaceMatcher(), -2));
        assertEquals(-1, sb.lastIndexOf(StrMatcher.spaceMatcher(), 0));
        assertEquals(2, sb.lastIndexOf(StrMatcher.spaceMatcher(), 2));
        assertEquals(2, sb.lastIndexOf(StrMatcher.spaceMatcher(), 4));
        assertEquals(2, sb.lastIndexOf(StrMatcher.spaceMatcher(), 20));

        assertEquals(-1, sb.lastIndexOf(StrMatcher.noneMatcher(), 0));
        assertEquals(-1, sb.lastIndexOf((StrMatcher) null, 0));

        sb.append(" A1 junction with A2");
        assertEquals(-1, sb.lastIndexOf(A_NUMBER_MATCHER, 5));
        assertEquals(-1, sb.lastIndexOf(A_NUMBER_MATCHER, 6)); // A matches, 1 is outside bounds
        assertEquals(6, sb.lastIndexOf(A_NUMBER_MATCHER, 7));
        assertEquals(6, sb.lastIndexOf(A_NUMBER_MATCHER, 22));
        assertEquals(6, sb.lastIndexOf(A_NUMBER_MATCHER, 23)); // A matches, 2 is outside bounds
        assertEquals(23, sb.lastIndexOf(A_NUMBER_MATCHER, 24));
    }

    static final StrMatcher A_NUMBER_MATCHER = new StrMatcher() {
        @Override
        public int isMatch(final char[] buffer, int pos, final int bufferStart, final int bufferEnd) {
            if (buffer[pos] == 'A') {
                pos++;
                if (pos < bufferEnd && buffer[pos] >= '0' && buffer[pos] <= '9') {
                    return 2;
                }
            }
            return 0;
        }
    };

    @Test
    public void testAsTokenizer() {
        // from Javadoc
        final StrBuilder b = new StrBuilder();
        b.append("a b ");
        final StrTokenizer t = b.asTokenizer();

        final String[] tokens1 = t.getTokenArray();
        assertEquals(2, tokens1.length);
        assertEquals("a", tokens1[0]);
        assertEquals("b", tokens1[1]);
        assertEquals(2, t.size());

        b.append("c d ");
        final String[] tokens2 = t.getTokenArray();
        assertEquals(2, tokens2.length);
        assertEquals("a", tokens2[0]);
        assertEquals("b", tokens2[1]);
        assertEquals(2, t.size());
        assertEquals("a", t.next());
        assertEquals("b", t.next());

        t.reset();
        final String[] tokens3 = t.getTokenArray();
        assertEquals(4, tokens3.length);
        assertEquals("a", tokens3[0]);
        assertEquals("b", tokens3[1]);
        assertEquals("c", tokens3[2]);
        assertEquals("d", tokens3[3]);
        assertEquals(4, t.size());
        assertEquals("a", t.next());
        assertEquals("b", t.next());
        assertEquals("c", t.next());
        assertEquals("d", t.next());

        assertEquals("a b c d ", t.getContent());
    }

    @Test
    public void testAsReader() throws Exception {
        final StrBuilder sb = new StrBuilder("some text");
        Reader reader = sb.asReader();
        assertTrue(reader.ready());
        final char[] buf = new char[40];
        assertEquals(9, reader.read(buf));
        assertEquals("some text", new String(buf, 0, 9));

        assertEquals(-1, reader.read());
        assertFalse(reader.ready());
        assertEquals(0, reader.skip(2));
        assertEquals(0, reader.skip(-1));

        assertTrue(reader.markSupported());
        reader = sb.asReader();
        assertEquals('s', reader.read());
        reader.mark(-1);
        char[] array = new char[3];
        assertEquals(3, reader.read(array, 0, 3));
        assertEquals('o', array[0]);
        assertEquals('m', array[1]);
        assertEquals('e', array[2]);
        reader.reset();
        assertEquals(1, reader.read(array, 1, 1));
        assertEquals('o', array[0]);
        assertEquals('o', array[1]);
        assertEquals('e', array[2]);
        assertEquals(2, reader.skip(2));
        assertEquals(' ', reader.read());

        assertTrue(reader.ready());
        reader.close();
        assertTrue(reader.ready());

        try (Reader r = sb.asReader()) {
            final char[] arr = new char[3];
            assertThrows(IndexOutOfBoundsException.class, () -> r.read(arr, -1, 0));
            assertThrows(IndexOutOfBoundsException.class, () -> r.read(arr, 0, -1));
            assertThrows(IndexOutOfBoundsException.class, () -> r.read(arr, 100, 1));
            assertThrows(IndexOutOfBoundsException.class, () -> r.read(arr, 0, 100));
            assertThrows(IndexOutOfBoundsException.class, () -> r.read(arr, Integer.MAX_VALUE, Integer.MAX_VALUE));

            assertEquals(0, r.read(arr, 0, 0));
            assertEquals(0, arr[0]);
            assertEquals(0, arr[1]);
            assertEquals(0, arr[2]);

            r.skip(9);
            assertEquals(-1, r.read(arr, 0, 1));

            r.reset();
            array = new char[30];
            assertEquals(9, r.read(array, 0, 30));
        }
    }

    @Test
    public void testAsWriter() throws Exception {
        final StrBuilder sb = new StrBuilder("base");
        try (Writer writer = sb.asWriter()) {

            writer.write('l');
            assertEquals("basel", sb.toString());

            writer.write(new char[] { 'i', 'n' });
            assertEquals("baselin", sb.toString());

            writer.write(new char[] { 'n', 'e', 'r' }, 1, 2);
            assertEquals("baseliner", sb.toString());

            writer.write(" rout");
            assertEquals("baseliner rout", sb.toString());

            writer.write("ping that server", 1, 3);
            assertEquals("baseliner routing", sb.toString());

            writer.flush(); // no effect
            assertEquals("baseliner routing", sb.toString());

            writer.close(); // no effect
            assertEquals("baseliner routing", sb.toString());

            writer.write(" hi"); // works after close
            assertEquals("baseliner routing hi", sb.toString());

            sb.setLength(4); // mix and match
            writer.write('d');
            assertEquals("based", sb.toString());
        }
    }

    @Test
    public void testEqualsIgnoreCase() {
        final StrBuilder sb1 = new StrBuilder();
        final StrBuilder sb2 = new StrBuilder();
        assertTrue(sb1.equalsIgnoreCase(sb1));
        assertTrue(sb1.equalsIgnoreCase(sb2));
        assertTrue(sb2.equalsIgnoreCase(sb2));

        sb1.append("abc");
        assertFalse(sb1.equalsIgnoreCase(sb2));

        sb2.append("ABC");
        assertTrue(sb1.equalsIgnoreCase(sb2));

        sb2.clear().append("abc");
        assertTrue(sb1.equalsIgnoreCase(sb2));
        assertTrue(sb1.equalsIgnoreCase(sb1));
        assertTrue(sb2.equalsIgnoreCase(sb2));

        sb2.clear().append("aBc");
        assertTrue(sb1.equalsIgnoreCase(sb2));
    }

    @Test
    public void testEquals() {
        final StrBuilder sb1 = new StrBuilder();
        final StrBuilder sb2 = new StrBuilder();
        assertTrue(sb1.equals(sb2));
        assertTrue(sb1.equals(sb1));
        assertTrue(sb2.equals(sb2));
        assertEquals(sb1, (Object) sb2);

        sb1.append("abc");
        assertFalse(sb1.equals(sb2));
        assertNotEquals(sb1, (Object) sb2);

        sb2.append("ABC");
        assertFalse(sb1.equals(sb2));
        assertNotEquals(sb1, (Object) sb2);

        sb2.clear().append("abc");
        assertTrue(sb1.equals(sb2));
        assertEquals(sb1, (Object) sb2);

        assertNotEquals(sb1, Integer.valueOf(1));
        assertNotEquals("abc", sb1);
    }

    @Test
    public void test_LANG_1131_EqualsWithNullStrBuilder() {
        final StrBuilder sb = new StrBuilder();
        final StrBuilder other = null;
        assertFalse(sb.equals(other));
    }

    @Test
    public void testHashCode() {
        final StrBuilder sb = new StrBuilder();
        final int hc1a = sb.hashCode();
        final int hc1b = sb.hashCode();
        assertEquals(0, hc1a);
        assertEquals(hc1a, hc1b);

        sb.append("abc");
        final int hc2a = sb.hashCode();
        final int hc2b = sb.hashCode();
        assertTrue(hc2a != 0);
        assertEquals(hc2a, hc2b);
    }

    @Test
    public void testToString() {
        final StrBuilder sb = new StrBuilder("abc");
        assertEquals("abc", sb.toString());
    }

    @Test
    public void testToStringBuffer() {
        final StrBuilder sb = new StrBuilder();
        assertEquals(new StringBuffer().toString(), sb.toStringBuffer().toString());

        sb.append("junit");
        assertEquals(new StringBuffer("junit").toString(), sb.toStringBuffer().toString());
    }

    @Test
    public void testToStringBuilder() {
        final StrBuilder sb = new StrBuilder();
        assertEquals(new StringBuilder().toString(), sb.toStringBuilder().toString());

        sb.append("junit");
        assertEquals(new StringBuilder("junit").toString(), sb.toStringBuilder().toString());
    }

    @Test
    public void testLang294() {
        final StrBuilder sb = new StrBuilder("\n%BLAH%\nDo more stuff\neven more stuff\n%BLAH%\n");
        sb.deleteAll("\n%BLAH%");
        assertEquals("\nDo more stuff\neven more stuff\n", sb.toString());
    }

    @Test
    public void testIndexOfLang294() {
        final StrBuilder sb = new StrBuilder("onetwothree");
        sb.deleteFirst("three");
        assertEquals(-1, sb.indexOf("three"));
    }

    @Test
    public void testLang295() {
        final StrBuilder sb = new StrBuilder("onetwothree");
        sb.deleteFirst("three");
        assertFalse(sb.contains('h'), "The contains(char) method is looking beyond the end of the string");
        assertEquals(-1, sb.indexOf('h'), "The indexOf(char) method is looking beyond the end of the string");
    }

    @Test
    public void testLang412Right() {
        final StrBuilder sb = new StrBuilder();
        sb.appendFixedWidthPadRight(null, 10, '*');
        assertEquals("**********", sb.toString(), "Failed to invoke appendFixedWidthPadRight correctly");
    }

    @Test
    public void testLang412Left() {
        final StrBuilder sb = new StrBuilder();
        sb.appendFixedWidthPadLeft(null, 10, '*');
        assertEquals("**********", sb.toString(), "Failed to invoke appendFixedWidthPadLeft correctly");
    }

    @Test
    public void testAsBuilder() {
        final StrBuilder sb = new StrBuilder().appendAll("Lorem", " ", "ipsum", " ", "dolor");
        assertEquals(sb.toString(), sb.build());
    }

    @Test
    public void testAppendCharBuffer() {
        final StrBuilder sb1 = new StrBuilder();
        final CharBuffer buf = CharBuffer.allocate(10);
        buf.append("0123456789");
        buf.flip();
        sb1.append(buf);
        assertEquals("0123456789", sb1.toString());

        final StrBuilder sb2 = new StrBuilder();
        sb2.append(buf, 1, 8);
        assertEquals("12345678", sb2.toString());
    }

    @Test
    public void testAppendToWriter() throws Exception {
        final StrBuilder sb = new StrBuilder("1234567890");
        final StringWriter writer = new StringWriter();
        writer.append("Test ");

        sb.appendTo(writer);

        assertEquals("Test 1234567890", writer.toString());
    }

    @Test
    public void testAppendToStringBuilder() throws Exception {
        final StrBuilder sb = new StrBuilder("1234567890");
        final StringBuilder builder = new StringBuilder("Test ");

        sb.appendTo(builder);

        assertEquals("Test 1234567890", builder.toString());
    }

    @Test
    public void testAppendToStringBuffer() throws Exception {
        final StrBuilder sb = new StrBuilder("1234567890");
        final StringBuffer buffer = new StringBuffer("Test ");

        sb.appendTo(buffer);

        assertEquals("Test 1234567890", buffer.toString());
    }

    @Test
    public void testAppendToCharBuffer() throws Exception {
        final StrBuilder sb = new StrBuilder("1234567890");
        final String text = "Test ";
        final CharBuffer buffer = CharBuffer.allocate(sb.size() + text.length());
        buffer.put(text);

        sb.appendTo(buffer);

        buffer.flip();
        assertEquals("Test 1234567890", buffer.toString());
    }
}
