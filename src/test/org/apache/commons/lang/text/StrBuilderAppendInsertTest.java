/*
 * Copyright 2005 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.commons.lang.text;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;

import org.apache.commons.lang.SystemUtils;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Unit tests for {@link org.apache.commons.lang.text.StrBuilder}.
 *
 * @version $Id$
 */
public class StrBuilderAppendInsertTest extends TestCase {

    /** Test subclass of Object, with a toString method. */
    private static Object FOO = new Object() {
        public String toString() {
            return "foo";
        }
    };

    /**
     * Main method.
     * 
     * @param args  command line arguments, ignored
     */
    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    /**
     * Return a new test suite containing this test case.
     * 
     * @return a new test suite containing this test case
     */
    public static Test suite() {
        TestSuite suite = new TestSuite(StrBuilderAppendInsertTest.class);
        suite.setName("StrBuilder Tests");
        return suite;
    }

    /**
     * Create a new test case with the specified name.
     * 
     * @param name  the name
     */
    public StrBuilderAppendInsertTest(String name) {
        super(name);
    }

    //-----------------------------------------------------------------------
    public void testAppendNewLine() {
        StrBuilder sb = new StrBuilder("---");
        sb.appendNewLine().append("+++");
        assertEquals("---" + SystemUtils.LINE_SEPARATOR + "+++", sb.toString());
        
        sb = new StrBuilder("---");
        sb.setNewLineText("#").appendNewLine().setNewLineText(null).appendNewLine();
        assertEquals("---#" + SystemUtils.LINE_SEPARATOR, sb.toString());
    }

    //-----------------------------------------------------------------------
    public void testAppendWithNullText() {
        StrBuilder sb = new StrBuilder();
        sb.setNullText("NULL");
        assertEquals("", sb.toString());

        sb.appendNull();
        assertEquals("NULL", sb.toString());

        sb.append((Object) null);
        assertEquals("NULLNULL", sb.toString());

        sb.append(FOO);
        assertEquals("NULLNULLfoo", sb.toString());

        sb.append((String) null);
        assertEquals("NULLNULLfooNULL", sb.toString());

        sb.append("");
        assertEquals("NULLNULLfooNULL", sb.toString());

        sb.append("bar");
        assertEquals("NULLNULLfooNULLbar", sb.toString());

        sb.append((StringBuffer) null);
        assertEquals("NULLNULLfooNULLbarNULL", sb.toString());

        sb.append(new StringBuffer("baz"));
        assertEquals("NULLNULLfooNULLbarNULLbaz", sb.toString());
    }

    //-----------------------------------------------------------------------
    public void testAppend_Object() {
        StrBuilder sb = new StrBuilder();
        sb.appendNull();
        assertEquals("", sb.toString());

        sb.append((Object) null);
        assertEquals("", sb.toString());

        sb.append(FOO);
        assertEquals("foo", sb.toString());

        sb.append((StringBuffer) null);
        assertEquals("foo", sb.toString());

        sb.append(new StringBuffer("baz"));
        assertEquals("foobaz", sb.toString());

        sb.append(new StrBuilder("yes"));
        assertEquals("foobazyes", sb.toString());
    }

    //-----------------------------------------------------------------------
    public void testAppend_String() {
        StrBuilder sb = new StrBuilder();
        sb.setNullText("NULL").append((String) null);
        assertEquals("NULL", sb.toString());

        sb = new StrBuilder();
        sb.append("foo");
        assertEquals("foo", sb.toString());

        sb.append("");
        assertEquals("foo", sb.toString());

        sb.append("bar");
        assertEquals("foobar", sb.toString());
    }

    //-----------------------------------------------------------------------
    public void testAppend_String_int_int() {
        StrBuilder sb = new StrBuilder();
        sb.setNullText("NULL").append((String) null, 0, 1);
        assertEquals("NULL", sb.toString());

        sb = new StrBuilder();
        sb.append("foo", 0, 3);
        assertEquals("foo", sb.toString());

        try {
            sb.append("bar", -1, 1);
            fail("append(char[], -1,) expected IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.append("bar", 3, 1);
            fail("append(char[], 3,) expected IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.append("bar", 1, -1);
            fail("append(char[],, -1) expected IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.append("bar", 1, 3);
            fail("append(char[], 1, 3) expected IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.append("bar", -1, 3);
            fail("append(char[], -1, 3) expected IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.append("bar", 4, 0);
            fail("append(char[], 4, 0) expected IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        sb.append("bar", 3, 0);
        assertEquals("foo", sb.toString());

        sb.append("abcbardef", 3, 3);
        assertEquals("foobar", sb.toString());
    }

    //-----------------------------------------------------------------------
    public void testAppend_StringBuffer() {
        StrBuilder sb = new StrBuilder();
        sb.setNullText("NULL").append((StringBuffer) null);
        assertEquals("NULL", sb.toString());

        sb = new StrBuilder();
        sb.append(new StringBuffer("foo"));
        assertEquals("foo", sb.toString());

        sb.append(new StringBuffer(""));
        assertEquals("foo", sb.toString());

        sb.append(new StringBuffer("bar"));
        assertEquals("foobar", sb.toString());
    }

    //-----------------------------------------------------------------------
    public void testAppend_StringBuffer_int_int() {
        StrBuilder sb = new StrBuilder();
        sb.setNullText("NULL").append((StringBuffer) null, 0, 1);
        assertEquals("NULL", sb.toString());

        sb = new StrBuilder();
        sb.append(new StringBuffer("foo"), 0, 3);
        assertEquals("foo", sb.toString());

        try {
            sb.append(new StringBuffer("bar"), -1, 1);
            fail("append(char[], -1,) expected IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.append(new StringBuffer("bar"), 3, 1);
            fail("append(char[], 3,) expected IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.append(new StringBuffer("bar"), 1, -1);
            fail("append(char[],, -1) expected IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.append(new StringBuffer("bar"), 1, 3);
            fail("append(char[], 1, 3) expected IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.append(new StringBuffer("bar"), -1, 3);
            fail("append(char[], -1, 3) expected IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.append(new StringBuffer("bar"), 4, 0);
            fail("append(char[], 4, 0) expected IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        sb.append(new StringBuffer("bar"), 3, 0);
        assertEquals("foo", sb.toString());

        sb.append(new StringBuffer("abcbardef"), 3, 3);
        assertEquals("foobar", sb.toString());
    }

    //-----------------------------------------------------------------------
    public void testAppend_StrBuilder() {
        StrBuilder sb = new StrBuilder();
        sb.setNullText("NULL").append((StrBuilder) null);
        assertEquals("NULL", sb.toString());

        sb = new StrBuilder();
        sb.append(new StrBuilder("foo"));
        assertEquals("foo", sb.toString());

        sb.append(new StrBuilder(""));
        assertEquals("foo", sb.toString());

        sb.append(new StrBuilder("bar"));
        assertEquals("foobar", sb.toString());
    }

    //-----------------------------------------------------------------------
    public void testAppend_StrBuilder_int_int() {
        StrBuilder sb = new StrBuilder();
        sb.setNullText("NULL").append((StrBuilder) null, 0, 1);
        assertEquals("NULL", sb.toString());

        sb = new StrBuilder();
        sb.append(new StrBuilder("foo"), 0, 3);
        assertEquals("foo", sb.toString());

        try {
            sb.append(new StrBuilder("bar"), -1, 1);
            fail("append(char[], -1,) expected IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.append(new StrBuilder("bar"), 3, 1);
            fail("append(char[], 3,) expected IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.append(new StrBuilder("bar"), 1, -1);
            fail("append(char[],, -1) expected IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.append(new StrBuilder("bar"), 1, 3);
            fail("append(char[], 1, 3) expected IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.append(new StrBuilder("bar"), -1, 3);
            fail("append(char[], -1, 3) expected IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.append(new StrBuilder("bar"), 4, 0);
            fail("append(char[], 4, 0) expected IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        sb.append(new StrBuilder("bar"), 3, 0);
        assertEquals("foo", sb.toString());

        sb.append(new StrBuilder("abcbardef"), 3, 3);
        assertEquals("foobar", sb.toString());
    }

    //-----------------------------------------------------------------------
    public void testAppend_CharArray() {
        StrBuilder sb = new StrBuilder();
        sb.setNullText("NULL").append((char[]) null);
        assertEquals("NULL", sb.toString());

        sb = new StrBuilder();
        sb.append(new char[0]);
        assertEquals("", sb.toString());

        sb.append(new char[]{'f', 'o', 'o'});
        assertEquals("foo", sb.toString());
    }

    //-----------------------------------------------------------------------
    public void testAppend_CharArray_int_int() {
        StrBuilder sb = new StrBuilder();
        sb.setNullText("NULL").append((char[]) null, 0, 1);
        assertEquals("NULL", sb.toString());

        sb = new StrBuilder();
        sb.append(new char[]{'f', 'o', 'o'}, 0, 3);
        assertEquals("foo", sb.toString());

        try {
            sb.append(new char[]{'b', 'a', 'r'}, -1, 1);
            fail("append(char[], -1,) expected IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.append(new char[]{'b', 'a', 'r'}, 3, 1);
            fail("append(char[], 3,) expected IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.append(new char[]{'b', 'a', 'r'}, 1, -1);
            fail("append(char[],, -1) expected IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.append(new char[]{'b', 'a', 'r'}, 1, 3);
            fail("append(char[], 1, 3) expected IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.append(new char[]{'b', 'a', 'r'}, -1, 3);
            fail("append(char[], -1, 3) expected IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.append(new char[]{'b', 'a', 'r'}, 4, 0);
            fail("append(char[], 4, 0) expected IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        sb.append(new char[]{'b', 'a', 'r'}, 3, 0);
        assertEquals("foo", sb.toString());

        sb.append(new char[]{'a', 'b', 'c', 'b', 'a', 'r', 'd', 'e', 'f'}, 3, 3);
        assertEquals("foobar", sb.toString());
    }

    //-----------------------------------------------------------------------
    public void testAppend_Primitive() {
        StrBuilder sb = new StrBuilder();
        sb.append(true);
        assertEquals("true", sb.toString());

        sb.append(false);
        assertEquals("truefalse", sb.toString());

        sb.append('!');
        assertEquals("truefalse!", sb.toString());
    }

    //-----------------------------------------------------------------------
    public void testAppend_PrimitiveNumber() {
        StrBuilder sb = new StrBuilder();
        sb.append(0);
        assertEquals("0", sb.toString());

        sb.append(1L);
        assertEquals("01", sb.toString());

        sb.append(2.3f);
        assertEquals("012.3", sb.toString());

        sb.append(4.5d);
        assertEquals("012.34.5", sb.toString());
    }

    //-----------------------------------------------------------------------
    public void testAppendPadding() {
        StrBuilder sb = new StrBuilder();
        sb.append("foo");
        assertEquals("foo", sb.toString());

        sb.appendPadding(-1, '-');
        assertEquals("foo", sb.toString());

        sb.appendPadding(0, '-');
        assertEquals("foo", sb.toString());

        sb.appendPadding(1, '-');
        assertEquals("foo-", sb.toString());

        sb.appendPadding(16, '-');
        assertEquals(20, sb.length());
        //            12345678901234567890
        assertEquals("foo-----------------", sb.toString());
    }

    //-----------------------------------------------------------------------
    public void testAppendFixedWidthPadLeft() {
        StrBuilder sb = new StrBuilder();
        sb.appendFixedWidthPadLeft("foo", -1, '-');
        assertEquals("", sb.toString());

        sb.clear();
        sb.appendFixedWidthPadLeft("foo", 0, '-');
        assertEquals("", sb.toString());

        sb.clear();
        sb.appendFixedWidthPadLeft("foo", 1, '-');
        assertEquals("o", sb.toString());

        sb.clear();
        sb.appendFixedWidthPadLeft("foo", 2, '-');
        assertEquals("oo", sb.toString());

        sb.clear();
        sb.appendFixedWidthPadLeft("foo", 3, '-');
        assertEquals("foo", sb.toString());

        sb.clear();
        sb.appendFixedWidthPadLeft("foo", 4, '-');
        assertEquals("-foo", sb.toString());

        sb.clear();
        sb.appendFixedWidthPadLeft("foo", 10, '-');
        assertEquals(10, sb.length());
        //            1234567890
        assertEquals("-------foo", sb.toString());

        sb.clear();
        sb.setNullText("null");
        sb.appendFixedWidthPadLeft(null, 5, '-');
        assertEquals("-null", sb.toString());
    }

    //-----------------------------------------------------------------------
    public void testAppendFixedWidthPadLeft_int() {
        StrBuilder sb = new StrBuilder();
        sb.appendFixedWidthPadLeft(123, -1, '-');
        assertEquals("", sb.toString());

        sb.clear();
        sb.appendFixedWidthPadLeft(123, 0, '-');
        assertEquals("", sb.toString());

        sb.clear();
        sb.appendFixedWidthPadLeft(123, 1, '-');
        assertEquals("3", sb.toString());

        sb.clear();
        sb.appendFixedWidthPadLeft(123, 2, '-');
        assertEquals("23", sb.toString());

        sb.clear();
        sb.appendFixedWidthPadLeft(123, 3, '-');
        assertEquals("123", sb.toString());

        sb.clear();
        sb.appendFixedWidthPadLeft(123, 4, '-');
        assertEquals("-123", sb.toString());

        sb.clear();
        sb.appendFixedWidthPadLeft(123, 10, '-');
        assertEquals(10, sb.length());
        //            1234567890
        assertEquals("-------123", sb.toString());
    }

    //-----------------------------------------------------------------------
    public void testAppendFixedWidthPadRight() {
        StrBuilder sb = new StrBuilder();
        sb.appendFixedWidthPadRight("foo", -1, '-');
        assertEquals("", sb.toString());

        sb.clear();
        sb.appendFixedWidthPadRight("foo", 0, '-');
        assertEquals("", sb.toString());

        sb.clear();
        sb.appendFixedWidthPadRight("foo", 1, '-');
        assertEquals("f", sb.toString());

        sb.clear();
        sb.appendFixedWidthPadRight("foo", 2, '-');
        assertEquals("fo", sb.toString());

        sb.clear();
        sb.appendFixedWidthPadRight("foo", 3, '-');
        assertEquals("foo", sb.toString());

        sb.clear();
        sb.appendFixedWidthPadRight("foo", 4, '-');
        assertEquals("foo-", sb.toString());

        sb.clear();
        sb.appendFixedWidthPadRight("foo", 10, '-');
        assertEquals(10, sb.length());
        //            1234567890
        assertEquals("foo-------", sb.toString());

        sb.clear();
        sb.setNullText("null");
        sb.appendFixedWidthPadRight(null, 5, '-');
        assertEquals("null-", sb.toString());
    }

    //-----------------------------------------------------------------------
    public void testAppendFixedWidthPadRight_int() {
        StrBuilder sb = new StrBuilder();
        sb.appendFixedWidthPadRight(123, -1, '-');
        assertEquals("", sb.toString());

        sb.clear();
        sb.appendFixedWidthPadRight(123, 0, '-');
        assertEquals("", sb.toString());

        sb.clear();
        sb.appendFixedWidthPadRight(123, 1, '-');
        assertEquals("1", sb.toString());

        sb.clear();
        sb.appendFixedWidthPadRight(123, 2, '-');
        assertEquals("12", sb.toString());

        sb.clear();
        sb.appendFixedWidthPadRight(123, 3, '-');
        assertEquals("123", sb.toString());

        sb.clear();
        sb.appendFixedWidthPadRight(123, 4, '-');
        assertEquals("123-", sb.toString());

        sb.clear();
        sb.appendFixedWidthPadRight(123, 10, '-');
        assertEquals(10, sb.length());
        //            1234567890
        assertEquals("123-------", sb.toString());
    }

    //-----------------------------------------------------------------------
    public void testAppendWithSeparators_Array() {
        StrBuilder sb = new StrBuilder();
        sb.appendWithSeparators((Object[]) null, ",");
        assertEquals("", sb.toString());

        sb.clear();
        sb.appendWithSeparators(new Object[0], ",");
        assertEquals("", sb.toString());

        sb.clear();
        sb.appendWithSeparators(new Object[]{"foo", "bar", "baz"}, ",");
        assertEquals("foo,bar,baz", sb.toString());

        sb.clear();
        sb.appendWithSeparators(new Object[]{"foo", "bar", "baz"}, null);
        assertEquals("foobarbaz", sb.toString());

        sb.clear();
        sb.appendWithSeparators(new Object[]{"foo", null, "baz"}, ",");
        assertEquals("foo,,baz", sb.toString());
    }

    //-----------------------------------------------------------------------
    public void testAppendWithSeparators_Collection() {
        StrBuilder sb = new StrBuilder();
        sb.appendWithSeparators((Collection) null, ",");
        assertEquals("", sb.toString());

        sb.clear();
        sb.appendWithSeparators(Collections.EMPTY_LIST, ",");
        assertEquals("", sb.toString());

        sb.clear();
        sb.appendWithSeparators(Arrays.asList(new Object[]{"foo", "bar", "baz"}), ",");
        assertEquals("foo,bar,baz", sb.toString());

        sb.clear();
        sb.appendWithSeparators(Arrays.asList(new Object[]{"foo", "bar", "baz"}), null);
        assertEquals("foobarbaz", sb.toString());

        sb.clear();
        sb.appendWithSeparators(Arrays.asList(new Object[]{"foo", null, "baz"}), ",");
        assertEquals("foo,,baz", sb.toString());
    }

    //-----------------------------------------------------------------------
    public void testAppendWithSeparators_Iterator() {
        StrBuilder sb = new StrBuilder();
        sb.appendWithSeparators((Iterator) null, ",");
        assertEquals("", sb.toString());

        sb.clear();
        sb.appendWithSeparators(Collections.EMPTY_LIST.iterator(), ",");
        assertEquals("", sb.toString());

        sb.clear();
        sb.appendWithSeparators(Arrays.asList(new Object[]{"foo", "bar", "baz"}).iterator(), ",");
        assertEquals("foo,bar,baz", sb.toString());

        sb.clear();
        sb.appendWithSeparators(Arrays.asList(new Object[]{"foo", "bar", "baz"}).iterator(), null);
        assertEquals("foobarbaz", sb.toString());

        sb.clear();
        sb.appendWithSeparators(Arrays.asList(new Object[]{"foo", null, "baz"}).iterator(), ",");
        assertEquals("foo,,baz", sb.toString());
    }

    //-----------------------------------------------------------------------
    public void testAppendWithSeparatorsWithNullText() {
        StrBuilder sb = new StrBuilder();
        sb.setNullText("null");
        sb.appendWithSeparators(new Object[]{"foo", null, "baz"}, ",");
        assertEquals("foo,null,baz", sb.toString());

        sb.clear();
        sb.appendWithSeparators(Arrays.asList(new Object[]{"foo", null, "baz"}), ",");
        assertEquals("foo,null,baz", sb.toString());
    }

    //-----------------------------------------------------------------------
    public void testInsert() {

        StrBuilder sb = new StrBuilder();
        sb.append("barbaz");
        assertEquals("barbaz", sb.toString());

        try {
            sb.insert(-1, FOO);
            fail("insert(-1, Object) expected StringIndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.insert(7, FOO);
            fail("insert(7, Object) expected StringIndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        sb.insert(0, (Object) null);
        assertEquals("barbaz", sb.toString());

        sb.insert(0, FOO);
        assertEquals("foobarbaz", sb.toString());

        sb.clear();
        sb.append("barbaz");
        assertEquals("barbaz", sb.toString());

        try {
            sb.insert(-1, "foo");
            fail("insert(-1, String) expected StringIndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.insert(7, "foo");
            fail("insert(7, String) expected StringIndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        sb.insert(0, (String) null);
        assertEquals("barbaz", sb.toString());

        sb.insert(0, "foo");
        assertEquals("foobarbaz", sb.toString());

        sb.clear();
        sb.append("barbaz");
        assertEquals("barbaz", sb.toString());

        try {
            sb.insert(-1, new char[]{'f', 'o', 'o'});
            fail("insert(-1, char[]) expected StringIndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.insert(7, new char[]{'f', 'o', 'o'});
            fail("insert(7, char[]) expected StringIndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        sb.insert(0, (char[]) null);
        assertEquals("barbaz", sb.toString());

        sb.insert(0, new char[0]);
        assertEquals("barbaz", sb.toString());

        sb.insert(0, new char[]{'f', 'o', 'o'});
        assertEquals("foobarbaz", sb.toString());

        sb.clear();
        sb.append("barbaz");
        assertEquals("barbaz", sb.toString());

        try {
            sb.insert(-1, new char[]{'a', 'b', 'c', 'f', 'o', 'o', 'd', 'e', 'f'}, 3, 3);
            fail("insert(-1, char[], 3, 3) expected StringIndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.insert(7, new char[]{'a', 'b', 'c', 'f', 'o', 'o', 'd', 'e', 'f'}, 3, 3);
            fail("insert(7, char[], 3, 3) expected StringIndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        sb.insert(0, (char[]) null, 0, 0);
        assertEquals("barbaz", sb.toString());

        sb.insert(0, new char[0], 0, 0);
        assertEquals("barbaz", sb.toString());

        try {
            sb.insert(0, new char[]{'a', 'b', 'c', 'f', 'o', 'o', 'd', 'e', 'f'}, -1, 3);
            fail("insert(0, char[], -1, 3) expected StringIndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.insert(0, new char[]{'a', 'b', 'c', 'f', 'o', 'o', 'd', 'e', 'f'}, 10, 3);
            fail("insert(0, char[], 10, 3) expected StringIndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.insert(0, new char[]{'a', 'b', 'c', 'f', 'o', 'o', 'd', 'e', 'f'}, 0, -1);
            fail("insert(0, char[], 0, -1) expected StringIndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.insert(0, new char[]{'a', 'b', 'c', 'f', 'o', 'o', 'd', 'e', 'f'}, 0, 10);
            fail("insert(0, char[], 0, 10) expected StringIndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        sb.insert(0, new char[]{'a', 'b', 'c', 'f', 'o', 'o', 'd', 'e', 'f'}, 0, 0);
        assertEquals("barbaz", sb.toString());

        sb.insert(0, new char[]{'a', 'b', 'c', 'f', 'o', 'o', 'd', 'e', 'f'}, 3, 3);
        assertEquals("foobarbaz", sb.toString());

        sb.clear();
        sb.append("barbaz");
        assertEquals("barbaz", sb.toString());

        try {
            sb.insert(-1, true);
            fail("insert(-1, boolean) expected StringIndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.insert(7, true);
            fail("insert(7, boolean) expected StringIndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        sb.insert(0, true);
        assertEquals("truebarbaz", sb.toString());

        sb.insert(0, false);
        assertEquals("falsetruebarbaz", sb.toString());

        sb.clear();
        sb.append("barbaz");
        assertEquals("barbaz", sb.toString());

        try {
            sb.insert(-1, '!');
            fail("insert(-1, char) expected StringIndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.insert(7, '!');
            fail("insert(7, char) expected StringIndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        sb.insert(0, '!');
        assertEquals("!barbaz", sb.toString());

        sb.clear();
        sb.append("barbaz");
        assertEquals("barbaz", sb.toString());

        try {
            sb.insert(-1, 0);
            fail("insert(-1, int) expected StringIndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.insert(7, 0);
            fail("insert(7, int) expected StringIndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        sb.insert(0, '0');
        assertEquals("0barbaz", sb.toString());

        sb.clear();
        sb.append("barbaz");
        assertEquals("barbaz", sb.toString());

        try {
            sb.insert(-1, 1L);
            fail("insert(-1, long) expected StringIndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.insert(7, 1L);
            fail("insert(7, long) expected StringIndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        sb.insert(0, 1L);
        assertEquals("1barbaz", sb.toString());

        sb.clear();
        sb.append("barbaz");
        assertEquals("barbaz", sb.toString());

        try {
            sb.insert(-1, 2.3F);
            fail("insert(-1, float) expected StringIndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.insert(7, 2.3F);
            fail("insert(7, float) expected StringIndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        sb.insert(0, 2.3F);
        assertEquals("2.3barbaz", sb.toString());

        sb.clear();
        sb.append("barbaz");
        assertEquals("barbaz", sb.toString());

        try {
            sb.insert(-1, 4.5D);
            fail("insert(-1, double) expected StringIndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.insert(7, 4.5D);
            fail("insert(7, double) expected StringIndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        sb.insert(0, 4.5D);
        assertEquals("4.5barbaz", sb.toString());
    }

    //-----------------------------------------------------------------------
    public void testInsertWithNullText() {
        StrBuilder sb = new StrBuilder();
        sb.setNullText("null");
        sb.append("barbaz");
        assertEquals("barbaz", sb.toString());

        try {
            sb.insert(-1, FOO);
            fail("insert(-1, Object) expected StringIndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.insert(7, FOO);
            fail("insert(7, Object) expected StringIndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        sb.insert(0, (Object) null);
        assertEquals("nullbarbaz", sb.toString());

        sb.insert(0, FOO);
        assertEquals("foonullbarbaz", sb.toString());

        sb.clear();
        sb.append("barbaz");
        assertEquals("barbaz", sb.toString());

        try {
            sb.insert(-1, "foo");
            fail("insert(-1, String) expected StringIndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        try {
            sb.insert(7, "foo");
            fail("insert(7, String) expected StringIndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException e) {
            // expected
        }

        sb.insert(0, (String) null);
        assertEquals("nullbarbaz", sb.toString());

        sb.insert(0, "foo");
        assertEquals("foonullbarbaz", sb.toString());

        sb.insert(0, (char[]) null);
        assertEquals("nullfoonullbarbaz", sb.toString());

        sb.insert(0, (char[]) null, 0, 0);
        assertEquals("nullnullfoonullbarbaz", sb.toString());
    }

}
