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
package org.apache.commons.lang;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Unit tests {@link org.apache.commons.lang.StringUtils} - Trim/Empty methods
 *
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @author <a href="mailto:ridesmet@users.sourceforge.net">Ringo De Smet</a>
 * @version $Id$
 */
public class StringUtilsTrimEmptyTest extends TestCase {
    private static final String FOO = "foo";

    public StringUtilsTrimEmptyTest(String name) {
        super(name);
    }

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(StringUtilsTrimEmptyTest.class);
        suite.setName("StringUtilsTrimEmpty Tests");
        return suite;
    }

    @Override
    protected void setUp() throws Exception {
        super.setUp();
    }

    @Override
    protected void tearDown() throws Exception {
        super.tearDown();
    }

    //-----------------------------------------------------------------------
    public void testIsEmpty() {
        assertEquals(true, StringUtils.isEmpty(null));
        assertEquals(true, StringUtils.isEmpty(""));
        assertEquals(false, StringUtils.isEmpty(" "));
        assertEquals(false, StringUtils.isEmpty("foo"));
        assertEquals(false, StringUtils.isEmpty("  foo  "));
    }

    public void testIsNotEmpty() {
        assertEquals(false, StringUtils.isNotEmpty(null));
        assertEquals(false, StringUtils.isNotEmpty(""));
        assertEquals(true, StringUtils.isNotEmpty(" "));
        assertEquals(true, StringUtils.isNotEmpty("foo"));
        assertEquals(true, StringUtils.isNotEmpty("  foo  "));
    }

    public void testIsBlank() {
        assertEquals(true, StringUtils.isBlank(null));
        assertEquals(true, StringUtils.isBlank(""));
        assertEquals(true, StringUtils.isBlank(StringUtilsTest.WHITESPACE));
        assertEquals(false, StringUtils.isBlank("foo"));
        assertEquals(false, StringUtils.isBlank("  foo  "));
    }

    public void testIsNotBlank() {
        assertEquals(false, StringUtils.isNotBlank(null));
        assertEquals(false, StringUtils.isNotBlank(""));
        assertEquals(false, StringUtils.isNotBlank(StringUtilsTest.WHITESPACE));
        assertEquals(true, StringUtils.isNotBlank("foo"));
        assertEquals(true, StringUtils.isNotBlank("  foo  "));
    }

    //-----------------------------------------------------------------------
    public void testTrim() {
        assertEquals(FOO, StringUtils.trim(FOO + "  "));
        assertEquals(FOO, StringUtils.trim(" " + FOO + "  "));
        assertEquals(FOO, StringUtils.trim(" " + FOO));
        assertEquals(FOO, StringUtils.trim(FOO + ""));
        assertEquals("", StringUtils.trim(" \t\r\n\b "));
        assertEquals("", StringUtils.trim(StringUtilsTest.TRIMMABLE));
        assertEquals(StringUtilsTest.NON_TRIMMABLE, StringUtils.trim(StringUtilsTest.NON_TRIMMABLE));
        assertEquals("", StringUtils.trim(""));
        assertEquals(null, StringUtils.trim(null));
    }

    public void testTrimToNull() {
        assertEquals(FOO, StringUtils.trimToNull(FOO + "  "));
        assertEquals(FOO, StringUtils.trimToNull(" " + FOO + "  "));
        assertEquals(FOO, StringUtils.trimToNull(" " + FOO));
        assertEquals(FOO, StringUtils.trimToNull(FOO + ""));
        assertEquals(null, StringUtils.trimToNull(" \t\r\n\b "));
        assertEquals(null, StringUtils.trimToNull(StringUtilsTest.TRIMMABLE));
        assertEquals(StringUtilsTest.NON_TRIMMABLE, StringUtils.trimToNull(StringUtilsTest.NON_TRIMMABLE));
        assertEquals(null, StringUtils.trimToNull(""));
        assertEquals(null, StringUtils.trimToNull(null));
    }

    public void testTrimToEmpty() {
        assertEquals(FOO, StringUtils.trimToEmpty(FOO + "  "));
        assertEquals(FOO, StringUtils.trimToEmpty(" " + FOO + "  "));
        assertEquals(FOO, StringUtils.trimToEmpty(" " + FOO));
        assertEquals(FOO, StringUtils.trimToEmpty(FOO + ""));
        assertEquals("", StringUtils.trimToEmpty(" \t\r\n\b "));
        assertEquals("", StringUtils.trimToEmpty(StringUtilsTest.TRIMMABLE));
        assertEquals(StringUtilsTest.NON_TRIMMABLE, StringUtils.trimToEmpty(StringUtilsTest.NON_TRIMMABLE));
        assertEquals("", StringUtils.trimToEmpty(""));
        assertEquals("", StringUtils.trimToEmpty(null));
    }

    //-----------------------------------------------------------------------
    public void testStrip_String() {
        assertEquals(null, StringUtils.strip(null));
        assertEquals("", StringUtils.strip(""));
        assertEquals("", StringUtils.strip("        "));
        assertEquals("abc", StringUtils.strip("  abc  "));
        assertEquals(StringUtilsTest.NON_WHITESPACE, 
            StringUtils.strip(StringUtilsTest.WHITESPACE + StringUtilsTest.NON_WHITESPACE + StringUtilsTest.WHITESPACE));
    }
    
    public void testStripToNull_String() {
        assertEquals(null, StringUtils.stripToNull(null));
        assertEquals(null, StringUtils.stripToNull(""));
        assertEquals(null, StringUtils.stripToNull("        "));
        assertEquals(null, StringUtils.stripToNull(StringUtilsTest.WHITESPACE));
        assertEquals("ab c", StringUtils.stripToNull("  ab c  "));
        assertEquals(StringUtilsTest.NON_WHITESPACE, 
            StringUtils.stripToNull(StringUtilsTest.WHITESPACE + StringUtilsTest.NON_WHITESPACE + StringUtilsTest.WHITESPACE));
    }
    
    public void testStripToEmpty_String() {
        assertEquals("", StringUtils.stripToEmpty(null));
        assertEquals("", StringUtils.stripToEmpty(""));
        assertEquals("", StringUtils.stripToEmpty("        "));
        assertEquals("", StringUtils.stripToEmpty(StringUtilsTest.WHITESPACE));
        assertEquals("ab c", StringUtils.stripToEmpty("  ab c  "));
        assertEquals(StringUtilsTest.NON_WHITESPACE, 
            StringUtils.stripToEmpty(StringUtilsTest.WHITESPACE + StringUtilsTest.NON_WHITESPACE + StringUtilsTest.WHITESPACE));
    }
    
    public void testStrip_StringString() {
        // null strip
        assertEquals(null, StringUtils.strip(null, null));
        assertEquals("", StringUtils.strip("", null));
        assertEquals("", StringUtils.strip("        ", null));
        assertEquals("abc", StringUtils.strip("  abc  ", null));
        assertEquals(StringUtilsTest.NON_WHITESPACE, 
            StringUtils.strip(StringUtilsTest.WHITESPACE + StringUtilsTest.NON_WHITESPACE + StringUtilsTest.WHITESPACE, null));

        // "" strip
        assertEquals(null, StringUtils.strip(null, ""));
        assertEquals("", StringUtils.strip("", ""));
        assertEquals("        ", StringUtils.strip("        ", ""));
        assertEquals("  abc  ", StringUtils.strip("  abc  ", ""));
        assertEquals(StringUtilsTest.WHITESPACE, StringUtils.strip(StringUtilsTest.WHITESPACE, ""));
        
        // " " strip
        assertEquals(null, StringUtils.strip(null, " "));
        assertEquals("", StringUtils.strip("", " "));
        assertEquals("", StringUtils.strip("        ", " "));
        assertEquals("abc", StringUtils.strip("  abc  ", " "));
        
        // "ab" strip
        assertEquals(null, StringUtils.strip(null, "ab"));
        assertEquals("", StringUtils.strip("", "ab"));
        assertEquals("        ", StringUtils.strip("        ", "ab"));
        assertEquals("  abc  ", StringUtils.strip("  abc  ", "ab"));
        assertEquals("c", StringUtils.strip("abcabab", "ab"));
        assertEquals(StringUtilsTest.WHITESPACE, StringUtils.strip(StringUtilsTest.WHITESPACE, ""));
    }
    
    public void testStripStart_StringString() {
        // null stripStart
        assertEquals(null, StringUtils.stripStart(null, null));
        assertEquals("", StringUtils.stripStart("", null));
        assertEquals("", StringUtils.stripStart("        ", null));
        assertEquals("abc  ", StringUtils.stripStart("  abc  ", null));
        assertEquals(StringUtilsTest.NON_WHITESPACE + StringUtilsTest.WHITESPACE, 
            StringUtils.stripStart(StringUtilsTest.WHITESPACE + StringUtilsTest.NON_WHITESPACE + StringUtilsTest.WHITESPACE, null));

        // "" stripStart
        assertEquals(null, StringUtils.stripStart(null, ""));
        assertEquals("", StringUtils.stripStart("", ""));
        assertEquals("        ", StringUtils.stripStart("        ", ""));
        assertEquals("  abc  ", StringUtils.stripStart("  abc  ", ""));
        assertEquals(StringUtilsTest.WHITESPACE, StringUtils.stripStart(StringUtilsTest.WHITESPACE, ""));
        
        // " " stripStart
        assertEquals(null, StringUtils.stripStart(null, " "));
        assertEquals("", StringUtils.stripStart("", " "));
        assertEquals("", StringUtils.stripStart("        ", " "));
        assertEquals("abc  ", StringUtils.stripStart("  abc  ", " "));
        
        // "ab" stripStart
        assertEquals(null, StringUtils.stripStart(null, "ab"));
        assertEquals("", StringUtils.stripStart("", "ab"));
        assertEquals("        ", StringUtils.stripStart("        ", "ab"));
        assertEquals("  abc  ", StringUtils.stripStart("  abc  ", "ab"));
        assertEquals("cabab", StringUtils.stripStart("abcabab", "ab"));
        assertEquals(StringUtilsTest.WHITESPACE, StringUtils.stripStart(StringUtilsTest.WHITESPACE, ""));
    }
    
    public void testStripEnd_StringString() {
        // null stripEnd
        assertEquals(null, StringUtils.stripEnd(null, null));
        assertEquals("", StringUtils.stripEnd("", null));
        assertEquals("", StringUtils.stripEnd("        ", null));
        assertEquals("  abc", StringUtils.stripEnd("  abc  ", null));
        assertEquals(StringUtilsTest.WHITESPACE + StringUtilsTest.NON_WHITESPACE, 
            StringUtils.stripEnd(StringUtilsTest.WHITESPACE + StringUtilsTest.NON_WHITESPACE + StringUtilsTest.WHITESPACE, null));

        // "" stripEnd
        assertEquals(null, StringUtils.stripEnd(null, ""));
        assertEquals("", StringUtils.stripEnd("", ""));
        assertEquals("        ", StringUtils.stripEnd("        ", ""));
        assertEquals("  abc  ", StringUtils.stripEnd("  abc  ", ""));
        assertEquals(StringUtilsTest.WHITESPACE, StringUtils.stripEnd(StringUtilsTest.WHITESPACE, ""));
        
        // " " stripEnd
        assertEquals(null, StringUtils.stripEnd(null, " "));
        assertEquals("", StringUtils.stripEnd("", " "));
        assertEquals("", StringUtils.stripEnd("        ", " "));
        assertEquals("  abc", StringUtils.stripEnd("  abc  ", " "));
        
        // "ab" stripEnd
        assertEquals(null, StringUtils.stripEnd(null, "ab"));
        assertEquals("", StringUtils.stripEnd("", "ab"));
        assertEquals("        ", StringUtils.stripEnd("        ", "ab"));
        assertEquals("  abc  ", StringUtils.stripEnd("  abc  ", "ab"));
        assertEquals("abc", StringUtils.stripEnd("abcabab", "ab"));
        assertEquals(StringUtilsTest.WHITESPACE, StringUtils.stripEnd(StringUtilsTest.WHITESPACE, ""));
    }

    public void testStripAll() {
        // test stripAll method, merely an array version of the above strip
        String[] empty = new String[0];
        String[] fooSpace = new String[] { "  "+FOO+"  ", "  "+FOO, FOO+"  " };
        String[] fooDots = new String[] { ".."+FOO+"..", ".."+FOO, FOO+".." };
        String[] foo = new String[] { FOO, FOO, FOO };

        assertEquals(null, StringUtils.stripAll(null));
        assertArrayEquals(empty, StringUtils.stripAll(empty));
        assertArrayEquals(foo, StringUtils.stripAll(fooSpace));
        
        assertEquals(null, StringUtils.stripAll(null, null));
        assertArrayEquals(foo, StringUtils.stripAll(fooSpace, null));
        assertArrayEquals(foo, StringUtils.stripAll(fooDots, "."));
    }

    private void assertArrayEquals(Object[] o1, Object[] o2) {
        if(o1 == null) {
            assertEquals(o1,o2);
            return;
        }
        assertEquals("Length not equal. ", o1.length, o2.length);
        int sz = o1.length;
        for(int i=0; i<sz; i++) {
            if(o1[i] instanceof Object[]) {
                // do an assert equals on type....
                assertArrayEquals( (Object[]) o1[i], (Object[]) o2[i] );
            } else {
                assertEquals(o1[i], o2[i]);
            }
        }
    }

}
