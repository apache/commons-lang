/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002-2003 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
 *
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
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
 * @version $Id: StringUtilsTrimEmptyTest.java,v 1.7 2003/03/24 00:47:02 scolebourne Exp $
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

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    //-----------------------------------------------------------------------

    public void testClean() {
        assertEquals(FOO, StringUtils.clean(FOO + "  "));
        assertEquals(FOO, StringUtils.clean(" " + FOO + "  "));
        assertEquals(FOO, StringUtils.clean(" " + FOO));
        assertEquals(FOO, StringUtils.clean(FOO + ""));
        assertEquals("", StringUtils.clean(" \t\r\n\b "));
        assertEquals("", StringUtils.clean(""));
        assertEquals("", StringUtils.clean(null));
    }

    public void testTrim() {
        assertEquals(FOO, StringUtils.trim(FOO + "  "));
        assertEquals(FOO, StringUtils.trim(" " + FOO + "  "));
        assertEquals(FOO, StringUtils.trim(" " + FOO));
        assertEquals(FOO, StringUtils.trim(FOO + ""));
        assertEquals("", StringUtils.trim(" \t\r\n\b "));
        assertEquals("", StringUtils.trim(""));
        assertEquals(null, StringUtils.trim(null));
    }

    public void testTrimToNull() {
        assertEquals(FOO, StringUtils.trimToNull(FOO + "  "));
        assertEquals(FOO, StringUtils.trimToNull(" " + FOO + "  "));
        assertEquals(FOO, StringUtils.trimToNull(" " + FOO));
        assertEquals(FOO, StringUtils.trimToNull(FOO + ""));
        assertEquals(null, StringUtils.trimToNull(" \t\r\n\b "));
        assertEquals(null, StringUtils.trimToNull(""));
        assertEquals(null, StringUtils.trimToNull(null));
    }

    public void testTrimToEmpty() {
        assertEquals(FOO, StringUtils.trimToEmpty(FOO + "  "));
        assertEquals(FOO, StringUtils.trimToEmpty(" " + FOO + "  "));
        assertEquals(FOO, StringUtils.trimToEmpty(" " + FOO));
        assertEquals(FOO, StringUtils.trimToEmpty(FOO + ""));
        assertEquals("", StringUtils.trimToEmpty(" \t\r\n\b "));
        assertEquals("", StringUtils.trimToEmpty(""));
        assertEquals("", StringUtils.trimToEmpty(null));
    }

    public void testIsNotEmpty() {
        assertEquals(true, StringUtils.isNotEmpty(FOO));
        assertEquals(true, StringUtils.isNotEmpty(" "));
        assertEquals(false, StringUtils.isNotEmpty(""));
        assertEquals(false, StringUtils.isNotEmpty(null));
    }

    public void testIsEmpty() {
        assertEquals(false, StringUtils.isEmpty(FOO));
        assertEquals(true, StringUtils.isEmpty(" "));
        assertEquals(true, StringUtils.isEmpty(""));
        assertEquals(true, StringUtils.isEmpty(null));
    }

    public void testDeleteSpace() {
        assertEquals("deleteWhitespace(String) failed",
                     "", StringUtils.deleteWhitespace(""));
        assertEquals("deleteWhitespace(String) failed",
                     "", StringUtils.deleteWhitespace("  \u000C  \t\t\u001F\n\n \u000B  "));
        // Note: u-2007 and u-000A both cause problems in the source code
        // it should ignore 2007 but delete 000A
        assertEquals("deleteWhitespace(String) failed",
                     "\u00A0\u202F", StringUtils.deleteWhitespace("  \u00A0  \t\t\n\n \u202F  "));
        assertEquals("deleteWhitespace(String) failed",
                     "\u00A0\u202F", StringUtils.deleteWhitespace("\u00A0\u202F"));
        assertEquals("deleteWhitespace(String) failed",
                     "test", StringUtils.deleteWhitespace("\u000Bt  \t\n\u0009e\rs\n\n   \tt"));

        assertEquals("deleteSpaces(String) failed",
                     "", StringUtils.deleteSpaces(""));
        assertEquals("deleteSpaces(String) failed",
                     "", StringUtils.deleteSpaces("    \t\t\n\n   "));
        assertEquals("deleteSpaces(String) failed",
                     "test", StringUtils.deleteSpaces("t  \t\ne\rs\n\n   \tt"));
    }

    public void testStrip() {
        // it's important that foo2Space is fooLeftSpace and fooRightSpace 
        // merged together. So same number of spaces to left as fLS and same 
        // to right as fLS. Same applies for foo2Dots.
        String foo2Space = "    "+FOO+"    ";
        String foo2Dots = "......"+FOO+".........";
        String fooLeftSpace = "    "+FOO;
        String fooLeftDots = "......"+FOO;
        String fooRightSpace = FOO+"    ";
        String fooRightDots = FOO+".........";

        assertEquals("", StringUtils.strip(""));
        assertEquals("", StringUtils.strip("        "));
        assertEquals(FOO, StringUtils.strip(foo2Space));
        assertEquals(FOO, StringUtils.strip(foo2Dots, "."));
        assertEquals(FOO, StringUtils.strip(fooRightSpace));
        assertEquals(FOO, StringUtils.strip(fooRightDots, "."));
        assertEquals(FOO, StringUtils.strip(fooLeftSpace));
        assertEquals(FOO, StringUtils.strip(fooLeftDots, "."));

        assertEquals("", StringUtils.stripStart("", " "));
        assertEquals(fooRightSpace, StringUtils.stripStart(foo2Space, " "));
        assertEquals(fooRightDots, StringUtils.stripStart(foo2Dots, "."));
        assertEquals(fooRightSpace, StringUtils.stripStart(fooRightSpace, " "));
        assertEquals(fooRightDots, StringUtils.stripStart(fooRightDots, "."));
        assertEquals(FOO, StringUtils.stripStart(fooLeftSpace, " "));
        assertEquals(FOO, StringUtils.stripStart(fooLeftDots, "."));

        assertEquals("", StringUtils.stripEnd("", " "));
        assertEquals(fooLeftSpace, StringUtils.stripEnd(foo2Space, " "));
        assertEquals(fooLeftDots, StringUtils.stripEnd(foo2Dots, "."));
        assertEquals(FOO, StringUtils.stripEnd(fooRightSpace, " "));
        assertEquals(FOO, StringUtils.stripEnd(fooRightDots, "."));
        assertEquals(fooLeftSpace, StringUtils.stripEnd(fooLeftSpace, " "));
        assertEquals(fooLeftDots, StringUtils.stripEnd(fooLeftDots, "."));

        assertEquals(FOO, StringUtils.strip(". . . . ."+FOO+". . ", " ."));
        assertEquals("-."+FOO, StringUtils.strip(". . . . -."+FOO+". . ", " ."));
        assertEquals(FOO, StringUtils.strip("..  .."+FOO+".. ", " ."));
        assertEquals(FOO, StringUtils.strip("..  .."+FOO+".. ", "+= ."));

        // test stripAll method, merely an array version of the above strip
        String[] empty = new String[0];
        String[] fooSpace = new String[] { foo2Space, fooLeftSpace, fooRightSpace };
        String[] fooDots = new String[] { foo2Dots, fooLeftDots, fooRightDots };
        String[] foo = new String[] { FOO, FOO, FOO };

        assertArrayEquals(empty, StringUtils.stripAll(empty));
        assertArrayEquals(foo, StringUtils.stripAll(fooSpace));
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
