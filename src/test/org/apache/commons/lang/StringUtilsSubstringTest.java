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
 * Unit tests {@link org.apache.commons.lang.StringUtils} - Substring methods
 *
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @author <a href="mailto:ridesmet@users.sourceforge.net">Ringo De Smet</a>
 * @version $Id: StringUtilsSubstringTest.java,v 1.3 2003/03/23 21:51:19 scolebourne Exp $
 */
public class StringUtilsSubstringTest extends TestCase {
    private static final String FOO = "foo";
    private static final String BAR = "bar";
    private static final String BAZ = "baz";
    private static final String FOOBAR = "foobar";
    private static final String SENTENCE = "foo bar baz";

    public StringUtilsSubstringTest(String name) {
        super(name);
    }

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
    	TestSuite suite = new TestSuite(StringUtilsSubstringTest.class);
    	suite.setName("StringUtilsSubstring Tests");
        return suite;
    }

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    //-----------------------------------------------------------------------


    public void testSubstring2() {
        assertEquals("", StringUtils.substring(SENTENCE, 80));
        assertEquals(BAZ, StringUtils.substring(SENTENCE, 8));
        assertEquals(BAZ, StringUtils.substring(SENTENCE, -3));
        assertEquals(SENTENCE, StringUtils.substring(SENTENCE, 0));
    }
    
    public void testSubstring3() {
        assertEquals("", StringUtils.substring(SENTENCE, 8, 6));
        assertEquals(FOO, StringUtils.substring(SENTENCE, 0, 3));
        assertEquals("o", StringUtils.substring(SENTENCE, -9, 3));
        assertEquals(FOO, StringUtils.substring(SENTENCE, 0, -8));
        assertEquals("o", StringUtils.substring(SENTENCE, -9, -8));
        assertEquals(SENTENCE, StringUtils.substring(SENTENCE, 0, 80));
    }
    
    public void testLeft() {
        assertSame(null, StringUtils.left(null, 0));
        assertSame(null, StringUtils.left(null, 2));
        assertSame("", StringUtils.left("", 0));
        assertSame("", StringUtils.left("", 2));
        assertEquals("", StringUtils.left(FOOBAR, 0));
        assertEquals(FOO, StringUtils.left(FOOBAR, 3));
        assertSame(FOOBAR, StringUtils.left(FOOBAR, 80));
    }
    
    public void testLeftEx() {
        try {
            StringUtils.left(FOOBAR, -1);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }

    public void testRight() {
        assertSame(null, StringUtils.right(null, 0));
        assertSame(null, StringUtils.right(null, 2));
        assertSame("", StringUtils.right("", 0));
        assertSame("", StringUtils.right("", 2));
        assertEquals("", StringUtils.right(FOOBAR, 0));
        assertEquals(BAR, StringUtils.right(FOOBAR, 3));
        assertSame(FOOBAR, StringUtils.right(FOOBAR, 80));
    }
    
    public void testRightEx() {
        try {
            StringUtils.right(FOOBAR, -1);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }

    public void testMid() {
        assertSame(null, StringUtils.mid(null, 3, 0));
        assertSame(null, StringUtils.mid(null, 3, 2));
        assertSame("", StringUtils.mid("", 0, 0));
        assertSame("", StringUtils.mid("", 0, 2));
        assertEquals("", StringUtils.mid(FOOBAR, 3, 0));
        assertEquals("b", StringUtils.mid(FOOBAR, 3, 1));
        assertEquals(FOO, StringUtils.mid(FOOBAR, 0, 3));
        assertEquals(BAR, StringUtils.mid(FOOBAR, 3, 3));
        assertEquals(FOOBAR, StringUtils.mid(FOOBAR, 0, 80));
        assertEquals(BAR, StringUtils.mid(FOOBAR, 3, 80));
    }
    
    public void testMidEx1() {
        try {
            StringUtils.mid(FOOBAR, 0, -1);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }

    public void testMidEx2() {
        try {
            StringUtils.mid(FOOBAR, -1, 3);
        } catch (IndexOutOfBoundsException ex) {
            return;
        }
        fail();
    }

    public void testMidEx3() {
        try {
            StringUtils.mid(FOOBAR, 7, 3);
        } catch (IndexOutOfBoundsException ex) {
            return;
        }
        fail();
    }

    public void testCountMatches() {
        try {
            assertEquals(-1, 
                StringUtils.countMatches(null, null));
            throw new RuntimeException("Should have thrown a NullPointerException. ");
        } catch(NullPointerException npe) {
        }

        assertEquals(0, 
             StringUtils.countMatches("x", "") );
        assertEquals(0, 
             StringUtils.countMatches("", "") );

        assertEquals(3, 
             StringUtils.countMatches("one long someone sentence of one", "one"));
        assertEquals(0, 
             StringUtils.countMatches("one long someone sentence of one", "two"));
        assertEquals(4, 
             StringUtils.countMatches("oooooooooooo", "ooo"));
    }

    public void testGetNestedString() {
        assertEquals( "", StringUtils.getNestedString("", "") );
        assertEquals( "", StringUtils.getNestedString("    ", " ") );
        assertEquals( "bar", StringUtils.getNestedString("\nbar\n", "\n") );
        assertEquals( "", StringUtils.getNestedString("", "", "") );
        assertEquals( "", StringUtils.getNestedString("    ", " ", "  ") );
        assertEquals( "bar", StringUtils.getNestedString("<foo>bar</foo>", "<foo>", "</foo>") );
    }

}
