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
 * Unit tests {@link org.apache.commons.lang.CharSetUtils}.
 *
 * @author <a href="mailto:bayard@generationjava.com">Henri Yandell</a>
 * @author <a href="mailto:ridesmet@users.sourceforge.net">Ringo De Smet</a>
 * @author Stephen Colebourne
 * @version $Id: CharSetUtilsTest.java,v 1.9 2003/07/30 00:08:38 scolebourne Exp $
 */
public class CharSetUtilsTest extends TestCase {
    
    public CharSetUtilsTest(String name) {
        super(name);
    }

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(CharSetUtilsTest.class);
        suite.setName("CharSetUtils Tests");
        return suite;
    }

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    //-----------------------------------------------------------------------

    public void testSqueeze() {
        assertEquals(null, CharSetUtils.squeeze(null, (String[]) null));
        assertEquals(null, CharSetUtils.squeeze(null, (String) null));
        assertEquals(null, CharSetUtils.squeeze(null, new String[] { "el" }));
        assertEquals("helo", CharSetUtils.squeeze("hello", new String[] { "el" }));
        assertEquals("hello", CharSetUtils.squeeze("hello", ""));
        assertEquals("", CharSetUtils.squeeze("", new String[] { "el" }));
        assertEquals("hello", CharSetUtils.squeeze("hello", new String[] { "e" }));
        assertEquals("fofof", CharSetUtils.squeeze("fooffooff", new String[] { "of" }));
        assertEquals("fof", CharSetUtils.squeeze("fooooff", new String[] { "fo" }));
        try {
            CharSetUtils.squeeze("hello", (String[]) null);
            fail("Expecting NullPointerException");
        } catch (NullPointerException ex) {}
        try {
            CharSetUtils.squeeze("hello", new String[] { "", null });
            fail("Expecting NullPointerException");
        } catch (NullPointerException ex) {}
    }

    public void testCount() {
        assertEquals(0, CharSetUtils.count(null, (String[]) null));
        assertEquals(0, CharSetUtils.count(null, (String) null));
        assertEquals(0, CharSetUtils.count(null, new String[] { "el" }));
        assertEquals(3, CharSetUtils.count("hello", new String[] { "el" }));
        assertEquals(0, CharSetUtils.count("", new String[] { "el" }));
        assertEquals(0, CharSetUtils.count("hello", new String[] { "x" }));
        assertEquals(2, CharSetUtils.count("hello", new String[] { "e-i" }));
        assertEquals(5, CharSetUtils.count("hello", new String[] { "a-z" }));
        assertEquals(0, CharSetUtils.count("hello", new String[] { "" }));
        assertEquals(0, CharSetUtils.count("hello", ""));
        try {
            CharSetUtils.count("hello", (String[]) null);
            fail("Expecting NullPointerException");
        } catch (NullPointerException ex) {}
        try {
            CharSetUtils.count("hello", new String[] { "", null });
            fail("Expecting NullPointerException");
        } catch (NullPointerException ex) {}
    }

    public void testKeep() {
        assertEquals(null, CharSetUtils.keep(null, (String[]) null));
        assertEquals(null, CharSetUtils.keep(null, (String) null));
        assertEquals(null, CharSetUtils.keep(null, new String[] { "el" }));
        assertEquals("ell", CharSetUtils.keep("hello", new String[] { "el" }));
        assertEquals("hello", CharSetUtils.keep("hello", new String[] { "elho" }));
        assertEquals("", CharSetUtils.keep("hello", new String[] { "" }));
        assertEquals("", CharSetUtils.keep("hello", ""));
        assertEquals("hello", CharSetUtils.keep("hello", new String[] { "a-z" }));
        assertEquals("----", CharSetUtils.keep("----", new String[] { "-" }));
        assertEquals("ll", CharSetUtils.keep("hello", new String[] { "l" }));
        try {
            CharSetUtils.keep("hello", (String[]) null);
            fail("Expecting NullPointerException");
        } catch (NullPointerException ex) {}
        try {
            CharSetUtils.keep("hello", new String[] { "", null});
            fail("Expecting NullPointerException");
        } catch (NullPointerException ex) {}
    }

    public void testDelete() {
        assertEquals(null, CharSetUtils.delete(null, (String[]) null));
        assertEquals(null, CharSetUtils.delete(null,(String) null));
        assertEquals(null, CharSetUtils.delete(null, new String[] { "el" }));
        assertEquals("ho", CharSetUtils.delete("hello", new String[] { "el" }));
        assertEquals("", CharSetUtils.delete("hello", new String[] { "elho" }));
        assertEquals("hello", CharSetUtils.delete("hello", new String[] { "" }));
        assertEquals("hello", CharSetUtils.delete("hello", ""));
        assertEquals("", CharSetUtils.delete("hello", new String[] { "a-z" }));
        assertEquals("", CharSetUtils.delete("----", new String[] { "-" }));
        assertEquals("heo", CharSetUtils.delete("hello", new String[] { "l" }));
        try {
            CharSetUtils.delete("hello", (String[]) null);
            fail("Expecting NullPointerException");
        } catch (NullPointerException ex) {}
        try {
            CharSetUtils.delete("hello",  new String[] { "-", null });
            fail("Expecting NullPointerException");
        } catch (NullPointerException ex) {}
    }
    
    public void testTranslate() {
        assertEquals(null, CharSetUtils.translate(null, null, null));
        assertEquals("", CharSetUtils.translate("","a", "b"));
        assertEquals("jelly", CharSetUtils.translate("hello", "ho", "jy"));
        assertEquals("jellj", CharSetUtils.translate("hello", "ho", "j"));
        assertEquals("jelly", CharSetUtils.translate("hello", "ho", "jyx"));
        assertEquals("\rhello\r", CharSetUtils.translate("\nhello\n", "\n", "\r"));
        assertEquals("hello", CharSetUtils.translate("hello", "", "x"));
        assertEquals("hello", CharSetUtils.translate("hello", "", ""));
        try {
            CharSetUtils.translate("hello", null, null);
            fail("Expecting NullPointerException");
        } catch (NullPointerException ex) {}
        try {
            CharSetUtils.translate("hello", "h", null);
            fail("Expecting NullPointerException");
        } catch (NullPointerException ex) {}
        try {
            CharSetUtils.translate("hello", null, "a");
            fail("Expecting NullPointerException");
        } catch (NullPointerException ex) {}
        try {
            CharSetUtils.translate("hello", "h", "");
            fail("Expecting ArrayIndexOutOfBoundsException");
        } catch (ArrayIndexOutOfBoundsException ex) {}
    }         
}
