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

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;

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
 * @version $Id: CharSetUtilsTest.java,v 1.12 2003/07/31 22:31:12 scolebourne Exp $
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
    public void testConstructor() {
        assertNotNull(new CharSetUtils());
        Constructor[] cons = CharSetUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertEquals(true, Modifier.isPublic(cons[0].getModifiers()));
        assertEquals(true, Modifier.isPublic(CharSetUtils.class.getModifiers()));
        assertEquals(false, Modifier.isFinal(CharSetUtils.class.getModifiers()));
    }
    
    //-----------------------------------------------------------------------
    public void testEvaluateSet_String() {
        assertEquals(null, CharSetUtils.evaluateSet((String) null));
        assertEquals("[]", CharSetUtils.evaluateSet("").toString());
        assertEquals("[a-e]", CharSetUtils.evaluateSet("a-e").toString());
    }
    
    public void testEvaluateSet_Stringarray() {
        assertEquals(null, CharSetUtils.evaluateSet((String[]) null));
        assertEquals("[]", CharSetUtils.evaluateSet(new String[0]).toString());
        assertEquals("[]", CharSetUtils.evaluateSet(new String[] {null}).toString());
        assertEquals("[a-e]", CharSetUtils.evaluateSet(new String[] {"a-e"}).toString());
    }
    
    //-----------------------------------------------------------------------
    public void testSqueeze_StringString() {
        assertEquals(null, CharSetUtils.squeeze(null, (String) null));
        assertEquals(null, CharSetUtils.squeeze(null, ""));
        
        assertEquals("", CharSetUtils.squeeze("", (String) null));
        assertEquals("", CharSetUtils.squeeze("", ""));
        assertEquals("", CharSetUtils.squeeze("", "a-e"));
        
        assertEquals("hello", CharSetUtils.squeeze("hello", (String) null));
        assertEquals("hello", CharSetUtils.squeeze("hello", ""));
        assertEquals("hello", CharSetUtils.squeeze("hello", "a-e"));
        assertEquals("helo", CharSetUtils.squeeze("hello", "l-p"));
        assertEquals("heloo", CharSetUtils.squeeze("helloo", "l"));
        assertEquals("hello", CharSetUtils.squeeze("helloo", "^l"));
    }
    
    public void testSqueeze_StringStringarray() {
        assertEquals(null, CharSetUtils.squeeze(null, (String[]) null));
        assertEquals(null, CharSetUtils.squeeze(null, new String[0]));
        assertEquals(null, CharSetUtils.squeeze(null, new String[] {null}));
        assertEquals(null, CharSetUtils.squeeze(null, new String[] {"el"}));
        
        assertEquals("", CharSetUtils.squeeze("", (String[]) null));
        assertEquals("", CharSetUtils.squeeze("", new String[0]));
        assertEquals("", CharSetUtils.squeeze("", new String[] {null}));
        assertEquals("", CharSetUtils.squeeze("", new String[] {"a-e"}));
        
        assertEquals("hello", CharSetUtils.squeeze("hello", (String[]) null));
        assertEquals("hello", CharSetUtils.squeeze("hello", new String[0]));
        assertEquals("hello", CharSetUtils.squeeze("hello", new String[] {null}));
        assertEquals("hello", CharSetUtils.squeeze("hello", new String[] {"a-e"}));
        
        assertEquals("helo", CharSetUtils.squeeze("hello", new String[] { "el" }));
        assertEquals("hello", CharSetUtils.squeeze("hello", new String[] { "e" }));
        assertEquals("fofof", CharSetUtils.squeeze("fooffooff", new String[] { "of" }));
        assertEquals("fof", CharSetUtils.squeeze("fooooff", new String[] { "fo" }));
    }

    //-----------------------------------------------------------------------
    public void testCount_StringString() {
        assertEquals(0, CharSetUtils.count(null, (String) null));
        assertEquals(0, CharSetUtils.count(null, ""));
        
        assertEquals(0, CharSetUtils.count("", (String) null));
        assertEquals(0, CharSetUtils.count("", ""));
        assertEquals(0, CharSetUtils.count("", "a-e"));
        
        assertEquals(0, CharSetUtils.count("hello", (String) null));
        assertEquals(0, CharSetUtils.count("hello", ""));
        assertEquals(1, CharSetUtils.count("hello", "a-e"));
        assertEquals(3, CharSetUtils.count("hello", "l-p"));
    }
    
    public void testCount_StringStringarray() {
        assertEquals(0, CharSetUtils.count(null, (String[]) null));
        assertEquals(0, CharSetUtils.count(null, new String[0]));
        assertEquals(0, CharSetUtils.count(null, new String[] {null}));
        assertEquals(0, CharSetUtils.count(null, new String[] {"a-e"}));
        
        assertEquals(0, CharSetUtils.count("", (String[]) null));
        assertEquals(0, CharSetUtils.count("", new String[0]));
        assertEquals(0, CharSetUtils.count("", new String[] {null}));
        assertEquals(0, CharSetUtils.count("", new String[] {"a-e"}));
        
        assertEquals(0, CharSetUtils.count("hello", (String[]) null));
        assertEquals(0, CharSetUtils.count("hello", new String[0]));
        assertEquals(0, CharSetUtils.count("hello", new String[] {null}));
        assertEquals(1, CharSetUtils.count("hello", new String[] {"a-e"}));
        
        assertEquals(3, CharSetUtils.count("hello", new String[] { "el" }));
        assertEquals(0, CharSetUtils.count("hello", new String[] { "x" }));
        assertEquals(2, CharSetUtils.count("hello", new String[] { "e-i" }));
        assertEquals(5, CharSetUtils.count("hello", new String[] { "a-z" }));
        assertEquals(0, CharSetUtils.count("hello", new String[] { "" }));
    }

    //-----------------------------------------------------------------------
    public void testKeep_StringString() {
        assertEquals(null, CharSetUtils.keep(null, (String) null));
        assertEquals(null, CharSetUtils.keep(null, ""));
        
        assertEquals("", CharSetUtils.keep("", (String) null));
        assertEquals("", CharSetUtils.keep("", ""));
        assertEquals("", CharSetUtils.keep("", "a-e"));
        
        assertEquals("", CharSetUtils.keep("hello", (String) null));
        assertEquals("", CharSetUtils.keep("hello", ""));
        assertEquals("", CharSetUtils.keep("hello", "xyz"));
        assertEquals("hello", CharSetUtils.keep("hello", "a-z"));
        assertEquals("hello", CharSetUtils.keep("hello", "oleh"));
        assertEquals("ell", CharSetUtils.keep("hello", "el"));
    }
    
    public void testKeep_StringStringarray() {
        assertEquals(null, CharSetUtils.keep(null, (String[]) null));
        assertEquals(null, CharSetUtils.keep(null, new String[0]));
        assertEquals(null, CharSetUtils.keep(null, new String[] {null}));
        assertEquals(null, CharSetUtils.keep(null, new String[] {"a-e"}));
        
        assertEquals("", CharSetUtils.keep("", (String[]) null));
        assertEquals("", CharSetUtils.keep("", new String[0]));
        assertEquals("", CharSetUtils.keep("", new String[] {null}));
        assertEquals("", CharSetUtils.keep("", new String[] {"a-e"}));
        
        assertEquals("", CharSetUtils.keep("hello", (String[]) null));
        assertEquals("", CharSetUtils.keep("hello", new String[0]));
        assertEquals("", CharSetUtils.keep("hello", new String[] {null}));
        assertEquals("e", CharSetUtils.keep("hello", new String[] {"a-e"}));
        
        assertEquals("e", CharSetUtils.keep("hello", new String[] { "a-e" }));
        assertEquals("ell", CharSetUtils.keep("hello", new String[] { "el" }));
        assertEquals("hello", CharSetUtils.keep("hello", new String[] { "elho" }));
        assertEquals("hello", CharSetUtils.keep("hello", new String[] { "a-z" }));
        assertEquals("----", CharSetUtils.keep("----", new String[] { "-" }));
        assertEquals("ll", CharSetUtils.keep("hello", new String[] { "l" }));
    }

    //-----------------------------------------------------------------------
    public void testDelete_StringString() {
        assertEquals(null, CharSetUtils.delete(null, (String) null));
        assertEquals(null, CharSetUtils.delete(null, ""));
        
        assertEquals("", CharSetUtils.delete("", (String) null));
        assertEquals("", CharSetUtils.delete("", ""));
        assertEquals("", CharSetUtils.delete("", "a-e"));
        
        assertEquals("hello", CharSetUtils.delete("hello", (String) null));
        assertEquals("hello", CharSetUtils.delete("hello", ""));
        assertEquals("hllo", CharSetUtils.delete("hello", "a-e"));
        assertEquals("he", CharSetUtils.delete("hello", "l-p"));
        assertEquals("hello", CharSetUtils.delete("hello", "z"));
    }
    
    public void testDelete_StringStringarray() {
        assertEquals(null, CharSetUtils.delete(null, (String[]) null));
        assertEquals(null, CharSetUtils.delete(null, new String[0]));
        assertEquals(null, CharSetUtils.delete(null, new String[] {null}));
        assertEquals(null, CharSetUtils.delete(null, new String[] {"el"}));
        
        assertEquals("", CharSetUtils.delete("", (String[]) null));
        assertEquals("", CharSetUtils.delete("", new String[0]));
        assertEquals("", CharSetUtils.delete("", new String[] {null}));
        assertEquals("", CharSetUtils.delete("", new String[] {"a-e"}));
        
        assertEquals("hello", CharSetUtils.delete("hello", (String[]) null));
        assertEquals("hello", CharSetUtils.delete("hello", new String[0]));
        assertEquals("hello", CharSetUtils.delete("hello", new String[] {null}));
        assertEquals("hello", CharSetUtils.delete("hello", new String[] {"xyz"}));

        assertEquals("ho", CharSetUtils.delete("hello", new String[] { "el" }));
        assertEquals("", CharSetUtils.delete("hello", new String[] { "elho" }));
        assertEquals("hello", CharSetUtils.delete("hello", new String[] { "" }));
        assertEquals("hello", CharSetUtils.delete("hello", ""));
        assertEquals("", CharSetUtils.delete("hello", new String[] { "a-z" }));
        assertEquals("", CharSetUtils.delete("----", new String[] { "-" }));
        assertEquals("heo", CharSetUtils.delete("hello", new String[] { "l" }));
    }
    
    //-----------------------------------------------------------------------
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
