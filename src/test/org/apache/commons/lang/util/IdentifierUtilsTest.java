/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002 The Apache Software Foundation.  All rights
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
package org.apache.commons.lang.util;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.commons.lang.functor.Factory;
import org.apache.commons.lang.functor.FactoryException;
/**
 * Tests the org.apache.commons.lang.util.IdentifierUtils class.
 *
 * @author Stephen Colebourne
 * @version $Id: IdentifierUtilsTest.java,v 1.1 2002/12/29 21:35:03 scolebourne Exp $
 */
public class IdentifierUtilsTest extends junit.framework.TestCase {

    /**
     * Construct
     */
    public IdentifierUtilsTest(String name) {
        super(name);
    }

    /**
     * Return class aa a test suite.
     */
    public static Test suite() {
        TestSuite suite = new TestSuite(IdentifierUtilsTest.class);
        suite.setName("IdentifierUtils Tests");
        return suite;
    }

    //--------------------------------------------------------------------------

    public void testLongIncrementing() {
        Factory f = IdentifierUtils.LONG_IDENTIFIER_FACTORY;
        assertEquals(new Long(0), f.create());
        assertEquals(new Long(1), f.create());
        assertEquals(new Long(2), f.create());
        assertEquals(new Long(3), f.create());
        assertEquals(new Long(4), IdentifierUtils.nextLongIdentifier());
        assertEquals(new Long(5), f.create());
        assertEquals(new Long(6), IdentifierUtils.nextLongIdentifier());
        assertEquals(new Long(7), IdentifierUtils.nextLongIdentifier());
    }

    public void testLongIncrementingNoArgs() {
        Factory f = IdentifierUtils.longIdentifierFactory();
        assertEquals(new Long(0), f.create());
        assertEquals(new Long(1), f.create());
        assertTrue(f != IdentifierUtils.LONG_IDENTIFIER_FACTORY);
    }

    public void testLongIncrementingInit() {
        Factory f = IdentifierUtils.longIdentifierFactory(true, 100);
        assertEquals(new Long(100), f.create());
        assertEquals(new Long(101), f.create());
    }

    public void testLongIncrementingWrap() {
        Factory f = IdentifierUtils.longIdentifierFactory(true, Long.MAX_VALUE);
        assertEquals(new Long(Long.MAX_VALUE), f.create());
        assertEquals(new Long(Long.MIN_VALUE), f.create());
    }

    public void testLongIncrementingNoWrap() {
        Factory f = IdentifierUtils.longIdentifierFactory(false, Long.MAX_VALUE);
        try {
            f.create();
            fail();
        } catch (FactoryException ex) {}
    }

    //--------------------------------------------------------------------------

    public void testStringNumericLong() {
        Factory f = IdentifierUtils.STRING_NUMERIC_IDENTIFIER_FACTORY;
        assertEquals("0", f.create());
        assertEquals("1", f.create());
        assertEquals("2", f.create());
        assertEquals("3", f.create());
        assertEquals("4", IdentifierUtils.nextStringNumericIdentifier());
        assertEquals("5", f.create());
        assertEquals("6", IdentifierUtils.nextStringNumericIdentifier());
        assertEquals("7", IdentifierUtils.nextStringNumericIdentifier());
    }

    public void testStringNumericNoArgs() {
        Factory f = IdentifierUtils.stringNumericIdentifierFactory();
        assertEquals("0", f.create());
        assertEquals("1", f.create());
        assertTrue(f != IdentifierUtils.STRING_NUMERIC_IDENTIFIER_FACTORY);
    }

    public void testStringNumericInit() {
        Factory f = IdentifierUtils.stringNumericIdentifierFactory(true, 100);
        assertEquals("100", f.create());
        assertEquals("101", f.create());
    }

    public void testStringNumericWrap() {
        Factory f = IdentifierUtils.stringNumericIdentifierFactory(true, Long.MAX_VALUE);
        assertEquals(Long.toString(Long.MAX_VALUE), f.create());
        assertEquals(Long.toString(Long.MIN_VALUE), f.create());
    }

    public void testStringNumericNoWrap() {
        Factory f = IdentifierUtils.stringNumericIdentifierFactory(false, Long.MAX_VALUE);
        try {
            f.create();
            fail();
        } catch (FactoryException ex) { }
    }

    //--------------------------------------------------------------------------

    public void testStringAlphanumeric() {
        Factory f = IdentifierUtils.STRING_ALPHANUMERIC_IDENTIFIER_FACTORY;
        assertEquals("000000000000001", f.create());
        assertEquals("000000000000002", f.create());
        assertEquals("000000000000003", f.create());
        assertEquals("000000000000004", f.create());
        assertEquals("000000000000005", f.create());
        assertEquals("000000000000006", f.create());
        assertEquals("000000000000007", f.create());
        assertEquals("000000000000008", f.create());
        assertEquals("000000000000009", f.create());
        assertEquals("00000000000000a", f.create());
        assertEquals("00000000000000b", f.create());
        assertEquals("00000000000000c", f.create());
        assertEquals("00000000000000d", IdentifierUtils.nextStringAlphanumericIdentifier());
        assertEquals("00000000000000e", f.create());
        assertEquals("00000000000000f", f.create());
        assertEquals("00000000000000g", f.create());
        assertEquals("00000000000000h", f.create());
        assertEquals("00000000000000i", f.create());
        assertEquals("00000000000000j", f.create());
        assertEquals("00000000000000k", f.create());
        assertEquals("00000000000000l", f.create());
        assertEquals("00000000000000m", f.create());
        assertEquals("00000000000000n", f.create());
        assertEquals("00000000000000o", f.create());
        assertEquals("00000000000000p", f.create());
        assertEquals("00000000000000q", f.create());
        assertEquals("00000000000000r", f.create());
        assertEquals("00000000000000s", f.create());
        assertEquals("00000000000000t", f.create());
        assertEquals("00000000000000u", f.create());
        assertEquals("00000000000000v", f.create());
        assertEquals("00000000000000w", f.create());
        assertEquals("00000000000000x", f.create());
        assertEquals("00000000000000y", f.create());
        assertEquals("00000000000000z", f.create());
        assertEquals("000000000000010", f.create());
        assertEquals("000000000000011", f.create());
        assertEquals("000000000000012", f.create());
        assertEquals("000000000000013", f.create());
    }

    public void testLongAlphanumericNoArgs() {
        Factory f = IdentifierUtils.stringAlphanumericIdentifierFactory();
        assertEquals("000000000000001", f.create());
        assertEquals("000000000000002", f.create());
        assertTrue(f != IdentifierUtils.STRING_ALPHANUMERIC_IDENTIFIER_FACTORY);
    }

    public void testStringAlphanumericWrap() {
        Factory f = IdentifierUtils.stringAlphanumericIdentifierFactory(true, 1);
        assertEquals("1", f.create());
        assertEquals("2", f.create());
        assertEquals("3", f.create());
        assertEquals("4", f.create());
        assertEquals("5", f.create());
        assertEquals("6", f.create());
        assertEquals("7", f.create());
        assertEquals("8", f.create());
        assertEquals("9", f.create());
        assertEquals("a", f.create());
        assertEquals("b", f.create());
        assertEquals("c", f.create());
        assertEquals("d", f.create());
        assertEquals("e", f.create());
        assertEquals("f", f.create());
        assertEquals("g", f.create());
        assertEquals("h", f.create());
        assertEquals("i", f.create());
        assertEquals("j", f.create());
        assertEquals("k", f.create());
        assertEquals("l", f.create());
        assertEquals("m", f.create());
        assertEquals("n", f.create());
        assertEquals("o", f.create());
        assertEquals("p", f.create());
        assertEquals("q", f.create());
        assertEquals("r", f.create());
        assertEquals("s", f.create());
        assertEquals("t", f.create());
        assertEquals("u", f.create());
        assertEquals("v", f.create());
        assertEquals("w", f.create());
        assertEquals("x", f.create());
        assertEquals("y", f.create());
        assertEquals("z", f.create());
        assertEquals("0", f.create());
    }

    public void testStringAlphanumericNoWrap() {
        Factory f = IdentifierUtils.stringAlphanumericIdentifierFactory(false, 1);
        assertEquals("1", f.create());
        assertEquals("2", f.create());
        assertEquals("3", f.create());
        assertEquals("4", f.create());
        assertEquals("5", f.create());
        assertEquals("6", f.create());
        assertEquals("7", f.create());
        assertEquals("8", f.create());
        assertEquals("9", f.create());
        assertEquals("a", f.create());
        assertEquals("b", f.create());
        assertEquals("c", f.create());
        assertEquals("d", f.create());
        assertEquals("e", f.create());
        assertEquals("f", f.create());
        assertEquals("g", f.create());
        assertEquals("h", f.create());
        assertEquals("i", f.create());
        assertEquals("j", f.create());
        assertEquals("k", f.create());
        assertEquals("l", f.create());
        assertEquals("m", f.create());
        assertEquals("n", f.create());
        assertEquals("o", f.create());
        assertEquals("p", f.create());
        assertEquals("q", f.create());
        assertEquals("r", f.create());
        assertEquals("s", f.create());
        assertEquals("t", f.create());
        assertEquals("u", f.create());
        assertEquals("v", f.create());
        assertEquals("w", f.create());
        assertEquals("x", f.create());
        assertEquals("y", f.create());
        assertEquals("z", f.create());
        try {
            f.create();
            fail();
        } catch (FactoryException ex) {}
    }

    //--------------------------------------------------------------------------

    public void testStringSession() {
        Factory f = IdentifierUtils.STRING_SESSION_IDENTIFIER_FACTORY;
        assertTrue(f != IdentifierUtils.stringSessionIdentifierFactory());
        
        String a = (String) f.create();
        String b = (String) IdentifierUtils.nextStringSessionIdentifier();
        assertTrue(a.length() >= 10);
        assertTrue(b.length() >= 10);
        // could fail, but unlikely
        assertTrue(a.substring(6, 9) != b.substring(6, 9));
        assertEquals("0", a.substring(9));
        assertEquals("1", b.substring(9));
    }

    //--------------------------------------------------------------------------

}
