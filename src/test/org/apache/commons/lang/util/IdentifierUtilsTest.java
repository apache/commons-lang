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

/**
 * Tests the org.apache.commons.lang.util.IdentifierUtils class.
 *
 * @author Stephen Colebourne
 * @version $Id: IdentifierUtilsTest.java,v 1.2 2003/05/16 22:07:38 scolebourne Exp $
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
        LongIdentifierFactory f = IdentifierUtils.LONG_IDENTIFIER_FACTORY;
        assertEquals(new Long(0), f.nextLongIdentifier());
        assertEquals(new Long(1), f.nextLongIdentifier());
        assertEquals(new Long(2), f.nextIdentifier());
        assertEquals(new Long(3), f.nextLongIdentifier());
        assertEquals(new Long(4), IdentifierUtils.nextLongIdentifier());
        assertEquals(new Long(5), f.nextLongIdentifier());
        assertEquals(new Long(6), IdentifierUtils.nextLongIdentifier());
        assertEquals(new Long(7), IdentifierUtils.nextLongIdentifier());
    }

    public void testLongIncrementingNoArgs() {
        LongIdentifierFactory f = IdentifierUtils.longIdentifierFactory();
        assertEquals(new Long(0), f.nextLongIdentifier());
        assertEquals(new Long(1), f.nextLongIdentifier());
        assertTrue(f != IdentifierUtils.LONG_IDENTIFIER_FACTORY);
    }

    public void testLongIncrementingInit() {
        LongIdentifierFactory f = IdentifierUtils.longIdentifierFactory(true, 100);
        assertEquals(new Long(100), f.nextLongIdentifier());
        assertEquals(new Long(101), f.nextLongIdentifier());
    }

    public void testLongIncrementingWrap() {
        LongIdentifierFactory f = IdentifierUtils.longIdentifierFactory(true, Long.MAX_VALUE);
        assertEquals(new Long(Long.MAX_VALUE), f.nextLongIdentifier());
        assertEquals(new Long(Long.MIN_VALUE), f.nextLongIdentifier());
    }

    public void testLongIncrementingNoWrap() {
        LongIdentifierFactory f = IdentifierUtils.longIdentifierFactory(false, Long.MAX_VALUE);
        try {
            f.nextLongIdentifier();
            fail();
        } catch (IllegalStateException ex) {}
    }

    //--------------------------------------------------------------------------

    public void testStringNumericLong() {
        StringIdentifierFactory f = IdentifierUtils.STRING_NUMERIC_IDENTIFIER_FACTORY;
        assertEquals("0", f.nextStringIdentifier());
        assertEquals("1", f.nextStringIdentifier());
        assertEquals("2", f.nextIdentifier());
        assertEquals("3", f.nextStringIdentifier());
        assertEquals("4", IdentifierUtils.nextStringNumericIdentifier());
        assertEquals("5", f.nextStringIdentifier());
        assertEquals("6", IdentifierUtils.nextStringNumericIdentifier());
        assertEquals("7", IdentifierUtils.nextStringNumericIdentifier());
    }

    public void testStringNumericNoArgs() {
        StringIdentifierFactory f = IdentifierUtils.stringNumericIdentifierFactory();
        assertEquals("0", f.nextStringIdentifier());
        assertEquals("1", f.nextStringIdentifier());
        assertTrue(f != IdentifierUtils.STRING_NUMERIC_IDENTIFIER_FACTORY);
    }

    public void testStringNumericInit() {
        StringIdentifierFactory f = IdentifierUtils.stringNumericIdentifierFactory(true, 100);
        assertEquals("100", f.nextStringIdentifier());
        assertEquals("101", f.nextStringIdentifier());
    }

    public void testStringNumericWrap() {
        StringIdentifierFactory f = IdentifierUtils.stringNumericIdentifierFactory(true, Long.MAX_VALUE);
        assertEquals(Long.toString(Long.MAX_VALUE), f.nextStringIdentifier());
        assertEquals(Long.toString(Long.MIN_VALUE), f.nextStringIdentifier());
    }

    public void testStringNumericNoWrap() {
        StringIdentifierFactory f = IdentifierUtils.stringNumericIdentifierFactory(false, Long.MAX_VALUE);
        try {
            f.nextStringIdentifier();
            fail();
        } catch (IllegalStateException ex) { }
    }

    //--------------------------------------------------------------------------

    public void testStringAlphanumeric() {
        StringIdentifierFactory f = IdentifierUtils.STRING_ALPHANUMERIC_IDENTIFIER_FACTORY;
        assertEquals("000000000000001", f.nextStringIdentifier());
        assertEquals("000000000000002", f.nextStringIdentifier());
        assertEquals("000000000000003", f.nextStringIdentifier());
        assertEquals("000000000000004", f.nextStringIdentifier());
        assertEquals("000000000000005", f.nextStringIdentifier());
        assertEquals("000000000000006", f.nextStringIdentifier());
        assertEquals("000000000000007", f.nextStringIdentifier());
        assertEquals("000000000000008", f.nextStringIdentifier());
        assertEquals("000000000000009", f.nextStringIdentifier());
        assertEquals("00000000000000a", f.nextStringIdentifier());
        assertEquals("00000000000000b", f.nextStringIdentifier());
        assertEquals("00000000000000c", f.nextStringIdentifier());
        assertEquals("00000000000000d", IdentifierUtils.nextStringAlphanumericIdentifier());
        assertEquals("00000000000000e", f.nextStringIdentifier());
        assertEquals("00000000000000f", f.nextStringIdentifier());
        assertEquals("00000000000000g", f.nextStringIdentifier());
        assertEquals("00000000000000h", f.nextStringIdentifier());
        assertEquals("00000000000000i", f.nextStringIdentifier());
        assertEquals("00000000000000j", f.nextStringIdentifier());
        assertEquals("00000000000000k", f.nextStringIdentifier());
        assertEquals("00000000000000l", f.nextStringIdentifier());
        assertEquals("00000000000000m", f.nextStringIdentifier());
        assertEquals("00000000000000n", f.nextStringIdentifier());
        assertEquals("00000000000000o", f.nextStringIdentifier());
        assertEquals("00000000000000p", f.nextStringIdentifier());
        assertEquals("00000000000000q", f.nextStringIdentifier());
        assertEquals("00000000000000r", f.nextStringIdentifier());
        assertEquals("00000000000000s", f.nextStringIdentifier());
        assertEquals("00000000000000t", f.nextStringIdentifier());
        assertEquals("00000000000000u", f.nextStringIdentifier());
        assertEquals("00000000000000v", f.nextStringIdentifier());
        assertEquals("00000000000000w", f.nextStringIdentifier());
        assertEquals("00000000000000x", f.nextStringIdentifier());
        assertEquals("00000000000000y", f.nextStringIdentifier());
        assertEquals("00000000000000z", f.nextStringIdentifier());
        assertEquals("000000000000010", f.nextStringIdentifier());
        assertEquals("000000000000011", f.nextStringIdentifier());
        assertEquals("000000000000012", f.nextStringIdentifier());
        assertEquals("000000000000013", f.nextStringIdentifier());
    }

    public void testLongAlphanumericNoArgs() {
        StringIdentifierFactory f = IdentifierUtils.stringAlphanumericIdentifierFactory();
        assertEquals("000000000000001", f.nextStringIdentifier());
        assertEquals("000000000000002", f.nextStringIdentifier());
        assertTrue(f != IdentifierUtils.STRING_ALPHANUMERIC_IDENTIFIER_FACTORY);
    }

    public void testStringAlphanumericWrap() {
        StringIdentifierFactory f = IdentifierUtils.stringAlphanumericIdentifierFactory(true, 1);
        assertEquals("1", f.nextStringIdentifier());
        assertEquals("2", f.nextStringIdentifier());
        assertEquals("3", f.nextStringIdentifier());
        assertEquals("4", f.nextStringIdentifier());
        assertEquals("5", f.nextStringIdentifier());
        assertEquals("6", f.nextStringIdentifier());
        assertEquals("7", f.nextStringIdentifier());
        assertEquals("8", f.nextStringIdentifier());
        assertEquals("9", f.nextStringIdentifier());
        assertEquals("a", f.nextStringIdentifier());
        assertEquals("b", f.nextStringIdentifier());
        assertEquals("c", f.nextStringIdentifier());
        assertEquals("d", f.nextStringIdentifier());
        assertEquals("e", f.nextStringIdentifier());
        assertEquals("f", f.nextStringIdentifier());
        assertEquals("g", f.nextStringIdentifier());
        assertEquals("h", f.nextStringIdentifier());
        assertEquals("i", f.nextStringIdentifier());
        assertEquals("j", f.nextStringIdentifier());
        assertEquals("k", f.nextStringIdentifier());
        assertEquals("l", f.nextStringIdentifier());
        assertEquals("m", f.nextStringIdentifier());
        assertEquals("n", f.nextStringIdentifier());
        assertEquals("o", f.nextStringIdentifier());
        assertEquals("p", f.nextStringIdentifier());
        assertEquals("q", f.nextStringIdentifier());
        assertEquals("r", f.nextStringIdentifier());
        assertEquals("s", f.nextStringIdentifier());
        assertEquals("t", f.nextStringIdentifier());
        assertEquals("u", f.nextStringIdentifier());
        assertEquals("v", f.nextStringIdentifier());
        assertEquals("w", f.nextStringIdentifier());
        assertEquals("x", f.nextStringIdentifier());
        assertEquals("y", f.nextStringIdentifier());
        assertEquals("z", f.nextStringIdentifier());
        assertEquals("0", f.nextStringIdentifier());
    }

    public void testStringAlphanumericNoWrap() {
        StringIdentifierFactory f = IdentifierUtils.stringAlphanumericIdentifierFactory(false, 1);
        assertEquals("1", f.nextStringIdentifier());
        assertEquals("2", f.nextStringIdentifier());
        assertEquals("3", f.nextStringIdentifier());
        assertEquals("4", f.nextStringIdentifier());
        assertEquals("5", f.nextStringIdentifier());
        assertEquals("6", f.nextStringIdentifier());
        assertEquals("7", f.nextStringIdentifier());
        assertEquals("8", f.nextStringIdentifier());
        assertEquals("9", f.nextStringIdentifier());
        assertEquals("a", f.nextStringIdentifier());
        assertEquals("b", f.nextStringIdentifier());
        assertEquals("c", f.nextStringIdentifier());
        assertEquals("d", f.nextStringIdentifier());
        assertEquals("e", f.nextStringIdentifier());
        assertEquals("f", f.nextStringIdentifier());
        assertEquals("g", f.nextStringIdentifier());
        assertEquals("h", f.nextStringIdentifier());
        assertEquals("i", f.nextStringIdentifier());
        assertEquals("j", f.nextStringIdentifier());
        assertEquals("k", f.nextStringIdentifier());
        assertEquals("l", f.nextStringIdentifier());
        assertEquals("m", f.nextStringIdentifier());
        assertEquals("n", f.nextStringIdentifier());
        assertEquals("o", f.nextStringIdentifier());
        assertEquals("p", f.nextStringIdentifier());
        assertEquals("q", f.nextStringIdentifier());
        assertEquals("r", f.nextStringIdentifier());
        assertEquals("s", f.nextStringIdentifier());
        assertEquals("t", f.nextStringIdentifier());
        assertEquals("u", f.nextStringIdentifier());
        assertEquals("v", f.nextStringIdentifier());
        assertEquals("w", f.nextStringIdentifier());
        assertEquals("x", f.nextStringIdentifier());
        assertEquals("y", f.nextStringIdentifier());
        assertEquals("z", f.nextStringIdentifier());
        try {
            f.nextStringIdentifier();
            fail();
        } catch (IllegalStateException ex) {}
    }

    //--------------------------------------------------------------------------

    public void testStringSession() {
        StringIdentifierFactory f = IdentifierUtils.STRING_SESSION_IDENTIFIER_FACTORY;
        assertTrue(f != IdentifierUtils.stringSessionIdentifierFactory());
        
        String a = (String) f.nextStringIdentifier();
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
