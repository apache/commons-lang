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
package org.apache.commons.lang.math;

import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * Test cases for the {@link LongRange} class.
 *
 * @author Stephen Colebourne
 * @version $Id: LongRangeTest.java,v 1.2 2003/06/08 14:19:43 scolebourne Exp $
 */
public final class LongRangeTest extends AbstractRangeTest {

    public LongRangeTest(String name) {
        super(name);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(LongRangeTest.class);
        suite.setName("LongRange Tests");
        return suite;
    }
    
    public void setUp() {
        super.setUp();
        tenToTwenty = new LongRange(long10, long20);
        otherRange = new NumberRange(ten, twenty);
    }

    protected Range createRange(Integer integer1, Integer integer2) {
        return new LongRange(integer1, integer2);
    }
    protected Range createRange(Integer integer) {
        return new NumberRange(integer);
    }
    
    //--------------------------------------------------------------------------

    public void testConstructor1a() {
        LongRange nr = new LongRange(8L);
        assertEquals(long8, nr.getMinimumNumber());
        assertEquals(long8, nr.getMaximumNumber());
    }
    
    public void testConstructor1b() {
        LongRange nr = new LongRange(long8);
        assertSame(long8, nr.getMinimumNumber());
        assertSame(long8, nr.getMaximumNumber());
        
        Range r = new LongRange(nonComparable);
        
        try {
            new LongRange(null);
            fail();
        } catch (IllegalArgumentException ex) {}
    }
    
    public void testConstructor2a() {
        LongRange nr = new LongRange(8L, 10L);
        assertEquals(long8, nr.getMinimumNumber());
        assertEquals(long10, nr.getMaximumNumber());
        
        nr = new LongRange(10L, 8L);
        assertEquals(long8, nr.getMinimumNumber());
        assertEquals(long10, nr.getMaximumNumber());
    }

    public void testConstructor2b() {
        LongRange nr = new LongRange(long8, long10);
        assertSame(long8, nr.getMinimumNumber());
        assertSame(long10, nr.getMaximumNumber());
        
        nr = new LongRange(long10, long8);
        assertSame(long8, nr.getMinimumNumber());
        assertSame(long10, nr.getMaximumNumber());
        
        nr = new LongRange(long8, long10);
        assertSame(long8, nr.getMinimumNumber());
        assertEquals(long10, nr.getMaximumNumber());
        
        // not null
        try {
            new LongRange(long8, null);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            new LongRange(null, long8);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            new LongRange(null, null);
            fail();
        } catch (IllegalArgumentException ex) {}
    }

    //--------------------------------------------------------------------------

    public void testIncludesNumber() {
        assertEquals(false, tenToTwenty.includesNumber(null));
        assertEquals(true, tenToTwenty.includesNumber(nonComparable));
        
        assertEquals(false, tenToTwenty.includesNumber(five));
        assertEquals(true, tenToTwenty.includesNumber(ten));
        assertEquals(true, tenToTwenty.includesNumber(fifteen));
        assertEquals(true, tenToTwenty.includesNumber(twenty));
        assertEquals(false, tenToTwenty.includesNumber(twentyFive));
        
        assertEquals(false, tenToTwenty.includesNumber(long8));
        assertEquals(true, tenToTwenty.includesNumber(long10));
        assertEquals(true, tenToTwenty.includesNumber(long12));
        assertEquals(true, tenToTwenty.includesNumber(long20));
        assertEquals(false, tenToTwenty.includesNumber(long21));
        
        assertEquals(false, tenToTwenty.includesNumber(double8));
        assertEquals(true, tenToTwenty.includesNumber(double10));
        assertEquals(true, tenToTwenty.includesNumber(double12));
        assertEquals(true, tenToTwenty.includesNumber(double20));
        assertEquals(false, tenToTwenty.includesNumber(double21));
        
        assertEquals(false, tenToTwenty.includesNumber(float8));
        assertEquals(true, tenToTwenty.includesNumber(float10));
        assertEquals(true, tenToTwenty.includesNumber(float12));
        assertEquals(true, tenToTwenty.includesNumber(float20));
        assertEquals(false, tenToTwenty.includesNumber(float21));
    }

    public void testIncludesLongBig() {
        LongRange big = new LongRange(Long.MAX_VALUE, Long.MAX_VALUE- 2);
        assertEquals(true, big.includesLong(Long.MAX_VALUE - 1));
        assertEquals(false, big.includesLong(Long.MAX_VALUE - 3));
    }

    //--------------------------------------------------------------------------
    
}
