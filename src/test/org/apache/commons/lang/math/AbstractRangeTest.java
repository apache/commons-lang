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

import junit.framework.TestCase;

/**
 * Test cases for the {@link Range} classes.
 *
 * @author Stephen Colebourne
 * @version $Id: AbstractRangeTest.java,v 1.2 2003/06/08 14:19:43 scolebourne Exp $
 */
public abstract class AbstractRangeTest extends TestCase {

    protected Range tenToTwenty;
    protected Range otherRange;
    
    protected Integer five;
    protected Integer ten;
    protected Integer twelve;
    protected Integer fifteen;
    protected Integer twenty;
    protected Integer twentyFive;
    protected Long long8;
    protected Long long10;
    protected Long long12;
    protected Long long20;
    protected Long long21;
    protected Double double8;
    protected Double double10;
    protected Double double12;
    protected Double double20;
    protected Double double21;
    protected Float float8;
    protected Float float10;
    protected Float float12;
    protected Float float20;
    protected Float float21;
    
    private static class InnerNumber extends Number {
        public double doubleValue() {
            return 12d;
        }
        public float floatValue() {
            return 12f;
        }
        public int intValue() {
            return 12;
        }
        public long longValue() {
            return 12L;
        }

    }
    
    protected InnerNumber nonComparable = new InnerNumber();


    public AbstractRangeTest(String name) {
        super(name);
    }

    public void setUp() {
        five        = new Integer(5);
        ten         = new Integer(10);
        twelve      = new Integer(12);
        fifteen     = new Integer(15);
        twenty      = new Integer(20);
        twentyFive  = new Integer(25);
        long8       = new Long(8);
        long10      = new Long(10);
        long12      = new Long(12);
        long20      = new Long(20);
        long21      = new Long(21);
        double8     = new Double(8);
        double10    = new Double(10);
        double12    = new Double(12);
        double20    = new Double(20);
        double21    = new Double(21);
        float8      = new Float(8);
        float10     = new Float(10);
        float12     = new Float(12);
        float20     = new Float(20);
        float21     = new Float(21);
    }

    //--------------------------------------------------------------------------

    public void testGetMinimum() {
        assertEquals(10L, tenToTwenty.getMinimumLong());
        assertEquals(10, tenToTwenty.getMinimumInteger());
        assertEquals(10d, tenToTwenty.getMinimumDouble(), 0.00001d);
        assertEquals(10f, tenToTwenty.getMinimumFloat(), 0.00001f);
    }
    
    public void testGetMaximum() {
        assertEquals(20L, tenToTwenty.getMaximumLong());
        assertEquals(20, tenToTwenty.getMaximumInteger());
        assertEquals(20d, tenToTwenty.getMaximumDouble(), 0.00001d);
        assertEquals(20f, tenToTwenty.getMaximumFloat(), 0.00001f);
    }

    //--------------------------------------------------------------------------

    public void testIncludesLong() {
        assertEquals(false, tenToTwenty.includesLong(null));
        assertEquals(true, tenToTwenty.includesLong(nonComparable));
        
        assertEquals(false, tenToTwenty.includesLong(five));
        assertEquals(true, tenToTwenty.includesLong(ten));
        assertEquals(true, tenToTwenty.includesLong(fifteen));
        assertEquals(true, tenToTwenty.includesLong(twenty));
        assertEquals(false, tenToTwenty.includesLong(twentyFive));
        
        assertEquals(false, tenToTwenty.includesLong(long8));
        assertEquals(true, tenToTwenty.includesLong(long10));
        assertEquals(true, tenToTwenty.includesLong(long12));
        assertEquals(true, tenToTwenty.includesLong(long20));
        assertEquals(false, tenToTwenty.includesLong(long21));
        
        assertEquals(false, tenToTwenty.includesLong(double8));
        assertEquals(true, tenToTwenty.includesLong(double10));
        assertEquals(true, tenToTwenty.includesLong(double12));
        assertEquals(true, tenToTwenty.includesLong(double20));
        assertEquals(false, tenToTwenty.includesLong(double21));
        
        assertEquals(false, tenToTwenty.includesLong(float8));
        assertEquals(true, tenToTwenty.includesLong(float10));
        assertEquals(true, tenToTwenty.includesLong(float12));
        assertEquals(true, tenToTwenty.includesLong(float20));
        assertEquals(false, tenToTwenty.includesLong(float21));
        
        assertEquals(false, tenToTwenty.includesLong(9L));
        assertEquals(true, tenToTwenty.includesLong(10L));
        assertEquals(true, tenToTwenty.includesLong(15L));
        assertEquals(true, tenToTwenty.includesLong(20L));
        assertEquals(false, tenToTwenty.includesLong(21L));
    }
        
    public void testIncludesInteger() {
        assertEquals(false, tenToTwenty.includesInteger(null));
        assertEquals(true, tenToTwenty.includesInteger(nonComparable));
        
        assertEquals(false, tenToTwenty.includesInteger(five));
        assertEquals(true, tenToTwenty.includesInteger(ten));
        assertEquals(true, tenToTwenty.includesInteger(fifteen));
        assertEquals(true, tenToTwenty.includesInteger(twenty));
        assertEquals(false, tenToTwenty.includesInteger(twentyFive));
        
        assertEquals(false, tenToTwenty.includesInteger(long8));
        assertEquals(true, tenToTwenty.includesInteger(long10));
        assertEquals(true, tenToTwenty.includesInteger(long12));
        assertEquals(true, tenToTwenty.includesInteger(long20));
        assertEquals(false, tenToTwenty.includesInteger(long21));
        
        assertEquals(false, tenToTwenty.includesInteger(double8));
        assertEquals(true, tenToTwenty.includesInteger(double10));
        assertEquals(true, tenToTwenty.includesInteger(double12));
        assertEquals(true, tenToTwenty.includesInteger(double20));
        assertEquals(false, tenToTwenty.includesInteger(double21));
        
        assertEquals(false, tenToTwenty.includesInteger(float8));
        assertEquals(true, tenToTwenty.includesInteger(float10));
        assertEquals(true, tenToTwenty.includesInteger(float12));
        assertEquals(true, tenToTwenty.includesInteger(float20));
        assertEquals(false, tenToTwenty.includesInteger(float21));
        
        assertEquals(false, tenToTwenty.includesInteger(9));
        assertEquals(true, tenToTwenty.includesInteger(10));
        assertEquals(true, tenToTwenty.includesInteger(15));
        assertEquals(true, tenToTwenty.includesInteger(20));
        assertEquals(false, tenToTwenty.includesInteger(21));
    }

    public void testIncludesDouble() {
        assertEquals(false, tenToTwenty.includesDouble(null));
        assertEquals(true, tenToTwenty.includesDouble(nonComparable));
        
        assertEquals(false, tenToTwenty.includesDouble(five));
        assertEquals(true, tenToTwenty.includesDouble(ten));
        assertEquals(true, tenToTwenty.includesDouble(fifteen));
        assertEquals(true, tenToTwenty.includesDouble(twenty));
        assertEquals(false, tenToTwenty.includesDouble(twentyFive));
        
        assertEquals(false, tenToTwenty.includesDouble(long8));
        assertEquals(true, tenToTwenty.includesDouble(long10));
        assertEquals(true, tenToTwenty.includesDouble(long12));
        assertEquals(true, tenToTwenty.includesDouble(long20));
        assertEquals(false, tenToTwenty.includesDouble(long21));
        
        assertEquals(false, tenToTwenty.includesDouble(double8));
        assertEquals(true, tenToTwenty.includesDouble(double10));
        assertEquals(true, tenToTwenty.includesDouble(double12));
        assertEquals(true, tenToTwenty.includesDouble(double20));
        assertEquals(false, tenToTwenty.includesDouble(double21));
        
        assertEquals(false, tenToTwenty.includesDouble(float8));
        assertEquals(true, tenToTwenty.includesDouble(float10));
        assertEquals(true, tenToTwenty.includesDouble(float12));
        assertEquals(true, tenToTwenty.includesDouble(float20));
        assertEquals(false, tenToTwenty.includesDouble(float21));
        
        assertEquals(false, tenToTwenty.includesDouble(9d));
        assertEquals(true, tenToTwenty.includesDouble(10d));
        assertEquals(true, tenToTwenty.includesDouble(15d));
        assertEquals(true, tenToTwenty.includesDouble(20d));
        assertEquals(false, tenToTwenty.includesDouble(21d));
    }

    public void testIncludesFloat() {
        assertEquals(false, tenToTwenty.includesFloat(null));
        assertEquals(true, tenToTwenty.includesFloat(nonComparable));
        
        assertEquals(false, tenToTwenty.includesFloat(five));
        assertEquals(true, tenToTwenty.includesFloat(ten));
        assertEquals(true, tenToTwenty.includesFloat(fifteen));
        assertEquals(true, tenToTwenty.includesFloat(twenty));
        assertEquals(false, tenToTwenty.includesFloat(twentyFive));
        
        assertEquals(false, tenToTwenty.includesFloat(long8));
        assertEquals(true, tenToTwenty.includesFloat(long10));
        assertEquals(true, tenToTwenty.includesFloat(long12));
        assertEquals(true, tenToTwenty.includesFloat(long20));
        assertEquals(false, tenToTwenty.includesFloat(long21));
        
        assertEquals(false, tenToTwenty.includesFloat(double8));
        assertEquals(true, tenToTwenty.includesFloat(double10));
        assertEquals(true, tenToTwenty.includesFloat(double12));
        assertEquals(true, tenToTwenty.includesFloat(double20));
        assertEquals(false, tenToTwenty.includesFloat(double21));
        
        assertEquals(false, tenToTwenty.includesFloat(float8));
        assertEquals(true, tenToTwenty.includesFloat(float10));
        assertEquals(true, tenToTwenty.includesFloat(float12));
        assertEquals(true, tenToTwenty.includesFloat(float20));
        assertEquals(false, tenToTwenty.includesFloat(float21));
        
        assertEquals(false, tenToTwenty.includesFloat(9f));
        assertEquals(true, tenToTwenty.includesFloat(10f));
        assertEquals(true, tenToTwenty.includesFloat(15f));
        assertEquals(true, tenToTwenty.includesFloat(20f));
        assertEquals(false, tenToTwenty.includesFloat(21f));
    }

    //--------------------------------------------------------------------------

    public void testIncludesRange() {
        assertEquals(false, tenToTwenty.includesRange(createRange(five, five)));
        assertEquals(false, tenToTwenty.includesRange(createRange(five, ten)));
        assertEquals(false, tenToTwenty.includesRange(createRange(five, twelve)));
        assertEquals(false, tenToTwenty.includesRange(createRange(five, fifteen)));
        assertEquals(false, tenToTwenty.includesRange(createRange(five, twenty)));
        assertEquals(false, tenToTwenty.includesRange(createRange(five, twentyFive)));
        
        assertEquals(true, tenToTwenty.includesRange(createRange(ten, ten)));
        assertEquals(true, tenToTwenty.includesRange(createRange(ten, twelve)));
        assertEquals(true, tenToTwenty.includesRange(createRange(ten, fifteen)));
        assertEquals(true, tenToTwenty.includesRange(createRange(ten, twenty)));
        assertEquals(false, tenToTwenty.includesRange(createRange(ten, twentyFive)));
        
        assertEquals(true, tenToTwenty.includesRange(createRange(twelve, twelve)));
        assertEquals(true, tenToTwenty.includesRange(createRange(twelve, fifteen)));
        assertEquals(true, tenToTwenty.includesRange(createRange(twelve, twenty)));
        assertEquals(false, tenToTwenty.includesRange(createRange(twelve, twentyFive)));
        
        assertEquals(true, tenToTwenty.includesRange(createRange(fifteen, fifteen)));
        assertEquals(true, tenToTwenty.includesRange(createRange(fifteen, twenty)));
        assertEquals(false, tenToTwenty.includesRange(createRange(fifteen, twentyFive)));
        
        assertEquals(true, tenToTwenty.includesRange(createRange(twenty, twenty)));
        assertEquals(false, tenToTwenty.includesRange(createRange(twenty, twentyFive)));
        
        assertEquals(false, tenToTwenty.includesRange(createRange(twentyFive, twentyFive)));
    }

    public void testOverlapsRange() {
        assertEquals(false, tenToTwenty.overlapsRange(createRange(five, five)));
        assertEquals(true, tenToTwenty.overlapsRange(createRange(five, ten)));
        assertEquals(true, tenToTwenty.overlapsRange(createRange(five, twelve)));
        assertEquals(true, tenToTwenty.overlapsRange(createRange(five, fifteen)));
        assertEquals(true, tenToTwenty.overlapsRange(createRange(five, twenty)));
        assertEquals(true, tenToTwenty.overlapsRange(createRange(five, twentyFive)));
        
        assertEquals(true, tenToTwenty.overlapsRange(createRange(ten, ten)));
        assertEquals(true, tenToTwenty.overlapsRange(createRange(ten, twelve)));
        assertEquals(true, tenToTwenty.overlapsRange(createRange(ten, fifteen)));
        assertEquals(true, tenToTwenty.overlapsRange(createRange(ten, twenty)));
        assertEquals(true, tenToTwenty.overlapsRange(createRange(ten, twentyFive)));
        
        assertEquals(true, tenToTwenty.overlapsRange(createRange(twelve, twelve)));
        assertEquals(true, tenToTwenty.overlapsRange(createRange(twelve, fifteen)));
        assertEquals(true, tenToTwenty.overlapsRange(createRange(twelve, twenty)));
        assertEquals(true, tenToTwenty.overlapsRange(createRange(twelve, twentyFive)));
        
        assertEquals(true, tenToTwenty.overlapsRange(createRange(fifteen, fifteen)));
        assertEquals(true, tenToTwenty.overlapsRange(createRange(fifteen, twenty)));
        assertEquals(true, tenToTwenty.overlapsRange(createRange(fifteen, twentyFive)));
        
        assertEquals(true, tenToTwenty.overlapsRange(createRange(twenty, twenty)));
        assertEquals(true, tenToTwenty.overlapsRange(createRange(twenty, twentyFive)));
        
        assertEquals(false, tenToTwenty.overlapsRange(createRange(twentyFive, twentyFive)));
    }

    //--------------------------------------------------------------------------

    public void testEquals() {
        assertEquals(false, tenToTwenty.equals(createRange(ten, fifteen)));
        assertEquals(false, tenToTwenty.equals(createRange(ten, twentyFive)));
        
        assertEquals(false, tenToTwenty.equals(createRange(fifteen, twenty)));
        assertEquals(false, tenToTwenty.equals(createRange(five, twenty)));
        
        assertEquals(false, tenToTwenty.equals(createRange(five, ten)));
        assertEquals(false, tenToTwenty.equals(createRange(ten)));

        assertEquals(true, tenToTwenty.equals(createRange(ten, twenty)));
        assertEquals(true, tenToTwenty.equals(createRange(twenty, ten)));
        
        assertEquals(false, tenToTwenty.equals(null));
        assertEquals(false, tenToTwenty.equals(new Object()));
        assertEquals(false, tenToTwenty.equals(otherRange));
    }

    public void testHashCode() {
        assertEquals(tenToTwenty.hashCode(), tenToTwenty.hashCode());
        assertTrue(tenToTwenty.hashCode() != 0);
    }
    
    public void testToString() {
        assertEquals("Range[10,20]", tenToTwenty.toString());
        assertEquals("Range[-20,-10]", createRange(new Integer(-20), new Integer(-10)).toString());
    }


    protected abstract Range createRange(Integer integer);
    protected abstract Range createRange(Integer integer1, Integer integer2);

}
