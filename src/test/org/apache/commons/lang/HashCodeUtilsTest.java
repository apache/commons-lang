package org.apache.commons.lang;

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

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;
/**
 * Unit tests {@link org.apache.commons.lang.ObjectUtils}.
 *
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @author <a href="mailto:ridesmet@users.sourceforge.net">Ringo De Smet</a>
 * @version $Id: HashCodeUtilsTest.java,v 1.1 2002/08/10 12:13:10 scolebourne Exp $
 */
public class HashCodeUtilsTest extends TestCase {

    public HashCodeUtilsTest(String name) {
        super(name);
    }

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(HashCodeUtilsTest.class);
        suite.setName("HashCodeUtils Tests");
        return suite;
    }

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    //-----------------------------------------------------------------------

    public void testObject() {
        Object obj = null;
        assertEquals(17 * 37, HashCodeUtils.buildHashCode(17, obj));
        obj = new Object();
        assertEquals(17 * 37 + obj.hashCode(), HashCodeUtils.buildHashCode(17, obj));
    }

    public void testLong() {
        assertEquals(17 * 37, HashCodeUtils.buildHashCode(17, (long) 0L));
        assertEquals(17 * 37 + (int) (123456789L ^ (123456789L >> 32)), HashCodeUtils.buildHashCode(17, (long) 123456789L));
    }

    public void testInt() {
        assertEquals(17 * 37, HashCodeUtils.buildHashCode(17, (int) 0));
        assertEquals(17 * 37 + 123456, HashCodeUtils.buildHashCode(17, (int) 123456));
    }

    public void testShort() {
        assertEquals(17 * 37, HashCodeUtils.buildHashCode(17, (short) 0));
        assertEquals(17 * 37 + 12345, HashCodeUtils.buildHashCode(17, (short) 12345));
    }

    public void testChar() {
        assertEquals(17 * 37, HashCodeUtils.buildHashCode(17, (char) 0));
        assertEquals(17 * 37 + 1234, HashCodeUtils.buildHashCode(17, (char) 1234));
    }

    public void testByte() {
        assertEquals(17 * 37, HashCodeUtils.buildHashCode(17, (byte) 0));
        assertEquals(17 * 37 + 123, HashCodeUtils.buildHashCode(17, (byte) 123));
    }

    public void testDouble() {
        assertEquals(17 * 37, HashCodeUtils.buildHashCode(17, (double) 0d));
        double d = 1234567.89;
        long l = Double.doubleToLongBits(d);
        assertEquals(17 * 37 + (int) (l ^ (l >> 32)), HashCodeUtils.buildHashCode(17, d));
    }

    public void testFloat() {
        assertEquals(17 * 37, HashCodeUtils.buildHashCode(17, (float) 0f));
        float f = 1234.89f;
        int i = Float.floatToIntBits(f);
        assertEquals(17 * 37 + i, HashCodeUtils.buildHashCode(17, f));
    }

    public void testBoolean() {
        assertEquals(17 * 37 + 0, HashCodeUtils.buildHashCode(17, true));
        assertEquals(17 * 37 + 1, HashCodeUtils.buildHashCode(17, false));
    }

    public void testObjectArray() {
        assertEquals(17 * 37, HashCodeUtils.buildHashCode(17, (Object[]) null));
        Object[] obj = new Object[2];
        assertEquals((17 * 37) * 37 , HashCodeUtils.buildHashCode(17, obj));
        obj[0] = new Object();
        assertEquals((17 * 37 + obj[0].hashCode()) * 37, HashCodeUtils.buildHashCode(17, obj));
        obj[1] = new Object();
        assertEquals( (17 * 37 + obj[0].hashCode()) * 37 + obj[1].hashCode(), HashCodeUtils.buildHashCode(17, obj));
    }

    public void testLongArray() {
        assertEquals(17 * 37, HashCodeUtils.buildHashCode(17, (long[]) null));
        long[] obj = new long[2];
        assertEquals((17 * 37) * 37 , HashCodeUtils.buildHashCode(17, obj));
        obj[0] = 5L;
        int h1 = (int) (5L ^ (5L >> 32));
        assertEquals((17 * 37 + h1) * 37, HashCodeUtils.buildHashCode(17, obj));
        obj[1] = 6L;
        int h2 = (int) (6L ^ (6L >> 32));
        assertEquals( (17 * 37 + h1) * 37 + h2, HashCodeUtils.buildHashCode(17, obj));
    }

    public void testIntArray() {
        assertEquals(17 * 37, HashCodeUtils.buildHashCode(17, (int[]) null));
        int[] obj = new int[2];
        assertEquals((17 * 37) * 37 , HashCodeUtils.buildHashCode(17, obj));
        obj[0] = 5;
        assertEquals((17 * 37 + 5) * 37, HashCodeUtils.buildHashCode(17, obj));
        obj[1] = 6;
        assertEquals( (17 * 37 + 5) * 37 + 6, HashCodeUtils.buildHashCode(17, obj));
    }

    public void testShortArray() {
        assertEquals(17 * 37, HashCodeUtils.buildHashCode(17, (short[]) null));
        short[] obj = new short[2];
        assertEquals((17 * 37) * 37 , HashCodeUtils.buildHashCode(17, obj));
        obj[0] = (short) 5;
        assertEquals((17 * 37 + 5) * 37, HashCodeUtils.buildHashCode(17, obj));
        obj[1] = (short) 6;
        assertEquals( (17 * 37 + 5) * 37 + 6, HashCodeUtils.buildHashCode(17, obj));
    }

    public void testCharArray() {
        assertEquals(17 * 37, HashCodeUtils.buildHashCode(17, (char[]) null));
        char[] obj = new char[2];
        assertEquals((17 * 37) * 37 , HashCodeUtils.buildHashCode(17, obj));
        obj[0] = (char) 5;
        assertEquals((17 * 37 + 5) * 37, HashCodeUtils.buildHashCode(17, obj));
        obj[1] = (char) 6;
        assertEquals( (17 * 37 + 5) * 37 + 6, HashCodeUtils.buildHashCode(17, obj));
    }

    public void testByteArray() {
        assertEquals(17 * 37, HashCodeUtils.buildHashCode(17, (byte[]) null));
        byte[] obj = new byte[2];
        assertEquals((17 * 37) * 37 , HashCodeUtils.buildHashCode(17, obj));
        obj[0] = (byte) 5;
        assertEquals((17 * 37 + 5) * 37, HashCodeUtils.buildHashCode(17, obj));
        obj[1] = (byte) 6;
        assertEquals( (17 * 37 + 5) * 37 + 6, HashCodeUtils.buildHashCode(17, obj));
    }

    public void testDoubleArray() {
        assertEquals(17 * 37, HashCodeUtils.buildHashCode(17, (double[]) null));
        double[] obj = new double[2];
        assertEquals((17 * 37) * 37 , HashCodeUtils.buildHashCode(17, obj));
        obj[0] = 5.4d;
        long l1 = Double.doubleToLongBits(5.4d);
        int h1 = (int) (l1 ^ (l1 >> 32));
        assertEquals((17 * 37 + h1) * 37, HashCodeUtils.buildHashCode(17, obj));
        obj[1] = 6.3d;
        long l2 = Double.doubleToLongBits(6.3d);
        int h2 = (int) (l2 ^ (l2 >> 32));
        assertEquals( (17 * 37 + h1) * 37 + h2, HashCodeUtils.buildHashCode(17, obj));
    }

    public void testFloatArray() {
        assertEquals(17 * 37, HashCodeUtils.buildHashCode(17, (float[]) null));
        float[] obj = new float[2];
        assertEquals((17 * 37) * 37 , HashCodeUtils.buildHashCode(17, obj));
        obj[0] = 5.4f;
        int h1 = Float.floatToIntBits(5.4f);
        assertEquals((17 * 37 + h1) * 37, HashCodeUtils.buildHashCode(17, obj));
        obj[1] = 6.3f;
        int h2 = Float.floatToIntBits(6.3f);
        assertEquals( (17 * 37 + h1) * 37 + h2, HashCodeUtils.buildHashCode(17, obj));
    }

    public void testBooleanArray() {
        assertEquals(17 * 37, HashCodeUtils.buildHashCode(17, (boolean[]) null));
        boolean[] obj = new boolean[2];
        assertEquals((17 * 37 + 1) * 37 + 1 , HashCodeUtils.buildHashCode(17, obj));
        obj[0] = true;
        assertEquals((17 * 37 + 0) * 37 + 1, HashCodeUtils.buildHashCode(17, obj));
        obj[1] = false;
        assertEquals( (17 * 37 + 0) * 37 + 1, HashCodeUtils.buildHashCode(17, obj));
    }

}
