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
package org.apache.commons.lang.builder;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;
/**
 * Unit tests {@link org.apache.commons.lang.HashCodeBuilder}.
 *
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @version $Id: HashCodeBuilderTest.java,v 1.4 2003/05/21 23:49:15 scolebourne Exp $
 */
public class HashCodeBuilderTest extends TestCase {

    public HashCodeBuilderTest(String name) {
        super(name);
    }

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(HashCodeBuilderTest.class);
        suite.setName("HashCodeBuilder Tests");
        return suite;
    }

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    //-----------------------------------------------------------------------

    public void testConstructorEx1() {
        try {
            new HashCodeBuilder(0, 0);
            
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }

    public void testConstructorEx2() {
        try {
            new HashCodeBuilder(2, 2);
            
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }

    static class TestObject {
        private int a;
        public TestObject(int a) {
            this.a = a;
        }
        public boolean equals(Object o) {
            if (o == this) {
                return true;
            }
            if (!(o instanceof TestObject)) {
                return false;
            }
            TestObject rhs = (TestObject) o;
            return (a == rhs.a);
        }

        public void setA(int a) {
            this.a = a;
        }

        public int getA() {
            return a;
        }
    }

    static class TestSubObject extends TestObject {
        private int b;
        transient private int t;
        public TestSubObject() {
            super(0);
        }
        public TestSubObject(int a, int b, int t) {
            super(a);
            this.b = b;
            this.t = t;
        }
        public boolean equals(Object o) {
            if (o == this) {
                return true;
            }
            if (!(o instanceof TestSubObject)) {
                return false;
            }
            TestSubObject rhs = (TestSubObject) o;
            return super.equals(o) && (b == rhs.b);
        }
    }

    public void testReflectionHashCode() {
        assertEquals(17 * 37, HashCodeBuilder.reflectionHashCode(new TestObject(0)));
        assertEquals(17 * 37 + 123456, HashCodeBuilder.reflectionHashCode(new TestObject(123456)));
    }

    public void testReflectionHierarchyHashCode() {
        assertEquals(17 * 37 * 37, HashCodeBuilder.reflectionHashCode(new TestSubObject(0, 0, 0)));
        assertEquals(17 * 37 * 37 * 37, HashCodeBuilder.reflectionHashCode(new TestSubObject(0, 0, 0), true));
        assertEquals((17 * 37 + 7890) * 37 + 123456, HashCodeBuilder.reflectionHashCode(new TestSubObject(123456, 7890, 0)));
        assertEquals(((17 * 37 + 7890) * 37 + 0) * 37 + 123456, HashCodeBuilder.reflectionHashCode(new TestSubObject(123456, 7890, 0), true));
    }

    public void testReflectionHierarchyHashCodeEx1() {
        try {
            HashCodeBuilder.reflectionHashCode(0, 0, new TestSubObject(0, 0, 0), true);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }

    public void testReflectionHierarchyHashCodeEx2() {
        try {
            HashCodeBuilder.reflectionHashCode(2, 2, new TestSubObject(0, 0, 0), true);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }

    public void testReflectionHashCodeEx1() {
        try {
            HashCodeBuilder.reflectionHashCode(0, 0, new TestObject(0), true);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }

    public void testReflectionHashCodeEx2() {
        try {
            HashCodeBuilder.reflectionHashCode(2, 2, new TestObject(0), true);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }

    public void testReflectionHashCodeEx3() {
        try {
            HashCodeBuilder.reflectionHashCode(13, 19, null, true);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }

    public void testSuper() {
        Object obj = new Object();
        assertEquals(17 * 37 + (19 * 41 + obj.hashCode()), new HashCodeBuilder(17, 37).appendSuper(
            new HashCodeBuilder(19, 41).append(obj).toHashCode()
        ).toHashCode());
    }

    public void testObject() {
        Object obj = null;
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj = new Object();
        assertEquals(17 * 37 + obj.hashCode(), new HashCodeBuilder(17, 37).append(obj).toHashCode());
    }

    public void testLong() {
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append((long) 0L).toHashCode());
        assertEquals(17 * 37 + (int) (123456789L ^ (123456789L >> 32)), new HashCodeBuilder(17, 37).append((long) 123456789L).toHashCode());
    }

    public void testInt() {
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append((int) 0).toHashCode());
        assertEquals(17 * 37 + 123456, new HashCodeBuilder(17, 37).append((int) 123456).toHashCode());
    }

    public void testShort() {
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append((short) 0).toHashCode());
        assertEquals(17 * 37 + 12345, new HashCodeBuilder(17, 37).append((short) 12345).toHashCode());
    }

    public void testChar() {
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append((char) 0).toHashCode());
        assertEquals(17 * 37 + 1234, new HashCodeBuilder(17, 37).append((char) 1234).toHashCode());
    }

    public void testByte() {
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append((byte) 0).toHashCode());
        assertEquals(17 * 37 + 123, new HashCodeBuilder(17, 37).append((byte) 123).toHashCode());
    }

    public void testDouble() {
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append((double) 0d).toHashCode());
        double d = 1234567.89;
        long l = Double.doubleToLongBits(d);
        assertEquals(17 * 37 + (int) (l ^ (l >> 32)), new HashCodeBuilder(17, 37).append(d).toHashCode());
    }

    public void testFloat() {
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append((float) 0f).toHashCode());
        float f = 1234.89f;
        int i = Float.floatToIntBits(f);
        assertEquals(17 * 37 + i, new HashCodeBuilder(17, 37).append(f).toHashCode());
    }

    public void testBoolean() {
        assertEquals(17 * 37 + 0, new HashCodeBuilder(17, 37).append(true).toHashCode());
        assertEquals(17 * 37 + 1, new HashCodeBuilder(17, 37).append(false).toHashCode());
    }

    public void testObjectArray() {
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append((Object[]) null).toHashCode());
        Object[] obj = new Object[2];
        assertEquals((17 * 37) * 37 , new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[0] = new Object();
        assertEquals((17 * 37 + obj[0].hashCode()) * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[1] = new Object();
        assertEquals( (17 * 37 + obj[0].hashCode()) * 37 + obj[1].hashCode(), new HashCodeBuilder(17, 37).append(obj).toHashCode());
    }

    public void testObjectArrayAsObject() {
        Object[] obj = new Object[2];
        assertEquals((17 * 37) * 37 , new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[0] = new Object();
        assertEquals((17 * 37 + obj[0].hashCode()) * 37, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[1] = new Object();
        assertEquals( (17 * 37 + obj[0].hashCode()) * 37 + obj[1].hashCode(), new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
    }

    public void testLongArray() {
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append((long[]) null).toHashCode());
        long[] obj = new long[2];
        assertEquals((17 * 37) * 37 , new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[0] = 5L;
        int h1 = (int) (5L ^ (5L >> 32));
        assertEquals((17 * 37 + h1) * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[1] = 6L;
        int h2 = (int) (6L ^ (6L >> 32));
        assertEquals( (17 * 37 + h1) * 37 + h2, new HashCodeBuilder(17, 37).append(obj).toHashCode());
    }

    public void testLongArrayAsObject() {
        long[] obj = new long[2];
        assertEquals((17 * 37) * 37 , new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[0] = 5L;
        int h1 = (int) (5L ^ (5L >> 32));
        assertEquals((17 * 37 + h1) * 37, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[1] = 6L;
        int h2 = (int) (6L ^ (6L >> 32));
        assertEquals( (17 * 37 + h1) * 37 + h2, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
    }

    public void testIntArray() {
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append((int[]) null).toHashCode());
        int[] obj = new int[2];
        assertEquals((17 * 37) * 37 , new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[0] = 5;
        assertEquals((17 * 37 + 5) * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[1] = 6;
        assertEquals( (17 * 37 + 5) * 37 + 6, new HashCodeBuilder(17, 37).append(obj).toHashCode());
    }

    public void testIntArrayAsObject() {
        int[] obj = new int[2];
        assertEquals((17 * 37) * 37 , new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[0] = 5;
        assertEquals((17 * 37 + 5) * 37, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[1] = 6;
        assertEquals( (17 * 37 + 5) * 37 + 6, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
    }

    public void testShortArray() {
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append((short[]) null).toHashCode());
        short[] obj = new short[2];
        assertEquals((17 * 37) * 37 , new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[0] = (short) 5;
        assertEquals((17 * 37 + 5) * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[1] = (short) 6;
        assertEquals( (17 * 37 + 5) * 37 + 6, new HashCodeBuilder(17, 37).append(obj).toHashCode());
    }

    public void testShortArrayAsObject() {
        short[] obj = new short[2];
        assertEquals((17 * 37) * 37 , new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[0] = (short) 5;
        assertEquals((17 * 37 + 5) * 37, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[1] = (short) 6;
        assertEquals( (17 * 37 + 5) * 37 + 6, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
    }

    public void testCharArray() {
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append((char[]) null).toHashCode());
        char[] obj = new char[2];
        assertEquals((17 * 37) * 37 , new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[0] = (char) 5;
        assertEquals((17 * 37 + 5) * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[1] = (char) 6;
        assertEquals( (17 * 37 + 5) * 37 + 6, new HashCodeBuilder(17, 37).append(obj).toHashCode());
    }

    public void testCharArrayAsObject() {
        char[] obj = new char[2];
        assertEquals((17 * 37) * 37 , new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[0] = (char) 5;
        assertEquals((17 * 37 + 5) * 37, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[1] = (char) 6;
        assertEquals( (17 * 37 + 5) * 37 + 6, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
    }

    public void testByteArray() {
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append((byte[]) null).toHashCode());
        byte[] obj = new byte[2];
        assertEquals((17 * 37) * 37 , new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[0] = (byte) 5;
        assertEquals((17 * 37 + 5) * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[1] = (byte) 6;
        assertEquals( (17 * 37 + 5) * 37 + 6, new HashCodeBuilder(17, 37).append(obj).toHashCode());
    }

    public void testByteArrayAsObject() {
        byte[] obj = new byte[2];
        assertEquals((17 * 37) * 37 , new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[0] = (byte) 5;
        assertEquals((17 * 37 + 5) * 37, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[1] = (byte) 6;
        assertEquals( (17 * 37 + 5) * 37 + 6, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
    }

    public void testDoubleArray() {
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append((double[]) null).toHashCode());
        double[] obj = new double[2];
        assertEquals((17 * 37) * 37 , new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[0] = 5.4d;
        long l1 = Double.doubleToLongBits(5.4d);
        int h1 = (int) (l1 ^ (l1 >> 32));
        assertEquals((17 * 37 + h1) * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[1] = 6.3d;
        long l2 = Double.doubleToLongBits(6.3d);
        int h2 = (int) (l2 ^ (l2 >> 32));
        assertEquals( (17 * 37 + h1) * 37 + h2, new HashCodeBuilder(17, 37).append(obj).toHashCode());
    }

    public void testDoubleArrayAsObject() {
        double[] obj = new double[2];
        assertEquals((17 * 37) * 37 , new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[0] = 5.4d;
        long l1 = Double.doubleToLongBits(5.4d);
        int h1 = (int) (l1 ^ (l1 >> 32));
        assertEquals((17 * 37 + h1) * 37, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[1] = 6.3d;
        long l2 = Double.doubleToLongBits(6.3d);
        int h2 = (int) (l2 ^ (l2 >> 32));
        assertEquals( (17 * 37 + h1) * 37 + h2, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
    }

    public void testFloatArray() {
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append((float[]) null).toHashCode());
        float[] obj = new float[2];
        assertEquals((17 * 37) * 37 , new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[0] = 5.4f;
        int h1 = Float.floatToIntBits(5.4f);
        assertEquals((17 * 37 + h1) * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[1] = 6.3f;
        int h2 = Float.floatToIntBits(6.3f);
        assertEquals( (17 * 37 + h1) * 37 + h2, new HashCodeBuilder(17, 37).append(obj).toHashCode());
    }

    public void testFloatArrayAsObject() {
        float[] obj = new float[2];
        assertEquals((17 * 37) * 37 , new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[0] = 5.4f;
        int h1 = Float.floatToIntBits(5.4f);
        assertEquals((17 * 37 + h1) * 37, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[1] = 6.3f;
        int h2 = Float.floatToIntBits(6.3f);
        assertEquals( (17 * 37 + h1) * 37 + h2, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
    }

    public void testBooleanArray() {
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append((boolean[]) null).toHashCode());
        boolean[] obj = new boolean[2];
        assertEquals((17 * 37 + 1) * 37 + 1 , new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[0] = true;
        assertEquals((17 * 37 + 0) * 37 + 1, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[1] = false;
        assertEquals( (17 * 37 + 0) * 37 + 1, new HashCodeBuilder(17, 37).append(obj).toHashCode());
    }

    public void testBooleanArrayAsObject() {
        boolean[] obj = new boolean[2];
        assertEquals((17 * 37 + 1) * 37 + 1 , new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[0] = true;
        assertEquals((17 * 37 + 0) * 37 + 1, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[1] = false;
        assertEquals( (17 * 37 + 0) * 37 + 1, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
    }

    public void testBooleanMultiArray() {
        boolean[][] obj = new boolean[2][];
        assertEquals((17 * 37) * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[0] = new boolean[0];
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[0] = new boolean[1];
        assertEquals((17 * 37 + 1) * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[0] = new boolean[2];
        assertEquals(((17 * 37 + 1) * 37 + 1) * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[0][0] = true;
        assertEquals(((17 * 37 + 0) * 37 + 1) * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[1] = new boolean[1];
        assertEquals( (((17 * 37 + 0) * 37 + 1) * 37 + 1), new HashCodeBuilder(17, 37).append(obj).toHashCode());
    }

}
