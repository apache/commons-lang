/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang3.builder;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.math.BigInteger;

import org.junit.Test;

/**
 * Unit tests {@link org.apache.commons.lang3.builder.CompareToBuilder}.
 *
 * @version $Id$
 */
public class CompareToBuilderTest {

    //-----------------------------------------------------------------------

    static class TestObject implements Comparable<TestObject> {
        private int a;
        public TestObject(int a) {
            this.a = a;
        }
        @Override
        public boolean equals(Object o) {
            if (o == this) {
                return true;
            }
            if (!(o instanceof TestObject)) {
                return false;
            }
            TestObject rhs = (TestObject) o;
            return a == rhs.a;
        }

        @Override
        public int hashCode() {
            return a;
        }

        public void setA(int a) {
            this.a = a;
        }

        public int getA() {
            return a;
        }
        @Override
        public int compareTo(TestObject rhs) {
            return a < rhs.a ? -1 : a > rhs.a ? +1 : 0;
        }
    }

    static class TestSubObject extends TestObject {
        private int b;
        public TestSubObject() {
            super(0);
        }
        public TestSubObject(int a, int b) {
            super(a);
            this.b = b;
        }
        @Override
        public boolean equals(Object o) {
            if (o == this) {
                return true;
            }
            if (!(o instanceof TestSubObject)) {
                return false;
            }
            TestSubObject rhs = (TestSubObject) o;
            return super.equals(o) && b == rhs.b;
        }
    }

    static class TestTransientSubObject extends TestObject {
        @SuppressWarnings("unused")
        private transient int t;
        public TestTransientSubObject(int a, int t) {
            super(a);
            this.t = t;
        }
    }
    
    @Test
    public void testReflectionCompare() {
        TestObject o1 = new TestObject(4);
        TestObject o2 = new TestObject(4);
        assertTrue(CompareToBuilder.reflectionCompare(o1, o1) == 0);
        assertTrue(CompareToBuilder.reflectionCompare(o1, o2) == 0);
        o2.setA(5);
        assertTrue(CompareToBuilder.reflectionCompare(o1, o2) < 0);
        assertTrue(CompareToBuilder.reflectionCompare(o2, o1) > 0);
    }

    @Test(expected=NullPointerException.class)
    public void testReflectionCompareEx1() {
        TestObject o1 = new TestObject(4);
        CompareToBuilder.reflectionCompare(o1, null);
    }

    @Test(expected=ClassCastException.class)
    public void testReflectionCompareEx2() {
        TestObject o1 = new TestObject(4);
        Object o2 = new Object();
        CompareToBuilder.reflectionCompare(o1, o2);
    }

    @Test
    public void testReflectionHierarchyCompare() {
        testReflectionHierarchyCompare(false, null);
    }
    
    @Test
    public void testReflectionHierarchyCompareExcludeFields() {
        String[] excludeFields = new String[] { "b" };
        testReflectionHierarchyCompare(true, excludeFields);
        
        TestSubObject x;
        TestSubObject y;
        TestSubObject z;
        
        x = new TestSubObject(1, 1);
        y = new TestSubObject(2, 1);
        z = new TestSubObject(3, 1);
        assertXYZCompareOrder(x, y, z, true, excludeFields);

        x = new TestSubObject(1, 3);
        y = new TestSubObject(2, 2);
        z = new TestSubObject(3, 1);
        assertXYZCompareOrder(x, y, z, true, excludeFields);
    }
    
    @Test
    public void testReflectionHierarchyCompareTransients() {
        testReflectionHierarchyCompare(true, null);

        TestTransientSubObject x;
        TestTransientSubObject y;
        TestTransientSubObject z;

        x = new TestTransientSubObject(1, 1);
        y = new TestTransientSubObject(2, 2);
        z = new TestTransientSubObject(3, 3);
        assertXYZCompareOrder(x, y, z, true, null);
        
        x = new TestTransientSubObject(1, 1);
        y = new TestTransientSubObject(1, 2);
        z = new TestTransientSubObject(1, 3);
        assertXYZCompareOrder(x, y, z, true, null);  
    }
    
    private void assertXYZCompareOrder(Object x, Object y, Object z, boolean testTransients, String[] excludeFields) {
        assertTrue(0 == CompareToBuilder.reflectionCompare(x, x, testTransients, null, excludeFields));
        assertTrue(0 == CompareToBuilder.reflectionCompare(y, y, testTransients, null, excludeFields));
        assertTrue(0 == CompareToBuilder.reflectionCompare(z, z, testTransients, null, excludeFields));
        
        assertTrue(0 > CompareToBuilder.reflectionCompare(x, y, testTransients, null, excludeFields));
        assertTrue(0 > CompareToBuilder.reflectionCompare(x, z, testTransients, null, excludeFields));
        assertTrue(0 > CompareToBuilder.reflectionCompare(y, z, testTransients, null, excludeFields));
        
        assertTrue(0 < CompareToBuilder.reflectionCompare(y, x, testTransients, null, excludeFields));
        assertTrue(0 < CompareToBuilder.reflectionCompare(z, x, testTransients, null, excludeFields));
        assertTrue(0 < CompareToBuilder.reflectionCompare(z, y, testTransients, null, excludeFields));
    }
    
    private void testReflectionHierarchyCompare(boolean testTransients, String[] excludeFields) {
        TestObject to1 = new TestObject(1);
        TestObject to2 = new TestObject(2);
        TestObject to3 = new TestObject(3);
        TestSubObject tso1 = new TestSubObject(1, 1);
        TestSubObject tso2 = new TestSubObject(2, 2);
        TestSubObject tso3 = new TestSubObject(3, 3);
        
        assertReflectionCompareContract(to1, to1, to1, false, excludeFields);
        assertReflectionCompareContract(to1, to2, to3, false, excludeFields);
        assertReflectionCompareContract(tso1, tso1, tso1, false, excludeFields);
        assertReflectionCompareContract(tso1, tso2, tso3, false, excludeFields);
        assertReflectionCompareContract("1", "2", "3", false, excludeFields);
        
        assertTrue(0 != CompareToBuilder.reflectionCompare(tso1, new TestSubObject(1, 0), testTransients));
        assertTrue(0 != CompareToBuilder.reflectionCompare(tso1, new TestSubObject(0, 1), testTransients));

        // root class
        assertXYZCompareOrder(to1, to2, to3, true, null);
        // subclass  
        assertXYZCompareOrder(tso1, tso2, tso3, true, null);  
    }

    /**
     * See "Effective Java" under "Consider Implementing Comparable".
     *  
     * @param x an object to compare 
     * @param y an object to compare
     * @param z an object to compare
     * @param testTransients Whether to include transients in the comparison
     * @param excludeFields fields to exclude
     */
    private void assertReflectionCompareContract(Object x, Object y, Object z, boolean testTransients, String[] excludeFields) {

        // signum
        assertTrue(reflectionCompareSignum(x, y, testTransients, excludeFields) == -reflectionCompareSignum(y, x, testTransients, excludeFields));
        
        // transitive
        if (CompareToBuilder.reflectionCompare(x, y, testTransients, null, excludeFields) > 0 
                && CompareToBuilder.reflectionCompare(y, z, testTransients, null, excludeFields) > 0){
            assertTrue(CompareToBuilder.reflectionCompare(x, z, testTransients, null, excludeFields) > 0);
        }
        
        // un-named
        if (CompareToBuilder.reflectionCompare(x, y, testTransients, null, excludeFields) == 0) {
            assertTrue(reflectionCompareSignum(x, z, testTransients, excludeFields) == -reflectionCompareSignum(y, z, testTransients, excludeFields));
        }
        
        // strongly recommended but not strictly required
        assertTrue(CompareToBuilder.reflectionCompare(x, y, testTransients) ==0 == EqualsBuilder.reflectionEquals(x, y, testTransients));
    }
    
    /**
     * Returns the signum of the result of comparing x and y with
     * <code>CompareToBuilder.reflectionCompare</code>
     * 
     * @param lhs The "left-hand-side" of the comparison.
     * @param rhs The "right-hand-side" of the comparison.
     * @param testTransients Whether to include transients in the comparison
     * @param excludeFields fields to exclude
     * @return int The signum
     */
    private int reflectionCompareSignum(Object lhs, Object rhs, boolean testTransients, String[] excludeFields) {
        return BigInteger.valueOf(CompareToBuilder.reflectionCompare(lhs, rhs, testTransients)).signum();
    }
    
    @Test
    public void testAppendSuper() {
        TestObject o1 = new TestObject(4);
        TestObject o2 = new TestObject(5);
        assertTrue(new CompareToBuilder().appendSuper(0).append(o1, o1).toComparison() == 0);
        assertTrue(new CompareToBuilder().appendSuper(0).append(o1, o2).toComparison() < 0);
        assertTrue(new CompareToBuilder().appendSuper(0).append(o2, o1).toComparison() > 0);
        
        assertTrue(new CompareToBuilder().appendSuper(-1).append(o1, o1).toComparison() < 0);
        assertTrue(new CompareToBuilder().appendSuper(-1).append(o1, o2).toComparison() < 0);
        
        assertTrue(new CompareToBuilder().appendSuper(1).append(o1, o1).toComparison() > 0);
        assertTrue(new CompareToBuilder().appendSuper(1).append(o1, o2).toComparison() > 0);
    }
    
    @Test
    public void testObject() {
        TestObject o1 = new TestObject(4);
        TestObject o2 = new TestObject(4);
        assertTrue(new CompareToBuilder().append(o1, o1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(o1, o2).toComparison() == 0);
        o2.setA(5);
        assertTrue(new CompareToBuilder().append(o1, o2).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(o2, o1).toComparison() > 0);
        
        assertTrue(new CompareToBuilder().append(o1, null).toComparison() > 0);
        assertTrue(new CompareToBuilder().append((Object) null, (Object) null).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(null, o1).toComparison() < 0);
    }
    
    @Test
    public void testObjectBuild() {
        TestObject o1 = new TestObject(4);
        TestObject o2 = new TestObject(4);
        assertEquals(Integer.valueOf(0), new CompareToBuilder().append(o1, o1).build());
        assertEquals(Integer.valueOf(0), new CompareToBuilder().append(o1, o2).build());
        o2.setA(5);
        assertTrue(new CompareToBuilder().append(o1, o2).build().intValue() < 0);
        assertTrue(new CompareToBuilder().append(o2, o1).build().intValue() > 0);
        
        assertTrue(new CompareToBuilder().append(o1, null).build().intValue() > 0);
        assertEquals(Integer.valueOf(0), new CompareToBuilder().append((Object) null, (Object) null).build());
        assertTrue(new CompareToBuilder().append(null, o1).build().intValue() < 0);
    }

    @Test(expected=ClassCastException.class)
    public void testObjectEx2() {
        TestObject o1 = new TestObject(4);
        Object o2 = new Object();
        new CompareToBuilder().append(o1, o2);
    }

    @Test
    public void testObjectComparator() {
        String o1 = "Fred";
        String o2 = "Fred";
        assertTrue(new CompareToBuilder().append(o1, o1, String.CASE_INSENSITIVE_ORDER).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(o1, o2, String.CASE_INSENSITIVE_ORDER).toComparison() == 0);
        o2 = "FRED";
        assertTrue(new CompareToBuilder().append(o1, o2, String.CASE_INSENSITIVE_ORDER).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(o2, o1, String.CASE_INSENSITIVE_ORDER).toComparison() == 0);
        o2 = "FREDA";
        assertTrue(new CompareToBuilder().append(o1, o2, String.CASE_INSENSITIVE_ORDER).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(o2, o1, String.CASE_INSENSITIVE_ORDER).toComparison() > 0);
        
        assertTrue(new CompareToBuilder().append(o1, null, String.CASE_INSENSITIVE_ORDER).toComparison() > 0);
        assertTrue(new CompareToBuilder().append((Object) null, (Object) null, String.CASE_INSENSITIVE_ORDER).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(null, o1, String.CASE_INSENSITIVE_ORDER).toComparison() < 0);
    }
    
    @Test
    public void testObjectComparatorNull() {
        String o1 = "Fred";
        String o2 = "Fred";
        assertTrue(new CompareToBuilder().append(o1, o1, null).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(o1, o2, null).toComparison() == 0);
        o2 = "Zebra";
        assertTrue(new CompareToBuilder().append(o1, o2, null).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(o2, o1, null).toComparison() > 0);
        
        assertTrue(new CompareToBuilder().append(o1, null, null).toComparison() > 0);
        assertTrue(new CompareToBuilder().append((Object) null, (Object) null, null).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(null, o1, null).toComparison() < 0);
    }

    @Test
    public void testLong() {
        long o1 = 1L;
        long o2 = 2L;
        assertTrue(new CompareToBuilder().append(o1, o1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(o1, o2).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(o2, o1).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(o1, Long.MAX_VALUE).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(Long.MAX_VALUE, o1).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(o1, Long.MIN_VALUE).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(Long.MIN_VALUE, o1).toComparison() < 0);
    }

    @Test
    public void testInt() {
        int o1 = 1;
        int o2 = 2;
        assertTrue(new CompareToBuilder().append(o1, o1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(o1, o2).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(o2, o1).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(o1, Integer.MAX_VALUE).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(Integer.MAX_VALUE, o1).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(o1, Integer.MIN_VALUE).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(Integer.MIN_VALUE, o1).toComparison() < 0);
    }

    @Test
    public void testShort() {
        short o1 = 1;
        short o2 = 2;
        assertTrue(new CompareToBuilder().append(o1, o1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(o1, o2).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(o2, o1).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(o1, Short.MAX_VALUE).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(Short.MAX_VALUE, o1).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(o1, Short.MIN_VALUE).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(Short.MIN_VALUE, o1).toComparison() < 0);
    }

    @Test
    public void testChar() {
        char o1 = 1;
        char o2 = 2;
        assertTrue(new CompareToBuilder().append(o1, o1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(o1, o2).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(o2, o1).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(o1, Character.MAX_VALUE).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(Character.MAX_VALUE, o1).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(o1, Character.MIN_VALUE).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(Character.MIN_VALUE, o1).toComparison() < 0);
    }

    @Test
    public void testByte() {
        byte o1 = 1;
        byte o2 = 2;
        assertTrue(new CompareToBuilder().append(o1, o1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(o1, o2).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(o2, o1).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(o1, Byte.MAX_VALUE).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(Byte.MAX_VALUE, o1).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(o1, Byte.MIN_VALUE).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(Byte.MIN_VALUE, o1).toComparison() < 0);
    }

    @Test
    public void testDouble() {
        double o1 = 1;
        double o2 = 2;
        assertTrue(new CompareToBuilder().append(o1, o1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(o1, o2).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(o2, o1).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(o1, Double.MAX_VALUE).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(Double.MAX_VALUE, o1).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(o1, Double.MIN_VALUE).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(Double.MIN_VALUE, o1).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(Double.NaN, Double.NaN).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(Double.NaN, Double.MAX_VALUE).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(Double.POSITIVE_INFINITY, Double.MAX_VALUE).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(Double.NEGATIVE_INFINITY, Double.MIN_VALUE).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(o1, Double.NaN).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(Double.NaN, o1).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(-0.0, 0.0).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(0.0, -0.0).toComparison() > 0);
    }

    @Test
    public void testFloat() {
        float o1 = 1;
        float o2 = 2;
        assertTrue(new CompareToBuilder().append(o1, o1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(o1, o2).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(o2, o1).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(o1, Float.MAX_VALUE).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(Float.MAX_VALUE, o1).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(o1, Float.MIN_VALUE).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(Float.MIN_VALUE, o1).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(Float.NaN, Float.NaN).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(Float.NaN, Float.MAX_VALUE).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(Float.POSITIVE_INFINITY, Float.MAX_VALUE).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(Float.NEGATIVE_INFINITY, Float.MIN_VALUE).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(o1, Float.NaN).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(Float.NaN, o1).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(-0.0, 0.0).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(0.0, -0.0).toComparison() > 0);
    }

    @Test
    public void testBoolean() {
        boolean o1 = true;
        boolean o2 = false;
        assertTrue(new CompareToBuilder().append(o1, o1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(o2, o2).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(o1, o2).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(o2, o1).toComparison() < 0);
    }

    @Test
    public void testObjectArray() {
        TestObject[] obj1 = new TestObject[2];
        obj1[0] = new TestObject(4);
        obj1[1] = new TestObject(5);
        TestObject[] obj2 = new TestObject[2];
        obj2[0] = new TestObject(4);
        obj2[1] = new TestObject(5);
        TestObject[] obj3 = new TestObject[3];
        obj3[0] = new TestObject(4);
        obj3[1] = new TestObject(5);
        obj3[2] = new TestObject(6);
        
        assertTrue(new CompareToBuilder().append(obj1, obj1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj3).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(obj3, obj1).toComparison() > 0);
        
        obj1[1] = new TestObject(7);
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(obj2, obj1).toComparison() < 0);

        assertTrue(new CompareToBuilder().append(obj1, null).toComparison() > 0);
        assertTrue(new CompareToBuilder().append((Object[]) null, (Object[]) null).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(null, obj1).toComparison() < 0);
    }

    @Test
    public void testLongArray() {
        long[] obj1 = new long[2];
        obj1[0] = 5L;
        obj1[1] = 6L;
        long[] obj2 = new long[2];
        obj2[0] = 5L;
        obj2[1] = 6L;
        long[] obj3 = new long[3];
        obj3[0] = 5L;
        obj3[1] = 6L;
        obj3[2] = 7L;
        
        assertTrue(new CompareToBuilder().append(obj1, obj1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj3).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(obj3, obj1).toComparison() > 0);

        obj1[1] = 7;
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(obj2, obj1).toComparison() < 0);

        assertTrue(new CompareToBuilder().append(obj1, null).toComparison() > 0);
        assertTrue(new CompareToBuilder().append((long[]) null, (long[]) null).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(null, obj1).toComparison() < 0);
    }

    @Test
    public void testIntArray() {
        int[] obj1 = new int[2];
        obj1[0] = 5;
        obj1[1] = 6;
        int[] obj2 = new int[2];
        obj2[0] = 5;
        obj2[1] = 6;
        int[] obj3 = new int[3];
        obj3[0] = 5;
        obj3[1] = 6;
        obj3[2] = 7;

        assertTrue(new CompareToBuilder().append(obj1, obj1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj3).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(obj3, obj1).toComparison() > 0);

        obj1[1] = 7;
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(obj2, obj1).toComparison() < 0);

        assertTrue(new CompareToBuilder().append(obj1, null).toComparison() > 0);
        assertTrue(new CompareToBuilder().append((int[]) null, (int[]) null).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(null, obj1).toComparison() < 0);
    }

    @Test
    public void testShortArray() {
        short[] obj1 = new short[2];
        obj1[0] = 5;
        obj1[1] = 6;
        short[] obj2 = new short[2];
        obj2[0] = 5;
        obj2[1] = 6;
        short[] obj3 = new short[3];
        obj3[0] = 5;
        obj3[1] = 6;
        obj3[2] = 7;

        assertTrue(new CompareToBuilder().append(obj1, obj1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj3).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(obj3, obj1).toComparison() > 0);

        obj1[1] = 7;
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(obj2, obj1).toComparison() < 0);

        assertTrue(new CompareToBuilder().append(obj1, null).toComparison() > 0);
        assertTrue(new CompareToBuilder().append((short[]) null, (short[]) null).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(null, obj1).toComparison() < 0);
    }

    @Test
    public void testCharArray() {
        char[] obj1 = new char[2];
        obj1[0] = 5;
        obj1[1] = 6;
        char[] obj2 = new char[2];
        obj2[0] = 5;
        obj2[1] = 6;
        char[] obj3 = new char[3];
        obj3[0] = 5;
        obj3[1] = 6;
        obj3[2] = 7;

        assertTrue(new CompareToBuilder().append(obj1, obj1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj3).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(obj3, obj1).toComparison() > 0);

        obj1[1] = 7;
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(obj2, obj1).toComparison() < 0);

        assertTrue(new CompareToBuilder().append(obj1, null).toComparison() > 0);
        assertTrue(new CompareToBuilder().append((char[]) null, (char[]) null).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(null, obj1).toComparison() < 0);
    }

    @Test
    public void testByteArray() {
        byte[] obj1 = new byte[2];
        obj1[0] = 5;
        obj1[1] = 6;
        byte[] obj2 = new byte[2];
        obj2[0] = 5;
        obj2[1] = 6;
        byte[] obj3 = new byte[3];
        obj3[0] = 5;
        obj3[1] = 6;
        obj3[2] = 7;

        assertTrue(new CompareToBuilder().append(obj1, obj1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj3).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(obj3, obj1).toComparison() > 0);

        obj1[1] = 7;
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(obj2, obj1).toComparison() < 0);

        assertTrue(new CompareToBuilder().append(obj1, null).toComparison() > 0);
        assertTrue(new CompareToBuilder().append((byte[]) null, (byte[]) null).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(null, obj1).toComparison() < 0);
    }

    @Test
    public void testDoubleArray() {
        double[] obj1 = new double[2];
        obj1[0] = 5;
        obj1[1] = 6;
        double[] obj2 = new double[2];
        obj2[0] = 5;
        obj2[1] = 6;
        double[] obj3 = new double[3];
        obj3[0] = 5;
        obj3[1] = 6;
        obj3[2] = 7;

        assertTrue(new CompareToBuilder().append(obj1, obj1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj3).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(obj3, obj1).toComparison() > 0);

        obj1[1] = 7;
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(obj2, obj1).toComparison() < 0);

        assertTrue(new CompareToBuilder().append(obj1, null).toComparison() > 0);
        assertTrue(new CompareToBuilder().append((double[]) null, (double[]) null).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(null, obj1).toComparison() < 0);
    }

    @Test
    public void testFloatArray() {
        float[] obj1 = new float[2];
        obj1[0] = 5;
        obj1[1] = 6;
        float[] obj2 = new float[2];
        obj2[0] = 5;
        obj2[1] = 6;
        float[] obj3 = new float[3];
        obj3[0] = 5;
        obj3[1] = 6;
        obj3[2] = 7;

        assertTrue(new CompareToBuilder().append(obj1, obj1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj3).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(obj3, obj1).toComparison() > 0);

        obj1[1] = 7;
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(obj2, obj1).toComparison() < 0);

        assertTrue(new CompareToBuilder().append(obj1, null).toComparison() > 0);
        assertTrue(new CompareToBuilder().append((float[]) null, (float[]) null).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(null, obj1).toComparison() < 0);
    }

    @Test
    public void testBooleanArray() {
        boolean[] obj1 = new boolean[2];
        obj1[0] = true;
        obj1[1] = false;
        boolean[] obj2 = new boolean[2];
        obj2[0] = true;
        obj2[1] = false;
        boolean[] obj3 = new boolean[3];
        obj3[0] = true;
        obj3[1] = false;
        obj3[2] = true;

        assertTrue(new CompareToBuilder().append(obj1, obj1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj3).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(obj3, obj1).toComparison() > 0);

        obj1[1] = true;
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(obj2, obj1).toComparison() < 0);

        assertTrue(new CompareToBuilder().append(obj1, null).toComparison() > 0);
        assertTrue(new CompareToBuilder().append((boolean[]) null, (boolean[]) null).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(null, obj1).toComparison() < 0);
    }

    @Test
    public void testMultiLongArray() {
        long[][] array1 = new long[2][2];
        long[][] array2 = new long[2][2];
        long[][] array3 = new long[2][3];
        for (int i = 0; i < array1.length; ++i) {
            for (int j = 0; j < array1[0].length; j++) {
                array1[i][j] = (i + 1) * (j + 1);
                array2[i][j] = (i + 1) * (j + 1);
                array3[i][j] = (i + 1) * (j + 1);
            }
        }
        array3[1][2] = 100;
        array3[1][2] = 100;
        
        assertTrue(new CompareToBuilder().append(array1, array1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(array1, array2).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(array1, array3).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(array3, array1).toComparison() > 0);
        array1[1][1] = 200;
        assertTrue(new CompareToBuilder().append(array1, array2).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(array2, array1).toComparison() < 0);
    }

    @Test
    public void testMultiIntArray() {
        int[][] array1 = new int[2][2];
        int[][] array2 = new int[2][2];
        int[][] array3 = new int[2][3];
        for (int i = 0; i < array1.length; ++i) {
            for (int j = 0; j < array1[0].length; j++) {
                array1[i][j] = (i + 1) * (j + 1);
                array2[i][j] = (i + 1) * (j + 1);
                array3[i][j] = (i + 1) * (j + 1);
            }
        }
        array3[1][2] = 100;
        array3[1][2] = 100;
        
        assertTrue(new CompareToBuilder().append(array1, array1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(array1, array2).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(array1, array3).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(array3, array1).toComparison() > 0);
        array1[1][1] = 200;
        assertTrue(new CompareToBuilder().append(array1, array2).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(array2, array1).toComparison() < 0);
    }

    @Test
    public void testMultiShortArray() {
        short[][] array1 = new short[2][2];
        short[][] array2 = new short[2][2];
        short[][] array3 = new short[2][3];
        for (short i = 0; i < array1.length; ++i) {
            for (short j = 0; j < array1[0].length; j++) {
                array1[i][j] = (short)((i + 1) * (j + 1));
                array2[i][j] = (short)((i + 1) * (j + 1));
                array3[i][j] = (short)((i + 1) * (j + 1));
            }
        }
        array3[1][2] = 100;
        array3[1][2] = 100;
        
        assertTrue(new CompareToBuilder().append(array1, array1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(array1, array2).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(array1, array3).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(array3, array1).toComparison() > 0);
        array1[1][1] = 200;
        assertTrue(new CompareToBuilder().append(array1, array2).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(array2, array1).toComparison() < 0);
    }

    @Test
    public void testMultiCharArray() {
        char[][] array1 = new char[2][2];
        char[][] array2 = new char[2][2];
        char[][] array3 = new char[2][3];
        for (short i = 0; i < array1.length; ++i) {
            for (short j = 0; j < array1[0].length; j++) {
                array1[i][j] = (char)((i + 1) * (j + 1));
                array2[i][j] = (char)((i + 1) * (j + 1));
                array3[i][j] = (char)((i + 1) * (j + 1));
            }
        }
        array3[1][2] = 100;
        array3[1][2] = 100;
        
        assertTrue(new CompareToBuilder().append(array1, array1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(array1, array2).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(array1, array3).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(array3, array1).toComparison() > 0);
        array1[1][1] = 200;
        assertTrue(new CompareToBuilder().append(array1, array2).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(array2, array1).toComparison() < 0);
    }

    @Test
    public void testMultiByteArray() {
        byte[][] array1 = new byte[2][2];
        byte[][] array2 = new byte[2][2];
        byte[][] array3 = new byte[2][3];
        for (byte i = 0; i < array1.length; ++i) {
            for (byte j = 0; j < array1[0].length; j++) {
                array1[i][j] = (byte)((i + 1) * (j + 1));
                array2[i][j] = (byte)((i + 1) * (j + 1));
                array3[i][j] = (byte)((i + 1) * (j + 1));
            }
        }
        array3[1][2] = 100;
        array3[1][2] = 100;
        
        assertTrue(new CompareToBuilder().append(array1, array1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(array1, array2).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(array1, array3).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(array3, array1).toComparison() > 0);
        array1[1][1] = 127;
        assertTrue(new CompareToBuilder().append(array1, array2).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(array2, array1).toComparison() < 0);
    }
    
    @Test
    public void testMultiFloatArray() {
        float[][] array1 = new float[2][2];
        float[][] array2 = new float[2][2];
        float[][] array3 = new float[2][3];
        for (int i = 0; i < array1.length; ++i) {
            for (int j = 0; j < array1[0].length; j++) {
                array1[i][j] = (i + 1) * (j + 1);
                array2[i][j] = (i + 1) * (j + 1);
                array3[i][j] = (i + 1) * (j + 1);
            }
        }
        array3[1][2] = 100;
        array3[1][2] = 100;
        
        assertTrue(new CompareToBuilder().append(array1, array1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(array1, array2).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(array1, array3).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(array3, array1).toComparison() > 0);
        array1[1][1] = 127;
        assertTrue(new CompareToBuilder().append(array1, array2).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(array2, array1).toComparison() < 0);
    }

    @Test
    public void testMultiDoubleArray() {
        double[][] array1 = new double[2][2];
        double[][] array2 = new double[2][2];
        double[][] array3 = new double[2][3];
        for (int i = 0; i < array1.length; ++i) {
            for (int j = 0; j < array1[0].length; j++) {
                array1[i][j] = (i + 1) * (j + 1);
                array2[i][j] = (i + 1) * (j + 1);
                array3[i][j] = (i + 1) * (j + 1);
            }
        }
        array3[1][2] = 100;
        array3[1][2] = 100;
        
        assertTrue(new CompareToBuilder().append(array1, array1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(array1, array2).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(array1, array3).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(array3, array1).toComparison() > 0);
        array1[1][1] = 127;
        assertTrue(new CompareToBuilder().append(array1, array2).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(array2, array1).toComparison() < 0);
    }

    @Test
    public void testMultiBooleanArray() {
        boolean[][] array1 = new boolean[2][2];
        boolean[][] array2 = new boolean[2][2];
        boolean[][] array3 = new boolean[2][3];
        for (int i = 0; i < array1.length; ++i) {
            for (int j = 0; j < array1[0].length; j++) {
                array1[i][j] = i == 1 ^ j == 1;
                array2[i][j] = i == 1 ^ j == 1;
                array3[i][j] = i == 1 ^ j == 1;
            }
        }
        array3[1][2] = false;
        array3[1][2] = false;
        
        assertTrue(new CompareToBuilder().append(array1, array1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(array1, array2).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(array1, array3).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(array3, array1).toComparison() > 0);
        array1[1][1] = true;
        assertTrue(new CompareToBuilder().append(array1, array2).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(array2, array1).toComparison() < 0);
    }

    @Test
    public void testRaggedArray() {
        long array1[][] = new long[2][];
        long array2[][] = new long[2][];
        long array3[][] = new long[3][];
        for (int i = 0; i < array1.length; ++i) {
            array1[i] = new long[2];
            array2[i] = new long[2];
            array3[i] = new long[3];
            for (int j = 0; j < array1[i].length; ++j) {
                array1[i][j] = (i + 1) * (j + 1);
                array2[i][j] = (i + 1) * (j + 1);
                array3[i][j] = (i + 1) * (j + 1);
            }
        }
        array3[1][2] = 100;
        array3[1][2] = 100;
        
        
        assertTrue(new CompareToBuilder().append(array1, array1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(array1, array2).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(array1, array3).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(array3, array1).toComparison() > 0);
        array1[1][1] = 200;
        assertTrue(new CompareToBuilder().append(array1, array2).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(array2, array1).toComparison() < 0);
    }

    @Test
    public void testMixedArray() {
        Object array1[] = new Object[2];
        Object array2[] = new Object[2];
        Object array3[] = new Object[2];
        for (int i = 0; i < array1.length; ++i) {
            array1[i] = new long[2];
            array2[i] = new long[2];
            array3[i] = new long[3];
            for (int j = 0; j < 2; ++j) {
                ((long[]) array1[i])[j] = (i + 1) * (j + 1);
                ((long[]) array2[i])[j] = (i + 1) * (j + 1);
                ((long[]) array3[i])[j] = (i + 1) * (j + 1);
            }
        }
        ((long[]) array3[0])[2] = 1;
        ((long[]) array3[1])[2] = 1;
        assertTrue(new CompareToBuilder().append(array1, array1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(array1, array2).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(array1, array3).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(array3, array1).toComparison() > 0);
        ((long[]) array1[1])[1] = 200;
        assertTrue(new CompareToBuilder().append(array1, array2).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(array2, array1).toComparison() < 0);
    }

    @Test
    public void testObjectArrayHiddenByObject() {
        TestObject[] array1 = new TestObject[2];
        array1[0] = new TestObject(4);
        array1[1] = new TestObject(5);
        TestObject[] array2 = new TestObject[2];
        array2[0] = new TestObject(4);
        array2[1] = new TestObject(5);
        TestObject[] array3 = new TestObject[3];
        array3[0] = new TestObject(4);
        array3[1] = new TestObject(5);
        array3[2] = new TestObject(6);
        
        Object obj1 = array1;
        Object obj2 = array2;
        Object obj3 = array3;
        
        assertTrue(new CompareToBuilder().append(obj1, obj1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj3).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(obj3, obj1).toComparison() > 0);

        array1[1] = new TestObject(7);
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(obj2, obj1).toComparison() < 0);
    }

    @Test
    public void testLongArrayHiddenByObject() {
        long[] array1 = new long[2];
        array1[0] = 5L;
        array1[1] = 6L;
        long[] array2 = new long[2];
        array2[0] = 5L;
        array2[1] = 6L;
        long[] array3 = new long[3];
        array3[0] = 5L;
        array3[1] = 6L;
        array3[2] = 7L;
        Object obj1 = array1;
        Object obj2 = array2;
        Object obj3 = array3;
        assertTrue(new CompareToBuilder().append(obj1, obj1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj3).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(obj3, obj1).toComparison() > 0);

        array1[1] = 7;
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(obj2, obj1).toComparison() < 0);
    }

    @Test
    public void testIntArrayHiddenByObject() {
        int[] array1 = new int[2];
        array1[0] = 5;
        array1[1] = 6;
        int[] array2 = new int[2];
        array2[0] = 5;
        array2[1] = 6;
        int[] array3 = new int[3];
        array3[0] = 5;
        array3[1] = 6;
        array3[2] = 7;
        Object obj1 = array1;
        Object obj2 = array2;
        Object obj3 = array3;
        assertTrue(new CompareToBuilder().append(obj1, obj1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj3).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(obj3, obj1).toComparison() > 0);

        array1[1] = 7;
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(obj2, obj1).toComparison() < 0);
    }

    @Test
    public void testShortArrayHiddenByObject() {
        short[] array1 = new short[2];
        array1[0] = 5;
        array1[1] = 6;
        short[] array2 = new short[2];
        array2[0] = 5;
        array2[1] = 6;
        short[] array3 = new short[3];
        array3[0] = 5;
        array3[1] = 6;
        array3[2] = 7;
        Object obj1 = array1;
        Object obj2 = array2;
        Object obj3 = array3;
        assertTrue(new CompareToBuilder().append(obj1, obj1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj3).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(obj3, obj1).toComparison() > 0);

        array1[1] = 7;
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(obj2, obj1).toComparison() < 0);
    }

    @Test
    public void testCharArrayHiddenByObject() {
        char[] array1 = new char[2];
        array1[0] = 5;
        array1[1] = 6;
        char[] array2 = new char[2];
        array2[0] = 5;
        array2[1] = 6;
        char[] array3 = new char[3];
        array3[0] = 5;
        array3[1] = 6;
        array3[2] = 7;
        Object obj1 = array1;
        Object obj2 = array2;
        Object obj3 = array3;
        assertTrue(new CompareToBuilder().append(obj1, obj1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj3).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(obj3, obj1).toComparison() > 0);

        array1[1] = 7;
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(obj2, obj1).toComparison() < 0);
    }

    @Test
    public void testByteArrayHiddenByObject() {
        byte[] array1 = new byte[2];
        array1[0] = 5;
        array1[1] = 6;
        byte[] array2 = new byte[2];
        array2[0] = 5;
        array2[1] = 6;
        byte[] array3 = new byte[3];
        array3[0] = 5;
        array3[1] = 6;
        array3[2] = 7;
        Object obj1 = array1;
        Object obj2 = array2;
        Object obj3 = array3;
        assertTrue(new CompareToBuilder().append(obj1, obj1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj3).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(obj3, obj1).toComparison() > 0);

        array1[1] = 7;
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(obj2, obj1).toComparison() < 0);
    }

    @Test
    public void testDoubleArrayHiddenByObject() {
        double[] array1 = new double[2];
        array1[0] = 5;
        array1[1] = 6;
        double[] array2 = new double[2];
        array2[0] = 5;
        array2[1] = 6;
        double[] array3 = new double[3];
        array3[0] = 5;
        array3[1] = 6;
        array3[2] = 7;
        Object obj1 = array1;
        Object obj2 = array2;
        Object obj3 = array3;
        assertTrue(new CompareToBuilder().append(obj1, obj1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj3).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(obj3, obj1).toComparison() > 0);

        array1[1] = 7;
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(obj2, obj1).toComparison() < 0);
    }

    @Test
    public void testFloatArrayHiddenByObject() {
        float[] array1 = new float[2];
        array1[0] = 5;
        array1[1] = 6;
        float[] array2 = new float[2];
        array2[0] = 5;
        array2[1] = 6;
        float[] array3 = new float[3];
        array3[0] = 5;
        array3[1] = 6;
        array3[2] = 7;
        Object obj1 = array1;
        Object obj2 = array2;
        Object obj3 = array3;
        assertTrue(new CompareToBuilder().append(obj1, obj1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj3).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(obj3, obj1).toComparison() > 0);

        array1[1] = 7;
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(obj2, obj1).toComparison() < 0);
    }

    @Test
    public void testBooleanArrayHiddenByObject() {
        boolean[] array1 = new boolean[2];
        array1[0] = true;
        array1[1] = false;
        boolean[] array2 = new boolean[2];
        array2[0] = true;
        array2[1] = false;
        boolean[] array3 = new boolean[3];
        array3[0] = true;
        array3[1] = false;
        array3[2] = true;
        Object obj1 = array1;
        Object obj2 = array2;
        Object obj3 = array3;
        assertTrue(new CompareToBuilder().append(obj1, obj1).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() == 0);
        assertTrue(new CompareToBuilder().append(obj1, obj3).toComparison() < 0);
        assertTrue(new CompareToBuilder().append(obj3, obj1).toComparison() > 0);

        array1[1] = true;
        assertTrue(new CompareToBuilder().append(obj1, obj2).toComparison() > 0);
        assertTrue(new CompareToBuilder().append(obj2, obj1).toComparison() < 0);
    }
  
 }
