/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.commons.lang3.builder;

import static org.apache.commons.lang3.LangAssertions.assertIllegalArgumentException;
import static org.apache.commons.lang3.LangAssertions.assertNullPointerException;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link org.apache.commons.lang3.builder.HashCodeBuilder}.
 */
class HashCodeBuilderTest extends AbstractLangTest {

    /**
     * A reflection test fixture.
     */
    static class ReflectionTestCycleA {
        ReflectionTestCycleB b;

        @Override
        public boolean equals(final Object o) {
            // Pairs with hashCode()
            return super.equals(o);
        }

        @Override
        public int hashCode() {
            return HashCodeBuilder.reflectionHashCode(this);
        }
    }

    /**
     * A reflection test fixture.
     */
    static class ReflectionTestCycleB {
        ReflectionTestCycleA a;

        @Override
        public boolean equals(final Object o) {
            // Pairs with hashCode()
            return super.equals(o);
        }

        @Override
        public int hashCode() {
            return HashCodeBuilder.reflectionHashCode(this);
        }
    }

    static class TestObject {
        private int a;

        TestObject(final int a) {
            this.a = a;
        }

        @Override
        public boolean equals(final Object o) {
            if (o == this) {
                return true;
            }
            if (!(o instanceof TestObject)) {
                return false;
            }
            final TestObject rhs = (TestObject) o;
            return a == rhs.a;
        }

        public int getA() {
            return a;
        }

        @Override
        public int hashCode() {
            return a;
        }

        public void setA(final int a) {
            this.a = a;
        }
    }

    static class TestObjectHashCodeExclude {
        @HashCodeExclude
        private final int a;
        private final int b;

        TestObjectHashCodeExclude(final int a, final int b) {
            this.a = a;
            this.b = b;
        }

        public int getA() {
            return a;
        }

        public int getB() {
            return b;
        }
    }

    static class TestObjectHashCodeExclude2 {
        @HashCodeExclude
        private final int a;
        @HashCodeExclude
        private final int b;

        TestObjectHashCodeExclude2(final int a, final int b) {
            this.a = a;
            this.b = b;
        }

        public int getA() {
            return a;
        }

        public int getB() {
            return b;
        }
    }

    static class TestObjectWithMultipleFields {
        @SuppressWarnings("unused")
        private final int one;

        @SuppressWarnings("unused")
        private final int two;

        @SuppressWarnings("unused")
        private final int three;

        TestObjectWithMultipleFields(final int one, final int two, final int three) {
            this.one = one;
            this.two = two;
            this.three = three;
        }
    }

    static class TestSubObject extends TestObject {
        private int b;

        @SuppressWarnings("unused")
        private transient int t;

        TestSubObject() {
            super(0);
        }

        TestSubObject(final int a, final int b, final int t) {
            super(a);
            this.b = b;
            this.t = t;
        }

        @Override
        public boolean equals(final Object o) {
            if (o == this) {
                return true;
            }
            if (!(o instanceof TestSubObject)) {
                return false;
            }
            final TestSubObject rhs = (TestSubObject) o;
            return super.equals(o) && b == rhs.b;
        }

        @Override
        public int hashCode() {
            return b * 17 + super.hashCode();
        }

    }

    @Test
    void testBoolean() {
        assertEquals(17 * 37 + 0, new HashCodeBuilder(17, 37).append(true).toHashCode());
        assertEquals(17 * 37 + 1, new HashCodeBuilder(17, 37).append(false).toHashCode());
    }

    @Test
    void testBooleanArray() {
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append((boolean[]) null).toHashCode());
        final boolean[] obj = new boolean[2];
        assertEquals((17 * 37 + 1) * 37 + 1, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[0] = true;
        assertEquals((17 * 37 + 0) * 37 + 1, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[1] = false;
        assertEquals((17 * 37 + 0) * 37 + 1, new HashCodeBuilder(17, 37).append(obj).toHashCode());
    }

    @Test
    void testBooleanArrayAsObject() {
        final boolean[] obj = new boolean[2];
        assertEquals((17 * 37 + 1) * 37 + 1, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[0] = true;
        assertEquals((17 * 37 + 0) * 37 + 1, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[1] = false;
        assertEquals((17 * 37 + 0) * 37 + 1, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
    }

    @Test
    void testBooleanMultiArray() {
        final boolean[][] obj = new boolean[2][];
        assertEquals(17 * 37 * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[0] = new boolean[0];
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[0] = new boolean[1];
        assertEquals((17 * 37 + 1) * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[0] = new boolean[2];
        assertEquals(((17 * 37 + 1) * 37 + 1) * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[0][0] = true;
        assertEquals(((17 * 37 + 0) * 37 + 1) * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[1] = new boolean[1];
        assertEquals(((17 * 37 + 0) * 37 + 1) * 37 + 1, new HashCodeBuilder(17, 37).append(obj).toHashCode());
    }

    @Test
    void testByte() {
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append((byte) 0).toHashCode());
        assertEquals(17 * 37 + 123, new HashCodeBuilder(17, 37).append((byte) 123).toHashCode());
    }

    @Test
    void testByteArray() {
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append((byte[]) null).toHashCode());
        final byte[] obj = new byte[2];
        assertEquals(17 * 37 * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[0] = (byte) 5;
        assertEquals((17 * 37 + 5) * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[1] = (byte) 6;
        assertEquals((17 * 37 + 5) * 37 + 6, new HashCodeBuilder(17, 37).append(obj).toHashCode());
    }

    @Test
    void testByteArrayAsObject() {
        final byte[] obj = new byte[2];
        assertEquals(17 * 37 * 37, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[0] = (byte) 5;
        assertEquals((17 * 37 + 5) * 37, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[1] = (byte) 6;
        assertEquals((17 * 37 + 5) * 37 + 6, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
    }

    @Test
    void testChar() {
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append((char) 0).toHashCode());
        assertEquals(17 * 37 + 1234, new HashCodeBuilder(17, 37).append((char) 1234).toHashCode());
    }

    @Test
    void testCharArray() {
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append((char[]) null).toHashCode());
        final char[] obj = new char[2];
        assertEquals(17 * 37 * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[0] = (char) 5;
        assertEquals((17 * 37 + 5) * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[1] = (char) 6;
        assertEquals((17 * 37 + 5) * 37 + 6, new HashCodeBuilder(17, 37).append(obj).toHashCode());
    }

    @Test
    void testCharArrayAsObject() {
        final char[] obj = new char[2];
        assertEquals(17 * 37 * 37, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[0] = (char) 5;
        assertEquals((17 * 37 + 5) * 37, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[1] = (char) 6;
        assertEquals((17 * 37 + 5) * 37 + 6, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
    }

    @Test
    void testConstructorExEvenFirst() {
        assertIllegalArgumentException(() -> new HashCodeBuilder(2, 3));
    }

    @Test
    void testConstructorExEvenNegative() {
        assertIllegalArgumentException(() -> new HashCodeBuilder(-2, -2));
    }

    @Test
    void testConstructorExEvenSecond() {
        assertIllegalArgumentException(() -> new HashCodeBuilder(3, 2));
    }

    @Test
    void testConstructorExZero() {
        assertIllegalArgumentException(() -> new HashCodeBuilder(0, 0));
    }

    @Test
    void testDouble() {
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append(0d).toHashCode());
        final double d = 1234567.89;
        final long l = Double.doubleToLongBits(d);
        assertEquals(17 * 37 + (int) (l ^ l >> 32), new HashCodeBuilder(17, 37).append(d).toHashCode());
    }

    @Test
    void testDoubleArray() {
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append((double[]) null).toHashCode());
        final double[] obj = new double[2];
        assertEquals(17 * 37 * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[0] = 5.4d;
        final long l1 = Double.doubleToLongBits(5.4d);
        final int h1 = (int) (l1 ^ l1 >> 32);
        assertEquals((17 * 37 + h1) * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[1] = 6.3d;
        final long l2 = Double.doubleToLongBits(6.3d);
        final int h2 = (int) (l2 ^ l2 >> 32);
        assertEquals((17 * 37 + h1) * 37 + h2, new HashCodeBuilder(17, 37).append(obj).toHashCode());
    }

    @Test
    void testDoubleArrayAsObject() {
        final double[] obj = new double[2];
        assertEquals(17 * 37 * 37, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[0] = 5.4d;
        final long l1 = Double.doubleToLongBits(5.4d);
        final int h1 = (int) (l1 ^ l1 >> 32);
        assertEquals((17 * 37 + h1) * 37, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[1] = 6.3d;
        final long l2 = Double.doubleToLongBits(6.3d);
        final int h2 = (int) (l2 ^ l2 >> 32);
        assertEquals((17 * 37 + h1) * 37 + h2, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
    }

    @Test
    void testEquals() {
        final HashCodeBuilder hcb1 = new HashCodeBuilder(17, 37).append(1).append('a');
        final HashCodeBuilder hcb2 = new HashCodeBuilder(17, 37).append(1).append('a');
        final HashCodeBuilder hcb3 = new HashCodeBuilder(17, 37).append(2).append('c');
        assertEquals(hcb1, hcb1);
        assertEquals(hcb1, hcb2);
        assertEquals(hcb2, hcb1);
        assertNotEquals(hcb1, hcb3);
        assertNotEquals(hcb2, hcb3);
    }

    @Test
    void testFloat() {
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append(0f).toHashCode());
        final float f = 1234.89f;
        final int i = Float.floatToIntBits(f);
        assertEquals(17 * 37 + i, new HashCodeBuilder(17, 37).append(f).toHashCode());
    }

    @Test
    void testFloatArray() {
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append((float[]) null).toHashCode());
        final float[] obj = new float[2];
        assertEquals(17 * 37 * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[0] = 5.4f;
        final int h1 = Float.floatToIntBits(5.4f);
        assertEquals((17 * 37 + h1) * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[1] = 6.3f;
        final int h2 = Float.floatToIntBits(6.3f);
        assertEquals((17 * 37 + h1) * 37 + h2, new HashCodeBuilder(17, 37).append(obj).toHashCode());
    }

    @Test
    void testFloatArrayAsObject() {
        final float[] obj = new float[2];
        assertEquals(17 * 37 * 37, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[0] = 5.4f;
        final int h1 = Float.floatToIntBits(5.4f);
        assertEquals((17 * 37 + h1) * 37, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[1] = 6.3f;
        final int h2 = Float.floatToIntBits(6.3f);
        assertEquals((17 * 37 + h1) * 37 + h2, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
    }

    @Test
    void testInt() {
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append(0).toHashCode());
        assertEquals(17 * 37 + 123456, new HashCodeBuilder(17, 37).append(123456).toHashCode());
    }

    @Test
    void testIntArray() {
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append((int[]) null).toHashCode());
        final int[] obj = new int[2];
        assertEquals(17 * 37 * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[0] = 5;
        assertEquals((17 * 37 + 5) * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[1] = 6;
        assertEquals((17 * 37 + 5) * 37 + 6, new HashCodeBuilder(17, 37).append(obj).toHashCode());
    }

    @Test
    void testIntArrayAsObject() {
        final int[] obj = new int[2];
        assertEquals(17 * 37 * 37, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[0] = 5;
        assertEquals((17 * 37 + 5) * 37, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[1] = 6;
        assertEquals((17 * 37 + 5) * 37 + 6, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
    }

    @Test
    void testLong() {
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append(0L).toHashCode());
        assertEquals(17 * 37 + (int) (123456789L ^ 123456789L >> 32), new HashCodeBuilder(17, 37).append(
                123456789L).toHashCode());
    }

    @Test
    void testLongArray() {
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append((long[]) null).toHashCode());
        final long[] obj = new long[2];
        assertEquals(17 * 37 * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[0] = 5L;
        final int h1 = (int) (5L ^ 5L >> 32);
        assertEquals((17 * 37 + h1) * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[1] = 6L;
        final int h2 = (int) (6L ^ 6L >> 32);
        assertEquals((17 * 37 + h1) * 37 + h2, new HashCodeBuilder(17, 37).append(obj).toHashCode());
    }

    @Test
    void testLongArrayAsObject() {
        final long[] obj = new long[2];
        assertEquals(17 * 37 * 37, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[0] = 5L;
        final int h1 = (int) (5L ^ 5L >> 32);
        assertEquals((17 * 37 + h1) * 37, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[1] = 6L;
        final int h2 = (int) (6L ^ 6L >> 32);
        assertEquals((17 * 37 + h1) * 37 + h2, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
    }

    @Test
    void testObject() {
        Object obj = null;
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj = new Object();
        assertEquals(17 * 37 + obj.hashCode(), new HashCodeBuilder(17, 37).append(obj).toHashCode());
    }

    @Test
    void testObjectArray() {
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append((Object[]) null).toHashCode());
        final Object[] obj = new Object[2];
        assertEquals(17 * 37 * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[0] = new Object();
        assertEquals((17 * 37 + obj[0].hashCode()) * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[1] = new Object();
        assertEquals((17 * 37 + obj[0].hashCode()) * 37 + obj[1].hashCode(), new HashCodeBuilder(17, 37).append(obj)
                .toHashCode());
    }

    @Test
    void testObjectArrayAsObject() {
        final Object[] obj = new Object[2];
        assertEquals(17 * 37 * 37, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[0] = new Object();
        assertEquals((17 * 37 + obj[0].hashCode()) * 37, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[1] = new Object();
        assertEquals((17 * 37 + obj[0].hashCode()) * 37 + obj[1].hashCode(), new HashCodeBuilder(17, 37).append(
                (Object) obj).toHashCode());
    }

    @Test
    void testObjectBuild() {
        Object obj = null;
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append(obj).build().intValue());
        obj = new Object();
        assertEquals(17 * 37 + obj.hashCode(), new HashCodeBuilder(17, 37).append(obj).build().intValue());
    }

    @Test
    void testReflectionHashCode() {
        assertEquals(17 * 37, HashCodeBuilder.reflectionHashCode(new TestObject(0)));
        assertEquals(17 * 37 + 123456, HashCodeBuilder.reflectionHashCode(new TestObject(123456)));
    }

    @Test
    void testReflectionHashCodeEx1() {
        assertIllegalArgumentException(() -> HashCodeBuilder.reflectionHashCode(0, 0, new TestObject(0), true));
    }

    @Test
    void testReflectionHashCodeEx2() {
        assertIllegalArgumentException(() -> HashCodeBuilder.reflectionHashCode(2, 2, new TestObject(0), true));
    }

    @Test
    void testReflectionHashCodeEx3() {
        assertNullPointerException(() -> HashCodeBuilder.reflectionHashCode(13, 19, null, true));
    }

    @Test
    void testReflectionHashCodeExcludeFields() {
        final TestObjectWithMultipleFields x = new TestObjectWithMultipleFields(1, 2, 3);

        assertEquals(((17 * 37 + 1) * 37 + 3) * 37 + 2, HashCodeBuilder.reflectionHashCode(x));

        assertEquals(((17 * 37 + 1) * 37 + 3) * 37 + 2, HashCodeBuilder.reflectionHashCode(x, (String[]) null));
        assertEquals(((17 * 37 + 1) * 37 + 3) * 37 + 2, HashCodeBuilder.reflectionHashCode(x));
        assertEquals(((17 * 37 + 1) * 37 + 3) * 37 + 2, HashCodeBuilder.reflectionHashCode(x, "xxx"));

        assertEquals((17 * 37 + 1) * 37 + 3, HashCodeBuilder.reflectionHashCode(x, "two"));
        assertEquals((17 * 37 + 1) * 37 + 2, HashCodeBuilder.reflectionHashCode(x, "three"));

        assertEquals(17 * 37 + 1, HashCodeBuilder.reflectionHashCode(x, "two", "three"));

        assertEquals(17, HashCodeBuilder.reflectionHashCode(x, "one", "two", "three"));
        assertEquals(17, HashCodeBuilder.reflectionHashCode(x, "one", "two", "three", "xxx"));
    }

    @Test
    void testReflectionHierarchyHashCode() {
        assertEquals(17 * 37 * 37, HashCodeBuilder.reflectionHashCode(new TestSubObject(0, 0, 0)));
        assertEquals(17 * 37 * 37 * 37, HashCodeBuilder.reflectionHashCode(new TestSubObject(0, 0, 0), true));
        assertEquals((17 * 37 + 7890) * 37 + 123456, HashCodeBuilder.reflectionHashCode(new TestSubObject(123456, 7890,
                0)));
        assertEquals(((17 * 37 + 7890) * 37 + 0) * 37 + 123456, HashCodeBuilder.reflectionHashCode(new TestSubObject(
                123456, 7890, 0), true));
    }

    @Test
    void testReflectionHierarchyHashCodeEx1() {
        assertIllegalArgumentException(() -> HashCodeBuilder.reflectionHashCode(0, 0, new TestSubObject(0, 0, 0), true));
    }

    @Test
    void testReflectionHierarchyHashCodeEx2() {
        assertIllegalArgumentException(() -> HashCodeBuilder.reflectionHashCode(2, 2, new TestSubObject(0, 0, 0), true));
    }

    /**
     * Test Objects pointing to each other.
     */
    @Test
    void testReflectionObjectCycle() {
        final ReflectionTestCycleA a = new ReflectionTestCycleA();
        final ReflectionTestCycleB b = new ReflectionTestCycleB();
        a.b = b;
        b.a = a;

        // Used to caused:
        // java.lang.StackOverflowError
        // at java.lang.ClassLoader.getCallerClassLoader(Native Method)
        // at java.lang.Class.getDeclaredFields(Class.java:992)
        // at org.apache.commons.lang.builder.HashCodeBuilder.reflectionAppend(HashCodeBuilder.java:373)
        // at org.apache.commons.lang.builder.HashCodeBuilder.reflectionHashCode(HashCodeBuilder.java:349)
        // at org.apache.commons.lang.builder.HashCodeBuilder.reflectionHashCode(HashCodeBuilder.java:155)
        // at
        // org.apache.commons.lang.builder.HashCodeBuilderTest$ReflectionTestCycleB.hashCode(HashCodeBuilderTest.java:53)
        // at org.apache.commons.lang.builder.HashCodeBuilder.append(HashCodeBuilder.java:422)
        // at org.apache.commons.lang.builder.HashCodeBuilder.reflectionAppend(HashCodeBuilder.java:383)
        // at org.apache.commons.lang.builder.HashCodeBuilder.reflectionHashCode(HashCodeBuilder.java:349)
        // at org.apache.commons.lang.builder.HashCodeBuilder.reflectionHashCode(HashCodeBuilder.java:155)
        // at
        // org.apache.commons.lang.builder.HashCodeBuilderTest$ReflectionTestCycleA.hashCode(HashCodeBuilderTest.java:42)
        // at org.apache.commons.lang.builder.HashCodeBuilder.append(HashCodeBuilder.java:422)

        a.hashCode();
        assertTrue(HashCodeBuilder.getRegistry().isEmpty());
        b.hashCode();
        assertTrue(HashCodeBuilder.getRegistry().isEmpty());
    }

    @Test
    void testShort() {
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append((short) 0).toHashCode());
        assertEquals(17 * 37 + 12345, new HashCodeBuilder(17, 37).append((short) 12345).toHashCode());
    }

    @Test
    void testShortArray() {
        assertEquals(17 * 37, new HashCodeBuilder(17, 37).append((short[]) null).toHashCode());
        final short[] obj = new short[2];
        assertEquals(17 * 37 * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[0] = (short) 5;
        assertEquals((17 * 37 + 5) * 37, new HashCodeBuilder(17, 37).append(obj).toHashCode());
        obj[1] = (short) 6;
        assertEquals((17 * 37 + 5) * 37 + 6, new HashCodeBuilder(17, 37).append(obj).toHashCode());
    }

    @Test
    void testShortArrayAsObject() {
        final short[] obj = new short[2];
        assertEquals(17 * 37 * 37, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[0] = (short) 5;
        assertEquals((17 * 37 + 5) * 37, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
        obj[1] = (short) 6;
        assertEquals((17 * 37 + 5) * 37 + 6, new HashCodeBuilder(17, 37).append((Object) obj).toHashCode());
    }

    @Test
    void testSuper() {
        final Object obj = new Object();
        assertEquals(17 * 37 + 19 * 41 + obj.hashCode(), new HashCodeBuilder(17, 37).appendSuper(
                new HashCodeBuilder(19, 41).append(obj).toHashCode()).toHashCode());
    }

    /**
     * Ensures LANG-520 remains true
     */
    @Test
    void testToHashCodeEqualsHashCode() {
        final HashCodeBuilder hcb = new HashCodeBuilder(17, 37).append(new Object()).append('a');
        assertEquals(hcb.toHashCode(), hcb.hashCode(),
            "hashCode() is no longer returning the same value as toHashCode() - see LANG-520");
    }

    @Test
    void testToHashCodeExclude() {
        final TestObjectHashCodeExclude one = new TestObjectHashCodeExclude(1, 2);
        final TestObjectHashCodeExclude2 two = new TestObjectHashCodeExclude2(1, 2);
        assertEquals(17 * 37 + 2, HashCodeBuilder.reflectionHashCode(one));
        assertEquals(17, HashCodeBuilder.reflectionHashCode(two));
    }

}
