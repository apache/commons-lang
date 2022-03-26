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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Unit tests for {@link org.apache.commons.lang3.builder.ToStringBuilder}.
 */
public class ToStringBuilderTest extends AbstractLangTest {

    // See LANG-1337 for more.
    private static final int ARRAYLIST_INITIAL_CAPACITY = 10;
    private final Integer base = Integer.valueOf(5);
    private final String baseStr = base.getClass().getName() + "@" + Integer.toHexString(System.identityHashCode(base));

    @Test
    public void testConstructorEx1() {
        assertEquals("<null>", new ToStringBuilder(null).toString());
    }

    @Test
    public void testConstructorEx2() {
        assertEquals("<null>", new ToStringBuilder(null, null).toString());
        new ToStringBuilder(this.base, null).toString();
    }

    @Test
    public void testConstructorEx3() {
        assertEquals("<null>", new ToStringBuilder(null, null, null).toString());
        new ToStringBuilder(this.base, null, null).toString();
        new ToStringBuilder(this.base, ToStringStyle.DEFAULT_STYLE, null).toString();
    }

    @Test
    public void testGetSetDefault() {
        try {
            ToStringBuilder.setDefaultStyle(ToStringStyle.NO_FIELD_NAMES_STYLE);
            assertSame(ToStringStyle.NO_FIELD_NAMES_STYLE, ToStringBuilder.getDefaultStyle());
        } finally {
            // reset for other tests
            ToStringBuilder.setDefaultStyle(ToStringStyle.DEFAULT_STYLE);
        }
    }

    @Test
    public void testSetDefaultEx() {
        assertThrows(NullPointerException.class, () -> ToStringBuilder.setDefaultStyle(null));
    }

    @Test
    public void testBlank() {
        assertEquals(baseStr + "[]", new ToStringBuilder(base).toString());
    }

    /**
     * Test wrapper for int primitive.
     */
    @Test
    public void testReflectionInteger() {
        assertEquals(baseStr + "[value=5]", ToStringBuilder.reflectionToString(base));
    }

    /**
     * Test wrapper for char primitive.
     */
    @Test
    public void testReflectionCharacter() {
        final Character c = 'A';
        assertEquals(this.toBaseString(c) + "[value=A]", ToStringBuilder.reflectionToString(c));
    }

    /**
     * Test wrapper for char boolean.
     */
    @Test
    public void testReflectionBoolean() {
        Boolean b;
        b = Boolean.TRUE;
        assertEquals(this.toBaseString(b) + "[value=true]", ToStringBuilder.reflectionToString(b));
        b = Boolean.FALSE;
        assertEquals(this.toBaseString(b) + "[value=false]", ToStringBuilder.reflectionToString(b));
    }

    /**
     * Create the same toString() as Object.toString().
     * @param o the object to create the string for.
     * @return a String in the Object.toString format.
     */
    private String toBaseString(final Object o) {
        return o.getClass().getName() + "@" + Integer.toHexString(System.identityHashCode(o));
    }

    // Reflection Array tests

    //
    // Note on the following line of code repeated in the reflection array tests.
    //
    // assertReflectionArray("<null>", array);
    //
    // The expected value is not baseStr + "[<null>]" since array==null and is typed as Object.
    // The null array does not carry array type information.
    // If we added a primitive array type constructor and pile of associated methods,
    // then type declaring type information could be carried forward. IMHO, null is null.
    //
    // Gary Gregory - 2003-03-12 - ggregory@seagullsw.com
    //

    public void assertReflectionArray(final String expected, final Object actual) {
        if (actual == null) {
            // Until ToStringBuilder supports null objects.
            return;
        }
        assertEquals(expected, ToStringBuilder.reflectionToString(actual));
        assertEquals(expected, ToStringBuilder.reflectionToString(actual, null));
        assertEquals(expected, ToStringBuilder.reflectionToString(actual, null, true));
        assertEquals(expected, ToStringBuilder.reflectionToString(actual, null, false));
    }

    @Test
    public void testReflectionObjectArray() {
        Object[] array = { null, base, new int[] { 3, 6 } };
        final String baseString = this.toBaseString(array);
        assertEquals(baseString + "[{<null>,5,{3,6}}]", ToStringBuilder.reflectionToString(array));
        array = null;
        assertReflectionArray("<null>", array);
    }

    @Test
    public void testReflectionLongArray() {
        long[] array = { 1, 2, -3, 4 };
        final String baseString = this.toBaseString(array);
        assertEquals(baseString + "[{1,2,-3,4}]", ToStringBuilder.reflectionToString(array));
        array = null;
        assertReflectionArray("<null>", array);
    }

    @Test
    public void testReflectionIntArray() {
        int[] array = { 1, 2, -3, 4 };
        final String baseString = this.toBaseString(array);
        assertEquals(baseString + "[{1,2,-3,4}]", ToStringBuilder.reflectionToString(array));
        array = null;
        assertReflectionArray("<null>", array);
    }

    @Test
    public void testReflectionShortArray() {
        short[] array = { 1, 2, -3, 4 };
        final String baseString = this.toBaseString(array);
        assertEquals(baseString + "[{1,2,-3,4}]", ToStringBuilder.reflectionToString(array));
        array = null;
        assertReflectionArray("<null>", array);
    }

    @Test
    public void testReflectionyteArray() {
        byte[] array = { 1, 2, -3, 4 };
        final String baseString = this.toBaseString(array);
        assertEquals(baseString + "[{1,2,-3,4}]", ToStringBuilder.reflectionToString(array));
        array = null;
        assertReflectionArray("<null>", array);
    }

    @Test
    public void testReflectionCharArray() {
        char[] array = { 'A', '2', '_', 'D' };
        final String baseString = this.toBaseString(array);
        assertEquals(baseString + "[{A,2,_,D}]", ToStringBuilder.reflectionToString(array));
        array = null;
        assertReflectionArray("<null>", array);
    }

    @Test
    public void testReflectionDoubleArray() {
        double[] array = { 1.0, 2.9876, -3.00001, 4.3 };
        final String baseString = this.toBaseString(array);
        assertEquals(baseString + "[{1.0,2.9876,-3.00001,4.3}]", ToStringBuilder.reflectionToString(array));
        array = null;
        assertReflectionArray("<null>", array);
    }

    @Test
    public void testReflectionFloatArray() {
        float[] array = { 1.0f, 2.9876f, -3.00001f, 4.3f };
        final String baseString = this.toBaseString(array);
        assertEquals(baseString + "[{1.0,2.9876,-3.00001,4.3}]", ToStringBuilder.reflectionToString(array));
        array = null;
        assertReflectionArray("<null>", array);
    }

    @Test
    public void testReflectionBooleanArray() {
        boolean[] array = { true, false, false };
        final String baseString = this.toBaseString(array);
        assertEquals(baseString + "[{true,false,false}]", ToStringBuilder.reflectionToString(array));
        array = null;
        assertReflectionArray("<null>", array);
    }

    // Reflection Array Array tests

    @Test
    public void testReflectionFloatArrayArray() {
        float[][] array = { { 1.0f, 2.29686f }, null, { Float.NaN } };
        final String baseString = this.toBaseString(array);
        assertEquals(baseString + "[{{1.0,2.29686},<null>,{NaN}}]", ToStringBuilder.reflectionToString(array));
        array = null;
        assertReflectionArray("<null>", array);
    }


    @Test
    public void testReflectionLongArrayArray() {
        long[][] array = { { 1, 2 }, null, { 5 } };
        final String baseString = this.toBaseString(array);
        assertEquals(baseString + "[{{1,2},<null>,{5}}]", ToStringBuilder.reflectionToString(array));
        array = null;
        assertReflectionArray("<null>", array);
    }

    @Test
    public void testReflectionIntArrayArray() {
        int[][] array = { { 1, 2 }, null, { 5 } };
        final String baseString = this.toBaseString(array);
        assertEquals(baseString + "[{{1,2},<null>,{5}}]", ToStringBuilder.reflectionToString(array));
        array = null;
        assertReflectionArray("<null>", array);
    }

    @Test
    public void testReflectionhortArrayArray() {
        short[][] array = { { 1, 2 }, null, { 5 } };
        final String baseString = this.toBaseString(array);
        assertEquals(baseString + "[{{1,2},<null>,{5}}]", ToStringBuilder.reflectionToString(array));
        array = null;
        assertReflectionArray("<null>", array);
    }

    @Test
    public void testReflectionByteArrayArray() {
        byte[][] array = { { 1, 2 }, null, { 5 } };
        final String baseString = this.toBaseString(array);
        assertEquals(baseString + "[{{1,2},<null>,{5}}]", ToStringBuilder.reflectionToString(array));
        array = null;
        assertReflectionArray("<null>", array);
    }

    @Test
    public void testReflectionCharArrayArray() {
        char[][] array = { { 'A', 'B' }, null, { 'p' } };
        final String baseString = this.toBaseString(array);
        assertEquals(baseString + "[{{A,B},<null>,{p}}]", ToStringBuilder.reflectionToString(array));
        array = null;
        assertReflectionArray("<null>", array);
    }

    @Test
    public void testReflectionDoubleArrayArray() {
        double[][] array = { { 1.0, 2.29686 }, null, { Double.NaN } };
        final String baseString = this.toBaseString(array);
        assertEquals(baseString + "[{{1.0,2.29686},<null>,{NaN}}]", ToStringBuilder.reflectionToString(array));
        array = null;
        assertReflectionArray("<null>", array);
    }

    @Test
    public void testReflectionBooleanArrayArray() {
        boolean[][] array = { { true, false }, null, { false } };
        final String baseString = this.toBaseString(array);
        assertEquals(baseString + "[{{true,false},<null>,{false}}]", ToStringBuilder.reflectionToString(array));
        assertEquals(baseString + "[{{true,false},<null>,{false}}]", ToStringBuilder.reflectionToString(array));
        array = null;
        assertReflectionArray("<null>", array);
    }

    // Reflection hierarchy tests
    @Test
    public void testReflectionHierarchyArrayList() {
        // LANG-1337 without this, the generated string can differ depending on the JVM version/vendor
        final List<Object> list = new ArrayList<>(ARRAYLIST_INITIAL_CAPACITY);
        final String baseString = this.toBaseString(list);
        final String expectedWithTransients = baseString + "[elementData={<null>,<null>,<null>,<null>,<null>,<null>,<null>,<null>,<null>,<null>},size=0,modCount=0]";
        final String toStringWithTransients = ToStringBuilder.reflectionToString(list, null, true);
        if (!expectedWithTransients.equals(toStringWithTransients)) {
            assertEquals(expectedWithTransients, toStringWithTransients);
        }
        final String expectedWithoutTransients = baseString + "[size=0]";
        final String toStringWithoutTransients = ToStringBuilder.reflectionToString(list, null, false);
        if (!expectedWithoutTransients.equals(toStringWithoutTransients)) {
            assertEquals(expectedWithoutTransients, toStringWithoutTransients);
        }
    }

    @Test
    public void testReflectionHierarchy() {
        final ReflectionTestFixtureA baseA = new ReflectionTestFixtureA();
        String baseString = this.toBaseString(baseA);
        assertEquals(baseString + "[a=a]", ToStringBuilder.reflectionToString(baseA));
        assertEquals(baseString + "[a=a]", ToStringBuilder.reflectionToString(baseA, null));
        assertEquals(baseString + "[a=a]", ToStringBuilder.reflectionToString(baseA, null, false));
        assertEquals(baseString + "[a=a,transientA=t]", ToStringBuilder.reflectionToString(baseA, null, true));
        assertEquals(baseString + "[a=a]", ToStringBuilder.reflectionToString(baseA, null, false, null));
        assertEquals(baseString + "[a=a]", ToStringBuilder.reflectionToString(baseA, null, false, Object.class));
        assertEquals(baseString + "[a=a]", ToStringBuilder.reflectionToString(baseA, null, false, ReflectionTestFixtureA.class));

        final ReflectionTestFixtureB baseB = new ReflectionTestFixtureB();
        baseString = this.toBaseString(baseB);
        assertEquals(baseString + "[b=b,a=a]", ToStringBuilder.reflectionToString(baseB));
        assertEquals(baseString + "[b=b,a=a]", ToStringBuilder.reflectionToString(baseB));
        assertEquals(baseString + "[b=b,a=a]", ToStringBuilder.reflectionToString(baseB, null));
        assertEquals(baseString + "[b=b,a=a]", ToStringBuilder.reflectionToString(baseB, null, false));
        assertEquals(baseString + "[b=b,transientB=t,a=a,transientA=t]", ToStringBuilder.reflectionToString(baseB, null, true));
        assertEquals(baseString + "[b=b,a=a]", ToStringBuilder.reflectionToString(baseB, null, false, null));
        assertEquals(baseString + "[b=b,a=a]", ToStringBuilder.reflectionToString(baseB, null, false, Object.class));
        assertEquals(baseString + "[b=b,a=a]", ToStringBuilder.reflectionToString(baseB, null, false, ReflectionTestFixtureA.class));
        assertEquals(baseString + "[b=b]", ToStringBuilder.reflectionToString(baseB, null, false, ReflectionTestFixtureB.class));
    }

    static class ReflectionTestFixtureA {
        @SuppressWarnings("unused")
        private final char a='a';
        @SuppressWarnings("unused")
        private final transient char transientA='t';
    }

    static class ReflectionTestFixtureB extends ReflectionTestFixtureA {
        @SuppressWarnings("unused")
        private final char b='b';
        @SuppressWarnings("unused")
        private final transient char transientB='t';
    }

    @Test
    public void testInnerClassReflection() {
        final Outer outer = new Outer();
        assertEquals(toBaseString(outer) + "[inner=" + toBaseString(outer.inner) + "[]]", outer.toString());
    }

    static class Outer {
        Inner inner = new Inner();
        class Inner {
            @Override
            public String toString() {
                return ToStringBuilder.reflectionToString(this);
            }
        }
        @Override
        public String toString() {
            return ToStringBuilder.reflectionToString(this);
        }
    }

    // Reflection cycle tests

    /**
     * Test an array element pointing to its container.
     */
    @Test
    public void testReflectionArrayCycle() {
        final Object[] objects = new Object[1];
        objects[0] = objects;
        assertEquals(
            this.toBaseString(objects) + "[{" + this.toBaseString(objects) + "}]",
            ToStringBuilder.reflectionToString(objects));
    }

    /**
     * Test an array element pointing to its container.
     */
    @Test
    public void testReflectionArrayCycleLevel2() {
        final Object[] objects = new Object[1];
        final Object[] objectsLevel2 = new Object[1];
        objects[0] = objectsLevel2;
        objectsLevel2[0] = objects;
        assertEquals(
            this.toBaseString(objects) + "[{{" + this.toBaseString(objects) + "}}]",
            ToStringBuilder.reflectionToString(objects));
        assertEquals(
            this.toBaseString(objectsLevel2) + "[{{" + this.toBaseString(objectsLevel2) + "}}]",
            ToStringBuilder.reflectionToString(objectsLevel2));
    }

    @Test
    public void testReflectionArrayArrayCycle() {
        final Object[][] objects = new Object[2][2];
        objects[0][0] = objects;
        objects[0][1] = objects;
        objects[1][0] = objects;
        objects[1][1] = objects;
        final String basicToString = this.toBaseString(objects);
        assertEquals(
            basicToString
                + "[{{"
                + basicToString
                + ","
                + basicToString
                + "},{"
                + basicToString
                + ","
                + basicToString
                + "}}]",
            ToStringBuilder.reflectionToString(objects));
    }

    /**
     * A reflection test fixture.
     */
    static class ReflectionTestCycleA {
        ReflectionTestCycleB b;

        @Override
        public String toString() {
            return ToStringBuilder.reflectionToString(this);
        }
    }

    /**
     * A reflection test fixture.
     */
    static class ReflectionTestCycleB {
        ReflectionTestCycleA a;

        @Override
        public String toString() {
            return ToStringBuilder.reflectionToString(this);
        }
    }

    /**
     * A reflection test fixture.
     */
    static class SimpleReflectionTestFixture {
        Object o;

        SimpleReflectionTestFixture() {
        }

        SimpleReflectionTestFixture(final Object o) {
            this.o = o;
        }

        @Override
        public String toString() {
            return ToStringBuilder.reflectionToString(this);
        }
    }

    private static class SelfInstanceVarReflectionTestFixture {
        @SuppressWarnings("unused")
        private final SelfInstanceVarReflectionTestFixture typeIsSelf;

        SelfInstanceVarReflectionTestFixture() {
            this.typeIsSelf = this;
        }

        @Override
        public String toString() {
            return ToStringBuilder.reflectionToString(this);
        }
      }

    private static class SelfInstanceTwoVarsReflectionTestFixture {
        @SuppressWarnings("unused")
        private final SelfInstanceTwoVarsReflectionTestFixture typeIsSelf;
        private final String otherType = "The Other Type";

        SelfInstanceTwoVarsReflectionTestFixture() {
            this.typeIsSelf = this;
        }

        public String getOtherType() {
            return this.otherType;
        }

        @Override
        public String toString() {
            return ToStringBuilder.reflectionToString(this);
        }
      }


    /**
     * Test an Object pointing to itself, the simplest test.
     */
    @Test
    public void testSimpleReflectionObjectCycle() {
        final SimpleReflectionTestFixture simple = new SimpleReflectionTestFixture();
        simple.o = simple;
        assertEquals(this.toBaseString(simple) + "[o=" + this.toBaseString(simple) + "]", simple.toString());
    }

    /**
     * Test a class that defines an ivar pointing to itself.
     */
    @Test
    public void testSelfInstanceVarReflectionObjectCycle() {
        final SelfInstanceVarReflectionTestFixture test = new SelfInstanceVarReflectionTestFixture();
        assertEquals(this.toBaseString(test) + "[typeIsSelf=" + this.toBaseString(test) + "]", test.toString());
    }

    /**
     * Test a class that defines an ivar pointing to itself.  This test was
     * created to show that handling cyclical object resulted in a missing endFieldSeparator call.
     */
    @Test
    public void testSelfInstanceTwoVarsReflectionObjectCycle() {
        final SelfInstanceTwoVarsReflectionTestFixture test = new SelfInstanceTwoVarsReflectionTestFixture();
        assertEquals(this.toBaseString(test) + "[otherType=" + test.getOtherType().toString() + ",typeIsSelf=" + this.toBaseString(test)  + "]", test.toString());
    }


    /**
     * Test Objects pointing to each other.
     */
    @Test
    public void testReflectionObjectCycle() {
        final ReflectionTestCycleA a = new ReflectionTestCycleA();
        final ReflectionTestCycleB b = new ReflectionTestCycleB();
        a.b = b;
        b.a = a;
        assertEquals(
            this.toBaseString(a) + "[b=" + this.toBaseString(b) + "[a=" + this.toBaseString(a) + "]]",
            a.toString());
    }

    /**
     * Test a nasty combination of arrays and Objects pointing to each other.
     * objects[0] -&gt; SimpleReflectionTestFixture[ o -&gt; objects ]
     */
    @Test
    public void testReflectionArrayAndObjectCycle() {
        final Object[] objects = new Object[1];
        final SimpleReflectionTestFixture simple = new SimpleReflectionTestFixture(objects);
        objects[0] = simple;
        assertEquals(
            this.toBaseString(objects)
                + "[{"
                + this.toBaseString(simple)
                + "[o="
                + this.toBaseString(objects)
                + "]"
                + "}]",
            ToStringBuilder.reflectionToString(objects));
        assertEquals(
            this.toBaseString(simple)
                + "[o={"
                + this.toBaseString(simple)
                + "}]",
            ToStringBuilder.reflectionToString(simple));
    }

    @Test
    public void testAppendSuper() {
        assertEquals(baseStr + "[]", new ToStringBuilder(base).appendSuper("Integer@8888[]").toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).appendSuper("Integer@8888[<null>]").toString());

        assertEquals(baseStr + "[a=hello]", new ToStringBuilder(base).appendSuper("Integer@8888[]").append("a", "hello").toString());
        assertEquals(baseStr + "[<null>,a=hello]", new ToStringBuilder(base).appendSuper("Integer@8888[<null>]").append("a", "hello").toString());
        assertEquals(baseStr + "[a=hello]", new ToStringBuilder(base).appendSuper(null).append("a", "hello").toString());
    }

    @Test
    public void testAppendToString() {
        assertEquals(baseStr + "[]", new ToStringBuilder(base).appendToString("Integer@8888[]").toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).appendToString("Integer@8888[<null>]").toString());

        assertEquals(baseStr + "[a=hello]", new ToStringBuilder(base).appendToString("Integer@8888[]").append("a", "hello").toString());
        assertEquals(baseStr + "[<null>,a=hello]", new ToStringBuilder(base).appendToString("Integer@8888[<null>]").append("a", "hello").toString());
        assertEquals(baseStr + "[a=hello]", new ToStringBuilder(base).appendToString(null).append("a", "hello").toString());
    }

    @Test
    public void testAppendAsObjectToString() {
        final String objectToAppend1 = "";
        final Boolean objectToAppend2 = Boolean.TRUE;
        final Object objectToAppend3 = new Object();

        assertEquals(baseStr + "[" + toBaseString(objectToAppend1) + "]",
                new ToStringBuilder(base).appendAsObjectToString(objectToAppend1).toString());
        assertEquals(baseStr + "[" + toBaseString(objectToAppend2) + "]",
                new ToStringBuilder(base).appendAsObjectToString(objectToAppend2).toString());
        assertEquals(baseStr + "[" + toBaseString(objectToAppend3) + "]",
                new ToStringBuilder(base).appendAsObjectToString(objectToAppend3).toString());
    }

    @Test
    public void testAppendAsObjectToStringNullPointerException() {
        ToStringBuilder builder = new ToStringBuilder(1);
        assertThrows(NullPointerException.class, () -> builder.appendAsObjectToString(null));
        builder.toString();
    }

    @Test
    public void testAppendBooleanArrayWithFieldName() {
        final boolean[] array = { true, false, false };
        assertEquals(baseStr + "[flags={true,false,false}]",
                new ToStringBuilder(base).append("flags", array).toString());
        assertEquals(baseStr + "[flags=<null>]",
                new ToStringBuilder(base).append("flags", (boolean[]) null).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(null, (boolean[]) null).toString());
        assertEquals(baseStr + "[{true,false,false}]", new ToStringBuilder(base).append(null, array).toString());
    }

    @Test
    public void testAppendBooleanArrayWithFieldNameAndFullDetatil() {
        final boolean[] array = { true, false, false };
        assertEquals(baseStr + "[flags={true,false,false}]",
                new ToStringBuilder(base).append("flags", array, true).toString());
        assertEquals(baseStr + "[length=<size=3>]",
                new ToStringBuilder(base).append("length", array, false).toString());
        assertEquals(baseStr + "[flags=<null>]",
                new ToStringBuilder(base).append("flags", (boolean[]) null, true).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(null, (boolean[]) null, false).toString());
        assertEquals(baseStr + "[<size=3>]", new ToStringBuilder(base).append(null, array, false).toString());
     }

    @Test
    public void testAppendCharArrayWithFieldName() {
        final char[] array = { 'A', '2', '_', 'D' };
        assertEquals(baseStr + "[chars={A,2,_,D}]", new ToStringBuilder(base).append("chars", array).toString());
        assertEquals(baseStr + "[letters={A,2,_,D}]", new ToStringBuilder(base).append("letters", array).toString());
        assertEquals(baseStr + "[flags=<null>]",
                new ToStringBuilder(base).append("flags", (boolean[]) null).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(null, (boolean[]) null).toString());
        assertEquals(baseStr + "[{A,2,_,D}]", new ToStringBuilder(base).append(null, array).toString());
    }

    @Test
    public void testAppendCharArrayWithFieldNameAndFullDetatil() {
        final char[] array = { 'A', '2', '_', 'D' };
        assertEquals(baseStr + "[chars={A,2,_,D}]", new ToStringBuilder(base).append("chars", array, true).toString());
        assertEquals(baseStr + "[letters=<size=4>]",
                new ToStringBuilder(base).append("letters", array, false).toString());
        assertEquals(baseStr + "[flags=<null>]",
                new ToStringBuilder(base).append("flags", (boolean[]) null, true).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(null, (boolean[]) null, false).toString());
        assertEquals(baseStr + "[<size=4>]", new ToStringBuilder(base).append(null, array, false).toString());
    }

    @Test
    public void testAppendDoubleArrayWithFieldName() {
        final double[] array = { 1.0, 2.9876, -3.00001, 4.3 };
        assertEquals(baseStr + "[values={1.0,2.9876,-3.00001,4.3}]",
                new ToStringBuilder(base).append("values", array).toString());
        assertEquals(baseStr + "[values=<null>]",
                new ToStringBuilder(base).append("values", (boolean[]) null).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(null, (boolean[]) null).toString());
        assertEquals(baseStr + "[{1.0,2.9876,-3.00001,4.3}]", new ToStringBuilder(base).append(null, array).toString());
    }

    @Test
    public void testAppendDoubleArrayWithFieldNameAndFullDetatil() {
        final double[] array = { 1.0, 2.9876, -3.00001, 4.3 };
        assertEquals(baseStr + "[values={1.0,2.9876,-3.00001,4.3}]",
                new ToStringBuilder(base).append("values", array, true).toString());
        assertEquals(baseStr + "[length=<size=4>]",
                new ToStringBuilder(base).append("length", array, false).toString());
        assertEquals(baseStr + "[values=<null>]",
                new ToStringBuilder(base).append("values", (boolean[]) null, true).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(null, (boolean[]) null, false).toString());
        assertEquals(baseStr + "[<size=4>]", new ToStringBuilder(base).append(null, array, false).toString());
    }

    @Test
    public void testAppendObjectArrayWithFieldName() {
        final Object[] array = { null, base, new int[] { 3, 6 } };
        assertEquals(baseStr + "[values={<null>,5,{3,6}}]",
                new ToStringBuilder(base).append("values", array).toString());
        assertEquals(baseStr + "[values=<null>]",
                new ToStringBuilder(base).append("values", (boolean[]) null).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(null, (boolean[]) null).toString());
        assertEquals(baseStr + "[{<null>,5,{3,6}}]", new ToStringBuilder(base).append(null, array).toString());
    }

    @Test
    public void testAppendObjectArrayWithFieldNameAndFullDetatil() {
       final Object[] array = { null, base, new int[] { 3, 6 } };
       assertEquals(baseStr + "[values={<null>,5,{3,6}}]",
               new ToStringBuilder(base).append("values", array, true).toString());
       assertEquals(baseStr + "[length=<size=3>]",
               new ToStringBuilder(base).append("length", array, false).toString());
       assertEquals(baseStr + "[values=<null>]",
               new ToStringBuilder(base).append("values", (boolean[]) null, true).toString());
       assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(null, (boolean[]) null, false).toString());
       assertEquals(baseStr + "[<size=3>]", new ToStringBuilder(base).append(null, array, false).toString());
    }

    @Test
    public void testAppendLongArrayWithFieldName() {
       final long[] array = { 1, 2, -3, 4 };
       assertEquals(baseStr + "[values={1,2,-3,4}]", new ToStringBuilder(base).append("values", array).toString());
       assertEquals(baseStr + "[values=<null>]",
               new ToStringBuilder(base).append("values", (boolean[]) null).toString());
       assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(null, (boolean[]) null).toString());
       assertEquals(baseStr + "[{1,2,-3,4}]", new ToStringBuilder(base).append(null, array).toString());
    }

    @Test
    public void testAppendLongArrayWithFieldNameAndFullDetatil() {
        final long[] array = { 1, 2, -3, 4 };
        assertEquals(baseStr + "[values={1,2,-3,4}]",
                new ToStringBuilder(base).append("values", array, true).toString());
        assertEquals(baseStr + "[length=<size=4>]",
                new ToStringBuilder(base).append("length", array, false).toString());
        assertEquals(baseStr + "[values=<null>]",
                new ToStringBuilder(base).append("values", (boolean[]) null, true).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(null, (boolean[]) null, false).toString());
        assertEquals(baseStr + "[<size=4>]", new ToStringBuilder(base).append(null, array, false).toString());
    }

    @Test
    public void testAppendIntArrayWithFieldName() {
        final int[] array = { 1, 2, -3, 4 };
        assertEquals(baseStr + "[values={1,2,-3,4}]", new ToStringBuilder(base).append("values", array).toString());
        assertEquals(baseStr + "[values=<null>]",
                new ToStringBuilder(base).append("values", (boolean[]) null).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(null, (boolean[]) null).toString());
        assertEquals(baseStr + "[{1,2,-3,4}]", new ToStringBuilder(base).append(null, array).toString());
    }

    @Test
    public void testAppendIntArrayWithFieldNameAndFullDetatil() {
        final int[] array = { 1, 2, -3, 4 };
        assertEquals(baseStr + "[values={1,2,-3,4}]",
                new ToStringBuilder(base).append("values", array, true).toString());
        assertEquals(baseStr + "[length=<size=4>]",
                new ToStringBuilder(base).append("length", array, false).toString());
        assertEquals(baseStr + "[values=<null>]",
                new ToStringBuilder(base).append("values", (boolean[]) null, true).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(null, (boolean[]) null, false).toString());
        assertEquals(baseStr + "[<size=4>]", new ToStringBuilder(base).append(null, array, false).toString());
    }

    @Test
    public void testAppendShortArrayWithFieldName() {
        final short[] array = { 1, 2, -3, 4 };
        assertEquals(baseStr + "[values={1,2,-3,4}]", new ToStringBuilder(base).append("values", array).toString());
        assertEquals(baseStr + "[values=<null>]",
                new ToStringBuilder(base).append("values", (boolean[]) null).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(null, (boolean[]) null).toString());
        assertEquals(baseStr + "[{1,2,-3,4}]", new ToStringBuilder(base).append(null, array).toString());
    }

    @Test
    public void testAppendShortArrayWithFieldNameAndFullDetatil() {
        final short[] array = { 1, 2, -3, 4 };
        assertEquals(baseStr + "[values={1,2,-3,4}]",
                new ToStringBuilder(base).append("values", array, true).toString());
        assertEquals(baseStr + "[length=<size=4>]",
                new ToStringBuilder(base).append("length", array, false).toString());
        assertEquals(baseStr + "[values=<null>]",
                new ToStringBuilder(base).append("values", (boolean[]) null, true).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(null, (boolean[]) null, false).toString());
        assertEquals(baseStr + "[<size=4>]", new ToStringBuilder(base).append(null, array, false).toString());
    }

    @Test
    public void testAppendByteArrayWithFieldName() {
        final byte[] array = { 1, 2, -3, 4 };
        assertEquals(baseStr + "[values={1,2,-3,4}]", new ToStringBuilder(base).append("values", array).toString());
        assertEquals(baseStr + "[values=<null>]",
                new ToStringBuilder(base).append("values", (boolean[]) null).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(null, (boolean[]) null).toString());
        assertEquals(baseStr + "[{1,2,-3,4}]", new ToStringBuilder(base).append(null, array).toString());
    }

    @Test
    public void testAppendByteArrayWithFieldNameAndFullDetatil() {
        final byte[] array = { 1, 2, -3, 4 };
        assertEquals(baseStr + "[values={1,2,-3,4}]",
                new ToStringBuilder(base).append("values", array, true).toString());
        assertEquals(baseStr + "[length=<size=4>]",
                new ToStringBuilder(base).append("length", array, false).toString());
        assertEquals(baseStr + "[values=<null>]",
                new ToStringBuilder(base).append("values", (boolean[]) null, true).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(null, (boolean[]) null, false).toString());
        assertEquals(baseStr + "[<size=4>]", new ToStringBuilder(base).append(null, array, false).toString());
    }

    @Test
    public void testAppendFloatArrayWithFieldName() {
        final float[] array = { 1.0f, 2.9876f, -3.00001f, 4.3f };
        assertEquals(baseStr + "[values={1.0,2.9876,-3.00001,4.3}]",
                new ToStringBuilder(base).append("values", array).toString());
        assertEquals(baseStr + "[values=<null>]",
                new ToStringBuilder(base).append("values", (boolean[]) null).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(null, (boolean[]) null).toString());
        assertEquals(baseStr + "[{1.0,2.9876,-3.00001,4.3}]", new ToStringBuilder(base).append(null, array).toString());
    }

    @Test
    public void testAppendFloatArrayWithFieldNameAndFullDetatil() {
        final float[] array = { 1.0f, 2.9876f, -3.00001f, 4.3f };
        assertEquals(baseStr + "[values={1.0,2.9876,-3.00001,4.3}]",
                new ToStringBuilder(base).append("values", array, true).toString());
        assertEquals(baseStr + "[length=<size=4>]",
                new ToStringBuilder(base).append("length", array, false).toString());
        assertEquals(baseStr + "[values=<null>]",
                new ToStringBuilder(base).append("values", (boolean[]) null, true).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(null, (boolean[]) null, false).toString());
        assertEquals(baseStr + "[<size=4>]", new ToStringBuilder(base).append(null, array, false).toString());
    }

    @Test
    public void testConstructToStringBuilder() {
        final ToStringBuilder stringBuilder1 = new ToStringBuilder(base, null, null);
        final ToStringBuilder stringBuilder2 = new ToStringBuilder(base, ToStringStyle.DEFAULT_STYLE, new StringBuffer(1024));
        assertEquals(ToStringStyle.DEFAULT_STYLE, stringBuilder1.getStyle());
        assertNotNull(stringBuilder1.getStringBuffer());
        assertNotNull(stringBuilder1.toString());
        assertEquals(ToStringStyle.DEFAULT_STYLE, stringBuilder2.getStyle());
        assertNotNull(stringBuilder2.getStringBuffer());
        assertNotNull(stringBuilder2.toString());
    }


    @Test
    public void testObject() {
        final Integer i3 = Integer.valueOf(3);
        final Integer i4 = Integer.valueOf(4);
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) null).toString());
        assertEquals(baseStr + "[3]", new ToStringBuilder(base).append(i3).toString());
        assertEquals(baseStr + "[a=<null>]", new ToStringBuilder(base).append("a", (Object) null).toString());
        assertEquals(baseStr + "[a=3]", new ToStringBuilder(base).append("a", i3).toString());
        assertEquals(baseStr + "[a=3,b=4]", new ToStringBuilder(base).append("a", i3).append("b", i4).toString());
        assertEquals(baseStr + "[a=<Integer>]", new ToStringBuilder(base).append("a", i3, false).toString());
        assertEquals(baseStr + "[a=<size=0>]", new ToStringBuilder(base).append("a", new ArrayList<>(), false).toString());
        assertEquals(baseStr + "[a=[]]", new ToStringBuilder(base).append("a", new ArrayList<>(), true).toString());
        assertEquals(baseStr + "[a=<size=0>]", new ToStringBuilder(base).append("a", new HashMap<>(), false).toString());
        assertEquals(baseStr + "[a={}]", new ToStringBuilder(base).append("a", new HashMap<>(), true).toString());
        assertEquals(baseStr + "[a=<size=0>]", new ToStringBuilder(base).append("a", (Object) new String[0], false).toString());
        assertEquals(baseStr + "[a={}]", new ToStringBuilder(base).append("a", (Object) new String[0], true).toString());
    }

    @Test
    public void testObjectBuild() {
        final Integer i3 = Integer.valueOf(3);
        final Integer i4 = Integer.valueOf(4);
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) null).build());
        assertEquals(baseStr + "[3]", new ToStringBuilder(base).append(i3).build());
        assertEquals(baseStr + "[a=<null>]", new ToStringBuilder(base).append("a", (Object) null).build());
        assertEquals(baseStr + "[a=3]", new ToStringBuilder(base).append("a", i3).build());
        assertEquals(baseStr + "[a=3,b=4]", new ToStringBuilder(base).append("a", i3).append("b", i4).build());
        assertEquals(baseStr + "[a=<Integer>]", new ToStringBuilder(base).append("a", i3, false).build());
        assertEquals(baseStr + "[a=<size=0>]", new ToStringBuilder(base).append("a", new ArrayList<>(), false).build());
        assertEquals(baseStr + "[a=[]]", new ToStringBuilder(base).append("a", new ArrayList<>(), true).build());
        assertEquals(baseStr + "[a=<size=0>]", new ToStringBuilder(base).append("a", new HashMap<>(), false).build());
        assertEquals(baseStr + "[a={}]", new ToStringBuilder(base).append("a", new HashMap<>(), true).build());
        assertEquals(baseStr + "[a=<size=0>]", new ToStringBuilder(base).append("a", (Object) new String[0], false).build());
        assertEquals(baseStr + "[a={}]", new ToStringBuilder(base).append("a", (Object) new String[0], true).build());
    }

    @Test
    public void testLong() {
        assertEquals(baseStr + "[3]", new ToStringBuilder(base).append(3L).toString());
        assertEquals(baseStr + "[a=3]", new ToStringBuilder(base).append("a", 3L).toString());
        assertEquals(baseStr + "[a=3,b=4]", new ToStringBuilder(base).append("a", 3L).append("b", 4L).toString());
    }

    @Test
    public void testInt() {
        assertEquals(baseStr + "[3]", new ToStringBuilder(base).append(3).toString());
        assertEquals(baseStr + "[a=3]", new ToStringBuilder(base).append("a", 3).toString());
        assertEquals(baseStr + "[a=3,b=4]", new ToStringBuilder(base).append("a", 3).append("b", 4).toString());
    }

    @Test
    public void testShort() {
        assertEquals(baseStr + "[3]", new ToStringBuilder(base).append((short) 3).toString());
        assertEquals(baseStr + "[a=3]", new ToStringBuilder(base).append("a", (short) 3).toString());
        assertEquals(baseStr + "[a=3,b=4]", new ToStringBuilder(base).append("a", (short) 3).append("b", (short) 4).toString());
    }

    @Test
    public void testChar() {
        assertEquals(baseStr + "[A]", new ToStringBuilder(base).append((char) 65).toString());
        assertEquals(baseStr + "[a=A]", new ToStringBuilder(base).append("a", (char) 65).toString());
        assertEquals(baseStr + "[a=A,b=B]", new ToStringBuilder(base).append("a", (char) 65).append("b", (char) 66).toString());
    }

    @Test
    public void testByte() {
        assertEquals(baseStr + "[3]", new ToStringBuilder(base).append((byte) 3).toString());
        assertEquals(baseStr + "[a=3]", new ToStringBuilder(base).append("a", (byte) 3).toString());
        assertEquals(baseStr + "[a=3,b=4]", new ToStringBuilder(base).append("a", (byte) 3).append("b", (byte) 4).toString());
    }

    @Test
    public void testDouble() {
        assertEquals(baseStr + "[3.2]", new ToStringBuilder(base).append(3.2).toString());
        assertEquals(baseStr + "[a=3.2]", new ToStringBuilder(base).append("a", 3.2).toString());
        assertEquals(baseStr + "[a=3.2,b=4.3]", new ToStringBuilder(base).append("a", 3.2).append("b", 4.3).toString());
    }

    @Test
    public void testFloat() {
        assertEquals(baseStr + "[3.2]", new ToStringBuilder(base).append((float) 3.2).toString());
        assertEquals(baseStr + "[a=3.2]", new ToStringBuilder(base).append("a", (float) 3.2).toString());
        assertEquals(baseStr + "[a=3.2,b=4.3]", new ToStringBuilder(base).append("a", (float) 3.2).append("b", (float) 4.3).toString());
    }

    @Test
    public void testBoolean() {
        assertEquals(baseStr + "[true]", new ToStringBuilder(base).append(true).toString());
        assertEquals(baseStr + "[a=true]", new ToStringBuilder(base).append("a", true).toString());
        assertEquals(baseStr + "[a=true,b=false]", new ToStringBuilder(base).append("a", true).append("b", false).toString());
    }


    @Test
    public void testObjectArray() {
        Object[] array = {null, base, new int[] {3, 6}};
        assertEquals(baseStr + "[{<null>,5,{3,6}}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{<null>,5,{3,6}}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

    @Test
    public void testLongArray() {
        long[] array = {1, 2, -3, 4};
        assertEquals(baseStr + "[{1,2,-3,4}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{1,2,-3,4}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

    @Test
    public void testIntArray() {
        int[] array = {1, 2, -3, 4};
        assertEquals(baseStr + "[{1,2,-3,4}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{1,2,-3,4}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

    @Test
    public void testShortArray() {
        short[] array = {1, 2, -3, 4};
        assertEquals(baseStr + "[{1,2,-3,4}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{1,2,-3,4}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

    @Test
    public void testByteArray() {
        byte[] array = {1, 2, -3, 4};
        assertEquals(baseStr + "[{1,2,-3,4}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{1,2,-3,4}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

    @Test
    public void testCharArray() {
        char[] array = {'A', '2', '_', 'D'};
        assertEquals(baseStr + "[{A,2,_,D}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{A,2,_,D}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

    @Test
    public void testDoubleArray() {
        double[] array = {1.0, 2.9876, -3.00001, 4.3};
        assertEquals(baseStr + "[{1.0,2.9876,-3.00001,4.3}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{1.0,2.9876,-3.00001,4.3}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

    @Test
    public void testFloatArray() {
        float[] array = {1.0f, 2.9876f, -3.00001f, 4.3f};
        assertEquals(baseStr + "[{1.0,2.9876,-3.00001,4.3}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{1.0,2.9876,-3.00001,4.3}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

    @Test
    public void testBooleanArray() {
        boolean[] array = {true, false, false};
        assertEquals(baseStr + "[{true,false,false}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{true,false,false}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

    @Test
    public void testLongArrayArray() {
        long[][] array = {{1, 2}, null, {5}};
        assertEquals(baseStr + "[{{1,2},<null>,{5}}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{{1,2},<null>,{5}}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

    @Test
    public void testIntArrayArray() {
        int[][] array = {{1, 2}, null, {5}};
        assertEquals(baseStr + "[{{1,2},<null>,{5}}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{{1,2},<null>,{5}}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

    @Test
    public void testShortArrayArray() {
        short[][] array = {{1, 2}, null, {5}};
        assertEquals(baseStr + "[{{1,2},<null>,{5}}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{{1,2},<null>,{5}}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

    @Test
    public void testByteArrayArray() {
        byte[][] array = {{1, 2}, null, {5}};
        assertEquals(baseStr + "[{{1,2},<null>,{5}}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{{1,2},<null>,{5}}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

    @Test
    public void testCharArrayArray() {
        char[][] array = {{'A', 'B'}, null, {'p'}};
        assertEquals(baseStr + "[{{A,B},<null>,{p}}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{{A,B},<null>,{p}}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

    @Test
    public void testDoubleArrayArray() {
        double[][] array = {{1.0, 2.29686}, null, {Double.NaN}};
        assertEquals(baseStr + "[{{1.0,2.29686},<null>,{NaN}}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{{1.0,2.29686},<null>,{NaN}}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

    @Test
    public void testFloatArrayArray() {
        float[][] array = {{1.0f, 2.29686f}, null, {Float.NaN}};
        assertEquals(baseStr + "[{{1.0,2.29686},<null>,{NaN}}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{{1.0,2.29686},<null>,{NaN}}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

    @Test
    public void testBooleanArrayArray() {
        boolean[][] array = {{true, false}, null, {false}};
        assertEquals(baseStr + "[{{true,false},<null>,{false}}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{{true,false},<null>,{false}}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

    @Test
    public void testObjectCycle() {
        final ObjectCycle a = new ObjectCycle();
        final ObjectCycle b = new ObjectCycle();
        a.obj = b;
        b.obj = a;

        final String expected = toBaseString(a) + "[" + toBaseString(b) + "[" + toBaseString(a) + "]]";
        assertEquals(expected, a.toString());
    }

    static class ObjectCycle {
        Object obj;

        @Override
        public String toString() {
            return new ToStringBuilder(this).append(obj).toString();
        }
    }

    @Test
    public void testSimpleReflectionStatics() {
        final SimpleReflectionStaticFieldsFixture instance1 = new SimpleReflectionStaticFieldsFixture();
        assertEquals(
            this.toBaseString(instance1) + "[staticInt=12345,staticString=staticString]",
            ReflectionToStringBuilder.toString(instance1, null, false, true, SimpleReflectionStaticFieldsFixture.class));
        assertEquals(
            this.toBaseString(instance1) + "[staticInt=12345,staticString=staticString]",
            ReflectionToStringBuilder.toString(instance1, null, true, true, SimpleReflectionStaticFieldsFixture.class));
        assertEquals(
            this.toBaseString(instance1) + "[staticInt=12345,staticString=staticString]",
            this.toStringWithStatics(instance1, null, SimpleReflectionStaticFieldsFixture.class));
        assertEquals(
            this.toBaseString(instance1) + "[staticInt=12345,staticString=staticString]",
            this.toStringWithStatics(instance1, null, SimpleReflectionStaticFieldsFixture.class));
    }

    /**
     * Tests ReflectionToStringBuilder.toString() for statics.
     */
    @Test
    public void testReflectionStatics() {
        final ReflectionStaticFieldsFixture instance1 = new ReflectionStaticFieldsFixture();
        assertEquals(
            this.toBaseString(instance1) + "[instanceInt=67890,instanceString=instanceString,staticInt=12345,staticString=staticString]",
            ReflectionToStringBuilder.toString(instance1, null, false, true, ReflectionStaticFieldsFixture.class));
        assertEquals(
            this.toBaseString(instance1) + "[instanceInt=67890,instanceString=instanceString,staticInt=12345,staticString=staticString,staticTransientInt=54321,staticTransientString=staticTransientString,transientInt=98765,transientString=transientString]",
            ReflectionToStringBuilder.toString(instance1, null, true, true, ReflectionStaticFieldsFixture.class));
        assertEquals(
            this.toBaseString(instance1) + "[instanceInt=67890,instanceString=instanceString,staticInt=12345,staticString=staticString]",
            this.toStringWithStatics(instance1, null, ReflectionStaticFieldsFixture.class));
        assertEquals(
            this.toBaseString(instance1) + "[instanceInt=67890,instanceString=instanceString,staticInt=12345,staticString=staticString]",
            this.toStringWithStatics(instance1, null, ReflectionStaticFieldsFixture.class));
    }

    /**
     * Tests ReflectionToStringBuilder.toString() for statics.
     */
    @Test
    public void testInheritedReflectionStatics() {
        final InheritedReflectionStaticFieldsFixture instance1 = new InheritedReflectionStaticFieldsFixture();
        assertEquals(
            this.toBaseString(instance1) + "[staticInt2=67890,staticString2=staticString2]",
            ReflectionToStringBuilder.toString(instance1, null, false, true, InheritedReflectionStaticFieldsFixture.class));
        assertEquals(
            this.toBaseString(instance1) + "[staticInt2=67890,staticString2=staticString2,staticInt=12345,staticString=staticString]",
            ReflectionToStringBuilder.toString(instance1, null, false, true, SimpleReflectionStaticFieldsFixture.class));
        assertEquals(
            this.toBaseString(instance1) + "[staticInt2=67890,staticString2=staticString2,staticInt=12345,staticString=staticString]",
            this.toStringWithStatics(instance1, null, SimpleReflectionStaticFieldsFixture.class));
        assertEquals(
            this.toBaseString(instance1) + "[staticInt2=67890,staticString2=staticString2,staticInt=12345,staticString=staticString]",
            this.toStringWithStatics(instance1, null, SimpleReflectionStaticFieldsFixture.class));
    }

    /**
     * <p>This method uses reflection to build a suitable
     * {@code toString} value which includes static fields.</p>
     *
     * <p>It uses {@code AccessibleObject.setAccessible} to gain access to private
     * fields. This means that it will throw a security exception if run
     * under a security manager, if the permissions are not set up correctly.
     * It is also not as efficient as testing explicitly. </p>
     *
     * <p>Transient fields are not output.</p>
     *
     * <p>Superclass fields will be appended up to and including the specified superclass.
     * A null superclass is treated as {@code java.lang.Object}.</p>
     *
     * <p>If the style is {@code null}, the default
     * {@code ToStringStyle} is used.</p>
     *
     * @param <T> the type of the output object
     * @param object  the Object to be output
     * @param style  the style of the {@code toString} to create,
     *  may be {@code null}
     * @param reflectUpToClass  the superclass to reflect up to (inclusive),
     *  may be {@code null}
     * @return the String result
     * @throws IllegalArgumentException if the Object is {@code null}
     */
    public <T> String toStringWithStatics(final T object, final ToStringStyle style, final Class<? super T> reflectUpToClass) {
        return ReflectionToStringBuilder.toString(object, style, false, true, reflectUpToClass);
    }

    /**
     * Tests ReflectionToStringBuilder setUpToClass().
     */
    @Test
    public void test_setUpToClass_valid() {
        final Integer val = Integer.valueOf(5);
        final ReflectionToStringBuilder test = new ReflectionToStringBuilder(val);
        test.setUpToClass(Number.class);
        test.toString();
    }

    /**
     * Tests ReflectionToStringBuilder setUpToClass().
     */
    @Test
    public void test_setUpToClass_invalid() {
        final Integer val = Integer.valueOf(5);
        final ReflectionToStringBuilder test = new ReflectionToStringBuilder(val);
        assertThrows(IllegalArgumentException.class, () -> test.setUpToClass(String.class));
        test.toString();
    }

    /**
     * Tests ReflectionToStringBuilder.toString() for statics.
     */
    class ReflectionStaticFieldsFixture {
        static final String staticString = "staticString";
        static final int staticInt = 12345;
        static final transient String staticTransientString = "staticTransientString";
        static final transient int staticTransientInt = 54321;
        String instanceString = "instanceString";
        int instanceInt = 67890;
        transient String transientString = "transientString";
        transient int transientInt = 98765;
    }

    /**
     * Test fixture for ReflectionToStringBuilder.toString() for statics.
     */
    class SimpleReflectionStaticFieldsFixture {
        static final String staticString = "staticString";
        static final int staticInt = 12345;
    }

    /**
     * Test fixture for ReflectionToStringBuilder.toString() for statics.
     */
    @SuppressWarnings("unused")
    class InheritedReflectionStaticFieldsFixture extends SimpleReflectionStaticFieldsFixture {
        static final String staticString2 = "staticString2";
        static final int staticInt2 = 67890;
    }

    @Test
    public void testReflectionNull() {
        assertThrows(NullPointerException.class, () -> ReflectionToStringBuilder.toString(null));
    }

    /**
     * Points out failure to print anything from appendToString methods using MULTI_LINE_STYLE.
     * See issue LANG-372.
     */
    class MultiLineTestObject {
        Integer i = Integer.valueOf(31337);
        @Override
        public String toString() {
            return new ToStringBuilder(this).append("testInt", i).toString();
        }
    }

    @Test
    public void testAppendToStringUsingMultiLineStyle() {
        final MultiLineTestObject obj = new MultiLineTestObject();
        final ToStringBuilder testBuilder = new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE)
                                          .appendToString(obj.toString());
        assertEquals(-1, testBuilder.toString().indexOf("testInt=31337"));
    }

}
