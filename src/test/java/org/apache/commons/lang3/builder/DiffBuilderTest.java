/**
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang3.builder;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

import org.apache.commons.lang3.ArrayUtils;
import org.junit.Test;


/**
 * Unit tests {@link DiffBuilder}.
 *
 * @version $Id$
 */
public class DiffBuilderTest {
    
    private static final ToStringStyle SHORT_STYLE = ToStringStyle.SHORT_PREFIX_STYLE;

    private static class TypeTestClass implements Diffable<TypeTestClass> {
        private ToStringStyle style = SHORT_STYLE;
        private boolean booleanField = true;
        private boolean[] booleanArrayField = {true};
        private byte byteField = (byte) 0xFF;
        private byte[] byteArrayField = {(byte) 0xFF};
        private char charField = 'a';
        private char[] charArrayField = {'a'};
        private double doubleField = 1.0;
        private double[] doubleArrayField = {1.0};
        private float floatField = 1.0f;
        private float[] floatArrayField = {1.0f};
        private int intField = 1;
        private int[] intArrayField = {1};
        private long longField = 1L;
        private long[] longArrayField = {1L};
        private short shortField = 1;
        private short[] shortArrayField = {1};
        private Object objectField = null;
        private Object[] objectArrayField = {null};

        @Override
        public DiffResult diff(final TypeTestClass obj) {
            return new DiffBuilder(this, obj, style)
                .append("boolean", booleanField, obj.booleanField)
                .append("booleanArray", booleanArrayField, obj.booleanArrayField)
                .append("byte", byteField, obj.byteField)
                .append("byteArray", byteArrayField, obj.byteArrayField)
                .append("char", charField, obj.charField)
                .append("charArray", charArrayField, obj.charArrayField)
                .append("double", doubleField, obj.doubleField)
                .append("doubleArray", doubleArrayField, obj.doubleArrayField)
                .append("float", floatField, obj.floatField)
                .append("floatArray", floatArrayField, obj.floatArrayField)
                .append("int", intField, obj.intField)
                .append("intArray", intArrayField, obj.intArrayField)
                .append("long", longField, obj.longField)
                .append("longArray", longArrayField, obj.longArrayField)
                .append("short", shortField, obj.shortField)
                .append("shortArray", shortArrayField, obj.shortArrayField)
                .append("objectField", objectField, obj.objectField)
                .append("objectArrayField", objectArrayField, obj.objectArrayField)
                .build();
        }        

        @Override
        public int hashCode() {
            return HashCodeBuilder.reflectionHashCode(this, false);
        }
        
        @Override
        public boolean equals(final Object obj) {
            return EqualsBuilder.reflectionEquals(this, obj, false);
        }
    }
    
    
    @Test
    public void testBoolean() {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.booleanField = false;
        final DiffResult list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertEquals(Boolean.class, diff.getType());
        assertEquals(Boolean.TRUE, diff.getLeft());
        assertEquals(Boolean.FALSE, diff.getRight());        
    }

    @Test
    public void testBooleanArray() throws Exception {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.booleanArrayField = new boolean[] {false, false};
        final DiffResult list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertArrayEquals(ArrayUtils.toObject(class1.booleanArrayField), 
                (Object[]) diff.getLeft());
        assertArrayEquals(ArrayUtils.toObject(class2.booleanArrayField), 
                (Object[]) diff.getRight());
    }

    
    @Test
    public void testByte() {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.byteField = 0x01;
        final DiffResult list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertEquals(Byte.valueOf(class1.byteField), diff.getLeft());
        assertEquals(Byte.valueOf(class2.byteField), diff.getRight());        
    }
    
    @Test
    public void testByteArray() throws Exception {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.byteArrayField= new byte[] {0x01, 0x02};
        final DiffResult list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertArrayEquals(ArrayUtils.toObject(class1.byteArrayField), 
                (Object[]) diff.getLeft());
        assertArrayEquals(ArrayUtils.toObject(class2.byteArrayField), 
                (Object[]) diff.getRight());
    }

    @Test
    public void testChar() {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.charField = 'z';
        final DiffResult list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertEquals(Character.valueOf(class1.charField), diff.getLeft());
        assertEquals(Character.valueOf(class2.charField), diff.getRight());
    }
    
    
    @Test
    public void testCharArray() throws Exception {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.charArrayField = new char[] {'f', 'o', 'o'};
        final DiffResult list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertArrayEquals(ArrayUtils.toObject(class1.charArrayField), 
                (Object[]) diff.getLeft());
        assertArrayEquals(ArrayUtils.toObject(class2.charArrayField), 
                (Object[]) diff.getRight());
    }
    
    
    @Test
    public void testDouble() {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.doubleField = 99.99;
        final DiffResult list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertEquals(Double.valueOf(class1.doubleField), diff.getLeft());
        assertEquals(Double.valueOf(class2.doubleField), diff.getRight());
    }    

    
    @Test
    public void testDoubleArray() throws Exception {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.doubleArrayField = new double[] {3.0, 2.9, 2.8};
        final DiffResult list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertArrayEquals(ArrayUtils.toObject(class1.doubleArrayField), 
                (Object[]) diff.getLeft());
        assertArrayEquals(ArrayUtils.toObject(class2.doubleArrayField), 
                (Object[]) diff.getRight());
    }
    
    @Test
    public void testFloat() {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.floatField = 99.99F;
        final DiffResult list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertEquals(Float.valueOf(class1.floatField), diff.getLeft());
        assertEquals(Float.valueOf(class2.floatField), diff.getRight());
    }    

    
    @Test
    public void testFloatArray() throws Exception {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.floatArrayField = new float[] {3.0F, 2.9F, 2.8F};
        final DiffResult list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertArrayEquals(ArrayUtils.toObject(class1.floatArrayField), 
                (Object[]) diff.getLeft());
        assertArrayEquals(ArrayUtils.toObject(class2.floatArrayField), 
                (Object[]) diff.getRight());
    }    
    
    
    @Test
    public void testInt() {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.intField = 42;
        final DiffResult list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertEquals(Integer.valueOf(class1.intField), diff.getLeft());
        assertEquals(Integer.valueOf(class2.intField), diff.getRight());
    }    

    
    @Test
    public void testIntArray() throws Exception {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.intArrayField = new int[] {3, 2, 1};
        final DiffResult list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertArrayEquals(ArrayUtils.toObject(class1.intArrayField), 
                (Object[]) diff.getLeft());
        assertArrayEquals(ArrayUtils.toObject(class2.intArrayField), 
                (Object[]) diff.getRight());
    }
    
    @Test
    public void testLong() {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.longField = 42L;
        final DiffResult list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertEquals(Long.valueOf(class1.longField), diff.getLeft());
        assertEquals(Long.valueOf(class2.longField), diff.getRight());
    }    

    
    @Test
    public void testLongArray() throws Exception {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.longArrayField = new long[] {3L, 2L, 1L};
        final DiffResult list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertArrayEquals(ArrayUtils.toObject(class1.longArrayField), 
                (Object[]) diff.getLeft());
        assertArrayEquals(ArrayUtils.toObject(class2.longArrayField), 
                (Object[]) diff.getRight());
    }
    
    @Test
    public void testShort() {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.shortField = 42;
        final DiffResult list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertEquals(Short.valueOf(class1.shortField), diff.getLeft());
        assertEquals(Short.valueOf(class2.shortField), diff.getRight());
    }    

    
    @Test
    public void testShortArray() throws Exception {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.shortArrayField = new short[] {3, 2, 1};
        final DiffResult list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertArrayEquals(ArrayUtils.toObject(class1.shortArrayField), 
                (Object[]) diff.getLeft());
        assertArrayEquals(ArrayUtils.toObject(class2.shortArrayField), 
                (Object[]) diff.getRight());
    }
    
    @Test
    public void testObject() throws Exception {        
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.objectField = "Some string";
        final DiffResult list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertEquals(class1.objectField, diff.getLeft());
        assertEquals(class2.objectField, diff.getRight());                
    }
    
    @Test
    public void testObjectsEqual() throws Exception {        
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class1.objectField = "Some string";
        class2.objectField = "Some string";
        final DiffResult list = class1.diff(class2);
        assertEquals(0, list.getNumberOfDiffs());
    }
    
    
    @Test
    public void testObjectArray() throws Exception {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class2.objectArrayField = new Object[] {"string", 1, 2};
        final DiffResult list = class1.diff(class2);
        assertEquals(1, list.getNumberOfDiffs());
        final Diff<?> diff = list.getDiffs().get(0);
        assertArrayEquals(class1.objectArrayField, (Object[]) diff.getLeft());
        assertArrayEquals(class2.objectArrayField, (Object[]) diff.getRight());
    }   
    
    @Test
    public void testObjectArrayEqual() throws Exception {
        final TypeTestClass class1 = new TypeTestClass();
        final TypeTestClass class2 = new TypeTestClass();
        class1.objectArrayField = new Object[] {"string", 1, 2};
        class2.objectArrayField = new Object[] {"string", 1, 2};
        final DiffResult list = class1.diff(class2);
        assertEquals(0, list.getNumberOfDiffs());
    }  
    
    
    @Test
    public void testByteArrayEqualAsObject() throws Exception {
        final DiffResult list = new DiffBuilder("String1", "String2", SHORT_STYLE)
            .append("foo", (Object) new boolean[] {false}, (Object) new boolean[] {false})
            .append("foo", (Object) new byte[] {0x01}, (Object) new byte[] {0x01})
            .append("foo", (Object) new char[] {'a'}, (Object) new char[] {'a'})
            .append("foo", (Object) new double[] {1.0}, (Object) new double[] {1.0})
            .append("foo", (Object) new float[] {1.0F}, (Object) new float[] {1.0F})
            .append("foo", (Object) new int[] {1}, (Object) new int[] {1})
            .append("foo", (Object) new long[] {1L}, (Object) new long[] {1L})
            .append("foo", (Object) new short[] {1}, (Object) new short[] {1})
            .append("foo", (Object) new Object[] {1, "two"}, (Object) new Object[] {1, "two"})
            .build();

        assertEquals(0, list.getNumberOfDiffs());
    }
    

    @Test(expected=IllegalArgumentException.class)
    public void testNullLhs() {      
        new DiffBuilder(null, this, ToStringStyle.DEFAULT_STYLE);
    }
    

    @Test(expected=IllegalArgumentException.class)
    public void testNullRhs() {      
        new DiffBuilder(this, null, ToStringStyle.DEFAULT_STYLE);
    }   
    
    @Test
    public void testSameObjectIgnoresAppends() {
        final TypeTestClass testClass = new TypeTestClass();        
        final DiffResult list = new DiffBuilder(testClass, testClass, SHORT_STYLE)
            .append("ignored", false, true)
            .build();
        assertEquals(0, list.getNumberOfDiffs());
    }
    
    @Test
    public void testSimilarObjectIgnoresAppends() {
        final TypeTestClass testClass1 = new TypeTestClass();
        final TypeTestClass testClass2 = new TypeTestClass();
        final DiffResult list = new DiffBuilder(testClass1, testClass2, SHORT_STYLE)
            .append("ignored", false, true)
            .build();
        assertEquals(0, list.getNumberOfDiffs());
    }
    
    
    @Test
    public void testStylePassedToDiffResult() {
        final TypeTestClass class1 = new TypeTestClass();
        DiffResult list = class1.diff(class1);
        assertEquals(SHORT_STYLE, list.getToStringStyle());
        
        class1.style = ToStringStyle.MULTI_LINE_STYLE;
        list = class1.diff(class1);
        assertEquals(ToStringStyle.MULTI_LINE_STYLE, list.getToStringStyle());
    }
}
