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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link ReflectionDiffBuilder}.
 */
class ReflectionDiffBuilderTest extends AbstractLangTest {

    private static class AtomicIntegerWrapper {

        private /* not final might not matter for the test. */ AtomicInteger value;

        AtomicIntegerWrapper(final int a) {
            value = new AtomicInteger(a);
        }
    }

    private static class FloatWrapper {

        private /* not final might not matter for the test. */ float value;

        FloatWrapper(final float a) {
            value = a;
        }
    }

    private static class FloatWrapperEquals {

        private /* not final might not matter for the test. */ float value;

        FloatWrapperEquals(final float a) {
            value = a;
        }

        @Override
        public boolean equals(final Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null || getClass() != obj.getClass()) {
                return false;
            }
            final FloatWrapperEquals other = (FloatWrapperEquals) obj;
            return Float.floatToIntBits(value) == Float.floatToIntBits(other.value);
        }

        @Override
        public int hashCode() {
            return Objects.hash(value);
        }
    }

    private static class FloatWrapperWrapper {

        private /* not final might not matter for the test. */ FloatWrapper value;

        FloatWrapperWrapper(final float a) {
            value = new FloatWrapper(a);
        }
    }

    private static class FloatWrapperWrapperDeepEquals {

        private /* not final might not matter for the test. */ FloatWrapperEquals value;

        FloatWrapperWrapperDeepEquals(final float a) {
            value = new FloatWrapperEquals(a);
        }

        FloatWrapperWrapperDeepEquals(final FloatWrapperEquals a) {
            value = a;
        }

        @Override
        public boolean equals(final Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null || getClass() != obj.getClass()) {
                return false;
            }
            final FloatWrapperWrapperDeepEquals other = (FloatWrapperWrapperDeepEquals) obj;
            return Objects.equals(value, other.value);
        }

        @Override
        public int hashCode() {
            return Objects.hash(value);
        }

    }

    private static class FloatWrapperWrapperEquals {

        private /* not final might not matter for the test. */ FloatWrapper value;

        FloatWrapperWrapperEquals(final float a) {
            value = new FloatWrapper(a);
        }

        FloatWrapperWrapperEquals(final FloatWrapper a) {
            value = a;
        }

        @Override
        public boolean equals(final Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null || getClass() != obj.getClass()) {
                return false;
            }
            final FloatWrapperWrapperEquals other = (FloatWrapperWrapperEquals) obj;
            return Objects.equals(value, other.value);
        }

        @Override
        public int hashCode() {
            return Objects.hash(value);
        }

    }

    @SuppressWarnings("unused")
    private static final class TypeTestChildClass extends TypeTestClass {
        String field = "a";
    }

    @SuppressWarnings("unused")
    private static class TypeTestClass implements Diffable<TypeTestClass> {

        private static int staticField;
        private final ToStringStyle style = SHORT_STYLE;
        private final boolean booleanField = true;
        private final boolean[] booleanArrayField = { true };
        private final byte byteField = (byte) 0xFF;
        private final byte[] byteArrayField = { (byte) 0xFF };
        private char charField = 'a';
        private char[] charArrayField = { 'a' };
        private final double doubleField = 1.0;
        private final double[] doubleArrayField = { 1.0 };
        private final float floatField = 1.0f;
        private final float[] floatArrayField = { 1.0f };
        int intField = 1;
        private final int[] intArrayField = { 1 };
        private final long longField = 1L;
        private final long[] longArrayField = { 1L };
        private final short shortField = 1;
        private final short[] shortArrayField = { 1 };
        private final Object objectField = null;
        private final Object[] objectArrayField = { null };
        private transient String transientField;
        private BigDecimal bigDecimal = BigDecimal.valueOf(20, 1);
        private BigInteger bigInteger = BigInteger.valueOf(2);
        @DiffExclude
        private String annotatedField = "a";
        private String excludedField = "a";

        @Override
        public DiffResult<TypeTestClass> diff(final TypeTestClass obj) {
            // @formatter:off
            return ReflectionDiffBuilder.<TypeTestClass>builder()
                    .setDiffBuilder(diffBuilder(obj))
                    .setExcludeFieldNames("excludedField")
                    .build()
                    .build();
            // @formatter:on
        }

        DiffBuilder<TypeTestClass> diffBuilder(final TypeTestClass obj) {
            // @formatter:off
            return DiffBuilder.<TypeTestClass>builder()
                .setLeft(this)
                .setRight(obj)
                .setStyle(style)
                .build();
            // @formatter:on
        }

        public DiffResult<TypeTestClass> diffDeprecated(final TypeTestClass obj) {
            return new ReflectionDiffBuilder<>(this, obj, style).setExcludeFieldNames("excludedField").build();
        }

        @Override
        public boolean equals(final Object obj) {
            return EqualsBuilder.reflectionEquals(this, obj, false);
        }

        @Override
        public int hashCode() {
            return HashCodeBuilder.reflectionHashCode(this, false);
        }
    }

    private static final ToStringStyle SHORT_STYLE = ToStringStyle.SHORT_PREFIX_STYLE;

    @Test
    void testArrayDifference() {
        final TypeTestClass firstObject = new TypeTestClass();
        firstObject.charArrayField = new char[] { 'c' };
        final TypeTestClass secondObject = new TypeTestClass();
        // Normal builder
        DiffResult<TypeTestClass> list = firstObject.diff(secondObject);
        assertEquals(1, list.getNumberOfDiffs());
        // Deprecated constructor
        list = firstObject.diffDeprecated(secondObject);
        assertEquals(1, list.getNumberOfDiffs());
    }

    @Test
    void testBigDecimalDifference() {
        final TypeTestClass firstObject = new TypeTestClass();
        // 2.0 is not equal to 2.00, see BigDecimal#equals()
        firstObject.bigDecimal = BigDecimal.valueOf(200, 2);
        final TypeTestClass secondObject = new TypeTestClass();
        final DiffResult<TypeTestClass> list = firstObject.diff(secondObject);
        assertEquals(1, list.getNumberOfDiffs());
    }

    @Test
    void testBigIntegerDifference() {
        final TypeTestClass firstObject = new TypeTestClass();
        firstObject.bigInteger = BigInteger.valueOf(100);
        final TypeTestClass secondObject = new TypeTestClass();

        final DiffResult<TypeTestClass> list = firstObject.diff(secondObject);
        assertEquals(1, list.getNumberOfDiffs());
    }

    @Test
    void testDifferenceInInherited_field() {
        final TypeTestChildClass firstObject = new TypeTestChildClass();
        firstObject.intField = 99;
        final TypeTestChildClass secondObject = new TypeTestChildClass();

        final DiffResult<TypeTestClass> list = firstObject.diff(secondObject);
        assertEquals(1, list.getNumberOfDiffs());
    }

    /*
     * See https://issues.apache.org/jira/browse/LANG-1708
     */
    @Test
    void testGetDiffAtomicInteger() {
        final AtomicInteger a = new AtomicInteger(1);
        final AtomicInteger b = new AtomicInteger(1);
        assertEquals(0, new ReflectionDiffBuilder<>(a, b, ToStringStyle.JSON_STYLE).build().getDiffs().size());
        assertEquals(0, new ReflectionDiffBuilder<>(a, a, ToStringStyle.JSON_STYLE).build().getDiffs().size());
        assertEquals(1,
                ((List<Diff<?>>) new ReflectionDiffBuilder<>(new AtomicInteger(1), new AtomicInteger(2), ToStringStyle.JSON_STYLE).build().getDiffs()).size());
    }

    /*
     * See https://issues.apache.org/jira/browse/LANG-1708
     */
    @Test
    void testGetDiffAtomicIntegerWrapper() {
        final AtomicIntegerWrapper a = new AtomicIntegerWrapper(1);
        final AtomicIntegerWrapper b = new AtomicIntegerWrapper(1);
        final List<Diff<?>> diffList = new ReflectionDiffBuilder<>(a, b, ToStringStyle.JSON_STYLE).build().getDiffs();
        assertEquals(1, diffList.size());
        final Diff<?> diff = diffList.get(0);
        assertFalse(diffList.isEmpty(), diff.toString());
        assertSame(a.value, diff.getKey());
        assertSame(b.value, diff.getValue());
        assertEquals(0, ((List<Diff<?>>) new ReflectionDiffBuilder<>(a, a, ToStringStyle.JSON_STYLE).build().getDiffs()).size());
    }

    /*
     * See https://issues.apache.org/jira/browse/LANG-1708
     */
    @Test
    void testGetDiffFloatWrapper() {
        final FloatWrapper a = new FloatWrapper(1f);
        final FloatWrapper b = new FloatWrapper(1f);
        assertEquals(0, ((List<Diff<?>>) new ReflectionDiffBuilder<>(a, b, ToStringStyle.JSON_STYLE).build().getDiffs()).size());
        assertEquals(0, ((List<Diff<?>>) new ReflectionDiffBuilder<>(a, a, ToStringStyle.JSON_STYLE).build().getDiffs()).size());
        assertEquals(1,
                ((List<Diff<?>>) new ReflectionDiffBuilder<>(new FloatWrapper(1f), new FloatWrapper(2f), ToStringStyle.JSON_STYLE).build().getDiffs()).size());
    }

    /*
     * See https://issues.apache.org/jira/browse/LANG-1708
     */
    @Test
    void testGetDiffFloatWrapperDeepEquals() {
        final FloatWrapperWrapperDeepEquals a = new FloatWrapperWrapperDeepEquals(1f);
        final FloatWrapperWrapperDeepEquals b = new FloatWrapperWrapperDeepEquals(1f);
        assertEquals(0, ((List<Diff<?>>) new ReflectionDiffBuilder<>(a, b, ToStringStyle.JSON_STYLE).build().getDiffs()).size());
        assertEquals(0, ((List<Diff<?>>) new ReflectionDiffBuilder<>(a, a, ToStringStyle.JSON_STYLE).build().getDiffs()).size());
        assertEquals(1, ((List<Diff<?>>) new ReflectionDiffBuilder<>(new FloatWrapperWrapperDeepEquals(1f), new FloatWrapperWrapperDeepEquals(2f),
                ToStringStyle.JSON_STYLE).build().getDiffs()).size());
        final FloatWrapperEquals fw1 = new FloatWrapperEquals(1f);
        assertEquals(0, ((List<Diff<?>>) new ReflectionDiffBuilder<>(new FloatWrapperWrapperDeepEquals(fw1), new FloatWrapperWrapperDeepEquals(fw1),
                ToStringStyle.JSON_STYLE).build().getDiffs()).size());
        final FloatWrapperEquals fw2 = new FloatWrapperEquals(2f);
        assertEquals(1, ((List<Diff<?>>) new ReflectionDiffBuilder<>(new FloatWrapperWrapperDeepEquals(fw1), new FloatWrapperWrapperDeepEquals(fw2),
                ToStringStyle.JSON_STYLE).build().getDiffs()).size());
    }

    /*
     * See https://issues.apache.org/jira/browse/LANG-1708
     */
    @Test
    void testGetDiffFloatWrapperEquals() {
        final FloatWrapperEquals a = new FloatWrapperEquals(1f);
        final FloatWrapperEquals b = new FloatWrapperEquals(1f);
        assertEquals(0, ((List<Diff<?>>) new ReflectionDiffBuilder<>(a, b, ToStringStyle.JSON_STYLE).build().getDiffs()).size());
        assertEquals(0, ((List<Diff<?>>) new ReflectionDiffBuilder<>(a, a, ToStringStyle.JSON_STYLE).build().getDiffs()).size());
        assertEquals(1,
                ((List<Diff<?>>) new ReflectionDiffBuilder<>(new FloatWrapperEquals(1f), new FloatWrapperEquals(2f), ToStringStyle.JSON_STYLE).build().getDiffs())
                        .size());
    }

    /*
     * See https://issues.apache.org/jira/browse/LANG-1708
     */
    @Test
    void testGetDiffFloatWrapperWrapper() {
        final FloatWrapperWrapper a = new FloatWrapperWrapper(1f);
        final FloatWrapperWrapper b = new FloatWrapperWrapper(1f);
        final List<Diff<?>> diffList = new ReflectionDiffBuilder<>(a, b, ToStringStyle.JSON_STYLE).build().getDiffs();
        assertEquals(1, diffList.size());
        final Diff<?> diff = diffList.get(0);
        assertFalse(diffList.isEmpty(), diff.toString());
        assertSame(a.value, diff.getKey());
        assertSame(b.value, diff.getValue());
        assertEquals(0, ((List<Diff<?>>) new ReflectionDiffBuilder<>(a, a, ToStringStyle.JSON_STYLE).build().getDiffs()).size());
        assertEquals(1,
                ((List<Diff<?>>) new ReflectionDiffBuilder<>(new FloatWrapperWrapper(1f), new FloatWrapperWrapper(2f), ToStringStyle.JSON_STYLE)
                        .build().getDiffs()).size());
    }

    /*
     * See https://issues.apache.org/jira/browse/LANG-1708
     */
    @Test
    void testGetDiffFloatWrapperWrapperEquals() {
        final FloatWrapperWrapperEquals a = new FloatWrapperWrapperEquals(1f);
        final FloatWrapperWrapperEquals b = new FloatWrapperWrapperEquals(1f);
        final List<Diff<?>> diffList = new ReflectionDiffBuilder<>(a, b, ToStringStyle.JSON_STYLE).build().getDiffs();
        assertEquals(1, diffList.size());
        final Diff<?> diff = diffList.get(0);
        assertFalse(diffList.isEmpty(), diff.toString());
        assertSame(a.value, diff.getKey());
        assertSame(b.value, diff.getValue());
        assertEquals(0, ((List<Diff<?>>) new ReflectionDiffBuilder<>(a, a, ToStringStyle.JSON_STYLE).build().getDiffs()).size());
        assertEquals(1,
                ((List<Diff<?>>) new ReflectionDiffBuilder<>(new FloatWrapperWrapperEquals(1f), new FloatWrapperWrapperEquals(2f), ToStringStyle.JSON_STYLE)
                        .build().getDiffs()).size());
        final FloatWrapper fw1 = new FloatWrapper(1f);
        assertEquals(0,
                ((List<Diff<?>>) new ReflectionDiffBuilder<>(new FloatWrapperWrapperEquals(fw1), new FloatWrapperWrapperEquals(fw1), ToStringStyle.JSON_STYLE)
                        .build().getDiffs()).size());
    }

    @Test
    void testGetExcludeFieldNamesEmpty() {
        final ReflectionDiffBuilder<?> reflectionDiffBuilder = new ReflectionDiffBuilder<>(new TypeTestClass(), new TypeTestChildClass(), SHORT_STYLE);
        final String[] excludeFieldNames = reflectionDiffBuilder.getExcludeFieldNames();
        assertNotNull(excludeFieldNames);
        assertEquals(0, excludeFieldNames.length);
    }

    @Test
    void testGetExcludeFieldNamesWithNullExcludedFieldNames() {
        // @formatter:off
        final ReflectionDiffBuilder<TypeTestClass> reflectionDiffBuilder = ReflectionDiffBuilder.<TypeTestClass>builder()
                .setDiffBuilder(DiffBuilder.<TypeTestClass>builder()
                        .setLeft(new TypeTestClass())
                        .setRight(new TypeTestChildClass())
                        .setStyle(SHORT_STYLE)
                        .build())
                .build();
        // @formatter:on
        final String[] excludeFieldNames = reflectionDiffBuilder.getExcludeFieldNames();
        assertNotNull(excludeFieldNames);
        assertEquals(0, excludeFieldNames.length);
        assertNotNull(reflectionDiffBuilder.build());
    }

    @Test
    void testGetExcludeFieldNamesWithNullExcludedFieldNamesCtor() {
        // @formatter:off
        final ReflectionDiffBuilder<TypeTestClass> reflectionDiffBuilder =
                new ReflectionDiffBuilder<>(new TypeTestClass(), new TypeTestChildClass(), SHORT_STYLE);
        // @formatter:on
        reflectionDiffBuilder.setExcludeFieldNames((String[]) null);
        final String[] excludeFieldNames = reflectionDiffBuilder.getExcludeFieldNames();
        assertNotNull(excludeFieldNames);
        assertEquals(0, excludeFieldNames.length);
        assertNotNull(reflectionDiffBuilder.build());
    }

    @Test
    void testGetExcludeFieldNamesWithNullValuesInExcludedFieldNames() {
        // @formatter:off
        final ReflectionDiffBuilder<TypeTestClass> reflectionDiffBuilder = ReflectionDiffBuilder.<TypeTestClass>builder()
                .setDiffBuilder(DiffBuilder.<TypeTestClass>builder()
                        .setLeft(new TypeTestClass())
                        .setRight(new TypeTestChildClass())
                        .setStyle(SHORT_STYLE)
                        .build())
                .setExcludeFieldNames("charField", null)
                .build();
        // @formatter:on
        final String[] excludeFieldNames = reflectionDiffBuilder.getExcludeFieldNames();
        assertNotNull(excludeFieldNames);
        assertEquals(1, excludeFieldNames.length);
        assertEquals("charField", excludeFieldNames[0]);
        assertNotNull(reflectionDiffBuilder.build());
    }

    @Test
    void testGetExcludeFieldNamesWithNullValuesInExcludedFieldNamesCtor() {
        // @formatter:off
        final ReflectionDiffBuilder<TypeTestClass> reflectionDiffBuilder =
                new ReflectionDiffBuilder<>(new TypeTestClass(), new TypeTestChildClass(), SHORT_STYLE);
        // @formatter:on
        reflectionDiffBuilder.setExcludeFieldNames("charField", null);
        final String[] excludeFieldNames = reflectionDiffBuilder.getExcludeFieldNames();
        assertNotNull(excludeFieldNames);
        assertEquals(1, excludeFieldNames.length);
        assertEquals("charField", excludeFieldNames[0]);
        assertNotNull(reflectionDiffBuilder.build());
    }

    @Test
    void testNoDiffBuilderSet() {
        assertThrows(NullPointerException.class, () -> ReflectionDiffBuilder.<TypeTestClass>builder().build());
    }

    @Test
    void testNoDifferences() {
        final TypeTestClass firstObject = new TypeTestClass();
        final TypeTestClass secondObject = new TypeTestClass();
        assertEquals(0, firstObject.diff(secondObject).getNumberOfDiffs());
        assertEquals(0, firstObject.diffDeprecated(secondObject).getNumberOfDiffs());
    }

    @Test
    void testNoDifferencesDiffExcludeAnnotatedField() {
        final TypeTestClass firstObject = new TypeTestClass();
        firstObject.annotatedField = "b";
        final TypeTestClass secondObject = new TypeTestClass();
        assertEquals(0, firstObject.diff(secondObject).getNumberOfDiffs());
        assertEquals(0, firstObject.diffDeprecated(secondObject).getNumberOfDiffs());
    }

    @Test
    void testNoDifferencesDiffExcludedFieldAndExcludeAnnotatedField() {
        final TypeTestClass firstObject = new TypeTestClass();
        firstObject.excludedField = "b";
        firstObject.annotatedField = "b";
        final TypeTestClass secondObject = new TypeTestClass();
        DiffResult<TypeTestClass> list = firstObject.diff(secondObject);
        assertEquals(0, list.getNumberOfDiffs());
        list = firstObject.diffDeprecated(secondObject);
        assertEquals(0, list.getNumberOfDiffs());
    }

    @Test
    void testNoDifferencesExcludedField() {
        final TypeTestClass firstObject = new TypeTestClass();
        firstObject.excludedField = "b";
        final TypeTestClass secondObject = new TypeTestClass();
        DiffResult<TypeTestClass> list = firstObject.diff(secondObject);
        assertEquals(0, list.getNumberOfDiffs());
        list = firstObject.diffDeprecated(secondObject);
        assertEquals(0, list.getNumberOfDiffs());
    }

    @Test
    void testNoDifferencesInheritance() {
        final TypeTestChildClass firstObject = new TypeTestChildClass();
        final TypeTestChildClass secondObject = new TypeTestChildClass();
        DiffResult<TypeTestClass> list = firstObject.diff(secondObject);
        assertEquals(0, list.getNumberOfDiffs());
        list = firstObject.diffDeprecated(secondObject);
        assertEquals(0, list.getNumberOfDiffs());
    }

    @Test
    void testPrimitiveDifference() {
        final TypeTestClass firstObject = new TypeTestClass();
        firstObject.charField = 'c';
        final TypeTestClass secondObject = new TypeTestClass();
        DiffResult<TypeTestClass> list = firstObject.diff(secondObject);
        assertEquals(1, list.getNumberOfDiffs());
        list = firstObject.diffDeprecated(secondObject);
        assertEquals(1, list.getNumberOfDiffs());
    }

    @Test
    void testRetention() throws Exception {
        // The following should not retain memory.
        for (int i = 0; i < Integer.getInteger("testRecursive", 10_000); i++) {
            final Class<?> clazz = TestClassBuilder.defineSimpleClass(getClass().getPackage().getName(), i);
            final Object firstObject = clazz.newInstance();
            final Object secondObject = clazz.newInstance();
            final ReflectionDiffBuilder<Object> reflectionDiffBuilder = new ReflectionDiffBuilder<>(firstObject, secondObject, SHORT_STYLE);
            assertNotNull(reflectionDiffBuilder.build());
        }
    }

    @Test
    void testTransientFieldDifference() {
        final TypeTestClass firstObject = new TypeTestClass();
        firstObject.transientField = "a";
        final TypeTestClass secondObject = new TypeTestClass();
        secondObject.transientField = "b";
        DiffResult<TypeTestClass> list = firstObject.diff(secondObject);
        assertEquals(0, list.getNumberOfDiffs());
        list = firstObject.diffDeprecated(secondObject);
        assertEquals(0, list.getNumberOfDiffs());
    }

}
