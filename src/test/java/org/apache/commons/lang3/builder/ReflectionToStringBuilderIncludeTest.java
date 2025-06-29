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
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.ArrayUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class ReflectionToStringBuilderIncludeTest extends AbstractLangTest {

    final class TestFeature {
        @SuppressWarnings("unused")
        private final String field1 = VALUES[0];

        @SuppressWarnings("unused")
        private final String field2 = VALUES[1];

        @SuppressWarnings("unused")
        private final String field3 = VALUES[2];

        @SuppressWarnings("unused")
        private final String field4 = VALUES[3];

        @SuppressWarnings("unused")
        private final String field5 = VALUES[4];
    }

    private static final String[] FIELDS = {"field1", "field2", "field3", "field4", "field5"};

    private static final String[] VALUES = {"value 1", "value 2", "value 3", "value 4", "value 5"};

    private static final String SINGLE_FIELD_TO_SHOW = FIELDS[2];

    private static final String SINGLE_FIELD_VALUE_TO_SHOW = VALUES[2];

    private static final String[] FIELDS_TO_SHOW = {FIELDS[0], FIELDS[3]};

    private static final String[] FIELDS_VALUES_TO_SHOW = {VALUES[0], VALUES[3]};

    @Test
    void test_toStringDefaultBehavior() {
        final ReflectionToStringBuilder builder = new ReflectionToStringBuilder(new TestFeature());
        final String toString = builder.toString();
        validateAllFieldsPresent(toString);
    }

    @Test
    void test_toStringInclude() {
        final String toString = ReflectionToStringBuilder.toStringInclude(new TestFeature(), SINGLE_FIELD_TO_SHOW);
        validateIncludeFieldsPresent(toString, new String[]{ SINGLE_FIELD_TO_SHOW }, new String[]{ SINGLE_FIELD_VALUE_TO_SHOW });
    }

    @Test
    void test_toStringIncludeArray() {
        final String toString = ReflectionToStringBuilder.toStringInclude(new TestFeature(), FIELDS_TO_SHOW);
        validateIncludeFieldsPresent(toString, FIELDS_TO_SHOW, FIELDS_VALUES_TO_SHOW);
    }

    @Test
    void test_toStringIncludeArrayWithNull() {
        final String toString = ReflectionToStringBuilder.toStringInclude(new TestFeature(), new String[]{null});
        validateAllFieldsPresent(toString);
    }

    @Test
    void test_toStringIncludeArrayWithNulls() {
        final String toString = ReflectionToStringBuilder.toStringInclude(new TestFeature(), null, null);
        validateAllFieldsPresent(toString);
    }

    @Test
    void test_toStringIncludeCollection() {
        final List<String> includeList = new ArrayList<>();
        includeList.add(SINGLE_FIELD_TO_SHOW);
        final String toString = ReflectionToStringBuilder.toStringInclude(new TestFeature(), includeList);
        validateIncludeFieldsPresent(toString, new String[]{ SINGLE_FIELD_TO_SHOW }, new String[]{ SINGLE_FIELD_VALUE_TO_SHOW });
    }

    @Test
    void test_toStringIncludeCollectionWithNull() {
        final List<String> includeList = new ArrayList<>();
        includeList.add(null);
        final String toString = ReflectionToStringBuilder.toStringInclude(new TestFeature(), includeList);
        validateAllFieldsPresent(toString);
    }

    @Test
    void test_toStringIncludeCollectionWithNulls() {
        final List<String> includeList = new ArrayList<>();
        includeList.add(null);
        includeList.add(null);
        final String toString = ReflectionToStringBuilder.toStringInclude(new TestFeature(), includeList);
        validateAllFieldsPresent(toString);
    }

    @Test
    void test_toStringIncludeEmptyArray() {
        final String toString = ReflectionToStringBuilder.toStringInclude(new TestFeature(), ArrayUtils.EMPTY_STRING_ARRAY);
        validateAllFieldsPresent(toString);
    }

    @Test
    void test_toStringIncludeEmptyCollection() {
        final String toString = ReflectionToStringBuilder.toStringInclude(new TestFeature(), new ArrayList<>());
        validateAllFieldsPresent(toString);
    }

    @Test
    void test_toStringIncludeNullArray() {
        final String toString = ReflectionToStringBuilder.toStringInclude(new TestFeature(), (String[]) null);
        validateAllFieldsPresent(toString);
    }

    @Test
    void test_toStringIncludeNullArrayMultiplesValues() {
        final String toString = ReflectionToStringBuilder.toStringInclude(new TestFeature(), null, null, null, null);
        validateAllFieldsPresent(toString);
    }

    @Test
    void test_toStringIncludeNullCollection() {
        final String toString = ReflectionToStringBuilder.toStringInclude(new TestFeature(), (Collection<String>) null);
        validateAllFieldsPresent(toString);
    }

    @Test
    void test_toStringIncludeWithoutInformingFields() {
        final String toString = ReflectionToStringBuilder.toStringInclude(new TestFeature());
        validateAllFieldsPresent(toString);
    }

    @Test
    void test_toStringSetIncludeAndExcludeWithIntersection() {
        final ReflectionToStringBuilder builder = new ReflectionToStringBuilder(new TestFeature());
        builder.setExcludeFieldNames(FIELDS[1], FIELDS[4]);
        builder.setIncludeFieldNames(FIELDS[0], FIELDS[1]);
        Assertions.assertThrows(IllegalStateException.class, () -> {
            builder.toString();
        });
    }

    @Test
    void test_toStringSetIncludeAndExcludeWithoutIntersection() {
        final ReflectionToStringBuilder builder = new ReflectionToStringBuilder(new TestFeature());
        builder.setExcludeFieldNames(FIELDS[1], FIELDS[4]);
        builder.setIncludeFieldNames(FIELDS_TO_SHOW);
        final String toString = builder.toString();
        validateIncludeFieldsPresent(toString, FIELDS_TO_SHOW, FIELDS_VALUES_TO_SHOW);
    }

    @Test
    void test_toStringSetIncludeAndExcludeWithRandomFieldsWithIntersection() {
        final ReflectionToStringBuilder builder = new ReflectionToStringBuilder(new TestFeature());
        builder.setExcludeFieldNames(FIELDS[1], "random1");
        builder.setIncludeFieldNames("random1");
        Assertions.assertThrows(IllegalStateException.class, () -> {
            builder.toString();
        });
    }

    @Test
    void test_toStringSetIncludeAndExcludeWithRandomFieldsWithoutIntersection() {
        final ReflectionToStringBuilder builder = new ReflectionToStringBuilder(new TestFeature());
        builder.setExcludeFieldNames(FIELDS[1], "random1");
        builder.setIncludeFieldNames("random2", FIELDS[2]);
        final String toString = builder.toString();
        validateIncludeFieldsPresent(toString, new String[]{FIELDS[2]}, new String[]{VALUES[2]});
    }

    @Test
    void test_toStringSetIncludeWithArrayWithMultipleNullFields() {
        final ReflectionToStringBuilder builder = new ReflectionToStringBuilder(new TestFeature());
        builder.setExcludeFieldNames(FIELDS[1], FIELDS[4]);
        builder.setIncludeFieldNames(null, null, null);
        final String toString = builder.toString();
        validateIncludeFieldsPresent(toString, new String[]{FIELDS[0], FIELDS[2], FIELDS[3]}, new String[]{VALUES[0], VALUES[2], VALUES[3]});
    }

    @Test
    void test_toStringSetIncludeWithMultipleNullFields() {
        final ReflectionToStringBuilder builder = new ReflectionToStringBuilder(new TestFeature());
        builder.setExcludeFieldNames(FIELDS[1], FIELDS[4]);
        builder.setIncludeFieldNames(null, null, null);
        final String toString = builder.toString();
        validateIncludeFieldsPresent(toString, new String[]{FIELDS[0], FIELDS[2], FIELDS[3]}, new String[]{VALUES[0], VALUES[2], VALUES[3]});
    }

    private void validateAllFieldsPresent(final String toString) {
        validateIncludeFieldsPresent(toString, FIELDS, VALUES);
    }

    private void validateIncludeFieldsPresent(final String toString, final String[] fieldsToShow, final String[] valuesToShow) {
        for (final String includeField : fieldsToShow) {
            assertTrue(toString.indexOf(includeField) > 0);
        }

        for (final String includeValue : valuesToShow) {
            assertTrue(toString.indexOf(includeValue) > 0);
        }

        validateNonIncludeFieldsAbsent(toString, fieldsToShow, valuesToShow);
    }

    private void validateNonIncludeFieldsAbsent(final String toString, final String[] IncludeFields, final String[] IncludeFieldsValues) {
        final String[] nonIncludeFields = ArrayUtils.removeElements(FIELDS.clone(), IncludeFields);
        final String[] nonIncludeFieldsValues = ArrayUtils.removeElements(VALUES.clone(), IncludeFieldsValues);

        for (final String nonIncludeField : nonIncludeFields) {
            assertEquals(ArrayUtils.INDEX_NOT_FOUND, toString.indexOf(nonIncludeField));
        }

        for (final String nonIncludeValue : nonIncludeFieldsValues) {
            assertEquals(ArrayUtils.INDEX_NOT_FOUND, toString.indexOf(nonIncludeValue));
        }
    }
}
