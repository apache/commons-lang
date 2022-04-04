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
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.apache.commons.lang3.ArrayUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class ReflectionToStringBuilderIncludeTest {

    class TestFeature {
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
    public void test_toStringInclude() {
        final String toString = ReflectionToStringBuilder.toStringInclude(new TestFeature(), SINGLE_FIELD_TO_SHOW);
        this.validateIncludeFieldsPresent(toString, new String[]{ SINGLE_FIELD_TO_SHOW }, new String[]{ SINGLE_FIELD_VALUE_TO_SHOW });
    }

    @Test
    public void test_toStringIncludeArray() {
        final String toString = ReflectionToStringBuilder.toStringInclude(new TestFeature(), FIELDS_TO_SHOW);
        this.validateIncludeFieldsPresent(toString, FIELDS_TO_SHOW, FIELDS_VALUES_TO_SHOW);
    }

    @Test
    public void test_toStringIncludeWithoutInformingFields() {
        final String toString = ReflectionToStringBuilder.toStringInclude(new TestFeature());
        this.validateAllFieldsPresent(toString);
    }

    @Test
    public void test_toStringIncludeArrayWithNull() {
        final String toString = ReflectionToStringBuilder.toStringInclude(new TestFeature(), new String[]{null});
        this.validateAllFieldsPresent(toString);
    }

    @Test
    public void test_toStringIncludeArrayWithNulls() {
        final String toString = ReflectionToStringBuilder.toStringInclude(new TestFeature(), null, null);
        this.validateAllFieldsPresent(toString);
    }

    @Test
    public void test_toStringIncludeCollection() {
        final List<String> IncludeList = new ArrayList<>();
        IncludeList.add(SINGLE_FIELD_TO_SHOW);
        final String toString = ReflectionToStringBuilder.toStringInclude(new TestFeature(), IncludeList);
        this.validateIncludeFieldsPresent(toString, new String[]{ SINGLE_FIELD_TO_SHOW }, new String[]{ SINGLE_FIELD_VALUE_TO_SHOW });
    }

    @Test
    public void test_toStringIncludeCollectionWithNull() {
        final List<String> IncludeList = new ArrayList<>();
        IncludeList.add(null);
        final String toString = ReflectionToStringBuilder.toStringInclude(new TestFeature(), IncludeList);
        this.validateAllFieldsPresent(toString);
    }

    @Test
    public void test_toStringIncludeCollectionWithNulls() {
        final List<String> IncludeList = new ArrayList<>();
        IncludeList.add(null);
        IncludeList.add(null);
        final String toString = ReflectionToStringBuilder.toStringInclude(new TestFeature(), IncludeList);
        this.validateAllFieldsPresent(toString);
    }

    @Test
    public void test_toStringIncludeEmptyArray() {
        final String toString = ReflectionToStringBuilder.toStringInclude(new TestFeature(), ArrayUtils.EMPTY_STRING_ARRAY);
        this.validateAllFieldsPresent(toString);
    }

    @Test
    public void test_toStringIncludeEmptyCollection() {
        final String toString = ReflectionToStringBuilder.toStringInclude(new TestFeature(), new ArrayList<>());
        this.validateAllFieldsPresent(toString);
    }

    @Test
    public void test_toStringIncludeNullArray() {
        final String toString = ReflectionToStringBuilder.toStringInclude(new TestFeature(), (String[]) null);
        this.validateAllFieldsPresent(toString);
    }

    @Test
    public void test_toStringIncludeNullCollection() {
        final String toString = ReflectionToStringBuilder.toStringInclude(new TestFeature(), (Collection<String>) null);
        this.validateAllFieldsPresent(toString);
    }

    @Test
    public void test_toStringDefaultBehavior() {
        ReflectionToStringBuilder builder = new ReflectionToStringBuilder(new TestFeature());
        final String toString = builder.toString();
        this.validateAllFieldsPresent(toString);
    }

    @Test
    public void test_toStringSetIncludeAndExcludeWithoutIntersection() {
        ReflectionToStringBuilder builder = new ReflectionToStringBuilder(new TestFeature());
        builder.setExcludeFieldNames(FIELDS[1], FIELDS[4]);
        builder.setIncludeFieldNames(FIELDS_TO_SHOW);
        final String toString = builder.toString();
        this.validateIncludeFieldsPresent(toString, FIELDS_TO_SHOW, FIELDS_VALUES_TO_SHOW);
    }

    @Test
    public void test_toStringSetIncludeAndExcludeWithIntersection() {
        ReflectionToStringBuilder builder = new ReflectionToStringBuilder(new TestFeature());
        builder.setExcludeFieldNames(FIELDS[1], FIELDS[4]);
        builder.setIncludeFieldNames(FIELDS[0], FIELDS[1]);
        Assertions.assertThrows(IllegalStateException.class, () -> {
            builder.toString();
        });
    }

    private void validateAllFieldsPresent(String toString) {
        validateIncludeFieldsPresent(toString, FIELDS, VALUES);
    }

    private void validateIncludeFieldsPresent(final String toString, final String[] fieldsToShow, final String[] valuesToShow) {
        for (String IncludeField : fieldsToShow) {
            assertTrue(toString.indexOf(IncludeField) > 0);
        }

        for (String IncludeValue : valuesToShow) {
            assertTrue(toString.indexOf(IncludeValue) > 0);
        }

        this.validateNonIncludeFieldsAbsent(toString, fieldsToShow, valuesToShow);
    }

    private void validateNonIncludeFieldsAbsent(String toString, String[] IncludeFields, String[] IncludeFieldsValues) {
        String[] nonIncludeFields = ArrayUtils.removeElements(FIELDS.clone(), IncludeFields);
        String[] nonIncludeFieldsValues = ArrayUtils.removeElements(VALUES.clone(), IncludeFieldsValues);

        for (String nonIncludeField : nonIncludeFields) {
            assertEquals(ArrayUtils.INDEX_NOT_FOUND, toString.indexOf(nonIncludeField));
        }

        for (String nonIncludeValue : nonIncludeFieldsValues) {
            assertEquals(ArrayUtils.INDEX_NOT_FOUND, toString.indexOf(nonIncludeValue));
        }
    }
}
