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
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.apache.commons.lang3.ArrayUtils;
import org.junit.jupiter.api.Test;

public class ReflectionToStringBuilderSelectedTest {

    class TestFixture {
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
    public void test_toStringSelected() {
        final String toString = ReflectionToStringBuilder.toStringSelected(new TestFixture(), SINGLE_FIELD_TO_SHOW);
        this.validateSelectedFieldsPresent(toString, new String[]{ SINGLE_FIELD_TO_SHOW }, new String[]{ SINGLE_FIELD_VALUE_TO_SHOW });
    }

    @Test
    public void test_toStringSelectedArray() {
        final String toString = ReflectionToStringBuilder.toStringSelected(new TestFixture(), FIELDS_TO_SHOW);
        this.validateSelectedFieldsPresent(toString, FIELDS_TO_SHOW, FIELDS_VALUES_TO_SHOW);
    }

    @Test
    public void test_toStringSelectedArrayWithNull() {
        final String toString = ReflectionToStringBuilder.toStringSelected(new TestFixture(), new String[]{null});
        this.validateAllFieldsPresent(toString);
    }

    @Test
    public void test_toStringSelectedArrayWithNulls() {
        final String toString = ReflectionToStringBuilder.toStringSelected(new TestFixture(), null, null);
        this.validateAllFieldsPresent(toString);
    }

    @Test
    public void test_toStringSelectedCollection() {
        final List<String> selectedList = new ArrayList<>();
        selectedList.add(SINGLE_FIELD_TO_SHOW);
        final String toString = ReflectionToStringBuilder.toStringSelected(new TestFixture(), selectedList);
        this.validateSelectedFieldsPresent(toString, new String[]{ SINGLE_FIELD_TO_SHOW }, new String[]{ SINGLE_FIELD_VALUE_TO_SHOW });
    }

    @Test
    public void test_toStringSelectedCollectionWithNull() {
        final List<String> selectedList = new ArrayList<>();
        selectedList.add(null);
        final String toString = ReflectionToStringBuilder.toStringSelected(new TestFixture(), selectedList);
        this.validateAllFieldsPresent(toString);
    }

    @Test
    public void test_toStringSelectedCollectionWithNulls() {
        final List<String> selectedList = new ArrayList<>();
        selectedList.add(null);
        selectedList.add(null);
        final String toString = ReflectionToStringBuilder.toStringSelected(new TestFixture(), selectedList);
        this.validateAllFieldsPresent(toString);
    }

    @Test
    public void test_toStringSelectedEmptyArray() {
        final String toString = ReflectionToStringBuilder.toStringSelected(new TestFixture(), ArrayUtils.EMPTY_STRING_ARRAY);
        this.validateAllFieldsPresent(toString);
    }

    @Test
    public void test_toStringSelectedEmptyCollection() {
        final String toString = ReflectionToStringBuilder.toStringSelected(new TestFixture(), new ArrayList<>());
        this.validateAllFieldsPresent(toString);
    }

    @Test
    public void test_toStringSelectedNullArray() {
        final String toString = ReflectionToStringBuilder.toStringSelected(new TestFixture(), (String[]) null);
        this.validateAllFieldsPresent(toString);
    }

    @Test
    public void test_toStringSelectedNullCollection() {
        final String toString = ReflectionToStringBuilder.toStringSelected(new TestFixture(), (Collection<String>) null);
        this.validateAllFieldsPresent(toString);
    }

    private void validateAllFieldsPresent(String toString) {
        validateSelectedFieldsPresent(toString, FIELDS, VALUES);
    }

    private void validateSelectedFieldsPresent(final String toString, final String[] fieldsToShow, final String[] valuesToShow) {
        for (String selectedField : fieldsToShow) {
            assertTrue(toString.indexOf(selectedField) > 0);
        }

        for (String selectedValue : valuesToShow) {
            assertTrue(toString.indexOf(selectedValue) > 0);
        }

        this.validateNonSelectedFieldsAbsent(toString, fieldsToShow, valuesToShow);
    }

    private void validateNonSelectedFieldsAbsent(String toString, String[] selectedFields, String[] selectedFieldsValues) {
        String[] nonSelectedFields = ArrayUtils.removeElements(FIELDS.clone(), selectedFields);
        String[] nonSelectedFieldsValues = ArrayUtils.removeElements(VALUES.clone(), selectedFieldsValues);

        for (String nonSelectedField : nonSelectedFields) {
            assertEquals(ArrayUtils.INDEX_NOT_FOUND, toString.indexOf(nonSelectedField));
        }

        for (String nonSelectedValue : nonSelectedFieldsValues) {
            assertEquals(ArrayUtils.INDEX_NOT_FOUND, toString.indexOf(nonSelectedValue));
        }
    }
}
