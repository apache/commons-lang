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

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

public class ReflectionToStringBuilderExcludeNullValuesTest extends AbstractLangTest {

    static class TestFixture {
        @SuppressWarnings("unused")
        private final Integer testIntegerField;
        @SuppressWarnings("unused")
        private final String testStringField;

        TestFixture(final Integer a, final String b) {
            this.testIntegerField = a;
            this.testStringField = b;
        }
    }

    private static final String INTEGER_FIELD_NAME = "testIntegerField";
    private static final String STRING_FIELD_NAME = "testStringField";
    private final TestFixture BOTH_NON_NULL = new TestFixture(0, "str");
    private final TestFixture FIRST_NULL = new TestFixture(null, "str");
    private final TestFixture SECOND_NULL = new TestFixture(0, null);
    private final TestFixture BOTH_NULL = new TestFixture(null, null);

    @Test
    public void test_NonExclude() {
        //normal case=
        String toString = ReflectionToStringBuilder.toString(BOTH_NON_NULL, null, false, false, false, null);
        assertTrue(toString.contains(INTEGER_FIELD_NAME));
        assertTrue(toString.contains(STRING_FIELD_NAME));

        //make one null
        toString = ReflectionToStringBuilder.toString(FIRST_NULL, null, false, false, false, null);
        assertTrue(toString.contains(INTEGER_FIELD_NAME));
        assertTrue(toString.contains(STRING_FIELD_NAME));

        //other one null
        toString = ReflectionToStringBuilder.toString(SECOND_NULL, null, false, false, false, null);
        assertTrue(toString.contains(INTEGER_FIELD_NAME));
        assertTrue(toString.contains(STRING_FIELD_NAME));

        //make the both null
        toString = ReflectionToStringBuilder.toString(BOTH_NULL, null, false, false, false, null);
        assertTrue(toString.contains(INTEGER_FIELD_NAME));
        assertTrue(toString.contains(STRING_FIELD_NAME));
    }

    @Test
    public void test_excludeNull() {

        //test normal case
        String toString = ReflectionToStringBuilder.toString(BOTH_NON_NULL, null, false, false, true, null);
        assertTrue(toString.contains(INTEGER_FIELD_NAME));
        assertTrue(toString.contains(STRING_FIELD_NAME));

        //make one null
        toString = ReflectionToStringBuilder.toString(FIRST_NULL, null, false, false, true, null);
        assertFalse(toString.contains(INTEGER_FIELD_NAME));
        assertTrue(toString.contains(STRING_FIELD_NAME));

        //other one null
        toString = ReflectionToStringBuilder.toString(SECOND_NULL, null, false, false, true, null);
        assertTrue(toString.contains(INTEGER_FIELD_NAME));
        assertFalse(toString.contains(STRING_FIELD_NAME));

        //both null
        toString = ReflectionToStringBuilder.toString(BOTH_NULL, null, false, false, true, null);
        assertFalse(toString.contains(INTEGER_FIELD_NAME));
        assertFalse(toString.contains(STRING_FIELD_NAME));
    }

    @Test
    public void test_ConstructorOption() {
        ReflectionToStringBuilder builder = new ReflectionToStringBuilder(BOTH_NON_NULL, null, null, null, false, false, true);
        assertTrue(builder.isExcludeNullValues());
        String toString = builder.toString();
        assertTrue(toString.contains(INTEGER_FIELD_NAME));
        assertTrue(toString.contains(STRING_FIELD_NAME));

        builder = new ReflectionToStringBuilder(FIRST_NULL, null, null, null, false, false, true);
        toString = builder.toString();
        assertFalse(toString.contains(INTEGER_FIELD_NAME));
        assertTrue(toString.contains(STRING_FIELD_NAME));

        builder = new ReflectionToStringBuilder(SECOND_NULL, null, null, null, false, false, true);
        toString = builder.toString();
        assertTrue(toString.contains(INTEGER_FIELD_NAME));
        assertFalse(toString.contains(STRING_FIELD_NAME));

        builder = new ReflectionToStringBuilder(BOTH_NULL, null, null, null, false, false, true);
        toString = builder.toString();
        assertFalse(toString.contains(INTEGER_FIELD_NAME));
        assertFalse(toString.contains(STRING_FIELD_NAME));
    }

    @Test
    public void test_ConstructorOptionNormal() {
        final ReflectionToStringBuilder builder = new ReflectionToStringBuilder(BOTH_NULL, null, null, null, false, false, false);
        assertFalse(builder.isExcludeNullValues());
        String toString = builder.toString();
        assertTrue(toString.contains(STRING_FIELD_NAME));
        assertTrue(toString.contains(INTEGER_FIELD_NAME));

        //regression test older constructors
        ReflectionToStringBuilder oldBuilder = new ReflectionToStringBuilder(BOTH_NULL);
        toString = oldBuilder.toString();
        assertTrue(toString.contains(STRING_FIELD_NAME));
        assertTrue(toString.contains(INTEGER_FIELD_NAME));

        oldBuilder = new ReflectionToStringBuilder(BOTH_NULL, null, null, null, false, false);
        toString = oldBuilder.toString();
        assertTrue(toString.contains(STRING_FIELD_NAME));
        assertTrue(toString.contains(INTEGER_FIELD_NAME));

        oldBuilder = new ReflectionToStringBuilder(BOTH_NULL, null, null);
        toString = oldBuilder.toString();
        assertTrue(toString.contains(STRING_FIELD_NAME));
        assertTrue(toString.contains(INTEGER_FIELD_NAME));
    }

    @Test
    public void test_ConstructorOption_ExcludeNull() {
        ReflectionToStringBuilder builder = new ReflectionToStringBuilder(BOTH_NULL, null, null, null, false, false, false);
        builder.setExcludeNullValues(true);
        assertTrue(builder.isExcludeNullValues());
        String toString = builder.toString();
        assertFalse(toString.contains(STRING_FIELD_NAME));
        assertFalse(toString.contains(INTEGER_FIELD_NAME));

        builder = new ReflectionToStringBuilder(BOTH_NULL, null, null, null, false, false, true);
        toString = builder.toString();
        assertFalse(toString.contains(STRING_FIELD_NAME));
        assertFalse(toString.contains(INTEGER_FIELD_NAME));

        final ReflectionToStringBuilder oldBuilder = new ReflectionToStringBuilder(BOTH_NULL);
        oldBuilder.setExcludeNullValues(true);
        assertTrue(oldBuilder.isExcludeNullValues());
        toString = oldBuilder.toString();
        assertFalse(toString.contains(STRING_FIELD_NAME));
        assertFalse(toString.contains(INTEGER_FIELD_NAME));
    }

}
