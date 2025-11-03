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

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Test class for ToStringExclude annotation
 */
class ReflectionToStringBuilderExcludeWithAnnotationTest extends AbstractLangTest {

    final class TestFixture {
        @ToStringExclude
        private final String excludedField = EXCLUDED_FIELD_VALUE;

        @SuppressWarnings("unused")
        private final String includedField = INCLUDED_FIELD_VALUE;
    }

    private static final String INCLUDED_FIELD_NAME = "includedField";

    private static final String INCLUDED_FIELD_VALUE = "Hello World!";

    private static final String EXCLUDED_FIELD_NAME = "excludedField";

    private static final String EXCLUDED_FIELD_VALUE = "excluded field value";

    @Test
    void test_toStringExclude() {
        final String toString = ReflectionToStringBuilder.toString(new TestFixture());
        assertFalse(toString.contains(EXCLUDED_FIELD_NAME));
        assertFalse(toString.contains(EXCLUDED_FIELD_VALUE));
        assertTrue(toString.contains(INCLUDED_FIELD_NAME));
        assertTrue(toString.contains(INCLUDED_FIELD_VALUE));
    }

}
