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

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.not;

/**
 * Test class for ToStringExclude annotation
 */
public class ReflectionToStringBuilderIncludeWithAnnotationTest extends AbstractLangTest {

    class TestFixture {
        @ToStringExclude
        private final String excludedField = EXCLUDED_FIELD_VALUE;

        @SuppressWarnings("unused")
        private final String includedField = INCLUDED_FIELD_VALUE;

        @ToStringInclude
        private String toStringExcludedField() {
            return EXCLUDED_FIELD_VALUE_MODIFIED;
        }

        @ToStringInclude("modifiedExcludedField")
        private String toStringModifiedExcludedField() {
            return EXCLUDED_FIELD_VALUE_MODIFIED;
        }

        private String methodNotAnnotatedWithToStringInclude() {
            return null;
        }
    }

    private static final String INCLUDED_FIELD_NAME = "includedField";

    private static final String INCLUDED_FIELD_VALUE = "Hello World!";

    private static final String EXCLUDED_FIELD_NAME = "excludedField";

    private static final String EXCLUDED_FIELD_VALUE = "excluded field value";
    private static final String EXCLUDED_FIELD_VALUE_MODIFIED = "excluded field modified value";

    @Test
    public void test_toStringInclude() {
        final String toString = ReflectionToStringBuilder.toString(new TestFixture());

        assertThat(toString, not(containsString(EXCLUDED_FIELD_NAME)));
        assertThat(toString, not(containsString(EXCLUDED_FIELD_VALUE)));
        assertThat(toString, containsString(INCLUDED_FIELD_NAME));
        assertThat(toString, containsString(INCLUDED_FIELD_VALUE));

        assertThat(toString, containsString("toStringExcludedField"));  // method name when annotation value is not set
        assertThat(toString, containsString("modifiedExcludedField"));  // Annotation value when its set explicitly
        assertThat(toString, containsString(EXCLUDED_FIELD_VALUE_MODIFIED));
    }

}
