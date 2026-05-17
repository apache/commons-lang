/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
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

import org.apache.commons.lang3.ArrayUtils;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link ReflectionToStringBuilder} allowing overriding annotations, for example in third party code.
 */
class ReflectionToStringBuilderOverrideExclude {

    static class SensitiveBean {

        String email = INCLUDE_2;
        @ToStringExclude
        String password = EXCLUDE;
        String username = INCLUDE_1;
    }
    private static final String EXCLUDE = "s3cr3t!";

    private static final String INCLUDE_1 = "alice";

    private static final String INCLUDE_2 = "alice@example.com";

    @Test
    void toStringExcludeFieldNotExposedViaIncludeList() {
        final SensitiveBean bean = new SensitiveBean();
        // Explicitly include the @ToStringExclude field by name
        final String result = ReflectionToStringBuilder.toStringExclude(bean, ArrayUtils.EMPTY_STRING_ARRAY);
        assertFalse(result.contains(EXCLUDE));
        assertTrue(result.contains(INCLUDE_1));
        assertTrue(result.contains(INCLUDE_2));
    }

    @Test
    void toStringIncludeDoesNotExposeExcludedField() {
        final SensitiveBean bean = new SensitiveBean();
        // Include both username and password explicitly (password is @ToStringExclude)
        final ReflectionToStringBuilder builder = new ReflectionToStringBuilder(bean, ToStringStyle.SHORT_PREFIX_STYLE);
        // By default, @ToStringExclude kicks in.
        assertFalse(builder.toString().contains(EXCLUDE));
        assertTrue(builder.toString().contains(INCLUDE_1));
        // But I really want it
        builder.setIncludeFieldNames("username", "password");
        assertTrue(builder.toString().contains(EXCLUDE));
        assertTrue(builder.toString().contains(INCLUDE_1));
        assertTrue(builder.toString().contains(INCLUDE_2));
    }

    @Test
    void toStringIncludeExposesNonExcludedFields() {
        final SensitiveBean bean = new SensitiveBean();
        final ReflectionToStringBuilder builder = new ReflectionToStringBuilder(bean, ToStringStyle.SHORT_PREFIX_STYLE);
        builder.setIncludeFieldNames("username", "email");
        final String result = builder.toString();
        assertTrue(result.contains(INCLUDE_1));
        assertTrue(result.contains(INCLUDE_2));
    }
}
