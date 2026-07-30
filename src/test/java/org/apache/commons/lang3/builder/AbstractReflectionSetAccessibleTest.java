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

import java.lang.reflect.Field;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link AbstractReflection#setAccessible(boolean, java.lang.reflect.AccessibleObject)}.
 */
class AbstractReflectionSetAccessibleTest extends AbstractLangTest {

    @SuppressWarnings("unused")
    private final String privateField = "value";

    @Test
    void setAccessibleHandlesNull() {
        assertFalse(AbstractReflection.setAccessible(true, null));
        assertFalse(AbstractReflection.setAccessible(false, null));
    }

    @Test
    void setAccessibleRespectsForceFlag() throws Exception {
        final Field field = getClass().getDeclaredField("privateField");
        field.setAccessible(false);
        assertFalse(field.isAccessible());
        assertFalse(AbstractReflection.setAccessible(false, field));
        assertFalse(field.isAccessible());
        assertTrue(AbstractReflection.setAccessible(true, field));
        assertTrue(field.isAccessible());
    }

    @Test
    void setAccessibleReturnsTrueWhenAlreadyAccessible() throws Exception {
        final Field field = getClass().getDeclaredField("privateField");
        field.setAccessible(true);
        assertTrue(field.isAccessible());
        assertTrue(AbstractReflection.setAccessible(false, field));
        assertTrue(AbstractReflection.setAccessible(true, field));
    }
}
