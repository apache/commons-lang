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

import java.util.Objects;
import java.util.function.Supplier;

import org.apache.commons.lang3.AbstractLangTest;

/**
 * Helps test this package.
 */
class AbstractBuilderTest extends AbstractLangTest {

    /**
     * Delegates to {@link AbstractReflection#accessibleFlag()}.
     *
     * @return {@link AbstractReflection#accessibleFlag()}.
     */
    protected static boolean accessibleFlag() {
        return AbstractReflection.accessibleFlag();
    }

    /**
     * Helps test reflection classes.
     *
     * @param string An input string.
     * @return The given string or "<null>".
     */
    protected static String accessibleString(final String string) {
        return accessibleFlag() ? string : "<null>";
    }

    protected static void assertEqualsIfAccessible(final Object expected, final Object actual) {
        assertEquals(accessibleFlag(), Objects.equals(expected, actual));
    }

    protected static void assertTrueIfAccessible(final boolean test) {
        assertEquals(accessibleFlag(), test);
    }

    protected static void assertTrueIfAccessible(final boolean test, final Supplier<String> messageSupplier) {
        assertEquals(accessibleFlag(), test, messageSupplier);
    }
}
