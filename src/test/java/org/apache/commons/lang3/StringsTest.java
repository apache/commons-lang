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

package org.apache.commons.lang3;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

/**
 * Tests {@link Strings}.
 */
public class StringsTest {

    @Test
    public void testBuilder() {
        assertTrue(Strings.builder().setIgnoreCase(false).get().isCaseSensitive());
        assertFalse(Strings.builder().setIgnoreCase(true).get().isCaseSensitive());
        //
        assertTrue(Strings.builder().setNullIsLess(false).get().isCaseSensitive());
        assertTrue(Strings.builder().setNullIsLess(true).get().isCaseSensitive());
    }

    @Test
    public void testBuilderDefaults() {
        final Strings strings = Strings.builder().get();
        assertTrue(strings.isCaseSensitive());
    }

    @Test
    public void testCaseInsensitiveConstant() {
        assertNotNull(Strings.CI);
        assertFalse(Strings.CI.isCaseSensitive());
    }

    @Test
    public void testCaseSensitiveConstant() {
        assertNotNull(Strings.CS);
        assertTrue(Strings.CS.isCaseSensitive());
    }
}
