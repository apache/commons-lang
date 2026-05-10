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

import java.util.HashMap;

import org.junit.jupiter.api.Test;
import org.junitpioneer.jupiter.SetSystemProperty;

/**
 * Like {@link EqualsBuilderTest} with force accessible disabled.
 */
@SetSystemProperty(key = AbstractReflectionTest.FORCE_ACCESSIBLE, value = "false")
public class EqualsBuilderNoForceTest extends EqualsBuilderTest {

    @Test
    void testReflectionOnCustomArrayList() {
        assertFalse(EqualsBuilder.reflectionEquals(new TestArrayList(1, "2", "3", "4"), new TestArrayList(1, "2", "3", "4")));
        assertFalse(EqualsBuilder.reflectionEquals(new TestArrayList(1, "2", "3", "4"), new TestArrayList(2, "2", "3", "4")));
    }

    @Test
    void testReflectionOnCustomHashMap() {
        assertFalse(EqualsBuilder.reflectionEquals(new TestHashMap(1, new HashMap<>()), new TestHashMap(1, new HashMap<>())));
        assertFalse(EqualsBuilder.reflectionEquals(new TestHashMap(1, new HashMap<>()), new TestHashMap(2, new HashMap<>())));
    }
}
