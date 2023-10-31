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

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class TypedEqualsBuilderTest {

    @Test
    void test_complete_equals() {
        TestObject testObject1 = new TestObject(1, 2);
        TestObject testObject2 = new TestObject(1, 3);
        TestObject testObject3 = new TestObject(1, 2);

        assertEquals(testObject1, testObject1);
        assertNotEquals(testObject1, testObject2);
        assertNotEquals(testObject2, testObject1);

        assertTrue(
                new TypedEqualsBuilder<>(testObject1, testObject2)
                        .append(TestObject::getA)
                        .isEquals());
        assertTrue(
                new TypedEqualsBuilder<>(testObject1, testObject3)
                        .append(TestObject::getA)
                        .append(TestObject::getB)
                        .isEquals());
        assertFalse(
                new TypedEqualsBuilder<>(testObject1, testObject2)
                        .append(TestObject::getA)
                        .append(TestObject::getB)
                        .isEquals());
        assertFalse(
                new TypedEqualsBuilder<>(testObject2, testObject1)
                        .append(TestObject::getA)
                        .append(TestObject::getB)
                        .isEquals());
        assertFalse(
                new TypedEqualsBuilder<>(testObject1, null)
                        .append(TestObject::getA)
                        .isEquals());
        assertFalse(
                new TypedEqualsBuilder<>(testObject1, "")
                        .append(TestObject::getA)
                        .isEquals());
        assertThrows(RuntimeException.class,
                () -> new TypedEqualsBuilder<>(testObject1, testObject2)
                        .append(TestObject::getSomethingWithException)
                        .isEquals());
    }

    private static class TestObject {
        int a;
        int b;

        public TestObject(int a, int b) {
            this.a = a;
            this.b = b;
        }

        public int getA() {
            return a;
        }

        public int getB() {
            return b;
        }

        public int getSomethingWithException() throws Exception {
            throw new Exception("something went wrong");
        }

        @Override
        public boolean equals(Object obj) {
            return new TypedEqualsBuilder<>(this, obj)
                    .append(TestObject::getA)
                    .append(TestObject::getB)
                    .isEquals();
        }
    }
}