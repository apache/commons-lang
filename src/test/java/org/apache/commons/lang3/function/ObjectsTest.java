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

package org.apache.commons.lang3.function;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.function.Supplier;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

public class ObjectsTest extends AbstractLangTest {

    public static class TestableFailableSupplier<O, E extends Exception> implements FailableSupplier<O, E> {
        private final FailableSupplier<O, E> supplier;
        private boolean invoked;

        TestableFailableSupplier(final FailableSupplier<O, E> pSupplier) {
            this.supplier = pSupplier;
        }

        @Override
        public O get() throws E {
            invoked = true;
            return supplier.get();
        }

        public boolean isInvoked() {
            return invoked;
        }
    }

    public static class TestableSupplier<O> implements Supplier<O> {
        private final Supplier<O> supplier;
        private boolean invoked;

        TestableSupplier(final Supplier<O> pSupplier) {
            this.supplier = pSupplier;
        }

        @Override
        public O get() {
            invoked = true;
            return supplier.get();
        }

        public boolean isInvoked() {
            return invoked;
        }
    }

    @Test
    void testRequireNonNullObject() {
        assertSame("foo", Objects.requireNonNull("foo"));
        try {
            Objects.requireNonNull(null);
            fail("Expected Exception");
        } catch (final NullPointerException e) {
            assertEquals("The value must not be null.", e.getMessage());
        }
    }

    @Test
    void testRequireNonNullObjectFailableSupplierString() {
        final TestableFailableSupplier<String, ?> supplier = new TestableFailableSupplier<>(() -> null);
        assertSame("foo", Objects.requireNonNull("foo", supplier));
        assertFalse(supplier.isInvoked());
        try {
            Objects.requireNonNull(null, supplier);
            fail("Expected Exception");
        } catch (final NullPointerException e) {
            assertEquals("The supplier must not return null.", e.getMessage());
            assertTrue(supplier.isInvoked());
        }
        final TestableFailableSupplier<String, ?> supplier2 = new TestableFailableSupplier<>(() -> null);
        try {
            Objects.requireNonNull(null, supplier2);
            fail("Expected Exception");
        } catch (final NullPointerException e) {
            assertEquals("The supplier must not return null.", e.getMessage());
            assertTrue(supplier2.isInvoked());
        }
        final TestableFailableSupplier<String, ?> supplier3 = new TestableFailableSupplier<>(() -> "bar");
        assertSame("bar", Objects.requireNonNull(null, supplier3));
        final RuntimeException rte = new RuntimeException();
        final TestableFailableSupplier<String, ?> supplier4 = new TestableFailableSupplier<>(() -> {
            throw rte;
        });
        try {
            Objects.requireNonNull(null, supplier4);
            fail("Expected Exception");
        } catch (final RuntimeException e) {
            assertSame(rte, e);
            assertTrue(supplier4.isInvoked());
        }
    }

    @Test
    void testRequireNonNullObjectString() {
        assertSame("foo", Objects.requireNonNull("foo", "bar"));
        try {
            Objects.requireNonNull(null, "bar");
            fail("Expected Exception");
        } catch (final NullPointerException e) {
            assertEquals("bar", e.getMessage());
        }
    }

    @Test
    void testRequireNonNullObjectSupplierString() {
        final TestableSupplier<String> supplier = new TestableSupplier<>(() -> "bar");
        assertSame("foo", Objects.requireNonNull("foo", supplier));
        assertFalse(supplier.isInvoked());
        try {
            Objects.requireNonNull(null, supplier);
            fail("Expected Exception");
        } catch (final NullPointerException e) {
            assertEquals("bar", e.getMessage());
            assertTrue(supplier.isInvoked());
        }
    }
}
