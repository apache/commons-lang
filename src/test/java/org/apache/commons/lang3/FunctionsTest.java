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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.lang.reflect.UndeclaredThrowableException;
import java.util.concurrent.Callable;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.BiPredicate;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

import org.apache.commons.lang3.Functions.FailableBiConsumer;
import org.apache.commons.lang3.Functions.FailableBiFunction;
import org.apache.commons.lang3.Functions.FailableCallable;
import org.apache.commons.lang3.Functions.FailableConsumer;
import org.apache.commons.lang3.Functions.FailableFunction;
import org.apache.commons.lang3.Functions.FailableSupplier;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class FunctionsTest {

    public static class CloseableObject {
        private boolean closed;

        public void close() {
            closed = true;
        }

        public boolean isClosed() {
            return closed;
        }

        public void reset() {
            closed = false;
        }

        public void run(final Throwable pTh) throws Throwable {
            if (pTh != null) {
                throw pTh;
            }
        }
    }

    public static class FailureOnOddInvocations {
        private static int invocations;

        static boolean failingBool() throws SomeException {
            throwOnOdd();
            return true;
        }

        private static void throwOnOdd() throws SomeException {
            final int i = ++invocations;
            if (i % 2 == 1) {
                throw new SomeException("Odd Invocation: " + i);
            }
        }

        FailureOnOddInvocations() throws SomeException {
            throwOnOdd();
        }

        boolean getAsBoolean() throws SomeException {
            throwOnOdd();
            return true;
        }
    }

    public static class SomeException extends Exception {

        private static final long serialVersionUID = -4965704778119283411L;

        private Throwable t;

        SomeException(final String message) {
            super(message);
        }

        public void setThrowable(final Throwable throwable) {
            t = throwable;
        }

        public void test() throws Throwable {
            if (t != null) {
                throw t;
            }
        }
    }

    public static class Testable<T, P> {
        private T acceptedObject;
        private P acceptedPrimitiveObject;
        private Throwable throwable;

        Testable(final Throwable throwable) {
            this.throwable = throwable;
        }

        public T getAcceptedObject() {
            return acceptedObject;
        }

        public P getAcceptedPrimitiveObject() {
            return acceptedPrimitiveObject;
        }

        public void setThrowable(final Throwable throwable) {
            this.throwable = throwable;
        }

        public void test() throws Throwable {
            test(throwable);
        }

        public void test(final Throwable throwable) throws Throwable {
            if (throwable != null) {
                throw throwable;
            }
        }

        public boolean testBooleanPrimitive() throws Throwable {
            return testBooleanPrimitive(throwable);
        }

        public boolean testBooleanPrimitive(final Throwable throwable) throws Throwable {
            if (throwable != null) {
                throw throwable;
            }
            return false;
        }

        public void testDouble(double i) throws Throwable {
            test(throwable);
            acceptedPrimitiveObject = (P) ((Double) i);
        }

        public double testDoublePrimitive() throws Throwable {
            return testDoublePrimitive(throwable);
        }

        public double testDoublePrimitive(final Throwable throwable) throws Throwable {
            if (throwable != null) {
                throw throwable;
            }
            return 0;
        }

        public void testInt(int i) throws Throwable {
            test(throwable);
            acceptedPrimitiveObject = (P) ((Integer) i);
        }

        public Integer testInteger() throws Throwable {
            return testInteger(throwable);
        }

        public Integer testInteger(final Throwable throwable) throws Throwable {
            if (throwable != null) {
                throw throwable;
            }
            return 0;
        }

        public int testIntPrimitive() throws Throwable {
            return testIntPrimitive(throwable);
        }

        public int testIntPrimitive(final Throwable throwable) throws Throwable {
            if (throwable != null) {
                throw throwable;
            }
            return 0;
        }

        public void testLong(long i) throws Throwable {
            test(throwable);
            acceptedPrimitiveObject = (P) ((Long) i);
        }

        public long testLongPrimitive() throws Throwable {
            return testLongPrimitive(throwable);
        }

        public long testLongPrimitive(final Throwable throwable) throws Throwable {
            if (throwable != null) {
                throw throwable;
            }
            return 0;
        }

        public void testObjDouble(T object, double i) throws Throwable {
            test(throwable);
            acceptedObject = object;
            acceptedPrimitiveObject = (P) ((Double) i);
        }

        public void testObjInt(T object, int i) throws Throwable {
            test(throwable);
            acceptedObject = object;
            acceptedPrimitiveObject = (P) ((Integer) i);
        }

        public void testObjLong(T object, long i) throws Throwable {
            test(throwable);
            acceptedObject = object;
            acceptedPrimitiveObject = (P) ((Long) i);
        }
    }

    @Test
    public void testConstructor() {
        // We allow this, which must be an omission to make the ctor private.
        new Functions();
    }

    @Test
    void testAcceptBiConsumer() {
        final IllegalStateException ise = new IllegalStateException();
        final Testable<?, ?> testable = new Testable(null);
        Throwable e = assertThrows(IllegalStateException.class, () -> Functions.accept(Testable::test, testable, ise));
        assertSame(ise, e);

        final Error error = new OutOfMemoryError();
        e = assertThrows(OutOfMemoryError.class, () -> Functions.accept(Testable::test, testable, error));
        assertSame(error, e);

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Functions.accept(Testable::test, testable, ioe));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        testable.setThrowable(null);
        Functions.accept(Testable::test, testable, (Throwable) null);
    }

    @Test
    void testAcceptConsumer() {
        final IllegalStateException ise = new IllegalStateException();
        final Testable<?, ?> testable = new Testable<>(ise);
        Throwable e = assertThrows(IllegalStateException.class, () -> Functions.accept(Testable::test, testable));
        assertSame(ise, e);

        final Error error = new OutOfMemoryError();
        testable.setThrowable(error);
        e = assertThrows(OutOfMemoryError.class, () -> Functions.accept(Testable::test, testable));
        assertSame(error, e);

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Functions.accept(Testable::test, testable));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        testable.setThrowable(null);
        Functions.accept(Testable::test, testable);
    }

    @Test
    void testAcceptDoubleConsumer() {
        final IllegalStateException ise = new IllegalStateException();
        final Testable<?, Double> testable = new Testable<>(ise);
        Throwable e = assertThrows(IllegalStateException.class, () -> Functions.accept(testable::testDouble, 1d));
        assertSame(ise, e);
        assertNull(testable.getAcceptedPrimitiveObject());

        final Error error = new OutOfMemoryError();
        testable.setThrowable(error);
        e = assertThrows(OutOfMemoryError.class, () -> Functions.accept(testable::testDouble, 1d));
        assertSame(error, e);
        assertNull(testable.getAcceptedPrimitiveObject());

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Functions.accept(testable::testDouble, 1d));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);
        assertNull(testable.getAcceptedPrimitiveObject());

        testable.setThrowable(null);
        Functions.accept(testable::testDouble, 1d);
        assertEquals(1, testable.getAcceptedPrimitiveObject());
    }

    @Test
    void testAcceptIntConsumer() {
        final IllegalStateException ise = new IllegalStateException();
        final Testable<?, Integer> testable = new Testable<>(ise);
        Throwable e = assertThrows(IllegalStateException.class, () -> Functions.accept(testable::testInt, 1));
        assertSame(ise, e);
        assertNull(testable.getAcceptedPrimitiveObject());

        final Error error = new OutOfMemoryError();
        testable.setThrowable(error);
        e = assertThrows(OutOfMemoryError.class, () -> Functions.accept(testable::testInt, 1));
        assertSame(error, e);
        assertNull(testable.getAcceptedPrimitiveObject());

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Functions.accept(testable::testInt, 1));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);
        assertNull(testable.getAcceptedPrimitiveObject());

        testable.setThrowable(null);
        Functions.accept(testable::testInt, 1);
        assertEquals(1, testable.getAcceptedPrimitiveObject());
    }

    @Test
    void testAcceptLongConsumer() {
        final IllegalStateException ise = new IllegalStateException();
        final Testable<?, Long> testable = new Testable<>(ise);
        Throwable e = assertThrows(IllegalStateException.class, () -> Functions.accept(testable::testLong, 1L));
        assertSame(ise, e);
        assertNull(testable.getAcceptedPrimitiveObject());

        final Error error = new OutOfMemoryError();
        testable.setThrowable(error);
        e = assertThrows(OutOfMemoryError.class, () -> Functions.accept(testable::testLong, 1L));
        assertSame(error, e);
        assertNull(testable.getAcceptedPrimitiveObject());

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Functions.accept(testable::testLong, 1L));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);
        assertNull(testable.getAcceptedPrimitiveObject());

        testable.setThrowable(null);
        Functions.accept(testable::testLong, 1L);
        assertEquals(1, testable.getAcceptedPrimitiveObject());
    }

    @Test
    void testAcceptObjDoubleConsumer() {
        final IllegalStateException ise = new IllegalStateException();
        final Testable<String, Double> testable = new Testable<>(ise);
        Throwable e = assertThrows(IllegalStateException.class, () -> Functions.accept(testable::testObjDouble, "X", 1d));
        assertSame(ise, e);
        assertNull(testable.getAcceptedObject());
        assertNull(testable.getAcceptedPrimitiveObject());

        final Error error = new OutOfMemoryError();
        testable.setThrowable(error);
        e = assertThrows(OutOfMemoryError.class, () -> Functions.accept(testable::testObjDouble, "X", 1d));
        assertSame(error, e);
        assertNull(testable.getAcceptedObject());
        assertNull(testable.getAcceptedPrimitiveObject());

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Functions.accept(testable::testObjDouble, "X", 1d));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);
        assertNull(testable.getAcceptedObject());
        assertNull(testable.getAcceptedPrimitiveObject());

        testable.setThrowable(null);
        Functions.accept(testable::testObjDouble, "X", 1d);
        assertEquals("X", testable.getAcceptedObject());
        assertEquals(1d, testable.getAcceptedPrimitiveObject());
    }

    @Test
    void testAcceptObjIntConsumer() {
        final IllegalStateException ise = new IllegalStateException();
        final Testable<String, Integer> testable = new Testable<>(ise);
        Throwable e = assertThrows(IllegalStateException.class, () -> Functions.accept(testable::testObjInt, "X", 1));
        assertSame(ise, e);
        assertNull(testable.getAcceptedObject());
        assertNull(testable.getAcceptedPrimitiveObject());

        final Error error = new OutOfMemoryError();
        testable.setThrowable(error);
        e = assertThrows(OutOfMemoryError.class, () -> Functions.accept(testable::testObjInt, "X", 1));
        assertSame(error, e);
        assertNull(testable.getAcceptedObject());
        assertNull(testable.getAcceptedPrimitiveObject());

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Functions.accept(testable::testObjInt, "X", 1));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);
        assertNull(testable.getAcceptedObject());
        assertNull(testable.getAcceptedPrimitiveObject());

        testable.setThrowable(null);
        Functions.accept(testable::testObjInt, "X", 1);
        assertEquals("X", testable.getAcceptedObject());
        assertEquals(1, testable.getAcceptedPrimitiveObject());
    }

    @Test
    void testAcceptObjLongConsumer() {
        final IllegalStateException ise = new IllegalStateException();
        final Testable<String, Long> testable = new Testable<>(ise);
        Throwable e = assertThrows(IllegalStateException.class, () -> Functions.accept(testable::testObjLong, "X", 1L));
        assertSame(ise, e);
        assertNull(testable.getAcceptedObject());
        assertNull(testable.getAcceptedPrimitiveObject());

        final Error error = new OutOfMemoryError();
        testable.setThrowable(error);
        e = assertThrows(OutOfMemoryError.class, () -> Functions.accept(testable::testObjLong, "X", 1L));
        assertSame(error, e);
        assertNull(testable.getAcceptedObject());
        assertNull(testable.getAcceptedPrimitiveObject());

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Functions.accept(testable::testObjLong, "X", 1L));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);
        assertNull(testable.getAcceptedObject());
        assertNull(testable.getAcceptedPrimitiveObject());

        testable.setThrowable(null);
        Functions.accept(testable::testObjLong, "X", 1L);
        assertEquals("X", testable.getAcceptedObject());
        assertEquals(1L, testable.getAcceptedPrimitiveObject());
    }

    @Test
    public void testApplyBiFunction() {
        final IllegalStateException ise = new IllegalStateException();
        final Testable<?, ?> testable = new Testable(null);
        Throwable e = assertThrows(IllegalStateException.class, () -> Functions.apply(Testable::testInteger, testable, ise));
        assertSame(ise, e);

        final Error error = new OutOfMemoryError();
        e = assertThrows(OutOfMemoryError.class, () -> Functions.apply(Testable::testInteger, testable, error));
        assertSame(error, e);

        final IOException ioe = new IOException("Unknown I/O error");
        e = assertThrows(UncheckedIOException.class, () -> Functions.apply(Testable::testInteger, testable, ioe));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        final Integer i = Functions.apply(Testable::testInteger, testable, (Throwable) null);
        assertNotNull(i);
        assertEquals(0, i.intValue());
    }

    @Test
    public void testApplyFunction() {
        final IllegalStateException ise = new IllegalStateException();
        final Testable<?, ?> testable = new Testable<>(ise);
        Throwable e = assertThrows(IllegalStateException.class, () -> Functions.apply(Testable::testInteger, testable));
        assertSame(ise, e);

        final Error error = new OutOfMemoryError();
        testable.setThrowable(error);
        e = assertThrows(OutOfMemoryError.class, () -> Functions.apply(Testable::testInteger, testable));
        assertSame(error, e);

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Functions.apply(Testable::testInteger, testable));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        testable.setThrowable(null);
        final Integer i = Functions.apply(Testable::testInteger, testable);
        assertNotNull(i);
        assertEquals(0, i.intValue());
    }

    @Test
    void testAsBiConsumer() {
        final IllegalStateException ise = new IllegalStateException();
        final Testable<?, ?> testable = new Testable(null);
        final FailableBiConsumer<Testable, Throwable, Throwable> failableBiConsumer = (t, th) -> {
            t.setThrowable(th); t.test();
        };
        final BiConsumer<Testable, Throwable> consumer = Functions.asBiConsumer(failableBiConsumer);
        Throwable e = assertThrows(IllegalStateException.class, () -> consumer.accept(testable, ise));
        assertSame(ise, e);

        final Error error = new OutOfMemoryError();
        e = assertThrows(OutOfMemoryError.class, () -> consumer.accept(testable, error));
        assertSame(error, e);

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> consumer.accept(testable,  ioe));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        consumer.accept(testable, null);
    }

    @Test
    public void testAsBiFunction() {
        final IllegalStateException ise = new IllegalStateException();
        final Testable<?, ?> testable = new Testable<>(ise);
        final FailableBiFunction<Testable, Throwable, Integer, Throwable> failableBiFunction = (t, th) -> {
            t.setThrowable(th);
            return Integer.valueOf(t.testInteger());
        };
        final BiFunction<Testable, Throwable, Integer> biFunction = Functions.asBiFunction(failableBiFunction);
        Throwable e = assertThrows(IllegalStateException.class, () -> biFunction.apply(testable, ise));
        assertSame(ise, e);

        final Error error = new OutOfMemoryError();
        testable.setThrowable(error);
        e = assertThrows(OutOfMemoryError.class, () -> biFunction.apply(testable, error));
        assertSame(error, e);

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> biFunction.apply(testable, ioe));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        assertEquals(0, biFunction.apply(testable, null).intValue());
    }

    @Test
    @DisplayName("Test that asPredicate(FailableBiPredicate) is converted to -> BiPredicate ")
    public void testAsBiPredicate() {
        FailureOnOddInvocations.invocations = 0;
        final Functions.FailableBiPredicate<Object, Object, Throwable> failableBiPredicate = (t1, t2) -> FailureOnOddInvocations.failingBool();
        final BiPredicate<?, ?> predicate = Functions.asBiPredicate(failableBiPredicate);
        final UndeclaredThrowableException e = assertThrows(UndeclaredThrowableException.class, () -> predicate.test(null, null));
        final Throwable cause = e.getCause();
        assertNotNull(cause);
        assertTrue(cause instanceof SomeException);
        assertEquals("Odd Invocation: 1", cause.getMessage());
        final boolean instance = predicate.test(null, null);
        assertNotNull(instance);
    }

    @Test
    void testAsCallable() {
        FailureOnOddInvocations.invocations = 0;
        final FailableCallable<FailureOnOddInvocations, SomeException> failableCallable = FailureOnOddInvocations::new;
        final Callable<FailureOnOddInvocations> callable = Functions.asCallable(failableCallable);
        final UndeclaredThrowableException e = assertThrows(UndeclaredThrowableException.class, callable::call);
        final Throwable cause = e.getCause();
        assertNotNull(cause);
        assertTrue(cause instanceof SomeException);
        assertEquals("Odd Invocation: 1", cause.getMessage());
        final FailureOnOddInvocations instance;
        try {
            instance = callable.call();
        } catch (final Exception ex) {
            throw Functions.rethrow(ex);
        }
        assertNotNull(instance);
    }

    @Test
    void testAsConsumer() {
        final IllegalStateException ise = new IllegalStateException();
        final Testable<?, ?> testable = new Testable<>(ise);
        final Consumer<Testable> consumer = Functions.asConsumer(Testable::test);
        Throwable e = assertThrows(IllegalStateException.class, () -> consumer.accept(testable));
        assertSame(ise, e);

        final Error error = new OutOfMemoryError();
        testable.setThrowable(error);
        e = assertThrows(OutOfMemoryError.class, () -> consumer.accept(testable));
        assertSame(error, e);

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> consumer.accept(testable));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        testable.setThrowable(null);
        Functions.accept(Testable::test, testable);
    }

    @Test
    public void testAsFunction() {
        final IllegalStateException ise = new IllegalStateException();
        final Testable<?, ?> testable = new Testable<>(ise);
        final FailableFunction<Throwable, Integer, Throwable> failableFunction = th -> {
            testable.setThrowable(th);
            return Integer.valueOf(testable.testInteger());
        };
        final Function<Throwable, Integer> function = Functions.asFunction(failableFunction);
        Throwable e = assertThrows(IllegalStateException.class, () -> function.apply(ise));
        assertSame(ise, e);

        final Error error = new OutOfMemoryError();
        testable.setThrowable(error);
        e = assertThrows(OutOfMemoryError.class, () -> function.apply(error));
        assertSame(error, e);

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> function.apply(ioe));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        assertEquals(0, function.apply(null).intValue());
    }

    @Test
    @DisplayName("Test that asPredicate(FailablePredicate) is converted to -> Predicate ")
    public void testAsPredicate() {
        FailureOnOddInvocations.invocations = 0;
        final Functions.FailablePredicate<Object, Throwable> failablePredicate = t -> FailureOnOddInvocations.failingBool();
        final Predicate<?> predicate = Functions.asPredicate(failablePredicate);
        final UndeclaredThrowableException e = assertThrows(UndeclaredThrowableException.class, () -> predicate.test(null));
        final Throwable cause = e.getCause();
        assertNotNull(cause);
        assertTrue(cause instanceof SomeException);
        assertEquals("Odd Invocation: 1", cause.getMessage());
        final boolean instance = predicate.test(null);
        assertNotNull(instance);
    }

    @Test
    void testAsRunnable() {
        FailureOnOddInvocations.invocations = 0;
        final Runnable runnable = Functions.asRunnable(FailureOnOddInvocations::new);
        final UndeclaredThrowableException e = assertThrows(UndeclaredThrowableException.class, runnable::run);
        final Throwable cause = e.getCause();
        assertNotNull(cause);
        assertTrue(cause instanceof SomeException);
        assertEquals("Odd Invocation: 1", cause.getMessage());

        // Even invocations, should not throw an exception
        runnable.run();
    }

    @Test
    public void testAsSupplier() {
        FailureOnOddInvocations.invocations = 0;
        final FailableSupplier<FailureOnOddInvocations, Throwable> failableSupplier = FailureOnOddInvocations::new;
        final Supplier<FailureOnOddInvocations> supplier = Functions.asSupplier(failableSupplier);
        final UndeclaredThrowableException e = assertThrows(UndeclaredThrowableException.class, supplier::get);
        final Throwable cause = e.getCause();
        assertNotNull(cause);
        assertTrue(cause instanceof SomeException);
        assertEquals("Odd Invocation: 1", cause.getMessage());
        assertNotNull(supplier.get());
    }

    @Test
    void testCallable() {
        FailureOnOddInvocations.invocations = 0;
        final UndeclaredThrowableException e = assertThrows(UndeclaredThrowableException.class, () ->  Functions.run(FailureOnOddInvocations::new));
        final Throwable cause = e.getCause();
        assertNotNull(cause);
        assertTrue(cause instanceof SomeException);
        assertEquals("Odd Invocation: 1", cause.getMessage());
        final FailureOnOddInvocations instance = Functions.call(FailureOnOddInvocations::new);
        assertNotNull(instance);
    }

    @Test
    public void testGetAsBooleanSupplier() {
        final IllegalStateException ise = new IllegalStateException();
        final Testable<?, ?> testable = new Testable<>(ise);
        Throwable e = assertThrows(IllegalStateException.class, () -> Functions.getAsBoolean(testable::testBooleanPrimitive));
        assertSame(ise, e);

        final Error error = new OutOfMemoryError();
        testable.setThrowable(error);
        e = assertThrows(OutOfMemoryError.class, () -> Functions.getAsBoolean(testable::testBooleanPrimitive));
        assertSame(error, e);

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Functions.getAsBoolean(testable::testBooleanPrimitive));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        testable.setThrowable(null);
        assertFalse(Functions.getAsBoolean(testable::testBooleanPrimitive));
    }

    @Test
    public void testGetAsDoubleSupplier() {
        final IllegalStateException ise = new IllegalStateException();
        final Testable<?, ?> testable = new Testable<>(ise);
        Throwable e = assertThrows(IllegalStateException.class, () -> Functions.getAsDouble(testable::testDoublePrimitive));
        assertSame(ise, e);

        final Error error = new OutOfMemoryError();
        testable.setThrowable(error);
        e = assertThrows(OutOfMemoryError.class, () -> Functions.getAsDouble(testable::testDoublePrimitive));
        assertSame(error, e);

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Functions.getAsDouble(testable::testDoublePrimitive));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        testable.setThrowable(null);
        assertEquals(0, Functions.getAsDouble(testable::testDoublePrimitive));
    }

    @Test
    public void testGetAsIntSupplier() {
        final IllegalStateException ise = new IllegalStateException();
        final Testable<?, ?> testable = new Testable<>(ise);
        Throwable e = assertThrows(IllegalStateException.class, () -> Functions.getAsInt(testable::testIntPrimitive));
        assertSame(ise, e);

        final Error error = new OutOfMemoryError();
        testable.setThrowable(error);
        e = assertThrows(OutOfMemoryError.class, () -> Functions.getAsInt(testable::testIntPrimitive));
        assertSame(error, e);

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Functions.getAsInt(testable::testIntPrimitive));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        testable.setThrowable(null);
        final int i = Functions.getAsInt(testable::testInteger);
        assertEquals(0, i);
    }

    @Test
    public void testGetAsLongSupplier() {
        final IllegalStateException ise = new IllegalStateException();
        final Testable<?, ?> testable = new Testable<>(ise);
        Throwable e = assertThrows(IllegalStateException.class, () -> Functions.getAsLong(testable::testLongPrimitive));
        assertSame(ise, e);

        final Error error = new OutOfMemoryError();
        testable.setThrowable(error);
        e = assertThrows(OutOfMemoryError.class, () -> Functions.getAsLong(testable::testLongPrimitive));
        assertSame(error, e);

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Functions.getAsLong(testable::testLongPrimitive));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        testable.setThrowable(null);
        final long i = Functions.getAsLong(testable::testLongPrimitive);
        assertEquals(0, i);
    }

    @Test
    public void testGetFromSupplier() {
        FailureOnOddInvocations.invocations = 0;
        final UndeclaredThrowableException e = assertThrows(UndeclaredThrowableException.class, () ->  Functions.run(FailureOnOddInvocations::new));
        final Throwable cause = e.getCause();
        assertNotNull(cause);
        assertTrue(cause instanceof SomeException);
        assertEquals("Odd Invocation: 1", cause.getMessage());
        final FailureOnOddInvocations instance = Functions.call(FailureOnOddInvocations::new);
        assertNotNull(instance);
    }

    @Test
    public void testGetSupplier() {
        final IllegalStateException ise = new IllegalStateException();
        final Testable<?, ?> testable = new Testable<>(ise);
        Throwable e = assertThrows(IllegalStateException.class, () -> Functions.get(testable::testInteger));
        assertSame(ise, e);

        final Error error = new OutOfMemoryError();
        testable.setThrowable(error);
        e = assertThrows(OutOfMemoryError.class, () -> Functions.get(testable::testInteger));
        assertSame(error, e);

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Functions.get(testable::testInteger));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        testable.setThrowable(null);
        final Integer i = Functions.apply(Testable::testInteger, testable);
        assertNotNull(i);
        assertEquals(0, i.intValue());
    }

    @Test
    void testRunnable() {
        FailureOnOddInvocations.invocations = 0;
        final UndeclaredThrowableException e = assertThrows(UndeclaredThrowableException.class, () ->  Functions.run(FailureOnOddInvocations::new));
        final Throwable cause = e.getCause();
        assertNotNull(cause);
        assertTrue(cause instanceof SomeException);
        assertEquals("Odd Invocation: 1", cause.getMessage());

        // Even invocations, should not throw an exception
        Functions.run(FailureOnOddInvocations::new);
    }

    @Test
    public void testTryWithResources() {
        final CloseableObject co = new CloseableObject();
        final FailableConsumer<Throwable, ? extends Throwable> consumer = co::run;
        final IllegalStateException ise = new IllegalStateException();
        Throwable e = assertThrows(IllegalStateException.class, () -> Functions.tryWithResources(() -> consumer.accept(ise), co::close));
        assertSame(ise, e);

        assertTrue(co.isClosed());
        co.reset();
        final Error error = new OutOfMemoryError();
        e = assertThrows(OutOfMemoryError.class, () -> Functions.tryWithResources(() -> consumer.accept(error), co::close));
        assertSame(error, e);

        assertTrue(co.isClosed());
        co.reset();
        final IOException ioe = new IOException("Unknown I/O error");
        final UncheckedIOException uioe = assertThrows(UncheckedIOException.class, () ->  Functions.tryWithResources(() -> consumer.accept(ioe), co::close));
        final IOException cause = uioe.getCause();
        assertSame(ioe, cause);

        assertTrue(co.isClosed());
        co.reset();
        Functions.tryWithResources(() -> consumer.accept(null), co::close);
        assertTrue(co.isClosed());
    }
}
