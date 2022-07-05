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

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/**
 * Tests "failable" interfaces defined in this package.
 */
public class FailableFunctionsTest extends AbstractLangTest {

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

        static boolean testDouble(final double value) throws SomeException {
            throwOnOdd();
            return true;
        }

        static boolean testInt(final int value) throws SomeException {
            throwOnOdd();
            return true;
        }

        static boolean testLong(final long value) throws SomeException {
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
        private P acceptedPrimitiveObject1;
        private P acceptedPrimitiveObject2;
        private Throwable throwable;

        Testable(final Throwable throwable) {
            this.throwable = throwable;
        }

        public T getAcceptedObject() {
            return acceptedObject;
        }

        public P getAcceptedPrimitiveObject1() {
            return acceptedPrimitiveObject1;
        }

        public P getAcceptedPrimitiveObject2() {
            return acceptedPrimitiveObject2;
        }

        public void setThrowable(final Throwable throwable) {
            this.throwable = throwable;
        }

        public void test() throws Throwable {
            test(throwable);
        }

        public Object test(final Object input1, final Object input2) throws Throwable {
            test(throwable);
            return acceptedObject;
        }

        public void test(final Throwable throwable) throws Throwable {
            if (throwable != null) {
                throw throwable;
            }
        }

        public boolean testAsBooleanPrimitive() throws Throwable {
            return testAsBooleanPrimitive(throwable);
        }

        public boolean testAsBooleanPrimitive(final Throwable throwable) throws Throwable {
            if (throwable != null) {
                throw throwable;
            }
            return false;
        }

        public double testAsDoublePrimitive() throws Throwable {
            return testAsDoublePrimitive(throwable);
        }

        public double testAsDoublePrimitive(final Throwable throwable) throws Throwable {
            if (throwable != null) {
                throw throwable;
            }
            return 0;
        }

        public Integer testAsInteger() throws Throwable {
            return testAsInteger(throwable);
        }

        public Integer testAsInteger(final Throwable throwable) throws Throwable {
            if (throwable != null) {
                throw throwable;
            }
            return 0;
        }

        public int testAsIntPrimitive() throws Throwable {
            return testAsIntPrimitive(throwable);
        }

        public int testAsIntPrimitive(final Throwable throwable) throws Throwable {
            if (throwable != null) {
                throw throwable;
            }
            return 0;
        }

        public long testAsLongPrimitive() throws Throwable {
            return testAsLongPrimitive(throwable);
        }

        public long testAsLongPrimitive(final Throwable throwable) throws Throwable {
            if (throwable != null) {
                throw throwable;
            }
            return 0;
        }

        public short testAsShortPrimitive() throws Throwable {
            return testAsShortPrimitive(throwable);
        }

        public short testAsShortPrimitive(final Throwable throwable) throws Throwable {
            if (throwable != null) {
                throw throwable;
            }
            return 0;
        }

        public void testDouble(final double i) throws Throwable {
            test(throwable);
            acceptedPrimitiveObject1 = (P) (Double) i;
        }

        public double testDoubleDouble(final double i, final double j) throws Throwable {
            test(throwable);
            acceptedPrimitiveObject1 = (P) (Double) i;
            acceptedPrimitiveObject2 = (P) (Double) j;
            return 3d;
        }

        public void testInt(final int i) throws Throwable {
            test(throwable);
            acceptedPrimitiveObject1 = (P) (Integer) i;
        }

        public void testLong(final long i) throws Throwable {
            test(throwable);
            acceptedPrimitiveObject1 = (P) (Long) i;
        }

        public void testObjDouble(final T object, final double i) throws Throwable {
            test(throwable);
            acceptedObject = object;
            acceptedPrimitiveObject1 = (P) (Double) i;
        }

        public void testObjInt(final T object, final int i) throws Throwable {
            test(throwable);
            acceptedObject = object;
            acceptedPrimitiveObject1 = (P) (Integer) i;
        }

        public void testObjLong(final T object, final long i) throws Throwable {
            test(throwable);
            acceptedObject = object;
            acceptedPrimitiveObject1 = (P) (Long) i;
        }
    }

    private static final OutOfMemoryError ERROR = new OutOfMemoryError();

    private static final IllegalStateException ILLEGAL_STATE_EXCEPTION = new IllegalStateException();

    @Test
    public void testAcceptBiConsumer() {
        final Testable<?, ?> testable = new Testable<>(null);
        Throwable e = assertThrows(IllegalStateException.class,
            () -> Failable.accept(Testable::test, testable, ILLEGAL_STATE_EXCEPTION));
        assertSame(ILLEGAL_STATE_EXCEPTION, e);

        e = assertThrows(OutOfMemoryError.class, () -> Failable.accept(Testable::test, testable, ERROR));
        assertSame(ERROR, e);

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Failable.accept(Testable::test, testable, ioe));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        testable.setThrowable(null);
        Failable.accept(Testable::test, testable, (Throwable) null);
    }

    @Test
    public void testAcceptConsumer() {
        final Testable<?, ?> testable = new Testable<>(ILLEGAL_STATE_EXCEPTION);
        Throwable e = assertThrows(IllegalStateException.class, () -> Failable.accept(Testable::test, testable));
        assertSame(ILLEGAL_STATE_EXCEPTION, e);

        testable.setThrowable(ERROR);
        e = assertThrows(OutOfMemoryError.class, () -> Failable.accept(Testable::test, testable));
        assertSame(ERROR, e);

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Failable.accept(Testable::test, testable));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        testable.setThrowable(null);
        Failable.accept(Testable::test, testable);
    }

    @Test
    public void testAcceptDoubleConsumer() {
        final Testable<?, Double> testable = new Testable<>(ILLEGAL_STATE_EXCEPTION);
        Throwable e = assertThrows(IllegalStateException.class, () -> Failable.accept(testable::testDouble, 1d));
        assertSame(ILLEGAL_STATE_EXCEPTION, e);
        assertNull(testable.getAcceptedPrimitiveObject1());

        testable.setThrowable(ERROR);
        e = assertThrows(OutOfMemoryError.class, () -> Failable.accept(testable::testDouble, 1d));
        assertSame(ERROR, e);
        assertNull(testable.getAcceptedPrimitiveObject1());

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Failable.accept(testable::testDouble, 1d));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);
        assertNull(testable.getAcceptedPrimitiveObject1());

        testable.setThrowable(null);
        Failable.accept(testable::testDouble, 1d);
        assertEquals(1, testable.getAcceptedPrimitiveObject1());
    }

    @Test
    public void testAcceptIntConsumer() {
        final Testable<?, Integer> testable = new Testable<>(ILLEGAL_STATE_EXCEPTION);
        Throwable e = assertThrows(IllegalStateException.class, () -> Failable.accept(testable::testInt, 1));
        assertSame(ILLEGAL_STATE_EXCEPTION, e);
        assertNull(testable.getAcceptedPrimitiveObject1());

        testable.setThrowable(ERROR);
        e = assertThrows(OutOfMemoryError.class, () -> Failable.accept(testable::testInt, 1));
        assertSame(ERROR, e);
        assertNull(testable.getAcceptedPrimitiveObject1());

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Failable.accept(testable::testInt, 1));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);
        assertNull(testable.getAcceptedPrimitiveObject1());

        testable.setThrowable(null);
        Failable.accept(testable::testInt, 1);
        assertEquals(1, testable.getAcceptedPrimitiveObject1());
    }

    @Test
    public void testAcceptLongConsumer() {
        final Testable<?, Long> testable = new Testable<>(ILLEGAL_STATE_EXCEPTION);
        Throwable e = assertThrows(IllegalStateException.class, () -> Failable.accept(testable::testLong, 1L));
        assertSame(ILLEGAL_STATE_EXCEPTION, e);
        assertNull(testable.getAcceptedPrimitiveObject1());

        testable.setThrowable(ERROR);
        e = assertThrows(OutOfMemoryError.class, () -> Failable.accept(testable::testLong, 1L));
        assertSame(ERROR, e);
        assertNull(testable.getAcceptedPrimitiveObject1());

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Failable.accept(testable::testLong, 1L));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);
        assertNull(testable.getAcceptedPrimitiveObject1());

        testable.setThrowable(null);
        Failable.accept(testable::testLong, 1L);
        assertEquals(1, testable.getAcceptedPrimitiveObject1());
    }

    @Test
    public void testAcceptObjDoubleConsumer() {
        final Testable<String, Double> testable = new Testable<>(ILLEGAL_STATE_EXCEPTION);
        Throwable e = assertThrows(IllegalStateException.class,
            () -> Failable.accept(testable::testObjDouble, "X", 1d));
        assertSame(ILLEGAL_STATE_EXCEPTION, e);
        assertNull(testable.getAcceptedObject());
        assertNull(testable.getAcceptedPrimitiveObject1());

        testable.setThrowable(ERROR);
        e = assertThrows(OutOfMemoryError.class, () -> Failable.accept(testable::testObjDouble, "X", 1d));
        assertSame(ERROR, e);
        assertNull(testable.getAcceptedObject());
        assertNull(testable.getAcceptedPrimitiveObject1());

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Failable.accept(testable::testObjDouble, "X", 1d));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);
        assertNull(testable.getAcceptedObject());
        assertNull(testable.getAcceptedPrimitiveObject1());

        testable.setThrowable(null);
        Failable.accept(testable::testObjDouble, "X", 1d);
        assertEquals("X", testable.getAcceptedObject());
        assertEquals(1d, testable.getAcceptedPrimitiveObject1());
    }

    @Test
    public void testAcceptObjIntConsumer() {
        final Testable<String, Integer> testable = new Testable<>(ILLEGAL_STATE_EXCEPTION);
        Throwable e = assertThrows(IllegalStateException.class, () -> Failable.accept(testable::testObjInt, "X", 1));
        assertSame(ILLEGAL_STATE_EXCEPTION, e);
        assertNull(testable.getAcceptedObject());
        assertNull(testable.getAcceptedPrimitiveObject1());

        testable.setThrowable(ERROR);
        e = assertThrows(OutOfMemoryError.class, () -> Failable.accept(testable::testObjInt, "X", 1));
        assertSame(ERROR, e);
        assertNull(testable.getAcceptedObject());
        assertNull(testable.getAcceptedPrimitiveObject1());

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Failable.accept(testable::testObjInt, "X", 1));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);
        assertNull(testable.getAcceptedObject());
        assertNull(testable.getAcceptedPrimitiveObject1());

        testable.setThrowable(null);
        Failable.accept(testable::testObjInt, "X", 1);
        assertEquals("X", testable.getAcceptedObject());
        assertEquals(1, testable.getAcceptedPrimitiveObject1());
    }

    @Test
    public void testAcceptObjLongConsumer() {
        final Testable<String, Long> testable = new Testable<>(ILLEGAL_STATE_EXCEPTION);
        Throwable e = assertThrows(IllegalStateException.class, () -> Failable.accept(testable::testObjLong, "X", 1L));
        assertSame(ILLEGAL_STATE_EXCEPTION, e);
        assertNull(testable.getAcceptedObject());
        assertNull(testable.getAcceptedPrimitiveObject1());

        testable.setThrowable(ERROR);
        e = assertThrows(OutOfMemoryError.class, () -> Failable.accept(testable::testObjLong, "X", 1L));
        assertSame(ERROR, e);
        assertNull(testable.getAcceptedObject());
        assertNull(testable.getAcceptedPrimitiveObject1());

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Failable.accept(testable::testObjLong, "X", 1L));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);
        assertNull(testable.getAcceptedObject());
        assertNull(testable.getAcceptedPrimitiveObject1());

        testable.setThrowable(null);
        Failable.accept(testable::testObjLong, "X", 1L);
        assertEquals("X", testable.getAcceptedObject());
        assertEquals(1L, testable.getAcceptedPrimitiveObject1());
    }

    @Test
    public void testApplyBiFunction() {
        final Testable<?, ?> testable = new Testable<>(null);
        Throwable e = assertThrows(IllegalStateException.class,
            () -> Failable.apply(Testable::testAsInteger, testable, ILLEGAL_STATE_EXCEPTION));
        assertSame(ILLEGAL_STATE_EXCEPTION, e);

        e = assertThrows(OutOfMemoryError.class, () -> Failable.apply(Testable::testAsInteger, testable, ERROR));
        assertSame(ERROR, e);

        final IOException ioe = new IOException("Unknown I/O error");
        e = assertThrows(UncheckedIOException.class, () -> Failable.apply(Testable::testAsInteger, testable, ioe));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        final Integer i = Failable.apply(Testable::testAsInteger, testable, (Throwable) null);
        assertNotNull(i);
        assertEquals(0, i.intValue());
    }

    @Test
    public void testApplyDoubleBinaryOperator() {
        final Testable<?, Double> testable = new Testable<>(ILLEGAL_STATE_EXCEPTION);
        final Throwable e = assertThrows(IllegalStateException.class,
            () -> Failable.applyAsDouble(testable::testDoubleDouble, 1d, 2d));
        assertSame(ILLEGAL_STATE_EXCEPTION, e);

        final Testable<?, Double> testable2 = new Testable<>(null);
        final double i = Failable.applyAsDouble(testable2::testDoubleDouble, 1d, 2d);
        assertEquals(3d, i);
    }

    @Test
    public void testApplyFunction() {
        final Testable<?, ?> testable = new Testable<>(ILLEGAL_STATE_EXCEPTION);
        Throwable e = assertThrows(IllegalStateException.class,
            () -> Failable.apply(Testable::testAsInteger, testable));
        assertSame(ILLEGAL_STATE_EXCEPTION, e);

        testable.setThrowable(ERROR);
        e = assertThrows(OutOfMemoryError.class, () -> Failable.apply(Testable::testAsInteger, testable));
        assertSame(ERROR, e);

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Failable.apply(Testable::testAsInteger, testable));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        testable.setThrowable(null);
        final Integer i = Failable.apply(Testable::testAsInteger, testable);
        assertNotNull(i);
        assertEquals(0, i.intValue());
    }

    @Test
    public void testAsCallable() {
        FailureOnOddInvocations.invocations = 0;
        final FailableCallable<FailureOnOddInvocations, SomeException> failableCallable = FailureOnOddInvocations::new;
        final Callable<FailureOnOddInvocations> callable = Failable.asCallable(failableCallable);
        final UndeclaredThrowableException e = assertThrows(UndeclaredThrowableException.class, callable::call);
        final Throwable cause = e.getCause();
        assertNotNull(cause);
        assertTrue(cause instanceof SomeException);
        assertEquals("Odd Invocation: 1", cause.getMessage());
        final FailureOnOddInvocations instance;
        try {
            instance = callable.call();
        } catch (final Exception ex) {
            throw Failable.rethrow(ex);
        }
        assertNotNull(instance);
    }

    @Test
    public void testAsConsumer() {
        final Testable<?, ?> testable = new Testable<>(ILLEGAL_STATE_EXCEPTION);
        final Consumer<Testable<?, ?>> consumer = Failable.asConsumer(Testable::test);
        Throwable e = assertThrows(IllegalStateException.class, () -> consumer.accept(testable));
        assertSame(ILLEGAL_STATE_EXCEPTION, e);

        testable.setThrowable(ERROR);
        e = assertThrows(OutOfMemoryError.class, () -> consumer.accept(testable));
        assertSame(ERROR, e);

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> consumer.accept(testable));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        testable.setThrowable(null);
        Failable.accept(Testable::test, testable);
    }

    @Test
    public void testAsRunnable() {
        FailureOnOddInvocations.invocations = 0;
        final Runnable runnable = Failable.asRunnable(FailureOnOddInvocations::new);
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
        final Supplier<FailureOnOddInvocations> supplier = Failable.asSupplier(failableSupplier);
        final UndeclaredThrowableException e = assertThrows(UndeclaredThrowableException.class, supplier::get);
        final Throwable cause = e.getCause();
        assertNotNull(cause);
        assertTrue(cause instanceof SomeException);
        assertEquals("Odd Invocation: 1", cause.getMessage());
        assertNotNull(supplier.get());
    }

    @Test
    public void testBiConsumer() throws Throwable {
        final Testable<?, ?> testable = new Testable<>(null);
        final FailableBiConsumer<Testable<?, ?>, Throwable, Throwable> failableBiConsumer = (t, th) -> {
            t.setThrowable(th);
            t.test();
        };
        final BiConsumer<Testable<?, ?>, Throwable> consumer = Failable.asBiConsumer(failableBiConsumer);
        Throwable e = assertThrows(IllegalStateException.class,
            () -> consumer.accept(testable, ILLEGAL_STATE_EXCEPTION));
        assertSame(ILLEGAL_STATE_EXCEPTION, e);

        e = assertThrows(OutOfMemoryError.class, () -> consumer.accept(testable, ERROR));
        assertSame(ERROR, e);

        e = assertThrows(OutOfMemoryError.class, () -> failableBiConsumer.accept(testable, ERROR));
        assertSame(ERROR, e);

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> consumer.accept(testable, ioe));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        consumer.accept(testable, null);
    }

    @Test
    public void testBiConsumerAndThen() throws Throwable {
        final Testable<?, ?> testable = new Testable<>(null);
        final FailableBiConsumer<Testable<?, ?>, Throwable, Throwable> failing = (t, th) -> {
            t.setThrowable(th);
            t.test();
        };
        final FailableBiConsumer<Testable<?, ?>, Throwable, Throwable> nop = FailableBiConsumer.nop();
        Throwable e = assertThrows(OutOfMemoryError.class, () -> nop.andThen(failing).accept(testable, ERROR));
        assertSame(ERROR, e);
        e = assertThrows(OutOfMemoryError.class, () -> failing.andThen(nop).accept(testable, ERROR));
        assertSame(ERROR, e);
        // Does not throw
        nop.andThen(nop);
        // Documented in Javadoc edge-case.
        assertThrows(NullPointerException.class, () -> failing.andThen(null));
    }

    @Test
    public void testBiFunction() {
        final Testable<?, ?> testable = new Testable<>(ILLEGAL_STATE_EXCEPTION);
        final FailableBiFunction<Testable<?, ?>, Throwable, Integer, Throwable> failableBiFunction = (t, th) -> {
            t.setThrowable(th);
            return t.testAsInteger();
        };
        final BiFunction<Testable<?, ?>, Throwable, Integer> biFunction = Failable.asBiFunction(failableBiFunction);
        Throwable e = assertThrows(IllegalStateException.class,
            () -> biFunction.apply(testable, ILLEGAL_STATE_EXCEPTION));
        assertSame(ILLEGAL_STATE_EXCEPTION, e);

        testable.setThrowable(ERROR);
        e = assertThrows(OutOfMemoryError.class, () -> biFunction.apply(testable, ERROR));
        assertSame(ERROR, e);

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> biFunction.apply(testable, ioe));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        assertEquals(0, biFunction.apply(testable, null).intValue());
    }

    @Test
    public void testBiFunctionAndThen() throws IOException {
        // Unchecked usage pattern in JRE
        final BiFunction<Object, Integer, Integer> nopBiFunction = (t, u) -> null;
        final Function<Object, Integer> nopFunction = t -> null;
        nopBiFunction.andThen(nopFunction);
        // Checked usage pattern
        final FailableBiFunction<Object, Integer, Integer, IOException> failingBiFunctionTest = (t, u) -> {
            throw new IOException();
        };
        final FailableFunction<Object, Integer, IOException> failingFunction = t -> {
            throw new IOException();
        };
        final FailableBiFunction<Object, Integer, Integer, IOException> nopFailableBiFunction = FailableBiFunction
            .nop();
        final FailableFunction<Object, Integer, IOException> nopFailableFunction = FailableFunction.nop();
        //
        assertThrows(IOException.class, () -> failingBiFunctionTest.andThen(failingFunction).apply(null, null));
        assertThrows(IOException.class, () -> failingBiFunctionTest.andThen(nopFailableFunction).apply(null, null));
        //
        assertThrows(IOException.class, () -> nopFailableBiFunction.andThen(failingFunction).apply(null, null));
        nopFailableBiFunction.andThen(nopFailableFunction).apply(null, null);
        // Documented in Javadoc edge-case.
        assertThrows(NullPointerException.class, () -> failingBiFunctionTest.andThen(null));
    }

    @Test
    @DisplayName("Test that asPredicate(FailableBiPredicate) is converted to -> BiPredicate ")
    public void testBiPredicate() {
        FailureOnOddInvocations.invocations = 0;
        final FailableBiPredicate<Object, Object, Throwable> failableBiPredicate = (t1, t2) -> FailureOnOddInvocations
            .failingBool();
        final BiPredicate<?, ?> predicate = Failable.asBiPredicate(failableBiPredicate);
        final UndeclaredThrowableException e = assertThrows(UndeclaredThrowableException.class,
            () -> predicate.test(null, null));
        final Throwable cause = e.getCause();
        assertNotNull(cause);
        assertTrue(cause instanceof SomeException);
        assertEquals("Odd Invocation: 1", cause.getMessage());
        assertTrue(predicate.test(null, null));
    }

    @Test
    public void testBiPredicateAnd() throws Throwable {
        assertTrue(FailableBiPredicate.TRUE.and(FailableBiPredicate.TRUE).test(null, null));
        assertFalse(FailableBiPredicate.TRUE.and(FailableBiPredicate.FALSE).test(null, null));
        assertFalse(FailableBiPredicate.FALSE.and(FailableBiPredicate.TRUE).test(null, null));
        assertFalse(FailableBiPredicate.FALSE.and(FailableBiPredicate.FALSE).test(null, null));
        // null tests
        assertThrows(NullPointerException.class,
            () -> assertFalse(FailableBiPredicate.falsePredicate().and(null).test(null, null)));
        assertThrows(NullPointerException.class,
            () -> assertTrue(FailableBiPredicate.truePredicate().and(null).test(null, null)));
    }

    @Test
    public void testBiPredicateNegate() throws Throwable {
        assertFalse(FailableBiPredicate.TRUE.negate().test(null, null));
        assertFalse(FailableBiPredicate.truePredicate().negate().test(null, null));
        assertTrue(FailableBiPredicate.FALSE.negate().test(null, null));
        assertTrue(FailableBiPredicate.falsePredicate().negate().test(null, null));
    }

    @Test
    public void testBiPredicateOr() throws Throwable {
        assertTrue(FailableBiPredicate.TRUE.or(FailableBiPredicate.TRUE).test(null, null));
        assertTrue(FailableBiPredicate.TRUE.or(FailableBiPredicate.FALSE).test(null, null));
        assertTrue(FailableBiPredicate.FALSE.or(FailableBiPredicate.TRUE).test(null, null));
        assertFalse(FailableBiPredicate.FALSE.or(FailableBiPredicate.FALSE).test(null, null));
        // null tests
        assertThrows(NullPointerException.class,
            () -> assertFalse(FailableBiPredicate.falsePredicate().or(null).test(null, null)));
        assertThrows(NullPointerException.class,
            () -> assertTrue(FailableBiPredicate.truePredicate().or(null).test(null, null)));
    }

    @Test
    public void testCallable() {
        FailureOnOddInvocations.invocations = 0;
        final UndeclaredThrowableException e = assertThrows(UndeclaredThrowableException.class,
            () -> Failable.run(FailureOnOddInvocations::new));
        final Throwable cause = e.getCause();
        assertNotNull(cause);
        assertTrue(cause instanceof SomeException);
        assertEquals("Odd Invocation: 1", cause.getMessage());
        final FailureOnOddInvocations instance = Failable.call(FailureOnOddInvocations::new);
        assertNotNull(instance);
    }

    @Test
    public void testConsumerAndThen() throws Throwable {
        final Testable<?, ?> testable = new Testable<>(null);
        final FailableConsumer<Throwable, Throwable> failableConsumer = th -> {
            testable.setThrowable(th);
            testable.test();
        };
        final FailableConsumer<Throwable, Throwable> nop = FailableConsumer.nop();
        final Throwable e = assertThrows(OutOfMemoryError.class, () -> nop.andThen(failableConsumer).accept(ERROR));
        assertSame(ERROR, e);
        // Does not throw
        nop.andThen(nop);
        // Documented in Javadoc edge-case.
        assertThrows(NullPointerException.class, () -> failableConsumer.andThen(null));
    }

    @Test
    public void testDoubleConsumerAndThen() throws Throwable {
        final Testable<?, ?> testable = new Testable<>(null);
        final FailableDoubleConsumer<Throwable> failing = t -> {
            testable.setThrowable(ERROR);
            testable.test();
        };
        final FailableDoubleConsumer<Throwable> nop = FailableDoubleConsumer.nop();
        Throwable e = assertThrows(OutOfMemoryError.class, () -> nop.andThen(failing).accept(0d));
        assertSame(ERROR, e);
        e = assertThrows(OutOfMemoryError.class, () -> failing.andThen(nop).accept(0d));
        assertSame(ERROR, e);
        // Does not throw
        nop.andThen(nop);
        // Documented in Javadoc edge-case.
        assertThrows(NullPointerException.class, () -> failing.andThen(null));
    }

    @Test
    public void testDoublePredicate() throws Throwable {
        FailureOnOddInvocations.invocations = 0;
        final FailableDoublePredicate<Throwable> failablePredicate = FailureOnOddInvocations::testDouble;
        assertThrows(SomeException.class, () -> failablePredicate.test(1d));
        failablePredicate.test(1d);
    }

    @Test
    public void testDoublePredicateAnd() throws Throwable {
        assertTrue(FailableDoublePredicate.TRUE.and(FailableDoublePredicate.TRUE).test(0));
        assertFalse(FailableDoublePredicate.TRUE.and(FailableDoublePredicate.FALSE).test(0));
        assertFalse(FailableDoublePredicate.FALSE.and(FailableDoublePredicate.TRUE).test(0));
        assertFalse(FailableDoublePredicate.FALSE.and(FailableDoublePredicate.FALSE).test(0));
        // null tests
        assertThrows(NullPointerException.class,
            () -> assertFalse(FailableDoublePredicate.falsePredicate().and(null).test(0)));
        assertThrows(NullPointerException.class,
            () -> assertTrue(FailableDoublePredicate.truePredicate().and(null).test(0)));
    }

    @Test
    public void testDoublePredicateNegate() throws Throwable {
        assertFalse(FailableDoublePredicate.TRUE.negate().test(0d));
        assertFalse(FailableDoublePredicate.truePredicate().negate().test(0d));
        assertTrue(FailableDoublePredicate.FALSE.negate().test(0d));
        assertTrue(FailableDoublePredicate.falsePredicate().negate().test(0d));
    }

    @Test
    public void testDoublePredicateOr() throws Throwable {
        assertTrue(FailableDoublePredicate.TRUE.or(FailableDoublePredicate.TRUE).test(0));
        assertTrue(FailableDoublePredicate.TRUE.or(FailableDoublePredicate.FALSE).test(0));
        assertTrue(FailableDoublePredicate.FALSE.or(FailableDoublePredicate.TRUE).test(0));
        assertFalse(FailableDoublePredicate.FALSE.or(FailableDoublePredicate.FALSE).test(0));
        // null tests
        assertThrows(NullPointerException.class,
            () -> assertFalse(FailableDoublePredicate.falsePredicate().or(null).test(0)));
        assertThrows(NullPointerException.class,
            () -> assertTrue(FailableDoublePredicate.truePredicate().or(null).test(0)));
    }

    @Test
    public void testDoubleUnaryOperatorAndThen() throws Throwable {
        final Testable<?, ?> testable = new Testable<>(null);
        final FailableDoubleUnaryOperator<Throwable> failing = t -> {
            testable.setThrowable(ERROR);
            testable.test();
            return 0d;
        };
        final FailableDoubleUnaryOperator<Throwable> nop = FailableDoubleUnaryOperator.nop();
        Throwable e = assertThrows(OutOfMemoryError.class, () -> nop.andThen(failing).applyAsDouble(0d));
        assertSame(ERROR, e);
        e = assertThrows(OutOfMemoryError.class, () -> failing.andThen(nop).applyAsDouble(0d));
        assertSame(ERROR, e);
        // Does not throw
        nop.andThen(nop);
        // Documented in Javadoc edge-case.
        assertThrows(NullPointerException.class, () -> failing.andThen(null));
    }

    @Test
    public void testDoubleUnaryOperatorCompose() throws Throwable {
        final Testable<?, ?> testable = new Testable<>(null);
        final FailableDoubleUnaryOperator<Throwable> failing = t -> {
            testable.setThrowable(ERROR);
            testable.test();
            return 0d;
        };
        final FailableDoubleUnaryOperator<Throwable> nop = FailableDoubleUnaryOperator.nop();
        Throwable e = assertThrows(OutOfMemoryError.class, () -> nop.compose(failing).applyAsDouble(0d));
        assertSame(ERROR, e);
        e = assertThrows(OutOfMemoryError.class, () -> failing.compose(nop).applyAsDouble(0d));
        assertSame(ERROR, e);
        // Does not throw
        nop.compose(nop);
        // Documented in Javadoc edge-case.
        assertThrows(NullPointerException.class, () -> failing.compose(null));
    }

    @Test
    public void testDoubleUnaryOperatorIdentity() throws Throwable {
        final FailableDoubleUnaryOperator<Throwable> nop = FailableDoubleUnaryOperator.identity();
        // Does not throw
        nop.compose(nop);
        // Documented in Javadoc edge-case.
        assertThrows(NullPointerException.class, () -> nop.compose(null));
    }

    @Test
    public void testFunction() {
        final Testable<?, ?> testable = new Testable<>(ILLEGAL_STATE_EXCEPTION);
        final FailableFunction<Throwable, Integer, Throwable> failableFunction = th -> {
            testable.setThrowable(th);
            return testable.testAsInteger();
        };
        final Function<Throwable, Integer> function = Failable.asFunction(failableFunction);
        Throwable e = assertThrows(IllegalStateException.class, () -> function.apply(ILLEGAL_STATE_EXCEPTION));
        assertSame(ILLEGAL_STATE_EXCEPTION, e);

        testable.setThrowable(ERROR);
        e = assertThrows(OutOfMemoryError.class, () -> function.apply(ERROR));
        assertSame(ERROR, e);

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> function.apply(ioe));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        assertEquals(0, function.apply(null).intValue());
    }

    @Test
    public void testFunctionAndThen() throws IOException {
        // Unchecked usage pattern in JRE
        final Function<Object, Integer> nopFunction = t -> null;
        nopFunction.andThen(nopFunction);
        // Checked usage pattern
        final FailableFunction<Object, Integer, IOException> failingFunction = t -> {
            throw new IOException();
        };
        final FailableFunction<Object, Integer, IOException> nopFailableFunction = FailableFunction.nop();
        //
        assertThrows(IOException.class, () -> failingFunction.andThen(failingFunction).apply(null));
        assertThrows(IOException.class, () -> failingFunction.andThen(nopFailableFunction).apply(null));
        //
        assertThrows(IOException.class, () -> nopFailableFunction.andThen(failingFunction).apply(null));
        nopFailableFunction.andThen(nopFailableFunction).apply(null);
        // Documented in Javadoc edge-case.
        assertThrows(NullPointerException.class, () -> failingFunction.andThen(null));
    }

    @Test
    public void testFunctionCompose() throws Throwable {
        final Testable<?, ?> testable = new Testable<>(null);
        final FailableFunction<Object, Integer, Throwable> failing = t -> {
            testable.setThrowable(ERROR);
            testable.test();
            return 0;
        };
        final FailableFunction<Object, Integer, Throwable> nop = FailableFunction.nop();
        Throwable e = assertThrows(OutOfMemoryError.class, () -> nop.compose(failing).apply(0));
        assertSame(ERROR, e);
        e = assertThrows(OutOfMemoryError.class, () -> failing.compose(nop).apply(0));
        assertSame(ERROR, e);
        // Does not throw
        nop.compose(nop);
        // Documented in Javadoc edge-case.
        assertThrows(NullPointerException.class, () -> failing.compose(null));
    }

    @Test
    public void testFunctionIdentity() throws Throwable {
        final FailableFunction<Integer, Integer, Throwable> nop = FailableFunction.identity();
        // Does not throw
        nop.compose(nop);
        // Documented in Javadoc edge-case.
        assertThrows(NullPointerException.class, () -> nop.compose(null));
    }

    @Test
    public void testGetAsBooleanSupplier() {
        final Testable<?, ?> testable = new Testable<>(ILLEGAL_STATE_EXCEPTION);
        Throwable e = assertThrows(IllegalStateException.class,
            () -> Failable.getAsBoolean(testable::testAsBooleanPrimitive));
        assertSame(ILLEGAL_STATE_EXCEPTION, e);

        testable.setThrowable(ERROR);
        e = assertThrows(OutOfMemoryError.class, () -> Failable.getAsBoolean(testable::testAsBooleanPrimitive));
        assertSame(ERROR, e);

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Failable.getAsBoolean(testable::testAsBooleanPrimitive));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        testable.setThrowable(null);
        assertFalse(Failable.getAsBoolean(testable::testAsBooleanPrimitive));
    }

    @Test
    public void testGetAsDoubleSupplier() {
        final Testable<?, ?> testable = new Testable<>(ILLEGAL_STATE_EXCEPTION);
        Throwable e = assertThrows(IllegalStateException.class,
            () -> Failable.getAsDouble(testable::testAsDoublePrimitive));
        assertSame(ILLEGAL_STATE_EXCEPTION, e);

        testable.setThrowable(ERROR);
        e = assertThrows(OutOfMemoryError.class, () -> Failable.getAsDouble(testable::testAsDoublePrimitive));
        assertSame(ERROR, e);

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Failable.getAsDouble(testable::testAsDoublePrimitive));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        testable.setThrowable(null);
        assertEquals(0, Failable.getAsDouble(testable::testAsDoublePrimitive));
    }

    @Test
    public void testGetAsIntSupplier() {
        final Testable<?, ?> testable = new Testable<>(ILLEGAL_STATE_EXCEPTION);
        Throwable e = assertThrows(IllegalStateException.class, () -> Failable.getAsInt(testable::testAsIntPrimitive));
        assertSame(ILLEGAL_STATE_EXCEPTION, e);

        testable.setThrowable(ERROR);
        e = assertThrows(OutOfMemoryError.class, () -> Failable.getAsInt(testable::testAsIntPrimitive));
        assertSame(ERROR, e);

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Failable.getAsInt(testable::testAsIntPrimitive));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        testable.setThrowable(null);
        final int i = Failable.getAsInt(testable::testAsInteger);
        assertEquals(0, i);
    }

    @Test
    public void testGetAsLongSupplier() {
        final Testable<?, ?> testable = new Testable<>(ILLEGAL_STATE_EXCEPTION);
        Throwable e = assertThrows(IllegalStateException.class,
            () -> Failable.getAsLong(testable::testAsLongPrimitive));
        assertSame(ILLEGAL_STATE_EXCEPTION, e);

        testable.setThrowable(ERROR);
        e = assertThrows(OutOfMemoryError.class, () -> Failable.getAsLong(testable::testAsLongPrimitive));
        assertSame(ERROR, e);

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Failable.getAsLong(testable::testAsLongPrimitive));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        testable.setThrowable(null);
        final long i = Failable.getAsLong(testable::testAsLongPrimitive);
        assertEquals(0, i);
    }

    @Test
    public void testGetAsShortSupplier() {
        final Testable<?, ?> testable = new Testable<>(ILLEGAL_STATE_EXCEPTION);
        Throwable e = assertThrows(IllegalStateException.class,
            () -> Failable.getAsShort(testable::testAsShortPrimitive));
        assertSame(ILLEGAL_STATE_EXCEPTION, e);

        testable.setThrowable(ERROR);
        e = assertThrows(OutOfMemoryError.class, () -> Failable.getAsShort(testable::testAsShortPrimitive));
        assertSame(ERROR, e);

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Failable.getAsShort(testable::testAsShortPrimitive));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        testable.setThrowable(null);
        final short i = Failable.getAsShort(testable::testAsShortPrimitive);
        assertEquals(0, i);
    }

    @Test
    public void testGetFromSupplier() {
        FailureOnOddInvocations.invocations = 0;
        final UndeclaredThrowableException e = assertThrows(UndeclaredThrowableException.class,
            () -> Failable.run(FailureOnOddInvocations::new));
        final Throwable cause = e.getCause();
        assertNotNull(cause);
        assertTrue(cause instanceof SomeException);
        assertEquals("Odd Invocation: 1", cause.getMessage());
        final FailureOnOddInvocations instance = Failable.call(FailureOnOddInvocations::new);
        assertNotNull(instance);
    }

    @Test
    public void testGetSupplier() {
        final Testable<?, ?> testable = new Testable<>(ILLEGAL_STATE_EXCEPTION);
        Throwable e = assertThrows(IllegalStateException.class, () -> Failable.get(testable::testAsInteger));
        assertSame(ILLEGAL_STATE_EXCEPTION, e);

        testable.setThrowable(ERROR);
        e = assertThrows(OutOfMemoryError.class, () -> Failable.get(testable::testAsInteger));
        assertSame(ERROR, e);

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Failable.get(testable::testAsInteger));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        testable.setThrowable(null);
        final Integer i = Failable.apply(Testable::testAsInteger, testable);
        assertNotNull(i);
        assertEquals(0, i.intValue());
    }

    @Test
    public void testIntConsumerAndThen() throws Throwable {
        final Testable<?, ?> testable = new Testable<>(null);
        final FailableIntConsumer<Throwable> failing = t -> {
            testable.setThrowable(ERROR);
            testable.test();
        };
        final FailableIntConsumer<Throwable> nop = FailableIntConsumer.nop();
        Throwable e = assertThrows(OutOfMemoryError.class, () -> nop.andThen(failing).accept(0));
        assertSame(ERROR, e);
        e = assertThrows(OutOfMemoryError.class, () -> failing.andThen(nop).accept(0));
        assertSame(ERROR, e);
        // Does not throw
        nop.andThen(nop);
        // Documented in Javadoc edge-case.
        assertThrows(NullPointerException.class, () -> failing.andThen(null));
    }

    @Test
    public void testIntPredicate() throws Throwable {
        FailureOnOddInvocations.invocations = 0;
        final FailableIntPredicate<Throwable> failablePredicate = FailureOnOddInvocations::testInt;
        assertThrows(SomeException.class, () -> failablePredicate.test(1));
        failablePredicate.test(1);
    }

    @Test
    public void testIntPredicateAnd() throws Throwable {
        assertTrue(FailableIntPredicate.TRUE.and(FailableIntPredicate.TRUE).test(0));
        assertFalse(FailableIntPredicate.TRUE.and(FailableIntPredicate.FALSE).test(0));
        assertFalse(FailableIntPredicate.FALSE.and(FailableIntPredicate.TRUE).test(0));
        assertFalse(FailableIntPredicate.FALSE.and(FailableIntPredicate.FALSE).test(0));
        // null tests
        assertThrows(NullPointerException.class,
            () -> assertFalse(FailableIntPredicate.falsePredicate().and(null).test(0)));
        assertThrows(NullPointerException.class,
            () -> assertTrue(FailableIntPredicate.truePredicate().and(null).test(0)));
    }

    @Test
    public void testIntPredicateNegate() throws Throwable {
        assertFalse(FailableIntPredicate.TRUE.negate().test(0));
        assertFalse(FailableIntPredicate.truePredicate().negate().test(0));
        assertTrue(FailableIntPredicate.FALSE.negate().test(0));
        assertTrue(FailableIntPredicate.falsePredicate().negate().test(0));
    }

    @Test
    public void testIntPredicateOr() throws Throwable {
        assertTrue(FailableIntPredicate.TRUE.or(FailableIntPredicate.TRUE).test(0));
        assertTrue(FailableIntPredicate.TRUE.or(FailableIntPredicate.FALSE).test(0));
        assertTrue(FailableIntPredicate.FALSE.or(FailableIntPredicate.TRUE).test(0));
        assertFalse(FailableIntPredicate.FALSE.or(FailableIntPredicate.FALSE).test(0));
        // null tests
        assertThrows(NullPointerException.class,
            () -> assertFalse(FailableIntPredicate.falsePredicate().or(null).test(0)));
        assertThrows(NullPointerException.class,
            () -> assertTrue(FailableIntPredicate.truePredicate().or(null).test(0)));
    }

    @Test
    public void testIntUnaryOperatorAndThen() throws Throwable {
        final Testable<?, ?> testable = new Testable<>(null);
        final FailableIntUnaryOperator<Throwable> failing = t -> {
            testable.setThrowable(ERROR);
            testable.test();
            return 0;
        };
        final FailableIntUnaryOperator<Throwable> nop = FailableIntUnaryOperator.nop();
        Throwable e = assertThrows(OutOfMemoryError.class, () -> nop.andThen(failing).applyAsInt(0));
        assertSame(ERROR, e);
        e = assertThrows(OutOfMemoryError.class, () -> failing.andThen(nop).applyAsInt(0));
        assertSame(ERROR, e);
        // Does not throw
        nop.andThen(nop);
        // Documented in Javadoc edge-case.
        assertThrows(NullPointerException.class, () -> failing.andThen(null));
    }

    @Test
    public void testIntUnaryOperatorCompose() throws Throwable {
        final Testable<?, ?> testable = new Testable<>(null);
        final FailableIntUnaryOperator<Throwable> failing = t -> {
            testable.setThrowable(ERROR);
            testable.test();
            return 0;
        };
        final FailableIntUnaryOperator<Throwable> nop = FailableIntUnaryOperator.nop();
        Throwable e = assertThrows(OutOfMemoryError.class, () -> nop.compose(failing).applyAsInt(0));
        assertSame(ERROR, e);
        e = assertThrows(OutOfMemoryError.class, () -> failing.compose(nop).applyAsInt(0));
        assertSame(ERROR, e);
        // Does not throw
        nop.compose(nop);
        // Documented in Javadoc edge-case.
        assertThrows(NullPointerException.class, () -> failing.compose(null));
    }

    @Test
    public void testIntUnaryOperatorIdentity() throws Throwable {
        final FailableIntUnaryOperator<Throwable> nop = FailableIntUnaryOperator.identity();
        // Does not throw
        nop.compose(nop);
        // Documented in Javadoc edge-case.
        assertThrows(NullPointerException.class, () -> nop.compose(null));
    }

    @Test
    public void testLongConsumerAndThen() throws Throwable {
        final Testable<?, ?> testable = new Testable<>(null);
        final FailableLongConsumer<Throwable> failing = t -> {
            testable.setThrowable(ERROR);
            testable.test();
        };
        final FailableLongConsumer<Throwable> nop = FailableLongConsumer.nop();
        Throwable e = assertThrows(OutOfMemoryError.class, () -> nop.andThen(failing).accept(0L));
        assertSame(ERROR, e);
        e = assertThrows(OutOfMemoryError.class, () -> failing.andThen(nop).accept(0L));
        assertSame(ERROR, e);
        // Does not throw
        nop.andThen(nop);
        // Documented in Javadoc edge-case.
        assertThrows(NullPointerException.class, () -> failing.andThen(null));
    }

    @Test
    public void testLongPredicate() throws Throwable {
        FailureOnOddInvocations.invocations = 0;
        final FailableLongPredicate<Throwable> failablePredicate = FailureOnOddInvocations::testLong;
        assertThrows(SomeException.class, () -> failablePredicate.test(1L));
        failablePredicate.test(1L);
    }

    @Test
    public void testLongPredicateAnd() throws Throwable {
        assertTrue(FailableLongPredicate.TRUE.and(FailableLongPredicate.TRUE).test(0));
        assertFalse(FailableLongPredicate.TRUE.and(FailableLongPredicate.FALSE).test(0));
        assertFalse(FailableLongPredicate.FALSE.and(FailableLongPredicate.TRUE).test(0));
        assertFalse(FailableLongPredicate.FALSE.and(FailableLongPredicate.FALSE).test(0));
        // null tests
        assertThrows(NullPointerException.class, () -> assertFalse(FailableLongPredicate.falsePredicate().and(null).test(0)));
        assertThrows(NullPointerException.class, () -> assertTrue(FailableLongPredicate.truePredicate().and(null).test(0)));
    }

    @Test
    public void testLongPredicateNegate() throws Throwable {
        assertFalse(FailableLongPredicate.TRUE.negate().test(0L));
        assertFalse(FailableLongPredicate.truePredicate().negate().test(0L));
        assertTrue(FailableLongPredicate.FALSE.negate().test(0L));
        assertTrue(FailableLongPredicate.falsePredicate().negate().test(0L));
    }

    @Test
    public void testLongPredicateOr() throws Throwable {
        assertTrue(FailableLongPredicate.TRUE.or(FailableLongPredicate.TRUE).test(0));
        assertTrue(FailableLongPredicate.TRUE.or(FailableLongPredicate.FALSE).test(0));
        assertTrue(FailableLongPredicate.FALSE.or(FailableLongPredicate.TRUE).test(0));
        assertFalse(FailableLongPredicate.FALSE.or(FailableLongPredicate.FALSE).test(0));
        // null tests
        assertThrows(NullPointerException.class, () -> assertFalse(FailableLongPredicate.falsePredicate().or(null).test(0)));
        assertThrows(NullPointerException.class, () -> assertTrue(FailableLongPredicate.truePredicate().or(null).test(0)));
    }

    @Test
    public void testLongUnaryOperatorAndThen() throws Throwable {
        final Testable<?, ?> testable = new Testable<>(null);
        final FailableLongUnaryOperator<Throwable> failing = t -> {
            testable.setThrowable(ERROR);
            testable.test();
            return 0L;
        };
        final FailableLongUnaryOperator<Throwable> nop = FailableLongUnaryOperator.nop();
        Throwable e = assertThrows(OutOfMemoryError.class, () -> nop.andThen(failing).applyAsLong(0L));
        assertSame(ERROR, e);
        e = assertThrows(OutOfMemoryError.class, () -> failing.andThen(nop).applyAsLong(0L));
        assertSame(ERROR, e);
        // Does not throw
        nop.andThen(nop);
        // Documented in Javadoc edge-case.
        assertThrows(NullPointerException.class, () -> failing.andThen(null));
    }

    @Test
    public void testLongUnaryOperatorCompose() throws Throwable {
        final Testable<?, ?> testable = new Testable<>(null);
        final FailableLongUnaryOperator<Throwable> failing = t -> {
            testable.setThrowable(ERROR);
            testable.test();
            return 0L;
        };
        final FailableLongUnaryOperator<Throwable> nop = FailableLongUnaryOperator.nop();
        Throwable e = assertThrows(OutOfMemoryError.class, () -> nop.compose(failing).applyAsLong(0L));
        assertSame(ERROR, e);
        e = assertThrows(OutOfMemoryError.class, () -> failing.compose(nop).applyAsLong(0L));
        assertSame(ERROR, e);
        // Does not throw
        nop.compose(nop);
        // Documented in Javadoc edge-case.
        assertThrows(NullPointerException.class, () -> failing.compose(null));
    }

    @Test
    public void testLongUnaryOperatorIdentity() throws Throwable {
        final FailableLongUnaryOperator<Throwable> nop = FailableLongUnaryOperator.identity();
        // Does not throw
        nop.compose(nop);
        // Documented in Javadoc edge-case.
        assertThrows(NullPointerException.class, () -> nop.compose(null));
    }

    @Test
    @DisplayName("Test that asPredicate(FailablePredicate) is converted to -> Predicate ")
    public void testPredicate() {
        FailureOnOddInvocations.invocations = 0;
        final FailablePredicate<Object, Throwable> failablePredicate = t -> FailureOnOddInvocations.failingBool();
        final Predicate<?> predicate = Failable.asPredicate(failablePredicate);
        final UndeclaredThrowableException e = assertThrows(UndeclaredThrowableException.class,
            () -> predicate.test(null));
        final Throwable cause = e.getCause();
        assertNotNull(cause);
        assertTrue(cause instanceof SomeException);
        assertEquals("Odd Invocation: 1", cause.getMessage());
        final boolean instance = predicate.test(null);
        assertNotNull(instance);
    }

    @Test
    public void testPredicateAnd() throws Throwable {
        assertTrue(FailablePredicate.TRUE.and(FailablePredicate.TRUE).test(null));
        assertFalse(FailablePredicate.TRUE.and(FailablePredicate.FALSE).test(null));
        assertFalse(FailablePredicate.FALSE.and(FailablePredicate.TRUE).test(null));
        assertFalse(FailablePredicate.FALSE.and(FailablePredicate.FALSE).test(null));
        // null tests
        assertThrows(NullPointerException.class, () -> assertFalse(FailablePredicate.FALSE.and(null).test(null)));
        assertThrows(NullPointerException.class, () -> assertTrue(FailablePredicate.TRUE.and(null).test(null)));
    }

    @Test
    public void testPredicateOr() throws Throwable {
        assertTrue(FailablePredicate.TRUE.or(FailablePredicate.TRUE).test(null));
        assertTrue(FailablePredicate.TRUE.or(FailablePredicate.FALSE).test(null));
        assertTrue(FailablePredicate.FALSE.or(FailablePredicate.TRUE).test(null));
        assertFalse(FailablePredicate.FALSE.or(FailablePredicate.FALSE).test(null));
        // null tests
        assertThrows(NullPointerException.class, () -> assertFalse(FailablePredicate.FALSE.or(null).test(null)));
        assertThrows(NullPointerException.class, () -> assertTrue(FailablePredicate.TRUE.or(null).test(null)));
    }

    @Test
    public void testPredicateNegate() throws Throwable {
        assertFalse(FailablePredicate.TRUE.negate().test(null));
        assertFalse(FailablePredicate.truePredicate().negate().test(null));
        assertTrue(FailablePredicate.FALSE.negate().test(null));
        assertTrue(FailablePredicate.falsePredicate().negate().test(null));
    }

    @Test
    public void testRunnable() {
        FailureOnOddInvocations.invocations = 0;
        final UndeclaredThrowableException e = assertThrows(UndeclaredThrowableException.class,
            () -> Failable.run(FailureOnOddInvocations::new));
        final Throwable cause = e.getCause();
        assertNotNull(cause);
        assertTrue(cause instanceof SomeException);
        assertEquals("Odd Invocation: 1", cause.getMessage());

        // Even invocations, should not throw an exception
        Failable.run(FailureOnOddInvocations::new);
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableBiConsumer_Object_Throwable() {
        new FailableBiConsumer<Object, Object, Throwable>() {

            @Override
            public void accept(final Object object1, final Object object2) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableBiConsumer_String_IOException() {
        new FailableBiConsumer<String, String, IOException>() {

            @Override
            public void accept(final String object1, final String object2) throws IOException {
                throw new IOException("test");

            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableBiFunction_Object_Throwable() {
        new FailableBiFunction<Object, Object, Object, Throwable>() {

            @Override
            public Object apply(final Object input1, final Object input2) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableBiFunction_String_IOException() {
        new FailableBiFunction<String, String, String, IOException>() {

            @Override
            public String apply(final String input1, final String input2) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableBiPredicate_Object_Throwable() {
        new FailableBiPredicate<Object, Object, Throwable>() {

            @Override
            public boolean test(final Object object1, final Object object2) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableBiPredicate_String_IOException() {
        new FailableBiPredicate<String, String, IOException>() {

            @Override
            public boolean test(final String object1, final String object2) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableBooleanSupplier_IOException() {
        new FailableBooleanSupplier<IOException>() {

            @Override
            public boolean getAsBoolean() throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableBooleanSupplier_Throwable() {
        new FailableBooleanSupplier<Throwable>() {

            @Override
            public boolean getAsBoolean() throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableCallable_Object_Throwable() {
        new FailableCallable<Object, Throwable>() {

            @Override
            public Object call() throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableCallable_String_IOException() {
        new FailableCallable<String, IOException>() {

            @Override
            public String call() throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableConsumer_Object_Throwable() {
        new FailableConsumer<Object, Throwable>() {

            @Override
            public void accept(final Object object) throws Throwable {
                throw new IOException("test");

            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableConsumer_String_IOException() {
        new FailableConsumer<String, IOException>() {

            @Override
            public void accept(final String object) throws IOException {
                throw new IOException("test");

            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableDoubleBinaryOperator_IOException() {
        new FailableDoubleBinaryOperator<IOException>() {

            @Override
            public double applyAsDouble(final double left, final double right) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableDoubleBinaryOperator_Throwable() {
        new FailableDoubleBinaryOperator<Throwable>() {

            @Override
            public double applyAsDouble(final double left, final double right) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableDoubleConsumer_IOException() {
        new FailableDoubleConsumer<IOException>() {

            @Override
            public void accept(final double value) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableDoubleConsumer_Throwable() {
        new FailableDoubleConsumer<Throwable>() {

            @Override
            public void accept(final double value) throws Throwable {
                throw new IOException("test");

            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableDoubleFunction_IOException() {
        new FailableDoubleFunction<String, IOException>() {

            @Override
            public String apply(final double input) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableDoubleFunction_Throwable() {
        new FailableDoubleFunction<Object, Throwable>() {

            @Override
            public Object apply(final double input) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableDoubleSupplier_IOException() {
        new FailableDoubleSupplier<IOException>() {

            @Override
            public double getAsDouble() throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableDoubleSupplier_Throwable() {
        new FailableDoubleSupplier<Throwable>() {

            @Override
            public double getAsDouble() throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableDoubleToIntFunction_IOException() {
        new FailableDoubleToIntFunction<IOException>() {

            @Override
            public int applyAsInt(final double value) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableDoubleToIntFunction_Throwable() {
        new FailableDoubleToIntFunction<Throwable>() {

            @Override
            public int applyAsInt(final double value) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableDoubleToLongFunction_IOException() {
        new FailableDoubleToLongFunction<IOException>() {

            @Override
            public int applyAsLong(final double value) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableDoubleToLongFunction_Throwable() {
        new FailableDoubleToLongFunction<Throwable>() {

            @Override
            public int applyAsLong(final double value) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableFunction_Object_Throwable() {
        new FailableFunction<Object, Object, Throwable>() {

            @Override
            public Object apply(final Object input) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableFunction_String_IOException() {
        new FailableFunction<String, String, IOException>() {

            @Override
            public String apply(final String input) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableIntBinaryOperator_IOException() {
        new FailableIntBinaryOperator<IOException>() {

            @Override
            public int applyAsInt(final int left, final int right) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableIntBinaryOperator_Throwable() {
        new FailableIntBinaryOperator<Throwable>() {

            @Override
            public int applyAsInt(final int left, final int right) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableIntConsumer_IOException() {
        new FailableIntConsumer<IOException>() {

            @Override
            public void accept(final int value) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableIntConsumer_Throwable() {
        new FailableIntConsumer<Throwable>() {

            @Override
            public void accept(final int value) throws Throwable {
                throw new IOException("test");

            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableIntFunction_Object_Throwable() {
        new FailableIntFunction<Object, Throwable>() {

            @Override
            public Object apply(final int input) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableIntFunction_String_IOException() {
        new FailableIntFunction<String, IOException>() {

            @Override
            public String apply(final int input) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableIntSupplier_IOException() {
        new FailableIntSupplier<IOException>() {

            @Override
            public int getAsInt() throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableIntSupplier_Throwable() {
        new FailableIntSupplier<Throwable>() {

            @Override
            public int getAsInt() throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableIntToDoubleFunction_IOException() {
        new FailableIntToDoubleFunction<IOException>() {

            @Override
            public double applyAsDouble(final int value) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableIntToDoubleFunction_Throwable() {
        new FailableIntToDoubleFunction<Throwable>() {

            @Override
            public double applyAsDouble(final int value) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableIntToLongFunction_IOException() {
        new FailableIntToLongFunction<IOException>() {

            @Override
            public long applyAsLong(final int value) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableIntToLongFunction_Throwable() {
        new FailableIntToLongFunction<Throwable>() {

            @Override
            public long applyAsLong(final int value) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableLongBinaryOperator_IOException() {
        new FailableLongBinaryOperator<IOException>() {

            @Override
            public long applyAsLong(final long left, final long right) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableLongBinaryOperator_Throwable() {
        new FailableLongBinaryOperator<Throwable>() {

            @Override
            public long applyAsLong(final long left, final long right) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableLongConsumer_IOException() {
        new FailableLongConsumer<IOException>() {

            @Override
            public void accept(final long object) throws IOException {
                throw new IOException("test");

            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableLongConsumer_Throwable() {
        new FailableLongConsumer<Throwable>() {

            @Override
            public void accept(final long object) throws Throwable {
                throw new IOException("test");

            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableLongFunction_IOException() {
        new FailableLongFunction<String, IOException>() {

            @Override
            public String apply(final long input) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableLongFunction_Throwable() {
        new FailableLongFunction<Object, Throwable>() {

            @Override
            public Object apply(final long input) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableLongSupplier_IOException() {
        new FailableLongSupplier<IOException>() {

            @Override
            public long getAsLong() throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableLongSupplier_Throwable() {
        new FailableLongSupplier<Throwable>() {

            @Override
            public long getAsLong() throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableLongToDoubleFunction_IOException() {
        new FailableLongToDoubleFunction<IOException>() {

            @Override
            public double applyAsDouble(final long value) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableLongToDoubleFunction_Throwable() {
        new FailableLongToDoubleFunction<Throwable>() {

            @Override
            public double applyAsDouble(final long value) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableLongToIntFunction_IOException() {
        new FailableLongToIntFunction<IOException>() {

            @Override
            public int applyAsInt(final long value) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableLongToIntFunction_Throwable() {
        new FailableLongToIntFunction<Throwable>() {

            @Override
            public int applyAsInt(final long value) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableObjDoubleConsumer_Object_Throwable() {
        new FailableObjDoubleConsumer<Object, Throwable>() {

            @Override
            public void accept(final Object object, final double value) throws Throwable {
                throw new IOException("test");

            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableObjDoubleConsumer_String_IOException() {
        new FailableObjDoubleConsumer<String, IOException>() {

            @Override
            public void accept(final String object, final double value) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableObjIntConsumer_Object_Throwable() {
        new FailableObjIntConsumer<Object, Throwable>() {

            @Override
            public void accept(final Object object, final int value) throws Throwable {
                throw new IOException("test");

            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableObjIntConsumer_String_IOException() {
        new FailableObjIntConsumer<String, IOException>() {

            @Override
            public void accept(final String object, final int value) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableObjLongConsumer_Object_Throwable() {
        new FailableObjLongConsumer<Object, Throwable>() {

            @Override
            public void accept(final Object object, final long value) throws Throwable {
                throw new IOException("test");

            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableObjLongConsumer_String_IOException() {
        new FailableObjLongConsumer<String, IOException>() {

            @Override
            public void accept(final String object, final long value) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailablePredicate_Object_Throwable() {
        new FailablePredicate<Object, Throwable>() {

            @Override
            public boolean test(final Object object) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailablePredicate_String_IOException() {
        new FailablePredicate<String, IOException>() {

            @Override
            public boolean test(final String object) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableRunnable_IOException() {
        new FailableRunnable<IOException>() {

            @Override
            public void run() throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableRunnable_Throwable() {
        new FailableRunnable<Throwable>() {

            @Override
            public void run() throws Throwable {
                throw new IOException("test");

            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableShortSupplier_IOException() {
        new FailableShortSupplier<IOException>() {

            @Override
            public short getAsShort() throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableShortSupplier_Throwable() {
        new FailableShortSupplier<Throwable>() {

            @Override
            public short getAsShort() throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableSupplier_Object_Throwable() {
        new FailableSupplier<Object, Throwable>() {

            @Override
            public Object get() throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableSupplier_String_IOException() {
        new FailableSupplier<String, IOException>() {

            @Override
            public String get() throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableToDoubleBiFunction_Object_Throwable() {
        new FailableToDoubleBiFunction<Object, Object, Throwable>() {

            @Override
            public double applyAsDouble(final Object t, final Object u) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableToDoubleBiFunction_String_IOException() {
        new FailableToDoubleBiFunction<String, String, IOException>() {

            @Override
            public double applyAsDouble(final String t, final String u) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableToDoubleFunction_Object_Throwable() {
        new FailableToDoubleFunction<Object, Throwable>() {

            @Override
            public double applyAsDouble(final Object t) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableToDoubleFunction_String_IOException() {
        new FailableToDoubleFunction<String, IOException>() {

            @Override
            public double applyAsDouble(final String t) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableToIntBiFunction_Object_Throwable() {
        new FailableToIntBiFunction<Object, Object, Throwable>() {

            @Override
            public int applyAsInt(final Object t, final Object u) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableToIntBiFunction_String_IOException() {
        new FailableToIntBiFunction<String, String, IOException>() {

            @Override
            public int applyAsInt(final String t, final String u) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableToIntFunction_Object_Throwable() {
        new FailableToIntFunction<Object, Throwable>() {

            @Override
            public int applyAsInt(final Object t) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableToIntFunction_String_IOException() {
        new FailableToIntFunction<String, IOException>() {

            @Override
            public int applyAsInt(final String t) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableToLongBiFunction_Object_Throwable() {
        new FailableToLongBiFunction<Object, Object, Throwable>() {

            @Override
            public long applyAsLong(final Object t, final Object u) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableToLongBiFunction_String_IOException() {
        new FailableToLongBiFunction<String, String, IOException>() {

            @Override
            public long applyAsLong(final String t, final String u) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableToLongFunction_Object_Throwable() {
        new FailableToLongFunction<Object, Throwable>() {

            @Override
            public long applyAsLong(final Object t) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableToLongFunction_String_IOException() {
        new FailableToLongFunction<String, IOException>() {

            @Override
            public long applyAsLong(final String t) throws IOException {
                throw new IOException("test");
            }
        };
    }

    @Test
    public void testTryWithResources() {
        final CloseableObject closeable = new CloseableObject();
        final FailableConsumer<Throwable, ? extends Throwable> consumer = closeable::run;
        Throwable e = assertThrows(IllegalStateException.class,
            () -> Failable.tryWithResources(() -> consumer.accept(ILLEGAL_STATE_EXCEPTION), closeable::close));
        assertSame(ILLEGAL_STATE_EXCEPTION, e);

        assertTrue(closeable.isClosed());
        closeable.reset();
        e = assertThrows(OutOfMemoryError.class,
            () -> Failable.tryWithResources(() -> consumer.accept(ERROR), closeable::close));
        assertSame(ERROR, e);

        assertTrue(closeable.isClosed());
        closeable.reset();
        final IOException ioe = new IOException("Unknown I/O error");
        final UncheckedIOException uioe = assertThrows(UncheckedIOException.class,
            () -> Failable.tryWithResources(() -> consumer.accept(ioe), closeable::close));
        final IOException cause = uioe.getCause();
        assertSame(ioe, cause);

        assertTrue(closeable.isClosed());
        closeable.reset();
        Failable.tryWithResources(() -> consumer.accept(null), closeable::close);
        assertTrue(closeable.isClosed());
    }

    @Test
    public void testFailableDoubleToIntFunctionNop() throws Throwable {
        assertEquals(0, FailableDoubleToIntFunction.nop().applyAsInt(Double.MAX_VALUE), "Expect NOP to return 0");
    }

    @Test
    public void testFailableDoubleToLongFunctionNop() throws Throwable {
        assertEquals(0, FailableDoubleToLongFunction.nop().applyAsLong(Double.MAX_VALUE), "Expect NOP to return 0");
    }

    @Test
    public void testFailableIntToDoubleFunctionNop() throws Throwable {
        assertEquals(0, FailableIntToDoubleFunction.nop().applyAsDouble(Integer.MAX_VALUE), "Expect NOP to return 0");
    }

    @Test
    public void testFailableIntToLongFunctionNop() throws Throwable {
        assertEquals(0, FailableIntToLongFunction.nop().applyAsLong(Integer.MAX_VALUE), "Expect NOP to return 0");
    }

    @Test
    public void testFailableLongToDoubleFunctionNop() throws Throwable {
        assertEquals(0, FailableLongToDoubleFunction.nop().applyAsDouble(Long.MAX_VALUE), "Expect NOP to return 0");
    }

    @Test
    public void testFailableLongToIntFunctionNop() throws Throwable {
        assertEquals(0, FailableLongToIntFunction.nop().applyAsInt(Long.MAX_VALUE), "Expect NOP to return 0");
    }

    @Test
    public void testFailableToIntFunctionNop() throws Throwable {
        assertEquals(0, FailableToIntFunction.nop().applyAsInt("Foo"), "Expect NOP to return 0");
    }

    @Test
    public void testFailableToIntBiFunctionNop() throws Throwable {
        assertEquals(0, FailableToIntBiFunction.nop().applyAsInt("Foo", "Bar"), "Expect NOP to return 0");
    }

    @Test
    public void testFailableToLongFunctionNop() throws Throwable {
        assertEquals(0, FailableToLongFunction.nop().applyAsLong("Foo"), "Expect NOP to return 0");
    }

    @Test
    public void testFailableToLongBiFunctionNop() throws Throwable {
        assertEquals(0, FailableToLongBiFunction.nop().applyAsLong("Foo", "Bar"), "Expect NOP to return 0");
    }

    @Test
    public void testFailableToDoubleFunctionNop() throws Throwable {
        assertEquals(0, FailableToDoubleFunction.nop().applyAsDouble("Foo"), "Expect NOP to return 0");
    }

    @Test
    public void testFailableToDoubleBiFunctionNop() throws Throwable {
        assertEquals(0, FailableToDoubleBiFunction.nop().applyAsDouble("Foo", "Bar"), "Expect NOP to return 0");
    }

    @Test
    public void testFailableBiFunctionNop() throws Throwable {
        assertNull(FailableBiFunction.nop().apply("Foo", "Bar"), "Expect NOP to return null");
    }

    @Test
    public void testFailableDoubleFunctionNop() throws Throwable {
        assertNull(FailableDoubleFunction.nop().apply(Double.MAX_VALUE), "Expect NOP to return null");
    }

    @Test
    public void testFailableIntFunctionNop() throws Throwable {
        assertNull(FailableIntFunction.nop().apply(Integer.MAX_VALUE), "Expect NOP to return null");
    }

    @Test
    public void testFailableLongFunctionNop() throws Throwable {
        assertNull(FailableLongFunction.nop().apply(Long.MAX_VALUE), "Expect NOP to return null");
    }

    @Test
    public void testFailableConsumerNop() throws Throwable {
        // Expect nothing thrown
        FailableConsumer.nop().accept("Foo");
    }

    @Test
    public void testFailableObjDoubleConsumerNop() throws Throwable {
        // Expect nothing thrown
        FailableObjDoubleConsumer.nop().accept("Foo", Double.MAX_VALUE);
    }

    @Test
    public void testFailableObjIntConsumerNop() throws Throwable {
        // Expect nothing thrown
        FailableObjIntConsumer.nop().accept("Foo", Integer.MAX_VALUE);
    }

    @Test
    public void testFailableObjLongConsumerNop() throws Throwable {
        // Expect nothing thrown
        FailableObjLongConsumer.nop().accept("Foo", Long.MAX_VALUE);
    }

}
