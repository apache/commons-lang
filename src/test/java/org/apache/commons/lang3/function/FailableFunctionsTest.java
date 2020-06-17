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

import org.apache.commons.lang3.Functions;
import org.junit.jupiter.api.Test;

/**
 * Tests "failable" interfaces defined in this package.
 */
public class FailableFunctionsTest {

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

        static boolean testDouble(double value) throws SomeException {
            throwOnOdd();
            return true;
        }

        static boolean testInt(int value) throws SomeException {
            throwOnOdd();
            return true;
        }

        static boolean testLong(long value) throws SomeException {
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

        public Object test(Object input1, Object input2) throws Throwable {
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

        public void testDouble(double i) throws Throwable {
            test(throwable);
            acceptedPrimitiveObject1 = (P) ((Double) i);
        }

        public double testDoubleDouble(double i, double j) throws Throwable {
            test(throwable);
            acceptedPrimitiveObject1 = (P) ((Double) i);
            acceptedPrimitiveObject2 = (P) ((Double) j);
            return 3d;
        }

        public void testInt(int i) throws Throwable {
            test(throwable);
            acceptedPrimitiveObject1 = (P) ((Integer) i);
        }

        public void testLong(long i) throws Throwable {
            test(throwable);
            acceptedPrimitiveObject1 = (P) ((Long) i);
        }

        public void testObjDouble(T object, double i) throws Throwable {
            test(throwable);
            acceptedObject = object;
            acceptedPrimitiveObject1 = (P) ((Double) i);
        }

        public void testObjInt(T object, int i) throws Throwable {
            test(throwable);
            acceptedObject = object;
            acceptedPrimitiveObject1 = (P) ((Integer) i);
        }

        public void testObjLong(T object, long i) throws Throwable {
            test(throwable);
            acceptedObject = object;
            acceptedPrimitiveObject1 = (P) ((Long) i);
        }
    }

    @Test
    public void testAcceptDoubleConsumer() {
        final IllegalStateException ise = new IllegalStateException();
        final Testable<?, Double> testable = new Testable<>(ise);
        Throwable e = assertThrows(IllegalStateException.class, () -> Functions.accept(testable::testDouble, 1d));
        assertSame(ise, e);
        assertNull(testable.getAcceptedPrimitiveObject1());

        final Error error = new OutOfMemoryError();
        testable.setThrowable(error);
        e = assertThrows(OutOfMemoryError.class, () -> Functions.accept(testable::testDouble, 1d));
        assertSame(error, e);
        assertNull(testable.getAcceptedPrimitiveObject1());

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Functions.accept(testable::testDouble, 1d));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);
        assertNull(testable.getAcceptedPrimitiveObject1());

        testable.setThrowable(null);
        Functions.accept(testable::testDouble, 1d);
        assertEquals(1, testable.getAcceptedPrimitiveObject1());
    }

    @Test
    public void testAcceptIntConsumer() {
        final IllegalStateException ise = new IllegalStateException();
        final Testable<?, Integer> testable = new Testable<>(ise);
        Throwable e = assertThrows(IllegalStateException.class, () -> Functions.accept(testable::testInt, 1));
        assertSame(ise, e);
        assertNull(testable.getAcceptedPrimitiveObject1());

        final Error error = new OutOfMemoryError();
        testable.setThrowable(error);
        e = assertThrows(OutOfMemoryError.class, () -> Functions.accept(testable::testInt, 1));
        assertSame(error, e);
        assertNull(testable.getAcceptedPrimitiveObject1());

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Functions.accept(testable::testInt, 1));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);
        assertNull(testable.getAcceptedPrimitiveObject1());

        testable.setThrowable(null);
        Functions.accept(testable::testInt, 1);
        assertEquals(1, testable.getAcceptedPrimitiveObject1());
    }

    @Test
    public void testAcceptLongConsumer() {
        final IllegalStateException ise = new IllegalStateException();
        final Testable<?, Long> testable = new Testable<>(ise);
        Throwable e = assertThrows(IllegalStateException.class, () -> Functions.accept(testable::testLong, 1L));
        assertSame(ise, e);
        assertNull(testable.getAcceptedPrimitiveObject1());

        final Error error = new OutOfMemoryError();
        testable.setThrowable(error);
        e = assertThrows(OutOfMemoryError.class, () -> Functions.accept(testable::testLong, 1L));
        assertSame(error, e);
        assertNull(testable.getAcceptedPrimitiveObject1());

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Functions.accept(testable::testLong, 1L));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);
        assertNull(testable.getAcceptedPrimitiveObject1());

        testable.setThrowable(null);
        Functions.accept(testable::testLong, 1L);
        assertEquals(1, testable.getAcceptedPrimitiveObject1());
    }

    @Test
    public void testAcceptObjDoubleConsumer() {
        final IllegalStateException ise = new IllegalStateException();
        final Testable<String, Double> testable = new Testable<>(ise);
        Throwable e = assertThrows(IllegalStateException.class,
            () -> Functions.accept(testable::testObjDouble, "X", 1d));
        assertSame(ise, e);
        assertNull(testable.getAcceptedObject());
        assertNull(testable.getAcceptedPrimitiveObject1());

        final Error error = new OutOfMemoryError();
        testable.setThrowable(error);
        e = assertThrows(OutOfMemoryError.class, () -> Functions.accept(testable::testObjDouble, "X", 1d));
        assertSame(error, e);
        assertNull(testable.getAcceptedObject());
        assertNull(testable.getAcceptedPrimitiveObject1());

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Functions.accept(testable::testObjDouble, "X", 1d));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);
        assertNull(testable.getAcceptedObject());
        assertNull(testable.getAcceptedPrimitiveObject1());

        testable.setThrowable(null);
        Functions.accept(testable::testObjDouble, "X", 1d);
        assertEquals("X", testable.getAcceptedObject());
        assertEquals(1d, testable.getAcceptedPrimitiveObject1());
    }

    @Test
    public void testAcceptObjIntConsumer() {
        final IllegalStateException ise = new IllegalStateException();
        final Testable<String, Integer> testable = new Testable<>(ise);
        Throwable e = assertThrows(IllegalStateException.class, () -> Functions.accept(testable::testObjInt, "X", 1));
        assertSame(ise, e);
        assertNull(testable.getAcceptedObject());
        assertNull(testable.getAcceptedPrimitiveObject1());

        final Error error = new OutOfMemoryError();
        testable.setThrowable(error);
        e = assertThrows(OutOfMemoryError.class, () -> Functions.accept(testable::testObjInt, "X", 1));
        assertSame(error, e);
        assertNull(testable.getAcceptedObject());
        assertNull(testable.getAcceptedPrimitiveObject1());

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Functions.accept(testable::testObjInt, "X", 1));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);
        assertNull(testable.getAcceptedObject());
        assertNull(testable.getAcceptedPrimitiveObject1());

        testable.setThrowable(null);
        Functions.accept(testable::testObjInt, "X", 1);
        assertEquals("X", testable.getAcceptedObject());
        assertEquals(1, testable.getAcceptedPrimitiveObject1());
    }

    @Test
    public void testAcceptObjLongConsumer() {
        final IllegalStateException ise = new IllegalStateException();
        final Testable<String, Long> testable = new Testable<>(ise);
        Throwable e = assertThrows(IllegalStateException.class, () -> Functions.accept(testable::testObjLong, "X", 1L));
        assertSame(ise, e);
        assertNull(testable.getAcceptedObject());
        assertNull(testable.getAcceptedPrimitiveObject1());

        final Error error = new OutOfMemoryError();
        testable.setThrowable(error);
        e = assertThrows(OutOfMemoryError.class, () -> Functions.accept(testable::testObjLong, "X", 1L));
        assertSame(error, e);
        assertNull(testable.getAcceptedObject());
        assertNull(testable.getAcceptedPrimitiveObject1());

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Functions.accept(testable::testObjLong, "X", 1L));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);
        assertNull(testable.getAcceptedObject());
        assertNull(testable.getAcceptedPrimitiveObject1());

        testable.setThrowable(null);
        Functions.accept(testable::testObjLong, "X", 1L);
        assertEquals("X", testable.getAcceptedObject());
        assertEquals(1L, testable.getAcceptedPrimitiveObject1());
    }

    @Test
    public void testApplyBiFunction() {
        final IllegalStateException ise = new IllegalStateException();
        final Testable<?, ?> testable = new Testable<>(null);
        Throwable e = assertThrows(IllegalStateException.class,
            () -> Functions.apply(Testable::testAsInteger, testable, ise));
        assertSame(ise, e);

        final Error error = new OutOfMemoryError();
        e = assertThrows(OutOfMemoryError.class, () -> Functions.apply(Testable::testAsInteger, testable, error));
        assertSame(error, e);

        final IOException ioe = new IOException("Unknown I/O error");
        e = assertThrows(UncheckedIOException.class, () -> Functions.apply(Testable::testAsInteger, testable, ioe));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        final Integer i = Functions.apply(Testable::testAsInteger, testable, (Throwable) null);
        assertNotNull(i);
        assertEquals(0, i.intValue());
    }

    @Test
    public void testApplyDoubleBinaryOperator() {
        final IllegalStateException ise = new IllegalStateException();
        final Testable<?, Double> testable = new Testable<>(ise);
        Throwable e = assertThrows(IllegalStateException.class,
            () -> Functions.applyAsDouble(testable::testDoubleDouble, 1d, 2d));
        assertSame(ise, e);

        final Testable<?, Double> testable2 = new Testable<>(null);
        final double i = Functions.applyAsDouble(testable2::testDoubleDouble, 1d, 2d);
        assertEquals(3d, i);
    }

    @Test
    public void testApplyFunction() {
        final IllegalStateException ise = new IllegalStateException();
        final Testable<?, ?> testable = new Testable<>(ise);
        Throwable e = assertThrows(IllegalStateException.class,
            () -> Functions.apply(Testable::testAsInteger, testable));
        assertSame(ise, e);

        final Error error = new OutOfMemoryError();
        testable.setThrowable(error);
        e = assertThrows(OutOfMemoryError.class, () -> Functions.apply(Testable::testAsInteger, testable));
        assertSame(error, e);

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Functions.apply(Testable::testAsInteger, testable));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        testable.setThrowable(null);
        final Integer i = Functions.apply(Testable::testAsInteger, testable);
        assertNotNull(i);
        assertEquals(0, i.intValue());
    }

    @Test
    public void testAsRunnable() {
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
    public void testDoublePredicate() throws Throwable {
        FailureOnOddInvocations.invocations = 0;
        final FailableDoublePredicate<Throwable> failablePredicate = t1 -> FailureOnOddInvocations.testDouble(t1);
        assertThrows(SomeException.class, () -> failablePredicate.test(1d));
        failablePredicate.test(1d);
    }

    @Test
    public void testGetAsBooleanSupplier() {
        final IllegalStateException ise = new IllegalStateException();
        final Testable<?, ?> testable = new Testable<>(ise);
        Throwable e = assertThrows(IllegalStateException.class,
            () -> Functions.getAsBoolean(testable::testAsBooleanPrimitive));
        assertSame(ise, e);

        final Error error = new OutOfMemoryError();
        testable.setThrowable(error);
        e = assertThrows(OutOfMemoryError.class, () -> Functions.getAsBoolean(testable::testAsBooleanPrimitive));
        assertSame(error, e);

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Functions.getAsBoolean(testable::testAsBooleanPrimitive));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        testable.setThrowable(null);
        assertFalse(Functions.getAsBoolean(testable::testAsBooleanPrimitive));
    }

    @Test
    public void testGetAsDoubleSupplier() {
        final IllegalStateException ise = new IllegalStateException();
        final Testable<?, ?> testable = new Testable<>(ise);
        Throwable e = assertThrows(IllegalStateException.class,
            () -> Functions.getAsDouble(testable::testAsDoublePrimitive));
        assertSame(ise, e);

        final Error error = new OutOfMemoryError();
        testable.setThrowable(error);
        e = assertThrows(OutOfMemoryError.class, () -> Functions.getAsDouble(testable::testAsDoublePrimitive));
        assertSame(error, e);

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Functions.getAsDouble(testable::testAsDoublePrimitive));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        testable.setThrowable(null);
        assertEquals(0, Functions.getAsDouble(testable::testAsDoublePrimitive));
    }

    @Test
    public void testGetAsIntSupplier() {
        final IllegalStateException ise = new IllegalStateException();
        final Testable<?, ?> testable = new Testable<>(ise);
        Throwable e = assertThrows(IllegalStateException.class, () -> Functions.getAsInt(testable::testAsIntPrimitive));
        assertSame(ise, e);

        final Error error = new OutOfMemoryError();
        testable.setThrowable(error);
        e = assertThrows(OutOfMemoryError.class, () -> Functions.getAsInt(testable::testAsIntPrimitive));
        assertSame(error, e);

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Functions.getAsInt(testable::testAsIntPrimitive));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        testable.setThrowable(null);
        final int i = Functions.getAsInt(testable::testAsInteger);
        assertEquals(0, i);
    }

    @Test
    public void testGetAsLongSupplier() {
        final IllegalStateException ise = new IllegalStateException();
        final Testable<?, ?> testable = new Testable<>(ise);
        Throwable e = assertThrows(IllegalStateException.class,
            () -> Functions.getAsLong(testable::testAsLongPrimitive));
        assertSame(ise, e);

        final Error error = new OutOfMemoryError();
        testable.setThrowable(error);
        e = assertThrows(OutOfMemoryError.class, () -> Functions.getAsLong(testable::testAsLongPrimitive));
        assertSame(error, e);

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Functions.getAsLong(testable::testAsLongPrimitive));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        testable.setThrowable(null);
        final long i = Functions.getAsLong(testable::testAsLongPrimitive);
        assertEquals(0, i);
    }

    @Test
    public void testGetFromSupplier() {
        FailureOnOddInvocations.invocations = 0;
        final UndeclaredThrowableException e = assertThrows(UndeclaredThrowableException.class,
            () -> Functions.run(FailureOnOddInvocations::new));
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
        Throwable e = assertThrows(IllegalStateException.class, () -> Functions.get(testable::testAsInteger));
        assertSame(ise, e);

        final Error error = new OutOfMemoryError();
        testable.setThrowable(error);
        e = assertThrows(OutOfMemoryError.class, () -> Functions.get(testable::testAsInteger));
        assertSame(error, e);

        final IOException ioe = new IOException("Unknown I/O error");
        testable.setThrowable(ioe);
        e = assertThrows(UncheckedIOException.class, () -> Functions.get(testable::testAsInteger));
        final Throwable t = e.getCause();
        assertNotNull(t);
        assertSame(ioe, t);

        testable.setThrowable(null);
        final Integer i = Functions.apply(Testable::testAsInteger, testable);
        assertNotNull(i);
        assertEquals(0, i.intValue());
    }

    @Test
    public void testIntPredicate() throws Throwable {
        FailureOnOddInvocations.invocations = 0;
        final FailableIntPredicate<Throwable> failablePredicate = t1 -> FailureOnOddInvocations.testInt(t1);
        assertThrows(SomeException.class, () -> failablePredicate.test(1));
        failablePredicate.test(1);
    }

    @Test
    public void testLongPredicate() throws Throwable {
        FailureOnOddInvocations.invocations = 0;
        final FailableLongPredicate<Throwable> failablePredicate = t1 -> FailureOnOddInvocations.testLong(t1);
        assertThrows(SomeException.class, () -> failablePredicate.test(1l));
        failablePredicate.test(1l);
    }

    @Test
    public void testRunnable() {
        FailureOnOddInvocations.invocations = 0;
        final UndeclaredThrowableException e = assertThrows(UndeclaredThrowableException.class,
            () -> Functions.run(FailureOnOddInvocations::new));
        final Throwable cause = e.getCause();
        assertNotNull(cause);
        assertTrue(cause instanceof SomeException);
        assertEquals("Odd Invocation: 1", cause.getMessage());

        // Even invocations, should not throw an exception
        Functions.run(FailureOnOddInvocations::new);
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableBiConsumer_Object_Throwable() {
        new Functions.FailableBiConsumer<Object, Object, Throwable>() {

            @Override
            public void accept(Object object1, Object object2) throws Throwable {
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
        new Functions.FailableBiConsumer<String, String, IOException>() {

            @Override
            public void accept(String object1, String object2) throws IOException {
                throw new IOException("test");

            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableBiFunction_Object_Throwable() {
        new Functions.FailableBiFunction<Object, Object, Object, Throwable>() {

            @Override
            public Object apply(Object input1, Object input2) throws Throwable {
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
        new Functions.FailableBiFunction<String, String, String, IOException>() {

            @Override
            public String apply(String input1, String input2) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableBiPredicate_Object_Throwable() {
        new Functions.FailableBiPredicate<Object, Object, Throwable>() {

            @Override
            public boolean test(Object object1, Object object2) throws Throwable {
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
        new Functions.FailableBiPredicate<String, String, IOException>() {

            @Override
            public boolean test(String object1, String object2) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableBooleanSupplier_Object_Throwable() {
        new FailableBooleanSupplier<Throwable>() {

            @Override
            public boolean getAsBoolean() throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableBooleanSupplier_String_IOException() {
        new FailableBooleanSupplier<IOException>() {

            @Override
            public boolean getAsBoolean() throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableCallable_Object_Throwable() {
        new Functions.FailableCallable<Object, Throwable>() {

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
        new Functions.FailableCallable<String, IOException>() {

            @Override
            public String call() throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableConsumer_Object_Throwable() {
        new Functions.FailableConsumer<Object, Throwable>() {

            @Override
            public void accept(Object object) throws Throwable {
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
        new Functions.FailableConsumer<String, IOException>() {

            @Override
            public void accept(String object) throws IOException {
                throw new IOException("test");

            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableDoubleBinaryOperator_Object_Throwable() {
        new FailableDoubleBinaryOperator<Throwable>() {

            @Override
            public double applyAsDouble(double left, double right) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableDoubleBinaryOperator_String_IOException() {
        new FailableDoubleBinaryOperator<IOException>() {

            @Override
            public double applyAsDouble(double left, double right) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableDoubleConsumer_Object_Throwable() {
        new FailableDoubleConsumer<Throwable>() {

            @Override
            public void accept(double value) throws Throwable {
                throw new IOException("test");

            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableDoubleConsumer_String_IOException() {
        new FailableDoubleConsumer<IOException>() {

            @Override
            public void accept(double value) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableDoubleFunction_Object_Throwable() {
        new FailableDoubleFunction<Object, Throwable>() {

            @Override
            public Object apply(double input) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableDoubleFunction_String_IOException() {
        new FailableDoubleFunction<String, IOException>() {

            @Override
            public String apply(double input) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableDoubleSupplier_Object_Throwable() {
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
    public void testThrows_FailableDoubleSupplier_String_IOException() {
        new FailableDoubleSupplier<IOException>() {

            @Override
            public double getAsDouble() throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableDoubleToIntFunction_Object_Throwable() {
        new FailableDoubleToIntFunction<Throwable>() {

            @Override
            public int applyAsInt(double value) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableDoubleToIntFunction_String_IOException() {
        new FailableDoubleToIntFunction<IOException>() {

            @Override
            public int applyAsInt(double value) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableDoubleToLongFunction_Object_Throwable() {
        new FailableDoubleToLongFunction<Throwable>() {

            @Override
            public int applyAsLong(double value) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableDoubleToLongFunction_String_IOException() {
        new FailableDoubleToLongFunction<IOException>() {

            @Override
            public int applyAsLong(double value) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableFunction_Object_Throwable() {
        new Functions.FailableFunction<Object, Object, Throwable>() {

            @Override
            public Object apply(Object input) throws Throwable {
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
        new Functions.FailableFunction<String, String, IOException>() {

            @Override
            public String apply(String input) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableIntBinaryOperator_Object_Throwable() {
        new FailableIntBinaryOperator<Throwable>() {

            @Override
            public int applyAsInt(int left, int right) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableIntBinaryOperator_String_IOException() {
        new FailableIntBinaryOperator<IOException>() {

            @Override
            public int applyAsInt(int left, int right) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableIntConsumer_Object_Throwable() {
        new FailableIntConsumer<Throwable>() {

            @Override
            public void accept(int value) throws Throwable {
                throw new IOException("test");

            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableIntConsumer_String_IOException() {
        new FailableIntConsumer<IOException>() {

            @Override
            public void accept(int value) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableIntFunction_Object_Throwable() {
        new FailableIntFunction<Object, Throwable>() {

            @Override
            public Object apply(int input) throws Throwable {
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
            public String apply(int input) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableIntSupplier_Object_Throwable() {
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
    public void testThrows_FailableIntSupplier_String_IOException() {
        new FailableIntSupplier<IOException>() {

            @Override
            public int getAsInt() throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableIntToDoubleFunction_Object_Throwable() {
        new FailableIntToDoubleFunction<Throwable>() {

            @Override
            public double applyAsDouble(int value) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableIntToDoubleFunction_String_IOException() {
        new FailableIntToDoubleFunction<IOException>() {

            @Override
            public double applyAsDouble(int value) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableIntToLongFunction_Object_Throwable() {
        new FailableIntToLongFunction<Throwable>() {

            @Override
            public long applyAsLong(int value) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableIntToLongFunction_String_IOException() {
        new FailableIntToLongFunction<IOException>() {

            @Override
            public long applyAsLong(int value) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableLongBinaryOperator_Object_Throwable() {
        new FailableLongBinaryOperator<Throwable>() {

            @Override
            public long applyAsLong(long left, long right) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableLongBinaryOperator_String_IOException() {
        new FailableLongBinaryOperator<IOException>() {

            @Override
            public long applyAsLong(long left, long right) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableLongConsumer_Object_Throwable() {
        new FailableLongConsumer<Throwable>() {

            @Override
            public void accept(long object) throws Throwable {
                throw new IOException("test");

            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableLongConsumer_String_IOException() {
        new FailableLongConsumer<IOException>() {

            @Override
            public void accept(long object) throws IOException {
                throw new IOException("test");

            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableLongFunction_Object_Throwable() {
        new FailableLongFunction<Object, Throwable>() {

            @Override
            public Object apply(long input) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableLongFunction_String_IOException() {
        new FailableLongFunction<String, IOException>() {

            @Override
            public String apply(long input) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableLongSupplier_Object_Throwable() {
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
    public void testThrows_FailableLongSupplier_String_IOException() {
        new FailableLongSupplier<IOException>() {

            @Override
            public long getAsLong() throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableLongToDoubleFunction_Object_Throwable() {
        new FailableLongToDoubleFunction<Throwable>() {

            @Override
            public double applyAsDouble(long value) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableLongToDoubleFunction_String_IOException() {
        new FailableLongToDoubleFunction<IOException>() {

            @Override
            public double applyAsDouble(long value) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableLongToIntFunction_Object_Throwable() {
        new FailableLongToIntFunction<Throwable>() {

            @Override
            public int applyAsInt(long value) throws Throwable {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception using String and IOExceptions as
     * generic test types.
     */
    @Test
    public void testThrows_FailableLongToIntFunction_String_IOException() {
        new FailableLongToIntFunction<IOException>() {

            @Override
            public int applyAsInt(long value) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableObjDoubleConsumer_Object_Throwable() {
        new FailableObjDoubleConsumer<Object, Throwable>() {

            @Override
            public void accept(Object object, double value) throws Throwable {
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
            public void accept(String object, double value) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableObjIntConsumer_Object_Throwable() {
        new FailableObjIntConsumer<Object, Throwable>() {

            @Override
            public void accept(Object object, int value) throws Throwable {
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
            public void accept(String object, int value) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableObjLongConsumer_Object_Throwable() {
        new FailableObjLongConsumer<Object, Throwable>() {

            @Override
            public void accept(Object object, long value) throws Throwable {
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
            public void accept(String object, long value) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailablePredicate_Object_Throwable() {
        new Functions.FailablePredicate<Object, Throwable>() {

            @Override
            public boolean test(Object object) throws Throwable {
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
        new Functions.FailablePredicate<String, IOException>() {

            @Override
            public boolean test(String object) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableRunnable_Object_Throwable() {
        new Functions.FailableRunnable<Throwable>() {

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
    public void testThrows_FailableRunnable_String_IOException() {
        new Functions.FailableRunnable<IOException>() {

            @Override
            public void run() throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableSupplier_Object_Throwable() {
        new Functions.FailableSupplier<Object, Throwable>() {

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
        new Functions.FailableSupplier<String, IOException>() {

            @Override
            public String get() throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableToDoubleBiFunction_Object_Throwable() {
        new FailableToDoubleBiFunction<Object, Object, Throwable>() {

            @Override
            public double applyAsDouble(Object t, Object u) throws Throwable {
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
            public double applyAsDouble(String t, String u) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableToDoubleFunction_Object_Throwable() {
        new FailableToDoubleFunction<Object, Throwable>() {

            @Override
            public double applyAsDouble(Object t) throws Throwable {
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
            public double applyAsDouble(String t) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableToIntBiFunction_Object_Throwable() {
        new FailableToIntBiFunction<Object, Object, Throwable>() {

            @Override
            public int applyAsInt(Object t, Object u) throws Throwable {
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
            public int applyAsInt(String t, String u) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableToIntFunction_Object_Throwable() {
        new FailableToIntFunction<Object, Throwable>() {

            @Override
            public int applyAsInt(Object t) throws Throwable {
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
            public int applyAsInt(String t) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableToLongBiFunction_Object_Throwable() {
        new FailableToLongBiFunction<Object, Object, Throwable>() {

            @Override
            public long applyAsLong(Object t, Object u) throws Throwable {
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
            public long applyAsLong(String t, String u) throws IOException {
                throw new IOException("test");
            }
        };
    }

    /**
     * Tests that our failable interface is properly defined to throw any exception. using the top level generic types
     * Object and Throwable.
     */
    @Test
    public void testThrows_FailableToLongFunction_Object_Throwable() {
        new FailableToLongFunction<Object, Throwable>() {

            @Override
            public long applyAsLong(Object t) throws Throwable {
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
            public long applyAsLong(String t) throws IOException {
                throw new IOException("test");
            }
        };
    }

}
