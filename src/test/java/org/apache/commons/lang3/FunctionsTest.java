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

import java.io.IOException;
import java.io.UncheckedIOException;
import java.lang.reflect.UndeclaredThrowableException;

import org.apache.commons.lang3.Functions.FailableConsumer;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

class FunctionsTest {
	public static class SomeException extends Exception {
		private static final long serialVersionUID = -4965704778119283411L;

		private Throwable t;

		public SomeException(String pMsg) {
			super(pMsg);
		}

		public void setThrowable(Throwable pThrowable) {
			t = pThrowable;
		}

		public void test() throws Throwable {
			if (t != null) {
				throw t;
			}
		}
	}
	public static class Testable {
		private Throwable t;

		public Testable(Throwable pTh) {
			t = pTh;
		}

		public void setThrowable(Throwable pThrowable) {
			t = pThrowable;
		}

		public void test() throws Throwable {
			test(t);
		}

		public void test(Throwable pThrowable) throws Throwable {
			if (pThrowable != null) {
				throw pThrowable;
			}
		}

		public Integer testInt() throws Throwable {
			return testInt(t);
		}

		public Integer testInt(Throwable pThrowable) throws Throwable {
			if (pThrowable != null) {
				throw pThrowable;
			}
			return 0;
		}
	}

	public static class FailureOnOddInvocations {
		private static int invocation;
		public FailureOnOddInvocations() throws SomeException {
			final int i = ++invocation;
			if (i % 2 == 1) {
				throw new SomeException("Odd Invocation: " + i);
			}
		}
	}

	public static class CloseableObject {
		private boolean closed;

		public void run(Throwable pTh) throws Throwable {
			if (pTh != null) {
				throw pTh;
			}
		}

		public void reset() {
			closed = false;
		}

		public void close() {
			closed = true;
		}

		public boolean isClosed() {
			return closed;
		}
	}

	@Test
	void testRunnable() {
		FailureOnOddInvocations.invocation = 0;
		try {
			Functions.run(() -> new FailureOnOddInvocations());
			fail("Expected Exception");
		} catch (UndeclaredThrowableException e) {
			final Throwable cause = e.getCause();
			assertNotNull(cause);
			assertTrue(cause instanceof SomeException);
			assertEquals("Odd Invocation: 1", cause.getMessage());
		}
		Functions.run(() -> new FailureOnOddInvocations());
	}

	@Test
	void testCallable() {
		FailureOnOddInvocations.invocation = 0;
		try {
			Functions.call(() -> new FailureOnOddInvocations());
			fail("Expected Exception");
		} catch (UndeclaredThrowableException e) {
			final Throwable cause = e.getCause();
			assertNotNull(cause);
			assertTrue(cause instanceof SomeException);
			assertEquals("Odd Invocation: 1", cause.getMessage());
		}
		final FailureOnOddInvocations instance = Functions.call(() -> new FailureOnOddInvocations());
		assertNotNull(instance);
	}

	@Test
	void testAcceptConsumer() {
		final IllegalStateException ise = new IllegalStateException();
		final Testable testable = new Testable(ise);
		try {
			Functions.accept((t) -> t.test(), testable);
			fail("Expected Exception");
		} catch (IllegalStateException e) {
			assertSame(ise, e);
		}
		final Error error = new OutOfMemoryError();
		testable.setThrowable(error);
		try {
			Functions.accept((t) -> t.test(), testable);
		} catch (OutOfMemoryError e) {
			assertSame(error, e);
		}
		final IOException ioe = new IOException("Unknown I/O error");
		testable.setThrowable(ioe);
		try {
			Functions.accept((t) -> t.test(), testable);
			fail("Expected Exception");
		} catch (UncheckedIOException e) {
			final Throwable t = e.getCause();
			assertNotNull(t);
			assertTrue(t instanceof IOException);
			assertSame(ioe, t);
		}
		testable.setThrowable(null);
		Functions.accept((t) -> t.test(), testable);
	}

	@Test
	void testAcceptBiConsumer() {
		final IllegalStateException ise = new IllegalStateException();
		final Testable testable = new Testable(null);
		try {
			Functions.accept((t1,t2) -> t1.test(t2), testable, ise);
			fail("Expected Exception");
		} catch (IllegalStateException e) {
			assertSame(ise, e);
		}
		final Error error = new OutOfMemoryError();
		try {
			Functions.accept((t1,t2) -> t1.test(t2), testable, error);
		} catch (OutOfMemoryError e) {
			assertSame(error, e);
		}
		final IOException ioe = new IOException("Unknown I/O error");
		testable.setThrowable(ioe);
		try {
			Functions.accept((t1,t2) -> t1.test(t2), testable, ioe);
			fail("Expected Exception");
		} catch (UncheckedIOException e) {
			final Throwable t = e.getCause();
			assertNotNull(t);
			assertTrue(t instanceof IOException);
			assertSame(ioe, t);
		}
		testable.setThrowable(null);
		Functions.accept((t1,t2) -> t1.test(t2), testable, (Throwable) null);
	}

	@Test
	public void testApplyFunction() {
		final IllegalStateException ise = new IllegalStateException();
		final Testable testable = new Testable(ise);
		try {
			Functions.apply((t) -> t.testInt(), testable);
			fail("Expected Exception");
		} catch (IllegalStateException e) {
			assertSame(ise, e);
		}
		final Error error = new OutOfMemoryError();
		testable.setThrowable(error);
		try {
			Functions.apply((t) -> t.testInt(), testable);
		} catch (OutOfMemoryError e) {
			assertSame(error, e);
		}
		final IOException ioe = new IOException("Unknown I/O error");
		testable.setThrowable(ioe);
		try {
			Functions.apply((t) -> t.testInt(), testable);
			fail("Expected Exception");
		} catch (UncheckedIOException e) {
			final Throwable t = e.getCause();
			assertNotNull(t);
			assertTrue(t instanceof IOException);
			assertSame(ioe, t);
		}
		testable.setThrowable(null);
		final Integer i = Functions.apply((t) -> t.testInt(), testable);
		assertNotNull(i);
		assertEquals(0, i.intValue());
	}

	@Test
	public void testApplyBiFunction() {
		final IllegalStateException ise = new IllegalStateException();
		final Testable testable = new Testable(null);
		try {
			Functions.apply((t1,t2) -> t1.testInt(t2), testable, ise);
			fail("Expected Exception");
		} catch (IllegalStateException e) {
			assertSame(ise, e);
		}
		final Error error = new OutOfMemoryError();
		try {
			Functions.apply((t1,t2) -> t1.testInt(t2), testable, error);
		} catch (OutOfMemoryError e) {
			assertSame(error, e);
		}
		final IOException ioe = new IOException("Unknown I/O error");
		try {
			Functions.apply((t1,t2) -> t1.testInt(t2), testable, ioe);
			fail("Expected Exception");
		} catch (UncheckedIOException e) {
			final Throwable t = e.getCause();
			assertNotNull(t);
			assertTrue(t instanceof IOException);
			assertSame(ioe, t);
		}
		final Integer i = Functions.apply((t1,t2) -> t1.testInt(t2), testable, (Throwable) null);
		assertNotNull(i);
		assertEquals(0, i.intValue());
	}

	@Test
	public void testTryWithResources() {
		final CloseableObject co = new CloseableObject();
		final FailableConsumer<Throwable,? extends Throwable> consumer = (th) -> co.run(th);
		final IllegalStateException ise = new IllegalStateException();
		try {
			Functions.tryWithResources(() -> consumer.accept(ise), () -> co.close());
			fail("Expected Exception");
		} catch (IllegalStateException e) {
			assertSame(ise, e);
		}
		assertTrue(co.isClosed());
		co.reset();
		final Error error = new OutOfMemoryError();
		try {
			Functions.tryWithResources(() -> consumer.accept(error), () -> co.close());
			fail("Expected Exception");
		} catch (OutOfMemoryError e) {
			assertSame(error, e);
		}
		assertTrue(co.isClosed());
		co.reset();
		final IOException ioe = new IOException("Unknown I/O error");
		try {
			Functions.tryWithResources(() -> consumer.accept(ioe), () -> co.close());
			fail("Expected Exception");
		} catch (UncheckedIOException e) {
			final IOException cause = e.getCause();
			assertSame(ioe, cause);
		}
		assertTrue(co.isClosed());
		co.reset();
		Functions.tryWithResources(() -> consumer.accept(null), () -> co.close());
		assertTrue(co.isClosed());
	}
}
