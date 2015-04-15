package org.apache.commons.lang3.concurrent;

import org.easymock.EasyMockRunner;
import org.easymock.Mock;
import org.junit.Test;
import org.junit.runner.RunWith;

import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

@RunWith(EasyMockRunner.class)
public class MemoizerTest {

	@Mock
	private Computable<Integer, Integer> computable;

	@Test
	public void testOnlyCallComputableOnceIfDoesNotThrowException() throws Exception {
		Integer input = 1;
		Memoizer<Integer, Integer> memoizer = new Memoizer<Integer, Integer>(computable);
		expect(computable.compute(input)).andReturn(input);
		replay(computable);

		assertEquals("Should call computable first time", input, memoizer.compute(input));
		assertEquals("Should not call the computable the second time", input, memoizer.compute(input));
	}

	@Test(expected = IllegalStateException.class)
	public void testDefaultBehaviourNotToRecalculateExecutionExceptions() throws Exception {
		Integer input = 1;
		Integer answer = 3;
		Memoizer<Integer, Integer> memoizer = new Memoizer<Integer, Integer>(computable);
		InterruptedException interruptedException = new InterruptedException();
		expect(computable.compute(input)).andThrow(interruptedException);
		replay(computable);

		try {
			memoizer.compute(input);
			fail();
		}
		catch (Throwable ex) {
			//Should always be thrown the first time
		}

		memoizer.compute(input);
	}

	@Test(expected = IllegalStateException.class)
	public void testDoesNotRecalculateWhenSetToFalse() throws Exception {
		Integer input = 1;
		Integer answer = 3;
		Memoizer<Integer, Integer> memoizer = new Memoizer<Integer, Integer>(computable, false);
		InterruptedException interruptedException = new InterruptedException();
		expect(computable.compute(input)).andThrow(interruptedException);
		replay(computable);

		try {
			memoizer.compute(input);
			fail();
		}
		catch (Throwable ex) {
			//Should always be thrown the first time
		}

		memoizer.compute(input);
	}

	@Test
	public void testDoesRecalculateWhenSetToTrue() throws Exception {
		Integer input = 1;
		Integer answer = 3;
		Memoizer<Integer, Integer> memoizer = new Memoizer<Integer, Integer>(computable, true);
		InterruptedException interruptedException = new InterruptedException();
		expect(computable.compute(input)).andThrow(interruptedException).andReturn(answer);
		replay(computable);

		try {
			memoizer.compute(input);
			fail();
		}
		catch (Throwable ex) {
			//Should always be thrown the first time
		}

		assertEquals(answer, memoizer.compute(input));
	}


	@Test(expected = RuntimeException.class)
	public void testWhenComputableThrowsRuntimeException() throws Exception {
		Integer input = 1;
		Memoizer<Integer, Integer> memoizer = new Memoizer<Integer, Integer>(computable);
		RuntimeException runtimeException = new RuntimeException("Some runtime exception");
		expect(computable.compute(input)).andThrow(runtimeException);
		replay(computable);

		memoizer.compute(input);
	}

	@Test(expected = Error.class)
	public void testWhenComputableThrowsError() throws Exception {
		Integer input = 1;
		Memoizer<Integer, Integer> memoizer = new Memoizer<Integer, Integer>(computable);
		Error error = new Error();
		expect(computable.compute(input)).andThrow(error);
		replay(computable);

		memoizer.compute(input);
	}
}
