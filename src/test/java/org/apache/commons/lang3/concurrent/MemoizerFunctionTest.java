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
package org.apache.commons.lang3.concurrent;

import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.function.Function;

import org.apache.commons.lang3.AbstractLangTest;
import org.easymock.EasyMock;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class MemoizerFunctionTest extends AbstractLangTest {

    private Function<Integer, Integer> function;

    @BeforeEach
    public void setUpComputableMock() {
        function = EasyMock.mock(Function.class);
    }

    @Test
    public void testDefaultBehaviourNotToRecalculateExecutionExceptions() throws Exception {
        final Integer input = 1;
        final Memoizer<Integer, Integer> memoizer = new Memoizer<>(function);
        final IllegalArgumentException interruptedException = new IllegalArgumentException();
        expect(function.apply(input)).andThrow(interruptedException);
        replay(function);

        assertThrows(Throwable.class, () -> memoizer.compute(input));
        assertThrows(IllegalArgumentException.class, () -> memoizer.compute(input));
    }

    @Test
    public void testDoesNotRecalculateWhenSetToFalse() throws Exception {
        final Integer input = 1;
        final Memoizer<Integer, Integer> memoizer = new Memoizer<>(function, false);
        final IllegalArgumentException interruptedException = new IllegalArgumentException();
        expect(function.apply(input)).andThrow(interruptedException);
        replay(function);

        assertThrows(Throwable.class, () -> memoizer.compute(input));
        assertThrows(IllegalArgumentException.class, () -> memoizer.compute(input));
    }

    @Test
    public void testDoesRecalculateWhenSetToTrue() throws Exception {
        final Integer input = 1;
        final Integer answer = 3;
        final Memoizer<Integer, Integer> memoizer = new Memoizer<>(function, true);
        final IllegalArgumentException interruptedException = new IllegalArgumentException();
        expect(function.apply(input)).andThrow(interruptedException).andReturn(answer);
        replay(function);

        assertThrows(Throwable.class, () -> memoizer.compute(input));
        assertEquals(answer, memoizer.compute(input));
    }

    @Test
    public void testOnlyCallComputableOnceIfDoesNotThrowException() throws Exception {
        final Integer input = 1;
        final Memoizer<Integer, Integer> memoizer = new Memoizer<>(function);
        expect(function.apply(input)).andReturn(input);
        replay(function);

        assertEquals(input, memoizer.compute(input), "Should call computable first time");
        assertEquals(input, memoizer.compute(input), "Should not call the computable the second time");
    }

    @Test
    public void testWhenComputableThrowsError() throws Exception {
        final Integer input = 1;
        final Memoizer<Integer, Integer> memoizer = new Memoizer<>(function);
        final Error error = new Error();
        expect(function.apply(input)).andThrow(error);
        replay(function);

        assertThrows(Error.class, () -> memoizer.compute(input));
    }

    @Test
    public void testWhenComputableThrowsRuntimeException() throws Exception {
        final Integer input = 1;
        final Memoizer<Integer, Integer> memoizer = new Memoizer<>(function);
        final RuntimeException runtimeException = new RuntimeException("Some runtime exception");
        expect(function.apply(input)).andThrow(runtimeException);
        replay(function);

        assertThrows(RuntimeException.class, () -> memoizer.compute(input));
    }
}
