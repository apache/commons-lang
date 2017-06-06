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

import org.apache.commons.lang3.exception.AbstractExceptionTest;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;


/**
 * JUnit tests for {@link CircuitBreakingException}.
 */
public class CircuitBreakingExceptionTest extends AbstractExceptionTest {

    @Test(expected = CircuitBreakingException.class)
    public void testThrowingInformativeException() throws Exception {
        throw new CircuitBreakingException(EXCEPTION_MESSAGE, generateCause());
    }

    @Test(expected = CircuitBreakingException.class)
    public void testThrowingExceptionWithMessage() throws Exception {
        throw new CircuitBreakingException(EXCEPTION_MESSAGE);
    }

    @Test(expected = CircuitBreakingException.class)
    public void testThrowingExceptionWithCause() throws Exception {
        throw new CircuitBreakingException(generateCause());
    }

    @Test(expected = CircuitBreakingException.class)
    public void testThrowingEmptyException() throws Exception {
        throw new CircuitBreakingException();
    }

    @Test
    public void testWithCauseAndMessage() throws Exception {
        final Exception exception = new CircuitBreakingException(EXCEPTION_MESSAGE, generateCause());
        assertNotNull(exception);
        assertEquals(WRONG_EXCEPTION_MESSAGE, EXCEPTION_MESSAGE, exception.getMessage());

        final Throwable cause = exception.getCause();
        assertNotNull(cause);
        assertEquals(WRONG_CAUSE_MESSAGE, CAUSE_MESSAGE, cause.getMessage());
    }

    @Test
    public void testWithoutCause() throws Exception {
        final Exception exception = new CircuitBreakingException(EXCEPTION_MESSAGE);
        assertNotNull(exception);
        assertEquals(WRONG_EXCEPTION_MESSAGE, EXCEPTION_MESSAGE, exception.getMessage());

        final Throwable cause = exception.getCause();
        assertNull(cause);
    }

    @Test
    public void testWithoutMessage() throws Exception {
        final Exception exception = new CircuitBreakingException(generateCause());
        assertNotNull(exception);
        assertNotNull(exception.getMessage());

        final Throwable cause = exception.getCause();
        assertNotNull(cause);
        assertEquals(WRONG_CAUSE_MESSAGE, CAUSE_MESSAGE, cause.getMessage());
    }
}
