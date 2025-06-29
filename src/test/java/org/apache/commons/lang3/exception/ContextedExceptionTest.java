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
package org.apache.commons.lang3.exception;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Date;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * JUnit tests for ContextedException.
 */
class ContextedExceptionTest extends AbstractExceptionContextTest<ContextedException> {

    @BeforeEach
    @Override
    public void setUp() throws Exception {
        exceptionContext = new ContextedException(new Exception(TEST_MESSAGE));
        super.setUp();
    }

    @Test
    void testContextedException() {
        exceptionContext = new ContextedException();
        final String message = exceptionContext.getMessage();
        final String trace = ExceptionUtils.getStackTrace(exceptionContext);
        assertTrue(trace.contains("ContextedException"));
        assertTrue(StringUtils.isEmpty(message));
    }

    @Test
    void testContextedExceptionString() {
        exceptionContext = new ContextedException(TEST_MESSAGE);
        assertEquals(TEST_MESSAGE, exceptionContext.getMessage());

        final String trace = ExceptionUtils.getStackTrace(exceptionContext);
        assertTrue(trace.contains(TEST_MESSAGE));
    }

    @Test
    void testContextedExceptionStringThrowable() {
        exceptionContext = new ContextedException(TEST_MESSAGE_2, new Exception(TEST_MESSAGE));
        final String message = exceptionContext.getMessage();
        final String trace = ExceptionUtils.getStackTrace(exceptionContext);
        assertTrue(trace.contains("ContextedException"));
        assertTrue(trace.contains(TEST_MESSAGE));
        assertTrue(trace.contains(TEST_MESSAGE_2));
        assertTrue(message.contains(TEST_MESSAGE_2));
    }

    @Test
    void testContextedExceptionStringThrowableContext() {
        exceptionContext = new ContextedException(TEST_MESSAGE_2, new Exception(TEST_MESSAGE), new DefaultExceptionContext());
        final String message = exceptionContext.getMessage();
        final String trace = ExceptionUtils.getStackTrace(exceptionContext);
        assertTrue(trace.contains("ContextedException"));
        assertTrue(trace.contains(TEST_MESSAGE));
        assertTrue(trace.contains(TEST_MESSAGE_2));
        assertTrue(message.contains(TEST_MESSAGE_2));
    }

    @Test
    void testContextedExceptionThrowable() {
        exceptionContext = new ContextedException(new Exception(TEST_MESSAGE));
        final String message = exceptionContext.getMessage();
        final String trace = ExceptionUtils.getStackTrace(exceptionContext);
        assertTrue(trace.contains("ContextedException"));
        assertTrue(trace.contains(TEST_MESSAGE));
        assertTrue(message.contains(TEST_MESSAGE));
    }

    @Test
    void testNullException() {
        assertEquals("", ExceptionUtils.getStackTrace(null), "Empty response.");
    }

    @Test
    void testNullExceptionPassing() {
        exceptionContext = new ContextedException(TEST_MESSAGE_2, new Exception(TEST_MESSAGE), null)
        .addContextValue("test1", null)
        .addContextValue("test2", "some value")
        .addContextValue("test Date", new Date())
        .addContextValue("test Nbr", Integer.valueOf(5))
        .addContextValue("test Poorly written obj", new ObjectWithFaultyToString());

        final String message = exceptionContext.getMessage();
        assertNotNull(message);
    }

    @Test
    void testRawMessage() {
        assertEquals(Exception.class.getName() + ": " + TEST_MESSAGE, exceptionContext.getRawMessage());
        exceptionContext = new ContextedException(TEST_MESSAGE_2, new Exception(TEST_MESSAGE), new DefaultExceptionContext());
        assertEquals(TEST_MESSAGE_2, exceptionContext.getRawMessage());
        exceptionContext = new ContextedException(null, new Exception(TEST_MESSAGE), new DefaultExceptionContext());
        assertNull(exceptionContext.getRawMessage());
    }
}
