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
package org.apache.commons.lang3.exception;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.apache.commons.lang3.ObjectToStringRuntimeException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * JUnit tests for DefaultExceptionContext.
 */
public class DefaultExceptionContextTest extends AbstractExceptionContextTest<DefaultExceptionContext> {

    @Override
    @BeforeEach
    public void setUp() throws Exception {
        exceptionContext = new DefaultExceptionContext();
        super.setUp();
    }

    @Test
    public void testFormattedExceptionMessageExceptionHandling() {
        exceptionContext = new DefaultExceptionContext();
        final String label1 = "throws 1";
        final String label2 = "throws 2";
        exceptionContext.addContextValue(label1, new ObjectToStringRuntimeException(label1));
        exceptionContext.addContextValue(label2, new ObjectToStringRuntimeException(label2));
        final String message = exceptionContext.getFormattedExceptionMessage(TEST_MESSAGE);
        assertTrue(message.startsWith(TEST_MESSAGE));
        assertTrue(message.contains(label1));
        assertTrue(message.contains(label2));
    }

    @Test
    public void testFormattedExceptionMessageNull() {
        exceptionContext = new DefaultExceptionContext();
        assertEquals("", exceptionContext.getFormattedExceptionMessage(null));
    }

    @Test
    public void testFormattedExceptionMessageNullValue() {
        exceptionContext = new DefaultExceptionContext();
        final String label1 = "throws 1";
        final String label2 = "throws 2";
        exceptionContext.addContextValue(label1, null);
        exceptionContext.addContextValue(label2, null);
        final String message = exceptionContext.getFormattedExceptionMessage(TEST_MESSAGE);
        assertTrue(message.startsWith(TEST_MESSAGE));
        assertTrue(message.contains(label1));
        assertTrue(message.contains(label2));
    }

}
