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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.fail;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import org.junit.Test;

/**
 * Test class for {@link ConcurrentUtils}.
 *
 * @version $Id$
 */
public class ConcurrentUtilsTest {
    /**
     * Tests creating a ConcurrentException with a runtime exception as cause.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testConcurrentExceptionCauseUnchecked() {
        new ConcurrentException(new RuntimeException());
    }

    /**
     * Tests creating a ConcurrentException with an error as cause.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testConcurrentExceptionCauseError() {
        new ConcurrentException("An error", new Error());
    }

    /**
     * Tests creating a ConcurrentException with null as cause.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testConcurrentExceptionCauseNull() {
        new ConcurrentException(null);
    }

    /**
     * Tests extractCause() for a null exception.
     */
    @Test
    public void testExtractCauseNull() {
        assertNull("Non null result", ConcurrentUtils.extractCause(null));
    }

    /**
     * Tests extractCause() if the cause of the passed in exception is null.
     */
    @Test
    public void testExtractCauseNullCause() {
        assertNull("Non null result", ConcurrentUtils
                .extractCause(new ExecutionException("Test", null)));
    }

    /**
     * Tests extractCause() if the cause is an error.
     */
    @Test
    public void testExtractCauseError() {
        Error err = new AssertionError("Test");
        try {
            ConcurrentUtils.extractCause(new ExecutionException(err));
            fail("Error not thrown!");
        } catch (Error e) {
            assertEquals("Wrong error", err, e);
        }
    }

    /**
     * Tests extractCause() if the cause is an unchecked exception.
     */
    @Test
    public void testExtractCauseUnchecked() {
        RuntimeException rex = new RuntimeException("Test");
        try {
            ConcurrentUtils.extractCause(new ExecutionException(rex));
            fail("Runtime exception not thrown!");
        } catch (RuntimeException r) {
            assertEquals("Wrong exception", rex, r);
        }
    }

    /**
     * Tests extractCause() if the cause is a checked exception.
     */
    @Test
    public void testExtractCauseChecked() {
        Exception ex = new Exception("Test");
        ConcurrentException cex = ConcurrentUtils
                .extractCause(new ExecutionException(ex));
        assertSame("Wrong cause", ex, cex.getCause());
    }

    /**
     * Tests handleCause() if the cause is an error.
     */
    @Test
    public void testHandleCauseError() throws ConcurrentException {
        Error err = new AssertionError("Test");
        try {
            ConcurrentUtils.handleCause(new ExecutionException(err));
            fail("Error not thrown!");
        } catch (Error e) {
            assertEquals("Wrong error", err, e);
        }
    }

    /**
     * Tests handleCause() if the cause is an unchecked exception.
     */
    @Test
    public void testHandleCauseUnchecked() throws ConcurrentException {
        RuntimeException rex = new RuntimeException("Test");
        try {
            ConcurrentUtils.handleCause(new ExecutionException(rex));
            fail("Runtime exception not thrown!");
        } catch (RuntimeException r) {
            assertEquals("Wrong exception", rex, r);
        }
    }

    /**
     * Tests handleCause() if the cause is a checked exception.
     */
    @Test
    public void testHandleCauseChecked() {
        Exception ex = new Exception("Test");
        try {
            ConcurrentUtils.handleCause(new ExecutionException(ex));
            fail("ConcurrentException not thrown!");
        } catch (ConcurrentException cex) {
            assertEquals("Wrong cause", ex, cex.getCause());
        }
    }

    /**
     * Tests handleCause() for a null parameter or a null cause. In this case
     * the method should do nothing. We can only test that no exception is
     * thrown.
     */
    @Test
    public void testHandleCauseNull() throws ConcurrentException {
        ConcurrentUtils.handleCause(null);
        ConcurrentUtils.handleCause(new ExecutionException("Test", null));
    }

    //-----------------------------------------------------------------------
    /**
     * Tests constant future.
     */
    @Test
    public void testConstantFuture_Integer() throws Exception {
        Integer value = new Integer(5);
        Future<Integer> test = ConcurrentUtils.constantFuture(value);
        assertEquals(true, test.isDone());
        assertSame(value, test.get());
        assertSame(value, test.get(1000, TimeUnit.SECONDS));
        assertSame(value, test.get(1000, null));
        assertEquals(false, test.isCancelled());
        assertEquals(false, test.cancel(true));
        assertEquals(false, test.cancel(false));
    }

    /**
     * Tests constant future.
     */
    @Test
    public void testConstantFuture_null() throws Exception {
        Integer value = null;
        Future<Integer> test = ConcurrentUtils.constantFuture(value);
        assertEquals(true, test.isDone());
        assertSame(value, test.get());
        assertSame(value, test.get(1000, TimeUnit.SECONDS));
        assertSame(value, test.get(1000, null));
        assertEquals(false, test.isCancelled());
        assertEquals(false, test.cancel(true));
        assertEquals(false, test.cancel(false));
    }

}
