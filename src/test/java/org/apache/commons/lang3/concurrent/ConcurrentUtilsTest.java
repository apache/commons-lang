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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import org.easymock.EasyMock;
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
     * Tries to create a ConcurrentRuntimeException with a runtime as cause.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testConcurrentRuntimeExceptionCauseUnchecked() {
        new ConcurrentRuntimeException(new RuntimeException());
    }

    /**
     * Tries to create a ConcurrentRuntimeException with an error as cause.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testConcurrentRuntimeExceptionCauseError() {
        new ConcurrentRuntimeException("An error", new Error());
    }

    /**
     * Tries to create a ConcurrentRuntimeException with null as cause.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testConcurrentRuntimeExceptionCauseNull() {
        new ConcurrentRuntimeException(null);
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
    public void testExtractCauseUncheckedException() {
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
     * Tests extractCauseUnchecked() for a null exception.
     */
    @Test
    public void testExtractCauseUncheckedNull() {
        assertNull("Non null result", ConcurrentUtils.extractCauseUnchecked(null));
    }

    /**
     * Tests extractCauseUnchecked() if the cause of the passed in exception is null.
     */
    @Test
    public void testExtractCauseUncheckedNullCause() {
        assertNull("Non null result", ConcurrentUtils
                .extractCauseUnchecked(new ExecutionException("Test", null)));
    }

    /**
     * Tests extractCauseUnchecked() if the cause is an error.
     */
    @Test
    public void testExtractCauseUncheckedError() {
        Error err = new AssertionError("Test");
        try {
            ConcurrentUtils.extractCauseUnchecked(new ExecutionException(err));
            fail("Error not thrown!");
        } catch (Error e) {
            assertEquals("Wrong error", err, e);
        }
    }

    /**
     * Tests extractCauseUnchecked() if the cause is an unchecked exception.
     */
    @Test
    public void testExtractCauseUncheckedUncheckedException() {
        RuntimeException rex = new RuntimeException("Test");
        try {
            ConcurrentUtils.extractCauseUnchecked(new ExecutionException(rex));
            fail("Runtime exception not thrown!");
        } catch (RuntimeException r) {
            assertEquals("Wrong exception", rex, r);
        }
    }

    /**
     * Tests extractCauseUnchecked() if the cause is a checked exception.
     */
    @Test
    public void testExtractCauseUncheckedChecked() {
        Exception ex = new Exception("Test");
        ConcurrentRuntimeException cex = ConcurrentUtils
                .extractCauseUnchecked(new ExecutionException(ex));
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
    public void testHandleCauseUncheckedException() throws ConcurrentException {
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

    /**
     * Tests handleCauseUnchecked() if the cause is an error.
     */
    @Test
    public void testHandleCauseUncheckedError() {
        Error err = new AssertionError("Test");
        try {
            ConcurrentUtils.handleCauseUnchecked(new ExecutionException(err));
            fail("Error not thrown!");
        } catch (Error e) {
            assertEquals("Wrong error", err, e);
        }
    }

    /**
     * Tests handleCauseUnchecked() if the cause is an unchecked exception.
     */
    @Test
    public void testHandleCauseUncheckedUncheckedException() {
        RuntimeException rex = new RuntimeException("Test");
        try {
            ConcurrentUtils.handleCauseUnchecked(new ExecutionException(rex));
            fail("Runtime exception not thrown!");
        } catch (RuntimeException r) {
            assertEquals("Wrong exception", rex, r);
        }
    }

    /**
     * Tests handleCauseUnchecked() if the cause is a checked exception.
     */
    @Test
    public void testHandleCauseUncheckedChecked() {
        Exception ex = new Exception("Test");
        try {
            ConcurrentUtils.handleCauseUnchecked(new ExecutionException(ex));
            fail("ConcurrentRuntimeException not thrown!");
        } catch (ConcurrentRuntimeException crex) {
            assertEquals("Wrong cause", ex, crex.getCause());
        }
    }

    /**
     * Tests handleCauseUnchecked() for a null parameter or a null cause. In
     * this case the method should do nothing. We can only test that no
     * exception is thrown.
     */
    @Test
    public void testHandleCauseUncheckedNull() {
        ConcurrentUtils.handleCauseUnchecked(null);
        ConcurrentUtils.handleCauseUnchecked(new ExecutionException("Test",
                null));
    }

    //-----------------------------------------------------------------------
    /**
     * Tests initialize() for a null argument.
     */
    @Test
    public void testInitializeNull() throws ConcurrentException {
        assertNull("Got a result", ConcurrentUtils.initialize(null));
    }

    /**
     * Tests a successful initialize() operation.
     */
    @Test
    public void testInitialize() throws ConcurrentException {
        @SuppressWarnings("unchecked")
        ConcurrentInitializer<Object> init = EasyMock
                .createMock(ConcurrentInitializer.class);
        final Object result = new Object();
        EasyMock.expect(init.get()).andReturn(result);
        EasyMock.replay(init);
        assertSame("Wrong result object", result, ConcurrentUtils
                .initialize(init));
        EasyMock.verify(init);
    }

    /**
     * Tests initializeUnchecked() for a null argument.
     */
    @Test
    public void testInitializeUncheckedNull() {
        assertNull("Got a result", ConcurrentUtils.initializeUnchecked(null));
    }

    /**
     * Tests a successful initializeUnchecked() operation.
     */
    @Test
    public void testInitializeUnchecked() throws ConcurrentException {
        @SuppressWarnings("unchecked")
        ConcurrentInitializer<Object> init = EasyMock
                .createMock(ConcurrentInitializer.class);
        final Object result = new Object();
        EasyMock.expect(init.get()).andReturn(result);
        EasyMock.replay(init);
        assertSame("Wrong result object", result, ConcurrentUtils
                .initializeUnchecked(init));
        EasyMock.verify(init);
    }

    /**
     * Tests whether exceptions are correctly handled by initializeUnchecked().
     */
    @Test
    public void testInitializeUncheckedEx() throws ConcurrentException {
        @SuppressWarnings("unchecked")
        ConcurrentInitializer<Object> init = EasyMock
                .createMock(ConcurrentInitializer.class);
        final Exception cause = new Exception();
        EasyMock.expect(init.get()).andThrow(new ConcurrentException(cause));
        EasyMock.replay(init);
        try {
            ConcurrentUtils.initializeUnchecked(init);
            fail("Exception not thrown!");
        } catch (ConcurrentRuntimeException crex) {
            assertSame("Wrong cause", cause, crex.getCause());
        }
        EasyMock.verify(init);
    }

    //-----------------------------------------------------------------------
    /**
     * Tests constant future.
     */
    @Test
    public void testConstantFuture_Integer() throws Exception {
        Integer value = Integer.valueOf(5);
        Future<Integer> test = ConcurrentUtils.constantFuture(value);
        assertTrue(test.isDone());
        assertSame(value, test.get());
        assertSame(value, test.get(1000, TimeUnit.SECONDS));
        assertSame(value, test.get(1000, null));
        assertFalse(test.isCancelled());
        assertFalse(test.cancel(true));
        assertFalse(test.cancel(false));
    }

    /**
     * Tests constant future.
     */
    @Test
    public void testConstantFuture_null() throws Exception {
        Integer value = null;
        Future<Integer> test = ConcurrentUtils.constantFuture(value);
        assertTrue(test.isDone());
        assertSame(value, test.get());
        assertSame(value, test.get(1000, TimeUnit.SECONDS));
        assertSame(value, test.get(1000, null));
        assertFalse(test.isCancelled());
        assertFalse(test.cancel(true));
        assertFalse(test.cancel(false));
    }

    //-----------------------------------------------------------------------
    /**
     * Tests putIfAbsent() if the map contains the key in question.
     */
    @Test
    public void testPutIfAbsentKeyPresent() {
        final String key = "testKey";
        final Integer value = 42;
        ConcurrentMap<String, Integer> map = new ConcurrentHashMap<String, Integer>();
        map.put(key, value);
        assertEquals("Wrong result", value,
                ConcurrentUtils.putIfAbsent(map, key, 0));
        assertEquals("Wrong value in map", value, map.get(key));
    }

    /**
     * Tests putIfAbsent() if the map does not contain the key in question.
     */
    @Test
    public void testPutIfAbsentKeyNotPresent() {
        final String key = "testKey";
        final Integer value = 42;
        ConcurrentMap<String, Integer> map = new ConcurrentHashMap<String, Integer>();
        assertEquals("Wrong result", value,
                ConcurrentUtils.putIfAbsent(map, key, value));
        assertEquals("Wrong value in map", value, map.get(key));
    }

    /**
     * Tests putIfAbsent() if a null map is passed in.
     */
    @Test
    public void testPutIfAbsentNullMap() {
        assertNull("Wrong result",
                ConcurrentUtils.putIfAbsent(null, "test", 100));
    }

    /**
     * Tests createIfAbsent() if the key is found in the map.
     */
    @Test
    public void testCreateIfAbsentKeyPresent() throws ConcurrentException {
        @SuppressWarnings("unchecked")
        ConcurrentInitializer<Integer> init = EasyMock
                .createMock(ConcurrentInitializer.class);
        EasyMock.replay(init);
        final String key = "testKey";
        final Integer value = 42;
        ConcurrentMap<String, Integer> map = new ConcurrentHashMap<String, Integer>();
        map.put(key, value);
        assertEquals("Wrong result", value,
                ConcurrentUtils.createIfAbsent(map, key, init));
        assertEquals("Wrong value in map", value, map.get(key));
        EasyMock.verify(init);
    }

    /**
     * Tests createIfAbsent() if the map does not contain the key in question.
     */
    @Test
    public void testCreateIfAbsentKeyNotPresent() throws ConcurrentException {
        @SuppressWarnings("unchecked")
        ConcurrentInitializer<Integer> init = EasyMock
                .createMock(ConcurrentInitializer.class);
        final String key = "testKey";
        final Integer value = 42;
        EasyMock.expect(init.get()).andReturn(value);
        EasyMock.replay(init);
        ConcurrentMap<String, Integer> map = new ConcurrentHashMap<String, Integer>();
        assertEquals("Wrong result", value,
                ConcurrentUtils.createIfAbsent(map, key, init));
        assertEquals("Wrong value in map", value, map.get(key));
        EasyMock.verify(init);
    }

    /**
     * Tests createIfAbsent() if a null map is passed in.
     */
    @Test
    public void testCreateIfAbsentNullMap() throws ConcurrentException {
        @SuppressWarnings("unchecked")
        ConcurrentInitializer<Integer> init = EasyMock
                .createMock(ConcurrentInitializer.class);
        EasyMock.replay(init);
        assertNull("Wrong result",
                ConcurrentUtils.createIfAbsent(null, "test", init));
        EasyMock.verify(init);
    }

    /**
     * Tests createIfAbsent() if a null initializer is passed in.
     */
    @Test
    public void testCreateIfAbsentNullInit() throws ConcurrentException {
        ConcurrentMap<String, Integer> map = new ConcurrentHashMap<String, Integer>();
        final String key = "testKey";
        final Integer value = 42;
        map.put(key, value);
        assertNull("Wrong result",
                ConcurrentUtils.createIfAbsent(map, key, null));
        assertEquals("Map was changed", value, map.get(key));
    }

    /**
     * Tests createIfAbsentUnchecked() if no exception is thrown.
     */
    @Test
    public void testCreateIfAbsentUncheckedSuccess() {
        final String key = "testKey";
        final Integer value = 42;
        ConcurrentMap<String, Integer> map = new ConcurrentHashMap<String, Integer>();
        assertEquals("Wrong result", value,
                ConcurrentUtils.createIfAbsentUnchecked(map, key,
                        new ConstantInitializer<Integer>(value)));
        assertEquals("Wrong value in map", value, map.get(key));
    }

    /**
     * Tests createIfAbsentUnchecked() if an exception is thrown.
     */
    @Test
    public void testCreateIfAbsentUncheckedException()
            throws ConcurrentException {
        @SuppressWarnings("unchecked")
        ConcurrentInitializer<Integer> init = EasyMock
                .createMock(ConcurrentInitializer.class);
        Exception ex = new Exception();
        EasyMock.expect(init.get()).andThrow(new ConcurrentException(ex));
        EasyMock.replay(init);
        try {
            ConcurrentUtils.createIfAbsentUnchecked(
                    new ConcurrentHashMap<String, Integer>(), "test", init);
            fail("Exception not thrown!");
        } catch (ConcurrentRuntimeException crex) {
            assertEquals("Wrong cause", ex, crex.getCause());
        }
        EasyMock.verify(init);
    }
}
