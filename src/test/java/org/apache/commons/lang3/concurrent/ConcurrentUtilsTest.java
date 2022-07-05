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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import org.apache.commons.lang3.AbstractLangTest;
import org.easymock.EasyMock;
import org.junit.jupiter.api.Test;

/**
 * Test class for {@link ConcurrentUtils}.
 */
public class ConcurrentUtilsTest extends AbstractLangTest {
    /**
     * Tests creating a ConcurrentException with a runtime exception as cause.
     */
    @Test
    public void testConcurrentExceptionCauseUnchecked() {
        assertThrows(IllegalArgumentException.class, () -> new ConcurrentException(new RuntimeException()));
    }

    /**
     * Tests creating a ConcurrentException with an error as cause.
     */
    @Test
    public void testConcurrentExceptionCauseError() {
        assertThrows(IllegalArgumentException.class, () -> new ConcurrentException("An error", new Error()));
    }

    /**
     * Tests creating a ConcurrentException with null as cause.
     */
    @Test
    public void testConcurrentExceptionCauseNull() {
        assertThrows(IllegalArgumentException.class, () -> new ConcurrentException(null));
    }

    /**
     * Tries to create a ConcurrentRuntimeException with a runtime as cause.
     */
    @Test
    public void testConcurrentRuntimeExceptionCauseUnchecked() {
        assertThrows(IllegalArgumentException.class, () -> new ConcurrentRuntimeException(new RuntimeException()));
    }

    /**
     * Tries to create a ConcurrentRuntimeException with an error as cause.
     */
    @Test
    public void testConcurrentRuntimeExceptionCauseError() {
        assertThrows(IllegalArgumentException.class, () -> new ConcurrentRuntimeException("An error", new Error()));
    }

    /**
     * Tries to create a ConcurrentRuntimeException with null as cause.
     */
    @Test
    public void testConcurrentRuntimeExceptionCauseNull() {
        assertThrows(IllegalArgumentException.class, () -> new ConcurrentRuntimeException(null));
    }

    /**
     * Tests extractCause() for a null exception.
     */
    @Test
    public void testExtractCauseNull() {
        assertNull(ConcurrentUtils.extractCause(null), "Non null result");
    }

    /**
     * Tests extractCause() if the cause of the passed in exception is null.
     */
    @Test
    public void testExtractCauseNullCause() {
        assertNull(ConcurrentUtils.extractCause(new ExecutionException("Test", null)), "Non null result");
    }

    /**
     * Tests extractCause() if the cause is an error.
     */
    @Test
    public void testExtractCauseError() {
        final Error err = new AssertionError("Test");
        final AssertionError e = assertThrows(AssertionError.class, () -> ConcurrentUtils.extractCause(new ExecutionException(err)));
        assertEquals(err, e, "Wrong error");
    }

    /**
     * Tests extractCause() if the cause is an unchecked exception.
     */
    @Test
    public void testExtractCauseUncheckedException() {
        final RuntimeException rex = new RuntimeException("Test");
        assertThrows(RuntimeException.class, () -> ConcurrentUtils.extractCause(new ExecutionException(rex)));
    }

    /**
     * Tests extractCause() if the cause is a checked exception.
     */
    @Test
    public void testExtractCauseChecked() {
        final Exception ex = new Exception("Test");
        final ConcurrentException cex = ConcurrentUtils.extractCause(new ExecutionException(ex));
        assertSame(ex, cex.getCause(), "Wrong cause");
    }

    /**
     * Tests extractCauseUnchecked() for a null exception.
     */
    @Test
    public void testExtractCauseUncheckedNull() {
        assertNull(ConcurrentUtils.extractCauseUnchecked(null), "Non null result");
    }

    /**
     * Tests extractCauseUnchecked() if the cause of the passed in exception is null.
     */
    @Test
    public void testExtractCauseUncheckedNullCause() {
        assertNull(ConcurrentUtils.extractCauseUnchecked(new ExecutionException("Test", null)), "Non null result");
    }

    /**
     * Tests extractCauseUnchecked() if the cause is an error.
     */
    @Test
    public void testExtractCauseUncheckedError() {
        final Error err = new AssertionError("Test");
        final Error e = assertThrows(Error.class, () -> ConcurrentUtils.extractCauseUnchecked(new ExecutionException(err)));
        assertEquals(err, e, "Wrong error");
    }

    /**
     * Tests extractCauseUnchecked() if the cause is an unchecked exception.
     */
    @Test
    public void testExtractCauseUncheckedUncheckedException() {
        final RuntimeException rex = new RuntimeException("Test");
        final RuntimeException r = assertThrows(RuntimeException.class, () -> ConcurrentUtils.extractCauseUnchecked(new ExecutionException(rex)));
        assertEquals(rex, r, "Wrong exception");
    }

    /**
     * Tests extractCauseUnchecked() if the cause is a checked exception.
     */
    @Test
    public void testExtractCauseUncheckedChecked() {
        final Exception ex = new Exception("Test");
        final ConcurrentRuntimeException cex = ConcurrentUtils.extractCauseUnchecked(new ExecutionException(ex));
        assertSame(ex, cex.getCause(), "Wrong cause");
    }

    /**
     * Tests handleCause() if the cause is an error.
     */
    @Test
    public void testHandleCauseError() {
        final Error err = new AssertionError("Test");
        final Error e = assertThrows(Error.class, () -> ConcurrentUtils.handleCause(new ExecutionException(err)));
        assertEquals(err, e, "Wrong error");
    }

    /**
     * Tests handleCause() if the cause is an unchecked exception.
     */
    @Test
    public void testHandleCauseUncheckedException() {
        final RuntimeException rex = new RuntimeException("Test");
        final RuntimeException r = assertThrows(RuntimeException.class, () -> ConcurrentUtils.handleCause(new ExecutionException(rex)));
        assertEquals(rex, r, "Wrong exception");
    }

    /**
     * Tests handleCause() if the cause is a checked exception.
     */
    @Test
    public void testHandleCauseChecked() {
        final Exception ex = new Exception("Test");
        final ConcurrentException cex = assertThrows(ConcurrentException.class, () -> ConcurrentUtils.handleCause(new ExecutionException(ex)));
        assertEquals(ex, cex.getCause(), "Wrong cause");
    }

    /**
     * Tests handleCause() for a null parameter or a null cause. In this case the method should do nothing. We can only test
     * that no exception is thrown.
     *
     * @throws org.apache.commons.lang3.concurrent.ConcurrentException so we don't have to catch it
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
        final Error err = new AssertionError("Test");
        final Error e = assertThrows(Error.class, () -> ConcurrentUtils.handleCauseUnchecked(new ExecutionException(err)));
        assertEquals(err, e, "Wrong error");
    }

    /**
     * Tests handleCauseUnchecked() if the cause is an unchecked exception.
     */
    @Test
    public void testHandleCauseUncheckedUncheckedException() {
        final RuntimeException rex = new RuntimeException("Test");
        final RuntimeException r = assertThrows(RuntimeException.class, () -> ConcurrentUtils.handleCauseUnchecked(new ExecutionException(rex)));
        assertEquals(rex, r, "Wrong exception");
    }

    /**
     * Tests handleCauseUnchecked() if the cause is a checked exception.
     */
    @Test
    public void testHandleCauseUncheckedChecked() {
        final Exception ex = new Exception("Test");
        final ConcurrentRuntimeException crex = assertThrows(ConcurrentRuntimeException.class,
            () -> ConcurrentUtils.handleCauseUnchecked(new ExecutionException(ex)));
        assertEquals(ex, crex.getCause(), "Wrong cause");
    }

    /**
     * Tests handleCauseUnchecked() for a null parameter or a null cause. In this case the method should do nothing. We can
     * only test that no exception is thrown.
     */
    @Test
    public void testHandleCauseUncheckedNull() {
        ConcurrentUtils.handleCauseUnchecked(null);
        ConcurrentUtils.handleCauseUnchecked(new ExecutionException("Test", null));
    }

    /**
     * Tests initialize() for a null argument.
     *
     * @throws org.apache.commons.lang3.concurrent.ConcurrentException so we don't have to catch it
     */
    @Test
    public void testInitializeNull() throws ConcurrentException {
        assertNull(ConcurrentUtils.initialize(null), "Got a result");
    }

    /**
     * Tests a successful initialize() operation.
     *
     * @throws org.apache.commons.lang3.concurrent.ConcurrentException so we don't have to catch it
     */
    @Test
    public void testInitialize() throws ConcurrentException {
        final ConcurrentInitializer<Object> init = EasyMock.createMock(ConcurrentInitializer.class);
        final Object result = new Object();
        EasyMock.expect(init.get()).andReturn(result);
        EasyMock.replay(init);
        assertSame(result, ConcurrentUtils.initialize(init), "Wrong result object");
        EasyMock.verify(init);
    }

    /**
     * Tests initializeUnchecked() for a null argument.
     */
    @Test
    public void testInitializeUncheckedNull() {
        assertNull(ConcurrentUtils.initializeUnchecked(null), "Got a result");
    }

    /**
     * Tests creating ConcurrentRuntimeException with no arguments.
     */
    @Test
    public void testUninitializedConcurrentRuntimeException() {
        assertNotNull(new ConcurrentRuntimeException(), "Error creating empty ConcurrentRuntimeException");
    }

    /**
     * Tests a successful initializeUnchecked() operation.
     *
     * @throws org.apache.commons.lang3.concurrent.ConcurrentException so we don't have to catch it
     */
    @Test
    public void testInitializeUnchecked() throws ConcurrentException {
        final ConcurrentInitializer<Object> init = EasyMock.createMock(ConcurrentInitializer.class);
        final Object result = new Object();
        EasyMock.expect(init.get()).andReturn(result);
        EasyMock.replay(init);
        assertSame(result, ConcurrentUtils.initializeUnchecked(init), "Wrong result object");
        EasyMock.verify(init);
    }

    /**
     * Tests whether exceptions are correctly handled by initializeUnchecked().
     *
     * @throws org.apache.commons.lang3.concurrent.ConcurrentException so we don't have to catch it
     */
    @Test
    public void testInitializeUncheckedEx() throws ConcurrentException {
        final ConcurrentInitializer<Object> init = EasyMock.createMock(ConcurrentInitializer.class);
        final Exception cause = new Exception();
        EasyMock.expect(init.get()).andThrow(new ConcurrentException(cause));
        EasyMock.replay(init);
        final ConcurrentRuntimeException crex = assertThrows(ConcurrentRuntimeException.class, () -> ConcurrentUtils.initializeUnchecked(init));
        assertSame(cause, crex.getCause(), "Wrong cause");
        EasyMock.verify(init);
    }

    /**
     * Tests constant future.
     *
     * @throws Exception so we don't have to catch it
     */
    @Test
    public void testConstantFuture_Integer() throws Exception {
        final Integer value = Integer.valueOf(5);
        final Future<Integer> test = ConcurrentUtils.constantFuture(value);
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
     *
     * @throws Exception so we don't have to catch it
     */
    @Test
    public void testConstantFuture_null() throws Exception {
        final Integer value = null;
        final Future<Integer> test = ConcurrentUtils.constantFuture(value);
        assertTrue(test.isDone());
        assertSame(value, test.get());
        assertSame(value, test.get(1000, TimeUnit.SECONDS));
        assertSame(value, test.get(1000, null));
        assertFalse(test.isCancelled());
        assertFalse(test.cancel(true));
        assertFalse(test.cancel(false));
    }

    /**
     * Tests putIfAbsent() if the map contains the key in question.
     */
    @Test
    public void testPutIfAbsentKeyPresent() {
        final String key = "testKey";
        final Integer value = 42;
        final ConcurrentMap<String, Integer> map = new ConcurrentHashMap<>();
        map.put(key, value);
        assertEquals(value, ConcurrentUtils.putIfAbsent(map, key, 0), "Wrong result");
        assertEquals(value, map.get(key), "Wrong value in map");
    }

    /**
     * Tests putIfAbsent() if the map does not contain the key in question.
     */
    @Test
    public void testPutIfAbsentKeyNotPresent() {
        final String key = "testKey";
        final Integer value = 42;
        final ConcurrentMap<String, Integer> map = new ConcurrentHashMap<>();
        assertEquals(value, ConcurrentUtils.putIfAbsent(map, key, value), "Wrong result");
        assertEquals(value, map.get(key), "Wrong value in map");
    }

    /**
     * Tests putIfAbsent() if a null map is passed in.
     */
    @Test
    public void testPutIfAbsentNullMap() {
        assertNull(ConcurrentUtils.putIfAbsent(null, "test", 100), "Wrong result");
    }

    /**
     * Tests createIfAbsent() if the key is found in the map.
     *
     * @throws org.apache.commons.lang3.concurrent.ConcurrentException so we don't have to catch it
     */
    @Test
    public void testCreateIfAbsentKeyPresent() throws ConcurrentException {
        final ConcurrentInitializer<Integer> init = EasyMock.createMock(ConcurrentInitializer.class);
        EasyMock.replay(init);
        final String key = "testKey";
        final Integer value = 42;
        final ConcurrentMap<String, Integer> map = new ConcurrentHashMap<>();
        map.put(key, value);
        assertEquals(value, ConcurrentUtils.createIfAbsent(map, key, init), "Wrong result");
        assertEquals(value, map.get(key), "Wrong value in map");
        EasyMock.verify(init);
    }

    /**
     * Tests createIfAbsent() if the map does not contain the key in question.
     *
     * @throws org.apache.commons.lang3.concurrent.ConcurrentException so we don't have to catch it
     */
    @Test
    public void testCreateIfAbsentKeyNotPresent() throws ConcurrentException {
        final ConcurrentInitializer<Integer> init = EasyMock.createMock(ConcurrentInitializer.class);
        final String key = "testKey";
        final Integer value = 42;
        EasyMock.expect(init.get()).andReturn(value);
        EasyMock.replay(init);
        final ConcurrentMap<String, Integer> map = new ConcurrentHashMap<>();
        assertEquals(value, ConcurrentUtils.createIfAbsent(map, key, init), "Wrong result");
        assertEquals(value, map.get(key), "Wrong value in map");
        EasyMock.verify(init);
    }

    /**
     * Tests createIfAbsent() if a null map is passed in.
     *
     * @throws org.apache.commons.lang3.concurrent.ConcurrentException so we don't have to catch it
     */
    @Test
    public void testCreateIfAbsentNullMap() throws ConcurrentException {
        final ConcurrentInitializer<Integer> init = EasyMock.createMock(ConcurrentInitializer.class);
        EasyMock.replay(init);
        assertNull(ConcurrentUtils.createIfAbsent(null, "test", init), "Wrong result");
        EasyMock.verify(init);
    }

    /**
     * Tests createIfAbsent() if a null initializer is passed in.
     *
     * @throws org.apache.commons.lang3.concurrent.ConcurrentException so we don't have to catch it
     */
    @Test
    public void testCreateIfAbsentNullInit() throws ConcurrentException {
        final ConcurrentMap<String, Integer> map = new ConcurrentHashMap<>();
        final String key = "testKey";
        final Integer value = 42;
        map.put(key, value);
        assertNull(ConcurrentUtils.createIfAbsent(map, key, null), "Wrong result");
        assertEquals(value, map.get(key), "Map was changed");
    }

    /**
     * Tests createIfAbsentUnchecked() if no exception is thrown.
     */
    @Test
    public void testCreateIfAbsentUncheckedSuccess() {
        final String key = "testKey";
        final Integer value = 42;
        final ConcurrentMap<String, Integer> map = new ConcurrentHashMap<>();
        assertEquals(value, ConcurrentUtils.createIfAbsentUnchecked(map, key, new ConstantInitializer<>(value)), "Wrong result");
        assertEquals(value, map.get(key), "Wrong value in map");
    }

    /**
     * Tests createIfAbsentUnchecked() if an exception is thrown.
     *
     * @throws org.apache.commons.lang3.concurrent.ConcurrentException so we don't have to catch it
     */
    @Test
    public void testCreateIfAbsentUncheckedException() throws ConcurrentException {
        final ConcurrentInitializer<Integer> init = EasyMock.createMock(ConcurrentInitializer.class);
        final Exception ex = new Exception();
        EasyMock.expect(init.get()).andThrow(new ConcurrentException(ex));
        EasyMock.replay(init);
        final ConcurrentRuntimeException crex = assertThrows(ConcurrentRuntimeException.class,
            () -> ConcurrentUtils.createIfAbsentUnchecked(new ConcurrentHashMap<>(), "test", init));
        assertEquals(ex, crex.getCause(), "Wrong cause");
        EasyMock.verify(init);
    }
}
