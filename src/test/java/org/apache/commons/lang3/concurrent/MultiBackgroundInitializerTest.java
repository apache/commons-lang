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

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import junit.framework.TestCase;

/**
 * Test class for {@link MultiBackgroundInitializer}.
 *
 * @version $Id$
 */
public class MultiBackgroundInitializerTest extends TestCase {
    /** Constant for the names of the child initializers. */
    private static final String CHILD_INIT = "childInitializer";

    /** The initializer to be tested. */
    private MultiBackgroundInitializer initializer;

    @Override
    protected void setUp() throws Exception {
        super.setUp();
        initializer = new MultiBackgroundInitializer();
    }

    /**
     * Tests whether a child initializer has been executed. Optionally the
     * expected executor service can be checked, too.
     *
     * @param child the child initializer
     * @param expExec the expected executor service (null if the executor should
     * not be checked)
     * @throws ConcurrentException if an error occurs
     */
    private void checkChild(BackgroundInitializer<?> child,
            ExecutorService expExec) throws ConcurrentException {
        ChildBackgroundInitializer cinit = (ChildBackgroundInitializer) child;
        Integer result = cinit.get();
        assertEquals("Wrong result", 1, result.intValue());
        assertEquals("Wrong number of executions", 1, cinit.initializeCalls);
        if (expExec != null) {
            assertEquals("Wrong executor service", expExec,
                    cinit.currentExecutor);
        }
    }

    /**
     * Tests addInitializer() if a null name is passed in. This should cause an
     * exception.
     */
    public void testAddInitializerNullName() {
        try {
            initializer.addInitializer(null, new ChildBackgroundInitializer());
            fail("Null name not detected!");
        } catch (IllegalArgumentException iex) {
            // ok
        }
    }

    /**
     * Tests addInitializer() if a null initializer is passed in. This should
     * cause an exception.
     */
    public void testAddInitializerNullInit() {
        try {
            initializer.addInitializer(CHILD_INIT, null);
            fail("Could add null initializer!");
        } catch (IllegalArgumentException iex) {
            // ok
        }
    }

    /**
     * Tests the background processing if there are no child initializers.
     */
    public void testInitializeNoChildren() throws ConcurrentException {
        assertTrue("Wrong result of start()", initializer.start());
        MultiBackgroundInitializer.MultiBackgroundInitializerResults res = initializer
                .get();
        assertTrue("Got child initializers", res.initializerNames().isEmpty());
        assertTrue("Executor not shutdown", initializer.getActiveExecutor()
                .isShutdown());
    }

    /**
     * Helper method for testing the initialize() method. This method can
     * operate with both an external and a temporary executor service.
     *
     * @return the result object produced by the initializer
     */
    private MultiBackgroundInitializer.MultiBackgroundInitializerResults checkInitialize()
            throws ConcurrentException {
        final int count = 5;
        for (int i = 0; i < count; i++) {
            initializer.addInitializer(CHILD_INIT + i,
                    new ChildBackgroundInitializer());
        }
        initializer.start();
        MultiBackgroundInitializer.MultiBackgroundInitializerResults res = initializer
                .get();
        assertEquals("Wrong number of child initializers", count, res
                .initializerNames().size());
        for (int i = 0; i < count; i++) {
            String key = CHILD_INIT + i;
            assertTrue("Name not found: " + key, res.initializerNames()
                    .contains(key));
            assertEquals("Wrong result object", Integer.valueOf(1), res
                    .getResultObject(key));
            assertFalse("Exception flag", res.isException(key));
            assertNull("Got an exception", res.getException(key));
            checkChild(res.getInitializer(key), initializer.getActiveExecutor());
        }
        return res;
    }

    /**
     * Tests background processing if a temporary executor is used.
     */
    public void testInitializeTempExec() throws ConcurrentException {
        checkInitialize();
        assertTrue("Executor not shutdown", initializer.getActiveExecutor()
                .isShutdown());
    }

    /**
     * Tests background processing if an external executor service is provided.
     */
    public void testInitializeExternalExec() throws ConcurrentException {
        ExecutorService exec = Executors.newCachedThreadPool();
        try {
            initializer = new MultiBackgroundInitializer(exec);
            checkInitialize();
            assertEquals("Wrong executor", exec, initializer
                    .getActiveExecutor());
            assertFalse("Executor was shutdown", exec.isShutdown());
        } finally {
            exec.shutdown();
        }
    }

    /**
     * Tests the behavior of initialize() if a child initializer has a specific
     * executor service. Then this service should not be overridden.
     */
    public void testInitializeChildWithExecutor() throws ConcurrentException {
        final String initExec = "childInitializerWithExecutor";
        ExecutorService exec = Executors.newSingleThreadExecutor();
        try {
            ChildBackgroundInitializer c1 = new ChildBackgroundInitializer();
            ChildBackgroundInitializer c2 = new ChildBackgroundInitializer();
            c2.setExternalExecutor(exec);
            initializer.addInitializer(CHILD_INIT, c1);
            initializer.addInitializer(initExec, c2);
            initializer.start();
            initializer.get();
            checkChild(c1, initializer.getActiveExecutor());
            checkChild(c2, exec);
        } finally {
            exec.shutdown();
        }
    }

    /**
     * Tries to add another child initializer after the start() method has been
     * called. This should not be allowed.
     */
    public void testAddInitializerAfterStart() throws ConcurrentException {
        initializer.start();
        try {
            initializer.addInitializer(CHILD_INIT,
                    new ChildBackgroundInitializer());
            fail("Could add initializer after start()!");
        } catch (IllegalStateException istex) {
            initializer.get();
        }
    }

    /**
     * Tries to query an unknown child initializer from the results object. This
     * should cause an exception.
     */
    public void testResultGetInitializerUnknown() throws ConcurrentException {
        MultiBackgroundInitializer.MultiBackgroundInitializerResults res = checkInitialize();
        try {
            res.getInitializer("unknown");
            fail("Could obtain unknown child initializer!");
        } catch (NoSuchElementException nex) {
            // ok
        }
    }

    /**
     * Tries to query the results of an unknown child initializer from the
     * results object. This should cause an exception.
     */
    public void testResultGetResultObjectUnknown() throws ConcurrentException {
        MultiBackgroundInitializer.MultiBackgroundInitializerResults res = checkInitialize();
        try {
            res.getResultObject("unknown");
            fail("Could obtain results from unknown child initializer!");
        } catch (NoSuchElementException nex) {
            // ok
        }
    }

    /**
     * Tries to query the exception of an unknown child initializer from the
     * results object. This should cause an exception.
     */
    public void testResultGetExceptionUnknown() throws ConcurrentException {
        MultiBackgroundInitializer.MultiBackgroundInitializerResults res = checkInitialize();
        try {
            res.getException("unknown");
            fail("Could obtain exception from unknown child initializer!");
        } catch (NoSuchElementException nex) {
            // ok
        }
    }

    /**
     * Tries to query the exception flag of an unknown child initializer from
     * the results object. This should cause an exception.
     */
    public void testResultIsExceptionUnknown() throws ConcurrentException {
        MultiBackgroundInitializer.MultiBackgroundInitializerResults res = checkInitialize();
        try {
            res.isException("unknown");
            fail("Could obtain exception status from unknown child initializer!");
        } catch (NoSuchElementException nex) {
            // ok
        }
    }

    /**
     * Tests that the set with the names of the initializers cannot be modified.
     */
    public void testResultInitializerNamesModify() throws ConcurrentException {
        checkInitialize();
        MultiBackgroundInitializer.MultiBackgroundInitializerResults res = initializer
                .get();
        Iterator<String> it = res.initializerNames().iterator();
        it.next();
        try {
            it.remove();
            fail("Could modify set with initializer names!");
        } catch (UnsupportedOperationException uex) {
            // ok
        }
    }

    /**
     * Tests the behavior of the initializer if one of the child initializers
     * throws a runtime exception.
     */
    public void testInitializeRuntimeEx() {
        ChildBackgroundInitializer child = new ChildBackgroundInitializer();
        child.ex = new RuntimeException();
        initializer.addInitializer(CHILD_INIT, child);
        initializer.start();
        try {
            initializer.get();
            fail("Runtime exception not thrown!");
        } catch (Exception ex) {
            assertEquals("Wrong exception", child.ex, ex);
        }
    }

    /**
     * Tests the behavior of the initializer if one of the child initializers
     * throws a checked exception.
     */
    public void testInitializeEx() throws ConcurrentException {
        ChildBackgroundInitializer child = new ChildBackgroundInitializer();
        child.ex = new Exception();
        initializer.addInitializer(CHILD_INIT, child);
        initializer.start();
        MultiBackgroundInitializer.MultiBackgroundInitializerResults res = initializer
                .get();
        assertTrue("No exception flag", res.isException(CHILD_INIT));
        assertNull("Got a results object", res.getResultObject(CHILD_INIT));
        ConcurrentException cex = res.getException(CHILD_INIT);
        assertEquals("Wrong cause", child.ex, cex.getCause());
    }

    /**
     * Tests whether MultiBackgroundInitializers can be combined in a nested
     * way.
     */
    public void testInitializeNested() throws ConcurrentException {
        final String nameMulti = "multiChildInitializer";
        initializer
                .addInitializer(CHILD_INIT, new ChildBackgroundInitializer());
        MultiBackgroundInitializer mi2 = new MultiBackgroundInitializer();
        final int count = 3;
        for (int i = 0; i < count; i++) {
            mi2
                    .addInitializer(CHILD_INIT + i,
                            new ChildBackgroundInitializer());
        }
        initializer.addInitializer(nameMulti, mi2);
        initializer.start();
        MultiBackgroundInitializer.MultiBackgroundInitializerResults res = initializer
                .get();
        ExecutorService exec = initializer.getActiveExecutor();
        checkChild(res.getInitializer(CHILD_INIT), exec);
        MultiBackgroundInitializer.MultiBackgroundInitializerResults res2 = (MultiBackgroundInitializer.MultiBackgroundInitializerResults) res
                .getResultObject(nameMulti);
        assertEquals("Wrong number of initializers", count, res2
                .initializerNames().size());
        for (int i = 0; i < count; i++) {
            checkChild(res2.getInitializer(CHILD_INIT + i), exec);
        }
        assertTrue("Executor not shutdown", exec.isShutdown());
    }

    /**
     * A concrete implementation of {@code BackgroundInitializer} used for
     * defining background tasks for {@code MultiBackgroundInitializer}.
     */
    private static class ChildBackgroundInitializer extends
            BackgroundInitializer<Integer> {
        /** Stores the current executor service. */
        volatile ExecutorService currentExecutor;

        /** A counter for the invocations of initialize(). */
        volatile int initializeCalls;

        /** An exception to be thrown by initialize(). */
        Exception ex;

        /**
         * Records this invocation. Optionally throws an exception.
         */
        @Override
        protected Integer initialize() throws Exception {
            currentExecutor = getActiveExecutor();
            initializeCalls++;

            if (ex != null) {
                throw ex;
            }

            return initializeCalls;
        }
    }
}
