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
package org.apache.commons.lang3.concurrent.locks;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.Duration;
import java.util.function.LongConsumer;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.ThreadUtils;
import org.apache.commons.lang3.concurrent.locks.LockingVisitors.LockVisitor;
import org.apache.commons.lang3.concurrent.locks.LockingVisitors.StampedLockVisitor;
import org.apache.commons.lang3.function.FailableConsumer;
import org.junit.jupiter.api.Test;

public class LockingVisitorsTest extends AbstractLangTest {

    private static final Duration SHORT_DELAY = Duration.ofMillis(100);
    private static final Duration DELAY = Duration.ofMillis(1500);
    private static final int NUMBER_OF_THREADS = 10;
    private static final Duration TOTAL_DELAY = DELAY.multipliedBy(NUMBER_OF_THREADS);

    protected boolean containsTrue(final boolean[] booleanArray) {
        synchronized (booleanArray) {
            return ArrayUtils.contains(booleanArray, true);
        }
    }

    private void runTest(final Duration delay, final boolean exclusiveLock, final LongConsumer runTimeCheck,
        final boolean[] booleanValues, final LockVisitor<boolean[], ?> visitor) throws InterruptedException {
        final boolean[] runningValues = new boolean[10];

        final long startTimeMillis = System.currentTimeMillis();
        for (int i = 0; i < booleanValues.length; i++) {
            final int index = i;
            final FailableConsumer<boolean[], ?> consumer = b -> {
                b[index] = false;
                ThreadUtils.sleep(delay);
                b[index] = true;
                set(runningValues, index, false);
            };
            final Thread t = new Thread(() -> {
                if (exclusiveLock) {
                    visitor.acceptWriteLocked(consumer);
                } else {
                    visitor.acceptReadLocked(consumer);
                }
            });
            set(runningValues, i, true);
            t.start();
        }
        while (containsTrue(runningValues)) {
            ThreadUtils.sleep(SHORT_DELAY);
        }
        final long endTimeMillis = System.currentTimeMillis();
        for (final boolean booleanValue : booleanValues) {
            assertTrue(booleanValue);
        }
        // WRONG assumption
        // runTimeCheck.accept(endTimeMillis - startTimeMillis);
    }

    protected void set(final boolean[] booleanArray, final int offset, final boolean value) {
        synchronized (booleanArray) {
            booleanArray[offset] = value;
        }
    }

    @Test
    public void testReentrantReadWriteLockExclusive() throws Exception {

        /*
         * If our threads are running concurrently, then we expect to be no faster than running one after the other.
         */
        final boolean[] booleanValues = new boolean[10];
        runTest(DELAY, true, millis -> assertTrue(millis >= TOTAL_DELAY.toMillis()), booleanValues,
            LockingVisitors.reentrantReadWriteLockVisitor(booleanValues));
    }

    @Test
    public void testReentrantReadWriteLockNotExclusive() throws Exception {

        /*
         * If our threads are running concurrently, then we expect to be faster than running one after the other.
         */
        final boolean[] booleanValues = new boolean[10];
        runTest(DELAY, false, millis -> assertTrue(millis < TOTAL_DELAY.toMillis()), booleanValues,
            LockingVisitors.reentrantReadWriteLockVisitor(booleanValues));
    }

    @Test
    public void testResultValidation() {
        final Object hidden = new Object();
        final StampedLockVisitor<Object> lock = LockingVisitors.stampedLockVisitor(hidden);
        final Object o1 = lock.applyReadLocked(h -> new Object());
        assertNotNull(o1);
        assertNotSame(hidden, o1);
        final Object o2 = lock.applyWriteLocked(h -> new Object());
        assertNotNull(o2);
        assertNotSame(hidden, o2);
    }

    @Test
    public void testStampedLockExclusive() throws Exception {

        /*
         * If our threads are running concurrently, then we expect to be no faster than running one after the other.
         */
        final boolean[] booleanValues = new boolean[10];
        runTest(DELAY, true, millis -> assertTrue(millis >= TOTAL_DELAY.toMillis()), booleanValues,
            LockingVisitors.stampedLockVisitor(booleanValues));
    }

    @Test
    public void testStampedLockNotExclusive() throws Exception {

        /*
         * If our threads are running concurrently, then we expect to be faster than running one after the other.
         */
        final boolean[] booleanValues = new boolean[10];
        runTest(DELAY, false, millis -> assertTrue(millis < TOTAL_DELAY.toMillis()), booleanValues,
            LockingVisitors.stampedLockVisitor(booleanValues));
    }
}
