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
package org.apache.commons.lang3.concurrent.lock;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.function.LongConsumer;

import org.apache.commons.lang3.concurrent.lock.Locks.Lock;
import org.apache.commons.lang3.function.FailableConsumer;
import org.junit.jupiter.api.Test;

public class LocksTest {
    private static final int NUMBER_OF_THREADS = 10;

    @Test
    public void testReadLock() throws Exception {
        final long DELAY=3000;
        /** If our threads are running concurrently, then we expect to be faster
         * than running one after the other.
         */
        runTest(DELAY, false, l -> assertTrue(l < NUMBER_OF_THREADS*DELAY));
    }

    @Test
    public void testWriteLock() throws Exception {
        final long DELAY = 100;
        /** If our threads are running concurrently, then we expect to be no faster
         * than running one after the other.
         */
        runTest(DELAY, true, l -> assertTrue(l >= NUMBER_OF_THREADS*DELAY));
    }

    @Test
    public void testResultValidation() {
        final Object hidden = new Object();
        final Lock<Object> lock = Locks.lock(hidden);
        final Object o1 = lock.applyReadLocked((h) -> {
            return new Object();
        });
        assertNotNull(o1);
        assertNotSame(hidden, o1);
        final Object o2 = lock.applyWriteLocked((h) -> {
            return new Object();
        });
        assertNotNull(o2);
        assertNotSame(hidden, o2);
        try {
            lock.applyReadLocked((h) -> {
                return hidden;
            });
            fail("Expected Exception");
        } catch (IllegalStateException e) {
            assertEquals("The returned object is, in fact, the hidden object.", e.getMessage());
        }
        try {
            lock.applyReadLocked((h) -> {
                return hidden;
            });
            fail("Expected Exception");
        } catch (IllegalStateException e) {
            assertEquals("The returned object is, in fact, the hidden object.", e.getMessage());
        }
    }

    private void runTest(final long delay, final boolean exclusiveLock, final LongConsumer runTimeCheck) throws InterruptedException {
        final boolean[] booleanValues = new boolean[10];
        final Lock<boolean[]> lock = Locks.lock(booleanValues);
        final boolean[] runningValues = new boolean[10];

        final long startTime = System.currentTimeMillis();
        for (int i = 0;  i < booleanValues.length;  i++) {
            final int index = i;
            final FailableConsumer<boolean[], ?> consumer = b -> {
                b[index] = false;
                Thread.sleep(delay);
                b[index] = true;
                modify(runningValues, index, false);
            };
            final Thread t = new Thread(() -> {
                if (exclusiveLock) {
                    lock.acceptWriteLocked(consumer);
                } else {
                    lock.acceptReadLocked(consumer);
                }
            });
            modify(runningValues, i, true);
            t.start();
        }
        while (someValueIsTrue(runningValues)) {
            Thread.sleep(100);
        }
        final long endTime = System.currentTimeMillis();
        for (int i = 0;  i < booleanValues.length;  i++) {
            assertTrue(booleanValues[i]);
        }
        runTimeCheck.accept(endTime-startTime);
    }

    protected void modify(final boolean[] booleanArray, final int offset, final boolean value) {
        synchronized(booleanArray) {
            booleanArray[offset] = value;
        }
    }

    protected boolean someValueIsTrue(final boolean[] booleanArray) {
        synchronized(booleanArray) {
            for (int i = 0;  i < booleanArray.length;  i++) {
                if (booleanArray[i]) {
                    return true;
                }
            }
            return false;
        }
    }
}
