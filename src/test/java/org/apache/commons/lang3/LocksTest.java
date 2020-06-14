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
package org.apache.commons.lang3;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.function.LongConsumer;

import org.apache.commons.lang3.Functions.FailableConsumer;
import org.apache.commons.lang3.Locks.Lock;
import org.junit.jupiter.api.Test;

class LocksTest {
    private static final int NUMBER_OF_THREADS = 10;

    @Test
    void testReadLock() throws Exception {
        final long DELAY=3000;
        /** If our threads are running concurrently, then we expect to be faster
         * than running one after the other.
         */
        runTest(DELAY, false, l -> assertTrue(l < NUMBER_OF_THREADS*DELAY));
    }

    void testWriteLock() throws Exception {
        final long DELAY = 100;
        /** If our threads are running concurrently, then we expect to be no faster
         * than running one after the other.
         */
        runTest(DELAY, true, l -> assertTrue(l >= NUMBER_OF_THREADS*DELAY));
    }

    private void runTest(long delay, boolean exclusiveLock, LongConsumer runTimeCheck) throws InterruptedException {
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
                    lock.runWriteLocked(consumer);
                } else {
                    lock.runReadLocked(consumer);
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

    protected void modify(boolean[] booleanArray, int offset, boolean value) {
        synchronized(booleanArray) {
            booleanArray[offset] = value;
        }
    }
    protected boolean someValueIsTrue(boolean[] booleanArray) {
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
