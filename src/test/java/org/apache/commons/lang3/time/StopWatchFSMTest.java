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

package org.apache.commons.lang3.time;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

public class StopWatchFSMTest {

    @Test
    public void testInitialState() {
        StopWatch sw = StopWatch.create();
        // In the initial (UNSTARTED) state: isStarted() is false, isStopped() returns
        // true, isSuspended() is false.
        assertFalse(sw.isStarted(), "StopWatch should not be started initially.");
        assertTrue(sw.isStopped(), "StopWatch should be considered stopped when unstarted.");
        assertFalse(sw.isSuspended(), "StopWatch should not be suspended initially.");
    }

    @Test
    public void testStartTransition() {
        StopWatch sw = StopWatch.create();
        sw.start(); // UNSTARTED -> RUNNING
        assertTrue(sw.isStarted(), "StopWatch should be started after calling start().");
        assertFalse(sw.isStopped(), "StopWatch should not be stopped when running.");
        assertFalse(sw.isSuspended(), "StopWatch should not be suspended when running.");
    }

    @Test
    public void testDoubleStartThrowsException() {
        StopWatch sw = StopWatch.create();
        sw.start();
        Exception exception = assertThrows(IllegalStateException.class, sw::start,
                "Calling start() twice without reset should throw an exception.");
        assertEquals("Stopwatch already started.", exception.getMessage());
    }

    @Test
    public void testStopTransition() throws InterruptedException {
        StopWatch sw = StopWatch.create();
        sw.start(); // Transition to RUNNING
        Thread.sleep(10);
        sw.stop(); // RUNNING -> STOPPED
        assertTrue(sw.isStopped(), "StopWatch should be stopped after calling stop().");
        // Elapsed time should be greater than zero.
        assertTrue(sw.getTime() > 0, "Elapsed time should be greater than 0 after stopping.");
    }

    @Test
    public void testSuspendAndResumeTransitions() throws InterruptedException {
        StopWatch sw = StopWatch.create();
        sw.start(); // UNSTARTED -> RUNNING
        Thread.sleep(10);
        sw.suspend(); // RUNNING -> SUSPENDED
        assertTrue(sw.isSuspended(), "StopWatch should be suspended after calling suspend().");
        long timeAtSuspend = sw.getTime();
        Thread.sleep(10); // This extra time should not be counted.
        // Resume: SUSPENDED -> RUNNING
        sw.resume();
        Thread.sleep(10);
        sw.stop(); // Transition to STOPPED
        long totalTime = sw.getTime();
        // Total time should be at least the time before suspend plus time after resume.
        assertTrue(totalTime >= timeAtSuspend, "Total time should include time before suspend and after resume.");
        assertTrue(sw.isStopped(), "StopWatch should be stopped after calling stop().");
    }

}
