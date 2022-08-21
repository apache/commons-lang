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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.Duration;
import java.util.concurrent.TimeUnit;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.ThreadUtils;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link StopWatch}.
 */
public class StopWatchTest extends AbstractLangTest {

    private static final Duration MILLIS_200 = Duration.ofMillis(200);
    private static final Duration MILLIS_550 = Duration.ofMillis(550);
    private static final String MESSAGE = "Baking cookies";
    private static final Duration MIN_SLEEP = Duration.ofMillis(20);
    private static final String ZERO_HOURS_PREFIX = "00:";
    private static final String ZERO_TIME_ELAPSED = "00:00:00.000";

    /**
     * <p>
     * Creates a suspended StopWatch object which appears to have elapsed for the requested amount of time in
     * nanoseconds.
     * <p>
     * <p>
     *
     * <pre>
     * // Create a mock StopWatch with a time of 2:59:01.999
     * final long nanos = TimeUnit.HOURS.toNanos(2)
     *         + TimeUnit.MINUTES.toNanos(59)
     *         + TimeUnit.SECONDS.toNanos(1)
     *         + TimeUnit.MILLISECONDS.toNanos(999);
     * final StopWatch watch = createMockStopWatch(nanos);
     * </pre>
     *
     * @param nanos Time in nanoseconds to have elapsed on the stop watch
     * @return StopWatch in a suspended state with the elapsed time
     */
    private StopWatch createMockStopWatch(final long nanos) {
        final StopWatch watch = StopWatch.createStarted();
        watch.suspend();
        try {
            final long currentNanos = System.nanoTime();
            FieldUtils.writeField(watch, "startTimeNanos", currentNanos - nanos, true);
            FieldUtils.writeField(watch, "stopTimeNanos", currentNanos, true);
        } catch (final IllegalAccessException e) {
            return null;
        }
        return watch;
    }

    private void sleepQuietly(final Duration duration) throws InterruptedException {
        ThreadUtils.sleep(duration);
    }

    // test bad states
    @Test
    public void testBadStates() {
        final StopWatch watch = new StopWatch();
        assertThrows(IllegalStateException.class, watch::stop,
            "Calling stop on an unstarted StopWatch should throw an exception. ");

        assertThrows(IllegalStateException.class, watch::suspend,
            "Calling suspend on an unstarted StopWatch should throw an exception. ");

        assertThrows(IllegalStateException.class, watch::split,
            "Calling split on a non-running StopWatch should throw an exception. ");

        assertThrows(IllegalStateException.class, watch::unsplit,
            "Calling unsplit on an unsplit StopWatch should throw an exception. ");

        assertThrows(IllegalStateException.class, watch::resume,
            "Calling resume on an unsuspended StopWatch should throw an exception. ");

        watch.start();

        assertThrows(IllegalStateException.class, watch::start,
            "Calling start on a started StopWatch should throw an exception. ");

        assertThrows(IllegalStateException.class, watch::unsplit,
            "Calling unsplit on an unsplit StopWatch should throw an exception. ");

        assertThrows(IllegalStateException.class, watch::getSplitTime,
            "Calling getSplitTime on an unsplit StopWatch should throw an exception. ");

        assertThrows(IllegalStateException.class, watch::resume,
            "Calling resume on an unsuspended StopWatch should throw an exception. ");

        watch.stop();

        assertThrows(IllegalStateException.class, watch::start,
            "Calling start on a stopped StopWatch should throw an exception as it needs to be reset. ");
    }

    @Test
    public void testBooleanStates() {
        final StopWatch watch = new StopWatch();
        assertFalse(watch.isStarted());
        assertFalse(watch.isSuspended());
        assertTrue(watch.isStopped());

        watch.start();
        assertTrue(watch.isStarted());
        assertFalse(watch.isSuspended());
        assertFalse(watch.isStopped());

        watch.suspend();
        assertTrue(watch.isStarted());
        assertTrue(watch.isSuspended());
        assertFalse(watch.isStopped());

        watch.stop();
        assertFalse(watch.isStarted());
        assertFalse(watch.isSuspended());
        assertTrue(watch.isStopped());
    }

    @Test
    public void testFormatSplitTime() {
        final StopWatch watch = StopWatch.createStarted();
        ThreadUtils.sleepQuietly(MIN_SLEEP);
        watch.split();
        final String formatSplitTime = watch.formatSplitTime();
        assertNotEquals(ZERO_TIME_ELAPSED, formatSplitTime);
        assertTrue(formatSplitTime.startsWith(ZERO_HOURS_PREFIX));
    }

    @Test
    public void testFormatSplitTimeWithMessage() {
        final StopWatch watch = new StopWatch(MESSAGE);
        watch.start();
        ThreadUtils.sleepQuietly(MIN_SLEEP);
        watch.split();
        final String formatSplitTime = watch.formatSplitTime();
        assertFalse(formatSplitTime.startsWith(MESSAGE), formatSplitTime);
        assertTrue(formatSplitTime.startsWith(ZERO_HOURS_PREFIX));
    }

    @Test
    public void testFormatTime() {
        final StopWatch watch = StopWatch.create();
        final String formatTime = watch.formatTime();
        assertEquals(ZERO_TIME_ELAPSED, formatTime);
        assertTrue(formatTime.startsWith(ZERO_HOURS_PREFIX));
    }

    @Test
    public void testFormatTimeWithMessage() {
        final StopWatch watch = new StopWatch(MESSAGE);
        final String formatTime = watch.formatTime();
        assertFalse(formatTime.startsWith(MESSAGE), formatTime);
    }

    @Test
    public void testGetStartTime() {
        final long beforeStopWatchMillis = System.currentTimeMillis();
        final StopWatch watch = new StopWatch();
        assertThrows(IllegalStateException.class, watch::getStartTime,
            "Calling getStartTime on an unstarted StopWatch should throw an exception");
        watch.start();

        watch.getStartTime();
        assertTrue(watch.getStartTime() >= beforeStopWatchMillis);

        watch.reset();
        assertThrows(IllegalStateException.class, watch::getStartTime,
            "Calling getStartTime on a reset, but unstarted StopWatch should throw an exception");
    }

    @Test
    public void testLang315() throws InterruptedException {
        final StopWatch watch = StopWatch.createStarted();
        sleepQuietly(MILLIS_200);
        watch.suspend();
        final long suspendTime = watch.getTime();
        sleepQuietly(MILLIS_200);
        watch.stop();
        final long totalTime = watch.getTime();
        assertEquals(suspendTime, totalTime);
    }

    @Test
    public void testMessage() {
        assertNull(StopWatch.create().getMessage());
        final StopWatch stopWatch = new StopWatch(MESSAGE);
        assertEquals(MESSAGE, stopWatch.getMessage());
        assertTrue(stopWatch.toString().startsWith(MESSAGE));
        stopWatch.start();
        stopWatch.split();
        assertTrue(stopWatch.toSplitString().startsWith(MESSAGE));
    }

    @Test
    public void testStopTimeSimple() throws InterruptedException {
        final StopWatch watch = StopWatch.createStarted();
        final long testStartMillis = System.currentTimeMillis();
        sleepQuietly(MILLIS_550);
        watch.stop();
        final long testEndMillis = System.currentTimeMillis();
        final long stopTime = watch.getStopTime();
        assertEquals(stopTime, watch.getStopTime());

        assertTrue(stopTime >= testStartMillis);
        assertTrue(stopTime <= testEndMillis);
    }

    @Test
    public void testStopWatchGetWithTimeUnit() {
        // Create a mock StopWatch with a time of 2:59:01.999
        // @formatter:off
        final StopWatch watch = createMockStopWatch(
            TimeUnit.HOURS.toNanos(2)
                    + TimeUnit.MINUTES.toNanos(59)
                    + TimeUnit.SECONDS.toNanos(1)
                    + TimeUnit.MILLISECONDS.toNanos(999));
        // @formatter:on

        assertEquals(2L, watch.getTime(TimeUnit.HOURS));
        assertEquals(179L, watch.getTime(TimeUnit.MINUTES));
        assertEquals(10741L, watch.getTime(TimeUnit.SECONDS));
        assertEquals(10741999L, watch.getTime(TimeUnit.MILLISECONDS));
    }

    @Test
    public void testStopWatchSimple() throws InterruptedException {
        final StopWatch watch = StopWatch.createStarted();
        sleepQuietly(MILLIS_550);
        watch.stop();
        final long time = watch.getTime();
        assertEquals(time, watch.getTime());

        assertTrue(time >= 500);
        assertTrue(time < 700);

        watch.reset();
        assertEquals(0, watch.getTime());
    }

    @Test
    public void testStopWatchSimpleGet() throws InterruptedException {
        final StopWatch watch = new StopWatch();
        assertEquals(0, watch.getTime());
        assertEquals(ZERO_TIME_ELAPSED, watch.toString());

        watch.start();
        sleepQuietly(MILLIS_550);
        assertTrue(watch.getTime() < 2000);
    }

    @Test
    public void testStopWatchSplit() throws InterruptedException {
        final StopWatch watch = StopWatch.createStarted();
        sleepQuietly(MILLIS_550);
        watch.split();
        final long splitTime = watch.getSplitTime();
        final String splitStr = watch.toSplitString();
        sleepQuietly(MILLIS_550);
        watch.unsplit();
        sleepQuietly(MILLIS_550);
        watch.stop();
        final long totalTime = watch.getTime();

        assertEquals(splitStr.length(), 12, "Formatted split string not the correct length");
        assertTrue(splitTime >= 500);
        assertTrue(splitTime < 700);
        assertTrue(totalTime >= 1500);
        assertTrue(totalTime < 1900);
    }

    @Test
    public void testStopWatchStatic() {
        final StopWatch watch = StopWatch.createStarted();
        assertTrue(watch.isStarted());
    }

    @Test
    public void testStopWatchSuspend() throws InterruptedException {
        final StopWatch watch = StopWatch.createStarted();
        final long testStartMillis = System.currentTimeMillis();
        sleepQuietly(MILLIS_550);
        watch.suspend();
        final long testSuspendMillis = System.currentTimeMillis();
        final long suspendTime = watch.getTime();
        final long stopTime = watch.getStopTime();

        assertTrue(testStartMillis <= stopTime);
        assertTrue(testSuspendMillis <= stopTime);

        sleepQuietly(MILLIS_550);
        watch.resume();
        sleepQuietly(MILLIS_550);
        watch.stop();
        final long totalTime = watch.getTime();

        assertTrue(suspendTime >= 500);
        assertTrue(suspendTime < 700);
        assertTrue(totalTime >= 1000);
        assertTrue(totalTime < 1300);
    }

    @Test
    public void testToSplitString() throws InterruptedException {
        final StopWatch watch = StopWatch.createStarted();
        sleepQuietly(MILLIS_550);
        watch.split();
        final String splitStr = watch.toSplitString();
        assertEquals(splitStr.length(), 12, "Formatted split string not the correct length");
    }

    @Test
    public void testToSplitStringWithMessage() throws InterruptedException {
        final StopWatch watch = new StopWatch(MESSAGE);
        watch.start();
        sleepQuietly(MILLIS_550);
        watch.split();
        final String splitStr = watch.toSplitString();
        assertEquals(splitStr.length(), 12 + MESSAGE.length() + 1, "Formatted split string not the correct length");
    }

    @Test
    public void testToString() throws InterruptedException {
        //
        final StopWatch watch = StopWatch.createStarted();
        sleepQuietly(MILLIS_550);
        watch.split();
        final String splitStr = watch.toString();
        assertEquals(splitStr.length(), 12, "Formatted split string not the correct length");
    }

    @Test
    public void testToStringWithMessage() throws InterruptedException {
        assertTrue(new StopWatch(MESSAGE).toString().startsWith(MESSAGE));
        //
        final StopWatch watch = new StopWatch(MESSAGE);
        watch.start();
        sleepQuietly(MILLIS_550);
        watch.split();
        final String splitStr = watch.toString();
        assertEquals(splitStr.length(), 12 + MESSAGE.length() + 1, "Formatted split string not the correct length");
    }
}
