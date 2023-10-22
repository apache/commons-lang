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

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.greaterThanOrEqualTo;
import static org.hamcrest.Matchers.lessThan;
import static org.hamcrest.Matchers.lessThanOrEqualTo;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.startsWith;
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

    private void sleep(final Duration duration) throws InterruptedException {
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
        assertThat("formatSplitTime", formatSplitTime, startsWith(ZERO_HOURS_PREFIX));
    }

    @Test
    public void testFormatSplitTimeWithMessage() {
        final StopWatch watch = new StopWatch(MESSAGE);
        watch.start();
        ThreadUtils.sleepQuietly(MIN_SLEEP);
        watch.split();
        final String formatSplitTime = watch.formatSplitTime();
        assertThat("formatSplitTime", formatSplitTime, not(startsWith(MESSAGE)));
        assertThat("formatSplitTime", formatSplitTime, startsWith(ZERO_HOURS_PREFIX));
    }

    @Test
    public void testFormatTime() {
        final StopWatch watch = StopWatch.create();
        final String formatTime = watch.formatTime();
        assertEquals(ZERO_TIME_ELAPSED, formatTime);
        assertThat("formatTime", formatTime, startsWith(ZERO_HOURS_PREFIX));
    }

    @Test
    public void testFormatTimeWithMessage() {
        final StopWatch watch = new StopWatch(MESSAGE);
        final String formatTime = watch.formatTime();
        assertThat("formatTime", formatTime, not(startsWith(MESSAGE)));
    }

    @Test
    public void testGetStartTime() {
        final long beforeStopWatchMillis = System.currentTimeMillis();
        final StopWatch watch = new StopWatch();
        assertThrows(IllegalStateException.class, watch::getStartTime,
            "Calling getStartTime on an unstarted StopWatch should throw an exception");
        watch.start();

        watch.getStartTime();
        assertThat("getStartTime", watch.getStartTime(), greaterThanOrEqualTo(beforeStopWatchMillis));

        watch.reset();
        assertThrows(IllegalStateException.class, watch::getStartTime,
            "Calling getStartTime on a reset, but unstarted StopWatch should throw an exception");
    }

    @Test
    public void testLang315() throws InterruptedException {
        final StopWatch watch = StopWatch.createStarted();
        sleep(MILLIS_200);
        watch.suspend();
        final long suspendTime = watch.getTime();
        sleep(MILLIS_200);
        watch.stop();
        final long totalTime = watch.getTime();
        assertEquals(suspendTime, totalTime);
    }

    @Test
    public void testMessage() {
        assertNull(StopWatch.create().getMessage());
        final StopWatch stopWatch = new StopWatch(MESSAGE);
        assertEquals(MESSAGE, stopWatch.getMessage());
        assertThat("stopWatch.toString", stopWatch.toString(), startsWith(MESSAGE));
        stopWatch.start();
        stopWatch.split();
        assertThat("stopWatch.toSplitString", stopWatch.toSplitString(), startsWith(MESSAGE));
    }

    @Test
    public void testStopTimeSimple() throws InterruptedException {
        final StopWatch watch = StopWatch.createStarted();
        final long testStartMillis = System.currentTimeMillis();
        sleep(MILLIS_550);
        watch.stop();
        final long testEndMillis = System.currentTimeMillis();
        final long stopTime = watch.getStopTime();
        assertEquals(stopTime, watch.getStopTime());

        assertThat("stopTime", stopTime, allOf(greaterThanOrEqualTo(testStartMillis), lessThanOrEqualTo(testEndMillis)));
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
        sleep(MILLIS_550);
        watch.stop();
        final long time = watch.getTime();
        assertEquals(time, watch.getTime());

        assertThat("time", time, allOf(greaterThanOrEqualTo(500L), lessThan(700L)));

        watch.reset();
        assertEquals(0, watch.getTime());
    }

    @Test
    public void testStopWatchSimpleGet() throws InterruptedException {
        final StopWatch watch = new StopWatch();
        assertEquals(0, watch.getTime());
        assertEquals(ZERO_TIME_ELAPSED, watch.toString());

        watch.start();
        sleep(MILLIS_550);
        assertThat("watch.getTime()", watch.getTime(), lessThan(2000L));
    }

    @Test
    public void testStopWatchSplit() throws InterruptedException {
        final StopWatch watch = StopWatch.createStarted();
        sleep(MILLIS_550);
        watch.split();
        final long splitTime = watch.getSplitTime();
        final String splitStr = watch.toSplitString();
        sleep(MILLIS_550);
        watch.unsplit();
        sleep(MILLIS_550);
        watch.stop();
        final long totalTime = watch.getTime();

        assertEquals(12, splitStr.length(), "Formatted split string not the correct length");
        assertThat("splitTime", splitTime, allOf(greaterThanOrEqualTo(500L), lessThan(700L)));
        assertThat("totalTime", totalTime, allOf(greaterThanOrEqualTo(1500L), lessThan(2000L)));
    }

    @Test
    public void testStopWatchStatic() {
        final StopWatch watch = StopWatch.createStarted();
        assertTrue(watch.isStarted());
    }

    @Test
    public void testStopWatchSuspend() throws InterruptedException {
        // Watch out comparing measurements from System.currentTimeMillis() vs. System.nanoTime()
        final StopWatch watch = StopWatch.createStarted();
        final long testStartMillis = System.currentTimeMillis();
        final long testStartNanos = System.nanoTime();
        sleep(MILLIS_550);
        watch.suspend();
        final long testSuspendMillis = System.currentTimeMillis();
        final long testSuspendNanos = System.nanoTime();
        final long testSuspendTimeNanos = testSuspendNanos - testStartNanos;
        final long suspendTimeFromNanos = watch.getTime();
        final long stopTimeMillis = watch.getStopTime();

        assertThat("testStartMillis <= stopTimeMillis", testStartMillis, lessThanOrEqualTo(stopTimeMillis));
        assertThat("testSuspendMillis <= stopTimeMillis", testSuspendMillis, lessThanOrEqualTo(stopTimeMillis));

        sleep(MILLIS_550);
        watch.resume();
        sleep(MILLIS_550);
        watch.stop();
        final long totalTimeFromNanos = watch.getTime();

        assertThat("suspendTimeFromNanos", suspendTimeFromNanos, greaterThanOrEqualTo(500L));
        assertThat("suspendTimeFromNanos <= testSuspendTimeNanos", suspendTimeFromNanos, lessThanOrEqualTo(testSuspendTimeNanos));
        assertThat("totalTimeFromNanos", totalTimeFromNanos, greaterThanOrEqualTo(1000L));
        // Be lenient for slow running builds
        assertThat("totalTimeFromNanos", totalTimeFromNanos, lessThan(2500L));
    }

    @Test
    public void testToSplitString() throws InterruptedException {
        final StopWatch watch = StopWatch.createStarted();
        sleep(MILLIS_550);
        watch.split();
        final String splitStr = watch.toSplitString();
        assertEquals(12, splitStr.length(), "Formatted split string not the correct length");
    }

    @Test
    public void testToSplitStringWithMessage() throws InterruptedException {
        final StopWatch watch = new StopWatch(MESSAGE);
        watch.start();
        sleep(MILLIS_550);
        watch.split();
        final String splitStr = watch.toSplitString();
        assertEquals(12 + MESSAGE.length() + 1, splitStr.length(), "Formatted split string not the correct length");
    }

    @Test
    public void testToString() throws InterruptedException {
        //
        final StopWatch watch = StopWatch.createStarted();
        sleep(MILLIS_550);
        watch.split();
        final String splitStr = watch.toString();
        assertEquals(12, splitStr.length(), "Formatted split string not the correct length");
    }

    @Test
    public void testToStringWithMessage() throws InterruptedException {
        assertThat("message", new StopWatch(MESSAGE).toString(), startsWith(MESSAGE));
        //
        final StopWatch watch = new StopWatch(MESSAGE);
        watch.start();
        sleep(MILLIS_550);
        watch.split();
        final String splitStr = watch.toString();
        assertEquals(12 + MESSAGE.length() + 1, splitStr.length(), "Formatted split string not the correct length");
    }
}
