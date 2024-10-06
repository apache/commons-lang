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
import static org.hamcrest.Matchers.greaterThanOrEqualTo;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.startsWith;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.time.Duration;
import java.time.Instant;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.ThreadUtils;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link StopWatch}.
 */
public class StopWatchTest extends AbstractLangTest {

    private static final int SPLIT_CLOCK_STR_LEN = 12;
    private static final Duration MIN_DURATION = Duration.ofMillis(20);
    private static final String MESSAGE = "Baking cookies";
    private static final String ZERO_HOURS_PREFIX = "00:";
    private static final String ZERO_TIME_ELAPSED = "00:00:00.000";

    /**
     * <p>
     * Creates a suspended StopWatch object which appears to have elapsed for the requested amount of time in nanoseconds.
     * <p>
     * <p>
     *
     * <pre>
     * // Create a mock StopWatch with a time of 2:59:01.999
     * final long nanos = TimeUnit.HOURS.toNanos(2) + TimeUnit.MINUTES.toNanos(59) + TimeUnit.SECONDS.toNanos(1) + TimeUnit.MILLISECONDS.toNanos(999);
     * final StopWatch watch = createMockStopWatch(nanos);
     * </pre>
     *
     * @param nanos Time in nanoseconds to have elapsed on the stop watch
     * @return StopWatch in a suspended state with the elapsed time
     */
    private StopWatch createMockStopWatch(final long nanos) {
        final StopWatch watch = StopWatch.createStarted();
        watch.suspend();
        return set(watch, nanos);
    }

    private StopWatch set(final StopWatch watch, final long nanos) {
        try {
            final long currentNanos = System.nanoTime();
            FieldUtils.writeField(watch, "startTimeNanos", currentNanos - nanos, true);
            FieldUtils.writeField(watch, "stopTimeNanos", currentNanos, true);
        } catch (final IllegalAccessException e) {
            return null;
        }
        return watch;
    }

    /**
     * Sleeps the requested duration plus one millisecond. On Java 8, sleeping for 2 or 20 millis can sleep for a tiny bit less.
     *
     * @param duration How long to sleep.
     * @throws InterruptedException if any thread has interrupted the current thread.
     */
    private void sleepPlus1(final Duration duration) throws InterruptedException {
        ThreadUtils.sleep(duration.plusMillis(1));
    }

    /**
     * Tests bad states.
     */
    @Test
    public void testBadStates() {
        final StopWatch watch = new StopWatch();
        assertThrows(IllegalStateException.class, watch::stop, "Calling stop on an unstarted StopWatch should throw an exception.");
        assertThrows(IllegalStateException.class, watch::suspend, "Calling suspend on an unstarted StopWatch should throw an exception.");
        assertThrows(IllegalStateException.class, watch::split, "Calling split on a non-running StopWatch should throw an exception.");
        assertThrows(IllegalStateException.class, watch::unsplit, "Calling unsplit on an unsplit StopWatch should throw an exception.");
        assertThrows(IllegalStateException.class, watch::resume, "Calling resume on an unsuspended StopWatch should throw an exception.");
        watch.start();
        assertThrows(IllegalStateException.class, watch::start, "Calling start on a started StopWatch should throw an exception.");
        assertThrows(IllegalStateException.class, watch::unsplit, "Calling unsplit on an unsplit StopWatch should throw an exception.");
        assertThrows(IllegalStateException.class, watch::getSplitTime, "Calling getSplitTime on an unsplit StopWatch should throw an exception.");
        assertThrows(IllegalStateException.class, watch::getSplitDuration, "Calling getSplitTime on an unsplit StopWatch should throw an exception.");
        assertThrows(IllegalStateException.class, watch::resume, "Calling resume on an unsuspended StopWatch should throw an exception.");
        watch.stop();
        assertThrows(IllegalStateException.class, watch::start, "Calling start on a stopped StopWatch should throw an exception as it needs to be reset.");
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
        ThreadUtils.sleepQuietly(MIN_DURATION);
        watch.split();
        final String formatSplitTime = watch.formatSplitTime();
        assertNotEquals(ZERO_TIME_ELAPSED, formatSplitTime);
        assertThat("formatSplitTime", formatSplitTime, startsWith(ZERO_HOURS_PREFIX));
    }

    @Test
    public void testFormatSplitTimeWithMessage() {
        final StopWatch watch = new StopWatch(MESSAGE);
        watch.start();
        ThreadUtils.sleepQuietly(MIN_DURATION);
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
    public void testGet() throws Throwable {
        final StopWatch watch = new StopWatch();
        final AtomicInteger i = new AtomicInteger();
        assertEquals(1, watch.get(i::incrementAndGet));
        assertEquals(2, watch.getT(i::incrementAndGet));
        final IOException e = assertThrows(IOException.class, () -> watch.getT(this::throwIOException));
        assertEquals("A", e.getMessage());
        // test state
        assertTrue(watch.isSuspended());
        assertEquals(3, watch.get(() -> {
            assertTrue(watch.isStarted());
            return i.incrementAndGet();
        }));
        assertTrue(watch.isSuspended());
        final long nanos1 = watch.getDuration().toNanos();
        assertTrue(nanos1 >= 0);
        // test state
        assertTrue(watch.isSuspended());
        assertEquals(4, watch.getT(() -> {
            assertTrue(watch.isStarted());
            return i.incrementAndGet();
        }));
        assertTrue(watch.isSuspended());
        assertTrue(watch.getDuration().toNanos() >= nanos1);
    }

    @Test
    public void testGetDuration() throws InterruptedException {
        final StopWatch watch = new StopWatch();
        assertEquals(Duration.ZERO, watch.getDuration());
        assertEquals(ZERO_TIME_ELAPSED, watch.toString());
        watch.start();
        sleepPlus1(MIN_DURATION);
        final long nanos = watch.getNanoTime();
        assertTrue(nanos > 0, () -> "getNanoTime(): " + nanos);
        assertTrue(DurationUtils.isPositive(watch.getDuration()));
    }

    @Test
    public void testGetSplitDuration() {
        // Create a mock StopWatch with a time of 2:59:01.999
        final StopWatch watch = StopWatch.createStarted();
        watch.split();
        set(watch, 123456);
        assertEquals(Duration.ofNanos(123456), watch.getSplitDuration());
    }

    @Test
    public void testGetStartInstant() {
        final long beforeStopWatchMillis = System.currentTimeMillis();
        final StopWatch watch = new StopWatch();
        assertThrows(IllegalStateException.class, watch::getStartInstant, "Calling getStartInstant on an unstarted StopWatch should throw an exception");
        watch.start();

        watch.getStartInstant();
        assertThat("getStartInstant", watch.getStartInstant(), greaterThanOrEqualTo(Instant.ofEpochMilli(beforeStopWatchMillis)));

        watch.reset();
        assertThrows(IllegalStateException.class, watch::getStartInstant,
                "Calling getStartInstant on a reset, but unstarted StopWatch should throw an exception");
    }

    @Test
    public void testGetStartTime() {
        final long beforeStopWatchMillis = System.currentTimeMillis();
        final StopWatch watch = new StopWatch();
        assertThrows(IllegalStateException.class, watch::getStartTime, "Calling getStartTime on an unstarted StopWatch should throw an exception");
        watch.start();

        watch.getStartTime();
        assertThat("getStartTime", watch.getStartTime(), greaterThanOrEqualTo(beforeStopWatchMillis));

        watch.reset();
        assertThrows(IllegalStateException.class, watch::getStartTime, "Calling getStartTime on a reset, but unstarted StopWatch should throw an exception");
    }

    @RepeatedTest(10)
    public void testGetTime() throws InterruptedException {
        final StopWatch watch = new StopWatch();
        assertEquals(0, watch.getTime());
        assertEquals(ZERO_TIME_ELAPSED, watch.toString());
        watch.start();
        sleepPlus1(MIN_DURATION);
        final long time = watch.getTime();
        assertTrue(time > 0, () -> "getTime() millis: " + time);
        assertTrue(time < 2000, () -> "getTime() millis: " + time);
    }

    @Test
    public void testGetWithTimeUnit() {
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
    public void testLang315() throws InterruptedException {
        final StopWatch watch = StopWatch.createStarted();
        sleepPlus1(MIN_DURATION);
        watch.suspend();
        final long suspendTime = watch.getTime();
        final Duration suspendDuration = watch.getDuration();
        sleepPlus1(MIN_DURATION);
        watch.stop();
        final long totalTime = watch.getTime();
        final Duration totalDuration = watch.getDuration();
        assertEquals(suspendTime, totalTime);
        assertEquals(suspendDuration, totalDuration);
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
    public void testRun() throws Throwable {
        final StopWatch watch = new StopWatch();
        final AtomicInteger i = new AtomicInteger();
        watch.run(i::incrementAndGet);
        assertEquals(1, i.get());
        watch.runT(i::incrementAndGet);
        assertEquals(2, i.get());
        final IOException e = assertThrows(IOException.class, () -> watch.runT(this::throwIOException));
        assertEquals("A", e.getMessage());
        // test state
        assertTrue(watch.isSuspended());
        watch.run(() -> {
            assertTrue(watch.isStarted());
            i.incrementAndGet();
        });
        assertEquals(3, i.get());
        assertTrue(watch.isSuspended());
        final long nanos1 = watch.getDuration().toNanos();
        assertTrue(nanos1 > 0);
        // test state
        assertTrue(watch.isSuspended());
        watch.runT(() -> {
            assertTrue(watch.isStarted());
            i.incrementAndGet();
        });
        assertEquals(4, i.get());
        assertTrue(watch.isSuspended());
        assertTrue(watch.getDuration().toNanos() >= nanos1);
    }

    @Test
    public void testSimple() throws InterruptedException {
        final StopWatch watch = StopWatch.createStarted();
        final Duration sleepDuration = MIN_DURATION;
        sleepPlus1(sleepDuration);
        watch.stop();
        final long time = watch.getTime();
        final Duration duration = watch.getDuration();
        assertEquals(time, watch.getTime());
        assertEquals(duration, watch.getDuration());
        assertTrue(duration.compareTo(sleepDuration) >= 0, () -> "duration: " + duration);
        watch.reset();
        assertEquals(0, watch.getTime());
        assertEquals(Duration.ZERO, watch.getDuration());
    }

    @Test
    public void testSplit() throws InterruptedException {
        final StopWatch watch = StopWatch.createStarted();
        final Duration sleepDuration = MIN_DURATION;
        final long sleepMillis = sleepDuration.toMillis();
        assertTrue(sleepMillis > 0);
        sleepPlus1(sleepDuration);
        watch.split();
        final long splitTime = watch.getSplitTime();
        final Duration splitDuration = watch.getSplitDuration();
        assertEquals(splitTime, watch.getSplitDuration().toMillis());
        assertEquals(SPLIT_CLOCK_STR_LEN, watch.toSplitString().length(), "Formatted split string not the correct length");
        sleepPlus1(sleepDuration);
        watch.unsplit();
        sleepPlus1(sleepDuration);
        watch.stop();
        final long totalTime = watch.getTime();
        final Duration totalDuration = watch.getDuration();
        assertTrue(splitTime >= sleepMillis, () -> "splitTime: " + splitTime);
        assertTrue(splitDuration.toMillis() >= sleepMillis, () -> "splitDuration: " + splitDuration);
        final long sleepMillisX3 = sleepMillis * 3;
        assertTrue(totalTime >= sleepMillisX3 && splitTime < 21000);
        assertTrue(totalDuration.toMillis() >= sleepMillisX3);
    }

    @Test
    public void testStatic() {
        final StopWatch watch = StopWatch.createStarted();
        assertTrue(watch.isStarted());
    }

    @Test
    public void testStopInstantSimple() throws InterruptedException {
        final StopWatch watch = StopWatch.createStarted();
        final long testStartMillis = System.currentTimeMillis();
        sleepPlus1(MIN_DURATION);
        watch.stop();
        final long testEndMillis = System.currentTimeMillis();
        final Instant stopTime = watch.getStopInstant();
        assertEquals(stopTime, watch.getStopInstant());
        // Only less than, not equal
        assertTrue(testStartMillis < testEndMillis);
        assertTrue(Instant.ofEpochMilli(testStartMillis).isBefore(Instant.ofEpochMilli(testEndMillis)));
    }

    @Test
    public void testStopTimeSimple() throws InterruptedException {
        final StopWatch watch = StopWatch.createStarted();
        final long testStartMillis = System.currentTimeMillis();
        sleepPlus1(MIN_DURATION);
        watch.stop();
        final long testEndMillis = System.currentTimeMillis();
        final long stopTime = watch.getStopTime();
        assertEquals(stopTime, watch.getStopTime());
        // Only less than, not equal
        assertTrue(testStartMillis < testEndMillis);
    }

    @Test
    public void testSuspend() throws InterruptedException {
        // Watch out comparing measurements from System.currentTimeMillis() vs. System.nanoTime()
        final StopWatch watch = StopWatch.createStarted();
        final long testStartMillis = System.currentTimeMillis();
        final long testStartNanos = System.nanoTime();
        final Instant testStartInstant = Instant.ofEpochMilli(testStartMillis);
        final Duration sleepDuration = MIN_DURATION;
        final long sleepMillis = sleepDuration.toMillis();
        sleepPlus1(sleepDuration);
        watch.suspend();
        final long testSuspendMillis = System.currentTimeMillis();
        final long testSuspendNanos = System.nanoTime();
        final long testSuspendTimeNanos = testSuspendNanos - testStartNanos;
        // See sleepPlus1
        final Duration testSuspendDuration = Duration.ofNanos(testSuspendTimeNanos).plusMillis(1);
        final long suspendTimeFromNanos = watch.getTime();
        final Duration suspendDuration = watch.getDuration();
        final long stopTimeMillis = watch.getStopTime();
        final Instant stopInstant = watch.getStopInstant();

        assertTrue(testStartMillis <= stopTimeMillis, () -> String.format("testStartMillis %s <= stopTimeMillis %s", testStartMillis, stopTimeMillis));
        assertTrue(testStartInstant.isBefore(stopInstant), () -> String.format("testStartInstant %s < stopInstant %s", testStartInstant, stopInstant));
        assertTrue(testSuspendMillis <= stopTimeMillis, () -> String.format("testSuspendMillis %s <= stopTimeMillis %s", testSuspendMillis, stopTimeMillis));
        assertTrue(testSuspendMillis <= stopInstant.toEpochMilli(),
                () -> String.format("testSuspendMillis %s <= stopInstant %s", testSuspendMillis, stopInstant));

        sleepPlus1(sleepDuration);
        watch.resume();
        sleepPlus1(sleepDuration);
        watch.stop();
        final long totalTimeFromNanos = watch.getTime();
        final Duration totalDuration = watch.getDuration();

        assertTrue(suspendTimeFromNanos >= sleepMillis, () -> String.format("suspendTimeFromNanos %s >= sleepMillis %s", suspendTimeFromNanos, sleepMillis));
        assertTrue(suspendDuration.compareTo(Duration.ofMillis(sleepMillis)) >= 0,
                () -> String.format("suspendDuration %s >= sleepMillis %s", suspendDuration, sleepMillis));
        assertTrue(suspendTimeFromNanos <= testSuspendTimeNanos,
                () -> String.format("suspendTimeFromNanos %s <= testSuspendTimeNanos %s", suspendTimeFromNanos, testSuspendTimeNanos));
        assertTrue(suspendDuration.compareTo(testSuspendDuration) <= 0,
                () -> String.format("suspendDuration %s <= testSuspendDuration %s", suspendDuration, testSuspendDuration));

        final long sleepMillisX2 = sleepMillis + sleepMillis;
        assertTrue(totalTimeFromNanos >= sleepMillisX2, () -> String.format("totalTimeFromNanos %s >= sleepMillisX2 %s", totalTimeFromNanos, sleepMillisX2));
        assertTrue(totalDuration.compareTo(Duration.ofMillis(sleepMillisX2)) >= 0,
                () -> String.format("totalDuration >= sleepMillisX2", totalDuration, sleepMillisX2));

        // Be lenient for slow running builds
        final long testTooLongMillis = sleepMillis * 100;
        assertTrue(totalTimeFromNanos < testTooLongMillis,
                () -> String.format("totalTimeFromNanos %s < testTooLongMillis %s", totalTimeFromNanos, testTooLongMillis));
        assertTrue(totalDuration.compareTo(Duration.ofMillis(testTooLongMillis)) < 0,
                () -> String.format("totalDuration %s < testTooLongMillis %s", totalDuration, testTooLongMillis));

    }

    @Test
    public void testToSplitString() throws InterruptedException {
        final StopWatch watch = StopWatch.createStarted();
        sleepPlus1(MIN_DURATION);
        watch.split();
        final String splitStr = watch.toSplitString();
        assertEquals(SPLIT_CLOCK_STR_LEN, splitStr.length(), "Formatted split string not the correct length");
    }

    @Test
    public void testToSplitStringWithMessage() throws InterruptedException {
        final StopWatch watch = new StopWatch(MESSAGE);
        watch.start();
        sleepPlus1(MIN_DURATION);
        watch.split();
        final String splitStr = watch.toSplitString();
        assertEquals(SPLIT_CLOCK_STR_LEN + MESSAGE.length() + 1, splitStr.length(), "Formatted split string not the correct length");
    }

    @Test
    public void testToString() throws InterruptedException {
        //
        final StopWatch watch = StopWatch.createStarted();
        sleepPlus1(MIN_DURATION);
        watch.split();
        final String splitStr = watch.toString();
        assertEquals(SPLIT_CLOCK_STR_LEN, splitStr.length(), "Formatted split string not the correct length");
    }

    @Test
    public void testToStringWithMessage() throws InterruptedException {
        assertThat("message", new StopWatch(MESSAGE).toString(), startsWith(MESSAGE));
        //
        final StopWatch watch = new StopWatch(MESSAGE);
        watch.start();
        sleepPlus1(MIN_DURATION);
        watch.split();
        final String splitStr = watch.toString();
        assertEquals(SPLIT_CLOCK_STR_LEN + MESSAGE.length() + 1, splitStr.length(), "Formatted split string not the correct length");
    }

    private int throwIOException() throws IOException {
        throw new IOException("A");
    }
}
