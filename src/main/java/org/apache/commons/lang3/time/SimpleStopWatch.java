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

/**
 * <p>
 * {@link SimpleStopWatch} provides a convenient API for timings.
 * </p>
 * <p>
 * {@link SimpleStopWatch} is simple alternative for {@link StopWatch}. It does not have state flow
 * and API is no-brainer.
 * </p>
 * <p>
 * {@link SimpleStopWatch} use {@link System#nanoTime()} for time measure.
 * </p>

 *
 * <p>
 * To start the watch, call {@link SimpleStopWatch#started()}.
 * </p>
 * <p>
 * To get time in millis since start, call {@link #time()}.
 * </p>
 * <p>
 * {@link #lap()} returns time in millis since last {@link #lap()} call or since start
 * when {@link #lap()} not called yet. For example:<br>
 * 1. var stopWatch = SimpleStopWatch.started()<br>
 * 2. sleep (200 ms)<br>
 * 3. stopWatch.getTime() == 200, stopWatch.lap() == 200<br>
 * 4. sleep (100 ms)<br>
 * 5. stopWatch.getTime() == 300, stopWatch.lap() == 100<br>

 * <p>This class is not thread-safe.</p>
 *
 * @since 3.12
 */
public class SimpleStopWatch {


    private static final long NANO_2_MILLIS = 1000000L;

    /**
     * Creates a simple stop watch.<br>
     * To avoid doubts for people used to {@link StopWatch} method name explains
     * that SimpleStopWatch is started when created.
     *
     * @return SimpleStopWatch a stopwatch.
     */
    public static SimpleStopWatch started() {
        return new SimpleStopWatch();
    }


    /**
     * The start time in nanoseconds.
     */
    private long startTimeNanos;


    /**
     * The lap start time in nanoseconds.
     */
    private long lapStartTimeNanos;


    private SimpleStopWatch() {
        this.startTimeNanos = System.nanoTime();
        this.lapStartTimeNanos = startTimeNanos;
    }

    /**
     * <p>
     * Gets the time on the stopwatch.
     * </p>
     *
     * @return the time in milliseconds
     */
    public long time() {
        return (System.nanoTime() - this.startTimeNanos) / NANO_2_MILLIS;
    }

    /**
     * <p>
     * Gets the time on the stopwatch since last ${lap} call or since start when ${lap} not called yet
     * </p>
     *
     * @return the time in milliseconds
     */
    public long lap() {
        long lapTime = (System.nanoTime() - lapStartTimeNanos) / NANO_2_MILLIS;
        lapStartTimeNanos = System.nanoTime();
        return lapTime;
    }
}
