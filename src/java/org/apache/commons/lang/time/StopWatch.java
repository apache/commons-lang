/*
 * Copyright 2002-2004 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang.time;

/**
 * <p><code>StopWatch</code> provides a convenient API for timings.</p>
 * 
 * <p>The methods do <b>not</b> protect against inappropriate calls. Thus you
 * can call stop before start, resume before suspend or unsplit before split.
 * The results are indeterminate in these cases.</p>
 * 
 * <p>To start the watch, call {@link #start()}. At this point you can:</p>
 * <ul>
 *  <li>{@link #split()} the watch to get the time whilst the watch continues in the
 *   background. {@link #unsplit()} will remove the effect of the split. At this point,
 *   these three options are available again.</li>
 *  <li>{@link #suspend()} the watch to pause it. {@link #resume()} allows the watch
 *   to continue. Any time between the suspend and resume will not be counted in
 *   the total. At this point, these three options are available again.</li>
 *  <li>{@link #stop()} the watch to complete the timing session.</li>
 * </ul>
 *
 * <p>It is intended that the output methods {@link #toString()} and {@link #getTime()}
 * should only be called after stop, split or suspend, however a suitable result will
 * be returned at other points.</p>
 *
 * @author Henri Yandell
 * @author Stephen Colebourne
 * @since 2.0
 * @version $Id: StopWatch.java,v 1.8 2004/02/18 22:56:42 ggregory Exp $
 */
public class StopWatch {
    
    /**
     * The start time.
     */
    private long startTime = -1;
    /**
     * The stop time.
     */
    private long stopTime = -1;

    /**
     * <p>Constructor.</p>
     */
    public StopWatch() {
    }

    /**
     * <p>Start the stopwatch.</p>
     * 
     * <p>This method starts a new timing session, clearing any previous values.</p>
     */
    public void start() {
        stopTime = -1;
        startTime = System.currentTimeMillis();
    }

    /**
     * <p>Stop the stopwatch.</p>
     * 
     * <p>This method ends a new timing session, allowing the time to be retrieved.</p>
     */
    public void stop() {
        stopTime = System.currentTimeMillis();
    }

    /**
     * <p>Reset the stopwatch.</p>
     * 
     * <p>This method clears the internal values to allow the object to be reused.</p>
     */
    public void reset() {
        startTime = -1;
        stopTime = -1;
    }

    /**
     * <p>Split the time.</p>
     * 
     * <p>This method sets the stop time of the watch to allow a time to be extracted.
     * The start time is unaffected, enabling {@link #unsplit()} to continue the 
     * timing from the original start point.</p>
     */
    public void split() {
        stopTime = System.currentTimeMillis();
    }

    /**
     * <p>Remove a split.</p>
     * 
     * <p>This method clears the stop time. The start time is unaffected, enabling 
     * timing from the original start point to continue.</p>
     */
    public void unsplit() {
        stopTime = -1;
    }

    /**
     * <p>Suspend the stopwatch for later resumption.</p>
     * 
     * <p>This method suspends the watch until it is resumed. The watch will not include
     * time between the suspend and resume calls in the total time.</p>
     */
    public void suspend() {
        stopTime = System.currentTimeMillis();
    }

    /**
     * <p>Resume the stopwatch after a suspend.</p>
     * 
     * <p>This method resumes the watch after it was suspended. The watch will not include
     * time between the suspend and resume calls in the total time.</p>
     */
    public void resume() {
        startTime += (System.currentTimeMillis() - stopTime);
        stopTime = -1;
    }

    /**
     * <p>Get the time on the stopwatch.</p>
     * 
     * <p>This is either the time between start and latest split, between start
     * and stop, or the time between the start and the moment this method is called.</p>
     * 
     * @return the time in milliseconds
     */
    public long getTime() {
        if (stopTime == -1) {
            if (startTime == -1) {
                return 0;
            }
            return (System.currentTimeMillis() - this.startTime);
        }
        return (this.stopTime - this.startTime);
    }

    /**
     * <p>Gets a summary of the time that the stopwatch recorded as a string.</p>
     * 
     * <p>The format used is ISO8601-like,
     * <i>hours</i>:<i>minutes</i>:<i>seconds</i>.<i>milliseconds</i>.</p>
     * 
     * @return the time as a String
     */
    public String toString() {
        return DurationFormatUtils.formatISO(getTime());
    }

}
