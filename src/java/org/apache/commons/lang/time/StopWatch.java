/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 1999-2002 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
 *
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
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
 * <li>{@link #split()} the watch to get the time whilst the watch continues in the
 *  background. {@link #unsplit()} will remove the effect of the split. At this point,
 *  these three options are available again.
 * <li>{@link #suspend()} the watch to pause it. {@link #resume()} allows the watch
 *  to continue. Any time between the suspend and resume will not be counted in
 *  the total. At this point, these three options are available again.
 * <li>{@link #stop()} the watch to complete the timing session.
 * </ul>
 * <p>It is intended that the output methods {@link #toString()} and {@link #getTime()}
 * should only be called after stop, split or suspend, however a suitable result will
 * be returned at other points.</p>
 *
 * @author Henri Yandell
 * @author Stephen Colebourne
 * @since 2.0
 * @version $Id: StopWatch.java,v 1.1 2002/12/22 22:59:58 scolebourne Exp $
 */
public class StopWatch {
    
    private static final int MILLIS_IN_HOUR = 60 * 60 * 1000;
    private static final int MILLIS_IN_MINUTE = 60 * 1000;

    /** The start time */
    private long startTime = -1;
    /** The stop time */
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
     * The start time is unaffected, enabling {@link #unsplit()} to contine the 
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
     * <p>The format used is ISO8601,
     * <i>hours</i>:<i>minutes</i>:<i>seconds</i>.<i>milliseconds</i>.</p>
     * 
     * @return the time as a String
     */
    public String toString() {
        return StopWatch.toString(getTime());
    }

    /**
     * <p>Get the time gap as a string.</p>
     * 
     * <p>The format used is ISO8601,
     * <i>hours</i>:<i>minutes</i>:<i>seconds</i>.<i>milliseconds</i>.</p>
     * 
     * @return the time as a String
     */
    public static String toString(long time) {
        int hours, minutes, seconds, milliseconds;
        hours = (int) (time / MILLIS_IN_HOUR);
        time = time - (hours * MILLIS_IN_HOUR);
        minutes = (int) (time / MILLIS_IN_MINUTE);
        time = time - (minutes * MILLIS_IN_MINUTE);
        seconds = (int) (time / 1000);
        time = time - (seconds * 1000);
        milliseconds = (int) time;

        StringBuffer buf = new StringBuffer(32);
        buf.append(hours);
        buf.append(':');
        if (minutes < 10) {
            buf.append('0');
        }
        buf.append(minutes);
        buf.append(':');
        if (seconds < 10) {
            buf.append('0');
        }
        buf.append(seconds);
        buf.append('.');
        if (milliseconds < 10) {
            buf.append("00");
        } else if (milliseconds < 100) {
            buf.append('0');
        }
        buf.append(milliseconds);
        return buf.toString();
    }

}
