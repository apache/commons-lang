/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002-2003 The Apache Software Foundation.  All rights
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
 * Duration formatting utilites and constants.
 *
 * @author Apache Ant - DateUtils
 * @author <a href="mailto:sbailliez@apache.org">Stephane Bailliez</a>
 * @author <a href="mailto:stefan.bodewig@epost.de">Stefan Bodewig</a>
 * @author Stephen Colebourne
 * @since 2.0
 * @version $Id: DurationFormatUtils.java,v 1.2 2003/06/09 21:23:14 scolebourne Exp $
 */
class DurationFormatUtils {
    // TODO: Make class public once methods can fully select which fields to output

    /**
     * DurationFormatUtils instances should NOT be constructed in standard programming.
     * <p>
     * This constructor is public to permit tools that require a JavaBean instance
     * to operate.
     */
    public DurationFormatUtils() {
    }

    /**
     * Format an elapsed time into a plurialization correct string.
     * It is limited only to report elapsed time in minutes and
     * seconds and has the following behavior.
     * <ul>
     * <li>minutes are not displayed when 0. (ie: "45 seconds")</li>
     * <li>seconds are always displayed in plural form (ie "0 seconds" or
     * "10 seconds") except for 1 (ie "1 second")</li>
     * </ul>
     * 
     * @param millis  the elapsed time to report in milliseconds
     * @return the formatted text in minutes/seconds
     */
    public static String formatWords(long millis, boolean supressLeadingZeroElements, boolean supressTrailingZeroElements) {
        long[] values = new long[4];
        values[0] = millis / DateUtils.MILLIS_IN_DAY;
        values[1] = (millis / DateUtils.MILLIS_IN_HOUR) % 24;
        values[2] = (millis / DateUtils.MILLIS_IN_MINUTE) % 60;
        values[3] = (millis / DateUtils.MILLIS_IN_SECOND) % 60;
        String[] fieldsOne = {" day ", " hour ", " minute ", " second"};
        String[] fieldsPlural = {" days ", " hours ", " minutes ", " seconds"};
        
        StringBuffer buf = new StringBuffer(64);
        boolean valueOutput = false;
        
        for (int i = 0; i < 4; i++) {
            long value = values[i];
            if (value == 0) {
                // handle zero
                if (valueOutput) {
                    if (supressTrailingZeroElements == false) {
                        buf.append('0').append(fieldsPlural[i]);
                    }
                } else {
                    if (supressLeadingZeroElements == false) {
                        buf.append('0').append(fieldsPlural[i]);
                    }
                }
            } else if (value == 1) {
                // one
                valueOutput = true;
                buf.append('1').append(fieldsOne[i]);
            } else {
                // other
                valueOutput = true;
                buf.append(value).append(fieldsPlural[i]);
            }
        }
        
        return buf.toString().trim();
    }

    /**
     * <p>Get the time gap as a string.</p>
     * 
     * <p>The format used is ISO8601-like.
     * <i>hours</i>:<i>minutes</i>:<i>seconds</i>.<i>milliseconds</i>.</p>
     * 
     * @param millis  the duration to format
     * @return the time as a String
     */
    public static String formatISO(long millis) {
        int hours, minutes, seconds, milliseconds;
        hours = (int) (millis / DateUtils.MILLIS_IN_HOUR);
        millis = millis - (hours * DateUtils.MILLIS_IN_HOUR);
        minutes = (int) (millis / DateUtils.MILLIS_IN_MINUTE);
        millis = millis - (minutes * DateUtils.MILLIS_IN_MINUTE);
        seconds = (int) (millis / DateUtils.MILLIS_IN_SECOND);
        millis = millis - (seconds * DateUtils.MILLIS_IN_SECOND);
        milliseconds = (int) millis;

        StringBuffer buf = new StringBuffer(32);
        buf.append(hours);
        buf.append(':');
        buf.append((char)(minutes / 10 + '0'));
        buf.append((char)(minutes % 10 + '0'));
        buf.append(':');
        buf.append((char)(seconds / 10 + '0'));
        buf.append((char)(seconds % 10 + '0'));
        buf.append('.');
        if (milliseconds < 10) {
            buf.append('0').append('0');
        } else if (milliseconds < 100) {
            buf.append('0');
        }
        buf.append(milliseconds);
        return buf.toString();
    }

}
