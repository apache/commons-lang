/*
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002 The Apache Software Foundation.  All rights
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
 * 4. The names "The Jakarta Project", "Ant", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Group.
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

import java.util.Date;
import java.util.Calendar;
import java.util.TimeZone;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * TestCase for DateUtils.  [Relies heavily on code taken from the
 * DateUtilsTest class of the jakarata-ant project.]
 *
 * @author <a href="mailto:sbailliez@apache.org">Stephane Bailliez</a>
 * @author <a href="mailto:stefan.bodewig@epost.de">Stefan Bodewig</a>
 */
public class DateUtilsTest extends TestCase {

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
    	TestSuite suite = new TestSuite(DateUtilsTest.class);
    	suite.setName("DateUtils Tests");
        return suite;
    }

    public DateUtilsTest(String s) {
        super(s);
    }

    public void testElapsedTime(){
        String text = DateUtils.formatElapsedTime(50*1000);
        assertEquals("50 seconds", text);
        text = DateUtils.formatElapsedTime(65*1000);
        assertEquals("1 minute 5 seconds", text);
        text = DateUtils.formatElapsedTime(120*1000);
        assertEquals("2 minutes 0 seconds", text);
        text = DateUtils.formatElapsedTime(121*1000);
        assertEquals("2 minutes 1 second", text);
    }

    public void testDateTimeISO(){
        TimeZone timeZone = TimeZone.getTimeZone("GMT+1");
        Calendar cal = Calendar.getInstance(timeZone);
        cal.set(2002,1,23,10,11,12);
        String text = DateUtils.format(cal.getTime(),
                DateUtils.ISO8601_DATETIME_PATTERN);
        assertEquals("2002-02-23T09:11:12", text);
    }

    public void testDateISO(){
        TimeZone timeZone = TimeZone.getTimeZone("GMT");
        Calendar cal = Calendar.getInstance(timeZone);
        cal.set(2002,1,23);
        String text = DateUtils.format(cal.getTime(),
                DateUtils.ISO8601_DATE_PATTERN);
        assertEquals("2002-02-23", text);
    }

    public void testTimeISODate(){
        // make sure that elapsed time in set via date works
        TimeZone timeZone = TimeZone.getTimeZone("GMT+1");
        Calendar cal = Calendar.getInstance(timeZone);
        cal.set(2002,1,23, 21, 11, 12);
        String text = DateUtils.format(cal.getTime(),
                DateUtils.ISO8601_TIME_PATTERN);
        assertEquals("20:11:12", text);
    }

    public void testTimeISO(){
        // make sure that elapsed time in ms works
        long ms = (20*3600 + 11*60 + 12)*1000;
        String text = DateUtils.format(ms,
                DateUtils.ISO8601_TIME_PATTERN);
        assertEquals("20:11:12", text);
    }

    public void testPhaseOfMoon() {
        TimeZone timeZone = TimeZone.getTimeZone("GMT");
        Calendar cal = Calendar.getInstance(timeZone);
        // should be full moon
        cal.set(2002, 2, 27);
        assertEquals(4, DateUtils.getPhaseOfMoon(cal));
        // should be new moon
        cal.set(2002, 2, 12);
        assertEquals(0, DateUtils.getPhaseOfMoon(cal));
    }
}
