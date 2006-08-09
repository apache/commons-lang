/*
 * Copyright 2002-2005 The Apache Software Foundation.
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

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.util.Calendar;
import java.util.TimeZone;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * TestCase for DurationFormatUtils.
 * 
 * @author Apache Ant - DateUtilsTest
 * @author <a href="mailto:sbailliez@apache.org">Stephane Bailliez</a>
 * @author <a href="mailto:stefan.bodewig@epost.de">Stefan Bodewig</a>
 * @author Stephen Colebourne
 * @author <a href="mailto:ggregory@seagullsw.com">Gary Gregory</a>
 * @author Henri Yandell
 */
public class DurationFormatUtilsTest extends TestCase {

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(DurationFormatUtilsTest.class);
        suite.setName("DurationFormatUtils Tests");
        return suite;
    }

    public DurationFormatUtilsTest(String s) {
        super(s);
    }

    // -----------------------------------------------------------------------
    public void testConstructor() {
        assertNotNull(new DurationFormatUtils());
        Constructor[] cons = DurationFormatUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertEquals(true, Modifier.isPublic(cons[0].getModifiers()));
        assertEquals(true, Modifier.isPublic(DurationFormatUtils.class.getModifiers()));
        assertEquals(false, Modifier.isFinal(DurationFormatUtils.class.getModifiers()));
    }

    // -----------------------------------------------------------------------
    public void testFormatDurationWords() {
        String text = null;

        text = DurationFormatUtils.formatDurationWords(50 * 1000, true, false);
        assertEquals("50 seconds", text);
        text = DurationFormatUtils.formatDurationWords(65 * 1000, true, false);
        assertEquals("1 minute 5 seconds", text);
        text = DurationFormatUtils.formatDurationWords(120 * 1000, true, false);
        assertEquals("2 minutes 0 seconds", text);
        text = DurationFormatUtils.formatDurationWords(121 * 1000, true, false);
        assertEquals("2 minutes 1 second", text);
        text = DurationFormatUtils.formatDurationWords(72 * 60 * 1000, true, false);
        assertEquals("1 hour 12 minutes 0 seconds", text);
        text = DurationFormatUtils.formatDurationWords(24 * 60 * 60 * 1000, true, false);
        assertEquals("1 day 0 hours 0 minutes 0 seconds", text);

        text = DurationFormatUtils.formatDurationWords(50 * 1000, true, true);
        assertEquals("50 seconds", text);
        text = DurationFormatUtils.formatDurationWords(65 * 1000, true, true);
        assertEquals("1 minute 5 seconds", text);
        text = DurationFormatUtils.formatDurationWords(120 * 1000, true, true);
        assertEquals("2 minutes", text);
        text = DurationFormatUtils.formatDurationWords(121 * 1000, true, true);
        assertEquals("2 minutes 1 second", text);
        text = DurationFormatUtils.formatDurationWords(72 * 60 * 1000, true, true);
        assertEquals("1 hour 12 minutes", text);
        text = DurationFormatUtils.formatDurationWords(24 * 60 * 60 * 1000, true, true);
        assertEquals("1 day", text);

        text = DurationFormatUtils.formatDurationWords(50 * 1000, false, true);
        assertEquals("0 days 0 hours 0 minutes 50 seconds", text);
        text = DurationFormatUtils.formatDurationWords(65 * 1000, false, true);
        assertEquals("0 days 0 hours 1 minute 5 seconds", text);
        text = DurationFormatUtils.formatDurationWords(120 * 1000, false, true);
        assertEquals("0 days 0 hours 2 minutes", text);
        text = DurationFormatUtils.formatDurationWords(121 * 1000, false, true);
        assertEquals("0 days 0 hours 2 minutes 1 second", text);
        text = DurationFormatUtils.formatDurationWords(72 * 60 * 1000, false, true);
        assertEquals("0 days 1 hour 12 minutes", text);
        text = DurationFormatUtils.formatDurationWords(24 * 60 * 60 * 1000, false, true);
        assertEquals("1 day", text);

        text = DurationFormatUtils.formatDurationWords(50 * 1000, false, false);
        assertEquals("0 days 0 hours 0 minutes 50 seconds", text);
        text = DurationFormatUtils.formatDurationWords(65 * 1000, false, false);
        assertEquals("0 days 0 hours 1 minute 5 seconds", text);
        text = DurationFormatUtils.formatDurationWords(120 * 1000, false, false);
        assertEquals("0 days 0 hours 2 minutes 0 seconds", text);
        text = DurationFormatUtils.formatDurationWords(121 * 1000, false, false);
        assertEquals("0 days 0 hours 2 minutes 1 second", text);
        text = DurationFormatUtils.formatDurationWords(72 * 60 * 1000, false, false);
        assertEquals("0 days 1 hour 12 minutes 0 seconds", text);
        text = DurationFormatUtils.formatDurationWords(24 * 60 * 60 * 1000 + 72 * 60 * 1000, false, false);
        assertEquals("1 day 1 hour 12 minutes 0 seconds", text);
        text = DurationFormatUtils.formatDurationWords(2 * 24 * 60 * 60 * 1000 + 72 * 60 * 1000, false, false);
        assertEquals("2 days 1 hour 12 minutes 0 seconds", text);
        for (int i = 2; i < 31; i++) {
            text = DurationFormatUtils.formatDurationWords(i * 24 * 60 * 60 * 1000, false, false);
            // assertEquals(i + " days 0 hours 0 minutes 0 seconds", text);
            //            
            // junit.framework.ComparisonFailure: expected:<25 days 0 hours 0 minutes 0...> but was:<-24 days -17 hours
            // -2 minutes -47...>
            // at junit.framework.Assert.assertEquals(Assert.java:81)
            // at junit.framework.Assert.assertEquals(Assert.java:87)
            // at
            // org.apache.commons.lang.time.DurationFormatUtilsTest.testFormatDurationWords(DurationFormatUtilsTest.java:124)
            // at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
            // at sun.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:39)
            // at sun.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:25)
            // at java.lang.reflect.Method.invoke(Method.java:324)
            // at junit.framework.TestCase.runTest(TestCase.java:154)
            // at junit.framework.TestCase.runBare(TestCase.java:127)
            // at junit.framework.TestResult$1.protect(TestResult.java:106)
            // at junit.framework.TestResult.runProtected(TestResult.java:124)
            // at junit.framework.TestResult.run(TestResult.java:109)
            // at junit.framework.TestCase.run(TestCase.java:118)
            // at junit.framework.TestSuite.runTest(TestSuite.java:208)
            // at junit.framework.TestSuite.run(TestSuite.java:203)
            // at org.eclipse.jdt.internal.junit.runner.RemoteTestRunner.runTests(RemoteTestRunner.java:478)
            // at org.eclipse.jdt.internal.junit.runner.RemoteTestRunner.run(RemoteTestRunner.java:344)
            // at org.eclipse.jdt.internal.junit.runner.RemoteTestRunner.main(RemoteTestRunner.java:196)
        }
    }

    /**
     * Tests that "1 <unit>s" gets converted to "1 <unit>" but that "11 <unit>s" is left alone.
     */
    public void testFormatDurationPluralWords() {
        long oneSecond = 1000;
        long oneMinute = oneSecond * 60;
        long oneHour = oneMinute * 60;
        long oneDay = oneHour * 24;
        String text = null;

        text = DurationFormatUtils.formatDurationWords(oneSecond, false, false);
        assertEquals("0 days 0 hours 0 minutes 1 second", text);
        text = DurationFormatUtils.formatDurationWords(oneSecond * 2, false, false);
        assertEquals("0 days 0 hours 0 minutes 2 seconds", text);
        text = DurationFormatUtils.formatDurationWords(oneSecond * 11, false, false);
        assertEquals("0 days 0 hours 0 minutes 11 seconds", text);

        text = DurationFormatUtils.formatDurationWords(oneMinute, false, false);
        assertEquals("0 days 0 hours 1 minute 0 seconds", text);
        text = DurationFormatUtils.formatDurationWords(oneMinute * 2, false, false);
        assertEquals("0 days 0 hours 2 minutes 0 seconds", text);
        text = DurationFormatUtils.formatDurationWords(oneMinute * 11, false, false);
        assertEquals("0 days 0 hours 11 minutes 0 seconds", text);
        text = DurationFormatUtils.formatDurationWords(oneMinute + oneSecond, false, false);
        assertEquals("0 days 0 hours 1 minute 1 second", text);

        text = DurationFormatUtils.formatDurationWords(oneHour, false, false);
        assertEquals("0 days 1 hour 0 minutes 0 seconds", text);
        text = DurationFormatUtils.formatDurationWords(oneHour * 2, false, false);
        assertEquals("0 days 2 hours 0 minutes 0 seconds", text);
        text = DurationFormatUtils.formatDurationWords(oneHour * 11, false, false);
        assertEquals("0 days 11 hours 0 minutes 0 seconds", text);
        text = DurationFormatUtils.formatDurationWords(oneHour + oneMinute + oneSecond, false, false);
        assertEquals("0 days 1 hour 1 minute 1 second", text);

        text = DurationFormatUtils.formatDurationWords(oneDay, false, false);
        assertEquals("1 day 0 hours 0 minutes 0 seconds", text);
        text = DurationFormatUtils.formatDurationWords(oneDay * 2, false, false);
        assertEquals("2 days 0 hours 0 minutes 0 seconds", text);
        text = DurationFormatUtils.formatDurationWords(oneDay * 11, false, false);
        assertEquals("11 days 0 hours 0 minutes 0 seconds", text);
        text = DurationFormatUtils.formatDurationWords(oneDay + oneHour + oneMinute + oneSecond, false, false);
        assertEquals("1 day 1 hour 1 minute 1 second", text);
    }

    public void testFormatDurationHMS() {
        long time = 0;
        assertEquals("0:00:00.000", DurationFormatUtils.formatDurationHMS(time));

        time = 1;
        assertEquals("0:00:00.001", DurationFormatUtils.formatDurationHMS(time));

        time = 15;
        assertEquals("0:00:00.015", DurationFormatUtils.formatDurationHMS(time));

        time = 165;
        assertEquals("0:00:00.165", DurationFormatUtils.formatDurationHMS(time));

        time = 1675;
        assertEquals("0:00:01.675", DurationFormatUtils.formatDurationHMS(time));

        time = 13465;
        assertEquals("0:00:13.465", DurationFormatUtils.formatDurationHMS(time));

        time = 72789;
        assertEquals("0:01:12.789", DurationFormatUtils.formatDurationHMS(time));

        time = 12789 + 32 * 60000;
        assertEquals("0:32:12.789", DurationFormatUtils.formatDurationHMS(time));

        time = 12789 + 62 * 60000;
        assertEquals("1:02:12.789", DurationFormatUtils.formatDurationHMS(time));
    }

    public void testFormatDurationISO() {
        assertEquals("P0Y0M0DT0H0M0.000S", DurationFormatUtils.formatDurationISO(0L));
        assertEquals("P0Y0M0DT0H0M0.001S", DurationFormatUtils.formatDurationISO(1L));
        assertEquals("P0Y0M0DT0H0M0.010S", DurationFormatUtils.formatDurationISO(10L));
        assertEquals("P0Y0M0DT0H0M0.100S", DurationFormatUtils.formatDurationISO(100L));
        assertEquals("P0Y0M0DT0H1M15.321S", DurationFormatUtils.formatDurationISO(75321L));
    }

    public void testFormatDuration() {
        long duration = 0;
        assertEquals("0", DurationFormatUtils.formatDuration(duration, "y"));
        assertEquals("0", DurationFormatUtils.formatDuration(duration, "M"));
        assertEquals("0", DurationFormatUtils.formatDuration(duration, "d"));
        assertEquals("0", DurationFormatUtils.formatDuration(duration, "H"));
        assertEquals("0", DurationFormatUtils.formatDuration(duration, "m"));
        assertEquals("0", DurationFormatUtils.formatDuration(duration, "s"));
        assertEquals("0", DurationFormatUtils.formatDuration(duration, "S"));
        assertEquals("0000", DurationFormatUtils.formatDuration(duration, "SSSS"));
        assertEquals("0000", DurationFormatUtils.formatDuration(duration, "yyyy"));
        assertEquals("0000", DurationFormatUtils.formatDuration(duration, "yyMM"));

        duration = 60 * 1000;
        assertEquals("0", DurationFormatUtils.formatDuration(duration, "y"));
        assertEquals("0", DurationFormatUtils.formatDuration(duration, "M"));
        assertEquals("0", DurationFormatUtils.formatDuration(duration, "d"));
        assertEquals("0", DurationFormatUtils.formatDuration(duration, "H"));
        assertEquals("1", DurationFormatUtils.formatDuration(duration, "m"));
        assertEquals("60", DurationFormatUtils.formatDuration(duration, "s"));
        assertEquals("60000", DurationFormatUtils.formatDuration(duration, "S"));
        assertEquals("01:00", DurationFormatUtils.formatDuration(duration, "mm:ss"));

        Calendar base = Calendar.getInstance();
        base.set(2000, 0, 1, 0, 0, 0);
        base.set(Calendar.MILLISECOND, 0);

        Calendar cal = Calendar.getInstance();
        cal.set(2003, 1, 1, 0, 0, 0);
        cal.set(Calendar.MILLISECOND, 0);
        duration = cal.getTime().getTime() - base.getTime().getTime(); // duration from 2000-01-01 to cal
        // don't use 1970 in test as time zones were less reliable in 1970 than now
        // remember that duration formatting ignores time zones, working on strict hour lengths
        int days = 366 + 365 + 365 + 31;
        assertEquals("0 0 " + days, DurationFormatUtils.formatDuration(duration, "y M d"));
    }

    public void testFormatPeriodISO() {
        TimeZone timeZone = TimeZone.getTimeZone("GMT-3");
        Calendar base = Calendar.getInstance(timeZone);
        base.set(1970, 0, 1, 0, 0, 0);
        base.set(Calendar.MILLISECOND, 0);

        Calendar cal = Calendar.getInstance(timeZone);
        cal.set(2002, 1, 23, 9, 11, 12);
        cal.set(Calendar.MILLISECOND, 1);
        String text;
        // repeat a test from testDateTimeISO to compare extended and not extended.
        text = DateFormatUtils.ISO_DATETIME_TIME_ZONE_FORMAT.format(cal);
        assertEquals("2002-02-23T09:11:12-03:00", text);
        // test fixture is the same as above, but now with extended format.
        text = DurationFormatUtils.formatPeriod(base.getTime().getTime(), cal.getTime().getTime(),
                DurationFormatUtils.ISO_EXTENDED_FORMAT_PATTERN, false, timeZone);
        assertEquals("P32Y1M22DT9H11M12.001S", text);
        // test fixture from example in http://www.w3.org/TR/xmlschema-2/#duration
        cal.set(1971, 1, 3, 10, 30, 0);
        cal.set(Calendar.MILLISECOND, 0);
        text = DurationFormatUtils.formatPeriod(base.getTime().getTime(), cal.getTime().getTime(),
                DurationFormatUtils.ISO_EXTENDED_FORMAT_PATTERN, false, timeZone);
        assertEquals("P1Y1M2DT10H30M0.000S", text);
        // want a way to say 'don't print the seconds in format()' or other fields for that matter:
        // assertEquals("P1Y2M3DT10H30M", text);
    }

    public void testFormatPeriod() {
        Calendar cal1970 = Calendar.getInstance();
        cal1970.set(1970, 0, 1, 0, 0, 0);
        cal1970.set(Calendar.MILLISECOND, 0);
        long time1970 = cal1970.getTime().getTime();

        assertEquals("0", DurationFormatUtils.formatPeriod(time1970, time1970, "y"));
        assertEquals("0", DurationFormatUtils.formatPeriod(time1970, time1970, "M"));
        assertEquals("0", DurationFormatUtils.formatPeriod(time1970, time1970, "d"));
        assertEquals("0", DurationFormatUtils.formatPeriod(time1970, time1970, "H"));
        assertEquals("0", DurationFormatUtils.formatPeriod(time1970, time1970, "m"));
        assertEquals("0", DurationFormatUtils.formatPeriod(time1970, time1970, "s"));
        assertEquals("0", DurationFormatUtils.formatPeriod(time1970, time1970, "S"));
        assertEquals("0000", DurationFormatUtils.formatPeriod(time1970, time1970, "SSSS"));
        assertEquals("0000", DurationFormatUtils.formatPeriod(time1970, time1970, "yyyy"));
        assertEquals("0000", DurationFormatUtils.formatPeriod(time1970, time1970, "yyMM"));

        long time = time1970 + 60 * 1000;
        assertEquals("0", DurationFormatUtils.formatPeriod(time1970, time, "y"));
        assertEquals("0", DurationFormatUtils.formatPeriod(time1970, time, "M"));
        assertEquals("0", DurationFormatUtils.formatPeriod(time1970, time, "d"));
        assertEquals("0", DurationFormatUtils.formatPeriod(time1970, time, "H"));
        assertEquals("1", DurationFormatUtils.formatPeriod(time1970, time, "m"));
        assertEquals("60", DurationFormatUtils.formatPeriod(time1970, time, "s"));
        assertEquals("60000", DurationFormatUtils.formatPeriod(time1970, time, "S"));
        assertEquals("01:00", DurationFormatUtils.formatPeriod(time1970, time, "mm:ss"));

        Calendar cal = Calendar.getInstance();
        cal.set(1973, 6, 1, 0, 0, 0);
        cal.set(Calendar.MILLISECOND, 0);
        time = cal.getTime().getTime();
        assertEquals("36", DurationFormatUtils.formatPeriod(time1970, time, "yM"));
        assertEquals("3 years 6 months", DurationFormatUtils.formatPeriod(time1970, time, "y' years 'M' months'"));
        assertEquals("03/06", DurationFormatUtils.formatPeriod(time1970, time, "yy/MM"));

        cal.set(1973, 10, 1, 0, 0, 0);
        cal.set(Calendar.MILLISECOND, 0);
        time = cal.getTime().getTime();
        assertEquals("310", DurationFormatUtils.formatPeriod(time1970, time, "yM"));
        assertEquals("3 years 10 months", DurationFormatUtils.formatPeriod(time1970, time, "y' years 'M' months'"));
        assertEquals("03/10", DurationFormatUtils.formatPeriod(time1970, time, "yy/MM"));

        cal.set(1974, 0, 1, 0, 0, 0);
        cal.set(Calendar.MILLISECOND, 0);
        time = cal.getTime().getTime();
        assertEquals("40", DurationFormatUtils.formatPeriod(time1970, time, "yM"));
        assertEquals("4 years 0 months", DurationFormatUtils.formatPeriod(time1970, time, "y' years 'M' months'"));
        assertEquals("04/00", DurationFormatUtils.formatPeriod(time1970, time, "yy/MM"));
        assertEquals("48", DurationFormatUtils.formatPeriod(time1970, time, "M"));
        assertEquals("48", DurationFormatUtils.formatPeriod(time1970, time, "MM"));
        assertEquals("048", DurationFormatUtils.formatPeriod(time1970, time, "MMM"));
    }

    public void testLexx() {
        // tests each constant
        assertArrayEquals(new DurationFormatUtils.Token[]{
            new DurationFormatUtils.Token(DurationFormatUtils.y, 1),
            new DurationFormatUtils.Token(DurationFormatUtils.M, 1),
            new DurationFormatUtils.Token(DurationFormatUtils.d, 1),
            new DurationFormatUtils.Token(DurationFormatUtils.H, 1),
            new DurationFormatUtils.Token(DurationFormatUtils.m, 1),
            new DurationFormatUtils.Token(DurationFormatUtils.s, 1),
            new DurationFormatUtils.Token(DurationFormatUtils.S, 1)}, DurationFormatUtils.lexx("yMdHmsS"));

        // tests the ISO8601-like
        assertArrayEquals(new DurationFormatUtils.Token[]{
            new DurationFormatUtils.Token(DurationFormatUtils.H, 1),
            new DurationFormatUtils.Token(new StringBuffer(":"), 1),
            new DurationFormatUtils.Token(DurationFormatUtils.m, 2),
            new DurationFormatUtils.Token(new StringBuffer(":"), 1),
            new DurationFormatUtils.Token(DurationFormatUtils.s, 2),
            new DurationFormatUtils.Token(new StringBuffer("."), 1),
            new DurationFormatUtils.Token(DurationFormatUtils.S, 3)}, DurationFormatUtils.lexx("H:mm:ss.SSS"));

        // test the iso extended format
        assertArrayEquals(new DurationFormatUtils.Token[]{
            new DurationFormatUtils.Token(new StringBuffer("P"), 1),
            new DurationFormatUtils.Token(DurationFormatUtils.y, 4),
            new DurationFormatUtils.Token(new StringBuffer("Y"), 1),
            new DurationFormatUtils.Token(DurationFormatUtils.M, 1),
            new DurationFormatUtils.Token(new StringBuffer("M"), 1),
            new DurationFormatUtils.Token(DurationFormatUtils.d, 1),
            new DurationFormatUtils.Token(new StringBuffer("DT"), 1),
            new DurationFormatUtils.Token(DurationFormatUtils.H, 1),
            new DurationFormatUtils.Token(new StringBuffer("H"), 1),
            new DurationFormatUtils.Token(DurationFormatUtils.m, 1),
            new DurationFormatUtils.Token(new StringBuffer("M"), 1),
            new DurationFormatUtils.Token(DurationFormatUtils.s, 1),
            new DurationFormatUtils.Token(new StringBuffer("."), 1),
            new DurationFormatUtils.Token(DurationFormatUtils.S, 1),
            new DurationFormatUtils.Token(new StringBuffer("S"), 1)}, DurationFormatUtils
                .lexx(DurationFormatUtils.ISO_EXTENDED_FORMAT_PATTERN));

        // test failures in equals
        DurationFormatUtils.Token token = new DurationFormatUtils.Token(DurationFormatUtils.y, 4);
        assertFalse("Token equal to non-Token class. ", token.equals(new Object()));
        assertFalse("Token equal to Token with wrong value class. ", token.equals(new DurationFormatUtils.Token(
                new Object())));
        assertFalse("Token equal to Token with different count. ", token.equals(new DurationFormatUtils.Token(
                DurationFormatUtils.y, 1)));
        DurationFormatUtils.Token numToken = new DurationFormatUtils.Token(new Integer(1), 4);
        assertTrue("Token with Number value not equal to itself. ", numToken.equals(numToken));
    }


    // http://issues.apache.org/bugzilla/show_bug.cgi?id=38401
    public void testBugzilla38401() {
        Calendar cal1 = Calendar.getInstance();
        cal1.set(2006, 0, 26, 18, 47, 34);
        Calendar cal2 = Calendar.getInstance();
        cal2.set(2006, 1, 26, 10, 47, 34);

        assertEquals( "0000/00/30 16:00:00 000", DurationFormatUtils.formatPeriod(cal1.getTime().getTime(), cal2.getTime().getTime(), "yyyy/MM/dd HH:mm:ss SSS") );
    }

    private void assertArrayEquals(DurationFormatUtils.Token[] obj1, DurationFormatUtils.Token[] obj2) {
        assertEquals("Arrays are unequal length. ", obj1.length, obj2.length);
        for (int i = 0; i < obj1.length; i++) {
            assertTrue("Index " + i + " not equal, " + obj1[i] + " vs " + obj2, obj1[i].equals(obj2[i]));
        }
    }

}
