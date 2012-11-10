/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional inparserion regarding copyright ownership.
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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import java.io.Serializable;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;

import junit.framework.Assert;

import org.apache.commons.lang3.SerializationUtils;
import org.junit.Test;

/**
 * Unit tests {@link org.apache.commons.lang3.time.FastDateParser}.
 *
 * @since 3.2
 */
public class FastDateParserTest {
    private static final String SHORT_FORMAT_NOERA = "y/M/d/h/a/m/E/Z";
    private static final String LONG_FORMAT_NOERA = "yyyy/MMMM/dddd/hhhh/mmmm/aaaa/EEEE/ZZZZ";
    private static final String SHORT_FORMAT = "G/" + SHORT_FORMAT_NOERA;
    private static final String LONG_FORMAT = "GGGG/" + LONG_FORMAT_NOERA;

    private static final String yMdHmsSZ = "yyyy-MM-dd'T'HH:mm:ss.SSS Z";
    private static final String DMY_DOT = "dd.MM.yyyy";
    private static final String YMD_SLASH = "yyyy/MM/dd";
    private static final String MDY_DASH = "MM-DD-yyyy";
    private static final String MDY_SLASH = "MM/DD/yyyy";

    private static final TimeZone REYKJAVIK = TimeZone.getTimeZone("Atlantic/Reykjavik");
    private static final TimeZone NEW_YORK = TimeZone.getTimeZone("America/New_York");
    private static final TimeZone GMT = TimeZone.getTimeZone("GMT");

    private static final Locale SWEDEN = new Locale("sv", "SE");

    DateParser getInstance(String format) {
        return getInstance(format, TimeZone.getDefault(), Locale.getDefault());
    }

    private DateParser getDateInstance(int dateStyle, Locale locale) {
        return getInstance(FormatCache.getPatternForStyle(Integer.valueOf(dateStyle), null, locale), TimeZone.getDefault(), Locale.getDefault());
    }

    private DateParser getInstance(String format, Locale locale) {
        return getInstance(format, TimeZone.getDefault(), locale);
    }

    private DateParser getInstance(String format, TimeZone timeZone) {
        return getInstance(format, timeZone, Locale.getDefault());
    }

    /**
     * Override this method in derived tests to change the construction of instances
     */
    protected DateParser getInstance(String format, TimeZone timeZone, Locale locale) {
        return new FastDateParser(format, timeZone, locale);
    }

    @Test
    public void test_Equality_Hash() {
        DateParser[] parsers= {
            getInstance(yMdHmsSZ, NEW_YORK, Locale.US),
            getInstance(DMY_DOT, NEW_YORK, Locale.US),
            getInstance(YMD_SLASH, NEW_YORK, Locale.US),
            getInstance(MDY_DASH, NEW_YORK, Locale.US),
            getInstance(MDY_SLASH, NEW_YORK, Locale.US),
            getInstance(MDY_SLASH, REYKJAVIK, Locale.US),
            getInstance(MDY_SLASH, REYKJAVIK, SWEDEN)
        };

        Map<DateParser,Integer> map= new HashMap<DateParser,Integer>();
        int i= 0;
        for(DateParser parser:parsers) {
            map.put(parser, Integer.valueOf(i++));
        }

        i= 0;
        for(DateParser parser:parsers) {
            assertEquals(i++, map.get(parser).intValue());
        }
    }

    @Test
    public void testParseZone() throws ParseException {
        Calendar cal= Calendar.getInstance(NEW_YORK, Locale.US);
        cal.clear();
        cal.set(2003, 6, 10, 16, 33, 20);

        DateParser fdf = getInstance(yMdHmsSZ, NEW_YORK, Locale.US);

        assertEquals(cal.getTime(), fdf.parse("2003-07-10T15:33:20.000 -0500"));
        assertEquals(cal.getTime(), fdf.parse("2003-07-10T15:33:20.000 GMT-05:00"));
        assertEquals(cal.getTime(), fdf.parse("2003-07-10T16:33:20.000 Eastern Daylight Time"));
        assertEquals(cal.getTime(), fdf.parse("2003-07-10T16:33:20.000 EDT"));

        cal.setTimeZone(TimeZone.getTimeZone("GMT-3"));
        cal.set(2003, 1, 10, 9, 0, 0);

        assertEquals(cal.getTime(), fdf.parse("2003-02-10T09:00:00.000 -0300"));

        cal.setTimeZone(TimeZone.getTimeZone("GMT+5"));
        cal.set(2003, 1, 10, 15, 5, 6);

        assertEquals(cal.getTime(), fdf.parse("2003-02-10T15:05:06.000 +0500"));
    }

    @Test
    public void testParseLongShort() throws ParseException {
        Calendar cal= Calendar.getInstance(NEW_YORK, Locale.US);
        cal.clear();
        cal.set(2003, 1, 10, 15, 33, 20);
        cal.set(Calendar.MILLISECOND, 989);
        cal.setTimeZone(NEW_YORK);

        DateParser fdf = getInstance("yyyy GGGG MMMM dddd aaaa EEEE HHHH mmmm ssss SSSS ZZZZ", NEW_YORK, Locale.US);

        assertEquals(cal.getTime(), fdf.parse("2003 AD February 0010 PM Monday 0015 0033 0020 0989 GMT-05:00"));
        cal.set(Calendar.ERA, GregorianCalendar.BC);

        Date parse = fdf.parse("2003 BC February 0010 PM Saturday 0015 0033 0020 0989 GMT-05:00");
                assertEquals(cal.getTime(), parse);

        fdf = getInstance("y G M d a E H m s S Z", NEW_YORK, Locale.US);
        assertEquals(cal.getTime(), fdf.parse("03 BC 2 10 PM Sat 15 33 20 989 -0500"));

        cal.set(Calendar.ERA, GregorianCalendar.AD);
        assertEquals(cal.getTime(), fdf.parse("03 AD 2 10 PM Saturday 15 33 20 989 -0500"));
    }

    @Test
    public void testAmPm() throws ParseException {
        Calendar cal= Calendar.getInstance(NEW_YORK, Locale.US);
        cal.clear();

        DateParser h = getInstance("yyyy-MM-dd hh a mm:ss", NEW_YORK, Locale.US);
        DateParser K = getInstance("yyyy-MM-dd KK a mm:ss", NEW_YORK, Locale.US);
        DateParser k = getInstance("yyyy-MM-dd kk:mm:ss", NEW_YORK, Locale.US);
        DateParser H = getInstance("yyyy-MM-dd HH:mm:ss", NEW_YORK, Locale.US);

        cal.set(2010, 7, 1, 0, 33, 20);
        assertEquals(cal.getTime(), h.parse("2010-08-01 12 AM 33:20"));
        assertEquals(cal.getTime(), K.parse("2010-08-01 0 AM 33:20"));
        assertEquals(cal.getTime(), k.parse("2010-08-01 00:33:20"));
        assertEquals(cal.getTime(), H.parse("2010-08-01 00:33:20"));

        cal.set(2010, 7, 1, 3, 33, 20);
        assertEquals(cal.getTime(), h.parse("2010-08-01 3 AM 33:20"));
        assertEquals(cal.getTime(), K.parse("2010-08-01 3 AM 33:20"));
        assertEquals(cal.getTime(), k.parse("2010-08-01 03:33:20"));
        assertEquals(cal.getTime(), H.parse("2010-08-01 03:33:20"));

        cal.set(2010, 7, 1, 15, 33, 20);
        assertEquals(cal.getTime(), h.parse("2010-08-01 3 PM 33:20"));
        assertEquals(cal.getTime(), K.parse("2010-08-01 3 PM 33:20"));
        assertEquals(cal.getTime(), k.parse("2010-08-01 15:33:20"));
        assertEquals(cal.getTime(), H.parse("2010-08-01 15:33:20"));

        cal.set(2010, 7, 1, 12, 33, 20);
        assertEquals(cal.getTime(), h.parse("2010-08-01 12 PM 33:20"));
        assertEquals(cal.getTime(), K.parse("2010-08-01 0 PM 33:20"));
        assertEquals(cal.getTime(), k.parse("2010-08-01 12:33:20"));
        assertEquals(cal.getTime(), H.parse("2010-08-01 12:33:20"));
    }

    @Test
    // Check that all Locales can parse the formats we use
    public void testParses() throws Exception {
        for(Locale locale : Locale.getAvailableLocales()) {
            for(TimeZone tz : new TimeZone[]{NEW_YORK, GMT}) {
                Calendar cal = Calendar.getInstance(tz);
                for(int year : new int[]{2003, 1940, 1868, 1867, 0, -1940}) {
                    // http://docs.oracle.com/javase/6/docs/technotes/guides/intl/calendar.doc.html
                    if (year < 1868 && locale.equals(FastDateParser.JAPANESE_IMPERIAL)) {
                        continue; // Japanese imperial calendar does not support eras before 1868
                    }
                    cal.clear();
                    if (year < 0) {
                        cal.set(-year, 1, 10);
                        cal.set(Calendar.ERA, GregorianCalendar.BC);
                    } else {
                        cal.set(year, 1, 10);
                    }
                    Date in = cal.getTime();
                    for(String format : new String[]{LONG_FORMAT, SHORT_FORMAT}) {
                        SimpleDateFormat sdf = new SimpleDateFormat(format, locale);
                        if (format.equals(SHORT_FORMAT)) {
                            if (year < 1930) {
                                sdf.set2DigitYearStart(cal.getTime());
                            }
                        }
                        String fmt = sdf.format(in);
                        try {
                            Date out = sdf.parse(fmt);

                            assertEquals(locale.toString()+" "+year+" "+ format+ " "+tz.getID(), in, out);
                        } catch (ParseException pe) {
                            System.out.println(fmt+" "+locale.toString()+" "+year+" "+ format+ " "+tz.getID());
                            throw pe;
                        }
                    }
                }
            }
        }
    }

    @Test
    public void testLocales_Long_AD() throws Exception {
        testLocales(LONG_FORMAT, false);
    }

    @Test
    public void testLocales_Long_BC() throws Exception {
        testLocales(LONG_FORMAT, true);
    }

    @Test
    public void testLocales_Short_AD() throws Exception {
        testLocales(SHORT_FORMAT, false);
    }

    @Test
    public void testLocales_Short_BC() throws Exception {
        testLocales(SHORT_FORMAT, true);
    }

    @Test
    public void testLocales_LongNoEra_AD() throws Exception {
        testLocales(LONG_FORMAT_NOERA, false);
    }

    @Test
    public void testLocales_LongNoEra_BC() throws Exception {
        testLocales(LONG_FORMAT_NOERA, true);
    }

    @Test
    public void testLocales_ShortNoEra_AD() throws Exception {
        testLocales(SHORT_FORMAT_NOERA, false);
    }

    @Test
    public void testLocales_ShortNoEra_BC() throws Exception {
        testLocales(SHORT_FORMAT_NOERA, true);
    }

    private void testLocales(String format, boolean eraBC) throws Exception {

        Calendar cal= Calendar.getInstance(GMT);
        cal.clear();
        cal.set(2003, 1, 10);
        if (eraBC) {
            cal.set(Calendar.ERA, GregorianCalendar.BC);
        }
        for(Locale locale : Locale.getAvailableLocales()) {
            // ja_JP_JP cannot handle dates before 1868 properly
            if (eraBC && locale.equals(FastDateParser.JAPANESE_IMPERIAL)) {
                continue;
            }
            SimpleDateFormat sdf = new SimpleDateFormat(format, locale);
            DateParser fdf = getInstance(format, locale);

            try {
                checkParse(locale, cal, sdf, fdf);
            } catch(ParseException ex) {
                Assert.fail("Locale "+locale+ " failed with "+format+" era "+(eraBC?"BC":"AD")+"\n" + trimMessage(ex.toString()));
            }
        }
    }

    private String trimMessage(String msg) {
        if (msg.length() < 100) {
            return msg;
        }
        int gmt = msg.indexOf("(GMT");
        if (gmt > 0) {
            return msg.substring(0, gmt+4)+"...)";
        }
        return msg.substring(0, 100)+"...";
    }

    private void checkParse(Locale locale, Calendar cal, SimpleDateFormat sdf, DateParser fdf) throws ParseException {
        String formattedDate= sdf.format(cal.getTime());
        Date expectedTime = sdf.parse(formattedDate);
        Date actualTime = fdf.parse(formattedDate);
        assertEquals(locale.toString()+" "+formattedDate
                +"\n",expectedTime, actualTime);
    }

    @Test
    public void testParseNumerics() throws ParseException {
        Calendar cal= Calendar.getInstance(NEW_YORK, Locale.US);
        cal.clear();
        cal.set(2003, 1, 10, 15, 33, 20);
        cal.set(Calendar.MILLISECOND, 989);

        DateParser fdf = getInstance("yyyyMMddHHmmssSSS", NEW_YORK, Locale.US);
        assertEquals(cal.getTime(), fdf.parse("20030210153320989"));
    }

    @Test
    public void testQuotes() throws ParseException {
        Calendar cal= Calendar.getInstance(NEW_YORK, Locale.US);
        cal.clear();
        cal.set(2003, 1, 10, 15, 33, 20);
        cal.set(Calendar.MILLISECOND, 989);

        DateParser fdf = getInstance("''yyyyMMdd'A''B'HHmmssSSS''", NEW_YORK, Locale.US);
        assertEquals(cal.getTime(), fdf.parse("'20030210A'B153320989'"));
    }

    @Test
    public void testSpecialCharacters() throws Exception {
        testSdfAndFdp("q" ,"", true); // bad pattern character (at present)
        testSdfAndFdp("Q" ,"", true); // bad pattern character
        testSdfAndFdp("$" ,"$", false); // OK
        testSdfAndFdp("?.d" ,"?.12", false); // OK
        testSdfAndFdp("''yyyyMMdd'A''B'HHmmssSSS''", "'20030210A'B153320989'", false); // OK
        testSdfAndFdp("''''yyyyMMdd'A''B'HHmmssSSS''", "''20030210A'B153320989'", false); // OK
        testSdfAndFdp("'$\\Ed'" ,"$\\Ed", false); // OK
    }

    @Test
    public void testLANG_832() throws Exception {
        testSdfAndFdp("'d'd" ,"d3", false); // OK
        testSdfAndFdp("'d'd'","d3", true); // should fail (unterminated quote)
    }

    @Test
    public void testLANG_831() throws Exception {
        testSdfAndFdp("M E","3  Tue", true);
    }

    private void testSdfAndFdp(String format, String date, boolean shouldFail)
            throws Exception {
        final boolean debug = false;
        Date dfdp = null;
        Date dsdf = null;
        Throwable f = null;
        Throwable s = null;

        try {
            SimpleDateFormat sdf = new SimpleDateFormat(format, Locale.US);
            sdf.setTimeZone(NEW_YORK);
            dsdf = sdf.parse(date);
            if (shouldFail) {
                Assert.fail("Expected SDF failure, but got " + dsdf + " for ["+format+","+date+"]");
            }
        } catch (Exception e) {
            s = e;
            if (!shouldFail) {
                throw e;
            }
            if (debug) {
                System.out.println("sdf:"+format+"/"+date+"=>"+e);
            }
        }

        try {
            DateParser fdp = getInstance(format, NEW_YORK, Locale.US);
            dfdp = fdp.parse(date);
            if (shouldFail) {
                Assert.fail("Expected FDF failure, but got " + dfdp + " for ["+format+","+date+"] using "+((FastDateParser)fdp).getParsePattern());
            }
        } catch (Exception e) {
            f = e;
            if (!shouldFail) {
                throw e;
            }
            if (debug) {
                System.out.println("fdf:"+format+"/"+date+"=>"+e);
            }
        }
        // SDF and FDF should produce equivalent results
        assertTrue("Should both or neither throw Exceptions", (f==null)==(s==null));
        assertEquals("Parsed dates should be equal", dsdf, dfdp);
        if (debug) {
            System.out.println(format + "," + date + " => " + dsdf);
        }
    }

    @Test
    public void testDayOf() throws ParseException {
        Calendar cal= Calendar.getInstance(NEW_YORK, Locale.US);
        cal.clear();
        cal.set(2003, 1, 10);

        DateParser fdf = getInstance("W w F D y", NEW_YORK, Locale.US);
        assertEquals(cal.getTime(), fdf.parse("3 7 2 41 03"));
    }

    /**
     * Test case for {@link FastDateParser#FastDateParser(String, TimeZone, Locale)}.
     * @throws ParseException
     */
    @Test
    public void testShortDateStyleWithLocales() throws ParseException {
        DateParser fdf = getDateInstance(FastDateFormat.SHORT, Locale.US);
        Calendar cal = Calendar.getInstance();
        cal.clear();

        cal.set(2004, 1, 3);
        assertEquals(cal.getTime(), fdf.parse("2/3/04"));

        fdf = getDateInstance(FastDateFormat.SHORT, SWEDEN);
        assertEquals(cal.getTime(), fdf.parse("2004-02-03"));
    }

    /**
     * Tests that pre-1000AD years get padded with yyyy
     * @throws ParseException
     */
    @Test
    public void testLowYearPadding() throws ParseException {
        DateParser parser = getInstance(YMD_SLASH);
        Calendar cal = Calendar.getInstance();
        cal.clear();

        cal.set(1,0,1);
        assertEquals(cal.getTime(), parser.parse("0001/01/01"));
        cal.set(10,0,1);
        assertEquals(cal.getTime(), parser.parse("0010/01/01"));
        cal.set(100,0,1);
        assertEquals(cal.getTime(), parser.parse("0100/01/01"));
        cal.set(999,0,1);
        assertEquals(cal.getTime(), parser.parse("0999/01/01"));
    }

    /**
     * @throws ParseException
     */
    @Test
    public void testMilleniumBug() throws ParseException {
        DateParser parser = getInstance(DMY_DOT);
        Calendar cal = Calendar.getInstance();
        cal.clear();

        cal.set(1000,0,1);
        assertEquals(cal.getTime(), parser.parse("01.01.1000"));
    }

    @Test
    public void testLang303() throws ParseException {
        DateParser parser = getInstance(YMD_SLASH);
        Calendar cal = Calendar.getInstance();
        cal.set(2004, 11, 31);

        Date date = parser.parse("2004/11/31");

        parser = SerializationUtils.deserialize(SerializationUtils.serialize((Serializable) parser));
        assertEquals(date, parser.parse("2004/11/31"));
    }

    @Test
    public void testLang538() throws ParseException {
        DateParser parser = getInstance("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'", GMT);

        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT-8"));
        cal.clear();
        cal.set(2009, 9, 16, 8, 42, 16);

        assertEquals(cal.getTime(), parser.parse("2009-10-16T16:42:16.000Z"));
    }

    @Test
    public void testEquals() {
        DateParser parser1= getInstance(YMD_SLASH);
        DateParser parser2= getInstance(YMD_SLASH);

        assertEquals(parser1, parser2);
        assertEquals(parser1.hashCode(), parser2.hashCode());

        assertFalse(parser1.equals(new Object()));
    }

    @Test
    public void testToStringContainsName() {
        DateParser parser= getInstance(YMD_SLASH);
        assertTrue(parser.toString().startsWith("FastDate"));
    }

    @Test
    public void testPatternMatches() {
        DateParser parser= getInstance(yMdHmsSZ);
        assertEquals(yMdHmsSZ, parser.getPattern());
    }

    @Test
    public void testLocaleMatches() {
        DateParser parser= getInstance(yMdHmsSZ, SWEDEN);
        assertEquals(SWEDEN, parser.getLocale());
    }

    @Test
    public void testTimeZoneMatches() {
        DateParser parser= getInstance(yMdHmsSZ, REYKJAVIK);
        assertEquals(REYKJAVIK, parser.getTimeZone());
    }
}
