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

import org.apache.commons.lang3.SerializationUtils;
import org.junit.Test;

/**
 * Unit tests {@link org.apache.commons.lang3.time.FastDateParser}.
 *
 * @since 3.2
 */
public class FastDateParserTest {
    private static final String yMdHmsSZ = "yyyy-MM-dd'T'HH:mm:ss.SSS Z";
    private static final String DMY_DOT = "dd.MM.yyyy";
    private static final String YMD_SLASH = "yyyy/MM/dd";
    private static final String MDY_DASH = "MM-DD-yyyy";
    private static final String MDY_SLASH = "MM/DD/yyyy";
    private static final TimeZone REYKJAVIK = TimeZone.getTimeZone("Atlantic/Reykjavik");
    private static final TimeZone NEW_YORK = TimeZone.getTimeZone("America/New_York");
    private static final Locale SWEDEN = new Locale("sv", "SE");

    DateParser getInstance(String format) {
        return getInstance(format, TimeZone.getDefault(), Locale.getDefault());
    }

    private DateParser getDateInstance(int dateStyle, Locale locale) {
        return getInstance(FormatCache.getPatternForStyle(dateStyle, null, locale), TimeZone.getDefault(), Locale.getDefault());
    }

    private DateParser getInstance(String format, Locale locale) {
        return getInstance(format, TimeZone.getDefault(), locale);
    }

    private DateParser getInstance(String format, TimeZone timeZone) {
        return getInstance(format, timeZone, Locale.getDefault());
    }

    /**
     * Override this method in derived tests to change the construction of instances
     * @param format
     * @param timeZone
     * @param locale
     * @return
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
            map.put(parser, i++);            
        }

        i= 0;
        for(DateParser parser:parsers) {
            assertEquals(i++, (int)map.get(parser));
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
    public void testLocales() throws ParseException {
                
        for(Locale locale : Locale.getAvailableLocales()) {
            Calendar cal= Calendar.getInstance(NEW_YORK, Locale.US);
            cal.clear();
            cal.set(2003, 1, 10);

            try {
                String longFormat= "GGGG/yyyy/MMMM/dddd/aaaa/EEEE/ZZZZ";
                SimpleDateFormat sdf = new SimpleDateFormat(longFormat, locale);
                DateParser fdf = getInstance(longFormat, locale);
                
                                checkParse(cal, sdf, fdf);
                
                cal.set(Calendar.ERA, GregorianCalendar.BC);
                                checkParse(cal, sdf, fdf);
                        
                String shortFormat= "G/y/M/d/a/E/Z";
                sdf = new SimpleDateFormat(shortFormat, locale);
                fdf = getInstance(shortFormat, locale);
                                checkParse(cal, sdf, fdf);
                
                cal.set(Calendar.ERA, GregorianCalendar.AD);
                                checkParse(cal, sdf, fdf);
            }
            catch(ParseException ex) {
                // TODO: why do ja_JP_JP, hi_IN, th_TH, and th_TH_TH fail?
                System.out.println("Locale "+locale+ " failed");
            }
        }
    }

    private void checkParse(Calendar cal, SimpleDateFormat sdf, DateParser fdf) throws ParseException {
        String formattedDate= sdf.format(cal.getTime());                
        Date expectedTime = sdf.parse(formattedDate);
        Date actualTime = fdf.parse(formattedDate);
        assertEquals(expectedTime, actualTime);
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
    public void testDayOf() throws ParseException {
        Calendar cal= Calendar.getInstance(NEW_YORK, Locale.US);
        cal.clear();
        cal.set(2003, 1, 10);
        
        DateParser fdf = getInstance("W w F D y", NEW_YORK, Locale.US);
        assertEquals(cal.getTime(), fdf.parse("3 7 2 41 03"));
    }
    
    /**
     * Test case for {@link FastDateParser#getDateInstance(int, java.util.Locale)}.
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
        cal.set(2004,11,31);

        Date date = parser.parse("2004/11/31");

        parser = (DateParser) SerializationUtils.deserialize( SerializationUtils.serialize( (Serializable)parser ) );
        assertEquals(date, parser.parse("2004/11/31"));
    }

    @Test
    public void testLang538() throws ParseException {
        DateParser parser = getInstance("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'", TimeZone.getTimeZone("GMT"));
        
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
