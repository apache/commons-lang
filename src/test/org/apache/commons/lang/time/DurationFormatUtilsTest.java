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

    //-----------------------------------------------------------------------
    public void testConstructor() {
        assertNotNull(new DurationFormatUtils());
        Constructor[] cons = DurationFormatUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertEquals(true, Modifier.isPublic(cons[0].getModifiers()));
        assertEquals(true, Modifier.isPublic(DurationFormatUtils.class.getModifiers()));
        assertEquals(false, Modifier.isFinal(DurationFormatUtils.class.getModifiers()));
    }
    
    //-----------------------------------------------------------------------
    public void testFormatWords(){
        String text = null;
        
        text = DurationFormatUtils.formatWords(50*1000, true, false);
        assertEquals("50 seconds", text);
        text = DurationFormatUtils.formatWords(65*1000, true, false);
        assertEquals("1 minute 5 seconds", text);
        text = DurationFormatUtils.formatWords(120*1000, true, false);
        assertEquals("2 minutes 0 seconds", text);
        text = DurationFormatUtils.formatWords(121*1000, true, false);
        assertEquals("2 minutes 1 second", text);
        text = DurationFormatUtils.formatWords(72*60*1000, true, false);
        assertEquals("1 hour 12 minutes 0 seconds", text);
        text = DurationFormatUtils.formatWords(24*60*60*1000, true, false);
        assertEquals("1 day 0 hours 0 minutes 0 seconds", text);
        
        text = DurationFormatUtils.formatWords(50*1000, true, true);
        assertEquals("50 seconds", text);
        text = DurationFormatUtils.formatWords(65*1000, true, true);
        assertEquals("1 minute 5 seconds", text);
        text = DurationFormatUtils.formatWords(120*1000, true, true);
        assertEquals("2 minutes", text);
        text = DurationFormatUtils.formatWords(121*1000, true, true);
        assertEquals("2 minutes 1 second", text);
        text = DurationFormatUtils.formatWords(72*60*1000, true, true);
        assertEquals("1 hour 12 minutes", text);
        text = DurationFormatUtils.formatWords(24*60*60*1000, true, true);
        assertEquals("1 day", text);
        
        text = DurationFormatUtils.formatWords(50*1000, false, true);
        assertEquals("0 days 0 hours 0 minutes 50 seconds", text);
        text = DurationFormatUtils.formatWords(65*1000, false, true);
        assertEquals("0 days 0 hours 1 minute 5 seconds", text);
        text = DurationFormatUtils.formatWords(120*1000, false, true);
        assertEquals("0 days 0 hours 2 minutes", text);
        text = DurationFormatUtils.formatWords(121*1000, false, true);
        assertEquals("0 days 0 hours 2 minutes 1 second", text);
        text = DurationFormatUtils.formatWords(72*60*1000, false, true);
        assertEquals("0 days 1 hour 12 minutes", text);
        text = DurationFormatUtils.formatWords(24*60*60*1000, false, true);
        assertEquals("1 day", text);
        
        text = DurationFormatUtils.formatWords(50*1000, false, false);
        assertEquals("0 days 0 hours 0 minutes 50 seconds", text);
        text = DurationFormatUtils.formatWords(65*1000, false, false);
        assertEquals("0 days 0 hours 1 minute 5 seconds", text);
        text = DurationFormatUtils.formatWords(120*1000, false, false);
        assertEquals("0 days 0 hours 2 minutes 0 seconds", text);
        text = DurationFormatUtils.formatWords(121*1000, false, false);
        assertEquals("0 days 0 hours 2 minutes 1 second", text);
        text = DurationFormatUtils.formatWords(72*60*1000, false, false);
        assertEquals("0 days 1 hour 12 minutes 0 seconds", text);
        text = DurationFormatUtils.formatWords(48*60*60*1000 + 72*60*1000 , false, false);
        assertEquals("2 days 1 hour 12 minutes 0 seconds", text);
    }

    public void testFormatISOStyle(){
        long time = 0;
        assertEquals("0:00:00.000", DurationFormatUtils.formatISO(time));
        
        time = 1;
        assertEquals("0:00:00.001", DurationFormatUtils.formatISO(time));
        
        time = 15;
        assertEquals("0:00:00.015", DurationFormatUtils.formatISO(time));
        
        time = 165;
        assertEquals("0:00:00.165", DurationFormatUtils.formatISO(time));
        
        time = 1675;
        assertEquals("0:00:01.675", DurationFormatUtils.formatISO(time));
        
        time = 13465;
        assertEquals("0:00:13.465", DurationFormatUtils.formatISO(time));
        
        time = 72789;
        assertEquals("0:01:12.789", DurationFormatUtils.formatISO(time));
        
        time = 12789 + 32 * 60000;
        assertEquals("0:32:12.789", DurationFormatUtils.formatISO(time));
        
        time = 12789 + 62 * 60000;
        assertEquals("1:02:12.789", DurationFormatUtils.formatISO(time));
    }

    public void testISODurationFormat(){
        TimeZone timeZone = TimeZone.getTimeZone("GMT-3");
        Calendar cal = Calendar.getInstance(timeZone);
        cal.set(2002, 1, 23, 9, 11, 12);
        cal.set(Calendar.MILLISECOND, 1);
        String text;
        // repeat a test from testDateTimeISO to compare extended and not extended.
        text = DateFormatUtils.ISO_DATETIME_TIME_ZONE_FORMAT.format(cal);
        assertEquals("2002-02-23T09:11:12-03:00", text);
        // test fixture is the same as above, but now with extended format.
        text = DurationFormatUtils.format(cal.getTime().getTime(), DurationFormatUtils.ISO_EXTENDED_FORMAT_PATTERN, false, timeZone);
        assertEquals("P32Y1M22DT9H11M12.1S", text);
        // test fixture from example in http://www.w3.org/TR/xmlschema-2/#duration
        cal.set(1971, 1, 3, 10, 30, 0);
        cal.set(Calendar.MILLISECOND, 0);
        text = DurationFormatUtils.format(cal.getTime().getTime(), DurationFormatUtils.ISO_EXTENDED_FORMAT_PATTERN, false, timeZone);
        assertEquals("P1Y1M2DT10H30M0.0S", text);
        // want a way to say 'don't print the seconds in format()' or other fields for that matter:
        //assertEquals("P1Y2M3DT10H30M", text);
    }

    public void testFormat() {
        long time = 0;
        assertEquals( "0", DurationFormatUtils.format(time, "y") );
        assertEquals( "0", DurationFormatUtils.format(time, "M") );
        assertEquals( "0", DurationFormatUtils.format(time, "d") );
        assertEquals( "0", DurationFormatUtils.format(time, "H") );
        assertEquals( "0", DurationFormatUtils.format(time, "m") );
        assertEquals( "0", DurationFormatUtils.format(time, "s") );
        assertEquals( "0", DurationFormatUtils.format(time, "S") );
        assertEquals( "0000", DurationFormatUtils.format(time, "SSSS") );
        assertEquals( "0000", DurationFormatUtils.format(time, "yyyy") );
        assertEquals( "0000", DurationFormatUtils.format(time, "yyMM") );

        time = 60 * 1000;
        assertEquals( "0", DurationFormatUtils.format(time, "y") );
        assertEquals( "0", DurationFormatUtils.format(time, "M") );
        assertEquals( "0", DurationFormatUtils.format(time, "d") );
        assertEquals( "0", DurationFormatUtils.format(time, "H") );
        assertEquals( "1", DurationFormatUtils.format(time, "m") );
        assertEquals( "60", DurationFormatUtils.format(time, "s") );
        assertEquals( "60000", DurationFormatUtils.format(time, "S") );
        assertEquals( "01:00", DurationFormatUtils.format(time, "mm:ss") );

        Calendar cal = Calendar.getInstance();
        cal.set(1973, 6, 1, 0, 0, 0);
        cal.set(Calendar.MILLISECOND, 0);
        time = cal.getTime().getTime();
        assertEquals( "36", DurationFormatUtils.format(time, "yM") );
        assertEquals( "3 years 6 months", DurationFormatUtils.format(time, "y' years 'M' months'") );
        assertEquals( "03/06", DurationFormatUtils.format(time, "yy/MM") );

        cal.set(1973, 10, 1, 0, 0, 0);
        cal.set(Calendar.MILLISECOND, 0);
        time = cal.getTime().getTime();
        assertEquals( "310", DurationFormatUtils.format(time, "yM") );
        assertEquals( "3 years 10 months", DurationFormatUtils.format(time, "y' years 'M' months'") );
        assertEquals( "03/10", DurationFormatUtils.format(time, "yy/MM") );

        cal.set(1974, 0, 1, 0, 0, 0);
        cal.set(Calendar.MILLISECOND, 0);
        time = cal.getTime().getTime();
        assertEquals( "40", DurationFormatUtils.format(time, "yM") );
        assertEquals( "4 years 0 months", DurationFormatUtils.format(time, "y' years 'M' months'") );
        assertEquals( "04/00", DurationFormatUtils.format(time, "yy/MM") );
        assertEquals( "48", DurationFormatUtils.format(time, "M") );
        assertEquals( "48", DurationFormatUtils.format(time, "MM") );
        assertEquals( "048", DurationFormatUtils.format(time, "MMM") );
    }

    public void testLexx() {
        // tests each constant
        assertArrayEquals( 
          new Token[] { 
            new Token( DurationFormatUtils.y, 1),
            new Token( DurationFormatUtils.M, 1),
            new Token( DurationFormatUtils.d, 1),
            new Token( DurationFormatUtils.H, 1),
            new Token( DurationFormatUtils.m, 1),
            new Token( DurationFormatUtils.s, 1),
            new Token( DurationFormatUtils.S, 1)
          }, DurationFormatUtils.lexx("yMdHmsS") 
        );

        // tests the ISO8601-like
        assertArrayEquals( 
          new Token[] { 
            new Token( DurationFormatUtils.H, 1),
            new Token( new StringBuffer(":"), 1),
            new Token( DurationFormatUtils.m, 2),
            new Token( new StringBuffer(":"), 1),
            new Token( DurationFormatUtils.s, 2),
            new Token( new StringBuffer("."), 1),
            new Token( DurationFormatUtils.S, 3)
          }, DurationFormatUtils.lexx("H:mm:ss.SSS")
        );

        // test the iso extended format
        assertArrayEquals( 
          new Token[] { 
            new Token( new StringBuffer("P"), 1),
            new Token( DurationFormatUtils.y, 4),
            new Token( new StringBuffer("Y"), 1),
            new Token( DurationFormatUtils.M, 1),
            new Token( new StringBuffer("M"), 1),
            new Token( DurationFormatUtils.d, 1),
            new Token( new StringBuffer("DT"), 1),
            new Token( DurationFormatUtils.H, 1),
            new Token( new StringBuffer("H"), 1),
            new Token( DurationFormatUtils.m, 1),
            new Token( new StringBuffer("M"), 1),
            new Token( DurationFormatUtils.s, 1),
            new Token( new StringBuffer("."), 1),
            new Token( DurationFormatUtils.S, 1),
            new Token( new StringBuffer("S"), 1)
          }, 
          DurationFormatUtils.lexx(DurationFormatUtils.ISO_EXTENDED_FORMAT_PATTERN)
        );
    }
    private void assertArrayEquals(Token[] obj1, Token[] obj2) {
        assertEquals( "Arrays are unequal length. ", obj1.length, obj2.length );
        for(int i=0; i<obj1.length; i++) {
            assertTrue( "Index " + i + " not equal, " + obj1[i] + " vs " + obj2, obj1[i].equals(obj2[i]));
        }
    }

    
}
