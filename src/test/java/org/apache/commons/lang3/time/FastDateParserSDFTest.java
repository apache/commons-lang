package org.apache.commons.lang3.time;

import static org.junit.Assert.*;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

/**
 * Compare FastDateParser with SimpleDateFormat 
 */
@RunWith(Parameterized.class)
public class FastDateParserSDFTest {

    @Parameters(name= "{index}: {0} {1} {2}")
    public static Collection<Object[]> data() {
        return Arrays.asList(new Object [][]{
                // General Time zone tests
                {"z yyyy", "GMT 2010",       Locale.UK, true}, // no offset specified, but this is allowed as a TimeZone name
                {"z yyyy", "GMT-123 2010",   Locale.UK, false},
                {"z yyyy", "GMT-1234 2010",  Locale.UK, false},
                {"z yyyy", "GMT-12:34 2010", Locale.UK, true},
                {"z yyyy", "GMT-1:23 2010",  Locale.UK, true},
                // RFC 822 tests
                {"z yyyy", "-1234 2010",     Locale.UK, true},
                {"z yyyy", "-12:34 2010",    Locale.UK, false},
                {"z yyyy", "-123 2010",      Locale.UK, false},
                // year tests
                { "MM/dd/yyyy", "01/11/12",  Locale.UK, true},
                { "MM/dd/yy", "01/11/12",    Locale.UK, true},
                });
    }

    private final String format;
    private final String input;
    private final Locale locale;
    private final boolean valid;

    public FastDateParserSDFTest(String format, String input, Locale locale, boolean valid) {
        this.format = format;
        this.input = input;
        this.locale = locale;
        this.valid = valid;
    }

    @Test
    public void testOriginal() throws Exception {
        final SimpleDateFormat sdf = new SimpleDateFormat(format, locale);
        final TimeZone timeZone = TimeZone.getDefault();
        final DateParser fdf = new FastDateParser(format, timeZone, locale);
        checkParse(locale, sdf, fdf, input);
    }

    @Test
    public void testUpperCase() throws Exception {
        final SimpleDateFormat sdf = new SimpleDateFormat(format, locale);
        final TimeZone timeZone = TimeZone.getDefault();
        final DateParser fdf = new FastDateParser(format, timeZone , locale);
        checkParse(locale, sdf, fdf, input.toUpperCase(locale));
    }

    @Test
    @Ignore // not currently supported
    public void testLowerCase() throws Exception {
        final SimpleDateFormat sdf = new SimpleDateFormat(format, locale);
        final TimeZone timeZone = TimeZone.getDefault();
        final DateParser fdf = new FastDateParser(format, timeZone , locale);
        checkParse(locale, sdf, fdf, input.toLowerCase(locale));
    }

    private void checkParse(final Locale locale, final SimpleDateFormat sdf, final DateParser fdf, final String formattedDate) throws ParseException {
        Date expectedTime=null;
        Class<?> sdfE = null;
        try {
            expectedTime = sdf.parse(formattedDate);
            if (!valid) {
                // Error in test data
                throw new RuntimeException("Test data error: expected SDF parse to fail, but got " + expectedTime);
            }
        } catch (ParseException e) {
            if (valid) {
                // Error in test data
                throw new RuntimeException("Test data error: expected SDF parse to succeed, but got " + e);
            }
            sdfE = e.getClass();
        }
        Date actualTime = null;
        Class<?> fdfE = null;
        try {
            actualTime = fdf.parse(formattedDate);
            if (!valid) {
                // failure in test
                fail("Expected FDP parse to fail, but got " + actualTime);
            }
        } catch (ParseException e) {
            if (valid) {
                // failure in test
                fail("Expected FDP parse to succeed, but got " + e);
            }
            fdfE = e.getClass();
        }
        if (valid) {
            assertEquals(locale.toString()+" "+formattedDate +"\n",expectedTime, actualTime);            
        } else {
            assertEquals(locale.toString()+" "+formattedDate + " expected same Exception ", sdfE, fdfE);            
        }
    }
}
