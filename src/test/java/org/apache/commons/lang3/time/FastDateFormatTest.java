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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.fail;

import java.text.FieldPosition;
import java.text.Format;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLongArray;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;
import org.junitpioneer.jupiter.DefaultLocale;
import org.junitpioneer.jupiter.DefaultTimeZone;

/**
 * Unit tests {@link org.apache.commons.lang3.time.FastDateFormat}.
 *
 * @since 2.0
 */
public class FastDateFormatTest extends AbstractLangTest {
    private static final int NTHREADS = 10;

    private static final int NROUNDS = 10000;

    final Locale FINNISH = Locale.forLanguageTag("fi");
    final Locale HUNGARIAN = Locale.forLanguageTag("hu");

    private AtomicLongArray measureTime(final Format printer, final Format parser) throws InterruptedException {
        final ExecutorService pool = Executors.newFixedThreadPool(NTHREADS);
        final AtomicInteger failures = new AtomicInteger(0);
        final AtomicLongArray totalElapsed = new AtomicLongArray(2);
        try {
            for (int i = 0; i < NTHREADS; ++i) {
                pool.submit(() -> {
                    for (int j = 0; j < NROUNDS; ++j) {
                        try {
                            final Date date = new Date();

                            final long t0Millis = System.currentTimeMillis();
                            final String formattedDate = printer.format(date);
                            totalElapsed.addAndGet(0, System.currentTimeMillis() - t0Millis);

                            final long t1Millis = System.currentTimeMillis();
                            final Object pd = parser.parseObject(formattedDate);
                            totalElapsed.addAndGet(1, System.currentTimeMillis() - t1Millis);

                            if (!date.equals(pd)) {
                                failures.incrementAndGet();
                            }
                        } catch (final Exception e) {
                            failures.incrementAndGet();
                            e.printStackTrace();
                        }
                    }
                });
            }
        } finally {
            pool.shutdown();
            // depending on the performance of the machine used to run the parsing,
            // the tests can run for a while. It should however complete within
            // 30 seconds. Might need increase on very slow machines.
            if (!pool.awaitTermination(30, TimeUnit.SECONDS)) {
                pool.shutdownNow();
                fail("did not complete tasks");
            }
        }
        assertEquals(0, failures.get());
        return totalElapsed;
    }

    @DefaultLocale(language = "en", country = "US")
    @Test
    public void test_changeDefault_Locale_DateInstance() {
        final FastDateFormat format1 = FastDateFormat.getDateInstance(FastDateFormat.FULL, Locale.GERMANY);
        final FastDateFormat format2 = FastDateFormat.getDateInstance(FastDateFormat.FULL);
        Locale.setDefault(Locale.GERMANY);
        final FastDateFormat format3 = FastDateFormat.getDateInstance(FastDateFormat.FULL);

        assertSame(Locale.GERMANY, format1.getLocale());
        assertEquals(Locale.US, format2.getLocale());
        assertSame(Locale.GERMANY, format3.getLocale());
        assertNotSame(format1, format2);
        assertNotSame(format2, format3);
    }

    @DefaultLocale(language = "en", country = "US")
    @Test
    public void test_changeDefault_Locale_DateTimeInstance() {
        final FastDateFormat format1 = FastDateFormat.getDateTimeInstance(FastDateFormat.FULL, FastDateFormat.FULL, Locale.GERMANY);
        final FastDateFormat format2 = FastDateFormat.getDateTimeInstance(FastDateFormat.FULL, FastDateFormat.FULL);
        Locale.setDefault(Locale.GERMANY);
        final FastDateFormat format3 = FastDateFormat.getDateTimeInstance(FastDateFormat.FULL, FastDateFormat.FULL);

        assertSame(Locale.GERMANY, format1.getLocale());
        assertEquals(Locale.US, format2.getLocale());
        assertSame(Locale.GERMANY, format3.getLocale());
        assertNotSame(format1, format2);
        assertNotSame(format2, format3);
    }

    /*
     * Only the cache methods need to be tested here.
     * The print methods are tested by {@link FastDateFormat_PrinterTest}
     * and the parse methods are tested by {@link FastDateFormat_ParserTest}
     */
    @Test
    public void test_getInstance() {
        final FastDateFormat format1 = FastDateFormat.getInstance();
        final FastDateFormat format2 = FastDateFormat.getInstance();
        assertSame(format1, format2);
    }

    @Test
    public void test_getInstance_String() {
        final FastDateFormat format1 = FastDateFormat.getInstance("MM/DD/yyyy");
        final FastDateFormat format2 = FastDateFormat.getInstance("MM-DD-yyyy");
        final FastDateFormat format3 = FastDateFormat.getInstance("MM-DD-yyyy");

        assertNotSame(format1, format2);
        assertSame(format2, format3);
        assertEquals("MM/DD/yyyy", format1.getPattern());
        assertEquals(TimeZone.getDefault(), format1.getTimeZone());
        assertEquals(TimeZone.getDefault(), format2.getTimeZone());
    }

    @DefaultLocale(language = "en", country = "US")
    @Test
    public void test_getInstance_String_Locale() {
        final FastDateFormat format1 = FastDateFormat.getInstance("MM/DD/yyyy", Locale.GERMANY);
        final FastDateFormat format2 = FastDateFormat.getInstance("MM/DD/yyyy");
        final FastDateFormat format3 = FastDateFormat.getInstance("MM/DD/yyyy", Locale.GERMANY);

        assertNotSame(format1, format2);
        assertSame(format1, format3);
        assertEquals(Locale.GERMANY, format1.getLocale());
    }

    @DefaultLocale(language = "en", country = "US")
    @DefaultTimeZone("America/New_York")
    @Test
    public void test_getInstance_String_TimeZone() {

        final FastDateFormat format1 = FastDateFormat.getInstance("MM/DD/yyyy",
                TimeZone.getTimeZone("Atlantic/Reykjavik"));
        final FastDateFormat format2 = FastDateFormat.getInstance("MM/DD/yyyy");
        final FastDateFormat format3 = FastDateFormat.getInstance("MM/DD/yyyy", TimeZone.getDefault());
        final FastDateFormat format4 = FastDateFormat.getInstance("MM/DD/yyyy", TimeZone.getDefault());
        final FastDateFormat format5 = FastDateFormat.getInstance("MM-DD-yyyy", TimeZone.getDefault());
        final FastDateFormat format6 = FastDateFormat.getInstance("MM-DD-yyyy");

        assertNotSame(format1, format2);
        assertEquals(TimeZone.getTimeZone("Atlantic/Reykjavik"), format1.getTimeZone());
        assertEquals(TimeZone.getDefault(), format2.getTimeZone());
        assertSame(format3, format4);
        assertNotSame(format3, format5);
        assertNotSame(format4, format6);
    }

    @DefaultLocale(language = "en", country = "US")
    @DefaultTimeZone("America/New_York")
    @Test
    public void test_getInstance_String_TimeZone_Locale() {
        final FastDateFormat format1 = FastDateFormat.getInstance("MM/DD/yyyy",
                TimeZone.getTimeZone("Atlantic/Reykjavik"), Locale.GERMANY);
        final FastDateFormat format2 = FastDateFormat.getInstance("MM/DD/yyyy", Locale.GERMANY);
        final FastDateFormat format3 = FastDateFormat.getInstance("MM/DD/yyyy",
                TimeZone.getDefault(), Locale.GERMANY);

        assertNotSame(format1, format2);
        assertEquals(TimeZone.getTimeZone("Atlantic/Reykjavik"), format1.getTimeZone());
        assertEquals(TimeZone.getDefault(), format2.getTimeZone());
        assertEquals(TimeZone.getDefault(), format3.getTimeZone());
        assertEquals(Locale.GERMANY, format1.getLocale());
        assertEquals(Locale.GERMANY, format2.getLocale());
        assertEquals(Locale.GERMANY, format3.getLocale());
    }

    @Test
    public void testCheckDefaults() {
        final FastDateFormat format = FastDateFormat.getInstance();
        final FastDateFormat medium = FastDateFormat.getDateTimeInstance(FastDateFormat.SHORT, FastDateFormat.SHORT);
        assertEquals(medium, format);

        final SimpleDateFormat sdf = new SimpleDateFormat();
        assertEquals(sdf.toPattern(), format.getPattern());

        assertEquals(Locale.getDefault(), format.getLocale());
        assertEquals(TimeZone.getDefault(), format.getTimeZone());
    }

    @Test
    public void testCheckDifferingStyles() {
        final FastDateFormat shortShort = FastDateFormat.getDateTimeInstance(FastDateFormat.SHORT, FastDateFormat.SHORT, Locale.US);
        final FastDateFormat shortLong = FastDateFormat.getDateTimeInstance(FastDateFormat.SHORT, FastDateFormat.LONG, Locale.US);
        final FastDateFormat longShort = FastDateFormat.getDateTimeInstance(FastDateFormat.LONG, FastDateFormat.SHORT, Locale.US);
        final FastDateFormat longLong = FastDateFormat.getDateTimeInstance(FastDateFormat.LONG, FastDateFormat.LONG, Locale.US);

        assertNotEquals(shortShort, shortLong);
        assertNotEquals(shortShort, longShort);
        assertNotEquals(shortShort, longLong);
        assertNotEquals(shortLong, longShort);
        assertNotEquals(shortLong, longLong);
        assertNotEquals(longShort, longLong);
    }

    @Test
    public void testDateDefaults() {
        assertEquals(FastDateFormat.getDateInstance(FastDateFormat.LONG, Locale.CANADA),
                FastDateFormat.getDateInstance(FastDateFormat.LONG, TimeZone.getDefault(), Locale.CANADA));

        assertEquals(FastDateFormat.getDateInstance(FastDateFormat.LONG, TimeZone.getTimeZone("America/New_York")),
                FastDateFormat.getDateInstance(FastDateFormat.LONG, TimeZone.getTimeZone("America/New_York"), Locale.getDefault()));

        assertEquals(FastDateFormat.getDateInstance(FastDateFormat.LONG),
                FastDateFormat.getDateInstance(FastDateFormat.LONG, TimeZone.getDefault(), Locale.getDefault()));
    }

    @Test
    public void testLANG_1152() {
        final TimeZone utc = FastTimeZone.getGmtTimeZone();
        final Date date = new Date(Long.MAX_VALUE);

        String dateAsString = FastDateFormat.getInstance("yyyy-MM-dd", utc, Locale.US).format(date);
        assertEquals("292278994-08-17", dateAsString);

        dateAsString = FastDateFormat.getInstance("dd/MM/yyyy", utc, Locale.US).format(date);
        assertEquals("17/08/292278994", dateAsString);
    }
    @Test
    public void testLANG_1267() {
        FastDateFormat.getInstance("yyyy-MM-dd'T'HH:mm:ss.SSSXXX");
    }

    /**
     * According to LANG-954 (https://issues.apache.org/jira/browse/LANG-954) this is broken in Android 2.1.
     */
    @Test
    public void testLANG_954() {
        final String pattern = "yyyy-MM-dd'T'";
        FastDateFormat.getInstance(pattern);
    }

    @Test
    public void testParseSync() throws InterruptedException {
        final String pattern = "yyyy-MM-dd'T'HH:mm:ss.SSS";
        final SimpleDateFormat inner = new SimpleDateFormat(pattern);
        final Format sdf= new Format() {
            private static final long serialVersionUID = 1L;

            @Override
            public StringBuffer format(final Object obj,
                    final StringBuffer toAppendTo,
                    final FieldPosition fieldPosition) {
                synchronized(this) {
                    return inner.format(obj, toAppendTo, fieldPosition);
                }
            }

            @Override
            public Object parseObject(final String source, final ParsePosition pos) {
                synchronized(this) {
                    return inner.parseObject(source, pos);
                }
            }
        };
        final AtomicLongArray sdfTime= measureTime(sdf, sdf);

        final Format fdf = FastDateFormat.getInstance(pattern);
        final AtomicLongArray fdfTime= measureTime(fdf, fdf);

        //System.out.println(">>FastDateFormatTest: FastDatePrinter:"+fdfTime.get(0)+"  SimpleDateFormat:"+sdfTime.get(0));
        //System.out.println(">>FastDateFormatTest: FastDateParser:"+fdfTime.get(1)+"  SimpleDateFormat:"+sdfTime.get(1));
    }

    @Test
    public void testTimeDateDefaults() {
        assertEquals(FastDateFormat.getDateTimeInstance(FastDateFormat.LONG, FastDateFormat.MEDIUM, Locale.CANADA),
                FastDateFormat.getDateTimeInstance(FastDateFormat.LONG, FastDateFormat.MEDIUM, TimeZone.getDefault(), Locale.CANADA));

        assertEquals(FastDateFormat.getDateTimeInstance(FastDateFormat.LONG, FastDateFormat.MEDIUM, TimeZone.getTimeZone("America/New_York")),
                FastDateFormat.getDateTimeInstance(FastDateFormat.LONG, FastDateFormat.MEDIUM, TimeZone.getTimeZone("America/New_York"), Locale.getDefault()));

        assertEquals(FastDateFormat.getDateTimeInstance(FastDateFormat.LONG, FastDateFormat.MEDIUM),
                FastDateFormat.getDateTimeInstance(FastDateFormat.LONG, FastDateFormat.MEDIUM, TimeZone.getDefault(), Locale.getDefault()));
    }

    @Test
    public void testTimeDefaults() {
        assertEquals(FastDateFormat.getTimeInstance(FastDateFormat.LONG, Locale.CANADA),
                FastDateFormat.getTimeInstance(FastDateFormat.LONG, TimeZone.getDefault(), Locale.CANADA));

        assertEquals(FastDateFormat.getTimeInstance(FastDateFormat.LONG, TimeZone.getTimeZone("America/New_York")),
                FastDateFormat.getTimeInstance(FastDateFormat.LONG, TimeZone.getTimeZone("America/New_York"), Locale.getDefault()));

        assertEquals(FastDateFormat.getTimeInstance(FastDateFormat.LONG),
                FastDateFormat.getTimeInstance(FastDateFormat.LONG, TimeZone.getDefault(), Locale.getDefault()));
    }

    @Test
    public void testStandaloneShortMonthForm() {
        final TimeZone utc = FastTimeZone.getGmtTimeZone();
        final Instant testInstant = LocalDate.of(1970, 9, 15).atStartOfDay(ZoneId.of("UTC")).toInstant();
        final Date date = Date.from(testInstant);

        String dateAsString = FastDateFormat.getInstance("yyyy-LLL-dd", utc, Locale.GERMAN).format(date);
        assertEquals("1970-Sep-15", dateAsString);

        dateAsString = FastDateFormat.getInstance("yyyy-LLL-dd", utc, FINNISH).format(date);
        assertEquals("1970-syys-15", dateAsString);

        dateAsString = FastDateFormat.getInstance("yyyy-LLL-dd", utc, HUNGARIAN).format(date);
        assertEquals("1970-szept.-15", dateAsString);
    }

    @Test
    public void testStandaloneLongMonthForm() {
        final TimeZone utc = FastTimeZone.getGmtTimeZone();
        final Instant testInstant = LocalDate.of(1970, 9, 15).atStartOfDay(ZoneId.of("UTC")).toInstant();
        final Date date = Date.from(testInstant);

        String dateAsString = FastDateFormat.getInstance("yyyy-LLLL-dd", utc, Locale.GERMAN).format(date);
        assertEquals("1970-September-15", dateAsString);

        dateAsString = FastDateFormat.getInstance("yyyy-LLLL-dd", utc, FINNISH).format(date);
        assertEquals("1970-syyskuu-15", dateAsString);

        dateAsString = FastDateFormat.getInstance("yyyy-LLLL-dd", utc, HUNGARIAN).format(date);
        assertEquals("1970-szeptember-15", dateAsString);
    }
}
