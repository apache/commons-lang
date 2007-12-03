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
package org.apache.commons.lang.text;

import java.text.ChoiceFormat;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.FieldPosition;
import java.text.Format;
import java.text.MessageFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.Locale;

import org.apache.commons.lang.SystemUtils;

import junit.framework.TestCase;

/**
 * Abstract testcase to verify behavior of default-configuration
 * ExtendedMessageFormat vs. MessageFormat.
 * 
 * @author Matt Benson
 * @since 2.4
 * @version $Id$
 */
public abstract class AbstractMessageFormatTest extends TestCase {
    protected static final Double[] NUMBERS = { new Double(0.1),
            new Double(1.1), new Double(2.1) };

    protected static final Object[] DATES = {
            new GregorianCalendar(1970, Calendar.JANUARY, 01, 0, 15, 20)
                    .getTime(),
            new GregorianCalendar(1970, Calendar.FEBRUARY, 02, 12, 30, 35)
                    .getTime(),
            new GregorianCalendar(1970, Calendar.MARCH, 03, 18, 45, 50)
                    .getTime() };

    protected Locale locale;

    /**
     * {@inheritDoc}
     */
    protected void setUp() throws Exception {
        super.setUp();
        this.locale = getLocale();
    }

    /**
     * Create a MessageFormat.
     * @param pattern
     * @param locale
     * @return
     */
    protected abstract MessageFormat createMessageFormat(String pattern,
            Locale locale);

    /**
     * Get the Locale to use.
     * @return
     */
    protected abstract Locale getLocale();

    protected void doAssertions(String expected, String pattern, Object[] args) {
        doAssertions(expected, pattern, args, pattern);
    }

    protected void doAssertions(String expected, String pattern, Object[] args,
            String toPattern) {
        MessageFormat f = createMessageFormat(pattern, locale);
        assertEquals(expected, f.format(args));
        if (SystemUtils.isJavaVersionAtLeast(140)) {
            assertEquals(toPattern, f.toPattern());
        }
    }

    protected void doAssertions(Format format, Object[] args) {
        doAssertions(format, args, null);
    }

    protected void doAssertions(Format format, Object[] args, String formatName) {
        doAssertions(format, args, formatName, null);
    }

    protected void doAssertions(Format format, Object[] args,
            String formatName, String decodeFormatName) {
        StringBuffer pattern = new StringBuffer();
        StringBuffer expected = new StringBuffer();
        StringBuffer decodePattern = new StringBuffer();
        for (int i = 0; i < args.length; i++) {
            pattern.append(i).append(": {").append(i);
            if (formatName != null) {
                pattern.append(',').append(formatName);
            }
            pattern.append("}; ");
            expected.append(i).append(": ");
            if (format != null) {
                format.format(args[i], expected, new FieldPosition(0));
            } else {
                expected.append(String.valueOf(args[i]));
            }
            expected.append("; ");
            decodePattern.append(i).append(": {").append(i);
            if (decodeFormatName != null || formatName != null) {
                decodePattern.append(',').append(
                        decodeFormatName == null ? formatName
                                : decodeFormatName);
            }
            decodePattern.append("}; ");
        }
        doAssertions(expected.toString(), pattern.toString(), args,
                decodePattern.toString());
    }

    public void testNoFormatElements() {
        StringBuffer pattern = new StringBuffer();
        for (int i = 0; i < NUMBERS.length; i++) {
            if (i > 0) {
                pattern.append("; ");
            }
            pattern.append(i).append(": ").append(NUMBERS[i]);
        }
        String p = pattern.toString();
        doAssertions(p, p, null);
    }

    public void testSimpleStrings() {
        doAssertions(null, new Object[] { "foo", "bar", "baz"}, null);
    }

    public void testSimpleNumbers() {
        doAssertions(NumberFormat.getInstance(locale), NUMBERS, null);
    }

    public void testSimpleDates() {
        doAssertions(DateFormat.getDateTimeInstance(DateFormat.SHORT,
                DateFormat.SHORT, locale), DATES, null);
    }

    public void testNumber() {
        doAssertions(NumberFormat.getInstance(locale), NUMBERS, "number");
    }

    public void testNumberLooseFormatting() {
        doAssertions(NumberFormat.getInstance(locale), NUMBERS, " number ",
                "number");
    }

    public void testInteger() {
        doAssertions(getIntegerNumberFormat(locale), NUMBERS,
                "number,integer");
    }

    public void testIntegerLooseFormatting() {
        doAssertions(getIntegerNumberFormat(locale), NUMBERS,
                " number , integer ", "number,integer");
    }

    public void testCurrency() {
        doAssertions(NumberFormat.getCurrencyInstance(locale), NUMBERS,
                "number,currency");
    }

    public void testPercent() {
        doAssertions(NumberFormat.getPercentInstance(locale), NUMBERS,
                "number,percent");
    }

    public void testNumberPattern() {
        doAssertions(new DecimalFormat("#000.000", new DecimalFormatSymbols(
                locale)), NUMBERS, "number,#000.000");
    }

    public void testDate() {
        doAssertions(DateFormat.getDateInstance(DateFormat.DEFAULT, locale),
                DATES, "date");
    }

    public void testDateLooseFormatting() {
        doAssertions(DateFormat.getDateInstance(DateFormat.DEFAULT, locale),
                DATES, " date ", "date");
    }

    public void testShortDate() {
        DateFormat shortDf = DateFormat.getDateInstance(DateFormat.SHORT, locale);
        DateFormat defaultDf = DateFormat.getDateInstance(DateFormat.DEFAULT, locale);
        doAssertions(shortDf, DATES, "date,short",
                shortDf.equals(defaultDf) ? "date" : "date,short");
    }

    public void testShortDateLooseFormatting() {
        DateFormat shortDf = DateFormat.getDateInstance(DateFormat.SHORT, locale);
        DateFormat defaultDf = DateFormat.getDateInstance(DateFormat.DEFAULT, locale);
        doAssertions(shortDf, DATES, " date , short ",
                shortDf.equals(defaultDf) ? "date" : "date,short");
    }

    public void testMediumDate() {
        doAssertions(DateFormat.getDateInstance(DateFormat.MEDIUM, locale),
                DATES, "date,medium", "date");
    }

    public void testLongDate() {
        DateFormat longDf = DateFormat.getDateInstance(DateFormat.LONG, locale);
        DateFormat defaultDf = DateFormat.getDateInstance(DateFormat.DEFAULT,
                locale);
        doAssertions(longDf, DATES, "date,long",
                longDf.equals(defaultDf) ? "date" : "date,long");
    }

    public void testFullDate() {
        DateFormat fullDf = DateFormat.getDateInstance(DateFormat.FULL, locale);
        DateFormat longDf = DateFormat.getDateInstance(DateFormat.LONG, locale);
        doAssertions(fullDf, DATES, "date,full",
                fullDf.equals(longDf) ? "date,long" : "date,full");
    }

    public void testDatePattern() {
        doAssertions(new SimpleDateFormat("Gyyyy.D", locale), DATES,
                "date,Gyyyy.D");
    }

    public void testTime() {
        doAssertions(DateFormat.getTimeInstance(DateFormat.DEFAULT, locale),
                DATES, "time");
    }

    public void testShortTime() {
        doAssertions(DateFormat.getTimeInstance(DateFormat.SHORT, locale),
                DATES, "time,short");
    }

    public void testMediumTime() {
        doAssertions(DateFormat.getTimeInstance(DateFormat.MEDIUM, locale),
                DATES, "time,medium", "time");
    }

    public void testLongTime() {
        doAssertions(DateFormat.getTimeInstance(DateFormat.LONG, locale),
                DATES, "time,long");
    }

    public void testFullTime() {
        DateFormat fullDf = DateFormat.getTimeInstance(DateFormat.FULL, locale);
        DateFormat longDf = DateFormat.getTimeInstance(DateFormat.LONG, locale);
        doAssertions(fullDf, DATES, "time,full",
                fullDf.equals(longDf) ? "time,long" : "time,full");
    }

    public void testTimePattern() {
        doAssertions(new SimpleDateFormat("aHms", locale), DATES, "date,aHms");
    }

    public void testChoice() {
        doAssertions(new ChoiceFormat("0.0#x|1.0#y|2.0#z"), NUMBERS,
                "choice,0.0#x|1.0#y|2.0#z");
    }

    public void testChoiceLooseFormatting() {
        doAssertions(new ChoiceFormat("0.0#x |1.0#y |2.0#z "), NUMBERS,
                "choice,0.0#x |1.0#y |2.0#z ");
    }

    public void testChoiceRecursive() {
        NumberFormat nf = NumberFormat.getInstance(locale);
        StringBuffer choice = new StringBuffer();
        StringBuffer format = new StringBuffer("choice,");
        for (int i = 0; i < NUMBERS.length; i++) {
            Double d = new Double(Math.floor(NUMBERS[i].doubleValue()));
            if (i > 0) {
                choice.append('|');
                format.append('|');
            }
            choice.append(d).append('#').append(
                    nf.format(NUMBERS[i].doubleValue()));
            format.append(d).append('#').append('{').append(i).append('}');
        }
        doAssertions(new ChoiceFormat(choice.toString()), NUMBERS, format
                .toString());
    }

    private NumberFormat getIntegerNumberFormat(Locale locale) {
        NumberFormat result = NumberFormat.getInstance(locale);
        result.setMaximumFractionDigits(0);
        result.setParseIntegerOnly(true);
        return result;
    }

}
