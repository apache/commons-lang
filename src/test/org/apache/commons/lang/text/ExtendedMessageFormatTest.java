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

import java.text.DateFormat;
import java.text.FieldPosition;
import java.text.Format;
import java.text.MessageFormat;
import java.text.NumberFormat;
import java.text.ParsePosition;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import org.apache.commons.lang.SystemUtils;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * Test case for {@link ExtendedMessageFormat}.
 *
 * @since 2.4
 * @version $Id$
 */
public class ExtendedMessageFormatTest extends TestCase {

    private Locale[] testLocales = new Locale[] {null, //default locale
                                                 Locale.US,
                                                 Locale.UK,
                                                 Locale.FRANCE,
                                                 Locale.GERMANY};

    private Map registry = new HashMap();

    /**
     * Return a new test suite containing this test case.
     * 
     * @return a new test suite containing this test case
     */
    public static Test suite() {
        TestSuite suite = new TestSuite(ExtendedMessageFormatTest.class);
        suite.setName("ExtendedMessageFormat Tests");
        return suite;
    }

    /**
     * Create a new test case.
     *
     * @param name The name of the test
     */
    public ExtendedMessageFormatTest(String name) {
        super(name);
    }

    protected void setUp() throws Exception {
        super.setUp();
        registry.put("lower", new LowerCaseFormatFactory());
        registry.put("upper", new UpperCaseFormatFactory());
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    /**
     * Test extended formats.
     */
    public void testExtendedFormats() {
        String pattern = "Lower: {0,lower} Upper: {1,upper}";
        ExtendedMessageFormat emf = new ExtendedMessageFormat(pattern, registry);
        assertPatternsEqual("TOPATTERN", pattern, emf.toPattern());
        assertEquals("Lower: foo Upper: BAR", emf.format(new Object[] {"foo", "bar"}));
        assertEquals("Lower: foo Upper: BAR", emf.format(new Object[] {"Foo", "Bar"}));
        assertEquals("Lower: foo Upper: BAR", emf.format(new Object[] {"FOO", "BAR"}));
        assertEquals("Lower: foo Upper: BAR", emf.format(new Object[] {"FOO", "bar"}));
        assertEquals("Lower: foo Upper: BAR", emf.format(new Object[] {"foo", "BAR"}));
    }

    /**
     * Test extended and built in formats.
     */
    public void testExtendedAndBuiltInFormats() {
        Calendar cal = Calendar.getInstance();
        cal.set(2007, Calendar.JANUARY, 23, 18, 33, 05);
        Object[] args = new Object[] {"John Doe", cal.getTime(), new Double("12345.67")};
        String pattern = "Name: {0,upper} DOB: {1,date,short} Salary: {2,number,currency}";

        for (int i = 0; i < testLocales.length; i++) {
            DateFormat df = null;
            NumberFormat nf = null;
            ExtendedMessageFormat emf = null;
            if (testLocales[i] == null) {
                df = DateFormat.getDateInstance(DateFormat.SHORT);
                nf = NumberFormat.getCurrencyInstance();
                emf = new ExtendedMessageFormat(pattern, registry);
            } else {
                df = DateFormat.getDateInstance(DateFormat.SHORT, testLocales[i]);
                nf = NumberFormat.getCurrencyInstance(testLocales[i]);
                emf = new ExtendedMessageFormat(pattern, testLocales[i], registry);
            }
            StringBuffer expected = new StringBuffer();
            expected.append("Name: ");
            expected.append(args[0].toString().toUpperCase());
            expected.append(" DOB: ");
            expected.append(df.format(args[1]));
            expected.append(" Salary: ");
            expected.append(nf.format(args[2]));
            assertPatternsEqual(null, pattern, emf.toPattern());
            assertEquals("" + testLocales[i], expected.toString(), emf.format(args));
        }
    }

//    /**
//     * Test extended formats with choice format.
//     *
//     * N.B. FAILING - currently sub-formats not supported
//     */
//    public void testExtendedWithChoiceFormat() {
//        String pattern = "Choice: {0,choice,1.0#{1,lower}|2.0#{1,upper}}";
//        ExtendedMessageFormat emf = new ExtendedMessageFormat(pattern, registry);
//        assertPatterns(null, pattern, emf.toPattern());
//        try {
//            assertEquals("one", emf.format(new Object[] {new Integer(1), "ONE"}));
//            assertEquals("TWO", emf.format(new Object[] {new Integer(2), "two"}));
//        } catch (IllegalArgumentException e) {
//            // currently sub-formats not supported
//        }
//    }

//    /**
//     * Test mixed extended and built-in formats with choice format.
//     *
//     * N.B. FAILING - currently sub-formats not supported
//     */
//    public void testExtendedAndBuiltInWithChoiceFormat() {
//        String pattern = "Choice: {0,choice,1.0#{0} {1,lower} {2,number}|2.0#{0} {1,upper} {2,number,currency}}";
//        Object[] lowArgs  = new Object[] {new Integer(1), "Low",  new Double("1234.56")};
//        Object[] highArgs = new Object[] {new Integer(2), "High", new Double("9876.54")};

//        for (int i = 0; i < testLocales.length; i++) {
//            NumberFormat nf = null;
//            NumberFormat cf = null;
//            ExtendedMessageFormat emf = null;
//            if (testLocales[i] == null) {
//                nf = NumberFormat.getNumberInstance();
//                cf = NumberFormat.getCurrencyInstance();
//                emf = new ExtendedMessageFormat(pattern, registry);
//            } else {
//                nf = NumberFormat.getNumberInstance(testLocales[i]);
//                cf = NumberFormat.getCurrencyInstance(testLocales[i]);
//                emf = new ExtendedMessageFormat(pattern, testLocales[i], registry);
//            }
//            assertPatterns(null, pattern, emf.toPattern());
//            try {
//                String lowExpected = lowArgs[0] + " low "    + nf.format(lowArgs[2]);
//                String highExpected = highArgs[0] + " HIGH "  + cf.format(highArgs[2]);
//                assertEquals(lowExpected,  emf.format(lowArgs));
//                assertEquals(highExpected, emf.format(highArgs));
//            } catch (IllegalArgumentException e) {
//                // currently sub-formats not supported
//            }
//        }
//    }

    /**
     * Test the built in choice format.
     */
    public void testBuiltInChoiceFormat() {
        Object[] values = new Number[] {new Integer(1), new Double("2.2"), new Double("1234.5")};
        String choicePattern = null; 

        choicePattern = "{0,choice,1#One|2#Two|3#Many {0,number}}";
        for (int i = 0; i < values.length; i++) {
            checkBuiltInFormat(values[i] + ": " + choicePattern, new Object[] {values[i]});
        }

        choicePattern = "{0,choice,1#''One''|2#\"Two\"|3#''{Many}'' {0,number}}";
        for (int i = 0; i < values.length; i++) {
            checkBuiltInFormat(values[i] + ": " + choicePattern, new Object[] {values[i]});
        }
    }

    /**
     * Test the built in date/time formats
     */
    public void testBuiltInDateTimeFormat() {
        Calendar cal = Calendar.getInstance();
        cal.set(2007, Calendar.JANUARY, 23, 18, 33, 05);
        Object[] args = new Object[] {cal.getTime()};

        checkBuiltInFormat("1: {0,date,short}",    args);
        checkBuiltInFormat("2: {0,date,medium}",   args);
        checkBuiltInFormat("3: {0,date,long}",     args);
        checkBuiltInFormat("4: {0,date,full}",     args);
        checkBuiltInFormat("5: {0,date,d MMM yy}", args);
        checkBuiltInFormat("6: {0,time,short}",    args);
        checkBuiltInFormat("7: {0,time,medium}",   args);
        checkBuiltInFormat("8: {0,time,long}",     args);
        checkBuiltInFormat("9: {0,time,full}",     args);
        checkBuiltInFormat("10: {0,time,HH:mm}",   args);
    }

    /**
     * Test the built in number formats.
     */
    public void testBuiltInNumberFormat() {
        Object[] args = new Object[] {new Double("6543.21")};
        checkBuiltInFormat("1: {0,number}",            args);
        checkBuiltInFormat("2: {0,number,integer}",    args);
        checkBuiltInFormat("3: {0,number,currency}",   args);
        checkBuiltInFormat("4: {0,number,percent}",    args);
        checkBuiltInFormat("5: {0,number,00000.000}",  args);
    }

    /**
     * Create ExtendedMessageFormats for the specified pattern and the set of locales
     * and check the formated output matches the expected result for the parameters.
     */
    private void checkBuiltInFormat(String pattern, Object[] args) {
        for (int i = 0; i < testLocales.length; i++) {
            checkBuiltInFormat(pattern, args, testLocales[i]);
        }
    }

    /**
     * Create an ExtendedMessageFormat for the specified pattern and locale and check the
     * formated output matches the expected result for the parameters.
     */
    private void checkBuiltInFormat(String pattern, Object[] args, Locale locale) {
        StringBuffer buffer = new StringBuffer();
        buffer.append("Pattern=[");
        buffer.append(pattern);
        buffer.append("], locale=[");
        buffer.append(locale);
        buffer.append("]");
        MessageFormat mf = new MessageFormat(pattern);
        if (locale != null) {
            mf.setLocale(locale);
            mf.applyPattern(pattern);
        }
        // System.out.println(buffer + ", result=[" + mf.format(args) +"]");
        ExtendedMessageFormat emf = null;
        if (locale == null) {
            emf = new ExtendedMessageFormat(pattern);
        } else {
            emf = new ExtendedMessageFormat(pattern, locale);
        }
        assertEquals("format "    + buffer.toString(), mf.format(args), emf.format(args));
        assertPatternsEqual("toPattern " + buffer.toString(), mf.toPattern(),  emf.toPattern());
    }

    //can't trust what MessageFormat does with toPattern() pre 1.4:
    private void assertPatternsEqual(String message, String expected, String actual) {
        if (SystemUtils.isJavaVersionAtLeast(1.4f)) {
            assertEquals(message, expected, actual);
        }
    }

    // ------------------------ Test Formats ------------------------

    /**
     * {@link Format} implementation which converts to upper case.
     */
    private static class LowerCaseFormat extends Format {
        public StringBuffer format(Object obj, StringBuffer toAppendTo, FieldPosition pos) {
            return toAppendTo.append(((String)obj).toLowerCase());
        }
        public Object parseObject(String source, ParsePosition pos) {throw new UnsupportedOperationException();}
    }

    /**
     * {@link Format} implementation which converts to lower case.
     */
    private static class UpperCaseFormat extends Format {
        public StringBuffer format(Object obj, StringBuffer toAppendTo, FieldPosition pos) {
            return toAppendTo.append(((String)obj).toUpperCase());
        }
        public Object parseObject(String source, ParsePosition pos) {throw new UnsupportedOperationException();}
    }


    // ------------------------ Test Format Factories ---------------
    /**
     * {@link FormatFactory} implementation for lower case format.
     */
    private static class LowerCaseFormatFactory implements FormatFactory {
        private static final Format LOWER_INSTANCE = new LowerCaseFormat();
        public Format getFormat(String name, String arguments, Locale locale) {
            return LOWER_INSTANCE;
        }
    }
    /**
     * {@link FormatFactory} implementation for upper case format.
     */
    private static class UpperCaseFormatFactory implements FormatFactory {
        private static final Format UPPER_INSTANCE = new UpperCaseFormat();
        public Format getFormat(String name, String arguments, Locale locale) {
            return UPPER_INSTANCE;
        }
    }
}
