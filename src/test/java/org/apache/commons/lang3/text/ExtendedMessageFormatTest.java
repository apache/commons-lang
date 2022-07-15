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
package org.apache.commons.lang3.text;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

import java.text.DateFormat;
import java.text.FieldPosition;
import java.text.Format;
import java.text.MessageFormat;
import java.text.NumberFormat;
import java.text.ParsePosition;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Locale;
import java.util.Map;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link ExtendedMessageFormat}.
 *
 * @since 2.4
 */
@Deprecated
public class ExtendedMessageFormatTest extends AbstractLangTest {

    private final Map<String, FormatFactory> registry = new HashMap<>();

    @BeforeEach
    public void setUp() {
        registry.put("lower", new LowerCaseFormatFactory());
        registry.put("upper", new UpperCaseFormatFactory());
    }

    /**
     * Test extended formats.
     */
    @Test
    public void testExtendedFormats() {
        final String pattern = "Lower: {0,lower} Upper: {1,upper}";
        final ExtendedMessageFormat emf = new ExtendedMessageFormat(pattern, registry);
        assertEquals(pattern, emf.toPattern(), "TOPATTERN");
        assertEquals(emf.format(new Object[] {"foo", "bar"}), "Lower: foo Upper: BAR");
        assertEquals(emf.format(new Object[] {"Foo", "Bar"}), "Lower: foo Upper: BAR");
        assertEquals(emf.format(new Object[] {"FOO", "BAR"}), "Lower: foo Upper: BAR");
        assertEquals(emf.format(new Object[] {"FOO", "bar"}), "Lower: foo Upper: BAR");
        assertEquals(emf.format(new Object[] {"foo", "BAR"}), "Lower: foo Upper: BAR");
    }

    /**
     * Test Bug LANG-477 - out of memory error with escaped quote
     */
    @Test
    public void testEscapedQuote_LANG_477() {
        final String pattern = "it''s a {0,lower} 'test'!";
        final ExtendedMessageFormat emf = new ExtendedMessageFormat(pattern, registry);
        assertEquals("it's a dummy test!", emf.format(new Object[] {"DUMMY"}));
    }

    /**
     * Test Bug LANG-917 - IndexOutOfBoundsException and/or infinite loop when using a choice pattern
     */
    @Test
    public void testEmbeddedPatternInChoice() {
        final String pattern = "Hi {0,lower}, got {1,choice,0#none|1#one|1<{1,number}}, {2,upper}!";
        final ExtendedMessageFormat emf = new ExtendedMessageFormat(pattern, registry);
        assertEquals(emf.format(new Object[] {"there", 3, "great"}), "Hi there, got 3, GREAT!");
    }

    /**
     * Test Bug LANG-948 - Exception while using ExtendedMessageFormat and escaping braces
     */
    @Test
    public void testEscapedBraces_LANG_948() {
        // message without placeholder because braces are escaped by quotes
        final String pattern = "Message without placeholders '{}'";
        final ExtendedMessageFormat emf = new ExtendedMessageFormat(pattern, registry);
        assertEquals("Message without placeholders {}", emf.format(new Object[] {"DUMMY"}));

        // message with placeholder because quotes are escaped by quotes
        final String pattern2 = "Message with placeholder ''{0}''";
        final ExtendedMessageFormat emf2 = new ExtendedMessageFormat(pattern2, registry);
        assertEquals("Message with placeholder 'DUMMY'", emf2.format(new Object[] {"DUMMY"}));
    }

    /**
     * Test extended and built in formats.
     */
    @Test
    public void testExtendedAndBuiltInFormats() {
        final Calendar cal = Calendar.getInstance();
        cal.set(2007, Calendar.JANUARY, 23, 18, 33, 5);
        final Object[] args = {"John Doe", cal.getTime(), Double.valueOf("12345.67")};
        final String builtinsPattern = "DOB: {1,date,short} Salary: {2,number,currency}";
        final String extendedPattern = "Name: {0,upper} ";
        final String pattern = extendedPattern + builtinsPattern;

        final HashSet<Locale> testLocales = new HashSet<>(Arrays.asList(DateFormat.getAvailableLocales()));
        testLocales.retainAll(Arrays.asList(NumberFormat.getAvailableLocales()));
        testLocales.add(null);

        for (final Locale locale : testLocales) {
            final MessageFormat builtins = createMessageFormat(builtinsPattern, locale);
            final String expectedPattern = extendedPattern + builtins.toPattern();
            DateFormat df = null;
            NumberFormat nf = null;
            ExtendedMessageFormat emf = null;
            if (locale == null) {
                df = DateFormat.getDateInstance(DateFormat.SHORT);
                nf = NumberFormat.getCurrencyInstance();
                emf = new ExtendedMessageFormat(pattern, registry);
            } else {
                df = DateFormat.getDateInstance(DateFormat.SHORT, locale);
                nf = NumberFormat.getCurrencyInstance(locale);
                emf = new ExtendedMessageFormat(pattern, locale, registry);
            }
            final StringBuilder expected = new StringBuilder();
            expected.append("Name: ");
            expected.append(args[0].toString().toUpperCase(Locale.ROOT));
            expected.append(" DOB: ");
            expected.append(df.format(args[1]));
            expected.append(" Salary: ");
            expected.append(nf.format(args[2]));
            assertEquals(expectedPattern, emf.toPattern(), "pattern comparison for locale " + locale);
            assertEquals(expected.toString(), emf.format(args), String.valueOf(locale));
        }
    }

//    /**
//     * Test extended formats with choice format.
//     *
//     * NOTE: FAILING - currently sub-formats not supported
//     */
//    public void testExtendedWithChoiceFormat() {
//        String pattern = "Choice: {0,choice,1.0#{1,lower}|2.0#{1,upper}}";
//        ExtendedMessageFormat emf = new ExtendedMessageFormat(pattern, registry);
//        assertPatterns(null, pattern, emf.toPattern());
//        try {
//            assertEquals("one", emf.format(new Object[] {Integer.valueOf(1), "ONE"}));
//            assertEquals("TWO", emf.format(new Object[] {Integer.valueOf(2), "two"}));
//        } catch (IllegalArgumentException e) {
//            // currently sub-formats not supported
//        }
//    }

//    /**
//     * Test mixed extended and built-in formats with choice format.
//     *
//     * NOTE: FAILING - currently sub-formats not supported
//     */
//    public void testExtendedAndBuiltInWithChoiceFormat() {
//        String pattern = "Choice: {0,choice,1.0#{0} {1,lower} {2,number}|2.0#{0} {1,upper} {2,number,currency}}";
//        Object[] lowArgs  = new Object[] {Integer.valueOf(1), "Low",  Double.valueOf("1234.56")};
//        Object[] highArgs = new Object[] {Integer.valueOf(2), "High", Double.valueOf("9876.54")};
//        Locale[] availableLocales = ChoiceFormat.getAvailableLocales();
//        Locale[] testLocales = new Locale[availableLocales.length + 1];
//        testLocales[0] = null;
//        System.arraycopy(availableLocales, 0, testLocales, 1, availableLocales.length);
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
     * Test the built-in choice format.
     */
    @Test
    public void testBuiltInChoiceFormat() {
        final Object[] values = new Number[] {Integer.valueOf(1), Double.valueOf("2.2"), Double.valueOf("1234.5")};
        String choicePattern;
        final Locale[] availableLocales = NumberFormat.getAvailableLocales();

        choicePattern = "{0,choice,1#One|2#Two|3#Many {0,number}}";
        for (final Object value : values) {
            checkBuiltInFormat(value + ": " + choicePattern, new Object[] {value}, availableLocales);
        }

        choicePattern = "{0,choice,1#''One''|2#\"Two\"|3#''{Many}'' {0,number}}";
        for (final Object value : values) {
            checkBuiltInFormat(value + ": " + choicePattern, new Object[] {value}, availableLocales);
        }
    }

    /**
     * Test the built-in date/time formats
     */
    @Test
    public void testBuiltInDateTimeFormat() {
        final Calendar cal = Calendar.getInstance();
        cal.set(2007, Calendar.JANUARY, 23, 18, 33, 5);
        final Object[] args = {cal.getTime()};
        final Locale[] availableLocales = DateFormat.getAvailableLocales();

        checkBuiltInFormat("1: {0,date,short}",    args, availableLocales);
        checkBuiltInFormat("2: {0,date,medium}",   args, availableLocales);
        checkBuiltInFormat("3: {0,date,long}",     args, availableLocales);
        checkBuiltInFormat("4: {0,date,full}",     args, availableLocales);
        checkBuiltInFormat("5: {0,date,d MMM yy}", args, availableLocales);
        checkBuiltInFormat("6: {0,time,short}",    args, availableLocales);
        checkBuiltInFormat("7: {0,time,medium}",   args, availableLocales);
        checkBuiltInFormat("8: {0,time,long}",     args, availableLocales);
        checkBuiltInFormat("9: {0,time,full}",     args, availableLocales);
        checkBuiltInFormat("10: {0,time,HH:mm}",   args, availableLocales);
        checkBuiltInFormat("11: {0,date}",         args, availableLocales);
        checkBuiltInFormat("12: {0,time}",         args, availableLocales);
    }

    @Test
    public void testOverriddenBuiltinFormat() {
        final Calendar cal = Calendar.getInstance();
        cal.set(2007, Calendar.JANUARY, 23);
        final Object[] args = {cal.getTime()};
        final Locale[] availableLocales = DateFormat.getAvailableLocales();
        final Map<String, ? extends FormatFactory> dateRegistry = Collections.singletonMap("date", new OverrideShortDateFormatFactory());

        //check the non-overridden builtins:
        checkBuiltInFormat("1: {0,date}", dateRegistry,          args, availableLocales);
        checkBuiltInFormat("2: {0,date,medium}", dateRegistry,   args, availableLocales);
        checkBuiltInFormat("3: {0,date,long}", dateRegistry,     args, availableLocales);
        checkBuiltInFormat("4: {0,date,full}", dateRegistry,     args, availableLocales);
        checkBuiltInFormat("5: {0,date,d MMM yy}", dateRegistry, args, availableLocales);

        //check the overridden format:
        for (int i = -1; i < availableLocales.length; i++) {
            final Locale locale = i < 0 ? null : availableLocales[i];
            final MessageFormat dateDefault = createMessageFormat("{0,date}", locale);
            final String pattern = "{0,date,short}";
            final ExtendedMessageFormat dateShort = new ExtendedMessageFormat(pattern, locale, dateRegistry);
            assertEquals(dateDefault.format(args), dateShort.format(args), "overridden date,short format");
            assertEquals(pattern, dateShort.toPattern(), "overridden date,short pattern");
        }
    }

    /**
     * Test the built-in number formats.
     */
    @Test
    public void testBuiltInNumberFormat() {
        final Object[] args = {Double.valueOf("6543.21")};
        final Locale[] availableLocales = NumberFormat.getAvailableLocales();
        checkBuiltInFormat("1: {0,number}",            args, availableLocales);
        checkBuiltInFormat("2: {0,number,integer}",    args, availableLocales);
        checkBuiltInFormat("3: {0,number,currency}",   args, availableLocales);
        checkBuiltInFormat("4: {0,number,percent}",    args, availableLocales);
        checkBuiltInFormat("5: {0,number,00000.000}",  args, availableLocales);
    }

    /**
     * Test equals() and hashcode.
     */
    @Test
    public void testEqualsHashcode() {
        final Map<String, ? extends FormatFactory> fmtRegistry = Collections.singletonMap("testfmt", new LowerCaseFormatFactory());
        final Map<String, ? extends FormatFactory> otherRegistry = Collections.singletonMap("testfmt", new UpperCaseFormatFactory());

        final String pattern = "Pattern: {0,testfmt}";
        final ExtendedMessageFormat emf = new ExtendedMessageFormat(pattern, Locale.US, fmtRegistry);

        ExtendedMessageFormat other;

        // Same object
        assertEquals(emf, emf, "same, equals()");
        assertEquals(emf.hashCode(), emf.hashCode(), "same, hashcode()");

        // Equal Object
        other = new ExtendedMessageFormat(pattern, Locale.US, fmtRegistry);
        assertEquals(emf, other, "equal, equals()");
        assertEquals(emf.hashCode(), other.hashCode(), "equal, hashcode()");

        // Different Class
        other = new OtherExtendedMessageFormat(pattern, Locale.US, fmtRegistry);
        assertNotEquals(emf, other, "class, equals()");
        assertEquals(emf.hashCode(), other.hashCode(), "class, hashcode()"); // same hashcode

        // Different pattern
        other = new ExtendedMessageFormat("X" + pattern, Locale.US, fmtRegistry);
        assertNotEquals(emf, other, "pattern, equals()");
        assertNotEquals(emf.hashCode(), other.hashCode(), "pattern, hashcode()");

        // Different registry
        other = new ExtendedMessageFormat(pattern, Locale.US, otherRegistry);
        assertNotEquals(emf, other, "registry, equals()");
        assertNotEquals(emf.hashCode(), other.hashCode(), "registry, hashcode()");

        // Different Locale
        other = new ExtendedMessageFormat(pattern, Locale.FRANCE, fmtRegistry);
        assertNotEquals(emf, other, "locale, equals()");
        assertEquals(emf.hashCode(), other.hashCode(), "locale, hashcode()"); // same hashcode
    }

    /**
     * Test a built-in format for the specified Locales, plus {@code null} Locale.
     * @param pattern MessageFormat pattern
     * @param args MessageFormat arguments
     * @param locales to test
     */
    private void checkBuiltInFormat(final String pattern, final Object[] args, final Locale[] locales) {
        checkBuiltInFormat(pattern, null, args, locales);
    }

    /**
     * Test a built-in format for the specified Locales, plus {@code null} Locale.
     * @param pattern MessageFormat pattern
     * @param fmtRegistry FormatFactory registry to use
     * @param args MessageFormat arguments
     * @param locales to test
     */
    private void checkBuiltInFormat(final String pattern, final Map<String, ?> fmtRegistry, final Object[] args, final Locale[] locales) {
        checkBuiltInFormat(pattern, fmtRegistry, args, (Locale) null);
        for (final Locale locale : locales) {
            checkBuiltInFormat(pattern, fmtRegistry, args, locale);
        }
    }

    /**
     * Create an ExtendedMessageFormat for the specified pattern and locale and check the
     * formatted output matches the expected result for the parameters.
     * @param pattern string
     * @param registryUnused map (currently unused)
     * @param args Object[]
     * @param locale Locale
     */
    private void checkBuiltInFormat(final String pattern, final Map<String, ?> registryUnused, final Object[] args, final Locale locale) {
        final StringBuilder buffer = new StringBuilder();
        buffer.append("Pattern=[");
        buffer.append(pattern);
        buffer.append("], locale=[");
        buffer.append(locale);
        buffer.append("]");
        final MessageFormat mf = createMessageFormat(pattern, locale);
        ExtendedMessageFormat emf = null;
        if (locale == null) {
            emf = new ExtendedMessageFormat(pattern);
        } else {
            emf = new ExtendedMessageFormat(pattern, locale);
        }
        assertEquals(mf.format(args), emf.format(args), "format "    + buffer.toString());
        assertEquals(mf.toPattern(), emf.toPattern(), "toPattern " + buffer.toString());
    }

    /**
     * Replace MessageFormat(String, Locale) constructor (not available until JDK 1.4).
     * @param pattern string
     * @param locale Locale
     * @return MessageFormat
     */
    private MessageFormat createMessageFormat(final String pattern, final Locale locale) {
        final MessageFormat result = new MessageFormat(pattern);
        if (locale != null) {
            result.setLocale(locale);
            result.applyPattern(pattern);
        }
        return result;
    }

    // ------------------------ Test Formats ------------------------

    /**
     * {@link Format} implementation which converts to lower case.
     */
    private static class LowerCaseFormat extends Format {
        private static final long serialVersionUID = 1L;

        @Override
        public StringBuffer format(final Object obj, final StringBuffer toAppendTo, final FieldPosition pos) {
            return toAppendTo.append(((String) obj).toLowerCase(Locale.ROOT));
        }
        @Override
        public Object parseObject(final String source, final ParsePosition pos) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * {@link Format} implementation which converts to upper case.
     */
    private static class UpperCaseFormat extends Format {
        private static final long serialVersionUID = 1L;

        @Override
        public StringBuffer format(final Object obj, final StringBuffer toAppendTo, final FieldPosition pos) {
            return toAppendTo.append(((String) obj).toUpperCase(Locale.ROOT));
        }

        @Override
        public Object parseObject(final String source, final ParsePosition pos) {
            throw new UnsupportedOperationException();
        }
    }


    // ------------------------ Test Format Factories ---------------
    /**
     * {@link FormatFactory} implementation for lower case format.
     */
    private static class LowerCaseFormatFactory implements FormatFactory {
        private static final Format LOWER_INSTANCE = new LowerCaseFormat();

        @Override
        public Format getFormat(final String name, final String arguments, final Locale locale) {
            return LOWER_INSTANCE;
        }
    }
    /**
     * {@link FormatFactory} implementation for upper case format.
     */
    private static class UpperCaseFormatFactory implements FormatFactory {
        private static final Format UPPER_INSTANCE = new UpperCaseFormat();

        @Override
        public Format getFormat(final String name, final String arguments, final Locale locale) {
            return UPPER_INSTANCE;
        }
    }
    /**
     * {@link FormatFactory} implementation to override date format "short" to "default".
     */
    private static class OverrideShortDateFormatFactory implements FormatFactory {

        @Override
        public Format getFormat(final String name, final String arguments, final Locale locale) {
            return !"short".equals(arguments) ? null
                    : locale == null ? DateFormat
                            .getDateInstance(DateFormat.DEFAULT) : DateFormat
                            .getDateInstance(DateFormat.DEFAULT, locale);
        }
    }

    /**
     * Alternative ExtendedMessageFormat impl.
     */
    private static class OtherExtendedMessageFormat extends ExtendedMessageFormat {
        private static final long serialVersionUID = 1L;

        OtherExtendedMessageFormat(final String pattern, final Locale locale,
                final Map<String, ? extends FormatFactory> registry) {
            super(pattern, locale, registry);
        }

    }

}
