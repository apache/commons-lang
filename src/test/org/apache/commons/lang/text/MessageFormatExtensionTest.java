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
import java.text.ParsePosition;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Locale;

/**
 * Extension tests for {@link ExtendedMessageFormat}
 * 
 * @author Matt Benson
 * @since 2.4
 * @version $Id$
 */
public abstract class MessageFormatExtensionTest extends
        AbstractMessageFormatTest {
    /**
     * Tests for <code>Locale.US</code>
     * 
     * @author mbenson
     */
    public static class US extends MessageFormatExtensionTest {
        /**
         * {@inheritDoc}
         */
        protected Locale getLocale() {
            return Locale.US;
        }
    }

    /**
     * Tests for <code>Locale.UK</code>
     * 
     * @author mbenson
     */
    public static class UK extends MessageFormatExtensionTest {
        /**
         * {@inheritDoc}
         */
        protected Locale getLocale() {
            return Locale.UK;
        }
    }

    /**
     * Tests for <code>Locale.GERMANY</code>
     * 
     * @author mbenson
     */
    public static class DE extends MessageFormatExtensionTest {
        /**
         * {@inheritDoc}
         */
        protected Locale getLocale() {
            return Locale.GERMANY;
        }
    }

    /**
     * Tests for <code>Locale.ITALY</code>
     * 
     * @author mbenson
     */
    public static class IT extends MessageFormatExtensionTest {
        /**
         * {@inheritDoc}
         */
        protected Locale getLocale() {
            return Locale.ITALY;
        }
    }

    /**
     * Tests for <code>Locale.JAPAN</code>
     * 
     * @author mbenson
     */
    public static class JP extends MessageFormatExtensionTest {
        /**
         * {@inheritDoc}
         */
        protected Locale getLocale() {
            return Locale.JAPAN;
        }
    }

    /**
     * Tests for <code>Locale.CHINA</code>
     * 
     * @author mbenson
     */
    public static class CN extends MessageFormatExtensionTest {
        /**
         * {@inheritDoc}
         */
        protected Locale getLocale() {
            return Locale.CHINA;
        }
    }

    /**
     * Tests for <code>Locale.CANADA</code>
     * 
     * @author mbenson
     */
    public static class CA extends MessageFormatExtensionTest {
        /**
         * {@inheritDoc}
         */
        protected Locale getLocale() {
            return Locale.CANADA;
        }
    }

    /**
     * Tests for <code>Locale.FRANCE</code>
     * 
     * @author mbenson
     */
    public static class FR extends MessageFormatExtensionTest {
        /**
         * {@inheritDoc}
         */
        protected Locale getLocale() {
            return Locale.FRANCE;
        }
    }

    /**
     * Tests for <code>Locale.KOREA</code>
     * 
     * @author mbenson
     */
    public static class KR extends MessageFormatExtensionTest {
        /**
         * {@inheritDoc}
         */
        protected Locale getLocale() {
            return Locale.KOREA;
        }
    }

    /**
     * Tests for <code>Locale.TAIWAN</code>
     * 
     * @author mbenson
     */
    public static class TW extends MessageFormatExtensionTest {
        /**
         * {@inheritDoc}
         */
        protected Locale getLocale() {
            return Locale.TAIWAN;
        }
    }

    static class ProperNameCapitalizationFormat extends Format {
        private static final long serialVersionUID = -6081911520622186866L;
        private static final StrMatcher MATCH = StrMatcher
                .charSetMatcher(" ,.");

        /**
         * {@inheritDoc}
         */
        public StringBuffer format(Object obj, StringBuffer toAppendTo,
                FieldPosition fpos) {
            if (!(obj instanceof String)) {
                throw new IllegalArgumentException();
            }
            char[] buffer = ((String) obj).toCharArray();
            ParsePosition pos = new ParsePosition(0);
            while (pos.getIndex() < buffer.length) {
                char c = buffer[pos.getIndex()];
                if (Character.isLowerCase(c)) {
                    c = Character.toUpperCase(c);
                }
                if (Character.isUpperCase(c)) {
                    toAppendTo.append(c);
                    next(pos);
                }
                int start = pos.getIndex();
                seekDelimiter(buffer, pos);
                toAppendTo.append(new String(buffer, start, pos.getIndex()
                        - start).toLowerCase());
            }
            return toAppendTo;
        }

        /**
         * Unable to do much; return the String.
         */
        public Object parseObject(String source, ParsePosition pos) {
            return source.substring(pos.getIndex());
        }

        private static void seekDelimiter(char[] buffer, ParsePosition pos) {
            for (; pos.getIndex() < buffer.length
                    && MATCH.isMatch(buffer, pos.getIndex()) == 0; next(pos))
                ;
            if (pos.getIndex() >= buffer.length) {
                return;
            }
            int len = 0;
            do {
                len = MATCH.isMatch(buffer, pos.getIndex());
                pos.setIndex(pos.getIndex() + len);
            } while (len > 0 && pos.getIndex() < buffer.length);
        }

        private static void next(ParsePosition pos) {
            pos.setIndex(pos.getIndex() + 1);
        }
    }

    /**
     * {@inheritDoc}
     */
    protected MessageFormat createMessageFormat(String pattern, Locale locale) {
        return new ExtendedMessageFormat(pattern, locale,
                new MultiFormat.Builder().add(
                        new NameKeyedMetaFormat.Builder().put("properName",
                                new ProperNameCapitalizationFormat())
                                .toNameKeyedMetaFormat()).add(
                        ExtendedMessageFormat.createDefaultMetaFormat(locale))
                        .toMultiFormat());
    }

    public void testProperName() {
        doAssertions("John Q. Public; John Q. Public",
                "{0,properName}; {1,properName}", new String[] {
                        "JOHN Q. PUBLIC", "john q. public" });
    }

    public void testMixed() {
        StringBuffer expected = new StringBuffer("John Q. Public was born on ");
        Date dob = new GregorianCalendar(1970, Calendar.JANUARY, 01, 0, 15, 20)
                .getTime();
        DateFormat longDf = DateFormat.getDateInstance(DateFormat.LONG, locale);
        longDf.format(dob, expected, new FieldPosition(0));
        expected.append('.');
        String pattern = "{0,properName} was born on {1,date,long}.";
        StringBuffer toPattern = new StringBuffer(pattern);
        if (longDf.equals(DateFormat.getDateInstance(DateFormat.DEFAULT, locale))) {
            int idx = pattern.indexOf(",long");
            toPattern.delete(idx, idx + ",long".length());
        }
        doAssertions(expected.toString(),
                pattern, new Object[] {
                        "john q. public",
                        new GregorianCalendar(1970, Calendar.JANUARY, 01, 0,
                                15, 20).getTime() }, toPattern.toString());
    }

}
