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
import java.text.NumberFormat;
import java.text.ParseException;
import java.text.ParsePosition;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Locale;

import org.apache.commons.lang.ClassUtils;
import org.apache.commons.lang.Validate;

import junit.framework.TestCase;

/**
 * Test MultiFormat
 * 
 * @author Matt Benson
 * @since 2.4
 * @version $Id$
 */
public class MultiFormatTest extends TestCase {
    private class GuardedFormat extends Format {
        private static final long serialVersionUID = 1L;

        Format delegate;
        Class[] allowableTypes;

        /**
         * Create a new MultiFormatTest.GuardedFormat.
         */
        public GuardedFormat(Format delegate, Class[] allowableTypes) {
            Validate.notNull(delegate);
            this.delegate = delegate;
            Validate.notNull(allowableTypes);
            this.allowableTypes = allowableTypes;
        }

        public StringBuffer format(Object obj, StringBuffer toAppendTo,
                FieldPosition pos) {
            Class c = obj == null ? null : obj.getClass();
            for (int i = 0; i < allowableTypes.length; i++) {
                if (ClassUtils.isAssignable(c, allowableTypes[i])) {
                    return delegate.format(obj, toAppendTo, pos);
                }
            }
            throw new IllegalArgumentException();
        }

        public Object parseObject(String source, ParsePosition pos) {
            return delegate.parseObject(source, pos);
        }
    }

    private Format format;

    /*
     * (non-Javadoc)
     * 
     * @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();
        // silliness to avoid the DateFormat grabbing the Integer, or the
        // integer parsing the first (month) date component:
        format = new MultiFormat.Builder().add(
                new GuardedFormat(DateFormat.getDateInstance(DateFormat.SHORT,
                        Locale.US), new Class[] { Date.class })).add(
                getIntegerNumberFormat(Locale.US)).toMultiFormat();
    }

    private NumberFormat getIntegerNumberFormat(Locale locale) {
        NumberFormat result = NumberFormat.getInstance(locale);
        result.setMaximumFractionDigits(0);
        result.setParseIntegerOnly(true);
        return result;
    }

    public void testFormatNumber() {
        assertEquals("1,000", format.format(new Integer(1000)));
    }

    public void testParseNumber() throws ParseException {
        assertEquals(new Integer(-1000).intValue(), ((Number) format
                .parseObject("-1,000")).intValue());
    }

    public void testFormatDate() {
        assertEquals("1/1/70", format.format(new GregorianCalendar(1970,
                Calendar.JANUARY, 01).getTime()));
    }

    public void testParseDate() throws ParseException {
        assertEquals(new GregorianCalendar(1970, Calendar.JANUARY, 01)
                .getTime(), format.parseObject("1/1/70"));
    }

    public void testFormatObject() {
        try {
            format.format(new Object());
            fail("expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // okay
        }
    }

    public void testParseGarbage() {
        try {
            format.parseObject("garbage");
            fail("expected ParseException");
        } catch (ParseException e) {
            //okay
        }
    }
}
