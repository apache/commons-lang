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

import java.text.FieldPosition;
import java.text.Format;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.Locale;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Unit tests for {@link org.apache.commons.lang3.text.CompositeFormat}.
 */
@Deprecated
public class CompositeFormatTest extends AbstractLangTest {

    /**
     * Ensures that the parse/format separation is correctly maintained.
     */
    @Test
    public void testCompositeFormat() {

        final Format parser = new Format() {
            private static final long serialVersionUID = 1L;

            @Override
            public StringBuffer format(final Object obj, final StringBuffer toAppendTo, final FieldPosition pos) {
                throw new UnsupportedOperationException("Not implemented");
            }

            @Override
            public Object parseObject(final String source, final ParsePosition pos) {
                return null;    // do nothing
            }
        };

        final Format formatter = new Format() {
            private static final long serialVersionUID = 1L;

            @Override
            public StringBuffer format(final Object obj, final StringBuffer toAppendTo, final FieldPosition pos) {
                return null;    // do nothing
            }

            @Override
            public Object parseObject(final String source, final ParsePosition pos) {
                throw new UnsupportedOperationException("Not implemented");
            }
        };

        final CompositeFormat composite = new CompositeFormat(parser, formatter);

        composite.parseObject("", null);
        composite.format(new Object(), new StringBuffer(), null);
        assertEquals(parser, composite.getParser(), "Parser get method incorrectly implemented");
        assertEquals(formatter, composite.getFormatter(), "Formatter get method incorrectly implemented");
    }

    @Test
    public void testUsage() throws Exception {
        final Format f1 = new SimpleDateFormat("MMddyyyy", Locale.ENGLISH);
        final Format f2 = new SimpleDateFormat("MMMM d, yyyy", Locale.ENGLISH);
        final CompositeFormat c = new CompositeFormat(f1, f2);
        final String testString = "January 3, 2005";
        assertEquals(testString, c.format(c.parseObject("01032005")));
        assertEquals(testString, c.reformat("01032005"));
    }

}
