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

import org.junit.Test;
import static org.junit.Assert.*;
import java.text.FieldPosition;
import java.text.Format;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.Locale;

/**
 * Unit tests for {@link org.apache.commons.lang3.text.CompositeFormat}.
 */
public class CompositeFormatTest {

    /**
     * Ensures that the parse/format separation is correctly maintained. 
     */
    @Test
    public void testCompositeFormat() {

        Format parser = new Format() {
            @Override
            public StringBuffer format(Object obj, StringBuffer toAppendTo, FieldPosition pos) {
                throw new UnsupportedOperationException("Not implemented");
            }

            @Override
            public Object parseObject(String source, ParsePosition pos) {
                return null;    // do nothing
            }
        };

        Format formatter = new Format() {
            @Override
            public StringBuffer format(Object obj, StringBuffer toAppendTo, FieldPosition pos) {
                return null;    // do nothing
            }

            @Override
            public Object parseObject(String source, ParsePosition pos) {
                throw new UnsupportedOperationException("Not implemented");
            }
        };

        CompositeFormat composite = new CompositeFormat(parser, formatter);

        composite.parseObject("", null);
        composite.format(new Object(), new StringBuffer(), null);
        assertEquals( "Parser get method incorrectly implemented", parser, composite.getParser() );
        assertEquals( "Formatter get method incorrectly implemented", formatter, composite.getFormatter() );
    }

    @Test
    public void testUsage() throws Exception {
        Format f1 = new SimpleDateFormat("MMddyyyy", Locale.ENGLISH);
        Format f2 = new SimpleDateFormat("MMMM d, yyyy", Locale.ENGLISH);
        CompositeFormat c = new CompositeFormat(f1, f2);
        String testString = "January 3, 2005";
        assertEquals(testString, c.format(c.parseObject("01032005")));
        assertEquals(testString, c.reformat("01032005"));
    }

}
