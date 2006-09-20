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

import java.text.Format;
import java.text.FieldPosition;
import java.text.ParsePosition;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

import java.text.SimpleDateFormat;
import java.util.Locale;

/**
 * Unit tests for {@link org.apache.commons.lang.text.CompositeFormat}.
 */
public class CompositeFormatTest extends TestCase {

    /**
     * Main method.
     * 
     * @param args  command line arguments, ignored
     */
    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    /**
     * Return a new test suite containing this test case.
     * 
     * @return a new test suite containing this test case
     */
    public static Test suite() {
        TestSuite suite = new TestSuite(CompositeFormatTest.class);
        suite.setName("CompositeFormat Tests");
        return suite;
    }

    /**
     * Create a new test case with the specified name.
     * 
     * @param name
     *            name
     */
    public CompositeFormatTest(String name) {
        super(name);
    }


    /**
     * Ensures that the parse/format separation is correctly maintained. 
     */
    public void testCompositeFormat() {

        Format parser = new Format() {
            public StringBuffer format(Object obj, StringBuffer toAppendTo, FieldPosition pos) {
                throw new UnsupportedOperationException("Not implemented");
            }

            public Object parseObject(String source, ParsePosition pos) {
                return null;    // do nothing
            }
        };

        Format formatter = new Format() {
            public StringBuffer format(Object obj, StringBuffer toAppendTo, FieldPosition pos) {
                return null;    // do nothing
            }

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

    public void testUsage() throws Exception {
        Format f1 = new SimpleDateFormat("MMddyyyy", Locale.ENGLISH);
        Format f2 = new SimpleDateFormat("MMMM d, yyyy", Locale.ENGLISH);
        CompositeFormat c = new CompositeFormat(f1, f2);
        String testString = "January 3, 2005";
        assertEquals(testString, c.format(c.parseObject("01032005")));
        assertEquals(testString, c.reformat("01032005"));
    }

}
