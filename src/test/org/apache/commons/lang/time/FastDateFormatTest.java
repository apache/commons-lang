/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
 *
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 */

package org.apache.commons.lang.time;

import junit.framework.*;
import junit.textui.TestRunner;
import java.util.*;
import java.text.*;

/**
 * Unit tests {@link org.apache.commons.lang.time.FastDateFormat}.
 *
 * @author Sean Schofield
 * @since 2.0
 * @version $Id: FastDateFormatTest.java,v 1.2 2003/01/07 21:21:43 bayard Exp $
 */
public class FastDateFormatTest extends TestCase {

    private FastDateFormat fastDateFormat = null;

    public FastDateFormatTest(String name) {
        super(name);
    }

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(FastDateFormatTest.class);
        suite.setName("FastDateFormat Tests");

        return suite;
    }

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    public void test_getInstance() {
        FastDateFormat format1 = FastDateFormat.getInstance();
        FastDateFormat format2 = FastDateFormat.getInstance();
        assertSame(format1, format2);
    }

    public void test_getInstance_String() {
        FastDateFormat format1 = FastDateFormat.getInstance("MM/DD/yyyy");
        FastDateFormat format2 = FastDateFormat.getInstance("MM-DD-yyyy");
        assertTrue(format1 != format2); // -- junit 3.8 version -- assertFalse(format1 == format2);
        assertSame(format1, FastDateFormat.getInstance("MM/DD/yyyy"));
    }

    public void test_getInstance_String_TimeZone() {
        Locale realDefaultLocale = Locale.getDefault();
        Locale.setDefault(Locale.US);
        TimeZone realDefaultZone = TimeZone.getDefault();
        TimeZone.setDefault(TimeZone.getTimeZone("America/New_York"));

        FastDateFormat format1 = FastDateFormat.getInstance("MM/DD/yyyy",
                TimeZone.getTimeZone("Atlantic/Reykjavik"));
        FastDateFormat format2 = FastDateFormat.getInstance("MM/DD/yyyy");
        FastDateFormat format3 = FastDateFormat.getInstance("MM/DD/yyyy", TimeZone.getDefault());
        FastDateFormat format4 = FastDateFormat.getInstance("MM/DD/yyyy", TimeZone.getDefault());
        FastDateFormat format5 = FastDateFormat.getInstance("MM-DD-yyyy", TimeZone.getDefault());

        assertTrue(format1 != format2); // -- junit 3.8 version -- assertFalse(format1 == format2);
        assertTrue(format1.getTimeZone().equals(TimeZone.getTimeZone("Atlantic/Reykjavik")));
        assertNull(format2.getTimeZone());
        assertSame(format3, format4);
        assertTrue(format3 != format5); // -- junit 3.8 version -- assertFalse(format3 == format5);

        Locale.setDefault(realDefaultLocale);
        TimeZone.setDefault(realDefaultZone);
    }

    public void test_getInstance_String_Locale() {
        Locale realDefaultLocale = Locale.getDefault();
        Locale.setDefault(Locale.US);
        FastDateFormat format1 = FastDateFormat.getInstance("MM/DD/yyyy", Locale.GERMANY);
        FastDateFormat format2 = FastDateFormat.getInstance("MM/DD/yyyy");
        FastDateFormat format3 = FastDateFormat.getInstance("MM/DD/yyyy", Locale.GERMANY);

        assertTrue(format1 != format2); // -- junit 3.8 version -- assertFalse(format1 == format2);
        assertSame(format1, format3);
        assertSame(Locale.GERMANY, format1.getLocale());

        Locale.setDefault(realDefaultLocale);
    }

    public void test_getInstance_String_TimeZone_Locale() {
        Locale realDefaultLocale = Locale.getDefault();
        Locale.setDefault(Locale.US);
        TimeZone realDefaultZone = TimeZone.getDefault();
        TimeZone.setDefault(TimeZone.getTimeZone("America/New_York"));

        FastDateFormat format1 = FastDateFormat.getInstance("MM/DD/yyyy",
                TimeZone.getTimeZone("Atlantic/Reykjavik"), Locale.GERMANY);
        FastDateFormat format2 = FastDateFormat.getInstance("MM/DD/yyyy", Locale.GERMANY);
        FastDateFormat format3 = FastDateFormat.getInstance("MM/DD/yyyy",
                TimeZone.getDefault(), Locale.GERMANY);

        assertTrue(format1 != format2); // -- junit 3.8 version -- assertNotSame(format1, format2);
        assertEquals(format1.getTimeZone(), TimeZone.getTimeZone("Atlantic/Reykjavik"));
        assertNull(format2.getTimeZone());
        assertEquals(format3.getTimeZone(), TimeZone.getDefault());
        assertEquals(format3.getTimeZone(), TimeZone.getTimeZone("America/New_York"));

        Locale.setDefault(realDefaultLocale);
        TimeZone.setDefault(realDefaultZone);
    }
}
