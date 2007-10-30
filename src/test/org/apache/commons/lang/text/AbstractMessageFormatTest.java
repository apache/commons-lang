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
import java.text.MessageFormat;
import java.util.Calendar;
import java.util.GregorianCalendar;

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
    protected static final Object[] NUMBERS = { new Double(0.1),
            new Double(1.1), new Double(2.1) };

    protected static final Object[] DATES = {
            new GregorianCalendar(1970, Calendar.JANUARY, 01, 0, 15, 20)
                    .getTime(),
            new GregorianCalendar(1970, Calendar.FEBRUARY, 02, 12, 30, 35)
                    .getTime(),
            new GregorianCalendar(1970, Calendar.MARCH, 03, 18, 45, 50)
                    .getTime() };

    /*
     * (non-Javadoc)
     * 
     * @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();
    }

    protected abstract MessageFormat createMessageFormat(String pattern);

    protected void doAssertions(String expected, String pattern, Object[] args) {
        doAssertions(expected, pattern, args, pattern);
    }

    protected void doAssertions(String expected, String pattern, Object[] args,
            String toPattern) {
        MessageFormat f = createMessageFormat(pattern);
        assertEquals(expected, f.format(args));
        assertEquals(toPattern, f.toPattern());
    }

    public void testPlain() {
        StringBuffer pattern = new StringBuffer();
        for (int i = 0; i < NUMBERS.length; i++) {
            if (i > 0) {
                pattern.append("; ");
            }
            pattern.append("Object ").append(i).append(": ").append(NUMBERS[i]);
        }
        String p = pattern.toString();
        doAssertions(p, p, NUMBERS);
    }

    public void testSimple() {
        doAssertions("Object 0: 0.1; Object 1: 1.1; Object 2: 2.1",
                "Object 0: {0}; Object 1: {1}; Object 2: {2}", NUMBERS);
    }

    public void testNumber() {
        doAssertions(
                "Number 0: 0.1; Number 1: 1.1; Number 2: 2.1",
                "Number 0: {0,number}; Number 1: {1,number}; Number 2: {2,number}",
                NUMBERS);
    }

    public void testNumberLooseFormatting() {
        doAssertions(
                "Number 0: 0.1; Number 1: 1.1; Number 2: 2.1",
                "Number 0: {0, number }; Number 1: {1, number }; Number 2: {2, number }",
                NUMBERS,
                "Number 0: {0,number}; Number 1: {1,number}; Number 2: {2,number}");
    }

    public void testInteger() {
        doAssertions(
                "Number 0: 0; Number 1: 1; Number 2: 2",
                "Number 0: {0,number,integer}; Number 1: {1,number,integer}; Number 2: {2,number,integer}",
                NUMBERS);
    }

    public void testIntegerLooseFormatting() {
        doAssertions(
                "Number 0: 0; Number 1: 1; Number 2: 2",
                "Number 0: {0, number , integer }; Number 1: {1, number , integer }; Number 2: {2, number , integer }",
                NUMBERS,
                "Number 0: {0,number,integer}; Number 1: {1,number,integer}; Number 2: {2,number,integer}");
    }

    public void testCurrency() {
        doAssertions(
                "Number 0: $0.10; Number 1: $1.10; Number 2: $2.10",
                "Number 0: {0,number,currency}; Number 1: {1,number,currency}; Number 2: {2,number,currency}",
                NUMBERS);
    }

    public void testPercent() {
        doAssertions(
                "Number 0: 10%; Number 1: 110%; Number 2: 210%",
                "Number 0: {0,number,percent}; Number 1: {1,number,percent}; Number 2: {2,number,percent}",
                NUMBERS);
    }

    public void testNumberPattern() {
        doAssertions(
                "Number 0: 000.100; Number 1: 001.100; Number 2: 002.100",
                "Number 0: {0,number,#000.000}; Number 1: {1,number,#000.000}; Number 2: {2,number,#000.000}",
                NUMBERS);
    }

    public void testDate() {
        doAssertions(
                "Date 0: Jan 1, 1970; Date 1: Feb 2, 1970; Date 2: Mar 3, 1970",
                "Date 0: {0,date}; Date 1: {1,date}; Date 2: {2,date}", DATES);
    }

    public void testDateLooseFormatting() {
        doAssertions(
                "Date 0: Jan 1, 1970; Date 1: Feb 2, 1970; Date 2: Mar 3, 1970",
                "Date 0: {0, date }; Date 1: {1, date }; Date 2: {2,  date  }",
                DATES, "Date 0: {0,date}; Date 1: {1,date}; Date 2: {2,date}");
    }

    public void testShortDate() {
        doAssertions(
                "Date 0: 1/1/70; Date 1: 2/2/70; Date 2: 3/3/70",
                "Date 0: {0,date,short}; Date 1: {1,date,short}; Date 2: {2,date,short}",
                DATES);
    }

    public void testShortDateLooseFormatting() {
        doAssertions(
                "Date 0: 1/1/70; Date 1: 2/2/70; Date 2: 3/3/70",
                "Date 0: {0, date , short }; Date 1: {1,  date  , short }; Date 2: {2, date ,  short  }",
                DATES,
                "Date 0: {0,date,short}; Date 1: {1,date,short}; Date 2: {2,date,short}");
    }

    public void testMediumDate() {
        doAssertions(
                "Date 0: Jan 1, 1970; Date 1: Feb 2, 1970; Date 2: Mar 3, 1970",
                "Date 0: {0,date,medium}; Date 1: {1,date,medium}; Date 2: {2,date,medium}",
                DATES, "Date 0: {0,date}; Date 1: {1,date}; Date 2: {2,date}");
    }

    public void testLongDate() {
        doAssertions(
                "Date 0: January 1, 1970; Date 1: February 2, 1970; Date 2: March 3, 1970",
                "Date 0: {0,date,long}; Date 1: {1,date,long}; Date 2: {2,date,long}",
                DATES);
    }

    public void testFullDate() {
        doAssertions(
                "Date 0: Thursday, January 1, 1970; Date 1: Monday, February 2, 1970; Date 2: Tuesday, March 3, 1970",
                "Date 0: {0,date,full}; Date 1: {1,date,full}; Date 2: {2,date,full}",
                DATES);
    }

    public void testDatePattern() {
        doAssertions(
                "Date 0: AD1970.1; Date 1: AD1970.33; Date 2: AD1970.62",
                "Date 0: {0,date,Gyyyy.D}; Date 1: {1,date,Gyyyy.D}; Date 2: {2,date,Gyyyy.D}",
                DATES);
    }

    public void testTime() {
        doAssertions(
                "Time 0: 12:15:20 AM; Time 1: 12:30:35 PM; Time 2: 6:45:50 PM",
                "Time 0: {0,time}; Time 1: {1,time}; Time 2: {2,time}", DATES);
    }

    public void testShortTime() {
        doAssertions(
                "Time 0: 12:15 AM; Time 1: 12:30 PM; Time 2: 6:45 PM",
                "Time 0: {0,time,short}; Time 1: {1,time,short}; Time 2: {2,time,short}",
                DATES);
    }

    public void testMediumTime() {
        doAssertions(
                "Time 0: 12:15:20 AM; Time 1: 12:30:35 PM; Time 2: 6:45:50 PM",
                "Time 0: {0,time,medium}; Time 1: {1,time,medium}; Time 2: {2,time,medium}",
                DATES, "Time 0: {0,time}; Time 1: {1,time}; Time 2: {2,time}");
    }

    public void testLongTime() {
        DateFormat df = DateFormat.getTimeInstance(DateFormat.LONG);
        StringBuffer expected = new StringBuffer();
        for (int i = 0; i < DATES.length; i++) {
            if (i > 0) {
                expected.append("; ");
            }
            expected.append("Time ").append(i).append(": ").append(
                    df.format(DATES[i]));
        }
        doAssertions(
                expected.toString(),
                "Time 0: {0,time,long}; Time 1: {1,time,long}; Time 2: {2,time,long}",
                DATES);
    }

    public void testFullTime() {
        DateFormat df = DateFormat.getTimeInstance(DateFormat.FULL);
        StringBuffer expected = new StringBuffer();
        for (int i = 0; i < DATES.length; i++) {
            if (i > 0) {
                expected.append("; ");
            }
            expected.append("Time ").append(i).append(": ").append(
                    df.format(DATES[i]));
        }
        doAssertions(
                expected.toString(),
                "Time 0: {0,time,full}; Time 1: {1,time,full}; Time 2: {2,time,full}",
                DATES,
                "Time 0: {0,time,long}; Time 1: {1,time,long}; Time 2: {2,time,long}");
    }

    public void testTimePattern() {
        doAssertions(
                "Time 0: AM01520; Time 1: PM123035; Time 2: PM184550",
                "Time 0: {0,time,aHms}; Time 1: {1,time,aHms}; Time 2: {2,time,aHms}",
                DATES,
                "Time 0: {0,date,aHms}; Time 1: {1,date,aHms}; Time 2: {2,date,aHms}");
    }

    public void testChoice() {
        String choice = "0.0#x|1.0#y|2.0#z";
        StringBuffer pattern = new StringBuffer();
        for (int i = 0; i < 3; i++) {
            if (i > 0) {
                pattern.append("; ");
            }
            pattern.append("Choice ").append(i).append(": {").append(i).append(
                    ",choice,").append(choice).append("}");
        }
        doAssertions("Choice 0: x; Choice 1: y; Choice 2: z", pattern
                .toString(), NUMBERS);
    }

    public void testChoiceLooseFormatting() {
        String choice = "0.0#x |1.0#y |2.0#z ";
        StringBuffer pattern = new StringBuffer();
        for (int i = 0; i < 3; i++) {
            if (i > 0) {
                pattern.append("; ");
            }
            pattern.append("Choice ").append(i).append(": {").append(i).append(
                    ",choice,").append(choice).append("}");
        }
        doAssertions("Choice 0: x ; Choice 1: y ; Choice 2: z ", pattern
                .toString(), NUMBERS);
    }

    public void testChoiceRecursive() {
        String choice = "0.0#{0}|1.0#{1}|2.0#{2}";
        StringBuffer pattern = new StringBuffer();
        for (int i = 0; i < 3; i++) {
            if (i > 0) {
                pattern.append("; ");
            }
            pattern.append("Choice ").append(i).append(": {").append(i).append(
                    ",choice,").append(choice).append("}");
        }
        doAssertions("Choice 0: 0.1; Choice 1: 1.1; Choice 2: 2.1", pattern
                .toString(), NUMBERS);
    }
}
