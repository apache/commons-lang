/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.commons.lang3;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import junit.framework.AssertionFailedError;
import junit.framework.TestCase;

/*
 * This class was created to hold test cases for the parseCVS method extracted from DateUtilsTest in commons-lang.
 * The code was originally submitted by Serge Knystautas sergek@lokitech.com. It was never
 * fully implemented, and has been moved to the sandbox for further development. Recent discussion
 * from the commons-dev mailing list:
 * http://marc.theaimsgroup.com/?l=jakarta-commons-dev&m=108904098032038&w=2
 * Moving the code to the sandbox satisfies bug is a temporary solution to
 * http://issues.apache.org/bugzilla/show_bug.cgi?id=22172 but this issue needs to be considered
 * when the class/method is further developed.
 */
public class DateFormatterTest extends TestCase {

    /*
     * Tests the parse method, which is supposed to handle various strings
     * as flexibly as CVS supports.
     */
    public void testParseCVS() throws Exception {
        try {
            DateFormatter.parseCVS(null);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            DateFormatter.parseCVS("gobbledegook");
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            DateFormatter.parseCVS("ago");
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            DateFormatter.parseCVS("1 junk ago");
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            DateFormatter.parseCVS("1month ago");
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            DateFormatter.parseCVS("last month");
            fail();
        } catch (IllegalArgumentException ex) {}


        //This is difficult to test since the "now" used in the
        //  parse function cannot be controlled.  We could possibly control
        //  it by trying before and after and making sure the value we expect
        //  is between the two values calculated.
        //For now we're just using the custom assertEquals that takes a delta

        Calendar now = null;

        // M/dd/yy H:mm:ss z
        now = Calendar.getInstance();
        now.set(Calendar.MILLISECOND, 0);
        assertEquals("parseCVS format M/dd/yy H:mm:ss z",
                  now, DateFormatter.parseCVS(new SimpleDateFormat("M/dd/yy H:mm:ss z").format(now.getTime())), 50);
        // MMM d, yyyy h:mm a
        now = Calendar.getInstance();
        now.set(Calendar.MILLISECOND, 0);
        now.set(Calendar.SECOND, 0);
        assertEquals("parseCVS format MMM d, yyyy h:mm a",
                  now, DateFormatter.parseCVS(new SimpleDateFormat("MMM d, yyyy h:mm a").format(now.getTime())), 50);
        // h:mm z
        //
        // This format is difficult to test using the current time because the
        // parseCVS method applies the default date of January 1, 1970 to the
        // parsed time. The most straightforward way to test the parse is to
        // pass in a known value, and test the output against this know value.
        // 
        now = Calendar.getInstance();
        now.setTime(new SimpleDateFormat("h:mm z").parse("16:30 GMT"));
        assertEquals("parseCVS format h:mm z 16:30 GMT", 
                  now, DateFormatter.parseCVS("16:30 GMT"), 50);
        now = Calendar.getInstance();
        now.setTime(new SimpleDateFormat("h:mm z").parse("16:30 EST"));
        assertEquals("parseCVS format h:mm z 16:30 EST", 
                  now, DateFormatter.parseCVS("16:30 EST"), 50);
        now = Calendar.getInstance();
        now.setTime(new SimpleDateFormat("h:mm z").parse("16:30 GMT-05:00"));
        assertEquals("parseCVS format h:mm z 16:30 GMT-05:00", 
                  now, DateFormatter.parseCVS("16:30 GMT-05:00"), 50);
        now = Calendar.getInstance();
        now.setTime(new SimpleDateFormat("h:mm z").parse("16:30 GMT+01:00"));
        assertEquals("parseCVS format h:mm z 16:30 GMT+01:00", 
                  now, DateFormatter.parseCVS("16:30 GMT+01:00"), 50);
        
        now = Calendar.getInstance();
        now.setTime(new SimpleDateFormat("h:mm z").parse("06:30 GMT"));
        assertEquals("parseCVS format h:mm z 06:30 GMT", 
                  now, DateFormatter.parseCVS("06:30 GMT"), 50);
        now = Calendar.getInstance();
        now.setTime(new SimpleDateFormat("h:mm z").parse("06:30 EST"));
        assertEquals("parseCVS format h:mm z 06:30 EST", 
                  now, DateFormatter.parseCVS("06:30 EST"), 50);
        now = Calendar.getInstance();
        now.setTime(new SimpleDateFormat("h:mm z").parse("06:30 GMT-05:00"));
        assertEquals("parseCVS format h:mm z 06:30 GMT-05:00", 
                  now, DateFormatter.parseCVS("06:30 GMT-05:00"), 50);
        now = Calendar.getInstance();
        now.setTime(new SimpleDateFormat("h:mm z").parse("06:30 GMT+01:00"));
        assertEquals("parseCVS format h:mm z 06:30 GMT+01:00", 
                  now, DateFormatter.parseCVS("06:30 GMT+01:00"), 50);
        
        now = Calendar.getInstance();
        now.add(Calendar.WEEK_OF_MONTH, -1);
        assertEquals("parseCVS a week ago",
                now, DateFormatter.parseCVS("a week ago"), 50);
        now = Calendar.getInstance();
        now.add(Calendar.WEEK_OF_MONTH, -1);
        assertEquals("parseCVS an week ago",
                now, DateFormatter.parseCVS("an week ago"), 50);
        
        now = Calendar.getInstance();
        now.add(Calendar.DAY_OF_MONTH, -14);
        assertEquals("parseCVS 1 fortnight ago",
                now, DateFormatter.parseCVS("1 fortnight ago"), 50);
        now = Calendar.getInstance();
        now.add(Calendar.DAY_OF_MONTH, -14);
        assertEquals("parseCVS 1 fortnights ago",
                now, DateFormatter.parseCVS("1 fortnights ago"), 50);
        
        now = Calendar.getInstance();
        now.add(Calendar.MINUTE, -1);
        assertEquals("parseCVS 1 minute ago",
                now, DateFormatter.parseCVS("1 minute ago"), 50);
        now = Calendar.getInstance();
        now.add(Calendar.MINUTE, -8);
        assertEquals("parseCVS 8 minutes ago",
                now, DateFormatter.parseCVS("8 minutes ago"), 50);

        now = Calendar.getInstance();
        now.add(Calendar.MILLISECOND, -1);
        assertEquals("parseCVS 1 millisecond ago",
                now, DateFormatter.parseCVS("1 millisecond ago"), 50);
        now = Calendar.getInstance();
        now.add(Calendar.MILLISECOND, -100);
        assertEquals("parseCVS 1 milliseconds ago",
                now, DateFormatter.parseCVS("100 milliseconds ago"), 50);
        now = Calendar.getInstance();
        now.add(Calendar.SECOND, -30);
        assertEquals("parseCVS 30 second ago",
                now, DateFormatter.parseCVS("30 second ago"), 50);
        now = Calendar.getInstance();
        now.add(Calendar.SECOND, -30);
        assertEquals("parseCVS 30 seconds ago",
                now, DateFormatter.parseCVS("30 seconds ago"), 50);
        now = Calendar.getInstance();
        now.add(Calendar.HOUR, -2);
        assertEquals("parseCVS 2 hour ago",
                now, DateFormatter.parseCVS("2 hour ago"), 50);
        now = Calendar.getInstance();
        now.add(Calendar.HOUR, -2);
        assertEquals("parseCVS 2 hours ago",
                now, DateFormatter.parseCVS("2 hours ago"), 50);
        now = Calendar.getInstance();
        now.add(Calendar.DAY_OF_MONTH, -2);
        assertEquals("parseCVS 2 day ago",
                now, DateFormatter.parseCVS("2 day ago"), 50);
        now = Calendar.getInstance();
        now.add(Calendar.DAY_OF_MONTH, -2);
        assertEquals("parseCVS 2 days ago",
                now, DateFormatter.parseCVS("2 days ago"), 50);
        now = Calendar.getInstance();
        now.add(Calendar.MONTH, -2);
        assertEquals("parseCVS 2 month ago",
                now, DateFormatter.parseCVS("2 month ago"), 50);
        now = Calendar.getInstance();
        now.add(Calendar.MONTH, -2);
        assertEquals("parseCVS 2 months ago",
                now, DateFormatter.parseCVS("2 months ago"), 50);
        now = Calendar.getInstance();
        now.add(Calendar.YEAR, -2);
        assertEquals("parseCVS 2 year ago",
                now, DateFormatter.parseCVS("2 year ago"), 50);
        now = Calendar.getInstance();
        now.add(Calendar.YEAR, -2);
        assertEquals("parseCVS 2 years ago",
                now, DateFormatter.parseCVS("2 years ago"), 50);
        
        now = Calendar.getInstance();
        now.add(Calendar.DATE, -1);
        assertEquals("parseCVS yesterday",
                now, DateFormatter.parseCVS("yesterday"), 50);

        now = Calendar.getInstance();
        now.add(Calendar.DATE, 1);
        assertEquals("parseCVS tomorrow",
                now, DateFormatter.parseCVS("tomorrow"), 50);

        now = Calendar.getInstance();
        //Sunday would be 1, Saturday would be 7, so we walk back up to 6 days.
        if (now.get(Calendar.DAY_OF_WEEK) == 1) {
            //If Sunday already, we go back a full week
            now.add(Calendar.DATE, -7);
        } else {
            now.add(Calendar.DATE, 1 - now.get(Calendar.DAY_OF_WEEK));
        }
        assertEquals("parseCVS last Sunday",
                now, DateFormatter.parseCVS("last Sunday"), 50);

        now = Calendar.getInstance();
        now.add(Calendar.DATE, -7);
        assertEquals("parseCVS last week",
                now, DateFormatter.parseCVS("last week"), 50);
        
        now = Calendar.getInstance();
        //January would be 0, December would be 11, so we walk back up to 11 months
        if (now.get(Calendar.MONTH) == 0) {
            //If January already, we go back a full year
            now.add(Calendar.MONTH, -12);
        } else {
            now.add(Calendar.MONTH, 0 - now.get(Calendar.MONTH));
        }
        assertEquals("parseCVS last January",
                now, DateFormatter.parseCVS("last January"), 50);
    }

    /**
     * Used to check that Calendar objects are close enough
     * delta is in milliseconds
     */
    public static void assertEquals(String message, Calendar cal1, Calendar cal2, long delta) {
        if (Math.abs(cal1.getTime().getTime() - cal2.getTime().getTime()) > delta) {
            throw new AssertionFailedError(
                    message + " expected " + cal1.getTime() + " but got " + cal2.getTime());
        }
    }
}

