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

 package org.apache.commons.lang3.time;

 import org.junit.jupiter.api.BeforeEach;
 import org.junit.jupiter.api.Test;
 
 import java.util.Calendar;
 import java.util.Date;
 import java.util.TimeZone;
 import java.util.Locale;
 
 import static org.junit.jupiter.api.Assertions.*;
 
 class DatePrinterTest {
 
     private DatePrinter datePrinter;
 
     @BeforeEach
     void setUp() {
         // Initialize DatePrinter using a factory method or a mock
         datePrinter = new FastDatePrinter("yyyy-MM-dd hh:mm:ss a z", TimeZone.getTimeZone("UTC"), Locale.US);
     }
 
     // Test 1: Format valid Calendar object
     @Test
     void testFormatCalendarValid() {
         Calendar calendar = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
         calendar.set(2023, Calendar.JANUARY, 1, 12, 0, 0);
         String formatted = datePrinter.format(calendar);
         assertNotNull(formatted, "Formatted date should not be null");
         assertEquals("2023-01-01 12:00:00 PM UTC", formatted, "Formatted date should match expected pattern");
     }
 
     @Test
     void testFormatCalendarBoundary() {
         // Setting calendar to December 31, 2023, 11:59:59 PM
         Calendar calendar = Calendar.getInstance(TimeZone.getTimeZone("GMT"), Locale.US);
         calendar.set(2023, Calendar.DECEMBER, 31, 23, 59, 59);
         calendar.set(Calendar.MILLISECOND, 0);
 
         // Format the calendar
         String formattedDate = datePrinter.format(calendar);
 
         // Assert that the formatted date matches the expected boundary format
         assertEquals("2023-12-31 11:59:59 PM UTC", formattedDate);
     }
 
     // Test 3: Format invalid Calendar (null object)
     @Test
     void testFormatCalendarInvalid() {
         assertThrows(NullPointerException.class, () -> datePrinter.format((Calendar) null),
                 "Expected NullPointerException for null Calendar");
     }
 
     // Test 4: Format valid Date object
     @Test
     void testFormatDateValid() {
         Date date = new Date(1672531200000L); // January 1, 2023
         String formatted = datePrinter.format(date);
         assertNotNull(formatted, "Formatted date should not be null");
         assertEquals("2023-01-01 12:00:00 AM UTC", formatted, "Formatted date should match expected pattern");
     }
 
     // Test 5: Format boundary Date (epoch time)
     @Test
     void testFormatDateBoundary() {
         Date date = new Date(0L); // Epoch time (January 1, 1970)
         String formatted = datePrinter.format(date);
         assertNotNull(formatted, "Formatted date should not be null");
         assertEquals("1970-01-01 12:00:00 AM UTC", formatted, "Formatted date should match boundary epoch time");
     }
 
     // Test 6: Format invalid Date (null object)
     @Test
     void testFormatDateInvalid() {
         assertThrows(NullPointerException.class, () -> datePrinter.format((Date) null),
                 "Expected NullPointerException for null Date");
     }
 
     // Test 7: Format valid millisecond value
     @Test
     void testFormatMillisValid() {
         long millis = 1672531200000L; // January 1, 2023
         String formatted = datePrinter.format(millis);
         assertNotNull(formatted, "Formatted date should not be null");
         assertEquals("2023-01-01 12:00:00 AM UTC", formatted, "Formatted date should match expected pattern");
     }
 
     // Test 8: Format boundary millisecond values
     @Test
     void testFormatMillisBoundary() {
         long millis = Long.MAX_VALUE;
         String formatted = datePrinter.format(millis);
         assertNotNull(formatted, "Formatted date should not be null");
         assertTrue(formatted.contains("UTC"), "Formatted date should include valid time zone info");
     }
 
     // Test 9: Format invalid millisecond value
     @Test
     void testFormatMillisInvalid() {
         long millis = -1L; // Negative value
         String formatted = datePrinter.format(millis);
         assertNotNull(formatted, "Formatted date should not be null");
         assertFalse(formatted.isEmpty(), "Formatted date should not be empty for invalid millisecond");
     }
 
     // Test 10: Get pattern of DatePrinter
     @Test
     void testGetPattern() {
         assertEquals("yyyy-MM-dd hh:mm:ss a z", datePrinter.getPattern(), "Pattern should match expected format");
     }
 
     // Test 11: Get time zone of DatePrinter
     @Test
     void testGetTimeZone() {
         assertEquals(TimeZone.getTimeZone("UTC"), datePrinter.getTimeZone(), "Time zone should be UTC");
     }
 
     // Test 12: Get locale of DatePrinter
     @Test
     void testGetLocale() {
         assertEquals(Locale.US, datePrinter.getLocale(), "Locale should be US");
     }
 }
