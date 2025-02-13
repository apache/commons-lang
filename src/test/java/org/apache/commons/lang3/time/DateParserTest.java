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
 
 import java.text.ParseException;
 import java.text.ParsePosition;
 import java.util.Calendar;
 import java.util.Date;
 import java.util.Locale;
 import java.util.TimeZone;
 
 import static org.junit.jupiter.api.Assertions.*;
 
 class DateParserTest {
 
     private DateParser dateParser;
 
     @BeforeEach
     void setUp() {
         dateParser = new FastDateParser("yyyy-MM-dd", TimeZone.getTimeZone("UTC"), Locale.US);
     }
 
     @Test
     void testValidDateParsing() throws ParseException {
         Date date = dateParser.parse("2024-02-05");
         assertNotNull(date, "Parsed date should not be null");
     }
 
     @Test
     void testInvalidDateParsing() {
         assertThrows(ParseException.class, () -> dateParser.parse("invalid-date"));
     }
 
     @Test
     void testParseWithParsePositionValid() {
         ParsePosition pos = new ParsePosition(0);
         Date date = dateParser.parse("2024-02-05", pos);
         assertNotNull(date, "Parsed date should not be null");
         assertEquals(10, pos.getIndex(), "Parse position index should match length of valid date");
     }
 
     @Test
     void testParseWithParsePositionInvalid() {
         ParsePosition pos = new ParsePosition(0);
         Date date = dateParser.parse("invalid", pos);
         assertNull(date, "Parsing should return null for invalid date");
         assertTrue(pos.getErrorIndex() >= 0, "Error index should be set");
     }
 
     @Test
     void testParseWithCalendar() {
         Calendar calendar = Calendar.getInstance();
         ParsePosition pos = new ParsePosition(0);
         boolean success = dateParser.parse("2024-02-05", pos, calendar);
         assertTrue(success, "Parsing should be successful");
         assertEquals(2024, calendar.get(Calendar.YEAR));
         assertEquals(Calendar.FEBRUARY, calendar.get(Calendar.MONTH));
         assertEquals(5, calendar.get(Calendar.DAY_OF_MONTH));
     }
 
     @Test
     void testGetPattern() {
         assertEquals("yyyy-MM-dd", dateParser.getPattern(), "Pattern should match");
     }
 
     @Test
     void testGetTimeZone() {
         assertEquals(TimeZone.getTimeZone("UTC"), dateParser.getTimeZone(), "Time zone should match UTC");
     }
 
     @Test
     void testGetLocale() {
         assertEquals(Locale.US, dateParser.getLocale(), "Locale should match US");
     }
 }