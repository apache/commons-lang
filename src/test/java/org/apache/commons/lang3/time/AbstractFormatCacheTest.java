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

import static org.junit.jupiter.api.Assertions.*;

import java.text.SimpleDateFormat;
import java.util.Locale;
import java.util.TimeZone;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class AbstractFormatCachePartitionTest {

    private static class TestFormatCache extends AbstractFormatCache<SimpleDateFormat> {
        @Override
        protected SimpleDateFormat createInstance(String pattern, TimeZone timeZone, Locale locale) {
            SimpleDateFormat format = new SimpleDateFormat(pattern, locale);
            format.setTimeZone(timeZone);
            return format;
        }
    }

    private TestFormatCache formatCache;

    @BeforeEach
    void setUp() {
        formatCache = new TestFormatCache();
    }

    @Test
    void testValidDateFormat() {
        SimpleDateFormat format = formatCache.getInstance("yyyy-MM-dd", TimeZone.getTimeZone("UTC"), Locale.US);
        assertNotNull(format);
        assertEquals("yyyy-MM-dd", format.toPattern());
    }

    @Test
    void testValidTimeFormat() {
        SimpleDateFormat format = formatCache.getInstance("HH:mm:ss", TimeZone.getTimeZone("UTC"), Locale.US);
        assertNotNull(format);
        assertEquals("HH:mm:ss", format.toPattern());
    }

    @Test
    void testValidDateTimeFormat() {
        SimpleDateFormat format = formatCache.getInstance("yyyy-MM-dd HH:mm:ss", TimeZone.getTimeZone("UTC"),
                Locale.US);
        assertNotNull(format);
        assertEquals("yyyy-MM-dd HH:mm:ss", format.toPattern());
    }

    @Test
    void testInvalidPattern() {
        Exception exception = assertThrows(IllegalArgumentException.class,
                () -> formatCache.getInstance("INVALID-PATTERN", TimeZone.getTimeZone("UTC"), Locale.US));
        assertTrue(exception.getMessage().contains("Illegal pattern"));
    }

    @Test
    void testEmptyPattern() {
        SimpleDateFormat format = formatCache.getInstance("", TimeZone.getTimeZone("UTC"), Locale.US);
        assertNotNull(format);
        assertEquals("", ((SimpleDateFormat) format).toPattern());
    }

    @Test
    void testStandardTimeZone() {
        SimpleDateFormat format = formatCache.getInstance("yyyy-MM-dd", TimeZone.getTimeZone("UTC"), Locale.US);
        assertEquals(TimeZone.getTimeZone("UTC"), format.getTimeZone());
    }

    @Test
    void testNonUtcTimeZone() {
        SimpleDateFormat format = formatCache.getInstance("yyyy-MM-dd", TimeZone.getTimeZone("America/New_York"),
                Locale.US);
        assertEquals(TimeZone.getTimeZone("America/New_York"), format.getTimeZone());
    }

    @Test
    void testUncommonTimeZone() {
        SimpleDateFormat format = formatCache.getInstance("yyyy-MM-dd", TimeZone.getTimeZone("Pacific/Apia"),
                Locale.US);
        assertEquals(TimeZone.getTimeZone("Pacific/Apia"), format.getTimeZone());
    }

    @Test
    void testNullTimeZone() {
        SimpleDateFormat format = formatCache.getInstance("yyyy-MM-dd", null, Locale.US);
        assertNotNull(format.getTimeZone()); // Should default to system time zone
    }

}
