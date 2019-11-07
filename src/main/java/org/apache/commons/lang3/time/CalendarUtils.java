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

import java.util.Calendar;
import java.util.Objects;

/**
 * Helps use {@link Calendar}s.
 *
 * @since 3.10
 */
public class CalendarUtils {

    /**
     * The singleton instance for {@link Calendar#getInstance()}.
     */
    public static final CalendarUtils INSTANCE = new CalendarUtils(Calendar.getInstance());

    private final Calendar calendar;

    /**
     * Creates an instance for the given Calendar.
     *
     * @param calendar A Calendar.
     */
    public CalendarUtils(final Calendar calendar) {
        super();
        this.calendar = Objects.requireNonNull(calendar, "calendar");
    }

    /**
     * Gets the current day of month.
     *
     * @return the current day of month.
     */
    public int getDayOfMonth() {
        return calendar.get(Calendar.DAY_OF_MONTH);
    }

    /**
     * Gets the current month.
     *
     * @return the current month.
     */
    public int getMonth() {
        return calendar.get(Calendar.MONTH);
    }

    /**
     * Gets the current year.
     *
     * @return the current year.
     */
    public int getYear() {
        return calendar.get(Calendar.YEAR);
    }
}
