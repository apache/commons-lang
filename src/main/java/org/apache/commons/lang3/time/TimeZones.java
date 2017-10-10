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

/**
 * Helps to deal with {@link java.util.TimeZone}s.
 *
 * @since 3.7
 */
public class TimeZones {

    // do not instantiate
    private TimeZones() {
    }

    /**
     * A public version of {@link java.util.TimeZone}'s package private {@code GMT_ID} field.
     */
    public static final String GMT_ID = "GMT";
}
