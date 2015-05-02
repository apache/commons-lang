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
package org.apache.commons.lang3.test;

import java.util.TimeZone;

/**
 * run a test with a different default TimeZone
 */
public abstract class DefaultTimeZone<E extends Throwable> {

    public DefaultTimeZone(TimeZone targetZone) throws E {
        // only one test at a time may change default
        synchronized (getClass()) {
            TimeZone defaultZone = TimeZone.getDefault();
            try {
                TimeZone.setDefault(targetZone);
                test();
            } finally {
                TimeZone.setDefault(defaultZone);
            }
        }
    }

    /**
     * Implement test in this method
     */
    abstract public void test() throws E;
}
