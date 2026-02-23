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

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * TestCase for SimpleStopWatch.
 */
class SimpleStopWatchTest {

    @Test
    void testSimpleStopWatch() throws InterruptedException {
        SimpleStopWatch simpleStopWatch = SimpleStopWatch.started();

        {
            Thread.sleep(500);
            long time500 = simpleStopWatch.time();
            long lap500 = simpleStopWatch.lap();
            assertAll(
                    () -> assertTrue(time500 > 450, String.valueOf(time500)),
                    () -> assertTrue(time500 < 550, String.valueOf(time500)),
                    () -> assertTrue(lap500 > 450, String.valueOf(lap500)),
                    () -> assertTrue(lap500 < 550, String.valueOf(lap500)));
        }
        {
            Thread.sleep(300);
            long time800 = simpleStopWatch.time();
            long lap300 = simpleStopWatch.lap();

            assertAll(
                    () -> assertTrue(time800 > 750, String.valueOf(time800)),
                    () -> assertTrue(time800 < 900, String.valueOf(time800)),
                    () -> assertTrue(lap300 > 250,  String.valueOf(lap300)),
                    () -> assertTrue(lap300 < 350,  String.valueOf(lap300)));
        }
    }
}
