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

package org.apache.commons.lang3.concurrent;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class DaemonThreadFactoryTest {

    private static Runnable NOOP_RUNNABLE = new Runnable() {

        @Override
        public void run() {
            // noop

        }
    };

    @Test
    public void testThreadFactory() {
        final Thread thread = new DaemonThreadFactory().newThread(NOOP_RUNNABLE);
        Assertions.assertTrue(thread.isDaemon());
        final String name = thread.getName();
        Assertions.assertTrue(name.startsWith("DaemonThreadFactory-"), name);
    }

    @Test
    public void testThreadFactoryPrefix() {
        final String expectedName = DaemonThreadFactoryTest.class.getSimpleName();
        final Thread thread = new DaemonThreadFactory(expectedName).newThread(NOOP_RUNNABLE);
        Assertions.assertTrue(thread.isDaemon());
        final String name = thread.getName();
        Assertions.assertTrue(name.startsWith(expectedName + "-"), name);
    }
}
