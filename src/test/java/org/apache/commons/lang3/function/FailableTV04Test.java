/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.commons.lang3.function;

import static org.junit.jupiter.api.Assertions.*;
import java.io.IOException;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.Test;

class FailableTV04Test {
    @Test void applyAndGetDelegateToFailableCallbacks() throws Exception {
        assertEquals(6, Failable.apply((FailableFunction<String, Integer, Exception>) String::length, "abcdef"));
        assertEquals("ok", Failable.get(() -> "ok"));
        assertTrue(Failable.getAsBoolean(() -> true));
        assertEquals(7, Failable.getAsInt(() -> 7));
    }
    @Test void checkedExceptionIsPropagatedFromFailableCallback() {
        IOException expected = new IOException("io");
        IOException actual = assertThrows(IOException.class,
                () -> Failable.apply((FailableFunction<String, String, IOException>) s -> { throw expected; }, "x"));
        assertSame(expected, actual);
    }
    @Test void asConsumerAdaptsFailableConsumer() {
        AtomicReference<String> ref = new AtomicReference<>();
        Failable.asConsumer((FailableConsumer<String, Exception>) ref::set).accept("adapted");
        assertEquals("adapted", ref.get());
    }
    @Test void runExecutesFailableRunnable() throws Exception {
        AtomicReference<Boolean> ran = new AtomicReference<>(false);
        Failable.run(() -> ran.set(true));
        assertTrue(ran.get());
    }
}
