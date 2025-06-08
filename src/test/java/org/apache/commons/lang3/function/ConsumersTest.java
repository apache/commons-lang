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

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;
import java.util.stream.Stream;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link Consumers}.
 */
class ConsumersTest extends AbstractLangTest {

    /**
     * Tests {@link Consumers#accept(Consumer, Object)}.
     */
    @Test
    void testAccept() {
        final AtomicBoolean bool = new AtomicBoolean();
        Consumers.accept(bool::set, true);
        assertTrue(bool.get());
        Consumers.accept(null, "");
        Consumers.accept(null, null);
    }

    /**
     * Tests {@link Consumers#nop()}.
     */
    @Test
    void testNop() {
        Stream.of("").forEach(Consumers.nop());
        //
        final Consumer<?> c1 = Consumers.nop();
        c1.accept(null);
        final Consumer<Object> c2 = Consumers.nop();
        c2.accept(null);
        final Consumer<String> c3 = Consumers.nop();
        c3.accept(null);
        //
        Consumers.nop().accept(null);
        Consumers.nop().accept("");
    }
}
