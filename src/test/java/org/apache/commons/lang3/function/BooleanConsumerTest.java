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

package org.apache.commons.lang3.function;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.concurrent.atomic.AtomicBoolean;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link BooleanConsumer}.
 */
public class BooleanConsumerTest extends AbstractLangTest {

    private BooleanConsumer accept(final BooleanConsumer consumer, final boolean expected) {
        consumer.accept(expected);
        return consumer;
    }

    @Test
    public void testAccept() {
        final AtomicBoolean aBool = new AtomicBoolean();
        accept(aBool::lazySet, true);
        assertTrue(aBool.get());
        accept(aBool::lazySet, false);
        assertFalse(aBool.get());
    }

    @Test
    public void testAndThen() throws Throwable {
        final BooleanConsumer nop = BooleanConsumer.nop();
        nop.andThen(nop);
        // Documented in Javadoc edge-case.
        assertThrows(NullPointerException.class, () -> nop.andThen(null));

        final AtomicBoolean aBool1 = new AtomicBoolean();
        final AtomicBoolean aBool2 = new AtomicBoolean();

        final BooleanConsumer bc = aBool1::lazySet;
        final BooleanConsumer composite = bc.andThen(aBool2::lazySet);

        composite.accept(true);
        assertTrue(aBool1.get());
        assertTrue(aBool2.get());

        composite.accept(false);
        assertFalse(aBool1.get());
        assertFalse(aBool2.get());

        // Check order
        final BooleanConsumer bad = value -> {
            throw new IllegalStateException();
        };
        final BooleanConsumer badComposite = bad.andThen(aBool2::lazySet);

        Assertions.assertThrows(IllegalStateException.class, () -> badComposite.accept(true));
        assertFalse(aBool2.get(), "Second consumer should not be invoked");
    }

}
