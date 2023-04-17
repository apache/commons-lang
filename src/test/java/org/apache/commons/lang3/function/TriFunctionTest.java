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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.math.BigInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link TriFunction}.
 */
public class TriFunctionTest extends AbstractLangTest {

    @Test
    public void testAccept() throws Throwable {
        final AtomicReference<Character> ref1 = new AtomicReference<>();
        final AtomicReference<Short> ref2 = new AtomicReference<>();
        final AtomicReference<String> ref3 = new AtomicReference<>();
        final TriFunction<AtomicReference<Character>, AtomicReference<Short>, AtomicReference<String>, String> tri = (t, u, v) -> {
            ref1.set(Character.valueOf('a'));
            ref2.set(Short.valueOf((short) 1));
            ref3.set("z");
            return "ABC";
        };
        assertEquals("ABC", tri.apply(ref1, ref2, ref3));
        assertEquals(Character.valueOf('a'), ref1.get());
        assertEquals(Short.valueOf((short) 1), ref2.get());
        assertEquals("z", ref3.get());
    }

    @Test
    public void testAndThen() throws Throwable {
        final AtomicReference<Character> ref1 = new AtomicReference<>();
        final AtomicReference<Short> ref2 = new AtomicReference<>();
        final AtomicReference<String> ref3 = new AtomicReference<>();
        final TriFunction<AtomicReference<Character>, AtomicReference<Short>, AtomicReference<String>, String> tri = (t, u, v) -> {
            ref1.set(Character.valueOf('a'));
            ref2.set(Short.valueOf((short) 1));
            ref3.set("z");
            return "9";
        };
        final Function<String, BigInteger> after = t -> {
            ref1.set(Character.valueOf('b'));
            ref2.set(Short.valueOf((short) 2));
            ref3.set("zz");
            return BigInteger.valueOf(Long.parseLong(t)).add(BigInteger.ONE);
        };
        assertEquals(BigInteger.TEN, tri.andThen(after).apply(ref1, ref2, ref3));
        assertEquals(Character.valueOf('b'), ref1.get());
        assertEquals(Short.valueOf((short) 2), ref2.get());
        assertEquals("zz", ref3.get());
    }

    @Test
    public void testTriFunctionNop() {
        assertNull(TriFunction.nop().apply(1, 2, 3), "Expect NOP to return null");
    }

}
