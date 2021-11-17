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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.lang.reflect.Method;
import java.util.function.BiFunction;

import org.apache.commons.lang3.exception.CustomUncheckedException;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link MethodInvokers#asBiFunction(Method)}.
 */
public class MethodInvokersBiFunctionTest extends MethodFixtures {

    @Test
    public void testApply1Arg() throws NoSuchMethodException, SecurityException {
        final BiFunction<MethodFixtures, String, String> func = MethodInvokers.asBiFunction(getMethodForGetString1Arg());
        assertEquals(INSTANCE.getString1Arg("A"), func.apply(INSTANCE, "A"));
    }

    @Test
    public void testApply1ArgThrowsUnchecked() throws NoSuchMethodException, SecurityException {
        final BiFunction<MethodFixtures, String, String> func = MethodInvokers.asBiFunction(getMethodForGetString1ArgThrowsUnchecked());
        assertThrows(CustomUncheckedException.class, () -> func.apply(INSTANCE, "A"));
    }

    @Test
    public void testConstructorForNull() throws SecurityException {
        assertThrows(NullPointerException.class, () -> MethodInvokers.asBiFunction(null));
    }

    @Test
    public void testFullExample() throws SecurityException, ReflectiveOperationException {
        final Method method = String.class.getMethod("charAt", int.class);
        final BiFunction<String, Integer, Character> function = MethodInvokers.asBiFunction(method);
        assertEquals('C', function.apply("ABC", 2));
    }

    @Test
    public void testToString() throws SecurityException, ReflectiveOperationException {
        // Should not blow up and must return _something_
        assertFalse(MethodInvokers.asBiFunction(getMethodForGetString1Arg()).toString().isEmpty());
    }

}
