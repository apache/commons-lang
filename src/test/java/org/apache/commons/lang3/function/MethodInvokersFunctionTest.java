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
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.exception.CustomUncheckedException;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link MethodInvokers#asFunction(Method)}.
 */
public class MethodInvokersFunctionTest extends MethodFixtures {

    @Test
    public void testApply0Arg() throws NoSuchMethodException, SecurityException {
        final Function<MethodFixtures, String> func = MethodInvokers.asFunction(getMethodForGetString());
        assertEquals(INSTANCE.getString(), func.apply(INSTANCE));
    }

    @Test
    public void testApply0ArgThrowsUnchecked() throws NoSuchMethodException, SecurityException {
        final Function<MethodFixtures, String> func = MethodInvokers.asFunction(getMethodForGetStringThrowsUnchecked());
        assertThrows(CustomUncheckedException.class, () -> func.apply(INSTANCE));
    }

    @Test
    public void testBuildVarArg() throws SecurityException, NoSuchMethodException {
        MethodInvokers.asFunction(getMethodForGetStringVarStringArgs());
    }

    @Test
    public void testConstructorForNull() throws SecurityException {
        assertThrows(NullPointerException.class, () -> MethodInvokers.asFunction(null));
    }

    @Test
    public void testFindAndInvoke() throws SecurityException {
        // Finding
        final List<Function<Object, Object>> invokers = Stream.of(MethodFixtures.class.getDeclaredMethods())
            .filter(m -> m.isAnnotationPresent(AnnotationTestFixture.class)).map(MethodInvokers::asFunction).collect(Collectors.toList());
        assertEquals(2, invokers.size());
        // ...
        // Invoking
        final Set<Object> set1 = invokers.stream().map(i -> i.apply(MethodFixtures.INSTANCE)).collect(Collectors.toSet());
        assertEquals(new HashSet<>(Arrays.asList(INSTANCE.getString(), INSTANCE.getString2())), set1);
        final Set<Object> set2 = Stream.of(INSTANCE).map(invokers.get(0)).collect(Collectors.toSet());
        final Set<Object> set3 = Stream.of(INSTANCE).map(invokers.get(1)).collect(Collectors.toSet());
        set2.addAll(set3);
        assertEquals(new HashSet<>(Arrays.asList(INSTANCE.getString(), INSTANCE.getString2())), set2);
    }

    @Test
    public void testFullExample() throws SecurityException, ReflectiveOperationException {
        final Method method = String.class.getMethod("length");
        final Function<String, Integer> function = MethodInvokers.asFunction(method);
        assertEquals(3, function.apply("ABC"));
    }

    @Test
    public void testMapComputeIfAbsent() throws NoSuchMethodException, SecurityException {
        final Map<MethodFixtures, String> map = new HashMap<>();
        map.computeIfAbsent(INSTANCE, MethodInvokers.asFunction(getMethodForGetString()));
        assertEquals(INSTANCE.getString(), map.get(INSTANCE));
    }

    @Test
    public void testToString() throws SecurityException, ReflectiveOperationException {
        // Should not blow up and must return _something_
        assertFalse(MethodInvokers.asFunction(getMethodForGetString()).toString().isEmpty());
    }

}
