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
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.exception.UncheckedException;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link MethodInvokers#asFailableFunction(Method)}.
 */
public class MethodInvokersFailableFunctionTest extends MethodFixtures {

    @Test
    public void testApply0Arg() throws Throwable {
        assertEquals(INSTANCE.getString(), MethodInvokers.asFailableFunction(getMethodForGetString()).apply(INSTANCE));
    }

    @Test
    public void testConstructorForNull() throws SecurityException {
        assertThrows(NullPointerException.class, () -> MethodInvokers.asFailableFunction((Method) null));
    }

    @Test
    public void testBuildVarArg() throws SecurityException, NoSuchMethodException {
        MethodInvokers.asFailableFunction(getMethodForGetStringVarStringArgs());
    }

    @Test
    public void testFindAndInvoke() throws SecurityException {
        // Finding
        final List<FailableFunction<Object, Object, Throwable>> invokers = Stream.of(MethodFixtures.class.getDeclaredMethods())
            .filter(m -> m.isAnnotationPresent(AnnotationTestFixture.class)).map(MethodInvokers::asFailableFunction).collect(Collectors.toList());
        assertEquals(2, invokers.size());
        // ...
        // Invoking
        final Set<Object> set = invokers.stream().map(i -> {
            try {
                return i.apply(MethodFixtures.INSTANCE);
            } catch (final Throwable e) {
                throw new UncheckedException(e);
            }
        }).collect(Collectors.toSet());
        assertEquals(new HashSet<>(Arrays.asList(INSTANCE.getString(), INSTANCE.getString2())), set);
    }

    @Test
    public void testThrowsChecked() throws Exception {
        assertThrows(Exception.class, () -> MethodInvokers.asFailableFunction(getMethodForGetStringThrowsChecked()).apply(INSTANCE));
    }

    @Test
    public void testToString() throws SecurityException, ReflectiveOperationException {
        // Should not blow up and must return _something_
        assertFalse(MethodInvokers.asFailableFunction(getMethodForGetString()).toString().isEmpty());
    }

}
