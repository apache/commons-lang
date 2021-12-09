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

import org.apache.commons.lang3.exception.CustomCheckedException;
import org.apache.commons.lang3.exception.CustomUncheckedException;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link MethodInvokers#asFailableBiConsumer(Method)}.
 */
public class MethodInvokersFailableBiConsumerTest extends MethodFixtures {

    @Test
    public void testApply1Arg() throws Throwable {
        MethodInvokers.asFailableBiConsumer(getMethodForSetString1Arg()).accept(INSTANCE, "A");
        assertEquals("A", INSTANCE.getValue1());
    }

    @Test
    public void testApply1ArgThrowsChecked() throws Exception {
        assertThrows(CustomCheckedException.class, () -> MethodInvokers.asFailableBiConsumer(getMethodForSetString1ArgThrowsChecked()).accept(INSTANCE, "A"));
    }

    @Test
    public void testApply1ArgThrowsUnchecked() throws Exception {
        assertThrows(CustomUncheckedException.class, () -> MethodInvokers.asFailableBiConsumer(getMethodForSetString1ArgThrowsUnchecked()).accept(INSTANCE, "A"));
    }

    @Test
    public void testConstructorForNull() throws Exception {
        assertThrows(NullPointerException.class, () -> MethodInvokers.asFailableBiConsumer(null));
    }

    @Test
    public void testToString() throws SecurityException, ReflectiveOperationException {
        // Should not blow up and must return _something_
        assertFalse(MethodInvokers.asFailableBiConsumer(getMethodForSetString1Arg()).toString().isEmpty());
    }

}
