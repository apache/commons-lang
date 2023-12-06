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
 * Tests {@link MethodInvokers#asFailableBiFunction(Method)}.
 */
public class MethodInvokersFailableBiFunctionTest extends MethodFixtures {

    @Test
    public void testApply1Arg() throws Throwable {
        // Use a local variable typed to the interface to make sure we compile.
        final FailableBiFunction<MethodFixtures, String, String[], Throwable> func = MethodInvokers.asFailableBiFunction(getMethodForGetString1ArgChecked());
        assertEquals(INSTANCE.getString1ArgChecked("A"), func.apply(INSTANCE, "A"));
    }

    @Test
    public void testApply1ArgThrowsChecked() throws NoSuchMethodException, SecurityException {
        // Use a local variable typed to the interface to make sure we compile.
        final FailableBiFunction<MethodFixtures, String, String[], Throwable> func = MethodInvokers
            .asFailableBiFunction(getMethodForGetString1ArgThrowsChecked());
        assertThrows(CustomCheckedException.class, () -> func.apply(INSTANCE, "A"));
    }

    @Test
    public void testApply1ArgThrowsUnchecked() throws NoSuchMethodException, SecurityException {
        // Use a local variable typed to the interface to make sure we compile.
        final FailableBiFunction<MethodFixtures, String, String[], Throwable> func = MethodInvokers
            .asFailableBiFunction(getMethodForGetString1ArgThrowsUnchecked());
        assertThrows(CustomUncheckedException.class, () -> func.apply(INSTANCE, "A"));
    }

    @Test
    public void testConstructorForNull() throws SecurityException {
        assertThrows(NullPointerException.class, () -> MethodInvokers.asFailableBiFunction(null));
    }

    @Test
    public void testToString() throws SecurityException, Throwable {
        // Should not blow up and must return _something_
        assertFalse(MethodInvokers.asFailableBiFunction(getMethodForGetString1ArgChecked()).toString().isEmpty());
    }

}
