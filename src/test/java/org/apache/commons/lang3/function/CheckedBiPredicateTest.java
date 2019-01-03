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

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.function.BiPredicate;

public class CheckedBiPredicateTest {

    @DisplayName("an unchecked BiPredicate should rethrow an exception")
    @Test
    void uncheckedBiPredicateExceptionRethrowingTest() {
        //given
        BiPredicate<String, Integer> uncheckedBiPredicate = CheckedBiPredicate.unchecked(this::hasLength);

        //then
        Assertions.assertThrows(RuntimeException.class, () -> uncheckedBiPredicate.test(null, 5));
    }

    private boolean hasLength (String word, int size) throws Exception {
        if (word == null) {
            throw new Exception();
        } else {
            return word.length() == size;
        }
    }
}
