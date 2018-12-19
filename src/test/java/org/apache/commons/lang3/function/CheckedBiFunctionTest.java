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

import java.util.function.BiFunction;

public class CheckedBiFunctionTest {

    @DisplayName("an unchecked BiFunction should throw an exception")
    @Test
    void uncheckedBiFunctionExceptionRethrowingTest() {
        //given
        String word = "World";
        int index = 10;
        BiFunction<String, Integer, Character> uncheckedBiFunction = CheckedBiFunction.unchecked(CheckedBiFunctionTest::characterAtIndex);

        //then
        Assertions.assertThrows(Exception.class, () -> uncheckedBiFunction.apply(word, index));
    }

    private static Character characterAtIndex(String word, Integer index) throws Exception{
        if (index > word.length() - 1) {
            throw new Exception();
        }
        return word.toCharArray()[index];
    }
}
