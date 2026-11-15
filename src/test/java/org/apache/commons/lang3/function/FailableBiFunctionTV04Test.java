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
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;
class FailableBiFunctionTV04Test {
 @Test void appliesAndComposesResult() throws Exception {
  FailableBiFunction<Integer,Integer,Integer,RuntimeException> add = Integer::sum;
  assertEquals(9, add.apply(4,5));
  assertEquals("9", add.andThen(String::valueOf).apply(4,5));
  assertNull(FailableBiFunction.<Integer,Integer,Integer,RuntimeException>nop().apply(1,2));
 }
}
