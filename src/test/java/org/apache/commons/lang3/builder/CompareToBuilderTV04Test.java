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

package org.apache.commons.lang3.builder;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;
class CompareToBuilderTV04Test {
 static class Value { private final int n; Value(int n){this.n=n;} }
 @Test void comparesPrimitiveAndArrayValuesLexicographically() {
  assertEquals(0,new CompareToBuilder().append(3,3).append(new int[]{1,2},new int[]{1,2}).toComparison());
  assertTrue(new CompareToBuilder().append(2,3).append(99,0).toComparison()<0);
  assertTrue(new CompareToBuilder().append(new String[]{"a","z"},new String[]{"a","b"}).toComparison()>0);
  assertEquals(1,new CompareToBuilder().appendSuper(1).append(0,9).toComparison());
 }
 @Test void reflectionComparisonComparesFieldsAndHandlesNulls() {
  assertEquals(0,CompareToBuilder.reflectionCompare(new Value(2),new Value(2)));
  assertTrue(CompareToBuilder.reflectionCompare(new Value(1),new Value(2))<0);
  assertThrows(NullPointerException.class,()->CompareToBuilder.reflectionCompare(null,new Value(1)));
 }
}
