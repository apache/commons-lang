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
import java.lang.reflect.Method;
import java.util.function.BiFunction;
import java.util.function.Function;
import org.junit.jupiter.api.Test;
class MethodInvokersTV04Test {
 @Test void wrapsInstanceMethodsAsFunctionAndBiFunction() throws Exception {
  Method length=String.class.getMethod("length"); Function<String,Integer> f=MethodInvokers.asFunction(length); assertEquals(3,f.apply("abc"));
  Method charAt=String.class.getMethod("charAt",int.class); BiFunction<String,Integer,Character> b=MethodInvokers.asBiFunction(charAt); assertEquals(Character.valueOf('c'),b.apply("abc",2));
 }
 @Test void rejectsNullMethodOrInterfaceType() {
  assertThrows(NullPointerException.class,()->MethodInvokers.asFunction(null));
  assertThrows(NullPointerException.class,()->MethodInvokers.asInterfaceInstance(null,String.class.getMethods()[0]));
 }
}