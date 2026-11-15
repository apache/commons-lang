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

package org.apache.commons.lang3.concurrent;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;
class MultiBackgroundInitializerTV04Test {
 @Test void collectsNamedChildResults() throws Exception {
  MultiBackgroundInitializer multi=new MultiBackgroundInitializer(); multi.addInitializer("one",new CallableBackgroundInitializer<>(()->1)); multi.addInitializer("two",new CallableBackgroundInitializer<>(()->"two"));
  assertTrue(multi.start()); MultiBackgroundInitializer.MultiBackgroundInitializerResults r=multi.get();
  assertTrue(r.isSuccessful()); assertEquals(1,r.getResultObject("one")); assertEquals("two",r.getResultObject("two")); assertEquals(2,r.initializerNames().size());
 }
 @Test void rejectsAddingChildAfterStart() { MultiBackgroundInitializer multi=new MultiBackgroundInitializer(); multi.start(); assertThrows(IllegalStateException.class,()->multi.addInitializer("late",new CallableBackgroundInitializer<>(()->1))); }
}
