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
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.jupiter.api.Test;
class MemoizerTV04Test {
 @Test void cachesSuccessfulComputationPerKey() throws Exception { AtomicInteger calls=new AtomicInteger(); Computable<Integer,Integer> computable=x->{calls.incrementAndGet();return x*x;}; Memoizer<Integer,Integer> m=new Memoizer<>(computable); assertEquals(9,m.compute(3)); assertEquals(9,m.compute(3)); assertEquals(1,calls.get()); assertEquals(16,m.compute(4)); assertEquals(2,calls.get()); }
 @Test void canRecalculateAfterFailedComputation() throws Exception { AtomicInteger calls=new AtomicInteger(); Computable<String,String> computable=x->{if(calls.incrementAndGet()==1) throw new IllegalStateException("first"); return "ok";}; Memoizer<String,String> m=new Memoizer<>(computable,true); assertThrows(IllegalStateException.class,()->m.compute("k")); assertEquals("ok",m.compute("k")); }
}
