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
import java.io.IOException;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import org.junit.jupiter.api.Test;
class ConcurrentUtilsTV04Test {
 @Test void constantFutureIsCompletedAndReturnsValue() throws Exception { Future<String> f=ConcurrentUtils.constantFuture("fixed"); assertTrue(f.isDone()); assertFalse(f.isCancelled()); assertEquals("fixed",f.get()); }
 @Test void createIfAbsentUsesExistingValueOrInitializer() throws Exception {
  ConcurrentHashMap<String,String> map=new ConcurrentHashMap<>(); assertEquals("new",ConcurrentUtils.createIfAbsent(map,"k",()->"new"));
  assertEquals("new",ConcurrentUtils.createIfAbsent(map,"k",()->"other")); assertEquals("new",map.get("k"));
  assertNull(ConcurrentUtils.createIfAbsent(null,"x",()->"x"));
 }
 @Test void putIfAbsentReturnsStoredValue() { ConcurrentHashMap<String,String> map=new ConcurrentHashMap<>(); assertEquals("a",ConcurrentUtils.putIfAbsent(map,"k","a")); assertEquals("a",ConcurrentUtils.putIfAbsent(map,"k","b")); }
 @Test void extractsCheckedCauseFromExecutionException() { IOException cause=new IOException("io"); ConcurrentException e=ConcurrentUtils.extractCause(new ExecutionException(cause)); assertSame(cause,e.getCause()); assertNull(ConcurrentUtils.extractCause(null)); }
 @Test void initializeDelegatesToInitializer() throws Exception { assertEquals("ok",ConcurrentUtils.initialize(()->"ok")); }
}
