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
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import org.junit.jupiter.api.Test;
class AbstractFutureProxyTV04Test {
 static class Proxy<V> extends AbstractFutureProxy<V> { Proxy(java.util.concurrent.Future<V> f){super(f);} }
 @Test void delegatesFutureOperations() throws Exception {
  CompletableFuture<String> future=CompletableFuture.completedFuture("done"); Proxy<String> proxy=new Proxy<>(future);
  assertSame(future,proxy.getFuture()); assertEquals("done",proxy.get()); assertEquals("done",proxy.get(1,TimeUnit.SECONDS)); assertTrue(proxy.isDone()); assertFalse(proxy.isCancelled());
 }
 @Test void rejectsNullDelegate() { assertThrows(NullPointerException.class,()->new Proxy<String>(null)); }
}
