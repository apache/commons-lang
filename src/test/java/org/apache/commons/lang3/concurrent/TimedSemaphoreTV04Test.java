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
import java.util.concurrent.TimeUnit;
import org.junit.jupiter.api.Test;
class TimedSemaphoreTV04Test {
 @Test void limitsPermitsWithinPeriodAndTracksAcquisitions() {
  TimedSemaphore semaphore=new TimedSemaphore(5,TimeUnit.SECONDS,2);
  try { assertEquals(2,semaphore.getAvailablePermits()); assertTrue(semaphore.tryAcquire()); assertTrue(semaphore.tryAcquire()); assertFalse(semaphore.tryAcquire()); assertEquals(2,semaphore.getAcquireCount()); assertEquals(2,semaphore.getLimit()); }
  finally { semaphore.shutdown(); }
  assertTrue(semaphore.isShutdown());
 }
 @Test void noLimitAllowsUnrestrictedAcquisition() { TimedSemaphore s=new TimedSemaphore(1,TimeUnit.SECONDS,TimedSemaphore.NO_LIMIT); try { assertTrue(s.tryAcquire()); assertTrue(s.tryAcquire()); } finally {s.shutdown();} }
}
