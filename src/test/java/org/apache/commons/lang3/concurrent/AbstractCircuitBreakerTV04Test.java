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
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.jupiter.api.Test;
class AbstractCircuitBreakerTV04Test {
 static class StubBreaker extends AbstractCircuitBreaker<Integer> {
  public boolean checkState(){return !isOpen();}
  public boolean incrementAndCheckState(Integer n){if(n>0)open(); return checkState();}
 }
 @Test void openCloseAndListenersObserveStateChanges() {
  StubBreaker breaker=new StubBreaker(); AtomicInteger changes=new AtomicInteger();
  PropertyChangeListener listener=(PropertyChangeEvent e)->changes.incrementAndGet(); breaker.addChangeListener(listener);
  assertTrue(breaker.isClosed()); breaker.open(); assertTrue(breaker.isOpen());
  breaker.close(); assertTrue(breaker.isClosed()); assertEquals(2,changes.get());
  breaker.removeChangeListener(listener); breaker.open(); assertEquals(2,changes.get());
 }
}
