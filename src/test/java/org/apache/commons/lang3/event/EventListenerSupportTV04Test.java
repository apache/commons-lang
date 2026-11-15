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

package org.apache.commons.lang3.event;
import static org.junit.jupiter.api.Assertions.*;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
class EventListenerSupportTV04Test {
 interface Listener { void changed(String value); }
 @Test void broadcastsEventsInRegistrationOrderAndRemovesListeners() {
  EventListenerSupport<Listener> support=EventListenerSupport.create(Listener.class); List<String> seen=new ArrayList<>();
  Listener first=seen::add, second=v->seen.add(v.toUpperCase()); support.addListener(first); support.addListener(second);
  support.fire().changed("event"); assertEquals(java.util.Arrays.asList("event","EVENT"),seen);
  assertEquals(2,support.getListeners().length); support.removeListener(first); seen.clear(); support.fire().changed("next");
  assertEquals(java.util.Collections.singletonList("NEXT"),seen); assertEquals(1,support.getListeners().length);
 }
 @Test void preventsDuplicatesUnlessExplicitlyAllowed() {
  EventListenerSupport<Listener> support=new EventListenerSupport<>(Listener.class); Listener l=v->{};
  support.addListener(l); support.addListener(l); assertEquals(1,support.getListeners().length);
  support.addListener(l,true); assertEquals(2,support.getListeners().length);
 }
 @Test void rejectsNonInterfaceListenerType() { assertThrows(IllegalArgumentException.class,()->new EventListenerSupport<>(String.class)); }
}
