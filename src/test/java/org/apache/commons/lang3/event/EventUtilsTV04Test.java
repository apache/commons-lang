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
import org.junit.jupiter.api.Test;
public class EventUtilsTV04Test {
 public interface PingListener { void ping(String message); void ignored(); }
 public static class Source { PingListener listener; public void addPingListener(PingListener l){listener=l;} }
 public static class Target { String received; int calls; public void onPing(String message){ received=message; calls++; } }
 @Test void bindsOnlyRequestedEventAndInvokesMatchingTargetMethod() {
  Source source=new Source(); Target target=new Target(); EventUtils.bindEventsToMethod(target,"onPing",source,PingListener.class,"ping");
  source.listener.ping("hello"); source.listener.ignored(); assertEquals("hello",target.received); assertEquals(1,target.calls);
 }
 @Test void addEventListenerUsesMatchingAdder() {
  Source source=new Source();
  PingListener listener=new PingListener() {
   @Override public void ping(String message) { }
   @Override public void ignored() { }
  };
  EventUtils.addEventListener(source,PingListener.class,listener); assertSame(listener,source.listener);
 }
 @Test void missingAdderIsReportedAsIllegalArgument() { assertThrows(IllegalArgumentException.class,()->EventUtils.addEventListener(new Object(),PingListener.class,new PingListener() {
  @Override public void ping(String message) { }
  @Override public void ignored() { }
 })); }
}
