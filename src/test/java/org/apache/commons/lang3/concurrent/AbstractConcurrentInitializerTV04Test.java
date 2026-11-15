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
class AbstractConcurrentInitializerTV04Test {
 static class Stub extends AbstractConcurrentInitializer<String,ConcurrentException> {
  boolean initialized;
  String invokeInitialize() throws ConcurrentException { String value=initialize(); initialized=true; return value; }
  protected ConcurrentException getTypedException(Exception e){return new ConcurrentException(e);}
  protected boolean isInitialized(){return initialized;}
  @Override
  public String get() throws ConcurrentException { return invokeInitialize(); }
 }
 @Test void defaultInitializerReturnsNullAndTypedStateCanBeTracked() throws Exception { Stub s=new Stub(); assertFalse(s.isInitialized()); assertNull(s.invokeInitialize()); assertTrue(s.isInitialized()); }
}
