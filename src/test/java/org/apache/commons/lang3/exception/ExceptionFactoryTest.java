/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache license, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the license for the specific language governing permissions and
 * limitations under the license.
 */

package org.apache.commons.lang3.exception;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

public class ExceptionFactoryTest {

  @Test
  public void testCreateDefault() throws Exception {
    Exception exception = ExceptionFactory.create(Exception.class, "Test");
    assertEquals("Test", exception.getMessage());
    assertNull(exception.getCause());
  }

  @Test
  public void testCreateCause() throws Exception {
    Exception cause = new Exception();
    Exception exception =
        ExceptionFactory.createThrowable(Exception.class, cause, "Test");
    assertEquals("Test", exception.getMessage());
    assertEquals(cause, exception.getCause());
  }

  @Test
  public void testMessagePattern() throws Exception {
    Exception exception =
        ExceptionFactory.create(Exception.class, "test %d", 123);
    assertEquals("test 123", exception.getMessage());
    assertNull(exception.getCause());
  }

  @Test
  public void testMessagePatternCause() throws Exception {
    Exception cause = new Exception();
    Exception exception = ExceptionFactory.createThrowable(Exception.class,
        cause, "test %d", 123);
    assertEquals("test 123", exception.getMessage());
    assertEquals(cause, exception.getCause());
  }

  @Test
  public void testCreateWithParamsDefault() throws Exception {
    Exception exception =
        ExceptionFactory.createThrowableWithParams(Exception.class, "Test");
    assertEquals("Test", exception.getMessage());
    assertNull(exception.getCause());
  }

  @Test
  public void testCreateWithParamsCause() throws Exception {
    Exception cause = new Exception();
    Exception exception = ExceptionFactory
        .createThrowableWithParams(Exception.class, "Test", cause);
    assertEquals("Test", exception.getMessage());
    assertEquals(cause, exception.getCause());
  }

  @Test
  public void testMessageWithParamsPattern() throws Exception {
    Exception exception = ExceptionFactory
        .createThrowableWithParams(Exception.class, "test {}", 123);
    assertEquals("test 123", exception.getMessage());
    assertNull(exception.getCause());
  }

  @Test
  public void testMessageWithParamsPatternCause() throws Exception {
    Exception cause = new Exception();
    Exception exception = ExceptionFactory
        .createThrowableWithParams(Exception.class, "test {}", 123, cause);
    assertEquals("test 123", exception.getMessage());
    assertEquals(cause, exception.getCause());
  }

}
