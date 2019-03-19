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

import org.apache.commons.lang3.reflect.ConstructorUtils;

/**
 * Provides convenience methods for creating Throwable objects.
 */
public class ExceptionFactory {

  /**
   * This {@link java.lang.Throwable} implementation uses the Java platform
   * String formatting tool for formatting error messages.
   *
   * @see <a href=
   *      "https://docs.oracle.com/javase/8/docs/api/java/util/Formatter.html#syntax">Java
   *      Formatter Syntax</a>
   *
   * @param clazz The class of the Throwable to create
   * @param format The format string
   * @param args Arguments referenced by the format specifiers in the format
   *          string. If there are more arguments than format specifiers, the
   *          extra arguments are ignored. The number of arguments is variable
   *          and may be zero. The maximum number of arguments is limited by the
   *          maximum dimension of a Java array as defined by The Java™ Virtual
   *          Machine Specification. The behavior on a null argument depends on
   *          the conversion.
   * @return
   */
  public static <T extends Throwable> T create(Class<T> clazz, String format,
      Object... args) {
    final String message = String.format(format, args);
    return buildException(clazz, null, message);
  }

  /**
   * This {@link java.lang.Throwable} implementation uses the Java platform
   * String formatting tool for formatting error messages.
   *
   * @see <a href=
   *      "https://docs.oracle.com/javase/8/docs/api/java/util/Formatter.html#syntax">Java
   *      Formatter Syntax</a>
   *
   * @param clazz the class of the Throwable to create
   * @param cause the cause (which is saved for later retrieval by the
   *          getCause() method). (A null value is permitted, and indicates that
   *          the cause is nonexistent or unknown.)
   * @param format the format string
   * @param args Arguments referenced by the format specifiers in the format
   *          string. If there are more arguments than format specifiers, the
   *          extra arguments are ignored. The number of arguments is variable
   *          and may be zero. The maximum number of arguments is limited by the
   *          maximum dimension of a Java array as defined by The Java™ Virtual
   *          Machine Specification. The behavior on a null argument depends on
   *          the conversion.
   * @return The constructed exception
   */
  public static <T extends Throwable> T createThrowable(Class<T> clazz,
      Throwable cause, String format, Object... args) {
    final String message = String.format(format, args);
    return buildException(clazz, cause, message);
  }

  /**
   * This {@link java.lang.Exception} implementation uses the same message
   * formatting implementation as SLF4J. This message formatting implementation
   * differs from that of the Java platform. This is justified by the fact that
   * SLF4J's implementation performs about 10 times faster but at the cost of
   * being non-standard and less flexible. Escaping the "{}" pair The "{}" pair
   * is called the formatting anchor. It serves to designate the location where
   * arguments need to be substituted within the message pattern. This
   * formatting implementation only cares about the formatting anchor, that is
   * the '{' character immediately followed by '}'. Thus, in case your message
   * contains the '{' or the '}' character, you do not have to do anything
   * special unless the '}' character immediately follows '}'. The SLF4J
   * formatting implementation also supports parameterization in the presence of
   * an exception, assuming the exception is the last parameter.
   * 
   * @see <a href="https://www.slf4j.org/faq.html#logging_performance">SLF4J
   *      parameterized logging</a>
   * @see <a href="https://www.slf4j.org/faq.html#paramException">Handling
   *      Exceptions</a>
   * 
   * @param clazz the class of the Throwable to create
   * @param messagePattern the parameterized format string
   * @param params array of message parameters
   * @return The constructed exception
   */
  public static <T extends Throwable> T createThrowableWithParams(
      Class<T> clazz, String messagePattern, Object... params) {
    final int paramCount =
        ParameterFormatter.countArgumentPlaceholders(messagePattern);
    final int argCount = params.length;
    Throwable cause = null;

    if (paramCount < argCount && params[argCount - 1] instanceof Throwable) {
      cause = (Throwable) params[argCount - 1];
    }

    final String message = ParameterFormatter.format(messagePattern, params);

    return buildException(clazz, cause, message);
  }

  private static <T extends Throwable> T buildException(final Class<T> clazz,
      final Throwable cause, final String message) {
    try {
      final T t = ConstructorUtils.invokeExactConstructor(clazz, message);
      t.initCause(cause);
      return t;
    } catch (ReflectiveOperationException e) {
      throw new IllegalArgumentException(String.format(
          "Caught %s while building exception class %s for message %s", e,
          clazz, message), e);
    }
  }
}
