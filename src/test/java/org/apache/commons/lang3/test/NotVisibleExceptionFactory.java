/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang3.test;

/**
 * Allows for testing an exception that is not visible to
 * {@link org.apache.commons.lang3.exception.ExceptionUtils}
 */
public class NotVisibleExceptionFactory {

  private NotVisibleExceptionFactory() {}

  /**
   * Create a new Exception whose getCause method returns the
   * provided cause.
   * @param cause the cause of the exception
   * @return a new {@link Exception}
   */
  public static Exception createException(final Throwable cause) {
    return new NotVisibleException(cause);
  }

  private static class NotVisibleException extends Exception {

    private static final long serialVersionUID = 1L; // avoid warning

    private final Throwable cause;

    private NotVisibleException(final Throwable cause) {
      this.cause = cause;
    }

    @Override
    public synchronized Throwable getCause() {
      return cause;
    }
  }
}
