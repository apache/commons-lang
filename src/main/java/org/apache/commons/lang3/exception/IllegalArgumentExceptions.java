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

/**
 * Shorthands creating {@link IllegalArgumentException} instances using formatted strings.
 *
 * <p>
 * Originally from Apache Commons Text.
 * </p>
 * 
 * @since 3.10
 */
public final class IllegalArgumentExceptions {

    /**
     * Creates an {@link IllegalArgumentException} with a message formated with {@link String#format(String,Object...)}.
     *
     * @param format See {@link String#format(String,Object...)}
     * @param args See {@link String#format(String,Object...)}
     * @return an {@link IllegalArgumentException} with a message formated with {@link String#format(String,Object...)}
     */
    public static IllegalArgumentException format(final String format, final Object... args) {
        return new IllegalArgumentException(String.format(format, args));
    }

    /**
     * Creates an {@link IllegalArgumentException} with a message formated with {@link String#format(String,Object...)}.
     *
     * @param t the throwable cause
     * @param format See {@link String#format(String,Object...)}
     * @param args See {@link String#format(String,Object...)}
     * @return an {@link IllegalArgumentException} with a message formated with {@link String#format(String,Object...)}
     */
    public static IllegalArgumentException format(final Throwable t, final String format, final Object... args) {
        return new IllegalArgumentException(String.format(format, args), t);
    }

    /**
     * No need to build instances.
     */
    private IllegalArgumentExceptions() {
        // empty
    }
}
