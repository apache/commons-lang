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

package org.apache.commons.lang3;

import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.function.Executable;

/**
 * Specialized APIs to complement {@link org.junit.jupiter.api.Assertions}.
 */
public class LangAssertions {

    /**
     * Asserts that execution of the given {@code executable} throws a {@link IllegalArgumentException}.
     *
     * <p>
     * The assertion passes if the thrown exception type is the same as {@link IllegalArgumentException} or a subtype. To check for the exact thrown type use
     * {@link Assertions#assertThrowsExactly(Class, Executable) assertThrowsExactly}. If no exception is thrown, or if an exception of a different type is
     * thrown, this method fails.
     * </p>
     *
     * @param executable What to test.
     * @return The thrown IllegalArgumentException.
     * @see Assertions#assertThrowsExactly(Class, Executable)
     */
    public static IllegalArgumentException assertIllegalArgumentException(final Executable executable) {
        return assertThrows(IllegalArgumentException.class, executable);
    }

    /**
     * Asserts that execution of the given {@code executable} throws a {@link IllegalArgumentException}.
     *
     * <p>
     * The assertion passes if the thrown exception type is the same as {@link IllegalArgumentException} or a subtype. To check for the exact thrown type use
     * {@link Assertions#assertThrowsExactly(Class, Executable) assertThrowsExactly}. If no exception is thrown, or if an exception of a different type is
     * thrown, this method fails with the given {@code message}.
     * </p>
     *
     * @param executable What to test.
     * @param message    The message for the failure if the executable doesn't throw a IllegalArgumentException.
     * @return The thrown IllegalArgumentException.
     * @see Assertions#assertThrowsExactly(Class, Executable)
     */
    public static IllegalArgumentException assertIllegalArgumentException(final Executable executable, final String message) {
        return assertThrows(IllegalArgumentException.class, executable, message);
    }

    /**
     * Asserts that execution of the given {@code executable} throws a {@link IndexOutOfBoundsException}.
     *
     * <p>
     * The assertion passes if the thrown exception type is the same as {@link IndexOutOfBoundsException} or a subtype. To check for the exact thrown type use
     * {@link Assertions#assertThrowsExactly(Class, Executable) assertThrowsExactly}. If no exception is thrown, or if an exception of a different type is
     * thrown, this method fails.
     * </p>
     *
     * @param executable What to test.
     * @return The thrown IndexOutOfBoundsException.
     * @see Assertions#assertThrowsExactly(Class, Executable)
     */
    public static IndexOutOfBoundsException assertIndexOutOfBoundsException(final Executable executable) {
        return assertThrows(IndexOutOfBoundsException.class, executable);
    }

    /**
     * Asserts that execution of the given {@code executable} throws a {@link IndexOutOfBoundsException}.
     *
     * <p>
     * The assertion passes if the thrown exception type is the same as {@link IndexOutOfBoundsException} or a subtype. To check for the exact thrown type use
     * {@link Assertions#assertThrowsExactly(Class, Executable) assertThrowsExactly}. If no exception is thrown, or if an exception of a different type is
     * thrown, this method fails with the given {@code message}.
     * </p>
     *
     * @param executable What to test.
     * @param message    The message for the failure if the executable doesn't throw a IndexOutOfBoundsException.
     * @return The thrown IndexOutOfBoundsException.
     * @see Assertions#assertThrowsExactly(Class, Executable)
     */
    public static IndexOutOfBoundsException assertIndexOutOfBoundsException(final Executable executable, final String message) {
        return assertThrows(IndexOutOfBoundsException.class, executable, message);
    }

    /**
     * Asserts that execution of the given {@code executable} throws a {@link NullPointerException}.
     *
     * <p>
     * The assertion passes if the thrown exception type is the same as {@link NullPointerException} or a subtype. To check for the exact thrown type use
     * {@link Assertions#assertThrowsExactly(Class, Executable) assertThrowsExactly}. If no exception is thrown, or if an exception of a different type is
     * thrown, this method fails.
     * </p>
     *
     * @param executable What to test.
     * @return The thrown NullPointerException.
     * @see Assertions#assertThrowsExactly(Class, Executable)
     */
    public static NullPointerException assertNullPointerException(final Executable executable) {
        return assertThrows(NullPointerException.class, executable);
    }

    /**
     * Asserts that execution of the given {@code executable} throws a {@link NullPointerException}.
     *
     * <p>
     * The assertion passes if the thrown exception type is the same as {@link NullPointerException} or a subtype. To check for the exact thrown type use
     * {@link Assertions#assertThrowsExactly(Class, Executable) assertThrowsExactly}. If no exception is thrown, or if an exception of a different type is
     * thrown, this method fails with the given {@code message}.
     * </p>
     *
     * @param executable What to test.
     * @param message    The message for the failure if the executable doesn't throw a NullPointerException.
     * @return The thrown NullPointerException.
     * @see Assertions#assertThrowsExactly(Class, Executable)
     */
    public static NullPointerException assertNullPointerException(final Executable executable, final String message) {
        return assertThrows(NullPointerException.class, executable, message);
    }
}
