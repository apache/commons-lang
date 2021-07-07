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
package org.apache.commons.lang3.tuple;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;

/**
 * <p>
 * White box unit testing of Either.
 * </p>
 */
public class EitherTest {

    /**
     * <p>
     * Gets a function to add something.
     * </p>
     *
     * @param i the value to add to the parameter of the function
     * @return a function which adds i to its parameter
     */
    public static Function<Integer, Integer> add(final int i) {
        return t -> t + i;
    }

    @Test
    public void testReduce() {
        // Given a left alternative with value one, and a right alternative of
        // value
        // two.
        final Either<Integer, Integer> left = Either.left(1);
        final Either<Integer, Integer> right = Either.right(2);

        // When we reduce by calling add one on the left size and add two on the
        // right
        final int resultLeft = left.reduce(add(1), add(2));
        final int resultRight = right.reduce(add(1), add(2));

        // Then we expect
        assertEquals(2, resultLeft);
        assertEquals(4, resultRight);
    }

    /**
     * <p>
     * Gets a function which returns a constant value regardless of its
     * parameter.
     * </p>
     *
     * @param <E> the type of the argument the function ignores
     * @param <F> the type of the constant the function returns
     * @param x the constant to be returned by the function
     * @return the function to return x
     */
    public static <E, F> Function<E, F> always(final F x) {
        return t -> x;
    }

    /**
     * <p>
     * Gets an identity function.
     * </p>
     *
     * @param <E> the type of the parameter and result of the identity function
     * @return identity function
     */
    public static <E> Function<E, E> id() {
        return t -> t;
    }

    @Test
    public void testReduceHeterogeneous() {
        // Given a left alternative with value one, and a right alternative of
        // value
        // two.
        final Either<Integer, Boolean> left = Either.left(1);
        final Either<Integer, Boolean> right = Either.right(false);

        // When we reduce by calling add one on the left size and add two on the
        // right
        final boolean resultLeft =
                left.reduce(always(true), EitherTest.<Boolean>id());
        final boolean resultRight =
                right.reduce(always(true), EitherTest.<Boolean>id());

        // Then we expect
        assertEquals(true, resultLeft);
        assertEquals(false, resultRight);
    }

    @Test
    public void testMap() {
        // Given a left alternative with value one, and a right alternative of
        // value
        // false.
        final Either<Integer, Boolean> left = Either.left(1);
        final Either<Integer, Boolean> right = Either.right(false);

        // When we call map with
        final Either<Integer, Boolean> leftResult =
                left.map(always(2), always(true));
        final Either<Integer, Boolean> rightResult =
                right.map(always(2), always(true));

        // Then we expect
        assertEquals(2, (int) leftResult.getLeft().get());
        assertEquals(Optional.empty(), leftResult.getRight());

        assertEquals(true, (boolean) rightResult.getRight().get());
        assertEquals(Optional.empty(), rightResult.getLeft());
    }

    /**
     * @param <E> the type to consume and check.
     * @param log to log the execution.
     * @param check value to assert equals with the argument of accept.
     * @return a consumer to log its execution and check its parameter.
     */
    private static <E> Consumer<E> logAndCheckConsumer(final AtomicBoolean log,
            final E check) {
        return t -> {
            assertEquals(t, check);
            log.set(true);
        };
    }

    @Test
    public void testIfLeftOrElse() {
        // Given a left alternative with value one, and a right alternative of
        // value
        // false.
        final Either<Integer, Boolean> left = Either.left(1);
        final Either<Integer, Boolean> right = Either.right(false);

        // Given four atomic booleans to log execution
        final AtomicBoolean leftExecutedLeft = new AtomicBoolean(false);
        final AtomicBoolean leftExecutedRight = new AtomicBoolean(false);
        final AtomicBoolean rightExecutedLeft = new AtomicBoolean(false);
        final AtomicBoolean rightExecutedRight = new AtomicBoolean(false);

        // Given a function to log its execution to a boolean and assert equals
        // on its argument.
        // see definition of logAndCheckConsumer

        // When we call ifLeftOrElse using a consumer which asserts equality of
        // reference value and
        // logs its execution to an atomic boolean.
        left.ifLeftOrElse(logAndCheckConsumer(leftExecutedLeft, 1),
                logAndCheckConsumer(leftExecutedRight, null));
        right.ifLeftOrElse(logAndCheckConsumer(rightExecutedLeft, null),
                logAndCheckConsumer(rightExecutedRight, false));

        // Then we expect
        assertEquals(true, leftExecutedLeft.get());
        assertEquals(false, leftExecutedRight.get());
        assertEquals(false, rightExecutedLeft.get());
        assertEquals(true, rightExecutedRight.get());
    }

    @Test
    public void testGettersAndSwap() {
        // Given a left alternative with value one, and a right alternative of
        // value
        // two.
        final Either<Integer, Integer> left = Either.left(1);
        final Either<Integer, Integer> right = Either.right(2);

        // Given their swapped values
        final Either<Integer, Integer> swappedLeft = left.swap();
        final Either<Integer, Integer> swappedRight = right.swap();

        // Then we expect
        assertEquals(1, (int) left.getLeft().get());
        assertEquals(Optional.empty(), left.getRight());

        assertEquals(2, (int) right.getRight().get());
        assertEquals(Optional.empty(), right.getLeft());

        assertEquals(1, (int) swappedLeft.getRight().get());
        assertEquals(Optional.empty(), swappedLeft.getLeft());

        assertEquals(2, (int) swappedRight.getLeft().get());
        assertEquals(Optional.empty(), swappedRight.getRight());
    }

    @Test
    public void testStream() {
        final Either<Integer, Boolean> left = Either.left(1);
        final Either<Integer, Boolean> right = Either.right(false);

        // Then
        final List<Integer> streamedLeft =
                left.streamLeft().collect(Collectors.<Integer>toList());
        assertEquals(1, streamedLeft.size());
        assertEquals(1, (int) streamedLeft.get(0));
        assertEquals(0, left.streamRight().collect(Collectors.toList()).size());

        final List<Boolean> streamedRight =
                right.streamRight().collect(Collectors.<Boolean>toList());
        assertEquals(1, streamedRight.size());
        assertEquals(false, (boolean) streamedRight.get(0));
        assertEquals(0, right.streamLeft().collect(Collectors.toList()).size());
    }

    @Test
    public void testLeftIfPresentOrElse() {
        // Given a left and a right made using an optional value for the left
        // side and a default value
        // for the right side.
        final Optional<Integer> leftOptionalValue = Optional.of(1);
        final Either<Integer, Boolean> left =
                Either.leftIfPresentOrElse(leftOptionalValue, false);
        final Either<Integer, Boolean> right;
        right = Either.leftIfPresentOrElse(Optional.<Integer>empty(), false);

        // Then we expect
        assertEquals(1, (int) left.getLeft().get());
        assertEquals(Optional.empty(), left.getRight());

        assertEquals(Optional.empty(), right.getLeft());
        assertEquals(false, (boolean) right.getRight().get());
    }

    @Test
    public void testRightIfPresentOrElse() {
        // Given a left and a right made using an optional value for the left
        // side and a default value
        // for the right side.
        final Optional<Boolean> rightOptionalValue = Optional.of(false);
        final Either<Integer, Boolean> left;
        left = Either.rightIfPresentOrElse(Optional.<Boolean>empty(), 1);
        final Either<Integer, Boolean> right;
        right = Either.rightIfPresentOrElse(rightOptionalValue, 1);

        // Then we expect
        assertEquals(1, (int) left.getLeft().get());
        assertEquals(Optional.empty(), left.getRight());

        assertEquals(Optional.empty(), right.getLeft());
        assertEquals(false, (boolean) right.getRight().get());
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testSerialization() throws IOException, ClassNotFoundException {
        // Given a left alternative with value one, and a right alternative of
        // value
        // false.
        final Either<Integer, Boolean> left = Either.left(1);
        final Either<Integer, Boolean> right = Either.right(false);

        // Given output streams
        final ByteArrayOutputStream baosLeft = new ByteArrayOutputStream();
        final ByteArrayOutputStream baosRight = new ByteArrayOutputStream();
        final ObjectOutputStream outLeft = new ObjectOutputStream(baosLeft);
        final ObjectOutputStream outRight = new ObjectOutputStream(baosRight);

        // When we serialize
        outLeft.writeObject(left);
        outRight.writeObject(right);

        // And deserialize
        final Either<Integer, Boolean> deserializedLeft;
        deserializedLeft = (Either<Integer,
                Boolean>) new ObjectInputStream(
                        new ByteArrayInputStream(baosLeft.toByteArray()))
                                .readObject();
        final Either<Integer, Boolean> deserializedRight;
        deserializedRight = (Either<Integer,
                Boolean>) new ObjectInputStream(
                        new ByteArrayInputStream(baosRight.toByteArray()))
                                .readObject();

        // Then we expect the deserialized object to behave as the original.
        assertEquals(1, (int) deserializedLeft.getLeft().get());
        assertEquals(Optional.empty(), deserializedLeft.getRight());

        assertEquals(false, (boolean) deserializedRight.getRight().get());
        assertEquals(Optional.empty(), deserializedRight.getLeft());
    }
}
