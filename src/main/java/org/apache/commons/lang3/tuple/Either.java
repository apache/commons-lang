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

import java.io.Serializable;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Stream;

/**
 * <p>
 * The {@code Either} data type stores one of two possible values. It is a
 * discriminated union of size two. It is like an Optional, but with an
 * alternative value instead of emptiness.
 * </p>
 *
 * <p>
 * The two alternatives are referred to as left and right. An object of type
 * {@code Either} holds either a left or a right value. These values may have
 * different types.
 * </p>
 *
 * @param <L> the type stored in the left alternative
 * @param <R> the type stored in the right alternative
 */
public abstract class Either<L, R> implements Serializable {

    /**
     * Serialization version.
     */
    private static final long serialVersionUID = 5569490827534264777L;

    /**
     * <p>
     * Reduces both alternatives to the same type, using a transformation for
     * either side.
     * </p>
     * <p>
     * Applies transformation on the left or the right alternative and gets the
     * result.
     * </p>
     *
     * @param <E> The type to reduce to, both functions turn a value of their
     *            respective sides type into this return type
     * @param left the function to call in the left alternative
     * @param right the function to call in the right alternative
     * @return the result of the transformation
     */
    public abstract <E> E reduce(Function<? super L, ? extends E> left,
            Function<? super R, ? extends E> right);

    /**
     * <p>
     * Applies transformation on the left or the right alternative, stores the
     * result in a new {@code Either} object and returns the new {@code Either}
     * object.
     * </p>
     *
     * <p>
     * When called on a left alternative, the result will also be a left
     * alternative. When called on a right alternative, the result will also be
     * a right alternative.
     * </p>
     *
     * @param <E> The type of the left alternative of the result
     * @param <F> The type of the right alternative of the result
     * @param left the function to call in the left alternative
     * @param right the function to call in the right alternative
     * @return a new Either object with a transformed left or right value.
     */
    public abstract <E, F> Either<E, F> map(
            Function<? super L, ? extends E> left,
            Function<? super R, ? extends F> right);

    /**
     * <p>
     * Consumes the Either by calling one of two consumers, depending on which
     * alternative this Either object represents.
     * </p>
     *
     * <p>
     * Terminal operation, in {@code Stream} jargon
     * </p>
     *
     * @param left the consumer of the left alternative
     * @param right the consumer of the right alternative
     */
    public abstract void ifLeftOrElse(Consumer<? super L> left,
            Consumer<? super R> right);

    /**
     * <p>
     * Convenience method when working with streams.
     * </p>
     *
     * <p>
     * {@code Stream.of(Either.left(1), Either.right(false)).flatMap(Either::streamLeft)}
     * will result in a {@code Stream<Integer>} containing a single integer of
     * value one.
     * </p>
     *
     * <p>
     * Modeled after {@code Optional.stream()}
     * </p>
     *
     * @return a {@code Stream} of the left leftValue, or an empty
     *         {@code Stream} if this {@code Either} object represents the right
     *         alternative
     */
    public abstract Stream<L> streamLeft();

    /**
     * <p>
     * Convenience method when working with streams.
     * </p>
     *
     * <p>
     * {@code Stream.of(Either.left(1), Either.right(false)).flatMap(Either::streamRight)}
     * will result in a {@code Stream<Boolean>} containing a single boolean of
     * value false.
     * </p>
     *
     * <p>
     * Modeled after {@code Optional.stream()}
     * </p>
     *
     * @return a Stream of the right leftValue, or an empty {@code Stream} if
     *         this {@code Either} object represents the left alternative
     */
    public abstract Stream<R> streamRight();

    /**
     * <p>
     * Gets the left value wrapped in an {@code Optional} when called on a left
     * alternative, returns empty otherwise.
     * </p>
     *
     * @return the left leftValue wrapped in an Optional if this object
     *         represents the left alternative, empty otherwise
     */
    public abstract Optional<L> getLeft();

    /**
     * <p>
     * Gets the right value wrapped in an {@code Optional} when called on a
     * right alternative, returns empty otherwise.
     * </p>
     *
     * @return the right value wrapped in an Optional if this object represents
     *         the left alternative, empty otherwise
     */
    public abstract Optional<R> getRight();

    /**
     * <p>
     * Gets a new Either object with the sides switched.
     * </p>
     *
     * <p>
     * Left will become right and vice versa.
     * </p>
     *
     * @return a new either object, that is inverted or swapped
     */
    public abstract Either<R, L> swap();

    /**
     * Creates an {@code Either} object that represent the left alternative.
     *
     * @param <L> The type of the left alternative of the result
     * @param <R> The type of the right alternative of the result
     * @param value the leftValue to store in the left alternative
     * @return a new Either object representing the left alternative
     */
    public static <L, R> Either<L, R> left(L value) {
        return new Left<>(value);
    }

    /**
     * Creates an {@code Either} object that represent the right alternative.
     *
     * @param <L> The type of the left alternative of the result
     * @param <R> The type of the right alternative of the result
     * @param value the leftValue to store in the right alternative
     * @return a new {@code Either} object representing the right alternative
     */
    public static <L, R> Either<L, R> right(final R value) {
        return new Right<>(value);
    }

    /**
     * <p>
     * Create an {@code Either} from an {@code Optional} and a default value for
     * the right side.
     * </p>
     *
     * @param <L> The type of the left alternative of the result
     * @param <R> The type of the right alternative of the result
     * @param optional the value held by the left side, if present
     * @param alternative the value held by the right side, if optional is not
     *            present
     * @return a new {@code Either} based on an optional and an alternative.
     */
    public static <L, R> Either<L, R> leftIfPresentOrElse(
            final Optional<? extends L> optional, final R alternative) {
        return optional.map(Either::<L, R>left)
                .orElseGet(() -> new Right<>(alternative));
    }

    /**
     * <p>
     * Create an {@code Either} from an {@code Optional} and a default value for
     * the right side.
     * </p>
     *
     * @param <L> The type of the left alternative of the result
     * @param <R> The type of the right alternative of the result
     * @param optional the value held by the right side, if present
     * @param alternative the value held by the left side, if optional is not
     *            present
     * @return a new {@code Either} based on an optional and an alternative.
     */
    public static <L, R> Either<L, R> rightIfPresentOrElse(//
            final Optional<? extends R> optional, //
            final L alternative) {
        return optional.map(Either::<L, R>right)
                .orElseGet(() -> new Left<>(alternative));
    }

    /**
     * <p>
     * Left alternative, the stored value is of type L.
     * </p>
     *
     * @param <L> the type stored in the left alternative
     * @param <R> the type stored in the right alternative
     */
    private static final class Left<L, R> extends Either<L, R> {

        /**
         * Serialization version.
         */
        private static final long serialVersionUID = -3735547322443371991L;
        /**
         * Stored leftValue of the left alternative.
         */
        private final L leftValue;

        /**
         * <p>
         * Creates a right alternative. Implementing the left side of the @{code
         * Either}, the side that holds a value of type {@code L}.
         * </p>
         *
         * @param value the object stored by this left alternative.
         */
        Left(final L value) {
            leftValue = value;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public <E> E reduce(final Function<? super L, ? extends E> left, //
                final Function<? super R, ? extends E> right) {
            return left.apply(leftValue);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public <E, F> Either<E, F> map(
                final Function<? super L, ? extends E> left, //
                final Function<? super R, ? extends F> right) {
            return Either.<E, F>left(left.apply(leftValue));
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void ifLeftOrElse(final Consumer<? super L> left,
                final Consumer<? super R> right) {
            left.accept(leftValue);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public Optional<L> getLeft() {
            return Optional.of(leftValue);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public Optional<R> getRight() {
            return Optional.empty();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public Either<R, L> swap() {
            return new Right<>(leftValue);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public Stream<L> streamLeft() {
            return Stream.of(leftValue);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public Stream<R> streamRight() {
            return Stream.empty();
        }
    }

    /**
     * <p>
     * Right alternative, the stored value is of type {@code R}.
     * </p>
     *
     * @param <L> the type stored in the left alternative
     * @param <R> the type stored in the right alternative
     */
    private static final class Right<L, R> extends Either<L, R> {

        /**
         * Serialization version.
         */
        private static final long serialVersionUID = -8702051397862504245L;
        /**
         * Stored leftValue of the right alternative.
         */
        private final R rightValue;

        /**
         * <p>
         * Creates a right alternative. Implementing the right side of
         * the @{code Either}, the side that holds a value of type {@code R}.
         * </p>
         *
         * @param value the object stored by this right alternative.
         */
        Right(final R value) {
            rightValue = value;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public <E> E reduce(final Function<? super L, ? extends E> left,
                final Function<? super R, ? extends E> right) {
            return right.apply(rightValue);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public <E, F> Either<E, F> map(
                final Function<? super L, ? extends E> left,
                final Function<? super R, ? extends F> right) {
            return Either.<E, F>right(right.apply(rightValue));
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void ifLeftOrElse(final Consumer<? super L> left,
                final Consumer<? super R> right) {
            right.accept(rightValue);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public Optional<R> getRight() {
            return Optional.of(rightValue);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public Optional<L> getLeft() {
            return Optional.empty();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public Either<R, L> swap() {
            return new Left<>(rightValue);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public Stream<L> streamLeft() {
            return Stream.empty();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public Stream<R> streamRight() {
            return Stream.of(rightValue);
        }
    }
}
