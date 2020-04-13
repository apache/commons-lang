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
package org.apache.commons.lang3.stream;

import org.apache.commons.lang3.Functions;
import org.apache.commons.lang3.Streams;
import org.apache.commons.lang3.function.FailableIntConsumer;
import org.apache.commons.lang3.function.FailableIntFunction;
import org.apache.commons.lang3.function.FailableIntPredicate;
import org.apache.commons.lang3.function.FailableIntToDoubleFunction;
import org.apache.commons.lang3.function.FailableIntToLongFunction;
import org.apache.commons.lang3.function.FailableIntUnaryOperator;

import java.util.Iterator;
import java.util.OptionalDouble;
import java.util.OptionalInt;
import java.util.Spliterator;
import java.util.function.IntConsumer;
import java.util.function.IntFunction;
import java.util.function.IntPredicate;
import java.util.function.IntToDoubleFunction;
import java.util.function.IntToLongFunction;
import java.util.function.IntUnaryOperator;
import java.util.stream.IntStream;

/**
 * A reduced and simplified version of an {@link IntStream} with
 * failable method signatures.
 *
 * @see IntStream
 * @since 3.10
 */
public class FailableIntStream extends FailableBaseStream<Integer, FailableIntStream> {
    private IntStream intStream;

    public FailableIntStream(IntStream intStream) {
        this.intStream = intStream;
    }

    /**
     * Returns the average of elements in this stream.
     *
     * <p>This is a terminal operation.
     *
     * @return the average of elements in this stream
     */
    public OptionalDouble average() {
        makeTerminated();
        return intStream.average();
    }

    /**
     * Returns a {@code FailableStream} consisting of the elements of this stream, each boxed to an Integer.
     *
     * @return a FailableStream consisting of the elements of this stream, each boxed to an Integer
     */
    public Streams.FailableStream<Integer> boxed() {
        return new Streams.FailableStream<>(intStream.boxed());
    }

    /**
     * Returns a stream consisting of the elements of this stream that match
     * the given predicate.
     *
     * <p>This is an intermediate operation.
     *
     * @param predicate a non-interfering, stateless predicate to
     *                  apply to each element to determine if it
     *                  should be included
     * @return the new stream
     *
     * @see IntStream#filter(IntPredicate)
     */
    public FailableIntStream filter(final FailableIntPredicate<?> predicate) {
        assertNotTerminated();
        intStream = intStream.filter(Functions.asIntPredicate(predicate));
        return this;
    }

    /**
     * Returns an {@link OptionalInt} describing some element of the stream,
     * or an empty {@code OptionalInt} if the stream is empty.
     *
     * <p>This is a short-circuiting terminal operation.
     *
     * <p>The behavior of this operation is explicitly nondeterministic; it is
     * free to select any value in the stream.  This is to allow for maximal
     * performance in parallel operations; the cost is that multiple invocations
     * on the same source may not return the same result. (If a stable result
     * is desired, use {@link #findFirst()} instead.)
     *
     * @return an {@code OptionalInt} describing some value of this stream,
     * or an empty {@code OptionalInt} if the stream is empty
     *
     * @see IntStream#findAny()
     * @see #findFirst()
     */
    public OptionalInt findAny() {
        makeTerminated();
        return intStream.findAny();
    }

    /**
     * Returns an {@link OptionalInt} describing the first value of this
     * stream, or an empty {@code OptionalInt} if the stream is empty. If
     * the stream has no encounter order, then any value may be returned.
     *
     * <p>This is a short-circuiting terminal operation.
     *
     * @return an {@code OptionalInt} describing the first value of this
     * stream, or an empty optional if the stream is empty
     *
     * @see IntStream#findFirst()
     */
    public OptionalInt findFirst() {
        makeTerminated();
        return intStream.findFirst();
    }

    /**
     * Performs an action for each element of this stream.
     *
     * <p>This is a terminal operation.
     *
     * <p>The behavior of this operation is explicitly nondeterministic.
     * For parallel stream pipelines, this operation does <em>not</em>
     * guarantee to respect the encounter order of the stream, as doing so
     * would sacrifice the benefit of parallelism.  For any given element, the
     * action may be performed at whatever time and in whatever thread the
     * library chooses.  If the action accesses shared state, it is
     * responsible for providing the required synchronization.
     *
     * @param action a non-interfering action to perform on the elements
     *
     * @see IntStream#forEach(IntConsumer)
     */
    public void forEach(final FailableIntConsumer<?> action) {
        makeTerminated();
        intStream.forEach(Functions.asIntConsumer(action));
    }

    /**
     * Returns a stream consisting of the results of applying the given
     * function to the elements of this stream.
     *
     * <p>This is an intermediate operation.
     *
     * @param mapper a non-interfering, stateless function to apply to each element
     * @return the new stream
     *
     * @see IntStream#map(IntUnaryOperator)
     */
    public FailableIntStream map(FailableIntUnaryOperator<?> mapper) {
        assertNotTerminated();
        intStream = intStream.map(Functions.asIntUnaryOperator(mapper));
        return this;
    }

    /**
     * Returns an object-valued {@code FailableStream} consisting of the results of
     * applying the given function to the elements of this stream.
     *
     * <p>This is an intermediate operation.
     *
     * @param mapper a non-interfering, stateless function to apply to each element
     * @param <O> the type of the object to be produced
     * @return the new stream
     *
     * @see IntStream#mapToObj(IntFunction)
     */
    public <O> Streams.FailableStream<O> mapToObj(final FailableIntFunction<O, ?> mapper) {
        assertNotTerminated();
        return new Streams.FailableStream<>(intStream.mapToObj(Functions.asIntFunction(mapper)));
    }

    /**
     * Returns a {@code FailableDoubleStream} consisting of the results of applying the
     * given function to the elements of this stream.
     *
     * <p>This is an intermediate operation.
     *
     * @param mapper a non-interfering, stateless function to apply to each element
     * @return the new stream
     *
     * @see IntStream#mapToDouble(IntToDoubleFunction)
     */
    public FailableDoubleStream mapToDouble(final FailableIntToDoubleFunction<?> mapper) {
        assertNotTerminated();
        return new FailableDoubleStream(intStream.mapToDouble(Functions.asIntToDoubleFunction(mapper)));
    }

    /**
     * Returns a {@code FailableLongStream} consisting of the results of applying the
     * given function to the elements of this stream.
     *
     * <p>This is an intermediate operation.
     *
     * @param mapper a non-interfering, stateless function to apply to each element
     * @return the new stream
     *
     * @see IntStream#mapToLong(IntToLongFunction)
     */
    public FailableLongStream mapToLong(final FailableIntToLongFunction<?> mapper) {
        assertNotTerminated();
        return new FailableLongStream(intStream.mapToLong(Functions.asIntToLongFunction(mapper)));
    }

    /**
     * Returns an {@code OptionalInt} describing the minimum value of this
     * stream, or an empty optional if this stream is empty.  This is a special
     * case of a reduction and is equivalent to:
     * <pre>{@code
     *     return reduce(Integer::min);
     * }</pre>
     *
     * <p>This is a terminal operation.
     *
     * @return an {@code OptionalInt} containing the minimum value of this
     * stream, or an empty {@code OptionalInt} if the stream is empty
     *
     * @see IntStream#max()
     */
    public OptionalInt max() {
        makeTerminated();
        return intStream.max();
    }

    /**
     * Returns an {@code OptionalInt} describing the minimum element of this
     * stream, or an empty optional if this stream is empty.  This is a special
     * case of a reduction and is equivalent to:
     * <pre>{@code
     *     return reduce(Integer::min);
     * }</pre>
     *
     * <p>This is a terminal operation.
     *
     * @return an {@code OptionalInt} containing the minimum element of this
     * stream, or an empty {@code OptionalInt} if the stream is empty
     *
     * @see IntStream#min()
     */
    public OptionalInt min() {
        makeTerminated();
        return intStream.min();
    }

    /**
     * Returns the {@link IntStream} which this stream wraps around.
     * @return the {@code IntStream} used by this stream
     */
    public IntStream stream() {
        return intStream;
    }

    /**
     * Returns the sum of elements in this stream.
     *
     * This is a special case of a reduction and is equivalent to:
     * <pre>{@code
     *     return reduce(0, Integer::sum);
     * }</pre>
     *
     * <p>This is a terminal operation.
     *
     * @return the sum of elements in this stream
     */
    public int sum() {
        makeTerminated();
        return intStream.sum();
    }

    /**
     * Returns whether all elements of this stream match the provided predicate.
     * May not evaluate the predicate on all elements if not necessary for
     * determining the result.  If the stream is empty then {@code true} is
     * returned and the predicate is not evaluated.
     *
     * <p>This is a short-circuiting terminal operation.
     *
     * @param predicate a non-interfering, stateless predicate to apply
     *                  to elements of this stream
     * @return {@code true} if either all elements of the stream match the
     * provided predicate or the stream is empty, otherwise {@code false}
     */
    public boolean allMatch(final FailableIntPredicate<?> predicate) {
        makeTerminated();
        return intStream.allMatch(Functions.asIntPredicate(predicate));
    }

    /**
     * Returns whether any elements of this stream match the provided
     * predicate.  May not evaluate the predicate on all elements if not
     * necessary for determining the result.  If the stream is empty then
     * {@code false} is returned and the predicate is not evaluated.
     *
     * <p>This is a short-circuiting terminal operation.
     *
     * @param predicate a non-interfering, stateless predicate to apply
     *                  to elements of this stream
     * @return {@code true} if any elements of the stream match the provided
     * predicate, otherwise {@code false}
     */
    public boolean anyMatch(final FailableIntPredicate<?> predicate) {
        makeTerminated();
        return intStream.anyMatch(Functions.asIntPredicate(predicate));
    }

    /**
     * Returns whether no elements of this stream match the provided predicate.
     * May not evaluate the predicate on all elements if not necessary for
     * determining the result.  If the stream is empty then {@code true} is
     * returned and the predicate is not evaluated.
     *
     * <p>This is a short-circuiting terminal operation.
     *
     * @param predicate a non-interfering, stateless predicate to apply to
     *                  elements of this stream
     * @return {@code true} if either no elements of the stream match the
     * provided predicate or the stream is empty, otherwise {@code false}
     */
    public boolean noneMatch(final FailableIntPredicate<?> predicate) {
        makeTerminated();
        return intStream.noneMatch(Functions.asIntPredicate(predicate));
    }

    @Override
    public long count() {
        makeTerminated();
        return intStream.count();
    }

    @Override
    public Iterator<Integer> iterator() {
        makeTerminated();
        return intStream.iterator();
    }

    @Override
    public Spliterator<Integer> spliterator() {
        makeTerminated();
        return intStream.spliterator();
    }

    @Override
    public FailableIntStream distinct() {
        assertNotTerminated();
        intStream = intStream.distinct();
        return this;
    }

    @Override
    public FailableIntStream sequential() {
        assertNotTerminated();
        intStream = intStream.sequential();
        return this;
    }

    @Override
    public FailableIntStream parallel() {
        assertNotTerminated();
        intStream = intStream.parallel();
        return this;
    }

    @Override
    public FailableIntStream sorted() {
        assertNotTerminated();
        intStream = intStream.sorted();
        return this;
    }

    @Override
    public FailableIntStream unordered() {
        assertNotTerminated();
        intStream = intStream.unordered();
        return this;
    }

    @Override
    public FailableIntStream limit(long maxSize) {
        assertNotTerminated();
        intStream = intStream.limit(maxSize);
        return this;
    }

    @Override
    public FailableIntStream skip(long n) {
        assertNotTerminated();
        intStream = intStream.skip(n);
        return this;
    }
}
