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
import org.apache.commons.lang3.function.FailableLongConsumer;
import org.apache.commons.lang3.function.FailableLongFunction;
import org.apache.commons.lang3.function.FailableLongPredicate;
import org.apache.commons.lang3.function.FailableLongToDoubleFunction;
import org.apache.commons.lang3.function.FailableLongToIntFunction;

import java.util.Iterator;
import java.util.OptionalDouble;
import java.util.OptionalLong;
import java.util.Spliterator;
import java.util.function.LongConsumer;
import java.util.function.LongFunction;
import java.util.function.LongPredicate;
import java.util.function.LongToDoubleFunction;
import java.util.function.LongToIntFunction;
import java.util.stream.LongStream;

/**
 * A reduced and simplified version of a {@link LongStream} with
 * failable method signatures.
 *
 * @see LongStream
 * @since 3.10
 */
public class FailableLongStream extends FailableBaseStream<Long, FailableLongStream> {
    private LongStream longStream;

    public FailableLongStream(LongStream longStream) {
        this.longStream = longStream;
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
        return longStream.average();
    }

    /**
     * Returns a {@code FailableStream} consisting of the elements of this stream, each boxed to a Long.
     *
     * @return a FailableStream consisting of the elements of this stream, each boxed to a Long
     */
    public FailableStream<Long> boxed() {
        return new FailableStream<>(longStream.boxed());
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
     * @see LongStream#filter(LongPredicate)
     */
    public FailableLongStream filter(final FailableLongPredicate<?> predicate) {
        assertNotTerminated();
        longStream = longStream.filter(Functions.asLongPredicate(predicate));
        return this;
    }

    /**
     * Returns an {@link OptionalLong} describing some element of the stream,
     * or an empty {@code OptionalLong} if the stream is empty.
     *
     * <p>This is a short-circuiting terminal operation.
     *
     * <p>The behavior of this operation is explicitly nondeterministic; it is
     * free to select any value in the stream.  This is to allow for maximal
     * performance in parallel operations; the cost is that multiple invocations
     * on the same source may not return the same result. (If a stable result
     * is desired, use {@link #findFirst()} instead.)
     *
     * @return an {@code OptionalLong} describing some value of this stream,
     * or an empty {@code OptionalLong} if the stream is empty
     *
     * @see LongStream#findAny()
     * @see #findFirst()
     */
    public OptionalLong findAny() {
        makeTerminated();
        return longStream.findAny();
    }

    /**
     * Returns an {@link OptionalLong} describing the first value of this
     * stream, or an empty {@code OptionalInt} if the stream is empty. If
     * the stream has no encounter order, then any value may be returned.
     *
     * <p>This is a short-circuiting terminal operation.
     *
     * @return an {@code OptionalInt} describing the first value of this
     * stream, or an empty optional if the stream is empty
     *
     * @see LongStream#findFirst()
     */
    public OptionalLong findFirst() {
        makeTerminated();
        return longStream.findFirst();
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
     * @see LongStream#forEach(LongConsumer)
     */
    public void forEach(final FailableLongConsumer<?> action) {
        makeTerminated();
        longStream.forEach(Functions.asLongConsumer(action));
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
     * @see LongStream#mapToObj(LongFunction)
     */
    public <O> FailableStream<O> mapToObj(final FailableLongFunction<O, ?> mapper) {
        assertNotTerminated();
        return new FailableStream<>(longStream.mapToObj(Functions.asLongFunction(mapper)));
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
     * @see LongStream#mapToDouble(LongToDoubleFunction)
     */
    public FailableDoubleStream mapToDouble(final FailableLongToDoubleFunction<?> mapper) {
        assertNotTerminated();
        return new FailableDoubleStream(longStream.mapToDouble(Functions.asLongToDoubleFunction(mapper)));
    }

    /**
     * Returns a {@code FailableIntStream} consisting of the results of applying the
     * given function to the elements of this stream.
     *
     * <p>This is an intermediate operation.
     *
     * @param mapper a non-interfering, stateless function to apply to each element
     * @return the new stream
     *
     * @see LongStream#mapToInt(LongToIntFunction)
     */
    public FailableIntStream mapToInt(final FailableLongToIntFunction<?> mapper) {
        assertNotTerminated();
        return new FailableIntStream(longStream.mapToInt(Functions.asLongToIntFunction(mapper)));
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
     * @see LongStream#max()
     */
    public OptionalLong max() {
        makeTerminated();
        return longStream.max();
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
     * @see LongStream#min()
     */
    public OptionalLong min() {
        makeTerminated();
        return longStream.min();
    }

    /**
     * Returns the {@link LongStream} which this stream wraps around.
     * @return the {@code LongStream} used by this stream
     */
    public LongStream stream() {
        return longStream;
    }

    /**
     * Returns the sum of elements in this stream.
     *
     * This is a special case of a reduction and is equivalent to:
     * <pre>{@code
     *     return reduce(0, Long::sum);
     * }</pre>
     *
     * <p>This is a terminal operation.
     *
     * @return the sum of elements in this stream
     */
    public long sum() {
        makeTerminated();
        return longStream.sum();
    }

    @Override
    public long count() {
        makeTerminated();
        return longStream.count();
    }

    @Override
    public Iterator<Long> iterator() {
        makeTerminated();
        return longStream.iterator();
    }

    @Override
    public Spliterator<Long> spliterator() {
        makeTerminated();
        return longStream.spliterator();
    }

    @Override
    public FailableLongStream distinct() {
        assertNotTerminated();
        longStream = longStream.distinct();
        return this;
    }

    @Override
    public FailableLongStream sequential() {
        assertNotTerminated();
        longStream = longStream.sequential();
        return this;
    }

    @Override
    public FailableLongStream parallel() {
        assertNotTerminated();
        longStream = longStream.parallel();
        return this;
    }
}
