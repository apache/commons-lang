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
import org.apache.commons.lang3.function.FailableDoubleConsumer;
import org.apache.commons.lang3.function.FailableDoubleFunction;
import org.apache.commons.lang3.function.FailableDoublePredicate;
import org.apache.commons.lang3.function.FailableDoubleToIntFunction;
import org.apache.commons.lang3.function.FailableDoubleToLongFunction;
import org.apache.commons.lang3.function.FailableDoubleUnaryOperator;

import java.util.Iterator;
import java.util.OptionalDouble;
import java.util.Spliterator;
import java.util.function.DoubleConsumer;
import java.util.function.DoubleFunction;
import java.util.function.DoublePredicate;
import java.util.function.DoubleToIntFunction;
import java.util.function.DoubleToLongFunction;
import java.util.function.DoubleUnaryOperator;
import java.util.stream.DoubleStream;

/**
 * A reduced and simplified version of a {@link DoubleStream} with
 * failable method signatures.
 *
 * @see DoubleStream
 * @since 3.10
 */
public class FailableDoubleStream extends FailableBaseStream<Double, FailableDoubleStream> {
    private DoubleStream doubleStream;

    public FailableDoubleStream(DoubleStream doubleStream) {
        this.doubleStream = doubleStream;
    }

    /**
     * Returns the average of elements in this stream.
     *
     * <p>This is a terminal operation.
     *
     * @return the average of elements in this stream
     *
     * @see DoubleStream#average()
     */
    public OptionalDouble average() {
        makeTerminated();
        return doubleStream.average();
    }

    /**
     * Returns a {@code FailableStream} consisting of the elements of this stream,
     * each boxed to a Double.
     *
     * <p>This is an intermediate operation.
     *
     * @return a FailableStream consisting of the elements of this stream,
     * each boxed to a Double
     *
     * @see DoubleStream#boxed()
     */
    public FailableStream<Double> boxed() {
        assertNotTerminated();
        return new FailableStream<>(doubleStream.boxed());
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
     * @see DoubleStream#filter(DoublePredicate)
     */
    public FailableDoubleStream filter(final FailableDoublePredicate<?> predicate) {
        assertNotTerminated();
        doubleStream = doubleStream.filter(Functions.asDoublePredicate(predicate));
        return this;
    }

    /**
     * Returns an {@link OptionalDouble} describing some element of the stream,
     * or an empty {@code OptionalDouble} if the stream is empty.
     *
     * <p>This is a short-circuiting terminal operation.
     *
     * <p>The behavior of this operation is explicitly nondeterministic; it is
     * free to select any value in the stream.  This is to allow for maximal
     * performance in parallel operations; the cost is that multiple invocations
     * on the same source may not return the same result. (If a stable result
     * is desired, use {@link #findFirst()} instead.)
     *
     * @return an {@code OptionalDouble} describing some value of this stream,
     * or an empty {@code OptionalDouble} if the stream is empty
     *
     * @see DoubleStream#findAny()
     * @see #findFirst()
     */
    public OptionalDouble findAny() {
        makeTerminated();
        return doubleStream.findAny();
    }

    /**
     * Returns an {@link OptionalDouble} describing the first value of this
     * stream, or an empty {@code OptionalDouble} if the stream is empty. If
     * the stream has no encounter order, then any value may be returned.
     *
     * <p>This is a short-circuiting terminal operation.
     *
     * @return an {@code OptionalDouble} describing the first value of this
     * stream, or an empty optional if the stream is empty
     *
     * @see DoubleStream#findFirst()
     */
    public OptionalDouble findFirst() {
        makeTerminated();
        return doubleStream.findFirst();
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
     * @see DoubleStream#forEach(DoubleConsumer)
     */
    public void forEach(final FailableDoubleConsumer<?> action) {
        makeTerminated();
        doubleStream.forEach(Functions.asDoubleConsumer(action));
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
     * @see DoubleStream#map(DoubleUnaryOperator)
     */
    public FailableDoubleStream map(final FailableDoubleUnaryOperator<?> mapper) {
        assertNotTerminated();
        doubleStream = doubleStream.map(Functions.asDoubleUnaryOperator(mapper));
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
     * @see DoubleStream#mapToObj(DoubleFunction)
     */
    public <O> FailableStream<O> mapToObj(final FailableDoubleFunction<O, ?> mapper) {
        assertNotTerminated();
        return new FailableStream<>(doubleStream.mapToObj(Functions.asDoubleFunction(mapper)));
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
     * @see DoubleStream#mapToInt(DoubleToIntFunction)
     */
    public FailableIntStream mapToInt(final FailableDoubleToIntFunction<?> mapper) {
        assertNotTerminated();
        return new FailableIntStream(doubleStream.mapToInt(Functions.asDoubleToIntFunction(mapper)));
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
     * @see DoubleStream#mapToLong(DoubleToLongFunction)
     */
    public FailableLongStream mapToLong(final FailableDoubleToLongFunction<?> mapper) {
        assertNotTerminated();
        return new FailableLongStream(doubleStream.mapToLong(Functions.asDoubleToLongFunction(mapper)));
    }

    /**
     * Returns an {@code OptionalDouble} describing the maximum value of this
     * stream, or an empty OptionalDouble if this stream is empty. The maximum
     * value will be {@code Double.NaN} if any stream value was NaN. Unlike
     * the numerical comparison operators, this method considers negative zero
     * to be strictly smaller than positive zero. This is a special case of a
     * reduction and is equivalent to:
     * <pre>{@code
     *     return reduce(Double::max);
     * }</pre>
     *
     * <p>This is a terminal operation.
     *
     * @return an {@code OptionalDouble} containing the maximum value of this
     * stream, or an empty optional if the stream is empty
     */
    public OptionalDouble max() {
        makeTerminated();
        return doubleStream.max();
    }

    /**
     * Returns an {@code OptionalDouble} describing the minimum value of this
     * stream, or an empty OptionalDouble if this stream is empty. The minimum
     * value will be {@code Double.NaN} if any stream value was NaN. Unlike
     * the numerical comparison operators, this method considers negative zero
     * to be strictly smaller than positive zero. This is a special case of a
     * reduction and is equivalent to:
     * <pre>{@code
     *     return reduce(Double::min);
     * }</pre>
     *
     * <p>This is a terminal operation.
     *
     * @return an {@code OptionalDouble} containing the minimum value of this stream,
     * or an empty optional if the stream is empty
     *
     * @see DoubleStream#min()
     */
    public OptionalDouble min() {
        makeTerminated();
        return doubleStream.min();
    }

    /**
     * Returns the {@link DoubleStream} which this stream wraps around.
     * @return the {@code DoubleStream} used by this stream
     */
    public DoubleStream stream() {
        return doubleStream;
    }

    /**
     * Returns the sum of elements in this stream.
     * <p>
     * Summation is a special case of a reduction. If
     * floating-point summation were exact, this method would be
     * equivalent to:
     *
     * <pre>{@code
     *     return reduce(0, Double::sum);
     * }</pre>
     * <p>
     * However, since floating-point summation is not exact, the above
     * code is not necessarily equivalent to the summation computation
     * done by this method.
     *
     * <p>The value of a floating-point sum is a function both
     * of the input values as well as the order of addition
     * operations. The order of addition operations of this method is
     * intentionally not defined to allow for implementation
     * flexibility to improve the speed and accuracy of the computed
     * result.
     * <p>
     * In particular, this method may be implemented using compensated
     * summation or other technique to reduce the error bound in the
     * numerical sum compared to a simple summation of {@code double}
     * values.
     * <p>
     * Because of the unspecified order of operations and the
     * possibility of using differing summation schemes, the output of
     * this method may vary on the same input elements.
     *
     * <p>Various conditions can result in a non-finite sum being
     * computed. This can occur even if the all the elements
     * being summed are finite. If any element is non-finite,
     * the sum will be non-finite:
     *
     * <ul>
     *
     * <li>If any element is a NaN, then the final sum will be
     * NaN.
     *
     * <li>If the elements contain one or more infinities, the
     * sum will be infinite or NaN.
     *
     * <ul>
     *
     * <li>If the elements contain infinities of opposite sign,
     * the sum will be NaN.
     *
     * <li>If the elements contain infinities of one sign and
     * an intermediate sum overflows to an infinity of the opposite
     * sign, the sum may be NaN.
     *
     * </ul>
     *
     * </ul>
     * <p>
     * It is possible for intermediate sums of finite values to
     * overflow into opposite-signed infinities; if that occurs, the
     * final sum will be NaN even if the elements are all
     * finite.
     * <p>
     * If all the elements are zero, the sign of zero is
     * <em>not</em> guaranteed to be preserved in the final sum.
     *
     * <p>This is a terminal
     * operation.
     *
     * @return the sum of elements in this stream
     * \@apiNote Elements sorted by increasing absolute magnitude tend
     * to yield more accurate results.
     *
     * @see DoubleStream#sum()
     */
    public double sum() {
        makeTerminated();
        return doubleStream.sum();
    }

    @Override
    public long count() {
        makeTerminated();
        return doubleStream.count();
    }

    @Override
    public Iterator<Double> iterator() {
        makeTerminated();
        return doubleStream.iterator();
    }

    @Override
    public Spliterator<Double> spliterator() {
        makeTerminated();
        return doubleStream.spliterator();
    }

    @Override
    public FailableDoubleStream distinct() {
        assertNotTerminated();
        doubleStream = doubleStream.distinct();
        return this;
    }

    @Override
    public FailableDoubleStream sequential() {
        assertNotTerminated();
        doubleStream = doubleStream.sequential();
        return this;
    }

    @Override
    public FailableDoubleStream parallel() {
        assertNotTerminated();
        doubleStream = doubleStream.parallel();
        return this;
    }

    @Override
    public FailableDoubleStream sorted() {
        assertNotTerminated();
        doubleStream = doubleStream.sorted();
        return this;
    }

    @Override
    public FailableDoubleStream unordered() {
        assertNotTerminated();
        doubleStream = doubleStream.unordered();
        return this;
    }

    @Override
    public FailableDoubleStream limit(long maxSize) {
        assertNotTerminated();
        doubleStream = doubleStream.limit(maxSize);
        return this;
    }

    @Override
    public FailableDoubleStream skip(long n) {
        assertNotTerminated();
        doubleStream = doubleStream.skip(n);
        return this;
    }
}
