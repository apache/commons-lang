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

import java.util.Iterator;
import java.util.Spliterator;
import java.util.stream.BaseStream;
import java.util.stream.DoubleStream;
import java.util.stream.IntStream;
import java.util.stream.LongStream;
import java.util.stream.Stream;

/**
 * A class with methods common to all failable streams.
 *
 * This class includes some methods not available in {@link BaseStream}, such as
 * {@link #count()} and {@link #distinct()}.
 *
 * @param <T> the type of the stream elements
 * @param <S> the type of the stream implementing {@code FailableBaseStream}
 *
 * @see BaseStream
 * @see FailableIntStream
 * @see FailableLongStream
 * @see FailableDoubleStream
 * @see FailableStream
 * @since 3.10
 */
public abstract class FailableBaseStream<T, S extends FailableBaseStream<T, S>> {
    private boolean terminated;

    /**
     * Throws an exception if the stream has already been terminated.
     *
     * @throws IllegalStateException if the stream has been terminated
     */
    protected final void assertNotTerminated() {
        if (terminated) {
            throw new IllegalStateException("This stream is already terminated.");
        }
    }

    /**
     * Terminates the stream if it has not been already terminated.
     */
    protected void makeTerminated() {
        assertNotTerminated();
        terminated = true;
    }

    /**
     * Returns the count of elements in this stream.  This is a special case of
     * a reduction.
     *
     * <p>This is a terminal operation.
     *
     * \@apiNote
     * An implementation may choose to not execute the stream pipeline (either
     * sequentially or in parallel) if it is capable of computing the count
     * directly from the stream source. In such cases, no source elements will
     * be traversed and no intermediate operations will be evaluated.
     * Behavioral parameters with side-effects, which are strongly discouraged
     * except for harmless cases such as debugging, may be affected.
     *
     * @return the count of elements in this stream
     *
     * @see Stream#count()
     * @see IntStream#count()
     * @see LongStream#count()
     * @see DoubleStream#count()
     */
    public abstract long count();

    /**
     * Returns an iterator for the elements of this stream.
     *
     * <p>This is a terminal operation.
     *
     * @return the element iterator for this stream
     *
     * @see BaseStream#iterator()
     */
    public abstract Iterator<T> iterator();

    /**
     * Returns a spliterator for the elements of this stream.
     *
     * <p>This is a terminal operation.
     *
     * <p>
     * The returned spliterator should report the set of characteristics derived
     * from the stream pipeline (namely the characteristics derived from the
     * stream source spliterator and the intermediate operations).
     * Implementations may report a sub-set of those characteristics. For
     * example, it may be too expensive to compute the entire set for some or
     * all possible stream pipelines.
     *
     * @return the element spliterator for this stream
     *
     * @see BaseStream#spliterator()
     */
    public abstract Spliterator<T> spliterator();

    /**
     * Returns a stream consisting of the distinct elements (according to
     * {@link Object#equals(Object)}) of this stream if it hold non-primitive
     * values.
     *
     * <p>This is a stateful intermediate operation.
     *
     * @return a stream containing unique elements
     *
     * @see Stream#distinct()
     * @see IntStream#distinct()
     * @see LongStream#distinct()
     * @see DoubleStream#distinct()
     */
    public abstract S distinct();

    /**
     * Returns an equivalent stream that is sequential. May return
     * itself, either because the stream was already sequential, or because
     * the underlying stream state was modified to be sequential.
     *
     * <p>This is an intermediate operation.
     *
     * @return a sequential stream
     *
     * @see BaseStream#sequential()
     */
    public abstract S sequential();

    /**
     * Returns an equivalent stream that is parallel. May return
     * itself, either because the stream was already parallel, or because
     * the underlying stream state was modified to be parallel.
     *
     * <p>This is an intermediate operation.
     *
     * @return a parallel stream
     *
     * @see BaseStream#parallel()
     */
    public abstract S parallel();

    /**
     * Returns a stream consisting of the elements of this stream, sorted
     * according to natural order. If the elements of this stream are not
     * {@code Comparable}, a {@code java.lang.ClassCastException} may be thrown
     * when the terminal operation is executed.
     *
     * <p>For ordered streams, the sort is stable. For unordered streams, no
     * stability guarantees are made.
     *
     * <p>This is a stateful intermediate operation.
     *
     * @return the new stream
     *
     * @see Stream#sorted()
     * @see IntStream#sorted()
     * @see LongStream#sorted()
     * @see DoubleStream#sorted()
     */
    public abstract S sorted();

    /**
     * Returns an equivalent stream that is unordered. May return
     * itself, either because the stream was already unordered, or because
     * the underlying stream state was modified to be unordered.
     *
     * <p>This is an intermediate operation.
     *
     * @return an unordered stream
     *
     * @see BaseStream#unordered()
     */
    public abstract S unordered();

    /**
     * Returns a stream consisting of the elements of this stream, truncated
     * to be no longer than {@code maxSize} in length.
     *
     * <p>This is a short-circuiting stateful intermediate operation.
     *
     * \@apiNote
     * While {@code limit()} is generally a cheap operation on sequential
     * stream pipelines, it can be quite expensive on ordered parallel pipelines,
     * especially for large values of {@code maxSize}, since {@code limit(n)}
     * is constrained to return not just any <em>n</em> elements, but the
     * <em>first n</em> elements in the encounter order.  Using an unordered
     * stream source or removing the ordering constraint with {@link #unordered()}
     * may result in significant speedups of {@code limit()} in parallel pipelines,
     * if the semantics of your situation permit. If consistency with encounter order
     * is required, and you are experiencing poor performance or memory utilization with
     * {@code limit()} in parallel pipelines, switching to sequential execution
     * with {@link #sequential()} may improve performance.
     *
     * @param maxSize the number of elements the stream should be limited to
     * @return the new stream
     * @throws IllegalArgumentException if {@code maxSize} is negative
     *
     * @see Stream#limit(long)
     * @see IntStream#limit(long)
     * @see LongStream#limit(long)
     * @see DoubleStream#limit(long)
     */
    public abstract S limit(long maxSize);

    /**
     * Returns a stream consisting of the remaining elements of this stream
     * after discarding the first {@code n} elements of the stream.
     * If this stream contains fewer than {@code n} elements then an
     * empty stream will be returned.
     *
     * <p>This is a stateful intermediate operation.
     *
     * \@apiNote
     * While {@code skip()} is generally a cheap operation on sequential
     * stream pipelines, it can be quite expensive on ordered parallel pipelines,
     * especially for large values of {@code n}, since {@code skip(n)}
     * is constrained to skip not just any <em>n</em> elements, but the
     * <em>first n</em> elements in the encounter order.  Using an unordered
     * stream source or removing the ordering constraint with {@link #unordered()}
     * may result in significant speedups of {@code skip()} in parallel pipelines,
     * if the semantics of your situation permit. If consistency with encounter order
     * is required, and you are experiencing poor performance or memory utilization with
     * {@code skip()} in parallel pipelines, switching to sequential execution
     * with {@link #sequential()} may improve performance.
     *
     * @param n the number of leading elements to skip
     * @return the new stream
     * @throws IllegalArgumentException if {@code n} is negative
     *
     * @see Stream#skip(long)
     * @see IntStream#skip(long)
     * @see LongStream#skip(long)
     * @see DoubleStream#skip(long)
     */
    public abstract S skip(long n);
}
