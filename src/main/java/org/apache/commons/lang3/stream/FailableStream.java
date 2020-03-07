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
import org.apache.commons.lang3.function.FailableToDoubleFunction;
import org.apache.commons.lang3.function.FailableToIntFunction;
import org.apache.commons.lang3.function.FailableToLongFunction;

import java.util.Iterator;
import java.util.Spliterator;
import java.util.function.BiConsumer;
import java.util.function.BinaryOperator;
import java.util.function.Supplier;
import java.util.stream.BaseStream;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.DoubleStream;
import java.util.stream.IntStream;
import java.util.stream.LongStream;
import java.util.stream.Stream;

/**
 * A reduced and simplified version of a {@link Stream} with
 * failable method signatures.
 * @param <O> The stream element type.
 *
 * @see Stream
 * @since 3.10
 */
 public class FailableStream<O> extends FailableBaseStream<O, FailableStream<O>> {
     private Stream<O> stream;

     /**
      * Constructs a new instance with the given {@code stream}.
      * @param stream The stream.
      */
     public FailableStream(final Stream<O> stream) {
         this.stream = stream;
     }

     /**
      * Returns a FailableStream consisting of the elements of this stream that match
      * the given FailablePredicate.
      *
      * <p>This is an intermediate operation.
      *
      * @param predicate a non-interfering, stateless predicate to apply to each
      * element to determine if it should be included.
      * @return the new stream
      */
     public FailableStream<O> filter(final Functions.FailablePredicate<O, ?> predicate){
         assertNotTerminated();
         stream = stream.filter(Functions.asPredicate(predicate));
         return this;
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
      */
     public void forEach(final Functions.FailableConsumer<O, ?> action) {
         makeTerminated();
         stream().forEach(Functions.asConsumer(action));
     }

     /**
      * Performs a mutable reduction operation on the elements of this stream using a
      * {@code Collector}.  A {@code Collector}
      * encapsulates the functions used as arguments to
      * {@link #collect(Supplier, BiConsumer, BiConsumer)}, allowing for reuse of
      * collection strategies and composition of collect operations such as
      * multiple-level grouping or partitioning.
      *
      * <p>If the underlying stream is parallel, and the {@code Collector}
      * is concurrent, and either the stream is unordered or the collector is
      * unordered, then a concurrent reduction will be performed
      * (see {@link Collector} for details on concurrent reduction.)
      *
      * <p>This is a terminal operation.
      *
      * <p>When executed in parallel, multiple intermediate results may be
      * instantiated, populated, and merged so as to maintain isolation of
      * mutable data structures.  Therefore, even when executed in parallel
      * with non-thread-safe data structures (such as {@code ArrayList}), no
      * additional synchronization is needed for a parallel reduction.
      *
      * \@apiNote
      * The following will accumulate strings into an ArrayList:
      * <pre>{@code
      *     List<String> asList = stringStream.collect(Collectors.toList());
      * }</pre>
      *
      * <p>The following will classify {@code Person} objects by city:
      * <pre>{@code
      *     Map<String, List<Person>> peopleByCity
      *         = personStream.collect(Collectors.groupingBy(Person::getCity));
      * }</pre>
      *
      * <p>The following will classify {@code Person} objects by state and city,
      * cascading two {@code Collector}s together:
      * <pre>{@code
      *     Map<String, Map<String, List<Person>>> peopleByStateAndCity
      *         = personStream.collect(Collectors.groupingBy(Person::getState,
      *                                                      Collectors.groupingBy(Person::getCity)));
      * }</pre>
      *
      * @param <R> the type of the result
      * @param <A> the intermediate accumulation type of the {@code Collector}
      * @param collector the {@code Collector} describing the reduction
      * @return the result of the reduction
      * @see #collect(Supplier, BiConsumer, BiConsumer)
      * @see Collectors
      */
     public <A, R> R collect(final Collector<? super O, A, R> collector) {
         makeTerminated();
         return stream.collect(collector);
     }

     /**
      * Performs a mutable reduction operation on the elements of this FailableStream.
      * A mutable reduction is one in which the reduced value is a mutable result
      * container, such as an {@code ArrayList}, and elements are incorporated by updating
      * the state of the result rather than by replacing the result. This produces a result equivalent to:
      * <pre>{@code
      *     R result = supplier.get();
      *     for (T element : this stream)
      *         accumulator.accept(result, element);
      *     return result;
      * }</pre>
      *
      * <p>Like {@link #reduce(Object, BinaryOperator)}, {@code collect} operations
      * can be parallelized without requiring additional synchronization.
      *
      * <p>This is a terminal operation.
      *
      * \@apiNote There are many existing classes in the JDK whose signatures are
      * well-suited for use with method references as arguments to {@code collect()}.
      * For example, the following will accumulate strings into an {@code ArrayList}:
      * <pre>{@code
      *     List<String> asList = stringStream.collect(ArrayList::new, ArrayList::add,
      *                                                ArrayList::addAll);
      * }</pre>
      *
      * <p>The following will take a stream of strings and concatenates them into a
      * single string:
      * <pre>{@code
      *     String concat = stringStream.collect(StringBuilder::new, StringBuilder::append,
      *                                          StringBuilder::append)
      *                                 .toString();
      * }</pre>
      *
      * @param <R> type of the result
      * @param supplier a function that creates a new result container. For a
      *                 parallel execution, this function may be called
      *                 multiple times and must return a fresh value each time.
      * @param accumulator An associative, non-interfering, stateless function for
      *   incorporating an additional element into a result
      * @param combiner An associative, non-interfering, stateless
      *   function for combining two values, which must be compatible with the
      *   accumulator function
      * @return The result of the reduction
      */
     public <R> R collect(final Supplier<R> supplier, final BiConsumer<R, ? super O> accumulator, final BiConsumer<R, R> combiner) {
         makeTerminated();
         return stream.collect(supplier, accumulator, combiner);
     }

     /**
      * Performs a reduction on the elements of this stream, using the provided
      * identity value and an associative accumulation function, and returns
      * the reduced value.  This is equivalent to:
      * <pre>{@code
      *     T result = identity;
      *     for (T element : this stream)
      *         result = accumulator.apply(result, element)
      *     return result;
      * }</pre>
      *
      * but is not constrained to execute sequentially.
      *
      * <p>The {@code identity} value must be an identity for the accumulator
      * function. This means that for all {@code t},
      * {@code accumulator.apply(identity, t)} is equal to {@code t}.
      * The {@code accumulator} function must be an associative function.
      *
      * <p>This is a terminal operation.
      *
      * \@apiNote Sum, min, max, average, and string concatenation are all special
      * cases of reduction. Summing a stream of numbers can be expressed as:
      *
      * <pre>{@code
      *     Integer sum = integers.reduce(0, (a, b) -> a+b);
      * }</pre>
      *
      * or:
      *
      * <pre>{@code
      *     Integer sum = integers.reduce(0, Integer::sum);
      * }</pre>
      *
      * <p>While this may seem a more roundabout way to perform an aggregation
      * compared to simply mutating a running total in a loop, reduction
      * operations parallelize more gracefully, without needing additional
      * synchronization and with greatly reduced risk of data races.
      *
      * @param identity the identity value for the accumulating function
      * @param accumulator an associative, non-interfering, stateless
      *                    function for combining two values
      * @return the result of the reduction
      */
     public O reduce(final O identity, final BinaryOperator<O> accumulator) {
         makeTerminated();
         return stream.reduce(identity, accumulator);
     }

     /**
      * Returns a stream consisting of the results of applying the given
      * function to the elements of this stream.
      *
      * <p>This is an intermediate operation.
      *
      * @param <R> The element type of the new stream
      * @param mapper A non-interfering, stateless function to apply to each element
      * @return the new stream
      */
     public <R> FailableStream<R> map(final Functions.FailableFunction<O, R, ?> mapper) {
         assertNotTerminated();
         return new FailableStream<>(stream.map(Functions.asFunction(mapper)));
     }

    /**
     * Returns a {@code FailableIntStream} consisting of the results of applying the
     * given function to the elements of this stream.
     *
     * <p>This is an intermediate operation.
     *
     * @param mapper a non-interfering, stateless function to apply to each element
     * @return the new stream
     */
     public FailableIntStream mapToInt(final FailableToIntFunction<O, ?> mapper) {
         assertNotTerminated();
         return new FailableIntStream(stream.mapToInt(Functions.asToIntFunction(mapper)));
     }

    /**
     * Returns a {@code FailableLongStream} consisting of the results of applying the
     * given function to the elements of this stream.
     *
     * <p>This is an intermediate operation.
     *
     * @param mapper a non-interfering, stateless function to apply to each element
     * @return the new stream
     */
     public FailableLongStream mapToLong(final FailableToLongFunction<O, ?> mapper) {
         assertNotTerminated();
         return new FailableLongStream(stream.mapToLong(Functions.asToLongFunction(mapper)));
     }

    /**
     * Returns a {@code FailableDoubleStream} consisting of the results of applying the
     * given function to the elements of this stream.
     *
     * <p>This is an intermediate operation.
     *
     * @param mapper a non-interfering, stateless function to apply to each element
     * @return the new stream
     */
     public FailableDoubleStream mapToDouble(final FailableToDoubleFunction<O, ?> mapper) {
         assertNotTerminated();
         return new FailableDoubleStream(stream.mapToDouble(Functions.asToDoubleFunction(mapper)));
     }

    /**
     * Returns a stream consisting of the results of replacing each element of
     * this stream with the contents of a mapped stream produced by applying
     * the provided mapping function to each element.  Each mapped stream is
     * {@link BaseStream#close() closed} after its contents
     * have been placed into this stream.  (If a mapped stream is {@code null}
     * an empty stream is used, instead.)
     *
     * <p>This is an intermediate operation.
     *
     * @param <R>    The element type of the new stream
     * @param mapper a non-interfering, stateless function to apply to each element which produces a stream of new values
     * @return the new stream
     * @apiNote The {@code flatMap()} operation has the effect of applying a one-to-many
     * transformation to the elements of the stream, and then flattening the
     * resulting elements into a new stream.
     */
     public <R> FailableStream<R> flatMap(Functions.FailableFunction<? super O, ? extends Stream<? extends R>, ?> mapper) {
         assertNotTerminated();
         return new FailableStream<>(stream.flatMap(Functions.asFunction(mapper)));
     }

    /**
     * Returns a {@code FailableIntStream} consisting of the results of replacing each
     * element of this stream with the contents of a mapped stream produced by
     * applying the provided mapping function to each element.  Each mapped
     * stream is closed after its contents have been placed into this stream.
     * (If a mapped stream is {@code null} an empty stream is used, instead.)
     *
     * <p>This is an intermediate operation.
     *
     * @param mapper a non-interfering, stateless function to apply to each element
     *               which produces a stream of new values
     * @return the new stream
     */
     public FailableIntStream flatMapToInt(Functions.FailableFunction<? super O, IntStream, ?> mapper) {
         assertNotTerminated();
         return new FailableIntStream(stream.flatMapToInt(Functions.asFunction(mapper)));
     }

    /**
     * Returns a {@code FailableLongStream} consisting of the results of replacing each
     * element of this stream with the contents of a mapped stream produced by
     * applying the provided mapping function to each element.  Each mapped
     * stream is closed after its contents have been placed into this stream.
     * (If a mapped stream is {@code null} an empty stream is used, instead.)
     *
     * <p>This is an intermediate operation.
     *
     * @param mapper a non-interfering, stateless function to apply to each element
     *               which produces a stream of new values
     * @return the new stream
     */
     public FailableLongStream flatMapToLong(Functions.FailableFunction<? super O, LongStream, ?> mapper) {
         assertNotTerminated();
         return new FailableLongStream(stream.flatMapToLong(Functions.asFunction(mapper)));
     }

    /**
     * Returns a {@code FailableDoubleStream} consisting of the results of replacing each
     * element of this stream with the contents of a mapped stream produced by
     * applying the provided mapping function to each element.  Each mapped
     * stream is closed after its contents have been placed into this stream.
     * (If a mapped stream is {@code null} an empty stream is used, instead.)
     *
     * <p>This is an intermediate operation.
     *
     * @param mapper a non-interfering, stateless function to apply to each element
     *               which produces a stream of new values
     * @return the new stream
     */
     public FailableDoubleStream flatMapToDouble(Functions.FailableFunction<? super O, DoubleStream, ?> mapper) {
         assertNotTerminated();
         return new FailableDoubleStream(stream.flatMapToDouble(Functions.asFunction(mapper)));
     }

     /**
      * Converts the FailableStream into an equivalent stream.
      * @return A stream, which will return the same elements, which this FailableStream would return.
      */
     public Stream<O> stream() {
         return stream;
     }

     /**
      * Returns whether all elements of this stream match the provided predicate.
      * May not evaluate the predicate on all elements if not necessary for
      * determining the result.  If the stream is empty then {@code true} is
      * returned and the predicate is not evaluated.
      *
      * <p>This is a short-circuiting terminal operation.
      *
      * \@apiNote
      * This method evaluates the <em>universal quantification</em> of the
      * predicate over the elements of the stream (for all x P(x)).  If the
      * stream is empty, the quantification is said to be <em>vacuously
      * satisfied</em> and is always {@code true} (regardless of P(x)).
      *
      * @param predicate A non-interfering, stateless predicate to apply to
      * elements of this stream
      * @return {@code true} If either all elements of the stream match the
      * provided predicate or the stream is empty, otherwise {@code false}.
      */
     public boolean allMatch(final Functions.FailablePredicate<O, ?> predicate) {
         assertNotTerminated();
         return stream().allMatch(Functions.asPredicate(predicate));
     }

     /**
      * Returns whether any elements of this stream match the provided
      * predicate.  May not evaluate the predicate on all elements if not
      * necessary for determining the result.  If the stream is empty then
      * {@code false} is returned and the predicate is not evaluated.
      *
      * <p>This is a short-circuiting terminal operation.
      *
      * \@apiNote
      * This method evaluates the <em>existential quantification</em> of the
      * predicate over the elements of the stream (for some x P(x)).
      *
      * @param predicate A non-interfering, stateless predicate to apply to
      * elements of this stream
      * @return {@code true} if any elements of the stream match the provided
      * predicate, otherwise {@code false}
      */
     public boolean anyMatch(final Functions.FailablePredicate<O, ?> predicate) {
         assertNotTerminated();
         return stream().anyMatch(Functions.asPredicate(predicate));
     }

    @Override
    public long count() {
        return 0;
    }

    @Override
    public Iterator<O> iterator() {
        return null;
    }

    @Override
    public Spliterator<O> spliterator() {
        return null;
    }

    @Override
    public FailableStream<O> distinct() {
        return null;
    }

    @Override
    public FailableStream<O> sequential() {
        return null;
    }

    @Override
    public FailableStream<O> parallel() {
        return null;
    }
}
