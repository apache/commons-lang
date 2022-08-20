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
package org.apache.commons.lang3;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.BinaryOperator;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.Functions.FailableConsumer;
import org.apache.commons.lang3.Functions.FailableFunction;
import org.apache.commons.lang3.Functions.FailablePredicate;

/**
 * Provides utility functions, and classes for working with the
 * {@code java.util.stream} package, or more generally, with Java 8 lambdas. More
 * specifically, it attempts to address the fact that lambdas are supposed
 * not to throw Exceptions, at least not checked Exceptions, AKA instances
 * of {@link Exception}. This enforces the use of constructs like
 * <pre>
 *     Consumer&lt;java.lang.reflect.Method&gt; consumer = m -&gt; {
 *         try {
 *             m.invoke(o, args);
 *         } catch (Throwable t) {
 *             throw Functions.rethrow(t);
 *         }
 *    };
 *    stream.forEach(consumer);
 * </pre>
 * Using a {@link FailableStream}, this can be rewritten as follows:
 * <pre>
 *     Streams.failable(stream).forEach((m) -&gt; m.invoke(o, args));
 * </pre>
 * Obviously, the second version is much more concise and the spirit of
 * Lambda expressions is met better than in the first version.
 *
 * @see Stream
 * @see Functions
 * @since 3.10
 * @deprecated Use {@link org.apache.commons.lang3.stream.Streams}.
 */
@Deprecated
public class Streams {

    /**
     * A reduced, and simplified version of a {@link Stream} with
     * failable method signatures.
     * @param <O> The streams element type.
     * @deprecated Use {@link org.apache.commons.lang3.stream.Streams.FailableStream}.
     */
    @Deprecated
    public static class FailableStream<O> {

        private Stream<O> stream;
        private boolean terminated;

        /**
         * Constructs a new instance with the given {@code stream}.
         * @param stream The stream.
         */
        public FailableStream(final Stream<O> stream) {
            this.stream = stream;
        }

        /**
         * Throws IllegalStateException if this stream is already terminated.
         *
         * @throws IllegalStateException if this stream is already terminated.
         */
        protected void assertNotTerminated() {
            if (terminated) {
                throw new IllegalStateException("This stream is already terminated.");
            }
        }

        /**
         * Marks this stream as terminated.
         *
         * @throws IllegalStateException if this stream is already terminated.
         */
        protected void makeTerminated() {
            assertNotTerminated();
            terminated = true;
        }

        /**
         * Returns a FailableStream consisting of the elements of this stream that match
         * the given FailablePredicate.
         *
         * <p>
         * This is an intermediate operation.
         * </p>
         *
         * @param predicate a non-interfering, stateless predicate to apply to each
         * element to determine if it should be included.
         * @return the new stream
         */
        public FailableStream<O> filter(final FailablePredicate<O, ?> predicate){
            assertNotTerminated();
            stream = stream.filter(Functions.asPredicate(predicate));
            return this;
        }

        /**
         * Performs an action for each element of this stream.
         *
         * <p>
         * This is an intermediate operation.
         * </p>
         *
         * <p>
         * The behavior of this operation is explicitly nondeterministic.
         * For parallel stream pipelines, this operation does <em>not</em>
         * guarantee to respect the encounter order of the stream, as doing so
         * would sacrifice the benefit of parallelism.  For any given element, the
         * action may be performed at whatever time and in whatever thread the
         * library chooses.  If the action accesses shared state, it is
         * responsible for providing the required synchronization.
         * </p>
         *
         * @param action a non-interfering action to perform on the elements
         */
        public void forEach(final FailableConsumer<O, ?> action) {
            makeTerminated();
            stream().forEach(Functions.asConsumer(action));
        }

        /**
         * Performs a mutable reduction operation on the elements of this stream using a
         * {@link Collector}.  A {@link Collector}
         * encapsulates the functions used as arguments to
         * {@link #collect(Supplier, BiConsumer, BiConsumer)}, allowing for reuse of
         * collection strategies and composition of collect operations such as
         * multiple-level grouping or partitioning.
         *
         * <p>
         * If the underlying stream is parallel, and the {@link Collector}
         * is concurrent, and either the stream is unordered or the collector is
         * unordered, then a concurrent reduction will be performed
         * (see {@link Collector} for details on concurrent reduction.)
         * </p>
         *
         * <p>
         * This is an intermediate operation.
         * </p>
         *
         * <p>
         * When executed in parallel, multiple intermediate results may be
         * instantiated, populated, and merged so as to maintain isolation of
         * mutable data structures.  Therefore, even when executed in parallel
         * with non-thread-safe data structures (such as {@link ArrayList}), no
         * additional synchronization is needed for a parallel reduction.
         * </p>
         * <p>
         * Note
         * The following will accumulate strings into an ArrayList:
         * </p>
         * <pre>{@code
         *     List<String> asList = stringStream.collect(Collectors.toList());
         * }</pre>
         *
         * <p>
         * The following will classify {@code Person} objects by city:
         * </p>
         * <pre>{@code
         *     Map<String, List<Person>> peopleByCity
         *         = personStream.collect(Collectors.groupingBy(Person::getCity));
         * }</pre>
         *
         * <p>
         * The following will classify {@code Person} objects by state and city,
         * cascading two {@link Collector}s together:
         * </p>
         * <pre>{@code
         *     Map<String, Map<String, List<Person>>> peopleByStateAndCity
         *         = personStream.collect(Collectors.groupingBy(Person::getState,
         *                                                      Collectors.groupingBy(Person::getCity)));
         * }</pre>
         *
         * @param <R> the type of the result
         * @param <A> the intermediate accumulation type of the {@link Collector}
         * @param collector the {@link Collector} describing the reduction
         * @return the result of the reduction
         * @see #collect(Supplier, BiConsumer, BiConsumer)
         * @see Collectors
         */
        public <A, R> R collect(final Collector<? super O, A, R> collector) {
            makeTerminated();
            return stream().collect(collector);
        }

        /**
         * Performs a mutable reduction operation on the elements of this FailableStream.
         * A mutable reduction is one in which the reduced value is a mutable result
         * container, such as an {@link ArrayList}, and elements are incorporated by updating
         * the state of the result rather than by replacing the result. This produces a result equivalent to:
         * <pre>{@code
         *     R result = supplier.get();
         *     for (T element : this stream)
         *         accumulator.accept(result, element);
         *     return result;
         * }</pre>
         *
         * <p>
         * Like {@link #reduce(Object, BinaryOperator)}, {@code collect} operations
         * can be parallelized without requiring additional synchronization.
         * </p>
         *
         * <p>
         * This is an intermediate operation.
         * </p>
         *
         * <p>
         * Note There are many existing classes in the JDK whose signatures are
         * well-suited for use with method references as arguments to {@code collect()}.
         * For example, the following will accumulate strings into an {@link ArrayList}:
         * </p>
         * <pre>{@code
         *     List<String> asList = stringStream.collect(ArrayList::new, ArrayList::add,
         *                                                ArrayList::addAll);
         * }</pre>
         *
         * <p>
         * The following will take a stream of strings and concatenates them into a
         * single string:
         * </p>
         * <pre>{@code
         *     String concat = stringStream.collect(StringBuilder::new, StringBuilder::append,
         *                                          StringBuilder::append)
         *                                 .toString();
         * }</pre>
         *
         * @param <R> type of the result
         * @param <A> Type of the accumulator.
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
        public <A, R> R collect(final Supplier<R> supplier, final BiConsumer<R, ? super O> accumulator, final BiConsumer<R, R> combiner) {
            makeTerminated();
            return stream().collect(supplier, accumulator, combiner);
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
         * <p>
         * The {@code identity} value must be an identity for the accumulator
         * function. This means that for all {@code t},
         * {@code accumulator.apply(identity, t)} is equal to {@code t}.
         * The {@code accumulator} function must be an associative function.
         * </p>
         *
         * <p>
         * This is an intermediate operation.
         * </p>
         *
         * Note Sum, min, max, average, and string concatenation are all special
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
         * <p>
         * While this may seem a more roundabout way to perform an aggregation
         * compared to simply mutating a running total in a loop, reduction
         * operations parallelize more gracefully, without needing additional
         * synchronization and with greatly reduced risk of data races.
         * </p>
         *
         * @param identity the identity value for the accumulating function
         * @param accumulator an associative, non-interfering, stateless
         *                    function for combining two values
         * @return the result of the reduction
         */
        public O reduce(final O identity, final BinaryOperator<O> accumulator) {
            makeTerminated();
            return stream().reduce(identity, accumulator);
        }

        /**
         * Returns a stream consisting of the results of applying the given
         * function to the elements of this stream.
         *
         * <p>
         * This is an intermediate operation.
         * </p>
         *
         * @param <R> The element type of the new stream
         * @param mapper A non-interfering, stateless function to apply to each element
         * @return the new stream
         */
        public <R> FailableStream<R> map(final FailableFunction<O, R, ?> mapper) {
            assertNotTerminated();
            return new FailableStream<>(stream.map(Functions.asFunction(mapper)));
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
         * <p>
         * This is a short-circuiting terminal operation.
         * </p>
         *
         * <p>
         * Note
         * This method evaluates the <em>universal quantification</em> of the
         * predicate over the elements of the stream (for all x P(x)).  If the
         * stream is empty, the quantification is said to be <em>vacuously
         * satisfied</em> and is always {@code true} (regardless of P(x)).
         * </p>
         *
         * @param predicate A non-interfering, stateless predicate to apply to
         * elements of this stream
         * @return {@code true} If either all elements of the stream match the
         * provided predicate or the stream is empty, otherwise {@code false}.
         */
        public boolean allMatch(final FailablePredicate<O, ?> predicate) {
            assertNotTerminated();
            return stream().allMatch(Functions.asPredicate(predicate));
        }

        /**
         * Returns whether any elements of this stream match the provided
         * predicate.  May not evaluate the predicate on all elements if not
         * necessary for determining the result.  If the stream is empty then
         * {@code false} is returned and the predicate is not evaluated.
         *
         * <p>
         * This is a short-circuiting terminal operation.
         * </p>
         *
         * Note
         * This method evaluates the <em>existential quantification</em> of the
         * predicate over the elements of the stream (for some x P(x)).
         *
         * @param predicate A non-interfering, stateless predicate to apply to
         * elements of this stream
         * @return {@code true} if any elements of the stream match the provided
         * predicate, otherwise {@code false}
         */
        public boolean anyMatch(final FailablePredicate<O, ?> predicate) {
            assertNotTerminated();
            return stream().anyMatch(Functions.asPredicate(predicate));
        }
    }

    /**
     * Converts the given {@link Stream stream} into a {@link FailableStream}.
     * This is basically a simplified, reduced version of the {@link Stream}
     * class, with the same underlying element stream, except that failable
     * objects, like {@link FailablePredicate}, {@link FailableFunction}, or
     * {@link FailableConsumer} may be applied, instead of
     * {@link Predicate}, {@link Function}, or {@link Consumer}. The idea is
     * to rewrite a code snippet like this:
     * <pre>
     *     final List&lt;O&gt; list;
     *     final Method m;
     *     final Function&lt;O,String&gt; mapper = (o) -&gt; {
     *         try {
     *             return (String) m.invoke(o);
     *         } catch (Throwable t) {
     *             throw Functions.rethrow(t);
     *         }
     *     };
     *     final List&lt;String&gt; strList = list.stream()
     *         .map(mapper).collect(Collectors.toList());
     *  </pre>
     *  as follows:
     *  <pre>
     *     final List&lt;O&gt; list;
     *     final Method m;
     *     final List&lt;String&gt; strList = Functions.stream(list.stream())
     *         .map((o) -&gt; (String) m.invoke(o)).collect(Collectors.toList());
     *  </pre>
     *  While the second version may not be <em>quite</em> as
     *  efficient (because it depends on the creation of additional,
     *  intermediate objects, of type FailableStream), it is much more
     *  concise, and readable, and meets the spirit of Lambdas better
     *  than the first version.
     * @param <O> The streams element type.
     * @param stream The stream, which is being converted.
     * @return The {@link FailableStream}, which has been created by
     *   converting the stream.
     */
    public static <O> FailableStream<O> stream(final Stream<O> stream) {
        return new FailableStream<>(stream);
    }

    /**
     * Converts the given {@link Collection} into a {@link FailableStream}.
     * This is basically a simplified, reduced version of the {@link Stream}
     * class, with the same underlying element stream, except that failable
     * objects, like {@link FailablePredicate}, {@link FailableFunction}, or
     * {@link FailableConsumer} may be applied, instead of
     * {@link Predicate}, {@link Function}, or {@link Consumer}. The idea is
     * to rewrite a code snippet like this:
     * <pre>
     *     final List&lt;O&gt; list;
     *     final Method m;
     *     final Function&lt;O,String&gt; mapper = (o) -&gt; {
     *         try {
     *             return (String) m.invoke(o);
     *         } catch (Throwable t) {
     *             throw Functions.rethrow(t);
     *         }
     *     };
     *     final List&lt;String&gt; strList = list.stream()
     *         .map(mapper).collect(Collectors.toList());
     *  </pre>
     *  as follows:
     *  <pre>
     *     final List&lt;O&gt; list;
     *     final Method m;
     *     final List&lt;String&gt; strList = Functions.stream(list.stream())
     *         .map((o) -&gt; (String) m.invoke(o)).collect(Collectors.toList());
     *  </pre>
     *  While the second version may not be <em>quite</em> as
     *  efficient (because it depends on the creation of additional,
     *  intermediate objects, of type FailableStream), it is much more
     *  concise, and readable, and meets the spirit of Lambdas better
     *  than the first version.
     * @param <O> The streams element type.
     * @param stream The stream, which is being converted.
     * @return The {@link FailableStream}, which has been created by
     *   converting the stream.
     */
    public static <O> FailableStream<O> stream(final Collection<O> stream) {
        return stream(stream.stream());
    }

    /**
     * A Collector type for arrays.
     *
     * @param <O> The array type.
     * @deprecated Use {@link org.apache.commons.lang3.stream.Streams.ArrayCollector}.
     */
    @Deprecated
    public static class ArrayCollector<O> implements Collector<O, List<O>, O[]> {
        private static final Set<Characteristics> characteristics = Collections.emptySet();
        private final Class<O> elementType;

        /**
         * Constructs a new instance for the given element type.
         *
         * @param elementType The element type.
         */
        public ArrayCollector(final Class<O> elementType) {
            this.elementType = elementType;
        }

        @Override
        public Supplier<List<O>> supplier() {
            return ArrayList::new;
        }

        @Override
        public BiConsumer<List<O>, O> accumulator() {
            return List::add;
        }

        @Override
        public BinaryOperator<List<O>> combiner() {
            return (left, right) -> {
                left.addAll(right);
                return left;
            };
        }

        @Override
        public Function<List<O>, O[]> finisher() {
            return list -> list.toArray(ArrayUtils.newInstance(elementType, list.size()));
        }

        @Override
        public Set<Characteristics> characteristics() {
            return characteristics;
        }
    }

    /**
     * Returns a {@link Collector} that accumulates the input elements into a
     * new array.
     *
     * @param pElementType Type of an element in the array.
     * @param <O> the type of the input elements
     * @return a {@link Collector} which collects all the input elements into an
     * array, in encounter order
     */
    public static <O> Collector<O, ?, O[]> toArray(final Class<O> pElementType) {
        return new ArrayCollector<>(pElementType);
    }
}
