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

import org.apache.commons.lang3.stream.FailableStream;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collector;
import java.util.stream.Stream;

/**
 * Provides utility functions, and classes for working with the
 * {@code java.util.stream} package, or more generally, with Java 8 lambdas. More
 * specifically, it attempts to address the fact that lambdas are supposed
 * not to throw Exceptions, at least not checked Exceptions, AKA instances
 * of {@link Exception}. This enforces the use of constructs like
 * <pre>
 *     Consumer&lt;java.lang.reflect.Method&gt; consumer = (m) -&gt; {
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
 */
public class Streams {
    /**
     * Converts the given {@link Stream stream} into a {@link FailableStream}.
     *
     * @param <O> The stream element type.
     * @param stream The stream which is being converted.
     * @return The {@link FailableStream} which has been created by converting the stream.
     */
    public static <O> FailableStream<O> stream(final Stream<O> stream) {
        return new FailableStream<>(stream);
    }

    /**
     * Converts the given {@link Collection} into a {@link FailableStream}.
     *
     * @param <O> The collection element type.
     * @param collection The collection from which the stream is to be obtained.
     * @return The {@link FailableStream} which has been created by converting
     * the stream obtained from the collection.
     */
    public static <O> FailableStream<O> stream(final Collection<O> collection) {
        return stream(collection.stream());
    }

    public static class ArrayCollector<O> implements Collector<O, List<O>, O[]> {
        private static final Set<Characteristics> characteristics = Collections.emptySet();
        private final Class<O> elementType;

        public ArrayCollector(final Class<O> elementType) {
            this.elementType = elementType;
        }

        @Override
        public Supplier<List<O>> supplier() {
            return () -> new ArrayList<>();
        }

        @Override
        public BiConsumer<List<O>, O> accumulator() {
            return (list, o) -> {
                list.add(o);
            };
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
            return (list) -> {
                @SuppressWarnings("unchecked")
                final O[] array = (O[]) Array.newInstance(elementType, list.size());
                return list.toArray(array);
            };
        }

        @Override
        public Set<Characteristics> characteristics() {
            return characteristics;
        }
    }

    /**
     * Returns a {@code Collector} that accumulates the input elements into a
     * new array.
     *
     * @param pElementType Type of an element in the array.
     * @param <O> the type of the input elements
     * @return a {@code Collector} which collects all the input elements into an
     * array, in encounter order
     */
    public static <O extends Object> Collector<O, ?, O[]> toArray(final Class<O> pElementType) {
        return new ArrayCollector<>(pElementType);
    }
}
