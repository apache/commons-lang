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

import java.util.Collections;
import java.util.Objects;
import java.util.Set;
import java.util.StringJoiner;
import java.util.function.BiConsumer;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collector;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;

/**
 * Implementations of {@link Collector} that implement various useful reduction operations.
 * <p>
 * This class is called {@code LangCollectors} instead of {@code Collectors} to avoid clashes with {@link Collectors}.
 * </p>
 *
 * @since 3.13.0
 */
public final class LangCollectors {

    /**
     * Simple implementation class for {@code Collector}.
     *
     * @param <T> the type of elements to be collected
     * @param <R> the type of the result
     */
    private static class SimpleCollector<T, A, R> implements Collector<T, A, R> {

        private final BiConsumer<A, T> accumulator;
        private final Set<Characteristics> characteristics;
        private final BinaryOperator<A> combiner;
        private final Function<A, R> finisher;
        private final Supplier<A> supplier;

        private SimpleCollector(final Supplier<A> supplier, final BiConsumer<A, T> accumulator, final BinaryOperator<A> combiner, final Function<A, R> finisher,
            final Set<Characteristics> characteristics) {
            this.supplier = supplier;
            this.accumulator = accumulator;
            this.combiner = combiner;
            this.finisher = finisher;
            this.characteristics = characteristics;
        }

        @Override
        public BiConsumer<A, T> accumulator() {
            return accumulator;
        }

        @Override
        public Set<Characteristics> characteristics() {
            return characteristics;
        }

        @Override
        public BinaryOperator<A> combiner() {
            return combiner;
        }

        @Override
        public Function<A, R> finisher() {
            return finisher;
        }

        @Override
        public Supplier<A> supplier() {
            return supplier;
        }
    }

    private static final Set<Collector.Characteristics> CH_NOID = Collections.emptySet();

    /**
     * Returns a {@code Collector} that concatenates the input elements, separated by the specified delimiter, in encounter
     * order.
     * <p>
     * This is a variation of {@link Collectors#joining()} that works with any element class, not just {@code CharSequence}.
     * </p>
     *
     * @return A {@code Collector} which concatenates Object elements, separated by the specified delimiter, in encounter
     *         order.
     */
    public static Collector<Object, ?, String> joining() {
        return new SimpleCollector<>(StringBuilder::new, StringBuilder::append, StringBuilder::append, StringBuilder::toString, CH_NOID);
    }

    /**
     * Returns a {@code Collector} that concatenates the input elements, separated by the specified delimiter, in encounter
     * order.
     * <p>
     * This is a variation of {@link Collectors#joining(CharSequence)} that works with any element class, not just
     * {@code CharSequence}.
     * </p>
     *
     * @param delimiter the delimiter to be used between each element.
     * @return A {@code Collector} which concatenates Object elements, separated by the specified delimiter, in encounter
     *         order.
     */
    public static Collector<Object, ?, String> joining(final CharSequence delimiter) {
        return joining(delimiter, StringUtils.EMPTY, StringUtils.EMPTY);
    }

    /**
     * Returns a {@code Collector} that concatenates the input elements, separated by the specified delimiter, with the
     * specified prefix and suffix, in encounter order.
     * <p>
     * This is a variation of {@link Collectors#joining(CharSequence, CharSequence, CharSequence)} that works with any
     * element class, not just {@code CharSequence}.
     * </p>
     *
     * @param delimiter the delimiter to be used between each element
     * @param prefix the sequence of characters to be used at the beginning of the joined result
     * @param suffix the sequence of characters to be used at the end of the joined result
     * @return A {@code Collector} which concatenates CharSequence elements, separated by the specified delimiter, in
     *         encounter order
     */
    public static Collector<Object, ?, String> joining(final CharSequence delimiter, final CharSequence prefix, final CharSequence suffix) {
        return joining(delimiter, prefix, suffix, Objects::toString);
    }

    /**
     * Returns a {@code Collector} that concatenates the input elements, separated by the specified delimiter, with the
     * specified prefix and suffix, in encounter order.
     * <p>
     * This is a variation of {@link Collectors#joining(CharSequence, CharSequence, CharSequence)} that works with any
     * element class, not just {@code CharSequence}.
     * </p>
     *
     * @param delimiter the delimiter to be used between each element
     * @param prefix the sequence of characters to be used at the beginning of the joined result
     * @param suffix the sequence of characters to be used at the end of the joined result
     * @param toString A function that takes an Object and returns a non-null String.
     * @return A {@code Collector} which concatenates CharSequence elements, separated by the specified delimiter, in
     *         encounter order
     */
    public static Collector<Object, ?, String> joining(final CharSequence delimiter, final CharSequence prefix, final CharSequence suffix,
        final Function<Object, String> toString) {
        return new SimpleCollector<>(() -> new StringJoiner(delimiter, prefix, suffix), (a, t) -> a.add(toString.apply(t)), StringJoiner::merge,
            StringJoiner::toString, CH_NOID);
    }

    private LangCollectors() {
        // No instance
    }

}
