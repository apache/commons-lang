/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.commons.lang3;

import java.io.IOException;
import java.util.Iterator;
import java.util.StringJoiner;
import java.util.function.Supplier;

import org.apache.commons.lang3.exception.UncheckedException;
import org.apache.commons.lang3.function.FailableBiConsumer;

/**
 * Joins an array or {@link Iterable} into an existing {@link Appendable} like a {@link StringBuilder}; with the goal for call sites to avoid creating
 * intermediary Strings. This is like {@link String#join(CharSequence, CharSequence...)}, {@link String#join(CharSequence, Iterable)}, and {@link StringJoiner}.
 * <p>
 * Keep an instance in a (static) variable for efficient joining into an {@link Appendable} or {@link StringBuilder} without creating temporary Strings.
 * </p>
 * <p>
 * Use the builder and instance methods to reuse the same kind of joining prefix, suffix, delimiter, and string conversion.
 * </p>
 * <p>
 * For example:
 * </p>
 *
 * <pre>{@code
 * // A reuseable instance
 * private static final AppendableJoiner<Object> JOINER = AppendableJoiner.builder()
 *     .setPrefix("[")
 *     .setSuffix("]")
 *     .setDelimiter(", ")
 *     .get();
 * }
 * ...
 * // Builds straight into a StringBuilder:
 * StringBuilder sbuilder = new StringBuilder("1");
 * JOINER.join(sbuilder, "A", "B");
 * sbuilder.append("2");
 * JOINER.join(sbuilder, "C", "D");
 * sbuilder.append("3");
 * // Returns "1[A, B]2[C, D]3"
 * return sbuilder.toString();
 * }</pre>
 * <p>
 * To provide a custom Object element to {@link CharSequence} converter, call {@link Builder#setElementAppender(FailableBiConsumer)}, for example:
 * </p>
 *
 * <pre>{@code
 * private static final AppendableJoiner<Item> JOINER = AppendableJoiner.builder()
 *     .setElementAppender(e -> (a, e) -> a.append(e.getFoo())
 *                                        a.append(e.getBar())
 *                                        a.append('!'))
 *     ...
 *     .get();
 * }
 * }</pre>
 * <p>
 * This class is immutable and thread-safe.
 * </p>
 *
 * @param <T> the type of elements to join.
 * @see Appendable
 * @see StringBuilder
 * @see String#join(CharSequence, CharSequence...)
 * @see String#join(CharSequence, Iterable)
 * @see StringJoiner
 * @since 3.15.0
 */
public final class AppendableJoiner<T> {

    /**
     * Builds instances of {@link AppendableJoiner}.
     *
     * @param <T> the type of elements to join.
     */
    public static final class Builder<T> implements Supplier<AppendableJoiner<T>> {

        /** The sequence of characters to be used at the beginning. */
        private CharSequence prefix;

        /** The sequence of characters to be used at the end. */
        private CharSequence suffix;

        /** The delimiter that separates each element. */
        private CharSequence delimiter;

        /** The consumer used to render each element of type {@code T} onto an {@link Appendable}. */
        private FailableBiConsumer<Appendable, T, IOException> appender;

        /**
         * Constructs a new instance.
         */
        Builder() {
            // empty
        }

        /**
         * Gets a new instance of {@link AppendableJoiner}.
         */
        @Override
        public AppendableJoiner<T> get() {
            return new AppendableJoiner<>(prefix, suffix, delimiter, appender);
        }

        /**
         * Sets the delimiter that separates each element.
         *
         * @param delimiter The delimiter that separates each element.
         * @return this instance.
         */
        public Builder<T> setDelimiter(final CharSequence delimiter) {
            this.delimiter = delimiter;
            return this;
        }

        /**
         * Sets the consumer used to render each element of type {@code T} onto an {@link Appendable}.
         *
         * @param appender The consumer used to render each element of type {@code T} onto an {@link Appendable}.
         * @return this instance.
         */
        public Builder<T> setElementAppender(final FailableBiConsumer<Appendable, T, IOException> appender) {
            this.appender = appender;
            return this;
        }

        /**
         * Sets the sequence of characters to be used at the beginning.
         *
         * @param prefix The sequence of characters to be used at the beginning.
         * @return this instance.
         */
        public Builder<T> setPrefix(final CharSequence prefix) {
            this.prefix = prefix;
            return this;
        }

        /**
         * Sets the sequence of characters to be used at the end.
         *
         * @param suffix The sequence of characters to be used at the end.
         * @return this instance.
         */
        public Builder<T> setSuffix(final CharSequence suffix) {
            this.suffix = suffix;
            return this;
        }

    }

    /**
     * Creates a new builder.
     *
     * @param <T> The type of elements.
     * @return a new builder.
     */
    public static <T> Builder<T> builder() {
        return new Builder<>();
    }

    /** Could be public in the future, in some form. */
    @SafeVarargs
    static <A extends Appendable, T> A joinA(final A appendable, final CharSequence prefix, final CharSequence suffix, final CharSequence delimiter,
            final FailableBiConsumer<Appendable, T, IOException> appender, final T... elements) throws IOException {
        return joinArray(appendable, prefix, suffix, delimiter, appender, elements);
    }

    private static <A extends Appendable, T> A joinArray(final A appendable, final CharSequence prefix, final CharSequence suffix, final CharSequence delimiter,
            final FailableBiConsumer<Appendable, T, IOException> appender, final T[] elements) throws IOException {
        appendable.append(prefix);
        if (elements != null) {
            if (elements.length > 0) {
                appender.accept(appendable, elements[0]);
            }
            for (int i = 1; i < elements.length; i++) {
                appendable.append(delimiter);
                appender.accept(appendable, elements[i]);
            }
        }
        appendable.append(suffix);
        return appendable;
    }

    /** Could be public in the future, in some form. */
    static <T> StringBuilder joinI(final StringBuilder stringBuilder, final CharSequence prefix, final CharSequence suffix, final CharSequence delimiter,
            final FailableBiConsumer<Appendable, T, IOException> appender, final Iterable<T> elements) {
        try {
            return joinIterable(stringBuilder, prefix, suffix, delimiter, appender, elements);
        } catch (final IOException e) {
            // Cannot happen with a StringBuilder.
            throw new UncheckedException(e);
        }
    }

    private static <A extends Appendable, T> A joinIterable(final A appendable, final CharSequence prefix, final CharSequence suffix,
            final CharSequence delimiter, final FailableBiConsumer<Appendable, T, IOException> appender, final Iterable<T> elements) throws IOException {
        appendable.append(prefix);
        if (elements != null) {
            final Iterator<T> iterator = elements.iterator();
            if (iterator.hasNext()) {
                appender.accept(appendable, iterator.next());
            }
            while (iterator.hasNext()) {
                appendable.append(delimiter);
                appender.accept(appendable, iterator.next());
            }
        }
        appendable.append(suffix);
        return appendable;
    }

    /** Could be public in the future, in some form. */
    @SafeVarargs
    static <T> StringBuilder joinSB(final StringBuilder stringBuilder, final CharSequence prefix, final CharSequence suffix, final CharSequence delimiter,
            final FailableBiConsumer<Appendable, T, IOException> appender, final T... elements) {
        try {
            return joinArray(stringBuilder, prefix, suffix, delimiter, appender, elements);
        } catch (final IOException e) {
            // Cannot happen with a StringBuilder.
            throw new UncheckedException(e);
        }
    }

    private static CharSequence nonNull(final CharSequence value) {
        return value != null ? value : StringUtils.EMPTY;
    }

    /** The sequence of characters to be used at the beginning. */
    private final CharSequence prefix;

    /** The sequence of characters to be used at the end. */
    private final CharSequence suffix;

    /** The delimiter that separates each element. */
    private final CharSequence delimiter;

    private final FailableBiConsumer<Appendable, T, IOException> appender;

    /**
     * Constructs a new instance.
     */
    private AppendableJoiner(final CharSequence prefix, final CharSequence suffix, final CharSequence delimiter,
            final FailableBiConsumer<Appendable, T, IOException> appender) {
        this.prefix = nonNull(prefix);
        this.suffix = nonNull(suffix);
        this.delimiter = nonNull(delimiter);
        this.appender = appender != null ? appender : (a, e) -> a.append(String.valueOf(e));
    }

    /**
     * Joins stringified objects from the given Iterable into a StringBuilder.
     *
     * @param stringBuilder The target.
     * @param elements      The source.
     * @return The given StringBuilder.
     */
    public StringBuilder join(final StringBuilder stringBuilder, final Iterable<T> elements) {
        return joinI(stringBuilder, prefix, suffix, delimiter, appender, elements);
    }

    /**
     * Joins stringified objects from the given array into a StringBuilder.
     *
     * @param stringBuilder The target.
     * @param elements      The source.
     * @return the given target StringBuilder.
     */
    public StringBuilder join(final StringBuilder stringBuilder, @SuppressWarnings("unchecked") final T... elements) {
        return joinSB(stringBuilder, prefix, suffix, delimiter, appender, elements);
    }

    /**
     * Joins stringified objects from the given Iterable into an Appendable.
     *
     * @param <A>        the Appendable type.
     * @param appendable The target.
     * @param elements   The source.
     * @return The given StringBuilder.
     * @throws IOException If an I/O error occurs
     */
    public <A extends Appendable> A joinA(final A appendable, final Iterable<T> elements) throws IOException {
        return joinIterable(appendable, prefix, suffix, delimiter, appender, elements);
    }

    /**
     * Joins stringified objects from the given array into an Appendable.
     *
     * @param <A>        the Appendable type.
     * @param appendable The target.
     * @param elements   The source.
     * @return The given StringBuilder.
     * @throws IOException If an I/O error occurs
     */
    public <A extends Appendable> A joinA(final A appendable, @SuppressWarnings("unchecked") final T... elements) throws IOException {
        return joinA(appendable, prefix, suffix, delimiter, appender, elements);
    }

}
