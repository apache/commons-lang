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
package org.apache.commons.lang3.builder;

import java.util.Collection;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Supplier;

import org.apache.commons.lang3.ClassUtils;
import org.apache.commons.lang3.mutable.MutableBoolean;

/**
 * Works with {@link ToStringBuilder} to create a "deep" {@code toString}.
 * <p>
 * To use this class write code as follows:
 * </p>
 *
 * <pre>
 * public class Job {
 *   String title;
 *   ...
 * }
 *
 * public class Person {
 *   String name;
 *   int age;
 *   boolean smoker;
 *   Job job;
 *
 *   ...
 *
 *   public String toString() {
 *     return new ReflectionToStringBuilder(this, new RecursiveToStringStyle()).toString();
 *   }
 * }
 * </pre>
 * <p>
 * This will produce a toString of the format: {@code Person@7f54[name=Stephen,age=29,smoker=false,job=Job@43cd2[title=Manager]]}
 * </p>
 * <p>
 * Graph safety: within one top-level {@code toString()} call, each object is rendered in detail at most once. A second (identity-equal) occurrence of an object
 * (whether through a true cycle or through a shared (acyclic) reference) is rendered in the abbreviated {@code Object.toString()} format instead of being
 * re-traversed. This keeps traversal cost linear in the size of the object graph; without it, shared references (reference "diamonds") would be re-traversed
 * exponentially. An optional output-length limit can be set via {@link RecursiveToStringStyle.Builder#setMaxOutputLength(int)}; once the produced string
 * reaches the limit, further nested objects are replaced by a {@code "...<truncated>"} marker.
 * </p>
 *
 * @since 3.2
 */
public class RecursiveToStringStyle extends ToStringStyle {

    /**
     * Builder for {@link RecursiveToStringStyle} instances.
     */
    public static class Builder implements Supplier<RecursiveToStringStyle> {

        private int maxOutputLength;

        private Builder() {
            this.maxOutputLength = 0;
        }

        @Override
        public RecursiveToStringStyle get() {
            return new RecursiveToStringStyle(this);
        }

        /**
         * Sets the maximum length the output buffer may reach before nested objects are elided with {@link #TRUNCATED_TEXT}; {@code 0} (the default) means
         * unlimited. This is a throttle, not an exact bound: objects already being rendered may still append their shallow content.
         *
         * @param maxOutputLength once the produced string reaches this length, further nested objects are replaced by a {@code "...<truncated>"} marker;
         *                        {@code 0} means unlimited.
         * @return this builder for chaining.
         */
        public Builder setMaxOutputLength(final int maxOutputLength) {
            this.maxOutputLength = maxOutputLength;
            return this;
        }
    }

    /**
     * Required for serialization support.
     *
     * @see java.io.Serializable
     */
    private static final long serialVersionUID = 1L;

    /**
     * Marker appended in place of a nested object once {@link #maxOutputLength} is reached.
     */
    private static final String TRUNCATED_TEXT = "...<truncated>";

    /**
     * Creates a new {@link Builder} for {@link RecursiveToStringStyle} instances.
     *
     * @return a new {@link Builder} for {@link RecursiveToStringStyle} instances.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Maximum length the output buffer may reach before nested objects are elided with
     * {@link #TRUNCATED_TEXT}; {@code 0} (the default) means unlimited. This is a throttle,
     * not an exact bound: objects already being rendered may still append their shallow content.
     */
    private final int maxOutputLength;

    /**
     * Constructs a new instance with no output-length limit.
     */
    public RecursiveToStringStyle() {
        this(RecursiveToStringStyle.builder());
    }

    private RecursiveToStringStyle(final Builder builder) {
        this.maxOutputLength = builder.maxOutputLength;
    }

    /**
     * Constructs a new instance with an output-length limit.
     *
     * @param maxOutputLength once the produced string reaches this length, further nested
     *        objects are replaced by a {@code "...<truncated>"} marker; {@code 0} means unlimited.
     * @since 3.21.0
     */
    public RecursiveToStringStyle(final int maxOutputLength) {
        this.maxOutputLength = maxOutputLength;
    }

    /**
     * Tests whether or not to recursively format the given {@link Class}.
     * <p>
     * By default, this method always filters out the following:
     * </p>
     * <ul>
     * <li><a href="https://docs.oracle.com/javase/specs/jls/se25/html/jls-5.html#jls-5.1.7">Boxed primitives</a>, see {@link ClassUtils#isPrimitiveWrapper(Class)}
     * <li>{@link String}</li>
     * <li>{@link Number} subclasses</li>
     * <li>{@link AtomicBoolean}</li>
     * <li>{@link MutableBoolean}</li>
     * </ul>
     *
     * @param clazz The class to test.
     * @return Whether or not to recursively format instances of the given {@link Class}.
     */
    protected boolean accept(final Class<?> clazz) {
        // @formatter:off
        return !ClassUtils.isPrimitiveWrapper(clazz) &&
               !String.class.equals(clazz) &&
               !Number.class.isAssignableFrom(clazz) &&
               !AtomicBoolean.class.equals(clazz) &&
               !MutableBoolean.class.equals(clazz);
        // @formatter:on
    }

    @Override
    protected void appendDetail(final StringBuffer buffer, final String fieldName, final Collection<?> coll) {
        appendClassName(buffer, coll);
        appendIdentityHashCode(buffer, coll);
        appendDetail(buffer, fieldName, coll.toArray());
    }

    @Override
    public void appendDetail(final StringBuffer buffer, final String fieldName, final Object value) {
        if (value != null && accept(value.getClass())) {
            buffer.append(ReflectionToStringBuilder.toString(value, this));
        } else {
            super.appendDetail(buffer, fieldName, value);
        }
    }

    /**
     * {@inheritDoc}
     *
     * <p>
     * In addition to the cycle check performed by the superclass, this implementation keeps a
     * per-thread set of objects already rendered in detail during the current top-level call.
     * Values that this style would traverse structurally (see {@link #accept(Class)}) are rendered
     * in detail at most once; later identity-equal occurrences are appended in the abbreviated
     * {@code Object.toString()} format. This bounds the traversal to one visit per object, so
     * shared (acyclic) references cannot cause exponential re-traversal. If an output-length limit
     * was configured and the buffer has reached it, a {@code "...<truncated>"} marker is appended
     * instead of the value.
     * </p>
     */
    @Override
    protected void appendInternal(final StringBuffer buffer, final String fieldName, final Object value, final boolean detail) {
        if (detail && value != null && accept(value.getClass())
                && !(value instanceof Number || value instanceof Boolean || value instanceof Character)) {
            if (isVisited(value)) {
                appendCyclicObject(buffer, fieldName, value);
                return;
            }
            if (maxOutputLength > 0 && buffer.length() >= maxOutputLength) {
                buffer.append(TRUNCATED_TEXT);
                return;
            }
            markVisited(value);
        }
        super.appendInternal(buffer, fieldName, value, detail);
    }
}
