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

/**
 * Immutable concrete manifestation of the {@link Pair} type.
 *
 * <p>#ThreadSafe# if the objects are threadsafe</p>
 * @since Lang 3.0
 * @author Matt Benson
 * @version $Id$
 *
 * @param <L> left generic type
 * @param <R> right generic type
 */
public class ImmutablePair<L, R> extends Pair<L, R> {
    /** Serialization version */
    private static final long serialVersionUID = 4954918890077093841L;

    /** Left object */
    public final L left;
    /** Right object */
    public final R right;

    /**
     * Create a new ImmutablePair instance.
     *
     * @param left the left value
     * @param right the right value
     */
    public ImmutablePair(L left, R right) {
        super();
        this.left = left;
        this.right = right;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public L getLeftElement() {
        return left;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public R getRightElement() {
        return right;
    }

    /**
     * {@link java.util.Map.Entry#setValue(Object)} implementation. Because this
     * class is immutable the {@code setValue()} operation is not supported.
     * Therefore always an exception is thrown.
     *
     * @param value the value to set
     * @return the old right value
     * @throws UnsupportedOperationException as this operation is not supported
     */
    public R setValue(R value) {
        throw new UnsupportedOperationException();
    }

    /**
     * Static fluent creation method for an {@link ImmutablePair}<L, R>:
     * <code>ImmutablePair.of(left, right)</code>
     *
     * @param <L> the left generic type
     * @param <R> the right generic type
     * @param left the let value
     * @param right the right value
     * @return ImmutablePair<L, R>(left, right)
     */
    public static <L, R> ImmutablePair<L, R> of(L left, R right) {
        return new ImmutablePair<L, R>(left, right);
    }
}
