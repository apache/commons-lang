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

import java.io.Serializable;
import java.util.Map;

import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * Abstract Pair (or 2-element Tuple).
 *
 * @since Lang 3.0
 * @version $Id$
 */
public abstract class Pair<L, R> implements Serializable, Map.Entry<L, R> {
    /** Serialization version */
    private static final long serialVersionUID = 4954918890077093841L;

    /**
     * Get the "left" element of the pair.
     * @return L
     */
    public abstract L getLeftElement();

    /**
     * Get the "right" element of the pair.
     * @return R
     */
    public abstract R getRightElement();

    /**
     * Return {@link #getLeftElement()} as a {@link java.util.Map.Entry}'s key.
     * @return L
     */
    public final L getKey() {
        return getLeftElement();
    }

    /**
     * Return {@link #getRightElement()} as a {@link java.util.Map.Entry}'s value.
     * @return R
     */
    public R getValue() {
        return getRightElement();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        if (obj instanceof Pair<?, ?> == false) {
            return false;
        }
        Pair<?, ?> other = (Pair<?, ?>) obj;
        return ObjectUtils.equals(getLeftElement(), other.getLeftElement())
                && ObjectUtils.equals(getRightElement(), other.getRightElement());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        // TODO should the hashCodeBuilder be seeded per concrete type?
        return new HashCodeBuilder().append(getLeftElement()).append(getRightElement())
                .toHashCode();
    }

    /**
     * Returns a String representation of the Pair in the form: (L,R)
     * @return a string for this object
     */
    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder(ClassUtils.getShortClassName(this, null));
        builder.append("(");
        builder.append(getLeftElement());
        builder.append(",");
        builder.append(getRightElement());
        builder.append(")");
        return builder.toString();
    }

    /**
     * Static fluent creation method for a {@link Pair}<L, R>:
     * <code>Pair.of(left, right)</code>
     * @param <L> the left generic type
     * @param <R> the right generic type
     * @param left the left value
     * @param right the right value
     * @return ImmutablePair<L, R>(left, right)
     */
    public static <L, R> Pair<L, R> of(L left, R right) {
        return new ImmutablePair<L, R>(left, right);
    }
}
