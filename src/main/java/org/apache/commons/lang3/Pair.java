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

import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A basic immutable Object pair.
 *
 * <p>#ThreadSafe# if the objects are threadsafe</p>
 * @since Lang 3.0
 * @author Matt Benson
 * @version $Id$
 */
public final class Pair<L, R> implements Serializable {
    /** Serialization version */
    private static final long serialVersionUID = 4954918890077093841L;

    /** Left object */
    public final L left;

    /** Right object */
    public final R right;

    /**
     * Create a new Pair instance.
     * @param left
     * @param right
     */
    public Pair(L left, R right) {
        this.left = left;
        this.right = right;
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
        return ObjectUtils.equals(left, other.left) && ObjectUtils.equals(right, other.right);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(left).append(right).toHashCode();
    }

    /**
     * Returns a String representation of the Pair in the form: (L,R)
     */
    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("(");
        builder.append(left);
        builder.append(",");
        builder.append(right);
        builder.append(")");
        return builder.toString();
    }

    /**
     * Static fluent creation method for a Pair<L, R>:  <code>Pair.of(left, right)</code>
     * @param <L>
     * @param <R>
     * @param left
     * @param right
     * @return Pair<L, R>(left, right)
     */
    public static <L, R> Pair<L, R> of(L left, R right) {
        return new Pair<L, R>(left, right);
    }
}
