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

import java.util.Map;

/**
 * Mutable concrete manifestation of the {@link Pair} type.
 * 
 * <p>#ThreadSafe# if the objects are threadsafe</p>
 * @since Lang 3.0
 * @author Matt Benson
 * @version $Id$
 * 
 * @param <L> left generic type
 * @param <R> right generic type
 */
public class MutablePair<L, R> extends Pair<L, R> {
    /** Serialization version */
    private static final long serialVersionUID = 4954918890077093841L;

    private L leftElement;
    private R rightElement;

    /**
     * Create a new MutablePair instance.
     */
    public MutablePair() {
        super();
    }

    /**
     * Create a new MutablePair instance.
     * 
     * @param leftElement
     * @param rightElement
     */
    public MutablePair(L leftElement, R rightElement) {
        super();
        this.leftElement = leftElement;
        this.rightElement = rightElement;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public L getLeftElement() {
        return leftElement;
    }

    /**
     * Set the left element of the pair.
     * @param leftElement
     */
    public void setLeftElement(L leftElement) {
        this.leftElement = leftElement;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public R getRightElement() {
        return rightElement;
    }

    /**
     * Set the right element of the pair.
     * @param rightElement
     */
    public void setRightElement(R rightElement) {
        this.rightElement = rightElement;
    }

    /**
     * Implement {@link Map.Entry#setValue(Object)}.
     * @param value value (<code>rightElement</code>) to set
     */
    public R setValue(R value) {
        R result = getRightElement();
        setRightElement(value);
        return result;
    }

    /**
     * Static fluent creation method for a {@link MutablePair}<L, R>:
     * <code>MutablePair.of(left, right)</code>
     * @param <L>
     * @param <R>
     * @param left
     * @param right
     * @return MutablePair<L, R>(left, right)
     */
    public static <L, R> MutablePair<L, R> of(L left, R right) {
        return new MutablePair<L, R>(left, right);
    }
}
