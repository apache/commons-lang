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
package org.apache.commons.lang3.builder;

import org.apache.commons.lang3.function.FailableFunction;

import java.util.Objects;
import java.util.function.Function;

/**
 * An extension of {@link EqualsBuilder} that is typed and meant to append field getter functions.
 * it does the nullity and class equality checks before checking the appended values.
 *
 * <p>Typical use for the code is as follows:</p>
 * <pre>
 * public boolean equals(Object obj) {
 *     return new TypedEqualsBuilder&lt;&gt;(this)
 *         .appendBaseObject(obj)
 *         .append(TestObject::getA)
 *         .append(TestObject::getB)
 *         .isEquals();
 *     }
 * </pre>
 *
 * @param <T> the type of the compared object.
 *
 * @since 3.14.0
 */
public class TypedEqualsBuilder<T> extends EqualsBuilder {

    private final T currentInstance;

    private boolean sameReference = false;
    private T other;

    @SuppressWarnings("unchecked")
    public TypedEqualsBuilder(T currentInstance, Object other) {
        Objects.requireNonNull(currentInstance);
        this.currentInstance = currentInstance;
        if (currentInstance == other) {
            sameReference = true;
            return;
        }
        Class<T> currentInstanceClass = (Class<T>) currentInstance.getClass();
        if (other == null || currentInstanceClass != other.getClass()) {
            isEquals = false;
            return;
        }
        this.other = (T) other;
    }

    @Override
    protected boolean interruptEqualityCheck() {
        return sameReference || super.interruptEqualityCheck();
    }

    public TypedEqualsBuilder<T> append(FailableFunction<T, ?, Exception> extractor) {
        if (interruptEqualityCheck()) {
            return this;
        }
        try {
            super.append(extractor.apply(currentInstance), extractor.apply(other));
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        return this;
    }
}
