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
package org.apache.commons.lang3.reflect;

import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;

import org.apache.commons.lang3.Validate;

/**
 * Type literal comparable to {@code javax.enterprise.util.TypeLiteral},
 * made generally available outside the JEE context. Allows the passing around of
 * a "token" that represents a type in a typesafe manner, as opposed to
 * passing the (non-parameterized) {@link Type} object itself.
 * Additionally {@link TypeLiteral} implements the {@link Typed} interface which
 * is a generalization of this concept. It is suggested that APIs be defined in
 * terms of the interface, which others might implemented in custom classes.
 */
public abstract class TypeLiteral<T> implements Typed<T> {

    @SuppressWarnings("rawtypes")
    private static final TypeVariable<Class<TypeLiteral>> T = TypeLiteral.class.getTypeParameters()[0];

    /**
     * Represented type.
     */
    public final Type value;

    private final String toString;

    protected TypeLiteral() {
        this.value =
            Validate.notNull(TypeUtils.getTypeArguments(getClass(), TypeLiteral.class).get(T),
                "%s does not assign type parameter %s", getClass(), TypeUtils.toLongString(T));

        this.toString = String.format("%s<%s>", TypeLiteral.class.getSimpleName(), TypeUtils.toString(value));
    }

    @Override
    public final boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        if (obj instanceof TypeLiteral == false) {
            return false;
        }
        final TypeLiteral<?> other = (TypeLiteral<?>) obj;
        return TypeUtils.equals(value, other.value);
    }

    @Override 
    public int hashCode() {
        return 37 << 4 | value.hashCode();
    }

    @Override
    public String toString() {
        return toString;
    }

    @Override
    public Type getType() {
        return value;
    }
}
