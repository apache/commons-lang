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

import java.lang.reflect.Array;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;

import org.apache.commons.lang3.Validate;

/**
 * <p>Utility methods focusing on type inspection, particularly with regard to
 * generics.</p>
 * @author James Carman
 * @author Matt Benson
 * @since 3.0
 * @version $Id$
 */
public class TypeUtils {

    /**
     * Get the raw type of a Java type, given its context. Primarily for use
     * with {@link TypeVariable}s and {@link GenericArrayType}s, or when you do
     * not know the runtime type of <code>type</code>: if you know you have a
     * {@link Class} instance, it is already raw; if you know you have a
     * {@link ParameterizedType}, its raw type is only a method call away.
     * @param enclosingType context
     * @param type to read
     * @return Class<?>
     */
    // original code taken from commons [proxy]'s 2.0 branch, then kneaded until firm
    public static Class<?> getRawType(Type enclosingType, Type type) {
        if (type instanceof Class<?>) {
            // it is raw, no problem
            return (Class<?>) type;
        }
        if (type instanceof ParameterizedType) {
            // simple enough to get the raw type of a ParameterizedType
            return (Class<?>) ((ParameterizedType) type).getRawType();
        }
        if (type instanceof TypeVariable<?>) {
            Validate.notNull(enclosingType,
                    "Cannot get raw type of TypeVariable without enclosing type");
            // resolve the variable against the enclosing type, hope for the best (casting)
            return (Class<?>) resolveVariable(enclosingType, (TypeVariable<?>) type);
        }
        if (type instanceof GenericArrayType) {
            Validate.notNull(enclosingType,
                    "Cannot get raw type of GenericArrayType without enclosing type");
            // not included in original code, but not too difficult:  just have to get raw component type...
            Class<?> rawComponentType = getRawType(enclosingType, ((GenericArrayType) type)
                    .getGenericComponentType());
            // ...and know how to reflectively create array types, uncommon but not unheard of:
            return Array.newInstance(rawComponentType, 0).getClass();
        }
        throw new IllegalArgumentException(String.valueOf(type));
    }

    /**
     * We plan to return Class<?> from the top-level call, as evidenced by the
     * cast in the above method, but to handle recursion and falling back up the
     * graph, as it were, return Type
     * @param enclosingType
     * @param typeVar
     * @return Type resolved
     */
    // original code taken from commons [proxy]'s 2.0 branch, then kneaded until firm
    private static Type resolveVariable(Type enclosingType, TypeVariable<?> typeVar) {
        if (enclosingType instanceof ParameterizedType) {
            ParameterizedType parameterizedEnclosingType = (ParameterizedType) enclosingType;
            TypeVariable<?>[] typeVariables = getRawType(null,
                    parameterizedEnclosingType.getRawType()).getTypeParameters();
            //look for the matching variable:
            for (int i = 0; i < typeVariables.length; i++) {
                if (typeVariables[i].equals(typeVar)) {
                    return parameterizedEnclosingType.getActualTypeArguments()[i];
                }
            }
            //otherwise recurse to try against raw class
            Type result = resolveVariable(parameterizedEnclosingType.getRawType(), typeVar);
            //unroll variable if returned
            if (result instanceof TypeVariable<?>) {
                return resolveVariable(enclosingType, (TypeVariable<?>) result);
            }
            return result;
        }
        if (enclosingType instanceof Class<?>) {
            Class<?> enclosingClass = (Class<?>) enclosingType;
            Type result = null;
            Type genericSuperclass = enclosingClass.getGenericSuperclass();
            if (genericSuperclass != null && !Object.class.equals(genericSuperclass)) {
                result = resolveVariable(genericSuperclass, typeVar);
            }
            if (result == null) {
                for (Type genericInterface : enclosingClass.getGenericInterfaces()) {
                    result = resolveVariable(genericInterface, typeVar);
                    if (result != null) {
                        break;
                    }
                }
            }
            if (result != null) {
                return result;
            }
        }
        throw new IllegalArgumentException(String.valueOf(typeVar));
    }

}
