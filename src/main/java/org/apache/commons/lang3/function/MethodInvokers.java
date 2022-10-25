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

package org.apache.commons.lang3.function;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandleProxies;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.Method;
import java.util.Objects;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

import org.apache.commons.lang3.exception.UncheckedIllegalAccessException;

/**
 * Converts {@link Method} objects to lambdas.
 * <p>
 * More specifically, produces instances of single-method interfaces which redirect calls to methods; see
 * {@link #asInterfaceInstance(Class, Method)}.
 * </p>
 * <h2>Calling supplier methods with no arguments</h2>
 * <p>
 * If the interface's single-method defines no arguments, use {@link #asFunction(Method)} and then apply the function
 * passing in the object to receive the method call.
 * </p>
 * <p>
 * For example to invoke {@link String#length()}:
 * </p>
 *
 * <pre>
 * final Method method = String.class.getMethod("length");
 * final Function&lt;String, Integer&gt; function = MethodInvokers.asFunction(method);
 * assertEquals(3, function.apply("ABC"));
 * </pre>
 *
 * <h2>Calling function methods with one argument</h2>
 * <p>
 * If the interface's single-method defines one argument, use {@link #asBiFunction(Method)} and then apply the function
 * passing in the object to receive the method call. The second argument to the function is the only argument to the
 * method.
 * </p>
 * <p>
 * For example to invoke {@link String#charAt(int)}:
 * </p>
 *
 * <pre>
 * final Method method = String.class.getMethod("charAt", int.class);
 * final BiFunction&lt;String, Integer, Character&gt; function = MethodInvokers.asBiFunction(method);
 * assertEquals('C', function.apply("ABC", 2));
 * </pre>
 *
 * @since 3.13.0
 */
public final class MethodInvokers {

    /**
     * Produces a {@link BiConsumer} for a given <em>consumer</em> Method. For example, a classic setter method (as opposed
     * to a fluent setter). You call the BiConsumer with two arguments: (1) the object receiving the method call, and (2)
     * the method argument.
     *
     * @param <T> the type of the first argument to the operation: The type containing the Method.
     * @param <U> the type of the second argument to the operation: The type of the method argument.
     * @param method the method to invoke.
     * @return a correctly-typed wrapper for the given target.
     */
    @SuppressWarnings("unchecked")
    public static <T, U> BiConsumer<T, U> asBiConsumer(final Method method) {
        return asInterfaceInstance(BiConsumer.class, method);
    }

    /**
     * Produces a {@link BiFunction} for a given a <em>function</em> Method. You call the BiFunction with two arguments: (1)
     * the object receiving the method call, and (2) the method argument. The BiFunction return type must match the method's
     * return type.
     * <p>
     * For example to invoke {@link String#charAt(int)}:
     * </p>
     *
     * <pre>
     * final Method method = String.class.getMethod("charAt", int.class);
     * final BiFunction&lt;String, Integer, Character&gt; function = MethodInvokers.asBiFunction(method);
     * assertEquals('C', function.apply("ABC", 2));
     * </pre>
     *
     * @param <T> the type of the first argument to the function: The type containing the method.
     * @param <U> the type of the second argument to the function: the method argument type.
     * @param <R> the type of the result of the function: The method return type.
     * @param method the method to invoke.
     * @return a correctly-typed wrapper for the given target.
     */
    @SuppressWarnings("unchecked")
    public static <T, U, R> BiFunction<T, U, R> asBiFunction(final Method method) {
        return asInterfaceInstance(BiFunction.class, method);
    }

    /**
     * Produces a {@link FailableBiConsumer} for a given <em>consumer</em> Method. For example, a classic setter method (as
     * opposed to a fluent setter). You call the FailableBiConsumer with two arguments: (1) the object receiving the method
     * call, and (2) the method argument.
     *
     * @param <T> the type of the first argument to the operation: The type containing the Method.
     * @param <U> the type of the second argument to the operation: The type of the method argument.
     * @param method the method to invoke.
     * @return a correctly-typed wrapper for the given target.
     */
    @SuppressWarnings("unchecked")
    public static <T, U> FailableBiConsumer<T, U, Throwable> asFailableBiConsumer(final Method method) {
        return asInterfaceInstance(FailableBiConsumer.class, method);
    }

    /**
     * Produces a {@link FailableBiFunction} for a given a <em>function</em> Method. You call the FailableBiFunction with
     * two arguments: (1) the object receiving the method call, and (2) the method argument. The BiFunction return type must
     * match the method's return type.
     *
     * @param <T> the type of the first argument to the function: The type containing the method.
     * @param <U> the type of the second argument to the function: the method argument type.
     * @param <R> the type of the result of the function: The method return type.
     * @param method the method to invoke.
     * @return a correctly-typed wrapper for the given target.
     */
    @SuppressWarnings("unchecked")
    public static <T, U, R> FailableBiFunction<T, U, R, Throwable> asFailableBiFunction(final Method method) {
        return asInterfaceInstance(FailableBiFunction.class, method);
    }

    /**
     * Produces a {@link FailableFunction} for a given a <em>supplier</em> Method. You call the Function with one argument:
     * the object receiving the method call. The FailableFunction return type must match the method's return type.
     *
     * @param <T> the type of the first argument to the function: The type containing the method.
     * @param <R> the type of the result of the function: The method return type.
     * @param method the method to invoke.
     * @return a correctly-typed wrapper for the given target.
     */
    @SuppressWarnings("unchecked")
    public static <T, R> FailableFunction<T, R, Throwable> asFailableFunction(final Method method) {
        return asInterfaceInstance(FailableFunction.class, method);
    }

    /**
     * Produces a {@link FailableSupplier} for a given a <em>supplier</em> Method. The FailableSupplier return type must
     * match the method's return type.
     * <p>
     * Only works with static methods.
     * </p>
     *
     * @param <R> The Method return type.
     * @param method the method to invoke.
     * @return a correctly-typed wrapper for the given target.
     */
    @SuppressWarnings("unchecked")
    public static <R> FailableSupplier<R, Throwable> asFailableSupplier(final Method method) {
        return asInterfaceInstance(FailableSupplier.class, method);
    }

    /**
     * Produces a {@link Function} for a given a <em>supplier</em> Method. You call the Function with one argument: the
     * object receiving the method call. The Function return type must match the method's return type.
     * <p>
     * For example to invoke {@link String#length()}:
     * </p>
     *
     * <pre>
     * final Method method = String.class.getMethod("length");
     * final Function&lt;String, Integer&gt; function = MethodInvokers.asFunction(method);
     * assertEquals(3, function.apply("ABC"));
     * </pre>
     *
     * @param <T> the type of the first argument to the function: The type containing the method.
     * @param <R> the type of the result of the function: The method return type.
     * @param method the method to invoke.
     * @return a correctly-typed wrapper for the given target.
     */
    @SuppressWarnings("unchecked")
    public static <T, R> Function<T, R> asFunction(final Method method) {
        return asInterfaceInstance(Function.class, method);
    }

    /**
     * Produces an instance of the given single-method interface which redirects its calls to the given method.
     * <p>
     * For the definition of "single-method", see {@linkplain MethodHandleProxies#asInterfaceInstance(Class, MethodHandle)}.
     * </p>
     *
     * @param <T> The interface type.
     * @param interfaceClass a class object representing {@code T}.
     * @param method the method to invoke.
     * @return a correctly-typed wrapper for the given target.
     * @see MethodHandleProxies#asInterfaceInstance(Class, MethodHandle)
     */
    public static <T> T asInterfaceInstance(final Class<T> interfaceClass, final Method method) {
        return MethodHandleProxies.asInterfaceInstance(Objects.requireNonNull(interfaceClass, "interfaceClass"), unreflectUnchecked(method));
    }

    /**
     * Produces a {@link Supplier} for a given a <em>supplier</em> Method. The Supplier return type must match the method's
     * return type.
     * <p>
     * Only works with static methods.
     * </p>
     *
     * @param <R> The Method return type.
     * @param method the method to invoke.
     * @return a correctly-typed wrapper for the given target.
     */
    @SuppressWarnings("unchecked")
    public static <R> Supplier<R> asSupplier(final Method method) {
        return asInterfaceInstance(Supplier.class, method);
    }

    /**
     * Throws NullPointerException if {@code method} is {@code null}.
     *
     * @param method The method to test.
     * @return The given method.
     * @throws NullPointerException if {@code method} is {@code null}.
     */
    private static Method requireMethod(final Method method) {
        return Objects.requireNonNull(method, "method");
    }

    private static MethodHandle unreflect(final Method method) throws IllegalAccessException {
        return MethodHandles.lookup().unreflect(requireMethod(method));
    }

    private static MethodHandle unreflectUnchecked(final Method method) {
        try {
            return unreflect(method);
        } catch (final IllegalAccessException e) {
            throw new UncheckedIllegalAccessException(e);
        }
    }

    /**
     * No need to create instances.
     */
    private MethodInvokers() {
        // noop
    }

}
