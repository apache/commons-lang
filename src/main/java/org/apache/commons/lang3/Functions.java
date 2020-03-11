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

import org.apache.commons.lang3.function.FailableDoubleConsumer;
import org.apache.commons.lang3.function.FailableDoubleFunction;
import org.apache.commons.lang3.function.FailableDoublePredicate;
import org.apache.commons.lang3.function.FailableDoubleToIntFunction;
import org.apache.commons.lang3.function.FailableDoubleToLongFunction;
import org.apache.commons.lang3.function.FailableDoubleUnaryOperator;
import org.apache.commons.lang3.function.FailableIntConsumer;
import org.apache.commons.lang3.function.FailableIntFunction;
import org.apache.commons.lang3.function.FailableIntPredicate;
import org.apache.commons.lang3.function.FailableIntToDoubleFunction;
import org.apache.commons.lang3.function.FailableIntToLongFunction;
import org.apache.commons.lang3.function.FailableIntUnaryOperator;
import org.apache.commons.lang3.function.FailableLongConsumer;
import org.apache.commons.lang3.function.FailableLongFunction;
import org.apache.commons.lang3.function.FailableLongPredicate;
import org.apache.commons.lang3.function.FailableLongToDoubleFunction;
import org.apache.commons.lang3.function.FailableLongToIntFunction;
import org.apache.commons.lang3.function.FailableLongUnaryOperator;
import org.apache.commons.lang3.function.FailableToDoubleFunction;
import org.apache.commons.lang3.function.FailableToIntFunction;
import org.apache.commons.lang3.function.FailableToLongFunction;
import org.apache.commons.lang3.function.FailableUnaryOperator;
import org.apache.commons.lang3.stream.FailableStream;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.lang.reflect.UndeclaredThrowableException;
import java.util.Collection;
import java.util.Objects;
import java.util.concurrent.Callable;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.BiPredicate;
import java.util.function.Consumer;
import java.util.function.DoubleConsumer;
import java.util.function.DoubleFunction;
import java.util.function.DoublePredicate;
import java.util.function.DoubleToIntFunction;
import java.util.function.DoubleToLongFunction;
import java.util.function.DoubleUnaryOperator;
import java.util.function.Function;
import java.util.function.IntConsumer;
import java.util.function.IntFunction;
import java.util.function.IntPredicate;
import java.util.function.IntToDoubleFunction;
import java.util.function.IntToLongFunction;
import java.util.function.IntUnaryOperator;
import java.util.function.LongConsumer;
import java.util.function.LongFunction;
import java.util.function.LongPredicate;
import java.util.function.LongToDoubleFunction;
import java.util.function.LongToIntFunction;
import java.util.function.LongUnaryOperator;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.function.ToDoubleFunction;
import java.util.function.ToIntFunction;
import java.util.function.ToLongFunction;
import java.util.function.UnaryOperator;
import java.util.stream.Stream;


/** This class provides utility functions, and classes for working with the
 * {@code java.util.function} package, or more generally, with Java 8
 * lambdas.
 * More specifically, it attempts to address the fact that lambdas are supposed
 * not to throw Exceptions, at least not checked Exceptions, aka instances of
 * {@link Exception}. This enforces the use of constructs like
 * <pre>{@code
 *   Consumer<java.lang.reflect.Method> consumer = (m) -> {
 *       try {
 *           m.invoke(o, args);
 *       } catch (Throwable t) {
 *           throw Functions.rethrow(t);
 *       }
 *   };
 * }</pre>
 * By replacing a {@link Consumer Consumer&lt;O&gt;} with a
 * {@link FailableConsumer FailableConsumer&lt;O,? extends Throwable&gt;}, this can be
 * written like follows:
 * <pre>{@code
 *   Functions.accept((m) -> m.invoke(o,args));
 * }</pre>
 * Obviously, the second version is much more concise and the spirit of
 * Lambda expressions is met better than the second version.
 *
 * @see org.apache.commons.lang3.function
 */
public class Functions {

    @FunctionalInterface
    public interface FailableRunnable<T extends Throwable> {
        /**
         * Runs the function.
         * @throws T if the function fails
         */
        void run() throws T;
    }

    @FunctionalInterface
    public interface FailableCallable<O, T extends Throwable> {
        /**
         * Calls the callable.
         * @return The value returned from the callable
         * @throws T if the callable fails
         */
        O call() throws T;
    }

    /**
     * Represents an operation that accepts two input arguments and returns no
     * result.
     *
     * <p>This is a functional interface whose functional method is
     * {@link #accept(Object, Object)}.
     *
     * <p>An exception will be thrown if an error occurs.
     *
     * @param <O1> the type of the first argument to the operation
     * @param <O2> the type of the second argument to the operation
     * @param <T>  the type of exception to be thrown
     *
     * @see BiConsumer
     * @since 3.10
     */
    @FunctionalInterface
    public interface FailableBiConsumer<O1, O2, T extends Throwable> {
        /**
         * Accepts the consumer.
         *
         * @param object1 the first parameter for the consumable to accept
         * @param object2 the second parameter for the consumable to accept
         * @throws T if the consumer fails
         */
        void accept(O1 object1, O2 object2) throws T;
    }

    /**
     * Represents a function that accepts two arguments and produces a result.
     *
     * <p>This is a functional interface whose functional method is {@link #apply(Object, Object)}.
     *
     * <p>An exception will be thrown if an error occurs.
     *
     * @param <I1> the type of the first argument to the operation
     * @param <I2> the type of the second argument to the operation
     * @param <O>  the type of the returned value
     * @param <T>  the type of exception to be thrown
     *
     * @see BiFunction
     * @since 3.10
     */
    @FunctionalInterface
    public interface FailableBiFunction<I1, I2, O, T extends Throwable> {
        /**
         * Apply the function.
         *
         * @param input1 the first input for the function
         * @param input2 the second input for the function
         * @return the result of the function
         * @throws T if the function fails
         */
        O apply(I1 input1, I2 input2) throws T;
    }

    /**
     * Represents a predicate (boolean-valued function) of two arguments.
     *
     * <p>This is a functional interface whose functional method is {@link #test(Object, Object)}.
     *
     * <p>An exception will be thrown if an error occurs.
     *
     * @param <O1> the type of the first argument to the operation
     * @param <O2> the type of the second argument to the operation
     * @param <T>  the type of exception to be thrown
     *
     * @see BiPredicate
     * @since 3.10
     */
    @FunctionalInterface
    public interface FailableBiPredicate<O1, O2, T extends Throwable> {
        /**
         * Test the predicate.
         *
         * @param object1 the first object to test the predicate on
         * @param object2 the second object to test the predicate on
         * @return the predicate's evaluation
         * @throws T if the predicate fails
         */
        boolean test(O1 object1, O2 object2) throws T;
    }

    /**
     * Represents an operation that accepts a single input argument and returns no
     * result.
     *
     * <p>This is a functional interface whose functional method is {@link #accept(Object)}.
     *
     * <p>An exception will be thrown if an error occurs.
     *
     * @param <O> the type of the argument to the operation
     * @param <T> the type of exception to be thrown
     *
     * @see Consumer
     * @since 3.10
     */
    @FunctionalInterface
    public interface FailableConsumer<O, T extends Throwable> {
        /**
         * Accepts the consumer.
         *
         * @param object the parameter for the consumable to accept
         * @throws T if the consumer fails
         */
        void accept(O object) throws T;
    }

    /**
     * Represents a function that accepts one argument and produces a result.
     *
     * <p>This is a functional interface whose functional method is {@link #apply(Object)}.
     *
     * <p>An exception will be thrown if an error occurs.
     *
     * @param <I>   the type of the input to the function
     * @param <O>   the type of the output of the function
     * @param <T>   the type of exception to be thrown
     *
     * @see Function
     * @since 3.10
     */
    @FunctionalInterface
    public interface FailableFunction<I, O, T extends Throwable> {
        /**
         * Apply the function.
         * @param input the input for the function
         * @return the result of the function
         * @throws T if the function fails
         */
        O apply(I input) throws T;
    }

    /**
     * Represents a predicate (boolean-valued function) of one argument.
     *
     * <p>This is a functional interface whose functional method is {@link #test(Object)}.
     *
     * <p>An exception will be thrown if an error occurs.
     *
     * @param <I> the type of the input to the function
     * @param <T> the type of exception to be thrown
     *
     * @see Predicate
     * @since 3.10
     */
    @FunctionalInterface
    public interface FailablePredicate<I, T extends Throwable> {
        /**
         * Test the predicate.
         *
         * @param object the object to test the predicate on
         * @return the predicate's evaluation
         * @throws T if the predicate fails
         */
        boolean test(I object) throws T;
    }

    /**
     * Represents a supplier of results.
     *
     * <p>There is no requirement that a new or distinct result be returned each
     * time the supplier is invoked.
     *
     * <p>This is a functional interface whose functional method is {@link #get()}.
     *
     * <p>An exception will be thrown if an error occurs.
     *
     * @param <R> the type of results produced by this supplier
     * @param <T> the type of exception to be thrown
     *
     * @see Supplier
     * @since 3.10
     */
    @FunctionalInterface
    public interface FailableSupplier<R, T extends Throwable> {
        /**
         * Supplies an object
         * @return the suppliers result
         * @throws T if the supplier fails
         */
        R get() throws T;
    }

    /**
     * Converts the given {@link FailableRunnable} into a standard {@link Runnable}.
     *
     * @param runnable a {@code FailableRunnable}
     * @return a standard {@code Runnable}
     */
    public static Runnable asRunnable(final FailableRunnable<?> runnable) {
        return () -> run(runnable);
    }

    /**
     * Converts the given {@link FailableConsumer} into a standard {@link Consumer}.
     *
     * @param <I> the type used by the consumers
     * @param consumer a {@code FailableConsumer}
     * @return a standard {@code Consumer}
     */
    public static <I> Consumer<I> asConsumer(final FailableConsumer<I, ?> consumer) {
        return input -> accept(consumer, input);
    }

    /**
     * Converts the given {@link FailableDoubleConsumer} into a standard {@link DoubleConsumer}.
     *
     * @param consumer a {@code FailableDoubleConsumer}
     * @return a standard {@code DoubleConsumer}
     */
    public static DoubleConsumer asDoubleConsumer(final FailableDoubleConsumer<?> consumer) {
        return input -> acceptDouble(consumer, input);
    }

    /**
     * Converts the given {@link FailableIntConsumer} into a standard {@link IntConsumer}.
     *
     * @param consumer a {@code FailableIntConsumer}
     * @return a standard {@code IntConsumer}
     */
    public static IntConsumer asIntConsumer(final FailableIntConsumer<?> consumer) {
        return input -> acceptInt(consumer, input);
    }

    /**
     * Converts the given {@link FailableLongConsumer} into a standard {@link LongConsumer}.
     *
     * @param consumer a {@code FailableLongConsumer}
     * @return a standard {@code LongConsumer}
     */
    public static LongConsumer asLongConsumer(final FailableLongConsumer<?> consumer) {
        return input -> acceptLong(consumer, input);
    }

    /**
     * Converts the given {@link FailableCallable} into a standard {@link Callable}.
     *
     * @param <O> the type used by the callables
     * @param callable a {@code FailableCallable}
     * @return a standard {@code Callable}
     */
    public static <O> Callable<O> asCallable(final FailableCallable<O, ?> callable) {
        return () -> call(callable);
    }

    /**
     * Converts the given {@link FailableBiConsumer} into a standard {@link BiConsumer}.
     *
     * @param <I1> the type of the first argument of the consumers
     * @param <I2> the type of the second argument of the consumers
     * @param consumer a failable {@code BiConsumer}
     * @return a standard {@code BiConsumer}
     */
    public static <I1, I2> BiConsumer<I1, I2> asBiConsumer(final FailableBiConsumer<I1, I2, ?> consumer) {
        return (input1, input2) -> accept(consumer, input1, input2);
    }

    /**
     * Converts the given {@link FailableFunction} into a standard {@link Function}.
     *
     * @param <I> the type of the input of the functions
     * @param <O> the type of the output of the functions
     * @param function a {code FailableFunction}
     * @return a standard {@code Function}
     */
    public static <I, O> Function<I, O> asFunction(final FailableFunction<I, O, ?> function) {
        return input -> apply(function, input);
    }

    /**
     * Converts the given {@link FailableDoubleFunction} into a standard {@link DoubleFunction}.
     *
     * @param <O> the type of the output of the functions
     * @param function a {code FailableDoubleFunction}
     * @return a standard {@code DoubleFunction}
     */
    public static <O> DoubleFunction<O> asDoubleFunction(final FailableDoubleFunction<O, ?> function) {
        return input -> applyDoubleFunction(function, input);
    }

    /**
     * Converts the given {@link FailableIntFunction} into a standard {@link IntFunction}.
     *
     * @param <O> the type of the output of the functions
     * @param function a {code FailableIntFunction}
     * @return a standard {@code IntFunction}
     */
    public static <O> IntFunction<O> asIntFunction(final FailableIntFunction<O, ?> function) {
        return input -> applyIntFunction(function, input);
    }

    /**
     * Converts the given {@link FailableLongFunction} into a standard {@link LongFunction}.
     *
     * @param <O> the type of the output of the functions
     * @param function a {code FailableLongFunction}
     * @return a standard {@code LongFunction}
     */
    public static <O> LongFunction<O> asLongFunction(final FailableLongFunction<O, ?> function) {
        return input -> applyLongFunction(function, input);
    }

    /**
     * Converts the given {@link FailableDoubleToIntFunction} into a standard {@link DoubleToIntFunction}.
     *
     * @param function a {code FailableDoubleToIntFunction}
     * @return a standard {@code DoubleToIntFunction}
     */
    public static DoubleToIntFunction asDoubleToIntFunction(final FailableDoubleToIntFunction<?> function) {
        return input -> applyDoubleToIntFunction(function, input);
    }

    /**
     * Converts the given {@link FailableDoubleToLongFunction} into a standard {@link DoubleToLongFunction}.
     *
     * @param function a {code FailableDoubleToLongFunction}
     * @return a standard {@code DoubleToLongFunction}
     */
    public static DoubleToLongFunction asDoubleToLongFunction(final FailableDoubleToLongFunction<?> function) {
        return input -> applyDoubleToLongFunction(function, input);
    }

    /**
     * Converts the given {@link FailableIntToDoubleFunction} into a standard {@link IntToDoubleFunction}.
     *
     * @param function a {code FailableIntToDoubleFunction}
     * @return a standard {@code IntToDoubleFunction}
     */
    public static IntToDoubleFunction asIntToDoubleFunction(final FailableIntToDoubleFunction<?> function) {
        return input -> applyIntToDoubleFunction(function, input);
    }

    /**
     * Converts the given {@link FailableIntToLongFunction} into a standard {@link IntToLongFunction}.
     *
     * @param function a {code FailableIntToLongFunction}
     * @return a standard {@code IntToLongFunction}
     */
    public static IntToLongFunction asIntToLongFunction(final FailableIntToLongFunction<?> function) {
        return input -> applyIntToLongFunction(function, input);
    }

    /**
     * Converts the given {@link FailableLongToDoubleFunction} into a standard {@link LongToDoubleFunction}.
     *
     * @param function a {code FailableLongToDoubleFunction}
     * @return a standard {@code LongToDoubleFunction}
     */
    public static LongToDoubleFunction asLongToDoubleFunction(final FailableLongToDoubleFunction<?> function) {
        return input -> applyLongToDoubleFunction(function, input);
    }

    /**
     * Converts the given {@link FailableLongToIntFunction} into a standard {@link LongToIntFunction}.
     *
     * @param function a {code FailableLongToIntFunction}
     * @return a standard {@code LongToIntFunction}
     */
    public static LongToIntFunction asLongToIntFunction(final FailableLongToIntFunction<?> function) {
        return input -> applyLongToIntFunction(function, input);
    }

    /**
     * Converts the given {@link FailableToIntFunction} into a standard {@link ToIntFunction}.
     *
     * @param <I> the type of the input of the functions
     * @param function a {code FailableToIntFunction}
     * @return a standard {@code ToIntFunction}
     */
    public static <I> ToIntFunction<I> asToIntFunction(FailableToIntFunction<I, ?> function) {
        return input -> applyToIntFunction(function, input);
    }

    /**
     * Converts the given {@link FailableToLongFunction} into a standard {@link ToLongFunction}.
     *
     * @param <I> the type of the input of the functions
     * @param function a {code FailableToLongFunction}
     * @return a standard {@code ToLongFunction}
     */
    public static <I> ToLongFunction<I> asToLongFunction(FailableToLongFunction<I, ?> function) {
        return input -> applyToLongFunction(function, input);
    }

    /**
     * Converts the given {@link FailableToDoubleFunction} into a standard {@link ToDoubleFunction}.
     *
     * @param <I> the type of the input of the functions
     * @param function a {code FailableToDoubleFunction}
     * @return a standard {@code ToDoubleFunction}
     */
    public static <I> ToDoubleFunction<I> asToDoubleFunction(FailableToDoubleFunction<I, ?> function) {
        return input -> applyToDoubleFunction(function, input);
    }

    /**
     * Converts the given {@link FailableBiFunction} into a standard {@link BiFunction}.
     *
     * @param <I1> the type of the first argument of the input of the functions
     * @param <I2> the type of the second argument of the input of the functions
     * @param <O> the type of the output of the functions
     * @param function a {@code FailableBiFunction}
     * @return a standard {@code BiFunction}
     */
    public static <I1, I2, O> BiFunction<I1, I2, O> asBiFunction(final FailableBiFunction<I1, I2, O, ?> function) {
        return (input1, input2) -> apply(function, input1, input2);
    }

    /**
     * Converts the given {@link FailablePredicate} into a standard {@link Predicate}.
     *
     * @param <I> the type used by the predicates
     * @param predicate a {@code FailablePredicate}
     * @return a standard {@code Predicate}
     */
    public static <I> Predicate<I> asPredicate(final FailablePredicate<I, ?> predicate) {
        return input -> test(predicate, input);
    }

    /**
     * Converts the given {@link FailableDoublePredicate} into a standard {@link DoublePredicate}.
     *
     * @param predicate a {@code FailableDoublePredicate}
     * @return a standard {@code DoublePredicate}
     */
    public static DoublePredicate asDoublePredicate(final FailableDoublePredicate<?> predicate) {
        return input -> testDouble(predicate, input);
    }

    /**
     * Converts the given {@link FailableIntPredicate} into a standard {@link IntPredicate}.
     *
     * @param predicate a {@code FailableIntPredicate}
     * @return a standard {@code IntPredicate}
     */
    public static IntPredicate asIntPredicate(final FailableIntPredicate<?> predicate) {
        return input -> testInt(predicate, input);
    }

    /**
     * Converts the given {@link FailableLongPredicate} into a standard {@link LongPredicate}.
     *
     * @param predicate a {@code FailableLongPredicate}
     * @return a standard {@code LongPredicate}
     */
    public static LongPredicate asLongPredicate(final FailableLongPredicate<?> predicate) {
        return input -> testLong(predicate, input);
    }

    /**
     * Converts the given {@link FailableBiPredicate} into a standard {@link BiPredicate}.
     *
     * @param <I1> the type of the first argument used by the predicates
     * @param <I2> the type of the second argument used by the predicates
     * @param predicate a {@code FailableBiPredicate}
     * @return a standard {@code BiPredicate}
     */
    public static <I1, I2> BiPredicate<I1, I2> asBiPredicate(final FailableBiPredicate<I1, I2, ?> predicate) {
        return (input1, input2) -> test(predicate, input1, input2);
    }

    /**
     * Converts the given {@link FailableSupplier} into a standard {@link Supplier}.
     *
     * @param <O> the type supplied by the suppliers
     * @param supplier a {@code FailableSupplier}
     * @return a standard {@code Supplier}
     */
    public static <O> Supplier<O> asSupplier(final FailableSupplier<O, ?> supplier) {
        return () -> get(supplier);
    }

    /**
     * Converts the given {@link FailableUnaryOperator} into a standard {@link UnaryOperator}.
     *
     * @param unaryOperator a {@code FailableUnaryOperator}
     * @param <I> the type of the input of the functions
     * @return a standard {@code UnaryOperator}
     */
    public static <I> UnaryOperator<I> asUnaryOperator(final FailableUnaryOperator<I, ?> unaryOperator) {
        return input -> apply(unaryOperator, input);
    }

    /**
     * Converts the given {@link FailableDoubleUnaryOperator} into a standard {@link DoubleUnaryOperator}.
     *
     * @param doubleUnaryOperator a {@code FailableDoubleUnaryOperator}
     * @return a standard {@code DoubleUnaryOperator}
     */
    public static DoubleUnaryOperator asDoubleUnaryOperator(final FailableDoubleUnaryOperator<?> doubleUnaryOperator) {
        return input -> applyDoubleUnaryOperator(doubleUnaryOperator, input);
    }

    /**
     * Converts the given {@link FailableIntUnaryOperator} into a standard {@link IntUnaryOperator}.
     *
     * @param intUnaryOperator a {@code FailableIntUnaryOperator}
     * @return a standard {@code IntUnaryOperator}
     */
    public static IntUnaryOperator asIntUnaryOperator(final FailableIntUnaryOperator<?> intUnaryOperator) {
        return input -> applyIntUnaryOperator(intUnaryOperator, input);
    }

    /**
     * Converts the given {@link FailableLongUnaryOperator} into a standard {@link LongUnaryOperator}.
     *
     * @param longUnaryOperator a {@code FailableLongUnaryOperator}
     * @return a standard {@code LongUnaryOperator}
     */
    public static LongUnaryOperator asLongUnaryOperator(final FailableLongUnaryOperator<?> longUnaryOperator) {
        return input -> applyLongUnaryOperator(longUnaryOperator, input);
    }

    /**
     * Runs a runnable and rethrows any exception as a {@link RuntimeException}.
     * @param runnable The runnable to run
     * @param <T> the type of checked exception the runnable may throw
     */
    public static <T extends Throwable> void run(final FailableRunnable<T> runnable) {
        try {
            runnable.run();
        } catch (final Throwable t) {
            throw rethrow(t);
        }
    }

    /**
     * Calls a callable and rethrows any exception as a {@link RuntimeException}.
     * @param callable the callable to call
     * @param <O> the return type of the callable
     * @param <T> the type of checked exception the callable may throw
     * @return the value returned from the callable
     */
    public static <O, T extends Throwable> O call(final FailableCallable<O, T> callable) {
        return get(callable::call);
    }

    /**
     * Consumes a consumer and rethrows any exception as a {@link RuntimeException}.
     * @param consumer the consumer to consume
     * @param object the object to be consumed by {@code consumer}
     * @param <O> the type the consumer accepts
     * @param <T> the type of checked exception the consumer may throw
     */
    public static <O, T extends Throwable> void accept(final FailableConsumer<O, T> consumer, final O object) {
        run(() -> consumer.accept(object));
    }

    /**
     * Consumes a consumer and rethrows any exception as a {@link RuntimeException}.
     * @param consumer the consumer to consume
     * @param value the value to be consumed by {@code consumer}
     * @param <T> the type of checked exception the consumer may throw
     */
    public static <T extends Throwable> void acceptDouble(final FailableDoubleConsumer<T> consumer, final double value) {
        run(() -> consumer.accept(value));
    }

    /**
     * Consumes a consumer and rethrows any exception as a {@link RuntimeException}.
     * @param consumer the consumer to consume
     * @param value the value to be consumed by {@code consumer}
     * @param <T> the type of checked exception the consumer may throw
     */
    public static <T extends Throwable> void acceptInt(final FailableIntConsumer<T> consumer, final int value) {
        run(() -> consumer.accept(value));
    }

    /**
     * Consumes a consumer and rethrows any exception as a {@link RuntimeException}.
     * @param consumer the consumer to consume
     * @param value the value to be consumed by {@code consumer}
     * @param <T> the type of checked exception the consumer may throw
     */
    public static <T extends Throwable> void acceptLong(final FailableLongConsumer<T> consumer, final long value) {
        run(() -> consumer.accept(value));
    }

    /**
     * Consumes a consumer and rethrows any exception as a {@link RuntimeException}.
     * @param consumer the consumer to consume
     * @param object1 the first object to consume by {@code consumer}
     * @param object2 the second object to consume by {@code consumer}
     * @param <O1> the type of the first argument the consumer accepts
     * @param <O2> the type of the second argument the consumer accepts
     * @param <T> the type of checked exception the consumer may throw
     */
    public static <O1, O2, T extends Throwable> void accept(final FailableBiConsumer<O1, O2, T> consumer, final O1 object1, final O2 object2) {
        run(() -> consumer.accept(object1, object2));
    }

    /**
     * Applies a function and rethrows any exception as a {@link RuntimeException}.
     * @param function the function to apply
     * @param input the input to apply {@code function} on
     * @param <I> the type of the argument the function accepts
     * @param <O> the return type of the function
     * @param <T> the type of checked exception the function may throw
     * @return the value returned from the function
     */
    public static <I, O, T extends Throwable> O apply(final FailableFunction<I, O, T> function, final I input) {
        return get(() -> function.apply(input));
    }

    /**
     * Applies a function and rethrows any exception as a {@link RuntimeException}.
     * @param function the function to apply
     * @param input the input to apply {@code function} on
     * @param <O> the return type of the function
     * @param <T> the type of checked exception the function may throw
     * @return the value returned from the function
     */
    public static <O, T extends Throwable> O applyDoubleFunction(final FailableDoubleFunction<O, T> function, final double input) {
        return get(() -> function.apply(input));
    }

    /**
     * Applies a function and rethrows any exception as a {@link RuntimeException}.
     * @param function the function to apply
     * @param input the input to apply {@code function} on
     * @param <T> the type of checked exception the function may throw
     * @return the value returned from the function
     */
    public static <T extends Throwable> int applyDoubleToIntFunction(final FailableDoubleToIntFunction<T> function, final double input) {
        return get(() -> function.applyAsInt(input));
    }

    /**
     * Applies a function and rethrows any exception as a {@link RuntimeException}.
     * @param function the function to apply
     * @param input the input to apply {@code function} on
     * @param <T> the type of checked exception the function may throw
     * @return the value returned from the function
     */
    public static <T extends Throwable> long applyDoubleToLongFunction(final FailableDoubleToLongFunction<T> function, final double input) {
        return get(() -> function.applyAsLong(input));
    }

    /**
     * Applies a function and rethrows any exception as a {@link RuntimeException}.
     * @param function the function to apply
     * @param input the input to apply {@code function} on
     * @param <O> the return type of the function
     * @param <T> the type of checked exception the function may throw
     * @return the value returned from the function
     */
    public static <O, T extends Throwable> O applyIntFunction(final FailableIntFunction<O, T> function, final int input) {
        return get(() -> function.apply(input));
    }

    /**
     * Applies a function and rethrows any exception as a {@link RuntimeException}.
     * @param function the function to apply
     * @param input the input to apply {@code function} on
     * @param <O> the return type of the function
     * @param <T> the type of checked exception the function may throw
     * @return the value returned from the function
     */
    public static <O, T extends Throwable> O applyLongFunction(final FailableLongFunction<O, T> function, final long input) {
        return get(() -> function.apply(input));
    }

    /**
     * Applies a function and rethrows any exception as a {@link RuntimeException}.
     * @param function the function to apply
     * @param input the input to apply {@code function} on
     * @param <T> the type of checked exception the function may throw
     * @return the value returned from the function
     */
    public static <T extends Throwable> double applyIntToDoubleFunction(final FailableIntToDoubleFunction<T> function, final int input) {
        return get(() -> function.applyAsDouble(input));
    }

    /**
     * Applies a function and rethrows any exception as a {@link RuntimeException}.
     * @param function the function to apply
     * @param input the input to apply {@code function} on
     * @param <T> the type of checked exception the function may throw
     * @return the value returned from the function
     */
    public static <T extends Throwable> double applyLongToDoubleFunction(final FailableLongToDoubleFunction<T> function, final long input) {
        return get(() -> function.applyAsDouble(input));
    }

    /**
     * Applies a function and rethrows any exception as a {@link RuntimeException}.
     * @param function the function to apply
     * @param input the input to apply {@code function} on
     * @param <T> the type of checked exception the function may throw
     * @return the value returned from the function
     */
    public static <T extends Throwable> int applyLongToIntFunction(final FailableLongToIntFunction<T> function, final long input) {
        return get(() -> function.applyAsInt(input));
    }

    /**
     * Applies a function and rethrows any exception as a {@link RuntimeException}.
     * @param function the function to apply
     * @param input the input to apply {@code function} on
     * @param <T> the type of checked exception the function may throw
     * @return the value returned from the function
     */
    public static <T extends Throwable> long applyIntToLongFunction(final FailableIntToLongFunction<T> function, final int input) {
        return get(() -> function.applyAsLong(input));
    }

    /**
     * Applies a function and rethrows any exception as a {@link RuntimeException}.
     * @param function the function to apply
     * @param input the input to apply {@code function} on
     * @param <I> the type of the argument the function accepts
     * @param <T> the type of checked exception the function may throw
     * @return the value returned from the function
     */
    public static <I, T extends Throwable> int applyToIntFunction(final FailableToIntFunction<I, T> function, final I input) {
        return get(() -> function.applyAsInt(input));
    }

    /**
     * Applies a function and rethrows any exception as a {@link RuntimeException}.
     * @param function the function to apply
     * @param input the input to apply {@code function} on
     * @param <I> the type of the argument the function accepts
     * @param <T> the type of checked exception the function may throw
     * @return the value returned from the function
     */
    public static <I, T extends Throwable> long applyToLongFunction(final FailableToLongFunction<I, T> function, final I input) {
        return get(() -> function.applyAsLong(input));
    }

    /**
     * Applies a function and rethrows any exception as a {@link RuntimeException}.
     * @param function the function to apply
     * @param input the input to apply {@code function} on
     * @param <I> the type of the argument the function accepts
     * @param <T> the type of checked exception the function may throw
     * @return the value returned from the function
     */
    public static <I, T extends Throwable> double applyToDoubleFunction(final FailableToDoubleFunction<I, T> function, final I input) {
        return get(() -> function.applyAsDouble(input));
    }

    /**
     * Applies a function and rethrows any exception as a {@link RuntimeException}.
     *
     * @param doubleUnaryOperator the function to apply
     * @param input the input to apply the function on
     * @return the value returned from the function
     */
    public static double applyDoubleUnaryOperator(final FailableDoubleUnaryOperator<?> doubleUnaryOperator, double input) {
        return get(() -> doubleUnaryOperator.applyAsDouble(input));
    }

    /**
     * Applies a function and rethrows any exception as a {@link RuntimeException}.
     *
     * @param intUnaryOperator the function to apply
     * @param input the input to apply the function on
     * @return the value returned from the function
     */
    public static int applyIntUnaryOperator(final FailableIntUnaryOperator<?> intUnaryOperator, int input) {
        return get(() -> intUnaryOperator.applyAsInt(input));
    }

    /**
     * Applies a function and rethrows any exception as a {@link RuntimeException}.
     *
     * @param longUnaryOperator the function to apply
     * @param input the input to apply the function on
     * @return the value returned from the function
     */
    public static long applyLongUnaryOperator(final FailableLongUnaryOperator<?> longUnaryOperator, long input) {
        return get(() -> longUnaryOperator.applyAsLong(input));
    }

    /**
     * Applies a function and rethrows any exception as a {@link RuntimeException}.
     * @param function the function to apply
     * @param input1 the first input to apply {@code function} on
     * @param input2 the second input to apply {@code function} on
     * @param <I1> the type of the first argument the function accepts
     * @param <I2> the type of the second argument the function accepts
     * @param <O> the return type of the function
     * @param <T> the type of checked exception the function may throw
     * @return the value returned from the function
     */
    public static <I1, I2, O, T extends Throwable> O apply(final FailableBiFunction<I1, I2, O, T> function, final I1 input1, final I2 input2) {
        return get(() -> function.apply(input1, input2));
    }

    /**
     * Tests a predicate and rethrows any exception as a {@link RuntimeException}.
     * @param predicate the predicate to test
     * @param object the input to be tested by {@code predicate}
     * @param <O> the type of argument the predicate tests
     * @param <T> the type of checked exception the predicate may throw
     * @return the boolean value returned by the predicate
     */
    public static <O, T extends Throwable> boolean test(final FailablePredicate<O, T> predicate, final O object) {
        return get(() -> predicate.test(object));
    }

    /**
     * Tests a predicate and rethrows any exception as a {@link RuntimeException}.
     * @param predicate the predicate to test
     * @param value the input to be tested by {@code predicate}
     * @param <T> the type of checked exception the predicate may throw
     * @return the boolean value returned by the predicate
     */
    public static <T extends Throwable> boolean testDouble(final FailableDoublePredicate<T> predicate, final double value) {
        return get(() -> predicate.test(value));
    }

    /**
     * Tests a predicate and rethrows any exception as a {@link RuntimeException}.
     * @param predicate the predicate to test
     * @param value the input to be tested by {@code predicate}
     * @param <T> the type of checked exception the predicate may throw
     * @return the boolean value returned by the predicate
     */
    public static <T extends Throwable> boolean testInt(final FailableIntPredicate<T> predicate, final int value) {
        return get(() -> predicate.test(value));
    }

    /**
     * Tests a predicate and rethrows any exception as a {@link RuntimeException}.
     * @param predicate the predicate to test
     * @param value the input to be tested by {@code predicate}
     * @param <T> the type of checked exception the predicate may throw
     * @return the boolean value returned by the predicate
     */
    public static <T extends Throwable> boolean testLong(final FailableLongPredicate<T> predicate, final long value) {
        return get(() -> predicate.test(value));
    }

    /**
     * Tests a predicate and rethrows any exception as a {@link RuntimeException}.
     * @param predicate the predicate to test
     * @param object1 the first input to test by {@code predicate}
     * @param object2 the second input to test by {@code predicate}
     * @param <O1> the type of the first argument the predicate tests
     * @param <O2> the type of the second argument the predicate tests
     * @param <T> the type of checked exception the predicate may throw
     * @return the boolean value returned by the predicate
     */
    public static <O1, O2, T extends Throwable> boolean test(final FailableBiPredicate<O1, O2, T> predicate, final O1 object1, final O2 object2) {
        return get(() -> predicate.test(object1, object2));
    }

    /**
     * Invokes the supplier, and returns the result.
     * @param supplier The supplier to invoke.
     * @param <O> The suppliers output type.
     * @param <T> The type of checked exception, which the supplier can throw.
     * @return The object, which has been created by the supplier
     */
    public static <O, T extends Throwable> O get(final FailableSupplier<O, T> supplier) {
        try {
            return supplier.get();
        } catch (final Throwable t) {
            throw rethrow(t);
        }
    }

    /**
     * Converts the given stream into a {@link FailableStream}. The
     * {@link FailableStream} consists of the same elements, than the
     * input stream. However, failable lambdas, like
     * {@link FailablePredicate}, {@link FailableFunction}, and
     * {@link FailableConsumer} may be applied, rather than
     * {@link Predicate}, {@link Function}, {@link Consumer}, etc.
     * @param stream The stream, which is being converted into a
     *   {@link FailableStream}.
     * @param <O> The streams element type.
     * @return The created {@link FailableStream}.
     */
    public static <O> FailableStream<O> stream(final Stream<O> stream) {
        return new FailableStream<>(stream);
    }

    /**
     * Converts the given collection into a {@link FailableStream}.
     * The {@link FailableStream} consists of the collections
     * elements. Shortcut for
     * <pre>
     *   Functions.stream(collection.stream());
     * </pre>
     * @param collection The collection, which is being converted into a
     *   {@link FailableStream}.
     * @param <O> The collections element type. (In turn, the result
     *   streams element type.)
     * @return The created {@link FailableStream}.
     */
    public static <O> FailableStream<O> stream(final Collection<O> collection) {
        return new FailableStream<>(collection.stream());
    }

    /**
     * A simple try-with-resources implementation, that can be used, if your
     * objects do not implement the {@link AutoCloseable} interface. The method
     * executes the {@code action}. The method guarantees, that <em>all</em>
     * the {@code resources} are being executed, in the given order, afterwards,
     * and regardless of success, or failure. If either the original action, or
     * any of the resource action fails, then the <em>first</em> failure (aka
     * {@link Throwable} is rethrown. Example use:
     * <pre>{@code
     *   final FileInputStream fis = new FileInputStream("my.file");
     *   Functions.tryWithResources(useInputStream(fis), null, () -> fis.close());
     * }</pre>
     * @param action The action to execute. This object <em>will</em> always
     *   be invoked.
     * @param errorHandler An optional error handler, which will be invoked finally,
     *   if any error occurred. The error handler will receive the first
     *   error, aka {@link Throwable}.
     * @param resources The resource actions to execute. <em>All</em> resource
     *   actions will be invoked, in the given order. A resource action is an
     *   instance of {@link FailableRunnable}, which will be executed.
     * @see #tryWithResources(FailableRunnable, FailableRunnable...)
     */
    @SafeVarargs
    public static void tryWithResources(final FailableRunnable<? extends Throwable> action,
                                            final FailableConsumer<Throwable, ? extends Throwable> errorHandler,
                                            final FailableRunnable<? extends Throwable>... resources) {
        final FailableConsumer<Throwable, ? extends Throwable> actualErrorHandler;
        if (errorHandler == null) {
            actualErrorHandler = Functions::rethrow;
        } else {
            actualErrorHandler = errorHandler;
        }
        if (resources != null) {
            for (final FailableRunnable<? extends Throwable> failableRunnable : resources) {
                Objects.requireNonNull(failableRunnable, "runnable");
            }
        }
        Throwable th = null;
        try {
            action.run();
        } catch (final Throwable t) {
            th = t;
        }
        if (resources != null) {
            for (final FailableRunnable<?> runnable : resources) {
                try {
                    runnable.run();
                } catch (final Throwable t) {
                    if (th == null) {
                        th = t;
                    }
                }
            }
        }
        if (th != null) {
            try {
                actualErrorHandler.accept(th);
            } catch (final Throwable t) {
                throw rethrow(t);
            }
        }
    }

    /**
     * A simple try-with-resources implementation, that can be used, if your
     * objects do not implement the {@link AutoCloseable} interface. The method
     * executes the {@code action}. The method guarantees, that <em>all</em>
     * the {@code resources} are being executed, in the given order, afterwards,
     * and regardless of success, or failure. If either the original action, or
     * any of the resource action fails, then the <em>first</em> failure (aka
     * {@link Throwable} is rethrown. Example use:
     * <pre>{@code
     *   final FileInputStream fis = new FileInputStream("my.file");
     *   Functions.tryWithResources(useInputStream(fis), () -> fis.close());
     * }</pre>
     * @param action The action to execute. This object <em>will</em> always
     *   be invoked.
     * @param resources The resource actions to execute. <em>All</em> resource
     *   actions will be invoked, in the given order. A resource action is an
     *   instance of {@link FailableRunnable}, which will be executed.
     * @see #tryWithResources(FailableRunnable, FailableConsumer, FailableRunnable...)
     */
    @SafeVarargs
    public static void tryWithResources(final FailableRunnable<? extends Throwable> action,
                                            final FailableRunnable<? extends Throwable>... resources) {
        tryWithResources(action, null, resources);
    }

    /**
     * <p>Rethrows a {@link Throwable} as an unchecked exception. If the argument is
     * already unchecked, namely a {@code RuntimeException} or {@code Error} then
     * the argument will be rethrown without modification. If the exception is
     * {@code IOException} then it will be wrapped into a {@code UncheckedIOException}.
     * In every other cases the exception will be wrapped into a {@code
     * UndeclaredThrowableException}</p>
     *
     * <p>Note that there is a declared return type for this method, even though it
     * never returns. The reason for that is to support the usual pattern:</p>
     *
     * <pre>
     *      throw rethrow(myUncheckedException);
     * </pre>
     *
     * <p>instead of just calling the method. This pattern may help the Java compiler to
     * recognize that at that point an exception will be thrown and the code flow
     * analysis will not demand otherwise mandatory commands that could follow the
     * method call, like a {@code return} statement from a value returning method.</p>
     *
     * @param throwable The throwable to rethrow ossibly wrapped into an unchecked exception
     * @return Never returns anything, this method never terminates normally.
     */
    public static RuntimeException rethrow(final Throwable throwable) {
        Objects.requireNonNull(throwable, "throwable");
        if (throwable instanceof RuntimeException) {
            throw (RuntimeException) throwable;
        } else if (throwable instanceof Error) {
            throw (Error) throwable;
        } else if (throwable instanceof IOException) {
            throw new UncheckedIOException((IOException) throwable);
        } else {
            throw new UndeclaredThrowableException(throwable);
        }
    }
}
