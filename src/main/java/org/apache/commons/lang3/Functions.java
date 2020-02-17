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
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Stream;

import org.apache.commons.lang3.Streams.FailableStream;


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
 * By replacing a {@link java.util.function.Consumer Consumer&lt;O&gt;} with a
 * {@link FailableConsumer FailableConsumer&lt;O,? extends Throwable&gt;}, this can be
 * written like follows:
 * <pre>{@code
 *   Functions.accept((m) -> m.invoke(o,args));
 * }</pre>
 * Obviously, the second version is much more concise and the spirit of
 * Lambda expressions is met better than the second version.
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

    @FunctionalInterface
    public interface FailableConsumer<O, T extends Throwable> {
        /**
         * Accepts the consumer.
         * @param object the parameter for the consumable to accept
         * @throws T if the consumer fails
         */
        void accept(O object) throws T;
    }

    @FunctionalInterface
    public interface FailableBiConsumer<O1, O2, T extends Throwable> {
        /**
         * Accepts the consumer.
         * @param object1 the first parameter for the consumable to accept
         * @param object2 the second parameter for the consumable to accept
         * @throws T if the consumer fails
         */
        void accept(O1 object1, O2 object2) throws T;
    }

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

    @FunctionalInterface
    public interface FailableBiFunction<I1, I2, O, T extends Throwable> {
        /**
         * Apply the function.
         * @param input1 the first input for the function
         * @param input2 the second input for the function
         * @return the result of the function
         * @throws T if the function fails
         */
        O apply(I1 input1, I2 input2) throws T;
    }

    @FunctionalInterface
    public interface FailablePredicate<O, T extends Throwable> {
        /**
         * Test the predicate.
         * @param object the object to test the predicate on
         * @return the predicate's evaluation
         * @throws T if the predicate fails
         */
        boolean test(O object) throws T;
    }

    @FunctionalInterface
    public interface FailableBiPredicate<O1, O2, T extends Throwable> {
        /**
         * Test the predicate.
         * @param object1 the first object to test the predicate on
         * @param object2 the second object to test the predicate on
         * @return the predicate's evaluation
         * @throws T if the predicate fails
         */
        boolean test(O1 object1, O2 object2) throws T;
    }

    @FunctionalInterface
    public interface FailableSupplier<O, T extends Throwable> {
        /**
         * Supplies an object
         * @return the suppliers result
         * @throws T if the supplier fails
         */
        O get() throws T;
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
     * @param object the object to consume by {@code consumer}
     * @param <O> the type the consumer accepts
     * @param <T> the type of checked exception the consumer may throw
     */
    public static <O, T extends Throwable> void accept(final FailableConsumer<O, T> consumer, final O object) {
        run(() -> consumer.accept(object));
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
     * @param object the input to test by {@code predicate}
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
