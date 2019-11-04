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
import java.util.concurrent.Callable;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.BiPredicate;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;


/** This class provides utility functions, and classes for working with the
 * {@code java.util.function} package, or more generally, with Java 8
 * lambdas.
 * More specifically, it attempts to address the fact that lambdas are supposed
 * not to throw Exceptions, at least not checked Exceptions, aka instances of
 * {@link Exception}. This enforces the use of constructs like
 * <pre>
 *   Consumer&lt;java.lang.reflect.Method&gt; consumer = (m) -&gt; {
 *       try {
 *           m.invoke(o, args);
 *       } catch (Throwable t) {
 *           throw Functions.rethrow(t);
 *       }
 *   };
 * </pre>
 * By replacing a {@link java.util.function.Consumer Consumer&lt;O&gt;} with a
 * {@link FailableConsumer FailableConsumer&lt;O,? extends Throwable&gt;}, this can be
 * written like follows:
 * <pre>
 *   Functions.accept((m) -&gt; m.invoke(o,args));
 * </pre>
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
         * @param pObject the parameter for the consumable to accept
         * @throws T if the consumer fails
         */
        void accept(O pObject) throws T;
    }

    @FunctionalInterface
    public interface FailableBiConsumer<O1, O2, T extends Throwable> {
        /**
         * Accepts the consumer.
         * @param pObject1 the first parameter for the consumable to accept
         * @param pObject2 the second parameter for the consumable to accept
         * @throws T if the consumer fails
         */
        void accept(O1 pObject1, O2 pObject2) throws T;
    }

    @FunctionalInterface
    public interface FailableFunction<I, O, T extends Throwable> {
        /**
         * Apply the function.
         * @param pInput the input for the function
         * @return the result of the function
         * @throws T if the function fails
         */
        O apply(I pInput) throws T;
    }

    @FunctionalInterface
    public interface FailableBiFunction<I1, I2, O, T extends Throwable> {
        /**
         * Apply the function.
         * @param pInput1 the first input for the function
         * @param pInput2 the second input for the function
         * @return the result of the function
         * @throws T if the function fails
         */
        O apply(I1 pInput1, I2 pInput2) throws T;
    }

    @FunctionalInterface
    public interface FailablePredicate<O, T extends Throwable> {
        /**
         * Test the predicate.
         * @param pObject the object to test the predicate on
         * @return the predicate's evaluation
         * @throws T if the predicate fails
         */
        boolean test(O pObject) throws T;
    }

    @FunctionalInterface
    public interface FailableBiPredicate<O1, O2, T extends Throwable> {
        /**
         * Test the predicate.
         * @param pObject1 the first object to test the predicate on
         * @param pObject2 the second object to test the predicate on
         * @return the predicate's evaluation
         * @throws T if the predicate fails
         */
        boolean test(O1 pObject1, O2 pObject2) throws T;
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
     * @param pRunnable a {@code FailableRunnable}
     * @return a standard {@code Runnable}
     */
    public static Runnable asRunnable(FailableRunnable<?> pRunnable) {
        return () -> run(pRunnable);
    }

    /**
     * Converts the given {@link FailableConsumer} into a standard {@link Consumer}.
     *
     * @param <I> the type used by the consumers
     * @param pConsumer a {@code FailableConsumer}
     * @return a standard {@code Consumer}
     */
    public static <I> Consumer<I> asConsumer(FailableConsumer<I, ?> pConsumer) {
        return (pInput) -> accept(pConsumer, pInput);
    }

    /**
     * Converts the given {@link FailableCallable} into a standard {@link Callable}.
     *
     * @param <O> the type used by the callables
     * @param pCallable a {@code FailableCallable}
     * @return a standard {@code Callable}
     */
    public static <O> Callable<O> asCallable(FailableCallable<O, ?> pCallable) {
        return () -> call(pCallable);
    }

    /**
     * Converts the given {@link FailableBiConsumer} into a standard {@link BiConsumer}.
     *
     * @param <I1> the type of the first argument of the consumers
     * @param <I2> the type of the second argument of the consumers
     * @param pConsumer a failable {@code BiConsumer}
     * @return a standard {@code BiConsumer}
     */
    public static <I1, I2> BiConsumer<I1, I2> asBiConsumer(FailableBiConsumer<I1, I2, ?> pConsumer) {
        return (pInput1, pInput2) -> accept(pConsumer, pInput1, pInput2);
    }

    /**
     * Converts the given {@link FailableFunction} into a standard {@link Function}.
     *
     * @param <I> the type of the input of the functions
     * @param <O> the type of the output of the functions
     * @param pFunction a {code FailableFunction}
     * @return a standard {@code Function}
     */
    public static <I, O> Function<I, O> asFunction(FailableFunction<I, O, ?> pFunction) {
        return (pInput) -> apply(pFunction, pInput);
    }

    /**
     * Converts the given {@link FailableBiFunction} into a standard {@link BiFunction}.
     *
     * @param <I1> the type of the first argument of the input of the functions
     * @param <I2> the type of the second argument of the input of the functions
     * @param <O> the type of the output of the functions
     * @param pFunction a {@code FailableBiFunction}
     * @return a standard {@code BiFunction}
     */
    public static <I1, I2, O> BiFunction<I1, I2, O> asBiFunction(FailableBiFunction<I1, I2, O, ?> pFunction) {
        return (pInput1, pInput2) -> apply(pFunction, pInput1, pInput2);
    }

    /**
     * Converts the given {@link FailablePredicate} into a standard {@link Predicate}.
     *
     * @param <I> the type used by the predicates
     * @param pPredicate a {@code FailablePredicate}
     * @return a standard {@code Predicate}
     */
    public static <I> Predicate<I> asPredicate(FailablePredicate<I, ?> pPredicate) {
        return (pInput) -> test(pPredicate, pInput);
    }

    /**
     * Converts the given {@link FailableBiPredicate} into a standard {@link BiPredicate}.
     *
     * @param <I1> the type of the first argument used by the predicates
     * @param <I2> the type of the second argument used by the predicates
     * @param pPredicate a {@code FailableBiPredicate}
     * @return a standard {@code BiPredicate}
     */
    public static <I1, I2> BiPredicate<I1, I2> asBiPredicate(FailableBiPredicate<I1, I2, ?> pPredicate) {
        return (pInput1, pInput2) -> test(pPredicate, pInput1, pInput2);
    }

    /**
     * Converts the given {@link FailableSupplier} into a standard {@link Supplier}.
     *
     * @param <O> the type supplied by the suppliers
     * @param pSupplier a {@code FailableSupplier}
     * @return a standard {@code Supplier}
     */
    public static <O> Supplier<O> asSupplier(FailableSupplier<O, ?> pSupplier) {
        return () -> get(pSupplier);
    }

    /**
     * Runs a runnable and rethrows any exception as a {@link RuntimeException}.
     * @param pRunnable The runnable to run
     * @param <T> the type of checked exception the runnable may throw
     */
    public static <T extends Throwable> void run(FailableRunnable<T> pRunnable) {
        try {
            pRunnable.run();
        } catch (Throwable t) {
            throw rethrow(t);
        }
    }

    /**
     * Calls a callable and rethrows any exception as a {@link RuntimeException}.
     * @param pCallable the callable to call
     * @param <O> the return type of the callable
     * @param <T> the type of checked exception the callable may throw
     * @return the value returned from the callable
     */
    public static <O, T extends Throwable> O call(FailableCallable<O, T> pCallable) {
        try {
            return pCallable.call();
        } catch (Throwable t) {
            throw rethrow(t);
        }
    }

    /**
     * Consumes a consumer and rethrows any exception as a {@link RuntimeException}.
     * @param pConsumer the consumer to consume
     * @param pObject the object to consume by <code>pConsumer</code>
     * @param <O> the type the consumer accepts
     * @param <T> the type of checked exception the consumer may throw
     */
    public static <O, T extends Throwable> void accept(FailableConsumer<O, T> pConsumer, O pObject) {
        try {
            pConsumer.accept(pObject);
        } catch (Throwable t) {
            throw rethrow(t);
        }
    }

    /**
     * Consumes a consumer and rethrows any exception as a {@link RuntimeException}.
     * @param pConsumer the consumer to consume
     * @param pObject1 the first object to consume by <code>pConsumer</code>
     * @param pObject2 the second object to consume by <code>pConsumer</code>
     * @param <O1> the type of the first argument the consumer accepts
     * @param <O2> the type of the second argument the consumer accepts
     * @param <T> the type of checked exception the consumer may throw
     */
    public static <O1, O2, T extends Throwable> void accept(FailableBiConsumer<O1, O2, T> pConsumer, O1 pObject1, O2 pObject2) {
        try {
            pConsumer.accept(pObject1, pObject2);
        } catch (Throwable t) {
            throw rethrow(t);
        }
    }

    /**
     * Applies a function and rethrows any exception as a {@link RuntimeException}.
     * @param pFunction the function to apply
     * @param pInput the input to apply <code>pFunction</code> on
     * @param <I> the type of the argument the function accepts
     * @param <O> the return type of the function
     * @param <T> the type of checked exception the function may throw
     * @return the value returned from the function
     */
    public static <I, O, T extends Throwable> O apply(FailableFunction<I, O, T> pFunction, I pInput) {
        try {
            return pFunction.apply(pInput);
        } catch (Throwable t) {
            throw rethrow(t);
        }
    }

    /**
     * Applies a function and rethrows any exception as a {@link RuntimeException}.
     * @param pFunction the function to apply
     * @param pInput1 the first input to apply <code>pFunction</code> on
     * @param pInput2 the second input to apply <code>pFunction</code> on
     * @param <I1> the type of the first argument the function accepts
     * @param <I2> the type of the second argument the function accepts
     * @param <O> the return type of the function
     * @param <T> the type of checked exception the function may throw
     * @return the value returned from the function
     */
    public static <I1, I2, O, T extends Throwable> O apply(FailableBiFunction<I1, I2, O, T> pFunction, I1 pInput1, I2 pInput2) {
        try {
            return pFunction.apply(pInput1, pInput2);
        } catch (Throwable t) {
            throw rethrow(t);
        }
    }

    /**
     * Tests a predicate and rethrows any exception as a {@link RuntimeException}.
     * @param pPredicate the predicate to test
     * @param pObject the input to test by <code>pPredicate</code>
     * @param <O> the type of argument the predicate tests
     * @param <T> the type of checked exception the predicate may throw
     * @return the boolean value returned by the predicate
     */
    public static <O, T extends Throwable> boolean test(FailablePredicate<O, T> pPredicate, O pObject) {
        try {
            return pPredicate.test(pObject);
        } catch (Throwable t) {
            throw rethrow(t);
        }
    }

    /**
     * Tests a predicate and rethrows any exception as a {@link RuntimeException}.
     * @param pPredicate the predicate to test
     * @param pObject1 the first input to test by <code>pPredicate</code>
     * @param pObject2 the second input to test by <code>pPredicate</code>
     * @param <O1> the type of the first argument the predicate tests
     * @param <O2> the type of the second argument the predicate tests
     * @param <T> the type of checked exception the predicate may throw
     * @return the boolean value returned by the predicate
     */
    public static <O1, O2, T extends Throwable> boolean test(FailableBiPredicate<O1, O2, T> pPredicate, O1 pObject1, O2 pObject2) {
        try {
            return pPredicate.test(pObject1, pObject2);
        } catch (Throwable t) {
            throw rethrow(t);
        }
    }

    /**
     * Invokes the supplier, and returns the result.
     * @param pSupplier The supplier to invoke.
     * @param <O> The suppliers output type.
     * @param <T> The type of checked exception, which the supplier can throw.
     * @return The object, which has been created by the supplier
     */
    public static <O, T extends Throwable> O get(FailableSupplier<O, T> pSupplier) {
        try {
            return pSupplier.get();
        } catch (Throwable t) {
            throw rethrow(t);
        }
    }


    /**
     * A simple try-with-resources implementation, that can be used, if your
     * objects do not implement the {@link AutoCloseable} interface. The method
     * executes the {@code pAction}. The method guarantees, that <em>all</em>
     * the {@code pResources} are being executed, in the given order, afterwards,
     * and regardless of success, or failure. If either the original action, or
     * any of the resource action fails, then the <em>first</em> failure (aka
     * {@link Throwable} is rethrown. Example use:
     * <pre>
     *   final FileInputStream fis = new FileInputStream("my.file");
     *   Functions.tryWithResources(useInputStream(fis), null, () -&gt; fis.close());
     * </pre>
     * @param pAction The action to execute. This object <em>will</em> always
     *   be invoked.
     * @param pErrorHandler An optional error handler, which will be invoked finally,
     *   if any error occurred. The error handler will receive the first
     *   error, aka {@link Throwable}.
     * @param pResources The resource actions to execute. <em>All</em> resource
     *   actions will be invoked, in the given order. A resource action is an
     *   instance of {@link FailableRunnable}, which will be executed.
     * @see #tryWithResources(FailableRunnable, FailableRunnable...)
     */
    @SafeVarargs
    public static void tryWithResources(FailableRunnable<? extends Throwable> pAction,
                                            FailableConsumer<Throwable, ? extends Throwable> pErrorHandler,
                                            FailableRunnable<? extends Throwable>... pResources) {
        final FailableConsumer<Throwable, ? extends Throwable> errorHandler;
        if (pErrorHandler == null) {
            errorHandler = (t) -> rethrow(t);
        } else {
            errorHandler = pErrorHandler;
        }
        if (pResources != null) {
            for (FailableRunnable<? extends Throwable> runnable : pResources) {
                if (runnable == null) {
                    throw new NullPointerException("A resource action must not be null.");
                }
            }
        }
        Throwable th = null;
        try {
            pAction.run();
        } catch (Throwable t) {
            th = t;
        }
        if (pResources != null) {
            for (FailableRunnable<? extends Object> runnable : pResources) {
                try {
                    runnable.run();
                } catch (Throwable t) {
                    if (th == null) {
                        th = t;
                    }
                }
            }
        }
        if (th != null) {
            try {
                errorHandler.accept(th);
            } catch (Throwable t) {
                throw rethrow(t);
            }
        }
    }

    /**
     * A simple try-with-resources implementation, that can be used, if your
     * objects do not implement the {@link AutoCloseable} interface. The method
     * executes the {@code pAction}. The method guarantees, that <em>all</em>
     * the {@code pResources} are being executed, in the given order, afterwards,
     * and regardless of success, or failure. If either the original action, or
     * any of the resource action fails, then the <em>first</em> failure (aka
     * {@link Throwable} is rethrown. Example use:
     * <pre>
     *   final FileInputStream fis = new FileInputStream("my.file");
     *   Functions.tryWithResources(useInputStream(fis), () -&gt; fis.close());
     * </pre>
     * @param pAction The action to execute. This object <em>will</em> always
     *   be invoked.
     * @param pResources The resource actions to execute. <em>All</em> resource
     *   actions will be invoked, in the given order. A resource action is an
     *   instance of {@link FailableRunnable}, which will be executed.
     * @see #tryWithResources(FailableRunnable, FailableConsumer, FailableRunnable...)
     */
    @SafeVarargs
    public static void tryWithResources(FailableRunnable<? extends Throwable> pAction,
                                            FailableRunnable<? extends Throwable>... pResources) {
        tryWithResources(pAction, null, pResources);
    }

    /**
     * Rethrows a {@link Throwable} as an unchecked exception.
     * @param pThrowable The throwable to rethrow
     * @return Never returns anything, this method never terminates normally
     */
    public static RuntimeException rethrow(Throwable pThrowable) {
        if (pThrowable == null) {
            throw new NullPointerException("The Throwable must not be null.");
        } else if (pThrowable instanceof RuntimeException) {
            throw (RuntimeException) pThrowable;
        } else if (pThrowable instanceof Error) {
            throw (Error) pThrowable;
        } else if (pThrowable instanceof IOException) {
            throw new UncheckedIOException((IOException) pThrowable);
        } else {
            throw new UndeclaredThrowableException(pThrowable);
        }
    }
}
