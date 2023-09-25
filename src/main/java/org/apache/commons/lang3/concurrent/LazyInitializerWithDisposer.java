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
package org.apache.commons.lang3.concurrent;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.Optional;

import org.apache.commons.lang3.function.FailableConsumer;
import org.apache.commons.lang3.function.FailableSupplier;

/**
 * This class provides a generic implementation of the lazy initialization
 * pattern and also contains a way to dispose of the wrapped object if it has
 * been previously created by using a supplier and a consumer respectively.
 * To ensure the supplier and disposer are only used once each this class 
 * uses the double-check idiom for an instance field as discussed in  Joshua 
 * Bloch's "Effective Java", 2nd edition, item 71. 
 *
 * <p>
 * Sometimes an application has to deal with an object only under certain
 * circumstances, e.g. when the user selects a specific menu item or if a
 * special event is received. If the creation of the object is costly or the
 * consumption of memory or other system resources is significant, it may make
 * sense to defer the creation of this object until it is really needed. This is
 * a use case for the lazy initialization pattern.
 * </p>
 *
 * <p>
 * If these objects must be disposed of, it would not make sense to create them
 * just so they can be disposed of. Therefore this class provides the ability to
 * dispose of objects if and only if they have been created.
 * </p>
 *
 * <p>
 * Access to the data object is provided through the {@code get()} method. and
 * the data object is disposed with the {@code dispose()} So, code that obtains
 * and eventually disposes of the {@code ComplexObject} instance would simply look
 * like this:
 * </p>
 *
 * <pre>
 * // Create the supplier and disposer: 
 * Supplier<ComplexObject> initializer = () -> new ComplexObject();
 * Consumer<ComplexObject> disposer = complexObject -> complexObject.shutDown();
 *
 * // Create an instance of the lazy initializer
 * ComplexObjectInitializer initializer = new ComplexObjectInitializer(initializer, disposer);
 * ...
 * // When the object is actually needed:
 * ComplexObject cobj = initializer.get();
 * 
 * // When it is time to dispose of the object
 * initializer.dispose();
 * </pre>
 *
 * <p>
 * If multiple threads call the {@code get()} method when the object has not yet
 * been created, they are blocked until initialization completes. The algorithm
 * guarantees that only a single instance of the wrapped object class is
 * created, which is passed to all callers. Once initialized, calls to the
 * {@code get()} method are pretty fast because no synchronization is needed
 * (only an access to a <b>volatile</b> member field).
 * </p>
 *
 * @since 3.14.0
 * @param <T> the type of the object managed by this initializer class
 */
public class LazyInitializerWithDisposer<T> implements ConcurrentInitializer<T> {

    //NO_INIT serves double duty as the lock object to prevent any other class acquiring the monitor
    private final Object NO_INIT = new Object(){};
    private final Object DISPOSED = new Object(){};

    private FailableConsumer<? super T, ? extends Exception> disposer;
    private FailableSupplier<? extends T, ? extends Exception> initializer;

    // Stores the managed object.
    private volatile T object = (T) NO_INIT;

    private int allowedTries;
    Exception firstFailure = null;

    /** 
     * Constructs a LazyInitializerWithDisposer with a given initializer and disposer
     *
     * @param initializer an implimentation of the FailableSupplier functional interface which will create the wrapped object
     * @param disposer an implimentation of the FailableConsumer functional interface which will dispose of the wrapped object
     * @param allowedTries how many calls to get() will be allowed to attempt to initialize in total, before a failure is cached and becomes persistent. Set to a negative number for infinite retries.
     */
    public LazyInitializerWithDisposer(FailableSupplier<? extends T, ? extends Exception> initializer, FailableConsumer<? super T, ? extends Exception> disposer, int allowedTries) {
        if (allowedTries == 0) {
           throw new IllegalArgumentException("allowedTries must be a positive or negative number");
        }
        this.allowedTries = allowedTries;

        this.initializer = initializer;
        this.disposer = disposer;
    }

    /** 
     * Constructs a LazyInitializerWithDisposer wtih a given initializer and disposer
     *
     * @param initializer an implimentation of the FailableSupplier functional interface which will create the wrapped object
     * @param disposer an implimentation of the FailableConsumer functional interface which will dispose of the wrapped object
     */
    public LazyInitializerWithDisposer(FailableSupplier<? extends T, ? extends Exception> initializer, FailableConsumer<? super T, ? extends Exception> disposer) {
        this(initializer, disposer, 4);
    }

    /**
     * Returns the object wrapped by this instance. On first access the object
     * is created. After that it is cached and can be accessed pretty fast.
     *
     * @return the object initialized by this {@link LazyInitializer}
     * @throws ConcurrentException if an error occurred during initialization of
     * the object. Or enough previous errors have occurred to use up all allowed tries.
     * @throws AlreadyDisposedException if dispose() or close() has already been called.
     */
    @Override
    public T get() throws ConcurrentException {
        return getInternal();
    }

    /**
     * Returns the object wrapped by this instance inside an Optional. On first access
     * the object is created. After that it is cached and can be accessed pretty fast.
     *
     * @return an Optional wrapping object initialized by this {@link LazyInitializer}
     * or an empty Optional if dispose() or close() has already been called.
     * @throws ConcurrentException if an error occurred during initialization of
     * the object. Or enough previous errors have occurred to use up all allowed tries.
     */
    public Optional<T> getIfPossible() throws ConcurrentException {
        try {
            return Optional.ofNullable(getInternal());
        } catch (AlreadyDisposedException e) {
            return Optional.empty();
        }
    }

    @SuppressWarnings("unchecked")
    private T getInternal() throws ConcurrentException {
        // use a temporary variable to reduce the number of reads of the
        // volatile field
        T result = object;

        if (result == DISPOSED) {
            throw new AlreadyDisposedException();
        }

        if (result == NO_INIT) {
            synchronized (NO_INIT) {
                result = object;

                if (result == DISPOSED) {
                    throw new AlreadyDisposedException();
                }

                if (result == NO_INIT) {
                    if (allowedTries == 0) {
                        this.initializer = null;
                        ConcurrentException ce = new ConcurrentException();
                        ce.addSuppressed(firstFailure);
                        throw ce;
                    } else if (allowedTries > 0) {
                        allowedTries --;
                    }

                    try {
                        object = result = initializer.get();
                        this.initializer = null;
                    } catch (RuntimeException e) {//So it doesn't get wrapped in the next block.
                        if (firstFailure == null) {
                            firstFailure = e;
                        } else {
                            firstFailure.addSuppressed(e); 
                        }
                        throw e; 
                    } catch (Exception e) {
                        if (firstFailure == null) {//Duplicate this code rather than use a finally block to avoid going into a finally block on every successful get
                            firstFailure = e;
                        } else {
                            firstFailure.addSuppressed(e); 
                        }
                        throw new ConcurrentException(e); 
                    }
                }
            }
        }
        return result;
    }

    /**
     * Disposes the object wrapped by this instance if it has been created and closes
     * this intializer.
     *
     * This method will only attempt to dispose an object if it has successfully been
     * initialized. It will then close this LazyInitializerWithDisposer permemently 
     * regardless of the state before the method was called, or any exceptions during
     * disposal. Subsequent calls will have no effect.
     *
     * @return true if the object was successfully disposed, otherwise false
     * @throws ConcurrentException if an error occurred during disposal of
     * the object. The state will still be set to disposed.
     */
    public boolean dispose() throws ConcurrentException {
        return disposeInternal(disposer);
    }

    /**
     * Closes this initializer without disposing the wrapped object. This is equivalent
     * to calling dispose() with a no-op consumer.
     * 
     * @return true if the object was previously created and not previously disposed, otherwise false
     */
    public boolean close() {
        try {
            return disposeInternal(ignored -> {});
        } catch (ConcurrentException ignored) {//a no-op consumer will never throw anything.
            return false;
        }
    }

    private boolean disposeInternal(FailableConsumer<? super T, ? extends Exception> disposer) throws ConcurrentException {
        T disposable = object;

        if (disposable != DISPOSED) {
            synchronized (NO_INIT) {
                disposable = object;

                if (disposable == DISPOSED) {
                    return false;
                }

                if (disposable == NO_INIT) {
                    object = (T) DISPOSED;
                    return false;
                }

                try {
                    object = (T) DISPOSED;
                    disposer.accept(disposable);
                    return true;
                } catch (RuntimeException e) {//So it doesn't get wrapped in the next block.
                    throw e; 
                } catch (Exception e) {
                    throw new ConcurrentException(e); 
                } finally {
                    this.initializer = null;
                    this.disposer = null;
                }
            }
        }
        return false;
    }

    /**
     * Acquire the object wrapped by this LazyInitializerWithDisposer but do not initialize it if it has not been initialized
     * 
     * @return An Optional that will contain the value produced by get(), or it will be empty if this LazyInitializerWithDisposer does not currently contain a wrapped object.
     */
    public Optional<T> peek() {
        return Optional.ofNullable((T) object).filter(o -> ! (o == NO_INIT || o == DISPOSED));
    }

    /**
     * @return true if the object was previously created, and the initializer has neither been closed or disposed, otherwise false.
     */
    public boolean isReady() {
        return (! (object == NO_INIT || object == DISPOSED) );
    }

    /**
     * @return true if initializer has been closed or disposed, otherwise false.
     */
    public boolean isDisposed() {
        return object == DISPOSED;
    }
}
