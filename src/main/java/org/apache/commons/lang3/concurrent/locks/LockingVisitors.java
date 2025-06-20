/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang3.concurrent.locks;

import java.util.Objects;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.concurrent.locks.StampedLock;
import java.util.function.Supplier;

import org.apache.commons.lang3.builder.AbstractSupplier;
import org.apache.commons.lang3.function.Failable;
import org.apache.commons.lang3.function.FailableConsumer;
import org.apache.commons.lang3.function.FailableFunction;
import org.apache.commons.lang3.function.Suppliers;

/**
 * Combines the monitor and visitor pattern to work with {@link java.util.concurrent.locks.Lock locked objects}. Locked
 * objects are an alternative to synchronization. This, on Wikipedia, is known as the Visitor pattern
 * (https://en.wikipedia.org/wiki/Visitor_pattern), and from the "Gang of Four" "Design Patterns" book's Visitor pattern
 * [Gamma, E., Helm, R., &amp; Johnson, R. (1998). Visitor. In Design patterns elements of reusable object oriented software (pp. 331-344). Reading: Addison Wesley.].
 *
 * <p>
 * Locking is preferable, if there is a distinction between read access (multiple threads may have read access
 * concurrently), and write access (only one thread may have write access at any given time). In comparison,
 * synchronization doesn't support read access, because synchronized access is exclusive.
 * </p>
 * <p>
 * Using this class is fairly straightforward:
 * </p>
 * <ol>
 * <li>While still in single thread mode, create an instance of {@link LockingVisitors.StampedLockVisitor} by calling
 * {@link #stampedLockVisitor(Object)}, passing the object which needs to be locked. Discard all references to the
 * locked object. Instead, use references to the lock.</li>
 * <li>If you want to access the locked object, create a {@link FailableConsumer}. The consumer will receive the locked
 * object as a parameter. For convenience, the consumer may be implemented as a Lambda. Then invoke
 * {@link LockingVisitors.StampedLockVisitor#acceptReadLocked(FailableConsumer)}, or
 * {@link LockingVisitors.StampedLockVisitor#acceptWriteLocked(FailableConsumer)}, passing the consumer.</li>
 * <li>As an alternative, if you need to produce a result object, you may use a {@link FailableFunction}. This function
 * may also be implemented as a Lambda. To have the function executed, invoke
 * {@link LockingVisitors.StampedLockVisitor#applyReadLocked(FailableFunction)}, or
 * {@link LockingVisitors.StampedLockVisitor#applyWriteLocked(FailableFunction)}.</li>
 * </ol>
 * <p>
 * Example: A thread safe logger class.
 * </p>
 *
 * <pre>{@code
 *   public class SimpleLogger {
 *
 *     private final StampedLockVisitor<PrintStream> lock;
 *     private final PrintStream ps;
 *
 *     public SimpleLogger(OutputStream out) {
 *         ps = new PrintStream(out);
 *         lock = LockingVisitors.stampedLockVisitor(ps);
 *     }
 *
 *     public void log(String message) {
 *         lock.acceptWriteLocked(ps -> ps.println(message));
 *     }
 *
 *     public void log(byte[] buffer) {
 *         lock.acceptWriteLocked(ps -> { ps.write(buffer); ps.println(); });
 *     }
 * }
 * }
 * </pre>
 *
 * @since 3.11
 */
public class LockingVisitors {

    /**
     * Wraps a domain object and a lock for access by lambdas.
     *
     * @param <O> the wrapped object type.
     * @param <L> the wrapped lock type.
     */
    public static class LockVisitor<O, L> {

        /**
         * Builds {@link LockVisitor} instances.
         *
         * @param <O> the wrapped object type.
         * @param <L> the wrapped lock type.
         * @param <B> the builder type.
         * @since 3.18.0
         */
        public static class LVBuilder<O, L, B extends LVBuilder<O, L, B>> extends AbstractSupplier<LockVisitor<O, L>, B, RuntimeException> {

            /**
             * The lock object, untyped, since, for example {@link StampedLock} does not implement a locking interface in
             * Java 8.
             */
            L lock;

            /**
             * The guarded object.
             */
            O object;

            /**
             * Supplies the read lock, usually from the lock object.
             */
            private Supplier<Lock> readLockSupplier;

            /**
             * Supplies the write lock, usually from the lock object.
             */
            private Supplier<Lock> writeLockSupplier;

            /**
             * Constructs a new instance.
             */
            public LVBuilder() {
                // empty
            }

            @Override
            public LockVisitor<O, L> get() {
                return new LockVisitor<>(this);
            }

            Supplier<Lock> getReadLockSupplier() {
                return readLockSupplier;
            }


            Supplier<Lock> getWriteLockSupplier() {
                return writeLockSupplier;
            }

            /**
             * Set the lock used from accept methods.
             *
             * @param lock the lock.
             * @return {@code this} instance.
             */
            public B setLock(final L lock) {
                this.lock = lock;
                return asThis();
            }

            /**
             * Set the resource.
             *
             * @param object the resource.
             * @return {@code this} instance.
             */
            public B setObject(final O object) {
                this.object = object;
                return asThis();
            }

            /**
             * Supplies the read lock.
             *
             * @param readLockSupplier Supplies the read lock.
             * @return {@code this} instance.
             */
            public B setReadLockSupplier(final Supplier<Lock> readLockSupplier) {
                this.readLockSupplier = readLockSupplier;
                return asThis();
            }

            /**
             * Supplies the write lock.
             *
             * @param writeLockSupplier Supplies the write lock.
             * @return {@code this} instance.
             */
            public B setWriteLockSupplier(final Supplier<Lock> writeLockSupplier) {
                this.writeLockSupplier = writeLockSupplier;
                return asThis();
            }
        }

        /**
         * The lock object, untyped, since, for example {@link StampedLock} does not implement a locking interface in
         * Java 8.
         */
        private final L lock;

        /**
         * The guarded object.
         */
        private final O object;

        /**
         * Supplies the read lock, usually from the lock object.
         */
        private final Supplier<Lock> readLockSupplier;

        /**
         * Supplies the write lock, usually from the lock object.
         */
        private final Supplier<Lock> writeLockSupplier;

        /**
         * Constructs an instance from a builder.
         *
         * @param builder The builder.
         */
        private LockVisitor(final LVBuilder<O, L, ?> builder) {
            this.object = Objects.requireNonNull(builder.object, "object");
            this.lock = Objects.requireNonNull(builder.lock, "lock");
            this.readLockSupplier = Objects.requireNonNull(builder.readLockSupplier, "readLockSupplier");
            this.writeLockSupplier = Objects.requireNonNull(builder.writeLockSupplier, "writeLockSupplier");
        }

        /**
         * Constructs an instance.
         *
         * @param object The object to guard.
         * @param lock The locking object.
         * @param readLockSupplier Supplies the read lock, usually from the lock object.
         * @param writeLockSupplier Supplies the write lock, usually from the lock object.
         */
        protected LockVisitor(final O object, final L lock, final Supplier<Lock> readLockSupplier, final Supplier<Lock> writeLockSupplier) {
            this.object = Objects.requireNonNull(object, "object");
            this.lock = Objects.requireNonNull(lock, "lock");
            this.readLockSupplier = Objects.requireNonNull(readLockSupplier, "readLockSupplier");
            this.writeLockSupplier = Objects.requireNonNull(writeLockSupplier, "writeLockSupplier");
        }

        /**
         * Provides read (shared, non-exclusive) access to the locked (hidden) object. More precisely, what the method
         * will do (in the given order):
         *
         * <ol>
         * <li>Obtain a read (shared) lock on the locked (hidden) object. The current thread may block, until such a
         * lock is granted.</li>
         * <li>Invokes the given {@link FailableConsumer consumer}, passing the locked object as the parameter.</li>
         * <li>Release the lock, as soon as the consumers invocation is done. If the invocation results in an error, the
         * lock will be released anyways.</li>
         * </ol>
         *
         * @param consumer The consumer, which is being invoked to use the hidden object, which will be passed as the
         *        consumers parameter.
         * @see #acceptWriteLocked(FailableConsumer)
         * @see #applyReadLocked(FailableFunction)
         */
        public void acceptReadLocked(final FailableConsumer<O, ?> consumer) {
            lockAcceptUnlock(readLockSupplier, consumer);
        }

        /**
         * Provides write (exclusive) access to the locked (hidden) object. More precisely, what the method will do (in
         * the given order):
         *
         * <ol>
         * <li>Obtain a write (shared) lock on the locked (hidden) object. The current thread may block, until such a
         * lock is granted.</li>
         * <li>Invokes the given {@link FailableConsumer consumer}, passing the locked object as the parameter.</li>
         * <li>Release the lock, as soon as the consumers invocation is done. If the invocation results in an error, the
         * lock will be released anyways.</li>
         * </ol>
         *
         * @param consumer The consumer, which is being invoked to use the hidden object, which will be passed as the
         *        consumers parameter.
         * @see #acceptReadLocked(FailableConsumer)
         * @see #applyWriteLocked(FailableFunction)
         */
        public void acceptWriteLocked(final FailableConsumer<O, ?> consumer) {
            lockAcceptUnlock(writeLockSupplier, consumer);
        }

        /**
         * Provides read (shared, non-exclusive) access to the locked (hidden) object for the purpose of computing a
         * result object. More precisely, what the method will do (in the given order):
         *
         * <ol>
         * <li>Obtain a read (shared) lock on the locked (hidden) object. The current thread may block, until such a
         * lock is granted.</li>
         * <li>Invokes the given {@link FailableFunction function}, passing the locked object as the parameter,
         * receiving the functions result.</li>
         * <li>Release the lock, as soon as the consumers invocation is done. If the invocation results in an error, the
         * lock will be released anyways.</li>
         * <li>Return the result object, that has been received from the functions invocation.</li>
         * </ol>
         * <p>
         * <em>Example:</em> Consider that the hidden object is a list, and we wish to know the current size of the
         * list. This might be achieved with the following:
         * </p>
         * <pre>{@code
         * private Lock<List<Object>> listLock;
         *
         * public int getCurrentListSize() {
         *     final Integer sizeInteger = listLock.applyReadLocked(list -> Integer.valueOf(list.size));
         *     return sizeInteger.intValue();
         * }
         * }
         * </pre>
         *
         * @param <T> The result type (both the functions, and this method's.)
         * @param function The function, which is being invoked to compute the result. The function will receive the
         *        hidden object.
         * @return The result object, which has been returned by the functions invocation.
         * @throws IllegalStateException The result object would be, in fact, the hidden object. This would extend
         *         access to the hidden object beyond this methods lifetime and will therefore be prevented.
         * @see #acceptReadLocked(FailableConsumer)
         * @see #applyWriteLocked(FailableFunction)
         */
        public <T> T applyReadLocked(final FailableFunction<O, T, ?> function) {
            return lockApplyUnlock(readLockSupplier, function);
        }

        /**
         * Provides write (exclusive) access to the locked (hidden) object for the purpose of computing a result object.
         * More precisely, what the method will do (in the given order):
         *
         * <ol>
         * <li>Obtain a read (shared) lock on the locked (hidden) object. The current thread may block, until such a
         * lock is granted.</li>
         * <li>Invokes the given {@link FailableFunction function}, passing the locked object as the parameter,
         * receiving the functions result.</li>
         * <li>Release the lock, as soon as the consumers invocation is done. If the invocation results in an error, the
         * lock will be released anyways.</li>
         * <li>Return the result object, that has been received from the functions invocation.</li>
         * </ol>
         *
         * @param <T> The result type (both the functions, and this method's.)
         * @param function The function, which is being invoked to compute the result. The function will receive the
         *        hidden object.
         * @return The result object, which has been returned by the functions invocation.
         * @throws IllegalStateException The result object would be, in fact, the hidden object. This would extend
         *         access to the hidden object beyond this methods lifetime and will therefore be prevented.
         * @see #acceptReadLocked(FailableConsumer)
         * @see #applyWriteLocked(FailableFunction)
         */
        public <T> T applyWriteLocked(final FailableFunction<O, T, ?> function) {
            return lockApplyUnlock(writeLockSupplier, function);
        }

        /**
         * Gets the lock.
         *
         * @return the lock.
         */
        public L getLock() {
            return lock;
        }

        /**
         * Gets the guarded object.
         *
         * @return the object.
         */
        public O getObject() {
            return object;
        }

        /**
         * This method provides the default implementation for {@link #acceptReadLocked(FailableConsumer)}, and
         * {@link #acceptWriteLocked(FailableConsumer)}.
         *
         * @param lockSupplier A supplier for the lock. (This provides, in fact, a long, because a {@link StampedLock} is used
         *        internally.)
         * @param consumer The consumer, which is to be given access to the locked (hidden) object, which will be passed
         *        as a parameter.
         * @see #acceptReadLocked(FailableConsumer)
         * @see #acceptWriteLocked(FailableConsumer)
         */
        protected void lockAcceptUnlock(final Supplier<Lock> lockSupplier, final FailableConsumer<O, ?> consumer) {
            final Lock lock = Objects.requireNonNull(Suppliers.get(lockSupplier), "lock");
            lock.lock();
            try {
                Failable.accept(consumer, object);
            } finally {
                lock.unlock();
            }
        }

        /**
         * This method provides the actual implementation for {@link #applyReadLocked(FailableFunction)}, and
         * {@link #applyWriteLocked(FailableFunction)}.
         *
         * @param <T> The result type (both the functions, and this method's.)
         * @param lockSupplier A supplier for the lock. (This provides, in fact, a long, because a {@link StampedLock} is used
         *        internally.)
         * @param function The function, which is being invoked to compute the result object. This function will receive
         *        the locked (hidden) object as a parameter.
         * @return The result object, which has been returned by the functions invocation.
         * @throws IllegalStateException The result object would be, in fact, the hidden object. This would extend
         *         access to the hidden object beyond this methods lifetime and will therefore be prevented.
         * @see #applyReadLocked(FailableFunction)
         * @see #applyWriteLocked(FailableFunction)
         */
        protected <T> T lockApplyUnlock(final Supplier<Lock> lockSupplier, final FailableFunction<O, T, ?> function) {
            final Lock lock = Objects.requireNonNull(Suppliers.get(lockSupplier), "lock");
            lock.lock();
            try {
                return Failable.apply(function, object);
            } finally {
                lock.unlock();
            }
        }

    }

    /**
     * This class implements a wrapper for a locked (hidden) object, and provides the means to access it. The basic
     * idea, is that the user code forsakes all references to the locked object, using only the wrapper object, and the
     * accessor methods {@link #acceptReadLocked(FailableConsumer)}, {@link #acceptWriteLocked(FailableConsumer)},
     * {@link #applyReadLocked(FailableFunction)}, and {@link #applyWriteLocked(FailableFunction)}. By doing so, the
     * necessary protections are guaranteed.
     *
     * @param <O> The locked (hidden) objects type.
     */
    public static class ReadWriteLockVisitor<O> extends LockVisitor<O, ReadWriteLock> {

        /**
         * Builds {@link LockVisitor} instances.
         *
         * @param <O> the wrapped object type.
         * @since 3.18.0
         */
        public static class Builder<O> extends LVBuilder<O, ReadWriteLock, Builder<O>> {

            /**
             * Constructs a new instance.
             */
            public Builder() {
                // empty
            }

            @Override
            public ReadWriteLockVisitor<O> get() {
                return new ReadWriteLockVisitor<>(this);
            }

            @Override
            public Builder<O> setLock(final ReadWriteLock readWriteLock) {
                setReadLockSupplier(readWriteLock::readLock);
                setWriteLockSupplier(readWriteLock::writeLock);
                return super.setLock(readWriteLock);
            }
        }

        /**
         * Creates a new builder.
         *
         * @param <O> the wrapped object type.
         * @return a new builder.
         * @since 3.18.0
         */
        public static <O> Builder<O> builder() {
            return new Builder<>();
        }

        /**
         * Constructs a new instance from a builder.
         *
         * @param builder a builder.
         */
        private ReadWriteLockVisitor(final Builder<O> builder) {
            super(builder);
        }

        /**
         * Creates a new instance with the given locked object. This constructor is supposed to be used for subclassing
         * only. In general, it is suggested to use {@link LockingVisitors#stampedLockVisitor(Object)} instead.
         *
         * @param object The locked (hidden) object. The caller is supposed to drop all references to the locked object.
         * @param readWriteLock the lock to use.
         */
        protected ReadWriteLockVisitor(final O object, final ReadWriteLock readWriteLock) {
            super(object, readWriteLock, readWriteLock::readLock, readWriteLock::writeLock);
        }

    }

    /**
     * This class implements a wrapper for a locked (hidden) object, and provides the means to access it. The basic
     * idea, is that the user code forsakes all references to the locked object, using only the wrapper object, and the
     * accessor methods {@link #acceptReadLocked(FailableConsumer)}, {@link #acceptWriteLocked(FailableConsumer)},
     * {@link #applyReadLocked(FailableFunction)}, and {@link #applyWriteLocked(FailableFunction)}. By doing so, the
     * necessary protections are guaranteed.
     *
     * @param <O> The locked (hidden) objects type.
     * @since 3.18.0
     */
    public static class ReentrantLockVisitor<O> extends LockVisitor<O, ReentrantLock> {

        /**
         * Builds {@link LockVisitor} instances.
         *
         * @param <O> the wrapped object type.
         * @since 3.18.0
         */
        public static class Builder<O> extends LVBuilder<O, ReentrantLock, Builder<O>> {

            /**
             * Constructs a new instance.
             */
            public Builder() {
                // empty
            }

            @Override
            public ReentrantLockVisitor<O> get() {
                return new ReentrantLockVisitor<>(this);
            }


            @Override
            public Builder<O> setLock(final ReentrantLock reentrantLock) {
                setReadLockSupplier(() -> reentrantLock);
                setWriteLockSupplier(() -> reentrantLock);
                return super.setLock(reentrantLock);
            }
        }

        /**
         * Creates a new builder.
         *
         * @param <O> the wrapped object type.
         * @return a new builder.
         * @since 3.18.0
         */
        public static <O> Builder<O> builder() {
            return new Builder<>();
        }

        /**
         * Constructs a new instance from a builder.
         *
         * @param builder a builder.
         */
        private ReentrantLockVisitor(final Builder<O> builder) {
            super(builder);
        }


        /**
         * Creates a new instance with the given locked object. This constructor is supposed to be used for subclassing
         * only. In general, it is suggested to use {@link LockingVisitors#reentrantLockVisitor(Object)} instead.
         *
         * @param object The locked (hidden) object. The caller is supposed to drop all references to the locked object.
         * @param reentrantLock the lock to use.
         */
        protected ReentrantLockVisitor(final O object, final ReentrantLock reentrantLock) {
            super(object, reentrantLock, () -> reentrantLock, () -> reentrantLock);
        }
    }

    /**
     * This class implements a wrapper for a locked (hidden) object, and provides the means to access it. The basic
     * idea is that the user code forsakes all references to the locked object, using only the wrapper object, and the
     * accessor methods {@link #acceptReadLocked(FailableConsumer)}, {@link #acceptWriteLocked(FailableConsumer)},
     * {@link #applyReadLocked(FailableFunction)}, and {@link #applyWriteLocked(FailableFunction)}. By doing so, the
     * necessary protections are guaranteed.
     *
     * @param <O> The locked (hidden) objects type.
     */
    public static class StampedLockVisitor<O> extends LockVisitor<O, StampedLock> {

        /**
         * Builds {@link LockVisitor} instances.
         *
         * @param <O> the wrapped object type.
         * @since 3.18.0
         */
        public static class Builder<O> extends LVBuilder<O, StampedLock, Builder<O>> {

            /**
             * Constructs a new instance.
             */
            public Builder() {
                // empty
            }

            @Override
            public StampedLockVisitor<O> get() {
                return new StampedLockVisitor<>(this);
            }


            @Override
            public Builder<O> setLock(final StampedLock stampedLock) {
                setReadLockSupplier(stampedLock::asReadLock);
                setWriteLockSupplier(stampedLock::asWriteLock);
                return super.setLock(stampedLock);
            }
        }

        /**
         * Creates a new builder.
         *
         * @param <O> the wrapped object type.
         * @return a new builder.
         * @since 3.18.0
         */
        public static <O> Builder<O> builder() {
            return new Builder<>();
        }

        /**
         * Constructs a new instance from a builder.
         *
         * @param builder a builder.
         */
        private StampedLockVisitor(final Builder<O> builder) {
            super(builder);
        }

        /**
         * Creates a new instance with the given locked object. This constructor is supposed to be used for subclassing
         * only. In general, it is suggested to use {@link LockingVisitors#stampedLockVisitor(Object)} instead.
         *
         * @param object The locked (hidden) object. The caller is supposed to drop all references to the locked object.
         * @param stampedLock the lock to use.
         */
        protected StampedLockVisitor(final O object, final StampedLock stampedLock) {
            super(object, stampedLock, stampedLock::asReadLock, stampedLock::asWriteLock);
        }
    }

    /**
     * Creates a new instance of {@link ReadWriteLockVisitor} with the given (hidden) object and lock.
     *
     * @param <O> The locked objects type.
     * @param object The locked (hidden) object.
     * @param readWriteLock The lock to use.
     * @return The created instance, a {@link StampedLockVisitor lock} for the given object.
     * @since 3.13.0
     */
    public static <O> ReadWriteLockVisitor<O> create(final O object, final ReadWriteLock readWriteLock) {
        return new LockingVisitors.ReadWriteLockVisitor<>(object, readWriteLock);
    }

    /**
     * Creates a new instance of {@link ReadWriteLockVisitor} with the given (hidden) object and lock.
     *
     * @param <O> The locked objects type.
     * @param object The locked (hidden) object.
     * @param reentrantLock The lock to use.
     * @return The created instance, a {@link StampedLockVisitor lock} for the given object.
     * @since 3.18.0
     */
    public static <O> ReentrantLockVisitor<O> create(final O object, final ReentrantLock reentrantLock) {
        return new LockingVisitors.ReentrantLockVisitor<>(object, reentrantLock);
    }

    /**
     * Creates a new instance of {@link ReadWriteLockVisitor} with the given (hidden) object.
     *
     * @param <O> The locked objects type.
     * @param object The locked (hidden) object.
     * @return The created instance, a {@link StampedLockVisitor lock} for the given object.
     * @since 3.18.0
     */
    public static <O> ReentrantLockVisitor<O> reentrantLockVisitor(final O object) {
        return create(object, new ReentrantLock());
    }

    /**
     * Creates a new instance of {@link ReadWriteLockVisitor} with the given (hidden) object.
     *
     * @param <O> The locked objects type.
     * @param object The locked (hidden) object.
     * @return The created instance, a {@link StampedLockVisitor lock} for the given object.
     */
    public static <O> ReadWriteLockVisitor<O> reentrantReadWriteLockVisitor(final O object) {
        return create(object, new ReentrantReadWriteLock());
    }

    /**
     * Creates a new instance of {@link StampedLockVisitor} with the given (hidden) object.
     *
     * @param <O> The locked objects type.
     * @param object The locked (hidden) object.
     * @return The created instance, a {@link StampedLockVisitor lock} for the given object.
     */
    public static <O> StampedLockVisitor<O> stampedLockVisitor(final O object) {
        return new LockingVisitors.StampedLockVisitor<>(object, new StampedLock());
    }

    /**
     * Make private in 4.0.
     *
     * @deprecated TODO Make private in 4.0.
     */
    @Deprecated
    public LockingVisitors() {
        // empty
    }
}
