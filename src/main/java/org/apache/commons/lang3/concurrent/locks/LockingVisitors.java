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
 * Combines the monitor and visitor pattern to work with {@link Lock}s as an alternative to synchronization.
 * <p>
 * The read and write methods use the locks supplied by the visitor. A {@link ReentrantLockVisitor} uses one exclusive lock for both methods.
 * A {@link ReadWriteLockVisitor} uses the underlying read and write locks, while a {@link StampedLockVisitor} uses its read and write
 * {@link Lock} views. Read operations may run concurrently only when the supplied lock supports shared reads.
 * </p>
 * <p>
 * For example, to use this class with a {@link ReentrantLock}:
 * </p>
 * <ol>
 * <li>In single threaded mode, call {@link #reentrantLockVisitor(Object)}, passing the object to protect. This creates a
 * {@link LockingVisitors.ReentrantLockVisitor}
 * </li>
 * <li>To access the protected object, create a {@link FailableConsumer} lambda. The consumer will receive the object as a parameter while the visitor holds the
 * lock. Then call
 * {@link LockingVisitors.LockVisitor#acceptReadLocked(FailableConsumer)}, or
 * {@link LockingVisitors.LockVisitor#acceptWriteLocked(FailableConsumer)}, passing the consumer.
 * </li>
 * <li>Alternatively, to receive a result object, use a {@link FailableFunction} lambda. To have the function executed, call
 * {@link LockingVisitors.LockVisitor#applyReadLocked(FailableFunction)}, or
 * {@link LockingVisitors.LockVisitor#applyWriteLocked(FailableFunction)}.
 * </li>
 * </ol>
 * <p>
 * Example 1: A thread safe logger class using a {@link ReentrantLockVisitor}.
 * </p>
 *
 * <pre>{@code
 *   public class SimpleLogger1 {
 *
 *     private final ReentrantLockVisitor<PrintStream> lock;
 *     private final PrintStream ps;
 *
 *     public SimpleLogger(OutputStream out) {
 *         ps = new PrintStream(out);
 *         lock = LockingVisitors.reentrantLockVisitor(ps);
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
 * <p>
 * Example 2: A thread safe logger class using a {@link ReadWriteLockVisitor}.
 * </p>
 *
 * <pre>{@code
 *   public class SimpleLogger2 {
 *
 *     private final ReadWriteLockVisitor<PrintStream> lock;
 *     private final PrintStream ps;
 *
 *     public SimpleLogger(OutputStream out) {
 *         ps = new PrintStream(out);
 *         lock = LockingVisitors.readWriteLockVisitor(ps);
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
 * <p>
 * Example 3: A thread safe logger class using a {@link StampedLock}.
 * </p>
 *
 * <pre>{@code
 *   public class SimpleLogger3 {
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
     * @param <O> The wrapped object type.
     * @param <L> The wrapped lock type.
     * @see LockingVisitors
     */
    public static class LockVisitor<O, L> {

        /**
         * Builds {@link LockVisitor} instances.
         *
         * @param <O> The wrapped object type.
         * @param <L> The wrapped lock type.
         * @param <B> The builder type.
         * @since 3.18.0
         */
        public static class LVBuilder<O, L, B extends LVBuilder<O, L, B>> extends AbstractSupplier<LockVisitor<O, L>, B, RuntimeException> {

            /**
             * The underlying lock object. Its type varies because {@link StampedLock} does not implement {@link Lock} or
             * {@link ReadWriteLock}.
             */
            L lock;

            /**
             * The guarded object.
             */
            O object;

            /**
             * Supplies the lock used by read methods.
             */
            private Supplier<Lock> readLockSupplier;

            /**
             * Supplies the lock used by write methods.
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
             * Sets the underlying lock returned by {@link LockVisitor#getLock()}.
             *
             * @param lock The lock.
             * @return {@code this} instance.
             */
            public B setLock(final L lock) {
                this.lock = lock;
                return asThis();
            }

            /**
             * Sets the resource.
             *
             * @param object The resource.
             * @return {@code this} instance.
             */
            public B setObject(final O object) {
                this.object = object;
                return asThis();
            }

            /**
             * Sets the supplier of the lock used by read methods.
             *
             * @param readLockSupplier Supplies the read lock.
             * @return {@code this} instance.
             */
            public B setReadLockSupplier(final Supplier<Lock> readLockSupplier) {
                this.readLockSupplier = readLockSupplier;
                return asThis();
            }

            /**
             * Sets the supplier of the lock used by write methods.
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
         * The underlying lock object. Its type varies because {@link StampedLock} does not implement {@link Lock} or
         * {@link ReadWriteLock}.
         */
        private final L lock;

        /**
         * The guarded object.
         */
        private final O object;

        /**
         * Supplies the lock used by read methods.
         */
        private final Supplier<Lock> readLockSupplier;

        /**
         * Supplies the lock used by write methods.
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
         * @param readLockSupplier Supplies the lock used by read methods.
         * @param writeLockSupplier Supplies the lock used by write methods.
         */
        protected LockVisitor(final O object, final L lock, final Supplier<Lock> readLockSupplier, final Supplier<Lock> writeLockSupplier) {
            this.object = Objects.requireNonNull(object, "object");
            this.lock = Objects.requireNonNull(lock, "lock");
            this.readLockSupplier = Objects.requireNonNull(readLockSupplier, "readLockSupplier");
            this.writeLockSupplier = Objects.requireNonNull(writeLockSupplier, "writeLockSupplier");
        }

        /**
         * Invokes the consumer while holding the lock supplied for read operations.
         * The lock is released in a {@code finally} block after the consumer returns or throws. Whether other readers can proceed concurrently depends on the
         * supplied lock.
         *
         * @param consumer The consumer of the guarded object.
         * @see #acceptWriteLocked(FailableConsumer)
         * @see #applyReadLocked(FailableFunction)
         */
        public void acceptReadLocked(final FailableConsumer<O, ?> consumer) {
            lockAcceptUnlock(readLockSupplier, consumer);
        }

        /**
         * Invokes the consumer while holding the lock supplied for write operations.
         * The lock is released in a {@code finally} block after the consumer returns or throws.
         *
         * @param consumer The consumer of the guarded object.
         * @see #acceptReadLocked(FailableConsumer)
         * @see #applyWriteLocked(FailableFunction)
         */
        public void acceptWriteLocked(final FailableConsumer<O, ?> consumer) {
            lockAcceptUnlock(writeLockSupplier, consumer);
        }

        /**
         * Applies the function while holding the lock supplied for read operations.
         * The lock is released in a {@code finally} block after the function returns or throws. Whether other readers can proceed concurrently depends on the
         * supplied lock.
         *
         * @param <T> The result type.
         * @param function The function applied to the guarded object.
         * @return The function result.
         * @throws NullPointerException Thrown if the lock supplier is null or returns null.
         * @see #acceptReadLocked(FailableConsumer)
         * @see #applyWriteLocked(FailableFunction)
         */
        public <T> T applyReadLocked(final FailableFunction<O, T, ?> function) {
            return lockApplyUnlock(readLockSupplier, function);
        }

        /**
         * Applies the function while holding the lock supplied for write operations.
         * The lock is released in a {@code finally} block after the function returns or throws.
         *
         * @param <T> The result type.
         * @param function The function applied to the guarded object.
         * @return The function result.
         * @throws NullPointerException Thrown if the lock supplier is null or returns null.
         * @see #acceptWriteLocked(FailableConsumer)
         * @see #applyReadLocked(FailableFunction)
         */
        public <T> T applyWriteLocked(final FailableFunction<O, T, ?> function) {
            return lockApplyUnlock(writeLockSupplier, function);
        }

        /**
         * Gets the lock.
         *
         * @return The lock.
         */
        public L getLock() {
            return lock;
        }

        /**
         * Gets the guarded object.
         *
         * @return The object.
         */
        public O getObject() {
            return object;
        }

        /**
         * Implements {@link #acceptReadLocked(FailableConsumer)} and
         * {@link #acceptWriteLocked(FailableConsumer)}.
         *
         * @param lockSupplier Supplies the {@link Lock} to acquire and release, including a {@link StampedLock} view.
         * @param consumer The consumer of the guarded object.
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
         * Implements {@link #applyReadLocked(FailableFunction)} and
         * {@link #applyWriteLocked(FailableFunction)}.
         *
         * @param <T> The result type.
         * @param lockSupplier Supplies the {@link Lock} to acquire and release, including a {@link StampedLock} view.
         * @param function The function applied to the guarded object.
         * @return The function result.
         * @throws NullPointerException Thrown if the lock supplier is null or returns null.
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
     * Wraps a {@link ReadWriteLock} and object to protect. Read methods use {@link ReadWriteLock#readLock()}, and write methods use
     * {@link ReadWriteLock#writeLock()}. To access the object, use the methods {@link #acceptReadLocked(FailableConsumer)},
     * {@link #acceptWriteLocked(FailableConsumer)}, {@link #applyReadLocked(FailableFunction)}, and {@link #applyWriteLocked(FailableFunction)}. The visitor
     * holds the lock while the consumer or function is called.
     *
     * @param <O> The type of the object to protect.
     * @see LockingVisitors#create(Object, ReadWriteLock)
     */
    public static class ReadWriteLockVisitor<O> extends LockVisitor<O, ReadWriteLock> {

        /**
         * Builds {@link LockVisitor} instances.
         *
         * @param <O> The wrapped object type.
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
         * @param <O> The wrapped object type.
         * @return A new builder.
         * @since 3.18.0
         */
        public static <O> Builder<O> builder() {
            return new Builder<>();
        }

        /**
         * Constructs a new instance from a builder.
         *
         * @param builder A builder.
         */
        private ReadWriteLockVisitor(final Builder<O> builder) {
            super(builder);
        }

        /**
         * Creates a new instance with the given object and lock.
         *
         * @param object The object to protect. The caller is supposed to drop all references to the locked object.
         * @param readWriteLock The lock to use.
         * @see LockingVisitors
         */
        protected ReadWriteLockVisitor(final O object, final ReadWriteLock readWriteLock) {
            super(object, readWriteLock, readWriteLock::readLock, readWriteLock::writeLock);
        }

    }

    /**
     * Wraps a {@link ReentrantLock} and object to protect. Both read and write methods acquire the same exclusive lock.
     * To access the object, use the methods {@link #acceptReadLocked(FailableConsumer)},
     * {@link #acceptWriteLocked(FailableConsumer)}, {@link #applyReadLocked(FailableFunction)}, and {@link #applyWriteLocked(FailableFunction)}. The visitor
     * holds the lock while the consumer or function is called.
     *
     * @param <O> The type of the object to protect.
     * @see LockingVisitors#reentrantLockVisitor(Object)
     * @since 3.18.0
     */
    public static class ReentrantLockVisitor<O> extends LockVisitor<O, ReentrantLock> {

        /**
         * Builds {@link LockVisitor} instances.
         *
         * @param <O> The wrapped object type.
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
         * @param <O> The wrapped object type.
         * @return A new builder.
         * @since 3.18.0
         */
        public static <O> Builder<O> builder() {
            return new Builder<>();
        }

        /**
         * Constructs a new instance from a builder.
         *
         * @param builder A builder.
         */
        private ReentrantLockVisitor(final Builder<O> builder) {
            super(builder);
        }


        /**
         * Creates a new instance with the given object and lock.
         * <p>
         * This visitor uses the given {@link ReentrantLock} for both read and write methods; both acquire it exclusively.
         * </p>
         *
         * @param object The object to protect. The caller is supposed to drop all references to the locked object.
         * @param reentrantLock The lock to use.
         * @see LockingVisitors
         */
        protected ReentrantLockVisitor(final O object, final ReentrantLock reentrantLock) {
            super(object, reentrantLock, () -> reentrantLock, () -> reentrantLock);
        }
    }

    /**
     * Wraps a {@link StampedLock} and object to protect. Read methods use {@link StampedLock#asReadLock()}, and write methods use
     * {@link StampedLock#asWriteLock()}. To access the object, use the methods {@link #acceptReadLocked(FailableConsumer)},
     * {@link #acceptWriteLocked(FailableConsumer)}, {@link #applyReadLocked(FailableFunction)}, and {@link #applyWriteLocked(FailableFunction)}. The visitor
     * holds the lock while the consumer or function is called.
     *
     * @param <O> The type of the object to protect.
     * @see LockingVisitors#stampedLockVisitor(Object)
     */
    public static class StampedLockVisitor<O> extends LockVisitor<O, StampedLock> {

        /**
         * Builds {@link LockVisitor} instances.
         *
         * @param <O> The wrapped object type.
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
         * @param <O> The wrapped object type.
         * @return A new builder.
         * @since 3.18.0
         */
        public static <O> Builder<O> builder() {
            return new Builder<>();
        }

        /**
         * Constructs a new instance from a builder.
         *
         * @param builder A builder.
         */
        private StampedLockVisitor(final Builder<O> builder) {
            super(builder);
        }

        /**
         * Creates a new instance with the given object and lock.
         *
         * @param object The object to protect. The caller is supposed to drop all references to the locked object.
         * @param stampedLock The lock to use.
         * @see LockingVisitors
         */
        protected StampedLockVisitor(final O object, final StampedLock stampedLock) {
            super(object, stampedLock, stampedLock::asReadLock, stampedLock::asWriteLock);
        }
    }

    /**
     * Creates a new instance of {@link ReadWriteLockVisitor} with the given object and lock.
     *
     * @param <O> The type of the object to protect.
     * @param object The object to protect.
     * @param readWriteLock The lock to use.
     * @return A new {@link ReadWriteLockVisitor}.
     * @see LockingVisitors
     * @since 3.13.0
     */
    public static <O> ReadWriteLockVisitor<O> create(final O object, final ReadWriteLock readWriteLock) {
        return new LockingVisitors.ReadWriteLockVisitor<>(object, readWriteLock);
    }

    /**
     * Creates a new instance of {@link ReentrantLockVisitor} with the given object and lock.
     *
     * @param <O> The type of the object to protect.
     * @param object The object to protect.
     * @param reentrantLock The lock to use.
     * @return A new {@link ReentrantLockVisitor}.
     * @see LockingVisitors
     * @since 3.18.0
     */
    public static <O> ReentrantLockVisitor<O> create(final O object, final ReentrantLock reentrantLock) {
        return new LockingVisitors.ReentrantLockVisitor<>(object, reentrantLock);
    }

    /**
     * Creates a new instance of {@link ReentrantLockVisitor} with the given object.
     *
     * @param <O> The type of the object to protect.
     * @param object The object to protect.
     * @return A new {@link ReentrantLockVisitor}.
     * @see LockingVisitors
     * @since 3.18.0
     */
    public static <O> ReentrantLockVisitor<O> reentrantLockVisitor(final O object) {
        return create(object, new ReentrantLock());
    }

    /**
     * Creates a new instance of {@link ReadWriteLockVisitor} with the given object.
     *
     * @param <O> The type of the object to protect.
     * @param object The object to protect.
     * @return A new {@link ReadWriteLockVisitor}.
     * @see LockingVisitors
     */
    public static <O> ReadWriteLockVisitor<O> reentrantReadWriteLockVisitor(final O object) {
        return create(object, new ReentrantReadWriteLock());
    }

    /**
     * Creates a new instance of {@link StampedLockVisitor} with the given object.
     *
     * @param <O> The type of the object to protect.
     * @param object The object to protect.
     * @return A new {@link StampedLockVisitor}.
     * @see LockingVisitors
     */
    public static <O> StampedLockVisitor<O> stampedLockVisitor(final O object) {
        return new LockingVisitors.StampedLockVisitor<>(object, new StampedLock());
    }

    /**
     * Make private in 4.0.
     *
     * @see LockingVisitors
     * @deprecated TODO Make private in 4.0.
     */
    @Deprecated
    public LockingVisitors() {
        // empty
    }
}
