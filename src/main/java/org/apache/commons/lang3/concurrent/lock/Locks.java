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
package org.apache.commons.lang3.concurrent.lock;

import java.util.Objects;
import java.util.concurrent.locks.StampedLock;
import java.util.function.LongSupplier;

import org.apache.commons.lang3.function.Failable;
import org.apache.commons.lang3.function.FailableConsumer;
import org.apache.commons.lang3.function.FailableFunction;

/**
 * Utility class for working with {@link java.util.concurrent.locks.Lock locked objects}. Locked objects are an
 * alternative to synchronization.
 *
 * Locking is preferable, if there is a distinction between read access (multiple threads may have read access
 * concurrently), and write access (only one thread may have write access at any given time. In comparison,
 * synchronization doesn't support read access, because synchronized access is exclusive.
 *
 * Using this class is fairly straightforward:
 * <ol>
 * <li>While still in single thread mode, create an instance of {@link Locks.Lock} by calling {@link #lock(Object)},
 * passing the object, which needs to be locked. Discard all references to the locked object. Instead, use references to
 * the lock.</li>
 * <li>If you want to access the locked object, create a {@link FailableConsumer}. The consumer will receive the locked
 * object as a parameter. For convenience, the consumer may be implemented as a Lambda. Then invoke
 * {@link Locks.Lock#acceptReadLocked(FailableConsumer)}, or {@link Locks.Lock#acceptWriteLocked(FailableConsumer)}, passing
 * the consumer.</li>
 * <li>As an alternative, if you need to produce a result object, you may use a {@link FailableFunction}. This function
 * may also be implemented as a Lambda. To have the function executed, invoke
 * {@link Locks.Lock#applyReadLocked(FailableFunction)}, or {@link Locks.Lock#applyWriteLocked(FailableFunction)}.</li>
 * </ol>
 *
 * Example: A thread safe logger class.
 *
 * <pre>
 *   public class SimpleLogger {
 *
 *     private final Lock&lt;PrintStream&gt; lock;
 *
 *     public SimpleLogger(OutputStream out) {
 *         lock = Locks.lock(new PrintStream(out));
 *     }
 *
 *     public void log(String message) {
 *         lock.acceptWriteLocked((ps) -&gt; ps.println(message));
 *     }
 *
 *     public void log(byte[] buffer) {
 *         lock.acceptWriteLocked((ps) -&gt; { ps.write(buffer); ps.println(); });
 *     }
 * </pre>
 *
 * @since 3.11
 */
public class Locks {

    /**
     * This class implements a wrapper for a locked (hidden) object, and
     * provides the means to access it. The basic idea, is that the user
     * code forsakes all references to the locked object, using only the
     * wrapper object, and the accessor methods
     * {@link #acceptReadLocked(FailableConsumer)},
     * {@link #acceptWriteLocked(FailableConsumer)},
     * {@link #applyReadLocked(FailableFunction)}, and
     * {@link #applyWriteLocked(FailableFunction)}. By doing so, the
     * necessary protections are guaranteed.
     * @param <O> The locked (hidden) objects type.
     *
     * @since 3.11
     */
    public static class Lock<O extends Object> {
        private final StampedLock lock = new StampedLock();
        private final O lockedObject;

        /**
         * Creates a new instance with the given locked object. This
         * constructor is supposed to be used for subclassing only.
         * In general, it is suggested to use {@link Locks#lock(Object)}
         * instead.
         * @param lockedObject The locked (hidden) object. The caller is
         * supposed to drop all references to the locked object.
         */
        public Lock(final O lockedObject) {
            this.lockedObject = Objects.requireNonNull(lockedObject, "Locked Object");
        }

        /**
         * Provides read (shared, non-exclusive) access to the locked (hidden) object.
         * More precisely, what the method will do (in the given order):
         * <ol>
         *   <li>Obtain a read (shared) lock on the locked (hidden) object.
         *     The current thread may block, until such a lock is granted.
         *   </li>
         *   <li>Invokes the given {@link FailableConsumer consumer}, passing the
         *   locked object as the parameter.</li>
         *   <li>Release the lock, as soon as the consumers invocation is done.
         *     If the invocation results in an error, the lock will be released
         *     anyways.
         *   </li>
         * </ol>
         * @param consumer The consumer, which is being invoked to use the
         *   hidden object, which will be passed as the consumers parameter.
         * @see #acceptWriteLocked(FailableConsumer)
         * @see #applyReadLocked(FailableFunction)
         */
        public void acceptReadLocked(final FailableConsumer<O, ?> consumer) {
            lockAcceptUnlock(() -> lock.readLock(), consumer);
        }

        /**
         * Provides write (exclusive) access to the locked (hidden) object.
         * More precisely, what the method will do (in the given order):
         * <ol>
         *   <li>Obtain a write (shared) lock on the locked (hidden) object.
         *     The current thread may block, until such a lock is granted.
         *   </li>
         *   <li>Invokes the given {@link FailableConsumer consumer}, passing the
         *   locked object as the parameter.</li>
         *   <li>Release the lock, as soon as the consumers invocation is done.
         *     If the invocation results in an error, the lock will be released
         *     anyways.
         *   </li>
         * </ol>
         * @param consumer The consumer, which is being invoked to use the
         *   hidden object, which will be passed as the consumers parameter.
         * @see #acceptReadLocked(FailableConsumer)
         * @see #applyWriteLocked(FailableFunction)
         */
        public void acceptWriteLocked(final FailableConsumer<O, ?> consumer) {
            lockAcceptUnlock(() -> lock.writeLock(), consumer);
        }

        /**
         * Provides read (shared, non-exclusive) access to the locked (hidden)
         * object for the purpose of computing a result object.
         * More precisely, what the method will do (in the given order):
         * <ol>
         *   <li>Obtain a read (shared) lock on the locked (hidden) object.
         *     The current thread may block, until such a lock is granted.
         *   </li>
         *   <li>Invokes the given {@link FailableFunction function}, passing the
         *   locked object as the parameter, receiving the functions result.</li>
         *   <li>Release the lock, as soon as the consumers invocation is done.
         *     If the invocation results in an error, the lock will be released
         *     anyways.
         *   </li>
         *   <li>
         *     Return the result object, that has been received from the
         *     functions invocation.</li>
         * </ol>
         *
         * <em>Example:</em> Suggest, that the hidden object is a list, and we
         * wish to know the current size of the list. This might be achieved
         * with the following:
         *
         * <pre>
         *   private Lock&lt;List&lt;Object&gt;&gt; listLock;
         *
         *   public int getCurrentListSize() {
         *       final Integer sizeInteger
         *           = listLock.applyReadLocked((list) -&gt; Integer.valueOf(list.size));
         *       return sizeInteger.intValue();
         *   }
         * </pre>
         * @param <T> The result type (both the functions, and this method's.)
         * @param function The function, which is being invoked to compute the
         *   result. The function will receive the hidden object.
         * @return The result object, which has been returned by the
         *   functions invocation.
         * @throws IllegalStateException The result object would be, in fact,
         *   the hidden object. This would extend access to the hidden object
         *   beyond this methods lifetime and will therefore be prevented.
         * @see #acceptReadLocked(FailableConsumer)
         * @see #applyWriteLocked(FailableFunction)
         */
        public <T> T applyReadLocked(final FailableFunction<O, T, ?> function) {
            return lockApplyUnlock(() -> lock.readLock(), function);
        }

        /**
         * Provides write (exclusive) access to the locked (hidden)
         * object for the purpose of computing a result object.
         * More precisely, what the method will do (in the given order):
         * <ol>
         *   <li>Obtain a read (shared) lock on the locked (hidden) object.
         *     The current thread may block, until such a lock is granted.
         *   </li>
         *   <li>Invokes the given {@link FailableFunction function}, passing the
         *   locked object as the parameter, receiving the functions result.</li>
         *   <li>Release the lock, as soon as the consumers invocation is done.
         *     If the invocation results in an error, the lock will be released
         *     anyways.
         *   </li>
         *   <li>
         *     Return the result object, that has been received from the
         *     functions invocation.</li>
         * </ol>
         * @param <T> The result type (both the functions, and this method's.)
         * @param function The function, which is being invoked to compute the
         *   result. The function will receive the hidden object.
         * @return The result object, which has been returned by the
         *   functions invocation.
         * @throws IllegalStateException The result object would be, in fact,
         *   the hidden object. This would extend access to the hidden object
         *   beyond this methods lifetime and will therefore be prevented.
         * @see #acceptReadLocked(FailableConsumer)
         * @see #applyWriteLocked(FailableFunction)
         */
        public <T> T applyWriteLocked(final FailableFunction<O, T, ?> function) {
            return lockApplyUnlock(() -> lock.writeLock(), function);
        }

        /**
         * This method provides the actual implementation for
         * {@link #acceptReadLocked(FailableConsumer)}, and
         * {@link #acceptWriteLocked(FailableConsumer)}.
         * @param stampSupplier A supplier for the lock. (This provides, in
         *   fact, a long, because a {@link StampedLock} is used internally.)
         * @param consumer The consumer, which is to be given access to the
         *   locked (hidden) object, which will be passed as a parameter.
         * @see #acceptReadLocked(FailableConsumer)
         * @see #acceptWriteLocked(FailableConsumer)
         * @see #lockApplyUnlock(LongSupplier, FailableFunction)
         */
        protected void lockAcceptUnlock(final LongSupplier stampSupplier, final FailableConsumer<O, ?> consumer) {
            final long stamp = stampSupplier.getAsLong();
            try {
                consumer.accept(lockedObject);
            } catch (final Throwable t) {
                throw Failable.rethrow(t);
            } finally {
                lock.unlock(stamp);
            }
        }

        /**
         * This method provides the actual implementation for
         * {@link #applyReadLocked(FailableFunction)}, and
         * {@link #applyWriteLocked(FailableFunction)}.
         * @param <T> The result type (both the functions, and this method's.)
         * @param stampSupplier A supplier for the lock. (This provides, in
         *   fact, a long, because a {@link StampedLock} is used internally.)
         * @param function The function, which is being invoked to compute
         *   the result object. This function will receive the locked (hidden)
         *   object as a parameter.
         * @return The result object, which has been returned by the
         *   functions invocation.
         * @throws IllegalStateException The result object would be, in fact,
         *   the hidden object. This would extend access to the hidden object
         *   beyond this methods lifetime and will therefore be prevented.
         * @see #applyReadLocked(FailableFunction)
         * @see #applyWriteLocked(FailableFunction)
         * @see #lockAcceptUnlock(LongSupplier, FailableConsumer)
         */
        protected <T> T lockApplyUnlock(final LongSupplier stampSupplier, final FailableFunction<O, T, ?> function) {
            final long stamp = stampSupplier.getAsLong();
            try {
                final T t = function.apply(lockedObject);
                if (t == lockedObject) {
                    throw new IllegalStateException("The returned object is, in fact, the hidden object.");
                }
                return t;
            } catch (final Throwable t) {
                throw Failable.rethrow(t);
            } finally {
                lock.unlock(stamp);
            }
        }
    }

    /**
     * Creates a new instance of {@link Lock} with the given locked
     * (hidden) object.
     * @param <O> The locked objects type.
     * @param object The locked (hidden) object.
     * @return The created instance, a {@link Lock lock} for the
     * given object.
     */
    public static <O extends Object> Locks.Lock<O> lock(final O object) {
        return new Locks.Lock<>(object);
    }
}
