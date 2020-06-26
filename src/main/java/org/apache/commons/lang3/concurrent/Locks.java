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

    public static class Lock<O extends Object> {

        private final StampedLock lock = new StampedLock();
        private final O lockedObject;

        public Lock(final O lockedObject) {
            this.lockedObject = Objects.requireNonNull(lockedObject, "Locked Object");
        }

        public void acceptReadLocked(final FailableConsumer<O, ?> consumer) {
            lockAcceptUnlock(() -> lock.readLock(), consumer);
        }

        public void acceptWriteLocked(final FailableConsumer<O, ?> consumer) {
            lockAcceptUnlock(() -> lock.writeLock(), consumer);
        }

        public <T> T applyReadLocked(final FailableFunction<O, T, ?> function) {
            return lockApplyUnlock(() -> lock.readLock(), function);
        }

        public <T> T applyWriteLocked(final FailableFunction<O, T, ?> function) {
            return lockApplyUnlock(() -> lock.writeLock(), function);
        }

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

        protected <T> T lockApplyUnlock(final LongSupplier stampSupplier, final FailableFunction<O, T, ?> function) {
            final long stamp = stampSupplier.getAsLong();
            try {
                return function.apply(lockedObject);
            } catch (final Throwable t) {
                throw Failable.rethrow(t);
            } finally {
                lock.unlock(stamp);
            }
        }
    }

    public static <O extends Object> Locks.Lock<O> lock(final O object) {
        return new Locks.Lock<>(object);
    }
}
