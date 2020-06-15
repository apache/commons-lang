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

import java.util.Objects;
import java.util.concurrent.locks.StampedLock;

import org.apache.commons.lang3.Functions.FailableConsumer;
import org.apache.commons.lang3.Functions.FailableFunction;


/** Utility class for working with {@link java.util.concurrent.locks.Lock locked objects}. Locked objects are an
 * alternative to synchronization.
 *
 * Locking is preferable, if there is a distinction between read access (multiple threads may have read
 * access concurrently), and write access (only one thread may have write access at any given time.
 * In comparison, synchronization doesn't support read access, because synchronized access is exclusive.
 *
 * Using this class is fairly straightforward:
 * <ol>
 *   <li>While still in single thread mode, create an instance of {@link Locks.Lock} by calling
 *     {@link #lock(Object)}, passing the object, which needs to be locked. Discard all
 *     references to the locked object. Instead, use references to the lock.</li>
 *   <li>If you want to access the locked object, create a {@link FailableConsumer}. The consumer
 *     will receive the locked object as a parameter. For convenience, the consumer may be
 *     implemented as a Lambda. Then invoke {@link Locks.Lock#runReadLocked(FailableConsumer)},
 *     or {@link Locks.Lock#runWriteLocked(FailableConsumer)}, passing the consumer.</li>
 *   <li>As an alternative, if you need to produce a result object, you may use a
 *     {@link FailableFunction}. This function may also be implemented as a Lambda. To
 *     have the function executed, invoke {@link Locks.Lock#callReadLocked(FailableFunction)}, or
 *     {@link Locks.Lock#callWriteLocked(FailableFunction)}.</li>
 * </ol>
 *
 * Example: A thread safe logger class.
 * <pre>
 *   public class SimpleLogger {
 *     private final Lock&lt;PrintStream&gt; lock;
 *
 *     public SimpleLogger(OutputStream out) {
 *         PrintStream ps = new PrintStream(out);
 *         lock = Locks.lock(ps);
 *     }
 *
 *     public void log(String message) {
 *         lock.runWriteLocked((ps) -&gt; ps.println(message));
 *     }
 *
 *     public void log(byte[] buffer) {
 *         lock.runWriteLocked((ps) -&gt; { ps.write(buffer); ps.println(); });
 *     }
 * </pre>
 */
public class Locks {
    public static class Lock<O extends Object> {
        private final O lockedObject;
        private final StampedLock lock = new StampedLock();

        public Lock(O lockedObject) {
            this.lockedObject = Objects.requireNonNull(lockedObject, "Locked Object");
        }

        public void runReadLocked(FailableConsumer<O, ?> consumer) {
            runLocked(lock.readLock(), consumer);
        }

        public void runWriteLocked(FailableConsumer<O, ?> consumer) {
            runLocked(lock.writeLock(), consumer);
        }

        public <T> T callReadLocked(FailableFunction<O, T, ?> function) {
            return callLocked(lock.readLock(), function);
        }

        public <T> T callWriteLocked(FailableFunction<O, T, ?> function) {
            return callLocked(lock.writeLock(), function);
        }

        protected void runLocked(long stamp, FailableConsumer<O, ?> consumer) {
            try {
                consumer.accept(lockedObject);
            } catch (Throwable t) {
                throw Functions.rethrow(t);
            } finally {
                lock.unlock(stamp);
            }
        }

        protected <T> T callLocked(long stamp, FailableFunction<O, T, ?> function) {
            try {
                return function.apply(lockedObject);
            } catch (Throwable t) {
                throw Functions.rethrow(t);
            } finally {
                lock.unlock(stamp);
            }
        }
    }

    public static <O extends Object> Locks.Lock<O> lock(O object) {
        return new Locks.Lock<O>(object);
    }
}
