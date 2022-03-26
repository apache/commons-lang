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

import java.time.Duration;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang3.time.DurationUtils;

/**
 * <p>
 * Helpers for {@code java.lang.Thread} and {@code java.lang.ThreadGroup}.
 * </p>
 * <p>
 * #ThreadSafe#
 * </p>
 *
 * @see java.lang.Thread
 * @see java.lang.ThreadGroup
 * @since 3.5
 */
public class ThreadUtils {

    /**
     * A predicate implementation which always returns true.
     */
    private static final class AlwaysTruePredicate implements ThreadPredicate, ThreadGroupPredicate {

        private AlwaysTruePredicate() {
        }

        @Override
        public boolean test(final Thread thread) {
            return true;
        }

        @Override
        public boolean test(final ThreadGroup threadGroup) {
            return true;
        }
    }

    /**
     * A predicate implementation which matches a thread or thread group name.
     */
    public static class NamePredicate implements ThreadPredicate, ThreadGroupPredicate {

        private final String name;

        /**
         * Predicate constructor
         *
         * @param name thread or thread group name
         * @throws NullPointerException if the name is {@code null}
         */
        public NamePredicate(final String name) {
            Validate.notNull(name, "name");
            this.name = name;
        }

        @Override
        public boolean test(final Thread thread) {
            return thread != null && thread.getName().equals(name);
        }

        @Override
        public boolean test(final ThreadGroup threadGroup) {
            return threadGroup != null && threadGroup.getName().equals(name);
        }
    }

    /**
     * A predicate for selecting thread groups.
     */
    // When breaking BC, replace this with Predicate<ThreadGroup>
    @FunctionalInterface
    public interface ThreadGroupPredicate {

        /**
         * Evaluates this predicate on the given thread group.
         * @param threadGroup the thread group
         * @return {@code true} if the threadGroup matches the predicate, otherwise {@code false}
         */
        boolean test(ThreadGroup threadGroup);
    }

    /**
     * A predicate implementation which matches a thread id.
     */
    public static class ThreadIdPredicate implements ThreadPredicate {

        private final long threadId;

        /**
         * Predicate constructor
         *
         * @param threadId the threadId to match
         * @throws IllegalArgumentException if the threadId is zero or negative
         */
        public ThreadIdPredicate(final long threadId) {
            if (threadId <= 0) {
                throw new IllegalArgumentException("The thread id must be greater than zero");
            }
            this.threadId = threadId;
        }

        @Override
        public boolean test(final Thread thread) {
            return thread != null && thread.getId() == threadId;
        }
    }

    /**
     * A predicate for selecting threads.
     */
    // When breaking BC, replace this with Predicate<Thread>
    @FunctionalInterface
    public interface ThreadPredicate {

        /**
         * Evaluates this predicate on the given thread.
         * @param thread the thread
         * @return {@code true} if the thread matches the predicate, otherwise {@code false}
         */
        boolean test(Thread thread);
    }

    /**
     * Predicate which always returns true.
     */
    public static final AlwaysTruePredicate ALWAYS_TRUE_PREDICATE = new AlwaysTruePredicate();

    /**
     * Finds the active thread with the specified id.
     *
     * @param threadId The thread id
     * @return The thread with the specified id or {@code null} if no such thread exists
     * @throws IllegalArgumentException if the specified id is zero or negative
     * @throws  SecurityException
     *          if the current thread cannot access the system thread group
     *
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     */
    public static Thread findThreadById(final long threadId) {
        final Collection<Thread> result = findThreads(new ThreadIdPredicate(threadId));
        return result.isEmpty() ? null : result.iterator().next();
    }

    /**
     * Finds the active thread with the specified id if it belongs to a thread group with the specified group name.
     *
     * @param threadId The thread id
     * @param threadGroupName The thread group name
     * @return The threads which belongs to a thread group with the specified group name and the thread's id match the specified id.
     * {@code null} is returned if no such thread exists
     * @throws NullPointerException if the group name is null
     * @throws IllegalArgumentException if the specified id is zero or negative
     * @throws  SecurityException
     *          if the current thread cannot access the system thread group
     *
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     */
    public static Thread findThreadById(final long threadId, final String threadGroupName) {
        Validate.notNull(threadGroupName, "threadGroupName");
        final Thread thread = findThreadById(threadId);
        if (thread != null && thread.getThreadGroup() != null && thread.getThreadGroup().getName().equals(threadGroupName)) {
            return thread;
        }
        return null;
    }

    /**
     * Finds the active thread with the specified id if it belongs to the specified thread group.
     *
     * @param threadId The thread id
     * @param threadGroup The thread group
     * @return The thread which belongs to a specified thread group and the thread's id match the specified id.
     * {@code null} is returned if no such thread exists
     * @throws NullPointerException if {@code threadGroup == null}
     * @throws IllegalArgumentException if the specified id is zero or negative
     * @throws  SecurityException
     *          if the current thread cannot access the system thread group
     *
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     */
    public static Thread findThreadById(final long threadId, final ThreadGroup threadGroup) {
        Validate.notNull(threadGroup, "threadGroup");
        final Thread thread = findThreadById(threadId);
        if (thread != null && threadGroup.equals(thread.getThreadGroup())) {
            return thread;
        }
        return null;
    }

    /**
     * Select all active thread groups which match the given predicate and which is a subgroup of the given thread group (or one of its subgroups).
     *
     * @param threadGroup the thread group
     * @param recurse if {@code true} then evaluate the predicate recursively on all thread groups in all subgroups of the given group
     * @param predicate the predicate
     * @return An unmodifiable {@code Collection} of active thread groups which match the given predicate and which is a subgroup of the given thread group
     * @throws NullPointerException if the given group or predicate is null
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     */
    public static Collection<ThreadGroup> findThreadGroups(final ThreadGroup threadGroup, final boolean recurse, final ThreadGroupPredicate predicate) {
        Validate.notNull(threadGroup, "group");
        Validate.notNull(predicate, "predicate");

        int count = threadGroup.activeGroupCount();
        ThreadGroup[] threadGroups;
        do {
            threadGroups = new ThreadGroup[count + (count / 2) + 1]; //slightly grow the array size
            count = threadGroup.enumerate(threadGroups, recurse);
            //return value of enumerate() must be strictly less than the array size according to javadoc
        } while (count >= threadGroups.length);

        final List<ThreadGroup> result = new ArrayList<>(count);
        for (int i = 0; i < count; ++i) {
            if (predicate.test(threadGroups[i])) {
                result.add(threadGroups[i]);
            }
        }
        return Collections.unmodifiableCollection(result);
    }

    /**
     * Select all active thread groups which match the given predicate.
     *
     * @param predicate the predicate
     * @return An unmodifiable {@code Collection} of active thread groups matching the given predicate
     * @throws NullPointerException if the predicate is null
     * @throws  SecurityException
     *          if the current thread cannot access the system thread group
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     */
    public static Collection<ThreadGroup> findThreadGroups(final ThreadGroupPredicate predicate) {
        return findThreadGroups(getSystemThreadGroup(), true, predicate);
    }

    /**
     * Finds active thread groups with the specified group name.
     *
     * @param threadGroupName The thread group name
     * @return the thread groups with the specified group name or an empty collection if no such thread group exists. The collection returned is always unmodifiable.
     * @throws NullPointerException if group name is null
     * @throws  SecurityException
     *          if the current thread cannot access the system thread group
     *
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     */
    public static Collection<ThreadGroup> findThreadGroupsByName(final String threadGroupName) {
        return findThreadGroups(new NamePredicate(threadGroupName));
    }

    /**
     * Select all active threads which match the given predicate and which belongs to the given thread group (or one of its subgroups).
     *
     * @param threadGroup the thread group
     * @param recurse if {@code true} then evaluate the predicate recursively on all threads in all subgroups of the given group
     * @param predicate the predicate
     * @return An unmodifiable {@code Collection} of active threads which match the given predicate and which belongs to the given thread group
     * @throws NullPointerException if the given group or predicate is null
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     */
    public static Collection<Thread> findThreads(final ThreadGroup threadGroup, final boolean recurse, final ThreadPredicate predicate) {
        Validate.notNull(threadGroup, "The group must not be null");
        Validate.notNull(predicate, "The predicate must not be null");

        int count = threadGroup.activeCount();
        Thread[] threads;
        do {
            threads = new Thread[count + (count / 2) + 1]; //slightly grow the array size
            count = threadGroup.enumerate(threads, recurse);
            //return value of enumerate() must be strictly less than the array size according to javadoc
        } while (count >= threads.length);

        final List<Thread> result = new ArrayList<>(count);
        for (int i = 0; i < count; ++i) {
            if (predicate.test(threads[i])) {
                result.add(threads[i]);
            }
        }
        return Collections.unmodifiableCollection(result);
    }

    /**
     * Select all active threads which match the given predicate.
     *
     * @param predicate the predicate
     * @return An unmodifiable {@code Collection} of active threads matching the given predicate
     *
     * @throws NullPointerException if the predicate is null
     * @throws  SecurityException
     *          if the current thread cannot access the system thread group
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     */
    public static Collection<Thread> findThreads(final ThreadPredicate predicate) {
        return findThreads(getSystemThreadGroup(), true, predicate);
    }

    /**
     * Finds active threads with the specified name.
     *
     * @param threadName The thread name
     * @return The threads with the specified name or an empty collection if no such thread exists. The collection returned is always unmodifiable.
     * @throws NullPointerException if the specified name is null
     * @throws  SecurityException
     *          if the current thread cannot access the system thread group
     *
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     */
    public static Collection<Thread> findThreadsByName(final String threadName) {
        return findThreads(new NamePredicate(threadName));
    }

    /**
     * Finds active threads with the specified name if they belong to a thread group with the specified group name.
     *
     * @param threadName The thread name
     * @param threadGroupName The thread group name
     * @return The threads which belongs to a thread group with the specified group name and the thread's name match the specified name,
     * An empty collection is returned if no such thread exists. The collection returned is always unmodifiable.
     * @throws NullPointerException if the specified thread name or group name is null
     * @throws  SecurityException
     *          if the current thread cannot access the system thread group
     *
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     */
    public static Collection<Thread> findThreadsByName(final String threadName, final String threadGroupName) {
        Validate.notNull(threadName, "threadName");
        Validate.notNull(threadGroupName, "threadGroupName");

        final Collection<ThreadGroup> threadGroups = findThreadGroups(new NamePredicate(threadGroupName));

        if (threadGroups.isEmpty()) {
            return Collections.emptyList();
        }

        final Collection<Thread> result = new ArrayList<>();
        final NamePredicate threadNamePredicate = new NamePredicate(threadName);
        for (final ThreadGroup group : threadGroups) {
            result.addAll(findThreads(group, false, threadNamePredicate));
        }
        return Collections.unmodifiableCollection(result);
    }

    /**
     * Finds active threads with the specified name if they belong to a specified thread group.
     *
     * @param threadName The thread name
     * @param threadGroup The thread group
     * @return The threads which belongs to a thread group and the thread's name match the specified name,
     * An empty collection is returned if no such thread exists. The collection returned is always unmodifiable.
     * @throws NullPointerException if the specified thread name or group is null
     * @throws  SecurityException
     *          if the current thread cannot access the system thread group
     *
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     */
    public static Collection<Thread> findThreadsByName(final String threadName, final ThreadGroup threadGroup) {
        return findThreads(threadGroup, false, new NamePredicate(threadName));
    }

    /**
     * Gets all active thread groups excluding the system thread group (A thread group is active if it has been not destroyed).
     *
     * @return all thread groups excluding the system thread group. The collection returned is always unmodifiable.
     * @throws  SecurityException
     *          if the current thread cannot access the system thread group
     *
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     */
    public static Collection<ThreadGroup> getAllThreadGroups() {
        return findThreadGroups(ALWAYS_TRUE_PREDICATE);
    }

    /**
     * Gets all active threads (A thread is active if it has been started and has not yet died).
     *
     * @return all active threads. The collection returned is always unmodifiable.
     * @throws  SecurityException
     *          if the current thread cannot access the system thread group
     *
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     */
    public static Collection<Thread> getAllThreads() {
        return findThreads(ALWAYS_TRUE_PREDICATE);
    }

    /**
     * Gets the system thread group (sometimes also referred as "root thread group").
     * <p>
     * This method returns null if this thread has died (been stopped).
     * </p>
     *
     * @return the system thread group
     * @throws SecurityException if the current thread cannot modify thread groups from this thread's thread group up to the
     *         system thread group
     */
    public static ThreadGroup getSystemThreadGroup() {
        ThreadGroup threadGroup = Thread.currentThread().getThreadGroup();
        while (threadGroup != null && threadGroup.getParent() != null) {
            threadGroup = threadGroup.getParent();
        }
        return threadGroup;
    }

    /**
     * Waits for the given thread to die for the given duration. Implemented using {@link Thread#join(long, int)}.
     *
     * @param thread The thread to join.
     * @param duration How long to wait.
     * @throws InterruptedException if any thread has interrupted the current thread.
     * @see Thread#join(long, int)
     * @since 3.12.0
     */
    public static void join(final Thread thread, final Duration duration) throws InterruptedException {
        DurationUtils.accept(thread::join, duration);
    }

    /**
     * Sleeps the current thread for the given duration. Implemented using {@link Thread#sleep(long, int)}.
     *
     * @param duration How long to sleep.
     * @throws InterruptedException if any thread has interrupted the current thread.
     * @see Thread#sleep(long, int)
     * @since 3.12.0
     */
    public static void sleep(final Duration duration) throws InterruptedException {
        DurationUtils.accept(Thread::sleep, duration);
    }

    /**
     * Sleeps for the given amount of milliseconds while ignoring {@link InterruptedException}.
     * <p>
     * The sleep duration may be shorter than {@code millis} if we catch a {@link InterruptedException}.
     * </p>
     *
     * @param millis the length of time to sleep in milliseconds
     * @since 3.13.0
     */
    public static void sleepQuietly(final long millis) {
        try {
            sleep(Duration.ofMillis(millis));
        } catch (final InterruptedException e) {
            // be quiet.
        }
    }

    /**
     * <p>
     * ThreadUtils instances should NOT be constructed in standard programming. Instead, the class should be used as
     * {@code ThreadUtils.getAllThreads()}
     * </p>
     * <p>
     * This constructor is public to permit tools that require a JavaBean instance to operate.
     * </p>
     */
    public ThreadUtils() {
    }
}
