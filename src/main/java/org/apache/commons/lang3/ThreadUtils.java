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
import java.util.Collection;
import java.util.Collections;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.time.DurationUtils;

/**
 * Helpers for {@code java.lang.Thread} and {@code java.lang.ThreadGroup}.
 *
 * <p>
 * #ThreadSafe#
 * </p>
 *
 * @see Thread
 * @see ThreadGroup
 * @since 3.5
 */
public class ThreadUtils {

    /**
     * A predicate implementation which always returns true.
     *
     * @deprecated Use a {@link Predicate}.
     */
    @Deprecated
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
     * Used internally, consider private.
     * <p>
     * A predicate implementation which matches a thread or thread group name.
     * </p>
     *
     * @deprecated Use a {@link Predicate}.
     */
    @Deprecated
    public static class NamePredicate implements ThreadPredicate, ThreadGroupPredicate {

        private final String name;

        /**
         * Constructs an instance.
         *
         * @param name thread or thread group name
         * @throws NullPointerException if the name is {@code null}
         */
        public NamePredicate(final String name) {
            Objects.requireNonNull(name, "name");
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
     *
     * @deprecated Use a {@link Predicate}.
     */
    @Deprecated
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
     *
     * @deprecated Use a {@link Predicate}.
     */
    @Deprecated
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
     *
     * @deprecated Use a {@link Predicate}.
     */
    @Deprecated
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
     *
     * @deprecated Use a {@link Predicate}.
     */
    @Deprecated
    public static final AlwaysTruePredicate ALWAYS_TRUE_PREDICATE = new AlwaysTruePredicate();

    private static final Predicate<?> ALWAYS_TRUE = t -> true;

    @SuppressWarnings("unchecked")
    private static <T> Predicate<T> alwaysTruePredicate() {
        return (Predicate<T>) ALWAYS_TRUE;
    }

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
        if (threadId <= 0) {
            throw new IllegalArgumentException("The thread id must be greater than zero");
        }
        final Collection<Thread> result = findThreads((Predicate<Thread>) t -> t != null && t.getId() == threadId);
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
        Objects.requireNonNull(threadGroupName, "threadGroupName");
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
        Objects.requireNonNull(threadGroup, "threadGroup");
        final Thread thread = findThreadById(threadId);
        if (thread != null && threadGroup.equals(thread.getThreadGroup())) {
            return thread;
        }
        return null;
    }

    /**
     * Finds all active thread groups which match the given predicate.
     *
     * @param predicate the predicate
     * @return An unmodifiable {@link Collection} of active thread groups matching the given predicate
     * @throws NullPointerException if the predicate is null
     * @throws  SecurityException
     *          if the current thread cannot access the system thread group
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     * @since 3.13.0
     */
    public static Collection<ThreadGroup> findThreadGroups(final Predicate<ThreadGroup> predicate) {
        return findThreadGroups(getSystemThreadGroup(), true, predicate);
    }

    /**
     * Finds all active thread groups which match the given predicate and which is a subgroup of the given thread group (or one of its subgroups).
     *
     * @param threadGroup the thread group
     * @param recurse if {@code true} then evaluate the predicate recursively on all thread groups in all subgroups of the given group
     * @param predicate the predicate
     * @return An unmodifiable {@link Collection} of active thread groups which match the given predicate and which is a subgroup of the given thread group
     * @throws NullPointerException if the given group or predicate is null
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     * @since 3.13.0
     */
    public static Collection<ThreadGroup> findThreadGroups(final ThreadGroup threadGroup, final boolean recurse, final Predicate<ThreadGroup> predicate) {
        Objects.requireNonNull(threadGroup, "threadGroup");
        Objects.requireNonNull(predicate, "predicate");

        int count = threadGroup.activeGroupCount();
        ThreadGroup[] threadGroups;
        do {
            threadGroups = new ThreadGroup[count + count / 2 + 1]; //slightly grow the array size
            count = threadGroup.enumerate(threadGroups, recurse);
            //return value of enumerate() must be strictly less than the array size according to Javadoc
        } while (count >= threadGroups.length);
        return Collections.unmodifiableCollection(Stream.of(threadGroups).filter(predicate).collect(Collectors.toList()));
    }

    /**
     * Finds all active thread groups which match the given predicate and which is a subgroup of the given thread group (or one of its subgroups).
     *
     * @param threadGroup the thread group
     * @param recurse if {@code true} then evaluate the predicate recursively on all thread groups in all subgroups of the given group
     * @param predicate the predicate
     * @return An unmodifiable {@link Collection} of active thread groups which match the given predicate and which is a subgroup of the given thread group
     * @throws NullPointerException if the given group or predicate is null
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     * @deprecated Use {@link #findThreadGroups(ThreadGroup, boolean, Predicate)}.
     */
    @Deprecated
    public static Collection<ThreadGroup> findThreadGroups(final ThreadGroup threadGroup, final boolean recurse, final ThreadGroupPredicate predicate) {
        return findThreadGroups(threadGroup, recurse, (Predicate<ThreadGroup>) predicate::test);
    }

    /**
     * Finds all active thread groups which match the given predicate.
     *
     * @param predicate the predicate
     * @return An unmodifiable {@link Collection} of active thread groups matching the given predicate
     * @throws NullPointerException if the predicate is null
     * @throws  SecurityException
     *          if the current thread cannot access the system thread group
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     * @deprecated Use {@link #findThreadGroups(Predicate)}.
     */
    @Deprecated
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
        return findThreadGroups(predicateThreadGroup(threadGroupName));
    }

    /**
     * Finds all active threads which match the given predicate.
     *
     * @param predicate the predicate
     * @return An unmodifiable {@link Collection} of active threads matching the given predicate
     *
     * @throws NullPointerException if the predicate is null
     * @throws  SecurityException
     *          if the current thread cannot access the system thread group
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     * @since 3.13.0
     */
    public static Collection<Thread> findThreads(final Predicate<Thread> predicate) {
        return findThreads(getSystemThreadGroup(), true, predicate);
    }

    /**
     * Finds all active threads which match the given predicate and which belongs to the given thread group (or one of its subgroups).
     *
     * @param threadGroup the thread group
     * @param recurse if {@code true} then evaluate the predicate recursively on all threads in all subgroups of the given group
     * @param predicate the predicate
     * @return An unmodifiable {@link Collection} of active threads which match the given predicate and which belongs to the given thread group
     * @throws NullPointerException if the given group or predicate is null
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     * @since 3.13.0
     */
    public static Collection<Thread> findThreads(final ThreadGroup threadGroup, final boolean recurse, final Predicate<Thread> predicate) {
        Objects.requireNonNull(threadGroup, "The group must not be null");
        Objects.requireNonNull(predicate, "The predicate must not be null");
        int count = threadGroup.activeCount();
        Thread[] threads;
        do {
            threads = new Thread[count + count / 2 + 1]; //slightly grow the array size
            count = threadGroup.enumerate(threads, recurse);
            //return value of enumerate() must be strictly less than the array size according to javadoc
        } while (count >= threads.length);
        return Collections.unmodifiableCollection(Stream.of(threads).filter(predicate).collect(Collectors.toList()));
    }

    /**
     * Finds all active threads which match the given predicate and which belongs to the given thread group (or one of its subgroups).
     *
     * @param threadGroup the thread group
     * @param recurse if {@code true} then evaluate the predicate recursively on all threads in all subgroups of the given group
     * @param predicate the predicate
     * @return An unmodifiable {@link Collection} of active threads which match the given predicate and which belongs to the given thread group
     * @throws NullPointerException if the given group or predicate is null
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     * @deprecated Use {@link #findThreads(ThreadGroup, boolean, Predicate)}.
     */
    @Deprecated
    public static Collection<Thread> findThreads(final ThreadGroup threadGroup, final boolean recurse, final ThreadPredicate predicate) {
        return findThreads(threadGroup, recurse, (Predicate<Thread>) predicate::test);
    }

    /**
     * Finds all active threads which match the given predicate.
     *
     * @param predicate the predicate
     * @return An unmodifiable {@link Collection} of active threads matching the given predicate
     *
     * @throws NullPointerException if the predicate is null
     * @throws  SecurityException
     *          if the current thread cannot access the system thread group
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     * @deprecated Use {@link #findThreads(Predicate)}.
     */
    @Deprecated
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
        return findThreads(predicateThread(threadName));
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
        Objects.requireNonNull(threadName, "threadName");
        Objects.requireNonNull(threadGroupName, "threadGroupName");
        return Collections.unmodifiableCollection(findThreadGroups(predicateThreadGroup(threadGroupName)).stream()
            .flatMap(group -> findThreads(group, false, predicateThread(threadName)).stream()).collect(Collectors.toList()));
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
        return findThreads(threadGroup, false, predicateThread(threadName));
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
        return findThreadGroups(alwaysTruePredicate());
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
        return findThreads(alwaysTruePredicate());
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

    private static <T> Predicate<T> namePredicate(final String name, final Function<T, String> nameGetter) {
        return (Predicate<T>) t -> t != null && Objects.equals(nameGetter.apply(t), Objects.requireNonNull(name));
    }

    private static Predicate<Thread> predicateThread(final String threadName) {
        return namePredicate(threadName, Thread::getName);
    }

    private static Predicate<ThreadGroup> predicateThreadGroup(final String threadGroupName) {
        return namePredicate(threadGroupName, ThreadGroup::getName);
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
     * Sleeps for the given duration while ignoring {@link InterruptedException}.
     * <p>
     * The sleep duration may be shorter than duration if we catch a {@link InterruptedException}.
     * </p>
     *
     * @param duration the length of time to sleep.
     * @since 3.13.0
     */
    public static void sleepQuietly(final Duration duration) {
        try {
            sleep(duration);
        } catch (final InterruptedException e) {
            // be quiet.
        }
    }

    /**
     * ThreadUtils instances should NOT be constructed in standard programming. Instead, the class should be used as
     * {@code ThreadUtils.getAllThreads()}
     *
     * <p>
     * This constructor is public to permit tools that require a JavaBean instance to operate.
     * </p>
     */
    public ThreadUtils() {
    }
}
