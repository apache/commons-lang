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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

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
     * Return the active thread with the specified id if it belong's to the specified thread group.
     *
     * @param threadId The thread id
     * @param threadGroup The thread group
     * @return The thread which belongs to a specified thread group and the thread's id match the specified id.
     * {@code null} is returned if no such thread exists
     * @throws IllegalArgumentException if the specified id is zero or negative or the group is null
     * @throws  SecurityException
     *          if the current thread cannot access the system thread group
     *
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     */
    public static Thread findThreadById(final long threadId, final ThreadGroup threadGroup) {
        if (threadGroup == null) {
            throw new IllegalArgumentException("The thread group must not be null");
        }
        final Thread thread = findThreadById(threadId);
        if(thread != null && threadGroup.equals(thread.getThreadGroup())) {
            return thread;
        }
        return null;
    }

    /**
     * Return the active thread with the specified id if it belong's to a thread group with the specified group name.
     *
     * @param threadId The thread id
     * @param threadGroupName The thread group name
     * @return The threads which belongs to a thread group with the specified group name and the thread's id match the specified id.
     * {@code null} is returned if no such thread exists
     * @throws IllegalArgumentException if the specified id is zero or negative or the group name is null
     * @throws  SecurityException
     *          if the current thread cannot access the system thread group
     *
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     */
    public static Thread findThreadById(final long threadId, final String threadGroupName) {
        if (threadGroupName == null) {
            throw new IllegalArgumentException("The thread group name must not be null");
        }
        final Thread thread = findThreadById(threadId);
        if(thread != null && thread.getThreadGroup() != null && thread.getThreadGroup().getName().equals(threadGroupName)) {
            return thread;
        }
        return null;
    }

    /**
     * Return active threads with the specified name if they belong to a specified thread group.
     *
     * @param threadName The thread name
     * @param threadGroup The thread group
     * @return The threads which belongs to a thread group and the thread's name match the specified name,
     * An empty collection is returned if no such thread exists. The collection returned is always unmodifiable.
     * @throws IllegalArgumentException if the specified thread name or group is null
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
     * Return active threads with the specified name if they belong to a thread group with the specified group name.
     *
     * @param threadName The thread name
     * @param threadGroupName The thread group name
     * @return The threads which belongs to a thread group with the specified group name and the thread's name match the specified name,
     * An empty collection is returned if no such thread exists. The collection returned is always unmodifiable.
     * @throws IllegalArgumentException if the specified thread name or group name is null
     * @throws  SecurityException
     *          if the current thread cannot access the system thread group
     *
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     */
    public static Collection<Thread> findThreadsByName(final String threadName, final String threadGroupName) {
        if (threadName == null) {
            throw new IllegalArgumentException("The thread name must not be null");
        }
        if (threadGroupName == null) {
            throw new IllegalArgumentException("The thread group name must not be null");
        }

        final Collection<ThreadGroup> threadGroups = findThreadGroups(new NamePredicate(threadGroupName));

        if(threadGroups.isEmpty()) {
            return Collections.emptyList();
        }

        final Collection<Thread> result = new ArrayList<Thread>();
        final NamePredicate threadNamePredicate = new NamePredicate(threadName);
        for(final ThreadGroup group : threadGroups) {
            result.addAll(findThreads(group, false, threadNamePredicate));
        }
        return Collections.unmodifiableCollection(result);
    }

    /**
     * Return active thread groups with the specified group name.
     *
     * @param threadGroupName The thread group name
     * @return the thread groups with the specified group name or an empty collection if no such thread group exists. The collection returned is always unmodifiable.
     * @throws IllegalArgumentException if group name is null
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
     * Return all active thread groups excluding the system thread group (A thread group is active if it has been not destroyed).
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
     * Return the system thread group (sometimes also referred as "root thread group").
     *
     * @return the system thread group
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     */
    public static ThreadGroup getSystemThreadGroup() {
        ThreadGroup threadGroup = Thread.currentThread().getThreadGroup();
        while(threadGroup.getParent() != null) {
            threadGroup = threadGroup.getParent();
        }
        return threadGroup;
    }

    /**
     * Return all active threads (A thread is active if it has been started and has not yet died).
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
     * Return active threads with the specified name.
     *
     * @param threadName The thread name
     * @return The threads with the specified name or an empty collection if no such thread exists. The collection returned is always unmodifiable.
     * @throws IllegalArgumentException if the specified name is null
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
     * Return the active thread with the specified id.
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
     * <p>
     * ThreadUtils instances should NOT be constructed in standard programming. Instead, the class should be used as
     * {@code ThreadUtils.getAllThreads()}
     * </p>
     * <p>
     * This constructor is public to permit tools that require a JavaBean instance to operate.
     * </p>
     */
    public ThreadUtils() {
        super();
    }

    /**
     * A predicate for selecting threads.
     */
    //if java minimal version for lang becomes 1.8 extend this interface from java.util.function.Predicate
    public interface ThreadPredicate /*extends java.util.function.Predicate<Thread>*/{

        /**
         * Evaluates this predicate on the given thread.
         * @param thread the thread
         * @return {@code true} if the thread matches the predicate, otherwise {@code false}
         */
        boolean test(Thread thread);
    }

    /**
     * A predicate for selecting threadgroups.
     */
    //if java minimal version for lang becomes 1.8 extend this interface from java.util.function.Predicate
    public interface ThreadGroupPredicate /*extends java.util.function.Predicate<ThreadGroup>*/{

        /**
         * Evaluates this predicate on the given threadgroup.
         * @param threadGroup the threadgroup
         * @return {@code true} if the threadGroup matches the predicate, otherwise {@code false}
         */
        boolean test(ThreadGroup threadGroup);
    }

    /**
     * Predicate which always returns true.
     */
    public static final AlwaysTruePredicate ALWAYS_TRUE_PREDICATE = new AlwaysTruePredicate();

    /**
     * A predicate implementation which always returns true.
     */
    private final static class AlwaysTruePredicate implements ThreadPredicate, ThreadGroupPredicate{

        private AlwaysTruePredicate() {
        }

        @Override
        public boolean test(final ThreadGroup threadGroup) {
            return true;
        }

        @Override
        public boolean test(final Thread thread) {
            return true;
        }
    }

    /**
     * A predicate implementation which matches a thread or threadgroup name.
     */
    public static class NamePredicate implements ThreadPredicate, ThreadGroupPredicate {

        private final String name;

        /**
         * Predicate constructor
         *
         * @param name thread or threadgroup name
         * @throws IllegalArgumentException if the name is {@code null}
         */
        public NamePredicate(final String name) {
            super();
            if (name == null) {
                throw new IllegalArgumentException("The name must not be null");
            }
            this.name = name;
        }

        @Override
        public boolean test(final ThreadGroup threadGroup) {
            return threadGroup != null && threadGroup.getName().equals(name);
        }

        @Override
        public boolean test(final Thread thread) {
            return thread != null && thread.getName().equals(name);
        }
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
            super();
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
     * Select all active threads which match the given predicate.
     *
     * @param predicate the predicate
     * @return An unmodifiable {@code Collection} of active threads matching the given predicate
     *
     * @throws IllegalArgumentException if the predicate is null
     * @throws  SecurityException
     *          if the current thread cannot access the system thread group
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     */
    public static Collection<Thread> findThreads(final ThreadPredicate predicate){
        return findThreads(getSystemThreadGroup(), true, predicate);
    }

    /**
     * Select all active threadgroups which match the given predicate.
     *
     * @param predicate the predicate
     * @return An unmodifiable {@code Collection} of active threadgroups matching the given predicate
     * @throws IllegalArgumentException if the predicate is null
     * @throws  SecurityException
     *          if the current thread cannot access the system thread group
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     */
    public static Collection<ThreadGroup> findThreadGroups(final ThreadGroupPredicate predicate){
        return findThreadGroups(getSystemThreadGroup(), true, predicate);
    }

    /**
     * Select all active threads which match the given predicate and which belongs to the given thread group (or one of its subgroups).
     *
     * @param group the thread group
     * @param recurse if {@code true} then evaluate the predicate recursively on all threads in all subgroups of the given group
     * @param predicate the predicate
     * @return An unmodifiable {@code Collection} of active threads which match the given predicate and which belongs to the given thread group
     * @throws IllegalArgumentException if the given group or predicate is null
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     */
    public static Collection<Thread> findThreads(final ThreadGroup group, final boolean recurse, final ThreadPredicate predicate) {
        if (group == null) {
            throw new IllegalArgumentException("The group must not be null");
        }
        if (predicate == null) {
            throw new IllegalArgumentException("The predicate must not be null");
        }

        int count = group.activeCount();
        Thread[] threads;
        do {
            threads = new Thread[count + (count / 2) + 1]; //slightly grow the array size
            count = group.enumerate(threads, recurse);
            //return value of enumerate() must be strictly less than the array size according to javadoc
        } while (count >= threads.length);

        final List<Thread> result = new ArrayList<Thread>(count);
        for (int i = 0; i < count; ++i) {
            if (predicate.test(threads[i])) {
                result.add(threads[i]);
            }
        }
        return Collections.unmodifiableCollection(result);
    }

    /**
     * Select all active threadgroups which match the given predicate and which is a subgroup of the given thread group (or one of its subgroups).
     *
     * @param group the thread group
     * @param recurse if {@code true} then evaluate the predicate recursively on all threadgroups in all subgroups of the given group
     * @param predicate the predicate
     * @return An unmodifiable {@code Collection} of active threadgroups which match the given predicate and which is a subgroup of the given thread group
     * @throws IllegalArgumentException if the given group or predicate is null
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     */
    public static Collection<ThreadGroup> findThreadGroups(final ThreadGroup group, final boolean recurse, final ThreadGroupPredicate predicate){
        if (group == null) {
            throw new IllegalArgumentException("The group must not be null");
        }
        if (predicate == null) {
            throw new IllegalArgumentException("The predicate must not be null");
        }

        int count = group.activeGroupCount();
        ThreadGroup[] threadGroups;
        do {
            threadGroups = new ThreadGroup[count + (count / 2) + 1]; //slightly grow the array size
            count = group.enumerate(threadGroups, recurse);
            //return value of enumerate() must be strictly less than the array size according to javadoc
        } while(count >= threadGroups.length);

        final List<ThreadGroup> result = new ArrayList<ThreadGroup>(count);
        for(int i = 0; i < count; ++i) {
            if(predicate.test(threadGroups[i])) {
                result.add(threadGroups[i]);
            }
        }
        return Collections.unmodifiableCollection(result);
    }
}
