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
import java.util.Arrays;
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
 * @since 3.4
 * @version $Id$
 */
public class ThreadUtils {

    private static final Thread[] EMPTY_THREAD_ARRAY = new Thread[0];


    /**
     * Return the active thread with the specified id if it belong's to the specified thread group
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
            throw new IllegalArgumentException("The threadGroup must not be null");
        }

        final Thread thread = findThreadById(threadId);

        if(thread != null && threadGroup.equals(thread.getThreadGroup())) {
            return thread;
        }
        return null;
    }

    /**
     * Return the active thread with the specified id if it belong's to a thread group with the specified group name
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
            throw new IllegalArgumentException("The threadGroupName must not be null");
        }

        final Thread thread = findThreadById(threadId);

        if(thread != null && thread.getThreadGroup() != null && thread.getThreadGroup().getName().equals(threadGroupName)) {
            return thread;
        }
        return null;
    }

    /**
     * Return active threads with the specified name if they belong to a specified thread group
     *
     * @param threadName The thread name
     * @param threadGroupName The thread group
     * @return The threads which belongs to a thread group and the thread's name match the specified name,
     * An empty array is returned if no such thread exists
     * @throws IllegalArgumentException if the specified thread name or group is null
     * @throws  SecurityException
     *          if the current thread cannot access the system thread group
     *
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     */
    public static Thread[] findThreadsByName(final String threadName, final ThreadGroup threadGroup) {
        if (threadName == null) {
            throw new IllegalArgumentException("The threadName must not be null");
        }
        if (threadGroup == null) {
            throw new IllegalArgumentException("The threadGroupName must not be null");
        }

        final Thread[] threads = findThreadsByName(threadName);

        if(threads.length == 0) {
            return EMPTY_THREAD_ARRAY;
        }

        final List<Thread> matchedThreads = new ArrayList<Thread>();

        for (int i = 0; i < threads.length; i++) {
            final Thread thread = threads[i];

            if(thread != null && threadGroup.equals(thread.getThreadGroup())) {
                matchedThreads.add(thread);
            }

        }
        return matchedThreads.toArray(new Thread[matchedThreads.size()]);
    }

    /**
     * Return active threads with the specified name if they belong to a thread group with the specified group name
     *
     * @param threadName The thread name
     * @param threadGroupName The thread group name
     * @return The threads which belongs to a thread group with the specified group name and the thread's name match the specified name,
     * An empty array is returned if no such thread exists
     * @throws IllegalArgumentException if the specified thread name or group name is null
     * @throws  SecurityException
     *          if the current thread cannot access the system thread group
     *
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     */
    public static Thread[] findThreadsByName(final String threadName, final String threadGroupName) {
        if (threadName == null) {
            throw new IllegalArgumentException("The threadName must not be null");
        }
        if (threadGroupName == null) {
            throw new IllegalArgumentException("The threadGroupName must not be null");
        }
        final ThreadGroup[] threadGroups = findThreadGroupsByName(threadGroupName);

        if(threadGroups.length == 0) {
            return EMPTY_THREAD_ARRAY;
        }

        final Thread[] threads = findThreadsByName(threadName);

        if(threads.length == 0) {
            return EMPTY_THREAD_ARRAY;
        }

        final List<Thread> matchedThreads = new ArrayList<Thread>();

        for (int i = 0; i < threads.length; i++) {
            final Thread thread = threads[i];

            if(thread != null && ArrayUtils.contains(threadGroups, thread.getThreadGroup())) {
                matchedThreads.add(thread);
            }

        }
        return matchedThreads.toArray(new Thread[matchedThreads.size()]);
    }

    /**
     * Return active thread groups with the specified group name
     *
     * @param threadGroupName The thread group name
     * @return the thread groups with the specified group name or an empty array if no such thread group exists
     * @throws IllegalArgumentException if group name is null
     * @throws  SecurityException
     *          if the current thread cannot access the system thread group
     *
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     */
    public static ThreadGroup[] findThreadGroupsByName(final String threadGroupName) {
        if (threadGroupName == null) {
            throw new IllegalArgumentException("The threadGroupName must not be null");
        }
        final ThreadGroup[] allThreadGroups = getAllThreadGroups();
        final List<ThreadGroup> threadGroups = new ArrayList<ThreadGroup>();
        for (int i = 0; i < allThreadGroups.length; i++) {
            final ThreadGroup threadGroup = allThreadGroups[i];
            if (threadGroup != null && threadGroupName.equals(threadGroup.getName())) {
                threadGroups.add(threadGroup);
            }
        }
        return threadGroups.toArray(new ThreadGroup[threadGroups.size()]);
    }

    /**
     * Return all active thread groups including the system thread group (A thread group is active if it has been not destroyed)
     *
     * @return all thread groups including the system thread group
     * @throws  SecurityException
     *          if the current thread cannot access the system thread group
     *
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     */
    public static ThreadGroup[] getAllThreadGroups() {
        final ThreadGroup systemThreadGroup = getSystemThreadGroup();
        int estimatedThreadGroupCount = systemThreadGroup.activeGroupCount();
        int threadGroupCount = 0;
        ThreadGroup[] threadGroups;
        do {
            estimatedThreadGroupCount *= 2;
            threadGroups = new ThreadGroup[estimatedThreadGroupCount];
            threadGroupCount = systemThreadGroup.enumerate(threadGroups);
        } while (threadGroupCount == estimatedThreadGroupCount);

        final ThreadGroup[] allGroups = new ThreadGroup[threadGroupCount+1];
        allGroups[0] = systemThreadGroup;
        System.arraycopy(threadGroups, 0, allGroups, 1, threadGroupCount);
        return allGroups;
    }

    /**
     * Return the system thread group (sometimes also referred as "root thread group")
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
     * Return all active threads (A thread is active if it has been started and has not yet died)
     *
     * @return all active threads
     * @throws  SecurityException
     *          if the current thread cannot access the system thread group
     *
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     */
    public static Thread[] getAllThreads() {
        final ThreadGroup systemThreadGroup = getSystemThreadGroup();
        int estimatedThreadCount = systemThreadGroup.activeCount();
        int threadCount = 0;
        Thread[] threads;
        do {
            estimatedThreadCount *= 2;
            threads = new Thread[estimatedThreadCount];
            threadCount = systemThreadGroup.enumerate(threads);
        } while (threadCount == estimatedThreadCount);

        return Arrays.copyOf(threads, threadCount);
    }

    /**
     * Return actve threads with the specified name
     *
     * @param threadName The thread name
     * @return The threads with the specified name or an empty array if no such thread exists
     * @throws IllegalArgumentException if the specified name is null
     * @throws  SecurityException
     *          if the current thread cannot access the system thread group
     *
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     */
    public static Thread[] findThreadsByName(final String threadName) {
        if (threadName == null) {
            throw new IllegalArgumentException("The threadName must not be null");
        }
        final Thread[] allThreads = getAllThreads();
        final List<Thread> threads = new ArrayList<Thread>();
        for (int i = 0; i < allThreads.length; i++) {
            final Thread thread = allThreads[i];
            if (thread != null && threadName.equals(thread.getName())) {
                threads.add(thread);
            }
        }
        return threads.toArray(new Thread[threads.size()]);
    }

    /**
     * Return the active thread with the specified id
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
            throw new IllegalArgumentException("The threadId must be greater than zero");
        }
        final Thread[] allThreads = getAllThreads();
        for (int i = 0; i < allThreads.length; i++) {
            final Thread thread = allThreads[i];
            if (thread != null && threadId == thread.getId()) {
                return thread;
            }
        }
        return null;
    }

    /**
     * <p>
     * ThreadUtils instances should NOT be constructed in standard programming. Instead, the class should be used as
     * {@code ThreadUtils.getAllThreads()}
     * </p>
     * <p>
     * This constructor is public to permit tools that require a JavaBean instance to operate
     * </p>
     */
    public ThreadUtils() {
        super();
    }

}
