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
 * @since 3.4
 * @version $Id$
 */
public class ThreadUtils {

    private static final int ENUMERATION_GUESS_EXTRA = 3;

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
     * An empty collection is returned if no such thread exists. The collection returned is always unmodifiable.
     * @throws IllegalArgumentException if the specified thread name or group is null
     * @throws  SecurityException
     *          if the current thread cannot access the system thread group
     *
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     */
    public static Collection<Thread> findThreadsByName(final String threadName, final ThreadGroup threadGroup) {
        if (threadName == null) {
            throw new IllegalArgumentException("The threadName must not be null");
        }
        if (threadGroup == null) {
            throw new IllegalArgumentException("The threadGroupName must not be null");
        }
        final NameNameVisitor nameNameVisitor = new NameNameVisitor(threadName, threadGroup);
        new ThreadUtils.ThreadGroupHolder(ThreadUtils.getSystemThreadGroup()).accept(nameNameVisitor);
        return Collections.unmodifiableCollection(nameNameVisitor.getResult());
    }

    /**
     * Return active threads with the specified name if they belong to a thread group with the specified group name
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
            throw new IllegalArgumentException("The threadName must not be null");
        }
        if (threadGroupName == null) {
            throw new IllegalArgumentException("The threadGroupName must not be null");
        }
        final NameNameVisitor nameNameVisitor = new NameNameVisitor(threadName, threadGroupName);
        new ThreadUtils.ThreadGroupHolder(ThreadUtils.getSystemThreadGroup()).accept(nameNameVisitor);
        return Collections.unmodifiableCollection(nameNameVisitor.getResult());
    }

    /**
     * Return active thread groups with the specified group name
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
        if (threadGroupName == null) {
            throw new IllegalArgumentException("The threadGroupName must not be null");
        }
        final NameVisitor nameVisitor = new NameVisitor(true, threadGroupName);
        new ThreadUtils.ThreadGroupHolder(ThreadUtils.getSystemThreadGroup()).accept(nameVisitor);
        return Collections.unmodifiableCollection(nameVisitor.getResult());
    }

    /**
     * Return all active thread groups including the system thread group (A thread group is active if it has been not destroyed)
     *
     * @return all thread groups including the system thread group. The collection returned is always unmodifiable.
     * @throws  SecurityException
     *          if the current thread cannot access the system thread group
     *
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     */
    public static Collection<ThreadGroup> getAllThreadGroups() {
        final NameVisitor nameVisitor = new NameVisitor(true, null);
        new ThreadUtils.ThreadGroupHolder(ThreadUtils.getSystemThreadGroup()).accept(nameVisitor);
        return Collections.unmodifiableCollection(nameVisitor.getResult());
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
     * @return all active threads. The collection returned is always unmodifiable.
     * @throws  SecurityException
     *          if the current thread cannot access the system thread group
     *
     * @throws  SecurityException  if the current thread cannot modify
     *          thread groups from this thread's thread group up to the system thread group
     */
    public static Collection<Thread> getAllThreads() {
        final NameVisitor nameVisitor = new NameVisitor(false, null);
        new ThreadUtils.ThreadGroupHolder(ThreadUtils.getSystemThreadGroup()).accept(nameVisitor);
        return Collections.unmodifiableCollection(nameVisitor.getResult());
    }

    /**
     * Return active threads with the specified name
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
        if (threadName == null) {
            throw new IllegalArgumentException("The threadName must not be null");
        }
        final NameVisitor nameVisitor = new NameVisitor(false, threadName);
        new ThreadUtils.ThreadGroupHolder(ThreadUtils.getSystemThreadGroup()).accept(nameVisitor);
        return Collections.unmodifiableCollection(nameVisitor.getResult());
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
        final IdVisitor idVisitor = new IdVisitor(threadId);
        new ThreadUtils.ThreadGroupHolder(ThreadUtils.getSystemThreadGroup()).accept(idVisitor);
        return idVisitor.getThread();
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

    //Hierarchical Visitor Pattern
    //make public?
    private static interface Visitable {
        public boolean accept(Visitor visitor);
    }

    private static final class ThreadHolder implements Visitable {
        private final Thread thread;

        public Thread getThread() {
            return thread;
        }

        @Override
        public boolean accept(final Visitor visitor)
        {
            return visitor.visit(this);
        }

        public ThreadHolder(final Thread thread) {
            if(thread == null) {
                throw new IllegalArgumentException("thread must not be null");
            }
            this.thread = thread;
        }
    }

    private static final class ThreadGroupHolder implements Visitable {
        private final ThreadGroup threadGroup;

        public ThreadGroupHolder(final ThreadGroup threadGroup) {
            if(threadGroup == null) {
                throw new IllegalArgumentException("thread group must not be null");
            }
            this.threadGroup = threadGroup;
        }

        public ThreadGroup getThreadGroup() {
            return threadGroup;
        }

        @Override
        public boolean accept(final Visitor v)
        {
            if (v.visitEnter(this))
            {
                int estimatedThreadCount = threadGroup.activeCount() + ENUMERATION_GUESS_EXTRA;
                int threadCount = 0;
                Thread[] threads;
                do {
                    estimatedThreadCount *= 2;
                    threads = new Thread[estimatedThreadCount];
                    threadCount = threadGroup.enumerate(threads, false);
                    //return value of enumerate() must be strictly less than the array size according to javadoc
                } while (threadCount >= estimatedThreadCount);

                for (int i = 0; i < threadCount; i++) {
                    final Thread thread = threads[i];
                    if(!(new ThreadHolder(thread).accept(v))) {
                        break;
                    }
                }

                int estimatedThreadGroupCount = threadGroup.activeGroupCount() + ENUMERATION_GUESS_EXTRA;
                int threadGroupCount = 0;
                ThreadGroup[] threadGroups;
                do {
                    estimatedThreadGroupCount *= 2;
                    threadGroups = new ThreadGroup[estimatedThreadGroupCount];
                    threadGroupCount = threadGroup.enumerate(threadGroups, false);
                    //return value of enumerate() must be strictly less than the array size according to javadoc
                } while (threadGroupCount >= estimatedThreadGroupCount);

                for (int i = 0; i < threadGroupCount; i++) {
                    final ThreadGroup threadGroup = threadGroups[i];
                    if (!(new ThreadGroupHolder(threadGroup)).accept(v)) {
                        break;
                    }
                }
            }
            return v.visitLeave( this );
        }
    }

    private static interface Visitor
    {
        boolean visitEnter(ThreadGroupHolder threadGroup); // going into a branch
        boolean visitLeave(ThreadGroupHolder threadGroup); // coming out
        boolean visit(ThreadHolder thread);
    }


    //private visitors, not for the public
    private static class IdVisitor implements Visitor {
        private Thread thread;
        private final long threadId;

        public IdVisitor(final long threadId) {
            this.threadId = threadId;
        }

        public Thread getThread() {
            return thread;
        }

        @Override
        public boolean visitLeave(final ThreadGroupHolder threadGroup) {
            return thread==null;
        }

        @Override
        public boolean visitEnter(final ThreadGroupHolder threadGroup) {
            return thread==null;
        }

        @Override
        public boolean visit(final ThreadHolder thread) {
            if(thread.getThread().getId() == threadId) {
                this.thread = thread.getThread();
                return false;
            }

            return true;
        }
    }

    private static class NameVisitor implements Visitor {
        private final boolean lookingForThreadGroup;
        private final String name;
        private final List result = new ArrayList();

        public NameVisitor(final boolean lookingForThreadGroup, final String name) {
            this.lookingForThreadGroup = lookingForThreadGroup;
            this.name = name;
        }

        @Override
        public boolean visitLeave(final ThreadGroupHolder threadGroup) {
            return true;
        }

        @Override
        public boolean visitEnter(final ThreadGroupHolder threadGroup) {
            if(lookingForThreadGroup && (name == null || threadGroup.getThreadGroup().getName().equals(name))) {
                result.add(threadGroup.getThreadGroup());
            }
            return true;
        }

        @Override
        public boolean visit(final ThreadHolder thread) {
            if(!lookingForThreadGroup && ( name == null || thread.getThread().getName().equals(name))) {
                result.add(thread.getThread());
            }
            return true;
        }

        public List getResult() {
            return result;
        }
    }

    private static class NameNameVisitor implements Visitor {
        private final String threadName;
        private final String threadGroupName;
        private final ThreadGroup threadGroup;
        private final List result = new ArrayList();

        public NameNameVisitor(final String threadName, final String threadGroupName) {
            this.threadName = threadName;
            this.threadGroupName = threadGroupName;
            this.threadGroup = null;
        }

        public NameNameVisitor(final String threadName, final ThreadGroup threadGroup) {
            this.threadName = threadName;
            this.threadGroup = threadGroup;
            this.threadGroupName = null;
        }

        @Override
        public boolean visitLeave(final ThreadGroupHolder threadGroup) {
            return true;
        }

        @Override
        public boolean visitEnter(final ThreadGroupHolder threadGroup) {
            return true;
        }

        @Override
        public boolean visit(final ThreadHolder thread) {
            if(threadGroup == null && thread.getThread().getName().equals(threadName) && thread.getThread().getThreadGroup().getName().equals(threadGroupName)) {
                result.add(thread.getThread());
            }else if(threadGroup != null && thread.getThread().getName().equals(threadName) && thread.getThread().getThreadGroup().equals(threadGroup)) {
                result.add(thread.getThread());
            }
            return true;
        }

        public List getResult() {
            return result;
        }
    }

}
