/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.apache.commons.lang3;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.CountDownLatch;

import org.junit.Test;

/**
 * Unit tests {@link org.apache.commons.lang3.ThreadUtils}.
 */
public class ThreadUtilsTest {

    @Test(expected=IllegalArgumentException.class)
    public void testNullThreadName() throws InterruptedException {
        ThreadUtils.findThreadsByName(null);
    }

    @Test(expected=IllegalArgumentException.class)
    public void testNullThreadGroupName() throws InterruptedException {
        ThreadUtils.findThreadGroupsByName(null);
    }

    @Test(expected=IllegalArgumentException.class)
    public void testNullThreadThreadGroupName1() throws InterruptedException {
        ThreadUtils.findThreadsByName(null, "tgname");
    }

    @Test(expected=IllegalArgumentException.class)
    public void testNullThreadThreadGroupName2() throws InterruptedException {
        ThreadUtils.findThreadsByName("tname", (String) null);
    }

    @Test(expected=IllegalArgumentException.class)
    public void testNullThreadThreadGroupName3() throws InterruptedException {
        ThreadUtils.findThreadsByName(null, (String) null);
    }

    @Test(expected=IllegalArgumentException.class)
    public void testNullThreadThreadGroup1() throws InterruptedException {
        ThreadUtils.findThreadsByName("tname", (ThreadGroup) null);
    }

    @Test(expected=IllegalArgumentException.class)
    public void testNullThreadThreadGroup2() throws InterruptedException {
        ThreadUtils.findThreadById(1L, (ThreadGroup) null);
    }

    @Test(expected=IllegalArgumentException.class)
    public void testNullThreadThreadGroup3() throws InterruptedException {
        ThreadUtils.findThreadsByName(null, (ThreadGroup) null);
    }

    @Test(expected=IllegalArgumentException.class)
    public void testInvalidThreadId() throws InterruptedException {
        ThreadUtils.findThreadById(-5L);
    }

    @Test(expected=IllegalArgumentException.class)
    public void testThreadGroupsByIdFail() throws InterruptedException {
        ThreadUtils.findThreadById(Thread.currentThread().getId(), (String) null);
    }

    @Test(expected=IllegalArgumentException.class)
    public void testThreadgroupsNullParent() throws InterruptedException {
        ThreadUtils.findThreadGroups(null, true, ThreadUtils.ALWAYS_TRUE_PREDICATE);
    }

    @Test(expected=IllegalArgumentException.class)
    public void testThreadgroupsNullPredicate() throws InterruptedException {
        ThreadUtils.findThreadGroups(null);
    }

    @Test(expected=IllegalArgumentException.class)
    public void testThreadsNullPredicate() throws InterruptedException {
        ThreadUtils.findThreads(null);
    }

    @Test
    public void testNoThread() throws InterruptedException {
        assertEquals(0, ThreadUtils.findThreadsByName("some_thread_which_does_not_exist_18762ZucTT").size());
    }

    @Test
    public void testNoThreadGroup() throws InterruptedException {
        assertEquals(0, ThreadUtils.findThreadGroupsByName("some_thread_group_which_does_not_exist_18762ZucTTII").size());
    }

    @Test
    public void testSystemThreadGroupExists() throws InterruptedException {
        final ThreadGroup systemThreadGroup = ThreadUtils.getSystemThreadGroup();
        assertNotNull(systemThreadGroup);
        assertNull(systemThreadGroup.getParent());
        assertEquals("system", systemThreadGroup.getName());
    }

    @Test
    public void testAtLeastOneThreadExists() throws InterruptedException {
        assertTrue(ThreadUtils.getAllThreads().size() > 0);
    }

    @Test
    public void testAtLeastOneThreadGroupsExists() throws InterruptedException {
        assertTrue(ThreadUtils.getAllThreadGroups().size() > 0);
    }

    @Test
    public void testThreadsSameName() throws InterruptedException {
        final Thread t1 = new TestThread("thread1_XXOOLL__");
        final Thread alsot1 = new TestThread("thread1_XXOOLL__");

        try {
            t1.start();
            alsot1.start();
            assertEquals(2, ThreadUtils.findThreadsByName("thread1_XXOOLL__").size());
        } finally {
            t1.interrupt();
            alsot1.interrupt();
            t1.join();
            alsot1.join();
        }
    }

    @Test
    public void testThreads() throws InterruptedException {
        final Thread t1 = new TestThread("thread1_XXOOLL__");
        final Thread t2 = new TestThread("thread2_XXOOLL__");

        try {
            t1.start();
            t2.start();
            assertEquals(1, ThreadUtils.findThreadsByName("thread2_XXOOLL__").size());
        } finally {
            t1.interrupt();
            t2.interrupt();
            t1.join();
            t2.join();
        }
    }

    @Test
    public void testThreadsById() throws InterruptedException {
        final Thread t1 = new TestThread("thread1_XXOOLL__");
        final Thread t2 = new TestThread("thread2_XXOOLL__");

        try {
            t1.start();
            t2.start();
            assertSame(t1, ThreadUtils.findThreadById(t1.getId()));
            assertSame(t2, ThreadUtils.findThreadById(t2.getId()));
        } finally {
            t1.interrupt();
            t2.interrupt();
            t1.join();
            t2.join();
        }
    }

    @Test
    public void testThreadsByIdWrongGroup() throws InterruptedException {
        final Thread t1 = new TestThread("thread1_XXOOLL__");
        final ThreadGroup tg = new ThreadGroup("tg__HHEE22");

        try {
            t1.start();
            assertNull(ThreadUtils.findThreadById(t1.getId(), tg));
        } finally {
            t1.interrupt();
            t1.join();
            tg.destroy();
        }
    }


    @Test
    public void testThreadGroups() throws InterruptedException {
        final ThreadGroup threadGroup = new ThreadGroup("thread_group_DDZZ99__");
        final Thread t1 = new TestThread(threadGroup, "thread1_XXOOPP__");
        final Thread t2 = new TestThread(threadGroup, "thread2_XXOOPP__");

        try {
            t1.start();
            t2.start();
            assertEquals(1, ThreadUtils.findThreadsByName("thread1_XXOOPP__").size());
            assertEquals(1, ThreadUtils.findThreadsByName("thread1_XXOOPP__","thread_group_DDZZ99__").size());
            assertEquals(1, ThreadUtils.findThreadsByName("thread2_XXOOPP__","thread_group_DDZZ99__").size());
            assertEquals(0, ThreadUtils.findThreadsByName("thread1_XXOOPP__","non_existent_thread_group_JJHHZZ__").size());
            assertEquals(0, ThreadUtils.findThreadsByName("non_existent_thread_BBDDWW__","thread_group_DDZZ99__").size());
            assertEquals(1, ThreadUtils.findThreadGroupsByName("thread_group_DDZZ99__").size());
            assertEquals(0, ThreadUtils.findThreadGroupsByName("non_existent_thread_group_JJHHZZ__").size());
            assertNotNull(ThreadUtils.findThreadById(t1.getId(),threadGroup));
        } finally {
            t1.interrupt();
            t2.interrupt();
            t1.join();
            t2.join();
            threadGroup.destroy();
        }
    }

    @Test
    public void testThreadGroupsRef() throws InterruptedException {
        final ThreadGroup threadGroup = new ThreadGroup("thread_group_DDZZ99__");
        final ThreadGroup deadThreadGroup = new ThreadGroup("dead_thread_group_MMQQSS__");
        deadThreadGroup.destroy();
        final Thread t1 = new TestThread(threadGroup, "thread1_XXOOPP__");
        final Thread t2 = new TestThread(threadGroup, "thread2_XXOOPP__");

        try {
            t1.start();
            t2.start();
            assertEquals(1, ThreadUtils.findThreadsByName("thread1_XXOOPP__").size());
            assertEquals(1, ThreadUtils.findThreadsByName("thread1_XXOOPP__",threadGroup).size());
            assertEquals(1, ThreadUtils.findThreadsByName("thread2_XXOOPP__",threadGroup).size());
            assertEquals(0, ThreadUtils.findThreadsByName("thread1_XXOOPP__",deadThreadGroup).size());
        } finally {
            t1.interrupt();
            t2.interrupt();
            t1.join();
            t2.join();
            threadGroup.destroy();
            assertEquals(0, ThreadUtils.findThreadsByName("thread2_XXOOPP__",threadGroup).size());
        }
    }

    @Test
    public void testThreadGroupsById() throws InterruptedException {
        final ThreadGroup threadGroup = new ThreadGroup("thread_group_DDZZ99__");
        final Thread t1 = new TestThread(threadGroup, "thread1_XXOOPP__");
        final Thread t2 = new TestThread(threadGroup, "thread2_XXOOPP__");
        final long nonExistingId = t1.getId()+t2.getId();

        try {
            t1.start();
            t2.start();
            assertSame(t1, ThreadUtils.findThreadById(t1.getId(),"thread_group_DDZZ99__"));
            assertSame(t2, ThreadUtils.findThreadById(t2.getId(),"thread_group_DDZZ99__"));
            assertNull(ThreadUtils.findThreadById(nonExistingId,"non_existent_thread_group_JJHHZZ__"));
            assertNull(ThreadUtils.findThreadById(nonExistingId,"thread_group_DDZZ99__"));
        } finally {
            t1.interrupt();
            t2.interrupt();
            t1.join();
            t2.join();
            threadGroup.destroy();
        }
    }

    @Test
    public void testConstructor() throws InterruptedException {
        assertNotNull(new ThreadUtils());
        final Constructor<?>[] cons = ThreadUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertTrue(Modifier.isPublic(cons[0].getModifiers()));
        assertTrue(Modifier.isPublic(ThreadUtils.class.getModifiers()));
        assertFalse(Modifier.isFinal(ThreadUtils.class.getModifiers()));
    }

    @Test
    public void testComplexThreadGroups() throws Exception {
        final ThreadGroup threadGroup1 = new ThreadGroup("thread_group_1__");
        final ThreadGroup threadGroup2 = new ThreadGroup("thread_group_2__");
        final ThreadGroup threadGroup3 = new ThreadGroup(threadGroup2, "thread_group_3__");
        final ThreadGroup threadGroup4 = new ThreadGroup(threadGroup2, "thread_group_4__");
        final ThreadGroup threadGroup5 = new ThreadGroup(threadGroup1, "thread_group_5__");
        final ThreadGroup threadGroup6 = new ThreadGroup(threadGroup4, "thread_group_6__");
        final ThreadGroup threadGroup7 = new ThreadGroup(threadGroup4, "thread_group_7__");
        final ThreadGroup threadGroup7Doubled = new ThreadGroup(threadGroup4, "thread_group_7__");
        final List<ThreadGroup> threadGroups = Arrays.asList(threadGroup1,threadGroup2,threadGroup3,threadGroup4,threadGroup5,threadGroup6, threadGroup7, threadGroup7Doubled);

        final Thread t1 = new TestThread("thread1_X__");
        final Thread t2 = new TestThread(threadGroup1, "thread2_X__");
        final Thread t3 = new TestThread(threadGroup2, "thread3_X__");
        final Thread t4 = new TestThread(threadGroup3, "thread4_X__");
        final Thread t5 = new TestThread(threadGroup4, "thread5_X__");
        final Thread t6 = new TestThread(threadGroup5, "thread6_X__");
        final Thread t7 = new TestThread(threadGroup6, "thread7_X__");
        final Thread t8 = new TestThread(threadGroup4, "thread8_X__");
        final Thread t9 = new TestThread(threadGroup6, "thread9_X__");
        final Thread t10 = new TestThread(threadGroup3, "thread10_X__");
        final Thread t11 = new TestThread(threadGroup7, "thread11_X__");
        final Thread t11Doubled = new TestThread(threadGroup7Doubled, "thread11_X__");
        final List<Thread> threads = Arrays.asList(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10, t11, t11Doubled);

        try {
            for (final Thread thread : threads) {
                thread.start();
            }
            assertTrue(ThreadUtils.getAllThreadGroups().size() >= 7);
            assertTrue(ThreadUtils.getAllThreads().size() >= 11);
            assertTrue(ThreadUtils.findThreads(ThreadUtils.ALWAYS_TRUE_PREDICATE).size() >= 11);
            assertEquals(1, ThreadUtils.findThreadsByName(t4.getName(), threadGroup3.getName()).size());
            assertEquals(0, ThreadUtils.findThreadsByName(t4.getName(), threadGroup2.getName()).size());
            assertEquals(2, ThreadUtils.findThreadsByName(t11.getName(), threadGroup7.getName()).size());
        }finally {
            for (final Thread thread : threads) {
                thread.interrupt();
                thread.join();
            }
            for (final ThreadGroup threadGroup : threadGroups) {
                if (!threadGroup.isDestroyed()) {
                    threadGroup.destroy();
                }
            }
        }
    }


    private static class TestThread extends Thread {
        private final CountDownLatch latch = new CountDownLatch(1);

        TestThread(final String name) {
            super(name);
        }

        TestThread(final ThreadGroup group, final String name) {
            super(group, name);
        }

        @Override
        public synchronized void start() {
            super.start();
            try {
                latch.await();
            } catch (final InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }

        @Override
        public void run() {
            latch.countDown();
            try {
                synchronized(this){
                    this.wait();
                }
            } catch (final InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
    }
}
