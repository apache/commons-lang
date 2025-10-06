/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.apache.commons.lang3;

import static org.apache.commons.lang3.LangAssertions.assertNullPointerException;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.time.Duration;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CountDownLatch;
import java.util.function.Predicate;

import org.apache.commons.lang3.ThreadUtils.ThreadGroupPredicate;
import org.apache.commons.lang3.ThreadUtils.ThreadPredicate;
import org.apache.commons.lang3.function.Predicates;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link ThreadUtils}.
 */
class ThreadUtilsTest extends AbstractLangTest {

    private static final class TestThread extends Thread {
        private final CountDownLatch latch = new CountDownLatch(1);

        TestThread(final String name) {
            super(name);
        }

        TestThread(final ThreadGroup group, final String name) {
            super(group, name);
        }

        @Override
        public void run() {
            latch.countDown();
            try {
                synchronized (this) {
                    this.wait();
                }
            } catch (final InterruptedException e) {
                Thread.currentThread().interrupt();
            }
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
    }

    @Test
    void testAtLeastOneThreadExists() {
        assertFalse(ThreadUtils.getAllThreads().isEmpty());
    }

    @Test
    void testAtLeastOneThreadGroupsExists() {
        assertFalse(ThreadUtils.getAllThreadGroups().isEmpty());
    }

    @Test
    void testComplexThreadGroups() throws Exception {
        final ThreadGroup threadGroup1 = new ThreadGroup("thread_group_1__");
        final ThreadGroup threadGroup2 = new ThreadGroup("thread_group_2__");
        final ThreadGroup threadGroup3 = new ThreadGroup(threadGroup2, "thread_group_3__");
        final ThreadGroup threadGroup4 = new ThreadGroup(threadGroup2, "thread_group_4__");
        final ThreadGroup threadGroup5 = new ThreadGroup(threadGroup1, "thread_group_5__");
        final ThreadGroup threadGroup6 = new ThreadGroup(threadGroup4, "thread_group_6__");
        final ThreadGroup threadGroup7 = new ThreadGroup(threadGroup4, "thread_group_7__");
        final ThreadGroup threadGroup7Doubled = new ThreadGroup(threadGroup4, "thread_group_7__");
        final List<ThreadGroup> threadGroups = Arrays.asList(threadGroup1, threadGroup2, threadGroup3, threadGroup4, threadGroup5, threadGroup6, threadGroup7,
            threadGroup7Doubled);

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
        final List<Thread> threads = Arrays.asList(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t11Doubled);

        try {
            for (final Thread thread : threads) {
                thread.start();
            }
            assertTrue(ThreadUtils.getAllThreadGroups().size() >= 7, "getAllThreadGroups");
            assertTrue(ThreadUtils.getAllThreads().size() >= 11, "getAllThreads");
            assertTrue(ThreadUtils.findThreads(Predicates.truePredicate()).size() >= 11, "findThreads(ThreadUtils.truePredicate())");
            assertEquals(1, ThreadUtils.findThreadsByName(t4.getName(), threadGroup3.getName()).size());
            assertEquals(0, ThreadUtils.findThreadsByName(t4.getName(), threadGroup2.getName()).size());
            assertEquals(2, ThreadUtils.findThreadsByName(t11.getName(), threadGroup7.getName()).size());
        } finally {
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

    @Test
    void testConstructor() {
        assertNotNull(new ThreadUtils());
        final Constructor<?>[] cons = ThreadUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertTrue(Modifier.isPublic(cons[0].getModifiers()));
        assertTrue(Modifier.isPublic(ThreadUtils.class.getModifiers()));
        assertFalse(Modifier.isFinal(ThreadUtils.class.getModifiers()));
    }

    @SuppressWarnings("deprecation")
    @Test
    void testDepreacted() {
        assertNotNull(ThreadUtils.ALWAYS_TRUE_PREDICATE);
        final ThreadPredicate tp = ThreadUtils.ALWAYS_TRUE_PREDICATE;
        assertTrue(tp.test(null));
        assertTrue(tp.test(new Thread()));
        final ThreadGroupPredicate tgp = ThreadUtils.ALWAYS_TRUE_PREDICATE;
        assertTrue(tgp.test(null));
        assertTrue(tgp.test(new ThreadGroup("")));
    }

    @Test
    void testGetAllThreadGroupsDoesNotReturnNull() {
        // LANG-1706 getAllThreadGroups and findThreadGroups should not return null items
        final Collection<ThreadGroup> threads = ThreadUtils.getAllThreadGroups();
        assertEquals(0, threads.stream().filter(Objects::isNull).count());
    }

    @Test
    void testGetAllThreadsDoesNotReturnNull() {
        // LANG-1706 getAllThreads and findThreads should not return null items
        final Collection<Thread> threads = ThreadUtils.getAllThreads();
        assertEquals(0, threads.stream().filter(Objects::isNull).count());
    }

    @Test
    void testInvalidThreadId() {
        assertThrows(IllegalArgumentException.class, () -> ThreadUtils.findThreadById(-5L));
    }

    @Test
    void testJoinDuration() throws InterruptedException {
        ThreadUtils.join(new Thread(), Duration.ZERO);
        ThreadUtils.join(new Thread(), Duration.ofMillis(1));
    }

    @Test
    void testNoThread() {
        assertEquals(0, ThreadUtils.findThreadsByName("some_thread_which_does_not_exist_18762ZucTT").size());
    }

    @Test
    void testNoThreadGroup() {
        assertEquals(0, ThreadUtils.findThreadGroupsByName("some_thread_group_which_does_not_exist_18762ZucTTII").size());
    }

    @Test
    void testNullThreadGroupName() {
        assertNullPointerException(() -> ThreadUtils.findThreadGroupsByName(null));
    }

    @Test
    void testNullThreadName() {
        assertNullPointerException(() -> ThreadUtils.findThreadsByName(null));
    }

    @Test
    void testNullThreadThreadGroup1() {
        assertNullPointerException(() -> ThreadUtils.findThreadsByName("tname", (ThreadGroup) null));
    }

    @Test
    void testNullThreadThreadGroup2() {
        assertNullPointerException(() -> ThreadUtils.findThreadById(1L, (ThreadGroup) null));
    }

    @Test
    void testNullThreadThreadGroup3() {
        assertNullPointerException(() -> ThreadUtils.findThreadsByName(null, (ThreadGroup) null));
    }

    @Test
    void testNullThreadThreadGroupName1() {
        assertNullPointerException(() -> ThreadUtils.findThreadsByName(null, "tgname"));
    }

    @Test
    void testNullThreadThreadGroupName2() {
        assertNullPointerException(() -> ThreadUtils.findThreadsByName("tname", (String) null));
    }

    @Test
    void testNullThreadThreadGroupName3() {
        assertNullPointerException(() -> ThreadUtils.findThreadsByName(null, (String) null));
    }

    @Test
    void testSleepDuration() throws InterruptedException {
        ThreadUtils.sleep(Duration.ZERO);
        ThreadUtils.sleep(Duration.ofMillis(1));
    }

    @Test
    void testSystemThreadGroupExists() {
        final ThreadGroup systemThreadGroup = ThreadUtils.getSystemThreadGroup();
        assertNotNull(systemThreadGroup);
        assertNull(systemThreadGroup.getParent());
        assertEquals("system", systemThreadGroup.getName());
    }

    @Test
    void testThreadGroups() throws InterruptedException {
        final String threadGroupName = "thread_group_DDZZ99__for_testThreadGroups";
        final ThreadGroup threadGroup = new ThreadGroup(threadGroupName);
        final Thread t1 = new TestThread(threadGroup, "thread1_XXOOPP__");
        final Thread t2 = new TestThread(threadGroup, "thread2_XXOOPP__");

        try {
            t1.start();
            t2.start();
            assertEquals(1, ThreadUtils.findThreadsByName("thread1_XXOOPP__").size());
            assertEquals(1, ThreadUtils.findThreadsByName("thread1_XXOOPP__", threadGroupName).size());
            assertEquals(1, ThreadUtils.findThreadsByName("thread2_XXOOPP__", threadGroupName).size());
            assertEquals(0, ThreadUtils.findThreadsByName("thread1_XXOOPP__", "non_existent_thread_group_JJHHZZ__").size());
            assertEquals(0, ThreadUtils.findThreadsByName("non_existent_thread_BBDDWW__", threadGroupName).size());
            assertEquals(1, ThreadUtils.findThreadGroupsByName(threadGroupName).size());
            assertEquals(0, ThreadUtils.findThreadGroupsByName("non_existent_thread_group_JJHHZZ__").size());
            assertNotNull(ThreadUtils.findThreadById(t1.getId(), threadGroup));
        } finally {
            t1.interrupt();
            t2.interrupt();
            t1.join();
            t2.join();
            threadGroup.destroy();
        }
    }

    @Test
    void testThreadGroupsById() throws InterruptedException {
        final String threadGroupName = "thread_group_DDZZ99__for_testThreadGroupsById";
        final ThreadGroup threadGroup = new ThreadGroup(threadGroupName);
        final Thread t1 = new TestThread(threadGroup, "thread1_XXOOPP__");
        final Thread t2 = new TestThread(threadGroup, "thread2_XXOOPP__");
        final long nonExistingId = t1.getId() + t2.getId();

        try {
            t1.start();
            t2.start();
            assertSame(t1, ThreadUtils.findThreadById(t1.getId(), threadGroupName));
            assertSame(t2, ThreadUtils.findThreadById(t2.getId(), threadGroupName));
            assertNull(ThreadUtils.findThreadById(nonExistingId, "non_existent_thread_group_JJHHZZ__"));
            assertNull(ThreadUtils.findThreadById(nonExistingId, threadGroupName));
        } finally {
            t1.interrupt();
            t2.interrupt();
            t1.join();
            t2.join();
            threadGroup.destroy();
        }
    }

    @Test
    void testThreadGroupsByIdFail() {
        assertNullPointerException(() -> ThreadUtils.findThreadById(Thread.currentThread().getId(), (String) null));
    }

    @Test
    void testThreadGroupsNullParent() {
        assertNullPointerException(() -> ThreadUtils.findThreadGroups(null, true, Predicates.truePredicate()));
        assertNullPointerException(() -> ThreadUtils.findThreadGroups(null, false, Predicates.truePredicate()));
    }

    @Test
    void testThreadGroupsNullPredicate() {
        assertNullPointerException(() -> ThreadUtils.findThreadGroups((ThreadGroupPredicate) null));
        assertNullPointerException(() -> ThreadUtils.findThreadGroups((Predicate<ThreadGroup>) null));
        assertNullPointerException(() -> ThreadUtils.findThreadGroups((Predicate) null));
    }

    @Test
    void testThreadGroupsRef() throws InterruptedException {
        final ThreadGroup threadGroup = new ThreadGroup("thread_group_DDZZ99__");
        final ThreadGroup deadThreadGroup = new ThreadGroup("dead_thread_group_MMQQSS__");
        deadThreadGroup.destroy();
        final Thread t1 = new TestThread(threadGroup, "thread1_XXOOPP__");
        final Thread t2 = new TestThread(threadGroup, "thread2_XXOOPP__");

        try {
            t1.start();
            t2.start();
            assertEquals(1, ThreadUtils.findThreadsByName("thread1_XXOOPP__").size());
            assertEquals(1, ThreadUtils.findThreadsByName("thread1_XXOOPP__", threadGroup).size());
            assertEquals(1, ThreadUtils.findThreadsByName("thread2_XXOOPP__", threadGroup).size());
            assertEquals(0, ThreadUtils.findThreadsByName("thread1_XXOOPP__", deadThreadGroup).size());
        } finally {
            t1.interrupt();
            t2.interrupt();
            t1.join();
            t2.join();
            threadGroup.destroy();
            assertEquals(0, ThreadUtils.findThreadsByName("thread2_XXOOPP__", threadGroup).size());
        }
    }

    @Test
    void testThreads() throws InterruptedException {
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
    void testThreadsById() throws InterruptedException {
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
    void testThreadsByIdWrongGroup() throws InterruptedException {
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
    void testThreadsNullPredicate() {
        assertNullPointerException(() -> ThreadUtils.findThreads((ThreadPredicate) null));
        assertNullPointerException(() -> ThreadUtils.findThreads((Predicate<Thread>) null));
        assertNullPointerException(() -> ThreadUtils.findThreads((Predicate) null));
    }

    @Test
    void testThreadsSameName() throws InterruptedException {
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

}
