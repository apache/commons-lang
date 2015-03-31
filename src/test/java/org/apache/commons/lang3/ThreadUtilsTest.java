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
import java.util.concurrent.CountDownLatch;

import org.junit.Test;

/**
 * Unit tests {@link org.apache.commons.lang3.ThreadUtils}.
 *
 * @version $Id$
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
        ThreadUtils.findThreadsByName("tname", null);
    }

    @Test(expected=IllegalArgumentException.class)
    public void testNullThreadThreadGroupName3() throws InterruptedException {
        ThreadUtils.findThreadsByName(null, null);
    }

    @Test(expected=IllegalArgumentException.class)
    public void testInvalidThreadId() throws InterruptedException {
        ThreadUtils.findThreadById(-5L);
    }

    @Test
    public void testNoThread() throws InterruptedException {
        assertEquals(0, ThreadUtils.findThreadsByName("some_thread_which_does_not_exist_18762ZucTT").length);
    }

    @Test
    public void testNoThreadGroup() throws InterruptedException {
        assertEquals(0, ThreadUtils.findThreadGroupsByName("some_thread_group_which_does_not_exist_18762ZucTTII").length);
    }

    @Test
    public void testSystemThreadGroupExists() throws InterruptedException {
        ThreadGroup systemThreadGroup = ThreadUtils.getSystemThreadGroup();
        assertNotNull(systemThreadGroup);
        assertNull(systemThreadGroup.getParent());
        assertEquals("system", systemThreadGroup.getName());
    }

    @Test
    public void testAtLeastOneThreadExists() throws InterruptedException {
        assertTrue(ThreadUtils.getAllThreads().length > 0);
    }

    @Test
    public void testAtLeastOneThreadGroupExists() throws InterruptedException {
        assertTrue(ThreadUtils.getAllThreadGroups().length > 0);
    }

    @Test
    public void testThreadsSameName() throws InterruptedException {
        Thread t1 = new TestThread("thread1_XXOOLL__");
        Thread alsot1 = new TestThread("thread1_XXOOLL__");

        try {
            t1.start();
            alsot1.start();
            assertEquals(2, ThreadUtils.findThreadsByName("thread1_XXOOLL__").length);
        } finally {
            t1.interrupt();
            alsot1.interrupt();
            t1.join();
            alsot1.join();
        }
    }

    @Test
    public void testThreads() throws InterruptedException {
        Thread t1 = new TestThread("thread1_XXOOLL__");
        Thread t2 = new TestThread("thread2_XXOOLL__");

        try {
            t1.start();
            t2.start();
            assertEquals(1, ThreadUtils.findThreadsByName("thread2_XXOOLL__").length);
        } finally {
            t1.interrupt();
            t2.interrupt();
            t1.join();
            t2.join();
        }
    }

    @Test
    public void testThreadsById() throws InterruptedException {
        Thread t1 = new TestThread("thread1_XXOOLL__");
        Thread t2 = new TestThread("thread2_XXOOLL__");

        try {
            t1.start();
            t2.start();
            assertEquals(t1.getName(), ThreadUtils.findThreadById(t1.getId()).getName());
            assertSame(t2, ThreadUtils.findThreadById(t2.getId()));
        } finally {
            t1.interrupt();
            t2.interrupt();
            t1.join();
            t2.join();
        }
    }

    @Test
    public void testThreadGroups() throws InterruptedException {
        ThreadGroup threadGroup = new ThreadGroup("thread_group_DDZZ99__");
        Thread t1 = new TestThread(threadGroup, "thread1_XXOOPP__");
        Thread t2 = new TestThread(threadGroup, "thread2_XXOOPP__");

        try {
            t1.start();
            t2.start();
            assertEquals(1, ThreadUtils.findThreadsByName("thread1_XXOOPP__").length);
            assertEquals(1, ThreadUtils.findThreadsByName("thread1_XXOOPP__","thread_group_DDZZ99__").length);
            assertEquals(1, ThreadUtils.findThreadsByName("thread2_XXOOPP__","thread_group_DDZZ99__").length);
            assertEquals(0, ThreadUtils.findThreadsByName("thread1_XXOOPP__","non_existent_thread_group_JJHHZZ__").length);
            assertEquals(0, ThreadUtils.findThreadsByName("non_existent_thread_BBDDWW__","thread_group_DDZZ99__").length);
            assertEquals(1, ThreadUtils.findThreadGroupsByName("thread_group_DDZZ99__").length);
            assertEquals(0, ThreadUtils.findThreadGroupsByName("non_existent_thread_group_JJHHZZ__").length);
        } finally {
            t1.interrupt();
            t2.interrupt();
            t1.join();
            t2.join();
            threadGroup.destroy();
        }
    }

    @Test
    public void testThreadGroupsById() throws InterruptedException {
        ThreadGroup threadGroup = new ThreadGroup("thread_group_DDZZ99__");
        Thread t1 = new TestThread(threadGroup, "thread1_XXOOPP__");
        Thread t2 = new TestThread(threadGroup, "thread2_XXOOPP__");
        long nonExistingId = t1.getId()+t2.getId();

        try {
            t1.start();
            t2.start();
            assertEquals(t1.getName(), ThreadUtils.findThreadById(t1.getId(),"thread_group_DDZZ99__").getName());
            assertEquals(t2.getName(), ThreadUtils.findThreadById(t2.getId(),"thread_group_DDZZ99__").getName());
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

    private static class TestThread extends Thread {
        private final CountDownLatch latch = new CountDownLatch(1);

        public TestThread(String name) {
            super(name);
        }

        public TestThread(ThreadGroup group, String name) {
            super(group, name);
        }

        @Override
        public synchronized void start() {
            super.start();
            try {
                latch.await();
            } catch (InterruptedException e) {
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
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
    }
}
