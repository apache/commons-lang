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

package org.apache.commons.lang3.builder;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import junit.framework.Assert;

import org.junit.Ignore;
import org.junit.Test;

/**
 * Tests concurrent access for {@link ReflectionToStringBuilder}.
 * <p>
 * The {@link ToStringStyle} class includes a registry to avoid infinite loops for objects with circular references. We
 * want to make sure that we do not get concurrency exceptions accessing this registry.
 * </p>
 * <p>
 * The tests on the non-thread-safe collections do not pass.
 * </p>
 * 
 * @see <a href="https://issues.apache.org/jira/browse/LANG-762">[LANG-762] Handle or document ReflectionToStringBuilder
 *      and ToStringBuilder for collections that are not thread safe</a>
 * @since 3.1
 * @version $Id$
 */
public class ReflectionToStringBuilderConcurrencyTest {

    static class CollectionHolder<T extends Collection<?>> {
        T collection;

        CollectionHolder(T collection) {
            this.collection = collection;
        }
    }

    private static final int DATA_SIZE = 100000;
    private static final int REPEAT = 100;

    @Test
    @Ignore
    public void testLinkedList() throws InterruptedException, ExecutionException {
        this.testConcurrency(new CollectionHolder<List<Integer>>(new LinkedList<Integer>()));
    }

    @Test
    @Ignore
    public void testArrayList() throws InterruptedException, ExecutionException {
        this.testConcurrency(new CollectionHolder<List<Integer>>(new ArrayList<Integer>()));
    }

    @Test
    @Ignore
    public void testCopyOnWriteArrayList() throws InterruptedException, ExecutionException {
        this.testConcurrency(new CollectionHolder<List<Integer>>(new CopyOnWriteArrayList<Integer>()));
    }

    private void testConcurrency(final CollectionHolder<List<Integer>> holder) throws InterruptedException,
            ExecutionException {
        final List<Integer> list = holder.collection;
        // make a big array that takes a long time to toString()
        for (int i = 0; i < DATA_SIZE; i++) {
            list.add(Integer.valueOf(i));
        }
        // Create a thread pool with two threads to cause the most contention on the underlying resource.
        final ExecutorService threadPool = Executors.newFixedThreadPool(2);
        // Consumes toStrings
        Callable<Integer> consumer = new Callable<Integer>() {
            @Override
            public Integer call() {
                for (int i = 0; i < REPEAT; i++) {
                    String s = ReflectionToStringBuilder.toString(holder);
                    Assert.assertNotNull(s);
                }
                return Integer.valueOf(REPEAT);
            }
        };
        // Produces changes in the list
        Callable<Integer> producer = new Callable<Integer>() {
            @Override
            public Integer call() {
                for (int i = 0; i < DATA_SIZE; i++) {
                    list.remove(list.get(0));
                }
                return Integer.valueOf(REPEAT);
            }
        };
        Collection<Callable<Integer>> tasks = new ArrayList<Callable<Integer>>();
        tasks.add(consumer);
        tasks.add(producer);
        final List<Future<Integer>> futures = threadPool.invokeAll(tasks);
        for (Future<Integer> future : futures) {
            Assert.assertEquals(REPEAT, future.get().intValue());
        }
    }
}
