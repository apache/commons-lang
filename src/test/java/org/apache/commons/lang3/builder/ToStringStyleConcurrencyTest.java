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

import org.junit.Test;

/**
 * Tests concurrent access for the default {@link ToStringStyle}.
 * <p>
 * The {@link ToStringStyle} class includes a registry to avoid infinite loops for objects with circular references. We
 * want to make sure that we do not get concurrency exceptions accessing this registry.
 * </p>
 * <p>
 * This test passes but only tests one aspect of the issue.
 * </p>
 * 
 * @see <a href="https://issues.apache.org/jira/browse/LANG-762">[LANG-762] Handle or document ReflectionToStringBuilder
 *      and ToStringBuilder for collections that are not thread safe</a>
 * @since 3.1
 * @version $Id$
 */
public class ToStringStyleConcurrencyTest {

    static class CollectionHolder<T extends Collection<?>> {
        T collection;

        CollectionHolder(T collection) {
            this.collection = collection;
        }
    }

    private static final List<Integer> LIST;
    private static final int LIST_SIZE = 100000;
    private static final int REPEAT = 100;

    static {
        LIST = new ArrayList<Integer>(LIST_SIZE);
        for (int i = 0; i < LIST_SIZE; i++) {
            LIST.add(Integer.valueOf(i));
        }
    }

    @Test
    public void testLinkedList() throws InterruptedException, ExecutionException {
        this.testConcurrency(new CollectionHolder<List<Integer>>(new LinkedList<Integer>()));
    }

    @Test
    public void testArrayList() throws InterruptedException, ExecutionException {
        this.testConcurrency(new CollectionHolder<List<Integer>>(new ArrayList<Integer>()));
    }

    @Test
    public void testCopyOnWriteArrayList() throws InterruptedException, ExecutionException {
        this.testConcurrency(new CollectionHolder<List<Integer>>(new CopyOnWriteArrayList<Integer>()));
    }

    private void testConcurrency(final CollectionHolder<List<Integer>> holder) throws InterruptedException,
            ExecutionException {
        final List<Integer> list = holder.collection;
        // make a big array that takes a long time to toString()
        list.addAll(LIST);
        // Create a thread pool with two threads to cause the most contention on the underlying resource.
        final ExecutorService threadPool = Executors.newFixedThreadPool(2);
        // Consumes toStrings
        Callable<Integer> consumer = new Callable<Integer>() {
            @Override
            public Integer call() {
                for (int i = 0; i < REPEAT; i++) {
                    // Calls ToStringStyle
                    new ToStringBuilder(holder).append(holder.collection);
                }
                return Integer.valueOf(REPEAT);
            }
        };
        Collection<Callable<Integer>> tasks = new ArrayList<Callable<Integer>>();
        tasks.add(consumer);
        tasks.add(consumer);
        final List<Future<Integer>> futures = threadPool.invokeAll(tasks);
        for (Future<Integer> future : futures) {
            future.get();
        }
    }
}
