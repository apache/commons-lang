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
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.concurrent.UncheckedFuture;
import org.junit.jupiter.api.Test;

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
 */
public class ToStringStyleConcurrencyTest extends AbstractLangTest {

    static class CollectionHolder<T extends Collection<?>> {
        T collection;

        CollectionHolder(final T collection) {
            this.collection = collection;
        }
    }

    private static final List<Integer> LIST;
    private static final int LIST_SIZE = 100000;
    private static final int REPEAT = 100;

    static {
        LIST = new ArrayList<>(LIST_SIZE);
        for (int i = 0; i < LIST_SIZE; i++) {
            LIST.add(Integer.valueOf(i));
        }
    }

    @Test
    public void testLinkedList() throws InterruptedException {
        this.testConcurrency(new CollectionHolder<>(new LinkedList<>()));
    }

    @Test
    public void testArrayList() throws InterruptedException {
        this.testConcurrency(new CollectionHolder<>(new ArrayList<>()));
    }

    @Test
    public void testCopyOnWriteArrayList() throws InterruptedException {
        this.testConcurrency(new CollectionHolder<>(new CopyOnWriteArrayList<>()));
    }

    private void testConcurrency(final CollectionHolder<List<Integer>> holder) throws InterruptedException {
        final List<Integer> list = holder.collection;
        // make a big array that takes a long time to toString()
        list.addAll(LIST);
        // Create a thread pool with two threads to cause the most contention on the underlying resource.
        final ExecutorService threadPool = Executors.newFixedThreadPool(2);
        try {
            // Consumes toStrings
            final Callable<Integer> consumer = () -> {
                for (int i = 0; i < REPEAT; i++) {
                    // Calls ToStringStyle
                    new ToStringBuilder(holder).append(holder.collection);
                }
                return Integer.valueOf(REPEAT);
            };
            final Collection<Callable<Integer>> tasks = new ArrayList<>();
            tasks.add(consumer);
            tasks.add(consumer);
            final List<Future<Integer>> futures = threadPool.invokeAll(tasks);
            UncheckedFuture.on(futures).forEach(UncheckedFuture::get);
        } finally {
            threadPool.shutdown();
            threadPool.awaitTermination(1, TimeUnit.SECONDS);
        }
    }
}
