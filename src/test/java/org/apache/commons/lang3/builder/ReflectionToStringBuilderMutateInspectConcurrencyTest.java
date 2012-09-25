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

import java.util.LinkedList;
import java.util.Random;

import org.junit.Ignore;
import org.junit.Test;

/**
 * Tests concurrent access for {@link ReflectionToStringBuilder}.
 * <p>
 * The {@link ToStringStyle} class includes a registry to avoid infinite loops for objects with circular references. We
 * want to make sure that we do not get concurrency exceptions accessing this registry.
 * </p>
 * 
 * @see <a href="https://issues.apache.org/jira/browse/LANG-762">[LANG-762] Handle or document ReflectionToStringBuilder
 *      and ToStringBuilder for collections that are not thread safe</a>
 * @since 3.1
 * @version $Id$
 */
public class ReflectionToStringBuilderMutateInspectConcurrencyTest {

    class TestFixture {
        final private LinkedList<Integer> listField = new LinkedList<Integer>();
        final private Random random = new Random();
        private int N = 100;

        public TestFixture() {
            synchronized (this) {
                for (int i = 0; i < N; i++) {
                    listField.add(Integer.valueOf(i));
                }
            }
        }

        public synchronized void add() {
            listField.add(Integer.valueOf(random.nextInt(N)));
        }

        public synchronized void delete() {
            listField.remove(Integer.valueOf(random.nextInt(N)));
        }
    }

    class MutatingClient implements Runnable {
        final private TestFixture testFixture;
        final private Random random = new Random();

        public MutatingClient(TestFixture testFixture) {
            this.testFixture = testFixture;
        }

        @Override
        public void run() {
            if (random.nextBoolean()) {
                testFixture.add();
            } else {
                testFixture.delete();
            }
        }
    }

    class InspectingClient implements Runnable {
        final private TestFixture testFixture;

        public InspectingClient(TestFixture testFixture) {
            this.testFixture = testFixture;
        }

        @Override
        public void run() {
            ReflectionToStringBuilder.toString(testFixture);
        }
    }

    @Test
    @Ignore
    public void testConcurrency() throws Exception {
        TestFixture testFixture = new TestFixture();
        final int numMutators = 10;
        final int numIterations = 10;
        for (int i = 0; i < numIterations; i++) {
            for (int j = 0; j < numMutators; j++) {
                Thread t = new Thread(new MutatingClient(testFixture));
                t.start();
                Thread s = new Thread(new InspectingClient(testFixture));
                s.start();
            }
        }
    }
}
