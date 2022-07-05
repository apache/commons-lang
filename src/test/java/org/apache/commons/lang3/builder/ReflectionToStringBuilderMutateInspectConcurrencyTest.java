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

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

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
 */
public class ReflectionToStringBuilderMutateInspectConcurrencyTest extends AbstractLangTest {

    class TestFixture {
        private final LinkedList<Integer> listField = new LinkedList<>();
        private final Random random = new Random();
        private final int N = 100;

        TestFixture() {
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
        private final TestFixture testFixture;
        private final Random random = new Random();

        MutatingClient(final TestFixture testFixture) {
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
        private final TestFixture testFixture;

        InspectingClient(final TestFixture testFixture) {
            this.testFixture = testFixture;
        }

        @Override
        public void run() {
            ReflectionToStringBuilder.toString(testFixture);
        }
    }

    @Test
    @Disabled
    public void testConcurrency() {
        final TestFixture testFixture = new TestFixture();
        final int numMutators = 10;
        final int numIterations = 10;
        for (int i = 0; i < numIterations; i++) {
            for (int j = 0; j < numMutators; j++) {
                final Thread t = new Thread(new MutatingClient(testFixture));
                t.start();
                final Thread s = new Thread(new InspectingClient(testFixture));
                s.start();
            }
        }
    }
}
