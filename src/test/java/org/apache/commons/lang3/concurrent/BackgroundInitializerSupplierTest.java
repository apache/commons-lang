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
package org.apache.commons.lang3.concurrent;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;

public class BackgroundInitializerSupplierTest extends BackgroundInitializerTest {

    protected BackgroundInitializerTestImpl getBackgroundInitializerTestImpl() {
        return new BackgroundInitializerTestImpl();
    }

    protected BackgroundInitializerTestImpl getBackgroundInitializerTestImpl(final ExecutorService exec) {
        return new BackgroundInitializerTestImpl(exec);
    }

    /**
     * A concrete implementation of BackgroundInitializer. It is designed as a warpper so the test can
     * use the same builder pattern that real code will.
     */
    protected static final class BackgroundInitializerTestWrapper extends BackgroundInitializerTestImpl {

        /** The BackgroundInitializer we are testing. */
        final BackgroundInitializer<Integer> wrappedBackgroundInitializer;

        BackgroundInitializerTestWrapper() {
            this(null);
        }

        BackgroundInitializerTestWrapper(final ExecutorService exec) {
            wrappedBackgroundInitializer = BackgroundInitializer.<Integer>builder()
                    .setInitializer(this::initialize).setExternalExecutor(exec).get();
        }

        // proxy methods begin here
        public synchronized boolean isInitialized() {
            return wrappedBackgroundInitializer.isInitialized();
        }

        public synchronized boolean isStarted() {
            return wrappedBackgroundInitializer.isStarted();
        }

        public synchronized boolean start() {
            return wrappedBackgroundInitializer.start();
        }

        public Integer get() throws ConcurrentException {
            return wrappedBackgroundInitializer.get();
        }

        public synchronized Future<Integer> getFuture() {
            return wrappedBackgroundInitializer.getFuture();
        }

        public void close() throws Exception {
            wrappedBackgroundInitializer.close();
        }
    }
}
