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

import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * A {@link ThreadFactory} that produces daemon threads.
 *
 * @since 3.10
 */
public class DaemonThreadFactory implements ThreadFactory {

    private static final String FORMAT = "%s-%s";
    private final String namePrefix;
    private final AtomicInteger threadCount = new AtomicInteger(0);

    public DaemonThreadFactory() {
        this(DaemonThreadFactory.class.getSimpleName());
    }

    public DaemonThreadFactory(final String prefix) {
        this.namePrefix = prefix;
    }

    @Override
    public Thread newThread(final Runnable runnable) {
        if (runnable == null) {
            return null;
        }
        final String name = String.format(FORMAT, this.namePrefix, this.threadCount.incrementAndGet());
        final Thread thread = new Thread(runnable, name);
        thread.setDaemon(true);
        if (thread.getPriority() != Thread.NORM_PRIORITY) {
            thread.setPriority(Thread.NORM_PRIORITY);
        }
        return thread;
    }
}
