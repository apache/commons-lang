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

import java.util.concurrent.Callable;
import java.util.concurrent.FutureTask;

/**
 * Consists of utility methods that work with {@link FutureTask}.
 *
 * @since 3.13.0
 */
public class FutureTasks {

    private FutureTasks() {
        // No instances needed.
    }

    /**
     * Creates a {@link FutureTask} and runs the given {@link Callable}.
     *
     * @param <V> The result type returned by this FutureTask's {@code get} methods.
     * @param callable the Callable task.
     * @return a new FutureTask.
     */
    public static <V> FutureTask<V> run(final Callable<V> callable) {
        final FutureTask<V> futureTask = new FutureTask<>(callable);
        futureTask.run();
        return futureTask;
    }
}
