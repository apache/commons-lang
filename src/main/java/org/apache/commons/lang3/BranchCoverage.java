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

package org.apache.commons.lang3;

import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;

public final class BranchCoverage {
    private static final Set<String> ALL = ConcurrentHashMap.newKeySet();
    private static final Set<String> HIT = ConcurrentHashMap.newKeySet();

    static {
        Runtime.getRuntime().addShutdownHook(new Thread(BranchCoverage::report));
    }

    private BranchCoverage() {}

    public static void hit(final String id) {
        ALL.add(id);
        HIT.add(id);
    }

    public static void report() {
        final var all = new TreeSet<>(ALL);
        int hit = 0;
        System.out.println("\n=== DIY BRANCH COVERAGE ===");
        for (final String id : all) {
            final boolean ok = HIT.contains(id);
            if (ok) hit++;
            System.out.println((ok ? "[HIT ] " : "[MISS] ") + id);
        }
        System.out.printf("Hit %d/%d (%.1f%%)%n", hit, all.size(),
                all.isEmpty() ? 100.0 : (hit * 100.0 / all.size()));
    }
}
