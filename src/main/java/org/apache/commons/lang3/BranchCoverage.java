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

    public static void register(final String id) {
        if (id != null) {
            ALL.add(id);
        }
    }

    /**
     * Marks an ID as executed.
     *
     * IMPORTANT: If the ID ends with "_true" or "_false", we automatically
     * register the opposite ID into ALL so that the not-taken side can show as [MISS].
     */
    public static void hit(final String id) {
        if (id == null) {
            return;
        }
        register(id);
        HIT.add(id);

        final String other = oppositeTrueFalse(id);
        if (other != null) {
            register(other);
        }
    }

    /**
     * Convenience helper: one call registers both and hits the taken side.
     * Example: BranchCoverage.branch("StringUtils.levThr:null", s == null);
     */
    public static void branch(final String baseId, final boolean condition) {
        if (baseId == null) {
            return;
        }
        final String t = baseId + "_true";
        final String f = baseId + "_false";
        register(t);
        register(f);
        hit(condition ? t : f);
    }

    private static String oppositeTrueFalse(final String id) {
        if (id.endsWith("_true")) {
            return id.substring(0, id.length() - "_true".length()) + "_false";
        }
        if (id.endsWith("_false")) {
            return id.substring(0, id.length() - "_false".length()) + "_true";
        }
        return null;
    }

    public static void report() {
        final TreeSet<String> all = new TreeSet<>(ALL);
        int hit = 0;

        System.out.println("\n=== DIY BRANCH COVERAGE ===");

        final java.io.File out = new java.io.File("target", "diy-coverage.txt");
        out.getParentFile().mkdirs();

        try (java.io.PrintWriter pw = new java.io.PrintWriter(new java.io.FileWriter(out, false))) {
            pw.println("=== DIY BRANCH COVERAGE ===");
            for (final String id : all) {
                final boolean ok = HIT.contains(id);
                if (ok) {
                    hit++;
                }
                final String line = (ok ? "[HIT ] " : "[MISS] ") + id;
                System.out.println(line);
                pw.println(line);
            }
            final String summary = String.format("Hit %d/%d (%.1f%%)",
                    hit, all.size(), all.isEmpty() ? 100.0 : (hit * 100.0 / all.size()));
            System.out.println(summary);
            pw.println(summary);
        } catch (final java.io.IOException e) {
            throw new RuntimeException(e);
        }
    }
}
