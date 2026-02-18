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
