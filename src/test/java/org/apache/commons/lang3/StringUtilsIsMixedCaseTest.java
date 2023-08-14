package org.apache.commons.lang3;

import org.openjdk.jmh.annotations.*;

import java.util.concurrent.TimeUnit;

import static org.apache.commons.lang3.StringUtils.isEmpty;

/**
 * Test to show the better performance of the new implementation of isMixedCase
 */
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
@Warmup(iterations = 5, time = 10)
public class StringUtilsIsMixedCaseTest {

    public static final String LOWER_CASE_LETTERS = "abcdefghijklmnopqrstuvwxyz";
    public static final String END_MATCH = "at the enD";
    public static final String Middle_MATCH = "at tHe Mid";
    public static final String EARLY_MATCH = "At tHe beginning";

    @Benchmark
    public boolean newIsMixedCaseNoneMatch() {
        return StringUtils.isMixedCase(LOWER_CASE_LETTERS);
    }

    @Benchmark
    public boolean oldIsMixedCaseNoneMatch() {
        return oldIsMixedCase(LOWER_CASE_LETTERS);
    }

    @Benchmark
    public boolean newIsMixedCaseEndMatch() {
        return StringUtils.isMixedCase(END_MATCH);
    }

    @Benchmark
    public boolean oldIsMixedCaseEndMatch() {
        return oldIsMixedCase(END_MATCH);
    }

    @Benchmark
    public boolean newIsMixedCaseMiddleMatch() {
        return StringUtils.isMixedCase(Middle_MATCH);
    }

    @Benchmark
    public boolean oldIsMixedCaseMiddleMatch() {
        return oldIsMixedCase(Middle_MATCH);
    }

    @Benchmark
    public boolean newIsMixedCaseBeginningMatch() {
        return StringUtils.isMixedCase(EARLY_MATCH);
    }

    @Benchmark
    public boolean oldIsMixedCaseBeginningMatch() {
        return oldIsMixedCase(EARLY_MATCH);
    }

    public static boolean oldIsMixedCase(final CharSequence cs) {
        if (isEmpty(cs) || cs.length() == 1) {
            return false;
        }
        boolean containsUppercase = false;
        boolean containsLowercase = false;
        final int sz = cs.length();
        for (int i = 0; i < sz; i++) {
            if (containsUppercase && containsLowercase) {
                return true;
            }
            if (Character.isUpperCase(cs.charAt(i))) {
                containsUppercase = true;
            } else if (Character.isLowerCase(cs.charAt(i))) {
                containsLowercase = true;
            }
        }
        return containsUppercase && containsLowercase;
    }
}
