/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.commons.lang3;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import java.util.Objects;
import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests {@link CharRange}.hashCode method:
 * 1. Verify compliance with the hashCode contract (if equals() returns true, hashCode must be equal)
 * 2. Compare execution efficiency between Objects.hash version and bitwise operation version
 */
@TestInstance(TestInstance.Lifecycle.PER_CLASS) // Share test instances globally
public class CharRangeHashCodeTest {

    // ========== Constants for testing ==========
    // Number of test executions (the larger the value, the more accurate; recommended at least 100 million times)
    private static final long TEST_COUNT = 100_000_000L;
    // CharRange instances for testing (covering multiple scenarios)
    private CharRange normalRange;    // Normal range: a-z
    private CharRange negatedRange;   // Negated range: 0-9
    private CharRange singleCharRange;// Single character range: x-x
    private CharRange extremeRange;   // Extreme value range: near Character.MAX_VALUE

    // ========== Initialize test data (executed only once) ==========
    @BeforeAll
    void initTestData() {
        normalRange = new CharRange('a', 'z', false);
        negatedRange = new CharRange('0', '9', true);
        singleCharRange = new CharRange('x', 'x', false);
        extremeRange = new CharRange(Character.MAX_VALUE, (char) (Character.MAX_VALUE - 100), false);
        // JVM warm-up: Eliminate JIT compilation interference in first execution
        warmUpJvm();
    }

    /**
     * JVM warm-up: Eliminate JIT compilation interference from first-time execution
     */
    private void warmUpJvm() {
        for (long i = 0; i < 1_000_000; i++) {
            normalRange.hashCodeObjects();
            normalRange.hashCodeBitwise();
            negatedRange.hashCodeObjects();
            negatedRange.hashCodeBitwise();
            singleCharRange.hashCodeObjects();
            singleCharRange.hashCodeBitwise();
            extremeRange.hashCodeObjects();
            extremeRange.hashCodeBitwise();
        }
    }

    // ========== Test 1: Verify hashCode contract compliance ==========
    @Test
    void testHashCode_ContractCompliance() {
        // Scenario 1: If equals() returns true → hashCode must be equal
        CharRange range1 = new CharRange('a', 'z', false);
        CharRange range2 = new CharRange('a', 'z', false);
        assertTrue(range1.equals(range2));
        assertEquals(range1.hashCodeObjects(), range2.hashCodeObjects());
        assertEquals(range1.hashCodeBitwise(), range2.hashCodeBitwise());

        // Scenario 2: If equals() returns false → hashCode is mostly unequal (non-mandatory, only verify dispersion)
        CharRange range3 = new CharRange('a', 'z', true);
        assertFalse(range1.equals(range3));
        // Assert hashCodes of the two implementations are unequal (probabilistic, ignore if accidental collision occurs)
        assertNotEquals(range1.hashCodeObjects(), range3.hashCodeObjects(), "Hash collision occurred in Objects.hash version (probabilistic)");
        assertNotEquals(range1.hashCodeBitwise(), range3.hashCodeBitwise(), "Hash collision occurred in bitwise version (extreme scenario)");

        // Scenario 3: Verify swap logic when start > end
        CharRange range4 = new CharRange('z', 'a', false); // start > end, automatically swapped to a-z
        assertTrue(range1.equals(range4));
        assertEquals(range1.hashCodeObjects(), range4.hashCodeObjects());
        assertEquals(range1.hashCodeBitwise(), range4.hashCodeBitwise());
    }

    // ========== Test 2: Compare efficiency of two hashCode implementations ==========
    @Test
    void testHashCode_PerformanceComparison() {
        // ---------- Test Objects.hash version ----------
        long startTimeObjects = System.nanoTime();
        long sumObjects = 0; // Accumulate results to avoid JIT dead code elimination
        for (long i = 0; i < TEST_COUNT; i++) {
            sumObjects += normalRange.hashCodeObjects();
            sumObjects += negatedRange.hashCodeObjects();
            sumObjects += singleCharRange.hashCodeObjects();
            sumObjects += extremeRange.hashCodeObjects();
        }
        long endTimeObjects = System.nanoTime();
        long costTimeObjects = (endTimeObjects - startTimeObjects) / 1_000_000; // Convert to milliseconds

        // ---------- Test bitwise operation version ----------
        long startTimeBitwise = System.nanoTime();
        long sumBitwise = 0;
        for (long i = 0; i < TEST_COUNT; i++) {
            sumBitwise += normalRange.hashCodeBitwise();
            sumBitwise += negatedRange.hashCodeBitwise();
            sumBitwise += singleCharRange.hashCodeBitwise();
            sumBitwise += extremeRange.hashCodeBitwise();
        }
        long endTimeBitwise = System.nanoTime();
        long costTimeBitwise = (endTimeBitwise - startTimeBitwise) / 1_000_000;

        // ---------- Output comparison results ----------
        System.out.println("===== CharRange hashCode Efficiency Comparison (Execution count: " + TEST_COUNT + " iterations/scenario) =====");
        System.out.println("Total time for Objects.hash version: " + costTimeObjects + " ms, accumulated result: " + sumObjects);
        System.out.println("Total time for bitwise operation version: " + costTimeBitwise + " ms, accumulated result: " + sumBitwise);
        // Calculate efficiency improvement ratio (avoid division by zero)
        double efficiencyImprovement = costTimeObjects == 0 ? 0 :
                (double) (costTimeObjects - costTimeBitwise) / costTimeObjects * 100;
        System.out.println("Efficiency improvement of bitwise version over Objects.hash version: " + String.format("%.2f%%", efficiencyImprovement));

        // ---------- Assertion: Bitwise version must be faster (core verification) ----------
        assertTrue(costTimeBitwise < costTimeObjects,
                "The bitwise version of hashCode should be more efficient than the Objects.hash version!");
    }

    // ========== Inner class: CharRange implementation (consistent with original class) ==========
    // Original class: org.apache.commons.lang3.CharRange (Apache Commons Lang official implementation)
    private static class CharRange {
        private final char start;
        private final char end;
        private final boolean negated;

        public CharRange(char start, char end, final boolean negated) {
            if (start > end) {
                final char temp = start;
                start = end;
                end = temp;
            }

            this.start = start;
            this.end = end;
            this.negated = negated;
        }

        /**
         * Option 1: Objects.hash version of hashCode
         */
        public int hashCodeObjects() {
            return Objects.hash(end, negated, start);
        }

        /**
         * Option 2: Bitwise operation version of hashCode
         */
        public int hashCodeBitwise() {
            final int charCombined = (start << 16) | (end & 0xFFFF);
            return charCombined ^ (negated ? 0x00010000 : 0);
        }

        /**
         * Override equals method (ensure contract integrity)
         */
        @Override
        public boolean equals(final Object obj) {
            if (obj == this) {
                return true;
            }
            if (!(obj instanceof CharRange)) {
                return false;
            }
            final CharRange other = (CharRange) obj;
            return start == other.start && end == other.end && negated == other.negated;
        }
    }
}