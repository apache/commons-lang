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

package org.apache.commons.lang3.concurrent;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;

class ThresholdCircuitBreakerTV04Test {
    @Test void remainsClosedAtThresholdAndOpensAboveThreshold() {
        ThresholdCircuitBreaker breaker = new ThresholdCircuitBreaker(10);
        assertTrue(breaker.checkState());
        assertTrue(breaker.incrementAndCheckState(4L));
        assertTrue(breaker.incrementAndCheckState(6L));
        assertFalse(breaker.incrementAndCheckState(1L));
        assertTrue(breaker.isOpen());
    }
    @Test void closeResetsCounterAndState() {
        ThresholdCircuitBreaker breaker = new ThresholdCircuitBreaker(5);
        assertFalse(breaker.incrementAndCheckState(6L));
        breaker.close();
        assertTrue(breaker.checkState());
        assertTrue(breaker.incrementAndCheckState(5L));
    }
    @Test void zeroThresholdIsPermanentlyOpenAndNegativeIncrementRejected() {
        ThresholdCircuitBreaker zero = new ThresholdCircuitBreaker(0);
        assertFalse(zero.incrementAndCheckState(0L));
        ThresholdCircuitBreaker breaker = new ThresholdCircuitBreaker(10);
        assertThrows(IllegalArgumentException.class, () -> breaker.incrementAndCheckState(-1L));
        assertTrue(breaker.checkState());
    }
    @Test void overflowSaturatesAndOpensBreaker() {
        ThresholdCircuitBreaker breaker = new ThresholdCircuitBreaker(Long.MAX_VALUE);
        assertTrue(breaker.incrementAndCheckState(Long.MAX_VALUE - 1));
        assertFalse(breaker.incrementAndCheckState(10L));
    }
}
