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

import static org.apache.commons.lang3.LangAssertions.assertIllegalArgumentException;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

/**
 * Tests {@link ConcurrentException}.
 */
class ConcurrentExceptionTest {

    /**
     * Tests creating a ConcurrentException with an error as cause.
     */
    @Test
    void testCauseError() {
        assertIllegalArgumentException(() -> new ConcurrentException("An error", new Error()));
    }

    /**
     * Tests creating a ConcurrentException with null as cause.
     */
    @Test
    void testCauseNull() {
        assertIllegalArgumentException(() -> new ConcurrentException((Throwable) null));
    }

    @Test
    void testCauseString() {
        assertEquals("test", new ConcurrentException("test").getMessage());
        assertNull(new ConcurrentException((String) null).getMessage());
    }

    /**
     * Tests creating a ConcurrentException with a runtime exception as cause.
     */
    @Test
    void testCauseUnchecked() {
        assertIllegalArgumentException(() -> new ConcurrentException(new RuntimeException()));
    }
}
