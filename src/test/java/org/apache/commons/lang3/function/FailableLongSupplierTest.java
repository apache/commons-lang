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

package org.apache.commons.lang3.function;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.IOException;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link FailableLongSupplier}.
 */
class FailableLongSupplierTest extends AbstractLangTest {

    @Test
    void testGetAsLong_returnsValue() throws IOException {
        final FailableLongSupplier<IOException> supplier = () -> 123L;
        assertEquals(123L, supplier.getAsLong());
    }

    @Test
    void testGetAsLong_throwsException() {
        final IOException expected = new IOException("fail");
        final FailableLongSupplier<IOException> supplier = () -> {
            throw expected;
        };
        final IOException thrown = assertThrows(IOException.class, supplier::getAsLong);
        assertEquals(expected, thrown);
    }
}
