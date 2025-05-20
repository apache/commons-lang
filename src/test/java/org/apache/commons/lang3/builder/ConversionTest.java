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
package org.apache.commons.lang3.builder;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.apache.commons.lang3.Conversion;
import org.junit.jupiter.api.Test;

class ConversionTest {

    @Test
    void testHexToByte() {
        assertEquals((byte) 0, Conversion.hexToByte("00", 0, (byte) 0, 0, 0));
        assertEquals((byte) 0, Conversion.hexToByte("00", 0, (byte) 0, 0, 2));
    }

    @Test
    void testHexToByte_IllegalArgument() {
        assertThrows(IllegalArgumentException.class, () -> Conversion.hexToByte("A0", 0, (byte) 0, 4, 2));
    }

    @Test
    void testHexToByte_nullString() {
        assertThrows(NullPointerException.class, () -> Conversion.hexToByte(null, 0, (byte) 0, 0, 2));
    }

}
