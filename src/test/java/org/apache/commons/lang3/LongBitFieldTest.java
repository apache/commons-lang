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

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

public class LongBitFieldTest {

    @Test
    public void testGetValueAndRawValue() {
        LongBitField field = new LongBitField(0xFF00L); // bits 8-15
        long holder = 0x1234L;

        // raw value: bits & mask
        assertEquals(0x1200L, field.getRawValue(holder));
        // shifted value: shifted right to LSB
        assertEquals(0x12L, field.getValue(holder));
    }

    @Test
    public void testSetValue() {
        LongBitField field = new LongBitField(0xFF00L); // bits 8-15
        long holder = 0x1200L;
        long result = field.setValue(holder, 0x34L); // replace bits 8-15 with 0x34
        assertEquals(0x3400L, result);
    }

    @Test
    public void testSet() {
        LongBitField field = new LongBitField(0x1000L); // bit 12
        long holder = 0x0000L;
        long result = field.set(holder);
        assertEquals(0x1000L, result);
    }

    @Test
    public void testSetBoolean() {
        LongBitField field = new LongBitField(0x1000L); // bit 12
        long holder = 0x0000L;

        long setTrue = field.setBoolean(holder, true);
        assertEquals(0x1000L, setTrue);

        long setFalse = field.setBoolean(setTrue, false);
        assertEquals(0x0000L, setFalse);
    }

    @Test
    public void testIsSetAndIsAllSet() {
        LongBitField field1 = new LongBitField(0x3000L); // bits 12-13
        long holder = 0x3000L;
        assertTrue(field1.isSet(holder));
        assertTrue(field1.isAllSet(holder));

        long holderPartial = 0x1000L;
        assertTrue(field1.isSet(holderPartial));
        assertFalse(field1.isAllSet(holderPartial));
    }

    @Test
    public void testEdgeCases() {
        LongBitField field = new LongBitField(0x1L); // bit 0
        assertEquals(1L, field.set(0L));
        assertEquals(0L, field.clear(1L));

        LongBitField highField = new LongBitField(0x8000000000000000L); // highest bit
        assertEquals(0x8000000000000000L, highField.set(0L));
        assertEquals(0L, highField.clear(0x8000000000000000L));
    }

    @Test
    public void testMultipleBits() {
        LongBitField field = new LongBitField(0xF0F0L); // multiple bits
        long holder = 0xAAAA;
        long newValue = 0x55;
        long result = field.setValue(holder, newValue);
        assertEquals((holder & ~0xF0F0L) | (newValue << 4 & 0xF0F0L), result);
    }

}
