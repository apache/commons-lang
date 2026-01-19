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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

/**
 * Tests {@link BitField} constructed with long masks.
 */
class BitFieldLongTest {

    private static final BitField BF_MULTI  = new BitField(0x3F80L);
    private static final BitField BF_MULTI_L  = new BitField(0x3F80_0000_0000_0000L);
    private static final BitField BF_SINGLE = new BitField(0x4000L);
    private static final BitField BF_SINGLE_L = new BitField(0x4000_0000_0000_0000L);
    private static final BitField BF_ZERO = new BitField(0L);
    private static final BitField BF_ZERO_L = new BitField(0L);

    @Test
    void testByteBoolean() {
        assertEquals(0, new BitField(0L).setByteBoolean((byte) 0, true));
        assertEquals(1, new BitField(1L).setByteBoolean((byte) 0, true));
        assertEquals(2, new BitField(2L).setByteBoolean((byte) 0, true));
        assertEquals(4, new BitField(4L).setByteBoolean((byte) 0, true));
        assertEquals(8, new BitField(8L).setByteBoolean((byte) 0, true));
        assertEquals(16, new BitField(16L).setByteBoolean((byte) 0, true));
        assertEquals(32, new BitField(32L).setByteBoolean((byte) 0, true));
        assertEquals(64, new BitField(64L).setByteBoolean((byte) 0, true));
        assertEquals(-128, new BitField(128L).setByteBoolean((byte) 0, true));
        assertEquals(1, new BitField(0L).setByteBoolean((byte) 1, false));
        assertEquals(0, new BitField(1L).setByteBoolean((byte) 1, false));
        assertEquals(0, new BitField(2L).setByteBoolean((byte) 2, false));
        assertEquals(0, new BitField(4L).setByteBoolean((byte) 4, false));
        assertEquals(0, new BitField(8L).setByteBoolean((byte) 8, false));
        assertEquals(0, new BitField(16L).setByteBoolean((byte) 16, false));
        assertEquals(0, new BitField(32L).setByteBoolean((byte) 32, false));
        assertEquals(0, new BitField(64L).setByteBoolean((byte) 64, false));
        assertEquals(0, new BitField(128L).setByteBoolean((byte) 128, false));
        assertEquals(-2, new BitField(1L).setByteBoolean((byte) 255, false));
        final byte clearedBit = new BitField(0x40L).setByteBoolean((byte) - 63, false);
        assertFalse(new BitField(0x40).isSet(clearedBit));
    }

    /**
     * Tests the {@link BitField#clear()} method.
     */
    @Test
    void testClearInt() {
        assertEquals(BF_MULTI.clear(-1), 0xFFFF_C07F);
        assertEquals(BF_SINGLE.clear(-1), 0xFFFF_BFFF);
        assertEquals(BF_ZERO.clear(-1), 0xFFFF_FFFF);
        assertEquals(BF_MULTI_L.clear(-1), 0xFFFF_FFFF);
        assertEquals(BF_SINGLE_L.clear(-1), 0xFFFF_FFFF);
        assertEquals(BF_ZERO_L.clear(-1), 0xFFFF_FFFF);
    }

    /**
     * Tests the {@link BitField#clear()} method.
     */
    @Test
    void testClearLong() {
        assertEquals(BF_MULTI.clear(-1L), 0xFFFF_FFFF_FFFF_C07FL);
        assertEquals(BF_SINGLE.clear(-1L), 0xFFFF_FFFF_FFFF_BFFFL);
        assertEquals(BF_ZERO.clear(-1L), 0xFFFF_FFFF);
        assertEquals(BF_MULTI_L.clear(-1L), 0xC07F_FFFF_FFFF_FFFFL);
        assertEquals(BF_SINGLE_L.clear(-1L), 0xBFFF_FFFF_FFFF_FFFFL);
        assertEquals(BF_ZERO_L.clear(-1L), 0xFFFF_FFFF_FFFF_FFFFL);
    }

    /**
     * Tests the {@link BitField#clearShort()} method.
     */
    @Test
    void testClearShort() {
        assertEquals(BF_MULTI.clearShort((short) - 1), (short) 0xC07F);
        assertEquals(BF_SINGLE.clearShort((short) - 1), (short) 0xBFFF);
        assertEquals(BF_ZERO.clearShort((short) -1), (short) 0xFFFF);
        assertEquals(BF_MULTI_L.clearShort((short) - 1), (short) 0xFFFF);
        assertEquals(BF_SINGLE_L.clearShort((short) - 1), (short) 0xFFFF);
        assertEquals(BF_ZERO_L.clearShort((short) -1), (short) 0xFFFF);
    }

    @Test
    void testEdgeCases() {
        final BitField field = new BitField(0x1L); // bit 0
        assertEquals(1L, field.set(0L));
        assertEquals(0L, field.clear(1L));
        final BitField highField = new BitField(0x8000_0000_0000_0000L); // highest bit
        assertEquals(0x8000_0000_0000_0000L, highField.set(0L));
        assertEquals(0L, highField.clear(0x8000_0000_0000_0000L));
    }

    /**
     * Tests the {@link BitField#getRawValue()} method.
     */
    @Test
    void testGetRawValue() {
        // mask < max int and int input
        assertEquals(BF_MULTI.getRawValue(-1), 0x3F80);
        assertEquals(BF_MULTI.getRawValue(0), 0);
        assertEquals(BF_SINGLE.getRawValue(-1), 0x4000);
        assertEquals(BF_SINGLE.getRawValue(0), 0);
        assertEquals(BF_ZERO.getRawValue(-1), 0);
        assertEquals(BF_ZERO.getRawValue(0), 0);
        // mask > max int and int input
        assertEquals(BF_MULTI_L.getRawValue(-1), 0);
        assertEquals(BF_MULTI_L.getRawValue(0), 0);
        assertEquals(BF_SINGLE_L.getRawValue(-1), 0);
        assertEquals(BF_SINGLE_L.getRawValue(0), 0);
        assertEquals(BF_ZERO_L.getRawValue(-1), 0);
        assertEquals(BF_ZERO_L.getRawValue(0), 0);
        // mask > max int and long input
        assertEquals(BF_MULTI_L.getRawValue(-1L), 0x3F80_0000_0000_0000L);
        assertEquals(BF_MULTI_L.getRawValue(0L), 0);
        assertEquals(BF_SINGLE_L.getRawValue(-1L), 0x4000_0000_0000_0000L);
        assertEquals(BF_SINGLE_L.getRawValue(0L), 0);
        assertEquals(BF_ZERO_L.getRawValue(-1L), 0);
        assertEquals(BF_ZERO_L.getRawValue(0L), 0);
    }

    /**
     * Tests the {@link BitField#getShortRawValue()} method.
     */
    @Test
    void testGetShortRawValue() {
        // mask < max int and short input
        assertEquals(BF_MULTI.getShortRawValue((short) - 1), (short) 0x3F80);
        assertEquals(BF_MULTI.getShortRawValue((short) 0), (short) 0);
        assertEquals(BF_SINGLE.getShortRawValue((short) - 1), (short) 0x4000);
        assertEquals(BF_SINGLE.getShortRawValue((short) 0), (short) 0);
        assertEquals(BF_ZERO.getShortRawValue((short) -1), (short) 0);
        assertEquals(BF_ZERO.getShortRawValue((short) 0), (short) 0);
        // mask > max int and short input
        assertEquals(BF_MULTI_L.getShortRawValue((short) - 1), (short) 0);
        assertEquals(BF_MULTI_L.getShortRawValue((short) 0), (short) 0);
        assertEquals(BF_SINGLE_L.getShortRawValue((short) - 1), (short) 0);
        assertEquals(BF_SINGLE_L.getShortRawValue((short) 0), (short) 0);
        assertEquals(BF_ZERO_L.getShortRawValue((short) -1), (short) 0);
        assertEquals(BF_ZERO_L.getShortRawValue((short) 0), (short) 0);
    }

    /**
     * Tests the getRawValue() and getValue() methods
     */
    @Test
    void testGetValueAndRawValueIntRange() {
        final BitField field = new BitField(0xFF00L); // bits 8-15
        final long holder = 0x1234L;
        // raw value: bits & mask
        assertEquals(0x1200L, field.getRawValue(holder));
        // shifted value: shifted right to LSB
        assertEquals(0x12L, field.getValue(holder));
    }

    /**
     * Tests the isSet() and isAllSet() methods
     */
    @Test
    void testIsSetAndIsAllSet() {
        final BitField field1 = new BitField(0x3000L); // bits 12-13
        final long holder = 0x3000L;
        assertTrue(field1.isSet(holder));
        assertTrue(field1.isAllSet(holder));
        final long holderPartial = 0x1000L;
        assertTrue(field1.isSet(holderPartial));
        assertFalse(field1.isAllSet(holderPartial));
    }

    @Test
    void testMultipleBits() {
        final BitField field = new BitField(0xF0F0L); // multiple bits
        final long holder = 0xAAAA;
        final long newValue = 0x55;
        final long result = field.setValue(holder, newValue);
        assertEquals(holder & ~0xF0F0L | newValue << 4 & 0xF0F0L, result);
    }

    /**
     * Tests the set() method
     */
    @Test
    void testSet() {
        final BitField field = new BitField(0x1000L); // bit 12
        final long holder = 0x0000L;
        final long result = field.set(holder);
        assertEquals(0x1000L, result);
    }

    /**
     * Tests the setBoolean() method
     */
    @Test
    void testSetBoolean() {
        final BitField field = new BitField(0x1000L); // bit 12
        final long holder = 0x0000L;
        final long setTrue = field.setBoolean(holder, true);
        assertEquals(0x1000L, setTrue);
        final long setFalse = field.setBoolean(setTrue, false);
        assertEquals(0x0000L, setFalse);
    }

    /**
     * Tests the setValue() method
     */
    @Test
    void testSetValue() {
        final BitField field = new BitField(0xFF00L); // bits 8-15
        final long holder = 0x1200L;
        final long result = field.setValue(holder, 0x34L); // replace bits 8-15 with 0x34
        assertEquals(0x3400L, result);
    }
}
