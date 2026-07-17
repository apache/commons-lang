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
 * Tests {@link BitField} constructed with int masks.
 */
class BitFieldTest extends AbstractLangTest {

    private static final BitField BF_MULTI = new BitField(0x3F80);

    private static final BitField BF_SINGLE = new BitField(0x4000);

    private static final BitField BF_ZERO = new BitField(0);

    @Test
    void testByteBoolean() {
        assertEquals(0, new BitField(0).setByteBoolean((byte) 0, true));
        assertEquals(1, new BitField(1).setByteBoolean((byte) 0, true));
        assertEquals(2, new BitField(2).setByteBoolean((byte) 0, true));
        assertEquals(4, new BitField(4).setByteBoolean((byte) 0, true));
        assertEquals(8, new BitField(8).setByteBoolean((byte) 0, true));
        assertEquals(16, new BitField(16).setByteBoolean((byte) 0, true));
        assertEquals(32, new BitField(32).setByteBoolean((byte) 0, true));
        assertEquals(64, new BitField(64).setByteBoolean((byte) 0, true));
        assertEquals(-128, new BitField(128).setByteBoolean((byte) 0, true));
        assertEquals(1, new BitField(0).setByteBoolean((byte) 1, false));
        assertEquals(0, new BitField(1).setByteBoolean((byte) 1, false));
        assertEquals(0, new BitField(2).setByteBoolean((byte) 2, false));
        assertEquals(0, new BitField(4).setByteBoolean((byte) 4, false));
        assertEquals(0, new BitField(8).setByteBoolean((byte) 8, false));
        assertEquals(0, new BitField(16).setByteBoolean((byte) 16, false));
        assertEquals(0, new BitField(32).setByteBoolean((byte) 32, false));
        assertEquals(0, new BitField(64).setByteBoolean((byte) 64, false));
        assertEquals(0, new BitField(128).setByteBoolean((byte) 128, false));
        assertEquals(-2, new BitField(1).setByteBoolean((byte) 255, false));
        final byte clearedBit = new BitField(0x40).setByteBoolean((byte) -63, false);
        assertFalse(new BitField(0x40).isSet(clearedBit));
    }

    /**
     * Tests the {@link BitField#clear()} method.
     */
    @Test
    void testClearInt() {
        assertEquals(BF_MULTI.clear(-1), 0xFFFFC07F);
        assertEquals(BF_SINGLE.clear(-1), 0xFFFFBFFF);
        assertEquals(BF_ZERO.clear(-1), 0xFFFFFFFF);
    }

    /**
     * Tests the {@link BitField#clear()} method.
     */
    @Test
    void testClearLong() {
        assertEquals(BF_MULTI.clear(-1L), 0xFFFFC07F);
        assertEquals(BF_SINGLE.clear(-1L), 0xFFFFBFFF);
        assertEquals(BF_ZERO.clear(-1L), 0xFFFFFFFF);
    }

    /**
     * Tests the {@link BitField#clearShort()} method.
     */
    @Test
    void testClearShort() {
        assertEquals(BF_MULTI.clearShort((short) -1), (short) 0xC07F);
        assertEquals(BF_SINGLE.clearShort((short) -1), (short) 0xBFFF);
        assertEquals(BF_ZERO.clearShort((short) -1), (short) 0xFFFF);
    }

    /**
     * Tests the {@link BitField#getRawValue()} method.
     */
    @Test
    void testGetRawValue() {
        assertEquals(BF_MULTI.getRawValue(-1), 0x3F80);
        assertEquals(BF_MULTI.getRawValue(0), 0);
        assertEquals(BF_SINGLE.getRawValue(-1), 0x4000);
        assertEquals(BF_SINGLE.getRawValue(0), 0);
        assertEquals(BF_ZERO.getRawValue(-1), 0);
        assertEquals(BF_ZERO.getRawValue(0), 0);
    }

    /**
     * Tests the {@link BitField#getShortRawValue()} method.
     */
    @Test
    void testGetShortRawValue() {
        assertEquals(BF_MULTI.getShortRawValue((short) -1), (short) 0x3F80);
        assertEquals(BF_MULTI.getShortRawValue((short) 0), (short) 0);
        assertEquals(BF_SINGLE.getShortRawValue((short) -1), (short) 0x4000);
        assertEquals(BF_SINGLE.getShortRawValue((short) 0), (short) 0);
        assertEquals(BF_ZERO.getShortRawValue((short) -1), (short) 0);
        assertEquals(BF_ZERO.getShortRawValue((short) 0), (short) 0);
    }

    /**
     * Tests the {@link BitField#getShortValue()} method.
     */
    @Test
    void testGetShortValue() {
        assertEquals(BF_MULTI.getShortValue((short) -1), (short) 127);
        assertEquals(BF_MULTI.getShortValue((short) 0), (short) 0);
        assertEquals(BF_SINGLE.getShortValue((short) -1), (short) 1);
        assertEquals(BF_SINGLE.getShortValue((short) 0), (short) 0);
        assertEquals(BF_ZERO.getShortValue((short) -1), (short) 0);
        assertEquals(BF_ZERO.getShortValue((short) 0), (short) 0);
    }

    /**
     * Tests the {@link BitField#getValue()} method.
     */
    @Test
    void testGetValue() {
        assertEquals(BF_MULTI.getValue(-1), 127);
        assertEquals(BF_MULTI.getValue(0), 0);
        assertEquals(BF_SINGLE.getValue(-1), 1);
        assertEquals(BF_SINGLE.getValue(0), 0);
        assertEquals(BF_ZERO.getValue(-1), 0);
        assertEquals(BF_ZERO.getValue(0), 0);
    }

    /**
     * Tests that {@link BitField#getValue(int)} and {@link BitField#getValue(long)} shift the selected bits right without sign extension when the field occupies
     * the top bit of the holder (bit 31 for int, bit 63 for long).
     */
    @Test
    void testGetValueTopBit() {
        final BitField bit31 = new BitField(0x80000000);
        assertEquals(bit31.getValue(-1), 1);
        assertEquals(bit31.getValue(0), 0);
        final BitField topByte = new BitField(0xFF000000);
        assertEquals(topByte.getValue(0xFF000000), 255);
        // The int and long overloads must agree for the same field and holder.
        assertEquals(topByte.getValue(0xFF000000L), 255L);
        // Verify the int and long overloads agree for the same field and holder.
        assertEquals(topByte.getValue(0xFF000000L), (long) topByte.getValue(0xFF000000));        
        final BitField bit63 = new BitField(0x8000000000000000L);
        assertEquals(bit63.getValue(0x8000000000000000L), 1L);
        assertEquals(bit63.getValue(0L), 0L);
        final BitField topNibble = new BitField(0xF000000000000000L);
        assertEquals(topNibble.getValue(topNibble.setValue(0L, 15L)), 15L);
    }

    /**
     * Tests that an int mask with the high bit set is treated as 32 unsigned bits on the long methods, instead of being sign-extended into bits 32-63.
     */
    @Test
    void testIntMaskHighBitOnLongHolder() {
        final BitField bit31 = new BitField(0x80000000);
        assertEquals(bit31.getRawValue(-1L), 0x80000000L);
        assertEquals(bit31.getValue(-1L), 1L);
        assertEquals(bit31.clear(-1L), 0xFFFFFFFF7FFFFFFFL);
        assertEquals(bit31.set(0L), 0x80000000L);
        assertTrue(bit31.isSet(0x80000000L));
        assertTrue(bit31.isAllSet(0x80000000L));
        final BitField topByte = new BitField(0xFF000000);
        assertEquals(topByte.getRawValue(-1L), 0xFF000000L);
        assertEquals(topByte.getValue(-1L), 0xFFL);
        assertEquals(topByte.clear(-1L), 0xFFFFFFFF00FFFFFFL);
    }

    /**
     * Tests the {@link BitField#isAllSet()} method.
     */
    @Test
    void testIsAllSet() {
        for (int j = 0; j < 0x3F80; j += 0x80) {
            assertFalse(BF_MULTI.isAllSet(j));
            assertTrue(BF_ZERO.isAllSet(j));
        }
        assertTrue(BF_MULTI.isAllSet(0x3F80));
        assertFalse(BF_SINGLE.isAllSet(0));
        assertTrue(BF_SINGLE.isAllSet(0x4000));
    }

    /**
     * test the isSet() method.
     */
    @Test
    void testIsSet() {
        assertFalse(BF_MULTI.isSet(0));
        assertFalse(BF_ZERO.isSet(0));
        for (int j = 0x80; j <= 0x3F80; j += 0x80) {
            assertTrue(BF_MULTI.isSet(j));
        }
        for (int j = 0x80; j <= 0x3F80; j += 0x80) {
            assertFalse(BF_ZERO.isSet(j));
        }
        assertFalse(BF_SINGLE.isSet(0));
        assertTrue(BF_SINGLE.isSet(0x4000));
    }

    /**
     * Tests the {@link BitField#set()} method.
     */
    @Test
    void testSet() {
        assertEquals(BF_MULTI.set(0), 0x3F80);
        assertEquals(BF_SINGLE.set(0), 0x4000);
        assertEquals(BF_ZERO.set(0), 0);
    }

    /**
     * Tests the {@link BitField#setBoolean()} method.
     */
    @Test
    void testSetBoolean() {
        assertEquals(BF_MULTI.set(0), BF_MULTI.setBoolean(0, true));
        assertEquals(BF_SINGLE.set(0), BF_SINGLE.setBoolean(0, true));
        assertEquals(BF_ZERO.set(0), BF_ZERO.setBoolean(0, true));
        assertEquals(BF_MULTI.clear(-1), BF_MULTI.setBoolean(-1, false));
        assertEquals(BF_SINGLE.clear(-1), BF_SINGLE.setBoolean(-1, false));
        assertEquals(BF_ZERO.clear(-1), BF_ZERO.setBoolean(-1, false));
    }

    /**
     * Tests the {@link BitField#setShort()} method.
     */
    @Test
    void testSetShort() {
        assertEquals(BF_MULTI.setShort((short) 0), (short) 0x3F80);
        assertEquals(BF_SINGLE.setShort((short) 0), (short) 0x4000);
        assertEquals(BF_ZERO.setShort((short) 0), (short) 0);
    }

    /**
     * test the setShortBoolean() method
     */
    @Test
    void testSetShortBoolean() {
        assertEquals(BF_MULTI.setShort((short) 0), BF_MULTI.setShortBoolean((short) 0, true));
        assertEquals(BF_SINGLE.setShort((short) 0), BF_SINGLE.setShortBoolean((short) 0, true));
        assertEquals(BF_ZERO.setShort((short) 0), BF_ZERO.setShortBoolean((short) 0, true));
        assertEquals(BF_MULTI.clearShort((short) -1), BF_MULTI.setShortBoolean((short) -1, false));
        assertEquals(BF_SINGLE.clearShort((short) -1), BF_SINGLE.setShortBoolean((short) -1, false));
        assertEquals(BF_ZERO.clearShort((short) -1), BF_ZERO.setShortBoolean((short) -1, false));
    }

    /**
     * test the setShortValue() method
     */
    @Test
    void testSetShortValue() {
        final int limit = 128;
        for (int j = 0; j < limit; j++) {
            assertEquals(BF_MULTI.getShortValue(BF_MULTI.setShortValue((short) 0, (short) j)), (short) j);
            assertEquals(BF_MULTI.setShortValue((short) 0, (short) j), (short) (j << 7));
        }
        for (int j = 0; j < limit; j++) {
            assertEquals(BF_ZERO.getShortValue(BF_ZERO.setShortValue((short) 0, (short) j)), (short) 0);
            assertEquals(BF_ZERO.setShortValue((short) 0, (short) j), (short) 0);
        }
        // verify that excess bits are stripped off
        assertEquals(BF_MULTI.setShortValue((short) 0x3f80, (short) 128), (short) 0);
        for (int j = 0; j < 2; j++) {
            assertEquals(BF_SINGLE.getShortValue(BF_SINGLE.setShortValue((short) 0, (short) j)), (short) j);
            assertEquals(BF_SINGLE.setShortValue((short) 0, (short) j), (short) (j << 14));
        }
        // verify that excess bits are stripped off
        assertEquals(BF_SINGLE.setShortValue((short) 0x4000, (short) 2), (short) 0);
    }

    /**
     * test the setValue() method
     */
    @Test
    void testSetValue() {
        final int limit = 128;
        for (int j = 0; j < limit; j++) {
            assertEquals(BF_MULTI.getValue(BF_MULTI.setValue(0, j)), j);
            assertEquals(BF_MULTI.setValue(0, j), j << 7);
        }
        for (int j = 0; j < limit; j++) {
            assertEquals(BF_ZERO.getValue(BF_ZERO.setValue(0, j)), 0);
            assertEquals(BF_ZERO.setValue(0, j), 0);
        }
        // verify that excess bits are stripped off
        assertEquals(BF_MULTI.setValue(0x3f80, 128), 0);
        for (int j = 0; j < 2; j++) {
            assertEquals(BF_SINGLE.getValue(BF_SINGLE.setValue(0, j)), j);
            assertEquals(BF_SINGLE.setValue(0, j), j << 14);
        }
        // verify that excess bits are stripped off
        assertEquals(BF_SINGLE.setValue(0x4000, 2), 0);
    }
}
