/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

package org.apache.commons.lang3.util;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.BitSet;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.ArrayUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link FluentBitSet}.
 * <p>
 * Test code originally from Apache Harmony for FluentBitSet and adapted.
 * </p>
 */
public class FluentBitSetTest extends AbstractLangTest {

    private BitSet eightBs;
    private FluentBitSet eightFbs;

    /**
     * BeforeEach.
     */
    @BeforeEach
    public void beforeEach() {

        eightFbs = newInstance();

        for (int i = 0; i < 8; i++) {
            eightFbs.set(i);
        }
        eightBs = eightFbs.bitSet();
    }

    private FluentBitSet newInstance() {
        return new FluentBitSet();
    }

    private FluentBitSet newInstance(final int nbits) {
        return new FluentBitSet(nbits);
    }

    /**
     * Tests {@link FluentBitSet#and(FluentBitSet)}.
     */
    @Test
    public void test_and() {
        // Test for method void java.util.BitSet.and(BitSet)
        final FluentBitSet bs = newInstance(128);
        // Initialize the bottom half of the BitSet

        for (int i = 64; i < 128; i++) {
            bs.set(i);
        }
        eightFbs.and(bs);
        assertFalse(eightFbs.equals(bs), "AND failed to clear bits");
        eightFbs.set(3);
        bs.set(3);
        eightFbs.and(bs);
        assertTrue(bs.get(3), "AND failed to maintain set bits");
        bs.and(eightFbs);
        for (int i = 64; i < 128; i++) {
            assertFalse(bs.get(i), "Failed to clear extra bits in the receiver BitSet");
        }
    }

    /**
     * Tests {@link FluentBitSet#and(BitSet)}.
     */
    @Test
    public void test_and_BitSet() {
        // Test for method void java.util.BitSet.and(BitSet)
        final FluentBitSet bs = newInstance(128);
        // Initialize the bottom half of the BitSet

        for (int i = 64; i < 128; i++) {
            bs.set(i);
        }
        eightFbs.and(bs.bitSet());
        assertFalse(eightFbs.equals(bs), "AND failed to clear bits");
        eightFbs.set(3);
        bs.set(3);
        eightFbs.and(bs.bitSet());
        assertTrue(bs.get(3), "AND failed to maintain set bits");
        bs.and(eightBs);
        for (int i = 64; i < 128; i++) {
            assertFalse(bs.get(i), "Failed to clear extra bits in the receiver BitSet");
        }
    }

    /**
     * Tests {@link FluentBitSet#andNot(BitSet)}.
     */
    @Test
    public void test_andNot() {
        FluentBitSet bs = (FluentBitSet) eightFbs.clone();
        bs.clear(5);
        final FluentBitSet bs2 = newInstance();
        bs2.set(2);
        bs2.set(3);
        bs.andNot(bs2);
        assertEquals("{0, 1, 4, 6, 7}", bs.toString(), "Incorrect bitset after andNot");

        bs = newInstance(0);
        bs.andNot(bs2);
        assertEquals(0, bs.size(), "Incorrect size");
    }

    /**
     * Tests {@link FluentBitSet#andNot(BitSet)}.
     */
    @Test
    public void test_andNot_BitSet() {
        FluentBitSet bs = (FluentBitSet) eightFbs.clone();
        bs.clear(5);
        final FluentBitSet bs2 = newInstance();
        bs2.set(2);
        bs2.set(3);
        bs.andNot(bs2.bitSet());
        assertEquals("{0, 1, 4, 6, 7}", bs.toString(), "Incorrect bitset after andNot");

        bs = newInstance(0);
        bs.andNot(bs2.bitSet());
        assertEquals(0, bs.size(), "Incorrect size");
    }

    /**
     * Tests {@link FluentBitSet#cardinality()}.
     */
    @Test
    public void test_cardinality() {
        // test for method int java.util.BitSet.cardinality()
        final FluentBitSet bs = newInstance(500);
        bs.set(5);
        bs.set(32);
        bs.set(63);
        bs.set(64);
        bs.set(71, 110);
        bs.set(127, 130);
        bs.set(193);
        bs.set(450);
        assertEquals(48, bs.cardinality(), "cardinality() returned wrong value");

        bs.flip(0, 500);
        assertEquals(452, bs.cardinality(), "cardinality() returned wrong value");

        bs.clear();
        assertEquals(0, bs.cardinality(), "cardinality() returned wrong value");

        bs.set(0, 500);
        assertEquals(500, bs.cardinality(), "cardinality() returned wrong value");
    }

    /**
     * Tests {@link FluentBitSet#clear()}.
     */
    @Test
    public void test_clear() {
        eightFbs.clear();
        for (int i = 0; i < 8; i++) {
            assertFalse(eightFbs.get(i), "Clear didn't clear bit " + i);
        }
        assertEquals(0, eightFbs.length(), "Test1: Wrong length");

        final FluentBitSet bs = newInstance(3400);
        bs.set(0, bs.size() - 1); // ensure all bits are 1's
        bs.set(bs.size() - 1);
        bs.clear();
        assertEquals(0, bs.length(), "Test2: Wrong length");
        assertTrue(bs.isEmpty(), "Test2: isEmpty() returned incorrect value");
        assertEquals(0, bs.cardinality(), "Test2: cardinality() returned incorrect value");
    }

    /**
     * Tests {@link FluentBitSet#clear(int)}.
     */
    @Test
    public void test_clearI() {
        // Test for method void java.util.BitSet.clear(int)

        eightFbs.clear(7);
        assertFalse(eightFbs.get(7), "Failed to clear bit");

        // Check to see all other bits are still set
        for (int i = 0; i < 7; i++) {
            assertTrue(eightFbs.get(i), "Clear cleared incorrect bits");
        }

        eightFbs.clear(165);
        assertFalse(eightFbs.get(165), "Failed to clear bit");
        // Try out of range
        assertThrows(IndexOutOfBoundsException.class, () -> eightFbs.clear(-1));

        final FluentBitSet bs = newInstance(0);
        assertEquals(0, bs.length(), "Test1: Wrong length,");
        assertEquals(0, bs.size(), "Test1: Wrong size,");

        bs.clear(0);
        assertEquals(0, bs.length(), "Test2: Wrong length,");
        assertEquals(0, bs.size(), "Test2: Wrong size,");

        bs.clear(60);
        assertEquals(0, bs.length(), "Test3: Wrong length,");
        assertEquals(0, bs.size(), "Test3: Wrong size,");

        bs.clear(120);
        assertEquals(0, bs.size(), "Test4: Wrong size,");
        assertEquals(0, bs.length(), "Test4: Wrong length,");

        bs.set(25);
        assertEquals(64, bs.size(), "Test5: Wrong size,");
        assertEquals(26, bs.length(), "Test5: Wrong length,");

        bs.clear(80);
        assertEquals(64, bs.size(), "Test6: Wrong size,");
        assertEquals(26, bs.length(), "Test6: Wrong length,");

        bs.clear(25);
        assertEquals(64, bs.size(), "Test7: Wrong size,");
        assertEquals(0, bs.length(), "Test7: Wrong length,");
    }

    /**
     * Tests {@link FluentBitSet#clear(int, int)}.
     */
    @Test
    public void test_clearII() {
        // Regression for HARMONY-98
        final FluentBitSet bitset = newInstance();
        for (int i = 0; i < 20; i++) {
            bitset.set(i);
        }
        bitset.clear(10, 10);

        // Test for method void java.util.BitSet.clear(int, int)
        // pos1 and pos2 are in the same bitset element
        FluentBitSet bs = newInstance(16);
        int initialSize = bs.size();
        bs.set(0, initialSize);
        bs.clear(5);
        bs.clear(15);
        bs.clear(7, 11);
        for (int i = 0; i < 7; i++) {
            if (i == 5) {
                assertFalse(bs.get(i), "Shouldn't have flipped bit " + i);
            } else {
                assertTrue(bs.get(i), "Shouldn't have cleared bit " + i);
            }
        }
        for (int i = 7; i < 11; i++) {
            assertFalse(bs.get(i), "Failed to clear bit " + i);
        }

        for (int i = 11; i < initialSize; i++) {
            if (i == 15) {
                assertFalse(bs.get(i), "Shouldn't have flipped bit " + i);
            } else {
                assertTrue(bs.get(i), "Shouldn't have cleared bit " + i);
            }
        }

        for (int i = initialSize; i < bs.size(); i++) {
            assertFalse(bs.get(i), "Shouldn't have flipped bit " + i);
        }

        // pos1 and pos2 is in the same bitset element, boundry testing
        bs = newInstance(16);
        initialSize = bs.size();
        bs.set(0, initialSize);
        bs.clear(7, 64);
        assertEquals(64, bs.size(), "Failed to grow BitSet");
        for (int i = 0; i < 7; i++) {
            assertTrue(bs.get(i), "Shouldn't have cleared bit " + i);
        }
        for (int i = 7; i < 64; i++) {
            assertFalse(bs.get(i), "Failed to clear bit " + i);
        }
        for (int i = 64; i < bs.size(); i++) {
            assertTrue(!bs.get(i), "Shouldn't have flipped bit " + i);
        }
        // more boundary testing
        bs = newInstance(32);
        initialSize = bs.size();
        bs.set(0, initialSize);
        bs.clear(0, 64);
        for (int i = 0; i < 64; i++) {
            assertFalse(bs.get(i), "Failed to clear bit " + i);
        }
        for (int i = 64; i < bs.size(); i++) {
            assertFalse(bs.get(i), "Shouldn't have flipped bit " + i);
        }

        bs = newInstance(32);
        initialSize = bs.size();
        bs.set(0, initialSize);
        bs.clear(0, 65);
        for (int i = 0; i < 65; i++) {
            assertFalse(bs.get(i), "Failed to clear bit " + i);
        }
        for (int i = 65; i < bs.size(); i++) {
            assertFalse(bs.get(i), "Shouldn't have flipped bit " + i);
        }

        // pos1 and pos2 are in two sequential bitset elements
        bs = newInstance(128);
        initialSize = bs.size();
        bs.set(0, initialSize);
        bs.clear(7);
        bs.clear(110);
        bs.clear(9, 74);
        for (int i = 0; i < 9; i++) {
            if (i == 7) {
                assertFalse(bs.get(i), "Shouldn't have flipped bit " + i);
            } else {
                assertTrue(bs.get(i), "Shouldn't have cleared bit " + i);
            }
        }
        for (int i = 9; i < 74; i++) {
            assertFalse(bs.get(i), "Failed to clear bit " + i);
        }
        for (int i = 74; i < initialSize; i++) {
            if (i == 110) {
                assertFalse(bs.get(i), "Shouldn't have flipped bit " + i);
            } else {
                assertTrue(bs.get(i), "Shouldn't have cleared bit " + i);
            }
        }
        for (int i = initialSize; i < bs.size(); i++) {
            assertFalse(bs.get(i), "Shouldn't have flipped bit " + i);
        }

        // pos1 and pos2 are in two non-sequential bitset elements
        bs = newInstance(256);
        bs.set(0, 256);
        bs.clear(7);
        bs.clear(255);
        bs.clear(9, 219);
        for (int i = 0; i < 9; i++) {
            if (i == 7) {
                assertFalse(bs.get(i), "Shouldn't have flipped bit " + i);
            } else {
                assertTrue(bs.get(i), "Shouldn't have cleared bit " + i);
            }
        }

        for (int i = 9; i < 219; i++) {
            assertFalse(bs.get(i), "failed to clear bit " + i);
        }

        for (int i = 219; i < 255; i++) {
            assertTrue(bs.get(i), "Shouldn't have cleared bit " + i);
        }

        for (int i = 255; i < bs.size(); i++) {
            assertFalse(bs.get(i), "Shouldn't have flipped bit " + i);
        }

        // test illegal args
        bs = newInstance(10);
        assertThrows(IndexOutOfBoundsException.class, () -> newInstance(10).clear(-1, 3),
            "Test1: Attempt to flip with negative index failed to generate exception");

        assertThrows(IndexOutOfBoundsException.class, () -> newInstance(10).clear(2, -1),
            "Test2: Attempt to flip with negative index failed to generate exception");

        bs.set(2, 4);
        bs.clear(2, 2);
        assertTrue(bs.get(2), "Bit got cleared incorrectly ");

        assertThrows(IndexOutOfBoundsException.class, () -> newInstance(10).clear(4, 2),
            "Test4: Attempt to flip with illegal args failed to generate exception");

        bs = newInstance(0);
        assertEquals(0, bs.length(), "Test1: Wrong length,");
        assertEquals(0, bs.size(), "Test1: Wrong size,");

        bs.clear(0, 2);
        assertEquals(0, bs.length(), "Test2: Wrong length,");
        assertEquals(0, bs.size(), "Test2: Wrong size,");

        bs.clear(60, 64);
        assertEquals(0, bs.length(), "Test3: Wrong length,");
        assertEquals(0, bs.size(), "Test3: Wrong size,");

        bs.clear(64, 120);
        assertEquals(0, bs.length(), "Test4: Wrong length,");
        assertEquals(0, bs.size(), "Test4: Wrong size,");

        bs.set(25);
        assertEquals(26, bs.length(), "Test5: Wrong length,");
        assertEquals(64, bs.size(), "Test5: Wrong size,");

        bs.clear(60, 64);
        assertEquals(26, bs.length(), "Test6: Wrong length,");
        assertEquals(64, bs.size(), "Test6: Wrong size,");

        bs.clear(64, 120);
        assertEquals(64, bs.size(), "Test7: Wrong size,");
        assertEquals(26, bs.length(), "Test7: Wrong length,");

        bs.clear(80);
        assertEquals(64, bs.size(), "Test8: Wrong size,");
        assertEquals(26, bs.length(), "Test8: Wrong length,");

        bs.clear(25);
        assertEquals(64, bs.size(), "Test9: Wrong size,");
        assertEquals(0, bs.length(), "Test9: Wrong length,");
    }

    /**
     * Tests {@link FluentBitSet#clear(int...)}.
     */
    @Test
    public void test_clearIntArray() {
        // Test for method void java.util.BitSet.clear(int)

        eightFbs.clear(new int[] {7});
        assertFalse(eightFbs.get(7), "Failed to clear bit");

        // Check to see all other bits are still set
        for (int i = 0; i < 7; i++) {
            assertTrue(eightFbs.get(i), "Clear cleared incorrect bits");
        }

        eightFbs.clear(165);
        assertFalse(eightFbs.get(165), "Failed to clear bit");
        // Try out of range
        assertThrows(IndexOutOfBoundsException.class, () -> eightFbs.clear(-1));

        final FluentBitSet bs = newInstance(0);
        assertEquals(0, bs.length(), "Test1: Wrong length,");
        assertEquals(0, bs.size(), "Test1: Wrong size,");

        bs.clear(new int[] {0});
        assertEquals(0, bs.length(), "Test2: Wrong length,");
        assertEquals(0, bs.size(), "Test2: Wrong size,");

        bs.clear(new int[] {60});
        assertEquals(0, bs.length(), "Test3: Wrong length,");
        assertEquals(0, bs.size(), "Test3: Wrong size,");

        bs.clear(new int[] {120});
        assertEquals(0, bs.size(), "Test4: Wrong size,");
        assertEquals(0, bs.length(), "Test4: Wrong length,");

        bs.set(25);
        assertEquals(64, bs.size(), "Test5: Wrong size,");
        assertEquals(26, bs.length(), "Test5: Wrong length,");

        bs.clear(new int[] {80});
        assertEquals(64, bs.size(), "Test6: Wrong size,");
        assertEquals(26, bs.length(), "Test6: Wrong length,");

        bs.clear(new int[] {25});
        assertEquals(64, bs.size(), "Test7: Wrong size,");
        assertEquals(0, bs.length(), "Test7: Wrong length,");
    }

    /**
     * Tests FluentBitSet#clone()
     */
    @Test
    public void test_clone() {
        final FluentBitSet bs = (FluentBitSet) eightFbs.clone();
        assertEquals(bs, eightFbs, "clone failed to return equal BitSet");
    }

    /**
     * Tests {@link FluentBitSet#FluentBitSet()}.
     */
    @Test
    public void test_Constructor() {
        final FluentBitSet bs = newInstance();
        assertEquals(64, bs.size(), "Create FluentBitSet of incorrect size");
        assertEquals("{}", bs.toString(), "New FluentBitSet had invalid string representation");
    }

    /**
     * Tests {@link FluentBitSet#FluentBitSet(int)}.
     */
    @Test
    public void test_ConstructorInt() {
        FluentBitSet bs = newInstance(128);
        assertEquals(128, bs.size(), "Create FluentBitSet of incorrect size");
        assertEquals("{}", bs.toString(), "New FluentBitSet had invalid string representation: " + bs.toString());
        // All BitSets are created with elements of multiples of 64
        bs = newInstance(89);
        assertEquals(128, bs.size(), "Failed to round FluentBitSet element size");

        assertThrows(NegativeArraySizeException.class, () -> newInstance(-9));
    }

    /**
     * Tests {@link FluentBitSet#equals(java.lang.Object)}.
     */
    @Test
    public void test_equals() {
        FluentBitSet bs;
        bs = (FluentBitSet) eightFbs.clone();
        assertEquals(eightFbs, eightFbs, "Same FluentBitSet returned false");
        assertEquals(bs, eightFbs, "Identical FluentBitSet returned false");
        bs.clear(6);
        assertFalse(eightFbs.equals(bs), "Different BitSets returned true");
        assertFalse(eightFbs.equals(null), "Different BitSets returned true");
        assertFalse(eightFbs.equals(new Object()), "Different BitSets returned true");

        bs = (FluentBitSet) eightFbs.clone();
        bs.set(128);
        assertFalse(eightFbs.equals(bs), "Different sized FluentBitSet with higher bit set returned true");
        bs.clear(128);
        assertTrue(eightFbs.equals(bs), "Different sized FluentBitSet with higher bits not set returned false");
    }

    /**
     * Tests {@link FluentBitSet#flip(int)}.
     */
    @Test
    public void test_flipI() {
        // Test for method void java.util.BitSet.flip(int)
        FluentBitSet bs = newInstance();
        bs.clear(8);
        bs.clear(9);
        bs.set(10);
        bs.flip(9);
        assertFalse(bs.get(8), "Failed to flip bit");
        assertTrue(bs.get(9), "Failed to flip bit");
        assertTrue(bs.get(10), "Failed to flip bit");

        bs.set(8);
        bs.set(9);
        bs.clear(10);
        bs.flip(9);
        assertTrue(bs.get(8), "Failed to flip bit");
        assertFalse(bs.get(9), "Failed to flip bit");
        assertFalse(bs.get(10), "Failed to flip bit");

        assertThrows(IndexOutOfBoundsException.class, () -> newInstance().flip(-1), "Attempt to flip at negative index failed to generate exception");

        // Try setting a bit on a 64 boundary
        bs.flip(128);
        assertEquals(192, bs.size(), "Failed to grow BitSet");
        assertTrue(bs.get(128), "Failed to flip bit");

        bs = newInstance(64);
        for (int i = bs.size(); --i >= 0;) {
            bs.flip(i);
            assertTrue(bs.get(i), "Test1: Incorrectly flipped bit" + i);
            assertEquals(i + 1, bs.length(), "Incorrect length");
            for (int j = bs.size(); --j > i;) {
                assertTrue(!bs.get(j), "Test2: Incorrectly flipped bit" + j);
            }
            for (int j = i; --j >= 0;) {
                assertTrue(!bs.get(j), "Test3: Incorrectly flipped bit" + j);
            }
            bs.flip(i);
        }

        final FluentBitSet bs0 = newInstance(0);
        assertEquals(0, bs0.size(), "Test1: Wrong size");
        assertEquals(0, bs0.length(), "Test1: Wrong length");

        bs0.flip(0);
        assertEquals(bs0.size(), 64, "Test2: Wrong size");
        assertEquals(1, bs0.length(), "Test2: Wrong length");

        bs0.flip(63);
        assertEquals(64, bs0.size(), "Test3: Wrong size");
        assertEquals(64, bs0.length(), "Test3: Wrong length");

        eightFbs.flip(7);
        assertTrue(!eightFbs.get(7), "Failed to flip bit 7");

        // Check to see all other bits are still set
        for (int i = 0; i < 7; i++) {
            assertTrue(eightFbs.get(i), "Flip flipped incorrect bits");
        }

        eightFbs.flip(127);
        assertTrue(eightFbs.get(127), "Failed to flip bit 127");

        eightFbs.flip(127);
        assertTrue(!eightFbs.get(127), "Failed to flip bit 127");
    }

    /**
     * Tests {@link FluentBitSet#clear(int, int)}.
     */
    @Test
    public void test_flipII() {
        final FluentBitSet bitset = newInstance();
        for (int i = 0; i < 20; i++) {
            bitset.set(i);
        }
        bitset.flip(10, 10);

        // Test for method void java.util.BitSet.flip(int, int)
        // pos1 and pos2 are in the same bitset element
        FluentBitSet bs = newInstance(16);
        bs.set(7);
        bs.set(10);
        bs.flip(7, 11);
        for (int i = 0; i < 7; i++) {
            assertTrue(!bs.get(i), "Shouldn't have flipped bit " + i);
        }
        assertFalse(bs.get(7), "Failed to flip bit 7");
        assertTrue(bs.get(8), "Failed to flip bit 8");
        assertTrue(bs.get(9), "Failed to flip bit 9");
        assertFalse(bs.get(10), "Failed to flip bit 10");
        for (int i = 11; i < bs.size(); i++) {
            assertTrue(!bs.get(i), "Shouldn't have flipped bit " + i);
        }

        // pos1 and pos2 is in the same bitset element, boundry testing
        bs = newInstance(16);
        bs.set(7);
        bs.set(10);
        bs.flip(7, 64);
        assertEquals(64, bs.size(), "Failed to grow BitSet");
        for (int i = 0; i < 7; i++) {
            assertTrue(!bs.get(i), "Shouldn't have flipped bit " + i);
        }
        assertFalse(bs.get(7), "Failed to flip bit 7");
        assertTrue(bs.get(8), "Failed to flip bit 8");
        assertTrue(bs.get(9), "Failed to flip bit 9");
        assertFalse(bs.get(10), "Failed to flip bit 10");
        for (int i = 11; i < 64; i++) {
            assertTrue(bs.get(i), "failed to flip bit " + i);
        }
        assertFalse(bs.get(64), "Shouldn't have flipped bit 64");

        // more boundary testing
        bs = newInstance(32);
        bs.flip(0, 64);
        for (int i = 0; i < 64; i++) {
            assertTrue(bs.get(i), "Failed to flip bit " + i);
        }
        assertFalse(bs.get(64), "Shouldn't have flipped bit 64");

        bs = newInstance(32);
        bs.flip(0, 65);
        for (int i = 0; i < 65; i++) {
            assertTrue(bs.get(i), "Failed to flip bit " + i);
        }
        assertFalse(bs.get(65), "Shouldn't have flipped bit 65");

        // pos1 and pos2 are in two sequential bitset elements
        bs = newInstance(128);
        bs.set(7);
        bs.set(10);
        bs.set(72);
        bs.set(110);
        bs.flip(9, 74);
        for (int i = 0; i < 7; i++) {
            assertFalse(bs.get(i), "Shouldn't have flipped bit " + i);
        }
        assertTrue(bs.get(7), "Shouldn't have flipped bit 7");
        assertFalse(bs.get(8), "Shouldn't have flipped bit 8");
        assertTrue(bs.get(9), "Failed to flip bit 9");
        assertFalse(bs.get(10), "Failed to flip bit 10");
        for (int i = 11; i < 72; i++) {
            assertTrue(bs.get(i), "failed to flip bit " + i);
        }
        assertFalse(bs.get(72), "Failed to flip bit 72");
        assertTrue(bs.get(73), "Failed to flip bit 73");
        for (int i = 74; i < 110; i++) {
            assertFalse(bs.get(i), "Shouldn't have flipped bit " + i);
        }
        assertTrue(bs.get(110), "Shouldn't have flipped bit 110");
        for (int i = 111; i < bs.size(); i++) {
            assertFalse(bs.get(i), "Shouldn't have flipped bit " + i);
        }

        // pos1 and pos2 are in two non-sequential bitset elements
        bs = newInstance(256);
        bs.set(7);
        bs.set(10);
        bs.set(72);
        bs.set(110);
        bs.set(181);
        bs.set(220);
        bs.flip(9, 219);
        for (int i = 0; i < 7; i++) {
            assertFalse(bs.get(i), "Shouldn't have flipped bit " + i);
        }
        assertTrue(bs.get(7), "Shouldn't have flipped bit 7");
        assertFalse(bs.get(8), "Shouldn't have flipped bit 8");
        assertTrue(bs.get(9), "Failed to flip bit 9");
        assertFalse(bs.get(10), "Failed to flip bit 10");
        for (int i = 11; i < 72; i++) {
            assertTrue(bs.get(i), "failed to flip bit " + i);
        }
        assertFalse(bs.get(72), "Failed to flip bit 72");
        for (int i = 73; i < 110; i++) {
            assertTrue(bs.get(i), "failed to flip bit " + i);
        }
        assertFalse(bs.get(110), "Failed to flip bit 110");
        for (int i = 111; i < 181; i++) {
            assertTrue(bs.get(i), "failed to flip bit " + i);
        }
        assertFalse(bs.get(181), "Failed to flip bit 181");
        for (int i = 182; i < 219; i++) {
            assertTrue(bs.get(i), "failed to flip bit " + i);
        }
        assertFalse(bs.get(219), "Shouldn't have flipped bit 219");
        assertTrue(bs.get(220), "Shouldn't have flipped bit 220");
        for (int i = 221; i < bs.size(); i++) {
            assertTrue(!bs.get(i), "Shouldn't have flipped bit " + i);
        }

        // test illegal args
        bs = newInstance(10);
        try {
            bs.flip(-1, 3);
            fail("Test1: Attempt to flip with  negative index failed to generate exception");
        } catch (final IndexOutOfBoundsException e) {
            // correct behavior
        }

        try {
            bs.flip(2, -1);
            fail("Test2: Attempt to flip with negative index failed to generate exception");
        } catch (final IndexOutOfBoundsException e) {
            // correct behavior
        }

        try {
            bs.flip(4, 2);
            fail("Test4: Attempt to flip with illegal args failed to generate exception");
        } catch (final IndexOutOfBoundsException e) {
            // correct behavior
        }
    }

    /**
     * Tests {@link FluentBitSet#get(int)}.
     */
    @Test
    public void test_getI() {
        // Test for method boolean java.util.BitSet.get(int)

        FluentBitSet bs = newInstance();
        bs.set(8);
        assertFalse(eightFbs.get(99), "Get returned true for index out of range");
        assertTrue(eightFbs.get(3), "Get returned false for set value");
        assertFalse(bs.get(0), "Get returned true for a non set value");

        assertThrows(IndexOutOfBoundsException.class, () -> newInstance().get(-1), "Attempt to get at negative index failed to generate exception");

        bs = newInstance(1);
        assertFalse(bs.get(64), "Access greater than size");

        bs = newInstance();
        bs.set(63);
        assertTrue(bs.get(63), "Test highest bit");

        bs = newInstance(0);
        assertEquals(0, bs.length(), "Test1: Wrong length,");
        assertEquals(0, bs.size(), "Test1: Wrong size,");

        bs.get(2);
        assertEquals(0, bs.length(), "Test2: Wrong length,");
        assertEquals(0, bs.size(), "Test2: Wrong size,");

        bs.get(70);
        assertEquals(0, bs.length(), "Test3: Wrong length,");
        assertEquals(0, bs.size(), "Test3: Wrong size,");

    }

    /**
     * Tests {@link FluentBitSet#get(int, int)}.
     */
    @Test
    public void test_getII() {
        final FluentBitSet bitset = newInstance(30);
        bitset.get(3, 3);

        // Test for method boolean java.util.BitSet.get(int, int)
        FluentBitSet bs, resultbs, correctbs;
        bs = newInstance(512);
        bs.set(3, 9);
        bs.set(10, 20);
        bs.set(60, 75);
        bs.set(121);
        bs.set(130, 140);

        // pos1 and pos2 are in the same bitset element, at index0
        resultbs = bs.get(3, 6);
        correctbs = newInstance(3);
        correctbs.set(0, 3);
        assertEquals(correctbs, resultbs, "Test1: Returned incorrect BitSet");

        // pos1 and pos2 are in the same bitset element, at index 1
        resultbs = bs.get(100, 125);
        correctbs = newInstance(25);
        correctbs.set(21);
        assertEquals(correctbs, resultbs, "Test2: Returned incorrect BitSet");

        // pos1 in bitset element at index 0, and pos2 in bitset element at
        // index 1
        resultbs = bs.get(15, 125);
        correctbs = newInstance(25);
        correctbs.set(0, 5);
        correctbs.set(45, 60);
        correctbs.set(121 - 15);
        assertEquals(correctbs, resultbs, "Test3: Returned incorrect BitSet");

        // pos1 in bitset element at index 1, and pos2 in bitset element at
        // index 2
        resultbs = bs.get(70, 145);
        correctbs = newInstance(75);
        correctbs.set(0, 5);
        correctbs.set(51);
        correctbs.set(60, 70);
        assertEquals(correctbs, resultbs, "Test4: Returned incorrect BitSet");

        // pos1 in bitset element at index 0, and pos2 in bitset element at
        // index 2
        resultbs = bs.get(5, 145);
        correctbs = newInstance(140);
        correctbs.set(0, 4);
        correctbs.set(5, 15);
        correctbs.set(55, 70);
        correctbs.set(116);
        correctbs.set(125, 135);
        assertEquals(correctbs, resultbs, "Test5: Returned incorrect BitSet");

        // pos1 in bitset element at index 0, and pos2 in bitset element at
        // index 3
        resultbs = bs.get(5, 250);
        correctbs = newInstance(200);
        correctbs.set(0, 4);
        correctbs.set(5, 15);
        correctbs.set(55, 70);
        correctbs.set(116);
        correctbs.set(125, 135);
        assertEquals(correctbs, resultbs, "Test6: Returned incorrect BitSet");

        assertEquals(bs.get(0, bs.size()), bs, "equality principle 1 ");

        // more tests
        FluentBitSet bs2 = newInstance(129);
        bs2.set(0, 20);
        bs2.set(62, 65);
        bs2.set(121, 123);
        resultbs = bs2.get(1, 124);
        correctbs = newInstance(129);
        correctbs.set(0, 19);
        correctbs.set(61, 64);
        correctbs.set(120, 122);
        assertEquals(correctbs, resultbs, "Test7: Returned incorrect BitSet");

        // equality principle with some boundary conditions
        bs2 = newInstance(128);
        bs2.set(2, 20);
        bs2.set(62);
        bs2.set(121, 123);
        bs2.set(127);
        resultbs = bs2.get(0, bs2.size());
        assertEquals(resultbs, bs2, "equality principle 2 ");

        bs2 = newInstance(128);
        bs2.set(2, 20);
        bs2.set(62);
        bs2.set(121, 123);
        bs2.set(127);
        bs2.flip(0, 128);
        resultbs = bs2.get(0, bs.size());
        assertEquals(resultbs, bs2, "equality principle 3 ");

        bs = newInstance(0);
        assertEquals(0, bs.length(), "Test1: Wrong length,");
        assertEquals(0, bs.size(), "Test1: Wrong size,");

        bs.get(0, 2);
        assertEquals(0, bs.length(), "Test2: Wrong length,");
        assertEquals(0, bs.size(), "Test2: Wrong size,");

        bs.get(60, 64);
        assertEquals(0, bs.length(), "Test3: Wrong length,");
        assertEquals(0, bs.size(), "Test3: Wrong size,");

        bs.get(64, 120);
        assertEquals(0, bs.length(), "Test4: Wrong length,");
        assertEquals(0, bs.size(), "Test4: Wrong size,");

        bs.set(25);
        assertEquals(26, bs.length(), "Test5: Wrong length,");
        assertEquals(64, bs.size(), "Test5: Wrong size,");

        bs.get(60, 64);
        assertEquals(26, bs.length(), "Test6: Wrong length,");
        assertEquals(64, bs.size(), "Test6: Wrong size,");

        bs.get(64, 120);
        assertEquals(64, bs.size(), "Test7: Wrong size,");
        assertEquals(26, bs.length(), "Test7: Wrong length,");

        bs.get(80);
        assertEquals(64, bs.size(), "Test8: Wrong size,");
        assertEquals(26, bs.length(), "Test8: Wrong length,");

        bs.get(25);
        assertEquals(64, bs.size(), "Test9: Wrong size,");
        assertEquals(26, bs.length(), "Test9: Wrong length,");

    }

    /**
     * Tests {@link FluentBitSet#hashCode()}.
     */
    @Test
    public void test_hashCode() {
        // Test for method int java.util.BitSet.hashCode()
        final FluentBitSet bs = (FluentBitSet) eightFbs.clone();
        bs.clear(2);
        bs.clear(6);
        assertEquals(bs.bitSet().hashCode(), bs.hashCode(), "BitSet returns wrong hash value");
        bs.set(10);
        bs.clear(3);
        assertEquals(97, bs.hashCode(), "BitSet returns wrong hash value");
    }

    /**
     * Tests {@link FluentBitSet#intersects(FluentBitSet)}.
     */
    @Test
    public void test_intersects() {
        // Test for method boolean java.util.BitSet.intersects(BitSet)
        final FluentBitSet bs = newInstance(500);
        bs.set(5);
        bs.set(63);
        bs.set(64);
        bs.set(71, 110);
        bs.set(127, 130);
        bs.set(192);
        bs.set(450);

        final FluentBitSet bs2 = newInstance(8);
        assertFalse(bs.intersects(bs2), "Test1: intersects() returned incorrect value");
        assertFalse(bs2.intersects(bs), "Test1: intersects() returned incorrect value");

        bs2.set(4);
        assertFalse(bs.intersects(bs2), "Test2: intersects() returned incorrect value");
        assertFalse(bs2.intersects(bs), "Test2: intersects() returned incorrect value");

        bs2.clear();
        bs2.set(5);
        assertTrue(bs.intersects(bs2), "Test3: intersects() returned incorrect value");
        assertTrue(bs2.intersects(bs), "Test3: intersects() returned incorrect value");

        bs2.clear();
        bs2.set(63);
        assertTrue(bs.intersects(bs2), "Test4: intersects() returned incorrect value");
        assertTrue(bs2.intersects(bs), "Test4: intersects() returned incorrect value");

        bs2.clear();
        bs2.set(80);
        assertTrue(bs.intersects(bs2), "Test5: intersects() returned incorrect value");
        assertTrue(bs2.intersects(bs), "Test5: intersects() returned incorrect value");

        bs2.clear();
        bs2.set(127);
        assertTrue(bs.intersects(bs2), "Test6: intersects() returned incorrect value");
        assertTrue(bs2.intersects(bs), "Test6: intersects() returned incorrect value");

        bs2.clear();
        bs2.set(192);
        assertTrue(bs.intersects(bs2), "Test7: intersects() returned incorrect value");
        assertTrue(bs2.intersects(bs), "Test7: intersects() returned incorrect value");

        bs2.clear();
        bs2.set(450);
        assertTrue(bs.intersects(bs2), "Test8: intersects() returned incorrect value");
        assertTrue(bs2.intersects(bs), "Test8: intersects() returned incorrect value");

        bs2.clear();
        bs2.set(500);
        assertFalse(bs.intersects(bs2), "Test9: intersects() returned incorrect value");
        assertFalse(bs2.intersects(bs), "Test9: intersects() returned incorrect value");
    }

    /**
     * Tests {@link FluentBitSet#intersects(BitSet)}.
     */
    @Test
    public void test_intersects_BitSet() {
        // Test for method boolean java.util.BitSet.intersects(BitSet)
        final FluentBitSet bs = newInstance(500);
        bs.set(5);
        bs.set(63);
        bs.set(64);
        bs.set(71, 110);
        bs.set(127, 130);
        bs.set(192);
        bs.set(450);

        final FluentBitSet bs2 = newInstance(8);
        assertFalse(bs.intersects(bs2.bitSet()), "Test1: intersects() returned incorrect value");
        assertFalse(bs2.intersects(bs.bitSet()), "Test1: intersects() returned incorrect value");

        bs2.set(4);
        assertFalse(bs.intersects(bs2.bitSet()), "Test2: intersects() returned incorrect value");
        assertFalse(bs2.intersects(bs.bitSet()), "Test2: intersects() returned incorrect value");

        bs2.clear();
        bs2.set(5);
        assertTrue(bs.intersects(bs2.bitSet()), "Test3: intersects() returned incorrect value");
        assertTrue(bs2.intersects(bs.bitSet()), "Test3: intersects() returned incorrect value");

        bs2.clear();
        bs2.set(63);
        assertTrue(bs.intersects(bs2.bitSet()), "Test4: intersects() returned incorrect value");
        assertTrue(bs2.intersects(bs.bitSet()), "Test4: intersects() returned incorrect value");

        bs2.clear();
        bs2.set(80);
        assertTrue(bs.intersects(bs2.bitSet()), "Test5: intersects() returned incorrect value");
        assertTrue(bs2.intersects(bs.bitSet()), "Test5: intersects() returned incorrect value");

        bs2.clear();
        bs2.set(127);
        assertTrue(bs.intersects(bs2.bitSet()), "Test6: intersects() returned incorrect value");
        assertTrue(bs2.intersects(bs.bitSet()), "Test6: intersects() returned incorrect value");

        bs2.clear();
        bs2.set(192);
        assertTrue(bs.intersects(bs2.bitSet()), "Test7: intersects() returned incorrect value");
        assertTrue(bs2.intersects(bs.bitSet()), "Test7: intersects() returned incorrect value");

        bs2.clear();
        bs2.set(450);
        assertTrue(bs.intersects(bs2.bitSet()), "Test8: intersects() returned incorrect value");
        assertTrue(bs2.intersects(bs.bitSet()), "Test8: intersects() returned incorrect value");

        bs2.clear();
        bs2.set(500);
        assertFalse(bs.intersects(bs2.bitSet()), "Test9: intersects() returned incorrect value");
        assertFalse(bs2.intersects(bs.bitSet()), "Test9: intersects() returned incorrect value");
    }

    /**
     * Tests {@link FluentBitSet#isEmpty()}.
     */
    @Test
    public void test_isEmpty() {
        final FluentBitSet bs = newInstance(500);
        assertTrue(bs.isEmpty(), "Test: isEmpty() returned wrong value");

        // at bitset element 0
        bs.set(3);
        assertFalse(bs.isEmpty(), "Test0: isEmpty() returned wrong value");

        // at bitset element 1
        bs.clear();
        bs.set(12);
        assertFalse(bs.isEmpty(), "Test1: isEmpty() returned wrong value");

        // at bitset element 2
        bs.clear();
        bs.set(128);
        assertFalse(bs.isEmpty(), "Test2: isEmpty() returned wrong value");

        // boundary testing
        bs.clear();
        bs.set(459);
        assertFalse(bs.isEmpty(), "Test3: isEmpty() returned wrong value");

        bs.clear();
        bs.set(511);
        assertFalse(bs.isEmpty(), "Test4: isEmpty() returned wrong value");
    }

    /**
     * Tests {@link FluentBitSet#length()}.
     */
    @Test
    public void test_length() {
        final FluentBitSet bs = newInstance();
        assertEquals(0, bs.length(), "BitSet returned wrong length");
        bs.set(5);
        assertEquals(6, bs.length(), "BitSet returned wrong length");
        bs.set(10);
        assertEquals(11, bs.length(), "BitSet returned wrong length");
        bs.set(432);
        assertEquals(433, bs.length(), "BitSet returned wrong length");
        bs.set(300);
        assertEquals(433, bs.length(), "BitSet returned wrong length");
    }

    /**
     * Tests {@link FluentBitSet#nextClearBit(int)}.
     */
    @Test
    public void test_nextClearBitI() {
        // Test for method int java.util.BitSet.nextSetBit()
        final FluentBitSet bs = newInstance(500);
        bs.set(0, bs.size() - 1); // ensure all the bits from 0 to bs.size()
                                  // -1
        bs.set(bs.size() - 1); // are set to true
        bs.clear(5);
        bs.clear(32);
        bs.clear(63);
        bs.clear(64);
        bs.clear(71, 110);
        bs.clear(127, 130);
        bs.clear(193);
        bs.clear(450);
        try {
            bs.nextClearBit(-1);
            fail("Expected IndexOutOfBoundsException for negative index");
        } catch (final IndexOutOfBoundsException e) {
            // correct behavior
        }
        assertEquals(5, bs.nextClearBit(0), "nextClearBit() returned the wrong value");
        assertEquals(5, bs.nextClearBit(5), "nextClearBit() returned the wrong value");
        assertEquals(32, bs.nextClearBit(6), "nextClearBit() returned the wrong value");
        assertEquals(32, bs.nextClearBit(32), "nextClearBit() returned the wrong value");
        assertEquals(63, bs.nextClearBit(33), "nextClearBit() returned the wrong value");

        // boundary tests
        assertEquals(63, bs.nextClearBit(63), "nextClearBit() returned the wrong value");
        assertEquals(64, bs.nextClearBit(64), "nextClearBit() returned the wrong value");

        // at bitset element 1
        assertEquals(71, bs.nextClearBit(65), "nextClearBit() returned the wrong value");
        assertEquals(71, bs.nextClearBit(71), "nextClearBit() returned the wrong value");
        assertEquals(72, bs.nextClearBit(72), "nextClearBit() returned the wrong value");
        assertEquals(127, bs.nextClearBit(110), "nextClearBit() returned the wrong value");

        // boundary tests
        assertEquals(127, bs.nextClearBit(127), "nextClearBit() returned the wrong value");
        assertEquals(128, bs.nextClearBit(128), "nextClearBit() returned the wrong value");

        // at bitset element 2
        assertEquals(193, bs.nextClearBit(130), "nextClearBit() returned the wrong value");
        assertEquals(193, bs.nextClearBit(191), "nextClearBit() returned the wrong value");

        assertEquals(193, bs.nextClearBit(192), "nextClearBit() returned the wrong value");
        assertEquals(193, bs.nextClearBit(193), "nextClearBit() returned the wrong value");
        assertEquals(450, bs.nextClearBit(194), "nextClearBit() returned the wrong value");
        assertEquals(450, bs.nextClearBit(255), "nextClearBit() returned the wrong value");
        assertEquals(450, bs.nextClearBit(256), "nextClearBit() returned the wrong value");
        assertEquals(450, bs.nextClearBit(450), "nextClearBit() returned the wrong value");

        // bitset has 1 still the end of bs.size() -1, but calling nextClearBit
        // with any index value
        // after the last true bit should return bs.size(),
        assertEquals(512, bs.nextClearBit(451), "nextClearBit() returned the wrong value");
        assertEquals(512, bs.nextClearBit(511), "nextClearBit() returned the wrong value");
        assertEquals(512, bs.nextClearBit(512), "nextClearBit() returned the wrong value");

        // if the index is larger than bs.size(), nextClearBit should return
        // index;
        assertEquals(513, bs.nextClearBit(513), "nextClearBit() returned the wrong value");
        assertEquals(800, bs.nextClearBit(800), "nextClearBit() returned the wrong value");
    }

    /**
     * Tests {@link FluentBitSet#nextSetBit(int)}.
     */
    @Test
    public void test_nextSetBitI() {
        // Test for method int java.util.BitSet.nextSetBit()
        final FluentBitSet bs = newInstance(500);
        bs.set(5);
        bs.set(32);
        bs.set(63);
        bs.set(64);
        bs.set(71, 110);
        bs.set(127, 130);
        bs.set(193);
        bs.set(450);
        try {
            bs.nextSetBit(-1);
            fail("Expected IndexOutOfBoundsException for negative index");
        } catch (final IndexOutOfBoundsException e) {
            // correct behavior
        }
        assertEquals(5, bs.nextSetBit(0), "nextSetBit() returned the wrong value");
        assertEquals(5, bs.nextSetBit(5), "nextSetBit() returned the wrong value");
        assertEquals(32, bs.nextSetBit(6), "nextSetBit() returned the wrong value");
        assertEquals(32, bs.nextSetBit(32), "nextSetBit() returned the wrong value");
        assertEquals(63, bs.nextSetBit(33), "nextSetBit() returned the wrong value");

        // boundary tests
        assertEquals(63, bs.nextSetBit(63), "nextSetBit() returned the wrong value");
        assertEquals(64, bs.nextSetBit(64), "nextSetBit() returned the wrong value");

        // at bitset element 1
        assertEquals(71, bs.nextSetBit(65), "nextSetBit() returned the wrong value");
        assertEquals(71, bs.nextSetBit(71), "nextSetBit() returned the wrong value");
        assertEquals(72, bs.nextSetBit(72), "nextSetBit() returned the wrong value");
        assertEquals(127, bs.nextSetBit(110), "nextSetBit() returned the wrong value");

        // boundary tests
        assertEquals(127, bs.nextSetBit(127), "nextSetBit() returned the wrong value");
        assertEquals(128, bs.nextSetBit(128), "nextSetBit() returned the wrong value");

        // at bitset element 2
        assertEquals(193, bs.nextSetBit(130), "nextSetBit() returned the wrong value");

        assertEquals(193, bs.nextSetBit(191), "nextSetBit() returned the wrong value");
        assertEquals(193, bs.nextSetBit(192), "nextSetBit() returned the wrong value");
        assertEquals(193, bs.nextSetBit(193), "nextSetBit() returned the wrong value");
        assertEquals(450, bs.nextSetBit(194), "nextSetBit() returned the wrong value");
        assertEquals(450, bs.nextSetBit(255), "nextSetBit() returned the wrong value");
        assertEquals(450, bs.nextSetBit(256), "nextSetBit() returned the wrong value");
        assertEquals(450, bs.nextSetBit(450), "nextSetBit() returned the wrong value");

        assertEquals(-1, bs.nextSetBit(451), "nextSetBit() returned the wrong value");
        assertEquals(-1, bs.nextSetBit(511), "nextSetBit() returned the wrong value");
        assertEquals(-1, bs.nextSetBit(512), "nextSetBit() returned the wrong value");
        assertEquals(-1, bs.nextSetBit(800), "nextSetBit() returned the wrong value");
    }

    /**
     * Tests {@link FluentBitSet#or(FluentBitSet)}.
     */
    @Test
    public void test_or() {
        // Test for method void java.util.BitSet.or(BitSet)
        FluentBitSet bs = newInstance(128);
        bs.or(eightFbs);
        for (int i = 0; i < 8; i++) {
            assertTrue(bs.get(i), "OR failed to set bits");
        }

        bs = newInstance(0);
        bs.or(eightFbs);
        for (int i = 0; i < 8; i++) {
            assertTrue(bs.get(i), "OR(0) failed to set bits");
        }

        eightFbs.clear(5);
        bs = newInstance(128);
        bs.or(eightFbs);
        assertFalse(bs.get(5), "OR set a bit which should be off");
    }

    /**
     * Tests {@link FluentBitSet#or(BitSet)}.
     */
    @Test
    public void test_or_BitSet() {
        // Test for method void java.util.BitSet.or(BitSet)
        FluentBitSet bs = newInstance(128);
        bs.or(eightFbs.bitSet());
        for (int i = 0; i < 8; i++) {
            assertTrue(bs.get(i), "OR failed to set bits");
        }

        bs = newInstance(0);
        bs.or(eightFbs.bitSet());
        for (int i = 0; i < 8; i++) {
            assertTrue(bs.get(i), "OR(0) failed to set bits");
        }

        eightFbs.clear(5);
        bs = newInstance(128);
        bs.or(eightFbs.bitSet());
        assertFalse(bs.get(5), "OR set a bit which should be off");
    }

    /**
     * Tests {@link FluentBitSet#or(FluentBitSet)}.
     */
    @Test
    public void test_or_FluentBitSetArray() {
        // Test for method void java.util.BitSet.or(BitSet)
        FluentBitSet bs = newInstance(128);
        bs.or(new FluentBitSet[] {eightFbs});
        for (int i = 0; i < 8; i++) {
            assertTrue(bs.get(i), "OR failed to set bits");
        }

        bs = newInstance(0);
        bs.or(new FluentBitSet[] {eightFbs});
        for (int i = 0; i < 8; i++) {
            assertTrue(bs.get(i), "OR(0) failed to set bits");
        }

        eightFbs.clear(5);
        bs = newInstance(128);
        bs.or(new FluentBitSet[] {eightFbs});
        assertFalse(bs.get(5), "OR set a bit which should be off");
    }

    /**
     * Tests {@link FluentBitSet#previousClearBit(int)}.
     */
    @Test
    public void test_previousClearBit() {
        final FluentBitSet bs = newInstance();
        assertEquals(1, bs.previousClearBit(1), "previousClearBit");
    }

    /**
     * Tests {@link FluentBitSet#previousSetBit(int)}.
     */
    @Test
    public void test_previousSetBit() {
        final FluentBitSet bs = newInstance();
        assertEquals(-1, bs.previousSetBit(1), "previousSetBit");
    }

    /**
     * Tests {@link FluentBitSet#set(int, int)}.
     */
    @Test
    public void test_setII() {
        final FluentBitSet bitset = newInstance(30);
        bitset.set(29, 29);

        // Test for method void java.util.BitSet.set(int, int)
        // pos1 and pos2 are in the same bitset element
        FluentBitSet bs = newInstance(16);
        bs.set(5);
        bs.set(15);
        bs.set(7, 11);
        for (int i = 0; i < 7; i++) {
            if (i == 5) {
                assertTrue(bs.get(i), "Shouldn't have flipped bit " + i);
            } else {
                assertFalse(bs.get(i), "Shouldn't have set bit " + i);
            }
        }
        for (int i = 7; i < 11; i++) {
            assertTrue(bs.get(i), "Failed to set bit " + i);
        }
        for (int i = 11; i < bs.size(); i++) {
            if (i == 15) {
                assertTrue(bs.get(i), "Shouldn't have flipped bit " + i);
            } else {
                assertFalse(bs.get(i), "Shouldn't have set bit " + i);
            }
        }

        // pos1 and pos2 is in the same bitset element, boundry testing
        bs = newInstance(16);
        bs.set(7, 64);
        assertEquals(64, bs.size(), "Failed to grow BitSet");
        for (int i = 0; i < 7; i++) {
            assertFalse(bs.get(i), "Shouldn't have set bit " + i);
        }
        for (int i = 7; i < 64; i++) {
            assertTrue(bs.get(i), "Failed to set bit " + i);
        }
        assertFalse(bs.get(64), "Shouldn't have set bit 64");

        // more boundary testing
        bs = newInstance(32);
        bs.set(0, 64);
        for (int i = 0; i < 64; i++) {
            assertTrue(bs.get(i), "Failed to set bit " + i);
        }
        assertFalse(bs.get(64), "Shouldn't have set bit 64");

        bs = newInstance(32);
        bs.set(0, 65);
        for (int i = 0; i < 65; i++) {
            assertTrue(bs.get(i), "Failed to set bit " + i);
        }
        assertFalse(bs.get(65), "Shouldn't have set bit 65");

        // pos1 and pos2 are in two sequential bitset elements
        bs = newInstance(128);
        bs.set(7);
        bs.set(110);
        bs.set(9, 74);
        for (int i = 0; i < 9; i++) {
            if (i == 7) {
                assertTrue(bs.get(i), "Shouldn't have flipped bit " + i);
            } else {
                assertFalse(bs.get(i), "Shouldn't have set bit " + i);
            }
        }
        for (int i = 9; i < 74; i++) {
            assertTrue(bs.get(i), "Failed to set bit " + i);
        }
        for (int i = 74; i < bs.size(); i++) {
            if (i == 110) {
                assertTrue(bs.get(i), "Shouldn't have flipped bit " + i);
            } else {
                assertFalse(bs.get(i), "Shouldn't have set bit " + i);
            }
        }

        // pos1 and pos2 are in two non-sequential bitset elements
        bs = newInstance(256);
        bs.set(7);
        bs.set(255);
        bs.set(9, 219);
        for (int i = 0; i < 9; i++) {
            if (i == 7) {
                assertTrue(bs.get(i), "Shouldn't have set flipped " + i);
            } else {
                assertFalse(bs.get(i), "Shouldn't have set bit " + i);
            }
        }

        for (int i = 9; i < 219; i++) {
            assertTrue(bs.get(i), "failed to set bit " + i);
        }

        for (int i = 219; i < 255; i++) {
            assertFalse(bs.get(i), "Shouldn't have set bit " + i);
        }

        assertTrue(bs.get(255), "Shouldn't have flipped bit 255");

        // test illegal args
        bs = newInstance(10);
        try {
            bs.set(-1, 3);
            fail("Test1: Attempt to flip with  negative index failed to generate exception");
        } catch (final IndexOutOfBoundsException e) {
            // Correct behavior
        }

        try {
            bs.set(2, -1);
            fail("Test2: Attempt to flip with negative index failed to generate exception");
        } catch (final IndexOutOfBoundsException e) {
            // Correct behavior
        }

        bs.set(2, 2);
        assertFalse(bs.get(2), "Bit got set incorrectly ");

        try {
            bs.set(4, 2);
            fail("Test4: Attempt to flip with illegal args failed to generate exception");
        } catch (final IndexOutOfBoundsException e) {
            // Correct behavior
        }
    }

    /**
     * Tests {@link FluentBitSet#set(int, int, boolean)}.
     */
    @Test
    public void test_setIIZ() {
        // Test for method void java.util.BitSet.set(int, int, boolean)
        eightFbs.set(3, 6, false);
        assertTrue(!eightFbs.get(3) && !eightFbs.get(4) && !eightFbs.get(5), "Should have set bits 3, 4, and 5 to false");

        eightFbs.set(3, 6, true);
        assertTrue(eightFbs.get(3) && eightFbs.get(4) && eightFbs.get(5), "Should have set bits 3, 4, and 5 to true");

    }

    /**
     * Tests {@link FluentBitSet#setInclusive(int, int)}.
     */
    @Test
    public void test_setInclusive() {
        final FluentBitSet bitset = newInstance(30);
        bitset.set(29, 29);

        // Test for method void java.util.BitSet.set(int, int)
        // pos1 and pos2 are in the same bitset element
        FluentBitSet bs = newInstance(16);
        bs.set(5);
        bs.set(15);
        bs.setInclusive(7, 11);
        for (int i = 0; i < 7; i++) {
            if (i == 5) {
                assertTrue(bs.get(i), "Shouldn't have flipped bit " + i);
            } else {
                assertFalse(bs.get(i), "Shouldn't have set bit " + i);
            }
        }
        for (int i = 7; i < 12; i++) {
            assertTrue(bs.get(i), "Failed to set bit " + i);
        }
        for (int i = 12; i < bs.size(); i++) {
            if (i == 15) {
                assertTrue(bs.get(i), "Shouldn't have flipped bit " + i);
            } else {
                assertFalse(bs.get(i), "Shouldn't have set bit " + i);
            }
        }

        // pos1 and pos2 is in the same bitset element, boundry testing
        bs = newInstance(16);
        bs.setInclusive(7, 64);
        assertEquals(128, bs.size(), "Failed to grow BitSet");
        for (int i = 0; i < 7; i++) {
            assertFalse(bs.get(i), "Shouldn't have set bit " + i);
        }
        for (int i = 7; i < 65; i++) {
            assertTrue(bs.get(i), "Failed to set bit " + i);
        }
        assertFalse(bs.get(65), "Shouldn't have set bit 64");

        // more boundary testing
        bs = newInstance(32);
        bs.setInclusive(0, 64);
        for (int i = 0; i < 65; i++) {
            assertTrue(bs.get(i), "Failed to set bit " + i);
        }
        assertFalse(bs.get(65), "Shouldn't have set bit 64");

        bs = newInstance(32);
        bs.setInclusive(0, 65);
        for (int i = 0; i < 66; i++) {
            assertTrue(bs.get(i), "Failed to set bit " + i);
        }
        assertFalse(bs.get(66), "Shouldn't have set bit 65");

        // pos1 and pos2 are in two sequential bitset elements
        bs = newInstance(128);
        bs.set(7);
        bs.set(110);
        bs.setInclusive(9, 74);
        for (int i = 0; i < 9; i++) {
            if (i == 7) {
                assertTrue(bs.get(i), "Shouldn't have flipped bit " + i);
            } else {
                assertFalse(bs.get(i), "Shouldn't have set bit " + i);
            }
        }
        for (int i = 9; i < 75; i++) {
            assertTrue(bs.get(i), "Failed to set bit " + i);
        }
        for (int i = 75; i < bs.size(); i++) {
            if (i == 110) {
                assertTrue(bs.get(i), "Shouldn't have flipped bit " + i);
            } else {
                assertFalse(bs.get(i), "Shouldn't have set bit " + i);
            }
        }

        // pos1 and pos2 are in two non-sequential bitset elements
        bs = newInstance(256);
        bs.set(7);
        bs.set(255);
        bs.setInclusive(9, 219);
        for (int i = 0; i < 9; i++) {
            if (i == 7) {
                assertTrue(bs.get(i), "Shouldn't have set flipped " + i);
            } else {
                assertFalse(bs.get(i), "Shouldn't have set bit " + i);
            }
        }

        for (int i = 9; i < 220; i++) {
            assertTrue(bs.get(i), "failed to set bit " + i);
        }

        for (int i = 220; i < 255; i++) {
            assertFalse(bs.get(i), "Shouldn't have set bit " + i);
        }

        assertTrue(bs.get(255), "Shouldn't have flipped bit 255");

        // test illegal args
        bs = newInstance(10);
        try {
            bs.setInclusive(-1, 3);
            fail("Test1: Attempt to flip with  negative index failed to generate exception");
        } catch (final IndexOutOfBoundsException e) {
            // Correct behavior
        }

        try {
            bs.setInclusive(2, -1);
            fail("Test2: Attempt to flip with negative index failed to generate exception");
        } catch (final IndexOutOfBoundsException e) {
            // Correct behavior
        }

        bs.setInclusive(2, 2);
        assertFalse(bs.get(3), "Bit got set incorrectly ");

        try {
            bs.setInclusive(4, 2);
            fail("Test4: Attempt to flip with illegal args failed to generate exception");
        } catch (final IndexOutOfBoundsException e) {
            // Correct behavior
        }
    }

    /**
     * Tests {@link FluentBitSet#set(int)}.
     */
    @Test
    public void test_setInt() {
        // Test for method void java.util.BitSet.set(int)

        FluentBitSet bs = newInstance();
        bs.set(8);
        assertTrue(bs.get(8), "Failed to set bit");

        try {
            bs.set(-1);
            fail("Attempt to set at negative index failed to generate exception");
        } catch (final IndexOutOfBoundsException e) {
            // Correct behavior
        }

        // Try setting a bit on a 64 boundary
        bs.set(128);
        assertEquals(192, bs.size(), "Failed to grow BitSet");
        assertTrue(bs.get(128), "Failed to set bit");

        bs = newInstance(64);
        for (int i = bs.size(); --i >= 0;) {
            bs.set(i);
            assertTrue(bs.get(i), "Incorrectly set");
            assertEquals(i + 1, bs.length(), "Incorrect length");
            for (int j = bs.size(); --j > i;) {
                assertFalse(bs.get(j), "Incorrectly set bit " + j);
            }
            for (int j = i; --j >= 0;) {
                assertFalse(bs.get(j), "Incorrectly set bit " + j);
            }
            bs.clear(i);
        }

        bs = newInstance(0);
        assertEquals(0, bs.length(), "Test1: Wrong length");
        bs.set(0);
        assertEquals(1, bs.length(), "Test2: Wrong length");
    }

    /**
     * Tests {@link FluentBitSet#set(int...)}.
     */
    @Test
    public void test_setIntArray() {
        // Test for method void java.util.BitSet.set(int)

        FluentBitSet bs = newInstance();
        bs.set(new int[] {8});
        assertTrue(bs.get(8), "Failed to set bit");

        try {
            bs.set(new int[] {-1});
            fail("Attempt to set at negative index failed to generate exception");
        } catch (final IndexOutOfBoundsException e) {
            // Correct behavior
        }

        // Try setting a bit on a 64 boundary
        bs.set(new int[] {128});
        assertEquals(192, bs.size(), "Failed to grow BitSet");
        assertTrue(bs.get(128), "Failed to set bit");

        bs = newInstance(64);
        for (int i = bs.size(); --i >= 0;) {
            bs.set(new int[] {i});
            assertTrue(bs.get(i), "Incorrectly set");
            assertEquals(i + 1, bs.length(), "Incorrect length");
            for (int j = bs.size(); --j > i;) {
                assertFalse(bs.get(j), "Incorrectly set bit " + j);
            }
            for (int j = i; --j >= 0;) {
                assertFalse(bs.get(j), "Incorrectly set bit " + j);
            }
            bs.clear(i);
        }

        bs = newInstance(0);
        assertEquals(0, bs.length(), "Test1: Wrong length");
        bs.set(new int[] {0});
        assertEquals(1, bs.length(), "Test2: Wrong length");
    }

    /**
     * Tests {@link FluentBitSet#set(int, boolean)}.
     */
    @Test
    public void test_setIZ() {
        // Test for method void java.util.BitSet.set(int, boolean)
        eightFbs.set(5, false);
        assertFalse(eightFbs.get(5), "Should have set bit 5 to true");

        eightFbs.set(5, true);
        assertTrue(eightFbs.get(5), "Should have set bit 5 to false");
    }

    /**
     * Tests {@link FluentBitSet#setInclusive(int, int)}.
     */
    @Test
    public void test_setRangeInclusive() {
        // Test for method int java.util.BitSet.size()
        assertEquals(64, eightFbs.size(), "Returned incorrect size");
        eightFbs.set(129);
        assertTrue(eightFbs.size() >= 129, "Returned incorrect size");

    }

    /**
     * Tests {@link FluentBitSet#size()}.
     */
    @Test
    public void test_size() {
        // Test for method int java.util.BitSet.size()
        assertEquals(64, eightFbs.size(), "Returned incorrect size");
        eightFbs.set(129);
        assertTrue(eightFbs.size() >= 129, "Returned incorrect size");

    }

    /**
     * Tests {@link FluentBitSet#previousSetBit(int)}.
     */
    @Test
    public void test_stream() {
        final FluentBitSet bs = newInstance();
        assertEquals(0, bs.stream().count(), "stream");
    }

    /**
     * Tests {@link FluentBitSet#previousSetBit(int)}.
     */
    @Test
    public void test_toByteArray() {
        final FluentBitSet bs = newInstance();
        assertArrayEquals(ArrayUtils.EMPTY_BYTE_ARRAY, bs.toByteArray(), "stream");
    }

    /**
     * Tests {@link FluentBitSet#previousSetBit(int)}.
     */
    @Test
    public void test_toLongArray() {
        final FluentBitSet bs = newInstance();
        assertArrayEquals(ArrayUtils.EMPTY_LONG_ARRAY, bs.toLongArray(), "stream");
    }

    /**
     * Tests {@link FluentBitSet#toString()}.
     */
    @Test
    public void test_toString() {
        // Test for method java.lang.String java.util.BitSet.toString()
        assertEquals("{0, 1, 2, 3, 4, 5, 6, 7}", eightFbs.toString(), "Returned incorrect string representation");
        eightFbs.clear(2);
        assertEquals("{0, 1, 3, 4, 5, 6, 7}", eightFbs.toString(), "Returned incorrect string representation");
    }

    /**
     * Tests {@link FluentBitSet#xor(FluentBitSet)}.
     */
    @Test
    public void test_xor() {
        // Test for method void java.util.BitSet.xor(BitSet)

        FluentBitSet bs = (FluentBitSet) eightFbs.clone();
        bs.xor(eightFbs);
        for (int i = 0; i < 8; i++) {
            assertFalse(bs.get(i), "XOR failed to clear bits");
        }

        bs.xor(eightFbs);
        for (int i = 0; i < 8; i++) {
            assertTrue(bs.get(i), "XOR failed to set bits");
        }

        bs = newInstance(0);
        bs.xor(eightFbs);
        for (int i = 0; i < 8; i++) {
            assertTrue(bs.get(i), "XOR(0) failed to set bits");
        }

        bs = newInstance();
        bs.set(63);
        assertEquals("{63}", bs.toString(), "Test highest bit");
    }

    /**
     * Tests {@link FluentBitSet#xor(BitSet)}.
     */
    @Test
    public void test_xor_BitSet() {
        // Test for method void java.util.BitSet.xor(BitSet)

        FluentBitSet bs = (FluentBitSet) eightFbs.clone();
        bs.xor(eightFbs.bitSet());
        for (int i = 0; i < 8; i++) {
            assertFalse(bs.get(i), "XOR failed to clear bits");
        }

        bs.xor(eightFbs.bitSet());
        for (int i = 0; i < 8; i++) {
            assertTrue(bs.get(i), "XOR failed to set bits");
        }

        bs = newInstance(0);
        bs.xor(eightFbs.bitSet());
        for (int i = 0; i < 8; i++) {
            assertTrue(bs.get(i), "XOR(0) failed to set bits");
        }

        bs = newInstance();
        bs.set(63);
        assertEquals("{63}", bs.toString(), "Test highest bit");
    }

}
