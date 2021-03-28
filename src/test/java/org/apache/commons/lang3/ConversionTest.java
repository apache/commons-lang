/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang3;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.Test;


/**
 * Unit tests {@link Conversion}.
 */
public class ConversionTest {

    /**
     * Tests {@link Conversion#hexDigitToInt(char)}.
     */
    @Test
    public void testHexDigitToInt() {
        assertEquals(0, Conversion.hexDigitToInt('0'));
        assertEquals(1, Conversion.hexDigitToInt('1'));
        assertEquals(2, Conversion.hexDigitToInt('2'));
        assertEquals(3, Conversion.hexDigitToInt('3'));
        assertEquals(4, Conversion.hexDigitToInt('4'));
        assertEquals(5, Conversion.hexDigitToInt('5'));
        assertEquals(6, Conversion.hexDigitToInt('6'));
        assertEquals(7, Conversion.hexDigitToInt('7'));
        assertEquals(8, Conversion.hexDigitToInt('8'));
        assertEquals(9, Conversion.hexDigitToInt('9'));
        assertEquals(10, Conversion.hexDigitToInt('A'));
        assertEquals(10, Conversion.hexDigitToInt('a'));
        assertEquals(11, Conversion.hexDigitToInt('B'));
        assertEquals(11, Conversion.hexDigitToInt('b'));
        assertEquals(12, Conversion.hexDigitToInt('C'));
        assertEquals(12, Conversion.hexDigitToInt('c'));
        assertEquals(13, Conversion.hexDigitToInt('D'));
        assertEquals(13, Conversion.hexDigitToInt('d'));
        assertEquals(14, Conversion.hexDigitToInt('E'));
        assertEquals(14, Conversion.hexDigitToInt('e'));
        assertEquals(15, Conversion.hexDigitToInt('F'));
        assertEquals(15, Conversion.hexDigitToInt('f'));
        assertThrows(IllegalArgumentException.class, () -> Conversion.hexDigitToInt('G'));
    }

    /**
     * Tests {@link Conversion#hexDigitMsb0ToInt(char)}.
     */
    @Test
    public void testHexDigitMsb0ToInt() {
        assertEquals(0x0, Conversion.hexDigitMsb0ToInt('0'));
        assertEquals(0x8, Conversion.hexDigitMsb0ToInt('1'));
        assertEquals(0x4, Conversion.hexDigitMsb0ToInt('2'));
        assertEquals(0xC, Conversion.hexDigitMsb0ToInt('3'));
        assertEquals(0x2, Conversion.hexDigitMsb0ToInt('4'));
        assertEquals(0xA, Conversion.hexDigitMsb0ToInt('5'));
        assertEquals(0x6, Conversion.hexDigitMsb0ToInt('6'));
        assertEquals(0xE, Conversion.hexDigitMsb0ToInt('7'));
        assertEquals(0x1, Conversion.hexDigitMsb0ToInt('8'));
        assertEquals(0x9, Conversion.hexDigitMsb0ToInt('9'));
        assertEquals(0x5, Conversion.hexDigitMsb0ToInt('A'));
        assertEquals(0x5, Conversion.hexDigitMsb0ToInt('a'));
        assertEquals(0xD, Conversion.hexDigitMsb0ToInt('B'));
        assertEquals(0xD, Conversion.hexDigitMsb0ToInt('b'));
        assertEquals(0x3, Conversion.hexDigitMsb0ToInt('C'));
        assertEquals(0x3, Conversion.hexDigitMsb0ToInt('c'));
        assertEquals(0xB, Conversion.hexDigitMsb0ToInt('D'));
        assertEquals(0xB, Conversion.hexDigitMsb0ToInt('d'));
        assertEquals(0x7, Conversion.hexDigitMsb0ToInt('E'));
        assertEquals(0x7, Conversion.hexDigitMsb0ToInt('e'));
        assertEquals(0xF, Conversion.hexDigitMsb0ToInt('F'));
        assertEquals(0xF, Conversion.hexDigitMsb0ToInt('f'));
        assertThrows(IllegalArgumentException.class, () -> Conversion.hexDigitMsb0ToInt('G'));
    }

    /**
     * Tests {@link Conversion#hexDigitToBinary(char)}.
     */
    @Test
    public void testHexDigitToBinary() {
        assertArrayEquals(
            new boolean[]{false, false, false, false}, Conversion.hexDigitToBinary('0'));
        assertArrayEquals(
            new boolean[]{true, false, false, false}, Conversion.hexDigitToBinary('1'));
        assertArrayEquals(
            new boolean[]{false, true, false, false}, Conversion.hexDigitToBinary('2'));
        assertArrayEquals(
            new boolean[]{true, true, false, false}, Conversion.hexDigitToBinary('3'));
        assertArrayEquals(
            new boolean[]{false, false, true, false}, Conversion.hexDigitToBinary('4'));
        assertArrayEquals(
            new boolean[]{true, false, true, false}, Conversion.hexDigitToBinary('5'));
        assertArrayEquals(
            new boolean[]{false, true, true, false}, Conversion.hexDigitToBinary('6'));
        assertArrayEquals(
            new boolean[]{true, true, true, false}, Conversion.hexDigitToBinary('7'));
        assertArrayEquals(
            new boolean[]{false, false, false, true}, Conversion.hexDigitToBinary('8'));
        assertArrayEquals(
            new boolean[]{true, false, false, true}, Conversion.hexDigitToBinary('9'));
        assertArrayEquals(
            new boolean[]{false, true, false, true}, Conversion.hexDigitToBinary('A'));
        assertArrayEquals(
            new boolean[]{false, true, false, true}, Conversion.hexDigitToBinary('a'));
        assertArrayEquals(
            new boolean[]{true, true, false, true}, Conversion.hexDigitToBinary('B'));
        assertArrayEquals(
            new boolean[]{true, true, false, true}, Conversion.hexDigitToBinary('b'));
        assertArrayEquals(
            new boolean[]{false, false, true, true}, Conversion.hexDigitToBinary('C'));
        assertArrayEquals(
            new boolean[]{false, false, true, true}, Conversion.hexDigitToBinary('c'));
        assertArrayEquals(
            new boolean[]{true, false, true, true}, Conversion.hexDigitToBinary('D'));
        assertArrayEquals(
            new boolean[]{true, false, true, true}, Conversion.hexDigitToBinary('d'));
        assertArrayEquals(
            new boolean[]{false, true, true, true}, Conversion.hexDigitToBinary('E'));
        assertArrayEquals(
            new boolean[]{false, true, true, true}, Conversion.hexDigitToBinary('e'));
        assertArrayEquals(
            new boolean[]{true, true, true, true}, Conversion.hexDigitToBinary('F'));
        assertArrayEquals(
            new boolean[]{true, true, true, true}, Conversion.hexDigitToBinary('f'));
        assertThrows(IllegalArgumentException.class, () -> Conversion.hexDigitToBinary('G'));
    }

    /**
     * Tests {@link Conversion#hexDigitMsb0ToBinary(char)}.
     */
    @Test
    public void testHexDigitMsb0ToBinary() {
        assertArrayEquals(
            new boolean[]{false, false, false, false}, Conversion.hexDigitMsb0ToBinary('0'));
        assertArrayEquals(
            new boolean[]{false, false, false, true}, Conversion.hexDigitMsb0ToBinary('1'));
        assertArrayEquals(
            new boolean[]{false, false, true, false}, Conversion.hexDigitMsb0ToBinary('2'));
        assertArrayEquals(
            new boolean[]{false, false, true, true}, Conversion.hexDigitMsb0ToBinary('3'));
        assertArrayEquals(
            new boolean[]{false, true, false, false}, Conversion.hexDigitMsb0ToBinary('4'));
        assertArrayEquals(
            new boolean[]{false, true, false, true}, Conversion.hexDigitMsb0ToBinary('5'));
        assertArrayEquals(
            new boolean[]{false, true, true, false}, Conversion.hexDigitMsb0ToBinary('6'));
        assertArrayEquals(
            new boolean[]{false, true, true, true}, Conversion.hexDigitMsb0ToBinary('7'));
        assertArrayEquals(
            new boolean[]{true, false, false, false}, Conversion.hexDigitMsb0ToBinary('8'));
        assertArrayEquals(
            new boolean[]{true, false, false, true}, Conversion.hexDigitMsb0ToBinary('9'));
        assertArrayEquals(
            new boolean[]{true, false, true, false}, Conversion.hexDigitMsb0ToBinary('A'));
        assertArrayEquals(
            new boolean[]{true, false, true, false}, Conversion.hexDigitMsb0ToBinary('a'));
        assertArrayEquals(
            new boolean[]{true, false, true, true}, Conversion.hexDigitMsb0ToBinary('B'));
        assertArrayEquals(
            new boolean[]{true, false, true, true}, Conversion.hexDigitMsb0ToBinary('b'));
        assertArrayEquals(
            new boolean[]{true, true, false, false}, Conversion.hexDigitMsb0ToBinary('C'));
        assertArrayEquals(
            new boolean[]{true, true, false, false}, Conversion.hexDigitMsb0ToBinary('c'));
        assertArrayEquals(
            new boolean[]{true, true, false, true}, Conversion.hexDigitMsb0ToBinary('D'));
        assertArrayEquals(
            new boolean[]{true, true, false, true}, Conversion.hexDigitMsb0ToBinary('d'));
        assertArrayEquals(
            new boolean[]{true, true, true, false}, Conversion.hexDigitMsb0ToBinary('E'));
        assertArrayEquals(
            new boolean[]{true, true, true, false}, Conversion.hexDigitMsb0ToBinary('e'));
        assertArrayEquals(
            new boolean[]{true, true, true, true}, Conversion.hexDigitMsb0ToBinary('F'));
        assertArrayEquals(
            new boolean[]{true, true, true, true}, Conversion.hexDigitMsb0ToBinary('f'));
        assertThrows(IllegalArgumentException.class, () -> Conversion.hexDigitMsb0ToBinary('G'));
    }

    /**
     * Tests {@link Conversion#binaryToHexDigit(boolean[])}.
     */
    @Test
    public void testBinaryToHexDigit() {
        assertEquals(
            '0', Conversion.binaryToHexDigit(new boolean[]{false, false, false, false}));
        assertEquals('1', Conversion.binaryToHexDigit(new boolean[]{true, false, false, false}));
        assertEquals('2', Conversion.binaryToHexDigit(new boolean[]{false, true, false, false}));
        assertEquals('3', Conversion.binaryToHexDigit(new boolean[]{true, true, false, false}));
        assertEquals('4', Conversion.binaryToHexDigit(new boolean[]{false, false, true, false}));
        assertEquals('5', Conversion.binaryToHexDigit(new boolean[]{true, false, true, false}));
        assertEquals('6', Conversion.binaryToHexDigit(new boolean[]{false, true, true, false}));
        assertEquals('7', Conversion.binaryToHexDigit(new boolean[]{true, true, true, false}));
        assertEquals('8', Conversion.binaryToHexDigit(new boolean[]{false, false, false, true}));
        assertEquals('9', Conversion.binaryToHexDigit(new boolean[]{true, false, false, true}));
        assertEquals('a', Conversion.binaryToHexDigit(new boolean[]{false, true, false, true}));
        assertEquals('b', Conversion.binaryToHexDigit(new boolean[]{true, true, false, true}));
        assertEquals('c', Conversion.binaryToHexDigit(new boolean[]{false, false, true, true}));
        assertEquals('d', Conversion.binaryToHexDigit(new boolean[]{true, false, true, true}));
        assertEquals('e', Conversion.binaryToHexDigit(new boolean[]{false, true, true, true}));
        assertEquals('f', Conversion.binaryToHexDigit(new boolean[]{true, true, true, true}));
        assertEquals('1', Conversion.binaryToHexDigit(new boolean[]{true}));
        assertEquals(
            'f', Conversion.binaryToHexDigit(new boolean[]{true, true, true, true, true}));
        assertThrows(IllegalArgumentException.class, () -> Conversion.binaryToHexDigit(new boolean[]{}));
    }

    /**
     * Tests {@link Conversion#binaryBeMsb0ToHexDigit(boolean[], int)}.
     */
    @Test
    public void testBinaryToHexDigit_2args() {
        final boolean[] shortArray = new boolean[]{false, true, true};
        assertEquals('6', Conversion.binaryToHexDigit(shortArray, 0));
        assertEquals('3', Conversion.binaryToHexDigit(shortArray, 1));
        assertEquals('1', Conversion.binaryToHexDigit(shortArray, 2));
        final boolean[] longArray = new boolean[]{true, false, true, false, false, true, true};
        assertEquals('5', Conversion.binaryToHexDigit(longArray, 0));
        assertEquals('2', Conversion.binaryToHexDigit(longArray, 1));
        assertEquals('9', Conversion.binaryToHexDigit(longArray, 2));
        assertEquals('c', Conversion.binaryToHexDigit(longArray, 3));
        assertEquals('6', Conversion.binaryToHexDigit(longArray, 4));
        assertEquals('3', Conversion.binaryToHexDigit(longArray, 5));
        assertEquals('1', Conversion.binaryToHexDigit(longArray, 6));
    }

    /**
     * Tests {@link Conversion#binaryToHexDigitMsb0_4bits(boolean[])}.
     */
    @Test
    public void testBinaryToHexDigitMsb0_bits() {
        assertEquals(
            '0',
            Conversion.binaryToHexDigitMsb0_4bits(new boolean[]{false, false, false, false}));
        assertEquals(
            '1',
            Conversion.binaryToHexDigitMsb0_4bits(new boolean[]{false, false, false, true}));
        assertEquals(
            '2',
            Conversion.binaryToHexDigitMsb0_4bits(new boolean[]{false, false, true, false}));
        assertEquals(
            '3', Conversion.binaryToHexDigitMsb0_4bits(new boolean[]{false, false, true, true}));
        assertEquals(
            '4',
            Conversion.binaryToHexDigitMsb0_4bits(new boolean[]{false, true, false, false}));
        assertEquals(
            '5', Conversion.binaryToHexDigitMsb0_4bits(new boolean[]{false, true, false, true}));
        assertEquals(
            '6', Conversion.binaryToHexDigitMsb0_4bits(new boolean[]{false, true, true, false}));
        assertEquals(
            '7', Conversion.binaryToHexDigitMsb0_4bits(new boolean[]{false, true, true, true}));
        assertEquals(
            '8',
            Conversion.binaryToHexDigitMsb0_4bits(new boolean[]{true, false, false, false}));
        assertEquals(
            '9', Conversion.binaryToHexDigitMsb0_4bits(new boolean[]{true, false, false, true}));
        assertEquals(
            'a', Conversion.binaryToHexDigitMsb0_4bits(new boolean[]{true, false, true, false}));
        assertEquals(
            'b', Conversion.binaryToHexDigitMsb0_4bits(new boolean[]{true, false, true, true}));
        assertEquals(
            'c', Conversion.binaryToHexDigitMsb0_4bits(new boolean[]{true, true, false, false}));
        assertEquals(
            'd', Conversion.binaryToHexDigitMsb0_4bits(new boolean[]{true, true, false, true}));
        assertEquals(
            'e', Conversion.binaryToHexDigitMsb0_4bits(new boolean[]{true, true, true, false}));
        assertEquals(
            'f', Conversion.binaryToHexDigitMsb0_4bits(new boolean[]{true, true, true, true}));
        assertThrows(IllegalArgumentException.class, () -> Conversion.binaryToHexDigitMsb0_4bits(new boolean[]{}));
    }

    /**
     * Tests {@link Conversion#binaryToHexDigitMsb0_4bits(boolean[], int)}.
     */
    @Test
    public void testBinaryToHexDigitMsb0_4bits_2args() {
        // boolean[] shortArray = new boolean[]{true, true, false};
        // assertEquals('6', Conversion.BinaryToHexDigitMsb0(shortArray, 0));
        // assertEquals('3', Conversion.BinaryToHexDigitMsb0(shortArray, 1));
        // assertEquals('1', Conversion.BinaryToHexDigitMsb0(shortArray, 2));
        final boolean[] shortArray = new boolean[]{true, true, false, true};
        assertEquals('d', Conversion.binaryToHexDigitMsb0_4bits(shortArray, 0));
        final boolean[] longArray = new boolean[]{true, false, true, false, false, true, true};
        assertEquals('a', Conversion.binaryToHexDigitMsb0_4bits(longArray, 0));
        assertEquals('4', Conversion.binaryToHexDigitMsb0_4bits(longArray, 1));
        assertEquals('9', Conversion.binaryToHexDigitMsb0_4bits(longArray, 2));
        assertEquals('3', Conversion.binaryToHexDigitMsb0_4bits(longArray, 3));
        // assertEquals('6', Conversion.BinaryToHexDigitMsb0(longArray, 4));
        // assertEquals('3', Conversion.BinaryToHexDigitMsb0(longArray, 5));
        // assertEquals('1', Conversion.BinaryToHexDigitMsb0(longArray, 6));
        final boolean[] maxLengthArray = new boolean[]{
            true, false, true, false, false, true, true, true};
        assertEquals('a', Conversion.binaryToHexDigitMsb0_4bits(maxLengthArray, 0));
        assertEquals('4', Conversion.binaryToHexDigitMsb0_4bits(maxLengthArray, 1));
        assertEquals('9', Conversion.binaryToHexDigitMsb0_4bits(maxLengthArray, 2));
        assertEquals('3', Conversion.binaryToHexDigitMsb0_4bits(maxLengthArray, 3));
        assertEquals('7', Conversion.binaryToHexDigitMsb0_4bits(maxLengthArray, 4));
        // assertEquals('7', Conversion.BinaryToHexDigitMsb0(longArray, 5));
        // assertEquals('3', Conversion.BinaryToHexDigitMsb0(longArray, 6));
        // assertEquals('1', Conversion.BinaryToHexDigitMsb0(longArray, 7));
        final boolean[] javaDocCheck = new boolean[]{
            true, false, false, true, true, false, true, false};
        assertEquals('d', Conversion.binaryToHexDigitMsb0_4bits(javaDocCheck, 3));

    }

    /**
     * Tests {@link Conversion#binaryToHexDigit(boolean[])}.
     */
    @Test
    public void testBinaryBeMsb0ToHexDigit() {
        assertEquals(
            '0', Conversion.binaryBeMsb0ToHexDigit(new boolean[]{false, false, false, false}));
        assertEquals(
            '1', Conversion.binaryBeMsb0ToHexDigit(new boolean[]{false, false, false, true}));
        assertEquals(
            '2', Conversion.binaryBeMsb0ToHexDigit(new boolean[]{false, false, true, false}));
        assertEquals(
            '3', Conversion.binaryBeMsb0ToHexDigit(new boolean[]{false, false, true, true}));
        assertEquals(
            '4', Conversion.binaryBeMsb0ToHexDigit(new boolean[]{false, true, false, false}));
        assertEquals(
            '5', Conversion.binaryBeMsb0ToHexDigit(new boolean[]{false, true, false, true}));
        assertEquals(
            '6', Conversion.binaryBeMsb0ToHexDigit(new boolean[]{false, true, true, false}));
        assertEquals(
            '7', Conversion.binaryBeMsb0ToHexDigit(new boolean[]{false, true, true, true}));
        assertEquals(
            '8', Conversion.binaryBeMsb0ToHexDigit(new boolean[]{true, false, false, false}));
        assertEquals(
            '9', Conversion.binaryBeMsb0ToHexDigit(new boolean[]{true, false, false, true}));
        assertEquals(
            'a', Conversion.binaryBeMsb0ToHexDigit(new boolean[]{true, false, true, false}));
        assertEquals(
            'b', Conversion.binaryBeMsb0ToHexDigit(new boolean[]{true, false, true, true}));
        assertEquals(
            'c', Conversion.binaryBeMsb0ToHexDigit(new boolean[]{true, true, false, false}));
        assertEquals(
            'd', Conversion.binaryBeMsb0ToHexDigit(new boolean[]{true, true, false, true}));
        assertEquals(
            'e', Conversion.binaryBeMsb0ToHexDigit(new boolean[]{true, true, true, false}));
        assertEquals(
            'f', Conversion.binaryBeMsb0ToHexDigit(new boolean[]{true, true, true, true}));
        assertEquals(
            '4',
            Conversion.binaryBeMsb0ToHexDigit(new boolean[]{
                true, false, false, false, false, false, false, false, false, false, false,
                false, false, true, false, false}));
        assertThrows(IllegalArgumentException.class, () -> Conversion.binaryBeMsb0ToHexDigit(new boolean[]{}));
    }

    /**
     * Tests {@link Conversion#binaryToHexDigit(boolean[], int)}.
     */
    @Test
    public void testBinaryBeMsb0ToHexDigit_2args() {
        assertEquals(
            '5',
            Conversion.binaryBeMsb0ToHexDigit(new boolean[]{
                true, false, false, false, false, false, false, false, false, false, false,
                true, false, true, false, false}, 2));

        final boolean[] shortArray = new boolean[]{true, true, false};
        assertEquals('6', Conversion.binaryBeMsb0ToHexDigit(shortArray, 0));
        assertEquals('3', Conversion.binaryBeMsb0ToHexDigit(shortArray, 1));
        assertEquals('1', Conversion.binaryBeMsb0ToHexDigit(shortArray, 2));
        final boolean[] shortArray2 = new boolean[]{true, true, true, false, false, true, false, true};
        assertEquals('5', Conversion.binaryBeMsb0ToHexDigit(shortArray2, 0));
        assertEquals('2', Conversion.binaryBeMsb0ToHexDigit(shortArray2, 1));
        assertEquals('9', Conversion.binaryBeMsb0ToHexDigit(shortArray2, 2));
        assertEquals('c', Conversion.binaryBeMsb0ToHexDigit(shortArray2, 3));
        assertEquals('e', Conversion.binaryBeMsb0ToHexDigit(shortArray2, 4));
        assertEquals('7', Conversion.binaryBeMsb0ToHexDigit(shortArray2, 5));
        assertEquals('3', Conversion.binaryBeMsb0ToHexDigit(shortArray2, 6));
        assertEquals('1', Conversion.binaryBeMsb0ToHexDigit(shortArray2, 7));
        final boolean[] multiBytesArray = new boolean[]{
            true, true, false, false, true, false, true, false, true, true, true, false, false,
            true, false, true};
        assertEquals('5', Conversion.binaryBeMsb0ToHexDigit(multiBytesArray, 0));
        assertEquals('2', Conversion.binaryBeMsb0ToHexDigit(multiBytesArray, 1));
        assertEquals('9', Conversion.binaryBeMsb0ToHexDigit(multiBytesArray, 2));
        assertEquals('c', Conversion.binaryBeMsb0ToHexDigit(multiBytesArray, 3));
        assertEquals('e', Conversion.binaryBeMsb0ToHexDigit(multiBytesArray, 4));
        assertEquals('7', Conversion.binaryBeMsb0ToHexDigit(multiBytesArray, 5));
        assertEquals('b', Conversion.binaryBeMsb0ToHexDigit(multiBytesArray, 6));
        assertEquals('5', Conversion.binaryBeMsb0ToHexDigit(multiBytesArray, 7));

        assertEquals('a', Conversion.binaryBeMsb0ToHexDigit(multiBytesArray, 8));
        assertEquals('5', Conversion.binaryBeMsb0ToHexDigit(multiBytesArray, 9));
        assertEquals('2', Conversion.binaryBeMsb0ToHexDigit(multiBytesArray, 10));
        assertEquals('9', Conversion.binaryBeMsb0ToHexDigit(multiBytesArray, 11));
        assertEquals('c', Conversion.binaryBeMsb0ToHexDigit(multiBytesArray, 12));
        assertEquals('6', Conversion.binaryBeMsb0ToHexDigit(multiBytesArray, 13));
        assertEquals('3', Conversion.binaryBeMsb0ToHexDigit(multiBytesArray, 14));
        assertEquals('1', Conversion.binaryBeMsb0ToHexDigit(multiBytesArray, 15));

    }

    /**
     * Tests {@link Conversion#intToHexDigit(int)}.
     */
    @Test
    public void testIntToHexDigit() {
        assertEquals('0', Conversion.intToHexDigit(0));
        assertEquals('1', Conversion.intToHexDigit(1));
        assertEquals('2', Conversion.intToHexDigit(2));
        assertEquals('3', Conversion.intToHexDigit(3));
        assertEquals('4', Conversion.intToHexDigit(4));
        assertEquals('5', Conversion.intToHexDigit(5));
        assertEquals('6', Conversion.intToHexDigit(6));
        assertEquals('7', Conversion.intToHexDigit(7));
        assertEquals('8', Conversion.intToHexDigit(8));
        assertEquals('9', Conversion.intToHexDigit(9));
        assertEquals('a', Conversion.intToHexDigit(10));
        assertEquals('b', Conversion.intToHexDigit(11));
        assertEquals('c', Conversion.intToHexDigit(12));
        assertEquals('d', Conversion.intToHexDigit(13));
        assertEquals('e', Conversion.intToHexDigit(14));
        assertEquals('f', Conversion.intToHexDigit(15));
        assertThrows(IllegalArgumentException.class, () -> Conversion.intToHexDigit(16));
    }

    /**
     * Tests {@link Conversion#intToHexDigitMsb0(int)}.
     */
    @Test
    public void testIntToHexDigitMsb0() {
        assertEquals('0', Conversion.intToHexDigitMsb0(0));
        assertEquals('8', Conversion.intToHexDigitMsb0(1));
        assertEquals('4', Conversion.intToHexDigitMsb0(2));
        assertEquals('c', Conversion.intToHexDigitMsb0(3));
        assertEquals('2', Conversion.intToHexDigitMsb0(4));
        assertEquals('a', Conversion.intToHexDigitMsb0(5));
        assertEquals('6', Conversion.intToHexDigitMsb0(6));
        assertEquals('e', Conversion.intToHexDigitMsb0(7));
        assertEquals('1', Conversion.intToHexDigitMsb0(8));
        assertEquals('9', Conversion.intToHexDigitMsb0(9));
        assertEquals('5', Conversion.intToHexDigitMsb0(10));
        assertEquals('d', Conversion.intToHexDigitMsb0(11));
        assertEquals('3', Conversion.intToHexDigitMsb0(12));
        assertEquals('b', Conversion.intToHexDigitMsb0(13));
        assertEquals('7', Conversion.intToHexDigitMsb0(14));
        assertEquals('f', Conversion.intToHexDigitMsb0(15));
        assertThrows(IllegalArgumentException.class, () -> Conversion.intToHexDigitMsb0(16));
    }

    static String dbgPrint(final boolean[] src) {
        final StringBuilder sb = new StringBuilder();
        for (final boolean e : src) {
            if (e) {
                sb.append("1, ");
            } else {
                sb.append("0, ");
            }
        }
        final String out = sb.toString();
        return out.substring(0, out.length() - 1);
    }

    /**
     * Tests {@link Conversion#intArrayToLong(int[], int, long, int, int)}.
     */
    @Test
    public void testIntArrayToLong() {
        final int[] src = new int[]{0xCDF1F0C1, 0x0F123456, 0x78000000};
        assertEquals(0x0000000000000000L, Conversion.intArrayToLong(src, 0, 0L, 0, 0));
        assertEquals(0x0000000000000000L, Conversion.intArrayToLong(src, 1, 0L, 0, 0));
        assertEquals(0x00000000CDF1F0C1L, Conversion.intArrayToLong(src, 0, 0L, 0, 1));
        assertEquals(0x0F123456CDF1F0C1L, Conversion.intArrayToLong(src, 0, 0L, 0, 2));
        assertEquals(0x000000000F123456L, Conversion.intArrayToLong(src, 1, 0L, 0, 1));
        assertEquals(
            0x123456789ABCDEF0L, Conversion.intArrayToLong(src, 0, 0x123456789ABCDEF0L, 0, 0));
        assertEquals(
            0x1234567878000000L, Conversion.intArrayToLong(src, 2, 0x123456789ABCDEF0L, 0, 1));
        // assertEquals(0x0F12345678000000L, Conversion.intsToLong(src, 1, 0x123456789ABCDEF0L, 32, 2));
    }

    /**
     * Tests {@link Conversion#shortArrayToLong(short[], int, long, int, int)}.
     */
    @Test
    public void testShortArrayToLong() {
        final short[] src = new short[]{
            (short) 0xCDF1, (short) 0xF0C1, (short) 0x0F12, (short) 0x3456, (short) 0x7800};
        assertEquals(0x0000000000000000L, Conversion.shortArrayToLong(src, 0, 0L, 0, 0));
        assertEquals(0x000000000000CDF1L, Conversion.shortArrayToLong(src, 0, 0L, 0, 1));
        assertEquals(0x00000000F0C1CDF1L, Conversion.shortArrayToLong(src, 0, 0L, 0, 2));
        assertEquals(0x780034560F12F0C1L, Conversion.shortArrayToLong(src, 1, 0L, 0, 4));
        assertEquals(
            0x123456789ABCDEF0L, Conversion.shortArrayToLong(src, 0, 0x123456789ABCDEF0L, 0, 0));
        assertEquals(
            0x123456CDF1BCDEF0L,
            Conversion.shortArrayToLong(src, 0, 0x123456789ABCDEF0L, 24, 1));
        assertEquals(
            0x123478003456DEF0L,
            Conversion.shortArrayToLong(src, 3, 0x123456789ABCDEF0L, 16, 2));
    }

    /**
     * Tests {@link Conversion#byteArrayToLong(byte[], int, long, int, int)}.
     */
    @Test
    public void testByteArrayToLong() {
        final byte[] src = new byte[]{
            (byte) 0xCD, (byte) 0xF1, (byte) 0xF0, (byte) 0xC1, (byte) 0x0F, (byte) 0x12, (byte) 0x34,
            (byte) 0x56, (byte) 0x78};
        assertEquals(0x0000000000000000L, Conversion.byteArrayToLong(src, 0, 0L, 0, 0));
        assertEquals(0x00000000000000CDL, Conversion.byteArrayToLong(src, 0, 0L, 0, 1));
        assertEquals(0x00000000C1F0F1CDL, Conversion.byteArrayToLong(src, 0, 0L, 0, 4));
        assertEquals(0x000000000FC1F0F1L, Conversion.byteArrayToLong(src, 1, 0L, 0, 4));
        assertEquals(
            0x123456789ABCDEF0L, Conversion.byteArrayToLong(src, 0, 0x123456789ABCDEF0L, 0, 0));
        assertEquals(
            0x12345678CDBCDEF0L, Conversion.byteArrayToLong(src, 0, 0x123456789ABCDEF0L, 24, 1));
        assertEquals(
            0x123456789A7856F0L, Conversion.byteArrayToLong(src, 7, 0x123456789ABCDEF0L, 8, 2));
    }

    /**
     * Tests {@link Conversion#shortArrayToInt(short[], int, int, int, int)}.
     */
    @Test
    public void testShortArrayToInt() {
        final short[] src = new short[]{
            (short) 0xCDF1, (short) 0xF0C1, (short) 0x0F12, (short) 0x3456, (short) 0x7800};
        assertEquals(0x00000000, Conversion.shortArrayToInt(src, 0, 0, 0, 0));
        assertEquals(0x0000CDF1, Conversion.shortArrayToInt(src, 0, 0, 0, 1));
        assertEquals(0xF0C1CDF1, Conversion.shortArrayToInt(src, 0, 0, 0, 2));
        assertEquals(0x0F12F0C1, Conversion.shortArrayToInt(src, 1, 0, 0, 2));
        assertEquals(0x12345678, Conversion.shortArrayToInt(src, 0, 0x12345678, 0, 0));
        assertEquals(0xCDF15678, Conversion.shortArrayToInt(src, 0, 0x12345678, 16, 1));
        // assertEquals(0x34567800, Conversion.ShortArrayToInt(src, 3, 0x12345678, 16, 2));
    }

    /**
     * Tests {@link Conversion#byteArrayToInt(byte[], int, int, int, int)}.
     */
    @Test
    public void testByteArrayToInt() {
        final byte[] src = new byte[]{
            (byte) 0xCD, (byte) 0xF1, (byte) 0xF0, (byte) 0xC1, (byte) 0x0F, (byte) 0x12, (byte) 0x34,
            (byte) 0x56, (byte) 0x78};
        assertEquals(0x00000000, Conversion.byteArrayToInt(src, 0, 0, 0, 0));
        assertEquals(0x000000CD, Conversion.byteArrayToInt(src, 0, 0, 0, 1));
        assertEquals(0xC1F0F1CD, Conversion.byteArrayToInt(src, 0, 0, 0, 4));
        assertEquals(0x0FC1F0F1, Conversion.byteArrayToInt(src, 1, 0, 0, 4));
        assertEquals(0x12345678, Conversion.byteArrayToInt(src, 0, 0x12345678, 0, 0));
        assertEquals(0xCD345678, Conversion.byteArrayToInt(src, 0, 0x12345678, 24, 1));
        // assertEquals(0x56341278, Conversion.ByteArrayToInt(src, 5, 0x01234567, 8, 4));
    }

    /**
     * Tests {@link Conversion#byteArrayToShort(byte[], int, short, int, int)}.
     */
    @Test
    public void testByteArrayToShort() {
        final byte[] src = new byte[]{
            (byte) 0xCD, (byte) 0xF1, (byte) 0xF0, (byte) 0xC1, (byte) 0x0F, (byte) 0x12, (byte) 0x34,
            (byte) 0x56, (byte) 0x78};
        assertEquals((short) 0x0000, Conversion.byteArrayToShort(src, 0, (short) 0, 0, 0));
        assertEquals((short) 0x00CD, Conversion.byteArrayToShort(src, 0, (short) 0, 0, 1));
        assertEquals((short) 0xF1CD, Conversion.byteArrayToShort(src, 0, (short) 0, 0, 2));
        assertEquals((short) 0xF0F1, Conversion.byteArrayToShort(src, 1, (short) 0, 0, 2));
        assertEquals((short) 0x1234, Conversion.byteArrayToShort(src, 0, (short) 0x1234, 0, 0));
        assertEquals((short) 0xCD34, Conversion.byteArrayToShort(src, 0, (short) 0x1234, 8, 1));
        // assertEquals((short) 0x5678, Conversion.ByteArrayToShort(src, 7, (short) 0x0123, 8,
        // 2));
    }

    /**
     * Tests {@link Conversion#hexToLong(String, int, long, int, int)}.
     */
    @Test
    public void testHexToLong() {
        final String src = "CDF1F0C10F12345678";
        assertEquals(0x0000000000000000L, Conversion.hexToLong(src, 0, 0L, 0, 0));
        assertEquals(0x000000000000000CL, Conversion.hexToLong(src, 0, 0L, 0, 1));
        assertEquals(0x000000001C0F1FDCL, Conversion.hexToLong(src, 0, 0L, 0, 8));
        assertEquals(0x0000000001C0F1FDL, Conversion.hexToLong(src, 1, 0L, 0, 8));
        assertEquals(
            0x123456798ABCDEF0L, Conversion.hexToLong(src, 0, 0x123456798ABCDEF0L, 0, 0));
        assertEquals(
            0x1234567876BCDEF0L, Conversion.hexToLong(src, 15, 0x123456798ABCDEF0L, 24, 3));
    }

    /**
     * Tests {@link Conversion#hexToInt(String, int, int, int, int)}.
     */
    @Test
    public void testHexToInt() {
        final String src = "CDF1F0C10F12345678";
        assertEquals(0x00000000, Conversion.hexToInt(src, 0, 0, 0, 0));
        assertEquals(0x0000000C, Conversion.hexToInt(src, 0, 0, 0, 1));
        assertEquals(0x1C0F1FDC, Conversion.hexToInt(src, 0, 0, 0, 8));
        assertEquals(0x01C0F1FD, Conversion.hexToInt(src, 1, 0, 0, 8));
        assertEquals(0x12345679, Conversion.hexToInt(src, 0, 0x12345679, 0, 0));
        assertEquals(0x87645679, Conversion.hexToInt(src, 15, 0x12345679, 20, 3));
    }

    /**
     * Tests {@link Conversion#hexToShort(String, int, short, int, int)}.
     */
    @Test
    public void testHexToShort() {
        final String src = "CDF1F0C10F12345678";
        assertEquals((short) 0x0000, Conversion.hexToShort(src, 0, (short) 0, 0, 0));
        assertEquals((short) 0x000C, Conversion.hexToShort(src, 0, (short) 0, 0, 1));
        assertEquals((short) 0x1FDC, Conversion.hexToShort(src, 0, (short) 0, 0, 4));
        assertEquals((short) 0xF1FD, Conversion.hexToShort(src, 1, (short) 0, 0, 4));
        assertEquals((short) 0x1234, Conversion.hexToShort(src, 0, (short) 0x1234, 0, 0));
        assertEquals((short) 0x8764, Conversion.hexToShort(src, 15, (short) 0x1234, 4, 3));
    }

    /**
     * Tests {@link Conversion#hexToByte(String, int, byte, int, int)}.
     */
    @Test
    public void testHexToByte() {
        final String src = "CDF1F0C10F12345678";
        assertEquals((byte) 0x00, Conversion.hexToByte(src, 0, (byte) 0, 0, 0));
        assertEquals((byte) 0x0C, Conversion.hexToByte(src, 0, (byte) 0, 0, 1));
        assertEquals((byte) 0xDC, Conversion.hexToByte(src, 0, (byte) 0, 0, 2));
        assertEquals((byte) 0xFD, Conversion.hexToByte(src, 1, (byte) 0, 0, 2));
        assertEquals((byte) 0x34, Conversion.hexToByte(src, 0, (byte) 0x34, 0, 0));
        assertEquals((byte) 0x84, Conversion.hexToByte(src, 17, (byte) 0x34, 4, 1));
    }

    /**
     * Tests {@link Conversion#binaryToLong(boolean[], int, long, int, int)}.
     */
    @Test
    public void testBinaryToLong() {
        final boolean[] src = new boolean[]{
            false, false, true, true, true, false, true, true, true, true, true, true, true,
            false, false, false, true, true, true, true, false, false, false, false, false,
            false, true, true, true, false, false, false, false, false, false, false, true,
            true, true, true, true, false, false, false, false, true, false, false, true, true,
            false, false, false, false, true, false, true, false, true, false, false, true,
            true, false, true, true, true, false, false, false, false, true};
        // conversion of "CDF1F0C10F12345678" by HexToBinary
        assertEquals(0x0000000000000000L, Conversion.binaryToLong(src, 0, 0L, 0, 0));
        assertEquals(0x000000000000000CL, Conversion.binaryToLong(src, 0, 0L, 0, 1 * 4));
        assertEquals(0x000000001C0F1FDCL, Conversion.binaryToLong(src, 0, 0L, 0, 8 * 4));
        assertEquals(0x0000000001C0F1FDL, Conversion.binaryToLong(src, 1 * 4, 0L, 0, 8 * 4));
        assertEquals(
            0x123456798ABCDEF0L, Conversion.binaryToLong(src, 0, 0x123456798ABCDEF0L, 0, 0));
        assertEquals(
            0x1234567876BCDEF0L,
            Conversion.binaryToLong(src, 15 * 4, 0x123456798ABCDEF0L, 24, 3 * 4));
    }

    /**
     * Tests {@link Conversion#binaryToInt(boolean[], int, int, int, int)}.
     */
    @Test
    public void testBinaryToInt() {
        final boolean[] src = new boolean[]{
            false, false, true, true, true, false, true, true, true, true, true, true, true,
            false, false, false, true, true, true, true, false, false, false, false, false,
            false, true, true, true, false, false, false, false, false, false, false, true,
            true, true, true, true, false, false, false, false, true, false, false, true, true,
            false, false, false, false, true, false, true, false, true, false, false, true,
            true, false, true, true, true, false, false, false, false, true};
        // conversion of "CDF1F0C10F12345678" by HexToBinary
        assertEquals(0x00000000, Conversion.binaryToInt(src, 0 * 4, 0, 0, 0 * 4));
        assertEquals(0x0000000C, Conversion.binaryToInt(src, 0 * 4, 0, 0, 1 * 4));
        assertEquals(0x1C0F1FDC, Conversion.binaryToInt(src, 0 * 4, 0, 0, 8 * 4));
        assertEquals(0x01C0F1FD, Conversion.binaryToInt(src, 1 * 4, 0, 0, 8 * 4));
        assertEquals(0x12345679, Conversion.binaryToInt(src, 0 * 4, 0x12345679, 0, 0 * 4));
        assertEquals(0x87645679, Conversion.binaryToInt(src, 15 * 4, 0x12345679, 20, 3 * 4));
    }

    /**
     * Tests {@link Conversion#binaryToShort(boolean[], int, short, int, int)}.
     */
    @Test
    public void testBinaryToShort() {
        final boolean[] src = new boolean[]{
            false, false, true, true, true, false, true, true, true, true, true, true, true,
            false, false, false, true, true, true, true, false, false, false, false, false,
            false, true, true, true, false, false, false, false, false, false, false, true,
            true, true, true, true, false, false, false, false, true, false, false, true, true,
            false, false, false, false, true, false, true, false, true, false, false, true,
            true, false, true, true, true, false, false, false, false, true};
        // conversion of "CDF1F0C10F12345678" by HexToBinary
        assertEquals((short) 0x0000, Conversion.binaryToShort(src, 0 * 4, (short) 0, 0, 0 * 4));
        assertEquals((short) 0x000C, Conversion.binaryToShort(src, 0 * 4, (short) 0, 0, 1 * 4));
        assertEquals((short) 0x1FDC, Conversion.binaryToShort(src, 0 * 4, (short) 0, 0, 4 * 4));
        assertEquals((short) 0xF1FD, Conversion.binaryToShort(src, 1 * 4, (short) 0, 0, 4 * 4));
        assertEquals(
            (short) 0x1234, Conversion.binaryToShort(src, 0 * 4, (short) 0x1234, 0, 0 * 4));
        assertEquals(
            (short) 0x8764, Conversion.binaryToShort(src, 15 * 4, (short) 0x1234, 4, 3 * 4));
    }

    /**
     * Tests {@link Conversion#binaryToByte(boolean[], int, byte, int, int)}.
     */
    @Test
    public void testBinaryToByte() {
        final boolean[] src = new boolean[]{
            false, false, true, true, true, false, true, true, true, true, true, true, true,
            false, false, false, true, true, true, true, false, false, false, false, false,
            false, true, true, true, false, false, false, false, false, false, false, true,
            true, true, true, true, false, false, false, false, true, false, false, true, true,
            false, false, false, false, true, false, true, false, true, false, false, true,
            true, false, true, true, true, false, false, false, false, true};
        // conversion of "CDF1F0C10F12345678" by HexToBinary
        assertEquals((byte) 0x00, Conversion.binaryToByte(src, 0 * 4, (byte) 0, 0, 0 * 4));
        assertEquals((byte) 0x0C, Conversion.binaryToByte(src, 0 * 4, (byte) 0, 0, 1 * 4));
        assertEquals((byte) 0xDC, Conversion.binaryToByte(src, 0 * 4, (byte) 0, 0, 2 * 4));
        assertEquals((byte) 0xFD, Conversion.binaryToByte(src, 1 * 4, (byte) 0, 0, 2 * 4));
        assertEquals((byte) 0x34, Conversion.binaryToByte(src, 0 * 4, (byte) 0x34, 0, 0 * 4));
        assertEquals((byte) 0x84, Conversion.binaryToByte(src, 17 * 4, (byte) 0x34, 4, 1 * 4));
    }

    /**
     * Tests {@link Conversion#longToIntArray(long, int, int[], int, int)}.
     */
    @Test
    public void testLongToIntArray() {
        assertArrayEquals(
            new int[]{}, Conversion.longToIntArray(0x0000000000000000L, 0, new int[]{}, 0, 0));
        assertArrayEquals(
            new int[]{}, Conversion.longToIntArray(0x0000000000000000L, 100, new int[]{}, 0, 0));
        assertArrayEquals(
            new int[]{}, Conversion.longToIntArray(0x0000000000000000L, 0, new int[]{}, 100, 0));
        assertArrayEquals(
            new int[]{0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF},
            Conversion.longToIntArray(0x1234567890ABCDEFL, 0, new int[]{-1, -1, -1, -1}, 0, 0));
        assertArrayEquals(
            new int[]{0x90ABCDEF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF},
            Conversion.longToIntArray(0x1234567890ABCDEFL, 0, new int[]{-1, -1, -1, -1}, 0, 1));
        assertArrayEquals(
            new int[]{0x90ABCDEF, 0x12345678, 0xFFFFFFFF, 0xFFFFFFFF},
            Conversion.longToIntArray(0x1234567890ABCDEFL, 0, new int[]{-1, -1, -1, -1}, 0, 2));
        // assertArrayEquals(new
        // int[]{0x90ABCDEF, 0x12345678, 0x90ABCDEF, 0x12345678}, Conversion.longToIntArray(0x1234567890ABCDEFL,
        // 0, new int[]{-1, -1, -1, -1}, 0, 4));//rejected by assertion
        // assertArrayEquals(new
        // int[]{0xFFFFFFFF, 0x90ABCDEF, 0x12345678, 0x90ABCDEF}, Conversion.longToIntArray(0x1234567890ABCDEFL,
        // 0, new int[]{-1, -1, -1, -1}, 1, 3));
        assertArrayEquals(
            new int[]{0xFFFFFFFF, 0xFFFFFFFF, 0x90ABCDEF, 0x12345678},
            Conversion.longToIntArray(0x1234567890ABCDEFL, 0, new int[]{-1, -1, -1, -1}, 2, 2));
        assertArrayEquals(
            new int[]{0xFFFFFFFF, 0xFFFFFFFF, 0x90ABCDEF, 0xFFFFFFFF},
            Conversion.longToIntArray(0x1234567890ABCDEFL, 0, new int[]{-1, -1, -1, -1}, 2, 1));
        assertArrayEquals(
            new int[]{0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0x90ABCDEF},
            Conversion.longToIntArray(0x1234567890ABCDEFL, 0, new int[]{-1, -1, -1, -1}, 3, 1));
        assertArrayEquals(
            new int[]{0xFFFFFFFF, 0xFFFFFFFF, 0x4855E6F7, 0xFFFFFFFF},
            Conversion.longToIntArray(0x1234567890ABCDEFL, 1, new int[]{-1, -1, -1, -1}, 2, 1));
        assertArrayEquals(
            new int[]{0xFFFFFFFF, 0xFFFFFFFF, 0x242AF37B, 0xFFFFFFFF},
            Conversion.longToIntArray(0x1234567890ABCDEFL, 2, new int[]{-1, -1, -1, -1}, 2, 1));
        assertArrayEquals(
            new int[]{0xFFFFFFFF, 0xFFFFFFFF, 0x121579BD, 0xFFFFFFFF},
            Conversion.longToIntArray(0x1234567890ABCDEFL, 3, new int[]{-1, -1, -1, -1}, 2, 1));
        assertArrayEquals(
            new int[]{0xFFFFFFFF, 0xFFFFFFFF, 0x890ABCDE, 0xFFFFFFFF},
            Conversion.longToIntArray(0x1234567890ABCDEFL, 4, new int[]{-1, -1, -1, -1}, 2, 1));
        // assertArrayEquals(new
        // int[]{0x4855E6F7, 0x091A2B3C, 0x4855E6F7, 0x091A2B3C}, Conversion.longToIntArray(0x1234567890ABCDEFL,
        // 1, new int[]{-1, -1, -1, -1}, 0, 4));//rejected by assertion
        assertArrayEquals(
            new int[]{0x091A2B3C},
            Conversion.longToIntArray(0x1234567890ABCDEFL, 33, new int[]{0}, 0, 1));
    }

    /**
     * Tests {@link Conversion#longToShortArray(long, int, short[], int, int)}.
     */
    @Test
    public void testLongToShortArray() {
        assertArrayEquals(
            new short[]{},
            Conversion.longToShortArray(0x0000000000000000L, 0, new short[]{}, 0, 0));
        assertArrayEquals(
            new short[]{},
            Conversion.longToShortArray(0x0000000000000000L, 100, new short[]{}, 0, 0));
        assertArrayEquals(
            new short[]{},
            Conversion.longToShortArray(0x0000000000000000L, 0, new short[]{}, 100, 0));
        assertArrayEquals(
            new short[]{(short) 0xFFFF, (short) 0xFFFF, (short) 0xFFFF, (short) 0xFFFF},
            Conversion.longToShortArray(
                0x1234567890ABCDEFL, 0, new short[]{-1, -1, -1, -1}, 0, 0));
        assertArrayEquals(
            new short[]{(short) 0xCDEF, (short) 0xFFFF, (short) 0xFFFF, (short) 0xFFFF},
            Conversion.longToShortArray(
                0x1234567890ABCDEFL, 0, new short[]{-1, -1, -1, -1}, 0, 1));
        assertArrayEquals(
            new short[]{(short) 0xCDEF, (short) 0x90AB, (short) 0xFFFF, (short) 0xFFFF},
            Conversion.longToShortArray(
                0x1234567890ABCDEFL, 0, new short[]{-1, -1, -1, -1}, 0, 2));
        assertArrayEquals(
            new short[]{(short) 0xCDEF, (short) 0x90AB, (short) 0x5678, (short) 0xFFFF},
            Conversion.longToShortArray(
                0x1234567890ABCDEFL, 0, new short[]{-1, -1, -1, -1}, 0, 3));
        assertArrayEquals(
            new short[]{(short) 0xCDEF, (short) 0x90AB, (short) 0x5678, (short) 0x1234},
            Conversion.longToShortArray(
                0x1234567890ABCDEFL, 0, new short[]{-1, -1, -1, -1}, 0, 4));
        assertArrayEquals(
            new short[]{(short) 0xFFFF, (short) 0xCDEF, (short) 0x90AB, (short) 0x5678},
            Conversion.longToShortArray(
                0x1234567890ABCDEFL, 0, new short[]{-1, -1, -1, -1}, 1, 3));
        assertArrayEquals(
            new short[]{(short) 0xFFFF, (short) 0xFFFF, (short) 0xCDEF, (short) 0x90AB},
            Conversion.longToShortArray(
                0x1234567890ABCDEFL, 0, new short[]{-1, -1, -1, -1}, 2, 2));
        assertArrayEquals(
            new short[]{(short) 0xFFFF, (short) 0xFFFF, (short) 0xCDEF, (short) 0xFFFF},
            Conversion.longToShortArray(
                0x1234567890ABCDEFL, 0, new short[]{-1, -1, -1, -1}, 2, 1));
        assertArrayEquals(
            new short[]{(short) 0xFFFF, (short) 0xFFFF, (short) 0xFFFF, (short) 0xCDEF},
            Conversion.longToShortArray(
                0x1234567890ABCDEFL, 0, new short[]{-1, -1, -1, -1}, 3, 1));
        assertArrayEquals(
            new short[]{(short) 0xFFFF, (short) 0xFFFF, (short) 0xE6F7, (short) 0xFFFF},
            Conversion.longToShortArray(
                0x1234567890ABCDEFL, 1, new short[]{-1, -1, -1, -1}, 2, 1));
        assertArrayEquals(
            new short[]{(short) 0xFFFF, (short) 0xFFFF, (short) 0xF37B, (short) 0xFFFF},
            Conversion.longToShortArray(
                0x1234567890ABCDEFL, 2, new short[]{-1, -1, -1, -1}, 2, 1));
        assertArrayEquals(
            new short[]{(short) 0xFFFF, (short) 0xFFFF, (short) 0x79BD, (short) 0xFFFF},
            Conversion.longToShortArray(
                0x1234567890ABCDEFL, 3, new short[]{-1, -1, -1, -1}, 2, 1));
        assertArrayEquals(
            new short[]{(short) 0xFFFF, (short) 0xFFFF, (short) 0xBCDE, (short) 0xFFFF},
            Conversion.longToShortArray(
                0x1234567890ABCDEFL, 4, new short[]{-1, -1, -1, -1}, 2, 1));
        assertArrayEquals(
            new short[]{(short) 0xE6F7, (short) 0x4855, (short) 0x2B3C, (short) 0x091A},
            Conversion.longToShortArray(
                0x1234567890ABCDEFL, 1, new short[]{-1, -1, -1, -1}, 0, 4));
        assertArrayEquals(
            new short[]{(short) 0x2B3C},
            Conversion.longToShortArray(0x1234567890ABCDEFL, 33, new short[]{0}, 0, 1));
    }

    /**
     * Tests {@link Conversion#intToShortArray(int, int, short[], int, int)}.
     */
    @Test
    public void testIntToShortArray() {
        assertArrayEquals(
            new short[]{}, Conversion.intToShortArray(0x00000000, 0, new short[]{}, 0, 0));
        assertArrayEquals(
            new short[]{}, Conversion.intToShortArray(0x00000000, 100, new short[]{}, 0, 0));
        assertArrayEquals(
            new short[]{}, Conversion.intToShortArray(0x00000000, 0, new short[]{}, 100, 0));
        assertArrayEquals(
            new short[]{(short) 0xFFFF, (short) 0xFFFF, (short) 0xFFFF, (short) 0xFFFF},
            Conversion.intToShortArray(0x12345678, 0, new short[]{-1, -1, -1, -1}, 0, 0));
        assertArrayEquals(
            new short[]{(short) 0x5678, (short) 0xFFFF, (short) 0xFFFF, (short) 0xFFFF},
            Conversion.intToShortArray(0x12345678, 0, new short[]{-1, -1, -1, -1}, 0, 1));
        assertArrayEquals(
            new short[]{(short) 0x5678, (short) 0x1234, (short) 0xFFFF, (short) 0xFFFF},
            Conversion.intToShortArray(0x12345678, 0, new short[]{-1, -1, -1, -1}, 0, 2));
        // assertArrayEquals(new
        // short[]{(short) 0x5678, (short) 0x1234, (short) 0x5678, (short) 0xFFFF}, Conversion.intToShortArray(0x12345678,
        // 0, new short[]{-1, -1, -1, -1}, 0, 3));//rejected by assertion
        // assertArrayEquals(new
        // short[]{(short) 0x5678, (short) 0x1234, (short) 0x5678, (short) 0x1234}, Conversion.intToShortArray(0x12345678,
        // 0, new short[]{-1, -1, -1, -1}, 0, 4));
        // assertArrayEquals(new
        // short[]{(short) 0xFFFF, (short) 0x5678, (short) 0x1234, (short) 0x5678}, Conversion.intToShortArray(0x12345678,
        // 0, new short[]{-1, -1, -1, -1}, 1, 3));
        assertArrayEquals(
            new short[]{(short) 0xFFFF, (short) 0xFFFF, (short) 0x5678, (short) 0x1234},
            Conversion.intToShortArray(0x12345678, 0, new short[]{-1, -1, -1, -1}, 2, 2));
        assertArrayEquals(
            new short[]{(short) 0xFFFF, (short) 0xFFFF, (short) 0x5678, (short) 0xFFFF},
            Conversion.intToShortArray(0x12345678, 0, new short[]{-1, -1, -1, -1}, 2, 1));
        assertArrayEquals(
            new short[]{(short) 0xFFFF, (short) 0xFFFF, (short) 0xFFFF, (short) 0x5678},
            Conversion.intToShortArray(0x12345678, 0, new short[]{-1, -1, -1, -1}, 3, 1));
        assertArrayEquals(
            new short[]{(short) 0xFFFF, (short) 0xFFFF, (short) 0x2B3C, (short) 0xFFFF},
            Conversion.intToShortArray(0x12345678, 1, new short[]{-1, -1, -1, -1}, 2, 1));
        assertArrayEquals(
            new short[]{(short) 0xFFFF, (short) 0xFFFF, (short) 0x159E, (short) 0xFFFF},
            Conversion.intToShortArray(0x12345678, 2, new short[]{-1, -1, -1, -1}, 2, 1));
        assertArrayEquals(
            new short[]{(short) 0xFFFF, (short) 0xFFFF, (short) 0x8ACF, (short) 0xFFFF},
            Conversion.intToShortArray(0x12345678, 3, new short[]{-1, -1, -1, -1}, 2, 1));
        assertArrayEquals(
            new short[]{(short) 0xFFFF, (short) 0xFFFF, (short) 0x4567, (short) 0xFFFF},
            Conversion.intToShortArray(0x12345678, 4, new short[]{-1, -1, -1, -1}, 2, 1));
        // assertArrayEquals(new
        // short[]{(short) 0xE6F7, (short) 0x4855, (short) 0x2B3C, (short) 0x091A}, Conversion.intToShortArray(0x12345678,
        // 1, new short[]{-1, -1, -1, -1}, 0, 4));//rejected by assertion
        // assertArrayEquals(new
        // short[]{(short) 0x2B3C}, Conversion.intToShortArray(0x12345678, 33, new
        // short[]{0}, 0, 1));//rejected by assertion
        assertArrayEquals(
            new short[]{(short) 0x091A},
            Conversion.intToShortArray(0x12345678, 17, new short[]{0}, 0, 1));
    }

    /**
     * Tests {@link Conversion#longToByteArray(long, int, byte[], int, int)}.
     */
    @Test
    public void testLongToByteArray() {
        assertArrayEquals(
            new byte[]{},
            Conversion.longToByteArray(0x0000000000000000L, 0, new byte[]{}, 0, 0));
        assertArrayEquals(
            new byte[]{},
            Conversion.longToByteArray(0x0000000000000000L, 100, new byte[]{}, 0, 0));
        assertArrayEquals(
            new byte[]{},
            Conversion.longToByteArray(0x0000000000000000L, 0, new byte[]{}, 100, 0));
        assertArrayEquals(
            new byte[]{
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF,
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF},
            Conversion.longToByteArray(0x1234567890ABCDEFL, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 0, 0));
        assertArrayEquals(
            new byte[]{
                (byte) 0xEF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF,
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF},
            Conversion.longToByteArray(0x1234567890ABCDEFL, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 0, 1));
        assertArrayEquals(
            new byte[]{
                (byte) 0xEF, (byte) 0xCD, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF,
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF},
            Conversion.longToByteArray(0x1234567890ABCDEFL, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 0, 2));
        assertArrayEquals(
            new byte[]{
                (byte) 0xEF, (byte) 0xCD, (byte) 0xAB, (byte) 0x90, (byte) 0xFF, (byte) 0xFF,
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF},
            Conversion.longToByteArray(0x1234567890ABCDEFL, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 0, 4));
        assertArrayEquals(
            new byte[]{
                (byte) 0xEF, (byte) 0xCD, (byte) 0xAB, (byte) 0x90, (byte) 0x78, (byte) 0x56,
                (byte) 0x34, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF},
            Conversion.longToByteArray(0x1234567890ABCDEFL, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 0, 7));
        assertArrayEquals(
            new byte[]{
                (byte) 0xEF, (byte) 0xCD, (byte) 0xAB, (byte) 0x90, (byte) 0x78, (byte) 0x56,
                (byte) 0x34, (byte) 0x12, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF},
            Conversion.longToByteArray(0x1234567890ABCDEFL, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 0, 8));
        assertArrayEquals(
            new byte[]{
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xEF, (byte) 0xFF, (byte) 0xFF,
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF},
            Conversion.longToByteArray(0x1234567890ABCDEFL, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 3, 1));
        assertArrayEquals(
            new byte[]{
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xEF, (byte) 0xCD, (byte) 0xFF,
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF},
            Conversion.longToByteArray(0x1234567890ABCDEFL, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 3, 2));
        assertArrayEquals(
            new byte[]{
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xEF, (byte) 0xCD, (byte) 0xAB,
                (byte) 0x90, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF},
            Conversion.longToByteArray(0x1234567890ABCDEFL, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 3, 4));
        assertArrayEquals(
            new byte[]{
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xEF, (byte) 0xCD, (byte) 0xAB,
                (byte) 0x90, (byte) 0x78, (byte) 0x56, (byte) 0x34, (byte) 0xFF},
            Conversion.longToByteArray(0x1234567890ABCDEFL, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 3, 7));
        assertArrayEquals(
            new byte[]{
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xEF, (byte) 0xCD, (byte) 0xAB,
                (byte) 0x90, (byte) 0x78, (byte) 0x56, (byte) 0x34, (byte) 0x12},
            Conversion.longToByteArray(0x1234567890ABCDEFL, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 3, 8));
        assertArrayEquals(
            new byte[]{
                (byte) 0xF7, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF,
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF},
            Conversion.longToByteArray(0x1234567890ABCDEFL, 1, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 0, 1));
        assertArrayEquals(
            new byte[]{
                (byte) 0x7B, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF,
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF},
            Conversion.longToByteArray(0x1234567890ABCDEFL, 2, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 0, 1));
        assertArrayEquals(
            new byte[]{
                (byte) 0xFF, (byte) 0x00, (byte) 0xFF, (byte) 0x6F, (byte) 0x5E, (byte) 0x85,
                (byte) 0xC4, (byte) 0xB3, (byte) 0xA2, (byte) 0x91, (byte) 0x00},
            Conversion.longToByteArray(0x1234567890ABCDEFL, 5, new byte[]{
                -1, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 3, 8));
        // assertArrayEquals(new
        // byte[]{(byte) 0xFF, (byte) 0x00, (byte) 0xFF, (byte) 0x5E, (byte) 0x85, (byte) 0xC4, (byte) 0xB3, (byte) 0xA2, (byte) 0x91, (byte) 0x00, (byte) 0x00}, Conversion.longToByteArray(0x1234567890ABCDEFL, 13, new
        // byte[]{-1, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 3, 8));//rejected by assertion
        assertArrayEquals(
            new byte[]{
                (byte) 0xFF, (byte) 0x00, (byte) 0xFF, (byte) 0x5E, (byte) 0x85, (byte) 0xC4,
                (byte) 0xB3, (byte) 0xA2, (byte) 0x91, (byte) 0x00, (byte) 0xFF},
            Conversion.longToByteArray(0x1234567890ABCDEFL, 13, new byte[]{
                -1, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 3, 7));
    }

    /**
     * Tests {@link Conversion#intToByteArray(int, int, byte[], int, int)}.
     */
    @Test
    public void testIntToByteArray() {
        assertArrayEquals(
            new byte[]{}, Conversion.intToByteArray(0x00000000, 0, new byte[]{}, 0, 0));
        assertArrayEquals(
            new byte[]{}, Conversion.intToByteArray(0x00000000, 100, new byte[]{}, 0, 0));
        assertArrayEquals(
            new byte[]{}, Conversion.intToByteArray(0x00000000, 0, new byte[]{}, 100, 0));
        assertArrayEquals(
            new byte[]{
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF,
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF},
            Conversion.intToByteArray(0x90ABCDEF, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 0, 0));
        assertArrayEquals(
            new byte[]{
                (byte) 0xEF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF,
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF},
            Conversion.intToByteArray(0x90ABCDEF, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 0, 1));
        assertArrayEquals(
            new byte[]{
                (byte) 0xEF, (byte) 0xCD, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF,
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF},
            Conversion.intToByteArray(0x90ABCDEF, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 0, 2));
        assertArrayEquals(
            new byte[]{
                (byte) 0xEF, (byte) 0xCD, (byte) 0xAB, (byte) 0x90, (byte) 0xFF, (byte) 0xFF,
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF},
            Conversion.intToByteArray(0x90ABCDEF, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 0, 4));
        assertArrayEquals(
            new byte[]{
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xEF, (byte) 0xFF, (byte) 0xFF,
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF},
            Conversion.intToByteArray(0x90ABCDEF, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 3, 1));
        assertArrayEquals(
            new byte[]{
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xEF, (byte) 0xCD, (byte) 0xFF,
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF},
            Conversion.intToByteArray(0x90ABCDEF, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 3, 2));
        assertArrayEquals(
            new byte[]{
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xEF, (byte) 0xCD, (byte) 0xAB,
                (byte) 0x90, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF},
            Conversion.intToByteArray(0x90ABCDEF, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 3, 4));
        assertArrayEquals(
            new byte[]{
                (byte) 0xF7, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF,
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF},
            Conversion.intToByteArray(0x90ABCDEF, 1, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 0, 1));
        assertArrayEquals(
            new byte[]{
                (byte) 0x7B, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF,
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF},
            Conversion.intToByteArray(0x90ABCDEF, 2, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 0, 1));
        assertArrayEquals(
            new byte[]{
                (byte) 0xFF, (byte) 0x00, (byte) 0xFF, (byte) 0x6F, (byte) 0x5E, (byte) 0x85,
                (byte) 0xFC, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF},
            Conversion.intToByteArray(0x90ABCDEF, 5, new byte[]{
                -1, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 3, 4));
        // assertArrayEquals(new
        // byte[]{(byte) 0xFF, (byte) 0x00, (byte) 0xFF, (byte) 0x5E, (byte) 0x85, (byte) 0xFC, (byte) 0x00, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF}, Conversion.intToByteArray(0x90ABCDEF, 13, new
        // byte[]{-1, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 3, 4));//rejected by assertion
        assertArrayEquals(
            new byte[]{
                (byte) 0xFF, (byte) 0x00, (byte) 0xFF, (byte) 0x5E, (byte) 0x85, (byte) 0xFC,
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF},
            Conversion.intToByteArray(0x90ABCDEF, 13, new byte[]{
                -1, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 3, 3));
    }

    /**
     * Tests {@link Conversion#shortToByteArray(short, int, byte[], int, int)}.
     */
    @Test
    public void testShortToByteArray() {
        assertArrayEquals(
            new byte[]{}, Conversion.shortToByteArray((short) 0x0000, 0, new byte[]{}, 0, 0));
        assertArrayEquals(
            new byte[]{}, Conversion.shortToByteArray((short) 0x0000, 100, new byte[]{}, 0, 0));
        assertArrayEquals(
            new byte[]{}, Conversion.shortToByteArray((short) 0x0000, 0, new byte[]{}, 100, 0));
        assertArrayEquals(
            new byte[]{
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF,
                (byte) 0xFF}, Conversion.shortToByteArray((short) 0xCDEF, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1}, 0, 0));
        assertArrayEquals(
            new byte[]{
                (byte) 0xEF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF,
                (byte) 0xFF}, Conversion.shortToByteArray((short) 0xCDEF, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1}, 0, 1));
        assertArrayEquals(
            new byte[]{
                (byte) 0xEF, (byte) 0xCD, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF,
                (byte) 0xFF}, Conversion.shortToByteArray((short) 0xCDEF, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1}, 0, 2));
        assertArrayEquals(
            new byte[]{
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xEF, (byte) 0xFF, (byte) 0xFF,
                (byte) 0xFF}, Conversion.shortToByteArray((short) 0xCDEF, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1}, 3, 1));
        assertArrayEquals(
            new byte[]{
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xEF, (byte) 0xCD, (byte) 0xFF,
                (byte) 0xFF}, Conversion.shortToByteArray((short) 0xCDEF, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1}, 3, 2));
        assertArrayEquals(
            new byte[]{
                (byte) 0xF7, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF,
                (byte) 0xFF}, Conversion.shortToByteArray((short) 0xCDEF, 1, new byte[]{
                -1, -1, -1, -1, -1, -1, -1}, 0, 1));
        assertArrayEquals(
            new byte[]{
                (byte) 0x7B, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF,
                (byte) 0xFF}, Conversion.shortToByteArray((short) 0xCDEF, 2, new byte[]{
                -1, -1, -1, -1, -1, -1, -1}, 0, 1));
        assertArrayEquals(
            new byte[]{
                (byte) 0xFF, (byte) 0x00, (byte) 0xFF, (byte) 0x6F, (byte) 0xFE, (byte) 0xFF,
                (byte) 0xFF}, Conversion.shortToByteArray((short) 0xCDEF, 5, new byte[]{
                -1, 0, -1, -1, -1, -1, -1}, 3, 2));
        // assertArrayEquals(new
        // byte[]{(byte) 0xFF, (byte) 0x00, (byte) 0xFF, (byte) 0x5E, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF}, Conversion.shortToByteArray((short) 0xCDEF, 13, new
        // byte[]{-1, 0, -1, -1, -1, -1, -1}, 3, 2));//rejected by assertion
        assertArrayEquals(
            new byte[]{
                (byte) 0xFF, (byte) 0x00, (byte) 0xFF, (byte) 0xFE, (byte) 0xFF, (byte) 0xFF,
                (byte) 0xFF}, Conversion.shortToByteArray((short) 0xCDEF, 13, new byte[]{
                -1, 0, -1, -1, -1, -1, -1}, 3, 1));
    }

    /**
     * Tests {@link Conversion#longToHex(long, int, String, int, int)}.
     */
    @Test
    public void testLongToHex() {
        assertEquals("", Conversion.longToHex(0x0000000000000000L, 0, "", 0, 0));
        assertEquals("", Conversion.longToHex(0x0000000000000000L, 100, "", 0, 0));
        assertEquals("", Conversion.longToHex(0x0000000000000000L, 0, "", 100, 0));
        assertEquals(
            "ffffffffffffffffffffffff",
            Conversion.longToHex(0x1234567890ABCDEFL, 0, "ffffffffffffffffffffffff", 0, 0));
        assertEquals(
            "3fffffffffffffffffffffff",
            Conversion.longToHex(0x1234567890ABCDE3L, 0, "ffffffffffffffffffffffff", 0, 1));
        assertEquals(
            "feffffffffffffffffffffff",
            Conversion.longToHex(0x1234567890ABCDEFL, 0, "ffffffffffffffffffffffff", 0, 2));
        assertEquals(
            "fedcffffffffffffffffffff",
            Conversion.longToHex(0x1234567890ABCDEFL, 0, "ffffffffffffffffffffffff", 0, 4));
        assertEquals(
            "fedcba098765432fffffffff",
            Conversion.longToHex(0x1234567890ABCDEFL, 0, "ffffffffffffffffffffffff", 0, 15));
        assertEquals(
            "fedcba0987654321ffffffff",
            Conversion.longToHex(0x1234567890ABCDEFL, 0, "ffffffffffffffffffffffff", 0, 16));
        assertEquals(
            "fff3ffffffffffffffffffff",
            Conversion.longToHex(0x1234567890ABCDE3L, 0, "ffffffffffffffffffffffff", 3, 1));
        assertEquals(
            "ffffefffffffffffffffffff",
            Conversion.longToHex(0x1234567890ABCDEFL, 0, "ffffffffffffffffffffffff", 3, 2));
        assertEquals(
            "ffffedcfffffffffffffffff",
            Conversion.longToHex(0x1234567890ABCDEFL, 0, "ffffffffffffffffffffffff", 3, 4));
        assertEquals(
            "ffffedcba098765432ffffff",
            Conversion.longToHex(0x1234567890ABCDEFL, 0, "ffffffffffffffffffffffff", 3, 15));
        assertEquals(
            "ffffedcba0987654321fffff",
            Conversion.longToHex(0x1234567890ABCDEFL, 0, "ffffffffffffffffffffffff", 3, 16));
        assertEquals(
            "7fffffffffffffffffffffff",
            Conversion.longToHex(0x1234567890ABCDEFL, 1, "ffffffffffffffffffffffff", 0, 1));
        assertEquals(
            "bfffffffffffffffffffffff",
            Conversion.longToHex(0x1234567890ABCDEFL, 2, "ffffffffffffffffffffffff", 0, 1));
        assertEquals(
            "fffdb975121fca86420fffff",
            Conversion.longToHex(0x1234567890ABCDEFL, 3, "ffffffffffffffffffffffff", 3, 16));
        // assertEquals("ffffffffffffffffffffffff", Conversion.longToHex(0x1234567890ABCDEFL, 4, "ffffffffffffffffffffffff", 3, 16));//rejected
        // by assertion
        assertEquals(
            "fffedcba0987654321ffffff",
            Conversion.longToHex(0x1234567890ABCDEFL, 4, "ffffffffffffffffffffffff", 3, 15));
        assertEquals(
            "fedcba0987654321", Conversion.longToHex(0x1234567890ABCDEFL, 0, "", 0, 16));
        assertThrows(StringIndexOutOfBoundsException.class, () -> Conversion.longToHex(0x1234567890ABCDEFL, 0, "", 1, 8));
    }

    /**
     * Tests {@link Conversion#intToHex(int, int, String, int, int)}.
     */
    @Test
    public void testIntToHex() {
        assertEquals("", Conversion.intToHex(0x00000000, 0, "", 0, 0));
        assertEquals("", Conversion.intToHex(0x00000000, 100, "", 0, 0));
        assertEquals("", Conversion.intToHex(0x00000000, 0, "", 100, 0));
        assertEquals(
            "ffffffffffffffffffffffff",
            Conversion.intToHex(0x90ABCDEF, 0, "ffffffffffffffffffffffff", 0, 0));
        assertEquals(
            "3fffffffffffffffffffffff",
            Conversion.intToHex(0x90ABCDE3, 0, "ffffffffffffffffffffffff", 0, 1));
        assertEquals(
            "feffffffffffffffffffffff",
            Conversion.intToHex(0x90ABCDEF, 0, "ffffffffffffffffffffffff", 0, 2));
        assertEquals(
            "fedcffffffffffffffffffff",
            Conversion.intToHex(0x90ABCDEF, 0, "ffffffffffffffffffffffff", 0, 4));
        assertEquals(
            "fedcba0fffffffffffffffff",
            Conversion.intToHex(0x90ABCDEF, 0, "ffffffffffffffffffffffff", 0, 7));
        assertEquals(
            "fedcba09ffffffffffffffff",
            Conversion.intToHex(0x90ABCDEF, 0, "ffffffffffffffffffffffff", 0, 8));
        assertEquals(
            "fff3ffffffffffffffffffff",
            Conversion.intToHex(0x90ABCDE3, 0, "ffffffffffffffffffffffff", 3, 1));
        assertEquals(
            "ffffefffffffffffffffffff",
            Conversion.intToHex(0x90ABCDEF, 0, "ffffffffffffffffffffffff", 3, 2));
        assertEquals(
            "ffffedcfffffffffffffffff",
            Conversion.intToHex(0x90ABCDEF, 0, "ffffffffffffffffffffffff", 3, 4));
        assertEquals(
            "ffffedcba0ffffffffffffff",
            Conversion.intToHex(0x90ABCDEF, 0, "ffffffffffffffffffffffff", 3, 7));
        assertEquals(
            "ffffedcba09fffffffffffff",
            Conversion.intToHex(0x90ABCDEF, 0, "ffffffffffffffffffffffff", 3, 8));
        assertEquals(
            "7fffffffffffffffffffffff",
            Conversion.intToHex(0x90ABCDEF, 1, "ffffffffffffffffffffffff", 0, 1));
        assertEquals(
            "bfffffffffffffffffffffff",
            Conversion.intToHex(0x90ABCDEF, 2, "ffffffffffffffffffffffff", 0, 1));
        assertEquals(
            "fffdb97512ffffffffffffff",
            Conversion.intToHex(0x90ABCDEF, 3, "ffffffffffffffffffffffff", 3, 8));
        // assertEquals("ffffffffffffffffffffffff", Conversion.intToHex(0x90ABCDEF,
        // 4, "ffffffffffffffffffffffff", 3, 8));//rejected by assertion
        assertEquals(
            "fffedcba09ffffffffffffff",
            Conversion.intToHex(0x90ABCDEF, 4, "ffffffffffffffffffffffff", 3, 7));
        assertEquals("fedcba09", Conversion.intToHex(0x90ABCDEF, 0, "", 0, 8));
        assertThrows(StringIndexOutOfBoundsException.class, () -> Conversion.intToHex(0x90ABCDEF, 0, "", 1, 8));
    }

    /**
     * Tests {@link Conversion#shortToHex(short, int, String, int, int)}.
     */
    @Test
    public void testShortToHex() {
        assertEquals("", Conversion.shortToHex((short) 0x0000, 0, "", 0, 0));
        assertEquals("", Conversion.shortToHex((short) 0x0000, 100, "", 0, 0));
        assertEquals("", Conversion.shortToHex((short) 0x0000, 0, "", 100, 0));
        assertEquals(
            "ffffffffffffffffffffffff",
            Conversion.shortToHex((short) 0xCDEF, 0, "ffffffffffffffffffffffff", 0, 0));
        assertEquals(
            "3fffffffffffffffffffffff",
            Conversion.shortToHex((short) 0xCDE3, 0, "ffffffffffffffffffffffff", 0, 1));
        assertEquals(
            "feffffffffffffffffffffff",
            Conversion.shortToHex((short) 0xCDEF, 0, "ffffffffffffffffffffffff", 0, 2));
        assertEquals(
            "fedfffffffffffffffffffff",
            Conversion.shortToHex((short) 0xCDEF, 0, "ffffffffffffffffffffffff", 0, 3));
        assertEquals(
            "fedcffffffffffffffffffff",
            Conversion.shortToHex((short) 0xCDEF, 0, "ffffffffffffffffffffffff", 0, 4));
        assertEquals(
            "fff3ffffffffffffffffffff",
            Conversion.shortToHex((short) 0xCDE3, 0, "ffffffffffffffffffffffff", 3, 1));
        assertEquals(
            "ffffefffffffffffffffffff",
            Conversion.shortToHex((short) 0xCDEF, 0, "ffffffffffffffffffffffff", 3, 2));
        assertEquals(
            "7fffffffffffffffffffffff",
            Conversion.shortToHex((short) 0xCDEF, 1, "ffffffffffffffffffffffff", 0, 1));
        assertEquals(
            "bfffffffffffffffffffffff",
            Conversion.shortToHex((short) 0xCDEF, 2, "ffffffffffffffffffffffff", 0, 1));
        assertEquals(
            "fffdb9ffffffffffffffffff",
            Conversion.shortToHex((short) 0xCDEF, 3, "ffffffffffffffffffffffff", 3, 4));
        // assertEquals("ffffffffffffffffffffffff", Conversion.shortToHex((short) 0xCDEF,
        // 4, "ffffffffffffffffffffffff", 3, 4));//rejected by assertion
        assertEquals(
            "fffedcffffffffffffffffff",
            Conversion.shortToHex((short) 0xCDEF, 4, "ffffffffffffffffffffffff", 3, 3));
        assertEquals("fedc", Conversion.shortToHex((short) 0xCDEF, 0, "", 0, 4));
        assertThrows(StringIndexOutOfBoundsException.class, () -> Conversion.shortToHex((short) 0xCDEF, 0, "", 1, 4));
    }

    /**
     * Tests {@link Conversion#byteToHex(byte, int, String, int, int)}.
     */
    @Test
    public void testByteToHex() {
        assertEquals("", Conversion.byteToHex((byte) 0x00, 0, "", 0, 0));
        assertEquals("", Conversion.byteToHex((byte) 0x00, 100, "", 0, 0));
        assertEquals("", Conversion.byteToHex((byte) 0x00, 0, "", 100, 0));
        assertEquals("00000", Conversion.byteToHex((byte) 0xEF, 0, "00000", 0, 0));
        assertEquals("f0000", Conversion.byteToHex((byte) 0xEF, 0, "00000", 0, 1));
        assertEquals("fe000", Conversion.byteToHex((byte) 0xEF, 0, "00000", 0, 2));
        assertEquals("000f0", Conversion.byteToHex((byte) 0xEF, 0, "00000", 3, 1));
        assertEquals("000fe", Conversion.byteToHex((byte) 0xEF, 0, "00000", 3, 2));
        assertEquals("70000", Conversion.byteToHex((byte) 0xEF, 1, "00000", 0, 1));
        assertEquals("b0000", Conversion.byteToHex((byte) 0xEF, 2, "00000", 0, 1));
        assertEquals("000df", Conversion.byteToHex((byte) 0xEF, 3, "00000", 3, 2));
        // assertEquals("00000", Conversion.byteToHex((byte) 0xEF, 4, "00000", 3, 2));//rejected by
        // assertion
        assertEquals("000e0", Conversion.byteToHex((byte) 0xEF, 4, "00000", 3, 1));
        assertEquals("fe", Conversion.byteToHex((byte) 0xEF, 0, "", 0, 2));
        assertThrows(StringIndexOutOfBoundsException.class, () -> Conversion.byteToHex((byte) 0xEF, 0, "", 1, 2));
    }

    /**
     * Tests {@link Conversion#longToBinary(long, int, boolean[], int, int)}.
     */
    @Test
    public void testLongToBinary() {
        assertArrayEquals(
            new boolean[]{},
            Conversion.longToBinary(0x0000000000000000L, 0, new boolean[]{}, 0, 0));
        assertArrayEquals(
            new boolean[]{},
            Conversion.longToBinary(0x0000000000000000L, 100, new boolean[]{}, 0, 0));
        assertArrayEquals(
            new boolean[]{},
            Conversion.longToBinary(0x0000000000000000L, 0, new boolean[]{}, 100, 0));
        assertArrayEquals(
            new boolean[69],
            Conversion.longToBinary(0x1234567890ABCDEFL, 0, new boolean[69], 0, 0));

        assertArrayEquals(
            new boolean[]{
                true, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false},
            Conversion.longToBinary(0x1234567890ABCDEFL, 0, new boolean[69], 0, 1));
        assertArrayEquals(
            new boolean[]{
                true, true, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false},
            Conversion.longToBinary(0x1234567890ABCDEFL, 0, new boolean[69], 0, 2));
        assertArrayEquals(
            new boolean[]{
                true, true, true, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false},
            Conversion.longToBinary(0x1234567890ABCDEFL, 0, new boolean[69], 0, 3));
        assertArrayEquals(
            new boolean[]{
                true, true, true, true, false, true, true, true, true, false, true, true,
                false, false, true, true, true, true, false, true, false, true, false, true,
                false, false, false, false, true, false, false, true, false, false, false,
                true, true, true, true, false, false, true, true, false, true, false, true,
                false, false, false, true, false, true, true, false, false, false, true, false,
                false, true, false, false, false, false, false, false, false, false},
            Conversion.longToBinary(0x1234567890ABCDEFL, 0, new boolean[69], 0, 63));
        assertArrayEquals(
            new boolean[]{
                true, true, true, true, false, true, true, true, true, false, true, true,
                false, false, true, true, true, true, false, true, false, true, false, true,
                false, false, false, false, true, false, false, true, false, false, false,
                true, true, true, true, false, false, true, true, false, true, false, true,
                false, false, false, true, false, true, true, false, false, false, true, false,
                false, true, false, false, false, false, false, false, false, false},
            Conversion.longToBinary(0x1234567890ABCDEFL, 0, new boolean[69], 0, 64));
        assertArrayEquals(
            new boolean[]{
                false, false, true, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false},
            Conversion.longToBinary(0x1234567890ABCDEFL, 0, new boolean[69], 2, 1));
        assertArrayEquals(
            new boolean[]{
                false, false, true, true, true, true, false, true, true, true, true, false,
                true, true, false, false, true, true, true, true, false, true, false, true,
                false, true, false, false, false, false, true, false, false, true, false,
                false, false, true, true, true, true, false, false, true, true, false, true,
                false, true, false, false, false, true, false, true, true, false, false, false,
                true, false, false, true, false, false, false, false, false, false},
            Conversion.longToBinary(0x1234567890ABCDEFL, 0, new boolean[69], 2, 64));
        assertArrayEquals(
            new boolean[]{
                true, true, true, false, true, true, true, true, false, true, true, false,
                false, true, true, true, true, false, true, false, true, false, true, false,
                false, false, false, true, false, false, true, false, false, false, true, true,
                true, true, false, false, true, true, false, true, false, true, false, false,
                false, true, false, true, true, false, false, false, true, false, false, true,
                false, false, false, false, false, false, false, false, false},
            Conversion.longToBinary(0x1234567890ABCDEFL, 1, new boolean[69], 0, 63));
        assertArrayEquals(
            new boolean[]{
                true, true, false, true, true, true, true, false, true, true, false, false,
                true, true, true, true, false, true, false, true, false, true, false, false,
                false, false, true, false, false, true, false, false, false, true, true, true,
                true, false, false, true, true, false, true, false, true, false, false, false,
                true, false, true, true, false, false, false, true, false, false, true, false,
                false, false, false, false, false, false, false, false, false},
            Conversion.longToBinary(0x1234567890ABCDEFL, 2, new boolean[69], 0, 62));

        // assertArrayEquals(new boolean[]{false, false, false, true, true, false, true, true,
        // true, true, false, true, true, false, false, true, true, true, true, false, true,
        // false, true, false, true, false, false, false, false, true, false, false, true,
        // false, false, false, true, true, true, true, false, false, true, true, false, true,
        // false, true, false, false, false, true, false, true, true, false, false, false, true,
        // false, false, true, false, false, false
        // , false, false, false, false}, Conversion.longToBinary(0x1234567890ABCDEFL, 2, new
        // boolean[69], 3, 63));//rejected by assertion
        assertArrayEquals(
            new boolean[]{
                false, false, false, true, true, false, true, true, true, true, false, true,
                true, false, false, true, true, true, true, false, true, false, true, false,
                true, false, false, false, false, true, false, false, true, false, false,
                false, true, true, true, true, false, false, true, true, false, true, false,
                true, false, false, false, true, false, true, true, false, false, false, true,
                false, false, true, false, false, false, false, false, false, false},
            Conversion.longToBinary(0x1234567890ABCDEFL, 2, new boolean[69], 3, 62));
    }

    /**
     * Tests {@link Conversion#intToBinary(int, int, boolean[], int, int)}.
     */
    @Test
    public void testIntToBinary() {
        assertArrayEquals(
            new boolean[]{}, Conversion.intToBinary(0x00000000, 0, new boolean[]{}, 0, 0));
        assertArrayEquals(
            new boolean[]{}, Conversion.intToBinary(0x00000000, 100, new boolean[]{}, 0, 0));
        assertArrayEquals(
            new boolean[]{}, Conversion.intToBinary(0x00000000, 0, new boolean[]{}, 100, 0));
        assertArrayEquals(
            new boolean[69], Conversion.intToBinary(0x90ABCDEF, 0, new boolean[69], 0, 0));
        assertArrayEquals(new boolean[]{
            true, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false}, Conversion.intToBinary(0x90ABCDEF, 0, new boolean[37], 0, 1));
        assertArrayEquals(new boolean[]{
            true, true, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false}, Conversion.intToBinary(0x90ABCDEF, 0, new boolean[37], 0, 2));
        assertArrayEquals(new boolean[]{
            true, true, true, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false}, Conversion.intToBinary(0x90ABCDEF, 0, new boolean[37], 0, 3));
        assertArrayEquals(
            new boolean[]{
                true, true, true, true, false, true, true, true, true, false, true, true,
                false, false, true, true, true, true, false, true, false, true, false, true,
                false, false, false, false, true, false, false, false, false, false, false,
                false, false}, Conversion.intToBinary(0x90ABCDEF, 0, new boolean[37], 0, 31));
        assertArrayEquals(
            new boolean[]{
                true, true, true, true, false, true, true, true, true, false, true, true,
                false, false, true, true, true, true, false, true, false, true, false, true,
                false, false, false, false, true, false, false, true, false, false, false,
                false, false}, Conversion.intToBinary(0x90ABCDEF, 0, new boolean[37], 0, 32));
        assertArrayEquals(new boolean[]{
            false, false, true, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false}, Conversion.intToBinary(0x90ABCDEF, 0, new boolean[37], 2, 1));
        assertArrayEquals(
            new boolean[]{
                false, false, true, true, true, true, false, true, true, true, true, false,
                true, true, false, false, true, true, true, true, false, true, false, true,
                false, true, false, false, false, false, true, false, false, true, false,
                false, false}, Conversion.intToBinary(0x90ABCDEF, 0, new boolean[37], 2, 32));
        assertArrayEquals(
            new boolean[]{
                true, true, true, false, true, true, true, true, false, true, true, false,
                false, true, true, true, true, false, true, false, true, false, true, false,
                false, false, false, true, false, false, true, false, false, false, false,
                false, false}, Conversion.intToBinary(0x90ABCDEF, 1, new boolean[37], 0, 31));
        assertArrayEquals(
            new boolean[]{
                true, true, false, true, true, true, true, false, true, true, false, false,
                true, true, true, true, false, true, false, true, false, true, false, false,
                false, false, true, false, false, true, false, false, false, false, false,
                false, false}, Conversion.intToBinary(0x90ABCDEF, 2, new boolean[37], 0, 30));
        // assertArrayEquals(new boolean[]{false, false, false, true, true, false, true,
        // true,
        // true, true, false, true, true, false, false, true, true, true, true, false, true,
        // false, true, false, true, false, false, false, false, true, false, false, false,
        // false, false, false, false}, Conversion.intToBinary(0x90ABCDEF, 2, new boolean[37],
        // 3, 31));//rejected by assertion
        assertArrayEquals(
            new boolean[]{
                false, false, false, true, true, false, true, true, true, true, false, true,
                true, false, false, true, true, true, true, false, true, false, true, false,
                true, false, false, false, false, true, false, false, true, false, false,
                false, false}, Conversion.intToBinary(0x90ABCDEF, 2, new boolean[37], 3, 30));
    }

    /**
     * Tests {@link Conversion#shortToBinary(short, int, boolean[], int, int)}.
     */
    @Test
    public void testShortToBinary() {
        assertArrayEquals(
            new boolean[]{}, Conversion.shortToBinary((short) 0x0000, 0, new boolean[]{}, 0, 0));
        assertArrayEquals(
            new boolean[]{},
            Conversion.shortToBinary((short) 0x0000, 100, new boolean[]{}, 0, 0));
        assertArrayEquals(
            new boolean[]{},
            Conversion.shortToBinary((short) 0x0000, 0, new boolean[]{}, 100, 0));
        assertArrayEquals(
            new boolean[69], Conversion.shortToBinary((short) 0xCDEF, 0, new boolean[69], 0, 0));
        assertArrayEquals(
            new boolean[]{
                true, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false},
            Conversion.shortToBinary((short) 0xCDEF, 0, new boolean[21], 0, 1));
        assertArrayEquals(
            new boolean[]{
                true, true, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false},
            Conversion.shortToBinary((short) 0xCDEF, 0, new boolean[21], 0, 2));
        assertArrayEquals(
            new boolean[]{
                true, true, true, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false},
            Conversion.shortToBinary((short) 0xCDEF, 0, new boolean[21], 0, 3));
        assertArrayEquals(
            new boolean[]{
                true, true, true, true, false, true, true, true, true, false, true, true,
                false, false, true, false, false, false, false, false, false},
            Conversion.shortToBinary((short) 0xCDEF, 0, new boolean[21], 0, 15));
        assertArrayEquals(
            new boolean[]{
                true, true, true, true, false, true, true, true, true, false, true, true,
                false, false, true, true, false, false, false, false, false},
            Conversion.shortToBinary((short) 0xCDEF, 0, new boolean[21], 0, 16));
        assertArrayEquals(
            new boolean[]{
                false, false, true, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false},
            Conversion.shortToBinary((short) 0xCDEF, 0, new boolean[21], 2, 1));
        assertArrayEquals(
            new boolean[]{
                false, false, true, true, true, true, false, true, true, true, true, false,
                true, true, false, false, true, true, false, false, false},
            Conversion.shortToBinary((short) 0xCDEF, 0, new boolean[21], 2, 16));
        assertArrayEquals(
            new boolean[]{
                true, true, true, false, true, true, true, true, false, true, true, false,
                false, true, true, false, false, false, false, false, false},
            Conversion.shortToBinary((short) 0xCDEF, 1, new boolean[21], 0, 15));
        assertArrayEquals(
            new boolean[]{
                true, true, false, true, true, true, true, false, true, true, false, false,
                true, true, false, false, false, false, false, false, false},
            Conversion.shortToBinary((short) 0xCDEF, 2, new boolean[21], 0, 14));
        // assertArrayEquals(new boolean[]{false, false, false, true, true, false, true, true,
        // true, true, false, true, true, false, false, true, false, false, false, false,
        // false}, Conversion.shortToBinary((short) 0xCDEF, 2, new boolean[21],
        // 3, 15));//rejected by
        // assertion
        assertArrayEquals(
            new boolean[]{
                false, false, false, true, true, false, true, true, true, true, false, true,
                true, false, false, true, true, false, false, false, false},
            Conversion.shortToBinary((short) 0xCDEF, 2, new boolean[21], 3, 14));
    }

    /**
     * Tests {@link Conversion#byteToBinary(byte, int, boolean[], int, int)}.
     */
    @Test
    public void testByteToBinary() {
        assertArrayEquals(
            new boolean[]{}, Conversion.byteToBinary((byte) 0x00, 0, new boolean[]{}, 0, 0));
        assertArrayEquals(
            new boolean[]{}, Conversion.byteToBinary((byte) 0x00, 100, new boolean[]{}, 0, 0));
        assertArrayEquals(
            new boolean[]{}, Conversion.byteToBinary((byte) 0x00, 0, new boolean[]{}, 100, 0));
        assertArrayEquals(
            new boolean[69], Conversion.byteToBinary((byte) 0xEF, 0, new boolean[69], 0, 0));
        assertArrayEquals(new boolean[]{
            true, false, false, false, false, false, false, false, false, false, false, false,
            false}, Conversion.byteToBinary((byte) 0x95, 0, new boolean[13], 0, 1));
        assertArrayEquals(new boolean[]{
            true, false, false, false, false, false, false, false, false, false, false, false,
            false}, Conversion.byteToBinary((byte) 0x95, 0, new boolean[13], 0, 2));
        assertArrayEquals(new boolean[]{
            true, false, true, false, false, false, false, false, false, false, false, false,
            false}, Conversion.byteToBinary((byte) 0x95, 0, new boolean[13], 0, 3));
        assertArrayEquals(new boolean[]{
            true, false, true, false, true, false, false, false, false, false, false, false,
            false}, Conversion.byteToBinary((byte) 0x95, 0, new boolean[13], 0, 7));
        assertArrayEquals(new boolean[]{
            true, false, true, false, true, false, false, true, false, false, false, false,
            false}, Conversion.byteToBinary((byte) 0x95, 0, new boolean[13], 0, 8));
        assertArrayEquals(new boolean[]{
            false, false, true, false, false, false, false, false, false, false, false, false,
            false}, Conversion.byteToBinary((byte) 0x95, 0, new boolean[13], 2, 1));
        assertArrayEquals(new boolean[]{
            false, false, true, false, true, false, true, false, false, true, false, false,
            false}, Conversion.byteToBinary((byte) 0x95, 0, new boolean[13], 2, 8));
        assertArrayEquals(new boolean[]{
            false, true, false, true, false, false, true, false, false, false, false, false,
            false}, Conversion.byteToBinary((byte) 0x95, 1, new boolean[13], 0, 7));
        assertArrayEquals(new boolean[]{
            true, false, true, false, false, true, false, false, false, false, false, false,
            false}, Conversion.byteToBinary((byte) 0x95, 2, new boolean[13], 0, 6));
        // assertArrayEquals(new boolean[]{false, false, false, true, true, false, true, true,
        // false, false, false, false, false}, Conversion.byteToBinary((byte) 0x95, 2, new
        // boolean[13], 3, 7));//rejected by assertion
        assertArrayEquals(new boolean[]{
            false, false, false, true, false, true, false, false, true, false, false, false,
            false}, Conversion.byteToBinary((byte) 0x95, 2, new boolean[13], 3, 6));
    }

    /**
     * Tests {@link Conversion#uuidToByteArray(UUID, byte[], int, int)}.
     */
    @Test
    public void testUuidToByteArray() {
        assertArrayEquals(new byte[]{
            (byte) 0xff, (byte) 0xff, (byte) 0xff, (byte) 0xff, (byte) 0xff, (byte) 0xff, (byte) 0xff,
            (byte) 0xff, (byte) 0xff, (byte) 0xff, (byte) 0xff, (byte) 0xff, (byte) 0xff, (byte) 0xff,
            (byte) 0xff, (byte) 0xff}, Conversion.uuidToByteArray(new UUID(
            0xFFFFFFFFFFFFFFFFL, 0xFFFFFFFFFFFFFFFFL), new byte[16], 0, 16));
        assertArrayEquals(new byte[]{
            (byte) 0x88, (byte) 0x99, (byte) 0xaa, (byte) 0xbb, (byte) 0xcc, (byte) 0xdd, (byte) 0xee,
            (byte) 0xff, (byte) 0x00, (byte) 0x11, (byte) 0x22, (byte) 0x33, (byte) 0x44, (byte) 0x55,
            (byte) 0x66, (byte) 0x77}, Conversion.uuidToByteArray(new UUID(
            0xFFEEDDCCBBAA9988L, 0x7766554433221100L), new byte[16], 0, 16));
        assertArrayEquals(new byte[]{
            (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x88, (byte) 0x99, (byte) 0xaa,
            (byte) 0xbb, (byte) 0xcc, (byte) 0xdd, (byte) 0xee, (byte) 0xff, (byte) 0x00, (byte) 0x00,
            (byte) 0x00, (byte) 0x00}, Conversion.uuidToByteArray(new UUID(
            0xFFEEDDCCBBAA9988L, 0x7766554433221100L), new byte[16], 4, 8));
        assertArrayEquals(new byte[]{
            (byte) 0x00, (byte) 0x00, (byte) 0x88, (byte) 0x99, (byte) 0xaa, (byte) 0xbb, (byte) 0xcc,
            (byte) 0xdd, (byte) 0xee, (byte) 0xff, (byte) 0x00, (byte) 0x11, (byte) 0x22, (byte) 0x33,
            (byte) 0x00, (byte) 0x00}, Conversion.uuidToByteArray(new UUID(
            0xFFEEDDCCBBAA9988L, 0x7766554433221100L), new byte[16], 2, 12));
    }

    /**
     * Tests {@link Conversion#byteArrayToUuid(byte[], int)}.
     */
    @Test
    public void testByteArrayToUuid() {
        assertEquals(
            new UUID(0xFFFFFFFFFFFFFFFFL, 0xFFFFFFFFFFFFFFFFL),
            Conversion.byteArrayToUuid(new byte[]{
                (byte) 0xff, (byte) 0xff, (byte) 0xff, (byte) 0xff, (byte) 0xff, (byte) 0xff,
                (byte) 0xff, (byte) 0xff, (byte) 0xff, (byte) 0xff, (byte) 0xff, (byte) 0xff,
                (byte) 0xff, (byte) 0xff, (byte) 0xff, (byte) 0xff}, 0));
        assertEquals(
            new UUID(0xFFEEDDCCBBAA9988L, 0x7766554433221100L),
            Conversion.byteArrayToUuid(new byte[]{
                (byte) 0x88, (byte) 0x99, (byte) 0xaa, (byte) 0xbb, (byte) 0xcc, (byte) 0xdd,
                (byte) 0xee, (byte) 0xff, (byte) 0x00, (byte) 0x11, (byte) 0x22, (byte) 0x33,
                (byte) 0x44, (byte) 0x55, (byte) 0x66, (byte) 0x77}, 0));
        assertEquals(
            new UUID(0xFFEEDDCCBBAA9988L, 0x7766554433221100L),
            Conversion.byteArrayToUuid(new byte[]{
                0, 0, (byte) 0x88, (byte) 0x99, (byte) 0xaa, (byte) 0xbb, (byte) 0xcc, (byte) 0xdd,
                (byte) 0xee, (byte) 0xff, (byte) 0x00, (byte) 0x11, (byte) 0x22, (byte) 0x33,
                (byte) 0x44, (byte) 0x55, (byte) 0x66, (byte) 0x77}, 2));
    }

    /**
     * Tests {@link Conversion#invertBitOrder(byte b)}.
     */
    @Test
    public void testInvertBitOrder_byte() {
        //Semi
        int len = TestBinaryByteBlocks.byteSemiFalseBits.length;
        int lastIndex = len-1;
        for (int x=0; x<len; x++) {
            assertEquals(TestBinaryByteBlocks.byteSemiFalseBits[lastIndex-x],
                    Conversion.invertBitOrder(TestBinaryByteBlocks.byteSemiTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.byteSemiTrueBits[lastIndex-x],
                    Conversion.invertBitOrder(TestBinaryByteBlocks.byteSemiFalseBits[x]));
        }

        //Single
        len = TestBinaryByteBlocks.byteSingleTrueBits.length;
        lastIndex = len-1;
        for (int x=0; x<len; x++) {
            assertEquals(TestBinaryByteBlocks.byteSingleTrueBits[lastIndex-x],
                    Conversion.invertBitOrder(TestBinaryByteBlocks.byteSingleTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.byteSingleFalseBits[lastIndex-x],
                    Conversion.invertBitOrder(TestBinaryByteBlocks.byteSingleFalseBits[x]));
        }

        //Pair
        len = TestBinaryByteBlocks.bytePairTrueBits.length;
        lastIndex = len-1;
        for (int x=0; x<len; x++) {
            assertEquals(TestBinaryByteBlocks.bytePairTrueBits[lastIndex-x],
                    Conversion.invertBitOrder(TestBinaryByteBlocks.bytePairTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.bytePairFalseBits[lastIndex-x],
                    Conversion.invertBitOrder(TestBinaryByteBlocks.bytePairFalseBits[x]));
        }
    }

    /**
     * Tests {@link Conversion#invertBitOrder(boolean[])}.
     */
    @Test
    public void testInvertBitOrder_binary() {
        //Semi
        int len = TestBinaryByteBlocks.binarySemiFalseBits.length;
        int lastIndex = len-1;
        for (int x=0; x<len; x++) {
            assertArrayEquals(TestBinaryByteBlocks.binarySemiFalseBits[lastIndex-x],
                    Conversion.invertBitOrder(TestBinaryByteBlocks.binarySemiTrueBits[x]));
            assertArrayEquals(TestBinaryByteBlocks.binarySemiTrueBits[lastIndex-x],
                    Conversion.invertBitOrder(TestBinaryByteBlocks.binarySemiFalseBits[x]));
        }

        //Single
        len = TestBinaryByteBlocks.binarySingleTrueBits.length;
        lastIndex = len-1;
        for (int x=0; x<len; x++) {
            assertArrayEquals(TestBinaryByteBlocks.binarySingleTrueBits[lastIndex-x],
                    Conversion.invertBitOrder(TestBinaryByteBlocks.binarySingleTrueBits[x]));
            assertArrayEquals(TestBinaryByteBlocks.binarySingleFalseBits[lastIndex-x],
                    Conversion.invertBitOrder(TestBinaryByteBlocks.binarySingleFalseBits[x]));
        }

        //Pair
        len = TestBinaryByteBlocks.binaryPairTrueBits.length;
        lastIndex = len-1;
        for (int x=0; x<len; x++) {
            assertArrayEquals(TestBinaryByteBlocks.binaryPairTrueBits[lastIndex-x],
                    Conversion.invertBitOrder(TestBinaryByteBlocks.binaryPairTrueBits[x]));
            assertArrayEquals(TestBinaryByteBlocks.binaryPairFalseBits[lastIndex-x],
                    Conversion.invertBitOrder(TestBinaryByteBlocks.binaryPairFalseBits[x]));
        }

        //Undersized
        assertArrayEquals(TestBinaryByteBlocks.binary_Undersized[0],
                Conversion.invertBitOrder(TestBinaryByteBlocks.binary_Undersized[1]));

        //Oversized
        assertArrayEquals(TestBinaryByteBlocks.binary_Oversized_Leading_0s[0],
                Conversion.invertBitOrder(TestBinaryByteBlocks.binary_Oversized_Leading_0s[1]));
        assertArrayEquals(TestBinaryByteBlocks.binary_Oversized_Leading_1s[0],
                Conversion.invertBitOrder(TestBinaryByteBlocks.binary_Oversized_Leading_1s[1]));
        assertArrayEquals(TestBinaryByteBlocks.binary_Oversized_Trailing_0s[0],
                Conversion.invertBitOrder(TestBinaryByteBlocks.binary_Oversized_Trailing_0s[1]));
        assertArrayEquals(TestBinaryByteBlocks.binary_Oversized_Trailing_1s[0],
                Conversion.invertBitOrder(TestBinaryByteBlocks.binary_Oversized_Trailing_1s[1]));

        //Check Null & Empty
        assertArrayEquals(new boolean[0], Conversion.invertBitOrder((boolean[]) null));
        assertArrayEquals(new boolean[0], Conversion.invertBitOrder(new boolean[0]));
    }

    /**
     * Tests {@link Conversion#invertBitOrder(String)}.
     */
    @Test
    public void testInvertBitOrder_bitString() {
        //Semi
        int len = TestBinaryByteBlocks.binarySemiTrueBits.length;
        int lastIndex = len-1;
        for (int x=0; x<len; x++) {
            assertEquals(TestBinaryByteBlocks.bitStringSemiFalseBits[lastIndex-x],
                    Conversion.invertBitOrder(TestBinaryByteBlocks.bitStringSemiTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.bitStringSemiTrueBits[lastIndex-x],
                    Conversion.invertBitOrder(TestBinaryByteBlocks.bitStringSemiFalseBits[x]));
        }

        //Single
        len = TestBinaryByteBlocks.bitStringSingleTrueBits.length;
        lastIndex = len-1;
        for (int x=0; x<len; x++) {
            assertEquals(TestBinaryByteBlocks.bitStringSingleTrueBits[lastIndex-x],
                    Conversion.invertBitOrder(TestBinaryByteBlocks.bitStringSingleTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.bitStringSingleFalseBits[lastIndex-x],
                    Conversion.invertBitOrder(TestBinaryByteBlocks.bitStringSingleFalseBits[x]));
        }

        //Pair
        len = TestBinaryByteBlocks.bitStringPairTrueBits.length;
        lastIndex = len-1;
        for (int x=0; x<len; x++) {
            assertEquals(TestBinaryByteBlocks.bitStringPairTrueBits[lastIndex-x],
                    Conversion.invertBitOrder(TestBinaryByteBlocks.bitStringPairTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.bitStringPairFalseBits[lastIndex-x],
                    Conversion.invertBitOrder(TestBinaryByteBlocks.bitStringPairFalseBits[x]));
        }

        //Undersized
        assertEquals(TestBinaryByteBlocks.bitString_Undersized[0],
                Conversion.invertBitOrder(TestBinaryByteBlocks.bitString_Undersized[1]));

        //Oversized
        assertEquals(TestBinaryByteBlocks.bitString_Oversized_Leading_0s[0],
                Conversion.invertBitOrder(TestBinaryByteBlocks.bitString_Oversized_Leading_0s[1]));
        assertEquals(TestBinaryByteBlocks.bitString_Oversized_Leading_1s[0],
                Conversion.invertBitOrder(TestBinaryByteBlocks.bitString_Oversized_Leading_1s[1]));
        assertEquals(TestBinaryByteBlocks.bitString_Oversized_Trailing_0s[0],
                Conversion.invertBitOrder(TestBinaryByteBlocks.bitString_Oversized_Trailing_0s[1]));
        assertEquals(TestBinaryByteBlocks.bitString_Oversized_Trailing_1s[0],
                Conversion.invertBitOrder(TestBinaryByteBlocks.bitString_Oversized_Trailing_1s[1]));

        //Check Null & Empty
        assertEquals("", Conversion.invertBitOrder((String) null));
        assertEquals("", Conversion.invertBitOrder(""));
    }

    /**
     * Tests {@link Conversion#byteToBinary(byte)}.
     */
    @Test
    public void testByteToBinary_convenienceMethod() {
        boolean[] BASE_CODE_ANSWER = new boolean[8];

        //Semi
        int len = TestBinaryByteBlocks.byteSemiTrueBits.length;
        int lastIndex = len-1;
        for (int x=0; x<len; x++) {
            //Validate against existing implementation
            Conversion.byteToBinary(TestBinaryByteBlocks.byteSemiTrueBits[x], 0, BASE_CODE_ANSWER, 0, 8);
            assertArrayEquals(BASE_CODE_ANSWER, Conversion.byteToBinary(TestBinaryByteBlocks.byteSemiTrueBits[x]));
            Conversion.byteToBinary(TestBinaryByteBlocks.byteSemiFalseBits[x], 0, BASE_CODE_ANSWER, 0, 8);
            assertArrayEquals(BASE_CODE_ANSWER, Conversion.byteToBinary(TestBinaryByteBlocks.byteSemiFalseBits[x]));

            //Check expected value
            assertArrayEquals(TestBinaryByteBlocks.binarySemiFalseBits[lastIndex-x],
                    Conversion.byteToBinary(TestBinaryByteBlocks.byteSemiTrueBits[x]));
            assertArrayEquals(TestBinaryByteBlocks.binarySemiTrueBits[lastIndex-x],
                    Conversion.byteToBinary(TestBinaryByteBlocks.byteSemiFalseBits[x]));

        }

        //Single
        len = TestBinaryByteBlocks.byteSingleTrueBits.length;
        lastIndex = len-1;
        for (int x=0; x<len; x++) {
            //Validate against existing implementation
            Conversion.byteToBinary(TestBinaryByteBlocks.byteSingleTrueBits[x], 0, BASE_CODE_ANSWER, 0, 8);
            assertArrayEquals(BASE_CODE_ANSWER, Conversion.byteToBinary(TestBinaryByteBlocks.byteSingleTrueBits[x]));
            Conversion.byteToBinary(TestBinaryByteBlocks.byteSingleFalseBits[x], 0, BASE_CODE_ANSWER, 0, 8);
            assertArrayEquals(BASE_CODE_ANSWER, Conversion.byteToBinary(TestBinaryByteBlocks.byteSingleFalseBits[x]));

            //Check expected value
            assertArrayEquals(TestBinaryByteBlocks.binarySingleTrueBits[lastIndex-x],
                    Conversion.byteToBinary(TestBinaryByteBlocks.byteSingleTrueBits[x]));
            assertArrayEquals(TestBinaryByteBlocks.binarySingleFalseBits[lastIndex-x],
                    Conversion.byteToBinary(TestBinaryByteBlocks.byteSingleFalseBits[x]));
        }

        //Pair
        len = TestBinaryByteBlocks.bytePairTrueBits.length;
        lastIndex = len-1;
        for (int x=0; x<len; x++) {
            //Validate against existing implementation
            Conversion.byteToBinary(TestBinaryByteBlocks.bytePairTrueBits[x], 0, BASE_CODE_ANSWER, 0, 8);
            assertArrayEquals(BASE_CODE_ANSWER, Conversion.byteToBinary(TestBinaryByteBlocks.bytePairTrueBits[x]));
            Conversion.byteToBinary(TestBinaryByteBlocks.bytePairFalseBits[x], 0, BASE_CODE_ANSWER, 0, 8);
            assertArrayEquals(BASE_CODE_ANSWER, Conversion.byteToBinary(TestBinaryByteBlocks.bytePairFalseBits[x]));

            //Check expected value
            assertArrayEquals(TestBinaryByteBlocks.binaryPairTrueBits[lastIndex-x],
                    Conversion.byteToBinary(TestBinaryByteBlocks.bytePairTrueBits[x]));
            assertArrayEquals(TestBinaryByteBlocks.binaryPairFalseBits[lastIndex-x],
                    Conversion.byteToBinary(TestBinaryByteBlocks.bytePairFalseBits[x]));
        }
    }

    /**
     * Tests {@link Conversion#byteToBinaryRaw(byte)}.
     */
    @Test
    public void testByteToBinaryRaw_convenienceMethod() {
        //Semi
        int len = TestBinaryByteBlocks.binarySemiTrueBits.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertArrayEquals(TestBinaryByteBlocks.binarySemiTrueBits[x],
                    Conversion.byteToBinaryRaw(TestBinaryByteBlocks.byteSemiTrueBits[x]));
            assertArrayEquals(TestBinaryByteBlocks.binarySemiFalseBits[x],
                    Conversion.byteToBinaryRaw(TestBinaryByteBlocks.byteSemiFalseBits[x]));
        }

        //Single
        len = TestBinaryByteBlocks.binarySingleTrueBits.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertArrayEquals(TestBinaryByteBlocks.binarySingleTrueBits[x],
                    Conversion.byteToBinaryRaw(TestBinaryByteBlocks.byteSingleTrueBits[x]));
            assertArrayEquals(TestBinaryByteBlocks.binarySingleFalseBits[x],
                    Conversion.byteToBinaryRaw(TestBinaryByteBlocks.byteSingleFalseBits[x]));
        }

        //Pair
        len = TestBinaryByteBlocks.binaryPairTrueBits.length;
        for (int x=0; x<len; x++) {
            assertArrayEquals(TestBinaryByteBlocks.binaryPairTrueBits[x],
                    Conversion.byteToBinaryRaw(TestBinaryByteBlocks.bytePairTrueBits[x]));
            assertArrayEquals(TestBinaryByteBlocks.binaryPairFalseBits[x],
                    Conversion.byteToBinaryRaw(TestBinaryByteBlocks.bytePairFalseBits[x]));
        }
    }

    /**
     * Tests {@link Conversion#byteToBinaryMsb0(byte)}.
     */
    @Test
    public void testByteToBinaryMsb0_convenienceMethod() {
        //Semi
        int len = TestBinaryByteBlocks.binarySemiTrueBits.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertArrayEquals(TestBinaryByteBlocks.binarySemiTrueBits[x],
                    Conversion.byteToBinaryMsb0(TestBinaryByteBlocks.byteSemiTrueBits[x]));
            assertArrayEquals(TestBinaryByteBlocks.binarySemiFalseBits[x],
                    Conversion.byteToBinaryMsb0(TestBinaryByteBlocks.byteSemiFalseBits[x]));
        }

        //Single
        len = TestBinaryByteBlocks.binarySingleTrueBits.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertArrayEquals(TestBinaryByteBlocks.binarySingleTrueBits[x],
                    Conversion.byteToBinaryMsb0(TestBinaryByteBlocks.byteSingleTrueBits[x]));
            assertArrayEquals(TestBinaryByteBlocks.binarySingleFalseBits[x],
                    Conversion.byteToBinaryMsb0(TestBinaryByteBlocks.byteSingleFalseBits[x]));
        }

        //Pair
        len = TestBinaryByteBlocks.binaryPairTrueBits.length;
        for (int x=0; x<len; x++) {
            assertArrayEquals(TestBinaryByteBlocks.binaryPairTrueBits[x],
                    Conversion.byteToBinaryMsb0(TestBinaryByteBlocks.bytePairTrueBits[x]));
            assertArrayEquals(TestBinaryByteBlocks.binaryPairFalseBits[x],
                    Conversion.byteToBinaryMsb0(TestBinaryByteBlocks.bytePairFalseBits[x]));
        }
    }

    /**
     * Tests {@link Conversion#binaryToByte(boolean[])}.
     */
    @Test
    public void testBinaryToByte_convenienceMethod() {
        byte BASE_CODE_ANSWER = 0;

        //Semi
        int len = TestBinaryByteBlocks.binarySemiTrueBits.length;
        int lastIndex = len-1;
        for (int x=0; x<len; x++) {
            //Validate against existing implementation
            BASE_CODE_ANSWER = Conversion.binaryToByte(TestBinaryByteBlocks.binarySemiTrueBits[x], 0, (byte) 0, 0, 8);
            assertEquals(BASE_CODE_ANSWER, Conversion.binaryToByte(TestBinaryByteBlocks.binarySemiTrueBits[x]));
            BASE_CODE_ANSWER = Conversion.binaryToByte(TestBinaryByteBlocks.binarySemiFalseBits[x], 0, (byte) 0, 0, 8);
            assertEquals(BASE_CODE_ANSWER, Conversion.binaryToByte(TestBinaryByteBlocks.binarySemiFalseBits[x]));

            //Check expected value
            assertEquals(TestBinaryByteBlocks.byteSemiFalseBits[lastIndex-x],
                    Conversion.binaryToByte(TestBinaryByteBlocks.binarySemiTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.byteSemiTrueBits[lastIndex-x],
                    Conversion.binaryToByte(TestBinaryByteBlocks.binarySemiFalseBits[x]));

        }

        //Single
        len = TestBinaryByteBlocks.binarySingleTrueBits.length;
        lastIndex = len-1;
        for (int x=0; x<len; x++) {
            //Validate against existing implementation
            BASE_CODE_ANSWER = Conversion.binaryToByte(TestBinaryByteBlocks.binarySingleTrueBits[x], 0, (byte) 0, 0, 8);
            assertEquals(BASE_CODE_ANSWER, Conversion.binaryToByte(TestBinaryByteBlocks.binarySingleTrueBits[x]));
            BASE_CODE_ANSWER = Conversion.binaryToByte(TestBinaryByteBlocks.binarySingleFalseBits[x], 0, (byte) 0, 0, 8);
            assertEquals(BASE_CODE_ANSWER, Conversion.binaryToByte(TestBinaryByteBlocks.binarySingleFalseBits[x]));

            //Check expected value
            assertEquals(TestBinaryByteBlocks.byteSingleTrueBits[lastIndex-x],
                    Conversion.binaryToByte(TestBinaryByteBlocks.binarySingleTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.byteSingleFalseBits[lastIndex-x],
                    Conversion.binaryToByte(TestBinaryByteBlocks.binarySingleFalseBits[x]));
        }

        //Pair
        len = TestBinaryByteBlocks.binaryPairTrueBits.length;
        lastIndex = len-1;
        for (int x=0; x<len; x++) {
            //Validate against existing implementation
            BASE_CODE_ANSWER = Conversion.binaryToByte(TestBinaryByteBlocks.binaryPairTrueBits[x], 0, (byte) 0, 0, 8);
            assertEquals(BASE_CODE_ANSWER, Conversion.binaryToByte(TestBinaryByteBlocks.binaryPairTrueBits[x]));
            BASE_CODE_ANSWER = Conversion.binaryToByte(TestBinaryByteBlocks.binaryPairFalseBits[x], 0, (byte) 0, 0, 8);
            assertEquals(BASE_CODE_ANSWER, Conversion.binaryToByte(TestBinaryByteBlocks.binaryPairFalseBits[x]));

            //Check expected value
            assertEquals(TestBinaryByteBlocks.bytePairTrueBits[lastIndex-x],
                    Conversion.binaryToByte(TestBinaryByteBlocks.binaryPairTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.bytePairFalseBits[lastIndex-x],
                    Conversion.binaryToByte(TestBinaryByteBlocks.binaryPairFalseBits[x]));
        }

        //Undersized
        len = TestBinaryByteBlocks.binary_Undersized.length;
        lastIndex = len-1;
        for (int x=0; x<len; x++) {
            //Validate against existing implementation
            final boolean[] undersizedBits = TestBinaryByteBlocks.binary_Undersized[x];
            BASE_CODE_ANSWER = Conversion.binaryToByte(undersizedBits, 0, (byte) 0, 0, undersizedBits.length);
            assertEquals(BASE_CODE_ANSWER, Conversion.binaryToByte(undersizedBits));

            //Check expected value
            assertEquals(TestBinaryByteBlocks.byte_Undersized[lastIndex-x],
                    Conversion.binaryToByte(TestBinaryByteBlocks.binary_Undersized[x]));
        }

        //Check Null & Empty
        assertEquals(0, Conversion.binaryToByte((boolean[]) null));
        assertEquals(0, Conversion.binaryToByte(new boolean[0]));
    }

    /**
     * Tests {@link Conversion#binaryToByte(boolean[])}.
     */
    @Test(expected=IllegalArgumentException.class)
    public void testBinaryToByte_convenienceMethod_oversizedException() {
        Conversion.binaryToByte(TestBinaryByteBlocks.binary_Oversized_Leading_0s[0]);
    }

    /**
     * Tests {@link Conversion#binaryToByteRaw(boolean[])}.
     */
    @Test
    public void testBinaryToByteRaw_convenienceMethod() {
        //Semi
        int len = TestBinaryByteBlocks.binarySemiTrueBits.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertEquals(TestBinaryByteBlocks.byteSemiTrueBits[x],
                    Conversion.binaryToByteRaw(TestBinaryByteBlocks.binarySemiTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.byteSemiFalseBits[x],
                    Conversion.binaryToByteRaw(TestBinaryByteBlocks.binarySemiFalseBits[x]));
        }

        //Single
        len = TestBinaryByteBlocks.binarySingleTrueBits.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertEquals(TestBinaryByteBlocks.byteSingleTrueBits[x],
                    Conversion.binaryToByteRaw(TestBinaryByteBlocks.binarySingleTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.byteSingleFalseBits[x],
                    Conversion.binaryToByteRaw(TestBinaryByteBlocks.binarySingleFalseBits[x]));
        }

        //Pair
        len = TestBinaryByteBlocks.binaryPairTrueBits.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertEquals(TestBinaryByteBlocks.bytePairTrueBits[x],
                    Conversion.binaryToByteRaw(TestBinaryByteBlocks.binaryPairTrueBits[x]));
            //Check expected value
            assertEquals(TestBinaryByteBlocks.bytePairFalseBits[x],
                    Conversion.binaryToByteRaw(TestBinaryByteBlocks.binaryPairFalseBits[x]));
        }

        //Undersized
        len = TestBinaryByteBlocks.binary_Undersized.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertEquals(TestBinaryByteBlocks.byte_Undersized_Msb0_fullByteValue[x],
                    Conversion.binaryToByteRaw(TestBinaryByteBlocks.binary_Undersized[x]));
        }

        //Check Null & Empty
        assertEquals(0, Conversion.binaryToByteRaw((boolean[]) null));
        assertEquals(0, Conversion.binaryToByteRaw(new boolean[0]));
    }

    /**
     * Tests {@link Conversion#binaryToByteRaw(boolean[])}.
     */
    @Test(expected=IllegalArgumentException.class)
    public void testBinaryToByteRaw_convenienceMethod_oversizedException() {
        Conversion.binaryToByteRaw(TestBinaryByteBlocks.binary_Oversized_Leading_0s[0]);
    }

    /**
     * Tests {@link Conversion#binaryToByteMsb0(boolean[])}.
     */
    @Test
    public void testBinaryToByteMsb0_convenienceMethod() {
        //Semi
        int len = TestBinaryByteBlocks.binarySemiTrueBits.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertEquals(TestBinaryByteBlocks.byteSemiTrueBits[x],
                    Conversion.binaryToByteMsb0(TestBinaryByteBlocks.binarySemiTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.byteSemiFalseBits[x],
                    Conversion.binaryToByteMsb0(TestBinaryByteBlocks.binarySemiFalseBits[x]));
        }

        //Single
        len = TestBinaryByteBlocks.binarySingleTrueBits.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertEquals(TestBinaryByteBlocks.byteSingleTrueBits[x],
                    Conversion.binaryToByteMsb0(TestBinaryByteBlocks.binarySingleTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.byteSingleFalseBits[x],
                    Conversion.binaryToByteMsb0(TestBinaryByteBlocks.binarySingleFalseBits[x]));
        }

        //Pair
        len = TestBinaryByteBlocks.binaryPairTrueBits.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertEquals(TestBinaryByteBlocks.bytePairTrueBits[x],
                    Conversion.binaryToByteMsb0(TestBinaryByteBlocks.binaryPairTrueBits[x]));
            //Check expected value
            assertEquals(TestBinaryByteBlocks.bytePairFalseBits[x],
                    Conversion.binaryToByteMsb0(TestBinaryByteBlocks.binaryPairFalseBits[x]));
        }

        //Undersized
        len = TestBinaryByteBlocks.binary_Undersized.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertEquals(TestBinaryByteBlocks.byte_Undersized_Msb0_fullByteValue[x],
                    Conversion.binaryToByteMsb0(TestBinaryByteBlocks.binary_Undersized[x]));
        }

        //Check Null & Empty
        assertEquals(0, Conversion.binaryToByteMsb0((boolean[]) null));
        assertEquals(0, Conversion.binaryToByteMsb0(new boolean[0]));
    }

    /**
     * Tests {@link Conversion#binaryToByteMsb0(boolean[])}.
     */
    @Test(expected=IllegalArgumentException.class)
    public void testBinaryToByteMsb0_convenienceMethod_oversizedException() {
        Conversion.binaryToByteMsb0(TestBinaryByteBlocks.binary_Oversized_Leading_0s[0]);
    }

    /**
     * Tests {@link Conversion#stripStringDelimiters(String)}.
     */
    @Test
    public void testStripStringDelimiters() {
        //Check Null & Empty
        assertEquals("", Conversion.stripStringDelimiters((String) null));
        assertEquals("", Conversion.stripStringDelimiters(""));
        assertEquals("", Conversion.stripStringDelimiters(".--..._\t\t__  . .\t_ --_-.   \t-"));

        //Check ignore delimiters
        int len = TestBinaryByteBlocks.bitString_Delimited_Dashes.length;
        for (int x=0; x<len; x++) {
            assertEquals(TestBinaryByteBlocks.bitString_Delimited_None[x],
                    Conversion.stripStringDelimiters(TestBinaryByteBlocks.bitString_Delimited_Spaces[x]));
            assertEquals(TestBinaryByteBlocks.bitString_Delimited_None[x],
                    Conversion.stripStringDelimiters(TestBinaryByteBlocks.bitString_Delimited_Dashes[x]));
            assertEquals(TestBinaryByteBlocks.bitString_Delimited_None[x],
                    Conversion.stripStringDelimiters(TestBinaryByteBlocks.bitString_Delimited_Underscores[x]));
            assertEquals(TestBinaryByteBlocks.bitString_Delimited_None[x],
                    Conversion.stripStringDelimiters(TestBinaryByteBlocks.bitString_Delimited_Periods[x]));
            assertEquals(TestBinaryByteBlocks.bitString_Delimited_None[x],
                    Conversion.stripStringDelimiters(TestBinaryByteBlocks.bitString_Delimited_All[x]));
        }
    }

    /**
     * Tests {@link Conversion#bitStringToBinaryRaw(String)}.
     */
    @Test
    public void testBitStringToBinaryRaw() {
        //Semi
        int len = TestBinaryByteBlocks.binarySemiTrueBits.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertArrayEquals(TestBinaryByteBlocks.binarySemiTrueBits[x],
                    Conversion.bitStringToBinaryRaw(TestBinaryByteBlocks.bitStringSemiTrueBits[x]));
            assertArrayEquals(TestBinaryByteBlocks.binarySemiFalseBits[x],
                    Conversion.bitStringToBinaryRaw(TestBinaryByteBlocks.bitStringSemiFalseBits[x]));
        }

        //Single
        len = TestBinaryByteBlocks.binarySingleTrueBits.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertArrayEquals(TestBinaryByteBlocks.binarySingleTrueBits[x],
                    Conversion.bitStringToBinaryRaw(TestBinaryByteBlocks.bitStringSingleTrueBits[x]));
            assertArrayEquals(TestBinaryByteBlocks.binarySingleFalseBits[x],
                    Conversion.bitStringToBinaryRaw(TestBinaryByteBlocks.bitStringSingleFalseBits[x]));
        }

        //Pair
        len = TestBinaryByteBlocks.binaryPairTrueBits.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertArrayEquals(TestBinaryByteBlocks.binaryPairTrueBits[x],
                    Conversion.bitStringToBinaryRaw(TestBinaryByteBlocks.bitStringPairTrueBits[x]));
            assertArrayEquals(TestBinaryByteBlocks.binaryPairFalseBits[x],
                    Conversion.bitStringToBinaryRaw(TestBinaryByteBlocks.bitStringPairFalseBits[x]));
        }

        //Undersized
        len = TestBinaryByteBlocks.binary_Undersized.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertArrayEquals(TestBinaryByteBlocks.binary_Undersized[x],
                    Conversion.bitStringToBinaryRaw(TestBinaryByteBlocks.bitString_Undersized[x]));
        }

        //Oversized
        len = TestBinaryByteBlocks.bitString_Oversized_Leading_0s.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertArrayEquals(TestBinaryByteBlocks.binary_Oversized_Leading_0s[x],
                    Conversion.bitStringToBinaryRaw(TestBinaryByteBlocks.bitString_Oversized_Leading_0s[x]));
            assertArrayEquals(TestBinaryByteBlocks.binary_Oversized_Leading_1s[x],
                    Conversion.bitStringToBinaryRaw(TestBinaryByteBlocks.bitString_Oversized_Leading_1s[x]));
            assertArrayEquals(TestBinaryByteBlocks.binary_Oversized_Trailing_0s[x],
                    Conversion.bitStringToBinaryRaw(TestBinaryByteBlocks.bitString_Oversized_Trailing_0s[x]));
            assertArrayEquals(TestBinaryByteBlocks.binary_Oversized_Trailing_1s[x],
                    Conversion.bitStringToBinaryRaw(TestBinaryByteBlocks.bitString_Oversized_Trailing_1s[x]));
        }

        //Check Null & Empty
        assertArrayEquals(new boolean[0], Conversion.bitStringToBinaryRaw((String) null));
        assertArrayEquals(new boolean[0], Conversion.bitStringToBinaryRaw(""));

        //Check ignore delimiters
        assertArrayEquals(new boolean[0], Conversion.bitStringToBinaryRaw("- \t_"));
        assertArrayEquals(new boolean[0], Conversion.bitStringToBinaryRaw("- \t_"));

        for (int x=0; x<TestBinaryByteBlocks.bitString_Delimited_Dashes.length; x++) {
            assertArrayEquals(TestBinaryByteBlocks.binary_Delimited_None[x],
                    Conversion.bitStringToBinaryRaw(TestBinaryByteBlocks.bitString_Delimited_Spaces[x]));
            assertArrayEquals(TestBinaryByteBlocks.binary_Delimited_None[x],
                    Conversion.bitStringToBinaryRaw(TestBinaryByteBlocks.bitString_Delimited_Dashes[x]));
            assertArrayEquals(TestBinaryByteBlocks.binary_Delimited_None[x],
                    Conversion.bitStringToBinaryRaw(TestBinaryByteBlocks.bitString_Delimited_Underscores[x]));
            assertArrayEquals(TestBinaryByteBlocks.binary_Delimited_None[x],
                    Conversion.bitStringToBinaryRaw(TestBinaryByteBlocks.bitString_Delimited_Periods[x]));
            assertArrayEquals(TestBinaryByteBlocks.binary_Delimited_None[x],
                    Conversion.bitStringToBinaryRaw(TestBinaryByteBlocks.bitString_Delimited_All[x]));
        }
    }

    /**
     * Tests {@link Conversion#binaryToBitStringRaw(boolean[])}.
     */
    @Test
    public void testBinaryToBitStringRaw() {
        //Semi
        int len = TestBinaryByteBlocks.bitStringSemiTrueBits.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertEquals(TestBinaryByteBlocks.bitStringSemiTrueBits[x],
                    Conversion.binaryToBitStringRaw(TestBinaryByteBlocks.binarySemiTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.bitStringSemiFalseBits[x],
                    Conversion.binaryToBitStringRaw(TestBinaryByteBlocks.binarySemiFalseBits[x]));
        }

        //Single
        len = TestBinaryByteBlocks.bitStringSingleTrueBits.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertEquals(TestBinaryByteBlocks.bitStringSingleTrueBits[x],
                    Conversion.binaryToBitStringRaw(TestBinaryByteBlocks.binarySingleTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.bitStringSingleFalseBits[x],
                    Conversion.binaryToBitStringRaw(TestBinaryByteBlocks.binarySingleFalseBits[x]));
        }

        //Pair
        len = TestBinaryByteBlocks.bitStringPairTrueBits.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertEquals(TestBinaryByteBlocks.bitStringPairTrueBits[x],
                    Conversion.binaryToBitStringRaw(TestBinaryByteBlocks.binaryPairTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.bitStringPairFalseBits[x],
                    Conversion.binaryToBitStringRaw(TestBinaryByteBlocks.binaryPairFalseBits[x]));
        }

        //Undersized
        len = TestBinaryByteBlocks.binary_Undersized.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertEquals(TestBinaryByteBlocks.bitString_Undersized[x],
                    Conversion.binaryToBitStringRaw(TestBinaryByteBlocks.binary_Undersized[x]));
        }

        //Oversized
        len = TestBinaryByteBlocks.bitString_Oversized_Leading_0s.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertEquals(TestBinaryByteBlocks.bitString_Oversized_Leading_0s[x],
                    Conversion.binaryToBitStringRaw(TestBinaryByteBlocks.binary_Oversized_Leading_0s[x]));
            assertEquals(TestBinaryByteBlocks.bitString_Oversized_Leading_1s[x],
                    Conversion.binaryToBitStringRaw(TestBinaryByteBlocks.binary_Oversized_Leading_1s[x]));
            assertEquals(TestBinaryByteBlocks.bitString_Oversized_Trailing_0s[x],
                    Conversion.binaryToBitStringRaw(TestBinaryByteBlocks.binary_Oversized_Trailing_0s[x]));
            assertEquals(TestBinaryByteBlocks.bitString_Oversized_Trailing_1s[x],
                    Conversion.binaryToBitStringRaw(TestBinaryByteBlocks.binary_Oversized_Trailing_1s[x]));
        }

        //Check Null & Empty
        assertEquals("", Conversion.binaryToBitStringRaw((boolean[]) null));
        assertEquals("", Conversion.binaryToBitStringRaw(new boolean[0]));
    }

    /**
     * Tests {@link Conversion#byteToBitString(byte)}.
     */
    @Test
    public void testByteToBitString() {
        //Semi
        int len = TestBinaryByteBlocks.bitStringSemiFalseBits.length;
        int lastIndex = len-1;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertEquals(TestBinaryByteBlocks.bitStringSemiFalseBits[lastIndex-x],
                    Conversion.byteToBitString(TestBinaryByteBlocks.byteSemiTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.bitStringSemiTrueBits[lastIndex-x],
                    Conversion.byteToBitString(TestBinaryByteBlocks.byteSemiFalseBits[x]));
        }

        //Single
        len = TestBinaryByteBlocks.bitStringSingleTrueBits.length;
        lastIndex = len-1;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertEquals(TestBinaryByteBlocks.bitStringSingleTrueBits[lastIndex-x],
                    Conversion.byteToBitString(TestBinaryByteBlocks.byteSingleTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.bitStringSingleFalseBits[lastIndex-x],
                    Conversion.byteToBitString(TestBinaryByteBlocks.byteSingleFalseBits[x]));
        }

        //Pair
        len = TestBinaryByteBlocks.bitStringPairTrueBits.length;
        lastIndex = len-1;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertEquals(TestBinaryByteBlocks.bitStringPairTrueBits[lastIndex-x],
                    Conversion.byteToBitString(TestBinaryByteBlocks.bytePairTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.bitStringPairFalseBits[lastIndex-x],
                    Conversion.byteToBitString(TestBinaryByteBlocks.bytePairFalseBits[x]));
        }
    }

    /**
     * Tests {@link Conversion#byteToBitStringRaw(byte)}.
     */
    @Test
    public void testByteToBitStringRaw() {
        //Semi
        int len = TestBinaryByteBlocks.bitStringSemiFalseBits.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertEquals(TestBinaryByteBlocks.bitStringSemiTrueBits[x],
                    Conversion.byteToBitStringRaw(TestBinaryByteBlocks.byteSemiTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.bitStringSemiFalseBits[x],
                    Conversion.byteToBitStringRaw(TestBinaryByteBlocks.byteSemiFalseBits[x]));
        }

        //Single
        len = TestBinaryByteBlocks.bitStringSingleTrueBits.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertEquals(TestBinaryByteBlocks.bitStringSingleTrueBits[x],
                    Conversion.byteToBitStringRaw(TestBinaryByteBlocks.byteSingleTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.bitStringSingleFalseBits[x],
                    Conversion.byteToBitStringRaw(TestBinaryByteBlocks.byteSingleFalseBits[x]));
        }

        //Pair
        len = TestBinaryByteBlocks.bitStringPairTrueBits.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertEquals(TestBinaryByteBlocks.bitStringPairTrueBits[x],
                    Conversion.byteToBitStringRaw(TestBinaryByteBlocks.bytePairTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.bitStringPairFalseBits[x],
                    Conversion.byteToBitStringRaw(TestBinaryByteBlocks.bytePairFalseBits[x]));
        }
    }

    /**
     * Tests {@link Conversion#byteToBitStringMsb0(byte)}.
     */
    @Test
    public void testByteToBitStringMsb0() {
        //Semi
        int len = TestBinaryByteBlocks.bitStringSemiFalseBits.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertEquals(TestBinaryByteBlocks.bitStringSemiTrueBits[x],
                    Conversion.byteToBitStringMsb0(TestBinaryByteBlocks.byteSemiTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.bitStringSemiFalseBits[x],
                    Conversion.byteToBitStringMsb0(TestBinaryByteBlocks.byteSemiFalseBits[x]));
        }

        //Single
        len = TestBinaryByteBlocks.bitStringSingleTrueBits.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertEquals(TestBinaryByteBlocks.bitStringSingleTrueBits[x],
                    Conversion.byteToBitStringMsb0(TestBinaryByteBlocks.byteSingleTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.bitStringSingleFalseBits[x],
                    Conversion.byteToBitStringMsb0(TestBinaryByteBlocks.byteSingleFalseBits[x]));
        }

        //Pair
        len = TestBinaryByteBlocks.bitStringPairTrueBits.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertEquals(TestBinaryByteBlocks.bitStringPairTrueBits[x],
                    Conversion.byteToBitStringMsb0(TestBinaryByteBlocks.bytePairTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.bitStringPairFalseBits[x],
                    Conversion.byteToBitStringMsb0(TestBinaryByteBlocks.bytePairFalseBits[x]));
        }
    }

    /**
     * Tests {@link Conversion#bitStringToByte(String)}.
     */
    @Test
    public void testBitStringToByte() {
        //Semi
        int len = TestBinaryByteBlocks.byteSemiTrueBits.length;
        int lastIndex = len-1;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertEquals(TestBinaryByteBlocks.byteSemiFalseBits[lastIndex-x],
                    Conversion.bitStringToByte(TestBinaryByteBlocks.bitStringSemiTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.byteSemiTrueBits[lastIndex-x],
                    Conversion.bitStringToByte(TestBinaryByteBlocks.bitStringSemiFalseBits[x]));
        }

        //Single
        len = TestBinaryByteBlocks.byteSingleTrueBits.length;
        lastIndex = len-1;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertEquals(TestBinaryByteBlocks.byteSingleTrueBits[lastIndex-x],
                    Conversion.bitStringToByte(TestBinaryByteBlocks.bitStringSingleTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.byteSingleFalseBits[lastIndex-x],
                    Conversion.bitStringToByte(TestBinaryByteBlocks.bitStringSingleFalseBits[x]));
        }

        //Pair
        len = TestBinaryByteBlocks.bytePairTrueBits.length;
        lastIndex = len-1;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertEquals(TestBinaryByteBlocks.bytePairTrueBits[lastIndex-x],
                    Conversion.bitStringToByte(TestBinaryByteBlocks.bitStringPairTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.bytePairFalseBits[lastIndex-x],
                    Conversion.bitStringToByte(TestBinaryByteBlocks.bitStringPairFalseBits[x]));
        }

        //Check Null & Empty
        assertEquals(0, Conversion.bitStringToByte((String) null));
        assertEquals(0, Conversion.bitStringToByte(""));

        //Check ignore delimiters
        assertEquals(0, Conversion.bitStringToByte("- \t_"));
        assertEquals(0, Conversion.bitStringToByte("- \t_"));

        len = TestBinaryByteBlocks.bitString_Delimited_Dashes.length;
        lastIndex = len-1;
        for (int x=0; x<len; x++) {
            assertEquals(TestBinaryByteBlocks.byte_Delimited_None[lastIndex-x],
                    Conversion.bitStringToByte(TestBinaryByteBlocks.bitString_Delimited_Spaces[x]));
            assertEquals(TestBinaryByteBlocks.byte_Delimited_None[lastIndex-x],
                    Conversion.bitStringToByte(TestBinaryByteBlocks.bitString_Delimited_Dashes[x]));
            assertEquals(TestBinaryByteBlocks.byte_Delimited_None[lastIndex-x],
                    Conversion.bitStringToByte(TestBinaryByteBlocks.bitString_Delimited_Underscores[x]));
            assertEquals(TestBinaryByteBlocks.byte_Delimited_None[lastIndex-x],
                    Conversion.bitStringToByte(TestBinaryByteBlocks.bitString_Delimited_Periods[x]));
            assertEquals(TestBinaryByteBlocks.byte_Delimited_None[lastIndex-x],
                    Conversion.bitStringToByte(TestBinaryByteBlocks.bitString_Delimited_All[x]));
        }
    }

    /**
     * Tests {@link Conversion#bitStringToByte(String)}.
     */
    @Test(expected=IllegalArgumentException.class)
    public void testBitStringToByte_oversizedException() {
        Conversion.bitStringToByte(TestBinaryByteBlocks.bitString_Oversized_Leading_0s[0]);
    }

    /**
     * Tests {@link Conversion#bitStringToByte(String)}.
     */
    @Test(expected=IllegalArgumentException.class)
    public void testBitStringToByte_notBitStringException() {
        Conversion.bitStringToByte("Not Bit String");
    }

    /**
     * Tests {@link Conversion#bitStringToByteRaw(String)}.
     */
    @Test
    public void testBitStringToByteRaw() {
        //Semi
        int len = TestBinaryByteBlocks.byteSemiTrueBits.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertEquals(TestBinaryByteBlocks.byteSemiTrueBits[x],
                    Conversion.bitStringToByteRaw(TestBinaryByteBlocks.bitStringSemiTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.byteSemiFalseBits[x],
                    Conversion.bitStringToByteRaw(TestBinaryByteBlocks.bitStringSemiFalseBits[x]));
        }

        //Single
        len = TestBinaryByteBlocks.byteSingleTrueBits.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertEquals(TestBinaryByteBlocks.byteSingleTrueBits[x],
                    Conversion.bitStringToByteRaw(TestBinaryByteBlocks.bitStringSingleTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.byteSingleFalseBits[x],
                    Conversion.bitStringToByteRaw(TestBinaryByteBlocks.bitStringSingleFalseBits[x]));
        }

        //Pair
        len = TestBinaryByteBlocks.bytePairTrueBits.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertEquals(TestBinaryByteBlocks.bytePairTrueBits[x],
                    Conversion.bitStringToByteRaw(TestBinaryByteBlocks.bitStringPairTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.bytePairFalseBits[x],
                    Conversion.bitStringToByteRaw(TestBinaryByteBlocks.bitStringPairFalseBits[x]));
        }

        //Check Null & Empty
        assertEquals(0, Conversion.bitStringToByteRaw((String) null));
        assertEquals(0, Conversion.bitStringToByteRaw(""));

        //Check ignore delimiters
        assertEquals(0, Conversion.bitStringToByteRaw("- \t_"));
        assertEquals(0, Conversion.bitStringToByteRaw("- \t_"));

        for (int x=0; x<TestBinaryByteBlocks.bitString_Delimited_Dashes.length; x++) {
            assertEquals(TestBinaryByteBlocks.byte_Delimited_None[x],
                    Conversion.bitStringToByteRaw(TestBinaryByteBlocks.bitString_Delimited_Spaces[x]));
            assertEquals(TestBinaryByteBlocks.byte_Delimited_None[x],
                    Conversion.bitStringToByteRaw(TestBinaryByteBlocks.bitString_Delimited_Dashes[x]));
            assertEquals(TestBinaryByteBlocks.byte_Delimited_None[x],
                    Conversion.bitStringToByteRaw(TestBinaryByteBlocks.bitString_Delimited_Underscores[x]));
            assertEquals(TestBinaryByteBlocks.byte_Delimited_None[x],
                    Conversion.bitStringToByteRaw(TestBinaryByteBlocks.bitString_Delimited_Periods[x]));
            assertEquals(TestBinaryByteBlocks.byte_Delimited_None[x],
                    Conversion.bitStringToByteRaw(TestBinaryByteBlocks.bitString_Delimited_All[x]));
        }
    }

    /**
     * Tests {@link Conversion#bitStringToByteRaw(String)}.
     */
    @Test(expected=IllegalArgumentException.class)
    public void testBitStringToByteRaw_oversizedException() {
        Conversion.bitStringToByteRaw(TestBinaryByteBlocks.bitString_Oversized_Leading_0s[0]);
    }

    /**
     * Tests {@link Conversion#bitStringToByteRaw(String)}.
     */
    @Test(expected=IllegalArgumentException.class)
    public void testBitStringToByteRaw_notBitStringException() {
        Conversion.bitStringToByteRaw("Not A Bit String");
    }

    /**
     * Tests {@link Conversion#bitStringToByteMsb0(String)}.
     */
    @Test
    public void testBitStringToByteMsb0() {
        //Semi
        int len = TestBinaryByteBlocks.byteSemiTrueBits.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertEquals(TestBinaryByteBlocks.byteSemiTrueBits[x],
                    Conversion.bitStringToByteMsb0(TestBinaryByteBlocks.bitStringSemiTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.byteSemiFalseBits[x],
                    Conversion.bitStringToByteMsb0(TestBinaryByteBlocks.bitStringSemiFalseBits[x]));
        }

        //Single
        len = TestBinaryByteBlocks.byteSingleTrueBits.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertEquals(TestBinaryByteBlocks.byteSingleTrueBits[x],
                    Conversion.bitStringToByteMsb0(TestBinaryByteBlocks.bitStringSingleTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.byteSingleFalseBits[x],
                    Conversion.bitStringToByteMsb0(TestBinaryByteBlocks.bitStringSingleFalseBits[x]));
        }

        //Pair
        len = TestBinaryByteBlocks.bytePairTrueBits.length;
        for (int x=0; x<len; x++) {
            //Check expected value
            assertEquals(TestBinaryByteBlocks.bytePairTrueBits[x],
                    Conversion.bitStringToByteMsb0(TestBinaryByteBlocks.bitStringPairTrueBits[x]));
            assertEquals(TestBinaryByteBlocks.bytePairFalseBits[x],
                    Conversion.bitStringToByteMsb0(TestBinaryByteBlocks.bitStringPairFalseBits[x]));
        }

        //Check Null & Empty
        assertEquals(0, Conversion.bitStringToByteMsb0((String) null));
        assertEquals(0, Conversion.bitStringToByteMsb0(""));

        //Check ignore delimiters
        assertEquals(0, Conversion.bitStringToByteMsb0("- \t_"));
        assertEquals(0, Conversion.bitStringToByteMsb0("- \t_"));

        for (int x=0; x<TestBinaryByteBlocks.bitString_Delimited_Dashes.length; x++) {
            assertEquals(TestBinaryByteBlocks.byte_Delimited_None[x],
                    Conversion.bitStringToByteMsb0(TestBinaryByteBlocks.bitString_Delimited_Spaces[x]));
            assertEquals(TestBinaryByteBlocks.byte_Delimited_None[x],
                    Conversion.bitStringToByteMsb0(TestBinaryByteBlocks.bitString_Delimited_Dashes[x]));
            assertEquals(TestBinaryByteBlocks.byte_Delimited_None[x],
                    Conversion.bitStringToByteMsb0(TestBinaryByteBlocks.bitString_Delimited_Underscores[x]));
            assertEquals(TestBinaryByteBlocks.byte_Delimited_None[x],
                    Conversion.bitStringToByteMsb0(TestBinaryByteBlocks.bitString_Delimited_Periods[x]));
            assertEquals(TestBinaryByteBlocks.byte_Delimited_None[x],
                    Conversion.bitStringToByteMsb0(TestBinaryByteBlocks.bitString_Delimited_All[x]));
        }
    }

    /**
     * Tests {@link Conversion#bitStringToByteMsb0(String)}.
     */
    @Test(expected=IllegalArgumentException.class)
    public void testBitStringToByteMsb0_oversizedException() {
        Conversion.bitStringToByteMsb0(TestBinaryByteBlocks.bitString_Oversized_Leading_0s[0]);
    }

    /**
     * Tests {@link Conversion#bitStringToByteMsb0(String)}.
     */
    @Test(expected=IllegalArgumentException.class)
    public void testBitStringToByteMsb0_notBitStringException() {
        Conversion.bitStringToByteMsb0("Not A Bit String");
    }

    /**
     * Tests {@link Conversion#hexToByteArrayRaw(String)}.
     */
    @Test
    public void testHexToByteArrayRaw() {
        //Check Values
        assertArrayEquals(TestBinaryByteBlocks.hexStringByteVals_Delimited_None,
                Conversion.hexToByteArrayRaw(TestBinaryByteBlocks.hexString_Delimited_None));
        assertArrayEquals(TestBinaryByteBlocks.hexStringByteVals_Delimited_None,
                Conversion.hexToByteArrayRaw(TestBinaryByteBlocks.hexString_Delimited_Spaces));
        assertArrayEquals(TestBinaryByteBlocks.hexStringByteVals_Delimited_None,
                Conversion.hexToByteArrayRaw(TestBinaryByteBlocks.hexString_Delimited_Dashes));
        assertArrayEquals(TestBinaryByteBlocks.hexStringByteVals_Delimited_None,
                Conversion.hexToByteArrayRaw(TestBinaryByteBlocks.hexString_Delimited_Underscores));
        assertArrayEquals(TestBinaryByteBlocks.hexStringByteVals_Delimited_None,
                Conversion.hexToByteArrayRaw(TestBinaryByteBlocks.hexString_Delimited_Periods));
        assertArrayEquals(TestBinaryByteBlocks.hexStringByteVals_Delimited_None,
                Conversion.hexToByteArrayRaw(TestBinaryByteBlocks.hexString_Delimited_Tabs));
        assertArrayEquals(TestBinaryByteBlocks.hexStringByteVals_Delimited_None,
                Conversion.hexToByteArrayRaw(TestBinaryByteBlocks.hexString_Delimited_All));

        //Check Null & Empty
        assertArrayEquals(new byte[0], Conversion.hexToByteArrayRaw((String)null));
        assertArrayEquals(new byte[0], Conversion.hexToByteArrayRaw(""));
        assertArrayEquals(new byte[0], Conversion.hexToByteArrayRaw(".-- .\t\t __  \t-._"));
    }

    /**
     * Tests {@link Conversion#hexToByteArrayRaw(String)}.
     */
    @Test(expected=IllegalArgumentException.class)
    public void testHexToByteArrayRaw_OddLengthException() {
        Conversion.hexToByteArrayRaw("FA219");
    }

    /**
     * Tests {@link Conversion#hexToByteArrayRaw(String)}.
     */
    @Test(expected=IllegalArgumentException.class)
    public void testHexToByteArrayRaw_OddLengthException_WithEvenTotal() {
        Conversion.hexToByteArrayRaw("FA.21-_9");
    }



    /* ************************************** *
     * ************************************** *
     * ***         ************************** *
     * ***  Short  ************************** *
     * ***         ************************** *
     * ************************************** *
     * ************************************** */

    /**
     * Tests {@link Conversion#toByteArray(short)}.
     */
    @Test
    public void testToByteArray_short() {
        int SIZE = TestBinaryByteBlocks.SHORT_BYTES;
        byte[] BASE_CODE_ANSWER = new byte[SIZE];

        //Compare against existing implementation
        assertArrayEquals(Conversion.toByteArray(Short.MAX_VALUE),
                Conversion.shortToByteArray(Short.MAX_VALUE, 0, BASE_CODE_ANSWER, 0, SIZE));

        assertArrayEquals(Conversion.toByteArray(Short.MIN_VALUE),
                Conversion.shortToByteArray(Short.MIN_VALUE, 0, BASE_CODE_ANSWER, 0, SIZE));

        assertArrayEquals(Conversion.toByteArray((short) -1),
                Conversion.shortToByteArray((short) -1, 0, BASE_CODE_ANSWER, 0, SIZE));

        assertArrayEquals(Conversion.toByteArray((short) 0),
                Conversion.shortToByteArray((short) 0, 0, BASE_CODE_ANSWER, 0, SIZE));

        assertArrayEquals(Conversion.toByteArray((short) 1),
                Conversion.shortToByteArray((short) 1, 0, BASE_CODE_ANSWER, 0, SIZE));

        //Check raw bytes
        assertArrayEquals(TestBinaryByteBlocks.SHORT_MAX_VAL_BYTES, Conversion.toByteArray(Short.MAX_VALUE));
        assertArrayEquals(TestBinaryByteBlocks.SHORT_MIN_VAL_BYTES, Conversion.toByteArray(Short.MIN_VALUE));
        assertArrayEquals(TestBinaryByteBlocks.SHORT_NEG_1_BYTES,   Conversion.toByteArray((short) -1));
        assertArrayEquals(TestBinaryByteBlocks.SHORT_0_BYTES,       Conversion.toByteArray((short) 0));
        assertArrayEquals(TestBinaryByteBlocks.SHORT_1_BYTES,       Conversion.toByteArray((short) 1));

        //Check full conversion cycle
        assertEquals(Short.MAX_VALUE, Conversion.toShort(Conversion.toByteArray(Short.MAX_VALUE)));
        assertEquals(Short.MIN_VALUE, Conversion.toShort(Conversion.toByteArray(Short.MIN_VALUE)));
        assertEquals((short) -1,      Conversion.toShort(Conversion.toByteArray((short) -1)));
        assertEquals((short) 0,       Conversion.toShort(Conversion.toByteArray((short) 0)));
        assertEquals((short) 1,       Conversion.toShort(Conversion.toByteArray((short) 1)));
    }

    /**
     * Tests {@link Conversion#toByteArrayBe(short)}.
     */
    @Test
    public void testToByteArrayBe_short() {
        //Check raw bytes
        assertArrayEquals(TestBinaryByteBlocks.Be_SHORT_MAX_VAL_BYTES, Conversion.toByteArrayBe(Short.MAX_VALUE));
        assertArrayEquals(TestBinaryByteBlocks.Be_SHORT_MIN_VAL_BYTES, Conversion.toByteArrayBe(Short.MIN_VALUE));
        assertArrayEquals(TestBinaryByteBlocks.Be_SHORT_NEG_1_BYTES,   Conversion.toByteArrayBe((short) -1));
        assertArrayEquals(TestBinaryByteBlocks.Be_SHORT_0_BYTES,       Conversion.toByteArrayBe((short) 0));
        assertArrayEquals(TestBinaryByteBlocks.Be_SHORT_1_BYTES,       Conversion.toByteArrayBe((short) 1));

        //Check full conversion cycle
        assertEquals(Short.MAX_VALUE, Conversion.toShortBe(Conversion.toByteArrayBe(Short.MAX_VALUE)));
        assertEquals(Short.MIN_VALUE, Conversion.toShortBe(Conversion.toByteArrayBe(Short.MIN_VALUE)));
        assertEquals((short) -1,      Conversion.toShortBe(Conversion.toByteArrayBe((short) -1)));
        assertEquals((short) 0,       Conversion.toShortBe(Conversion.toByteArrayBe((short) 0)));
        assertEquals((short) 1,       Conversion.toShortBe(Conversion.toByteArrayBe((short) 1)));
    }

    /**
     * Tests {@link Conversion#toShort(byte[])}.
     */
    @Test
    public void testToShort() {
        int SIZE = TestBinaryByteBlocks.SHORT_BYTES;
        short BASE_CODE_ANSWER = 0;

        //Compare against existing implementation
        assertEquals(Conversion.byteArrayToShort(TestBinaryByteBlocks.SHORT_MAX_VAL_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
                Conversion.toShort(TestBinaryByteBlocks.SHORT_MAX_VAL_BYTES));

        assertEquals(Conversion.byteArrayToShort(TestBinaryByteBlocks.SHORT_MIN_VAL_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
                Conversion.toShort(TestBinaryByteBlocks.SHORT_MIN_VAL_BYTES));

        assertEquals(Conversion.byteArrayToShort(TestBinaryByteBlocks.SHORT_NEG_1_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
                Conversion.toShort(TestBinaryByteBlocks.SHORT_NEG_1_BYTES));

        assertEquals(Conversion.byteArrayToShort(TestBinaryByteBlocks.SHORT_0_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
                Conversion.toShort(TestBinaryByteBlocks.SHORT_0_BYTES));

        assertEquals(Conversion.byteArrayToShort(TestBinaryByteBlocks.SHORT_1_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
                Conversion.toShort(TestBinaryByteBlocks.SHORT_1_BYTES));

        //Check values
        assertEquals(Short.MAX_VALUE, Conversion.toShort(TestBinaryByteBlocks.SHORT_MAX_VAL_BYTES));
        assertEquals(Short.MIN_VALUE, Conversion.toShort(TestBinaryByteBlocks.SHORT_MIN_VAL_BYTES));
        assertEquals((short) -1,      Conversion.toShort(TestBinaryByteBlocks.SHORT_NEG_1_BYTES));
        assertEquals((short) 0,       Conversion.toShort(TestBinaryByteBlocks.SHORT_0_BYTES));
        assertEquals((short) 1,       Conversion.toShort(TestBinaryByteBlocks.SHORT_1_BYTES));

        //Check Null & Empty
        assertEquals((short) 0, Conversion.toShort((byte[]) null));
        assertEquals((short) 0, Conversion.toShort(new byte[0]));

        //Check full conversion cycle
        assertEquals(Short.MAX_VALUE, Conversion.toShort(Conversion.toByteArray(Short.MAX_VALUE)));
        assertEquals(Short.MIN_VALUE, Conversion.toShort(Conversion.toByteArray(Short.MIN_VALUE)));
        assertEquals((short) -1,      Conversion.toShort(Conversion.toByteArray((short) -1)));
        assertEquals((short) 0,       Conversion.toShort(Conversion.toByteArray((short) 0)));
        assertEquals((short) 1,       Conversion.toShort(Conversion.toByteArray((short) 1)));
    }

    /**
     * Tests {@link Conversion#toShort(byte[])}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testToShort_tooLong() {
        Conversion.toShort(new byte[TestBinaryByteBlocks.SHORT_BYTES+1]);
    }

    /**
     * Tests {@link Conversion#toShort(byte[])}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testToShort_tooSmall() {
        Conversion.toShort(new byte[TestBinaryByteBlocks.SHORT_BYTES-1]);
    }

    /**
     * Tests {@link Conversion#toShortBe(byte[])}.
     */
    @Test
    public void testToShortBe() {
        //Check values
        assertEquals(Short.MAX_VALUE, Conversion.toShortBe(TestBinaryByteBlocks.Be_SHORT_MAX_VAL_BYTES));
        assertEquals(Short.MIN_VALUE, Conversion.toShortBe(TestBinaryByteBlocks.Be_SHORT_MIN_VAL_BYTES));
        assertEquals((short) -1,      Conversion.toShortBe(TestBinaryByteBlocks.Be_SHORT_NEG_1_BYTES));
        assertEquals((short) 0,       Conversion.toShortBe(TestBinaryByteBlocks.Be_SHORT_0_BYTES));
        assertEquals((short) 1,       Conversion.toShortBe(TestBinaryByteBlocks.Be_SHORT_1_BYTES));

        //Check Null & Empty
        assertEquals((short) 0, Conversion.toShortBe((byte[]) null));
        assertEquals((short) 0, Conversion.toShortBe(new byte[0]));

        //Check full conversion cycle
        assertEquals(Short.MAX_VALUE, Conversion.toShortBe(Conversion.toByteArrayBe(Short.MAX_VALUE)));
        assertEquals(Short.MIN_VALUE, Conversion.toShortBe(Conversion.toByteArrayBe(Short.MIN_VALUE)));
        assertEquals((short) -1,      Conversion.toShortBe(Conversion.toByteArrayBe((short) -1)));
        assertEquals((short) 0,       Conversion.toShortBe(Conversion.toByteArrayBe((short) 0)));
        assertEquals((short) 1,       Conversion.toShortBe(Conversion.toByteArrayBe((short) 1)));
    }

    /**
     * Tests {@link Conversion#toShortBe(byte[])}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testToShortBe_tooLong() {
        Conversion.toShortBe(new byte[TestBinaryByteBlocks.SHORT_BYTES+1]);
    }

    /**
     * Tests {@link Conversion#toShortBe(byte[])}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testToShortBe_tooSmall() {
        Conversion.toShortBe(new byte[TestBinaryByteBlocks.SHORT_BYTES-1]);
    }

    /* ************************************** *
     * ***   Hex   ************************** *
     * ************************************** */

    /**
     * Tests {@link Conversion#hexToShort(String)}.
     */
    @Test
    public void testHexToShort_ConvenienceMethod() {
//        int SIZE = TestBinaryByteBlocks.SHORT_BYTES*2; //2 hex digits per byte
//        short BASE_CODE_ANSWER = 0;
//
//        //Compare against existing implementation
//        assertEquals(Conversion.hexToShort(TestBinaryByteBlocks.HEX_SHORT_MAX_VAL_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
//                Conversion.hexToShort(TestBinaryByteBlocks.HEX_SHORT_MAX_VAL_BYTES));
//
//        assertEquals(Conversion.hexToShort(TestBinaryByteBlocks.HEX_SHORT_MIN_VAL_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
//                Conversion.hexToShort(TestBinaryByteBlocks.HEX_SHORT_MIN_VAL_BYTES));
//
//        assertEquals(Conversion.hexToShort(TestBinaryByteBlocks.HEX_SHORT_NEG_1_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
//                Conversion.hexToShort(TestBinaryByteBlocks.HEX_SHORT_NEG_1_BYTES));
//
//        assertEquals(Conversion.hexToShort(TestBinaryByteBlocks.HEX_SHORT_0_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
//                Conversion.hexToShort(TestBinaryByteBlocks.HEX_SHORT_0_BYTES));
//
//        assertEquals(Conversion.hexToShort(TestBinaryByteBlocks.HEX_SHORT_1_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
//                Conversion.hexToShort(TestBinaryByteBlocks.HEX_SHORT_1_BYTES));

        //Check values
        assertEquals(Short.MAX_VALUE, Conversion.hexToShort(TestBinaryByteBlocks.HEX_SHORT_MAX_VAL_BYTES));
        assertEquals(Short.MIN_VALUE, Conversion.hexToShort(TestBinaryByteBlocks.HEX_SHORT_MIN_VAL_BYTES));
        assertEquals((short) -1,      Conversion.hexToShort(TestBinaryByteBlocks.HEX_SHORT_NEG_1_BYTES));
        assertEquals((short) 0,       Conversion.hexToShort(TestBinaryByteBlocks.HEX_SHORT_0_BYTES));
        assertEquals((short) 1,       Conversion.hexToShort(TestBinaryByteBlocks.HEX_SHORT_1_BYTES));

        //Check Null & Empty
        assertEquals((short) 0, Conversion.hexToShort((String) null));
        assertEquals((short) 0, Conversion.hexToShort(""));
        assertEquals((short) 0, Conversion.hexToShort(".-- .\t\t __  \t-._"));

        //Check full conversion cycle
        assertEquals(Short.MAX_VALUE, Conversion.hexToShort(Conversion.toHex(Short.MAX_VALUE)));
        assertEquals(Short.MIN_VALUE, Conversion.hexToShort(Conversion.toHex(Short.MIN_VALUE)));
        assertEquals((short) -1,      Conversion.hexToShort(Conversion.toHex((short) -1)));
        assertEquals((short) 0,       Conversion.hexToShort(Conversion.toHex((short) 0)));
        assertEquals((short) 1,       Conversion.hexToShort(Conversion.toHex((short) 1)));
    }

    /**
     * Tests {@link Conversion#hexToShort(String)}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testHexToShort_tooLong() {
        Conversion.hexToShort(TestBinaryByteBlocks.getNumHexDigits(TestBinaryByteBlocks.SHORT_BYTES+1));
    }

    /**
     * Tests {@link Conversion#hexToShort(String)}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testHexToShort_tooSmall() {
        Conversion.hexToShort(TestBinaryByteBlocks.getNumHexDigits(TestBinaryByteBlocks.SHORT_BYTES-1));
    }

    /**
     * Tests {@link Conversion#hexToShortBe(String)}.
     */
    @Test
    public void testHexToShortBe_ConvenienceMethod() {
        //Check values
        assertEquals(Short.MAX_VALUE, Conversion.hexToShortBe(TestBinaryByteBlocks.Be_HEX_SHORT_MAX_VAL_BYTES));
        assertEquals(Short.MIN_VALUE, Conversion.hexToShortBe(TestBinaryByteBlocks.Be_HEX_SHORT_MIN_VAL_BYTES));
        assertEquals((short) -1,      Conversion.hexToShortBe(TestBinaryByteBlocks.Be_HEX_SHORT_NEG_1_BYTES));
        assertEquals((short) 0,       Conversion.hexToShortBe(TestBinaryByteBlocks.Be_HEX_SHORT_0_BYTES));
        assertEquals((short) 1,       Conversion.hexToShortBe(TestBinaryByteBlocks.Be_HEX_SHORT_1_BYTES));

        //Check Null & Empty
        assertEquals((short) 0, Conversion.hexToShortBe((String) null));
        assertEquals((short) 0, Conversion.hexToShortBe(""));
        assertEquals((short) 0, Conversion.hexToShortBe(".-- .\t\t __  \t-._"));

        //Check full conversion cycle
        assertEquals(Short.MAX_VALUE, Conversion.hexToShortBe(Conversion.toHexBe(Short.MAX_VALUE)));
        assertEquals(Short.MIN_VALUE, Conversion.hexToShortBe(Conversion.toHexBe(Short.MIN_VALUE)));
        assertEquals((short) -1,      Conversion.hexToShortBe(Conversion.toHexBe((short) -1)));
        assertEquals((short) 0,       Conversion.hexToShortBe(Conversion.toHexBe((short) 0)));
        assertEquals((short) 1,       Conversion.hexToShortBe(Conversion.toHexBe((short) 1)));
    }

    /**
     * Tests {@link Conversion#hexToShortBe(String)}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testHexToShortBe_tooLong() {
        Conversion.hexToShortBe(TestBinaryByteBlocks.getNumHexDigits(TestBinaryByteBlocks.SHORT_BYTES+1));
    }

    /**
     * Tests {@link Conversion#hexToShortBe(String)}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testHexToShortBe_tooSmall() {
        Conversion.hexToShortBe(TestBinaryByteBlocks.getNumHexDigits(TestBinaryByteBlocks.SHORT_BYTES-1));
    }

    /* ************************************** *
     * ************************************** *
     * ***             ********************** *
     * ***  End Short  ********************** *
     * ***             ********************** *
     * ************************************** *
     * ************************************** */

    /* ************************************** *
     * ************************************** *
     * ***       **************************** *
     * ***  Int  **************************** *
     * ***       **************************** *
     * ************************************** *
     * ************************************** */

    /**
     * Tests {@link Conversion#toByteArray(int)}.
     */
    @Test
    public void testToByteArray_int() {
        int SIZE = TestBinaryByteBlocks.INT_BYTES;
        byte[] BASE_CODE_ANSWER = new byte[SIZE];

        //Compare against existing implementation
        assertArrayEquals(Conversion.toByteArray(Integer.MAX_VALUE),
                Conversion.intToByteArray(Integer.MAX_VALUE, 0, BASE_CODE_ANSWER, 0, SIZE));

        assertArrayEquals(Conversion.toByteArray(Integer.MIN_VALUE),
                Conversion.intToByteArray(Integer.MIN_VALUE, 0, BASE_CODE_ANSWER, 0, SIZE));

        assertArrayEquals(Conversion.toByteArray((int) -1),
                Conversion.intToByteArray((int) -1, 0, BASE_CODE_ANSWER, 0, SIZE));

        assertArrayEquals(Conversion.toByteArray((int) 0),
                Conversion.intToByteArray((int) 0, 0, BASE_CODE_ANSWER, 0, SIZE));

        assertArrayEquals(Conversion.toByteArray((int) 1),
                Conversion.intToByteArray((int) 1, 0, BASE_CODE_ANSWER, 0, SIZE));

        //Check raw bytes
        assertArrayEquals(TestBinaryByteBlocks.INT_MAX_VAL_BYTES, Conversion.toByteArray(Integer.MAX_VALUE));
        assertArrayEquals(TestBinaryByteBlocks.INT_MIN_VAL_BYTES, Conversion.toByteArray(Integer.MIN_VALUE));
        assertArrayEquals(TestBinaryByteBlocks.INT_NEG_1_BYTES,   Conversion.toByteArray((int) -1));
        assertArrayEquals(TestBinaryByteBlocks.INT_0_BYTES,       Conversion.toByteArray((int) 0));
        assertArrayEquals(TestBinaryByteBlocks.INT_1_BYTES,       Conversion.toByteArray((int) 1));

        //Check full conversion cycle
        assertEquals(Integer.MAX_VALUE, Conversion.toInt(Conversion.toByteArray(Integer.MAX_VALUE)));
        assertEquals(Integer.MIN_VALUE, Conversion.toInt(Conversion.toByteArray(Integer.MIN_VALUE)));
        assertEquals((int) -1,          Conversion.toInt(Conversion.toByteArray((int) -1)));
        assertEquals((int) 0,           Conversion.toInt(Conversion.toByteArray((int) 0)));
        assertEquals((int) 1,           Conversion.toInt(Conversion.toByteArray((int) 1)));
    }

    /**
     * Tests {@link Conversion#toByteArrayBe(int)}.
     */
    @Test
    public void testToByteArrayBe_int() {
        //Check raw bytes
        assertArrayEquals(TestBinaryByteBlocks.Be_INT_MAX_VAL_BYTES, Conversion.toByteArrayBe(Integer.MAX_VALUE));
        assertArrayEquals(TestBinaryByteBlocks.Be_INT_MIN_VAL_BYTES, Conversion.toByteArrayBe(Integer.MIN_VALUE));
        assertArrayEquals(TestBinaryByteBlocks.Be_INT_NEG_1_BYTES,   Conversion.toByteArrayBe((int) -1));
        assertArrayEquals(TestBinaryByteBlocks.Be_INT_0_BYTES,       Conversion.toByteArrayBe((int) 0));
        assertArrayEquals(TestBinaryByteBlocks.Be_INT_1_BYTES,       Conversion.toByteArrayBe((int) 1));

        //Check full conversion cycle
        assertEquals(Integer.MAX_VALUE, Conversion.toIntBe(Conversion.toByteArrayBe(Integer.MAX_VALUE)));
        assertEquals(Integer.MIN_VALUE, Conversion.toIntBe(Conversion.toByteArrayBe(Integer.MIN_VALUE)));
        assertEquals((int) -1,          Conversion.toIntBe(Conversion.toByteArrayBe((int) -1)));
        assertEquals((int) 0,           Conversion.toIntBe(Conversion.toByteArrayBe((int) 0)));
        assertEquals((int) 1,           Conversion.toIntBe(Conversion.toByteArrayBe((int) 1)));
    }

    /**
     * Tests {@link Conversion#toInt(byte[])}.
     */
    @Test
    public void testToInt() {
        int SIZE = TestBinaryByteBlocks.INT_BYTES;
        int BASE_CODE_ANSWER = 0;

        //Compare against existing implementation
        assertEquals(Conversion.byteArrayToInt(TestBinaryByteBlocks.INT_MAX_VAL_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
                Conversion.toInt(TestBinaryByteBlocks.INT_MAX_VAL_BYTES));

        assertEquals(Conversion.byteArrayToInt(TestBinaryByteBlocks.INT_MIN_VAL_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
                Conversion.toInt(TestBinaryByteBlocks.INT_MIN_VAL_BYTES));

        assertEquals(Conversion.byteArrayToInt(TestBinaryByteBlocks.INT_NEG_1_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
                Conversion.toInt(TestBinaryByteBlocks.INT_NEG_1_BYTES));

        assertEquals(Conversion.byteArrayToInt(TestBinaryByteBlocks.INT_0_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
                Conversion.toInt(TestBinaryByteBlocks.INT_0_BYTES));

        assertEquals(Conversion.byteArrayToInt(TestBinaryByteBlocks.INT_1_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
                Conversion.toInt(TestBinaryByteBlocks.INT_1_BYTES));

        //Check values
        assertEquals(Integer.MAX_VALUE, Conversion.toInt(TestBinaryByteBlocks.INT_MAX_VAL_BYTES));
        assertEquals(Integer.MIN_VALUE, Conversion.toInt(TestBinaryByteBlocks.INT_MIN_VAL_BYTES));
        assertEquals((int) -1,          Conversion.toInt(TestBinaryByteBlocks.INT_NEG_1_BYTES));
        assertEquals((int) 0,           Conversion.toInt(TestBinaryByteBlocks.INT_0_BYTES));
        assertEquals((int) 1,           Conversion.toInt(TestBinaryByteBlocks.INT_1_BYTES));

        //Check Null & Empty
        assertEquals((int) 0, Conversion.toInt((byte[]) null));
        assertEquals((int) 0, Conversion.toInt(new byte[0]));

        //Check full conversion cycle
        assertEquals(Integer.MAX_VALUE, Conversion.toInt(Conversion.toByteArray(Integer.MAX_VALUE)));
        assertEquals(Integer.MIN_VALUE, Conversion.toInt(Conversion.toByteArray(Integer.MIN_VALUE)));
        assertEquals((int) -1,          Conversion.toInt(Conversion.toByteArray((int) -1)));
        assertEquals((int) 0,           Conversion.toInt(Conversion.toByteArray((int) 0)));
        assertEquals((int) 1,           Conversion.toInt(Conversion.toByteArray((int) 1)));
    }

    /**
     * Tests {@link Conversion#toInt(byte[])}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testToInt_tooLong() {
        Conversion.toInt(new byte[TestBinaryByteBlocks.INT_BYTES+1]);
    }

    /**
     * Tests {@link Conversion#toInt(byte[])}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testToInt_tooSmall() {
        Conversion.toInt(new byte[TestBinaryByteBlocks.INT_BYTES-1]);
    }

    /**
     * Tests {@link Conversion#toIntBe(byte[])}.
     */
    @Test
    public void testToIntBe() {
        //Check values
        assertEquals(Integer.MAX_VALUE, Conversion.toIntBe(TestBinaryByteBlocks.Be_INT_MAX_VAL_BYTES));
        assertEquals(Integer.MIN_VALUE, Conversion.toIntBe(TestBinaryByteBlocks.Be_INT_MIN_VAL_BYTES));
        assertEquals((int) -1,          Conversion.toIntBe(TestBinaryByteBlocks.Be_INT_NEG_1_BYTES));
        assertEquals((int) 0,           Conversion.toIntBe(TestBinaryByteBlocks.Be_INT_0_BYTES));
        assertEquals((int) 1,           Conversion.toIntBe(TestBinaryByteBlocks.Be_INT_1_BYTES));

        //Check Null & Empty
        assertEquals((int) 0, Conversion.toIntBe((byte[]) null));
        assertEquals((int) 0, Conversion.toIntBe(new byte[0]));

        //Check full conversion cycle
        assertEquals(Integer.MAX_VALUE, Conversion.toIntBe(Conversion.toByteArrayBe(Integer.MAX_VALUE)));
        assertEquals(Integer.MIN_VALUE, Conversion.toIntBe(Conversion.toByteArrayBe(Integer.MIN_VALUE)));
        assertEquals((int) -1,          Conversion.toIntBe(Conversion.toByteArrayBe((int) -1)));
        assertEquals((int) 0,           Conversion.toIntBe(Conversion.toByteArrayBe((int) 0)));
        assertEquals((int) 1,           Conversion.toIntBe(Conversion.toByteArrayBe((int) 1)));
    }

    /**
     * Tests {@link Conversion#toIntBe(byte[])}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testToIntBe_tooLong() {
        Conversion.toIntBe(new byte[TestBinaryByteBlocks.INT_BYTES+1]);
    }

    /**
     * Tests {@link Conversion#toIntBe(byte[])}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testToIntBe_tooSmall() {
        Conversion.toIntBe(new byte[TestBinaryByteBlocks.INT_BYTES-1]);
    }

    /* ************************************** *
     * ***   Hex   ************************** *
     * ************************************** */

    /**
     * Tests {@link Conversion#hexToInt(String)}.
     */
    @Test
    public void testHexToInt_ConvenienceMethod() {
//        int SIZE = TestBinaryByteBlocks.INT_BYTES*2; //2 hex digits per byte
//        int BASE_CODE_ANSWER = 0;
//
//        //Compare against existing implementation
//        assertEquals(Conversion.hexToInt(TestBinaryByteBlocks.HEX_INT_MAX_VAL_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
//                Conversion.hexToInt(TestBinaryByteBlocks.HEX_INT_MAX_VAL_BYTES));
//
//        assertEquals(Conversion.hexToInt(TestBinaryByteBlocks.HEX_INT_MIN_VAL_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
//                Conversion.hexToInt(TestBinaryByteBlocks.HEX_INT_MIN_VAL_BYTES));
//
//        assertEquals(Conversion.hexToInt(TestBinaryByteBlocks.HEX_INT_NEG_1_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
//                Conversion.hexToInt(TestBinaryByteBlocks.HEX_INT_NEG_1_BYTES));
//
//        assertEquals(Conversion.hexToInt(TestBinaryByteBlocks.HEX_INT_0_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
//                Conversion.hexToInt(TestBinaryByteBlocks.HEX_INT_0_BYTES));
//
//        assertEquals(Conversion.hexToInt(TestBinaryByteBlocks.HEX_INT_1_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
//                Conversion.hexToInt(TestBinaryByteBlocks.HEX_INT_1_BYTES));

        //Check values
        assertEquals(Integer.MAX_VALUE, Conversion.hexToInt(TestBinaryByteBlocks.HEX_INT_MAX_VAL_BYTES));
        assertEquals(Integer.MIN_VALUE, Conversion.hexToInt(TestBinaryByteBlocks.HEX_INT_MIN_VAL_BYTES));
        assertEquals((int) -1,      Conversion.hexToInt(TestBinaryByteBlocks.HEX_INT_NEG_1_BYTES));
        assertEquals((int) 0,       Conversion.hexToInt(TestBinaryByteBlocks.HEX_INT_0_BYTES));
        assertEquals((int) 1,       Conversion.hexToInt(TestBinaryByteBlocks.HEX_INT_1_BYTES));

        //Check Null & Empty
        assertEquals((int) 0, Conversion.hexToInt((String) null));
        assertEquals((int) 0, Conversion.hexToInt(""));
        assertEquals((int) 0, Conversion.hexToInt(".-- .\t\t __  \t-._"));

        //Check full conversion cycle
        assertEquals(Integer.MAX_VALUE, Conversion.hexToInt(Conversion.toHex(Integer.MAX_VALUE)));
        assertEquals(Integer.MIN_VALUE, Conversion.hexToInt(Conversion.toHex(Integer.MIN_VALUE)));
        assertEquals((int) -1,      Conversion.hexToInt(Conversion.toHex((int) -1)));
        assertEquals((int) 0,       Conversion.hexToInt(Conversion.toHex((int) 0)));
        assertEquals((int) 1,       Conversion.hexToInt(Conversion.toHex((int) 1)));
    }

    /**
     * Tests {@link Conversion#hexToInt(String)}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testHexToInt_tooLong() {
        Conversion.hexToInt(TestBinaryByteBlocks.getNumHexDigits(TestBinaryByteBlocks.INT_BYTES+1));
    }

    /**
     * Tests {@link Conversion#hexToInt(String)}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testHexToInt_tooSmall() {
        Conversion.hexToInt(TestBinaryByteBlocks.getNumHexDigits(TestBinaryByteBlocks.INT_BYTES-1));
    }

    /**
     * Tests {@link Conversion#hexToIntBe(String)}.
     */
    @Test
    public void testHexToIntBe_ConvenienceMethod() {
        //Check values
        assertEquals(Integer.MAX_VALUE, Conversion.hexToIntBe(TestBinaryByteBlocks.Be_HEX_INT_MAX_VAL_BYTES));
        assertEquals(Integer.MIN_VALUE, Conversion.hexToIntBe(TestBinaryByteBlocks.Be_HEX_INT_MIN_VAL_BYTES));
        assertEquals((int) -1,      Conversion.hexToIntBe(TestBinaryByteBlocks.Be_HEX_INT_NEG_1_BYTES));
        assertEquals((int) 0,       Conversion.hexToIntBe(TestBinaryByteBlocks.Be_HEX_INT_0_BYTES));
        assertEquals((int) 1,       Conversion.hexToIntBe(TestBinaryByteBlocks.Be_HEX_INT_1_BYTES));

        //Check Null & Empty
        assertEquals((int) 0, Conversion.hexToIntBe((String) null));
        assertEquals((int) 0, Conversion.hexToIntBe(""));
        assertEquals((int) 0, Conversion.hexToIntBe(".-- .\t\t __  \t-._"));

        //Check full conversion cycle
        assertEquals(Integer.MAX_VALUE, Conversion.hexToIntBe(Conversion.toHexBe(Integer.MAX_VALUE)));
        assertEquals(Integer.MIN_VALUE, Conversion.hexToIntBe(Conversion.toHexBe(Integer.MIN_VALUE)));
        assertEquals((int) -1,      Conversion.hexToIntBe(Conversion.toHexBe((int) -1)));
        assertEquals((int) 0,       Conversion.hexToIntBe(Conversion.toHexBe((int) 0)));
        assertEquals((int) 1,       Conversion.hexToIntBe(Conversion.toHexBe((int) 1)));
    }

    /**
     * Tests {@link Conversion#hexToIntBe(String)}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testHexToIntBe_tooLong() {
        Conversion.hexToIntBe(TestBinaryByteBlocks.getNumHexDigits(TestBinaryByteBlocks.INT_BYTES+1));
    }

    /**
     * Tests {@link Conversion#hexToIntBe(String)}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testHexToIntBe_tooSmall() {
        Conversion.hexToIntBe(TestBinaryByteBlocks.getNumHexDigits(TestBinaryByteBlocks.INT_BYTES-1));
    }

    /* ************************************** *
     * ************************************** *
     * ***           ************************ *
     * ***  End Int  ************************ *
     * ***           ************************ *
     * ************************************** *
     * ************************************** */

    /* ************************************** *
     * ************************************** *
     * ***        *************************** *
     * ***  Long  *************************** *
     * ***        *************************** *
     * ************************************** *
     * ************************************** */

    /**
     * Tests {@link Conversion#toByteArray(long)}.
     */
    @Test
    public void testToByteArray_long() {
        int SIZE = TestBinaryByteBlocks.LONG_BYTES;
        byte[] BASE_CODE_ANSWER = new byte[SIZE];

        //Compare against existing implementation
        assertArrayEquals(Conversion.toByteArray(Long.MAX_VALUE),
                Conversion.longToByteArray(Long.MAX_VALUE, 0, BASE_CODE_ANSWER, 0, SIZE));

        assertArrayEquals(Conversion.toByteArray(Long.MIN_VALUE),
                Conversion.longToByteArray(Long.MIN_VALUE, 0, BASE_CODE_ANSWER, 0, SIZE));

        assertArrayEquals(Conversion.toByteArray((long) -1),
                Conversion.longToByteArray((long) -1, 0, BASE_CODE_ANSWER, 0, SIZE));

        assertArrayEquals(Conversion.toByteArray((long) 0),
                Conversion.longToByteArray((long) 0, 0, BASE_CODE_ANSWER, 0, SIZE));

        assertArrayEquals(Conversion.toByteArray((long) 1),
                Conversion.longToByteArray((long) 1, 0, BASE_CODE_ANSWER, 0, SIZE));

        //Check raw bytes
        assertArrayEquals(TestBinaryByteBlocks.LONG_MAX_VAL_BYTES, Conversion.toByteArray(Long.MAX_VALUE));
        assertArrayEquals(TestBinaryByteBlocks.LONG_MIN_VAL_BYTES, Conversion.toByteArray(Long.MIN_VALUE));
        assertArrayEquals(TestBinaryByteBlocks.LONG_NEG_1_BYTES,   Conversion.toByteArray((long) -1));
        assertArrayEquals(TestBinaryByteBlocks.LONG_0_BYTES,       Conversion.toByteArray((long) 0));
        assertArrayEquals(TestBinaryByteBlocks.LONG_1_BYTES,       Conversion.toByteArray((long) 1));

        //Check full conversion cycle
        assertEquals(Long.MAX_VALUE, Conversion.toLong(Conversion.toByteArray(Long.MAX_VALUE)));
        assertEquals(Long.MIN_VALUE, Conversion.toLong(Conversion.toByteArray(Long.MIN_VALUE)));
        assertEquals((long) -1,      Conversion.toLong(Conversion.toByteArray((long) -1)));
        assertEquals((long) 0,       Conversion.toLong(Conversion.toByteArray((long) 0)));
        assertEquals((long) 1,       Conversion.toLong(Conversion.toByteArray((long) 1)));
    }

    /**
     * Tests {@link Conversion#toByteArrayBe(long)}.
     */
    @Test
    public void testToByteArrayBe_long() {
        //Check raw bytes
        assertArrayEquals(TestBinaryByteBlocks.Be_LONG_MAX_VAL_BYTES, Conversion.toByteArrayBe(Long.MAX_VALUE));
        assertArrayEquals(TestBinaryByteBlocks.Be_LONG_MIN_VAL_BYTES, Conversion.toByteArrayBe(Long.MIN_VALUE));
        assertArrayEquals(TestBinaryByteBlocks.Be_LONG_NEG_1_BYTES,   Conversion.toByteArrayBe((long) -1));
        assertArrayEquals(TestBinaryByteBlocks.Be_LONG_0_BYTES,       Conversion.toByteArrayBe((long) 0));
        assertArrayEquals(TestBinaryByteBlocks.Be_LONG_1_BYTES,       Conversion.toByteArrayBe((long) 1));

        //Check full conversion cycle
        assertEquals(Long.MAX_VALUE, Conversion.toLongBe(Conversion.toByteArrayBe(Long.MAX_VALUE)));
        assertEquals(Long.MIN_VALUE, Conversion.toLongBe(Conversion.toByteArrayBe(Long.MIN_VALUE)));
        assertEquals((long) -1,      Conversion.toLongBe(Conversion.toByteArrayBe((long) -1)));
        assertEquals((long) 0,       Conversion.toLongBe(Conversion.toByteArrayBe((long) 0)));
        assertEquals((long) 1,       Conversion.toLongBe(Conversion.toByteArrayBe((long) 1)));
    }

    /**
     * Tests {@link Conversion#toLong(byte[])}.
     */
    @Test
    public void testToLong() {
        int SIZE = TestBinaryByteBlocks.LONG_BYTES;
        long BASE_CODE_ANSWER = 0;

        //Compare against existing implementation
        assertEquals(Conversion.byteArrayToLong(TestBinaryByteBlocks.LONG_MAX_VAL_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
                Conversion.toLong(TestBinaryByteBlocks.LONG_MAX_VAL_BYTES));

        assertEquals(Conversion.byteArrayToLong(TestBinaryByteBlocks.LONG_MIN_VAL_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
                Conversion.toLong(TestBinaryByteBlocks.LONG_MIN_VAL_BYTES));

        assertEquals(Conversion.byteArrayToLong(TestBinaryByteBlocks.LONG_NEG_1_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
                Conversion.toLong(TestBinaryByteBlocks.LONG_NEG_1_BYTES));

        assertEquals(Conversion.byteArrayToLong(TestBinaryByteBlocks.LONG_0_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
                Conversion.toLong(TestBinaryByteBlocks.LONG_0_BYTES));

        assertEquals(Conversion.byteArrayToLong(TestBinaryByteBlocks.LONG_1_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
                Conversion.toLong(TestBinaryByteBlocks.LONG_1_BYTES));

        //Check values
        assertEquals(Long.MAX_VALUE, Conversion.toLong(TestBinaryByteBlocks.LONG_MAX_VAL_BYTES));
        assertEquals(Long.MIN_VALUE, Conversion.toLong(TestBinaryByteBlocks.LONG_MIN_VAL_BYTES));
        assertEquals((long) -1,      Conversion.toLong(TestBinaryByteBlocks.LONG_NEG_1_BYTES));
        assertEquals((long) 0,       Conversion.toLong(TestBinaryByteBlocks.LONG_0_BYTES));
        assertEquals((long) 1,       Conversion.toLong(TestBinaryByteBlocks.LONG_1_BYTES));

        //Check Null & Empty
        assertEquals((long) 0, Conversion.toLong((byte[]) null));
        assertEquals((long) 0, Conversion.toLong(new byte[0]));

        //Check full conversion cycle
        assertEquals(Long.MAX_VALUE, Conversion.toLong(Conversion.toByteArray(Long.MAX_VALUE)));
        assertEquals(Long.MIN_VALUE, Conversion.toLong(Conversion.toByteArray(Long.MIN_VALUE)));
        assertEquals((long) -1,      Conversion.toLong(Conversion.toByteArray((long) -1)));
        assertEquals((long) 0,       Conversion.toLong(Conversion.toByteArray((long) 0)));
        assertEquals((long) 1,       Conversion.toLong(Conversion.toByteArray((long) 1)));
    }

    /**
     * Tests {@link Conversion#toLong(byte[])}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testToLong_tooLong() {
        Conversion.toLong(new byte[TestBinaryByteBlocks.LONG_BYTES+1]);
    }

    /**
     * Tests {@link Conversion#toLong(byte[])}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testToLong_tooSmall() {
        Conversion.toLong(new byte[TestBinaryByteBlocks.LONG_BYTES-1]);
    }

    /**
     * Tests {@link Conversion#toLongBe(byte[])}.
     */
    @Test
    public void testToLongBe() {
        //Check values
        assertEquals(Long.MAX_VALUE, Conversion.toLongBe(TestBinaryByteBlocks.Be_LONG_MAX_VAL_BYTES));
        assertEquals(Long.MIN_VALUE, Conversion.toLongBe(TestBinaryByteBlocks.Be_LONG_MIN_VAL_BYTES));
        assertEquals((long) -1,      Conversion.toLongBe(TestBinaryByteBlocks.Be_LONG_NEG_1_BYTES));
        assertEquals((long) 0,       Conversion.toLongBe(TestBinaryByteBlocks.Be_LONG_0_BYTES));
        assertEquals((long) 1,       Conversion.toLongBe(TestBinaryByteBlocks.Be_LONG_1_BYTES));

        //Check Null & Empty
        assertEquals((long) 0, Conversion.toLongBe((byte[]) null));
        assertEquals((long) 0, Conversion.toLongBe(new byte[0]));

        //Check full conversion cycle
        assertEquals(Long.MAX_VALUE, Conversion.toLongBe(Conversion.toByteArrayBe(Long.MAX_VALUE)));
        assertEquals(Long.MIN_VALUE, Conversion.toLongBe(Conversion.toByteArrayBe(Long.MIN_VALUE)));
        assertEquals((long) -1,      Conversion.toLongBe(Conversion.toByteArrayBe((long) -1)));
        assertEquals((long) 0,       Conversion.toLongBe(Conversion.toByteArrayBe((long) 0)));
        assertEquals((long) 1,       Conversion.toLongBe(Conversion.toByteArrayBe((long) 1)));
    }

    /**
     * Tests {@link Conversion#toLongBe(byte[])}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testToLongBe_tooLong() {
        Conversion.toLongBe(new byte[TestBinaryByteBlocks.LONG_BYTES+1]);
    }

    /**
     * Tests {@link Conversion#toLongBe(byte[])}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testToLongBe_tooSmall() {
        Conversion.toLongBe(new byte[TestBinaryByteBlocks.LONG_BYTES-1]);
    }

    /* ************************************** *
     * ***   Hex   ************************** *
     * ************************************** */

    /**
     * Tests {@link Conversion#hexToLong(String)}.
     */
    @Test
    public void testHexToLong_ConvenienceMethod() {
//        int SIZE = TestBinaryByteBlocks.LONG_BYTES*2; //2 hex digits per byte
//        long BASE_CODE_ANSWER = 0;
//
//        //Compare against existing implementation
//        assertEquals(Conversion.hexToLong(TestBinaryByteBlocks.HEX_LONG_MAX_VAL_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
//                Conversion.hexToLong(TestBinaryByteBlocks.HEX_LONG_MAX_VAL_BYTES));
//
//        assertEquals(Conversion.hexToLong(TestBinaryByteBlocks.HEX_LONG_MIN_VAL_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
//                Conversion.hexToLong(TestBinaryByteBlocks.HEX_LONG_MIN_VAL_BYTES));
//
//        assertEquals(Conversion.hexToLong(TestBinaryByteBlocks.HEX_LONG_NEG_1_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
//                Conversion.hexToLong(TestBinaryByteBlocks.HEX_LONG_NEG_1_BYTES));
//
//        assertEquals(Conversion.hexToLong(TestBinaryByteBlocks.HEX_LONG_0_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
//                Conversion.hexToLong(TestBinaryByteBlocks.HEX_LONG_0_BYTES));
//
//        assertEquals(Conversion.hexToLong(TestBinaryByteBlocks.HEX_LONG_1_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
//                Conversion.hexToLong(TestBinaryByteBlocks.HEX_LONG_1_BYTES));

        //Check values
        assertEquals(Long.MAX_VALUE, Conversion.hexToLong(TestBinaryByteBlocks.HEX_LONG_MAX_VAL_BYTES));
        assertEquals(Long.MIN_VALUE, Conversion.hexToLong(TestBinaryByteBlocks.HEX_LONG_MIN_VAL_BYTES));
        assertEquals((long) -1,      Conversion.hexToLong(TestBinaryByteBlocks.HEX_LONG_NEG_1_BYTES));
        assertEquals((long) 0,       Conversion.hexToLong(TestBinaryByteBlocks.HEX_LONG_0_BYTES));
        assertEquals((long) 1,       Conversion.hexToLong(TestBinaryByteBlocks.HEX_LONG_1_BYTES));

        //Check Null & Empty
        assertEquals((long) 0, Conversion.hexToLong((String) null));
        assertEquals((long) 0, Conversion.hexToLong(""));
        assertEquals((long) 0, Conversion.hexToLong(".-- .\t\t __  \t-._"));

        //Check full conversion cycle
        assertEquals(Long.MAX_VALUE, Conversion.hexToLong(Conversion.toHex(Long.MAX_VALUE)));
        assertEquals(Long.MIN_VALUE, Conversion.hexToLong(Conversion.toHex(Long.MIN_VALUE)));
        assertEquals((long) -1,      Conversion.hexToLong(Conversion.toHex((long) -1)));
        assertEquals((long) 0,       Conversion.hexToLong(Conversion.toHex((long) 0)));
        assertEquals((long) 1,       Conversion.hexToLong(Conversion.toHex((long) 1)));
    }

    /**
     * Tests {@link Conversion#hexToLong(String)}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testHexToLong_tooLong() {
        Conversion.hexToLong(TestBinaryByteBlocks.getNumHexDigits(TestBinaryByteBlocks.LONG_BYTES+1));
    }

    /**
     * Tests {@link Conversion#hexToLong(String)}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testHexToLong_tooSmall() {
        Conversion.hexToLong(TestBinaryByteBlocks.getNumHexDigits(TestBinaryByteBlocks.LONG_BYTES-1));
    }

    /**
     * Tests {@link Conversion#hexToLongBe(String)}.
     */
    @Test
    public void testHexToLongBe_ConvenienceMethod() {
        //Check values
        assertEquals(Long.MAX_VALUE, Conversion.hexToLongBe(TestBinaryByteBlocks.Be_HEX_LONG_MAX_VAL_BYTES));
        assertEquals(Long.MIN_VALUE, Conversion.hexToLongBe(TestBinaryByteBlocks.Be_HEX_LONG_MIN_VAL_BYTES));
        assertEquals((long) -1,      Conversion.hexToLongBe(TestBinaryByteBlocks.Be_HEX_LONG_NEG_1_BYTES));
        assertEquals((long) 0,       Conversion.hexToLongBe(TestBinaryByteBlocks.Be_HEX_LONG_0_BYTES));
        assertEquals((long) 1,       Conversion.hexToLongBe(TestBinaryByteBlocks.Be_HEX_LONG_1_BYTES));

        //Check Null & Empty
        assertEquals((long) 0, Conversion.hexToLongBe((String) null));
        assertEquals((long) 0, Conversion.hexToLongBe(""));
        assertEquals((long) 0, Conversion.hexToLongBe(".-- .\t\t __  \t-._"));

        //Check full conversion cycle
        assertEquals(Long.MAX_VALUE, Conversion.hexToLongBe(Conversion.toHexBe(Long.MAX_VALUE)));
        assertEquals(Long.MIN_VALUE, Conversion.hexToLongBe(Conversion.toHexBe(Long.MIN_VALUE)));
        assertEquals((long) -1,      Conversion.hexToLongBe(Conversion.toHexBe((long) -1)));
        assertEquals((long) 0,       Conversion.hexToLongBe(Conversion.toHexBe((long) 0)));
        assertEquals((long) 1,       Conversion.hexToLongBe(Conversion.toHexBe((long) 1)));
    }

    /**
     * Tests {@link Conversion#hexToLongBe(String)}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testHexToLongBe_tooLong() {
        Conversion.hexToLongBe(TestBinaryByteBlocks.getNumHexDigits(TestBinaryByteBlocks.LONG_BYTES+1));
    }

    /**
     * Tests {@link Conversion#hexToLongBe(String)}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testHexToLongBe_tooSmall() {
        Conversion.hexToLongBe(TestBinaryByteBlocks.getNumHexDigits(TestBinaryByteBlocks.LONG_BYTES-1));
    }

    /* ************************************** *
     * ************************************** *
     * ***            *********************** *
     * ***  End Long  *********************** *
     * ***            *********************** *
     * ************************************** *
     * ************************************** */

    /* ************************************** *
     * ************************************** *
     * ***         ************************** *
     * ***  Float  ************************** *
     * ***         ************************** *
     * ************************************** *
     * ************************************** */

    /**
     * Tests {@link Conversion#toByteArray(float)}.
     */
    @Test
    public void testToByteArray_float() {
        //Check raw bytes
        assertArrayEquals(TestBinaryByteBlocks.FLOAT_MAX_VAL_BYTES, Conversion.toByteArray(Float.MAX_VALUE));
        assertArrayEquals(TestBinaryByteBlocks.FLOAT_MIN_VAL_BYTES, Conversion.toByteArray(Float.MIN_VALUE));
        assertArrayEquals(TestBinaryByteBlocks.FLOAT_NEG_1_BYTES,   Conversion.toByteArray((float) -1));
        assertArrayEquals(TestBinaryByteBlocks.FLOAT_0_BYTES,       Conversion.toByteArray((float) 0));
        assertArrayEquals(TestBinaryByteBlocks.FLOAT_1_BYTES,       Conversion.toByteArray((float) 1));

        //Check full conversion cycle
        assertEquals(Float.MAX_VALUE, Conversion.toFloat(Conversion.toByteArray(Float.MAX_VALUE)), 0);
        assertEquals(Float.MIN_VALUE, Conversion.toFloat(Conversion.toByteArray(Float.MIN_VALUE)), 0);
        assertEquals((float) -1,      Conversion.toFloat(Conversion.toByteArray((float) -1)), 0);
        assertEquals((float) 0,       Conversion.toFloat(Conversion.toByteArray((float) 0)), 0);
        assertEquals((float) 1,       Conversion.toFloat(Conversion.toByteArray((float) 1)), 0);
    }

    /**
     * Tests {@link Conversion#toByteArrayBe(float)}.
     */
    @Test
    public void testToByteArrayBe_float() {
        //Check raw bytes
        assertArrayEquals(TestBinaryByteBlocks.Be_FLOAT_MAX_VAL_BYTES, Conversion.toByteArrayBe(Float.MAX_VALUE));
        assertArrayEquals(TestBinaryByteBlocks.Be_FLOAT_MIN_VAL_BYTES, Conversion.toByteArrayBe(Float.MIN_VALUE));
        assertArrayEquals(TestBinaryByteBlocks.Be_FLOAT_NEG_1_BYTES,   Conversion.toByteArrayBe((float) -1));
        assertArrayEquals(TestBinaryByteBlocks.Be_FLOAT_0_BYTES,       Conversion.toByteArrayBe((float) 0));
        assertArrayEquals(TestBinaryByteBlocks.Be_FLOAT_1_BYTES,       Conversion.toByteArrayBe((float) 1));

        //Check full conversion cycle
        assertEquals(Float.MAX_VALUE, Conversion.toFloatBe(Conversion.toByteArrayBe(Float.MAX_VALUE)), 0);
        assertEquals(Float.MIN_VALUE, Conversion.toFloatBe(Conversion.toByteArrayBe(Float.MIN_VALUE)), 0);
        assertEquals((float) -1,      Conversion.toFloatBe(Conversion.toByteArrayBe((float) -1)), 0);
        assertEquals((float) 0,       Conversion.toFloatBe(Conversion.toByteArrayBe((float) 0)), 0);
        assertEquals((float) 1,       Conversion.toFloatBe(Conversion.toByteArrayBe((float) 1)), 0);
    }

    /**
     * Tests {@link Conversion#toFloat(byte[])}.
     */
    @Test
    public void testToFloat() {
        //Check values
        assertEquals(Float.MAX_VALUE, Conversion.toFloat(TestBinaryByteBlocks.FLOAT_MAX_VAL_BYTES), 0);
        assertEquals(Float.MIN_VALUE, Conversion.toFloat(TestBinaryByteBlocks.FLOAT_MIN_VAL_BYTES), 0);
        assertEquals((float) -1,      Conversion.toFloat(TestBinaryByteBlocks.FLOAT_NEG_1_BYTES), 0);
        assertEquals((float) 0,       Conversion.toFloat(TestBinaryByteBlocks.FLOAT_0_BYTES), 0);
        assertEquals((float) 1,       Conversion.toFloat(TestBinaryByteBlocks.FLOAT_1_BYTES), 0);

        //Check Null & Empty
        assertEquals((float) 0, Conversion.toFloat((byte[]) null), 0);
        assertEquals((float) 0, Conversion.toFloat(new byte[0]), 0);

        //Check full conversion cycle
        assertEquals(Float.MAX_VALUE, Conversion.toFloat(Conversion.toByteArray(Float.MAX_VALUE)), 0);
        assertEquals(Float.MIN_VALUE, Conversion.toFloat(Conversion.toByteArray(Float.MIN_VALUE)), 0);
        assertEquals((float) -1,      Conversion.toFloat(Conversion.toByteArray((float) -1)), 0);
        assertEquals((float) 0,       Conversion.toFloat(Conversion.toByteArray((float) 0)), 0);
        assertEquals((float) 1,       Conversion.toFloat(Conversion.toByteArray((float) 1)), 0);
    }

    /**
     * Tests {@link Conversion#toFloat(byte[])}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testToFloat_tooLong() {
        Conversion.toFloat(new byte[TestBinaryByteBlocks.FLOAT_BYTES+1]);
    }

    /**
     * Tests {@link Conversion#toFloat(byte[])}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testToFloat_tooSmall() {
        Conversion.toFloat(new byte[TestBinaryByteBlocks.FLOAT_BYTES-1]);
    }

    /**
     * Tests {@link Conversion#toFloatBe(byte[])}.
     */
    @Test
    public void testToFloatBe() {
        //Check values
        assertEquals(Float.MAX_VALUE, Conversion.toFloatBe(TestBinaryByteBlocks.Be_FLOAT_MAX_VAL_BYTES), 0);
        assertEquals(Float.MIN_VALUE, Conversion.toFloatBe(TestBinaryByteBlocks.Be_FLOAT_MIN_VAL_BYTES), 0);
        assertEquals((float) -1,      Conversion.toFloatBe(TestBinaryByteBlocks.Be_FLOAT_NEG_1_BYTES), 0);
        assertEquals((float) 0,       Conversion.toFloatBe(TestBinaryByteBlocks.Be_FLOAT_0_BYTES), 0);
        assertEquals((float) 1,       Conversion.toFloatBe(TestBinaryByteBlocks.Be_FLOAT_1_BYTES), 0);

        //Check Null & Empty
        assertEquals((float) 0, Conversion.toFloatBe((byte[]) null), 0);
        assertEquals((float) 0, Conversion.toFloatBe(new byte[0]), 0);

        //Check full conversion cycle
        assertEquals(Float.MAX_VALUE, Conversion.toFloatBe(Conversion.toByteArrayBe(Float.MAX_VALUE)), 0);
        assertEquals(Float.MIN_VALUE, Conversion.toFloatBe(Conversion.toByteArrayBe(Float.MIN_VALUE)), 0);
        assertEquals((float) -1,      Conversion.toFloatBe(Conversion.toByteArrayBe((float) -1)), 0);
        assertEquals((float) 0,       Conversion.toFloatBe(Conversion.toByteArrayBe((float) 0)), 0);
        assertEquals((float) 1,       Conversion.toFloatBe(Conversion.toByteArrayBe((float) 1)), 0);
    }

    /**
     * Tests {@link Conversion#toFloatBe(byte[])}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testToFloatBe_tooLong() {
        Conversion.toFloatBe(new byte[TestBinaryByteBlocks.FLOAT_BYTES+1]);
    }

    /**
     * Tests {@link Conversion#toFloatBe(byte[])}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testToFloatBe_tooSmall() {
        Conversion.toFloatBe(new byte[TestBinaryByteBlocks.FLOAT_BYTES-1]);
    }

    /* ************************************** *
     * ***   Hex   ************************** *
     * ************************************** */

    /**
     * Tests {@link Conversion#hexToFloat(String)}.
     */
    @Test
    public void testHexToFloat_ConvenienceMethod() {
//        int SIZE = TestBinaryByteBlocks.FLOAT_BYTES*2; //2 hex digits per byte
//        float BASE_CODE_ANSWER = 0;
//
//        //Compare against existing implementation
//        assertEquals(Conversion.hexToFloat(TestBinaryByteBlocks.HEX_FLOAT_MAX_VAL_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
//                Conversion.hexToFloat(TestBinaryByteBlocks.HEX_FLOAT_MAX_VAL_BYTES));
//
//        assertEquals(Conversion.hexToFloat(TestBinaryByteBlocks.HEX_FLOAT_MIN_VAL_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
//                Conversion.hexToFloat(TestBinaryByteBlocks.HEX_FLOAT_MIN_VAL_BYTES));
//
//        assertEquals(Conversion.hexToFloat(TestBinaryByteBlocks.HEX_FLOAT_NEG_1_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
//                Conversion.hexToFloat(TestBinaryByteBlocks.HEX_FLOAT_NEG_1_BYTES));
//
//        assertEquals(Conversion.hexToFloat(TestBinaryByteBlocks.HEX_FLOAT_0_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
//                Conversion.hexToFloat(TestBinaryByteBlocks.HEX_FLOAT_0_BYTES));
//
//        assertEquals(Conversion.hexToFloat(TestBinaryByteBlocks.HEX_FLOAT_1_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
//                Conversion.hexToFloat(TestBinaryByteBlocks.HEX_FLOAT_1_BYTES));

        //Check values
        assertEquals(Float.MAX_VALUE, Conversion.hexToFloat(TestBinaryByteBlocks.HEX_FLOAT_MAX_VAL_BYTES), 0);
        assertEquals(Float.MIN_VALUE, Conversion.hexToFloat(TestBinaryByteBlocks.HEX_FLOAT_MIN_VAL_BYTES), 0);
        assertEquals((float) -1,      Conversion.hexToFloat(TestBinaryByteBlocks.HEX_FLOAT_NEG_1_BYTES), 0);
        assertEquals((float) 0,       Conversion.hexToFloat(TestBinaryByteBlocks.HEX_FLOAT_0_BYTES), 0);
        assertEquals((float) 1,       Conversion.hexToFloat(TestBinaryByteBlocks.HEX_FLOAT_1_BYTES), 0);

        //Check Null & Empty
        assertEquals((float) 0, Conversion.hexToFloat((String) null), 0);
        assertEquals((float) 0, Conversion.hexToFloat(""), 0);
        assertEquals((float) 0, Conversion.hexToFloat(".-- .\t\t __  \t-._"), 0);

        //Check full conversion cycle
        assertEquals(Float.MAX_VALUE, Conversion.hexToFloat(Conversion.toHex(Float.MAX_VALUE)), 0);
        assertEquals(Float.MIN_VALUE, Conversion.hexToFloat(Conversion.toHex(Float.MIN_VALUE)), 0);
        assertEquals((float) -1,      Conversion.hexToFloat(Conversion.toHex((float) -1)), 0);
        assertEquals((float) 0,       Conversion.hexToFloat(Conversion.toHex((float) 0)), 0);
        assertEquals((float) 1,       Conversion.hexToFloat(Conversion.toHex((float) 1)), 0);
    }

    /**
     * Tests {@link Conversion#hexToFloat(String)}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testHexToFloat_tooLong() {
        Conversion.hexToFloat(TestBinaryByteBlocks.getNumHexDigits(TestBinaryByteBlocks.FLOAT_BYTES+1));
    }

    /**
     * Tests {@link Conversion#hexToFloat(String)}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testHexToFloat_tooSmall() {
        Conversion.hexToFloat(TestBinaryByteBlocks.getNumHexDigits(TestBinaryByteBlocks.FLOAT_BYTES-1));
    }

    /**
     * Tests {@link Conversion#hexToFloatBe(String)}.
     */
    @Test
    public void testHexToFloatBe_ConvenienceMethod() {
        //Check values
        assertEquals(Float.MAX_VALUE, Conversion.hexToFloatBe(TestBinaryByteBlocks.Be_HEX_FLOAT_MAX_VAL_BYTES), 0);
        assertEquals(Float.MIN_VALUE, Conversion.hexToFloatBe(TestBinaryByteBlocks.Be_HEX_FLOAT_MIN_VAL_BYTES), 0);
        assertEquals((float) -1,      Conversion.hexToFloatBe(TestBinaryByteBlocks.Be_HEX_FLOAT_NEG_1_BYTES), 0);
        assertEquals((float) 0,       Conversion.hexToFloatBe(TestBinaryByteBlocks.Be_HEX_FLOAT_0_BYTES), 0);
        assertEquals((float) 1,       Conversion.hexToFloatBe(TestBinaryByteBlocks.Be_HEX_FLOAT_1_BYTES), 0);

        //Check Null & Empty
        assertEquals((float) 0, Conversion.hexToFloatBe((String) null), 0);
        assertEquals((float) 0, Conversion.hexToFloatBe(""), 0);
        assertEquals((float) 0, Conversion.hexToFloatBe(".-- .\t\t __  \t-._"), 0);

        //Check full conversion cycle
        assertEquals(Float.MAX_VALUE, Conversion.hexToFloatBe(Conversion.toHexBe(Float.MAX_VALUE)), 0);
        assertEquals(Float.MIN_VALUE, Conversion.hexToFloatBe(Conversion.toHexBe(Float.MIN_VALUE)), 0);
        assertEquals((float) -1,      Conversion.hexToFloatBe(Conversion.toHexBe((float) -1)), 0);
        assertEquals((float) 0,       Conversion.hexToFloatBe(Conversion.toHexBe((float) 0)), 0);
        assertEquals((float) 1,       Conversion.hexToFloatBe(Conversion.toHexBe((float) 1)), 0);
    }

    /**
     * Tests {@link Conversion#hexToFloatBe(String)}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testHexToFloatBe_tooLong() {
        Conversion.hexToFloatBe(TestBinaryByteBlocks.getNumHexDigits(TestBinaryByteBlocks.FLOAT_BYTES+1));
    }

    /**
     * Tests {@link Conversion#hexToFloatBe(String)}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testHexToFloatBe_tooSmall() {
        Conversion.hexToFloatBe(TestBinaryByteBlocks.getNumHexDigits(TestBinaryByteBlocks.FLOAT_BYTES-1));
    }

    /* ************************************** *
     * ************************************** *
     * ***             ********************** *
     * ***  End Float  ********************** *
     * ***             ********************** *
     * ************************************** *
     * ************************************** */

    /* ************************************** *
     * ************************************** *
     * ***          ************************* *
     * ***  Double  ************************* *
     * ***          ************************* *
     * ************************************** *
     * ************************************** */

    /**
     * Tests {@link Conversion#toByteArray(double)}.
     */
    @Test
    public void testToByteArray_double() {
        //Check raw bytes
        assertArrayEquals(TestBinaryByteBlocks.DOUBLE_MAX_VAL_BYTES, Conversion.toByteArray(Double.MAX_VALUE));
        assertArrayEquals(TestBinaryByteBlocks.DOUBLE_MIN_VAL_BYTES, Conversion.toByteArray(Double.MIN_VALUE));
        assertArrayEquals(TestBinaryByteBlocks.DOUBLE_NEG_1_BYTES,   Conversion.toByteArray((double) -1));
        assertArrayEquals(TestBinaryByteBlocks.DOUBLE_0_BYTES,       Conversion.toByteArray((double) 0));
        assertArrayEquals(TestBinaryByteBlocks.DOUBLE_1_BYTES,       Conversion.toByteArray((double) 1));

        //Check full conversion cycle
        assertEquals(Double.MAX_VALUE, Conversion.toDouble(Conversion.toByteArray(Double.MAX_VALUE)), 0);
        assertEquals(Double.MIN_VALUE, Conversion.toDouble(Conversion.toByteArray(Double.MIN_VALUE)), 0);
        assertEquals((double) -1,      Conversion.toDouble(Conversion.toByteArray((double) -1)), 0);
        assertEquals((double) 0,       Conversion.toDouble(Conversion.toByteArray((double) 0)), 0);
        assertEquals((double) 1,       Conversion.toDouble(Conversion.toByteArray((double) 1)), 0);
    }

    /**
     * Tests {@link Conversion#toByteArrayBe(double)}.
     */
    @Test
    public void testToByteArrayBe_double() {
        //Check raw bytes
        assertArrayEquals(TestBinaryByteBlocks.Be_DOUBLE_MAX_VAL_BYTES, Conversion.toByteArrayBe(Double.MAX_VALUE));
        assertArrayEquals(TestBinaryByteBlocks.Be_DOUBLE_MIN_VAL_BYTES, Conversion.toByteArrayBe(Double.MIN_VALUE));
        assertArrayEquals(TestBinaryByteBlocks.Be_DOUBLE_NEG_1_BYTES,   Conversion.toByteArrayBe((double) -1));
        assertArrayEquals(TestBinaryByteBlocks.Be_DOUBLE_0_BYTES,       Conversion.toByteArrayBe((double) 0));
        assertArrayEquals(TestBinaryByteBlocks.Be_DOUBLE_1_BYTES,       Conversion.toByteArrayBe((double) 1));

        //Check full conversion cycle
        assertEquals(Double.MAX_VALUE, Conversion.toDoubleBe(Conversion.toByteArrayBe(Double.MAX_VALUE)), 0);
        assertEquals(Double.MIN_VALUE, Conversion.toDoubleBe(Conversion.toByteArrayBe(Double.MIN_VALUE)), 0);
        assertEquals((double) -1,      Conversion.toDoubleBe(Conversion.toByteArrayBe((double) -1)), 0);
        assertEquals((double) 0,       Conversion.toDoubleBe(Conversion.toByteArrayBe((double) 0)), 0);
        assertEquals((double) 1,       Conversion.toDoubleBe(Conversion.toByteArrayBe((double) 1)), 0);
    }

    /**
     * Tests {@link Conversion#toDouble(byte[])}.
     */
    @Test
    public void testToDouble() {
        //Check values
        assertEquals(Double.MAX_VALUE, Conversion.toDouble(TestBinaryByteBlocks.DOUBLE_MAX_VAL_BYTES), 0);
        assertEquals(Double.MIN_VALUE, Conversion.toDouble(TestBinaryByteBlocks.DOUBLE_MIN_VAL_BYTES), 0);
        assertEquals((double) -1,      Conversion.toDouble(TestBinaryByteBlocks.DOUBLE_NEG_1_BYTES), 0);
        assertEquals((double) 0,       Conversion.toDouble(TestBinaryByteBlocks.DOUBLE_0_BYTES), 0);
        assertEquals((double) 1,       Conversion.toDouble(TestBinaryByteBlocks.DOUBLE_1_BYTES), 0);

        //Check Null & Empty
        assertEquals((double) 0, Conversion.toDouble((byte[]) null), 0);
        assertEquals((double) 0, Conversion.toDouble(new byte[0]), 0);

        //Check full conversion cycle
        assertEquals(Double.MAX_VALUE, Conversion.toDouble(Conversion.toByteArray(Double.MAX_VALUE)), 0);
        assertEquals(Double.MIN_VALUE, Conversion.toDouble(Conversion.toByteArray(Double.MIN_VALUE)), 0);
        assertEquals((double) -1,      Conversion.toDouble(Conversion.toByteArray((double) -1)), 0);
        assertEquals((double) 0,       Conversion.toDouble(Conversion.toByteArray((double) 0)), 0);
        assertEquals((double) 1,       Conversion.toDouble(Conversion.toByteArray((double) 1)), 0);
    }

    /**
     * Tests {@link Conversion#toDouble(byte[])}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testToDouble_tooLong() {
        Conversion.toDouble(new byte[TestBinaryByteBlocks.DOUBLE_BYTES+1]);
    }

    /**
     * Tests {@link Conversion#toDouble(byte[])}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testToDouble_tooSmall() {
        Conversion.toDouble(new byte[TestBinaryByteBlocks.DOUBLE_BYTES-1]);
    }

    /**
     * Tests {@link Conversion#toDoubleBe(byte[])}.
     */
    @Test
    public void testToDoubleBe() {
        //Check values
        assertEquals(Double.MAX_VALUE, Conversion.toDoubleBe(TestBinaryByteBlocks.Be_DOUBLE_MAX_VAL_BYTES), 0);
        assertEquals(Double.MIN_VALUE, Conversion.toDoubleBe(TestBinaryByteBlocks.Be_DOUBLE_MIN_VAL_BYTES), 0);
        assertEquals((double) -1,      Conversion.toDoubleBe(TestBinaryByteBlocks.Be_DOUBLE_NEG_1_BYTES), 0);
        assertEquals((double) 0,       Conversion.toDoubleBe(TestBinaryByteBlocks.Be_DOUBLE_0_BYTES), 0);
        assertEquals((double) 1,       Conversion.toDoubleBe(TestBinaryByteBlocks.Be_DOUBLE_1_BYTES), 0);

        //Check Null & Empty
        assertEquals((double) 0, Conversion.toDoubleBe((byte[]) null), 0);
        assertEquals((double) 0, Conversion.toDoubleBe(new byte[0]), 0);

        //Check full conversion cycle
        assertEquals(Double.MAX_VALUE, Conversion.toDoubleBe(Conversion.toByteArrayBe(Double.MAX_VALUE)), 0);
        assertEquals(Double.MIN_VALUE, Conversion.toDoubleBe(Conversion.toByteArrayBe(Double.MIN_VALUE)), 0);
        assertEquals((double) -1,      Conversion.toDoubleBe(Conversion.toByteArrayBe((double) -1)), 0);
        assertEquals((double) 0,       Conversion.toDoubleBe(Conversion.toByteArrayBe((double) 0)), 0);
        assertEquals((double) 1,       Conversion.toDoubleBe(Conversion.toByteArrayBe((double) 1)), 0);
    }

    /**
     * Tests {@link Conversion#toDoubleBe(byte[])}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testToDoubleBe_tooLong() {
        Conversion.toDoubleBe(new byte[TestBinaryByteBlocks.DOUBLE_BYTES+1]);
    }

    /**
     * Tests {@link Conversion#toDoubleBe(byte[])}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testToDoubleBe_tooSmall() {
        Conversion.toDoubleBe(new byte[TestBinaryByteBlocks.DOUBLE_BYTES-1]);
    }

    /* ************************************** *
     * ***   Hex   ************************** *
     * ************************************** */

    /**
     * Tests {@link Conversion#hexToDouble(String)}.
     */
    @Test
    public void testHexToDouble_ConvenienceMethod() {
//        int SIZE = TestBinaryByteBlocks.DOUBLE_BYTES*2; //2 hex digits per byte
//        double BASE_CODE_ANSWER = 0;
//
//        //Compare against existing implementation
//        assertEquals(Conversion.hexToDouble(TestBinaryByteBlocks.HEX_DOUBLE_MAX_VAL_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
//                Conversion.hexToDouble(TestBinaryByteBlocks.HEX_DOUBLE_MAX_VAL_BYTES));
//
//        assertEquals(Conversion.hexToDouble(TestBinaryByteBlocks.HEX_DOUBLE_MIN_VAL_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
//                Conversion.hexToDouble(TestBinaryByteBlocks.HEX_DOUBLE_MIN_VAL_BYTES));
//
//        assertEquals(Conversion.hexToDouble(TestBinaryByteBlocks.HEX_DOUBLE_NEG_1_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
//                Conversion.hexToDouble(TestBinaryByteBlocks.HEX_DOUBLE_NEG_1_BYTES));
//
//        assertEquals(Conversion.hexToDouble(TestBinaryByteBlocks.HEX_DOUBLE_0_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
//                Conversion.hexToDouble(TestBinaryByteBlocks.HEX_DOUBLE_0_BYTES));
//
//        assertEquals(Conversion.hexToDouble(TestBinaryByteBlocks.HEX_DOUBLE_1_BYTES, 0, BASE_CODE_ANSWER, 0, SIZE),
//                Conversion.hexToDouble(TestBinaryByteBlocks.HEX_DOUBLE_1_BYTES));

        //Check values
        assertEquals(Double.MAX_VALUE, Conversion.hexToDouble(TestBinaryByteBlocks.HEX_DOUBLE_MAX_VAL_BYTES), 0);
        assertEquals(Double.MIN_VALUE, Conversion.hexToDouble(TestBinaryByteBlocks.HEX_DOUBLE_MIN_VAL_BYTES), 0);
        assertEquals((double) -1,      Conversion.hexToDouble(TestBinaryByteBlocks.HEX_DOUBLE_NEG_1_BYTES), 0);
        assertEquals((double) 0,       Conversion.hexToDouble(TestBinaryByteBlocks.HEX_DOUBLE_0_BYTES), 0);
        assertEquals((double) 1,       Conversion.hexToDouble(TestBinaryByteBlocks.HEX_DOUBLE_1_BYTES), 0);

        //Check Null & Empty
        assertEquals((double) 0, Conversion.hexToDouble((String) null), 0);
        assertEquals((double) 0, Conversion.hexToDouble(""), 0);
        assertEquals((double) 0, Conversion.hexToDouble(".-- .\t\t __  \t-._"), 0);

        //Check full conversion cycle
        assertEquals(Double.MAX_VALUE, Conversion.hexToDouble(Conversion.toHex(Double.MAX_VALUE)), 0);
        assertEquals(Double.MIN_VALUE, Conversion.hexToDouble(Conversion.toHex(Double.MIN_VALUE)), 0);
        assertEquals((double) -1,      Conversion.hexToDouble(Conversion.toHex((double) -1)), 0);
        assertEquals((double) 0,       Conversion.hexToDouble(Conversion.toHex((double) 0)), 0);
        assertEquals((double) 1,       Conversion.hexToDouble(Conversion.toHex((double) 1)), 0);
    }

    /**
     * Tests {@link Conversion#hexToDouble(String)}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testHexToDouble_tooLong() {
        Conversion.hexToDouble(TestBinaryByteBlocks.getNumHexDigits(TestBinaryByteBlocks.DOUBLE_BYTES+1));
    }

    /**
     * Tests {@link Conversion#hexToDouble(String)}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testHexToDouble_tooSmall() {
        Conversion.hexToDouble(TestBinaryByteBlocks.getNumHexDigits(TestBinaryByteBlocks.DOUBLE_BYTES-1));
    }

    /**
     * Tests {@link Conversion#hexToDoubleBe(String)}.
     */
    @Test
    public void testHexToDoubleBe_ConvenienceMethod() {
        //Check values
        assertEquals(Double.MAX_VALUE, Conversion.hexToDoubleBe(TestBinaryByteBlocks.Be_HEX_DOUBLE_MAX_VAL_BYTES), 0);
        assertEquals(Double.MIN_VALUE, Conversion.hexToDoubleBe(TestBinaryByteBlocks.Be_HEX_DOUBLE_MIN_VAL_BYTES), 0);
        assertEquals((double) -1,      Conversion.hexToDoubleBe(TestBinaryByteBlocks.Be_HEX_DOUBLE_NEG_1_BYTES), 0);
        assertEquals((double) 0,       Conversion.hexToDoubleBe(TestBinaryByteBlocks.Be_HEX_DOUBLE_0_BYTES), 0);
        assertEquals((double) 1,       Conversion.hexToDoubleBe(TestBinaryByteBlocks.Be_HEX_DOUBLE_1_BYTES), 0);

        //Check Null & Empty
        assertEquals((double) 0, Conversion.hexToDoubleBe((String) null), 0);
        assertEquals((double) 0, Conversion.hexToDoubleBe(""), 0);
        assertEquals((double) 0, Conversion.hexToDoubleBe(".-- .\t\t __  \t-._"), 0);

        //Check full conversion cycle
        assertEquals(Double.MAX_VALUE, Conversion.hexToDoubleBe(Conversion.toHexBe(Double.MAX_VALUE)), 0);
        assertEquals(Double.MIN_VALUE, Conversion.hexToDoubleBe(Conversion.toHexBe(Double.MIN_VALUE)), 0);
        assertEquals((double) -1,      Conversion.hexToDoubleBe(Conversion.toHexBe((double) -1)), 0);
        assertEquals((double) 0,       Conversion.hexToDoubleBe(Conversion.toHexBe((double) 0)), 0);
        assertEquals((double) 1,       Conversion.hexToDoubleBe(Conversion.toHexBe((double) 1)), 0);
    }

    /**
     * Tests {@link Conversion#hexToDoubleBe(String)}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testHexToDoubleBe_tooLong() {
        Conversion.hexToDoubleBe(TestBinaryByteBlocks.getNumHexDigits(TestBinaryByteBlocks.DOUBLE_BYTES+1));
    }

    /**
     * Tests {@link Conversion#hexToDoubleBe(String)}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testHexToDoubleBe_tooSmall() {
        Conversion.hexToDoubleBe(TestBinaryByteBlocks.getNumHexDigits(TestBinaryByteBlocks.DOUBLE_BYTES-1));
    }

    /* ************************************** *
     * ************************************** *
     * ***              ********************* *
     * ***  End Double  ********************* *
     * ***              ********************* *
     * ************************************** *
     * ************************************** */

    /**
     * Tests {@link Conversion#SINGLE_DIGIT_HEX_LOOKUP_TABLE}.
     */
    @Test
    public void testSingleDigitHexLookupTable() {
        assertArrayEquals(TestBinaryByteBlocks.SINGLE_HEX_LU, Conversion.SINGLE_DIGIT_HEX_LOOKUP_TABLE);
    }

    /**
     * Tests {@link Conversion#DOUBLE_DIGIT_HEX_LOOKUP_TABLE}.
     */
    @Test
    public void testDoubleDigitHexLookupTable() {
        assertArrayEquals(TestBinaryByteBlocks.DOUBLE_HEX_LU, Conversion.DOUBLE_DIGIT_HEX_LOOKUP_TABLE);
    }

    /**
     * Tests {@link Conversion#DOUBLE_DIGIT_HEX_REVERSE_LOOKUP_TABLE}.
     */
    @Test
    public void testDoubleDigitHexReverseLookupTable() {
        int SIZE = 256;

        //Check num elements
        assertEquals(TestBinaryByteBlocks.DOUBLE_HEX_REV_LU.size(), Conversion.DOUBLE_DIGIT_HEX_REVERSE_LOOKUP_TABLE.size());
        assertEquals(SIZE, Conversion.DOUBLE_DIGIT_HEX_REVERSE_LOOKUP_TABLE.size());

        //Ensure contains all
        for (int x=0; x<SIZE; x++) {
            String hexVal = TestBinaryByteBlocks.DOUBLE_HEX_LU[x];
            assertEquals(TestBinaryByteBlocks.DOUBLE_HEX_REV_LU.get(hexVal),
                    Conversion.DOUBLE_DIGIT_HEX_REVERSE_LOOKUP_TABLE.get(hexVal));
        }
    }

    /**
     * Tests {@link Conversion#toHex(byte)}.
     */
    @Test
    public void testToHex_byte() {
        for (byte x=Byte.MIN_VALUE; x<Byte.MAX_VALUE; x++) {
            assertEquals(TestBinaryByteBlocks.DOUBLE_HEX_LU[x & 0xFF], Conversion.toHex(x));
        }
    }

    /**
     * Tests {@link Conversion#toHexRaw(byte[])}.
     */
    @Test
    public void testToHexRaw() {
        //Check values
        assertEquals(_getHexOctets(TestBinaryByteBlocks.LONG_MAX_VAL_BYTES),
                Conversion.toHexRaw(TestBinaryByteBlocks.LONG_MAX_VAL_BYTES));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.LONG_MIN_VAL_BYTES),
                Conversion.toHexRaw(TestBinaryByteBlocks.LONG_MIN_VAL_BYTES));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.LONG_NEG_1_BYTES),
                Conversion.toHexRaw(TestBinaryByteBlocks.LONG_NEG_1_BYTES));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.LONG_0_BYTES),
                Conversion.toHexRaw(TestBinaryByteBlocks.LONG_0_BYTES));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.LONG_1_BYTES),
                Conversion.toHexRaw(TestBinaryByteBlocks.LONG_1_BYTES));

        //Check Null & Empty
        assertEquals("00", Conversion.toHexRaw((byte[]) null));
        assertEquals("00", Conversion.toHexRaw(new byte[0]));
    }

    private String _getHexOctets(byte[] chunks) {
        StringBuilder builder = new StringBuilder();
        for (int x=0; x<chunks.length; x++) {
            builder.append(TestBinaryByteBlocks.DOUBLE_HEX_LU[chunks[x] & 0xFF]);
        }
        return builder.toString();
    }

    @Test
    public void testToHex_short() {
        assertEquals(_getHexOctets(TestBinaryByteBlocks.SHORT_MAX_VAL_BYTES), Conversion.toHex(Short.MAX_VALUE));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.SHORT_MIN_VAL_BYTES), Conversion.toHex(Short.MIN_VALUE));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.SHORT_NEG_1_BYTES),   Conversion.toHex((short) -1));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.SHORT_0_BYTES),       Conversion.toHex((short) 0));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.SHORT_1_BYTES),       Conversion.toHex((short) 1));
    }

    @Test
    public void testToHex_shortBe() {
        assertEquals(_getHexOctets(TestBinaryByteBlocks.Be_SHORT_MAX_VAL_BYTES), Conversion.toHexBe(Short.MAX_VALUE));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.Be_SHORT_MIN_VAL_BYTES), Conversion.toHexBe(Short.MIN_VALUE));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.Be_SHORT_NEG_1_BYTES),   Conversion.toHexBe((short) -1));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.Be_SHORT_0_BYTES),       Conversion.toHexBe((short) 0));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.Be_SHORT_1_BYTES),       Conversion.toHexBe((short) 1));
    }

    @Test
    public void testToHex_int() {
        assertEquals(_getHexOctets(TestBinaryByteBlocks.INT_MAX_VAL_BYTES), Conversion.toHex(Integer.MAX_VALUE));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.INT_MIN_VAL_BYTES), Conversion.toHex(Integer.MIN_VALUE));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.INT_NEG_1_BYTES),   Conversion.toHex((int) -1));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.INT_0_BYTES),       Conversion.toHex((int) 0));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.INT_1_BYTES),       Conversion.toHex((int) 1));
    }

    @Test
    public void testToHex_intBe() {
        assertEquals(_getHexOctets(TestBinaryByteBlocks.Be_INT_MAX_VAL_BYTES), Conversion.toHexBe(Integer.MAX_VALUE));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.Be_INT_MIN_VAL_BYTES), Conversion.toHexBe(Integer.MIN_VALUE));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.Be_INT_NEG_1_BYTES),   Conversion.toHexBe((int) -1));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.Be_INT_0_BYTES),       Conversion.toHexBe((int) 0));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.Be_INT_1_BYTES),       Conversion.toHexBe((int) 1));
    }

    @Test
    public void testToHex_long() {
        assertEquals(_getHexOctets(TestBinaryByteBlocks.LONG_MAX_VAL_BYTES), Conversion.toHex(Long.MAX_VALUE));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.LONG_MIN_VAL_BYTES), Conversion.toHex(Long.MIN_VALUE));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.LONG_NEG_1_BYTES),   Conversion.toHex((long) -1));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.LONG_0_BYTES),       Conversion.toHex((long) 0));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.LONG_1_BYTES),       Conversion.toHex((long) 1));
    }

    @Test
    public void testToHex_longBe() {
        assertEquals(_getHexOctets(TestBinaryByteBlocks.Be_LONG_MAX_VAL_BYTES), Conversion.toHexBe(Long.MAX_VALUE));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.Be_LONG_MIN_VAL_BYTES), Conversion.toHexBe(Long.MIN_VALUE));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.Be_LONG_NEG_1_BYTES),   Conversion.toHexBe((long) -1));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.Be_LONG_0_BYTES),       Conversion.toHexBe((long) 0));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.Be_LONG_1_BYTES),       Conversion.toHexBe((long) 1));
    }

    @Test
    public void testToHex_float() {
        assertEquals(_getHexOctets(TestBinaryByteBlocks.FLOAT_MAX_VAL_BYTES), Conversion.toHex(Float.MAX_VALUE));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.FLOAT_MIN_VAL_BYTES), Conversion.toHex(Float.MIN_VALUE));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.FLOAT_NEG_1_BYTES),   Conversion.toHex((float) -1));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.FLOAT_0_BYTES),       Conversion.toHex((float) 0));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.FLOAT_1_BYTES),       Conversion.toHex((float) 1));
    }

    @Test
    public void testToHex_floatBe() {
        assertEquals(_getHexOctets(TestBinaryByteBlocks.Be_FLOAT_MAX_VAL_BYTES), Conversion.toHexBe(Float.MAX_VALUE));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.Be_FLOAT_MIN_VAL_BYTES), Conversion.toHexBe(Float.MIN_VALUE));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.Be_FLOAT_NEG_1_BYTES),   Conversion.toHexBe((float) -1));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.Be_FLOAT_0_BYTES),       Conversion.toHexBe((float) 0));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.Be_FLOAT_1_BYTES),       Conversion.toHexBe((float) 1));
    }

    @Test
    public void testToHex_double() {
        assertEquals(_getHexOctets(TestBinaryByteBlocks.DOUBLE_MAX_VAL_BYTES), Conversion.toHex(Double.MAX_VALUE));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.DOUBLE_MIN_VAL_BYTES), Conversion.toHex(Double.MIN_VALUE));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.DOUBLE_NEG_1_BYTES),   Conversion.toHex((double) -1));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.DOUBLE_0_BYTES),       Conversion.toHex((double) 0));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.DOUBLE_1_BYTES),       Conversion.toHex((double) 1));
    }

    @Test
    public void testToHex_doubleBe() {
        assertEquals(_getHexOctets(TestBinaryByteBlocks.Be_DOUBLE_MAX_VAL_BYTES), Conversion.toHexBe(Double.MAX_VALUE));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.Be_DOUBLE_MIN_VAL_BYTES), Conversion.toHexBe(Double.MIN_VALUE));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.Be_DOUBLE_NEG_1_BYTES),   Conversion.toHexBe((double) -1));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.Be_DOUBLE_0_BYTES),       Conversion.toHexBe((double) 0));
        assertEquals(_getHexOctets(TestBinaryByteBlocks.Be_DOUBLE_1_BYTES),       Conversion.toHexBe((double) 1));
    }

    public static final class TestBinaryByteBlocks {
        public static String getNumHexDigits(int numHexDigits) {
            return getNumHexDigits(numHexDigits, 'F');
        }
        public static String getNumHexDigits(int numHexDigits, char hexDigit) {
            StringBuilder builder = new StringBuilder();
            for (int x=0; x<numHexDigits; x++) {
                builder.append(hexDigit);
            }
            return builder.toString();
        }
        
        public static final int SHORT_BYTES  = 2;
        public static final int INT_BYTES    = 4;
        public static final int LONG_BYTES   = 8;
        public static final int FLOAT_BYTES  = 4;
        public static final int DOUBLE_BYTES = 8;

        public static final char[] SINGLE_HEX_LU = new char[] {'0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'};
        public static final String[] DOUBLE_HEX_LU;
        public static final Map<String, Byte> DOUBLE_HEX_REV_LU;
        static {
            //Init Forward Lookup
            DOUBLE_HEX_LU = new String[256];
            int index = 0;
            for (int x=0; x<16; x++) {
                for (int y=0; y<16; y++) {
                    DOUBLE_HEX_LU[index++] = "" + SINGLE_HEX_LU[x] + SINGLE_HEX_LU[y];
                }
            }

            //Init Reverse Lookup
            DOUBLE_HEX_REV_LU = new HashMap<String, Byte>(256);
            for (int x=0; x<256; x++) {
                DOUBLE_HEX_REV_LU.put(DOUBLE_HEX_LU[x], (byte)x);
            }
        }

        //Short
        public static final byte[]    SHORT_MAX_VAL_BYTES = new byte[] {(byte) 0xFF, (byte) 0x7F};
        public static final byte[] Be_SHORT_MAX_VAL_BYTES = new byte[] {(byte) 0x7F, (byte) 0xFF};
        public static final byte[]    SHORT_MIN_VAL_BYTES = new byte[] {(byte) 0x00, (byte) 0x80};
        public static final byte[] Be_SHORT_MIN_VAL_BYTES = new byte[] {(byte) 0x80, (byte) 0x00};
        public static final byte[]    SHORT_NEG_1_BYTES   = new byte[] {(byte) 0xFF, (byte) 0xFF};
        public static final byte[] Be_SHORT_NEG_1_BYTES   = new byte[] {(byte) 0xFF, (byte) 0xFF};
        public static final byte[]    SHORT_0_BYTES       = new byte[] {(byte) 0x00, (byte) 0x00};
        public static final byte[] Be_SHORT_0_BYTES       = new byte[] {(byte) 0x00, (byte) 0x00};
        public static final byte[]    SHORT_1_BYTES       = new byte[] {(byte) 0x01, (byte) 0x00};
        public static final byte[] Be_SHORT_1_BYTES       = new byte[] {(byte) 0x00, (byte) 0x01};
        public static final String    HEX_SHORT_MAX_VAL_BYTES = "FF7F";
        public static final String Be_HEX_SHORT_MAX_VAL_BYTES = "7FFF";
        public static final String    HEX_SHORT_MIN_VAL_BYTES = "0080";
        public static final String Be_HEX_SHORT_MIN_VAL_BYTES = "8000";
        public static final String    HEX_SHORT_NEG_1_BYTES   = "FFFF";
        public static final String Be_HEX_SHORT_NEG_1_BYTES   = "FFFF";
        public static final String    HEX_SHORT_0_BYTES       = "0000";
        public static final String Be_HEX_SHORT_0_BYTES       = "0000";
        public static final String    HEX_SHORT_1_BYTES       = "0100";
        public static final String Be_HEX_SHORT_1_BYTES       = "0001";

        //Int
        public static final byte[]    INT_MAX_VAL_BYTES = new byte[] {(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x7F};
        public static final byte[] Be_INT_MAX_VAL_BYTES = new byte[] {(byte) 0x7F, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF};
        public static final byte[]    INT_MIN_VAL_BYTES = new byte[] {(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x80};
        public static final byte[] Be_INT_MIN_VAL_BYTES = new byte[] {(byte) 0x80, (byte) 0x00, (byte) 0x00, (byte) 0x00};
        public static final byte[]    INT_NEG_1_BYTES   = new byte[] {(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF};
        public static final byte[] Be_INT_NEG_1_BYTES   = new byte[] {(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF};
        public static final byte[]    INT_0_BYTES       = new byte[] {(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00};
        public static final byte[] Be_INT_0_BYTES       = new byte[] {(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00};
        public static final byte[]    INT_1_BYTES       = new byte[] {(byte) 0x01, (byte) 0x00, (byte) 0x00, (byte) 0x00};
        public static final byte[] Be_INT_1_BYTES       = new byte[] {(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x01};
        public static final String    HEX_INT_MAX_VAL_BYTES = "FFFFFF7F";
        public static final String Be_HEX_INT_MAX_VAL_BYTES = "7FFFFFFF";
        public static final String    HEX_INT_MIN_VAL_BYTES = "00000080";
        public static final String Be_HEX_INT_MIN_VAL_BYTES = "80000000";
        public static final String    HEX_INT_NEG_1_BYTES   = "FFFFFFFF";
        public static final String Be_HEX_INT_NEG_1_BYTES   = "FFFFFFFF";
        public static final String    HEX_INT_0_BYTES       = "00000000";
        public static final String Be_HEX_INT_0_BYTES       = "00000000";
        public static final String    HEX_INT_1_BYTES       = "01000000";
        public static final String Be_HEX_INT_1_BYTES       = "00000001";

        //Long
        public static final byte[]    LONG_MAX_VAL_BYTES = new byte[] {(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x7F};
        public static final byte[] Be_LONG_MAX_VAL_BYTES = new byte[] {(byte) 0x7F, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF};
        public static final byte[]    LONG_MIN_VAL_BYTES = new byte[] {(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x80};
        public static final byte[] Be_LONG_MIN_VAL_BYTES = new byte[] {(byte) 0x80, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00};
        public static final byte[]    LONG_NEG_1_BYTES   = new byte[] {(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF};
        public static final byte[] Be_LONG_NEG_1_BYTES   = new byte[] {(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF};
        public static final byte[]    LONG_0_BYTES       = new byte[] {(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00};
        public static final byte[] Be_LONG_0_BYTES       = new byte[] {(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00};
        public static final byte[]    LONG_1_BYTES       = new byte[] {(byte) 0x01, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00};
        public static final byte[] Be_LONG_1_BYTES       = new byte[] {(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x01};
        public static final String    HEX_LONG_MAX_VAL_BYTES = "FFFFFFFFFFFFFF7F";
        public static final String Be_HEX_LONG_MAX_VAL_BYTES = "7FFFFFFFFFFFFFFF";
        public static final String    HEX_LONG_MIN_VAL_BYTES = "0000000000000080";
        public static final String Be_HEX_LONG_MIN_VAL_BYTES = "8000000000000000";
        public static final String    HEX_LONG_NEG_1_BYTES   = "FFFFFFFFFFFFFFFF";
        public static final String Be_HEX_LONG_NEG_1_BYTES   = "FFFFFFFFFFFFFFFF";
        public static final String    HEX_LONG_0_BYTES       = "0000000000000000";
        public static final String Be_HEX_LONG_0_BYTES       = "0000000000000000";
        public static final String    HEX_LONG_1_BYTES       = "0100000000000000";
        public static final String Be_HEX_LONG_1_BYTES       = "0000000000000001";

        //Float
        public static final byte[]    FLOAT_MAX_VAL_BYTES = new byte[] {(byte) 0xFF, (byte) 0xFF, (byte) 0x7F, (byte) 0x7F};
        public static final byte[] Be_FLOAT_MAX_VAL_BYTES = new byte[] {(byte) 0x7F, (byte) 0x7F, (byte) 0xFF, (byte) 0xFF};
        public static final byte[]    FLOAT_MIN_VAL_BYTES = new byte[] {(byte) 0x01, (byte) 0x00, (byte) 0x00, (byte) 0x00};
        public static final byte[] Be_FLOAT_MIN_VAL_BYTES = new byte[] {(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x01};
        public static final byte[]    FLOAT_NEG_1_BYTES   = new byte[] {(byte) 0x00, (byte) 0x00, (byte) 0x80, (byte) 0xBF};
        public static final byte[] Be_FLOAT_NEG_1_BYTES   = new byte[] {(byte) 0xBF, (byte) 0x80, (byte) 0x00, (byte) 0x00};
        public static final byte[]    FLOAT_0_BYTES       = new byte[] {(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00};
        public static final byte[] Be_FLOAT_0_BYTES       = new byte[] {(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00};
        public static final byte[]    FLOAT_1_BYTES       = new byte[] {(byte) 0x00, (byte) 0x00, (byte) 0x80, (byte) 0x3F};
        public static final byte[] Be_FLOAT_1_BYTES       = new byte[] {(byte) 0x3F, (byte) 0x80, (byte) 0x00, (byte) 0x00};
        public static final String    HEX_FLOAT_MAX_VAL_BYTES = "FFFF7F7F";
        public static final String Be_HEX_FLOAT_MAX_VAL_BYTES = "7F7FFFFF";
        public static final String    HEX_FLOAT_MIN_VAL_BYTES = "01000000";
        public static final String Be_HEX_FLOAT_MIN_VAL_BYTES = "00000001";
        public static final String    HEX_FLOAT_NEG_1_BYTES   = "000080BF";
        public static final String Be_HEX_FLOAT_NEG_1_BYTES   = "BF800000";
        public static final String    HEX_FLOAT_0_BYTES       = "00000000";
        public static final String Be_HEX_FLOAT_0_BYTES       = "00000000";
        public static final String    HEX_FLOAT_1_BYTES       = "0000803F";
        public static final String Be_HEX_FLOAT_1_BYTES       = "3F800000";

        //Double
        public static final byte[]    DOUBLE_MAX_VAL_BYTES = new byte[] {(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xEF, (byte) 0x7F};
        public static final byte[] Be_DOUBLE_MAX_VAL_BYTES = new byte[] {(byte) 0x7F, (byte) 0xEF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF};
        public static final byte[]    DOUBLE_MIN_VAL_BYTES = new byte[] {(byte) 0x01, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00};
        public static final byte[] Be_DOUBLE_MIN_VAL_BYTES = new byte[] {(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x01};
        public static final byte[]    DOUBLE_NEG_1_BYTES   = new byte[] {(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0xF0, (byte) 0xBF};
        public static final byte[] Be_DOUBLE_NEG_1_BYTES   = new byte[] {(byte) 0xBF, (byte) 0xF0, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00};
        public static final byte[]    DOUBLE_0_BYTES       = new byte[] {(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00};
        public static final byte[] Be_DOUBLE_0_BYTES       = new byte[] {(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00};
        public static final byte[]    DOUBLE_1_BYTES       = new byte[] {(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0xF0, (byte) 0x3F};
        public static final byte[] Be_DOUBLE_1_BYTES       = new byte[] {(byte) 0x3F, (byte) 0xF0, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00};
        public static final String    HEX_DOUBLE_MAX_VAL_BYTES = "FFFFFFFFFFFFEF7F";
        public static final String Be_HEX_DOUBLE_MAX_VAL_BYTES = "7FEFFFFFFFFFFFFF";
        public static final String    HEX_DOUBLE_MIN_VAL_BYTES = "0100000000000000";
        public static final String Be_HEX_DOUBLE_MIN_VAL_BYTES = "0000000000000001";
        public static final String    HEX_DOUBLE_NEG_1_BYTES   = "000000000000F0BF";
        public static final String Be_HEX_DOUBLE_NEG_1_BYTES   = "BFF0000000000000";
        public static final String    HEX_DOUBLE_0_BYTES       = "0000000000000000";
        public static final String Be_HEX_DOUBLE_0_BYTES       = "0000000000000000";
        public static final String    HEX_DOUBLE_1_BYTES       = "000000000000F03F";
        public static final String Be_HEX_DOUBLE_1_BYTES       = "3FF0000000000000";

        public static final boolean[][] binarySemiTrueBits = new boolean[][] {
            {true, false, false, false, false, false, false, false},  //128
            {true, true, false, false, false, false, false, false},   //192
            {true, true, true, false, false, false, false, false},    //224
            {true, true, true, true, false, false, false, false},     //240
            {true, true, true, true, true, false, false, false},      //248
            {true, true, true, true, true, true, false, false},       //252
            {true, true, true, true, true, true, true, false}         //254
        };

        public static final String[] bitStringSemiTrueBits = new String[] {
            "10000000", //128
            "11000000", //192
            "11100000", //224
            "11110000", //240
            "11111000", //248
            "11111100", //252
            "11111110"  //254
        };

        public static final byte[] byteSemiTrueBits = new byte[] {
            (byte) 128,
            (byte) 192,
            (byte) 224,
            (byte) 240,
            (byte) 248,
            (byte) 252,
            (byte) 254
        };

        public static final boolean[][] binarySemiFalseBits = new boolean[][] {
            {false, true, true, true, true, true, true, true},        //127
            {false, false, true, true, true, true, true, true},       //63
            {false, false, false, true, true, true, true, true},      //31
            {false, false, false, false, true, true, true, true},     //15
            {false, false, false, false, false, true, true, true},    //7
            {false, false, false, false, false, false, true, true},   //3
            {false, false, false, false, false, false, false, true}   //1
        };

        public static final String[] bitStringSemiFalseBits = new String[] {
            "01111111", //127
            "00111111", //63
            "00011111", //31
            "00001111", //15
            "00000111", //7
            "00000011", //3
            "00000001"  //1
        };

        public static final byte[] byteSemiFalseBits = new byte[] {
            (byte) 127,
            (byte) 63,
            (byte) 31,
            (byte) 15,
            (byte) 7,
            (byte) 3,
            (byte) 1
        };

        public static final boolean[][] binarySingleTrueBits = new boolean[][] {
            {true, false, false, false, false, false, false, false},  //128
            {false, true, false, false, false, false, false, false},  //64
            {false, false, true, false, false, false, false, false},  //32
            {false, false, false, true, false, false, false, false},  //16
            {false, false, false, false, true, false, false, false},  //8
            {false, false, false, false, false, true, false, false},  //4
            {false, false, false, false, false, false, true, false},  //2
            {false, false, false, false, false, false, false, true}   //1
        };

        public static final String[] bitStringSingleTrueBits = new String[] {
            "10000000", //128
            "01000000", //64
            "00100000", //32
            "00010000", //16
            "00001000", //8
            "00000100", //4
            "00000010", //2
            "00000001"  //1
        };

        public static final byte[] byteSingleTrueBits = new byte[] {
            (byte) 128,
            (byte) 64,
            (byte) 32,
            (byte) 16,
            (byte) 8,
            (byte) 4,
            (byte) 2,
            (byte) 1
        };

        public static final boolean[][] binarySingleFalseBits = new boolean[][] {
            {false, true, true, true, true, true, true, true},  //127
            {true, false, true, true, true, true, true, true},  //191
            {true, true, false, true, true, true, true, true},  //223
            {true, true, true, false, true, true, true, true},  //239
            {true, true, true, true, false, true, true, true},  //247
            {true, true, true, true, true, false, true, true},  //251
            {true, true, true, true, true, true, false, true},  //253
            {true, true, true, true, true, true, true, false}   //254
        };

        public static final String[] bitStringSingleFalseBits = new String[] {
            "01111111", //127
            "10111111", //191
            "11011111", //223
            "11101111", //239
            "11110111", //247
            "11111011", //251
            "11111101", //253
            "11111110"  //254
        };

        public static final byte[] byteSingleFalseBits = new byte[] {
            (byte) 127,
            (byte) 191,
            (byte) 223,
            (byte) 239,
            (byte) 247,
            (byte) 251,
            (byte) 253,
            (byte) 254
        };

        public static final boolean[][] binaryPairTrueBits = new boolean[][] {
            {true, false, true, false, false, false, false, false}, //160
            {false, true, false, true, false, false, false, false}, //80
            {false, false, true, false, true, false, false, false}, //40
            {false, false, false, true, false, true, false, false}, //20
            {false, false, false, false, true, false, true, false}, //10
            {false, false, false, false, false, true, false, true}  //5
        };

        public static final String[] bitStringPairTrueBits = new String[] {
            "10100000", //160
            "01010000", //80
            "00101000", //40
            "00010100", //20
            "00001010", //10
            "00000101"  //5
        };

        public static final byte[] bytePairTrueBits = new byte[] {
            (byte) 160,
            (byte) 80,
            (byte) 40,
            (byte) 20,
            (byte) 10,
            (byte) 5
        };

        public static final boolean[][] binaryPairFalseBits = new boolean[][] {
            {false, true, false, true, true, true, true, true},   //95
            {true, false, true, false, true, true, true, true},   //175
            {true, true, false, true, false, true, true, true},   //215
            {true, true, true, false, true, false, true, true},   //235
            {true, true, true, true, false, true, false, true},   //245
            {true, true, true, true, true, false, true, false}    //250
        };

        public static final String[] bitStringPairFalseBits = new String[] {
            "01011111", //95
            "10101111", //175
            "11010111", //215
            "11101011", //235
            "11110101", //245
            "11111010"  //250
        };

        public static final byte[] bytePairFalseBits = new byte[] {
            (byte) 95,
            (byte) 175,
            (byte) 215,
            (byte) 235,
            (byte) 245,
            (byte) 250
        };

        public static final boolean[][] binary_Undersized = new boolean[][] {
            {true, false, true, false}, //10
            {false, true, false, true}  //5
        };

        public static final String[] bitString_Undersized  = new String[] {
            "1010", //10
            "0101"  //5
        };

        public static final byte[] byte_Undersized = new byte[] {
            (byte) 10,
            (byte) 5
        };

        public static final byte[] byte_Undersized_Msb0_fullByteValue = new byte[] {
            (byte) 160,
            (byte) 80
        };

        public static final boolean[][] binary_Oversized_Leading_1s = new boolean[][] {
            {true, true, true, true, true, true, true, true, true, false, true, false},     //4090
            {false, true, false, true, true, true, true, true, true, true, true, true}      //1535
        };

        public static final boolean[][] binary_Oversized_Trailing_1s = new boolean[][] {
            {true, true, true, true, true, false, true, false, true, true, true, true},     //4015
            {true, true, true, true, false, true, false, true, true, true, true, true}      //3935
        };

        public static final boolean[][] binary_Oversized_Leading_0s = new boolean[][] {
            {false, false, false, false, true, true, true, true, true, false, true, false}, //250
            {false, true, false, true, true, true, true, true, false, false, false, false}  //1520
        };

        public static final boolean[][] binary_Oversized_Trailing_0s = new boolean[][] {
            {true, true, true, true, true, false, true, false, false, false, false, false}, //4000
            {false, false, false, false, false, true, false, true, true, true, true, true}  //95
        };

        public static final String[] bitString_Oversized_Leading_1s  = new String[] {
            "111111111010", //4090
            "010111111111"  //1535
        };

        public static final String[] bitString_Oversized_Trailing_1s = new String[] {
            "111110101111", //4015
            "111101011111"  //3935
        };

        public static final String[] bitString_Oversized_Leading_0s  = new String[] {
            "000011111010", //250
            "010111110000"  //1520
        };

        public static final String[] bitString_Oversized_Trailing_0s = new String[] {
            "111110100000", //4000
            "000001011111"  //95
        };

        public static final byte[] byte_Delimited_None = new byte[] {
            (byte) 183,
            (byte) 237
        };

        public static final boolean[][] binary_Delimited_None = new boolean[][] {
            {true, false, true, true, false, true, true, true}, //183
            {true, true, true, false, true, true, false, true}  //237
        };

        public static final String[] bitString_Delimited_None = new String[] {
            "10110111", //183
            "11101101"  //237
        };

        public static final String[] bitString_Delimited_Spaces = new String[] {
            "1011 01  11", //183
            "111011   01"  //237
        };

        public static final String[] bitString_Delimited_Dashes = new String[] {
            "1011-01--11", //183
            "111011---01"  //237
        };

        public static final String[] bitString_Delimited_Underscores = new String[] {
            "1011_01__11", //183
            "111011___01"  //237
        };

        public static final String[] bitString_Delimited_Periods = new String[] {
            "1011.01..11", //183
            "111011...01"  //237
        };

        public static final String[] bitString_Delimited_Tabs = new String[] {
            "1011\t01\t\t11", //183
            "111011\t\t\t01"  //237
        };

        public static final String[] bitString_Delimited_All = new String[] {
            "__  10_-11\t0..1\t\t1. --1", //183
            " 1--1-_\t10__.-.11\t\t..01"  //237
        };

        public static final byte[] hexStringByteVals_Delimited_None = new byte[] {
            (byte) 0xAF, //175
            (byte) 0xF3, //243
            (byte) 0x26, //38
            (byte) 0x6E, //110
            (byte) 0xA1, //161
            (byte) 0x4E, //78
            (byte) 0x23, //35
            (byte) 0x01  //1
        };

        public static final String hexString_Delimited_None         = "AFF3266EA14E2301";
        public static final String hexString_Delimited_Spaces       = "AFF3 26  6EA14E   2301";
        public static final String hexString_Delimited_Dashes       = "AFF3-26--6EA14E---2301";
        public static final String hexString_Delimited_Underscores  = "AFF3_26__6EA14E___2301";
        public static final String hexString_Delimited_Periods      = "AFF3.26..6EA14E...2301";
        public static final String hexString_Delimited_Tabs         = "AFF3\t26\t\t6EA14E\t\t\t2301";
        public static final String hexString_Delimited_All          = "__ AF_-F3\t2..6\t\t6. --E  A14.\t_-_E2-.301";
    }
}
