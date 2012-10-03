/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/
package org.apache.commons.lang3;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.UUID;

import org.junit.Test;


/**
 * Unit tests {@link Conversion}.
 * 
 * @version $Id$
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
        try {
            Conversion.hexDigitToInt('G');
            fail("Thrown " + IllegalArgumentException.class.getName() + " expected");
        } catch (final IllegalArgumentException e) {
            // OK
        }
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
        try {
            Conversion.hexDigitMsb0ToInt('G');
            fail("Thrown " + IllegalArgumentException.class.getName() + " expected");
        } catch (final IllegalArgumentException e) {
            // OK
        }
    }

    /**
     * Tests {@link Conversion#hexDigitToBoolArray(char)}.
     */
    @Test
    public void testHexDigitToBoolArray() {
        assertBoolArrayEquals(
            new boolean[]{false, false, false, false}, Conversion.hexDigitToBoolArray('0'));
        assertBoolArrayEquals(
            new boolean[]{true, false, false, false}, Conversion.hexDigitToBoolArray('1'));
        assertBoolArrayEquals(
            new boolean[]{false, true, false, false}, Conversion.hexDigitToBoolArray('2'));
        assertBoolArrayEquals(
            new boolean[]{true, true, false, false}, Conversion.hexDigitToBoolArray('3'));
        assertBoolArrayEquals(
            new boolean[]{false, false, true, false}, Conversion.hexDigitToBoolArray('4'));
        assertBoolArrayEquals(
            new boolean[]{true, false, true, false}, Conversion.hexDigitToBoolArray('5'));
        assertBoolArrayEquals(
            new boolean[]{false, true, true, false}, Conversion.hexDigitToBoolArray('6'));
        assertBoolArrayEquals(
            new boolean[]{true, true, true, false}, Conversion.hexDigitToBoolArray('7'));
        assertBoolArrayEquals(
            new boolean[]{false, false, false, true}, Conversion.hexDigitToBoolArray('8'));
        assertBoolArrayEquals(
            new boolean[]{true, false, false, true}, Conversion.hexDigitToBoolArray('9'));
        assertBoolArrayEquals(
            new boolean[]{false, true, false, true}, Conversion.hexDigitToBoolArray('A'));
        assertBoolArrayEquals(
            new boolean[]{false, true, false, true}, Conversion.hexDigitToBoolArray('a'));
        assertBoolArrayEquals(
            new boolean[]{true, true, false, true}, Conversion.hexDigitToBoolArray('B'));
        assertBoolArrayEquals(
            new boolean[]{true, true, false, true}, Conversion.hexDigitToBoolArray('b'));
        assertBoolArrayEquals(
            new boolean[]{false, false, true, true}, Conversion.hexDigitToBoolArray('C'));
        assertBoolArrayEquals(
            new boolean[]{false, false, true, true}, Conversion.hexDigitToBoolArray('c'));
        assertBoolArrayEquals(
            new boolean[]{true, false, true, true}, Conversion.hexDigitToBoolArray('D'));
        assertBoolArrayEquals(
            new boolean[]{true, false, true, true}, Conversion.hexDigitToBoolArray('d'));
        assertBoolArrayEquals(
            new boolean[]{false, true, true, true}, Conversion.hexDigitToBoolArray('E'));
        assertBoolArrayEquals(
            new boolean[]{false, true, true, true}, Conversion.hexDigitToBoolArray('e'));
        assertBoolArrayEquals(
            new boolean[]{true, true, true, true}, Conversion.hexDigitToBoolArray('F'));
        assertBoolArrayEquals(
            new boolean[]{true, true, true, true}, Conversion.hexDigitToBoolArray('f'));
        try {
            Conversion.hexDigitToBoolArray('G');
            fail("Thrown " + IllegalArgumentException.class.getName() + " expected");
        } catch (final IllegalArgumentException e) {
            // OK
        }
    }

    /**
     * Tests {@link Conversion#hexDigitMsb0ToBoolArray(char)}.
     */
    @Test
    public void testHexDigitMsb0ToBoolArray() {
        assertBoolArrayEquals(
            new boolean[]{false, false, false, false}, Conversion.hexDigitMsb0ToBoolArray('0'));
        assertBoolArrayEquals(
            new boolean[]{false, false, false, true}, Conversion.hexDigitMsb0ToBoolArray('1'));
        assertBoolArrayEquals(
            new boolean[]{false, false, true, false}, Conversion.hexDigitMsb0ToBoolArray('2'));
        assertBoolArrayEquals(
            new boolean[]{false, false, true, true}, Conversion.hexDigitMsb0ToBoolArray('3'));
        assertBoolArrayEquals(
            new boolean[]{false, true, false, false}, Conversion.hexDigitMsb0ToBoolArray('4'));
        assertBoolArrayEquals(
            new boolean[]{false, true, false, true}, Conversion.hexDigitMsb0ToBoolArray('5'));
        assertBoolArrayEquals(
            new boolean[]{false, true, true, false}, Conversion.hexDigitMsb0ToBoolArray('6'));
        assertBoolArrayEquals(
            new boolean[]{false, true, true, true}, Conversion.hexDigitMsb0ToBoolArray('7'));
        assertBoolArrayEquals(
            new boolean[]{true, false, false, false}, Conversion.hexDigitMsb0ToBoolArray('8'));
        assertBoolArrayEquals(
            new boolean[]{true, false, false, true}, Conversion.hexDigitMsb0ToBoolArray('9'));
        assertBoolArrayEquals(
            new boolean[]{true, false, true, false}, Conversion.hexDigitMsb0ToBoolArray('A'));
        assertBoolArrayEquals(
            new boolean[]{true, false, true, false}, Conversion.hexDigitMsb0ToBoolArray('a'));
        assertBoolArrayEquals(
            new boolean[]{true, false, true, true}, Conversion.hexDigitMsb0ToBoolArray('B'));
        assertBoolArrayEquals(
            new boolean[]{true, false, true, true}, Conversion.hexDigitMsb0ToBoolArray('b'));
        assertBoolArrayEquals(
            new boolean[]{true, true, false, false}, Conversion.hexDigitMsb0ToBoolArray('C'));
        assertBoolArrayEquals(
            new boolean[]{true, true, false, false}, Conversion.hexDigitMsb0ToBoolArray('c'));
        assertBoolArrayEquals(
            new boolean[]{true, true, false, true}, Conversion.hexDigitMsb0ToBoolArray('D'));
        assertBoolArrayEquals(
            new boolean[]{true, true, false, true}, Conversion.hexDigitMsb0ToBoolArray('d'));
        assertBoolArrayEquals(
            new boolean[]{true, true, true, false}, Conversion.hexDigitMsb0ToBoolArray('E'));
        assertBoolArrayEquals(
            new boolean[]{true, true, true, false}, Conversion.hexDigitMsb0ToBoolArray('e'));
        assertBoolArrayEquals(
            new boolean[]{true, true, true, true}, Conversion.hexDigitMsb0ToBoolArray('F'));
        assertBoolArrayEquals(
            new boolean[]{true, true, true, true}, Conversion.hexDigitMsb0ToBoolArray('f'));
        try {
            Conversion.hexDigitMsb0ToBoolArray('G');
            fail("Thrown " + IllegalArgumentException.class.getName() + " expected");
        } catch (final IllegalArgumentException e) {
            // OK
        }
    }

    /**
     * Tests {@link Conversion#boolArrayToHexDigit(boolean[])}.
     */
    @Test
    public void testBoolArrayToHexDigit() {
        assertEquals(
            '0', Conversion.boolArrayToHexDigit(new boolean[]{false, false, false, false}));
        assertEquals(
            '1', Conversion.boolArrayToHexDigit(new boolean[]{true, false, false, false}));
        assertEquals(
            '2', Conversion.boolArrayToHexDigit(new boolean[]{false, true, false, false}));
        assertEquals(
            '3', Conversion.boolArrayToHexDigit(new boolean[]{true, true, false, false}));
        assertEquals(
            '4', Conversion.boolArrayToHexDigit(new boolean[]{false, false, true, false}));
        assertEquals(
            '5', Conversion.boolArrayToHexDigit(new boolean[]{true, false, true, false}));
        assertEquals(
            '6', Conversion.boolArrayToHexDigit(new boolean[]{false, true, true, false}));
        assertEquals(
            '7', Conversion.boolArrayToHexDigit(new boolean[]{true, true, true, false}));
        assertEquals(
            '8', Conversion.boolArrayToHexDigit(new boolean[]{false, false, false, true}));
        assertEquals(
            '9', Conversion.boolArrayToHexDigit(new boolean[]{true, false, false, true}));
        assertEquals(
            'a', Conversion.boolArrayToHexDigit(new boolean[]{false, true, false, true}));
        assertEquals(
            'b', Conversion.boolArrayToHexDigit(new boolean[]{true, true, false, true}));
        assertEquals(
            'c', Conversion.boolArrayToHexDigit(new boolean[]{false, false, true, true}));
        assertEquals(
            'd', Conversion.boolArrayToHexDigit(new boolean[]{true, false, true, true}));
        assertEquals(
            'e', Conversion.boolArrayToHexDigit(new boolean[]{false, true, true, true}));
        assertEquals('f', Conversion.boolArrayToHexDigit(new boolean[]{true, true, true, true}));
        assertEquals('1', Conversion.boolArrayToHexDigit(new boolean[]{true}));
        assertEquals(
            'f', Conversion.boolArrayToHexDigit(new boolean[]{true, true, true, true, true}));
        try {
            Conversion.boolArrayToHexDigit(new boolean[]{});
            fail("Thrown " + IllegalArgumentException.class.getName() + " expected");
        } catch (final IllegalArgumentException e) {
            // OK
        }
    }

    /**
     * Tests {@link Conversion#boolArrayBeMsb0ToHexDigit(boolean[], int)}.
     */
    @Test
    public void testBoolArrayToHexDigit_2args() {
        boolean[] shortArray = new boolean[]{false, true, true};
        assertEquals('6', Conversion.boolArrayToHexDigit(shortArray, 0));
        assertEquals('3', Conversion.boolArrayToHexDigit(shortArray, 1));
        assertEquals('1', Conversion.boolArrayToHexDigit(shortArray, 2));
        boolean[] longArray = new boolean[]{true, false, true, false, false, true, true};
        assertEquals('5', Conversion.boolArrayToHexDigit(longArray, 0));
        assertEquals('2', Conversion.boolArrayToHexDigit(longArray, 1));
        assertEquals('9', Conversion.boolArrayToHexDigit(longArray, 2));
        assertEquals('c', Conversion.boolArrayToHexDigit(longArray, 3));
        assertEquals('6', Conversion.boolArrayToHexDigit(longArray, 4));
        assertEquals('3', Conversion.boolArrayToHexDigit(longArray, 5));
        assertEquals('1', Conversion.boolArrayToHexDigit(longArray, 6));
    }

    /**
     * Tests {@link Conversion#boolArrayToHexDigitMsb0_4bits(boolean[])}.
     */
    @Test
    public void testBoolArrayToHexDigitMsb0_bits() {
        assertEquals(
            '0',
            Conversion.boolArrayToHexDigitMsb0_4bits(new boolean[]{false, false, false, false}));
        assertEquals(
            '1',
            Conversion.boolArrayToHexDigitMsb0_4bits(new boolean[]{false, false, false, true}));
        assertEquals(
            '2',
            Conversion.boolArrayToHexDigitMsb0_4bits(new boolean[]{false, false, true, false}));
        assertEquals(
            '3',
            Conversion.boolArrayToHexDigitMsb0_4bits(new boolean[]{false, false, true, true}));
        assertEquals(
            '4',
            Conversion.boolArrayToHexDigitMsb0_4bits(new boolean[]{false, true, false, false}));
        assertEquals(
            '5',
            Conversion.boolArrayToHexDigitMsb0_4bits(new boolean[]{false, true, false, true}));
        assertEquals(
            '6',
            Conversion.boolArrayToHexDigitMsb0_4bits(new boolean[]{false, true, true, false}));
        assertEquals(
            '7',
            Conversion.boolArrayToHexDigitMsb0_4bits(new boolean[]{false, true, true, true}));
        assertEquals(
            '8',
            Conversion.boolArrayToHexDigitMsb0_4bits(new boolean[]{true, false, false, false}));
        assertEquals(
            '9',
            Conversion.boolArrayToHexDigitMsb0_4bits(new boolean[]{true, false, false, true}));
        assertEquals(
            'a',
            Conversion.boolArrayToHexDigitMsb0_4bits(new boolean[]{true, false, true, false}));
        assertEquals(
            'b',
            Conversion.boolArrayToHexDigitMsb0_4bits(new boolean[]{true, false, true, true}));
        assertEquals(
            'c',
            Conversion.boolArrayToHexDigitMsb0_4bits(new boolean[]{true, true, false, false}));
        assertEquals(
            'd',
            Conversion.boolArrayToHexDigitMsb0_4bits(new boolean[]{true, true, false, true}));
        assertEquals(
            'e',
            Conversion.boolArrayToHexDigitMsb0_4bits(new boolean[]{true, true, true, false}));
        assertEquals(
            'f',
            Conversion.boolArrayToHexDigitMsb0_4bits(new boolean[]{true, true, true, true}));
        try {
            Conversion.boolArrayToHexDigitMsb0_4bits(new boolean[]{});
            fail("Thrown " + IllegalArgumentException.class.getName() + " expected");
        } catch (final IllegalArgumentException e) {
            // OK
        }
    }

    /**
     * Tests {@link Conversion#boolArrayToHexDigitMsb0_4bits(boolean[], int)}.
     */
    @Test
    public void testBoolArrayToHexDigitMsb0_4bits_2args() {
        // boolean[] shortArray = new boolean[]{true,true,false};
        // assertEquals('6', Conversion.BoolArrayToHexDigitMsb0(shortArray,0));
        // assertEquals('3', Conversion.BoolArrayToHexDigitMsb0(shortArray,1));
        // assertEquals('1', Conversion.BoolArrayToHexDigitMsb0(shortArray,2));
        boolean[] shortArray = new boolean[]{true, true, false, true};
        assertEquals('d', Conversion.boolArrayToHexDigitMsb0_4bits(shortArray, 0));
        boolean[] longArray = new boolean[]{true, false, true, false, false, true, true};
        assertEquals('a', Conversion.boolArrayToHexDigitMsb0_4bits(longArray, 0));
        assertEquals('4', Conversion.boolArrayToHexDigitMsb0_4bits(longArray, 1));
        assertEquals('9', Conversion.boolArrayToHexDigitMsb0_4bits(longArray, 2));
        assertEquals('3', Conversion.boolArrayToHexDigitMsb0_4bits(longArray, 3));
        // assertEquals('6', Conversion.BoolArrayToHexDigitMsb0(longArray,4));
        // assertEquals('3', Conversion.BoolArrayToHexDigitMsb0(longArray,5));
        // assertEquals('1', Conversion.BoolArrayToHexDigitMsb0(longArray,6));
        boolean[] maxLengthArray = new boolean[]{
            true, false, true, false, false, true, true, true};
        assertEquals('a', Conversion.boolArrayToHexDigitMsb0_4bits(maxLengthArray, 0));
        assertEquals('4', Conversion.boolArrayToHexDigitMsb0_4bits(maxLengthArray, 1));
        assertEquals('9', Conversion.boolArrayToHexDigitMsb0_4bits(maxLengthArray, 2));
        assertEquals('3', Conversion.boolArrayToHexDigitMsb0_4bits(maxLengthArray, 3));
        assertEquals('7', Conversion.boolArrayToHexDigitMsb0_4bits(maxLengthArray, 4));
        // assertEquals('7', Conversion.BoolArrayToHexDigitMsb0(longArray,5));
        // assertEquals('3', Conversion.BoolArrayToHexDigitMsb0(longArray,6));
        // assertEquals('1', Conversion.BoolArrayToHexDigitMsb0(longArray,7));
        boolean[] javaDocCheck = new boolean[]{
            true, false, false, true, true, false, true, false};
        assertEquals('d', Conversion.boolArrayToHexDigitMsb0_4bits(javaDocCheck, 3));

    }

    /**
     * Tests {@link Conversion#boolArrayToHexDigit(boolean[])}.
     */
    @Test
    public void testBoolArrayBeMsb0ToHexDigit() {
        assertEquals(
            '0',
            Conversion.boolArrayBeMsb0ToHexDigit(new boolean[]{false, false, false, false}));
        assertEquals(
            '1', Conversion.boolArrayBeMsb0ToHexDigit(new boolean[]{false, false, false, true}));
        assertEquals(
            '2', Conversion.boolArrayBeMsb0ToHexDigit(new boolean[]{false, false, true, false}));
        assertEquals(
            '3', Conversion.boolArrayBeMsb0ToHexDigit(new boolean[]{false, false, true, true}));
        assertEquals(
            '4', Conversion.boolArrayBeMsb0ToHexDigit(new boolean[]{false, true, false, false}));
        assertEquals(
            '5', Conversion.boolArrayBeMsb0ToHexDigit(new boolean[]{false, true, false, true}));
        assertEquals(
            '6', Conversion.boolArrayBeMsb0ToHexDigit(new boolean[]{false, true, true, false}));
        assertEquals(
            '7', Conversion.boolArrayBeMsb0ToHexDigit(new boolean[]{false, true, true, true}));
        assertEquals(
            '8', Conversion.boolArrayBeMsb0ToHexDigit(new boolean[]{true, false, false, false}));
        assertEquals(
            '9', Conversion.boolArrayBeMsb0ToHexDigit(new boolean[]{true, false, false, true}));
        assertEquals(
            'a', Conversion.boolArrayBeMsb0ToHexDigit(new boolean[]{true, false, true, false}));
        assertEquals(
            'b', Conversion.boolArrayBeMsb0ToHexDigit(new boolean[]{true, false, true, true}));
        assertEquals(
            'c', Conversion.boolArrayBeMsb0ToHexDigit(new boolean[]{true, true, false, false}));
        assertEquals(
            'd', Conversion.boolArrayBeMsb0ToHexDigit(new boolean[]{true, true, false, true}));
        assertEquals(
            'e', Conversion.boolArrayBeMsb0ToHexDigit(new boolean[]{true, true, true, false}));
        assertEquals(
            'f', Conversion.boolArrayBeMsb0ToHexDigit(new boolean[]{true, true, true, true}));
        assertEquals(
            '4',
            Conversion.boolArrayBeMsb0ToHexDigit(new boolean[]{
                true, false, false, false, false, false, false, false, false, false, false,
                false, false, true, false, false}));
        try {
            Conversion.boolArrayBeMsb0ToHexDigit(new boolean[]{});
            fail("Thrown " + IllegalArgumentException.class.getName() + " expected");
        } catch (final IllegalArgumentException e) {
            // OK
        }
    }

    /**
     * Tests {@link Conversion#boolArrayToHexDigit(boolean[], int)}.
     */
    @Test
    public void testBoolArrayBeMsb0ToHexDigit_2args() {
        assertEquals(
            '5',
            Conversion.boolArrayBeMsb0ToHexDigit(new boolean[]{
                true, false, false, false, false, false, false, false, false, false, false,
                true, false, true, false, false}, 2));

        boolean[] shortArray = new boolean[]{true, true, false};
        assertEquals('6', Conversion.boolArrayBeMsb0ToHexDigit(shortArray, 0));
        assertEquals('3', Conversion.boolArrayBeMsb0ToHexDigit(shortArray, 1));
        assertEquals('1', Conversion.boolArrayBeMsb0ToHexDigit(shortArray, 2));
        boolean[] shortArray2 = new boolean[]{true, true, true, false, false, true, false, true};
        assertEquals('5', Conversion.boolArrayBeMsb0ToHexDigit(shortArray2, 0));
        assertEquals('2', Conversion.boolArrayBeMsb0ToHexDigit(shortArray2, 1));
        assertEquals('9', Conversion.boolArrayBeMsb0ToHexDigit(shortArray2, 2));
        assertEquals('c', Conversion.boolArrayBeMsb0ToHexDigit(shortArray2, 3));
        assertEquals('e', Conversion.boolArrayBeMsb0ToHexDigit(shortArray2, 4));
        assertEquals('7', Conversion.boolArrayBeMsb0ToHexDigit(shortArray2, 5));
        assertEquals('3', Conversion.boolArrayBeMsb0ToHexDigit(shortArray2, 6));
        assertEquals('1', Conversion.boolArrayBeMsb0ToHexDigit(shortArray2, 7));
        boolean[] multiBytesArray = new boolean[]{
            true, true, false, false, true, false, true, false, true, true, true, false, false,
            true, false, true};
        assertEquals('5', Conversion.boolArrayBeMsb0ToHexDigit(multiBytesArray, 0));
        assertEquals('2', Conversion.boolArrayBeMsb0ToHexDigit(multiBytesArray, 1));
        assertEquals('9', Conversion.boolArrayBeMsb0ToHexDigit(multiBytesArray, 2));
        assertEquals('c', Conversion.boolArrayBeMsb0ToHexDigit(multiBytesArray, 3));
        assertEquals('e', Conversion.boolArrayBeMsb0ToHexDigit(multiBytesArray, 4));
        assertEquals('7', Conversion.boolArrayBeMsb0ToHexDigit(multiBytesArray, 5));
        assertEquals('b', Conversion.boolArrayBeMsb0ToHexDigit(multiBytesArray, 6));
        assertEquals('5', Conversion.boolArrayBeMsb0ToHexDigit(multiBytesArray, 7));

        assertEquals('a', Conversion.boolArrayBeMsb0ToHexDigit(multiBytesArray, 8));
        assertEquals('5', Conversion.boolArrayBeMsb0ToHexDigit(multiBytesArray, 9));
        assertEquals('2', Conversion.boolArrayBeMsb0ToHexDigit(multiBytesArray, 10));
        assertEquals('9', Conversion.boolArrayBeMsb0ToHexDigit(multiBytesArray, 11));
        assertEquals('c', Conversion.boolArrayBeMsb0ToHexDigit(multiBytesArray, 12));
        assertEquals('6', Conversion.boolArrayBeMsb0ToHexDigit(multiBytesArray, 13));
        assertEquals('3', Conversion.boolArrayBeMsb0ToHexDigit(multiBytesArray, 14));
        assertEquals('1', Conversion.boolArrayBeMsb0ToHexDigit(multiBytesArray, 15));

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
        try {
            Conversion.intToHexDigit(16);
            fail("Thrown " + IllegalArgumentException.class.getName() + " expected");
        } catch (final IllegalArgumentException e) {
            // OK
        }
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
        try {
            Conversion.intToHexDigitMsb0(16);
            fail("Thrown " + IllegalArgumentException.class.getName() + " expected");
        } catch (final IllegalArgumentException e) {
            // OK
        }
    }

    static String dbgPrint(boolean[] src) {
        StringBuilder sb = new StringBuilder();
        for (boolean e : src) {
            if (e) {
                sb.append("1,");
            } else {
                sb.append("0,");
            }
        }
        String out = sb.toString();
        return out.substring(0, out.length() - 1);
    }

    // org.junit.Assert(boolean[], boolean[]) does not exist in JUnit 4.2
    static void assertBoolArrayEquals(boolean[] expected, boolean[] actual) {
        assertEquals(expected.length, actual.length);
        for (int i = 0; i < expected.length; i++ ) {
            try {
                assertEquals(expected[i], actual[i]);
            } catch (Throwable e) {
                String msg = "Mismatch at index "
                    + i
                    + " between:\n"
                    + dbgPrint(expected)
                    + " and\n"
                    + dbgPrint(actual);
                fail(msg + "\n" + e.getMessage());
            }
        }
    }

    /**
     * Tests {@link Conversion#intArrayToLong(int[], int, long, int, int)}.
     */
    @Test
    public void testIntArrayToLong() {
        int[] src = new int[]{0xCDF1F0C1, 0x0F123456, 0x78000000};
        assertEquals(0x0000000000000000L, Conversion.intArrayToLong(src, 0, 0L, 0, 0));
        assertEquals(0x0000000000000000L, Conversion.intArrayToLong(src, 1, 0L, 0, 0));
        assertEquals(0x00000000CDF1F0C1L, Conversion.intArrayToLong(src, 0, 0L, 0, 1));
        assertEquals(0x0F123456CDF1F0C1L, Conversion.intArrayToLong(src, 0, 0L, 0, 2));
        assertEquals(0x000000000F123456L, Conversion.intArrayToLong(src, 1, 0L, 0, 1));
        assertEquals(
            0x123456789ABCDEF0L, Conversion.intArrayToLong(src, 0, 0x123456789ABCDEF0L, 0, 0));
        assertEquals(
            0x1234567878000000L, Conversion.intArrayToLong(src, 2, 0x123456789ABCDEF0L, 0, 1));
        // assertEquals(0x0F12345678000000L,Conversion.intsToLong(src,1,0x123456789ABCDEF0L,32,2));
    }

    /**
     * Tests {@link Conversion#shortArrayToLong(short[], int, long, int, int)}.
     */
    @Test
    public void testShortArrayToLong() {
        short[] src = new short[]{
            (short)0xCDF1, (short)0xF0C1, (short)0x0F12, (short)0x3456, (short)0x7800};
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
        byte[] src = new byte[]{
            (byte)0xCD, (byte)0xF1, (byte)0xF0, (byte)0xC1, (byte)0x0F, (byte)0x12, (byte)0x34,
            (byte)0x56, (byte)0x78};
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
        short[] src = new short[]{
            (short)0xCDF1, (short)0xF0C1, (short)0x0F12, (short)0x3456, (short)0x7800};
        assertEquals(0x00000000, Conversion.shortArrayToInt(src, 0, 0, 0, 0));
        assertEquals(0x0000CDF1, Conversion.shortArrayToInt(src, 0, 0, 0, 1));
        assertEquals(0xF0C1CDF1, Conversion.shortArrayToInt(src, 0, 0, 0, 2));
        assertEquals(0x0F12F0C1, Conversion.shortArrayToInt(src, 1, 0, 0, 2));
        assertEquals(0x12345678, Conversion.shortArrayToInt(src, 0, 0x12345678, 0, 0));
        assertEquals(0xCDF15678, Conversion.shortArrayToInt(src, 0, 0x12345678, 16, 1));
        // assertEquals(0x34567800,Conversion.ShortArrayToInt(src, 3, 0x12345678, 16, 2));
    }

    /**
     * Tests {@link Conversion#byteArrayToInt(byte[], int, int, int, int)}.
     */
    @Test
    public void testByteArrayToInt() {
        byte[] src = new byte[]{
            (byte)0xCD, (byte)0xF1, (byte)0xF0, (byte)0xC1, (byte)0x0F, (byte)0x12, (byte)0x34,
            (byte)0x56, (byte)0x78};
        assertEquals(0x00000000, Conversion.byteArrayToInt(src, 0, 0, 0, 0));
        assertEquals(0x000000CD, Conversion.byteArrayToInt(src, 0, 0, 0, 1));
        assertEquals(0xC1F0F1CD, Conversion.byteArrayToInt(src, 0, 0, 0, 4));
        assertEquals(0x0FC1F0F1, Conversion.byteArrayToInt(src, 1, 0, 0, 4));
        assertEquals(0x12345678, Conversion.byteArrayToInt(src, 0, 0x12345678, 0, 0));
        assertEquals(0xCD345678, Conversion.byteArrayToInt(src, 0, 0x12345678, 24, 1));
        // assertEquals(0x56341278,Conversion.ByteArrayToInt(src, 5, 0x01234567, 8, 4));
    }

    /**
     * Tests {@link Conversion#byteArrayToShort(byte[], int, short, int, int)}.
     */
    @Test
    public void testByteArrayToShort() {
        byte[] src = new byte[]{
            (byte)0xCD, (byte)0xF1, (byte)0xF0, (byte)0xC1, (byte)0x0F, (byte)0x12, (byte)0x34,
            (byte)0x56, (byte)0x78};
        assertEquals((short)0x0000, Conversion.byteArrayToShort(src, 0, (short)0, 0, 0));
        assertEquals((short)0x00CD, Conversion.byteArrayToShort(src, 0, (short)0, 0, 1));
        assertEquals((short)0xF1CD, Conversion.byteArrayToShort(src, 0, (short)0, 0, 2));
        assertEquals((short)0xF0F1, Conversion.byteArrayToShort(src, 1, (short)0, 0, 2));
        assertEquals((short)0x1234, Conversion.byteArrayToShort(src, 0, (short)0x1234, 0, 0));
        assertEquals((short)0xCD34, Conversion.byteArrayToShort(src, 0, (short)0x1234, 8, 1));
        // assertEquals((short)0x5678,Conversion.ByteArrayToShort(src, 7, (short) 0x0123, 8,
        // 2));
    }

    /**
     * Tests {@link Conversion#hexToLong(String, int, long, int, int)}.
     */
    @Test
    public void testHexToLong() {
        String src = "CDF1F0C10F12345678";
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
        String src = "CDF1F0C10F12345678";
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
        String src = "CDF1F0C10F12345678";
        assertEquals((short)0x0000, Conversion.hexToShort(src, 0, (short)0, 0, 0));
        assertEquals((short)0x000C, Conversion.hexToShort(src, 0, (short)0, 0, 1));
        assertEquals((short)0x1FDC, Conversion.hexToShort(src, 0, (short)0, 0, 4));
        assertEquals((short)0xF1FD, Conversion.hexToShort(src, 1, (short)0, 0, 4));
        assertEquals((short)0x1234, Conversion.hexToShort(src, 0, (short)0x1234, 0, 0));
        assertEquals((short)0x8764, Conversion.hexToShort(src, 15, (short)0x1234, 4, 3));
    }

    /**
     * Tests {@link Conversion#hexToByte(String, int, byte, int, int)}.
     */
    @Test
    public void testHexToByte() {
        String src = "CDF1F0C10F12345678";
        assertEquals((byte)0x00, Conversion.hexToByte(src, 0, (byte)0, 0, 0));
        assertEquals((byte)0x0C, Conversion.hexToByte(src, 0, (byte)0, 0, 1));
        assertEquals((byte)0xDC, Conversion.hexToByte(src, 0, (byte)0, 0, 2));
        assertEquals((byte)0xFD, Conversion.hexToByte(src, 1, (byte)0, 0, 2));
        assertEquals((byte)0x34, Conversion.hexToByte(src, 0, (byte)0x34, 0, 0));
        assertEquals((byte)0x84, Conversion.hexToByte(src, 17, (byte)0x34, 4, 1));
    }

    /**
     * Tests {@link Conversion#boolArrayToLong(boolean[], int, long, int, int)}.
     */
    @Test
    public void testBoolArrayToLong() {
        boolean[] src = new boolean[]{
            false, false, true, true, true, false, true, true, true, true, true, true, true,
            false, false, false, true, true, true, true, false, false, false, false, false,
            false, true, true, true, false, false, false, false, false, false, false, true,
            true, true, true, true, false, false, false, false, true, false, false, true, true,
            false, false, false, false, true, false, true, false, true, false, false, true,
            true, false, true, true, true, false, false, false, false, true};
        // conversion of "CDF1F0C10F12345678" by HexToBoolArray
        assertEquals(0x0000000000000000L, Conversion.boolArrayToLong(src, 0, 0L, 0, 0));
        assertEquals(0x000000000000000CL, Conversion.boolArrayToLong(src, 0, 0L, 0, 1 * 4));
        assertEquals(0x000000001C0F1FDCL, Conversion.boolArrayToLong(src, 0, 0L, 0, 8 * 4));
        assertEquals(0x0000000001C0F1FDL, Conversion.boolArrayToLong(src, 1 * 4, 0L, 0, 8 * 4));
        assertEquals(
            0x123456798ABCDEF0L, Conversion.boolArrayToLong(src, 0, 0x123456798ABCDEF0L, 0, 0));
        assertEquals(
            0x1234567876BCDEF0L,
            Conversion.boolArrayToLong(src, 15 * 4, 0x123456798ABCDEF0L, 24, 3 * 4));
    }

    /**
     * Tests {@link Conversion#boolArrayToInt(boolean[], int, int, int, int)}.
     */
    @Test
    public void testBoolArrayToInt() {
        boolean[] src = new boolean[]{
            false, false, true, true, true, false, true, true, true, true, true, true, true,
            false, false, false, true, true, true, true, false, false, false, false, false,
            false, true, true, true, false, false, false, false, false, false, false, true,
            true, true, true, true, false, false, false, false, true, false, false, true, true,
            false, false, false, false, true, false, true, false, true, false, false, true,
            true, false, true, true, true, false, false, false, false, true};
        // conversion of "CDF1F0C10F12345678" by HexToBoolArray
        assertEquals(0x00000000, Conversion.boolArrayToInt(src, 0 * 4, 0, 0, 0 * 4));
        assertEquals(0x0000000C, Conversion.boolArrayToInt(src, 0 * 4, 0, 0, 1 * 4));
        assertEquals(0x1C0F1FDC, Conversion.boolArrayToInt(src, 0 * 4, 0, 0, 8 * 4));
        assertEquals(0x01C0F1FD, Conversion.boolArrayToInt(src, 1 * 4, 0, 0, 8 * 4));
        assertEquals(0x12345679, Conversion.boolArrayToInt(src, 0 * 4, 0x12345679, 0, 0 * 4));
        assertEquals(0x87645679, Conversion.boolArrayToInt(src, 15 * 4, 0x12345679, 20, 3 * 4));
    }

    /**
     * Tests {@link Conversion#boolArrayToShort(boolean[], int, short, int, int)}.
     */
    @Test
    public void testBoolArrayToShort() {
        boolean[] src = new boolean[]{
            false, false, true, true, true, false, true, true, true, true, true, true, true,
            false, false, false, true, true, true, true, false, false, false, false, false,
            false, true, true, true, false, false, false, false, false, false, false, true,
            true, true, true, true, false, false, false, false, true, false, false, true, true,
            false, false, false, false, true, false, true, false, true, false, false, true,
            true, false, true, true, true, false, false, false, false, true};
        // conversion of "CDF1F0C10F12345678" by HexToBoolArray
        assertEquals((short)0x0000, Conversion.boolArrayToShort(src, 0 * 4, (short)0, 0, 0 * 4));
        assertEquals((short)0x000C, Conversion.boolArrayToShort(src, 0 * 4, (short)0, 0, 1 * 4));
        assertEquals((short)0x1FDC, Conversion.boolArrayToShort(src, 0 * 4, (short)0, 0, 4 * 4));
        assertEquals((short)0xF1FD, Conversion.boolArrayToShort(src, 1 * 4, (short)0, 0, 4 * 4));
        assertEquals(
            (short)0x1234, Conversion.boolArrayToShort(src, 0 * 4, (short)0x1234, 0, 0 * 4));
        assertEquals(
            (short)0x8764, Conversion.boolArrayToShort(src, 15 * 4, (short)0x1234, 4, 3 * 4));
    }

    /**
     * Tests {@link Conversion#boolArrayToByte(boolean[], int, byte, int, int)}.
     */
    @Test
    public void testBoolArrayToByte() {
        boolean[] src = new boolean[]{
            false, false, true, true, true, false, true, true, true, true, true, true, true,
            false, false, false, true, true, true, true, false, false, false, false, false,
            false, true, true, true, false, false, false, false, false, false, false, true,
            true, true, true, true, false, false, false, false, true, false, false, true, true,
            false, false, false, false, true, false, true, false, true, false, false, true,
            true, false, true, true, true, false, false, false, false, true};
        // conversion of "CDF1F0C10F12345678" by HexToBoolArray
        assertEquals((byte)0x00, Conversion.boolArrayToByte(src, 0 * 4, (byte)0, 0, 0 * 4));
        assertEquals((byte)0x0C, Conversion.boolArrayToByte(src, 0 * 4, (byte)0, 0, 1 * 4));
        assertEquals((byte)0xDC, Conversion.boolArrayToByte(src, 0 * 4, (byte)0, 0, 2 * 4));
        assertEquals((byte)0xFD, Conversion.boolArrayToByte(src, 1 * 4, (byte)0, 0, 2 * 4));
        assertEquals((byte)0x34, Conversion.boolArrayToByte(src, 0 * 4, (byte)0x34, 0, 0 * 4));
        assertEquals((byte)0x84, Conversion.boolArrayToByte(src, 17 * 4, (byte)0x34, 4, 1 * 4));
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
        // int[]{0x90ABCDEF,0x12345678,0x90ABCDEF,0x12345678},Conversion.longToIntArray(0x1234567890ABCDEFL,
        // 0,new int[]{-1,-1,-1,-1},0,4));//rejected by assertion
        // assertArrayEquals(new
        // int[]{0xFFFFFFFF,0x90ABCDEF,0x12345678,0x90ABCDEF},Conversion.longToIntArray(0x1234567890ABCDEFL,
        // 0,new int[]{-1,-1,-1,-1},1,3));
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
        // int[]{0x4855E6F7,0x091A2B3C,0x4855E6F7,0x091A2B3C},Conversion.longToIntArray(0x1234567890ABCDEFL,
        // 1,new int[]{-1,-1,-1,-1},0,4));//rejected by assertion
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
            new short[]{(short)0xFFFF, (short)0xFFFF, (short)0xFFFF, (short)0xFFFF},
            Conversion.longToShortArray(
                0x1234567890ABCDEFL, 0, new short[]{-1, -1, -1, -1}, 0, 0));
        assertArrayEquals(
            new short[]{(short)0xCDEF, (short)0xFFFF, (short)0xFFFF, (short)0xFFFF},
            Conversion.longToShortArray(
                0x1234567890ABCDEFL, 0, new short[]{-1, -1, -1, -1}, 0, 1));
        assertArrayEquals(
            new short[]{(short)0xCDEF, (short)0x90AB, (short)0xFFFF, (short)0xFFFF},
            Conversion.longToShortArray(
                0x1234567890ABCDEFL, 0, new short[]{-1, -1, -1, -1}, 0, 2));
        assertArrayEquals(
            new short[]{(short)0xCDEF, (short)0x90AB, (short)0x5678, (short)0xFFFF},
            Conversion.longToShortArray(
                0x1234567890ABCDEFL, 0, new short[]{-1, -1, -1, -1}, 0, 3));
        assertArrayEquals(
            new short[]{(short)0xCDEF, (short)0x90AB, (short)0x5678, (short)0x1234},
            Conversion.longToShortArray(
                0x1234567890ABCDEFL, 0, new short[]{-1, -1, -1, -1}, 0, 4));
        assertArrayEquals(
            new short[]{(short)0xFFFF, (short)0xCDEF, (short)0x90AB, (short)0x5678},
            Conversion.longToShortArray(
                0x1234567890ABCDEFL, 0, new short[]{-1, -1, -1, -1}, 1, 3));
        assertArrayEquals(
            new short[]{(short)0xFFFF, (short)0xFFFF, (short)0xCDEF, (short)0x90AB},
            Conversion.longToShortArray(
                0x1234567890ABCDEFL, 0, new short[]{-1, -1, -1, -1}, 2, 2));
        assertArrayEquals(
            new short[]{(short)0xFFFF, (short)0xFFFF, (short)0xCDEF, (short)0xFFFF},
            Conversion.longToShortArray(
                0x1234567890ABCDEFL, 0, new short[]{-1, -1, -1, -1}, 2, 1));
        assertArrayEquals(
            new short[]{(short)0xFFFF, (short)0xFFFF, (short)0xFFFF, (short)0xCDEF},
            Conversion.longToShortArray(
                0x1234567890ABCDEFL, 0, new short[]{-1, -1, -1, -1}, 3, 1));
        assertArrayEquals(
            new short[]{(short)0xFFFF, (short)0xFFFF, (short)0xE6F7, (short)0xFFFF},
            Conversion.longToShortArray(
                0x1234567890ABCDEFL, 1, new short[]{-1, -1, -1, -1}, 2, 1));
        assertArrayEquals(
            new short[]{(short)0xFFFF, (short)0xFFFF, (short)0xF37B, (short)0xFFFF},
            Conversion.longToShortArray(
                0x1234567890ABCDEFL, 2, new short[]{-1, -1, -1, -1}, 2, 1));
        assertArrayEquals(
            new short[]{(short)0xFFFF, (short)0xFFFF, (short)0x79BD, (short)0xFFFF},
            Conversion.longToShortArray(
                0x1234567890ABCDEFL, 3, new short[]{-1, -1, -1, -1}, 2, 1));
        assertArrayEquals(
            new short[]{(short)0xFFFF, (short)0xFFFF, (short)0xBCDE, (short)0xFFFF},
            Conversion.longToShortArray(
                0x1234567890ABCDEFL, 4, new short[]{-1, -1, -1, -1}, 2, 1));
        assertArrayEquals(
            new short[]{(short)0xE6F7, (short)0x4855, (short)0x2B3C, (short)0x091A},
            Conversion.longToShortArray(
                0x1234567890ABCDEFL, 1, new short[]{-1, -1, -1, -1}, 0, 4));
        assertArrayEquals(
            new short[]{(short)0x2B3C},
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
            new short[]{(short)0xFFFF, (short)0xFFFF, (short)0xFFFF, (short)0xFFFF},
            Conversion.intToShortArray(0x12345678, 0, new short[]{-1, -1, -1, -1}, 0, 0));
        assertArrayEquals(
            new short[]{(short)0x5678, (short)0xFFFF, (short)0xFFFF, (short)0xFFFF},
            Conversion.intToShortArray(0x12345678, 0, new short[]{-1, -1, -1, -1}, 0, 1));
        assertArrayEquals(
            new short[]{(short)0x5678, (short)0x1234, (short)0xFFFF, (short)0xFFFF},
            Conversion.intToShortArray(0x12345678, 0, new short[]{-1, -1, -1, -1}, 0, 2));
        // assertArrayEquals(new
        // short[]{(short)0x5678,(short)0x1234,(short)0x5678,(short)0xFFFF},Conversion.intToShortArray(0x12345678,
        // 0,new short[]{-1,-1,-1,-1},0,3));//rejected by assertion
        // assertArrayEquals(new
        // short[]{(short)0x5678,(short)0x1234,(short)0x5678,(short)0x1234},Conversion.intToShortArray(0x12345678,
        // 0,new short[]{-1,-1,-1,-1},0,4));
        // assertArrayEquals(new
        // short[]{(short)0xFFFF,(short)0x5678,(short)0x1234,(short)0x5678},Conversion.intToShortArray(0x12345678,
        // 0,new short[]{-1,-1,-1,-1},1,3));
        assertArrayEquals(
            new short[]{(short)0xFFFF, (short)0xFFFF, (short)0x5678, (short)0x1234},
            Conversion.intToShortArray(0x12345678, 0, new short[]{-1, -1, -1, -1}, 2, 2));
        assertArrayEquals(
            new short[]{(short)0xFFFF, (short)0xFFFF, (short)0x5678, (short)0xFFFF},
            Conversion.intToShortArray(0x12345678, 0, new short[]{-1, -1, -1, -1}, 2, 1));
        assertArrayEquals(
            new short[]{(short)0xFFFF, (short)0xFFFF, (short)0xFFFF, (short)0x5678},
            Conversion.intToShortArray(0x12345678, 0, new short[]{-1, -1, -1, -1}, 3, 1));
        assertArrayEquals(
            new short[]{(short)0xFFFF, (short)0xFFFF, (short)0x2B3C, (short)0xFFFF},
            Conversion.intToShortArray(0x12345678, 1, new short[]{-1, -1, -1, -1}, 2, 1));
        assertArrayEquals(
            new short[]{(short)0xFFFF, (short)0xFFFF, (short)0x159E, (short)0xFFFF},
            Conversion.intToShortArray(0x12345678, 2, new short[]{-1, -1, -1, -1}, 2, 1));
        assertArrayEquals(
            new short[]{(short)0xFFFF, (short)0xFFFF, (short)0x8ACF, (short)0xFFFF},
            Conversion.intToShortArray(0x12345678, 3, new short[]{-1, -1, -1, -1}, 2, 1));
        assertArrayEquals(
            new short[]{(short)0xFFFF, (short)0xFFFF, (short)0x4567, (short)0xFFFF},
            Conversion.intToShortArray(0x12345678, 4, new short[]{-1, -1, -1, -1}, 2, 1));
        // assertArrayEquals(new
        // short[]{(short)0xE6F7,(short)0x4855,(short)0x2B3C,(short)0x091A},Conversion.intToShortArray(0x12345678,
        // 1,new short[]{-1,-1,-1,-1},0,4));//rejected by assertion
        // assertArrayEquals(new
        // short[]{(short)0x2B3C},Conversion.intToShortArray(0x12345678,33,new
        // short[]{0},0,1));//rejected by assertion
        assertArrayEquals(
            new short[]{(short)0x091A},
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
                (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF,
                (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF},
            Conversion.longToByteArray(0x1234567890ABCDEFL, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 0, 0));
        assertArrayEquals(
            new byte[]{
                (byte)0xEF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF,
                (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF},
            Conversion.longToByteArray(0x1234567890ABCDEFL, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 0, 1));
        assertArrayEquals(
            new byte[]{
                (byte)0xEF, (byte)0xCD, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF,
                (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF},
            Conversion.longToByteArray(0x1234567890ABCDEFL, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 0, 2));
        assertArrayEquals(
            new byte[]{
                (byte)0xEF, (byte)0xCD, (byte)0xAB, (byte)0x90, (byte)0xFF, (byte)0xFF,
                (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF},
            Conversion.longToByteArray(0x1234567890ABCDEFL, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 0, 4));
        assertArrayEquals(
            new byte[]{
                (byte)0xEF, (byte)0xCD, (byte)0xAB, (byte)0x90, (byte)0x78, (byte)0x56,
                (byte)0x34, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF},
            Conversion.longToByteArray(0x1234567890ABCDEFL, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 0, 7));
        assertArrayEquals(
            new byte[]{
                (byte)0xEF, (byte)0xCD, (byte)0xAB, (byte)0x90, (byte)0x78, (byte)0x56,
                (byte)0x34, (byte)0x12, (byte)0xFF, (byte)0xFF, (byte)0xFF},
            Conversion.longToByteArray(0x1234567890ABCDEFL, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 0, 8));
        assertArrayEquals(
            new byte[]{
                (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xEF, (byte)0xFF, (byte)0xFF,
                (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF},
            Conversion.longToByteArray(0x1234567890ABCDEFL, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 3, 1));
        assertArrayEquals(
            new byte[]{
                (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xEF, (byte)0xCD, (byte)0xFF,
                (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF},
            Conversion.longToByteArray(0x1234567890ABCDEFL, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 3, 2));
        assertArrayEquals(
            new byte[]{
                (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xEF, (byte)0xCD, (byte)0xAB,
                (byte)0x90, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF},
            Conversion.longToByteArray(0x1234567890ABCDEFL, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 3, 4));
        assertArrayEquals(
            new byte[]{
                (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xEF, (byte)0xCD, (byte)0xAB,
                (byte)0x90, (byte)0x78, (byte)0x56, (byte)0x34, (byte)0xFF},
            Conversion.longToByteArray(0x1234567890ABCDEFL, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 3, 7));
        assertArrayEquals(
            new byte[]{
                (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xEF, (byte)0xCD, (byte)0xAB,
                (byte)0x90, (byte)0x78, (byte)0x56, (byte)0x34, (byte)0x12},
            Conversion.longToByteArray(0x1234567890ABCDEFL, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 3, 8));
        assertArrayEquals(
            new byte[]{
                (byte)0xF7, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF,
                (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF},
            Conversion.longToByteArray(0x1234567890ABCDEFL, 1, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 0, 1));
        assertArrayEquals(
            new byte[]{
                (byte)0x7B, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF,
                (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF},
            Conversion.longToByteArray(0x1234567890ABCDEFL, 2, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 0, 1));
        assertArrayEquals(
            new byte[]{
                (byte)0xFF, (byte)0x00, (byte)0xFF, (byte)0x6F, (byte)0x5E, (byte)0x85,
                (byte)0xC4, (byte)0xB3, (byte)0xA2, (byte)0x91, (byte)0x00},
            Conversion.longToByteArray(0x1234567890ABCDEFL, 5, new byte[]{
                -1, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 3, 8));
        // assertArrayEquals(new
        // byte[]{(byte)0xFF,(byte)0x00,(byte)0xFF,(byte)0x5E,(byte)0x85,(byte)0xC4,(byte)0xB3,(byte)0xA2,(byte)0x91,(byte)0x00,(byte)0x00},Conversion.longToByteArray(0x1234567890ABCDEFL,13,new
        // byte[]{-1, 0,-1,-1,-1,-1,-1,-1,-1,-1,-1},3,8));//rejected by assertion
        assertArrayEquals(
            new byte[]{
                (byte)0xFF, (byte)0x00, (byte)0xFF, (byte)0x5E, (byte)0x85, (byte)0xC4,
                (byte)0xB3, (byte)0xA2, (byte)0x91, (byte)0x00, (byte)0xFF},
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
                (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF,
                (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF},
            Conversion.intToByteArray(0x90ABCDEF, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 0, 0));
        assertArrayEquals(
            new byte[]{
                (byte)0xEF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF,
                (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF},
            Conversion.intToByteArray(0x90ABCDEF, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 0, 1));
        assertArrayEquals(
            new byte[]{
                (byte)0xEF, (byte)0xCD, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF,
                (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF},
            Conversion.intToByteArray(0x90ABCDEF, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 0, 2));
        assertArrayEquals(
            new byte[]{
                (byte)0xEF, (byte)0xCD, (byte)0xAB, (byte)0x90, (byte)0xFF, (byte)0xFF,
                (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF},
            Conversion.intToByteArray(0x90ABCDEF, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 0, 4));
        assertArrayEquals(
            new byte[]{
                (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xEF, (byte)0xFF, (byte)0xFF,
                (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF},
            Conversion.intToByteArray(0x90ABCDEF, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 3, 1));
        assertArrayEquals(
            new byte[]{
                (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xEF, (byte)0xCD, (byte)0xFF,
                (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF},
            Conversion.intToByteArray(0x90ABCDEF, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 3, 2));
        assertArrayEquals(
            new byte[]{
                (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xEF, (byte)0xCD, (byte)0xAB,
                (byte)0x90, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF},
            Conversion.intToByteArray(0x90ABCDEF, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 3, 4));
        assertArrayEquals(
            new byte[]{
                (byte)0xF7, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF,
                (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF},
            Conversion.intToByteArray(0x90ABCDEF, 1, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 0, 1));
        assertArrayEquals(
            new byte[]{
                (byte)0x7B, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF,
                (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF},
            Conversion.intToByteArray(0x90ABCDEF, 2, new byte[]{
                -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 0, 1));
        assertArrayEquals(
            new byte[]{
                (byte)0xFF, (byte)0x00, (byte)0xFF, (byte)0x6F, (byte)0x5E, (byte)0x85,
                (byte)0xFC, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF},
            Conversion.intToByteArray(0x90ABCDEF, 5, new byte[]{
                -1, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 3, 4));
        // assertArrayEquals(new
        // byte[]{(byte)0xFF,(byte)0x00,(byte)0xFF,(byte)0x5E,(byte)0x85,(byte)0xFC,(byte)0x00,(byte)0xFF,(byte)0xFF,(byte)0xFF,(byte)0xFF},Conversion.intToByteArray(0x90ABCDEF,13,new
        // byte[]{-1, 0,-1,-1,-1,-1,-1,-1,-1,-1,-1},3,4));//rejected by assertion
        assertArrayEquals(
            new byte[]{
                (byte)0xFF, (byte)0x00, (byte)0xFF, (byte)0x5E, (byte)0x85, (byte)0xFC,
                (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF},
            Conversion.intToByteArray(0x90ABCDEF, 13, new byte[]{
                -1, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 3, 3));
    }

    /**
     * Tests {@link Conversion#shortToByteArray(short, int, byte[], int, int)}.
     */
    @Test
    public void testShortToByteArray() {
        assertArrayEquals(
            new byte[]{}, Conversion.shortToByteArray((short)0x0000, 0, new byte[]{}, 0, 0));
        assertArrayEquals(
            new byte[]{}, Conversion.shortToByteArray((short)0x0000, 100, new byte[]{}, 0, 0));
        assertArrayEquals(
            new byte[]{}, Conversion.shortToByteArray((short)0x0000, 0, new byte[]{}, 100, 0));
        assertArrayEquals(
            new byte[]{
                (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF,
                (byte)0xFF}, Conversion.shortToByteArray((short)0xCDEF, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1}, 0, 0));
        assertArrayEquals(
            new byte[]{
                (byte)0xEF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF,
                (byte)0xFF}, Conversion.shortToByteArray((short)0xCDEF, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1}, 0, 1));
        assertArrayEquals(
            new byte[]{
                (byte)0xEF, (byte)0xCD, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF,
                (byte)0xFF}, Conversion.shortToByteArray((short)0xCDEF, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1}, 0, 2));
        assertArrayEquals(
            new byte[]{
                (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xEF, (byte)0xFF, (byte)0xFF,
                (byte)0xFF}, Conversion.shortToByteArray((short)0xCDEF, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1}, 3, 1));
        assertArrayEquals(
            new byte[]{
                (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xEF, (byte)0xCD, (byte)0xFF,
                (byte)0xFF}, Conversion.shortToByteArray((short)0xCDEF, 0, new byte[]{
                -1, -1, -1, -1, -1, -1, -1}, 3, 2));
        assertArrayEquals(
            new byte[]{
                (byte)0xF7, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF,
                (byte)0xFF}, Conversion.shortToByteArray((short)0xCDEF, 1, new byte[]{
                -1, -1, -1, -1, -1, -1, -1}, 0, 1));
        assertArrayEquals(
            new byte[]{
                (byte)0x7B, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF, (byte)0xFF,
                (byte)0xFF}, Conversion.shortToByteArray((short)0xCDEF, 2, new byte[]{
                -1, -1, -1, -1, -1, -1, -1}, 0, 1));
        assertArrayEquals(
            new byte[]{
                (byte)0xFF, (byte)0x00, (byte)0xFF, (byte)0x6F, (byte)0xFE, (byte)0xFF,
                (byte)0xFF}, Conversion.shortToByteArray((short)0xCDEF, 5, new byte[]{
                -1, 0, -1, -1, -1, -1, -1}, 3, 2));
        // assertArrayEquals(new
        // byte[]{(byte)0xFF,(byte)0x00,(byte)0xFF,(byte)0x5E,(byte)0xFF,(byte)0xFF,(byte)0xFF},Conversion.shortToByteArray((short)0xCDEF,13,new
        // byte[]{-1, 0,-1,-1,-1,-1,-1},3,2));//rejected by assertion
        assertArrayEquals(
            new byte[]{
                (byte)0xFF, (byte)0x00, (byte)0xFF, (byte)0xFE, (byte)0xFF, (byte)0xFF,
                (byte)0xFF}, Conversion.shortToByteArray((short)0xCDEF, 13, new byte[]{
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
        // assertEquals("ffffffffffffffffffffffff",Conversion.longToHex(0x1234567890ABCDEFL,4,"ffffffffffffffffffffffff",3,16));//rejected
        // by assertion
        assertEquals(
            "fffedcba0987654321ffffff",
            Conversion.longToHex(0x1234567890ABCDEFL, 4, "ffffffffffffffffffffffff", 3, 15));
        assertEquals(
            "fedcba0987654321", Conversion.longToHex(0x1234567890ABCDEFL, 0, "", 0, 16));
        try {
            Conversion.longToHex(0x1234567890ABCDEFL, 0, "", 1, 8);
            fail("Thrown " + StringIndexOutOfBoundsException.class.getName() + " expected");
        } catch (final StringIndexOutOfBoundsException e) {
            // OK
        }
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
        // assertEquals("ffffffffffffffffffffffff",Conversion.intToHex(0x90ABCDEF,
        // 4,"ffffffffffffffffffffffff",3,8));//rejected by assertion
        assertEquals(
            "fffedcba09ffffffffffffff",
            Conversion.intToHex(0x90ABCDEF, 4, "ffffffffffffffffffffffff", 3, 7));
        assertEquals("fedcba09", Conversion.intToHex(0x90ABCDEF, 0, "", 0, 8));
        try {
            Conversion.intToHex(0x90ABCDEF, 0, "", 1, 8);
            fail("Thrown " + StringIndexOutOfBoundsException.class.getName() + " expected");
        } catch (final StringIndexOutOfBoundsException e) {
            // OK
        }
    }

    /**
     * Tests {@link Conversion#shortToHex(short, int, String, int, int)}.
     */
    @Test
    public void testShortToHex() {
        assertEquals("", Conversion.shortToHex((short)0x0000, 0, "", 0, 0));
        assertEquals("", Conversion.shortToHex((short)0x0000, 100, "", 0, 0));
        assertEquals("", Conversion.shortToHex((short)0x0000, 0, "", 100, 0));
        assertEquals(
            "ffffffffffffffffffffffff",
            Conversion.shortToHex((short)0xCDEF, 0, "ffffffffffffffffffffffff", 0, 0));
        assertEquals(
            "3fffffffffffffffffffffff",
            Conversion.shortToHex((short)0xCDE3, 0, "ffffffffffffffffffffffff", 0, 1));
        assertEquals(
            "feffffffffffffffffffffff",
            Conversion.shortToHex((short)0xCDEF, 0, "ffffffffffffffffffffffff", 0, 2));
        assertEquals(
            "fedfffffffffffffffffffff",
            Conversion.shortToHex((short)0xCDEF, 0, "ffffffffffffffffffffffff", 0, 3));
        assertEquals(
            "fedcffffffffffffffffffff",
            Conversion.shortToHex((short)0xCDEF, 0, "ffffffffffffffffffffffff", 0, 4));
        assertEquals(
            "fff3ffffffffffffffffffff",
            Conversion.shortToHex((short)0xCDE3, 0, "ffffffffffffffffffffffff", 3, 1));
        assertEquals(
            "ffffefffffffffffffffffff",
            Conversion.shortToHex((short)0xCDEF, 0, "ffffffffffffffffffffffff", 3, 2));
        assertEquals(
            "7fffffffffffffffffffffff",
            Conversion.shortToHex((short)0xCDEF, 1, "ffffffffffffffffffffffff", 0, 1));
        assertEquals(
            "bfffffffffffffffffffffff",
            Conversion.shortToHex((short)0xCDEF, 2, "ffffffffffffffffffffffff", 0, 1));
        assertEquals(
            "fffdb9ffffffffffffffffff",
            Conversion.shortToHex((short)0xCDEF, 3, "ffffffffffffffffffffffff", 3, 4));
        // assertEquals("ffffffffffffffffffffffff",Conversion.shortToHex((short)0xCDEF,
        // 4,"ffffffffffffffffffffffff",3,4));//rejected by assertion
        assertEquals(
            "fffedcffffffffffffffffff",
            Conversion.shortToHex((short)0xCDEF, 4, "ffffffffffffffffffffffff", 3, 3));
        assertEquals("fedc", Conversion.shortToHex((short)0xCDEF, 0, "", 0, 4));
        try {
            Conversion.shortToHex((short)0xCDEF, 0, "", 1, 4);
            fail("Thrown " + StringIndexOutOfBoundsException.class.getName() + " expected");
        } catch (final StringIndexOutOfBoundsException e) {
            // OK
        }
    }

    /**
     * Tests {@link Conversion#byteToHex(byte, int, String, int, int)}.
     */
    @Test
    public void testByteToHex() {
        assertEquals("", Conversion.byteToHex((byte)0x00, 0, "", 0, 0));
        assertEquals("", Conversion.byteToHex((byte)0x00, 100, "", 0, 0));
        assertEquals("", Conversion.byteToHex((byte)0x00, 0, "", 100, 0));
        assertEquals("00000", Conversion.byteToHex((byte)0xEF, 0, "00000", 0, 0));
        assertEquals("f0000", Conversion.byteToHex((byte)0xEF, 0, "00000", 0, 1));
        assertEquals("fe000", Conversion.byteToHex((byte)0xEF, 0, "00000", 0, 2));
        assertEquals("000f0", Conversion.byteToHex((byte)0xEF, 0, "00000", 3, 1));
        assertEquals("000fe", Conversion.byteToHex((byte)0xEF, 0, "00000", 3, 2));
        assertEquals("70000", Conversion.byteToHex((byte)0xEF, 1, "00000", 0, 1));
        assertEquals("b0000", Conversion.byteToHex((byte)0xEF, 2, "00000", 0, 1));
        assertEquals("000df", Conversion.byteToHex((byte)0xEF, 3, "00000", 3, 2));
        // assertEquals("00000",Conversion.byteToHex((byte)0xEF, 4,"00000",3,2));//rejected by
        // assertion
        assertEquals("000e0", Conversion.byteToHex((byte)0xEF, 4, "00000", 3, 1));
        assertEquals("fe", Conversion.byteToHex((byte)0xEF, 0, "", 0, 2));
        try {
            Conversion.byteToHex((byte)0xEF, 0, "", 1, 2);
            fail("Thrown " + StringIndexOutOfBoundsException.class.getName() + " expected");
        } catch (final StringIndexOutOfBoundsException e) {
            // OK
        }
    }

    /**
     * Tests {@link Conversion#longToBoolArray(long, int, boolean[], int, int)}.
     */
    @Test
    public void testLongToBoolArray() {
        assertBoolArrayEquals(
            new boolean[]{},
            Conversion.longToBoolArray(0x0000000000000000L, 0, new boolean[]{}, 0, 0));
        assertBoolArrayEquals(
            new boolean[]{},
            Conversion.longToBoolArray(0x0000000000000000L, 100, new boolean[]{}, 0, 0));
        assertBoolArrayEquals(
            new boolean[]{},
            Conversion.longToBoolArray(0x0000000000000000L, 0, new boolean[]{}, 100, 0));
        assertBoolArrayEquals(
            new boolean[69],
            Conversion.longToBoolArray(0x1234567890ABCDEFL, 0, new boolean[69], 0, 0));

        assertBoolArrayEquals(
            new boolean[]{
                true, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false},
            Conversion.longToBoolArray(0x1234567890ABCDEFL, 0, new boolean[69], 0, 1));
        assertBoolArrayEquals(
            new boolean[]{
                true, true, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false},
            Conversion.longToBoolArray(0x1234567890ABCDEFL, 0, new boolean[69], 0, 2));
        assertBoolArrayEquals(
            new boolean[]{
                true, true, true, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false},
            Conversion.longToBoolArray(0x1234567890ABCDEFL, 0, new boolean[69], 0, 3));
        assertBoolArrayEquals(
            new boolean[]{
                true, true, true, true, false, true, true, true, true, false, true, true,
                false, false, true, true, true, true, false, true, false, true, false, true,
                false, false, false, false, true, false, false, true, false, false, false,
                true, true, true, true, false, false, true, true, false, true, false, true,
                false, false, false, true, false, true, true, false, false, false, true, false,
                false, true, false, false, false, false, false, false, false, false},
            Conversion.longToBoolArray(0x1234567890ABCDEFL, 0, new boolean[69], 0, 63));
        assertBoolArrayEquals(
            new boolean[]{
                true, true, true, true, false, true, true, true, true, false, true, true,
                false, false, true, true, true, true, false, true, false, true, false, true,
                false, false, false, false, true, false, false, true, false, false, false,
                true, true, true, true, false, false, true, true, false, true, false, true,
                false, false, false, true, false, true, true, false, false, false, true, false,
                false, true, false, false, false, false, false, false, false, false},
            Conversion.longToBoolArray(0x1234567890ABCDEFL, 0, new boolean[69], 0, 64));
        assertBoolArrayEquals(
            new boolean[]{
                false, false, true, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false,
                false, false, false},
            Conversion.longToBoolArray(0x1234567890ABCDEFL, 0, new boolean[69], 2, 1));
        assertBoolArrayEquals(
            new boolean[]{
                false, false, true, true, true, true, false, true, true, true, true, false,
                true, true, false, false, true, true, true, true, false, true, false, true,
                false, true, false, false, false, false, true, false, false, true, false,
                false, false, true, true, true, true, false, false, true, true, false, true,
                false, true, false, false, false, true, false, true, true, false, false, false,
                true, false, false, true, false, false, false, false, false, false},
            Conversion.longToBoolArray(0x1234567890ABCDEFL, 0, new boolean[69], 2, 64));
        assertBoolArrayEquals(
            new boolean[]{
                true, true, true, false, true, true, true, true, false, true, true, false,
                false, true, true, true, true, false, true, false, true, false, true, false,
                false, false, false, true, false, false, true, false, false, false, true, true,
                true, true, false, false, true, true, false, true, false, true, false, false,
                false, true, false, true, true, false, false, false, true, false, false, true,
                false, false, false, false, false, false, false, false, false},
            Conversion.longToBoolArray(0x1234567890ABCDEFL, 1, new boolean[69], 0, 63));
        assertBoolArrayEquals(
            new boolean[]{
                true, true, false, true, true, true, true, false, true, true, false, false,
                true, true, true, true, false, true, false, true, false, true, false, false,
                false, false, true, false, false, true, false, false, false, true, true, true,
                true, false, false, true, true, false, true, false, true, false, false, false,
                true, false, true, true, false, false, false, true, false, false, true, false,
                false, false, false, false, false, false, false, false, false},
            Conversion.longToBoolArray(0x1234567890ABCDEFL, 2, new boolean[69], 0, 62));

        // assertBoolArrayEquals(new boolean[]{false,false,false, true, true, false, true, true,
        // true, true, false, true, true, false, false, true, true, true, true, false, true,
        // false, true, false, true, false, false, false, false, true, false, false, true,
        // false, false, false, true, true, true, true, false, false, true, true, false, true,
        // false, true, false, false, false, true, false, true, true, false, false, false, true,
        // false, false, true, false, false, false
        // ,false,false,false,false},Conversion.longToBoolArray(0x1234567890ABCDEFL, 2,new
        // boolean[69], 3, 63));//rejected by assertion
        assertBoolArrayEquals(
            new boolean[]{
                false, false, false, true, true, false, true, true, true, true, false, true,
                true, false, false, true, true, true, true, false, true, false, true, false,
                true, false, false, false, false, true, false, false, true, false, false,
                false, true, true, true, true, false, false, true, true, false, true, false,
                true, false, false, false, true, false, true, true, false, false, false, true,
                false, false, true, false, false, false, false, false, false, false},
            Conversion.longToBoolArray(0x1234567890ABCDEFL, 2, new boolean[69], 3, 62));
    }

    /**
     * Tests {@link Conversion#intToBoolArray(int, int, boolean[], int, int)}.
     */
    @Test
    public void testIntToBoolArray() {
        assertBoolArrayEquals(
            new boolean[]{}, Conversion.intToBoolArray(0x00000000, 0, new boolean[]{}, 0, 0));
        assertBoolArrayEquals(
            new boolean[]{}, Conversion.intToBoolArray(0x00000000, 100, new boolean[]{}, 0, 0));
        assertBoolArrayEquals(
            new boolean[]{}, Conversion.intToBoolArray(0x00000000, 0, new boolean[]{}, 100, 0));
        assertBoolArrayEquals(
            new boolean[69], Conversion.intToBoolArray(0x90ABCDEF, 0, new boolean[69], 0, 0));
        assertBoolArrayEquals(new boolean[]{
            true, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false}, Conversion.intToBoolArray(0x90ABCDEF, 0, new boolean[37], 0, 1));
        assertBoolArrayEquals(new boolean[]{
            true, true, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false}, Conversion.intToBoolArray(0x90ABCDEF, 0, new boolean[37], 0, 2));
        assertBoolArrayEquals(new boolean[]{
            true, true, true, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false}, Conversion.intToBoolArray(0x90ABCDEF, 0, new boolean[37], 0, 3));
        assertBoolArrayEquals(
            new boolean[]{
                true, true, true, true, false, true, true, true, true, false, true, true,
                false, false, true, true, true, true, false, true, false, true, false, true,
                false, false, false, false, true, false, false, false, false, false, false,
                false, false}, Conversion.intToBoolArray(0x90ABCDEF, 0, new boolean[37], 0, 31));
        assertBoolArrayEquals(
            new boolean[]{
                true, true, true, true, false, true, true, true, true, false, true, true,
                false, false, true, true, true, true, false, true, false, true, false, true,
                false, false, false, false, true, false, false, true, false, false, false,
                false, false}, Conversion.intToBoolArray(0x90ABCDEF, 0, new boolean[37], 0, 32));
        assertBoolArrayEquals(new boolean[]{
            false, false, true, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false, false, false, false, false, false, false, false,
            false}, Conversion.intToBoolArray(0x90ABCDEF, 0, new boolean[37], 2, 1));
        assertBoolArrayEquals(
            new boolean[]{
                false, false, true, true, true, true, false, true, true, true, true, false,
                true, true, false, false, true, true, true, true, false, true, false, true,
                false, true, false, false, false, false, true, false, false, true, false,
                false, false}, Conversion.intToBoolArray(0x90ABCDEF, 0, new boolean[37], 2, 32));
        assertBoolArrayEquals(
            new boolean[]{
                true, true, true, false, true, true, true, true, false, true, true, false,
                false, true, true, true, true, false, true, false, true, false, true, false,
                false, false, false, true, false, false, true, false, false, false, false,
                false, false}, Conversion.intToBoolArray(0x90ABCDEF, 1, new boolean[37], 0, 31));
        assertBoolArrayEquals(
            new boolean[]{
                true, true, false, true, true, true, true, false, true, true, false, false,
                true, true, true, true, false, true, false, true, false, true, false, false,
                false, false, true, false, false, true, false, false, false, false, false,
                false, false}, Conversion.intToBoolArray(0x90ABCDEF, 2, new boolean[37], 0, 30));
        // assertBoolArrayEquals(new boolean[]{false, false, false, true, true, false, true,
        // true,
        // true, true, false, true, true, false, false, true, true, true, true, false, true,
        // false, true, false, true, false, false, false, false, true, false, false, false,
        // false, false, false, false},Conversion.intToBoolArray(0x90ABCDEF, 2,new boolean[37],
        // 3,31));//rejected by assertion
        assertBoolArrayEquals(
            new boolean[]{
                false, false, false, true, true, false, true, true, true, true, false, true,
                true, false, false, true, true, true, true, false, true, false, true, false,
                true, false, false, false, false, true, false, false, true, false, false,
                false, false}, Conversion.intToBoolArray(0x90ABCDEF, 2, new boolean[37], 3, 30));
    }

    /**
     * Tests {@link Conversion#shortToBoolArray(short, int, boolean[], int, int)}.
     */
    @Test
    public void testShortToBoolArray() {
        assertBoolArrayEquals(
            new boolean[]{},
            Conversion.shortToBoolArray((short)0x0000, 0, new boolean[]{}, 0, 0));
        assertBoolArrayEquals(
            new boolean[]{},
            Conversion.shortToBoolArray((short)0x0000, 100, new boolean[]{}, 0, 0));
        assertBoolArrayEquals(
            new boolean[]{},
            Conversion.shortToBoolArray((short)0x0000, 0, new boolean[]{}, 100, 0));
        assertBoolArrayEquals(
            new boolean[69],
            Conversion.shortToBoolArray((short)0xCDEF, 0, new boolean[69], 0, 0));
        assertBoolArrayEquals(
            new boolean[]{
                true, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false},
            Conversion.shortToBoolArray((short)0xCDEF, 0, new boolean[21], 0, 1));
        assertBoolArrayEquals(
            new boolean[]{
                true, true, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false},
            Conversion.shortToBoolArray((short)0xCDEF, 0, new boolean[21], 0, 2));
        assertBoolArrayEquals(
            new boolean[]{
                true, true, true, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false},
            Conversion.shortToBoolArray((short)0xCDEF, 0, new boolean[21], 0, 3));
        assertBoolArrayEquals(
            new boolean[]{
                true, true, true, true, false, true, true, true, true, false, true, true,
                false, false, true, false, false, false, false, false, false},
            Conversion.shortToBoolArray((short)0xCDEF, 0, new boolean[21], 0, 15));
        assertBoolArrayEquals(
            new boolean[]{
                true, true, true, true, false, true, true, true, true, false, true, true,
                false, false, true, true, false, false, false, false, false},
            Conversion.shortToBoolArray((short)0xCDEF, 0, new boolean[21], 0, 16));
        assertBoolArrayEquals(
            new boolean[]{
                false, false, true, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false},
            Conversion.shortToBoolArray((short)0xCDEF, 0, new boolean[21], 2, 1));
        assertBoolArrayEquals(
            new boolean[]{
                false, false, true, true, true, true, false, true, true, true, true, false,
                true, true, false, false, true, true, false, false, false},
            Conversion.shortToBoolArray((short)0xCDEF, 0, new boolean[21], 2, 16));
        assertBoolArrayEquals(
            new boolean[]{
                true, true, true, false, true, true, true, true, false, true, true, false,
                false, true, true, false, false, false, false, false, false},
            Conversion.shortToBoolArray((short)0xCDEF, 1, new boolean[21], 0, 15));
        assertBoolArrayEquals(
            new boolean[]{
                true, true, false, true, true, true, true, false, true, true, false, false,
                true, true, false, false, false, false, false, false, false},
            Conversion.shortToBoolArray((short)0xCDEF, 2, new boolean[21], 0, 14));
        // assertArrayEquals(new boolean[]{false, false, false, true, true, false, true, true,
        // true, true, false, true, true, false, false, true, false, false, false, false,
        // false},Conversion.shortToBoolArray((short)0xCDEF, 2,new boolean[21],
        // 3,15));//rejected by
        // assertion
        assertBoolArrayEquals(
            new boolean[]{
                false, false, false, true, true, false, true, true, true, true, false, true,
                true, false, false, true, true, false, false, false, false},
            Conversion.shortToBoolArray((short)0xCDEF, 2, new boolean[21], 3, 14));
    }

    /**
     * Tests {@link Conversion#byteToBoolArray(byte, int, boolean[], int, int)}.
     */
    @Test
    public void testByteToBoolArray() {
        assertBoolArrayEquals(
            new boolean[]{}, Conversion.byteToBoolArray((byte)0x00, 0, new boolean[]{}, 0, 0));
        assertBoolArrayEquals(
            new boolean[]{}, Conversion.byteToBoolArray((byte)0x00, 100, new boolean[]{}, 0, 0));
        assertBoolArrayEquals(
            new boolean[]{}, Conversion.byteToBoolArray((byte)0x00, 0, new boolean[]{}, 100, 0));
        assertBoolArrayEquals(
            new boolean[69], Conversion.byteToBoolArray((byte)0xEF, 0, new boolean[69], 0, 0));
        assertBoolArrayEquals(new boolean[]{
            true, false, false, false, false, false, false, false, false, false, false, false,
            false}, Conversion.byteToBoolArray((byte)0x95, 0, new boolean[13], 0, 1));
        assertBoolArrayEquals(new boolean[]{
            true, false, false, false, false, false, false, false, false, false, false, false,
            false}, Conversion.byteToBoolArray((byte)0x95, 0, new boolean[13], 0, 2));
        assertBoolArrayEquals(new boolean[]{
            true, false, true, false, false, false, false, false, false, false, false, false,
            false}, Conversion.byteToBoolArray((byte)0x95, 0, new boolean[13], 0, 3));
        assertBoolArrayEquals(new boolean[]{
            true, false, true, false, true, false, false, false, false, false, false, false,
            false}, Conversion.byteToBoolArray((byte)0x95, 0, new boolean[13], 0, 7));
        assertBoolArrayEquals(new boolean[]{
            true, false, true, false, true, false, false, true, false, false, false, false,
            false}, Conversion.byteToBoolArray((byte)0x95, 0, new boolean[13], 0, 8));
        assertBoolArrayEquals(new boolean[]{
            false, false, true, false, false, false, false, false, false, false, false, false,
            false}, Conversion.byteToBoolArray((byte)0x95, 0, new boolean[13], 2, 1));
        assertBoolArrayEquals(new boolean[]{
            false, false, true, false, true, false, true, false, false, true, false, false,
            false}, Conversion.byteToBoolArray((byte)0x95, 0, new boolean[13], 2, 8));
        assertBoolArrayEquals(new boolean[]{
            false, true, false, true, false, false, true, false, false, false, false, false,
            false}, Conversion.byteToBoolArray((byte)0x95, 1, new boolean[13], 0, 7));
        assertBoolArrayEquals(new boolean[]{
            true, false, true, false, false, true, false, false, false, false, false, false,
            false}, Conversion.byteToBoolArray((byte)0x95, 2, new boolean[13], 0, 6));
        // assertArrayEquals(new boolean[]{false, false, false, true, true, false, true, true,
        // false, false, false, false, false},Conversion.byteToBoolArray((byte)0x95, 2,new
        // boolean[13], 3, 7));//rejected by assertion
        assertBoolArrayEquals(new boolean[]{
            false, false, false, true, false, true, false, false, true, false, false, false,
            false}, Conversion.byteToBoolArray((byte)0x95, 2, new boolean[13], 3, 6));
    }

    /**
     * Tests {@link Conversion#uuidToByteArray(UUID, byte[], int, int)}.
     */
    @Test
    public void testUuidToByteArray() {
        assertArrayEquals(new byte[]{
            (byte)0xff, (byte)0xff, (byte)0xff, (byte)0xff, (byte)0xff, (byte)0xff, (byte)0xff,
            (byte)0xff, (byte)0xff, (byte)0xff, (byte)0xff, (byte)0xff, (byte)0xff, (byte)0xff,
            (byte)0xff, (byte)0xff}, Conversion.uuidToByteArray(new UUID(
            0xFFFFFFFFFFFFFFFFL, 0xFFFFFFFFFFFFFFFFL), new byte[16], 0, 16));
        assertArrayEquals(new byte[]{
            (byte)0x88, (byte)0x99, (byte)0xaa, (byte)0xbb, (byte)0xcc, (byte)0xdd, (byte)0xee,
            (byte)0xff, (byte)0x00, (byte)0x11, (byte)0x22, (byte)0x33, (byte)0x44, (byte)0x55,
            (byte)0x66, (byte)0x77}, Conversion.uuidToByteArray(new UUID(
            0xFFEEDDCCBBAA9988L, 0x7766554433221100L), new byte[16], 0, 16));
        assertArrayEquals(new byte[]{
            (byte)0x00, (byte)0x00, (byte)0x00, (byte)0x00, (byte)0x88, (byte)0x99, (byte)0xaa,
            (byte)0xbb, (byte)0xcc, (byte)0xdd, (byte)0xee, (byte)0xff, (byte)0x00, (byte)0x00,
            (byte)0x00, (byte)0x00}, Conversion.uuidToByteArray(new UUID(
            0xFFEEDDCCBBAA9988L, 0x7766554433221100L), new byte[16], 4, 8));
        assertArrayEquals(new byte[]{
            (byte)0x00, (byte)0x00, (byte)0x88, (byte)0x99, (byte)0xaa, (byte)0xbb, (byte)0xcc,
            (byte)0xdd, (byte)0xee, (byte)0xff, (byte)0x00, (byte)0x11, (byte)0x22, (byte)0x33,
            (byte)0x00, (byte)0x00}, Conversion.uuidToByteArray(new UUID(
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
                (byte)0xff, (byte)0xff, (byte)0xff, (byte)0xff, (byte)0xff, (byte)0xff,
                (byte)0xff, (byte)0xff, (byte)0xff, (byte)0xff, (byte)0xff, (byte)0xff,
                (byte)0xff, (byte)0xff, (byte)0xff, (byte)0xff}, 0));
        assertEquals(
            new UUID(0xFFEEDDCCBBAA9988L, 0x7766554433221100L),
            Conversion.byteArrayToUuid(new byte[]{
                (byte)0x88, (byte)0x99, (byte)0xaa, (byte)0xbb, (byte)0xcc, (byte)0xdd,
                (byte)0xee, (byte)0xff, (byte)0x00, (byte)0x11, (byte)0x22, (byte)0x33,
                (byte)0x44, (byte)0x55, (byte)0x66, (byte)0x77}, 0));
        assertEquals(
            new UUID(0xFFEEDDCCBBAA9988L, 0x7766554433221100L),
            Conversion.byteArrayToUuid(new byte[]{
                0, 0, (byte)0x88, (byte)0x99, (byte)0xaa, (byte)0xbb, (byte)0xcc, (byte)0xdd,
                (byte)0xee, (byte)0xff, (byte)0x00, (byte)0x11, (byte)0x22, (byte)0x33,
                (byte)0x44, (byte)0x55, (byte)0x66, (byte)0x77}, 2));
    }
}
