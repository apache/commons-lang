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

import java.util.UUID;


/**
 * Static methods to convert a type into another, with endianness and bit ordering awareness.
 *
 * <p>
 * The methods names follow a naming rule:<br>
 * {@code <source type>[source endianness][source bit ordering]To<destination type>[destination endianness][destination bit ordering]}
 * </p>
 * <p>
 * Source/destination type fields is one of the following:
 * </p>
 * <ul>
 * <li>binary: an array of booleans</li>
 * <li>byte or byteArray</li>
 * <li>int or intArray</li>
 * <li>long or longArray</li>
 * <li>hex: a String containing hexadecimal digits (lowercase in destination)</li>
 * <li>hexDigit: a Char containing a hexadecimal digit (lowercase in destination)</li>
 * <li>uuid</li>
 * </ul>
 * <p>
 * Endianness field: little endian is the default, in this case the field is absent. In case of
 * big endian, the field is "Be".<br> Bit ordering: Lsb0 is the default, in this case the field
 * is absent. In case of Msb0, the field is "Msb0".
 * </p>
 * <p>
 * Example: intBeMsb0ToHex convert an int with big endian byte order and Msb0 bit order into its
 * hexadecimal string representation
 * </p>
 * <p>
 * Most of the methods provide only default encoding for destination, this limits the number of
 * ways to do one thing. Unless you are dealing with data from/to outside of the JVM platform,
 * you should not need to use "Be" and "Msb0" methods.
 * </p>
 * <p>
 * Development status: work on going, only a part of the little endian, Lsb0 methods implemented
 * so far.
 * </p>
 *
 * @since 3.2
 */

public class Conversion {

    private static final boolean[] TTTT = {true, true, true, true};
    private static final boolean[] FTTT = {false, true, true, true};
    private static final boolean[] TFTT = {true, false, true, true};
    private static final boolean[] FFTT = {false, false, true, true};
    private static final boolean[] TTFT = {true, true, false, true};
    private static final boolean[] FTFT = {false, true, false, true};
    private static final boolean[] TFFT = {true, false, false, true};
    private static final boolean[] FFFT = {false, false, false, true};
    private static final boolean[] TTTF = {true, true, true, false};
    private static final boolean[] FTTF = {false, true, true, false};
    private static final boolean[] TFTF = {true, false, true, false};
    private static final boolean[] FFTF = {false, false, true, false};
    private static final boolean[] TTFF = {true, true, false, false};
    private static final boolean[] FTFF = {false, true, false, false};
    private static final boolean[] TFFF = {true, false, false, false};
    private static final boolean[] FFFF = {false, false, false, false};

    /**
     * Converts a hexadecimal digit into an int using the default (Lsb0) bit ordering.
     *
     * <p>
     * '1' is converted to 1
     * </p>
     *
     * @param hexDigit the hexadecimal digit to convert
     * @return an int equals to {@code hexDigit}
     * @throws IllegalArgumentException if {@code hexDigit} is not a hexadecimal digit
     */
    public static int hexDigitToInt(final char hexDigit) {
        final int digit = Character.digit(hexDigit, 16);
        if (digit < 0) {
            throw new IllegalArgumentException("Cannot interpret '" + hexDigit + "' as a hexadecimal digit");
        }
        return digit;
    }

    /**
     * Converts a hexadecimal digit into an int using the Msb0 bit ordering.
     *
     * <p>
     * '1' is converted to 8
     * </p>
     *
     * @param hexDigit the hexadecimal digit to convert
     * @return an int equals to {@code hexDigit}
     * @throws IllegalArgumentException if {@code hexDigit} is not a hexadecimal digit
     */
    public static int hexDigitMsb0ToInt(final char hexDigit) {
        switch (hexDigit) {
        case '0':
            return 0x0;
        case '1':
            return 0x8;
        case '2':
            return 0x4;
        case '3':
            return 0xC;
        case '4':
            return 0x2;
        case '5':
            return 0xA;
        case '6':
            return 0x6;
        case '7':
            return 0xE;
        case '8':
            return 0x1;
        case '9':
            return 0x9;
        case 'a':// fall through
        case 'A':
            return 0x5;
        case 'b':// fall through
        case 'B':
            return 0xD;
        case 'c':// fall through
        case 'C':
            return 0x3;
        case 'd':// fall through
        case 'D':
            return 0xB;
        case 'e':// fall through
        case 'E':
            return 0x7;
        case 'f':// fall through
        case 'F':
            return 0xF;
        default:
            throw new IllegalArgumentException("Cannot interpret '" + hexDigit + "' as a hexadecimal digit");
        }
    }

    /**
     * Converts a hexadecimal digit into binary (represented as boolean array) using the default
     * (Lsb0) bit ordering.
     *
     * <p>
     * '1' is converted as follow: (1, 0, 0, 0)
     * </p>
     *
     * @param hexDigit the hexadecimal digit to convert
     * @return a boolean array with the binary representation of {@code hexDigit}
     * @throws IllegalArgumentException if {@code hexDigit} is not a hexadecimal digit
     */
    public static boolean[] hexDigitToBinary(final char hexDigit) {
        switch (hexDigit) {
        case '0':
            return FFFF.clone();
        case '1':
            return TFFF.clone();
        case '2':
            return FTFF.clone();
        case '3':
            return TTFF.clone();
        case '4':
            return FFTF.clone();
        case '5':
            return TFTF.clone();
        case '6':
            return FTTF.clone();
        case '7':
            return TTTF.clone();
        case '8':
            return FFFT.clone();
        case '9':
            return TFFT.clone();
        case 'a':// fall through
        case 'A':
            return FTFT.clone();
        case 'b':// fall through
        case 'B':
            return TTFT.clone();
        case 'c':// fall through
        case 'C':
            return FFTT.clone();
        case 'd':// fall through
        case 'D':
            return TFTT.clone();
        case 'e':// fall through
        case 'E':
            return FTTT.clone();
        case 'f':// fall through
        case 'F':
            return TTTT.clone();
        default:
            throw new IllegalArgumentException("Cannot interpret '" + hexDigit + "' as a hexadecimal digit");
        }
    }

    /**
     * Converts a hexadecimal digit into binary (represented as boolean array) using the Msb0
     * bit ordering.
     *
     * <p>
     * '1' is converted as follow: (0, 0, 0, 1)
     * </p>
     *
     * @param hexDigit the hexadecimal digit to convert
     * @return a boolean array with the binary representation of {@code hexDigit}
     * @throws IllegalArgumentException if {@code hexDigit} is not a hexadecimal digit
     */
    public static boolean[] hexDigitMsb0ToBinary(final char hexDigit) {
        switch (hexDigit) {
        case '0':
            return FFFF.clone();
        case '1':
            return FFFT.clone();
        case '2':
            return FFTF.clone();
        case '3':
            return FFTT.clone();
        case '4':
            return FTFF.clone();
        case '5':
            return FTFT.clone();
        case '6':
            return FTTF.clone();
        case '7':
            return FTTT.clone();
        case '8':
            return TFFF.clone();
        case '9':
            return TFFT.clone();
        case 'a':// fall through
        case 'A':
            return TFTF.clone();
        case 'b':// fall through
        case 'B':
            return TFTT.clone();
        case 'c':// fall through
        case 'C':
            return TTFF.clone();
        case 'd':// fall through
        case 'D':
            return TTFT.clone();
        case 'e':// fall through
        case 'E':
            return TTTF.clone();
        case 'f':// fall through
        case 'F':
            return TTTT.clone();
        default:
            throw new IllegalArgumentException("Cannot interpret '" + hexDigit + "' as a hexadecimal digit");
        }
    }

    /**
     * Converts binary (represented as boolean array) to a hexadecimal digit using the default
     * (Lsb0) bit ordering.
     *
     * <p>
     * (1, 0, 0, 0) is converted as follow: '1'
     * </p>
     *
     * @param src the binary to convert
     * @return a hexadecimal digit representing the selected bits
     * @throws IllegalArgumentException if {@code src} is empty
     * @throws NullPointerException if {@code src} is {@code null}
     */
    public static char binaryToHexDigit(final boolean[] src) {
        return binaryToHexDigit(src, 0);
    }

    /**
     * Converts binary (represented as boolean array) to a hexadecimal digit using the default
     * (Lsb0) bit ordering.
     *
     * <p>
     * (1, 0, 0, 0) is converted as follow: '1'
     * </p>
     *
     * @param src the binary to convert
     * @param srcPos the position of the lsb to start the conversion
     * @return a hexadecimal digit representing the selected bits
     * @throws IllegalArgumentException if {@code src} is empty
     * @throws NullPointerException if {@code src} is {@code null}
     */
    public static char binaryToHexDigit(final boolean[] src, final int srcPos) {
        if (src.length == 0) {
            throw new IllegalArgumentException("Cannot convert an empty array.");
        }
        if (src.length > srcPos + 3 && src[srcPos + 3]) {
            if (src[srcPos + 2]) {
                if (src[srcPos + 1]) {
                    return src[srcPos] ? 'f' : 'e';
                }
                return src[srcPos] ? 'd' : 'c';
            }
            if (src[srcPos + 1]) {
                return src[srcPos] ? 'b' : 'a';
            }
            return src[srcPos] ? '9' : '8';
        }
        if (src.length > srcPos + 2 && src[srcPos + 2]) {
            if (src[srcPos + 1]) {
                return src[srcPos] ? '7' : '6';
            }
            return src[srcPos] ? '5' : '4';
        }
        if (src.length > srcPos + 1 && src[srcPos + 1]) {
            return src[srcPos] ? '3' : '2';
        }
        return src[srcPos] ? '1' : '0';
    }

    /**
     * Converts binary (represented as boolean array) to a hexadecimal digit using the Msb0 bit
     * ordering.
     *
     * <p>
     * (1, 0, 0, 0) is converted as follow: '8'
     * </p>
     *
     * @param src the binary to convert
     * @return a hexadecimal digit representing the selected bits
     * @throws IllegalArgumentException if {@code src} is empty, {@code src.length < 4} or
     *             {@code src.length > 8}
     * @throws NullPointerException if {@code src} is {@code null}
     */
    public static char binaryToHexDigitMsb0_4bits(final boolean[] src) {
        return binaryToHexDigitMsb0_4bits(src, 0);
    }

    /**
     * Converts binary (represented as boolean array) to a hexadecimal digit using the Msb0 bit
     * ordering.
     *
     * <p>
     * (1, 0, 0, 0) is converted as follow: '8' (1, 0, 0, 1, 1, 0, 1, 0) with srcPos = 3 is converted
     * to 'D'
     * </p>
     *
     * @param src the binary to convert
     * @param srcPos the position of the lsb to start the conversion
     * @return a hexadecimal digit representing the selected bits
     * @throws IllegalArgumentException if {@code src} is empty, {@code src.length > 8} or
     *             {@code src.length - srcPos < 4}
     * @throws NullPointerException if {@code src} is {@code null}
     */
    public static char binaryToHexDigitMsb0_4bits(final boolean[] src, final int srcPos) {
        if (src.length > 8) {
            throw new IllegalArgumentException("src.length>8: src.length=" + src.length);
        }
        if (src.length - srcPos < 4) {
            throw new IllegalArgumentException("src.length-srcPos<4: src.length=" + src.length + ", srcPos=" + srcPos);
        }
        if (src[srcPos + 3]) {
            if (src[srcPos + 2]) {
                if (src[srcPos + 1]) {
                    return src[srcPos] ? 'f' : '7';
                }
                return src[srcPos] ? 'b' : '3';
            }
            if (src[srcPos + 1]) {
                return src[srcPos] ? 'd' : '5';
            }
            return src[srcPos] ? '9' : '1';
        }
        if (src[srcPos + 2]) {
            if (src[srcPos + 1]) {
                return src[srcPos] ? 'e' : '6';
            }
            return src[srcPos] ? 'a' : '2';
        }
        if (src[srcPos + 1]) {
            return src[srcPos] ? 'c' : '4';
        }
        return src[srcPos] ? '8' : '0';
    }

    /**
     * Converts the first 4 bits of a binary (represented as boolean array) in big endian Msb0
     * bit ordering to a hexadecimal digit.
     *
     * <p>
     * (1, 0, 0, 0) is converted as follow: '8' (1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0) is converted
     * to '4'
     * </p>
     *
     * @param src the binary to convert
     * @return a hexadecimal digit representing the selected bits
     * @throws IllegalArgumentException if {@code src} is empty
     * @throws NullPointerException if {@code src} is {@code null}
     */
    public static char binaryBeMsb0ToHexDigit(final boolean[] src) {
        return binaryBeMsb0ToHexDigit(src, 0);
    }

    /**
     * Converts a binary (represented as boolean array) in big endian Msb0 bit ordering to a
     * hexadecimal digit.
     *
     * <p>
     * (1, 0, 0, 0) with srcPos = 0 is converted as follow: '8' (1, 0, 0, 0, 0, 0, 0, 0,
     * 0, 0, 0, 1, 0, 1, 0, 0) with srcPos = 2 is converted to '5'
     * </p>
     *
     * @param src the binary to convert
     * @param srcPos the position of the lsb to start the conversion
     * @return a hexadecimal digit representing the selected bits
     * @throws IllegalArgumentException if {@code src} is empty
     * @throws NullPointerException if {@code src} is {@code null}
     * @throws IndexOutOfBoundsException if {@code srcPos} is outside the array.
     */
    public static char binaryBeMsb0ToHexDigit(final boolean[] src, final int srcPos) {
        // JDK 9: Objects.checkIndex(int index, int length)
        if (Integer.compareUnsigned(srcPos, src.length) >= 0) {
            // Throw the correct exception
            if (src.length == 0) {
                throw new IllegalArgumentException("Cannot convert an empty array.");
            }
            throw new IndexOutOfBoundsException(srcPos + " is not within array length " + src.length);
        }
        // Little-endian bit 0 position
        final int pos = src.length - 1 - srcPos;
        if (3 <= pos && src[pos - 3]) {
            if (src[pos - 2]) {
                if (src[pos - 1]) {
                    return src[pos] ? 'f' : 'e';
                }
                return src[pos] ? 'd' : 'c';
            }
            if (src[pos - 1]) {
                return src[pos] ? 'b' : 'a';
            }
            return src[pos] ? '9' : '8';
        }
        if (2 <= pos && src[pos - 2]) {
            if (src[pos - 1]) {
                return src[pos] ? '7' : '6';
            }
            return src[pos] ? '5' : '4';
        }
        if (1 <= pos && src[pos - 1]) {
            return src[pos] ? '3' : '2';
        }
        return src[pos] ? '1' : '0';
    }

    /**
     * Converts the 4 lsb of an int to a hexadecimal digit.
     *
     * <p>
     * 0 returns '0'
     * </p>
     * <p>
     * 1 returns '1'
     * </p>
     * <p>
     * 10 returns 'A' and so on...
     * </p>
     *
     * @param nibble the 4 bits to convert
     * @return a hexadecimal digit representing the 4 lsb of {@code nibble}
     * @throws IllegalArgumentException if {@code nibble < 0} or {@code nibble > 15}
     */
    public static char intToHexDigit(final int nibble) {
        final char c = Character.forDigit(nibble, 16);
        if (c == Character.MIN_VALUE) {
            throw new IllegalArgumentException("nibble value not between 0 and 15: " + nibble);
        }
        return c;
    }

    /**
     * Converts the 4 lsb of an int to a hexadecimal digit encoded using the Msb0 bit ordering.
     *
     * <p>
     * 0 returns '0'
     * </p>
     * <p>
     * 1 returns '8'
     * </p>
     * <p>
     * 10 returns '5' and so on...
     * </p>
     *
     * @param nibble the 4 bits to convert
     * @return a hexadecimal digit representing the 4 lsb of {@code nibble}
     * @throws IllegalArgumentException if {@code nibble < 0} or {@code nibble > 15}
     */
    public static char intToHexDigitMsb0(final int nibble) {
        switch (nibble) {
        case 0x0:
            return '0';
        case 0x1:
            return '8';
        case 0x2:
            return '4';
        case 0x3:
            return 'c';
        case 0x4:
            return '2';
        case 0x5:
            return 'a';
        case 0x6:
            return '6';
        case 0x7:
            return 'e';
        case 0x8:
            return '1';
        case 0x9:
            return '9';
        case 0xA:
            return '5';
        case 0xB:
            return 'd';
        case 0xC:
            return '3';
        case 0xD:
            return 'b';
        case 0xE:
            return '7';
        case 0xF:
            return 'f';
        default:
            throw new IllegalArgumentException("nibble value not between 0 and 15: " + nibble);
        }
    }

    /**
     * Converts an array of int into a long using the default (little endian, Lsb0) byte and bit
     * ordering.
     *
     * @param src the int array to convert
     * @param srcPos the position in {@code src}, in int unit, from where to start the
     *            conversion
     * @param dstInit initial value of the destination long
     * @param dstPos the position of the lsb, in bits, in the result long
     * @param nInts the number of ints to convert
     * @return a long containing the selected bits
     * @throws IllegalArgumentException if {@code (nInts-1)*32+dstPos >= 64}
     * @throws NullPointerException if {@code src} is {@code null}
     * @throws ArrayIndexOutOfBoundsException if {@code srcPos + nInts > src.length}
     */
    public static long intArrayToLong(final int[] src, final int srcPos, final long dstInit, final int dstPos,
            final int nInts) {
        if (src.length == 0 && srcPos == 0 || 0 == nInts) {
            return dstInit;
        }
        if ((nInts - 1) * 32 + dstPos >= 64) {
            throw new IllegalArgumentException("(nInts-1)*32+dstPos is greater or equal to than 64");
        }
        long out = dstInit;
        for (int i = 0; i < nInts; i++) {
            final int shift = i * 32 + dstPos;
            final long bits = (0xffffffffL & src[i + srcPos]) << shift;
            final long mask = 0xffffffffL << shift;
            out = (out & ~mask) | bits;
        }
        return out;
    }

    /**
     * Converts an array of short into a long using the default (little endian, Lsb0) byte and
     * bit ordering.
     *
     * @param src the short array to convert
     * @param srcPos the position in {@code src}, in short unit, from where to start the
     *            conversion
     * @param dstInit initial value of the destination long
     * @param dstPos the position of the lsb, in bits, in the result long
     * @param nShorts the number of shorts to convert
     * @return a long containing the selected bits
     * @throws NullPointerException if {@code src} is {@code null}
     * @throws IllegalArgumentException if {@code (nShorts-1)*16+dstPos >= 64}
     * @throws ArrayIndexOutOfBoundsException if {@code srcPos + nShorts > src.length}
     */
    public static long shortArrayToLong(final short[] src, final int srcPos, final long dstInit, final int dstPos,
            final int nShorts) {
        if (src.length == 0 && srcPos == 0 || 0 == nShorts) {
            return dstInit;
        }
        if ((nShorts - 1) * 16 + dstPos >= 64) {
            throw new IllegalArgumentException("(nShorts-1)*16+dstPos is greater or equal to than 64");
        }
        long out = dstInit;
        for (int i = 0; i < nShorts; i++) {
            final int shift = i * 16 + dstPos;
            final long bits = (0xffffL & src[i + srcPos]) << shift;
            final long mask = 0xffffL << shift;
            out = (out & ~mask) | bits;
        }
        return out;
    }

    /**
     * Converts an array of short into an int using the default (little endian, Lsb0) byte and
     * bit ordering.
     *
     * @param src the short array to convert
     * @param srcPos the position in {@code src}, in short unit, from where to start the
     *            conversion
     * @param dstInit initial value of the destination int
     * @param dstPos the position of the lsb, in bits, in the result int
     * @param nShorts the number of shorts to convert
     * @return an int containing the selected bits
     * @throws NullPointerException if {@code src} is {@code null}
     * @throws IllegalArgumentException if {@code (nShorts-1)*16+dstPos >= 32}
     * @throws ArrayIndexOutOfBoundsException if {@code srcPos + nShorts > src.length}
     */
    public static int shortArrayToInt(final short[] src, final int srcPos, final int dstInit, final int dstPos,
            final int nShorts) {
        if (src.length == 0 && srcPos == 0 || 0 == nShorts) {
            return dstInit;
        }
        if ((nShorts - 1) * 16 + dstPos >= 32) {
            throw new IllegalArgumentException("(nShorts-1)*16+dstPos is greater or equal to than 32");
        }
        int out = dstInit;
        for (int i = 0; i < nShorts; i++) {
            final int shift = i * 16 + dstPos;
            final int bits = (0xffff & src[i + srcPos]) << shift;
            final int mask = 0xffff << shift;
            out = (out & ~mask) | bits;
        }
        return out;
    }

    /**
     * Converts an array of byte into a long using the default (little endian, Lsb0) byte and
     * bit ordering.
     *
     * @param src the byte array to convert
     * @param srcPos the position in {@code src}, in byte unit, from where to start the
     *            conversion
     * @param dstInit initial value of the destination long
     * @param dstPos the position of the lsb, in bits, in the result long
     * @param nBytes the number of bytes to convert
     * @return a long containing the selected bits
     * @throws NullPointerException if {@code src} is {@code null}
     * @throws IllegalArgumentException if {@code (nBytes-1)*8+dstPos >= 64}
     * @throws ArrayIndexOutOfBoundsException if {@code srcPos + nBytes > src.length}
     */
    public static long byteArrayToLong(final byte[] src, final int srcPos, final long dstInit, final int dstPos,
            final int nBytes) {
        if (src.length == 0 && srcPos == 0 || 0 == nBytes) {
            return dstInit;
        }
        if ((nBytes - 1) * 8 + dstPos >= 64) {
            throw new IllegalArgumentException("(nBytes-1)*8+dstPos is greater or equal to than 64");
        }
        long out = dstInit;
        for (int i = 0; i < nBytes; i++) {
            final int shift = i * 8 + dstPos;
            final long bits = (0xffL & src[i + srcPos]) << shift;
            final long mask = 0xffL << shift;
            out = (out & ~mask) | bits;
        }
        return out;
    }

    /**
     * Converts an array of byte into an int using the default (little endian, Lsb0) byte and bit
     * ordering.
     *
     * @param src the byte array to convert
     * @param srcPos the position in {@code src}, in byte unit, from where to start the
     *            conversion
     * @param dstInit initial value of the destination int
     * @param dstPos the position of the lsb, in bits, in the result int
     * @param nBytes the number of bytes to convert
     * @return an int containing the selected bits
     * @throws NullPointerException if {@code src} is {@code null}
     * @throws IllegalArgumentException if {@code (nBytes-1)*8+dstPos >= 32}
     * @throws ArrayIndexOutOfBoundsException if {@code srcPos + nBytes > src.length}
     */
    public static int byteArrayToInt(final byte[] src, final int srcPos, final int dstInit, final int dstPos,
            final int nBytes) {
        if (src.length == 0 && srcPos == 0 || 0 == nBytes) {
            return dstInit;
        }
        if ((nBytes - 1) * 8 + dstPos >= 32) {
            throw new IllegalArgumentException("(nBytes-1)*8+dstPos is greater or equal to than 32");
        }
        int out = dstInit;
        for (int i = 0; i < nBytes; i++) {
            final int shift = i * 8 + dstPos;
            final int bits = (0xff & src[i + srcPos]) << shift;
            final int mask = 0xff << shift;
            out = (out & ~mask) | bits;
        }
        return out;
    }

    /**
     * Converts an array of byte into a short using the default (little endian, Lsb0) byte and
     * bit ordering.
     *
     * @param src the byte array to convert
     * @param srcPos the position in {@code src}, in byte unit, from where to start the
     *            conversion
     * @param dstInit initial value of the destination short
     * @param dstPos the position of the lsb, in bits, in the result short
     * @param nBytes the number of bytes to convert
     * @return a short containing the selected bits
     * @throws NullPointerException if {@code src} is {@code null}
     * @throws IllegalArgumentException if {@code (nBytes-1)*8+dstPos >= 16}
     * @throws ArrayIndexOutOfBoundsException if {@code srcPos + nBytes > src.length}
     */
    public static short byteArrayToShort(final byte[] src, final int srcPos, final short dstInit, final int dstPos,
            final int nBytes) {
        if (src.length == 0 && srcPos == 0 || 0 == nBytes) {
            return dstInit;
        }
        if ((nBytes - 1) * 8 + dstPos >= 16) {
            throw new IllegalArgumentException("(nBytes-1)*8+dstPos is greater or equal to than 16");
        }
        short out = dstInit;
        for (int i = 0; i < nBytes; i++) {
            final int shift = i * 8 + dstPos;
            final int bits = (0xff & src[i + srcPos]) << shift;
            final int mask = 0xff << shift;
            out = (short) ((out & ~mask) | bits);
        }
        return out;
    }

    /**
     * Converts an array of Char into a long using the default (little endian, Lsb0) byte and
     * bit ordering.
     *
     * @param src the hex string to convert
     * @param srcPos the position in {@code src}, in Char unit, from where to start the
     *            conversion
     * @param dstInit initial value of the destination long
     * @param dstPos the position of the lsb, in bits, in the result long
     * @param nHex the number of Chars to convert
     * @return a long containing the selected bits
     * @throws IllegalArgumentException if {@code (nHexs-1)*4+dstPos >= 64}
     */
    public static long hexToLong(final String src, final int srcPos, final long dstInit, final int dstPos,
            final int nHex) {
        if (0 == nHex) {
            return dstInit;
        }
        if ((nHex - 1) * 4 + dstPos >= 64) {
            throw new IllegalArgumentException("(nHexs-1)*4+dstPos is greater or equal to than 64");
        }
        long out = dstInit;
        for (int i = 0; i < nHex; i++) {
            final int shift = i * 4 + dstPos;
            final long bits = (0xfL & hexDigitToInt(src.charAt(i + srcPos))) << shift;
            final long mask = 0xfL << shift;
            out = (out & ~mask) | bits;
        }
        return out;
    }

    /**
     * Converts an array of Char into an int using the default (little endian, Lsb0) byte and bit
     * ordering.
     *
     * @param src the hex string to convert
     * @param srcPos the position in {@code src}, in Char unit, from where to start the
     *            conversion
     * @param dstInit initial value of the destination int
     * @param dstPos the position of the lsb, in bits, in the result int
     * @param nHex the number of Chars to convert
     * @return an int containing the selected bits
     * @throws IllegalArgumentException if {@code (nHexs-1)*4+dstPos >= 32}
     */
    public static int hexToInt(final String src, final int srcPos, final int dstInit, final int dstPos, final int nHex) {
        if (0 == nHex) {
            return dstInit;
        }
        if ((nHex - 1) * 4 + dstPos >= 32) {
            throw new IllegalArgumentException("(nHexs-1)*4+dstPos is greater or equal to than 32");
        }
        int out = dstInit;
        for (int i = 0; i < nHex; i++) {
            final int shift = i * 4 + dstPos;
            final int bits = (0xf & hexDigitToInt(src.charAt(i + srcPos))) << shift;
            final int mask = 0xf << shift;
            out = (out & ~mask) | bits;
        }
        return out;
    }

    /**
     * Converts an array of Char into a short using the default (little endian, Lsb0) byte and
     * bit ordering.
     *
     * @param src the hex string to convert
     * @param srcPos the position in {@code src}, in Char unit, from where to start the
     *            conversion
     * @param dstInit initial value of the destination short
     * @param dstPos the position of the lsb, in bits, in the result short
     * @param nHex the number of Chars to convert
     * @return a short containing the selected bits
     * @throws IllegalArgumentException if {@code (nHexs-1)*4+dstPos >= 16}
     */
    public static short hexToShort(final String src, final int srcPos, final short dstInit, final int dstPos,
            final int nHex) {
        if (0 == nHex) {
            return dstInit;
        }
        if ((nHex - 1) * 4 + dstPos >= 16) {
            throw new IllegalArgumentException("(nHexs-1)*4+dstPos is greater or equal to than 16");
        }
        short out = dstInit;
        for (int i = 0; i < nHex; i++) {
            final int shift = i * 4 + dstPos;
            final int bits = (0xf & hexDigitToInt(src.charAt(i + srcPos))) << shift;
            final int mask = 0xf << shift;
            out = (short) ((out & ~mask) | bits);
        }
        return out;
    }

    /**
     * Converts an array of Char into a byte using the default (little endian, Lsb0) byte and
     * bit ordering.
     *
     * @param src the hex string to convert
     * @param srcPos the position in {@code src}, in Char unit, from where to start the
     *            conversion
     * @param dstInit initial value of the destination byte
     * @param dstPos the position of the lsb, in bits, in the result byte
     * @param nHex the number of Chars to convert
     * @return a byte containing the selected bits
     * @throws IllegalArgumentException if {@code (nHexs-1)*4+dstPos >= 8}
     */
    public static byte hexToByte(final String src, final int srcPos, final byte dstInit, final int dstPos,
            final int nHex) {
        if (0 == nHex) {
            return dstInit;
        }
        if ((nHex - 1) * 4 + dstPos >= 8) {
            throw new IllegalArgumentException("(nHexs-1)*4+dstPos is greater or equal to than 8");
        }
        byte out = dstInit;
        for (int i = 0; i < nHex; i++) {
            final int shift = i * 4 + dstPos;
            final int bits = (0xf & hexDigitToInt(src.charAt(i + srcPos))) << shift;
            final int mask = 0xf << shift;
            out = (byte) ((out & ~mask) | bits);
        }
        return out;
    }

    /**
     * Converts binary (represented as boolean array) into a long using the default (little
     * endian, Lsb0) byte and bit ordering.
     *
     * @param src the binary to convert
     * @param srcPos the position in {@code src}, in boolean unit, from where to start the
     *            conversion
     * @param dstInit initial value of the destination long
     * @param dstPos the position of the lsb, in bits, in the result long
     * @param nBools the number of booleans to convert
     * @return a long containing the selected bits
     * @throws NullPointerException if {@code src} is {@code null}
     * @throws IllegalArgumentException if {@code nBools-1+dstPos >= 64}
     * @throws ArrayIndexOutOfBoundsException if {@code srcPos + nBools > src.length}
     */
    public static long binaryToLong(final boolean[] src, final int srcPos, final long dstInit, final int dstPos,
            final int nBools) {
        if (src.length == 0 && srcPos == 0 || 0 == nBools) {
            return dstInit;
        }
        if (nBools - 1 + dstPos >= 64) {
            throw new IllegalArgumentException("nBools-1+dstPos is greater or equal to than 64");
        }
        long out = dstInit;
        for (int i = 0; i < nBools; i++) {
            final int shift = i + dstPos;
            final long bits = (src[i + srcPos] ? 1L : 0) << shift;
            final long mask = 0x1L << shift;
            out = (out & ~mask) | bits;
        }
        return out;
    }

    /**
     * Converts binary (represented as boolean array) into an int using the default (little
     * endian, Lsb0) byte and bit ordering.
     *
     * @param src the binary to convert
     * @param srcPos the position in {@code src}, in boolean unit, from where to start the
     *            conversion
     * @param dstInit initial value of the destination int
     * @param dstPos the position of the lsb, in bits, in the result int
     * @param nBools the number of booleans to convert
     * @return an int containing the selected bits
     * @throws NullPointerException if {@code src} is {@code null}
     * @throws IllegalArgumentException if {@code nBools-1+dstPos >= 32}
     * @throws ArrayIndexOutOfBoundsException if {@code srcPos + nBools > src.length}
     */
    public static int binaryToInt(final boolean[] src, final int srcPos, final int dstInit, final int dstPos,
            final int nBools) {
        if (src.length == 0 && srcPos == 0 || 0 == nBools) {
            return dstInit;
        }
        if (nBools - 1 + dstPos >= 32) {
            throw new IllegalArgumentException("nBools-1+dstPos is greater or equal to than 32");
        }
        int out = dstInit;
        for (int i = 0; i < nBools; i++) {
            final int shift = i + dstPos;
            final int bits = (src[i + srcPos] ? 1 : 0) << shift;
            final int mask = 0x1 << shift;
            out = (out & ~mask) | bits;
        }
        return out;
    }

    /**
     * Converts binary (represented as boolean array) into a short using the default (little
     * endian, Lsb0) byte and bit ordering.
     *
     * @param src the binary to convert
     * @param srcPos the position in {@code src}, in boolean unit, from where to start the
     *            conversion
     * @param dstInit initial value of the destination short
     * @param dstPos the position of the lsb, in bits, in the result short
     * @param nBools the number of booleans to convert
     * @return a short containing the selected bits
     * @throws NullPointerException if {@code src} is {@code null}
     * @throws IllegalArgumentException if {@code nBools-1+dstPos >= 16}
     * @throws ArrayIndexOutOfBoundsException if {@code srcPos + nBools > src.length}
     */
    public static short binaryToShort(final boolean[] src, final int srcPos, final short dstInit, final int dstPos,
            final int nBools) {
        if (src.length == 0 && srcPos == 0 || 0 == nBools) {
            return dstInit;
        }
        if (nBools - 1 + dstPos >= 16) {
            throw new IllegalArgumentException("nBools-1+dstPos is greater or equal to than 16");
        }
        short out = dstInit;
        for (int i = 0; i < nBools; i++) {
            final int shift = i + dstPos;
            final int bits = (src[i + srcPos] ? 1 : 0) << shift;
            final int mask = 0x1 << shift;
            out = (short) ((out & ~mask) | bits);
        }
        return out;
    }

    /**
     * Converts binary (represented as boolean array) into a byte using the default (little
     * endian, Lsb0) byte and bit ordering.
     *
     * @param src the binary to convert
     * @param srcPos the position in {@code src}, in boolean unit, from where to start the
     *            conversion
     * @param dstInit initial value of the destination byte
     * @param dstPos the position of the lsb, in bits, in the result byte
     * @param nBools the number of booleans to convert
     * @return a byte containing the selected bits
     * @throws NullPointerException if {@code src} is {@code null}
     * @throws IllegalArgumentException if {@code nBools-1+dstPos >= 8}
     * @throws ArrayIndexOutOfBoundsException if {@code srcPos + nBools > src.length}
     */
    public static byte binaryToByte(final boolean[] src, final int srcPos, final byte dstInit, final int dstPos,
            final int nBools) {
        if (src.length == 0 && srcPos == 0 || 0 == nBools) {
            return dstInit;
        }
        if (nBools - 1 + dstPos >= 8) {
            throw new IllegalArgumentException("nBools-1+dstPos is greater or equal to than 8");
        }
        byte out = dstInit;
        for (int i = 0; i < nBools; i++) {
            final int shift = i + dstPos;
            final int bits = (src[i + srcPos] ? 1 : 0) << shift;
            final int mask = 0x1 << shift;
            out = (byte) ((out & ~mask) | bits);
        }
        return out;
    }

    /**
     * Converts a long into an array of int using the default (little endian, Lsb0) byte and bit
     * ordering.
     *
     * @param src the long to convert
     * @param srcPos the position in {@code src}, in bits, from where to start the conversion
     * @param dst the destination array
     * @param dstPos the position in {@code dst} where to copy the result
     * @param nInts the number of ints to copy to {@code dst}, must be smaller or equal to the
     *            width of the input (from srcPos to msb)
     * @return {@code dst}
     * @throws NullPointerException if {@code dst} is {@code null} and {@code nInts > 0}
     * @throws IllegalArgumentException if {@code (nInts-1)*32+srcPos >= 64}
     * @throws ArrayIndexOutOfBoundsException if {@code dstPos + nInts > dst.length}
     */
    public static int[] longToIntArray(final long src, final int srcPos, final int[] dst, final int dstPos,
            final int nInts) {
        if (0 == nInts) {
            return dst;
        }
        if ((nInts - 1) * 32 + srcPos >= 64) {
            throw new IllegalArgumentException("(nInts-1)*32+srcPos is greater or equal to than 64");
        }
        for (int i = 0; i < nInts; i++) {
            final int shift = i * 32 + srcPos;
            dst[dstPos + i] = (int) (0xffffffff & (src >> shift));
        }
        return dst;
    }

    /**
     * Converts a long into an array of short using the default (little endian, Lsb0) byte and
     * bit ordering.
     *
     * @param src the long to convert
     * @param srcPos the position in {@code src}, in bits, from where to start the conversion
     * @param dst the destination array
     * @param dstPos the position in {@code dst} where to copy the result
     * @param nShorts the number of shorts to copy to {@code dst}, must be smaller or equal to
     *            the width of the input (from srcPos to msb)
     * @return {@code dst}
     * @throws NullPointerException if {@code dst} is {@code null}
     * @throws IllegalArgumentException if {@code (nShorts-1)*16+srcPos >= 64}
     * @throws ArrayIndexOutOfBoundsException if {@code dstPos + nShorts > dst.length}
     */
    public static short[] longToShortArray(final long src, final int srcPos, final short[] dst, final int dstPos,
            final int nShorts) {
        if (0 == nShorts) {
            return dst;
        }
        if ((nShorts - 1) * 16 + srcPos >= 64) {
            throw new IllegalArgumentException("(nShorts-1)*16+srcPos is greater or equal to than 64");
        }
        for (int i = 0; i < nShorts; i++) {
            final int shift = i * 16 + srcPos;
            dst[dstPos + i] = (short) (0xffff & (src >> shift));
        }
        return dst;
    }

    /**
     * Converts an int into an array of short using the default (little endian, Lsb0) byte and
     * bit ordering.
     *
     * @param src the int to convert
     * @param srcPos the position in {@code src}, in bits, from where to start the conversion
     * @param dst the destination array
     * @param dstPos the position in {@code dst} where to copy the result
     * @param nShorts the number of shorts to copy to {@code dst}, must be smaller or equal to
     *            the width of the input (from srcPos to msb)
     * @return {@code dst}
     * @throws NullPointerException if {@code dst} is {@code null}
     * @throws IllegalArgumentException if {@code (nShorts-1)*16+srcPos >= 32}
     * @throws ArrayIndexOutOfBoundsException if {@code dstPos + nShorts > dst.length}
     */
    public static short[] intToShortArray(final int src, final int srcPos, final short[] dst, final int dstPos,
            final int nShorts) {
        if (0 == nShorts) {
            return dst;
        }
        if ((nShorts - 1) * 16 + srcPos >= 32) {
            throw new IllegalArgumentException("(nShorts-1)*16+srcPos is greater or equal to than 32");
        }
        for (int i = 0; i < nShorts; i++) {
            final int shift = i * 16 + srcPos;
            dst[dstPos + i] = (short) (0xffff & (src >> shift));
        }
        return dst;
    }

    /**
     * Converts a long into an array of byte using the default (little endian, Lsb0) byte and
     * bit ordering.
     *
     * @param src the long to convert
     * @param srcPos the position in {@code src}, in bits, from where to start the conversion
     * @param dst the destination array
     * @param dstPos the position in {@code dst} where to copy the result
     * @param nBytes the number of bytes to copy to {@code dst}, must be smaller or equal to the
     *            width of the input (from srcPos to msb)
     * @return {@code dst}
     * @throws NullPointerException if {@code dst} is {@code null}
     * @throws IllegalArgumentException if {@code (nBytes-1)*8+srcPos >= 64}
     * @throws ArrayIndexOutOfBoundsException if {@code dstPos + nBytes > dst.length}
     */
    public static byte[] longToByteArray(final long src, final int srcPos, final byte[] dst, final int dstPos,
            final int nBytes) {
        if (0 == nBytes) {
            return dst;
        }
        if ((nBytes - 1) * 8 + srcPos >= 64) {
            throw new IllegalArgumentException("(nBytes-1)*8+srcPos is greater or equal to than 64");
        }
        for (int i = 0; i < nBytes; i++) {
            final int shift = i * 8 + srcPos;
            dst[dstPos + i] = (byte) (0xff & (src >> shift));
        }
        return dst;
    }

    /**
     * Converts an int into an array of byte using the default (little endian, Lsb0) byte and bit
     * ordering.
     *
     * @param src the int to convert
     * @param srcPos the position in {@code src}, in bits, from where to start the conversion
     * @param dst the destination array
     * @param dstPos the position in {@code dst} where to copy the result
     * @param nBytes the number of bytes to copy to {@code dst}, must be smaller or equal to the
     *            width of the input (from srcPos to msb)
     * @return {@code dst}
     * @throws NullPointerException if {@code dst} is {@code null}
     * @throws IllegalArgumentException if {@code (nBytes-1)*8+srcPos >= 32}
     * @throws ArrayIndexOutOfBoundsException if {@code dstPos + nBytes > dst.length}
     */
    public static byte[] intToByteArray(final int src, final int srcPos, final byte[] dst, final int dstPos,
            final int nBytes) {
        if (0 == nBytes) {
            return dst;
        }
        if ((nBytes - 1) * 8 + srcPos >= 32) {
            throw new IllegalArgumentException("(nBytes-1)*8+srcPos is greater or equal to than 32");
        }
        for (int i = 0; i < nBytes; i++) {
            final int shift = i * 8 + srcPos;
            dst[dstPos + i] = (byte) (0xff & (src >> shift));
        }
        return dst;
    }

    /**
     * Converts a short into an array of byte using the default (little endian, Lsb0) byte and
     * bit ordering.
     *
     * @param src the short to convert
     * @param srcPos the position in {@code src}, in bits, from where to start the conversion
     * @param dst the destination array
     * @param dstPos the position in {@code dst} where to copy the result
     * @param nBytes the number of bytes to copy to {@code dst}, must be smaller or equal to the
     *            width of the input (from srcPos to msb)
     * @return {@code dst}
     * @throws NullPointerException if {@code dst} is {@code null}
     * @throws IllegalArgumentException if {@code (nBytes-1)*8+srcPos >= 16}
     * @throws ArrayIndexOutOfBoundsException if {@code dstPos + nBytes > dst.length}
     */
    public static byte[] shortToByteArray(final short src, final int srcPos, final byte[] dst, final int dstPos,
            final int nBytes) {
        if (0 == nBytes) {
            return dst;
        }
        if ((nBytes - 1) * 8 + srcPos >= 16) {
            throw new IllegalArgumentException("(nBytes-1)*8+srcPos is greater or equal to than 16");
        }
        for (int i = 0; i < nBytes; i++) {
            final int shift = i * 8 + srcPos;
            dst[dstPos + i] = (byte) (0xff & (src >> shift));
        }
        return dst;
    }

    /**
     * Converts a long into an array of Char using the default (little endian, Lsb0) byte and
     * bit ordering.
     *
     * @param src the long to convert
     * @param srcPos the position in {@code src}, in bits, from where to start the conversion
     * @param dstInit the initial value for the result String
     * @param dstPos the position in {@code dst} where to copy the result
     * @param nHexs the number of Chars to copy to {@code dst}, must be smaller or equal to the
     *            width of the input (from srcPos to msb)
     * @return {@code dst}
     * @throws IllegalArgumentException if {@code (nHexs-1)*4+srcPos >= 64}
     * @throws StringIndexOutOfBoundsException if {@code dst.init.length() < dstPos}
     */
    public static String longToHex(final long src, final int srcPos, final String dstInit, final int dstPos,
            final int nHexs) {
        if (0 == nHexs) {
            return dstInit;
        }
        if ((nHexs - 1) * 4 + srcPos >= 64) {
            throw new IllegalArgumentException("(nHexs-1)*4+srcPos is greater or equal to than 64");
        }
        final StringBuilder sb = new StringBuilder(dstInit);
        int append = sb.length();
        for (int i = 0; i < nHexs; i++) {
            final int shift = i * 4 + srcPos;
            final int bits = (int) (0xF & (src >> shift));
            if (dstPos + i == append) {
                ++append;
                sb.append(intToHexDigit(bits));
            } else {
                sb.setCharAt(dstPos + i, intToHexDigit(bits));
            }
        }
        return sb.toString();
    }

    /**
     * Converts an int into an array of Char using the default (little endian, Lsb0) byte and bit
     * ordering.
     *
     * @param src the int to convert
     * @param srcPos the position in {@code src}, in bits, from where to start the conversion
     * @param dstInit the initial value for the result String
     * @param dstPos the position in {@code dst} where to copy the result
     * @param nHexs the number of Chars to copy to {@code dst}, must be smaller or equal to the
     *            width of the input (from srcPos to msb)
     * @return {@code dst}
     * @throws IllegalArgumentException if {@code (nHexs-1)*4+srcPos >= 32}
     * @throws StringIndexOutOfBoundsException if {@code dst.init.length() < dstPos}
     */
    public static String intToHex(final int src, final int srcPos, final String dstInit, final int dstPos,
            final int nHexs) {
        if (0 == nHexs) {
            return dstInit;
        }
        if ((nHexs - 1) * 4 + srcPos >= 32) {
            throw new IllegalArgumentException("(nHexs-1)*4+srcPos is greater or equal to than 32");
        }
        final StringBuilder sb = new StringBuilder(dstInit);
        int append = sb.length();
        for (int i = 0; i < nHexs; i++) {
            final int shift = i * 4 + srcPos;
            final int bits = 0xF & (src >> shift);
            if (dstPos + i == append) {
                ++append;
                sb.append(intToHexDigit(bits));
            } else {
                sb.setCharAt(dstPos + i, intToHexDigit(bits));
            }
        }
        return sb.toString();
    }

    /**
     * Converts a short into an array of Char using the default (little endian, Lsb0) byte and
     * bit ordering.
     *
     * @param src the short to convert
     * @param srcPos the position in {@code src}, in bits, from where to start the conversion
     * @param dstInit the initial value for the result String
     * @param dstPos the position in {@code dst} where to copy the result
     * @param nHexs the number of Chars to copy to {@code dst}, must be smaller or equal to the
     *            width of the input (from srcPos to msb)
     * @return {@code dst}
     * @throws IllegalArgumentException if {@code (nHexs-1)*4+srcPos >= 16}
     * @throws StringIndexOutOfBoundsException if {@code dst.init.length() < dstPos}
     */
    public static String shortToHex(final short src, final int srcPos, final String dstInit, final int dstPos,
            final int nHexs) {
        if (0 == nHexs) {
            return dstInit;
        }
        if ((nHexs - 1) * 4 + srcPos >= 16) {
            throw new IllegalArgumentException("(nHexs-1)*4+srcPos is greater or equal to than 16");
        }
        final StringBuilder sb = new StringBuilder(dstInit);
        int append = sb.length();
        for (int i = 0; i < nHexs; i++) {
            final int shift = i * 4 + srcPos;
            final int bits = 0xF & (src >> shift);
            if (dstPos + i == append) {
                ++append;
                sb.append(intToHexDigit(bits));
            } else {
                sb.setCharAt(dstPos + i, intToHexDigit(bits));
            }
        }
        return sb.toString();
    }

    /**
     * Converts a byte into an array of Char using the default (little endian, Lsb0) byte and
     * bit ordering.
     *
     * @param src the byte to convert
     * @param srcPos the position in {@code src}, in bits, from where to start the conversion
     * @param dstInit the initial value for the result String
     * @param dstPos the position in {@code dst} where to copy the result
     * @param nHexs the number of Chars to copy to {@code dst}, must be smaller or equal to the
     *            width of the input (from srcPos to msb)
     * @return {@code dst}
     * @throws IllegalArgumentException if {@code (nHexs-1)*4+srcPos >= 8}
     * @throws StringIndexOutOfBoundsException if {@code dst.init.length() < dstPos}
     */
    public static String byteToHex(final byte src, final int srcPos, final String dstInit, final int dstPos,
            final int nHexs) {
        if (0 == nHexs) {
            return dstInit;
        }
        if ((nHexs - 1) * 4 + srcPos >= 8) {
            throw new IllegalArgumentException("(nHexs-1)*4+srcPos is greater or equal to than 8");
        }
        final StringBuilder sb = new StringBuilder(dstInit);
        int append = sb.length();
        for (int i = 0; i < nHexs; i++) {
            final int shift = i * 4 + srcPos;
            final int bits = 0xF & (src >> shift);
            if (dstPos + i == append) {
                ++append;
                sb.append(intToHexDigit(bits));
            } else {
                sb.setCharAt(dstPos + i, intToHexDigit(bits));
            }
        }
        return sb.toString();
    }

    /**
     * Converts a long into an array of boolean using the default (little endian, Lsb0) byte and
     * bit ordering.
     *
     * @param src the long to convert
     * @param srcPos the position in {@code src}, in bits, from where to start the conversion
     * @param dst the destination array
     * @param dstPos the position in {@code dst} where to copy the result
     * @param nBools the number of booleans to copy to {@code dst}, must be smaller or equal to
     *            the width of the input (from srcPos to msb)
     * @return {@code dst}
     * @throws NullPointerException if {@code dst} is {@code null}
     * @throws IllegalArgumentException if {@code nBools-1+srcPos >= 64}
     * @throws ArrayIndexOutOfBoundsException if {@code dstPos + nBools > dst.length}
     */
    public static boolean[] longToBinary(final long src, final int srcPos, final boolean[] dst, final int dstPos,
            final int nBools) {
        if (0 == nBools) {
            return dst;
        }
        if (nBools - 1 + srcPos >= 64) {
            throw new IllegalArgumentException("nBools-1+srcPos is greater or equal to than 64");
        }
        for (int i = 0; i < nBools; i++) {
            final int shift = i + srcPos;
            dst[dstPos + i] = (0x1 & (src >> shift)) != 0;
        }
        return dst;
    }

    /**
     * Converts an int into an array of boolean using the default (little endian, Lsb0) byte and
     * bit ordering.
     *
     * @param src the int to convert
     * @param srcPos the position in {@code src}, in bits, from where to start the conversion
     * @param dst the destination array
     * @param dstPos the position in {@code dst} where to copy the result
     * @param nBools the number of booleans to copy to {@code dst}, must be smaller or equal to
     *            the width of the input (from srcPos to msb)
     * @return {@code dst}
     * @throws NullPointerException if {@code dst} is {@code null}
     * @throws IllegalArgumentException if {@code nBools-1+srcPos >= 32}
     * @throws ArrayIndexOutOfBoundsException if {@code dstPos + nBools > dst.length}
     */
    public static boolean[] intToBinary(final int src, final int srcPos, final boolean[] dst, final int dstPos,
            final int nBools) {
        if (0 == nBools) {
            return dst;
        }
        if (nBools - 1 + srcPos >= 32) {
            throw new IllegalArgumentException("nBools-1+srcPos is greater or equal to than 32");
        }
        for (int i = 0; i < nBools; i++) {
            final int shift = i + srcPos;
            dst[dstPos + i] = (0x1 & (src >> shift)) != 0;
        }
        return dst;
    }

    /**
     * Converts a short into an array of boolean using the default (little endian, Lsb0) byte
     * and bit ordering.
     *
     * @param src the short to convert
     * @param srcPos the position in {@code src}, in bits, from where to start the conversion
     * @param dst the destination array
     * @param dstPos the position in {@code dst} where to copy the result
     * @param nBools the number of booleans to copy to {@code dst}, must be smaller or equal to
     *            the width of the input (from srcPos to msb)
     * @return {@code dst}
     * @throws NullPointerException if {@code dst} is {@code null}
     * @throws IllegalArgumentException if {@code nBools-1+srcPos >= 16}
     * @throws ArrayIndexOutOfBoundsException if {@code dstPos + nBools > dst.length}
     */
    public static boolean[] shortToBinary(final short src, final int srcPos, final boolean[] dst, final int dstPos,
            final int nBools) {
        if (0 == nBools) {
            return dst;
        }
        if (nBools - 1 + srcPos >= 16) {
            throw new IllegalArgumentException("nBools-1+srcPos is greater or equal to than 16");
        }
        assert (nBools - 1) < 16 - srcPos;
        for (int i = 0; i < nBools; i++) {
            final int shift = i + srcPos;
            dst[dstPos + i] = (0x1 & (src >> shift)) != 0;
        }
        return dst;
    }

    /**
     * Converts a byte into an array of boolean using the default (little endian, Lsb0) byte and
     * bit ordering.
     *
     * @param src the byte to convert
     * @param srcPos the position in {@code src}, in bits, from where to start the conversion
     * @param dst the destination array
     * @param dstPos the position in {@code dst} where to copy the result
     * @param nBools the number of booleans to copy to {@code dst}, must be smaller or equal to
     *            the width of the input (from srcPos to msb)
     * @return {@code dst}
     * @throws NullPointerException if {@code dst} is {@code null}
     * @throws IllegalArgumentException if {@code nBools-1+srcPos >= 8}
     * @throws ArrayIndexOutOfBoundsException if {@code dstPos + nBools > dst.length}
     */
    public static boolean[] byteToBinary(final byte src, final int srcPos, final boolean[] dst, final int dstPos,
            final int nBools) {
        if (0 == nBools) {
            return dst;
        }
        if (nBools - 1 + srcPos >= 8) {
            throw new IllegalArgumentException("nBools-1+srcPos is greater or equal to than 8");
        }
        for (int i = 0; i < nBools; i++) {
            final int shift = i + srcPos;
            dst[dstPos + i] = (0x1 & (src >> shift)) != 0;
        }
        return dst;
    }

    /**
     * Converts UUID into an array of byte using the default (little endian, Lsb0) byte and bit
     * ordering.
     *
     * @param src the UUID to convert
     * @param dst the destination array
     * @param dstPos the position in {@code dst} where to copy the result
     * @param nBytes the number of bytes to copy to {@code dst}, must be smaller or equal to the
     *            width of the input (from srcPos to msb)
     * @return {@code dst}
     * @throws NullPointerException if {@code dst} is {@code null}
     * @throws IllegalArgumentException if {@code nBytes > 16}
     * @throws ArrayIndexOutOfBoundsException if {@code dstPos + nBytes > dst.length}
     */
    public static byte[] uuidToByteArray(final UUID src, final byte[] dst, final int dstPos, final int nBytes) {
        if (0 == nBytes) {
            return dst;
        }
        if (nBytes > 16) {
            throw new IllegalArgumentException("nBytes is greater than 16");
        }
        longToByteArray(src.getMostSignificantBits(), 0, dst, dstPos, Math.min(nBytes, 8));
        if (nBytes >= 8) {
            longToByteArray(src.getLeastSignificantBits(), 0, dst, dstPos + 8, nBytes - 8);
        }
        return dst;
    }

    /**
     * Converts bytes from an array into a UUID using the default (little endian, Lsb0) byte and
     * bit ordering.
     *
     * @param src the byte array to convert
     * @param srcPos the position in {@code src} where to copy the result from
     * @return a UUID
     * @throws NullPointerException if {@code src} is {@code null}
     * @throws IllegalArgumentException if array does not contain at least 16 bytes beginning
     *             with {@code srcPos}
     */
    public static UUID byteArrayToUuid(final byte[] src, final int srcPos) {
        if (src.length - srcPos < 16) {
            throw new IllegalArgumentException("Need at least 16 bytes for UUID");
        }
        return new UUID(byteArrayToLong(src, srcPos, 0, 0, 8), byteArrayToLong(src, srcPos + 8, 0, 0, 8));
    }
}
