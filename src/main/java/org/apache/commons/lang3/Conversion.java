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

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;


/**
 * <p>
 * Static methods to convert a type into another, with endianness and bit ordering awareness.
 * </p>
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
 * <li>bitString: a String containing one's and zero's representing bits (Ex: "10100110")</li>
 * <li>uuid</li>
 * </ul>
 * <p>
 * Endianness field: little endian is the default, in this case the field is absent. In case of
 * big endian, the field is "Be".<br>
 * Bit ordering: Lsb0 is the default, in this case the field is absent. In case of Msb0, the
 * field is "Msb0". The field "Raw" indicates that bits are copied "as-is" from the source object,
 * to the destination object. Bit order is maintained when read from left to right. In many cases,
 * this is equivalent to BeMsb0.<br>
 * Ex: a bitString indexed as "01234567" converts 'Raw' to a byte indexed as (01234567)
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
 * @since Lang 3.2
 */

public class Conversion {

    private static final boolean[] TTTT = new boolean[] { true, true, true, true };
    private static final boolean[] FTTT = new boolean[] { false, true, true, true };
    private static final boolean[] TFTT = new boolean[] { true, false, true, true };
    private static final boolean[] FFTT = new boolean[] { false, false, true, true };
    private static final boolean[] TTFT = new boolean[] { true, true, false, true };
    private static final boolean[] FTFT = new boolean[] { false, true, false, true };
    private static final boolean[] TFFT = new boolean[] { true, false, false, true };
    private static final boolean[] FFFT = new boolean[] { false, false, false, true };
    private static final boolean[] TTTF = new boolean[] { true, true, true, false };
    private static final boolean[] FTTF = new boolean[] { false, true, true, false };
    private static final boolean[] TFTF = new boolean[] { true, false, true, false };
    private static final boolean[] FFTF = new boolean[] { false, false, true, false };
    private static final boolean[] TTFF = new boolean[] { true, true, false, false };
    private static final boolean[] FTFF = new boolean[] { false, true, false, false };
    private static final boolean[] TFFF = new boolean[] { true, false, false, false };
    private static final boolean[] FFFF = new boolean[] { false, false, false, false };

    private static final int SHORT_BYTES  = 2;
    private static final int INT_BYTES    = 4;
    private static final int LONG_BYTES   = 8;
    private static final int FLOAT_BYTES  = 4;
    private static final int DOUBLE_BYTES = 8;

    /**
     * <p>
     * Converts a hexadecimal digit into an int using the default (Lsb0) bit ordering.
     * </p>
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
     * <p>
     * Converts a hexadecimal digit into an int using the Msb0 bit ordering.
     * </p>
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
     * <p>
     * Converts a hexadecimal digit into binary (represented as boolean array) using the default
     * (Lsb0) bit ordering.
     * </p>
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
     * <p>
     * Converts a hexadecimal digit into binary (represented as boolean array) using the Msb0
     * bit ordering.
     * </p>
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
     * <p>
     * Converts binary (represented as boolean array) to a hexadecimal digit using the default
     * (Lsb0) bit ordering.
     * </p>
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
     * <p>
     * Converts binary (represented as boolean array) to a hexadecimal digit using the default
     * (Lsb0) bit ordering.
     * </p>
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
            if (src.length > srcPos + 2 && src[srcPos + 2]) {
                if (src.length > srcPos + 1 && src[srcPos + 1]) {
                    return src[srcPos] ? 'f' : 'e';
                }
                return src[srcPos] ? 'd' : 'c';
            }
            if (src.length > srcPos + 1 && src[srcPos + 1]) {
                return src[srcPos] ? 'b' : 'a';
            }
            return src[srcPos] ? '9' : '8';
        }
        if (src.length > srcPos + 2 && src[srcPos + 2]) {
            if (src.length > srcPos + 1 && src[srcPos + 1]) {
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
     * <p>
     * Converts binary (represented as boolean array) to a hexadecimal digit using the Msb0 bit
     * ordering.
     * </p>
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
     * <p>
     * Converts binary (represented as boolean array) to a hexadecimal digit using the Msb0 bit
     * ordering.
     * </p>
     * <p>
     * (1, 0, 0, 0) is converted as follow: '8' (1,0,0,1,1,0,1,0) with srcPos = 3 is converted
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
     * <p>
     * Converts the first 4 bits of a binary (represented as boolean array) in big endian Msb0
     * bit ordering to a hexadecimal digit.
     * </p>
     * <p>
     * (1, 0, 0, 0) is converted as follow: '8' (1,0,0,0,0,0,0,0, 0,0,0,0,0,1,0,0) is converted
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
     * <p>
     * Converts a binary (represented as boolean array) in big endian Msb0 bit ordering to a
     * hexadecimal digit.
     * </p>
     * <p>
     * (1, 0, 0, 0) with srcPos = 0 is converted as follow: '8' (1,0,0,0,0,0,0,0,
     * 0,0,0,1,0,1,0,0) with srcPos = 2 is converted to '5'
     * </p>
     *
     * @param src the binary to convert
     * @param srcPos the position of the lsb to start the conversion
     * @return a hexadecimal digit representing the selected bits
     * @throws IllegalArgumentException if {@code src} is empty
     * @throws NullPointerException if {@code src} is {@code null}
     */
    public static char binaryBeMsb0ToHexDigit(boolean[] src, int srcPos) {
        if (src.length == 0) {
            throw new IllegalArgumentException("Cannot convert an empty array.");
        }
        final int beSrcPos = src.length - 1 - srcPos;
        final int srcLen = Math.min(4, beSrcPos + 1);
        final boolean[] paddedSrc = new boolean[4];
        System.arraycopy(src, beSrcPos + 1 - srcLen, paddedSrc, 4 - srcLen, srcLen);
        src = paddedSrc;
        srcPos = 0;
        if (src[srcPos]) {
            if (src.length > srcPos + 1 && src[srcPos + 1]) {
                if (src.length > srcPos + 2 && src[srcPos + 2]) {
                    return src.length > srcPos + 3 && src[srcPos + 3] ? 'f' : 'e';
                }
                return src.length > srcPos + 3 && src[srcPos + 3] ? 'd' : 'c';
            }
            if (src.length > srcPos + 2 && src[srcPos + 2]) {
                return src.length > srcPos + 3 && src[srcPos + 3] ? 'b' : 'a';
            }
            return src.length > srcPos + 3 && src[srcPos + 3] ? '9' : '8';
        }
        if (src.length > srcPos + 1 && src[srcPos + 1]) {
            if (src.length > srcPos + 2 && src[srcPos + 2]) {
                return src.length > srcPos + 3 && src[srcPos + 3] ? '7' : '6';
            }
            return src.length > srcPos + 3 && src[srcPos + 3] ? '5' : '4';
        }
        if (src.length > srcPos + 2 && src[srcPos + 2]) {
            return src.length > srcPos + 3 && src[srcPos + 3] ? '3' : '2';
        }
        return src.length > srcPos + 3 && src[srcPos + 3] ? '1' : '0';
    }

    /**
     * <p>
     * Converts the 4 lsb of an int to a hexadecimal digit.
     * </p>
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
     * <p>
     * Converts the 4 lsb of an int to a hexadecimal digit encoded using the Msb0 bit ordering.
     * </p>
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
     * <p>
     * Converts an array of int into a long using the default (little endian, Lsb0) byte and bit
     * ordering.
     * </p>
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
     * <p>
     * Converts an array of short into a long using the default (little endian, Lsb0) byte and
     * bit ordering.
     * </p>
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
     * <p>
     * Converts an array of short into an int using the default (little endian, Lsb0) byte and
     * bit ordering.
     * </p>
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
     * <p>
     * Converts an array of byte into a long using the default (little endian, Lsb0) byte and
     * bit ordering.
     * </p>
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
     * <p>
     * Converts an array of byte into an int using the default (little endian, Lsb0) byte and bit
     * ordering.
     * </p>
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
     * <p>
     * Converts an array of byte into a short using the default (little endian, Lsb0) byte and
     * bit ordering.
     * </p>
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
     * <p>
     * Converts an array of Char into a long using the default (little endian, Lsb0) byte and
     * bit ordering.
     * </p>
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
     * <p>
     * Converts an array of Char into an int using the default (little endian, Lsb0) byte and bit
     * ordering.
     * </p>
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
     * <p>
     * Converts an array of Char into a short using the default (little endian, Lsb0) byte and
     * bit ordering.
     * </p>
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
     * <p>
     * Converts an array of Char into a byte using the default (little endian, Lsb0) byte and
     * bit ordering.
     * </p>
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
     * <p>
     * Converts binary (represented as boolean array) into a long using the default (little
     * endian, Lsb0) byte and bit ordering.
     * </p>
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
     * <p>
     * Converts binary (represented as boolean array) into an int using the default (little
     * endian, Lsb0) byte and bit ordering.
     * </p>
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
     * <p>
     * Converts binary (represented as boolean array) into a short using the default (little
     * endian, Lsb0) byte and bit ordering.
     * </p>
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
     * <p>
     * Converts binary (represented as boolean array) into a byte using the default (little
     * endian, Lsb0) byte and bit ordering.
     * </p>
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
     * <p>
     * Converts a long into an array of int using the default (little endian, Lsb0) byte and bit
     * ordering.
     * </p>
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
     * <p>
     * Converts a long into an array of short using the default (little endian, Lsb0) byte and
     * bit ordering.
     * </p>
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
     * <p>
     * Converts an int into an array of short using the default (little endian, Lsb0) byte and
     * bit ordering.
     * </p>
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
     * <p>
     * Converts a long into an array of byte using the default (little endian, Lsb0) byte and
     * bit ordering.
     * </p>
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
     * <p>
     * Converts an int into an array of byte using the default (little endian, Lsb0) byte and bit
     * ordering.
     * </p>
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
     * <p>
     * Converts a short into an array of byte using the default (little endian, Lsb0) byte and
     * bit ordering.
     * </p>
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
     * <p>
     * Converts a long into an array of Char using the default (little endian, Lsb0) byte and
     * bit ordering.
     * </p>
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
     * <p>
     * Converts an int into an array of Char using the default (little endian, Lsb0) byte and bit
     * ordering.
     * </p>
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
     * <p>
     * Converts a short into an array of Char using the default (little endian, Lsb0) byte and
     * bit ordering.
     * </p>
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
     * <p>
     * Converts a byte into an array of Char using the default (little endian, Lsb0) byte and
     * bit ordering.
     * </p>
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
     * <p>
     * Converts a long into an array of boolean using the default (little endian, Lsb0) byte and
     * bit ordering.
     * </p>
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
     * <p>
     * Converts an int into an array of boolean using the default (little endian, Lsb0) byte and
     * bit ordering.
     * </p>
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
     * <p>
     * Converts a short into an array of boolean using the default (little endian, Lsb0) byte
     * and bit ordering.
     * </p>
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
     * <p>
     * Converts a byte into an array of boolean using the default (little endian, Lsb0) byte and
     * bit ordering.
     * </p>
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
     * <p>
     * Converts UUID into an array of byte using the default (little endian, Lsb0) byte and bit
     * ordering.
     * </p>
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
        longToByteArray(src.getMostSignificantBits(), 0, dst, dstPos, nBytes > 8 ? 8 : nBytes);
        if (nBytes >= 8) {
            longToByteArray(src.getLeastSignificantBits(), 0, dst, dstPos + 8, nBytes - 8);
        }
        return dst;
    }

    /**
     * <p>
     * Converts bytes from an array into a UUID using the default (little endian, Lsb0) byte and
     * bit ordering.
     * </p>
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

    /**
     * <p>
     * Inverts the order of the bits. Ex: A byte indexed as (0,1,2,3,4,5,6,7),
     * is returned as (7,6,5,4,3,2,1,0)
     * </p>
     *
     * @param b The byte to invert
     * @return
     */
    public static byte invertBitOrder(final byte b) {
        return (byte) (
            ((b >> 7) & 1) |
            ((b >> 5) & 2) |
            ((b >> 3) & 4) |
            ((b >> 1) & 8) |
            ((b << 1) & 16) |
            ((b << 3) & 32) |
            ((b << 5) & 64) |
            ((b << 7) & 128)
        );
    }

    /**
     * <p>
     * Inverts the order of the bits. Ex: A binary array indexed as [0,1,2,3,4,5,6,7],
     * is returned as [7,6,5,4,3,2,1,0]
     * </p>
     *
     * <p>
     * Note: There is no limit on input length.
     * </p>
     *
     * @param bits The binary array to invert.
     * @return
     */
    public static boolean[] invertBitOrder(final boolean[] bits) {
        if (bits == null || bits.length == 0) {
            return new boolean[0];
        }
        boolean[] out = bits.clone();
        ArrayUtils.reverse(out);
        return out;
    }

    /**
     * <p>
     * Inverts the order of the bits. Ex: A bitString indexed as "01234567"
     * is returned as "76543210"
     * </p>
     *
     * <p>
     * Note: There is no limit on input length.
     * </p>
     *
     * @param bitString
     * @return
     */
    public static String invertBitOrder(final String bitString) {
        if (bitString == null || bitString.length() == 0) {
            return "";
        }
        char[] out = bitString.toCharArray();
        ArrayUtils.reverse(out);
        return new String(out);
    }

    /**
     * <p>
     * Converts a byte into an array of booleans using the default Lsb0 bit ordering.
     * Ex: a byte indexed as (01234567) is returned as [7,6,5,4,3,2,1,0]
     * </p>
     *
     * @param b the byte to convert.
     * @return
     */
    public static boolean[] byteToBinary(final byte b) {
        boolean[] bits = new boolean[8];
        for (int x=0; x<8; x++) {
            bits[x] = ((b >> x) & 1) == 1;
        }
        return bits;
    }

    /**
     * <p>
     * Converts a byte into an array of booleans "as-is". Bit order is maintained, when
     * read from left to right.
     * Ex: a byte indexed as (01234567) is returned as [0,1,2,3,4,5,6,7]
     * </p>
     *
     * @param b the byte to convert.
     * @return
     */
    public static boolean[] byteToBinaryRaw(final byte b) {
        return byteToBinaryMsb0(b);
    }

    /**
     * <p>
     * Converts a byte into an array of booleans using the Msb0 bit ordering.
     * Ex: a byte indexed as (01234567) is returned as [0,1,2,3,4,5,6,7]
     * </p>
     *
     * @param b the byte to convert.
     * @return
     */
    public static boolean[] byteToBinaryMsb0(final byte b) {
        boolean[] bits = new boolean[8];
        for (int x=0; x<8; x++) {
            bits[7-x] = ((b >> x) & 1) == 1;
        }
        return bits;
    }

    /**
     * <p>
     * Converts binary (represented as boolean array) into a byte using the default Lsb0 bit ordering.
     * Ex: A bit array indexed as [0,1,2,3,4,5,6,7], is returned as (76543210)
     * </p>
     *
     * <p>
     * Note: The input array <code>bits</code> cannot be longer than 8 elements.
     * </p>
     *
     * @param bits The bits to convert.
     * @return
     */
    public static byte binaryToByte(final boolean[] bits) {
        if (bits == null || bits.length == 0) {
            return 0;
        }
        int len = bits.length;
        if (len > 8) {
            throw new IllegalArgumentException("Binary data is longer than 1 byte (8 digits)");
        }

        byte byteOut = 0;
        for (int x=0; x<len; x++) {
            byte bitVal = (byte) ((bits[x])?1:0);
            byteOut |= (byte) ((bitVal) << x);
        }
        return byteOut;
    }

    /**
     * <p>
     * Converts binary (represented as boolean array) into a byte "as-is". Bit order is maintained
     * when read from left to right.
     * Ex: a bit array indexed as [0,1,2,3,4,5,6,7] is returned as (01234567)
     * </p>
     *
     * <p>
     * Note: The input array <code>bits</code> cannot be longer than 8 elements.
     * </p>
     *
     * @param bits The bits to convert.
     * @return
     */
    public static byte binaryToByteRaw(final boolean[] bits) {
        return binaryToByteMsb0(bits);
    }

    /**
     * <p>
     * Converts binary (represented as boolean array) into a byte using Msb0 bit ordering.
     * Ex: a bit array indexed as [0,1,2,3,4,5,6,7] is returned as (01234567)
     * </p>
     *
     * <p>
     * Note: The input array <code>bits</code> cannot be longer than 8 elements.
     * </p>
     *
     * @param bits The bits to convert.
     * @return
     */
    public static byte binaryToByteMsb0(final boolean[] bits) {
        if (bits == null || bits.length == 0) {
            return 0;
        }
        int len = bits.length;
        if (len > 8) {
            throw new IllegalArgumentException("Binary data is longer than 1 byte (8 digits)");
        }

        byte byteOut = 0;
        for (int x=0; x<len; x++) {
            byte bitVal = (byte) ((bits[x])?1:0);
            byteOut |= (byte) ((bitVal) << (7-x));
        }
        return byteOut;
    }

    /**
     * Strips out all accepted bitString/hexString delimiters, and returns a pure bitString, containing
     * only 1 (one)'s & 0 (zero)'s.
     *
     * @param dataString The source bitString or hexString to parse.
     * @return
     */
    public static String stripStringDelimiters(String dataString) {
        if (dataString == null) {
            return "";
        }
        return dataString.replaceAll("[-_\t\\. ]", "");
    }

    /**
     * <p>
     * Converts a bitString (represented as a String of 1 (one)'s & 0 (zero)'s) into binary
     * (represented as boolean array) "as-is". Bit order is maintained, when read from left
     * to right. Ex: A bitString indexed as "01234567" is returned as [0,1,2,3,4,5,6,7]
     * </p>
     *
     * <p>
     * Note: There is no limit on input length. You may break up the bitString using any of the
     * following delimiters: (delimiters are ignored and have no impact on the output)<br>
     * '-' [hyphen]<br>
     * '_' [underscore]<br>
     * '.' [period]<br>
     * ' ' [space]<br>
     * '\t' [tab]
     * </p>
     *
     * @param bitString
     * @return
     */
    public static boolean[] bitStringToBinaryRaw(final String bitString) {
        if (bitString == null) {
            return new boolean[0];
        }
        final String cleanBitString = stripStringDelimiters(bitString);
        if (cleanBitString.length() == 0) {
            return new boolean[0];
        }
        char[] digits = cleanBitString.toCharArray();
        int len = digits.length;
        boolean[] bits = new boolean[len];
        for (int x=0; x<len; x++) {
            char digit = digits[x];
            if (digit == '0') {
                bits[x] = false;
            } else if (digit == '1') {
                bits[x] = true;
            } else {
                throw new IllegalArgumentException("Bit String can only contain one's (1) and zero's (0)");
            }
        }
        return bits;
    }

    /**
     * <p>
     * Converts binary (represented as boolean array) into a bitString (represented as a String of
     * 1 (one)'s & 0 (zero)'s) "as-is". Bit order is maintained, when read from left to right.
     * Ex: A bit array indexed as [0,1,2,3,4,5,6,7] is returned as "01234567"
     * </p>
     *
     * <p>
     * Note: There is no limit on input length.
     * </p>
     *
     * @param bits
     * @return
     */
    public static String binaryToBitStringRaw(final boolean[] bits) {
        if (bits == null || bits.length == 0) {
            return "";
        }
        StringBuilder builder = new StringBuilder();
        int len = bits.length;
        for (int x=0; x<len; x++) {
            builder.append(bits[x]?'1':'0');
        }
        return builder.toString();
    }

    /**
     * <p>
     * Converts a byte into binary (represented as boolean array) using the default Lsb0 bit ordering.
     * Ex: A byte indexed as (01234567) is returned as "76543210"
     * </p>
     *
     * @param bits
     * @return
     */
    public static String byteToBitString(final byte b) {
        StringBuilder builder = new StringBuilder();
        for (int x=0; x<8; x++) {
            builder.append( (((b >> x) & 1) == 1)?'1':'0');
        }
        return builder.toString();
    }

    /**
     * <p>
     * Converts a byte into binary (represented as boolean array) "as-is". Bit order is maintained
     * when read from left to right.
     * Ex: A byte indexed as (01234567) is returned as "01234567"
     * </p>
     *
     * @param bits
     * @return
     */
    public static String byteToBitStringRaw(final byte b) {
        return byteToBitStringMsb0(b);
    }

    /**
     * <p>
     * Converts a byte into binary (represented as boolean array) using Msb0 bit ordering.
     * Ex: A byte indexed as (01234567) is returned as "01234567"
     * </p>
     *
     * @param bits
     * @return
     */
    public static String byteToBitStringMsb0(final byte b) {
        StringBuilder builder = new StringBuilder();
        for (int x=0; x<8; x++) {
            builder.append( (((b >> (7-x)) & 1) == 1)?'1':'0');
        }
        return builder.toString();
    }

    /**
     * <p>
     * Converts a bitString (represented as a String of 1 (one)'s & 0 (zero)'s) into a byte
     * using the default Lsb0 bit ordering.
     * Ex: A bitString indexed as "01234567" is returned as (76543210)
     * </p>
     *
     * <p>
     * Note: The input String <code>bitString</code> cannot be longer than 8 digits.
     * You may break up the bitString using any of the following delimiters: (delimiters
     * are ignored and have no impact on the output)<br>
     * '-' [hyphen]<br>
     * '_' [underscore]<br>
     * '.' [period]<br>
     * ' ' [space]<br>
     * '\t' [tab]
     * </p>
     *
     * @param bitString The String of bits to convert.
     * @return
     */
    public static byte bitStringToByte(final String bitString) {
        if (bitString == null) {
            return 0;
        }
        final String cleanBitString = stripStringDelimiters(bitString);
        if (cleanBitString.length() == 0) {
            return 0;
        }
        char[] bits = cleanBitString.toCharArray();
        int len = bits.length;
        if (len > 8) {
            throw new IllegalArgumentException("Bit String is longer than 1 byte (8 digits)");
        }

        byte byteOut = 0;
        for (int x=0; x<len; x++) {
            char bit = bits[x];
            if (bit == '1') {
                byte bitVal = (byte) ((bits[x] == '1')?1:0);
                byteOut = (byte) (byteOut | ((bitVal) << x));
            } else if (bit != '0') {
                throw new IllegalArgumentException("Bit String can only contain one's (1) and zero's (0)");
            }
        }
        return byteOut;
    }

    /**
     * <p>
     * Converts a bitString (represented as a String of 1 (one)'s & 0 (zero)'s) into a byte
     * "as-is". Bit order is maintained, when read from left to right.
     * Ex: A bitString indexed as "01234567" is returned as (01234567)
     * </p>
     *
     * <p>
     * Note: The input String <code>bitString</code> cannot be longer than 8 digits. You may
     * break up the bitString using any of the delimiters: (delimiters are ignored and have
     * no impact on the output)<br>
     * '-' [hyphen]<br>
     * '_' [underscore]<br>
     * '.' [period]<br>
     * ' ' [space]<br>
     * '\t' [tab]
     * </p>
     *
     * @param bitString The String of bits to convert.
     * @return
     */
    public static byte bitStringToByteRaw(final String bitString) {
        return bitStringToByteMsb0(bitString);
    }

    /**
     * <p>
     * Converts a bitString (represented as a String of 1 (one)'s & 0 (zero)'s) into a byte
     * using Msb0 bit ordering.
     * Ex: A bitString indexed as "01234567" is returned as (01234567)
     * </p>
     *
     * <p>
     * Note: The input String <code>bitString</code> cannot be longer than 8 digits. You may
     * break up the bitString using any of the following delimiters: (delimiters are ignored
     * and have no impact on the output)<br>
     * '-' [hyphen]<br>
     * '_' [underscore]<br>
     * '.' [period]<br>
     * ' ' [space]<br>
     * '\t' [tab]
     * </p>
     *
     * @param bitString The String of bits to convert.
     * @return
     */
    public static byte bitStringToByteMsb0(final String bitString) {
        if (bitString == null) {
            return 0;
        }
        final String cleanBitString = stripStringDelimiters(bitString);
        if (cleanBitString.length() == 0) {
            return 0;
        }
        char[] bits = cleanBitString.toCharArray();
        int len = bits.length;
        if (len > 8) {
            throw new IllegalArgumentException("Bit String is longer than 1 byte (8 digits)");
        }

        byte byteOut = 0;
        for (int x=0; x<len; x++) {
            char bit = bits[x];
            if (bit == '1') {
                byte bitVal = (byte) ((bits[x] == '1')?1:0);
                byteOut = (byte) (byteOut | ((bitVal) << (7-x)));
            } else if (bit != '0') {
                throw new IllegalArgumentException("Bit String can only contain one's (1) and zero's (0)");
            }
        }
        return byteOut;
    }

    /**
     * <p>
     * Converts a short into an array of bytes using the default little-endian byte ordering.
     * </p>
     *
     * @param s The value to convert.
     * @return
     */
    public static byte[] toByteArray(short s) {
        return new byte[] {
                (byte) ((s >> 0) & 0xFF),
                (byte) ((s >> 8) & 0xFF)
        };
    }


    /**
     * <p>
     * Converts a short into an array of bytes using big-endian byte ordering.
     * </p>
     *
     * @param s The value to convert.
     * @return
     */
    public static byte[] toByteArrayBe(short s) {
        return new byte[] {
                (byte) ((s >> 8) & 0xFF),
                (byte) ((s >> 0) & 0xFF)
        };
    }

    /**
     * <p>
     * Converts an array of bytes into a short using the default little-endian byte ordering.
     * </p>
     *
     * @param inputBytes The bytes to convert.
     * @return
     */
    public static short toShort(byte[] inputBytes) {
        if (inputBytes == null || inputBytes.length == 0) {
            return 0;
        }
        final int SIZE = SHORT_BYTES;
        if (inputBytes.length > SIZE) {
            throw new IllegalArgumentException("Input data is longer than size of 'short' primitive ("+SIZE+" bytes)");
        } else if (inputBytes.length < SIZE) {
            throw new IllegalArgumentException("Input data is smaller than size of 'short' primitive ("+SIZE+" bytes)");
        }
        return (short) (
                ( (((short) inputBytes[0]) & 0xFF) << 0) |
                ( (((short) inputBytes[1]) & 0xFF) << 8)
        );
    }

    /**
     * <p>
     * Converts an array of bytes into a short using big-endian byte ordering.
     * </p>
     *
     * @param inputBytes The bytes to convert.
     * @return
     */
    public static short toShortBe(byte[] inputBytes) {
        if (inputBytes == null || inputBytes.length == 0) {
            return 0;
        }
        final int SIZE = SHORT_BYTES;
        if (inputBytes.length > SIZE) {
            throw new IllegalArgumentException("Input data is longer than size of 'short' primitive ("+SIZE+" bytes)");
        } else if (inputBytes.length < SIZE) {
            throw new IllegalArgumentException("Input data is smaller than size of 'short' primitive ("+SIZE+" bytes)");
        }
        return (short) (
                ( (((short) inputBytes[0]) & 0xFF) << 8) |
                ( (((short) inputBytes[1]) & 0xFF) << 0)
        );
    }

    /**
     * <p>
     * Converts an int into an array of bytes using the default little-endian byte ordering.
     * </p>
     *
     * @param i The value to convert.
     * @return
     */
    public static byte[] toByteArray(int i) {
        return new byte[] {
                (byte) ((i >> 0) & 0xFF),
                (byte) ((i >> 8) & 0xFF),
                (byte) ((i >> 16) & 0xFF),
                (byte) ((i >> 24) & 0xFF)
        };
    }

    /**
     * <p>
     * Converts an int into an array of bytes using big-endian byte ordering.
     * </p>
     *
     * @param i The value to convert.
     * @return
     */
    public static byte[] toByteArrayBe(int i) {
        return new byte[] {
                (byte) ((i >> 24) & 0xFF),
                (byte) ((i >> 16) & 0xFF),
                (byte) ((i >> 8) & 0xFF),
                (byte) ((i >> 0) & 0xFF)
        };
    }

    /**
     * <p>
     * Converts an array of bytes into an int using the default little-endian byte ordering.
     * </p>
     *
     * @param inputBytes The bytes to convert.
     * @return
     */
    public static int toInt(byte[] inputBytes) {
        if (inputBytes == null || inputBytes.length == 0) {
            return 0;
        }
        final int SIZE = INT_BYTES;
        if (inputBytes.length > SIZE) {
            throw new IllegalArgumentException("Input data is longer than size of 'int' primitive ("+SIZE+" bytes)");
        } else if (inputBytes.length < SIZE) {
            throw new IllegalArgumentException("Input data is smaller than size of 'int' primitive ("+SIZE+" bytes)");
        }
        return (int) (
                ( (((int) inputBytes[0]) & 0xFF) << 0) |
                ( (((int) inputBytes[1]) & 0xFF) << 8) |
                ( (((int) inputBytes[2]) & 0xFF) << 16) |
                ( (((int) inputBytes[3]) & 0xFF) << 24)
        );
    }

    /**
     * <p>
     * Converts an array of bytes into an int using big-endian byte ordering.
     * </p>
     *
     * @param inputBytes The bytes to convert.
     * @return
     */
    public static int toIntBe(byte[] inputBytes) {
        if (inputBytes == null || inputBytes.length == 0) {
            return 0;
        }
        final int SIZE = INT_BYTES;
        if (inputBytes.length > SIZE) {
            throw new IllegalArgumentException("Input data is longer than size of 'int' primitive ("+SIZE+" bytes)");
        } else if (inputBytes.length < SIZE) {
            throw new IllegalArgumentException("Input data is smaller than size of 'int' primitive ("+SIZE+" bytes)");
        }
        return (int) (
                ( (((int) inputBytes[0]) & 0xFF) << 24) |
                ( (((int) inputBytes[1]) & 0xFF) << 16) |
                ( (((int) inputBytes[2]) & 0xFF) << 8) |
                ( (((int) inputBytes[3]) & 0xFF) << 0)
        );
    }

    /**
     * <p>
     * Converts a long into an array of bytes using the default little-endian byte ordering.
     * </p>
     *
     * @param l The value to convert.
     * @return
     */
    public static byte[] toByteArray(long l) {
        return new byte[] {
                (byte) ((l >> 0) & 0xFFL),
                (byte) ((l >> 8) & 0xFFL),
                (byte) ((l >> 16) & 0xFFL),
                (byte) ((l >> 24) & 0xFFL),
                (byte) ((l >> 32) & 0xFFL),
                (byte) ((l >> 40) & 0xFFL),
                (byte) ((l >> 48) & 0xFFL),
                (byte) ((l >> 56) & 0xFFL)
        };
    }

    /**
     * <p>
     * Converts a long into an array of bytes using big-endian byte ordering.
     * </p>
     *
     * @param l The value to convert.
     * @return
     */
    public static byte[] toByteArrayBe(long l) {
        return new byte[] {
                (byte) ((l >> 56) & 0xFFL),
                (byte) ((l >> 48) & 0xFFL),
                (byte) ((l >> 40) & 0xFFL),
                (byte) ((l >> 32) & 0xFFL),
                (byte) ((l >> 24) & 0xFFL),
                (byte) ((l >> 16) & 0xFFL),
                (byte) ((l >> 8) & 0xFFL),
                (byte) ((l >> 0) & 0xFFL)
        };
    }

    /**
     * <p>
     * Converts an array of bytes into a long using the default little-endian byte ordering.
     * </p>
     *
     * @param inputBytes The bytes to convert.
     * @return
     */
    public static long toLong(byte[] inputBytes) {
        if (inputBytes == null || inputBytes.length == 0) {
            return 0;
        }
        final long SIZE = LONG_BYTES;
        if (inputBytes.length > SIZE) {
            throw new IllegalArgumentException("Input data is longer than size of 'long' primitive ("+SIZE+" bytes)");
        } else if (inputBytes.length < SIZE) {
            throw new IllegalArgumentException("Input data is smaller than size of 'long' primitive ("+SIZE+" bytes)");
        }
        return (long) (
                ( (((long) inputBytes[0]) & 0xFFL) << 0) |
                ( (((long) inputBytes[1]) & 0xFFL) << 8) |
                ( (((long) inputBytes[2]) & 0xFFL) << 16) |
                ( (((long) inputBytes[3]) & 0xFFL) << 24) |
                ( (((long) inputBytes[4]) & 0xFFL) << 32) |
                ( (((long) inputBytes[5]) & 0xFFL) << 40) |
                ( (((long) inputBytes[6]) & 0xFFL) << 48) |
                ( (((long) inputBytes[7]) & 0xFFL) << 56)
        );
    }

    /**
     * <p>
     * Converts an array of bytes into a long using big-endian byte ordering.
     * </p>
     *
     * @param inputBytes The bytes to convert.
     * @return
     */
    public static long toLongBe(byte[] inputBytes) {
        if (inputBytes == null || inputBytes.length == 0) {
            return 0;
        }
        final long SIZE = LONG_BYTES;
        if (inputBytes.length > SIZE) {
            throw new IllegalArgumentException("Input data is longer than size of 'long' primitive ("+SIZE+" bytes)");
        } else if (inputBytes.length < SIZE) {
            throw new IllegalArgumentException("Input data is smaller than size of 'long' primitive ("+SIZE+" bytes)");
        }
        return (long) (
                ( (((long) inputBytes[0]) & 0xFFL) << 56) |
                ( (((long) inputBytes[1]) & 0xFFL) << 48) |
                ( (((long) inputBytes[2]) & 0xFFL) << 40) |
                ( (((long) inputBytes[3]) & 0xFFL) << 32) |
                ( (((long) inputBytes[4]) & 0xFFL) << 24) |
                ( (((long) inputBytes[5]) & 0xFFL) << 16) |
                ( (((long) inputBytes[6]) & 0xFFL) << 8) |
                ( (((long) inputBytes[7]) & 0xFFL) << 0)
        );
    }

    /**
     * <p>
     * Converts a float into an array of bytes using the default little-endian byte ordering.
     * </p>
     *
     * @param f The value to convert.
     * @return
     */
    public static byte[] toByteArray(float f) {
        return toByteArray(Float.floatToIntBits(f));
    }

    /**
     * <p>
     * Converts a float into an array of bytes using big-endian byte ordering.
     * </p>
     *
     * @param f The value to convert.
     * @return
     */
    public static byte[] toByteArrayBe(float f) {
        return toByteArrayBe(Float.floatToIntBits(f));
    }

    /**
     * <p>
     * Converts an array of bytes into a float using the default little-endian byte ordering.
     * </p>
     *
     * @param inputBytes The bytes to convert.
     * @return
     */
    public static float toFloat(byte[] inputBytes) {
        if (inputBytes == null || inputBytes.length == 0) {
            return 0;
        }
        return Float.intBitsToFloat(toInt(inputBytes));
    }

    /**
     * <p>
     * Converts an array of bytes into a float using big-endian byte ordering.
     * </p>
     *
     * @param inputBytes The bytes to convert.
     * @return
     */
    public static float toFloatBe(byte[] inputBytes) {
        if (inputBytes == null || inputBytes.length == 0) {
            return 0;
        }
        return Float.intBitsToFloat(toIntBe(inputBytes));
    }

    /**
     * <p>
     * Converts a double into an array of bytes using the default little-endian byte ordering.
     * </p>
     *
     * @param d The value to convert.
     * @return
     */
    public static byte[] toByteArray(double d) {
        return toByteArray(Double.doubleToLongBits(d));
    }

    /**
     * <p>
     * Converts a double into an array of bytes using big-endian byte ordering.
     * </p>
     *
     * @param d The value to convert.
     * @return
     */
    public static byte[] toByteArrayBe(double d) {
        return toByteArrayBe(Double.doubleToLongBits(d));
    }

    /**
     * <p>
     * Converts an array of bytes into a double using the default little-endian byte ordering.
     * </p>
     *
     * @param inputBytes The bytes to convert.
     * @return
     */
    public static double toDouble(byte[] inputBytes) {
        if (inputBytes == null || inputBytes.length == 0) {
            return 0;
        }
        return Double.longBitsToDouble(toLong(inputBytes));
    }

    /**
     * <p>
     * Converts an array of bytes into a double using big-endian byte ordering.
     * </p>
     *
     * @param inputBytes The bytes to convert.
     * @return
     */
    public static double toDoubleBe(byte[] inputBytes) {
        if (inputBytes == null || inputBytes.length == 0) {
            return 0;
        }
        return Double.longBitsToDouble(toLongBe(inputBytes));
    }

    /**
     * A lookup table for converting integer values from 0 to 15 into a single hex digit (char).
     * Value: ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F']
     */
    public static final char[] SINGLE_DIGIT_HEX_LOOKUP_TABLE = new char[] {'0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'};

    /**
     * A lookup table for converting integer values from 0 to 255 into a 2 digit hex String.
     * Value: ["00","01","02","03", ... "38","39","3A","3B","3C", ... "9E","9F","A0","A1","A2", ..."FD","FE","FF"]
     */
    public static final String[] DOUBLE_DIGIT_HEX_LOOKUP_TABLE;

    /**
     * A lookup table for converting a 2 digit hex String into an integer value from 0 to 255.
     * Value:<br>
     * [
     * "00":0,
     * "01":1,
     * "02":2,
     * "03":3,
     * ...
     * "38":56,
     * "39":57,
     * "3A":58,
     * "3B":59,
     * "3C":60,
     * ...
     * "9E":158,
     * "9F":159,
     * "A0":160,
     * "A1":161,
     * "A2":162,
     * ...
     * "FD":253,
     * "FE":254,
     * "FF":255
     * ]
     */
    public static final Map<String, Byte> DOUBLE_DIGIT_HEX_REVERSE_LOOKUP_TABLE;
    static {
        //Init Forward Lookup
        DOUBLE_DIGIT_HEX_LOOKUP_TABLE = new String[256];
        int index = 0;
        for (int x=0; x<16; x++) {
            for (int y=0; y<16; y++) {
                DOUBLE_DIGIT_HEX_LOOKUP_TABLE[index++] = "" + SINGLE_DIGIT_HEX_LOOKUP_TABLE[x] + SINGLE_DIGIT_HEX_LOOKUP_TABLE[y];
            }
        }

        //Init Reverse Lookup
        DOUBLE_DIGIT_HEX_REVERSE_LOOKUP_TABLE = new HashMap<String, Byte>(256);
        for (int x=0; x<256; x++) {
            DOUBLE_DIGIT_HEX_REVERSE_LOOKUP_TABLE.put(DOUBLE_DIGIT_HEX_LOOKUP_TABLE[x], (byte)x);
        }
    }

    /**
     * Converts a byte's unsigned value to the equivalent 2 digit hex value.
     *
     * @param b The byte to convert.
     * @return
     */
    public static String toHex(byte b) {
        return DOUBLE_DIGIT_HEX_LOOKUP_TABLE[b & 0xFF];
    }

    /**
     * Splits a short into its constituent bytes (using little-endian byte ordering), and
     * then converts each of these bytes (using their unsigned value) into the equivalent 2
     * digit hex value.
     *
     * @param s The short to convert.
     * @return
     */
    public static String toHex(short s) {
        byte[] chunks = toByteArray(s);
        return toHexRaw(chunks);
    }

    /**
     * Splits a short into its constituent bytes (using big-endian byte ordering), and
     * then converts each of these bytes (using their unsigned value) into the equivalent 2
     * digit hex value.
     *
     * @param s The short to convert.
     * @return
     */
    public static String toHexBe(short s) {
        byte[] chunks = toByteArrayBe(s);
        return toHexRaw(chunks);
    }

    /**
     * Splits an int into its constituent bytes (using little-endian byte ordering), and
     * then converts each of these bytes (using their unsigned value) into the equivalent 2
     * digit hex value.
     *
     * @param i The int to convert.
     * @return
     */
    public static String toHex(int i) {
        byte[] chunks = toByteArray(i);
        return toHexRaw(chunks);
    }

    /**
     * Splits an int into its constituent bytes (using big-endian byte ordering), and
     * then converts each of these bytes (using their unsigned value) into the equivalent 2
     * digit hex value.
     *
     * @param i The int to convert.
     * @return
     */
    public static String toHexBe(int i) {
        byte[] chunks = toByteArrayBe(i);
        return toHexRaw(chunks);
    }

    /**
     * Splits a long into its constituent bytes (using little-endian byte ordering), and
     * then converts each of these bytes (using their unsigned value) into the equivalent 2
     * digit hex value.
     *
     * @param l The long to convert.
     * @return
     */
    public static String toHex(long l) {
        byte[] chunks = toByteArray(l);
        return toHexRaw(chunks);
    }

    /**
     * Splits a long into its constituent bytes (using big-endian byte ordering), and
     * then converts each of these bytes (using their unsigned value) into the equivalent 2
     * digit hex value.
     *
     * @param l The long to convert.
     * @return
     */
    public static String toHexBe(long l) {
        byte[] chunks = toByteArrayBe(l);
        return toHexRaw(chunks);
    }

    /**
     * Splits a float into its constituent bytes (using little-endian byte ordering), and
     * then converts each of these bytes (using their unsigned value) into the equivalent 2
     * digit hex value.
     *
     * @param f The float to convert.
     * @return
     */
    public static String toHex(float f) {
        byte[] chunks = toByteArray(f);
        return toHexRaw(chunks);
    }

    /**
     * Splits a float into its constituent bytes (using big-endian byte ordering), and
     * then converts each of these bytes (using their unsigned value) into the equivalent 2
     * digit hex value.
     *
     * @param f The float to convert.
     * @return
     */
    public static String toHexBe(float f) {
        byte[] chunks = toByteArrayBe(f);
        return toHexRaw(chunks);
    }

    /**
     * Splits a double into its constituent bytes (using little-endian byte ordering), and
     * then converts each of these bytes (using their unsigned value) into the equivalent 2
     * digit hex value.
     *
     * @param d The double to convert.
     * @return
     */
    public static String toHex(double d) {
        byte[] chunks = toByteArray(d);
        return toHexRaw(chunks);
    }

    /**
     * Splits a double into its constituent bytes (using big-endian byte ordering), and
     * then converts each of these bytes (using their unsigned value) into the equivalent 2
     * digit hex value.
     *
     * @param d The double to convert.
     * @return
     */
    public static String toHexBe(double d) {
        byte[] chunks = toByteArrayBe(d);
        return toHexRaw(chunks);
    }

    /**
     * Converts an array of bytes into a String of hexadecimal octets (2 digit hex grouping).
     * Each byte is converted using its unsigned value into the equivalent 2 digit hex value.
     *
     * @param chunks The byte to convert.
     * @return
     */
    public static String toHexRaw(byte[] chunks) {
        if (chunks == null || chunks.length == 0) {
            return "00";
        }
        StringBuilder builder = new StringBuilder();
        for (int x=0; x<chunks.length; x++) {
            builder.append(DOUBLE_DIGIT_HEX_LOOKUP_TABLE[chunks[x] & 0xFF]);
        }
        return builder.toString();
    }

    /**
     * <p>
     * Converts a String of hexadecimal octets (2 digit hex grouping) into an array of bytes.
     * Each octet is converted into its equivalent unsigned byte value.
     * </p>
     *
     * <p>
     * Note: there is no limit on the input length. You may break up the hexString using any of
     * the following delimiters: (delimiters are ignored and have no impact on the output)<br>
     * '-' [hyphen]<br>
     * '_' [underscore]<br>
     * '.' [period]<br>
     * ' ' [space]<br>
     * '\t' [tab]
     * </p>
     *
     * @param chunks The byte to convert.
     * @return
     */
    public static byte[] hexToByteArrayRaw(final String hexString) {
        //Check Null
        if (hexString == null) {
            return new byte[0];
        }

        //Check Empty (no delimiters)
        final String cleanBitString = stripStringDelimiters(hexString);
        if (cleanBitString.length() == 0) {
            return new byte[0];
        }

        //Ensure even number of octets
        char[] digits = cleanBitString.toCharArray();
        int totalLen = digits.length;
        if (totalLen % 2 != 0 ) {
            throw new IllegalArgumentException("Hex String is an odd number of hex digits. Input data must be an even number of hex digits.");
        }

        //Do Conversion
        int numOctets = totalLen / 2;
        byte[] outBytes = new byte[numOctets];
        int index = 0;
        for (int x=0; x<numOctets; x++) {
            byte val = DOUBLE_DIGIT_HEX_REVERSE_LOOKUP_TABLE.get(
                new String(new char[]{
                    digits[index++],
                    digits[index++]
                })
            );
            outBytes[x] = val;
        }
        return outBytes;
    }

    /**
     * <p>
     * Converts a String of hexadecimal octets (2 digit hex grouping) into a short
     * using little-endian byte ordering. Each octet is converted into its equivalent
     * unsigned byte value.
     * </p>
     *
     * <p>
     * Note: A short is composed of 2 octets (4 Hex Digits). You may break up the
     * hexString using any of the following delimiters: (delimiters are ignored and
     * have no impact on the output)<br>
     * '-' [hyphen]<br>
     * '_' [underscore]<br>
     * '.' [period]<br>
     * ' ' [space]<br>
     * '\t' [tab]
     * </p>
     *
     * @param hexString The String of hexadecimal octets to convert.
     * @return
     */
    public static short hexToShort(String hexString) {
        //Convert to Bytes
        byte[] inputBytes = hexToByteArrayRaw(hexString);
        if (inputBytes.length == 0) {
            return 0;
        }

        //Check Proper Length
        final long SIZE = SHORT_BYTES;
        if (inputBytes.length > SIZE) {
            throw new IllegalArgumentException("Input data is longer than size of 'short' primitive ("+SIZE+" bytes)");
        } else if (inputBytes.length < SIZE) {
            throw new IllegalArgumentException("Input data is smaller than size of 'short' primitive ("+SIZE+" bytes)");
        }

        //Do conversion
        return toShort(inputBytes);
    }

    /**
     * <p>
     * Converts a String of hexadecimal octets (2 digit hex grouping) into a short
     * using big-endian byte ordering. Each octet is converted into its equivalent
     * unsigned byte value.
     * </p>
     *
     * <p>
     * Note: A short is composed of 2 octets (4 Hex Digits). You may break up the
     * hexString using any of the following delimiters: (delimiters are ignored and
     * have no impact on the output)<br>
     * '-' [hyphen]<br>
     * '_' [underscore]<br>
     * '.' [period]<br>
     * ' ' [space]<br>
     * '\t' [tab]
     * </p>
     *
     * @param hexString The String of hexadecimal octets to convert.
     * @return
     */
    public static short hexToShortBe(String hexString) {
        //Convert to Bytes
        byte[] inputBytes = hexToByteArrayRaw(hexString);
        if (inputBytes.length == 0) {
            return 0;
        }

        //Check Proper Length
        final long SIZE = SHORT_BYTES;
        if (inputBytes.length > SIZE) {
            throw new IllegalArgumentException("Input data is longer than size of 'short' primitive ("+SIZE+" bytes)");
        } else if (inputBytes.length < SIZE) {
            throw new IllegalArgumentException("Input data is smaller than size of 'short' primitive ("+SIZE+" bytes)");
        }

        //Do conversion
        return toShortBe(inputBytes);
    }

    /**
     * <p>
     * Converts a String of hexadecimal octets (2 digit hex grouping) into an int
     * using little-endian byte ordering. Each octet is converted into its equivalent
     * unsigned byte value.
     * </p>
     *
     * <p>
     * Note: An int is composed of 4 octets (8 Hex Digits). You may break up the
     * hexString using any of the following delimiters: (delimiters are ignored and
     * have no impact on the output)<br>
     * '-' [hyphen]<br>
     * '_' [underscore]<br>
     * '.' [period]<br>
     * ' ' [space]<br>
     * '\t' [tab]
     * </p>
     *
     * @param hexString The String of hexadecimal octets to convert.
     * @return
     */
    public static int hexToInt(String hexString) {
        //Convert to Bytes
        byte[] inputBytes = hexToByteArrayRaw(hexString);
        if (inputBytes.length == 0) {
            return 0;
        }

        //Check Proper Length
        final long SIZE = INT_BYTES;
        if (inputBytes.length > SIZE) {
            throw new IllegalArgumentException("Input data is longer than size of 'int' primitive ("+SIZE+" bytes)");
        } else if (inputBytes.length < SIZE) {
            throw new IllegalArgumentException("Input data is smaller than size of 'int' primitive ("+SIZE+" bytes)");
        }

        //Do conversion
        return toInt(inputBytes);
    }

    /**
     * <p>
     * Converts a String of hexadecimal octets (2 digit hex grouping) into an int
     * using big-endian byte ordering. Each octet is converted into its equivalent
     * unsigned byte value.
     * </p>
     *
     * <p>
     * Note: An int is composed of 4 octets (8 Hex Digits). You may break up the
     * hexString using any of the following delimiters: (delimiters are ignored and
     * have no impact on the output)<br>
     * '-' [hyphen]<br>
     * '_' [underscore]<br>
     * '.' [period]<br>
     * ' ' [space]<br>
     * '\t' [tab]
     * </p>
     *
     * @param hexString The String of hexadecimal octets to convert.
     * @return
     */
    public static int hexToIntBe(String hexString) {
        //Convert to Bytes
        byte[] inputBytes = hexToByteArrayRaw(hexString);
        if (inputBytes.length == 0) {
            return 0;
        }

        //Check Proper Length
        final long SIZE = INT_BYTES;
        if (inputBytes.length > SIZE) {
            throw new IllegalArgumentException("Input data is longer than size of 'int' primitive ("+SIZE+" bytes)");
        } else if (inputBytes.length < SIZE) {
            throw new IllegalArgumentException("Input data is smaller than size of 'int' primitive ("+SIZE+" bytes)");
        }

        //Do conversion
        return toIntBe(inputBytes);
    }

    /**
     * <p>
     * Converts a String of hexadecimal octets (2 digit hex grouping) into a long
     * using little-endian byte ordering. Each octet is converted into its equivalent
     * unsigned byte value.
     * </p>
     *
     * <p>
     * Note: A long is composed of 8 octets (16 Hex Digits). You may break up the
     * hexString using any of the following delimiters: (delimiters are ignored and
     * have no impact on the output)<br>
     * '-' [hyphen]<br>
     * '_' [underscore]<br>
     * '.' [period]<br>
     * ' ' [space]<br>
     * '\t' [tab]
     * </p>
     *
     * @param hexString The String of hexadecimal octets to convert.
     * @return
     */
    public static long hexToLong(String hexString) {
        //Convert to Bytes
        byte[] inputBytes = hexToByteArrayRaw(hexString);
        if (inputBytes.length == 0) {
            return 0;
        }

        //Check Proper Length
        final long SIZE = LONG_BYTES;
        if (inputBytes.length > SIZE) {
            throw new IllegalArgumentException("Input data is longer than size of 'long' primitive ("+SIZE+" bytes)");
        } else if (inputBytes.length < SIZE) {
            throw new IllegalArgumentException("Input data is smaller than size of 'long' primitive ("+SIZE+" bytes)");
        }

        //Do conversion
        return toLong(inputBytes);
    }

    /**
     * <p>
     * Converts a String of hexadecimal octets (2 digit hex grouping) into a long
     * using big-endian byte ordering. Each octet is converted into its equivalent
     * unsigned byte value.
     * </p>
     *
     * <p>
     * Note: A long is composed of 8 octets (16 Hex Digits). You may break up the
     * hexString using any of the following delimiters: (delimiters are ignored and
     * have no impact on the output)<br>
     * '-' [hyphen]<br>
     * '_' [underscore]<br>
     * '.' [period]<br>
     * ' ' [space]<br>
     * '\t' [tab]
     * </p>
     *
     * @param hexString The String of hexadecimal octets to convert.
     * @return
     */
    public static long hexToLongBe(String hexString) {
        //Convert to Bytes
        byte[] inputBytes = hexToByteArrayRaw(hexString);
        if (inputBytes.length == 0) {
            return 0;
        }

        //Check Proper Length
        final long SIZE = LONG_BYTES;
        if (inputBytes.length > SIZE) {
            throw new IllegalArgumentException("Input data is longer than size of 'long' primitive ("+SIZE+" bytes)");
        } else if (inputBytes.length < SIZE) {
            throw new IllegalArgumentException("Input data is smaller than size of 'long' primitive ("+SIZE+" bytes)");
        }

        //Do conversion
        return toLongBe(inputBytes);
    }

    /**
     * <p>
     * Converts a String of hexadecimal octets (2 digit hex grouping) into a float
     * using little-endian byte ordering. Each octet is converted into its equivalent
     * unsigned byte value.
     * </p>
     *
     * <p>
     * Note: A float is composed of 4 octets (8 Hex Digits). You may break up the
     * hexString using any of the following delimiters: (delimiters are ignored and
     * have no impact on the output)<br>
     * '-' [hyphen]<br>
     * '_' [underscore]<br>
     * '.' [period]<br>
     * ' ' [space]<br>
     * '\t' [tab]
     * </p>
     *
     * @param hexString The String of hexadecimal octets to convert.
     * @return
     */
    public static float hexToFloat(String hexString) {
        //Convert to Bytes
        byte[] inputBytes = hexToByteArrayRaw(hexString);
        if (inputBytes.length == 0) {
            return 0;
        }

        //Check Proper Length
        final float SIZE = FLOAT_BYTES;
        if (inputBytes.length > SIZE) {
            throw new IllegalArgumentException("Input data is floater than size of 'float' primitive ("+SIZE+" bytes)");
        } else if (inputBytes.length < SIZE) {
            throw new IllegalArgumentException("Input data is smaller than size of 'float' primitive ("+SIZE+" bytes)");
        }

        //Do conversion
        return toFloat(inputBytes);
    }

    /**
     * <p>
     * Converts a String of hexadecimal octets (2 digit hex grouping) into a float
     * using big-endian byte ordering. Each octet is converted into its equivalent
     * unsigned byte value.
     * </p>
     *
     * <p>
     * Note: A float is composed of 4 octets (8 Hex Digits). You may break up the
     * hexString using any of the following delimiters: (delimiters are ignored and
     * have no impact on the output)<br>
     * '-' [hyphen]<br>
     * '_' [underscore]<br>
     * '.' [period]<br>
     * ' ' [space]<br>
     * '\t' [tab]
     * </p>
     *
     * @param hexString The String of hexadecimal octets to convert.
     * @return
     */
    public static float hexToFloatBe(String hexString) {
        //Convert to Bytes
        byte[] inputBytes = hexToByteArrayRaw(hexString);
        if (inputBytes.length == 0) {
            return 0;
        }

        //Check Proper Length
        final float SIZE = FLOAT_BYTES;
        if (inputBytes.length > SIZE) {
            throw new IllegalArgumentException("Input data is floater than size of 'float' primitive ("+SIZE+" bytes)");
        } else if (inputBytes.length < SIZE) {
            throw new IllegalArgumentException("Input data is smaller than size of 'float' primitive ("+SIZE+" bytes)");
        }

        //Do conversion
        return toFloatBe(inputBytes);
    }

    /**
     * <p>
     * Converts a String of hexadecimal octets (2 digit hex grouping) into a double
     * using little-endian byte ordering. Each octet is converted into its equivalent
     * unsigned byte value.
     * </p>
     *
     * <p>
     * Note: A double is composed of 8 octets (16 Hex Digits). You may break up the
     * hexString using any of the following delimiters: (delimiters are ignored and
     * have no impact on the output)<br>
     * '-' [hyphen]<br>
     * '_' [underscore]<br>
     * '.' [period]<br>
     * ' ' [space]<br>
     * '\t' [tab]
     * </p>
     *
     * @param hexString The String of hexadecimal octets to convert.
     * @return
     */
    public static double hexToDouble(String hexString) {
        //Convert to Bytes
        byte[] inputBytes = hexToByteArrayRaw(hexString);
        if (inputBytes.length == 0) {
            return 0;
        }

        //Check Proper Length
        final double SIZE = DOUBLE_BYTES;
        if (inputBytes.length > SIZE) {
            throw new IllegalArgumentException("Input data is doubleer than size of 'double' primitive ("+SIZE+" bytes)");
        } else if (inputBytes.length < SIZE) {
            throw new IllegalArgumentException("Input data is smaller than size of 'double' primitive ("+SIZE+" bytes)");
        }

        //Do conversion
        return toDouble(inputBytes);
    }

    /**
     * <p>
     * Converts a String of hexadecimal octets (2 digit hex grouping) into a double
     * using big-endian byte ordering. Each octet is converted into its equivalent
     * unsigned byte value.
     * </p>
     *
     * <p>
     * Note: A double is composed of 8 octets (16 Hex Digits). You may break up the
     * hexString using any of the following delimiters: (delimiters are ignored and
     * have no impact on the output)<br>
     * '-' [hyphen]<br>
     * '_' [underscore]<br>
     * '.' [period]<br>
     * ' ' [space]<br>
     * '\t' [tab]
     * </p>
     *
     * @param hexString The String of hexadecimal octets to convert.
     * @return
     */
    public static double hexToDoubleBe(String hexString) {
        //Convert to Bytes
        byte[] inputBytes = hexToByteArrayRaw(hexString);
        if (inputBytes.length == 0) {
            return 0;
        }

        //Check Proper Length
        final double SIZE = DOUBLE_BYTES;
        if (inputBytes.length > SIZE) {
            throw new IllegalArgumentException("Input data is doubleer than size of 'double' primitive ("+SIZE+" bytes)");
        } else if (inputBytes.length < SIZE) {
            throw new IllegalArgumentException("Input data is smaller than size of 'double' primitive ("+SIZE+" bytes)");
        }

        //Do conversion
        return toDoubleBe(inputBytes);
    }
}
