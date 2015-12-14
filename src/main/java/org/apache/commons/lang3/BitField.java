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

/**
 * <p>Supports operations on bit-mapped fields. Instances of this class can be 
 * used to store a flag or data within an {@code int}, {@code short} or 
 * {@code byte}.</p>
 * 
 * <p>Each {@code BitField} is constructed with a mask value, which indicates
 * the bits that will be used to store and retrieve the data for that field. 
 * For instance, the mask {@code 0xFF} indicates the least-significant byte 
 * should be used to store the data.</p>
 * 
 * <p>As an example, consider a car painting machine that accepts
 * paint instructions as integers. Bit fields can be used to encode this:</p>
 *
 *<pre>
 *    // blue, green and red are 1 byte values (0-255) stored in the three least 
 *    // significant bytes
 *    BitField blue = new BitField(0xFF);
 *    BitField green = new BitField(0xFF00);
 *    BitField red = new BitField(0xFF0000);
 * 
 *    // anyColor is a flag triggered if any color is used
 *    BitField anyColor = new BitField(0xFFFFFF);
 * 
 *    // isMetallic is a single bit flag
 *    BitField isMetallic = new BitField(0x1000000);
 *</pre>
 *
 * <p>Using these {@code BitField} instances, a paint instruction can be
 * encoded into an integer:</p>
 *
 *<pre>
 *    int paintInstruction = 0;
 *    paintInstruction = red.setValue(paintInstruction, 35);
 *    paintInstruction = green.setValue(paintInstruction, 100);
 *    paintInstruction = blue.setValue(paintInstruction, 255);
 *</pre>
 *
 * <p>Flags and data can be retrieved from the integer:</p>
 * 
 *<pre>
 *    // Prints true if red, green or blue is non-zero
 *    System.out.println(anyColor.isSet(paintInstruction));   // prints true
 *   
 *    // Prints value of red, green and blue
 *    System.out.println(red.getValue(paintInstruction));     // prints 35
 *    System.out.println(green.getValue(paintInstruction));   // prints 100
 *    System.out.println(blue.getValue(paintInstruction));    // prints 255
 *   
 *    // Prints true if isMetallic was set 
 *    System.out.println(isMetallic.isSet(paintInstruction)); // prints false
 *</pre>
 *
 * @since 2.0
 */
public class BitField {
    
    private final int _mask;
    private final int _shift_count;

    /**
     * <p>Creates a BitField instance.</p>
     *
     * @param mask the mask specifying which bits apply to this
     *  BitField. Bits that are set in this mask are the bits
     *  that this BitField operates on
     */
    public BitField(final int mask) {
        _mask = mask;
        _shift_count = mask != 0 ? Integer.numberOfTrailingZeros(mask) : 0;
    }

    /**
     * <p>Obtains the value for the specified BitField, appropriately
     * shifted right.</p>
     *
     * <p>Many users of a BitField will want to treat the specified
     * bits as an int value, and will not want to be aware that the
     * value is stored as a BitField (and so shifted left so many
     * bits).</p>
     *
     * @see #setValue(int,int)
     * @param holder the int data containing the bits we're interested
     *  in
     * @return the selected bits, shifted right appropriately
     */
    public int getValue(final int holder) {
        return getRawValue(holder) >> _shift_count;
    }

    /**
     * <p>Obtains the value for the specified BitField, appropriately
     * shifted right, as a short.</p>
     *
     * <p>Many users of a BitField will want to treat the specified
     * bits as an int value, and will not want to be aware that the
     * value is stored as a BitField (and so shifted left so many
     * bits).</p>
     *
     * @see #setShortValue(short,short)
     * @param holder the short data containing the bits we're
     *  interested in
     * @return the selected bits, shifted right appropriately
     */
    public short getShortValue(final short holder) {
        return (short) getValue(holder);
    }

    /**
     * <p>Obtains the value for the specified BitField, unshifted.</p>
     *
     * @param holder the int data containing the bits we're
     *  interested in
     * @return the selected bits
     */
    public int getRawValue(final int holder) {
        return holder & _mask;
    }

    /**
     * <p>Obtains the value for the specified BitField, unshifted.</p>
     *
     * @param holder the short data containing the bits we're
     *  interested in
     * @return the selected bits
     */
    public short getShortRawValue(final short holder) {
        return (short) getRawValue(holder);
    }

    /**
     * <p>Returns whether the field is set or not.</p>
     *
     * <p>This is most commonly used for a single-bit field, which is
     * often used to represent a boolean value; the results of using
     * it for a multi-bit field is to determine whether *any* of its
     * bits are set.</p>
     *
     * @param holder the int data containing the bits we're interested
     *  in
     * @return {@code true} if any of the bits are set,
     *  else {@code false}
     */
    public boolean isSet(final int holder) {
        return (holder & _mask) != 0;
    }

    /**
     * <p>Returns whether all of the bits are set or not.</p>
     *
     * <p>This is a stricter test than {@link #isSet(int)},
     * in that all of the bits in a multi-bit set must be set
     * for this method to return {@code true}.</p>
     *
     * @param holder the int data containing the bits we're
     *  interested in
     * @return {@code true} if all of the bits are set,
     *  else {@code false}
     */
    public boolean isAllSet(final int holder) {
        return (holder & _mask) == _mask;
    }

    /**
     * <p>Replaces the bits with new values.</p>
     *
     * @see #getValue(int)
     * @param holder the int data containing the bits we're
     *  interested in
     * @param value the new value for the specified bits
     * @return the value of holder with the bits from the value
     *  parameter replacing the old bits
     */
    public int setValue(final int holder, final int value) {
        return (holder & ~_mask) | ((value << _shift_count) & _mask);
    }

    /**
     * <p>Replaces the bits with new values.</p>
     *
     * @see #getShortValue(short)
     * @param holder the short data containing the bits we're
     *  interested in
     * @param value the new value for the specified bits
     * @return the value of holder with the bits from the value
     *  parameter replacing the old bits
     */
    public short setShortValue(final short holder, final short value) {
        return (short) setValue(holder, value);
    }

    /**
     * <p>Clears the bits.</p>
     *
     * @param holder the int data containing the bits we're
     *  interested in
     * @return the value of holder with the specified bits cleared
     *  (set to {@code 0})
     */
    public int clear(final int holder) {
        return holder & ~_mask;
    }

    /**
     * <p>Clears the bits.</p>
     *
     * @param holder the short data containing the bits we're
     *  interested in
     * @return the value of holder with the specified bits cleared
     *  (set to {@code 0})
     */
    public short clearShort(final short holder) {
        return (short) clear(holder);
    }

    /**
     * <p>Clears the bits.</p>
     *
     * @param holder the byte data containing the bits we're
     *  interested in
     *
     * @return the value of holder with the specified bits cleared
     *  (set to {@code 0})
     */
    public byte clearByte(final byte holder) {
        return (byte) clear(holder);
    }

    /**
     * <p>Sets the bits.</p>
     *
     * @param holder the int data containing the bits we're
     *  interested in
     * @return the value of holder with the specified bits set
     *  to {@code 1}
     */
    public int set(final int holder) {
        return holder | _mask;
    }

    /**
     * <p>Sets the bits.</p>
     *
     * @param holder the short data containing the bits we're
     *  interested in
     * @return the value of holder with the specified bits set
     *  to {@code 1}
     */
    public short setShort(final short holder) {
        return (short) set(holder);
    }

    /**
     * <p>Sets the bits.</p>
     *
     * @param holder the byte data containing the bits we're
     *  interested in
     *
     * @return the value of holder with the specified bits set
     *  to {@code 1}
     */
    public byte setByte(final byte holder) {
        return (byte) set(holder);
    }

    /**
     * <p>Sets a boolean BitField.</p>
     *
     * @param holder the int data containing the bits we're
     *  interested in
     * @param flag indicating whether to set or clear the bits
     * @return the value of holder with the specified bits set or
     *         cleared
     */
    public int setBoolean(final int holder, final boolean flag) {
        return flag ? set(holder) : clear(holder);
    }

    /**
     * <p>Sets a boolean BitField.</p>
     *
     * @param holder the short data containing the bits we're
     *  interested in
     * @param flag indicating whether to set or clear the bits
     * @return the value of holder with the specified bits set or
     *  cleared
     */
    public short setShortBoolean(final short holder, final boolean flag) {
        return flag ? setShort(holder) : clearShort(holder);
    }

    /**
     * <p>Sets a boolean BitField.</p>
     *
     * @param holder the byte data containing the bits we're
     *  interested in
     * @param flag indicating whether to set or clear the bits
     * @return the value of holder with the specified bits set or
     *  cleared
     */
    public byte setByteBoolean(final byte holder, final boolean flag) {
        return flag ? setByte(holder) : clearByte(holder);
    }

}
