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

/**
 * Supports operations on bit-mapped fields. Instances of this class can be used to store a flag or data within an {@code int}, {@code short} or {@code byte}.
 * <p>
 * Each {@link BitField} is constructed with a mask value, which indicates the bits that will be used to store and retrieve the data for that field. For
 * instance, the mask {@code 0xFF} indicates the least-significant byte should be used to store the data.
 * </p>
 * <p>
 * As an example, consider a car painting machine that accepts paint instructions as integers. Bit fields can be used to encode this:
 * </p>
 *
 * <pre>
 *
 * // blue, green and red are 1 byte values (0-255) stored in the three least
 * // significant bytes
 * BitField blue = new BitField(0xFF);
 *
 * BitField green = new BitField(0xFF00);
 *
 * BitField red = new BitField(0xFF0000);
 *
 * // anyColor is a flag triggered if any color is used
 * BitField anyColor = new BitField(0xFFFFFF);
 *
 * // isMetallic is a single bit flag
 * BitField isMetallic = new BitField(0x1000000);
 * </pre>
 * <p>
 * Using these {@link BitField} instances, a paint instruction can be encoded into an integer:
 * </p>
 *
 * <pre>
 * int paintInstruction = 0;
 * paintInstruction = red.setValue(paintInstruction, 35);
 * paintInstruction = green.setValue(paintInstruction, 100);
 * paintInstruction = blue.setValue(paintInstruction, 255);
 * </pre>
 * <p>
 * Flags and data can be retrieved from the integer:
 * </p>
 *
 * <pre>
 * // Prints true if red, green or blue is non-zero
 * System.out.println(anyColor.isSet(paintInstruction)); // prints true
 * // Prints value of red, green and blue
 * System.out.println(red.getValue(paintInstruction)); // prints 35
 * System.out.println(green.getValue(paintInstruction)); // prints 100
 * System.out.println(blue.getValue(paintInstruction)); // prints 255
 * // Prints true if isMetallic was set
 * System.out.println(isMetallic.isSet(paintInstruction)); // prints false
 * </pre>
 *
 * @since 2.0
 */
public class BitField {

    private final long mask;

    private final int shiftCount;

    /**
     * Creates a BitField instance.
     *
     * @param mask The mask specifying which bits apply to this BitField. Bits that are set in this mask are the bits that this BitField operates on.
     */
    public BitField(final int mask) {
        this.mask = Integer.toUnsignedLong(mask);
        this.shiftCount = this.mask == 0 ? 0 : Long.numberOfTrailingZeros(this.mask);
    }

    /**
     * Creates a BitField instance.
     * <p>
     * If any bit above bit 31 is set in the mask, the resulting field can only be used with the {@code long} holder accessors; the {@code int}, {@code short}
     * and {@code byte} holder accessors throw {@link IllegalStateException} for such a field, because those holder types cannot contain the masked bits and
     * would otherwise silently answer wrongly (shift counts are truncated mod 32 and negative holders are sign-extended into bits 32-63).
     * </p>
     *
     * @param mask The mask specifying which bits apply to this BitField. Bits that are set in this mask are the bits that this BitField operates on.
     * @since 3.21.0
     */
    public BitField(final long mask) {
        this.mask = mask;
        this.shiftCount = mask == 0 ? 0 : Long.numberOfTrailingZeros(mask);
    }

    /**
     * Clears the bits.
     *
     * @param holder The int data containing the bits we're interested in.
     * @return The value of holder with the specified bits cleared (set to {@code 0}).
     * @throws IllegalStateException Thrown if this field's mask has bits set above bit 31 (only possible via {@link #BitField(long)}) and so cannot be
     *         represented in this holder type.
     */
    public int clear(final int holder) {
        return (int) (holder & ~intMask());
    }

    /**
     * Clears the bits.
     *
     * @param holder The long data containing the bits we're interested in.
     * @return The value of holder with the specified bits cleared (set to {@code 0}).
     * @since 3.21.0
     */
    public long clear(final long holder) {
        return holder & ~mask;
    }

    /**
     * Clears the bits.
     *
     * @param holder The byte data containing the bits we're interested in.
     * @return The value of holder with the specified bits cleared (set to {@code 0}).
     * @throws IllegalStateException Thrown if this field's mask has bits set above bit 31 (only possible via {@link #BitField(long)}) and so cannot be
     *         represented in this holder type.
     */
    public byte clearByte(final byte holder) {
        return (byte) clear(holder);
    }

    /**
     * Clears the bits.
     *
     * @param holder The short data containing the bits we're interested in.
     * @return The value of holder with the specified bits cleared (set to {@code 0}).
     * @throws IllegalStateException Thrown if this field's mask has bits set above bit 31 (only possible via {@link #BitField(long)}) and so cannot be
     *         represented in this holder type.
     */
    public short clearShort(final short holder) {
        return (short) clear(holder);
    }

    /**
     * Gets the value for the specified BitField, unshifted.
     *
     * @param holder The int data containing the bits we're interested in.
     * @return The selected bits.
     * @throws IllegalStateException Thrown if this field's mask has bits set above bit 31 (only possible via {@link #BitField(long)}) and so cannot be
     *         represented in this holder type.
     */
    public int getRawValue(final int holder) {
        return (int) (holder & intMask());
    }

    /**
     * Gets the value for the specified BitField, unshifted.
     *
     * @param holder The long data containing the bits we're interested in.
     * @return The selected bits.
     * @since 3.21.0
     */
    public long getRawValue(final long holder) {
        return holder & mask;
    }

    /**
     * Gets the value for the specified BitField, unshifted.
     *
     * @param holder The short data containing the bits we're interested in.
     * @return The selected bits.
     * @throws IllegalStateException Thrown if this field's mask has bits set above bit 31 (only possible via {@link #BitField(long)}) and so cannot be
     *         represented in this holder type.
     */
    public short getShortRawValue(final short holder) {
        return (short) getRawValue(holder);
    }

    /**
     * Gets the value for the specified BitField, appropriately shifted right, as a short.
     * <p>
     * Many users of a BitField will want to treat the specified bits as an int value, and will not want to be aware that the value is stored as a BitField (and
     * so shifted left so many bits).
     * </p>
     *
     * @param holder The short data containing the bits we're interested in.
     * @return The selected bits, shifted right appropriately.
     * @see #setShortValue(short,short)
     * @throws IllegalStateException Thrown if this field's mask has bits set above bit 31 (only possible via {@link #BitField(long)}) and so cannot be
     *         represented in this holder type.
     */
    public short getShortValue(final short holder) {
        return (short) getValue(holder);
    }

    /**
     * Gets the value for the specified BitField, appropriately shifted right.
     * <p>
     * Many users of a BitField will want to treat the specified bits as an int value, and will not want to be aware that the value is stored as a BitField (and
     * so shifted left so many bits).
     * </p>
     *
     * @param holder The int data containing the bits we're interested in.
     * @return The selected bits, shifted right appropriately.
     * @see #setValue(int,int)
     * @throws IllegalStateException Thrown if this field's mask has bits set above bit 31 (only possible via {@link #BitField(long)}) and so cannot be
     *         represented in this holder type.
     */
    public int getValue(final int holder) {
        return getRawValue(holder) >>> shiftCount;
    }

    /**
     * Gets the value for the specified BitField, appropriately shifted right.
     * <p>
     * Many users of a BitField will want to treat the specified bits as an long value, and will not want to be aware that the value is stored as a BitField (and
     * so shifted left so many bits).
     * </p>
     *
     * @param holder The long data containing the bits we're interested in.
     * @return The selected bits, shifted right appropriately.
     * @see #setValue(long,long)
     * @since 3.21.0
     */
    public long getValue(final long holder) {
        return getRawValue(holder) >>> shiftCount;
    }

    /**
     * Verifies that this field's mask fits in an {@code int} holder before an {@code int}, {@code short} or {@code byte} accessor uses it.
     * <p>
     * Without this check, a mask with bits above bit 31 makes the narrow accessors silently wrong: the {@code int} shift count is truncated mod 32, and a
     * negative narrow holder is sign-extended to 64 bits before the {@code long} mask is applied, reporting above-bit-31 flags as set even though the holder
     * type cannot contain them.
     * </p>
     *
     * @return the mask, guaranteed to fit in 32 bits.
     * @throws IllegalStateException Thrown if the mask has bits set above bit 31.
     */
    private long intMask() {
        if (mask >>> Integer.SIZE != 0) {
            throw new IllegalStateException("BitField mask 0x" + Long.toHexString(mask) + " exceeds 32 bits; use the long accessors for this field.");
        }
        return mask;
    }

    /**
     * Tests whether all of the bits are set or not.
     * <p>
     * This is a stricter test than {@link #isSet(int)}, in that all of the bits in a multi-bit set must be set for this method to return {@code true}.
     * </p>
     *
     * @param holder The int data containing the bits we're interested in.
     * @return {@code true} if all of the bits are set, else {@code false}.
     * @throws IllegalStateException Thrown if this field's mask has bits set above bit 31 (only possible via {@link #BitField(long)}) and so cannot be
     *         represented in this holder type.
     */
    public boolean isAllSet(final int holder) {
        final long intMask = intMask();
        return (holder & intMask) == intMask;
    }

    /**
     * Tests whether all of the bits are set or not.
     * <p>
     * This is a stricter test than {@link #isSet(long)}, in that all of the bits in a multi-bit set must be set for this method to return {@code true}.
     * </p>
     *
     * @param holder The long data containing the bits we're interested in.
     * @return {@code true} if all of the bits are set, else {@code false}.
     * @since 3.21.0
     */
    public boolean isAllSet(final long holder) {
        return (holder & mask) == mask;
    }

    /**
     * Tests whether the field is set or not.
     * <p>
     * This is most commonly used for a single-bit field, which is often used to represent a boolean value; the results of using it for a multi-bit field is to
     * determine whether <em>any</em> of its bits are set.
     * </p>
     *
     * @param holder The int data containing the bits we're interested in
     * @return {@code true} if any of the bits are set, else {@code false}
     * @throws IllegalStateException Thrown if this field's mask has bits set above bit 31 (only possible via {@link #BitField(long)}) and so cannot be
     *         represented in this holder type.
     */
    public boolean isSet(final int holder) {
        return (holder & intMask()) != 0;
    }

    /**
     * Tests whether the field is set or not.
     * <p>
     * This is most commonly used for a single-bit field, which is often used to represent a boolean value; the results of using it for a multi-bit field is to
     * determine whether <em>any</em> of its bits are set.
     * </p>
     *
     * @param holder The long data containing the bits we're interested in
     * @return {@code true} if any of the bits are set, else {@code false}
     * @since 3.21.0
     */
    public boolean isSet(final long holder) {
        return (holder & mask) != 0;
    }

    /**
     * Sets the bits.
     *
     * @param holder The int data containing the bits we're interested in.
     * @return The value of holder with the specified bits set to {@code 1}.
     * @throws IllegalStateException Thrown if this field's mask has bits set above bit 31 (only possible via {@link #BitField(long)}) and so cannot be
     *         represented in this holder type.
     */
    public int set(final int holder) {
        return (int) (holder | intMask());
    }

    /**
     * Sets the bits.
     *
     * @param holder The long data containing the bits we're interested in.
     * @return The value of holder with the specified bits set to {@code 1}.
     * @since 3.21.0
     */
    public long set(final long holder) {
        return holder | mask;
    }

    /**
     * Sets a boolean BitField.
     *
     * @param holder The int data containing the bits we're interested in.
     * @param flag   indicating whether to set or clear the bits.
     * @return The value of holder with the specified bits set or cleared.
     * @throws IllegalStateException Thrown if this field's mask has bits set above bit 31 (only possible via {@link #BitField(long)}) and so cannot be
     *         represented in this holder type.
     */
    public int setBoolean(final int holder, final boolean flag) {
        return flag ? set(holder) : clear(holder);
    }

    /**
     * Sets a boolean BitField.
     *
     * @param holder The long data containing the bits we're interested in.
     * @param flag   indicating whether to set or clear the bits.
     * @return The value of holder with the specified bits set or cleared.
     * @since 3.21.0
     */
    public long setBoolean(final long holder, final boolean flag) {
        return flag ? set(holder) : clear(holder);
    }

    /**
     * Sets the bits.
     *
     * @param holder The byte data containing the bits we're interested in
     * @return The value of holder with the specified bits set to {@code 1}
     * @throws IllegalStateException Thrown if this field's mask has bits set above bit 31 (only possible via {@link #BitField(long)}) and so cannot be
     *         represented in this holder type.
     */
    public byte setByte(final byte holder) {
        return (byte) set(holder);
    }

    /**
     * Sets a boolean BitField.
     *
     * @param holder The byte data containing the bits we're interested in.
     * @param flag   indicating whether to set or clear the bits.
     * @return The value of holder with the specified bits set or cleared.
     * @throws IllegalStateException Thrown if this field's mask has bits set above bit 31 (only possible via {@link #BitField(long)}) and so cannot be
     *         represented in this holder type.
     */
    public byte setByteBoolean(final byte holder, final boolean flag) {
        return flag ? setByte(holder) : clearByte(holder);
    }

    /**
     * Sets the bits.
     *
     * @param holder The short data containing the bits we're interested in.
     * @return The value of holder with the specified bits set to {@code 1}.
     * @throws IllegalStateException Thrown if this field's mask has bits set above bit 31 (only possible via {@link #BitField(long)}) and so cannot be
     *         represented in this holder type.
     */
    public short setShort(final short holder) {
        return (short) set(holder);
    }

    /**
     * Sets a boolean BitField.
     *
     * @param holder The short data containing the bits we're interested in.
     * @param flag   indicating whether to set or clear the bits.
     * @return The value of holder with the specified bits set or cleared.
     * @throws IllegalStateException Thrown if this field's mask has bits set above bit 31 (only possible via {@link #BitField(long)}) and so cannot be
     *         represented in this holder type.
     */
    public short setShortBoolean(final short holder, final boolean flag) {
        return flag ? setShort(holder) : clearShort(holder);
    }

    /**
     * Sets the bits with new values.
     *
     * @param holder The short data containing the bits we're interested in
     * @param value  The new value for the specified bits
     * @return The value of holder with the bits from the value parameter replacing the old bits
     * @see #getShortValue(short)
     * @throws IllegalStateException Thrown if this field's mask has bits set above bit 31 (only possible via {@link #BitField(long)}) and so cannot be
     *         represented in this holder type.
     */
    public short setShortValue(final short holder, final short value) {
        return (short) setValue(holder, value);
    }

    /**
     * Sets the bits with new values.
     *
     * @param holder The int data containing the bits we're interested in.
     * @param value  The new value for the specified bits.
     * @return The value of holder with the bits from the value parameter replacing the old bits.
     * @see #getValue(int)
     * @throws IllegalStateException Thrown if this field's mask has bits set above bit 31 (only possible via {@link #BitField(long)}) and so cannot be
     *         represented in this holder type.
     */
    public int setValue(final int holder, final int value) {
        final long intMask = intMask();
        return (int) (holder & ~intMask | value << shiftCount & intMask);
    }

    /**
     * Sets the bits with new values.
     *
     * @param holder The long data containing the bits we're interested in.
     * @param value  The new value for the specified bits.
     * @return The value of holder with the bits from the value parameter replacing the old bits.
     * @see #getValue(long)
     * @since 3.21.0
     */
    public long setValue(final long holder, final long value) {
        return holder & ~mask | value << shiftCount & mask;
    }
}
