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
 * Supports operations on bit-mapped fields for 64-bit values. Instances of this class can be
 * used to store a flag or data within a {@code long}.
 *
 * <p>Each {@link LongBitField} is constructed with a mask value, which indicates
 * the bits that will be used to store and retrieve the data for that field.
 * For instance, the mask {@code 0xFFL} indicates the least-significant byte
 * should be used to store the data.</p>
 *
 * <p>As an example, consider a system that encodes multiple flags and values
 * into a single {@code long}. Bit fields can be used to encode this:</p>
 *
 * <pre>
 *     LongBitField low  = new LongBitField(0xFFL);
 *     LongBitField mid  = new LongBitField(0xFF00L);
 *     LongBitField high = new LongBitField(0xFF0000L);
 *
 *     long value = 0L;
 *     value = low.setValue(value, 18);
 *     value = mid.setValue(value, 52);
 *     value = high.setValue(value, 86);
 * </pre>
 *
 * <p>Flags and data can be retrieved from the {@code long} value:</p>
 *
 * <pre>
 *     low.getValue(value);   // 18
 *     mid.getValue(value);   // 52
 *     high.getValue(value);  // 86
 * </pre>
 *
 * @since 3.14.0
 */
public class LongBitField {

    private final long mask;
    private final int shiftCount;

    /**
     * Creates a LongBitField instance.
     *
     * @param mask the mask specifying which bits apply to this
     *  LongBitField. Bits that are set in this mask are the bits
     *  that this LongBitField operates on.
     */
    public LongBitField(final long mask) {
        this.mask = mask;
        this.shiftCount = mask == 0 ? 0 : Long.numberOfTrailingZeros(mask);
    }

    /**
     * Clears the bits.
     *
     * @param holder the long data containing the bits we're interested in.
     * @return the value of holder with the specified bits cleared (set to {@code 0}).
     */
    public long clear(final long holder) {
        return holder & ~mask;
    }

    /**
     * Gets the raw value for this bit field from the given holder.
     *
     * @param holder the long data containing the bits.
     * @return the selected bits.
     */
    public long getRawValue(final long holder) {
        return holder & mask;
    }

    /**
     * Gets the value for this bit field from the given holder.
     *
     * @param holder the long data containing the bits.
     * @return the selected bits, shifted right appropriately.
     */
    public long getValue(final long holder) {
        return getRawValue(holder) >> shiftCount;
    }

    /**
     * Tests whether all of the bits are set or not.
     *
     * @param holder the long data containing the bits.
     * @return {@code true} if all bits in the mask are set.
     */
    public boolean isAllSet(final long holder) {
        return (holder & mask) == mask;
    }

    /**
     * Tests whether any bit in the field is set.
     *
     * @param holder the long data containing the bits.
     * @return {@code true} if any bit in the mask is set.
     */
    public boolean isSet(final long holder) {
        return (holder & mask) != 0;
    }

    /**
     * Sets the bits defined by this bit field.
     *
     * @param holder the long data containing the bits.
     * @return the value of holder with the specified bits set to {@code 1}.
     */
    public long set(final long holder) {
        return holder | mask;
    }

    /**
     * Sets a boolean LongBitField.
     *
     * @param holder the long data containing the bits.
     * @param flag whether to set or clear the bits.
     * @return the value of holder with the specified bits set or cleared.
     */
    public long setBoolean(final long holder, final boolean flag) {
        return flag ? set(holder) : clear(holder);
    }

    /**
     * Sets the value of this bit field in the given holder.
     *
     * @param holder the long data containing the bits.
     * @param value the new value for the specified bits.
     * @return the value of holder with the bits from the value parameter replacing the old bits.
     */
    public long setValue(final long holder, final long value) {
        return holder & ~mask | (value << shiftCount) & mask;
    }
}
