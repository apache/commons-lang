/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002-2003 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
 *
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 */
package org.apache.commons.lang.util;

/**
 * Manage operations dealing with bit-mapped fields.
 * <p>
 * Code originated from the POI project.
 *
 * @author Scott Sanders (sanders at apache dot org)
 * @author Marc Johnson (mjohnson at apache dot org)
 * @author Andrew C. Oliver (acoliver at apache dot org)
 * @author Stephen Colebourne
 * @since 2.0
 * @version $Id: BitField.java,v 1.5 2003/04/09 00:07:49 ggregory Exp $
 */
public class BitField {
    
    private final int _mask;
    private final int _shift_count;

    /**
     * Create a BitField instance
     *
     * @param mask the mask specifying which bits apply to this
     *             BitField. Bits that are set in this mask are the
     *             bits that this BitField operates on
     */
    public BitField(final int mask) {
        _mask = mask;
        int count = 0;
        int bit_pattern = mask;

        if (bit_pattern != 0) {
            while ((bit_pattern & 1) == 0) {
                count++;
                bit_pattern >>= 1;
            }
        }
        _shift_count = count;
    }

    /**
     * Obtain the value for the specified BitField, appropriately
     * shifted right. Many users of a BitField will want to treat the
     * specified bits as an int value, and will not want to be aware
     * that the value is stored as a BitField (and so shifted left so
     * many bits)
     *
     * @param holder the int data containing the bits we're interested
     *               in
     *
     * @return the selected bits, shifted right appropriately
     */
    public int getValue(final int holder) {
        return getRawValue(holder) >> _shift_count;
    }

    /**
     * Obtain the value for the specified BitField, appropriately
     * shifted right, as a short. Many users of a BitField will want
     * to treat the specified bits as an int value, and will not want
     * to be aware that the value is stored as a BitField (and so
     * shifted left so many bits)
     *
     * @param holder the short data containing the bits we're
     *               interested in
     *
     * @return the selected bits, shifted right appropriately
     */
    public short getShortValue(final short holder) {
        return (short) getValue(holder);
    }

    /**
     * Obtain the value for the specified BitField, unshifted
     *
     * @param holder the int data containing the bits we're interested
     *               in
     *
     * @return the selected bits
     */
    public int getRawValue(final int holder) {
        return (holder & _mask);
    }

    /**
     * Obtain the value for the specified BitField, unshifted
     *
     * @param holder the short data containing the bits we're
     *               interested in
     *
     * @return the selected bits
     */
    public short getShortRawValue(final short holder) {
        return (short) getRawValue(holder);
    }

    /**
     * Returns whether the field is set or not. This is most commonly used for a
     * single-bit field, which is often used to represent a boolean
     * value; the results of using it for a multi-bit field is to
     * determine whether *any* of its bits are set
     *
     * @param holder the int data containing the bits we're interested
     *               in
     *
     * @return true if any of the bits are set, else false
     */
    public boolean isSet(final int holder) {
        return (holder & _mask) != 0;
    }

    /**
     * Returns whether all of the bits are set or not. This is a stricter test than
     * isSet, in that all of the bits in a multi-bit set must be set
     * for this method to return true
     *
     * @param holder the int data containing the bits we're interested
     *               in
     *
     * @return true if all of the bits are set, else false
     */
    public boolean isAllSet(final int holder) {
        return (holder & _mask) == _mask;
    }

    /**
     * Replace the bits with new values.
     *
     * @param holder the int data containint the bits we're interested
     *               in
     * @param value the new value for the specified bits
     *
     * @return the value of holder with the bits from the value
     *         parameter replacing the old bits
     */
    public int setValue(final int holder, final int value) {
        return (holder & ~_mask) | ((value << _shift_count) & _mask);
    }

    /**
     * Replace the bits with new values.
     *
     * @param holder the short data containing the bits we're
     *               interested in
     * @param value the new value for the specified bits
     *
     * @return the value of holder with the bits from the value
     *         parameter replacing the old bits
     */
    public short setShortValue(final short holder, final short value) {
        return (short) setValue(holder, value);
    }

    /**
     * Clear the bits.
     *
     * @param holder the int data containing the bits we're interested
     *               in
     *
     * @return the value of holder with the specified bits cleared
     *         (set to 0)
     */
    public int clear(final int holder) {
        return holder & ~_mask;
    }

    /**
     * Clear the bits.
     *
     * @param holder the short data containing the bits we're
     *               interested in
     *
     * @return the value of holder with the specified bits cleared
     *         (set to 0)
     */
    public short clearShort(final short holder) {
        return (short) clear(holder);
    }

    /**
     * Clear the bits.
     *
     * @param holder the byte data containing the bits we're
     *               interested in
     *
     * @return the value of holder with the specified bits cleared
     *         (set to 0)
     */
    public byte clearByte(final byte holder) {
        return (byte) clear(holder);
    }

    /**
     * Set the bits.
     *
     * @param holder the int data containing the bits we're interested
     *               in
     *
     * @return the value of holder with the specified bits set to 1
     */
    public int set(final int holder) {
        return holder | _mask;
    }

    /**
     * Set the bits.
     *
     * @param holder the short data containing the bits we're
     *               interested in
     *
     * @return the value of holder with the specified bits set to 1
     */
    public short setShort(final short holder) {
        return (short) set(holder);
    }

    /**
     * Set the bits.
     *
     * @param holder the byte data containing the bits we're
     *               interested in
     *
     * @return the value of holder with the specified bits set to 1
     */
    public byte setByte(final byte holder) {
        return (byte) set(holder);
    }

    /**
     * Set a boolean BitField
     *
     * @param holder the int data containing the bits we're interested
     *               in
     * @param flag indicating whether to set or clear the bits
     *
     * @return the value of holder with the specified bits set or
     *         cleared
     */
    public int setBoolean(final int holder, final boolean flag) {
        return flag ? set(holder) : clear(holder);
    }

    /**
     * Set a boolean BitField
     *
     * @param holder the short data containing the bits we're
     *               interested in
     * @param flag indicating whether to set or clear the bits
     *
     * @return the value of holder with the specified bits set or
     *         cleared
     */
    public short setShortBoolean(final short holder, final boolean flag) {
        return flag ? setShort(holder) : clearShort(holder);
    }

    /**
     * Set a boolean BitField
     *
     * @param holder the byte data containing the bits we're
     *               interested in
     * @param flag indicating whether to set or clear the bits
     *
     * @return the value of holder with the specified bits set or
     *         cleared
     */
    public byte setByteBoolean(final byte holder, final boolean flag) {
        return flag ? setByte(holder) : clearByte(holder);
    }

}
