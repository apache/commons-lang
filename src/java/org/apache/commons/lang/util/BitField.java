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
 *    any, must include the following acknowledgement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowledgement may appear in the software itself,
 *    if and wherever such third-party acknowledgements normally appear.
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
 * <p>Manage operations dealing with bit-mapped fields.</p>
 *
 * @author Apache Jakarta POI
 * @author Scott Sanders (sanders at apache dot org)
 * @author Marc Johnson (mjohnson at apache dot org)
 * @author Andrew C. Oliver (acoliver at apache dot org)
 * @author Stephen Colebourne
 * @author Pete Gieser
 * @since 2.0
 * @version $Id: BitField.java,v 1.8 2003/08/18 02:22:25 bayard Exp $
 */
public class BitField {
    
    private final int _mask;
    private final int _shift_count;

    /**
     * <p>Create a BitField instance.</p>
     *
     * @param mask the mask specifying which bits apply to this
     *  BitField. Bits that are set in this mask are the bits
     *  that this BitField operates on
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
     * <p>Obtain the value for the specified BitField, appropriately
     * shifted right.</p>
     *
     * <p>Many users of a BitField will want to treat the specified
     * bits as an int value, and will not want to be aware that the
     * value is stored as a BitField (and so shifted left so many
     * bits).</p>
     *
     * @see #setValue
     * @param holder the int data containing the bits we're interested
     *  in
     * @return the selected bits, shifted right appropriately
     */
    public int getValue(final int holder) {
        return getRawValue(holder) >> _shift_count;
    }

    /**
     * <p>Obtain the value for the specified BitField, appropriately
     * shifted right, as a short.</p>
     *
     * <p>Many users of a BitField will want to treat the specified
     * bits as an int value, and will not want to be aware that the
     * value is stored as a BitField (and so shifted left so many
     * bits).</p>
     *
     * @see #setShortValue
     * @param holder the short data containing the bits we're
     *  interested in
     * @return the selected bits, shifted right appropriately
     */
    public short getShortValue(final short holder) {
        return (short) getValue(holder);
    }

    /**
     * <p>Obtain the value for the specified BitField, unshifted.</p>
     *
     * @param holder the int data containing the bits we're
     *  interested in
     * @return the selected bits
     */
    public int getRawValue(final int holder) {
        return (holder & _mask);
    }

    /**
     * <p>Obtain the value for the specified BitField, unshifted.</p>
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
     * @return <code>true</code> if any of the bits are set,
     *  else <code>false</code>
     */
    public boolean isSet(final int holder) {
        return (holder & _mask) != 0;
    }

    /**
     * <p>Returns whether all of the bits are set or not.</p>
     *
     * <p>This is a stricter test than {@link #isSet(int)},
     * in that all of the bits in a multi-bit set must be set
     * for this method to return <code>true</code>.</p>
     *
     * @param holder the int data containing the bits we're
     *  interested in
     * @return <code>true</code> if all of the bits are set,
     *  else <code>false</code>
     */
    public boolean isAllSet(final int holder) {
        return (holder & _mask) == _mask;
    }

    /**
     * <p>Replace the bits with new values.</p>
     *
     * @see #getValue
     * @param holder the int data containint the bits we're
     *  interested in
     * @param value the new value for the specified bits
     * @return the value of holder with the bits from the value
     *  parameter replacing the old bits
     */
    public int setValue(final int holder, final int value) {
        return (holder & ~_mask) | ((value << _shift_count) & _mask);
    }

    /**
     * <p>Replace the bits with new values.</p>
     *
     * @see #getShortValue
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
     * <p>Clear the bits.</p>
     *
     * @param holder the int data containing the bits we're
     *  interested in
     * @return the value of holder with the specified bits cleared
     *  (set to <code>0</code>)
     */
    public int clear(final int holder) {
        return holder & ~_mask;
    }

    /**
     * <p>Clear the bits.</p>
     *
     * @param holder the short data containing the bits we're
     *  interested in
     * @return the value of holder with the specified bits cleared
     *  (set to <code>0</code>)
     */
    public short clearShort(final short holder) {
        return (short) clear(holder);
    }

    /**
     * <p>Clear the bits.</p>
     *
     * @param holder the byte data containing the bits we're
     *  interested in
     *
     * @return the value of holder with the specified bits cleared
     *  (set to <code>0</code>)
     */
    public byte clearByte(final byte holder) {
        return (byte) clear(holder);
    }

    /**
     * <p>Set the bits.</p>
     *
     * @param holder the int data containing the bits we're
     *  interested in
     * @return the value of holder with the specified bits set
     *  to <code>1</code>
     */
    public int set(final int holder) {
        return holder | _mask;
    }

    /**
     * <p>Set the bits.</p>
     *
     * @param holder the short data containing the bits we're
     *  interested in
     * @return the value of holder with the specified bits set
     *  to <code>1</code>
     */
    public short setShort(final short holder) {
        return (short) set(holder);
    }

    /**
     * <p>Set the bits.</p>
     *
     * @param holder the byte data containing the bits we're
     *  interested in
     *
     * @return the value of holder with the specified bits set
     *  to <code>1</code>
     */
    public byte setByte(final byte holder) {
        return (byte) set(holder);
    }

    /**
     * <p>Set a boolean BitField.</p>
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
     * <p>Set a boolean BitField.</p>
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
     * <p>Set a boolean BitField.</p>
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
