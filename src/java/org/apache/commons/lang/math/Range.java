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
package org.apache.commons.lang.math;

import org.apache.commons.lang.NumberUtils;

/**
 * <p><code>Range</code> represents a range of numbers of the same type.</p>
 * 
 * <p>Specific subclasses hold the range values as different types. Each
 * subclass should be immutable and {@link java.io.Serializable Serializable}
 * if possible.</p>
 *
 * @author Stephen Colebourne
 * @since 2.0
 * @version $Id: Range.java,v 1.2 2003/03/23 17:51:15 scolebourne Exp $
 */
public abstract class Range {

    /**
     * <p>Constructs a new range.</p>
     */
    public Range() {
        super();
    }

    // Accessors
    //--------------------------------------------------------------------

    /**
     * <p>Gets the minimum number in this range.</p>
     *
     * @return the minimum number in this range
     */
    public abstract Number getMinimumNumber();

    /**
     * <p>Gets the minimum number in this range as a <code>long</code>.</p>
     * 
     * <p>This implementation uses the {@link #getMinimumNumber()} method. 
     * Subclasses may be able to optimise this.</p>
     *
     * @return the minimum number in this range
     */
    public long getMinimumLong() {
        return getMinimumNumber().longValue();
    }

    /**
     * <p>Gets the minimum number in this range as a <code>int</code>.</p>
     * 
     * <p>This implementation uses the {@link #getMinimumNumber()} method. 
     * Subclasses may be able to optimise this.</p>
     *
     * @return the minimum number in this range
     */
    public int getMinimumInteger() {
        return getMinimumNumber().intValue();
    }

    /**
     * <p>Gets the minimum number in this range as a <code>double</code>.</p>
     * 
     * <p>This implementation uses the {@link #getMinimumNumber()} method. 
     * Subclasses may be able to optimise this.</p>
     *
     * @return the minimum number in this range
     */
    public double getMinimumDouble() {
        return getMinimumNumber().doubleValue();
    }

    /**
     * <p>Gets the minimum number in this range as a <code>float</code>.</p>
     * 
     * <p>This implementation uses the {@link #getMinimumNumber()} method. 
     * Subclasses may be able to optimise this.</p>
     *
     * @return the minimum number in this range
     */
    public float getMinimumFloat() {
        return getMinimumNumber().floatValue();
    }

    /**
     * <p>Gets the maximum number in this range.</p>
     *
     * @return the maximum number in this range
     */
    public abstract Number getMaximumNumber();

    /**
     * <p>Gets the maximum number in this range as a <code>long</code>.</p>
     * 
     * <p>This implementation uses the {@link #getMaximumNumber()} method. 
     * Subclasses may be able to optimise this.</p>
     *
     * @return the maximum number in this range
     */
    public long getMaximumLong() {
        return getMaximumNumber().longValue();
    }

    /**
     * <p>Gets the maximum number in this range as a <code>int</code>.</p>
     * 
     * <p>This implementation uses the {@link #getMaximumNumber()} method. 
     * Subclasses may be able to optimise this.</p>
     *
     * @return the maximum number in this range
     */
    public int getMaximumInteger() {
        return getMaximumNumber().intValue();
    }

    /**
     * <p>Gets the maximum number in this range as a <code>double</code>.</p>
     * 
     * <p>This implementation uses the {@link #getMaximumNumber()} method. 
     * Subclasses may be able to optimise this.</p>
     *
     * @return the maximum number in this range
     */
    public double getMaximumDouble() {
        return getMaximumNumber().doubleValue();
    }

    /**
     * <p>Gets the maximum number in this range as a <code>float</code>.</p>
     * 
     * <p>This implementation uses the {@link #getMaximumNumber()} method. 
     * Subclasses may be able to optimise this.</p>
     *
     * @return the maximum number in this range
     */
    public float getMaximumFloat() {
        return getMaximumNumber().floatValue();
    }

    // Include tests
    //--------------------------------------------------------------------
    
    /**
     * <p>Tests whether the specified <code>Number</code> occurs within
     * this range.</p>
     * 
     * <p>The exact comparison implementation varies by subclass. It is
     * intended that an <code>int</code> specific subclass will compare using
     * <code>int</code> comparison.</p>
     * 
     * <p><code>null</code> is handled and returns <code>false</code>.</p>
     *
     * @param number  the number to test, may be <code>null</code>
     * @return <code>true</code> if the specified number occurs within this range
     * @throws IllegalArgumentException if the <code>Number</code> cannot be compared
     */
    public abstract boolean includesNumber(Number number);

    /**
     * <p>Tests whether the specified <code>Number</code> occurs within
     * this range using <code>long</code> comparison..</p>
     * 
     * <p><code>null</code> is handled and returns <code>false</code>.</p>
     * 
     * <p>This implementation forwards to the {@link #includesLong(long)} method.</p>
     *
     * @param value  the long to test, may be <code>null</code>
     * @return <code>true</code> if the specified number occurs within this
     *  range by <code>long</code> comparison
     */
    public boolean includesLong(Number value) {
        if (value == null) {
            return false;
        }
        return includesLong(value.longValue());
    }

    /**
     * <p>Tests whether the specified <code>long</code> occurs within
     * this range using <code>long</code> comparison.</p>
     * 
     * <p>This implementation uses the {@link #getMinimumLong()} and 
     * {@link #getMaximumLong()} methods and should be good for most uses.</p>
     * 
     * @param value  the long to test
     * @return <code>true</code> if the specified number occurs within this
     *  range by <code>long</code> comparison
     */
    public boolean includesLong(long value) {
        return (value >= getMinimumLong() && value <= getMaximumLong());
    }

    /**
     * <p>Tests whether the specified <code>Number</code> occurs within
     * this range using <code>int</code> comparison..</p>
     * 
     * <p><code>null</code> is handled and returns <code>false</code>.</p>
     * 
     * <p>This implementation forwards to the {@link #includesInteger(int)} method.</p>
     *
     * @param value  the integer to test, may be <code>null</code>
     * @return <code>true</code> if the specified number occurs within this
     *  range by <code>int</code> comparison
     */
    public boolean includesInteger(Number value) {
        if (value == null) {
            return false;
        }
        return includesInteger(value.intValue());
    }

    /**
     * <p>Tests whether the specified <code>int</code> occurs within
     * this range using <code>int</code> comparison.</p>
     * 
     * <p>This implementation uses the {@link #getMinimumInteger()} and 
     * {@link #getMaximumInteger()} methods and should be good for most uses.</p>
     * 
     * @param value  the int to test
     * @return <code>true</code> if the specified number occurs within this
     *  range by <code>int</code> comparison
     */
    public boolean includesInteger(int value) {
        return (value >= getMinimumInteger() && value <= getMaximumInteger());
    }

    /**
     * <p>Tests whether the specified <code>Number</code> occurs within
     * this range using <code>double</code> comparison..</p>
     * 
     * <p><code>null</code> is handled and returns <code>false</code>.</p>
     * 
     * <p>This implementation forwards to the {@link #includesDouble(double)} method.</p>
     *
     * @param value  the double to test, may be <code>null</code>
     * @return <code>true</code> if the specified number occurs within this
     *  range by <code>double</code> comparison
     */
    public boolean includesDouble(Number value) {
        if (value == null) {
            return false;
        }
        return includesDouble(value.doubleValue());
    }

    /**
     * <p>Tests whether the specified <code>double</code> occurs within
     * this range using <code>double</code> comparison.</p>
     * 
     * <p>This implementation uses the {@link #getMinimumDouble()} and 
     * {@link #getMaximumDouble()} methods and should be good for most uses.</p>
     * 
     * @param value  the double to test
     * @return <code>true</code> if the specified number occurs within this
     *  range by <code>double</code> comparison
     */
    public boolean includesDouble(double value) {
        int compareMin = NumberUtils.compare(getMinimumDouble(), value);
        int compareMax = NumberUtils.compare(getMaximumDouble(), value);
        return (compareMin <= 0 && compareMax >= 0);
    }

    /**
     * <p>Tests whether the specified <code>Number</code> occurs within
     * this range using <code>float</code> comparison.</p>
     * 
     * <p><code>null</code> is handled and returns <code>false</code>.</p>
     * 
     * <p>This implementation forwards to the {@link #includesFloat(float)} method.</p>
     *
     * @param value  the float to test, may be <code>null</code>
     * @return <code>true</code> if the specified number occurs within this
     *  range by <code>float</code> comparison
     */
    public boolean includesFloat(Number value) {
        if (value == null) {
            return false;
        }
        return includesFloat(value.floatValue());
    }

    /**
     * <p>Tests whether the specified <code>float</code> occurs within
     * this range using <code>float</code> comparison.</p>
     * 
     * <p>This implementation uses the {@link #getMinimumFloat()} and 
     * {@link #getMaximumFloat()} methods and should be good for most uses.</p>
     * 
     * @param value  the float to test
     * @return <code>true</code> if the specified number occurs within this
     *  range by <code>float</code> comparison
     */
    public boolean includesFloat(float value) {
        int compareMin = NumberUtils.compare(getMinimumFloat(), value);
        int compareMax = NumberUtils.compare(getMaximumFloat(), value);
        return (compareMin <= 0 && compareMax >= 0);
    }

    // Range tests
    //--------------------------------------------------------------------

    /**
     * <p>Tests whether the specified range occurs entirely within this range.</p>
     * 
     * <p>The exact comparison implementation varies by subclass. It is
     * intended that an <code>int</code> specific subclass will compare using
     * <code>int</code> comparison.</p>
     * 
     * <p><code>null</code> is handled and returns <code>false</code>.</p>
     * 
     * <p>This implementation uses the {@link #includesNumber(Number)} method.
     * Subclasses may be able to optimise this.</p>
     *
     * @param range  the range to test, may be <code>null</code>
     * @return <code>true</code> if the specified range occurs entirely within
     *  this range; otherwise, <code>false</code>
     * @throws IllegalArgumentException if the <code>Range</code> cannot be compared
     */
    public boolean includesRange(Range range) {
        if (range == null) {
            return false;
        }
        return includesNumber(range.getMinimumNumber()) &&
               includesNumber(range.getMaximumNumber());
    }

    /**
     * <p>Tests whether the specified range overlaps with this range.</p>
     * 
     * <p>The exact comparison implementation varies by subclass. It is
     * intended that an <code>int</code> specific subclass will compare using
     * <code>int</code> comparison.</p>
     * 
     * <p><code>null</code> is handled and returns <code>false</code>.</p>
     * 
     * <p>This implementation uses the {@link #includesNumber(Number)} and
     * {@link #includesRange(Range)} methods.
     * Subclasses may be able to optimise this.</p>
     *
     * @param range  the range to test, may be <code>null</code>
     * @return <code>true</code> if the specified range overlaps with this
     *  range; otherwise, <code>false</code>
     * @throws IllegalArgumentException if the <code>Range</code> cannot be compared
     */
    public boolean overlapsRange(Range range) {
        if (range == null) {
            return false;
        }
        return range.includesNumber(getMinimumNumber()) ||
               range.includesNumber(getMaximumNumber()) || 
               includesNumber(range.getMinimumNumber());
    }

    // Basics
    //--------------------------------------------------------------------

    /**
     * <p>Compares this range to another object to test if they are equal.</p>.
     * 
     * <p>To be equal, the class, minimum and maximum must be equal.</p>
     * 
     * <p>This implementation uses the {@link #getMinimumNumber()} and 
     * {@link #getMaximumNumber()} methods. 
     * Subclasses may be able to optimise this.</p>
     *
     * @param obj the reference object with which to compare
     * @return <code>true</code> if this object is equal
     */
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        } else if (obj == null || obj.getClass() != getClass()) {
            return false;
        } else {
            Range range = (Range) obj;
            return getMinimumNumber().equals(range.getMinimumNumber()) &&
                   getMaximumNumber().equals(range.getMaximumNumber());
        }
    }

    /**
     * <p>Gets a hashCode for the range.</p>
     * 
     * <p>This implementation uses the {@link #getMinimumNumber()} and 
     * {@link #getMaximumNumber()} methods. 
     * Subclasses may be able to optimise this.</p>
     *
     * @return a hash code value for this object
     */
    public int hashCode() {
        int result = 17;
        result = 37 * result + getClass().hashCode();
        result = 37 * result + getMinimumNumber().hashCode();
        result = 37 * result + getMaximumNumber().hashCode();
        return result;
    }

    /**
     * <p>Gets the range as a <code>String</code>.</p>
     *
     * <p>The format of the String is 'Range[<i>min</i>,<i>max</i>]'.</p>
     * 
     * <p>This implementation uses the {@link #getMinimumNumber()} and 
     * {@link #getMaximumNumber()} methods. 
     * Subclasses may be able to optimise this.</p>
     *
     * @return the <code>String</code> representation of this range
     */
    public String toString() {
        StringBuffer buf = new StringBuffer(32);
        buf.append("Range[");
        buf.append(getMinimumNumber());
        buf.append(',');
        buf.append(getMaximumNumber());
        buf.append(']');
        return buf.toString();
    }

}
