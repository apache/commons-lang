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
package org.apache.commons.lang.math;

import java.io.Serializable;

/**
 * <p><code>LongRange</code> represents an inclusive range of <code>long</code>s.</p>
 *
 * @author Stephen Colebourne
 * @since 2.0
 * @version $Id$
 */
public final class LongRange extends Range implements Serializable {
    
    /**
     * Required for serialization support.
     * 
     * @see java.io.Serializable
     */
    private static final long serialVersionUID = 71849363892720L;

    /**
     * The minimum number in this range (inclusive).
     */
    private final long min;
    /**
     * The maximum number in this range (inclusive).
     */
    private final long max;
    
    /**
     * Cached output minObject (class is immutable).
     */
    private transient Long minObject = null;
    /**
     * Cached output maxObject (class is immutable).
     */
    private transient Long maxObject = null;
    /**
     * Cached output hashCode (class is immutable).
     */
    private transient int hashCode = 0;
    /**
     * Cached output toString (class is immutable).
     */
    private transient String toString = null;
    
    /**
     * <p>Constructs a new <code>LongRange</code> using the specified
     * number as both the minimum and maximum in this range.</p>
     *
     * @param number  the number to use for this range
     */
    public LongRange(long number) {
        super();
        this.min = number;
        this.max = number;
    }

    /**
     * <p>Constructs a new <code>LongRange</code> using the specified
     * number as both the minimum and maximum in this range.</p>
     *
     * @param number  the number to use for this range, must not
     *  be <code>null</code>
     * @throws IllegalArgumentException if the number is <code>null</code>
     */
    public LongRange(Number number) {
        super();
        if (number == null) {
            throw new IllegalArgumentException("The number must not be null");
        }
        this.min = number.longValue();
        this.max = number.longValue();
        if (number instanceof Long) {
            this.minObject = (Long) number;
            this.maxObject = (Long) number;
        }
    }

    /**
     * <p>Constructs a new <code>LongRange</code> with the specified
     * minimum and maximum numbers (both inclusive).</p>
     * 
     * <p>The arguments may be passed in the order (min,max) or (max,min). The
     * getMinimum and getMaximum methods will return the correct values.</p>
     * 
     * @param number1  first number that defines the edge of the range, inclusive
     * @param number2  second number that defines the edge of the range, inclusive
     */
    public LongRange(long number1, long number2) {
        super();
        if (number2 < number1) {
            this.min = number2;
            this.max = number1;
        } else {
            this.min = number1;
            this.max = number2;
        }
    }

    /**
     * <p>Constructs a new <code>LongRange</code> with the specified
     * minimum and maximum numbers (both inclusive).</p>
     * 
     * <p>The arguments may be passed in the order (min,max) or (max,min). The
     * getMinimum and getMaximum methods will return the correct values.</p>
     *
     * @param number1  first number that defines the edge of the range, inclusive
     * @param number2  second number that defines the edge of the range, inclusive
     * @throws IllegalArgumentException if either number is <code>null</code>
     */
    public LongRange(Number number1, Number number2) {
        super();
        if (number1 == null || number2 == null) {
            throw new IllegalArgumentException("The numbers must not be null");
        }
        long number1val = number1.longValue();
        long number2val = number2.longValue();
        if (number2val < number1val) {
            this.min = number2val;
            this.max = number1val;
            if (number2 instanceof Long) {
                this.minObject = (Long) number2;
            }
            if (number1 instanceof Long) {
                this.maxObject = (Long) number1;
            }
        } else {
            this.min = number1val;
            this.max = number2val;
            if (number1 instanceof Long) {
                this.minObject = (Long) number1;
            }
            if (number2 instanceof Long) {
                this.maxObject = (Long) number2;
            }
        }
    }

    // Accessors
    //--------------------------------------------------------------------

    /**
     * <p>Returns the minimum number in this range.</p>
     *
     * @return the minimum number in this range
     */
    public Number getMinimumNumber() {
        if (minObject == null) {
            minObject = new Long(min);            
        }
        return minObject;
    }

    /**
     * <p>Gets the minimum number in this range as a <code>long</code>.</p>
     *
     * @return the minimum number in this range
     */
    public long getMinimumLong() {
        return min;
    }

    /**
     * <p>Gets the minimum number in this range as a <code>int</code>.</p>
     * 
     * <p>This conversion can lose information for large values.</p>
     *
     * @return the minimum number in this range
     */
    public int getMinimumInteger() {
        return (int) min;
    }

    /**
     * <p>Gets the minimum number in this range as a <code>double</code>.</p>
     * 
     * <p>This conversion can lose information for large values.</p>
     *
     * @return the minimum number in this range
     */
    public double getMinimumDouble() {
        return min;
    }

    /**
     * <p>Gets the minimum number in this range as a <code>float</code>.</p>
     * 
     * <p>This conversion can lose information for large values.</p>
     *
     * @return the minimum number in this range
     */
    public float getMinimumFloat() {
        return min;
    }

    /**
     * <p>Returns the maximum number in this range.</p>
     *
     * @return the maximum number in this range
     */
    public Number getMaximumNumber() {
        if (maxObject == null) {
            maxObject = new Long(max);            
        }
        return maxObject;
    }

    /**
     * <p>Gets the maximum number in this range as a <code>long</code>.</p>
     *
     * @return the maximum number in this range
     */
    public long getMaximumLong() {
        return max;
    }

    /**
     * <p>Gets the maximum number in this range cast to an <code>int</code>.</p>
     * 
     * <p>This conversion can lose information for large values.</p>
     * 
     * @return the maximum number in this range cast to an <code>int</code>.
     */
    public int getMaximumInteger() {
        return (int) max;
    }

    /**
     * <p>Gets the maximum number in this range as a <code>double</code>.</p>
     * 
     * <p>This conversion can lose information for large values.</p>
     * 
     * @return The maximum number in this range as a <code>double</code>.
     */
    public double getMaximumDouble() {
        return max;
    }

    /**
     * <p>Gets the maximum number in this range as a <code>float</code>.</p>
     * 
     * <p>This conversion can lose information for large values.</p>
     * 
     * @return The maximum number in this range as a <code>float</code>.
     */
    public float getMaximumFloat() {
        return max;
    }

    // Tests
    //--------------------------------------------------------------------
    
    /**
     * <p>Tests whether the specified <code>number</code> occurs within
     * this range using <code>long</code> comparison.</p>
     * 
     * <p><code>null</code> is handled and returns <code>false</code>.</p>
     *
     * @param number  the number to test, may be <code>null</code>
     * @return <code>true</code> if the specified number occurs within this range
     */
    public boolean containsNumber(Number number) {
        if (number == null) {
            return false;
        }
        return containsLong(number.longValue());
    }

    /**
     * <p>Tests whether the specified <code>long</code> occurs within
     * this range using <code>long</code> comparison.</p>
     * 
     * <p>This implementation overrides the superclass for performance as it is
     * the most common case.</p>
     * 
     * @param value  the long to test
     * @return <code>true</code> if the specified number occurs within this
     *  range by <code>long</code> comparison
     */
    public boolean containsLong(long value) {
        return value >= min && value <= max;
    }

    // Range tests
    //--------------------------------------------------------------------

    /**
     * <p>Tests whether the specified range occurs entirely within this range
     * using <code>long</code> comparison.</p>
     * 
     * <p><code>null</code> is handled and returns <code>false</code>.</p>
     *
     * @param range  the range to test, may be <code>null</code>
     * @return <code>true</code> if the specified range occurs entirely within this range
     * @throws IllegalArgumentException if the range is not of this type
     */
    public boolean containsRange(Range range) {
        if (range == null) {
            return false;
        }
        return containsLong(range.getMinimumLong()) &&
               containsLong(range.getMaximumLong());
    }

    /**
     * <p>Tests whether the specified range overlaps with this range
     * using <code>long</code> comparison.</p>
     * 
     * <p><code>null</code> is handled and returns <code>false</code>.</p>
     *
     * @param range  the range to test, may be <code>null</code>
     * @return <code>true</code> if the specified range overlaps with this range
     */
    public boolean overlapsRange(Range range) {
        if (range == null) {
            return false;
        }
        return range.containsLong(min) ||
               range.containsLong(max) || 
               containsLong(range.getMinimumLong());
    }

    // Basics
    //--------------------------------------------------------------------

    /**
     * <p>Compares this range to another object to test if they are equal.</p>.
     * 
     * <p>To be equal, the class, minimum and maximum must be equal.</p>
     *
     * @param obj the reference object with which to compare
     * @return <code>true</code> if this object is equal
     */
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        if (obj instanceof LongRange == false) {
            return false;
        }
        LongRange range = (LongRange) obj;
        return min == range.min && max == range.max;
    }

    /**
     * <p>Gets a hashCode for the range.</p>
     *
     * @return a hash code value for this object
     */
    public int hashCode() {
        if (hashCode == 0) {
            hashCode = 17;
            hashCode = 37 * hashCode + getClass().hashCode();
            hashCode = 37 * hashCode + ((int) (min ^ (min >> 32)));
            hashCode = 37 * hashCode + ((int) (max ^ (max >> 32)));
        }
        return hashCode;
    }

    /**
     * <p>Gets the range as a <code>String</code>.</p>
     *
     * <p>The format of the String is 'Range[<i>min</i>,<i>max</i>]'.</p>
     *
     * @return the <code>String</code> representation of this range
     */
    public String toString() {
        if (toString == null) {
            StringBuffer buf = new StringBuffer(32);
            buf.append("Range[");
            buf.append(min);
            buf.append(',');
            buf.append(max);
            buf.append(']');
            toString = buf.toString();
        }
        return toString;
    }

}
