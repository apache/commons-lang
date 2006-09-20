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
 * <p><code>FloatRange</code> represents an inclusive range of <code>float</code>s.</p>
 *
 * @author Stephen Colebourne
 * @since 2.0
 * @version $Id$
 */
public final class FloatRange extends Range implements Serializable {
    
    /**
     * Required for serialization support.
     * 
     * @see java.io.Serializable
     */
    private static final long serialVersionUID = 71849363892750L;

    /**
     * The minimum number in this range (inclusive).
     */
    private final float min;
    /**
     * The maximum number in this range (inclusive).
     */
    private final float max;
    
    /**
     * Cached output minObject (class is immutable).
     */
    private transient Float minObject = null;
    /**
     * Cached output maxObject (class is immutable).
     */
    private transient Float maxObject = null;
    /**
     * Cached output hashCode (class is immutable).
     */
    private transient int hashCode = 0;
    /**
     * Cached output toString (class is immutable).
     */
    private transient String toString = null;
    
    /**
     * <p>Constructs a new <code>FloatRange</code> using the specified
     * number as both the minimum and maximum in this range.</p>
     *
     * @param number  the number to use for this range
     * @throws IllegalArgumentException if the number is <code>NaN</code>
     */
    public FloatRange(float number) {
        super();
        if (Float.isNaN(number)) {
            throw new IllegalArgumentException("The number must not be NaN");
        }
        this.min = number;
        this.max = number;
    }

    /**
     * <p>Constructs a new <code>FloatRange</code> using the specified
     * number as both the minimum and maximum in this range.</p>
     *
     * @param number  the number to use for this range, must not
     *  be <code>null</code>
     * @throws IllegalArgumentException if the number is <code>null</code>
     * @throws IllegalArgumentException if the number is <code>NaN</code>
     */
    public FloatRange(Number number) {
        super();
        if (number == null) {
            throw new IllegalArgumentException("The number must not be null");
        }
        this.min = number.floatValue();
        this.max = number.floatValue();
        if (Float.isNaN(min) || Float.isNaN(max)) {
            throw new IllegalArgumentException("The number must not be NaN");
        }
        if (number instanceof Float) {
            this.minObject = (Float) number;
            this.maxObject = (Float) number;
        }
    }

    /**
     * <p>Constructs a new <code>FloatRange</code> with the specified
     * minimum and maximum numbers (both inclusive).</p>
     * 
     * <p>The arguments may be passed in the order (min,max) or (max,min). The
     * getMinimum and getMaximum methods will return the correct values.</p>
     * 
     * @param number1  first number that defines the edge of the range, inclusive
     * @param number2  second number that defines the edge of the range, inclusive
     * @throws IllegalArgumentException if either number is <code>NaN</code>
     */
    public FloatRange(float number1, float number2) {
        super();
        if (Float.isNaN(number1) || Float.isNaN(number2)) {
            throw new IllegalArgumentException("The numbers must not be NaN");
        }
        if (number2 < number1) {
            this.min = number2;
            this.max = number1;
        } else {
            this.min = number1;
            this.max = number2;
        }
    }

    /**
     * <p>Constructs a new <code>FloatRange</code> with the specified
     * minimum and maximum numbers (both inclusive).</p>
     * 
     * <p>The arguments may be passed in the order (min,max) or (max,min). The
     * getMinimum and getMaximum methods will return the correct values.</p>
     *
     * @param number1  first number that defines the edge of the range, inclusive
     * @param number2  second number that defines the edge of the range, inclusive
     * @throws IllegalArgumentException if either number is <code>null</code>
     * @throws IllegalArgumentException if either number is <code>NaN</code>
     */
    public FloatRange(Number number1, Number number2) {
        super();
        if (number1 == null || number2 == null) {
            throw new IllegalArgumentException("The numbers must not be null");
        }
        float number1val = number1.floatValue();
        float number2val = number2.floatValue();
        if (Float.isNaN(number1val) || Float.isNaN(number2val)) {
            throw new IllegalArgumentException("The numbers must not be NaN");
        }
        if (number2val < number1val) {
            this.min = number2val;
            this.max = number1val;
            if (number2 instanceof Float) {
                this.minObject = (Float) number2;
            }
            if (number1 instanceof Float) {
                this.maxObject = (Float) number1;
            }
        } else {
            this.min = number1val;
            this.max = number2val;
            if (number1 instanceof Float) {
                this.minObject = (Float) number1;
            }
            if (number2 instanceof Float) {
                this.maxObject = (Float) number2;
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
            minObject = new Float(min);            
        }
        return minObject;
    }

    /**
     * <p>Gets the minimum number in this range as a <code>long</code>.</p>
     * 
     * <p>This conversion can lose information for large values or decimals.</p>
     *
     * @return the minimum number in this range
     */
    public long getMinimumLong() {
        return (long) min;
    }

    /**
     * <p>Gets the minimum number in this range as a <code>int</code>.</p>
     * 
     * <p>This conversion can lose information for large values or decimals.</p>
     *
     * @return the minimum number in this range
     */
    public int getMinimumInteger() {
        return (int) min;
    }

    /**
     * <p>Gets the minimum number in this range as a <code>double</code>.</p>
     *
     * @return the minimum number in this range
     */
    public double getMinimumDouble() {
        return min;
    }

    /**
     * <p>Gets the minimum number in this range as a <code>float</code>.</p>
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
            maxObject = new Float(max);            
        }
        return maxObject;
    }

    /**
     * <p>Gets the maximum number in this range as a <code>long</code>.</p>
     * 
     * <p>This conversion can lose information for large values or decimals.</p>
     *
     * @return the maximum number in this range
     */
    public long getMaximumLong() {
        return (long) max;
    }

    /**
     * <p>Gets the maximum number in this range as a <code>int</code>.</p>
     * 
     * <p>This conversion can lose information for large values or decimals.</p>
     *
     * @return the maximum number in this range
     */
    public int getMaximumInteger() {
        return (int) max;
    }

    /**
     * <p>Gets the maximum number in this range as a <code>double</code>.</p>
     *
     * @return the maximum number in this range
     */
    public double getMaximumDouble() {
        return max;
    }

    /**
     * <p>Gets the maximum number in this range as a <code>float</code>.</p>
     *
     * @return the maximum number in this range
     */
    public float getMaximumFloat() {
        return max;
    }

    // Tests
    //--------------------------------------------------------------------
    
    /**
     * <p>Tests whether the specified <code>number</code> occurs within
     * this range using <code>float</code> comparison.</p>
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
        return containsFloat(number.floatValue());
    }

    /**
     * <p>Tests whether the specified <code>float</code> occurs within
     * this range using <code>float</code> comparison.</p>
     * 
     * <p>This implementation overrides the superclass for performance as it is
     * the most common case.</p>
     * 
     * @param value  the float to test
     * @return <code>true</code> if the specified number occurs within this
     *  range by <code>float</code> comparison
     */
    public boolean containsFloat(float value) {
        return value >= min && value <= max;
    }

    // Range tests
    //--------------------------------------------------------------------

    /**
     * <p>Tests whether the specified range occurs entirely within this range
     * using <code>float</code> comparison.</p>
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
        return containsFloat(range.getMinimumFloat()) &&
               containsFloat(range.getMaximumFloat());
    }

    /**
     * <p>Tests whether the specified range overlaps with this range
     * using <code>float</code> comparison.</p>
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
        return range.containsFloat(min) ||
               range.containsFloat(max) || 
               containsFloat(range.getMinimumFloat());
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
        if (obj instanceof FloatRange == false) {
            return false;
        }
        FloatRange range = (FloatRange) obj;
        return (Float.floatToIntBits(min) == Float.floatToIntBits(range.min) &&
                Float.floatToIntBits(max) == Float.floatToIntBits(range.max));
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
            hashCode = 37 * hashCode + Float.floatToIntBits(min);
            hashCode = 37 * hashCode + Float.floatToIntBits(max);
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
