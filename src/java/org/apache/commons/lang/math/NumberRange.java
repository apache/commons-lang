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

import java.io.Serializable;

/**
 * <p><code>NumberRange</code> represents an inclusive range of 
 * {@link java.lang.Number Number} objects of the same type.</p>
 *
 * @author <a href="mailto:chrise@esha.com">Christopher Elkins</a>
 * @author Stephen Colebourne
 * @since 2.0 (previously in org.apache.commons.lang)
 * @version $Id: NumberRange.java,v 1.3 2003/04/09 01:04:47 ggregory Exp $
 */
public final class NumberRange extends Range implements Serializable {
    
    private static final long serialVersionUID = 71849363892710L;

    /* The minimum number in this range. */
    private final Number min;
    /* The maximum number in this range. */
    private final Number max;
    
    /** Cached output hashCode (class is immutable) */
    private transient int hashCode = 0;
    /** Cached output toString (class is immutable) */
    private transient String toString = null;

    /**
     * <p>Constructs a new <code>NumberRange</code> using the specified
     * number as both the minimum and maximum in this range.</p>
     *
     * @param num the number to use for this range
     * @throws IllegalArgumentException if the number is <code>null</code>
     * @throws IllegalArgumentException if the number doesn't implement <code>Comparable</code>
     * @throws IllegalArgumentException if the number is <code>Double.NaN</code> or <code>Float.NaN</code>
     */
    public NumberRange(Number num) {
        if (num == null) {
            throw new IllegalArgumentException("The number must not be null");
        }
        if (num instanceof Comparable == false) {
            throw new IllegalArgumentException("The number must implement Comparable");
        }
        if (num instanceof Double && ((Double) num).isNaN()) {
            throw new IllegalArgumentException("The number must not be NaN");
        }
        if (num instanceof Float && ((Float) num).isNaN()) {
            throw new IllegalArgumentException("The number must not be NaN");
        }

        this.min = num;
        this.max = num;
    }

    /**
     * <p>Constructs a new <code>NumberRange</code> with the specified
     * minimum and maximum numbers (both inclusive).</p>
     * 
     * <p>The arguments may be passed in the order (min,max) or (max,min). The
     * {@link #getMinimumNumber()} and {@link #getMaximumNumber()} methods will return the
     * correct value.</p>
     * 
     * <p>This constructor is designed to be used with two <code>Number</code>
     * objects of the same type. If two objects of different types are passed in,
     * an exception is thrown.</p>
     *
     * @param num1  first number that defines the edge of the range, inclusive
     * @param num2  second number that defines the edge of the range, inclusive
     * @throws IllegalArgumentException if either number is <code>null</code>
     * @throws IllegalArgumentException if the numbers are of different types
     * @throws IllegalArgumentException if the numbers don't implement <code>Comparable</code>
     */
    public NumberRange(Number num1, Number num2) {
        if (num1 == null || num2 == null) {
            throw new IllegalArgumentException("The numbers must not be null");
        }
        if (num1.getClass() != num2.getClass()) {
            throw new IllegalArgumentException("The numbers must be of the same type");
        }
        if (num1 instanceof Comparable == false) {
            throw new IllegalArgumentException("The numbers must implement Comparable");
        }
        if (num1 instanceof Double) {
            if (((Double) num1).isNaN() || ((Double) num2).isNaN()) {
                throw new IllegalArgumentException("The number must not be NaN");
            }
        } else if (num1 instanceof Float) {
            if (((Float) num1).isNaN() || ((Float) num2).isNaN()) {
                throw new IllegalArgumentException("The number must not be NaN");
            }
        }
        
        int compare = ((Comparable) num1).compareTo(num2);
        if (compare == 0) {
            this.min = num1;
            this.max = num1;
        } else if (compare > 0) {
            this.min = num2;
            this.max = num1;
        } else {
            this.min = num1;
            this.max = num2;
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
        return min;
    }

    /**
     * <p>Returns the maximum number in this range.</p>
     *
     * @return the maximum number in this range
     */
    public Number getMaximumNumber() {
        return max;
    }

    // Tests
    //--------------------------------------------------------------------
    
    /**
     * <p>Tests whether the specified <code>number</code> occurs within
     * this range.</p>
     * 
     * <p><code>null</code> is handled and returns <code>false</code>.</p>
     *
     * @param number  the number to test, may be <code>null</code>
     * @return <code>true</code> if the specified number occurs within this range
     * @throws IllegalArgumentException if the number is of a different type to the range
     */
    public boolean includesNumber(Number number) {
        if (number == null) {
            return false;
        }
        if (number.getClass() != min.getClass()) {
            throw new IllegalArgumentException("The number must be of the same type as the range numbers");
        }
        int compareMin = ((Comparable) min).compareTo(number);
        int compareMax = ((Comparable) max).compareTo(number);
        return (compareMin <= 0 && compareMax >= 0);
    }

    // Range tests
    //--------------------------------------------------------------------
    // use Range implementations

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
        if (obj instanceof NumberRange == false) {
            return false;
        }
        NumberRange range = (NumberRange) obj;
        return min.equals(range.min) && max.equals(range.max);
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
            hashCode = 37 * hashCode + min.hashCode();
            hashCode = 37 * hashCode + max.hashCode();
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
