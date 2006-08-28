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
package org.apache.commons.lang.mutable;

import org.apache.commons.lang.math.NumberUtils;

/**
 * A mutable <code>float</code> wrapper.
 * 
 * @see Float
 * @since 2.1
 * @version $Id$
 */
public class MutableFloat extends Number implements Comparable, Mutable {

    /**
     * Required for serialization support.
     * 
     * @see java.io.Serializable
     */
    private static final long serialVersionUID = 5787169186L;

    /** The mutable value. */
    private float value;

    /**
     * Constructs a new MutableFloat with the default value of zero.
     */
    public MutableFloat() {
        super();
    }

    /**
     * Constructs a new MutableFloat with the specified value.
     * 
     * @param value
     *            a value.
     */
    public MutableFloat(float value) {
        super();
        this.value = value;
    }

    /**
     * Constructs a new MutableFloat with the specified value.
     * 
     * @param value
     *            a value.
     * @throws NullPointerException
     *             if the object is null
     */
    public MutableFloat(Number value) {
        super();
        this.value = value.floatValue();
    }

    //-----------------------------------------------------------------------
    /**
     * Gets the value as a Float instance.
     * 
     * @return the value as a Float
     */
    public Object getValue() {
        return new Float(this.value);
    }

    /**
     * Sets the value.
     * 
     * @param value
     *            the value to set
     */
    public void setValue(float value) {
        this.value = value;
    }

    /**
     * Sets the value from any Number instance.
     * 
     * @param value
     *            the value to set
     * @throws NullPointerException
     *             if the object is null
     * @throws ClassCastException
     *             if the type is not a {@link Number}
     */
    public void setValue(Object value) {
        setValue(((Number) value).floatValue());
    }

    //-----------------------------------------------------------------------
    /**
     * Increments the value.
     *
     * @since Commons Lang 2.2
     */
    public void increment() {
        value++;
    }

    /**
     * Decrements the value.
     *
     * @since Commons Lang 2.2
     */
    public void decrement() {
        value--;
    }

    //-----------------------------------------------------------------------
    /**
     * Adds a value.
     * 
     * @param operand
     *            the value to add
     *
     * @since Commons Lang 2.2
     */
    public void add(float operand) {
        this.value += operand;
    }

    /**
     * Adds a value.
     * 
     * @param operand
     *            the value to add
     * @throws NullPointerException
     *             if the object is null
     *
     * @since Commons Lang 2.2
     */
    public void add(Number operand) {
        this.value += operand.floatValue();
    }

    /**
     * Subtracts a value.
     * 
     * @param operand
     *            the value to add
     *
     * @since Commons Lang 2.2
     */
    public void subtract(float operand) {
        this.value -= operand;
    }

    /**
     * Subtracts a value.
     * 
     * @param operand
     *            the value to add
     * @throws NullPointerException
     *             if the object is null
     *
     * @since Commons Lang 2.2
     */
    public void subtract(Number operand) {
        this.value -= operand.floatValue();
    }

    //-----------------------------------------------------------------------
    // shortValue and bytValue rely on Number implementation
    /**
     * Returns the value of this MutableFloat as a int.
     *
     * @return the numeric value represented by this object after conversion to type int.
     */
    public int intValue() {
        return (int) value;
    }

    /**
     * Returns the value of this MutableFloat as a long.
     *
     * @return the numeric value represented by this object after conversion to type long.
     */
    public long longValue() {
        return (long) value;
    }

    /**
     * Returns the value of this MutableFloat as a float.
     *
     * @return the numeric value represented by this object after conversion to type float.
     */
    public float floatValue() {
        return value;
    }

    /**
     * Returns the value of this MutableFloat as a double.
     *
     * @return the numeric value represented by this object after conversion to type double.
     */
    public double doubleValue() {
        return value;
    }

    /**
     * Checks whether the float value is the special NaN value.
     * 
     * @return true if NaN
     */
    public boolean isNaN() {
        return Float.isNaN(value);
    }

    /**
     * Checks whether the float value is infinite.
     * 
     * @return true if infinite
     */
    public boolean isInfinite() {
        return Float.isInfinite(value);
    }

    //-----------------------------------------------------------------------
    /**
     * Gets this mutable as an instance of Float.
     *
     * @return a Float instance containing the value from this mutable
     */
    public Float toFloat() {
        return new Float(floatValue());
    }

    //-----------------------------------------------------------------------
    /**
     * Compares this object against some other object. The result is <code>true</code> if and only if the argument is
     * not <code>null</code> and is a <code>Float</code> object that represents a <code>float</code> that has the
     * identical bit pattern to the bit pattern of the <code>float</code> represented by this object. For this
     * purpose, two float values are considered to be the same if and only if the method
     * {@link Float#floatToIntBits(float)}returns the same int value when applied to each.
     * <p>
     * Note that in most cases, for two instances of class <code>Float</code>,<code>f1</code> and <code>f2</code>,
     * the value of <code>f1.equals(f2)</code> is <code>true</code> if and only if <blockquote>
     * 
     * <pre>
     *   f1.floatValue() == f2.floatValue()
     * </pre>
     * 
     * </blockquote>
     * <p>
     * also has the value <code>true</code>. However, there are two exceptions:
     * <ul>
     * <li>If <code>f1</code> and <code>f2</code> both represent <code>Float.NaN</code>, then the
     * <code>equals</code> method returns <code>true</code>, even though <code>Float.NaN==Float.NaN</code> has
     * the value <code>false</code>.
     * <li>If <code>f1</code> represents <code>+0.0f</code> while <code>f2</code> represents <code>-0.0f</code>,
     * or vice versa, the <code>equal</code> test has the value <code>false</code>, even though
     * <code>0.0f==-0.0f</code> has the value <code>true</code>.
     * </ul>
     * This definition allows hashtables to operate properly.
     * 
     * @param obj
     *            the object to be compared
     * @return <code>true</code> if the objects are the same; <code>false</code> otherwise.
     * @see java.lang.Float#floatToIntBits(float)
     */
    public boolean equals(Object obj) {
        return (obj instanceof MutableFloat)
            && (Float.floatToIntBits(((MutableFloat) obj).value) == Float.floatToIntBits(value));
    }

    //-----------------------------------------------------------------------
    /**
     * Returns a suitable hashcode for this mutable.
     * 
     * @return a suitable hashcode
     */
    public int hashCode() {
        return Float.floatToIntBits(value);
    }

    /**
     * Compares this mutable to another in ascending order.
     * 
     * @param obj
     *            the mutable to compare to
     * @return negative if this is less, zero if equal, positive if greater
     */
    public int compareTo(Object obj) {
        MutableFloat other = (MutableFloat) obj;
        float anotherVal = other.value;
        return NumberUtils.compare(value, anotherVal);
    }

    /**
     * Returns the String value of this mutable.
     * 
     * @return the mutable value as a string
     */
    public String toString() {
        return String.valueOf(value);
    }

}
