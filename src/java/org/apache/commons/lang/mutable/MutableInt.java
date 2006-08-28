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

/**
 * A mutable <code>int</code> wrapper.
 * 
 * @see Integer
 * @since 2.1
 * @version $Id$
 */
public class MutableInt extends Number implements Comparable, Mutable {

    /**
     * Required for serialization support.
     * 
     * @see java.io.Serializable
     */
    private static final long serialVersionUID = 512176391864L;

    /** The mutable value. */
    private int value;

    /**
     * Constructs a new MutableInt with the default value of zero.
     */
    public MutableInt() {
        super();
    }

    /**
     * Constructs a new MutableInt with the specified value.
     * 
     * @param value
     *                  a value.
     */
    public MutableInt(int value) {
        super();
        this.value = value;
    }

    /**
     * Constructs a new MutableInt with the specified value.
     * 
     * @param value
     *                  a value.
     * @throws NullPointerException
     *                  if the object is null
     */
    public MutableInt(Number value) {
        super();
        this.value = value.intValue();
    }

    //-----------------------------------------------------------------------
    /**
     * Gets the value as a Integer instance.
     * 
     * @return the value as a Integer
     */
    public Object getValue() {
        return new Integer(this.value);
    }

    /**
     * Sets the value.
     * 
     * @param value
     *                  the value to set
     */
    public void setValue(int value) {
        this.value = value;
    }

    /**
     * Sets the value from any Number instance.
     * 
     * @param value
     *                  the value to set
     * @throws NullPointerException
     *                  if the object is null
     * @throws ClassCastException
     *                  if the type is not a {@link Number}
     */
    public void setValue(Object value) {
        setValue(((Number) value).intValue());
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
    public void add(int operand) {
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
        this.value += operand.intValue();
    }

    /**
     * Subtracts a value.
     * 
     * @param operand
     *            the value to add
     *
     * @since Commons Lang 2.2
     */
    public void subtract(int operand) {
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
        this.value -= operand.intValue();
    }

    //-----------------------------------------------------------------------
    // shortValue and bytValue rely on Number implementation
    /**
     * Returns the value of this MutableInt as a int.
     *
     * @return the numeric value represented by this object after conversion to type int.
     */
    public int intValue() {
        return value;
    }

    /**
     * Returns the value of this MutableInt as a long.
     *
     * @return the numeric value represented by this object after conversion to type long.
     */
    public long longValue() {
        return value;
    }

    /**
     * Returns the value of this MutableInt as a float.
     *
     * @return the numeric value represented by this object after conversion to type float.
     */
    public float floatValue() {
        return value;
    }

    /**
     * Returns the value of this MutableInt as a double.
     *
     * @return the numeric value represented by this object after conversion to type double.
     */
    public double doubleValue() {
        return value;
    }

    //-----------------------------------------------------------------------
    /**
     * Gets this mutable as an instance of Integer.
     *
     * @return a Integer instance containing the value from this mutable
     */
    public Integer toInteger() {
        return new Integer(intValue());
    }

    //-----------------------------------------------------------------------
    /**
     * Compares this object to the specified object. The result is <code>true</code> if and only if the argument is
     * not <code>null</code> and is an <code>MutableInt</code> object that contains the same <code>int</code> value
     * as this object.
     * 
     * @param obj
     *                  the object to compare with.
     * @return <code>true</code> if the objects are the same; <code>false</code> otherwise.
     */
    public boolean equals(Object obj) {
        if (obj instanceof MutableInt) {
            return value == ((MutableInt) obj).intValue();
        }
        return false;
    }

    /**
     * Returns a suitable hashcode for this mutable.
     * 
     * @return a suitable hashcode
     */
    public int hashCode() {
        return value;
    }

    /**
     * Compares this mutable to another in ascending order.
     * 
     * @param obj
     *                  the mutable to compare to
     * @return negative if this is less, zero if equal, positive if greater
     * @throws ClassCastException if the argument is not a MutableInt
     */
    public int compareTo(Object obj) {
        MutableInt other = (MutableInt) obj;
        int anotherVal = other.value;
        return value < anotherVal ? -1 : (value == anotherVal ? 0 : 1);
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
