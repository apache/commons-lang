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
 * A mutable <code>long</code> wrapper.
 * 
 * @see Long
 * @since 2.1
 * @version $Id$
 */
public class MutableLong extends Number implements Comparable, Mutable {

    /**
     * Required for serialization support.
     * 
     * @see java.io.Serializable
     */
    private static final long serialVersionUID = 62986528375L;

    /** The mutable value. */
    private long value;

    /**
     * Constructs a new MutableLong with the default value of zero.
     */
    public MutableLong() {
        super();
    }

    /**
     * Constructs a new MutableLong with the specified value.
     * 
     * @param value
     *            a value.
     */
    public MutableLong(long value) {
        super();
        this.value = value;
    }

    /**
     * Constructs a new MutableLong with the specified value.
     * 
     * @param value
     *            a value.
     * @throws NullPointerException
     *             if the object is null
     */
    public MutableLong(Number value) {
        super();
        this.value = value.longValue();
    }

    //-----------------------------------------------------------------------
    /**
     * Gets the value as a Long instance.
     * 
     * @return the value as a Long
     */
    public Object getValue() {
        return new Long(this.value);
    }

    /**
     * Sets the value.
     * 
     * @param value
     *            the value to set
     */
    public void setValue(long value) {
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
        setValue(((Number) value).longValue());
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
    public void add(long operand) {
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
        this.value += operand.longValue();
    }

    /**
     * Subtracts a value.
     * 
     * @param operand
     *            the value to add
     *
     * @since Commons Lang 2.2
     */
    public void subtract(long operand) {
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
        this.value -= operand.longValue();
    }

    //-----------------------------------------------------------------------
    // shortValue and bytValue rely on Number implementation
    /**
     * Returns the value of this MutableLong as a int.
     *
     * @return the numeric value represented by this object after conversion to type int.
     */
    public int intValue() {
        return (int) value;
    }

    /**
     * Returns the value of this MutableLong as a long.
     *
     * @return the numeric value represented by this object after conversion to type long.
     */
    public long longValue() {
        return value;
    }

    /**
     * Returns the value of this MutableLong as a float.
     *
     * @return the numeric value represented by this object after conversion to type float.
     */
    public float floatValue() {
        return value;
    }

    /**
     * Returns the value of this MutableLong as a double.
     *
     * @return the numeric value represented by this object after conversion to type double.
     */
    public double doubleValue() {
        return value;
    }

    //-----------------------------------------------------------------------
    /**
     * Gets this mutable as an instance of Long.
     *
     * @return a Long instance containing the value from this mutable
     */
    public Long toLong() {
        return new Long(longValue());
    }

    //-----------------------------------------------------------------------
    /**
     * Compares this object against the specified object. The result is <code>true</code> if and only if the argument
     * is not <code>null</code> and is a <code>MutableLong</code> object that contains the same <code>long</code>
     * value as this object.
     * 
     * @param obj
     *            the object to compare with.
     * @return <code>true</code> if the objects are the same; <code>false</code> otherwise.
     */
    public boolean equals(Object obj) {
        if (obj instanceof MutableLong) {
            return value == ((MutableLong) obj).longValue();
        }
        return false;
    }

    /**
     * Returns a suitable hashcode for this mutable.
     * 
     * @return a suitable hashcode
     */
    public int hashCode() {
        return (int) (value ^ (value >>> 32));
    }

    /**
     * Compares this mutable to another in ascending order.
     * 
     * @param obj
     *            the mutable to compare to
     * @return negative if this is less, zero if equal, positive if greater
     * @throws ClassCastException if the argument is not a MutableLong
     */
    public int compareTo(Object obj) {
        MutableLong other = (MutableLong) obj;
        long anotherVal = other.value;
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
