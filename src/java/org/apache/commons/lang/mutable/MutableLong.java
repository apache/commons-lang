/*
 * Copyright 2004 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
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
 * @version $Id: MutableLong.java,v 1.6 2004/10/08 19:45:46 ggregory Exp $
 */
public class MutableLong extends Number implements Comparable, Mutable {

    /** Serialization lock. */
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
    public int intValue() {
        return (int) value;
    }

    public long longValue() {
        return value;
    }

    public float floatValue() {
        return value;
    }

    public double doubleValue() {
        return value;
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
        return (value < anotherVal ? -1 : (value == anotherVal ? 0 : 1));
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
