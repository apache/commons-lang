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

import java.io.Serializable;

/**
 * A mutable <code>int</code>.
 * 
 * @since 2.1
 * @version $Id: MutableInt.java,v 1.2 2004/07/07 23:50:28 scolebourne Exp $
 */
public class MutableInt extends Number
        implements Comparable, Mutable, Serializable {

    /** Serialization lock. */
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
     * @param value a value.
     */
    public MutableInt(int value) {
        super();
        this.value = value;
    }

    /**
     * Constructs a new MutableInt with the specified value.
     * 
     * @param value a value.
     * @throws NullPointerException if the object is null
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
     * @param value  the value to set
     */
    public void setValue(int value) {
        this.value = value;
    }

    /**
     * Sets the value from any Number instance.
     * 
     * @param value  the value to set
     * @throws NullPointerException if the object is null
     * @throws ClassCastException if the type is invalid
     */
    public void setValue(Object value) {
        setValue(((Number) value).intValue());
    }

    //-----------------------------------------------------------------------
    public int intValue() {
        return value;
    }

    public long longValue() {
        return (long) value;
    }

    public float floatValue() {
        return (float) value;
    }

    public double doubleValue() {
        return (double) value;
    }

    //-----------------------------------------------------------------------
    /**
     * Checks if this object equals the specified object.
     * <p>
     * The object must be a MutableInt with the same value to be equal.
     *
     * @param obj  the object to compare to
     * @return true if equal
     */
    public boolean equals(Object obj) {
        if (obj instanceof MutableInt) {
            return (value == ((MutableInt) obj).value);
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
     * @param obj  the mutable to compare to
     * @return negative if this is less, zero if equal, positive if greater
     */
    public int compareTo(Object obj) {
        MutableInt other = (MutableInt) obj;
        int anotherVal = other.value;
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
