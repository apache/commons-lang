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

import org.apache.commons.lang.math.NumberUtils;

/**
 * A mutable <code>float</code>.
 * 
 * @since 2.1
 * @version $Id: MutableFloat.java,v 1.5 2004/10/01 17:12:29 ggregory Exp $
 */
public class MutableFloat extends Number
        implements Comparable, Mutable, Serializable {

    /** Serialization lock. */
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
     * @param value a value.
     */
    public MutableFloat(float value) {
        super();
        this.value = value;
    }

    /**
     * Constructs a new MutableFloat with the specified value.
     * 
     * @param value a value.
     * @throws NullPointerException if the object is null
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
     * @param value  the value to set
     */
    public void setValue(float value) {
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
        setValue(((Number) value).floatValue());
    }

    //-----------------------------------------------------------------------
    public int intValue() {
        return (int) value;
    }

    public long longValue() {
        return (long) value;
    }

    public float floatValue() {
        return value;
    }

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
     * Checks if this object equals the specified object.
     * <p>
     * The object must be a MutableFloat with the same value to be equal.
     *
     * @param obj  the object to compare to
     * @return true if equal
     */
    public boolean equals(Object obj) {
        if (obj instanceof MutableFloat) {
            float other = ((MutableFloat) obj).value;
            return (Float.floatToIntBits(other) == Float.floatToIntBits(value));
        }
        return false;
    }

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
     * @param obj  the mutable to compare to
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
