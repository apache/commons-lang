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
 * A mutable <code>double</code>.
 * 
 * @since 2.1
 * @version $Id: MutableDouble.java,v 1.4 2004/07/07 23:50:28 scolebourne Exp $
 */
public class MutableDouble extends Number
        implements Comparable, Mutable, Serializable {

    /** Serialization lock. */
    private static final long serialVersionUID = 1587163916L;

    /** The mutable value. */
    private double value;

    /**
     * Constructs a new MutableDouble with the default value of zero.
     */
    public MutableDouble() {
        super();
    }

    /**
     * Constructs a new MutableDouble with the specified value.
     * 
     * @param value a value.
     */
    public MutableDouble(double value) {
        super();
        this.value = value;
    }

    /**
     * Constructs a new MutableDouble with the specified value.
     * 
     * @param value a value.
     * @throws NullPointerException if the object is null
     */
    public MutableDouble(Number value) {
        super();
        this.value = value.doubleValue();
    }

    //-----------------------------------------------------------------------
    /**
     * Gets the value as a Double instance.
     * 
     * @return the value as a Double
     */
    public Object getValue() {
        return new Double(this.value);
    }

    /**
     * Sets the value.
     * 
     * @param value  the value to set
     */
    public void setValue(double value) {
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
        setValue(((Number) value).doubleValue());
    }

    //-----------------------------------------------------------------------
    public int intValue() {
        return (int) value;
    }

    public long longValue() {
        return (long) value;
    }

    public float floatValue() {
        return (float) value;
    }

    public double doubleValue() {
        return value;
    }

    /**
     * Checks whether the double value is the special NaN value.
     *
     * @return true if NaN
     */
    public boolean isNaN() {
        return Double.isNaN(value);
    }

    /**
     * Checks whether the double value is infinite.
     *
     * @return true if infinite
     */
    public boolean isInfinite() {
        return Double.isInfinite(value);
    }

    //-----------------------------------------------------------------------
    /**
     * Checks if this object equals the specified object.
     * <p>
     * The object must be a MutableDouble with the same value to be equal.
     *
     * @param obj  the object to compare to
     * @return true if equal
     */
    public boolean equals(Object obj) {
        if (obj instanceof MutableDouble) {
            double other = ((MutableDouble) obj).value;
            return (Double.doubleToLongBits(other) == Double.doubleToLongBits(value));
        }
        return false;
    }

    /**
     * Returns a suitable hashcode for this mutable.
     *
     * @return a suitable hashcode
     */
    public int hashCode() {
        long bits = Double.doubleToLongBits(value);
        return (int)(bits ^ (bits >>> 32));
    }

    /**
     * Compares this mutable to another in ascending order.
     *
     * @param obj  the mutable to compare to
     * @return negative if this is less, zero if equal, positive if greater
     */
    public int compareTo(Object obj) {
        MutableDouble other = (MutableDouble) obj;
        double anotherVal = other.value;
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
