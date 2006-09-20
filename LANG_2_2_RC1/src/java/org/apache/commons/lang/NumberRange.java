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
package org.apache.commons.lang;

/**
 * <p>Represents a range of {@link Number} objects.</p>
 * 
 * <p>This class uses <code>double</code> comparisons. This means that it
 * is unsuitable for dealing with large <code>Long</code>, <code>BigDecimal</code>
 * or <code>BigInteger</code> numbers.</p>
 *
 * @author <a href="mailto:chrise@esha.com">Christopher Elkins</a>
 * @author Stephen Colebourne
 * @since 1.0
 * @version $Revision$ $Date$
 * 
 * @deprecated Use one of the Range classes in org.apache.commons.lang.math.
 *             Class will be removed in Commons Lang 3.0.
 * 
 */
public final class NumberRange {

    /* The minimum number in this range. */
    private final Number min;

    /* The maximum number in this range. */
    private final Number max;


    /**
     * <p>Constructs a new <code>NumberRange</code> using
     * <code>number</code> as both the minimum and maximum in
     * this range.</p>
     *
     * @param num the number to use for this range
     * @throws NullPointerException if the number is <code>null</code>
     */
    public NumberRange(Number num) {
        if (num == null) {
            throw new NullPointerException("The number must not be null");
        }

        this.min = num;
        this.max = num;
    }

    /**
     * <p>Constructs a new <code>NumberRange</code> with the specified
     * minimum and maximum numbers.</p>
     * 
     * <p><em>If the maximum is less than the minimum, the range will be constructed
     * from the minimum value to the minimum value, not what you would expect!.</em></p>
     *
     * @param min the minimum number in this range
     * @param max the maximum number in this range
     * @throws NullPointerException if either the minimum or maximum number is
     *  <code>null</code>
     */
    public NumberRange(Number min, Number max) {
        if (min == null) {
            throw new NullPointerException("The minimum value must not be null");
        } else if (max == null) {
            throw new NullPointerException("The maximum value must not be null");
        }

        if (max.doubleValue() < min.doubleValue()) {
            this.min = this.max = min;
        } else {
            this.min = min;
            this.max = max;
        }
    }

    /**
     * <p>Returns the minimum number in this range.</p>
     *
     * @return the minimum number in this range
     */
    public Number getMinimum() {
        return min;
    }

    /**
     * <p>Returns the maximum number in this range.</p>
     *
     * @return the maximum number in this range
     */
    public Number getMaximum() {
        return max;
    }

    /**
     * <p>Tests whether the specified <code>number</code> occurs within
     * this range using <code>double</code> comparison.</p>
     *
     * @param number the number to test
     * @return <code>true</code> if the specified number occurs within this
     *  range; otherwise, <code>false</code>
     */
    public boolean includesNumber(Number number) {
        if (number == null) {
            return false;
        } else {
            return !(min.doubleValue() > number.doubleValue()) &&
                !(max.doubleValue() < number.doubleValue());
        }
    }

    /**
     * <p>Tests whether the specified range occurs entirely within this
     * range using <code>double</code> comparison.</p>
     *
     * @param range the range to test
     * @return <code>true</code> if the specified range occurs entirely within
     *  this range; otherwise, <code>false</code>
     */
    public boolean includesRange(NumberRange range) {
        if (range == null) {
            return false;
        } else {
            return includesNumber(range.min) && includesNumber(range.max);
        }
    }

    /**
     * <p>Tests whether the specified range overlaps with this range
     * using <code>double</code> comparison.</p>
     *
     * @param range the range to test
     * @return <code>true</code> if the specified range overlaps with this
     *  range; otherwise, <code>false</code>
     */
    public boolean overlaps(NumberRange range) {
        if (range == null) {
            return false;
        } else {
            return range.includesNumber(min) || range.includesNumber(max) || 
                includesRange(range);
        }
    }

    /**
     * <p>Indicates whether some other <code>Object</code> is
     * &quot;equal&quot; to this one.</p>
     *
     * @param obj the reference object with which to compare
     * @return <code>true</code> if this object is the same as the obj
     *  argument; <code>false</code> otherwise
     */
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        } else if (!(obj instanceof NumberRange)) {
            return false;
        } else {
            NumberRange range = (NumberRange)obj;
            return min.equals(range.min) && max.equals(range.max);
        }
    }

    /**
     * <p>Returns a hash code value for this object.</p>
     *
     * @return a hash code value for this object
     */
    public int hashCode() {
        int result = 17;
        result = 37 * result + min.hashCode();
        result = 37 * result + max.hashCode();
        return result;
    }

    /**
     * <p>Returns the string representation of this range.</p>
     *
     * <p>This string is the string representation of the minimum and
     * maximum numbers in the range, separated by a hyphen. If a number
     * is negative, then it is enclosed in parentheses.</p>
     *
     * @return the string representation of this range
     */
    public String toString() {
        StringBuffer sb = new StringBuffer();

        if (min.doubleValue() < 0) {
            sb.append('(')
                .append(min)
                .append(')');
        } else {
            sb.append(min);
        }

        sb.append('-');

        if (max.doubleValue() < 0) {
            sb.append('(')
                .append(max)
                .append(')');
        } else {
            sb.append(max);
        }

        return sb.toString();
    }

}
