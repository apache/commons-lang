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

import java.util.Comparator;

/**
 * <p><code>Range</code> represents a range of numbers of the same type.</p>
 * 
 * @author Apache Software Foundation
 * @since 3.0
 * @version $Id: Range.java 830032 2009-10-27 00:15:00Z scolebourne $
 */
// TODO: Serializable?
public class Range<T> {

    private final Comparator comparator;
    private final T minimum;
    private final T maximum;

    /**
     * <p>Constructs a new <code>Range</code> using the specified
     * element as both the minimum and maximum in this range.</p>
     * <p>The range uses the natural ordering of the elements to 
     * determine where values lie in the range.</p>
     *
     * @param element  the value to use for this range, must not be <code>null</code>
     * @throws IllegalArgumentException if the value is <code>null</code>
     */
// TODO: Ideally this would only support <T extends Comparable<? super T>>
    public Range(T element) {
        this(element, element);
    }

    /**
     * <p>Constructs a new <code>Range</code> with the specified
     * minimum and maximum values (both inclusive).</p>
     * <p>The range uses the natural ordering of the elements to 
     * determine where values lie in the range.</p>
     *
     * <p>The arguments may be passed in the order (min,max) or (max,min). The
     * getMinimum and getMaximum methods will return the correct values.</p>
     *
     * @param element1  first value that defines the edge of the range, inclusive
     * @param element2  second value that defines the edge of the range, inclusive
     * @throws IllegalArgumentException if either value is <code>null</code>
     */
// TODO: Ideally this would only support <T extends Comparable<? super T>>
    public Range(T element1, T element2) {
        this(element1, element2, ComparableComparator.INSTANCE);
    }

    /**
     * <p>Constructs a new <code>Range</code> using the specified
     * element as both the minimum and maximum in this range.</p>
     * <p>The range uses the passed in <code>Comparator</code> to 
     * determine where values lie in the range.</p>
     *
     * @param element  the value to use for this range, must not be <code>null</code>
     * @throws IllegalArgumentException if the value is <code>null</code>
     */
    public Range(T element, Comparator c) {
        this(element, element, c);
    }

    /**
     * <p>Constructs a new <code>Range</code> with the specified
     * minimum and maximum values (both inclusive).</p>
     * <p>The range uses the passed in <code>Comparator</code> to 
     * determine where values lie in the range.</p>
     *
     * <p>The arguments may be passed in the order (min,max) or (max,min). The
     * getMinimum and getMaximum methods will return the correct values.</p>
     *
     * @param element1  first value that defines the edge of the range, inclusive
     * @param element2  second value that defines the edge of the range, inclusive
     * @throws IllegalArgumentException if either value is <code>null</code>
     */
    public Range(T element1, T element2, Comparator c) {
        if(element1 == null || element2 == null) {
            throw new IllegalArgumentException("Elements in a range must not be null: element1=" + 
                                               element1 + ", element2=" + element2);
        }

        if(c == null) {
            throw new IllegalArgumentException("Comparator must not be null");
        }

        if(c.compare(element1, element2) < 1) {
            this.minimum = element1;
            this.maximum = element2;
        } else {
            this.minimum = element2;
            this.maximum = element1;
        }
        this.comparator = c;
    }

    // Accessors
    //--------------------------------------------------------------------

    /**
     * <p>Gets the minimum value in this range.</p>
     *
     * @return the minimum value in this range
     */
    public T getMinimum() {
        return this.minimum;
    }

    /**
     * <p>Gets the maximum value in this range.</p>
     *
     * @return the maximum value in this range
     */
    public T getMaximum() {
        return this.maximum;
    }

    /**
     * <p>Gets the comparator being used to determine if objects are within the range. </p>
     *
     * @return the comparator being used
     */
    public Comparator getComparator() {
        return this.comparator;
    }

    /**
     * <p>Whether or not the Range is using the default natural comparison method 
     * to compare elements. </p>
     *
     * @return whether or not the default Comparator is in use
     */
    public boolean isDefaultNaturalOrdering() {
        return this.comparator == ComparableComparator.INSTANCE;
    }

    // Include tests
    //--------------------------------------------------------------------
    
    /**
     * <p>Tests whether the specified element occurs within this range.</p>
     * 
     * <p><code>null</code> is handled and returns <code>false</code>.</p>
     *
     * @param element  the element to test, may be <code>null</code>
     * @return <code>true</code> if the specified element occurs within this range
     * @throws IllegalArgumentException if the <code>Number</code> cannot be compared
     */
    public boolean contains(T element) {
        if(element == null) {
            return false;
        }
        return (comparator.compare(element, getMinimum()) > -1) && (comparator.compare(element, getMaximum()) < 1);
    }

    public boolean lessThan(T element) {
        if (element == null) {
            return false;
        }
        
        return this.comparator.compare(getMinimum(), element) < 1;
    }

    public boolean greaterThan(T element) {
        if (element == null) {
            return false;
        }
        
        return this.comparator.compare(getMaximum(), element) > -1;
    }

    // Range tests
    //--------------------------------------------------------------------

    /**
     * <p>Tests whether the specified range occurs entirely within this range.</p>
     * 
     * <p>The exact comparison implementation varies by subclass. It is
     * intended that an <code>int</code> specific subclass will compare using
     * <code>int</code> comparison.</p>
     * 
     * <p><code>null</code> is handled and returns <code>false</code>.</p>
     * 
     * <p>This implementation uses the {@link #containsNumber(Number)} method.
     * Subclasses may be able to optimise this.</p>
     *
     * @param range  the range to test, may be <code>null</code>
     * @return <code>true</code> if the specified range occurs entirely within
     *  this range; otherwise, <code>false</code>
     * @throws IllegalArgumentException if the <code>Range</code> cannot be compared
     */
    public boolean containsRange(Range<T> range) {
        if (range == null) {
            return false;
        }
        return contains(range.getMinimum()) 
            && contains(range.getMaximum());
    }

    /**
     * <p>Tests whether the specified range overlaps with this range.</p>
     * 
     * <p>The exact comparison implementation varies by subclass. It is
     * intended that an <code>int</code> specific subclass will compare using
     * <code>int</code> comparison.</p>
     * 
     * <p><code>null</code> is handled and returns <code>false</code>.</p>
     * 
     * <p>This implementation uses the {@link #containsNumber(Number)} and
     * {@link #containsRange(Range)} methods.
     * Subclasses may be able to optimise this.</p>
     *
     * @param range  the range to test, may be <code>null</code>
     * @return <code>true</code> if the specified range overlaps with this
     *  range; otherwise, <code>false</code>
     * @throws IllegalArgumentException if the <code>Range</code> cannot be compared
     */
    public boolean overlapsRange(Range<T> range) {
        if (range == null) {
            return false;
        }
        return range.contains(getMinimum())
            || range.contains(getMaximum())
            || contains(range.getMinimum());
    }

    // Basics
    //--------------------------------------------------------------------

    /**
     * <p>Compares this range to another object to test if they are equal.</p>.
     * 
     * <p>To be equal, the class, minimum and maximum must be equal.</p>
     * 
     * <p>This implementation uses the {@link #getMinimumNumber()} and 
     * {@link #getMaximumNumber()} methods. 
     * Subclasses may be able to optimise this.</p>
     *
     * @param obj the reference object with which to compare
     * @return <code>true</code> if this object is equal
     */
    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        } else if (obj == null || obj.getClass() != getClass()) {
            return false;
        } else {
            Range range = (Range) obj;
            return getMinimum().equals(range.getMinimum()) &&
                   getMaximum().equals(range.getMaximum());
        }
    }

    /**
     * <p>Gets a hashCode for the range.</p>
     * 
     * <p>This implementation uses the {@link #getMinimumNumber()} and 
     * {@link #getMaximumNumber()} methods. 
     * Subclasses may be able to optimise this.</p>
     *
     * @return a hash code value for this object
     */
    @Override
    public int hashCode() {
        int result = 17;
        result = 37 * result + getClass().hashCode();
        result = 37 * result + getMinimum().hashCode();
        result = 37 * result + getMaximum().hashCode();
        return result;
    }

    /**
     * <p>Gets the range as a <code>String</code>.</p>
     *
     * <p>The format of the String is 'Range[<i>min</i>,<i>max</i>]'.</p>
     * 
     * <p>This implementation uses the {@link #getMinimumNumber()} and 
     * {@link #getMaximumNumber()} methods. 
     * Subclasses may be able to optimise this.</p>
     *
     * @return the <code>String</code> representation of this range
     */
    @Override
    public String toString() {
        StringBuilder buf = new StringBuilder(32);
        buf.append("Range[");
        buf.append(getMinimum());
        buf.append(',');
        buf.append(getMaximum());
        buf.append(']');
        return buf.toString();
    }


    // Taken from Commons Collections - documentation removed as not a public class
    private static class ComparableComparator<E extends Comparable<? super E>> implements Comparator<E> {

        @SuppressWarnings("unchecked")
        public static final ComparableComparator<?> INSTANCE = new ComparableComparator();

        @SuppressWarnings("unchecked")
        public static <E extends Comparable<? super E>> ComparableComparator<E> getInstance() {
            return (ComparableComparator<E>) INSTANCE;
        }

        public ComparableComparator() {
            super();
        }

        public int compare(E obj1, E obj2) {
            return obj1.compareTo(obj2);
        }

        public int hashCode() {
            return "ComparableComparator".hashCode();
        }

        public boolean equals(Object object) {
            return (this == object) || 
                   ((null != object) && (object.getClass().equals(this.getClass())));
        }

    }

}
