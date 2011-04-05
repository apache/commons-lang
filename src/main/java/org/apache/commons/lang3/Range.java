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
package org.apache.commons.lang3;

import java.io.Serializable;
import java.util.Comparator;

/**
 * <p>{@code Range} represents an immutable range of comparables of the same type.</p>
 * <p>The objects need to either be implementations of {@code java.lang.Comparable}
 * or you need to supply a {@code java.util.Comparator}. </p>
 *
 * <p>#ThreadSafe# if the comparables are thread-safe</p>
 * @since 3.0
 * @version $Id$
 */
public final class Range<T> implements Serializable {

    /**
     * Required for serialization support.
     *
     * @see java.io.Serializable
     */
    private static final long serialVersionUID = 1L;

    /**
     * The ordering scheme used in this range.
     */
    private final Comparator<T> comparator;

    /**
     * The minimum value in this range (inclusive).
     */
    private final T minimum;

    /**
     * The maximum value in this range (inclusive).
     */
    private final T maximum;

    /**
     * Cached output hashCode (class is immutable).
     */
    private transient int hashCode = 0;

    /**
     * Cached output toString (class is immutable).
     */
    private transient String toString = null;

    /**
     * <p>Constructs a new {@code Range} using the specified
     * element as both the minimum and maximum in this range.</p>
     * <p>The range uses the natural ordering of the elements to
     * determine where values lie in the range.</p>
     *
     * @param <T> the type of this {@code Range}
     * @param element  the value to use for this range, must not be {@code null}
     * @return the new range object
     * @throws IllegalArgumentException if the value is {@code null}
     * @throws ClassCastException if the value is not Comparable
     */
    public static <T extends Comparable<T>> Range<T> is(T element) {
        return new Range<T>(element, element, ComparableComparator.<T>getInstance());
    }

    /**
     * <p>Constructs a new {@code Range} with the specified
     * minimum and maximum values (both inclusive).</p>
     * <p>The range uses the natural ordering of the elements to
     * determine where values lie in the range.</p>
     *
     * <p>The arguments may be passed in the order (min,max) or (max,min). The
     * getMinimum and getMaximum methods will return the correct values.</p>
     *
     * @param <T> the type of this {@code Range}
     * @param element1  first value that defines the edge of the range, inclusive
     * @param element2  second value that defines the edge of the range, inclusive
     * @return the new range object
     * @throws IllegalArgumentException if either value is {@code null}
     * @throws ClassCastException if either value is not Comparable
     */
    public static <T extends Comparable<T>> Range<T> between(T element1, T element2) {
        return new Range<T>( element1, element2, ComparableComparator.<T>getInstance());
    }

    /**
     * <p>Constructs a new {@code Range} using the specified
     * element as both the minimum and maximum in this range.</p>
     * <p>The range uses the passed in {@code Comparator} to
     * determine where values lie in the range.</p>
     *
     * @param <T> the type of this {@code Range}
     * @param element  the value to use for this range, must not be {@code null}
     * @param c comparator to be used
     * @return the new range object
     * @throws IllegalArgumentException if the value is {@code null}
     */
    public static <T> Range<T> is(T element, Comparator<T> c) {
        return new Range<T>(element, element, c);
    }

    /**
     * <p>Constructs a new {@code Range} with the specified
     * minimum and maximum values (both inclusive).</p>
     * <p>The range uses the passed in {@code Comparator} to
     * determine where values lie in the range.</p>
     *
     * <p>The arguments may be passed in the order (min,max) or (max,min). The
     * getMinimum and getMaximum methods will return the correct values.</p>
     *
     * @param <T> the type of this {@code Range}
     * @param element1  first value that defines the edge of the range, inclusive
     * @param element2  second value that defines the edge of the range, inclusive
     * @param c comparator to be used
     * @return the new range object
     * @throws IllegalArgumentException if either value is {@code null}
     */
    public static <T> Range<T> between(T element1, T element2, Comparator<T> c) {
        return new Range<T>(element1, element2, c);
    }

    /**
     * Creates a new instance of {@code Range}.
     *
     * @param element1 the first element
     * @param element2 the second element
     * @param c the comparator to be used
     */
    private Range(T element1, T element2, Comparator<T> c) {
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
    public Comparator<T> getComparator() {
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
     * <p>{@code null} is handled and returns {@code false}.</p>
     *
     * @param element  the element to test, may be {@code null}
     * @return {@code true} if the specified element occurs within this range
     */
    public boolean contains(T element) {
        if(element == null) {
            return false;
        }
        return (comparator.compare(element, this.minimum) > -1) && (comparator.compare(element, this.maximum) < 1);
    }

    /**
     * <p>Tests whether the specified element occurs before this range.</p>
     *
     * <p>{@code null} is handled and returns {@code false}.</p>
     *
     * @param element  the element to test, may be {@code null}
     * @return {@code true} if the specified element occurs before this range
     */
    public boolean elementBefore(T element) {
        if (element == null) {
            return false;
        }

        return this.comparator.compare(element, this.minimum) < 0;
    }

    /**
     * <p>Tests whether the specified element occurs after this range.</p>
     *
     * <p>{@code null} is handled and returns {@code false}.</p>
     *
     * @param element  the element to test, may be {@code null}
     * @return {@code true} if the specified element occurs after this range
     */
    public boolean elementAfter(T element) {
        if (element == null) {
            return false;
        }

        return this.comparator.compare(element, this.maximum) > 0;
    }

    /**
     * <p>Tests where the specified element occurs relative to this range.</p>
     * <p>The API is reminiscent of the Comparable interface returning {@code -1} if
     * the element is before the range, {@code 0} if contained within the range and
     * {@code 1} if the element is after the range. </p>
     *
     * @param element  the element to test
     * @return -1, 0 or +1 depending on the element's location relative to the range
     */
    public int elementCompareTo(T element) {
        if(element == null) {
            // Comparable API says throw NPE on null
            throw new NullPointerException("Element is null");
        }
        if(elementBefore(element)) {
            return -1;
        } else
        if(elementAfter(element)) {
            return 1;
        } else {
            return 0;
        }
    }

    // Range tests
    //--------------------------------------------------------------------

    /**
     * <p>Tests whether the specified range occurs entirely within this range.</p>
     *
     * <p>{@code null} is handled and returns {@code false}.</p>
     *
     * @param range  the range to test, may be {@code null}
     * @return {@code true} if the specified range occurs entirely within
     *  this range; otherwise, {@code false}
     * @throws IllegalArgumentException if the {@code Range} cannot be compared
     */
    public boolean containsAll(Range<T> range) {
        if (range == null) {
            return false;
        }
        return contains(range.getMinimum())
            && contains(range.getMaximum());
    }

    /**
     * <p>Tests whether the specified range overlaps with this range.</p>
     *
     * <p>{@code null} is handled and returns {@code false}.</p>
     *
     * @param range  the range to test, may be {@code null}
     * @return {@code true} if the specified range overlaps with this
     *  range; otherwise, {@code false}
     * @throws IllegalArgumentException if the {@code Range} cannot be compared
     */
    public boolean overlapsWith(Range<T> range) {
        if (range == null) {
            return false;
        }
        return range.contains(this.minimum)
            || range.contains(this.maximum)
            || contains(range.getMinimum());
    }

    // Basics
    //--------------------------------------------------------------------

    /**
     * <p>Compares this range to another object to test if they are equal.</p>.
     *
     * <p>To be equal, the class, minimum and maximum must be equal.</p>
     *
     * @param obj the reference object with which to compare
     * @return {@code true} if this object is equal
     */
    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        } else if (obj == null || obj.getClass() != getClass()) {
            return false;
        } else {
            @SuppressWarnings("unchecked") // OK because we checked the class above
            Range<T> range = (Range<T>) obj;
            return getMinimum().equals(range.getMinimum()) &&
                   getMaximum().equals(range.getMaximum());
        }
    }

    /**
     * <p>Gets a hashCode for the range.</p>
     *
     * @return a hash code value for this object
     */
    @Override
    public int hashCode() {
        int result = hashCode;
        if (hashCode == 0) {
            result = 17;
            result = 37 * result + getClass().hashCode();
            result = 37 * result + this.minimum.hashCode();
            result = 37 * result + this.maximum.hashCode();
            hashCode = result;
        }
        return result;
    }

    /**
     * <p>Gets the range as a {@code String}.</p>
     *
     * <p>The format of the String is 'Range[<i>min</i>,<i>max</i>]'.</p>
     *
     * @return the {@code String} representation of this range
     */
    @Override
    public String toString() {
        String result = toString;
        if (result == null) {
            StringBuilder buf = new StringBuilder(32);
            buf.append("Range[");
            buf.append(this.minimum);
            buf.append(',');
            buf.append(this.maximum);
            buf.append(']');
            result = buf.toString();
            toString = result;
        }
        return result;
    }

    // Taken from Commons Collections - documentation removed as not a public class
    private static class ComparableComparator<E extends Comparable<? super E>> implements Comparator<E>, Serializable {

        private static final long serialVersionUID = 1L;

        @SuppressWarnings("rawtypes") // Comparator works for all types
        public static final ComparableComparator<?> INSTANCE = new ComparableComparator();

        /**
         * Returns a comparator for the specified {@code Comparable} type.
         *
         * @param <E> the {@code Comparable} type
         * @return the comparator for this type
         */
        @SuppressWarnings("unchecked") // OK to cast, because comparator works for all types
        public static <E extends Comparable<? super E>> ComparableComparator<E> getInstance() {
            return (ComparableComparator<E>) INSTANCE;
        }

        /**
         * Creates a new instance of {@code ComparableComparator}.
         */
        public ComparableComparator() {
            super();
        }

        /**
         * Compares two objects.
         *
         * @param obj1 the first object
         * @param obj2 the second object
         * @return the result of the comparison
         */
        public int compare(E obj1, E obj2) {
            return obj1.compareTo(obj2);
        }

        @Override
        public int hashCode() {
            return "ComparableComparator".hashCode();
        }

        @Override
        public boolean equals(Object object) {
            return (this == object) ||
                   ((null != object) && (object.getClass().equals(this.getClass())));
        }

    }

}
