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
import java.util.Objects;

/**
 * An immutable range of objects from a minimum to maximum point inclusive.
 *
 * <p>The objects need to either be implementations of {@link Comparable}
 * or you need to supply a {@link Comparator}.</p>
 *
 * <p>#ThreadSafe# if the objects and comparator are thread-safe.</p>
 *
 * @param <T> The type of range values.
 * @since 3.0
 */
public class Range<T> implements Serializable {

    @SuppressWarnings({"rawtypes", "unchecked"})
    private enum ComparableComparator implements Comparator {
        INSTANCE;

        /**
         * Comparable based compare implementation.
         *
         * @param obj1 left-hand side of comparison
         * @param obj2 right-hand side of comparison
         * @return negative, 0, positive comparison value
         */
        @Override
        public int compare(final Object obj1, final Object obj2) {
            return ((Comparable) obj1).compareTo(obj2);
        }
    }

    /**
     * Serialization version.
     *
     * @see java.io.Serializable
     */
    private static final long serialVersionUID = 1L;

    /**
     * Creates a range with the specified minimum and maximum values (both inclusive).
     *
     * <p>The range uses the natural ordering of the elements to determine where
     * values lie in the range.</p>
     *
     * <p>The arguments may be passed in the order (min,max) or (max,min).
     * The getMinimum and getMaximum methods will return the correct values.</p>
     *
     * @param <T> the type of the elements in this range
     * @param fromInclusive  the first value that defines the edge of the range, inclusive
     * @param toInclusive  the second value that defines the edge of the range, inclusive
     * @return the range object, not null
     * @throws NullPointerException when fromInclusive is null.
     * @throws NullPointerException when toInclusive is null.
     * @throws ClassCastException if the elements are not {@link Comparable}
     * @deprecated Use {@link #of(Comparable, Comparable)}.
     */
    @Deprecated
    public static <T extends Comparable<? super T>> Range<T> between(final T fromInclusive, final T toInclusive) {
        return of(fromInclusive, toInclusive, null);
    }

    /**
     * Creates a range with the specified minimum and maximum values (both inclusive).
     *
     * <p>The range uses the specified {@link Comparator} to determine where
     * values lie in the range.</p>
     *
     * <p>The arguments may be passed in the order (min,max) or (max,min).
     * The getMinimum and getMaximum methods will return the correct values.</p>
     *
     * @param <T> the type of the elements in this range
     * @param fromInclusive  the first value that defines the edge of the range, inclusive
     * @param toInclusive  the second value that defines the edge of the range, inclusive
     * @param comparator  the comparator to be used, null for natural ordering
     * @return the range object, not null
     * @throws NullPointerException when fromInclusive is null.
     * @throws NullPointerException when toInclusive is null.
     * @throws ClassCastException if using natural ordering and the elements are not {@link Comparable}
     * @deprecated Use {@link #of(Object, Object, Comparator)}.
     */
    @Deprecated
    public static <T> Range<T> between(final T fromInclusive, final T toInclusive, final Comparator<T> comparator) {
        return new Range<>(fromInclusive, toInclusive, comparator);
    }

    /**
     * Creates a range using the specified element as both the minimum
     * and maximum in this range.
     *
     * <p>The range uses the natural ordering of the elements to determine where
     * values lie in the range.</p>
     *
     * @param <T> the type of the elements in this range
     * @param element  the value to use for this range, not null
     * @return the range object, not null
     * @throws NullPointerException if the element is null
     * @throws ClassCastException if the element is not {@link Comparable}
     */
    public static <T extends Comparable<? super T>> Range<T> is(final T element) {
        return of(element, element, null);
    }

    /**
     * Creates a range using the specified element as both the minimum
     * and maximum in this range.
     *
     * <p>The range uses the specified {@link Comparator} to determine where
     * values lie in the range.</p>
     *
     * @param <T> the type of the elements in this range
     * @param element  the value to use for this range, must not be {@code null}
     * @param comparator  the comparator to be used, null for natural ordering
     * @return the range object, not null
     * @throws NullPointerException if the element is null
     * @throws ClassCastException if using natural ordering and the elements are not {@link Comparable}
     */
    public static <T> Range<T> is(final T element, final Comparator<T> comparator) {
        return of(element, element, comparator);
    }

    /**
     * Creates a range with the specified minimum and maximum values (both inclusive).
     *
     * <p>The range uses the natural ordering of the elements to determine where
     * values lie in the range.</p>
     *
     * <p>The arguments may be passed in the order (min,max) or (max,min).
     * The getMinimum and getMaximum methods will return the correct values.</p>
     *
     * @param <T> the type of the elements in this range
     * @param fromInclusive  the first value that defines the edge of the range, inclusive
     * @param toInclusive  the second value that defines the edge of the range, inclusive
     * @return the range object, not null
     * @throws NullPointerException if either element is null
     * @throws ClassCastException if the elements are not {@link Comparable}
     * @since 3.13.0
     */
    public static <T extends Comparable<? super T>> Range<T> of(final T fromInclusive, final T toInclusive) {
        return of(fromInclusive, toInclusive, null);
    }

    /**
     * Creates a range with the specified minimum and maximum values (both inclusive).
     *
     * <p>The range uses the specified {@link Comparator} to determine where
     * values lie in the range.</p>
     *
     * <p>The arguments may be passed in the order (min,max) or (max,min).
     * The getMinimum and getMaximum methods will return the correct values.</p>
     *
     * @param <T> the type of the elements in this range
     * @param fromInclusive  the first value that defines the edge of the range, inclusive
     * @param toInclusive  the second value that defines the edge of the range, inclusive
     * @param comparator  the comparator to be used, null for natural ordering
     * @return the range object, not null
     * @throws NullPointerException when fromInclusive is null.
     * @throws NullPointerException when toInclusive is null.
     * @throws ClassCastException if using natural ordering and the elements are not {@link Comparable}
     * @since 3.13.0
     */
    public static <T> Range<T> of(final T fromInclusive, final T toInclusive, final Comparator<T> comparator) {
        return new Range<>(fromInclusive, toInclusive, comparator);
    }

    /**
     * The ordering scheme used in this range.
     */
    private final Comparator<T> comparator;

    /**
     * Cached output hashCode (class is immutable).
     */
    private transient int hashCode;

    /**
     * The maximum value in this range (inclusive).
     */
    private final T maximum;

    /**
     * The minimum value in this range (inclusive).
     */
    private final T minimum;

    /**
     * Cached output toString (class is immutable).
     */
    private transient String toString;

    /**
     * Creates an instance.
     *
     * @param element1  the first element, not null
     * @param element2  the second element, not null
     * @param comp  the comparator to be used, null for natural ordering
     * @throws NullPointerException when element1 is null.
     * @throws NullPointerException when element2 is null.
     */
    @SuppressWarnings("unchecked")
    Range(final T element1, final T element2, final Comparator<T> comp) {
        Objects.requireNonNull(element1, "element1");
        Objects.requireNonNull(element2, "element2");
        if (comp == null) {
            this.comparator = ComparableComparator.INSTANCE;
        } else {
            this.comparator = comp;
        }
        if (this.comparator.compare(element1, element2) < 1) {
            this.minimum = element1;
            this.maximum = element2;
        } else {
            this.minimum = element2;
            this.maximum = element1;
        }
    }

    /**
     * Checks whether the specified element occurs within this range.
     *
     * @param element  the element to check for, null returns false
     * @return true if the specified element occurs within this range
     */
    public boolean contains(final T element) {
        if (element == null) {
            return false;
        }
        return comparator.compare(element, minimum) > -1 && comparator.compare(element, maximum) < 1;
    }

    /**
     * Checks whether this range contains all the elements of the specified range.
     *
     * <p>This method may fail if the ranges have two different comparators or element types.</p>
     *
     * @param otherRange  the range to check, null returns false
     * @return true if this range contains the specified range
     * @throws RuntimeException if ranges cannot be compared
     */
    public boolean containsRange(final Range<T> otherRange) {
        if (otherRange == null) {
            return false;
        }
        return contains(otherRange.minimum)
            && contains(otherRange.maximum);
    }

    /**
     * Checks where the specified element occurs relative to this range.
     *
     * <p>The API is reminiscent of the Comparable interface returning {@code -1} if
     * the element is before the range, {@code 0} if contained within the range and
     * {@code 1} if the element is after the range.</p>
     *
     * @param element  the element to check for, not null
     * @return -1, 0 or +1 depending on the element's location relative to the range
     */
    public int elementCompareTo(final T element) {
        // Comparable API says throw NPE on null
        Objects.requireNonNull(element, "element");
        if (isAfter(element)) {
            return -1;
        }
        if (isBefore(element)) {
            return 1;
        }
        return 0;
    }

    /**
     * Compares this range to another object to test if they are equal..
     *
     * <p>To be equal, the minimum and maximum values must be equal, which
     * ignores any differences in the comparator.</p>
     *
     * @param obj the reference object with which to compare
     * @return true if this object is equal
     */
    @Override
    public boolean equals(final Object obj) {
        if (obj == this) {
            return true;
        }
        if (obj == null || obj.getClass() != getClass()) {
            return false;
        }
        @SuppressWarnings("unchecked") // OK because we checked the class above
        final
        Range<T> range = (Range<T>) obj;
        return minimum.equals(range.minimum) &&
               maximum.equals(range.maximum);
    }

    /**
     * Fits the given element into this range by returning the given element or, if out of bounds, the range minimum if
     * below, or the range maximum if above.
     *
     * <pre>
     * Range&lt;Integer&gt; range = Range.between(16, 64);
     * range.fit(-9) --&gt;  16
     * range.fit(0)  --&gt;  16
     * range.fit(15) --&gt;  16
     * range.fit(16) --&gt;  16
     * range.fit(17) --&gt;  17
     * ...
     * range.fit(63) --&gt;  63
     * range.fit(64) --&gt;  64
     * range.fit(99) --&gt;  64
     * </pre>
     * @param element the element to check for, not null
     * @return the minimum, the element, or the maximum depending on the element's location relative to the range
     * @since 3.10
     */
    public T fit(final T element) {
        // Comparable API says throw NPE on null
        Objects.requireNonNull(element, "element");
        if (isAfter(element)) {
            return minimum;
        }
        if (isBefore(element)) {
            return maximum;
        }
        return element;
    }

    /**
     * Gets the comparator being used to determine if objects are within the range.
     *
     * <p>Natural ordering uses an internal comparator implementation, thus this
     * method never returns null. See {@link #isNaturalOrdering()}.</p>
     *
     * @return the comparator being used, not null
     */
    public Comparator<T> getComparator() {
        return comparator;
    }

    /**
     * Gets the maximum value in this range.
     *
     * @return the maximum value in this range, not null
     */
    public T getMaximum() {
        return maximum;
    }

    /**
     * Gets the minimum value in this range.
     *
     * @return the minimum value in this range, not null
     */
    public T getMinimum() {
        return minimum;
    }

    /**
     * Gets a suitable hash code for the range.
     *
     * @return a hash code value for this object
     */
    @Override
    public int hashCode() {
        int result = hashCode;
        if (hashCode == 0) {
            result = 17;
            result = 37 * result + getClass().hashCode();
            result = 37 * result + minimum.hashCode();
            result = 37 * result + maximum.hashCode();
            hashCode = result;
        }
        return result;
    }

    /**
     * Calculate the intersection of {@code this} and an overlapping Range.
     * @param other overlapping Range
     * @return range representing the intersection of {@code this} and {@code other} ({@code this} if equal)
     * @throws IllegalArgumentException if {@code other} does not overlap {@code this}
     * @since 3.0.1
     */
    public Range<T> intersectionWith(final Range<T> other) {
        if (!this.isOverlappedBy(other)) {
            throw new IllegalArgumentException(String.format(
                "Cannot calculate intersection with non-overlapping range %s", other));
        }
        if (this.equals(other)) {
            return this;
        }
        final T min = getComparator().compare(minimum, other.minimum) < 0 ? other.minimum : minimum;
        final T max = getComparator().compare(maximum, other.maximum) < 0 ? maximum : other.maximum;
        return of(min, max, getComparator());
    }

    /**
     * Checks whether this range is after the specified element.
     *
     * @param element  the element to check for, null returns false
     * @return true if this range is entirely after the specified element
     */
    public boolean isAfter(final T element) {
        if (element == null) {
            return false;
        }
        return comparator.compare(element, minimum) < 0;
    }

    /**
     * Checks whether this range is completely after the specified range.
     *
     * <p>This method may fail if the ranges have two different comparators or element types.</p>
     *
     * @param otherRange  the range to check, null returns false
     * @return true if this range is completely after the specified range
     * @throws RuntimeException if ranges cannot be compared
     */
    public boolean isAfterRange(final Range<T> otherRange) {
        if (otherRange == null) {
            return false;
        }
        return isAfter(otherRange.maximum);
    }

    /**
     * Checks whether this range is before the specified element.
     *
     * @param element  the element to check for, null returns false
     * @return true if this range is entirely before the specified element
     */
    public boolean isBefore(final T element) {
        if (element == null) {
            return false;
        }
        return comparator.compare(element, maximum) > 0;
    }

    /**
     * Checks whether this range is completely before the specified range.
     *
     * <p>This method may fail if the ranges have two different comparators or element types.</p>
     *
     * @param otherRange  the range to check, null returns false
     * @return true if this range is completely before the specified range
     * @throws RuntimeException if ranges cannot be compared
     */
    public boolean isBeforeRange(final Range<T> otherRange) {
        if (otherRange == null) {
            return false;
        }
        return isBefore(otherRange.minimum);
    }

    /**
     * Checks whether this range ends with the specified element.
     *
     * @param element  the element to check for, null returns false
     * @return true if the specified element occurs within this range
     */
    public boolean isEndedBy(final T element) {
        if (element == null) {
            return false;
        }
        return comparator.compare(element, maximum) == 0;
    }

    /**
     * Whether or not the Range is using the natural ordering of the elements.
     *
     * <p>Natural ordering uses an internal comparator implementation, thus this
     * method is the only way to check if a null comparator was specified.</p>
     *
     * @return true if using natural ordering
     */
    public boolean isNaturalOrdering() {
        return comparator == ComparableComparator.INSTANCE;
    }

    /**
     * Checks whether this range is overlapped by the specified range.
     *
     * <p>Two ranges overlap if there is at least one element in common.</p>
     *
     * <p>This method may fail if the ranges have two different comparators or element types.</p>
     *
     * @param otherRange  the range to test, null returns false
     * @return true if the specified range overlaps with this
     *  range; otherwise, {@code false}
     * @throws RuntimeException if ranges cannot be compared
     */
    public boolean isOverlappedBy(final Range<T> otherRange) {
        if (otherRange == null) {
            return false;
        }
        return otherRange.contains(minimum)
            || otherRange.contains(maximum)
            || contains(otherRange.minimum);
    }

    /**
     * Checks whether this range starts with the specified element.
     *
     * @param element  the element to check for, null returns false
     * @return true if the specified element occurs within this range
     */
    public boolean isStartedBy(final T element) {
        if (element == null) {
            return false;
        }
        return comparator.compare(element, minimum) == 0;
    }

    /**
     * Gets the range as a {@link String}.
     *
     * <p>The format of the String is '[<i>min</i>..<i>max</i>]'.</p>
     *
     * @return the {@link String} representation of this range
     */
    @Override
    public String toString() {
        if (toString == null) {
            toString = "[" + minimum + ".." + maximum + "]";
        }
        return toString;
    }

    /**
     * Formats the receiver using the given format.
     *
     * <p>This uses {@link java.util.Formattable} to perform the formatting. Three variables may
     * be used to embed the minimum, maximum and comparator.
     * Use {@code %1$s} for the minimum element, {@code %2$s} for the maximum element
     * and {@code %3$s} for the comparator.
     * The default format used by {@code toString()} is {@code [%1$s..%2$s]}.</p>
     *
     * @param format  the format string, optionally containing {@code %1$s}, {@code %2$s} and  {@code %3$s}, not null
     * @return the formatted string, not null
     */
    public String toString(final String format) {
        return String.format(format, minimum, maximum, comparator);
    }

}
