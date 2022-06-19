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
package org.apache.commons.lang3.util;

import java.io.Serializable;
import java.util.BitSet;
import java.util.Objects;
import java.util.stream.IntStream;

/**
 * A fluent {@link BitSet} with additional operations.
 * <p>
 * Originally from Apache Commons VFS with more added to act as a fluent replacement for {@link java.util.BitSet}.
 * </p>
 * @since 3.13.0
 */
public final class FluentBitSet implements Cloneable, Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Working BitSet.
     */
    private final BitSet bitSet;

    /**
     * Creates a new bit set. All bits are initially {@code false}.
     */
    public FluentBitSet() {
        this(new BitSet());
    }

    /**
     * Creates a new instance for the given bit set.
     *
     * @param set The bit set to wrap.
     */
    public FluentBitSet(final BitSet set) {
        this.bitSet = Objects.requireNonNull(set, "set");
    }

    /**
     * Creates a bit set whose initial size is large enough to explicitly represent bits with indices in the range {@code 0}
     * through {@code nbits-1}. All bits are initially {@code false}.
     *
     * @param nbits the initial size of the bit set.
     * @throws NegativeArraySizeException if the specified initial size is negative.
     */
    public FluentBitSet(final int nbits) {
        this(new BitSet(nbits));
    }

    /**
     * Performs a logical <b>AND</b> of this target bit set with the argument bit set. This bit set is modified so that each
     * bit in it has the value {@code true} if and only if it both initially had the value {@code true} and the
     * corresponding bit in the bit set argument also had the value {@code true}.
     *
     * @param set a bit set.
     * @return this.
     */
    public FluentBitSet and(final BitSet set) {
        bitSet.and(set);
        return this;
    }

    /**
     * Performs a logical <b>AND</b> of this target bit set with the argument bit set. This bit set is modified so that each
     * bit in it has the value {@code true} if and only if it both initially had the value {@code true} and the
     * corresponding bit in the bit set argument also had the value {@code true}.
     *
     * @param set a bit set.
     * @return this.
     */
    public FluentBitSet and(final FluentBitSet set) {
        bitSet.and(set.bitSet);
        return this;
    }

    /**
     * Clears all of the bits in this {@link BitSet} whose corresponding bit is set in the specified {@link BitSet}.
     *
     * @param set the {@link BitSet} with which to mask this {@link BitSet}.
     * @return this.
     */
    public FluentBitSet andNot(final BitSet set) {
        bitSet.andNot(set);
        return this;
    }

    /**
     * Clears all of the bits in this {@link BitSet} whose corresponding bit is set in the specified {@link BitSet}.
     *
     * @param set the {@link BitSet} with which to mask this {@link BitSet}.
     * @return this.
     */
    public FluentBitSet andNot(final FluentBitSet set) {
        this.bitSet.andNot(set.bitSet);
        return this;
    }

    /**
     * Gets the wrapped bit set.
     *
     * @return the wrapped bit set.
     */
    public BitSet bitSet() {
        return bitSet;
    }

    /**
     * Returns the number of bits set to {@code true} in this {@link BitSet}.
     *
     * @return the number of bits set to {@code true} in this {@link BitSet}.
     */
    public int cardinality() {
        return bitSet.cardinality();
    }

    /**
     * Sets all of the bits in this BitSet to {@code false}.
     *
     * @return this.
     */
    public FluentBitSet clear() {
        bitSet.clear();
        return this;
    }

    /**
     * Sets the bits specified by the indexes to {@code false}.
     *
     * @param bitIndexArray the index of the bit to be cleared.
     * @throws IndexOutOfBoundsException if the specified index is negative.
     * @return this.
     */
    public FluentBitSet clear(final int... bitIndexArray) {
        for (final int e : bitIndexArray) {
            this.bitSet.clear(e);
        }
        return this;
    }

    /**
     * Sets the bit specified by the index to {@code false}.
     *
     * @param bitIndex the index of the bit to be cleared.
     * @throws IndexOutOfBoundsException if the specified index is negative.
     * @return this.
     */
    public FluentBitSet clear(final int bitIndex) {
        bitSet.clear(bitIndex);
        return this;
    }

    /**
     * Sets the bits from the specified {@code fromIndex} (inclusive) to the specified {@code toIndex} (exclusive) to
     * {@code false}.
     *
     * @param fromIndex index of the first bit to be cleared.
     * @param toIndex index after the last bit to be cleared.
     * @throws IndexOutOfBoundsException if {@code fromIndex} is negative, or {@code toIndex} is negative, or
     *         {@code fromIndex} is larger than {@code toIndex}.
     * @return this.
     */
    public FluentBitSet clear(final int fromIndex, final int toIndex) {
        bitSet.clear(fromIndex, toIndex);
        return this;
    }

    /**
     * Cloning this {@link BitSet} produces a new {@link BitSet} that is equal to it. The clone of the bit set is another
     * bit set that has exactly the same bits set to {@code true} as this bit set.
     *
     * @return a clone of this bit set
     * @see #size()
     */
    @Override
    public Object clone() {
        return new FluentBitSet((BitSet) bitSet.clone());
    }

    @Override
    public boolean equals(final Object obj) {
        if (this == obj) {
            return true;
        }
        if (!(obj instanceof FluentBitSet)) {
            return false;
        }
        final FluentBitSet other = (FluentBitSet) obj;
        return Objects.equals(bitSet, other.bitSet);
    }

    /**
     * Sets the bit at the specified index to the complement of its current value.
     *
     * @param bitIndex the index of the bit to flip.
     * @throws IndexOutOfBoundsException if the specified index is negative.
     * @return this.
     */
    public FluentBitSet flip(final int bitIndex) {
        bitSet.flip(bitIndex);
        return this;
    }

    /**
     * Sets each bit from the specified {@code fromIndex} (inclusive) to the specified {@code toIndex} (exclusive) to the
     * complement of its current value.
     *
     * @param fromIndex index of the first bit to flip.
     * @param toIndex index after the last bit to flip.
     * @throws IndexOutOfBoundsException if {@code fromIndex} is negative, or {@code toIndex} is negative, or
     *         {@code fromIndex} is larger than {@code toIndex}.
     * @return this.
     */
    public FluentBitSet flip(final int fromIndex, final int toIndex) {
        bitSet.flip(fromIndex, toIndex);
        return this;
    }

    /**
     * Returns the value of the bit with the specified index. The value is {@code true} if the bit with the index
     * {@code bitIndex} is currently set in this {@link BitSet}; otherwise, the result is {@code false}.
     *
     * @param bitIndex the bit index.
     * @return the value of the bit with the specified index.
     * @throws IndexOutOfBoundsException if the specified index is negative.
     */
    public boolean get(final int bitIndex) {
        return bitSet.get(bitIndex);
    }

    /**
     * Returns a new {@link BitSet} composed of bits from this {@link BitSet} from {@code fromIndex} (inclusive) to
     * {@code toIndex} (exclusive).
     *
     * @param fromIndex index of the first bit to include.
     * @param toIndex index after the last bit to include.
     * @return a new {@link BitSet} from a range of this {@link BitSet}.
     * @throws IndexOutOfBoundsException if {@code fromIndex} is negative, or {@code toIndex} is negative, or
     *         {@code fromIndex} is larger than {@code toIndex}.
     */
    public FluentBitSet get(final int fromIndex, final int toIndex) {
        return new FluentBitSet(bitSet.get(fromIndex, toIndex));
    }

    @Override
    public int hashCode() {
        return bitSet.hashCode();
    }

    /**
     * Returns true if the specified {@link BitSet} has any bits set to {@code true} that are also set to {@code true} in
     * this {@link BitSet}.
     *
     * @param set {@link BitSet} to intersect with.
     * @return boolean indicating whether this {@link BitSet} intersects the specified {@link BitSet}.
     */
    public boolean intersects(final BitSet set) {
        return bitSet.intersects(set);
    }

    /**
     * Returns true if the specified {@link BitSet} has any bits set to {@code true} that are also set to {@code true} in
     * this {@link BitSet}.
     *
     * @param set {@link BitSet} to intersect with.
     * @return boolean indicating whether this {@link BitSet} intersects the specified {@link BitSet}.
     */
    public boolean intersects(final FluentBitSet set) {
        return bitSet.intersects(set.bitSet);
    }

    /**
     * Returns true if this {@link BitSet} contains no bits that are set to {@code true}.
     *
     * @return boolean indicating whether this {@link BitSet} is empty.
     */
    public boolean isEmpty() {
        return bitSet.isEmpty();
    }

    /**
     * Returns the "logical size" of this {@link BitSet}: the index of the highest set bit in the {@link BitSet} plus one.
     * Returns zero if the {@link BitSet} contains no set bits.
     *
     * @return the logical size of this {@link BitSet}.
     */
    public int length() {
        return bitSet.length();
    }

    /**
     * Returns the index of the first bit that is set to {@code false} that occurs on or after the specified starting index.
     *
     * @param fromIndex the index to start checking from (inclusive).
     * @return the index of the next clear bit.
     * @throws IndexOutOfBoundsException if the specified index is negative.
     */
    public int nextClearBit(final int fromIndex) {
        return bitSet.nextClearBit(fromIndex);
    }

    /**
     * Returns the index of the first bit that is set to {@code true} that occurs on or after the specified starting index.
     * If no such bit exists then {@code -1} is returned.
     * <p>
     * To iterate over the {@code true} bits in a {@link BitSet}, use the following loop:
     * </p>
     *
     * <pre>
     * {@code
     * for (int i = bs.nextSetBit(0); i >= 0; i = bs.nextSetBit(i+1)) {
     *     // operate on index i here
     *     if (i == Integer.MAX_VALUE) {
     *         break; // or (i+1) would overflow
     *     }
     * }}
     * </pre>
     *
     * @param fromIndex the index to start checking from (inclusive).
     * @return the index of the next set bit, or {@code -1} if there is no such bit.
     * @throws IndexOutOfBoundsException if the specified index is negative.
     */
    public int nextSetBit(final int fromIndex) {
        return bitSet.nextSetBit(fromIndex);
    }

    /**
     * Performs a logical <b>OR</b> of this bit set with the bit set argument. This bit set is modified so that a bit in it
     * has the value {@code true} if and only if it either already had the value {@code true} or the corresponding bit in
     * the bit set argument has the value {@code true}.
     *
     * @param set a bit set.
     * @return this.
     */
    public FluentBitSet or(final BitSet set) {
        bitSet.or(set);
        return this;
    }

    /**
     * Performs a logical <b>OR</b> of this bit set with the bit set arguments. This bit set is modified so that a bit in it
     * has the value {@code true} if and only if it either already had the value {@code true} or the corresponding bit in
     * the bit set argument has the value {@code true}.
     *
     * @param set a bit set.
     * @return this.
     */
    public FluentBitSet or(final FluentBitSet... set) {
        for (final FluentBitSet e : set) {
            this.bitSet.or(e.bitSet);
        }
        return this;
    }

    /**
     * Performs a logical <b>OR</b> of this bit set with the bit set argument. This bit set is modified so that a bit in it
     * has the value {@code true} if and only if it either already had the value {@code true} or the corresponding bit in
     * the bit set argument has the value {@code true}.
     *
     * @param set a bit set.
     * @return this.
     */
    public FluentBitSet or(final FluentBitSet set) {
        this.bitSet.or(set.bitSet);
        return this;
    }

    /**
     * Returns the index of the nearest bit that is set to {@code false} that occurs on or before the specified starting
     * index. If no such bit exists, or if {@code -1} is given as the starting index, then {@code -1} is returned.
     *
     * @param fromIndex the index to start checking from (inclusive).
     * @return the index of the previous clear bit, or {@code -1} if there is no such bit.
     * @throws IndexOutOfBoundsException if the specified index is less than {@code -1}.
     */
    public int previousClearBit(final int fromIndex) {
        return bitSet.previousClearBit(fromIndex);
    }

    /**
     * Returns the index of the nearest bit that is set to {@code true} that occurs on or before the specified starting
     * index. If no such bit exists, or if {@code -1} is given as the starting index, then {@code -1} is returned.
     *
     * <p>
     * To iterate over the {@code true} bits in a {@link BitSet}, use the following loop:
     *
     * <pre>
     *  {@code
     * for (int i = bs.length(); (i = bs.previousSetBit(i-1)) >= 0; ) {
     *     // operate on index i here
     * }}
     * </pre>
     *
     * @param fromIndex the index to start checking from (inclusive)
     * @return the index of the previous set bit, or {@code -1} if there is no such bit
     * @throws IndexOutOfBoundsException if the specified index is less than {@code -1}
     */
    public int previousSetBit(final int fromIndex) {
        return bitSet.previousSetBit(fromIndex);
    }

    /**
     * Sets the bit at the specified indexes to {@code true}.
     *
     * @param bitIndexArray a bit index array.
     * @throws IndexOutOfBoundsException if the specified index is negative.
     * @return this.
     */
    public FluentBitSet set(final int... bitIndexArray) {
        for (final int e : bitIndexArray) {
            bitSet.set(e);
        }
        return this;
    }

    /**
     * Sets the bit at the specified index to {@code true}.
     *
     * @param bitIndex a bit index
     * @throws IndexOutOfBoundsException if the specified index is negative
     * @return this.
     */
    public FluentBitSet set(final int bitIndex) {
        bitSet.set(bitIndex);
        return this;
    }

    /**
     * Sets the bit at the specified index to the specified value.
     *
     * @param bitIndex a bit index.
     * @param value a boolean value to set.
     * @throws IndexOutOfBoundsException if the specified index is negative.
     * @return this.
     */
    public FluentBitSet set(final int bitIndex, final boolean value) {
        bitSet.set(bitIndex, value);
        return this;
    }

    /**
     * Sets the bits from the specified {@code fromIndex} (inclusive) to the specified {@code toIndex} (exclusive) to
     * {@code true}.
     *
     * @param fromIndex index of the first bit to be set.
     * @param toIndex index after the last bit to be set.
     * @throws IndexOutOfBoundsException if {@code fromIndex} is negative, or {@code toIndex} is negative, or
     *         {@code fromIndex} is larger than {@code toIndex}.
     * @return this.
     */
    public FluentBitSet set(final int fromIndex, final int toIndex) {
        bitSet.set(fromIndex, toIndex);
        return this;
    }

    /**
     * Sets the bits from the specified {@code fromIndex} (inclusive) to the specified {@code toIndex} (exclusive) to the
     * specified value.
     *
     * @param fromIndex index of the first bit to be set.
     * @param toIndex index after the last bit to be set.
     * @param value value to set the selected bits to.
     * @throws IndexOutOfBoundsException if {@code fromIndex} is negative, or {@code toIndex} is negative, or
     *         {@code fromIndex} is larger than {@code toIndex}.
     * @return this.
     */
    public FluentBitSet set(final int fromIndex, final int toIndex, final boolean value) {
        bitSet.set(fromIndex, toIndex, value);
        return this;
    }

    /**
     * Sets the bits from the specified {@code fromIndex} (inclusive) to the specified {@code toIndex} (exclusive) to
     * {@code true}.
     *
     * @param fromIndex index of the first bit to be set
     * @param toIndex index of the last bit to be set
     * @throws IndexOutOfBoundsException if {@code fromIndex} is negative, or {@code toIndex} is negative, or
     *         {@code fromIndex} is larger than {@code toIndex}
     * @return this.
     */
    public FluentBitSet setInclusive(final int fromIndex, final int toIndex) {
        bitSet.set(fromIndex, toIndex + 1);
        return this;
    }

    /**
     * Returns the number of bits of space actually in use by this {@link BitSet} to represent bit values. The maximum
     * element in the set is the size - 1st element.
     *
     * @return the number of bits currently in this bit set.
     */
    public int size() {
        return bitSet.size();
    }

    /**
     * Returns a stream of indices for which this {@link BitSet} contains a bit in the set state. The indices are returned
     * in order, from lowest to highest. The size of the stream is the number of bits in the set state, equal to the value
     * returned by the {@link #cardinality()} method.
     *
     * <p>
     * The bit set must remain constant during the execution of the terminal stream operation. Otherwise, the result of the
     * terminal stream operation is undefined.
     * </p>
     *
     * @return a stream of integers representing set indices.
     * @since 1.8
     */
    public IntStream stream() {
        return bitSet.stream();
    }

    /**
     * Returns a new byte array containing all the bits in this bit set.
     *
     * <p>
     * More precisely, if:
     * </p>
     * <ol>
     * <li>{@code byte[] bytes = s.toByteArray();}</li>
     * <li>then {@code bytes.length == (s.length()+7)/8} and</li>
     * <li>{@code s.get(n) == ((bytes[n/8] & (1<<(n%8))) != 0)}</li>
     * <li>for all {@code n < 8 * bytes.length}.</li>
     * </ol>
     *
     * @return a byte array containing a little-endian representation of all the bits in this bit set
     */
    public byte[] toByteArray() {
        return bitSet.toByteArray();
    }

    /**
     * Returns a new byte array containing all the bits in this bit set.
     *
     * <p>
     * More precisely, if:
     * </p>
     * <ol>
     * <li>{@code long[] longs = s.toLongArray();}</li>
     * <li>then {@code longs.length == (s.length()+63)/64} and</li>
     * <li>{@code s.get(n) == ((longs[n/64] & (1L<<(n%64))) != 0)}</li>
     * <li>for all {@code n < 64 * longs.length}.</li>
     * </ol>
     *
     * @return a byte array containing a little-endian representation of all the bits in this bit set
     */
    public long[] toLongArray() {
        return bitSet.toLongArray();
    }

    @Override
    public String toString() {
        return bitSet.toString();
    }

    /**
     * Performs a logical <b>XOR</b> of this bit set with the bit set argument. This bit set is modified so that a bit in it
     * has the value {@code true} if and only if one of the following statements holds:
     * <ul>
     * <li>The bit initially has the value {@code true}, and the corresponding bit in the argument has the value
     * {@code false}.
     * <li>The bit initially has the value {@code false}, and the corresponding bit in the argument has the value
     * {@code true}.
     * </ul>
     *
     * @param set a bit set
     * @return this.
     */
    public FluentBitSet xor(final BitSet set) {
        bitSet.xor(set);
        return this;
    }

    /**
     * Performs a logical <b>XOR</b> of this bit set with the bit set argument. This bit set is modified so that a bit in it
     * has the value {@code true} if and only if one of the following statements holds:
     * <ul>
     * <li>The bit initially has the value {@code true}, and the corresponding bit in the argument has the value
     * {@code false}.
     * <li>The bit initially has the value {@code false}, and the corresponding bit in the argument has the value
     * {@code true}.
     * </ul>
     *
     * @param set a bit set
     * @return this.
     */
    public FluentBitSet xor(final FluentBitSet set) {
        bitSet.xor(set.bitSet);
        return this;
    }

}
