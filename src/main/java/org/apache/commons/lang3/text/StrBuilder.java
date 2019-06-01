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
package org.apache.commons.lang3.text;

import java.io.IOException;
import java.io.Reader;
import java.io.Serializable;
import java.io.Writer;
import java.nio.CharBuffer;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.CharUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.Builder;

import org.checkerframework.checker.index.qual.NonNegative;
import org.checkerframework.checker.index.qual.GTENegativeOne;
import org.checkerframework.checker.index.qual.LTEqLengthOf;
import org.checkerframework.checker.index.qual.IndexFor;
import org.checkerframework.checker.index.qual.LTLengthOf;
import org.checkerframework.checker.index.qual.IndexOrLow;
import org.checkerframework.checker.index.qual.IndexOrHigh;
import org.checkerframework.checker.index.qual.SameLen;

/**
 * Builds a string from constituent parts providing a more flexible and powerful API
 * than StringBuffer.
 * <p>
 * The main differences from StringBuffer/StringBuilder are:
 * </p>
 * <ul>
 * <li>Not synchronized</li>
 * <li>Not final</li>
 * <li>Subclasses have direct access to character array</li>
 * <li>Additional methods
 *  <ul>
 *   <li>appendWithSeparators - adds an array of values, with a separator</li>
 *   <li>appendPadding - adds a length padding characters</li>
 *   <li>appendFixedLength - adds a fixed width field to the builder</li>
 *   <li>toCharArray/getChars - simpler ways to get a range of the character array</li>
 *   <li>delete - delete char or string</li>
 *   <li>replace - search and replace for a char or string</li>
 *   <li>leftString/rightString/midString - substring without exceptions</li>
 *   <li>contains - whether the builder contains a char or string</li>
 *   <li>size/clear/isEmpty - collections style API methods</li>
 *  </ul>
 * </li>
 * <li>Views
 *  <ul>
 *   <li>asTokenizer - uses the internal buffer as the source of a StrTokenizer</li>
 *   <li>asReader - uses the internal buffer as the source of a Reader</li>
 *   <li>asWriter - allows a Writer to write directly to the internal buffer</li>
 *  </ul>
 * </li>
 * </ul>
 * <p>
 * The aim has been to provide an API that mimics very closely what StringBuffer
 * provides, but with additional methods. It should be noted that some edge cases,
 * with invalid indices or null input, have been altered - see individual methods.
 * The biggest of these changes is that by default, null will not output the text
 * 'null'. This can be controlled by a property, {@link #setNullText(String)}.
 * <p>
 * Prior to 3.0, this class implemented Cloneable but did not implement the
 * clone method so could not be used. From 3.0 onwards it no longer implements
 * the interface.
 *
 * @since 2.2
 * @deprecated as of 3.6, use commons-text
 * <a href="https://commons.apache.org/proper/commons-text/javadocs/api-release/org/apache/commons/text/TextStringBuilder.html">
 * TextStringBuilder</a> instead
 */
@Deprecated
public class StrBuilder implements CharSequence, Appendable, Serializable, Builder<String> {

    /**
     * The extra capacity for new builders.
     */
    static final int CAPACITY = 32;

    /**
     * Required for serialization support.
     *
     * @see java.io.Serializable
     */
    private static final long serialVersionUID = 7628716375283629643L;

    /** Internal data storage. */
    protected char[] buffer; // TODO make private?
    /** Current size of the buffer. */
    protected @NonNegative @LTEqLengthOf("this.buffer") int size; // TODO make private?
    /** The new line. */
    private String newLine;
    /** The null text. */
    private String nullText;

    //-----------------------------------------------------------------------
    /**
     * Constructor that creates an empty builder initial capacity 32 characters.
     */
    public StrBuilder() {
        this(CAPACITY);
    }

    /**
     * Constructor that creates an empty builder the specified initial capacity.
     *
     * @param initialCapacity  the initial capacity, zero or less will be converted to 32
     */
    public StrBuilder(int initialCapacity) {
        super();
        if (initialCapacity <= 0) {
            initialCapacity = CAPACITY;
        }
        buffer = new char[initialCapacity];
    }

    /**
     * Constructor that creates a builder from the string, allocating
     * 32 extra characters for growth.
     *
     * @param str  the string to copy, null treated as blank string
     */
    public StrBuilder(final String str) {
        super();
        if (str == null) {
            buffer = new char[CAPACITY];
        } else {
            buffer = new char[str.length() + CAPACITY];
            append(str);
        }
    }

    //-----------------------------------------------------------------------
    /**
     * Gets the text to be appended when a new line is added.
     *
     * @return the new line text, null means use system default
     */
    public String getNewLineText() {
        return newLine;
    }

    /**
     * Sets the text to be appended when a new line is added.
     *
     * @param newLine  the new line text, null means use system default
     * @return this, to enable chaining
     */
    public StrBuilder setNewLineText(final String newLine) {
        this.newLine = newLine;
        return this;
    }

    //-----------------------------------------------------------------------
    /**
     * Gets the text to be appended when null is added.
     *
     * @return the null text, null means no append
     */
    public String getNullText() {
        return nullText;
    }

    /**
     * Sets the text to be appended when null is added.
     *
     * @param nullText  the null text, null means no append
     * @return this, to enable chaining
     */
    public StrBuilder setNullText(String nullText) {
        if (nullText != null && nullText.isEmpty()) {
            nullText = null;
        }
        this.nullText = nullText;
        return this;
    }

    //-----------------------------------------------------------------------
    /**
     * Gets the length of the string builder.
     *
     * @return the length
     */
    @SuppressWarnings("override.return.invalid") // it says required is @LTEqLengthOf("this"), which does not makes sense, it should be @LTEqLengthOf("this.buffer")
    @Override
    public @NonNegative @LTEqLengthOf("this.buffer") int length() {
        return size;
    }

    /**
     * Updates the length of the builder by either dropping the last characters
     * or adding filler of Unicode zero.
     *
     * @param length  the length to set to, must be zero or positive
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if the length is negative
     */
    @SuppressWarnings("assignment.type.incompatible") // #1, #2: ensurecapacity(length) makes the buffer array of the size length*2 if length > size 
    public StrBuilder setLength(final int length) {
        if (length < 0) {
            throw new StringIndexOutOfBoundsException(length);
        }
        if (length < size) {
            size = length;
        } else if (length > size) {
            ensureCapacity(length);
            final int oldEnd = size;
            final @NonNegative @LTEqLengthOf("this.buffer") int newEnd = length; // #1
            size = length; // #2
            for (int i = oldEnd; i < newEnd; i++) {
                buffer[i] = CharUtils.NUL;
            }
        }
        return this;
    }

    //-----------------------------------------------------------------------
    /**
     * Gets the current size of the internal character array buffer.
     *
     * @return the capacity
     */
    public int capacity() {
        return buffer.length;
    }

    /**
     * Checks the capacity and ensures that it is at least the size specified.
     *
     * @param capacity  the capacity to ensure
     * @return this, to enable chaining
     */
    public StrBuilder ensureCapacity(final int capacity) {
        if (capacity > buffer.length) {
            final char[] old = buffer;
            buffer = new char[capacity * 2];
            System.arraycopy(old, 0, buffer, 0, size);
        }
        return this;
    }

    /**
     * Minimizes the capacity to the actual length of the string.
     *
     * @return this, to enable chaining
     */
    public StrBuilder minimizeCapacity() {
        if (buffer.length > length()) {
            final char[] old = buffer;
            buffer = new char[length()];
            System.arraycopy(old, 0, buffer, 0, size);
        }
        return this;
    }

    //-----------------------------------------------------------------------
    /**
     * Gets the length of the string builder.
     * <p>
     * This method is the same as {@link #length()} and is provided to match the
     * API of Collections.
     *
     * @return the length
     */
    public int size() {
        return size;
    }

    /**
     * Checks is the string builder is empty (convenience Collections API style method).
     * <p>
     * This method is the same as checking {@link #length()} and is provided to match the
     * API of Collections.
     *
     * @return <code>true</code> if the size is <code>0</code>.
     */
    public boolean isEmpty() {
        return size == 0;
    }

    /**
     * Clears the string builder (convenience Collections API style method).
     * <p>
     * This method does not reduce the size of the internal character buffer.
     * To do that, call <code>clear()</code> followed by {@link #minimizeCapacity()}.
     * <p>
     * This method is the same as {@link #setLength(int)} called with zero
     * and is provided to match the API of Collections.
     *
     * @return this, to enable chaining
     */
    public StrBuilder clear() {
        size = 0;
        return this;
    }

    //-----------------------------------------------------------------------
    /**
     * Gets the character at the specified index.
     *
     * @see #setCharAt(int, char)
     * @see #deleteCharAt(int)
     * @param index  the index to retrieve, must be valid
     * @return the character at the index
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    @Override
    public char charAt(final int index) {
        if (index < 0 || index >= length()) {
            throw new StringIndexOutOfBoundsException(index);
        }
        return buffer[index];
    }

    /**
     * Sets the character at the specified index.
     *
     * @see #charAt(int)
     * @see #deleteCharAt(int)
     * @param index  the index to set
     * @param ch  the new character
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    public StrBuilder setCharAt(final int index, final char ch) {
        if (index < 0 || index >= length()) {
            throw new StringIndexOutOfBoundsException(index);
        }
        buffer[index] = ch;
        return this;
    }

    /**
     * Deletes the character at the specified index.
     *
     * @see #charAt(int)
     * @see #setCharAt(int, char)
     * @param index  the index to delete
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    @SuppressWarnings("index:argument.type.incompatible") // #1: index >= 0 ensures @MinLen(1), hence 1 is @LTEqLengthOf("this.buffer")
    public StrBuilder deleteCharAt(final int index) {
        if (index < 0 || index >= size) {
            throw new StringIndexOutOfBoundsException(index);
        }
        deleteImpl(index, index + 1, 1); // #1
        return this;
    }

    //-----------------------------------------------------------------------
    /**
     * Copies the builder's character array into a new character array.
     *
     * @return a new array that represents the contents of the builder
     */
    public char[] toCharArray() {
        if (size == 0) {
            return ArrayUtils.EMPTY_CHAR_ARRAY;
        }
        final char chars[] = new char[size];
        System.arraycopy(buffer, 0, chars, 0, size);
        return chars;
    }

    /**
     * Copies part of the builder's character array into a new character array.
     *
     * @param startIndex  the start index, inclusive, must be valid
     * @param endIndex  the end index, exclusive, must be valid except that
     *  if too large it is treated as end of string
     * @return a new array that holds part of the contents of the builder
     * @throws IndexOutOfBoundsException if startIndex is invalid,
     *  or if endIndex is invalid (but endIndex greater than size is valid)
     */
    @SuppressWarnings("index:argument.type.incompatible") /* #1: startIndex is a valid index for buffer as checked by validateRange and 
    0 is always @LTLengthOf(value={"this.buffer", "chars"}, offset={"startIndex - 1", "-1"}) as startIndex can be maximum this.buffer.length()
    */
    public char[] toCharArray(final int startIndex, int endIndex) {
        endIndex = validateRange(startIndex, endIndex);
        @SuppressWarnings("assignment.type.incompatible") // validateRange(startIndex, endIndex) => endIndex - startIndex is @NonNegative and @LTEqLengthOf("this.buffer")
        final @NonNegative @LTEqLengthOf("this.buffer") int len = endIndex - startIndex;
        if (len == 0) {
            return ArrayUtils.EMPTY_CHAR_ARRAY;
        }
        final char chars[] = new char[len];
        System.arraycopy(buffer, startIndex, chars, 0, len); // #1
        return chars;
    }

    /**
     * Copies the character array into the specified array.
     *
     * @param destination  the destination array, null will cause an array to be created
     * @return the input array, unless that was null or too small
     */
    public char[] getChars(char[] destination) {
        final int len = length();
        if (destination == null || destination.length < len) {
            destination = new char[len];
        }
        System.arraycopy(buffer, 0, destination, 0, len);
        return destination;
    }

    /**
     * Copies the character array into the specified array.
     *
     * @param startIndex  first index to copy, inclusive, must be valid
     * @param endIndex  last index, exclusive, must be valid
     * @param destination  the destination array, must not be null or too small
     * @param destinationIndex  the index to start copying in destination
     * @throws NullPointerException if the array is null
     * @throws IndexOutOfBoundsException if any index is invalid
     */
    @SuppressWarnings("index:argument.type.incompatible") // #1: endIndex >= startIndex as checked by the previous if
    public void getChars(final int startIndex, final int endIndex, final char destination[], final @IndexFor("#3") int destinationIndex) {
        if (startIndex < 0) {
            throw new StringIndexOutOfBoundsException(startIndex);
        }
        if (endIndex < 0 || endIndex > length()) {
            throw new StringIndexOutOfBoundsException(endIndex);
        }
        if (startIndex > endIndex) {
            throw new StringIndexOutOfBoundsException("end < start");
        }
        System.arraycopy(buffer, startIndex, destination, destinationIndex, endIndex - startIndex); // #1
    }

    //-----------------------------------------------------------------------
    /**
     * If possible, reads chars from the provided {@link Readable} directly into underlying
     * character buffer without making extra copies.
     *
     * @param readable  object to read from
     * @return the number of characters read
     * @throws IOException if an I/O error occurs
     *
     * @since 3.4
     * @see #appendTo(Appendable)
     */
    @SuppressWarnings({"index:argument.type.incompatible","index:compound.assignment.type.incompatible"}) /*
    #1, #4: buffer.length >= buffer.size as ensureCapacity(int x) makes an array of length x*2 if x > size
    #2: read is the number of characters read from the offset size, hence size + offset is still @NonNegative and @LTEqLengthOf("buffer")
    #3: ensureCapacity(size + remaining) => size + remaining is @LTEqLengthOf("this.buffer")
    */
    public int readFrom(final Readable readable) throws IOException {
        final int oldSize = size;
        if (readable instanceof Reader) {
            final Reader r = (Reader) readable;
            ensureCapacity(size + 1);
            int read;
            while ((read = r.read(buffer, size, buffer.length - size)) != -1) { // #1
                size += read; // #2
                ensureCapacity(size + 1);
            }
        } else if (readable instanceof CharBuffer) {
            final CharBuffer cb = (CharBuffer) readable;
            final int remaining = cb.remaining();
            ensureCapacity(size + remaining);
            cb.get(buffer, size, remaining);
            size += remaining; // #3
        } else {
            while (true) {
                ensureCapacity(size + 1);
                final CharBuffer buf = CharBuffer.wrap(buffer, size, buffer.length - size);
                final int read = readable.read(buf);
                if (read == -1) {
                    break;
                }
                size += read; // #4
            }
        }
        return size - oldSize;
    }

    //-----------------------------------------------------------------------
    /**
     * Appends the new line string to this string builder.
     * <p>
     * The new line string can be altered using {@link #setNewLineText(String)}.
     * This might be used to force the output to always use Unix line endings
     * even when on Windows.
     *
     * @return this, to enable chaining
     */
    public StrBuilder appendNewLine() {
        if (newLine == null)  {
            append(System.lineSeparator());
            return this;
        }
        return append(newLine);
    }

    /**
     * Appends the text representing <code>null</code> to this string builder.
     *
     * @return this, to enable chaining
     */
    public StrBuilder appendNull() {
        if (nullText == null)  {
            return this;
        }
        return append(nullText);
    }

    /**
     * Appends an object to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param obj  the object to append
     * @return this, to enable chaining
     */
    public StrBuilder append(final Object obj) {
        if (obj == null) {
            return appendNull();
        }
        if (obj instanceof CharSequence) {
            return append((CharSequence) obj);
        }
        return append(obj.toString());
    }

    /**
     * Appends a CharSequence to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param seq  the CharSequence to append
     * @return this, to enable chaining
     * @since 3.0
     */
    @Override
    public StrBuilder append(final CharSequence seq) {
        if (seq == null) {
            return appendNull();
        }
        if (seq instanceof StrBuilder) {
            return append((StrBuilder) seq);
        }
        if (seq instanceof StringBuilder) {
            return append((StringBuilder) seq);
        }
        if (seq instanceof StringBuffer) {
            return append((StringBuffer) seq);
        }
        if (seq instanceof CharBuffer) {
            return append((CharBuffer) seq);
        }
        return append(seq.toString());
    }

    /**
     * Appends part of a CharSequence to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param seq  the CharSequence to append
     * @param startIndex  the start index, inclusive, must be valid
     * @param length  the length to append, must be valid
     * @return this, to enable chaining
     * @since 3.0
     */
    @Override
    public StrBuilder append(final CharSequence seq, final int startIndex, final int length) {
        if (seq == null) {
            return appendNull();
        }
        return append(seq.toString(), startIndex, length);
    }

    /**
     * Appends a string to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str  the string to append
     * @return this, to enable chaining
     */
    @SuppressWarnings("index:compound.assignment.type.incompatible") // #1: ensureCapacity(len + strLen) => size + strLen is @NonNegative @LTEqLengthOf("this.buffer")
    public StrBuilder append(final String str) {
        if (str == null) {
            return appendNull();
        }
        final int strLen = str.length();
        if (strLen > 0) {
            final int len = length();
            ensureCapacity(len + strLen);
            str.getChars(0, strLen, buffer, len);
            size += strLen; // #1
        }
        return this;
    }


    /**
     * Appends part of a string to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str  the string to append
     * @param startIndex  the start index, inclusive, must be valid
     * @param length  the length to append, must be valid
     * @return this, to enable chaining
     */
    @SuppressWarnings("index:compound.assignment.type.incompatible") // #1: ensureCapacity(len + length) => size + length is @NonNegative @LTEqLengthOf("this.buffer")
    public StrBuilder append(final String str, final int startIndex, final int length) {
        if (str == null) {
            return appendNull();
        }
        if (startIndex < 0 || startIndex > str.length()) {
            throw new StringIndexOutOfBoundsException("startIndex must be valid");
        }
        if (length < 0 || (startIndex + length) > str.length()) {
            throw new StringIndexOutOfBoundsException("length must be valid");
        }
        if (length > 0) {
            final int len = length();
            ensureCapacity(len + length);
            str.getChars(startIndex, startIndex + length, buffer, len);
            size += length; // #1
        }
        return this;
    }

    /**
     * Calls {@link String#format(String, Object...)} and appends the result.
     *
     * @param format the format string
     * @param objs the objects to use in the format string
     * @return {@code this} to enable chaining
     * @see String#format(String, Object...)
     * @since 3.2
     */
    public StrBuilder append(final String format, final Object... objs) {
        return append(String.format(format, objs));
    }

    /**
     * Appends the contents of a char buffer to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param buf  the char buffer to append
     * @return this, to enable chaining
     * @since 3.4
     */
    @SuppressWarnings({"index:argument.type.incompatible","index:compound.assignment.type.incompatible"}) /* #1: ensureCapacity(len + length) => length <= buffer.length - len, 
    also length is buf.remaining() so it ensured length <= buf.array.length - (buf.arrayOffset() + buf.position)
    buf.arrayOffset() (where the offset of the buffer is) + buf.position() (relative position of the current buffer) is @LTLengthOf("buf.array()")
    #2: ensureCapacity(len + length) => size + length is @LTEqLengthOf("this.buffer")
    */
    public StrBuilder append(final CharBuffer buf) {
        if (buf == null) {
            return appendNull();
        }
        if (buf.hasArray()) {
            final int length = buf.remaining();
            final int len = length();
            ensureCapacity(len + length);
            System.arraycopy(buf.array(), buf.arrayOffset() + buf.position(), buffer, len, length); // #1
            size += length; // #2
        } else {
            append(buf.toString());
        }
        return this;
    }

    /**
     * Appends the contents of a char buffer to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param buf  the char buffer to append
     * @param startIndex  the start index, inclusive, must be valid
     * @param length  the length to append, must be valid
     * @return this, to enable chaining
     * @since 3.4
     */
    @SuppressWarnings({"index:argument.type.incompatible","index:compound.assignment.type.incompatible"}) /* #1: ensureCapacity(len + length) => length <= buffer.length - len, 
    buf.arrayOffset() (where the offset of the buffer is) + buf.position() (relative position of the current buffer) + startIndex(which is between 0 and buf.remaining()) is @LTLengthOf("buf.array()")
    startIndex + length <= totalLength => length <= buf.remaining() - startIndex, buf.remaining() is the number of characters left to be read = buf.array().length - buf.arrayOffset() - buf.position()
    hence, length is @LTLengthOf(value = "buf.array()", offset = "buf.arrayOffset() + buf.position() + startIndex") as well
    #2: ensureCapacity(len + length) => size + length is @LTEqLengthOf("this.buffer")
    */
    public StrBuilder append(final CharBuffer buf, final int startIndex, final int length) {
        if (buf == null) {
            return appendNull();
        }
        if (buf.hasArray()) {
            final int totalLength = buf.remaining();
            if (startIndex < 0 || startIndex > totalLength) {
                throw new StringIndexOutOfBoundsException("startIndex must be valid");
            }
            if (length < 0 || (startIndex + length) > totalLength) {
                throw new StringIndexOutOfBoundsException("length must be valid");
            }
            final int len = length();
            ensureCapacity(len + length);
            System.arraycopy(buf.array(), buf.arrayOffset() + buf.position() + startIndex, buffer, len, length); // #1
            size += length; // #2
        } else {
            append(buf.toString(), startIndex, length);
        }
        return this;
    }

    /**
     * Appends a string buffer to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str  the string buffer to append
     * @return this, to enable chaining
     */
    @SuppressWarnings("index:compound.assignment.type.incompatible") // #1: ensureCapacity(len + strLen) => size + strLen to be @LTEqLengthOf("this.buffer")
    public StrBuilder append(final StringBuffer str) {
        if (str == null) {
            return appendNull();
        }
        final int strLen = str.length();
        if (strLen > 0) {
            final int len = length();
            ensureCapacity(len + strLen);
            str.getChars(0, strLen, buffer, len);
            size += strLen; // #1
        }
        return this;
    }

    /**
     * Appends part of a string buffer to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str  the string to append
     * @param startIndex  the start index, inclusive, must be valid
     * @param length  the length to append, must be valid
     * @return this, to enable chaining
     */
    @SuppressWarnings("index:compound.assignment.type.incompatible") // #1: ensureCapacity(len + length) => size + length to be @LTEqLengthOf("this.buffer")
    public StrBuilder append(final StringBuffer str, final int startIndex, final int length) {
        if (str == null) {
            return appendNull();
        }
        if (startIndex < 0 || startIndex > str.length()) {
            throw new StringIndexOutOfBoundsException("startIndex must be valid");
        }
        if (length < 0 || (startIndex + length) > str.length()) {
            throw new StringIndexOutOfBoundsException("length must be valid");
        }
        if (length > 0) {
            final int len = length();
            ensureCapacity(len + length);
            str.getChars(startIndex, startIndex + length, buffer, len);
            size += length; // #1
        }
        return this;
    }

    /**
     * Appends a StringBuilder to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str the StringBuilder to append
     * @return this, to enable chaining
     * @since 3.2
     */
    @SuppressWarnings("index:compound.assignment.type.incompatible") // #1: ensureCapacity(len + strLen) => size + strLen to be @LTEqLengthOf("this.buffer")
    public StrBuilder append(final StringBuilder str) {
        if (str == null) {
            return appendNull();
        }
        final int strLen = str.length();
        if (strLen > 0) {
            final int len = length();
            ensureCapacity(len + strLen);
            str.getChars(0, strLen, buffer, len);
            size += strLen; // #1
        }
        return this;
    }

    /**
     * Appends part of a StringBuilder to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str the StringBuilder to append
     * @param startIndex the start index, inclusive, must be valid
     * @param length the length to append, must be valid
     * @return this, to enable chaining
     * @since 3.2
     */
    @SuppressWarnings("index:compound.assignment.type.incompatible") // #1: ensureCapacity(len + length) => size + length to be @LTEqLengthOf("this.buffer")
    public StrBuilder append(final StringBuilder str, final int startIndex, final int length) {
        if (str == null) {
            return appendNull();
        }
        if (startIndex < 0 || startIndex > str.length()) {
            throw new StringIndexOutOfBoundsException("startIndex must be valid");
        }
        if (length < 0 || (startIndex + length) > str.length()) {
            throw new StringIndexOutOfBoundsException("length must be valid");
        }
        if (length > 0) {
            final int len = length();
            ensureCapacity(len + length);
            str.getChars(startIndex, startIndex + length, buffer, len);
            size += length; // #1
        }
        return this;
    }

    /**
     * Appends another string builder to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str  the string builder to append
     * @return this, to enable chaining
     */
    @SuppressWarnings({"index:compound.assignment.type.incompatible","index:argument.type.incompatible"}) /*
    #1: ensureCapacity(len + strLen) => strLen <= str.buffer.length - len
    #2: ensureCapacity(len + strLen) => size + strLen to be @LTEqLengthOf("this.buffer")
    */
    public StrBuilder append(final StrBuilder str) {
        if (str == null) {
            return appendNull();
        }
        final int strLen = str.length();
        if (strLen > 0) {
            final int len = length();
            ensureCapacity(len + strLen);
            System.arraycopy(str.buffer, 0, buffer, len, strLen); // #1
            size += strLen;
        }
        return this;
    }

    /**
     * Appends part of a string builder to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str  the string to append
     * @param startIndex  the start index, inclusive, must be valid
     * @param length  the length to append, must be valid
     * @return this, to enable chaining
     */
    @SuppressWarnings({"index:compound.assignment.type.incompatible","index:argument.type.incompatible"}) /*
    #1: ensureCapacity(len + length) => len <= str.buffer.length - length, and length > 0 => len is @LTLengthOf("buffer")
    #2: ensureCapacity(len + length) => size + length to be @LTEqLengthOf("this.buffer")
    */
    public StrBuilder append(final StrBuilder str, final int startIndex, final int length) {
        if (str == null) {
            return appendNull();
        }
        if (startIndex < 0 || startIndex > str.length()) {
            throw new StringIndexOutOfBoundsException("startIndex must be valid");
        }
        if (length < 0 || (startIndex + length) > str.length()) {
            throw new StringIndexOutOfBoundsException("length must be valid");
        }
        if (length > 0) {
            final int len = length();
            ensureCapacity(len + length);
            str.getChars(startIndex, startIndex + length, buffer, len); // #1
            size += length; // #2
        }
        return this;
    }

    /**
     * Appends a char array to the string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param chars  the char array to append
     * @return this, to enable chaining
     */
     @SuppressWarnings({"index:compound.assignment.type.incompatible","index:argument.type.incompatible"}) /*
    #1: ensureCapacity(len + length) => strLen <= str.buffer.length - len
    #2: ensureCapacity(len + strLen) => size + strLen to be @LTEqLengthOf("this.buffer")
    */
    public StrBuilder append(final char[] chars) {
        if (chars == null) {
            return appendNull();
        }
        final int strLen = chars.length;
        if (strLen > 0) {
            final int len = length();
            ensureCapacity(len + strLen);
            System.arraycopy(chars, 0, buffer, len, strLen); // #1
            size += strLen; // #2
        }
        return this;
    }

    /**
     * Appends a char array to the string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param chars  the char array to append
     * @param startIndex  the start index, inclusive, must be valid
     * @param length  the length to append, must be valid
     * @return this, to enable chaining
     */
     @SuppressWarnings({"index:compound.assignment.type.incompatible","index:argument.type.incompatible"}) /*
    #1: ensureCapacity(len + length) => length <= str.buffer.length - len, 
    #2: ensureCapacity(len + length) => size + length to be @LTEqLengthOf("this.buffer")
    */
    public StrBuilder append(final char[] chars, final int startIndex, final int length) {
        if (chars == null) {
            return appendNull();
        }
        if (startIndex < 0 || startIndex > chars.length) {
            throw new StringIndexOutOfBoundsException("Invalid startIndex: " + length);
        }
        if (length < 0 || (startIndex + length) > chars.length) {
            throw new StringIndexOutOfBoundsException("Invalid length: " + length);
        }
        if (length > 0) {
            final int len = length();
            ensureCapacity(len + length);
            System.arraycopy(chars, startIndex, buffer, len, length);
            size += length;
        }
        return this;
    }

    /**
     * Appends a boolean value to the string builder.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    @SuppressWarnings({"index:array.access.unsafe.high","index:compound.assignment.type.incompatible"}) /*
    #1: ensureCapacity(size +4) ensures size till size+=4 to be @LTEqLengthOf("buffer") and indices are used upto size + 3
    #2: ensureCapacity(size + 5) ensures size till size+=5 to be @LTEqLengthOf("buffer") and indices are used upto size + 4
    */
    public StrBuilder append(final boolean value) {
        if (value) {
            ensureCapacity(size + 4);
            buffer[size++] = 't'; // #1
            buffer[size++] = 'r'; // #1
            buffer[size++] = 'u'; // #1
            buffer[size++] = 'e'; // #1
        } else {
            ensureCapacity(size + 5);
            buffer[size++] = 'f'; // #2
            buffer[size++] = 'a'; // #2
            buffer[size++] = 'l'; // #2
            buffer[size++] = 's'; // #2
            buffer[size++] = 'e'; // #2
        }
        return this;
    }

    /**
     * Appends a char value to the string builder.
     *
     * @param ch  the value to append
     * @return this, to enable chaining
     * @since 3.0
     */
    @SuppressWarnings({"index:array.access.unsafe.high","index:compound.assignment.type.incompatible"}) // #1: ensureCapacity(len + 1) => size + 1 to be @LTEqLengthOf("buffer"), hence size is @LTLengthOf("buffer")
    @Override
    public StrBuilder append(final char ch) {
        final int len = length();
        ensureCapacity(len + 1);
        buffer[size++] = ch; // #1
        return this;
    }

    /**
     * Appends an int value to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder append(final int value) {
        return append(String.valueOf(value));
    }

    /**
     * Appends a long value to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder append(final long value) {
        return append(String.valueOf(value));
    }

    /**
     * Appends a float value to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder append(final float value) {
        return append(String.valueOf(value));
    }

    /**
     * Appends a double value to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder append(final double value) {
        return append(String.valueOf(value));
    }

    //-----------------------------------------------------------------------
    /**
     * Appends an object followed by a new line to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param obj  the object to append
     * @return this, to enable chaining
     * @since 2.3
     */
    public StrBuilder appendln(final Object obj) {
        return append(obj).appendNewLine();
    }

    /**
     * Appends a string followed by a new line to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str  the string to append
     * @return this, to enable chaining
     * @since 2.3
     */
    public StrBuilder appendln(final String str) {
        return append(str).appendNewLine();
    }

    /**
     * Appends part of a string followed by a new line to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str  the string to append
     * @param startIndex  the start index, inclusive, must be valid
     * @param length  the length to append, must be valid
     * @return this, to enable chaining
     * @since 2.3
     */
    public StrBuilder appendln(final String str, final int startIndex, final int length) {
        return append(str, startIndex, length).appendNewLine();
    }

    /**
     * Calls {@link String#format(String, Object...)} and appends the result.
     *
     * @param format the format string
     * @param objs the objects to use in the format string
     * @return {@code this} to enable chaining
     * @see String#format(String, Object...)
     * @since 3.2
     */
    public StrBuilder appendln(final String format, final Object... objs) {
        return append(format, objs).appendNewLine();
    }

    /**
     * Appends a string buffer followed by a new line to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str  the string buffer to append
     * @return this, to enable chaining
     * @since 2.3
     */
    public StrBuilder appendln(final StringBuffer str) {
        return append(str).appendNewLine();
    }

    /**
     * Appends a string builder followed by a new line to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str  the string builder to append
     * @return this, to enable chaining
     * @since 3.2
     */
    public StrBuilder appendln(final StringBuilder str) {
        return append(str).appendNewLine();
    }

    /**
     * Appends part of a string builder followed by a new line to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str  the string builder to append
     * @param startIndex  the start index, inclusive, must be valid
     * @param length  the length to append, must be valid
     * @return this, to enable chaining
     * @since 3.2
     */
    public StrBuilder appendln(final StringBuilder str, final int startIndex, final int length) {
        return append(str, startIndex, length).appendNewLine();
    }

    /**
     * Appends part of a string buffer followed by a new line to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str  the string to append
     * @param startIndex  the start index, inclusive, must be valid
     * @param length  the length to append, must be valid
     * @return this, to enable chaining
     * @since 2.3
     */
    public StrBuilder appendln(final StringBuffer str, final int startIndex, final int length) {
        return append(str, startIndex, length).appendNewLine();
    }

    /**
     * Appends another string builder followed by a new line to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str  the string builder to append
     * @return this, to enable chaining
     * @since 2.3
     */
    public StrBuilder appendln(final StrBuilder str) {
        return append(str).appendNewLine();
    }

    /**
     * Appends part of a string builder followed by a new line to this string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str  the string to append
     * @param startIndex  the start index, inclusive, must be valid
     * @param length  the length to append, must be valid
     * @return this, to enable chaining
     * @since 2.3
     */
    public StrBuilder appendln(final StrBuilder str, final int startIndex, final int length) {
        return append(str, startIndex, length).appendNewLine();
    }

    /**
     * Appends a char array followed by a new line to the string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param chars  the char array to append
     * @return this, to enable chaining
     * @since 2.3
     */
    public StrBuilder appendln(final char[] chars) {
        return append(chars).appendNewLine();
    }

    /**
     * Appends a char array followed by a new line to the string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param chars  the char array to append
     * @param startIndex  the start index, inclusive, must be valid
     * @param length  the length to append, must be valid
     * @return this, to enable chaining
     * @since 2.3
     */
    public StrBuilder appendln(final char[] chars, final int startIndex, final int length) {
        return append(chars, startIndex, length).appendNewLine();
    }

    /**
     * Appends a boolean value followed by a new line to the string builder.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     * @since 2.3
     */
    public StrBuilder appendln(final boolean value) {
        return append(value).appendNewLine();
    }

    /**
     * Appends a char value followed by a new line to the string builder.
     *
     * @param ch  the value to append
     * @return this, to enable chaining
     * @since 2.3
     */
    public StrBuilder appendln(final char ch) {
        return append(ch).appendNewLine();
    }

    /**
     * Appends an int value followed by a new line to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     * @since 2.3
     */
    public StrBuilder appendln(final int value) {
        return append(value).appendNewLine();
    }

    /**
     * Appends a long value followed by a new line to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     * @since 2.3
     */
    public StrBuilder appendln(final long value) {
        return append(value).appendNewLine();
    }

    /**
     * Appends a float value followed by a new line to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     * @since 2.3
     */
    public StrBuilder appendln(final float value) {
        return append(value).appendNewLine();
    }

    /**
     * Appends a double value followed by a new line to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     * @since 2.3
     */
    public StrBuilder appendln(final double value) {
        return append(value).appendNewLine();
    }

    //-----------------------------------------------------------------------
    /**
     * Appends each item in an array to the builder without any separators.
     * Appending a null array will have no effect.
     * Each object is appended using {@link #append(Object)}.
     *
     * @param <T>  the element type
     * @param array  the array to append
     * @return this, to enable chaining
     * @since 2.3
     */
    public <T> StrBuilder appendAll(@SuppressWarnings("unchecked") final T... array) {
        /*
         * @SuppressWarnings used to hide warning about vararg usage. We cannot
         * use @SafeVarargs, since this method is not final. Using @SuppressWarnings
         * is fine, because it isn't inherited by subclasses, so each subclass must
         * vouch for itself whether its use of 'array' is safe.
         */
        if (array != null && array.length > 0) {
            for (final Object element : array) {
                append(element);
            }
        }
        return this;
    }

    /**
     * Appends each item in an iterable to the builder without any separators.
     * Appending a null iterable will have no effect.
     * Each object is appended using {@link #append(Object)}.
     *
     * @param iterable  the iterable to append
     * @return this, to enable chaining
     * @since 2.3
     */
    public StrBuilder appendAll(final Iterable<?> iterable) {
        if (iterable != null) {
            for (final Object o : iterable) {
                append(o);
            }
        }
        return this;
    }

    /**
     * Appends each item in an iterator to the builder without any separators.
     * Appending a null iterator will have no effect.
     * Each object is appended using {@link #append(Object)}.
     *
     * @param it  the iterator to append
     * @return this, to enable chaining
     * @since 2.3
     */
    public StrBuilder appendAll(final Iterator<?> it) {
        if (it != null) {
            while (it.hasNext()) {
                append(it.next());
            }
        }
        return this;
    }

    //-----------------------------------------------------------------------
    /**
     * Appends an array placing separators between each value, but
     * not before the first or after the last.
     * Appending a null array will have no effect.
     * Each object is appended using {@link #append(Object)}.
     *
     * @param array  the array to append
     * @param separator  the separator to use, null means no separator
     * @return this, to enable chaining
     */
    public StrBuilder appendWithSeparators(final Object[] array, final String separator) {
        if (array != null && array.length > 0) {
            final String sep = Objects.toString(separator, "");
            append(array[0]);
            for (int i = 1; i < array.length; i++) {
                append(sep);
                append(array[i]);
            }
        }
        return this;
    }

    /**
     * Appends an iterable placing separators between each value, but
     * not before the first or after the last.
     * Appending a null iterable will have no effect.
     * Each object is appended using {@link #append(Object)}.
     *
     * @param iterable  the iterable to append
     * @param separator  the separator to use, null means no separator
     * @return this, to enable chaining
     */
    public StrBuilder appendWithSeparators(final Iterable<?> iterable, final String separator) {
        if (iterable != null) {
            final String sep = Objects.toString(separator, "");
            final Iterator<?> it = iterable.iterator();
            while (it.hasNext()) {
                append(it.next());
                if (it.hasNext()) {
                    append(sep);
                }
            }
        }
        return this;
    }

    /**
     * Appends an iterator placing separators between each value, but
     * not before the first or after the last.
     * Appending a null iterator will have no effect.
     * Each object is appended using {@link #append(Object)}.
     *
     * @param it  the iterator to append
     * @param separator  the separator to use, null means no separator
     * @return this, to enable chaining
     */
    public StrBuilder appendWithSeparators(final Iterator<?> it, final String separator) {
        if (it != null) {
            final String sep = Objects.toString(separator, "");
            while (it.hasNext()) {
                append(it.next());
                if (it.hasNext()) {
                    append(sep);
                }
            }
        }
        return this;
    }

    //-----------------------------------------------------------------------
    /**
     * Appends a separator if the builder is currently non-empty.
     * Appending a null separator will have no effect.
     * The separator is appended using {@link #append(String)}.
     * <p>
     * This method is useful for adding a separator each time around the
     * loop except the first.
     * <pre>
     * for (Iterator it = list.iterator(); it.hasNext(); ) {
     *   appendSeparator(",");
     *   append(it.next());
     * }
     * </pre>
     * Note that for this simple example, you should use
     * {@link #appendWithSeparators(Iterable, String)}.
     *
     * @param separator  the separator to use, null means no separator
     * @return this, to enable chaining
     * @since 2.3
     */
    public StrBuilder appendSeparator(final String separator) {
        return appendSeparator(separator, null);
    }

    /**
     * Appends one of both separators to the StrBuilder.
     * If the builder is currently empty it will append the defaultIfEmpty-separator
     * Otherwise it will append the standard-separator
     *
     * Appending a null separator will have no effect.
     * The separator is appended using {@link #append(String)}.
     * <p>
     * This method is for example useful for constructing queries
     * <pre>
     * StrBuilder whereClause = new StrBuilder();
     * if (searchCommand.getPriority() != null) {
     *  whereClause.appendSeparator(" and", " where");
     *  whereClause.append(" priority = ?")
     * }
     * if (searchCommand.getComponent() != null) {
     *  whereClause.appendSeparator(" and", " where");
     *  whereClause.append(" component = ?")
     * }
     * selectClause.append(whereClause)
     * </pre>
     *
     * @param standard the separator if builder is not empty, null means no separator
     * @param defaultIfEmpty the separator if builder is empty, null means no separator
     * @return this, to enable chaining
     * @since 2.5
     */
    public StrBuilder appendSeparator(final String standard, final String defaultIfEmpty) {
        final String str = isEmpty() ? defaultIfEmpty : standard;
        if (str != null) {
            append(str);
        }
        return this;
    }

    /**
     * Appends a separator if the builder is currently non-empty.
     * The separator is appended using {@link #append(char)}.
     * <p>
     * This method is useful for adding a separator each time around the
     * loop except the first.
     * <pre>
     * for (Iterator it = list.iterator(); it.hasNext(); ) {
     *   appendSeparator(',');
     *   append(it.next());
     * }
     * </pre>
     * Note that for this simple example, you should use
     * {@link #appendWithSeparators(Iterable, String)}.
     *
     * @param separator  the separator to use
     * @return this, to enable chaining
     * @since 2.3
     */
    public StrBuilder appendSeparator(final char separator) {
        if (size() > 0) {
            append(separator);
        }
        return this;
    }

    /**
     * Append one of both separators to the builder
     * If the builder is currently empty it will append the defaultIfEmpty-separator
     * Otherwise it will append the standard-separator
     *
     * The separator is appended using {@link #append(char)}.
     * @param standard the separator if builder is not empty
     * @param defaultIfEmpty the separator if builder is empty
     * @return this, to enable chaining
     * @since 2.5
     */
    public StrBuilder appendSeparator(final char standard, final char defaultIfEmpty) {
        if (size() > 0) {
            append(standard);
        } else {
            append(defaultIfEmpty);
        }
        return this;
    }
    /**
     * Appends a separator to the builder if the loop index is greater than zero.
     * Appending a null separator will have no effect.
     * The separator is appended using {@link #append(String)}.
     * <p>
     * This method is useful for adding a separator each time around the
     * loop except the first.
     * </p>
     * <pre>
     * for (int i = 0; i &lt; list.size(); i++) {
     *   appendSeparator(",", i);
     *   append(list.get(i));
     * }
     * </pre>
     * Note that for this simple example, you should use
     * {@link #appendWithSeparators(Iterable, String)}.
     *
     * @param separator  the separator to use, null means no separator
     * @param loopIndex  the loop index
     * @return this, to enable chaining
     * @since 2.3
     */
    public StrBuilder appendSeparator(final String separator, final int loopIndex) {
        if (separator != null && loopIndex > 0) {
            append(separator);
        }
        return this;
    }

    /**
     * Appends a separator to the builder if the loop index is greater than zero.
     * The separator is appended using {@link #append(char)}.
     * <p>
     * This method is useful for adding a separator each time around the
     * loop except the first.
     * </p>
     * <pre>
     * for (int i = 0; i &lt; list.size(); i++) {
     *   appendSeparator(",", i);
     *   append(list.get(i));
     * }
     * </pre>
     * Note that for this simple example, you should use
     * {@link #appendWithSeparators(Iterable, String)}.
     *
     * @param separator  the separator to use
     * @param loopIndex  the loop index
     * @return this, to enable chaining
     * @since 2.3
     */
    public StrBuilder appendSeparator(final char separator, final int loopIndex) {
        if (loopIndex > 0) {
            append(separator);
        }
        return this;
    }

    //-----------------------------------------------------------------------
    /**
     * Appends the pad character to the builder the specified number of times.
     *
     * @param length  the length to append, negative means no append
     * @param padChar  the character to append
     * @return this, to enable chaining
     */
    @SuppressWarnings({"index:array.access.unsafe.high","index:compound.assignment.type.incompatible"}) // #1: ensureCapacity(size + length) => size + length is @LTEqLengthOf("this.buffer"), and in index to buffer, size is used upto only size + length - 1
    public StrBuilder appendPadding(final int length, final char padChar) {
        if (length >= 0) {
            ensureCapacity(size + length);
            for (int i = 0; i < length; i++) {
                buffer[size++] = padChar; // #1
            }
        }
        return this;
    }

    //-----------------------------------------------------------------------
    /**
     * Appends an object to the builder padding on the left to a fixed width.
     * The <code>toString</code> of the object is used.
     * If the object is larger than the length, the left hand side is lost.
     * If the object is null, the null text value is used.
     *
     * @param obj  the object to append, null uses null text
     * @param width  the fixed field width, zero or negative has no effect
     * @param padChar  the pad character to use
     * @return this, to enable chaining
     */
    @SuppressWarnings({"index:array.access.unsafe.high","index:compound.assignment.type.incompatible","argument.type.incompatible"})/*
    #1: ensureCapacity(size + width) => size + padlen - 1 is @LTLengthOf("buffer")
    #2: ensureCapacity(size + width) => size + padlen = size + width - strLen(NonNegative) is @LTEqLengthOf("this.buffer")
    #3: ensureCapacity(size + width) => size + width <= this.buffer.length
    */
    public StrBuilder appendFixedWidthPadLeft(final Object obj, final int width, final char padChar) {
        if (width > 0) {
            ensureCapacity(size + width);
            String str = (obj == null ? getNullText() : obj.toString());
            if (str == null) {
                str = StringUtils.EMPTY;
            }
            final int strLen = str.length();
            if (strLen >= width) {
                str.getChars(strLen - width, strLen, buffer, size);
            } else {
                final int padLen = width - strLen;
                for (int i = 0; i < padLen; i++) {
                    buffer[size + i] = padChar; // #1
                }
                str.getChars(0, strLen, buffer, size + padLen); // #2
            }
            size += width; // #3
        }
        return this;
    }

    /**
     * Appends an object to the builder padding on the left to a fixed width.
     * The <code>String.valueOf</code> of the <code>int</code> value is used.
     * If the formatted value is larger than the length, the left hand side is lost.
     *
     * @param value  the value to append
     * @param width  the fixed field width, zero or negative has no effect
     * @param padChar  the pad character to use
     * @return this, to enable chaining
     */
    public StrBuilder appendFixedWidthPadLeft(final int value, final int width, final char padChar) {
        return appendFixedWidthPadLeft(String.valueOf(value), width, padChar);
    }

    /**
     * Appends an object to the builder padding on the right to a fixed length.
     * The <code>toString</code> of the object is used.
     * If the object is larger than the length, the right hand side is lost.
     * If the object is null, null text value is used.
     *
     * @param obj  the object to append, null uses null text
     * @param width  the fixed field width, zero or negative has no effect
     * @param padChar  the pad character to use
     * @return this, to enable chaining
     */
    @SuppressWarnings({"index:array.access.unsafe.high","index:compound.assignment.type.incompatible"})/*
    #1: size + strLen + i < size + width which is @LTEqLengthOf("this.buffer") as ensured by ensureCapacity(size + width)
    #2: ensureCapacity(size + width) => size + width <= this.buffer.length
    */
    public StrBuilder appendFixedWidthPadRight(final Object obj, final int width, final char padChar) {
        if (width > 0) {
            ensureCapacity(size + width);
            String str = (obj == null ? getNullText() : obj.toString());
            if (str == null) {
                str = StringUtils.EMPTY;
            }
            final int strLen = str.length();
            if (strLen >= width) {
                str.getChars(0, width, buffer, size);
            } else {
                final int padLen = width - strLen;
                str.getChars(0, strLen, buffer, size);
                for (int i = 0; i < padLen; i++) {
                    buffer[size + strLen + i] = padChar; // #1
                }
            }
            size += width; // #2
        }
        return this;
    }

    /**
     * Appends an object to the builder padding on the right to a fixed length.
     * The <code>String.valueOf</code> of the <code>int</code> value is used.
     * If the object is larger than the length, the right hand side is lost.
     *
     * @param value  the value to append
     * @param width  the fixed field width, zero or negative has no effect
     * @param padChar  the pad character to use
     * @return this, to enable chaining
     */
    public StrBuilder appendFixedWidthPadRight(final int value, final int width, final char padChar) {
        return appendFixedWidthPadRight(String.valueOf(value), width, padChar);
    }

    //-----------------------------------------------------------------------
    /**
     * Inserts the string representation of an object into this builder.
     * Inserting null will use the stored null text value.
     *
     * @param index  the index to add at, must be valid
     * @param obj  the object to insert
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    public StrBuilder insert(final int index, final Object obj) {
        if (obj == null) {
            return insert(index, nullText);
        }
        return insert(index, obj.toString());
    }

    /**
     * Inserts the string into this builder.
     * Inserting null will use the stored null text value.
     *
     * @param index  the index to add at, must be valid
     * @param str  the string to insert
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    @SuppressWarnings({"index:argument.type.incompatible","index:assignment.type.incompatible"}) /*
    #1: ensureCapacity(newSize) (ensures the size of buffer increases) and validateIndex(index) => index is @NonNegative @LTLengthOf("buffer")
        and size - index <= buffer.length - index - strLength
    #2: ensureCapacity(newSize) => newSize is @LTEqLengthOf("this.buffer"), hence assignment is valid
    #3: ensureCapacity(newSize) (ensures the size of buffer increases) and validateIndex(index) => index is @NonNegative @LTLengthOf("buffer")
    */
    public StrBuilder insert(final int index, String str) {
        validateIndex(index);
        if (str == null) {
            str = nullText;
        }
        if (str != null) {
            final int strLen = str.length();
            if (strLen > 0) {
                final int newSize = size + strLen;
                ensureCapacity(newSize);
                System.arraycopy(buffer, index, buffer, index + strLen, size - index); // #1
                size = newSize; // #2
                str.getChars(0, strLen, buffer, index); // #3
            }
        }
        return this;
    }

    /**
     * Inserts the character array into this builder.
     * Inserting null will use the stored null text value.
     *
     * @param index  the index to add at, must be valid
     * @param chars  the char array to insert
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    @SuppressWarnings({"index:argument.type.incompatible","index:compound.assignment.type.incompatible"}) /*
    #1: ensureCapacity(size + len) (ensures the size of buffer increases) and validateIndex(index) => index is @NonNegative @LTLengthOf("buffer")
        and size - index <= buffer.length - index - len
    #2: ensureCapacity(size + len) => size + len <= buffer.length, hence len <= buffer.length - size, hence len <= buffer.length - index
    #3: ensureCapacity(size + len) => size + len <= buffer.length
    */
    public StrBuilder insert(final int index, final char chars[]) {
        validateIndex(index);
        if (chars == null) {
            return insert(index, nullText);
        }
        final int len = chars.length;
        if (len > 0) {
            ensureCapacity(size + len);
            System.arraycopy(buffer, index, buffer, index + len, size - index); // #1
            System.arraycopy(chars, 0, buffer, index, len); // #2
            size += len; // #3
        }
        return this;
    }

    /**
     * Inserts part of the character array into this builder.
     * Inserting null will use the stored null text value.
     *
     * @param index  the index to add at, must be valid
     * @param chars  the char array to insert
     * @param offset  the offset into the character array to start at, must be valid
     * @param length  the length of the character array part to copy, must be positive
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if any index is invalid
     */
    @SuppressWarnings({"index:argument.type.incompatible","index:compound.assignment.type.incompatible"}) /*
    #1: validateIndex(index) implies index to be @IndexOrHigh("buffer"), but ensureCapacity(size + length) 
        for length > 0 increases the size of buffer hence making index to be @IndexFor("buffer")
        ensureCapacity(size + length) for length > 0 => size - index to be @NonNegative, 
        also size - index <= buffer.length - index - length because of ensureCapacity(size + length)
    #2: offset >= 0, and offset + length <= chars.length, but length > 0, hence offset < chars.length
    #3: ensureCapacity(size + length) => size + length is @LTEqLengthOf("this.buffer")
    */
    public StrBuilder insert(final int index, final char chars[], final int offset, final int length) {
        validateIndex(index);
        if (chars == null) {
            return insert(index, nullText);
        }
        if (offset < 0 || offset > chars.length) {
            throw new StringIndexOutOfBoundsException("Invalid offset: " + offset);
        }
        if (length < 0 || offset + length > chars.length) {
            throw new StringIndexOutOfBoundsException("Invalid length: " + length);
        }
        if (length > 0) {
            ensureCapacity(size + length);
            System.arraycopy(buffer, index, buffer, index + length, size - index); // #1
            System.arraycopy(chars, offset, buffer, index, length); // #2
            size += length; // #3
        }
        return this;
    }

    /**
     * Inserts the value into this builder.
     *
     * @param index  the index to add at, must be valid
     * @param value  the value to insert
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    @SuppressWarnings({"index:argument.type.incompatible","array.access.unsafe.low","array.access.unsafe.high","compound.assignment.type.incompatible"}) /*
    #1: validateIndex(index) => index is @LTEqLengthOf("buffer"), but ensureCapacity(size + 4) makes index @LTLengthOf("buffer"),
        also, size - index <= buffer.length - index - 4 (because initially index <= size, but ensureCapacity(size + 4) increased buffer.length to at least size + 4)
    #2: validateIndex(index) => index is @LTEqLengthOf("buffer"), but ensureCapacity(size + 5) makes index @LTLengthOf("buffer"),
        also, size - index <= buffer.length - index - 5 (because initially index <= size, but ensureCapacity(size + 5) increased buffer.length to at least size + 5)
    #3: ensureCapacity(size + 4) and index initially to be @IndexOrHigh("buffer") by validateIndex(index) ensures index till index + 3 be @IndexFor("buffer")
    #4: ensureCapacity(size + 5) and index initially to be @IndexOrHigh("buffer") by validateIndex(index) ensures index till index + 4 be @IndexFor("buffer")
    #5: ensureCapacity(size + 4) => size + 4 to be @LTEqLengthOf("buffer")
    #6: ensureCapacity(size + 5) => size + 5 to be @LTEqLengthOf("buffer")
    */
    public StrBuilder insert(int index, final boolean value) {
        validateIndex(index);
        if (value) {
            ensureCapacity(size + 4);
            System.arraycopy(buffer, index, buffer, index + 4, size - index); // #1
            buffer[index++] = 't'; // #3
            buffer[index++] = 'r'; // #3
            buffer[index++] = 'u'; // #3
            buffer[index] = 'e'; // #3
            size += 4; // #5
        } else {
            ensureCapacity(size + 5);
            System.arraycopy(buffer, index, buffer, index + 5, size - index); // #2
            buffer[index++] = 'f'; // #4
            buffer[index++] = 'a'; // #4
            buffer[index++] = 'l'; // #4
            buffer[index++] = 's'; // #4
            buffer[index] = 'e'; // #4
            size += 5; // #6
        }
        return this;
    }

    /**
     * Inserts the value into this builder.
     *
     * @param index  the index to add at, must be valid
     * @param value  the value to insert
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    @SuppressWarnings({"index:compound.assignment.type.incompatible","index:argument.type.incompatible","index:array.access.unsafe.low","array.access.unsafe.high"}) /*
    #1: validateIndex(index) ensures index to be @IndexOrHigh("buffer"), but ensureCapacity(size + 1) makes index @IndexFor("buffer")
        size - index <= buffer.length - index - 1 (as buffer.length is increased to at least size + 1)
    #2: index is @IndexFor("buffer") as explained in #1
    #3: ensureCapacity(size + 1) ensures size + 1 to be @LTEqLengthOf("this.buffer")
    */
    public StrBuilder insert(final int index, final char value) {
        validateIndex(index);
        ensureCapacity(size + 1);
        System.arraycopy(buffer, index, buffer, index + 1, size - index); // #1
        buffer[index] = value; // #2
        size++; // #3
        return this;
    }

    /**
     * Inserts the value into this builder.
     *
     * @param index  the index to add at, must be valid
     * @param value  the value to insert
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    public StrBuilder insert(final int index, final int value) {
        return insert(index, String.valueOf(value));
    }

    /**
     * Inserts the value into this builder.
     *
     * @param index  the index to add at, must be valid
     * @param value  the value to insert
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    public StrBuilder insert(final int index, final long value) {
        return insert(index, String.valueOf(value));
    }

    /**
     * Inserts the value into this builder.
     *
     * @param index  the index to add at, must be valid
     * @param value  the value to insert
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    public StrBuilder insert(final int index, final float value) {
        return insert(index, String.valueOf(value));
    }

    /**
     * Inserts the value into this builder.
     *
     * @param index  the index to add at, must be valid
     * @param value  the value to insert
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    public StrBuilder insert(final int index, final double value) {
        return insert(index, String.valueOf(value));
    }

    //-----------------------------------------------------------------------
    /**
     * Internal method to delete a range without validation.
     *
     * @param startIndex  the start index, must be valid
     * @param endIndex  the end index (exclusive), must be valid
     * @param len  the length, must be valid
     * @throws IndexOutOfBoundsException if any index is invalid
     */
    @SuppressWarnings({"index:argument.type.incompatible"}) /* #1: cannot annotate to compare the two integers size and endIndex, but this is an internal method
    and it is called from other methods where endIndex <= size always
    #2: The checker does not issue an error here where it should have, but still the code is correct as size >= len as explained in #1
    */
    private void deleteImpl(final @NonNegative @LTLengthOf("this.buffer") int startIndex, final @NonNegative @LTEqLengthOf("this.buffer") int endIndex, final @NonNegative @LTEqLengthOf("this.buffer") int len) {
        System.arraycopy(buffer, endIndex, buffer, startIndex, size - endIndex); // #1
        size -= len; // #2
    }

    /**
     * Deletes the characters between the two specified indices.
     *
     * @param startIndex  the start index, inclusive, must be valid
     * @param endIndex  the end index, exclusive, must be valid except
     *  that if too large it is treated as end of string
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    @SuppressWarnings("index:argument.type.incompatible") // #1 validateRange(startIndex, endIndex) => the two arguments are a valid index for buffer and hence len = endIndex - startIndex is also @NonNegative and @LTEqLengthOf("this.buffer")
    public StrBuilder delete(final int startIndex, int endIndex) {
        endIndex = validateRange(startIndex, endIndex);
        final int len = endIndex - startIndex;
        if (len > 0) {
            deleteImpl(startIndex, endIndex, len); // #1
        }
        return this;
    }

    //-----------------------------------------------------------------------
    /**
     * Deletes the character wherever it occurs in the builder.
     *
     * @param ch  the character to delete
     * @return this, to enable chaining
     */
    @SuppressWarnings({"index:compound.assignment.type.incompatible","index:array.access.unsafe.low","index:argument.type.incompatible"})/*
    #1: start = i before the while loop, and i = i - len = i - i + start = start which is @NonNegative
    #2: start = i < size => start is @NonNegative and @LTLengthOf("this.buffer")
        i < size and ++i < size => i is @NonNegative and @LTEqLengthOf("this.buffer")
        len = i - start, i >= start, as i has been incremented after start = i, hence len is @NonNegative and @LTEqLengthOf("this,buffer")
    */
    public StrBuilder deleteAll(final char ch) {
        for (@NonNegative int i = 0; i < size; i++) {
            if (buffer[i] == ch) {
                final @NonNegative int start = i;
                while (++i < size) {
                    if (buffer[i] != ch) {
                        break;
                    }
                }
                final int len = i - start;
                deleteImpl(start, i, len); // #2
                i -= len; // #1
            }
        }
        return this;
    }

    /**
     * Deletes the character wherever it occurs in the builder.
     *
     * @param ch  the character to delete
     * @return this, to enable chaining
     */
    @SuppressWarnings("index:argument.type.incompatible") // #1: if this code is executed then size is at least 1, hence 1 is @LTEqLengthOf("this.buffer")
    public StrBuilder deleteFirst(final char ch) {
        for (int i = 0; i < size; i++) {
            if (buffer[i] == ch) {
                deleteImpl(i, i + 1, 1); // #1
                break;
            }
        }
        return this;
    }

    //-----------------------------------------------------------------------
    /**
     * Deletes the string wherever it occurs in the builder.
     *
     * @param str  the string to delete, null causes no action
     * @return this, to enable chaining
     */
    @SuppressWarnings("index:argument.type.incompatible") // #1 str.length() > 0 => index < buffer.length and index + len <= buffer.length, also len here is @LTEqLengthOf("buffer") as it was found in the buffer
    public StrBuilder deleteAll(final String str) {
        final int len = (str == null ? 0 : str.length());
        if (len > 0) {
            int index = indexOf(str, 0);
            while (index >= 0) {
                deleteImpl(index, index + len, len); // #1
                index = indexOf(str, index);
            }
        }
        return this;
    }

    /**
     * Deletes the string wherever it occurs in the builder.
     *
     * @param str  the string to delete, null causes no action
     * @return this, to enable chaining
     */
    @SuppressWarnings("index:argument.type.incompatible") // #1 str.length() > 0 => index < buffer.length and index + len <= buffer.length, also len here is @LTEqLengthOf("buffer") as it was found in the buffer
    public StrBuilder deleteFirst(final String str) {
        final int len = (str == null ? 0 : str.length());
        if (len > 0) {
            final int index = indexOf(str, 0);
            if (index >= 0) {
                deleteImpl(index, index + len, len); // #1
            }
        }
        return this;
    }

    //-----------------------------------------------------------------------
    /**
     * Deletes all parts of the builder that the matcher matches.
     * <p>
     * Matchers can be used to perform advanced deletion behaviour.
     * For example you could write a matcher to delete all occurrences
     * where the character 'a' is followed by a number.
     *
     * @param matcher  the matcher to use to find the deletion, null causes no action
     * @return this, to enable chaining
     */
    public StrBuilder deleteAll(final StrMatcher matcher) {
        return replace(matcher, null, 0, size, -1);
    }

    /**
     * Deletes the first match within the builder using the specified matcher.
     * <p>
     * Matchers can be used to perform advanced deletion behaviour.
     * For example you could write a matcher to delete
     * where the character 'a' is followed by a number.
     *
     * @param matcher  the matcher to use to find the deletion, null causes no action
     * @return this, to enable chaining
     */
    public StrBuilder deleteFirst(final StrMatcher matcher) {
        return replace(matcher, null, 0, size, 1);
    }

    //-----------------------------------------------------------------------
    /**
     * Internal method to delete a range without validation.
     *
     * @param startIndex  the start index, must be valid
     * @param endIndex  the end index (exclusive), must be valid
     * @param removeLen  the length to remove (endIndex - startIndex), must be valid
     * @param insertStr  the string to replace with, null means delete range
     * @param insertLen  the length of the insert string, must be valid
     * @throws IndexOutOfBoundsException if any index is invalid
     */
    @SuppressWarnings({"index:assignment.type.incompatible","index:argument.type.incompatible"}) /*
    #1: cannot annotate to compare the two integers size and endIndex, but this is an internal method and it is called from other methods where endIndex <= size always, hence size - endIndex is @NonNegative
        Also, ensureCapacity(newSize) => size - endIndex + startIndex + insertLen <= buffer.length, hence size - endIndex <= buffer.length - startIndex - insertLen
    #2 ensureCapacity(newSize) => size = newSize = @LTEqLengthOf("this.buffer")
    */
    private void replaceImpl(final @IndexFor("this.buffer") int startIndex, final @NonNegative @LTEqLengthOf("this.buffer") int endIndex, final @NonNegative int removeLen, final String insertStr, final @NonNegative @LTEqLengthOf("#4") int insertLen) {
        final int newSize = size - removeLen + insertLen;
        if (insertLen != removeLen) {
            ensureCapacity(newSize);
            System.arraycopy(buffer, endIndex, buffer, startIndex + insertLen, size - endIndex); // #1
            size = newSize; // #2
        }
        if (insertLen > 0) {
            insertStr.getChars(0, insertLen, buffer, startIndex); // #3
        }
    }

    /**
     * Replaces a portion of the string builder with another string.
     * The length of the inserted string does not have to match the removed length.
     *
     * @param startIndex  the start index, inclusive, must be valid
     * @param endIndex  the end index, exclusive, must be valid except
     *  that if too large it is treated as end of string
     * @param replaceStr  the string to replace with, null means delete range
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    @SuppressWarnings("index:argument.type.incompatible") /*
    #1: validateRange => startIndex and endIndex are valid indices for buffer and endIndex - startIndex is @NonNegative,
    insertLen is also @NonNegative and @LTEqLengthOf("replaceStr") 
    */ 
    public StrBuilder replace(final int startIndex, int endIndex, final String replaceStr) {
        endIndex = validateRange(startIndex, endIndex);
        final int insertLen = (replaceStr == null ? 0 : replaceStr.length());
        replaceImpl(startIndex, endIndex, endIndex - startIndex, replaceStr, insertLen); // #1
        return this;
    }

    //-----------------------------------------------------------------------
    /**
     * Replaces the search character with the replace character
     * throughout the builder.
     *
     * @param search  the search character
     * @param replace  the replace character
     * @return this, to enable chaining
     */
    public StrBuilder replaceAll(final char search, final char replace) {
        if (search != replace) {
            for (int i = 0; i < size; i++) {
                if (buffer[i] == search) {
                    buffer[i] = replace;
                }
            }
        }
        return this;
    }

    /**
     * Replaces the first instance of the search character with the
     * replace character in the builder.
     *
     * @param search  the search character
     * @param replace  the replace character
     * @return this, to enable chaining
     */
    public StrBuilder replaceFirst(final char search, final char replace) {
        if (search != replace) {
            for (int i = 0; i < size; i++) {
                if (buffer[i] == search) {
                    buffer[i] = replace;
                    break;
                }
            }
        }
        return this;
    }

    //-----------------------------------------------------------------------
    /**
     * Replaces the search string with the replace string throughout the builder.
     *
     * @param searchStr  the search string, null causes no action to occur
     * @param replaceStr  the replace string, null is equivalent to an empty string
     * @return this, to enable chaining
     */
    @SuppressWarnings("index:argument.type.incompatible") /*
    #1: searchLen > 0 => index + searchLen (index is @LTLengthOf(value="this.buffer", offset="searchStr.length() - 1")) int to be @LTLengthOf("this.buffer")
        searchLen is @NonNegative and @LTEqLengthOf("this.buffer") as defined
        replaceLen is @NonNegative and @LTEqLengthOf("replaceStr") as defined
    */
    public StrBuilder replaceAll(final String searchStr, final String replaceStr) {
        final int searchLen = (searchStr == null ? 0 : searchStr.length());
        if (searchLen > 0) {
            final int replaceLen = (replaceStr == null ? 0 : replaceStr.length());
            int index = indexOf(searchStr, 0);
            while (index >= 0) {
                replaceImpl(index, index + searchLen, searchLen, replaceStr, replaceLen); // #1
                index = indexOf(searchStr, index + replaceLen);
            }
        }
        return this;
    }

    /**
     * Replaces the first instance of the search string with the replace string.
     *
     * @param searchStr  the search string, null causes no action to occur
     * @param replaceStr  the replace string, null is equivalent to an empty string
     * @return this, to enable chaining
     */
    @SuppressWarnings("index:argument.type.incompatible") /*
    #1: searchLen > 0 => index + searchLen (index is @LTLengthOf(value="this.buffer", offset="searchStr.length() - 1")) int to be @LTLengthOf("this.buffer")
        searchLen is @NonNegative and @LTEqLengthOf("this.buffer") as defined
        replaceLen is @NonNegative and @LTEqLengthOf("replaceStr") as defined
    */
    public StrBuilder replaceFirst(final String searchStr, final String replaceStr) {
        final int searchLen = (searchStr == null ? 0 : searchStr.length());
        if (searchLen > 0) {
            final int index = indexOf(searchStr, 0);
            if (index >= 0) {
                final int replaceLen = (replaceStr == null ? 0 : replaceStr.length());
                replaceImpl(index, index + searchLen, searchLen, replaceStr, replaceLen);
            }
        }
        return this;
    }

    //-----------------------------------------------------------------------
    /**
     * Replaces all matches within the builder with the replace string.
     * <p>
     * Matchers can be used to perform advanced replace behaviour.
     * For example you could write a matcher to replace all occurrences
     * where the character 'a' is followed by a number.
     *
     * @param matcher  the matcher to use to find the deletion, null causes no action
     * @param replaceStr  the replace string, null is equivalent to an empty string
     * @return this, to enable chaining
     */
    public StrBuilder replaceAll(final StrMatcher matcher, final String replaceStr) {
        return replace(matcher, replaceStr, 0, size, -1);
    }

    /**
     * Replaces the first match within the builder with the replace string.
     * <p>
     * Matchers can be used to perform advanced replace behaviour.
     * For example you could write a matcher to replace
     * where the character 'a' is followed by a number.
     *BUILD FAILURE
     * @param matcher  the matcher to use to find the deletion, null causes no action
     * @param replaceStr  the replace string, null is equivalent to an empty string
     * @return this, to enable chaining
     */
    public StrBuilder replaceFirst(final StrMatcher matcher, final String replaceStr) {
        return replace(matcher, replaceStr, 0, size, 1);
    }

    // -----------------------------------------------------------------------
    /**
     * Advanced search and replaces within the builder using a matcher.
     * <p>
     * Matchers can be used to perform advanced behaviour.
     * For example you could write a matcher to delete all occurrences
     * where the character 'a' is followed by a number.
     *
     * @param matcher  the matcher to use to find the deletion, null causes no action
     * @param replaceStr  the string to replace the match with, null is a delete
     * @param startIndex  the start index, inclusive, must be valid
     * @param endIndex  the end index, exclusive, must be valid except
     *  that if too large it is treated as end of string
     * @param replaceCount  the number of times to replace, -1 for replace all
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if start index is invalid
     */
    @SuppressWarnings("index:argument.type.incompatible") // #1: validateRange(startIndex, endIndex) => startIndex is @IndexFor("buffer") and endIndex is @IndexOrHigh("buffer")
    public StrBuilder replace(
            final StrMatcher matcher, final String replaceStr,
            final int startIndex, int endIndex, final int replaceCount) {
        endIndex = validateRange(startIndex, endIndex);
        return replaceImpl(matcher, replaceStr, startIndex, endIndex, replaceCount); // #1
    }

    /**
     * Replaces within the builder using a matcher.
     * <p>
     * Matchers can be used to perform advanced behaviour.
     * For example you could write a matcher to delete all occurrences
     * where the character 'a' is followed by a number.
     *
     * @param matcher  the matcher to use to find the deletion, null causes no action
     * @param replaceStr  the string to replace the match with, null is a delete
     * @param from  the start index, must be valid
     * @param to  the end index (exclusive), must be valid
     * @param replaceCount  the number of times to replace, -1 for replace all
     * @return this, to enable chaining
     * @throws IndexOutOfBoundsException if any index is invalid
     */
    @SuppressWarnings({"index:argument.type.incompatible","index:assignment.type.incompatible"}) /*
    #1: removeLen is @LTLengthOf(value = "buf", offset = "i - 1") but buf.length = buffer.length, hence removeLen is @LTLengthOf(value = "buffer", offset = "i - 1")
        Also, replaceLen is @NonNegative @LTEqLengthOf("replaceStr") as defined
    #2: to - removeLen + replaceLen = to - @LTLengthOf(value = "buffer", offset = "i - 1") + replaceStr.length() and to >= buffer.length - i + 1  hence the expression is @NonNegative
        The upperbound is handled by expanding the buffer in the call for replaceImpl in #1 by ensureCapacity(newSize)
    */
    private StrBuilder replaceImpl(
            final StrMatcher matcher, final String replaceStr,
            final @IndexFor("this.buffer") int from, @IndexOrHigh("this.buffer") int to, int replaceCount) {
        if (matcher == null || size == 0) {
            return this;
        }
        final int replaceLen = (replaceStr == null ? 0 : replaceStr.length());
        for (int i = from; i < to && replaceCount != 0; i++) {
            final char @SameLen("this.buffer") [] buf = buffer;
            final int removeLen = matcher.isMatch(buf, i, from, to);
            if (removeLen > 0) {
                replaceImpl(i, i + removeLen, removeLen, replaceStr, replaceLen); // #1
                to = to - removeLen + replaceLen; // #2
                i = i + replaceLen - 1;
                if (replaceCount > 0) {
                    replaceCount--;
                }
            }
        }
        return this;
    }

    //-----------------------------------------------------------------------
    /**
     * Reverses the string builder placing each character in the opposite index.
     *
     * @return this, to enable chaining
     */
    @SuppressWarnings({"index:assignment.type.incompatible","compound.assignment.type.incompatible"}) // #1: rightIdx from size - 1 till half the length or half + 1
    public StrBuilder reverse() {
        if (size == 0) {
            return this;
        }

        final int half = size / 2;
        final char[] buf = buffer;
        for (@IndexFor("buf") int leftIdx = 0, rightIdx = size - 1; leftIdx < half; leftIdx++, rightIdx--) { // #1
            final char swap = buf[leftIdx];
            buf[leftIdx] = buf[rightIdx];
            buf[rightIdx] = swap;
        }
        return this;
    }

    //-----------------------------------------------------------------------
    /**
     * Trims the builder by removing characters less than or equal to a space
     * from the beginning and end.
     *
     * @return this, to enable chaining
     */
    public StrBuilder trim() {
        if (size == 0) {
            return this;
        }
        int len = size;
        final char[] buf = buffer;
        int pos = 0;
        while (pos < len && buf[pos] <= ' ') {
            pos++;
        }
        while (pos < len && buf[len - 1] <= ' ') {
            len--;
        }
        if (len < size) {
            delete(len, size);
        }
        if (pos > 0) {
            delete(0, pos);
        }
        return this;
    }

    //-----------------------------------------------------------------------
    /**
     * Checks whether this builder starts with the specified string.
     * <p>
     * Note that this method handles null input quietly, unlike String.
     *
     * @param str  the string to search for, null returns false
     * @return true if the builder starts with the string
     */
    public boolean startsWith(final String str) {
        if (str == null) {
            return false;
        }
        final int len = str.length();
        if (len == 0) {
            return true;
        }
        if (len > size) {
            return false;
        }
        for (int i = 0; i < len; i++) {
            if (buffer[i] != str.charAt(i)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Checks whether this builder ends with the specified string.
     * <p>
     * Note that this method handles null input quietly, unlike String.
     *
     * @param str  the string to search for, null returns false
     * @return true if the builder ends with the string
     */
   @SuppressWarnings({"index:assignment.type.incompatible","compound.assignment.type.incompatible","array.access.unsafe.high"}) /*
    #1: len <= size as checked by previous if
    #2: pos++ till len times will make reach pos till size
    #3: pos is used only till size - 1 as the index to buffer
    */
    public boolean endsWith(final String str) {
        if (str == null) {
            return false;
        }
        final int len = str.length();
        if (len == 0) {
            return true;
        }
        if (len > size) {
            return false;
        }
        @NonNegative @IndexOrHigh("this.buffer") int pos = size - len; // #1
        for (int i = 0; i < len; i++, pos++) { // #2
            if (buffer[pos] != str.charAt(i)) { // #3
                return false;
            }
        }
        return true;
    }

    //-----------------------------------------------------------------------
    /**
     * {@inheritDoc}
     * @since 3.0
     */
    @Override
    public CharSequence subSequence(final int startIndex, final int endIndex) {
      if (startIndex < 0) {
          throw new StringIndexOutOfBoundsException(startIndex);
      }
      if (endIndex > size) {
          throw new StringIndexOutOfBoundsException(endIndex);
      }
      if (startIndex > endIndex) {
          throw new StringIndexOutOfBoundsException(endIndex - startIndex);
      }
      return substring(startIndex, endIndex);
    }

    /**
     * Extracts a portion of this string builder as a string.
     *
     * @param start  the start index, inclusive, must be valid
     * @return the new string
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    public String substring(final int start) {
        return substring(start, size);
    }

    /**
     * Extracts a portion of this string builder as a string.
     * <p>
     * Note: This method treats an endIndex greater than the length of the
     * builder as equal to the length of the builder, and continues
     * without error, unlike StringBuffer or String.
     *
     * @param startIndex  the start index, inclusive, must be valid
     * @param endIndex  the end index, exclusive, must be valid except
     *  that if too large it is treated as end of string
     * @return the new string
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    @SuppressWarnings("index:argument.type.incompatible") // validateRange => startIndex to be @IndexFor("buffer") and endIndex to be @IndexOrHigh("buffer") and endIndex >= startIndex
    public String substring(final int startIndex, int endIndex) {
        endIndex = validateRange(startIndex, endIndex);
        return new String(buffer, startIndex, endIndex - startIndex);
    }

    /**
     * Extracts the leftmost characters from the string builder without
     * throwing an exception.
     * <p>
     * This method extracts the left <code>length</code> characters from
     * the builder. If this many characters are not available, the whole
     * builder is returned. Thus the returned string may be shorter than the
     * length requested.
     *
     * @param length  the number of characters to extract, negative returns empty string
     * @return the new string
     */
    public String leftString(final int length) {
        if (length <= 0) {
            return StringUtils.EMPTY;
        } else if (length >= size) {
            return new String(buffer, 0, size);
        } else {
            return new String(buffer, 0, length);
        }
    }

    /**
     * Extracts the rightmost characters from the string builder without
     * throwing an exception.
     * <p>
     * This method extracts the right <code>length</code> characters from
     * the builder. If this many characters are not available, the whole
     * builder is returned. Thus the returned string may be shorter than the
     * length requested.
     *
     * @param length  the number of characters to extract, negative returns empty string
     * @return the new string
     */
    @SuppressWarnings("index:argument.type.incompatible") // #1: length < size => size - length is @NonNegative, also length > 0, so size - length is @LTLengthOf("buffer")
    public String rightString(final int length) {
        if (length <= 0) {
            return StringUtils.EMPTY;
        } else if (length >= size) {
            return new String(buffer, 0, size);
        } else {
            return new String(buffer, size - length, length); // #1
        }
    }

    /**
     * Extracts some characters from the middle of the string builder without
     * throwing an exception.
     * <p>
     * This method extracts <code>length</code> characters from the builder
     * at the specified index.
     * If the index is negative it is treated as zero.
     * If the index is greater than the builder size, it is treated as the builder size.
     * If the length is negative, the empty string is returned.
     * If insufficient characters are available in the builder, as much as possible is returned.
     * Thus the returned string may be shorter than the length requested.
     *
     * @param index  the index to start at, negative means zero
     * @param length  the number of characters to extract, negative returns empty string
     * @return the new string
     */
    @SuppressWarnings("index:argument.type.incompatible") // #1 index < size => size - index is @Positive
    public String midString(int index, final int length) {
        if (index < 0) {
            index = 0;
        }
        if (length <= 0 || index >= size) {
            return StringUtils.EMPTY;
        }
        if (size <= index + length) {
            return new String(buffer, index, size - index); // #1
        }
        return new String(buffer, index, length);
    }

    //-----------------------------------------------------------------------
    /**
     * Checks if the string builder contains the specified char.
     *
     * @param ch  the character to find
     * @return true if the builder contains the character
     */
    public boolean contains(final char ch) {
        final char[] thisBuf = buffer;
        for (int i = 0; i < this.size; i++) {
            if (thisBuf[i] == ch) {
                return true;
            }
        }
        return false;
    }

    /**
     * Checks if the string builder contains the specified string.
     *
     * @param str  the string to find
     * @return true if the builder contains the string
     */
    public boolean contains(final String str) {
        return indexOf(str, 0) >= 0;
    }

    /**
     * Checks if the string builder contains a string matched using the
     * specified matcher.
     * <p>
     * Matchers can be used to perform advanced searching behaviour.
     * For example you could write a matcher to search for the character
     * 'a' followed by a number.
     *
     * @param matcher  the matcher to use, null returns -1
     * @return true if the matcher finds a match in the builder
     */
    public boolean contains(final StrMatcher matcher) {
        return indexOf(matcher, 0) >= 0;
    }

    //-----------------------------------------------------------------------
    /**
     * Searches the string builder to find the first reference to the specified char.
     *
     * @param ch  the character to find
     * @return the first index of the character, or -1 if not found
     */
    public @IndexOrLow("this.buffer") int indexOf(final char ch) {
        return indexOf(ch, 0);
    }

    /**
     * Searches the string builder to find the first reference to the specified char.
     *
     * @param ch  the character to find
     * @param startIndex  the index to start at, invalid index rounded to edge
     * @return the first index of the character, or -1 if not found
     */
    public @IndexOrLow("this.buffer") int indexOf(final char ch, int startIndex) {
        startIndex = (startIndex < 0 ? 0 : startIndex);
        if (startIndex >= size) {
            return -1;
        }
        final char[] thisBuf = buffer;
        for (int i = startIndex; i < size; i++) {
            if (thisBuf[i] == ch) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Searches the string builder to find the first reference to the specified string.
     * <p>
     * Note that a null input string will return -1, whereas the JDK throws an exception.
     *
     * @param str  the string to find, null returns -1
     * @return the first index of the string, or -1 if not found
     */
    public @GTENegativeOne @LTLengthOf(value = {"this.buffer"}, offset = {"#1.length() - 1"}) int indexOf(final String str) {
        return indexOf(str, 0);
    }

    /**
     * Searches the string builder to find the first reference to the specified
     * string starting searching from the given index.
     * <p>
     * Note that a null input string will return -1, whereas the JDK throws an exception.
     *
     * @param str  the string to find, null returns -1
     * @param startIndex  the index to start at, invalid index rounded to edge
     * @return the first index of the string, or -1 if not found
     */
    @SuppressWarnings({"index:return.type.incompatible","index:argument.type.incompatible","array.access.unsafe.high"}) /*
    #1, #4, #7: return -1 which is a compatible return type
    #2: 0 <= startIndex < size => argument is compatible, also, return type is also compatible as it returns @LTLengthof(value = "buffer", offset = "0")
    #3: startIndex is @IndexFor("buffer") and if strLen = 0, required return type is  @LTLengthOf(value = {"this.buffer"}, offset = {"- 1"})
    #5: i + j when being used as index has max value len + strLen - 2 = size - strLen + 1 + strLen - 2 = size - 1 which is a valid index
    #6 i = len here, len = size - strLen + 1, which is @LTLengthOf(value = {"this.buffer"}, offset = {"strLen - 1"})
    */
    public @GTENegativeOne @LTLengthOf(value = {"this.buffer"}, offset = {"#1.length() - 1"}) int indexOf(final String str, int startIndex) {
        startIndex = (startIndex < 0 ? 0 : startIndex);
        if (str == null || startIndex >= size) {
            return -1; // #1
        }
        final int strLen = str.length();
        if (strLen == 1) {
            return indexOf(str.charAt(0), startIndex); // #2
        }
        if (strLen == 0) {
            return startIndex; // #3
        }
        if (strLen > size) {
            return -1; // #4
        }
        final char[] thisBuf = buffer;
        final int len = size - strLen + 1;
        outer:
        for (int i = startIndex; i < len; i++) {
            for (int j = 0; j < strLen; j++) {
                if (str.charAt(j) != thisBuf[i + j]) { // #5
                    continue outer;
                }
            }
            return i; // #6
        }
        return -1; // #7
    }

    /**
     * Searches the string builder using the matcher to find the first match.
     * <p>
     * Matchers can be used to perform advanced searching behaviour.
     * For example you could write a matcher to find the character 'a'
     * followed by a number.
     *
     * @param matcher  the matcher to use, null returns -1
     * @return the first index matched, or -1 if not found
     */
    public @IndexOrLow("this.buffer") int indexOf(final StrMatcher matcher) {
        return indexOf(matcher, 0);
    }

    /**
     * Searches the string builder using the matcher to find the first
     * match searching from the given index.
     * <p>
     * Matchers can be used to perform advanced searching behaviour.
     * For example you could write a matcher to find the character 'a'
     * followed by a number.
     *
     * @param matcher  the matcher to use, null returns -1
     * @param startIndex  the index to start at, invalid index rounded to edge
     * @return the first index matched, or -1 if not found
     */
    public @IndexOrLow("this.buffer") int indexOf(final StrMatcher matcher, int startIndex) {
        startIndex = (startIndex < 0 ? 0 : startIndex);
        if (matcher == null || startIndex >= size) {
            return -1;
        }
        final int len = size;
        final char[] buf = buffer;
        for (int i = startIndex; i < len; i++) {
            if (matcher.isMatch(buf, i, startIndex, len) > 0) {
                return i;
            }
        }
        return -1;
    }

    //-----------------------------------------------------------------------
    /**
     * Searches the string builder to find the last reference to the specified char.
     *
     * @param ch  the character to find
     * @return the last index of the character, or -1 if not found
     */
    public @IndexOrLow("this.buffer") int lastIndexOf(final char ch) {
        return lastIndexOf(ch, size - 1);
    }

    /**
     * Searches the string builder to find the last reference to the specified char.
     *
     * @param ch  the character to find
     * @param startIndex  the index to start at, invalid index rounded to edge
     * @return the last index of the character, or -1 if not found
     */
    public @IndexOrLow("this.buffer") int lastIndexOf(final char ch, int startIndex) {
        startIndex = (startIndex >= size ? size - 1 : startIndex);
        if (startIndex < 0) {
            return -1;
        }
        for (int i = startIndex; i >= 0; i--) {
            if (buffer[i] == ch) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Searches the string builder to find the last reference to the specified string.
     * <p>
     * Note that a null input string will return -1, whereas the JDK throws an exception.
     *
     * @param str  the string to find, null returns -1
     * @return the last index of the string, or -1 if not found
     */
    public @IndexOrLow("this.buffer") int lastIndexOf(final String str) {
        return lastIndexOf(str, size - 1);
    }

    /**
     * Searches the string builder to find the last reference to the specified
     * string starting searching from the given index.
     * <p>
     * Note that a null input string will return -1, whereas the JDK throws an exception.
     *
     * @param str  the string to find, null returns -1
     * @param startIndex  the index to start at, invalid index rounded to edge
     * @return the last index of the string, or -1 if not found
     */
    @SuppressWarnings({"index:argument.type.incompatible","index:return.type.incompatible","index:array.access.unsafe.high"}) /*
    #1: 0 < startIndex < size as ensured in the previous statements
    #2: i + j when used as the index of buffer has the max value startIndex - strLen + 1 + strLen - 2 = startIndex - 1 < buffer.length
    #3: i is -1 here, which is a valid return value
    */
    public @IndexOrLow("this.buffer") int lastIndexOf(final String str, int startIndex) {
        startIndex = (startIndex >= size ? size - 1 : startIndex);
        if (str == null || startIndex < 0) {
            return -1;
        }
        final int strLen = str.length();
        if (strLen > 0 && strLen <= size) {
            if (strLen == 1) {
                return lastIndexOf(str.charAt(0), startIndex); // #1
            }

            outer:
            for (int i = startIndex - strLen + 1; i >= 0; i--) {
                for (int j = 0; j < strLen; j++) {
                    if (str.charAt(j) != buffer[i + j]) { // #2
                        continue outer;
                    }
                }
                return i; // #3
            }

        } else if (strLen == 0) {
            return startIndex;
        }
        return -1;
    }

    /**
     * Searches the string builder using the matcher to find the last match.
     * <p>
     * Matchers can be used to perform advanced searching behaviour.
     * For example you could write a matcher to find the character 'a'
     * followed by a number.
     *
     * @param matcher  the matcher to use, null returns -1
     * @return the last index matched, or -1 if not found
     */
    public @IndexOrLow("this.buffer") int lastIndexOf(final StrMatcher matcher) {
        return lastIndexOf(matcher, size);
    }

    /**
     * Searches the string builder using the matcher to find the last
     * match searching from the given index.
     * <p>
     * Matchers can be used to perform advanced searching behaviour.
     * For example you could write a matcher to find the character 'a'
     * followed by a number.
     *
     * @param matcher  the matcher to use, null returns -1
     * @param startIndex  the index to start at, invalid index rounded to edge
     * @return the last index matched, or -1 if not found
     */
    public @IndexOrLow("this.buffer") int lastIndexOf(final StrMatcher matcher, int startIndex) {
        startIndex = (startIndex >= size ? size - 1 : startIndex);
        if (matcher == null || startIndex < 0) {
            return -1;
        }
        final char[] buf = buffer;
        final int endIndex = startIndex + 1;
        for (int i = startIndex; i >= 0; i--) {
            if (matcher.isMatch(buf, i, 0, endIndex) > 0) {
                return i;
            }
        }
        return -1;
    }

    //-----------------------------------------------------------------------
    /**
     * Creates a tokenizer that can tokenize the contents of this builder.
     * <p>
     * This method allows the contents of this builder to be tokenized.
     * The tokenizer will be setup by default to tokenize on space, tab,
     * newline and formfeed (as per StringTokenizer). These values can be
     * changed on the tokenizer class, before retrieving the tokens.
     * <p>
     * The returned tokenizer is linked to this builder. You may intermix
     * calls to the builder and tokenizer within certain limits, however
     * there is no synchronization. Once the tokenizer has been used once,
     * it must be {@link StrTokenizer#reset() reset} to pickup the latest
     * changes in the builder. For example:
     * <pre>
     * StrBuilder b = new StrBuilder();
     * b.append("a b ");
     * StrTokenizer t = b.asTokenizer();
     * String[] tokens1 = t.getTokenArray();  // returns a,b
     * b.append("c d ");
     * String[] tokens2 = t.getTokenArray();  // returns a,b (c and d ignored)
     * t.reset();              // reset causes builder changes to be picked up
     * String[] tokens3 = t.getTokenArray();  // returns a,b,c,d
     * </pre>
     * In addition to simply intermixing appends and tokenization, you can also
     * call the set methods on the tokenizer to alter how it tokenizes. Just
     * remember to call reset when you want to pickup builder changes.
     * <p>
     * Calling {@link StrTokenizer#reset(String)} or {@link StrTokenizer#reset(char[])}
     * with a non-null value will break the link with the builder.
     *
     * @return a tokenizer that is linked to this builder
     */
    public StrTokenizer asTokenizer() {
        return new StrBuilderTokenizer();
    }

    //-----------------------------------------------------------------------
    /**
     * Gets the contents of this builder as a Reader.
     * <p>
     * This method allows the contents of the builder to be read
     * using any standard method that expects a Reader.
     * <p>
     * To use, simply create a <code>StrBuilder</code>, populate it with
     * data, call <code>asReader</code>, and then read away.
     * <p>
     * The internal character array is shared between the builder and the reader.
     * This allows you to append to the builder after creating the reader,
     * and the changes will be picked up.
     * Note however, that no synchronization occurs, so you must perform
     * all operations with the builder and the reader in one thread.
     * <p>
     * The returned reader supports marking, and ignores the flush method.
     *
     * @return a reader that reads from this builder
     */
    public Reader asReader() {
        return new StrBuilderReader();
    }

    //-----------------------------------------------------------------------
    /**
     * Gets this builder as a Writer that can be written to.
     * <p>
     * This method allows you to populate the contents of the builder
     * using any standard method that takes a Writer.
     * <p>
     * To use, simply create a <code>StrBuilder</code>,
     * call <code>asWriter</code>, and populate away. The data is available
     * at any time using the methods of the <code>StrBuilder</code>.
     * <p>
     * The internal character array is shared between the builder and the writer.
     * This allows you to intermix calls that append to the builder and
     * write using the writer and the changes will be occur correctly.
     * Note however, that no synchronization occurs, so you must perform
     * all operations with the builder and the writer in one thread.
     * <p>
     * The returned writer ignores the close and flush methods.
     *
     * @return a writer that populates this builder
     */
    public Writer asWriter() {
        return new StrBuilderWriter();
    }

    /**
     * Appends current contents of this <code>StrBuilder</code> to the
     * provided {@link Appendable}.
     * <p>
     * This method tries to avoid doing any extra copies of contents.
     *
     * @param appendable  the appendable to append data to
     * @throws IOException  if an I/O error occurs
     *
     * @since 3.4
     * @see #readFrom(Readable)
     */
    public void appendTo(final Appendable appendable) throws IOException {
        if (appendable instanceof Writer) {
            ((Writer) appendable).write(buffer, 0, size);
        } else if (appendable instanceof StringBuilder) {
            ((StringBuilder) appendable).append(buffer, 0, size);
        } else if (appendable instanceof StringBuffer) {
            ((StringBuffer) appendable).append(buffer, 0, size);
        } else if (appendable instanceof CharBuffer) {
            ((CharBuffer) appendable).put(buffer, 0, size);
        } else {
            appendable.append(this);
        }
    }

    /**
     * Checks the contents of this builder against another to see if they
     * contain the same character content ignoring case.
     *
     * @param other  the object to check, null returns false
     * @return true if the builders contain the same characters in the same order
     */
    public boolean equalsIgnoreCase(final StrBuilder other) {
        if (this == other) {
            return true;
        }
        if (this.size != other.size) {
            return false;
        }
        final char thisBuf[] = this.buffer;
        final char otherBuf[] = other.buffer;
        for (int i = size - 1; i >= 0; i--) {
            final char c1 = thisBuf[i];
            final char c2 = otherBuf[i];
            if (c1 != c2 && Character.toUpperCase(c1) != Character.toUpperCase(c2)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Checks the contents of this builder against another to see if they
     * contain the same character content.
     *
     * @param other  the object to check, null returns false
     * @return true if the builders contain the same characters in the same order
     */
    public boolean equals(final StrBuilder other) {
        if (this == other) {
            return true;
        }
        if (other == null) {
            return false;
        }
        if (this.size != other.size) {
            return false;
        }
        final char thisBuf[] = this.buffer;
        final char otherBuf[] = other.buffer;
        for (int i = size - 1; i >= 0; i--) {
            if (thisBuf[i] != otherBuf[i]) {
                return false;
            }
        }
        return true;
    }

    /**
     * Checks the contents of this builder against another to see if they
     * contain the same character content.
     *
     * @param obj  the object to check, null returns false
     * @return true if the builders contain the same characters in the same order
     */
    @Override
    public boolean equals(final Object obj) {
        return obj instanceof StrBuilder && equals((StrBuilder) obj);
    }

    /**
     * Gets a suitable hash code for this builder.
     *
     * @return a hash code
     */
    @Override
    public int hashCode() {
        final char buf[] = buffer;
        int hash = 0;
        for (int i = size - 1; i >= 0; i--) {
            hash = 31 * hash + buf[i];
        }
        return hash;
    }

    //-----------------------------------------------------------------------
    /**
     * Gets a String version of the string builder, creating a new instance
     * each time the method is called.
     * <p>
     * Note that unlike StringBuffer, the string version returned is
     * independent of the string builder.
     *
     * @return the builder as a String
     */
    @SuppressWarnings("override.return.invalid") // cannot annotate functions value to be @SameLen("this.buffer") because buffer's actual length may be larger than size, but the length of buffer in use is size only
    @Override
    public String toString() {
        return new String(buffer, 0, size);
    }

    /**
     * Gets a StringBuffer version of the string builder, creating a
     * new instance each time the method is called.
     *
     * @return the builder as a StringBuffer
     */
    public StringBuffer toStringBuffer() {
        return new StringBuffer(size).append(buffer, 0, size);
    }

    /**
     * Gets a StringBuilder version of the string builder, creating a
     * new instance each time the method is called.
     *
     * @return the builder as a StringBuilder
     * @since 3.2
     */
    public StringBuilder toStringBuilder() {
        return new StringBuilder(size).append(buffer, 0, size);
    }

    /**
     * Implement the {@link Builder} interface.
     * @return the builder as a String
     * @since 3.2
     * @see #toString()
     */
    @Override
    public String build() {
        return toString();
    }

    //-----------------------------------------------------------------------
    /**
     * Validates parameters defining a range of the builder.
     *
     * @param startIndex  the start index, inclusive, must be valid
     * @param endIndex  the end index, exclusive, must be valid except
     *  that if too large it is treated as end of string
     * @return the new string
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    protected int validateRange(final int startIndex, int endIndex) {
        if (startIndex < 0) {
            throw new StringIndexOutOfBoundsException(startIndex);
        }
        if (endIndex > size) {
            endIndex = size;
        }
        if (startIndex > endIndex) {
            throw new StringIndexOutOfBoundsException("end < start");
        }
        return endIndex;
    }

    /**
     * Validates parameters defining a single index in the builder.
     *
     * @param index  the index, must be valid
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    protected void validateIndex(final int index) {
        if (index < 0 || index > size) {
            throw new StringIndexOutOfBoundsException(index);
        }
    }

    //-----------------------------------------------------------------------
    /**
     * Inner class to allow StrBuilder to operate as a tokenizer.
     */
    class StrBuilderTokenizer extends StrTokenizer {

        /**
         * Default constructor.
         */
        StrBuilderTokenizer() {
            super();
        }

        /** {@inheritDoc} */
        @Override
        protected List<String> tokenize(final char[] chars, final int offset, final int count) {
            if (chars == null) {
                return super.tokenize(StrBuilder.this.buffer, 0, StrBuilder.this.size());
            }
            return super.tokenize(chars, offset, count);
        }

        /** {@inheritDoc} */
        @Override
        public String getContent() {
            final String str = super.getContent();
            if (str == null) {
                return StrBuilder.this.toString();
            }
            return str;
        }
    }

    //-----------------------------------------------------------------------
    /**
     * Inner class to allow StrBuilder to operate as a reader.
     */
    class StrBuilderReader extends Reader {
        /** The current stream position. */
        private @NonNegative int pos;
        /** The last mark position. */
        private @NonNegative int mark;

        /**
         * Default constructor.
         */
        StrBuilderReader() {
            super();
        }

        /** {@inheritDoc} */
        @Override
        public void close() {
            // do nothing
        }

        /** {@inheritDoc} */
        @SuppressWarnings("index:return.type.incompatible") // #1: returns the ASCII value of the character which is @NonNegative
        @Override
        public @GTENegativeOne int read() {
            if (ready() == false) {
                return -1;
            }
            return StrBuilder.this.charAt(pos++); // #1
        }

        /** {@inheritDoc} */
        @SuppressWarnings({"index:argument.type.incompatible","index:return.type.incompatible","index:compound.assignment.type.incompatible"}) /* 
        #1: pos + len <= size() and len > 0 => pos is @LTLengthOf("b")
        #2: len > 0 => pos + len is @NonNegative
        #3: len >= 0 and pos + len <= size => len <= size
        */
        @Override
        public @GTENegativeOne @LTEqLengthOf("#1") int read(final char b[], final int off, int len) {
            if (off < 0 || len < 0 || off > b.length ||
                    (off + len) > b.length || (off + len) < 0) {
                throw new IndexOutOfBoundsException();
            }
            if (len == 0) {
                return 0;
            }
            if (pos >= StrBuilder.this.size()) {
                return -1;
            }
            if (pos + len > size()) {
                len = StrBuilder.this.size() - pos;
            }
            StrBuilder.this.getChars(pos, pos + len, b, off); // #1
            pos += len; // #2
            return len; // #3
        }

        /** {@inheritDoc} */
        @Override
        public long skip(long n) {
            if (pos + n > StrBuilder.this.size()) {
                n = StrBuilder.this.size() - pos;
            }
            if (n < 0) {
                return 0;
            }
            pos += n;
            return n;
        }

        /** {@inheritDoc} */
        @Override
        public boolean ready() {
            return pos < StrBuilder.this.size();
        }

        /** {@inheritDoc} */
        @Override
        public boolean markSupported() {
            return true;
        }

        /** {@inheritDoc} */
        @Override
        public void mark(final int readAheadLimit) {
            mark = pos;
        }

        /** {@inheritDoc} */
        @Override
        public void reset() {
            pos = mark;
        }
    }

    //-----------------------------------------------------------------------
    /**
     * Inner class to allow StrBuilder to operate as a writer.
     */
    class StrBuilderWriter extends Writer {

        /**
         * Default constructor.
         */
        StrBuilderWriter() {
            super();
        }

        /** {@inheritDoc} */
        @Override
        public void close() {
            // do nothing
        }

        /** {@inheritDoc} */
        @Override
        public void flush() {
            // do nothing
        }

        /** {@inheritDoc} */
        @Override
        public void write(final int c) {
            StrBuilder.this.append((char) c);
        }

        /** {@inheritDoc} */
        @Override
        public void write(final char[] cbuf) {
            StrBuilder.this.append(cbuf);
        }

        /** {@inheritDoc} */
        @Override
        public void write(final char[] cbuf, final int off, final int len) {
            StrBuilder.this.append(cbuf, off, len);
        }

        /** {@inheritDoc} */
        @Override
        public void write(final String str) {
            StrBuilder.this.append(str);
        }

        /** {@inheritDoc} */
        @Override
        public void write(final String str, final int off, final int len) {
            StrBuilder.this.append(str, off, len);
        }
    }

}
