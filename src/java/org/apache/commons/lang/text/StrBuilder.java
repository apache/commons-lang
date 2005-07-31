/*
 * Copyright 2004-2005 The Apache Software Foundation.
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
package org.apache.commons.lang.text;

import java.util.Collection;
import java.util.Iterator;

import org.apache.commons.lang.ArrayUtils;

/**
 * Builds a string from consituant parts providing a more flexible and powerful API
 * than StringBuffer.
 * <p>
 * The main differences from StringBuffer/StringBuilder are:
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
 * </ul>
 * <p>
 * The aim has been to provide an API that mimics very closely what StringBuffer
 * provides, but with additional methods. It should be noted that some edge cases,
 * with invalid indices or null input, have been altered - see individual methods.
 * The biggest of these changes is that by default, null will not output the text
 * 'null'. This can be controlled by a property, {@link #setNullText(String)}.
 *
 * @author Stephen Colebourne
 * @since 2.2
 * @version $Id$
 */
public class StrBuilder implements Cloneable {

    /**
     * The extra capacity for new builders.
     */
    static final int CAPACITY = 32;

    /** Serialization lock. */
    private static final long serialVersionUID = 7628716375283629643L;

    /** Internal data storage. */
    protected char[] buf;
    /** Current size of the buffer. */
    protected int size;
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
        buf = new char[initialCapacity];
    }

    /**
     * Constructor that creates a builder from the string, allocating
     * 32 extra characters for growth.
     *
     * @param str  the string to copy, null treated as blank string
     */
    public StrBuilder(String str) {
        super();
        if (str == null) {
            buf = new char[32];
        } else {
            buf = new char[str.length() + 32];
            append(str);
        }
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
     * @param str  the null text, null means no append
     */
    public void setNullText(String str) {
        if (str != null && str.length() == 0) {
            str = null;
        }
        nullText = str;
    }

    //-----------------------------------------------------------------------
    /**
     * Gets the length of the string builder.
     *
     * @return the length
     */
    public int length() {
        return size;
    }

    /**
     * Updates the length of the builder by either dropping the last characters
     * or adding filler of unicode zero.
     *
     * @param length  the length to set to, must be zero or positive
     * @throws IndexOutOfBoundsException if the length is negative
     */
    public void setLength(int length) {
        if (length < 0) {
            throw new StringIndexOutOfBoundsException(length);
        }
        if (length == size) {
            // ok
        } else if (length < size) {
            size = length;
        } else {
            ensureCapacity(length);
            int oldEnd = size;
            int newEnd = length;
            size = length;
            for (int i = oldEnd; i < newEnd; i++) {
                buf[i] = '\0';
            }
        }
    }

    //-----------------------------------------------------------------------
    /**
     * Gets the current size of the internal character array buffer.
     *
     * @return the capacity
     */
    public int capacity() {
        return buf.length;
    }

    /**
     * Checks the capacity and ensures that it is at least the size specified.
     *
     * @param capacity  the capacity to ensure
     */
    public void ensureCapacity(int capacity) {
        if (capacity > buf.length) {
            char[] old = buf;
            buf = new char[capacity];
            System.arraycopy(old, 0, buf, 0, size);
        }
    }

    /**
     * Minimizes the capacity to the actual length of the string.
     */
    public void minimizeCapacity() {
        if (buf.length > length()) {
            char[] old = buf;
            buf = new char[length()];
            System.arraycopy(old, 0, buf, 0, size);
        }
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
     * This method is the same as {@link #setLength(int)} and is provided to match the
     * API of Collections.
     */
    public void clear() {
        size = 0;
    }

    //-----------------------------------------------------------------------
    /**
     * Gets the character at the specified index.
     *
     * @param index  the index to retrieve, must be valid
     * @return the character at the index
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    public char charAt(int index) {
        if (index < 0 || index >= length()) {
            throw new StringIndexOutOfBoundsException(index);
        }
        return buf[index];
    }

    /**
     * Sets the character at the specified index.
     *
     * @param index  the index to set
     * @param ch  the new character
     * @throws IndexOutOfBoundsException if the index is invalid
     */
    public void setCharAt(int index, char ch) {
        if (index < 0 || index >= length()) {
            throw new StringIndexOutOfBoundsException(index);
        }
        buf[index] = ch;
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
        char chars[] = new char[size];
        System.arraycopy(buf, 0, chars, 0, size);
        return chars;
    }

    /**
     * Copies part of the builder's character array into a new character array.
     * 
     * @param startIndex  the start index, inclusive, must be valid
     * @param endIndex  the end index, exclusive, must be valid except
     *  that if too large it is treated as end of string
     * @return a new array that holds part of the contents of the builder
     * 
     * @throws StringIndexOutOfBoundsException when <code>startIndex</code> is less than 0;
     *                   when <code>startIndex</code> is greater than <code>endIndex</code> (if <code>endIndex</code>
     *                   is larger than {@link #size() }, then it is massaged to equal {@link #size()} before the validation).
     */
    public char[] toCharArray(int startIndex, int endIndex) {
        endIndex = validateRange(startIndex, endIndex);
        int len = endIndex - startIndex;
        if (len == 0) {
            return ArrayUtils.EMPTY_CHAR_ARRAY;
        }
        char chars[] = new char[len];
        System.arraycopy(buf, startIndex, chars, 0, len);
        return chars;
    }

    /**
     * Copies the character array into the specified array.
     * 
     * @param destination  the destination array, null will cause an array to be created
     * @return the input array, unless that was null or too small
     */
    public char[] getChars(char[] destination) {
        int len = length();
        if (destination == null || destination.length < len) {
            destination = new char[len];
        }
        System.arraycopy(buf, 0, destination, 0, len);
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
    public void getChars(int startIndex, int endIndex, char destination[], int destinationIndex) {
        if (startIndex < 0) {
            throw new StringIndexOutOfBoundsException(startIndex);
        }
        if (endIndex < 0 || endIndex > length()) {
            throw new StringIndexOutOfBoundsException(endIndex);
        }
        if (startIndex > endIndex) {
            throw new StringIndexOutOfBoundsException("end < start");
        }
        System.arraycopy(buf, startIndex, destination, destinationIndex, endIndex - startIndex);
    }

    //-----------------------------------------------------------------------
    /**
     * Appends the text representing <code>null</code> to the string builder.
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
     * Appends an object to the string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param obj  the object to append
     * @return this, to enable chaining
     */
    public StrBuilder append(Object obj) {
        if (obj == null) {
            return appendNull();
        } else {
            return append(obj.toString());
        }
    }

    /**
     * Appends a string to the string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str  the string to append
     * @return this, to enable chaining
     */
    public StrBuilder append(String str) {
        if (str == null) {
            return appendNull();
        }
        int strLen = str.length();
        if (strLen > 0) {
            int len = length();
            ensureCapacity(len + strLen);
            str.getChars(0, strLen, buf, len);
            size += strLen;
        }
        return this;
    }

    /**
     * Appends a string buffer to the string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str  the string buffer to append
     * @return this, to enable chaining
     */
    public StrBuilder append(StringBuffer str) {
        if (str == null) {
            return appendNull();
        }
        int strLen = str.length();
        if (strLen > 0) {
            int len = length();
            ensureCapacity(len + strLen);
            str.getChars(0, strLen, buf, len);
            size += strLen;
        }
        return this;
    }

    /**
     * Appends another string builder to the string builder.
     * Appending null will call {@link #appendNull()}.
     *
     * @param str  the string builder to append
     * @return this, to enable chaining
     */
    public StrBuilder append(StrBuilder str) {
        if (str == null) {
            return appendNull();
        }
        int strLen = str.length();
        if (strLen > 0) {
            int len = length();
            ensureCapacity(len + strLen);
            System.arraycopy(str.buf, 0, buf, len, strLen);
            size += strLen;
        }
        return this;
    }

    /**
     * Appends a char array to the string builder.
     * Appending null has no effect.
     * <p>
     * Note: This method treats a null char array as an empty char array,
     * unlike StringBuffer or String.
     *
     * @param chars  the char array to append
     * @return this, to enable chaining
     */
    public StrBuilder append(char[] chars) {
        if (chars == null) {
            return this;
        }
        int strLen = chars.length;
        if (strLen > 0) {
            int len = length();
            ensureCapacity(len + strLen);
            System.arraycopy(chars, 0, buf, len, strLen);
            size += strLen;
        }
        return this;
    }

    /**
     * Appends a char array to the string builder.
     * Appending null has no effect.
     * <p>
     * Note: This method treats a null char array as an empty char array,
     * unlike StringBuffer or String.
     *
     * @param chars  the char array to append
     * @param startIndex  the start index, inclusive, must be valid
     * @param length  the length to append, must be valid
     * @return this, to enable chaining
     */
    public StrBuilder append(char[] chars, int startIndex, int length) {
        if (chars == null) {
            return this;
        }
        if (startIndex < 0 || startIndex > chars.length) {
            throw new StringIndexOutOfBoundsException("startIndex must be valid");
        }
        if (length < 0) {
            throw new StringIndexOutOfBoundsException("length must not be negative");
        }
        if (length > 0) {
            int len = length();
            ensureCapacity(len + length);
            System.arraycopy(chars, startIndex, buf, len, length);
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
    public StrBuilder append(boolean value) {
        if (value) {
            ensureCapacity(size + 4);
            buf[size++] = 't';
            buf[size++] = 'r';
            buf[size++] = 'u';
            buf[size++] = 'e';
        } else {
            ensureCapacity(size + 5);
            buf[size++] = 'f';
            buf[size++] = 'a';
            buf[size++] = 'l';
            buf[size++] = 's';
            buf[size++] = 'e';
        }
        return this;
    }

    /**
     * Appends a char value to the string builder.
     *
     * @param ch  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder append(char ch) {
        int len = length();
        ensureCapacity(len + 1);
        buf[size++] = ch;
        return this;
    }

    /**
     * Appends an int value to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder append(int value) {
        return append(String.valueOf(value));
    }

    /**
     * Appends a long value to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder append(long value) {
        return append(String.valueOf(value));
    }

    /**
     * Appends a float value to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder append(float value) {
        return append(String.valueOf(value));
    }

    /**
     * Appends a double value to the string builder using <code>String.valueOf</code>.
     *
     * @param value  the value to append
     * @return this, to enable chaining
     */
    public StrBuilder append(double value) {
        return append(String.valueOf(value));
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
    public StrBuilder appendWithSeparators(Object[] array, String separator) {
        if (array != null && array.length > 0) {
            separator = (separator == null ? "" : separator);
            append(array[0]);
            for (int i = 1; i < array.length; i++) {
                append(separator);
                append(array[i]);
            }
        }
        return this;
    }

    /**
     * Appends a collection placing separators between each value, but
     * not before the first or after the last.
     * Appending a null collection will have no effect.
     * Each object is appended using {@link #append(Object)}.
     *
     * @param coll  the collection to append
     * @param separator  the separator to use, null means no separator
     * @return this, to enable chaining
     */
    public StrBuilder appendWithSeparators(Collection coll, String separator) {
        if (coll != null && coll.size() > 0) {
            separator = (separator == null ? "" : separator);
            Iterator it = coll.iterator();
            while (it.hasNext()) {
                append(it.next());
                if (it.hasNext()) {
                    append(separator);
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
    public StrBuilder appendWithSeparators(Iterator it, String separator) {
        if (it != null) {
            separator = (separator == null ? "" : separator);
            while (it.hasNext()) {
                append(it.next());
                if (it.hasNext()) {
                    append(separator);
                }
            }
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
    public StrBuilder appendPadding(int length, char padChar) {
        if (length >= 0) {
            ensureCapacity(size + length);
            for (int i = 0; i < length; i++) {
                buf[size++] = padChar;
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
    public StrBuilder appendFixedWidthPadLeft(Object obj, int width, char padChar) {
        if (width > 0) {
            ensureCapacity(size + width);
            String str = (obj == null ? getNullText() : obj.toString());
            int strLen = str.length();
            if (strLen >= width) {
                str.getChars(strLen - width, strLen, buf, size);
            } else {
                int padLen = width - strLen;
                for (int i = 0; i < padLen; i++) {
                    buf[size + i] = padChar;
                }
                str.getChars(0, strLen, buf, size + padLen);
            }
            size += width;
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
    public StrBuilder appendFixedWidthPadLeft(int value, int width, char padChar) {
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
    public StrBuilder appendFixedWidthPadRight(Object obj, int width, char padChar) {
        if (width > 0) {
            ensureCapacity(size + width);
            String str = (obj == null ? getNullText() : obj.toString());
            int strLen = str.length();
            if (strLen >= width) {
                str.getChars(0, strLen, buf, size);
            } else {
                int padLen = width - strLen;
                str.getChars(0, strLen, buf, size);
                for (int i = 0; i < padLen; i++) {
                    buf[size + strLen + i] = padChar;
                }
            }
            size += width;
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
    public StrBuilder appendFixedWidthPadRight(int value, int width, char padChar) {
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
     * @throws StringIndexOutOfBoundsException if the index is invalid
     */
    public StrBuilder insert(int index, Object obj) {
        if (obj == null) {
            return insert(index, nullText);
        } else {
            return insert(index, obj.toString());
        }
    }

    /**
     * Inserts the string into this builder.
     * Inserting null will use the stored null text value.
     *
     * @param index  the index to add at, must be valid
     * @param str  the string to insert
     * @return this, to enable chaining
     * @throws StringIndexOutOfBoundsException if the index is invalid
     */
    public StrBuilder insert(int index, String str) {
        validateIndex(index);
        if (str == null) {
            str = nullText;
        }
        int strLen = (str == null ? 0 : str.length());
        if (strLen > 0) {
            int newSize = size + strLen;
            ensureCapacity(newSize);
            System.arraycopy(buf, index, buf, index + strLen, size - index);
            size = newSize;
            str.getChars(0, strLen, buf, index);
        }
        return this;
    }

    /**
     * Inserts the character array into this builder.
     * Inserting null has no effect.
     * <p>
     * Note: This method treats a null char array as an empty char array,
     * unlike StringBuffer or String.
     *
     * @param index  the index to add at, must be valid
     * @param chars  the char array to insert
     * @return this, to enable chaining
     * @throws StringIndexOutOfBoundsException if the index is invalid
     */
    public StrBuilder insert(int index, char chars[]) {
        validateIndex(index);
        if (chars == null) {
            return this;
        }
        int len = chars.length;
        if (len > 0) {
            ensureCapacity(size + len);
            System.arraycopy(buf, index, buf, index + len, size - index);
            System.arraycopy(chars, 0, buf, index, len);
            size += len;
        }
        return this;
    }

    /**
     * Inserts part of the character array into this builder.
     * Inserting null has no effect.
     * <p>
     * Note: This method treats a null char array as an empty char array,
     * unlike StringBuffer or String.
     *
     * @param index  the index to add at, must be valid
     * @param chars  the char array to insert
     * @param offset  the offset into the character array to start at, must be valid
     * @param length  the length of the character array part to copy, must be positive
     * @return this, to enable chaining
     * @throws StringIndexOutOfBoundsException if any index is invalid
     */
    public StrBuilder insert(int index, char chars[], int offset, int length) {
        validateIndex(index);
        if (chars == null) {
            return this;
        }
        if (offset < 0 || offset > chars.length) {
            throw new StringIndexOutOfBoundsException("Invalid offset: " + offset);
        }
        if (length < 0 || offset + length > chars.length) {
            throw new StringIndexOutOfBoundsException("Invalid length: " + length);
        }
        if (length > 0) {
            ensureCapacity(size + length);
            System.arraycopy(buf, index, buf, index + length, size - index);
            System.arraycopy(chars, offset, buf, index, length);
            size += length;
        }
        return this;
    }

    /**
     * Inserts the value into this builder.
     *
     * @param index  the index to add at, must be valid
     * @param value  the value to insert
     * @return this, to enable chaining
     * @throws StringIndexOutOfBoundsException if the index is invalid
     */
    public StrBuilder insert(int index, boolean value) {
        validateIndex(index);
        if (value) {
            ensureCapacity(size + 4);
            System.arraycopy(buf, index, buf, index + 4, size - index);
            buf[index++] = 't';
            buf[index++] = 'r';
            buf[index++] = 'u';
            buf[index] = 'e';
            size += 4;
        } else {
            ensureCapacity(size + 5);
            System.arraycopy(buf, index, buf, index + 5, size - index);
            buf[index++] = 'f';
            buf[index++] = 'a';
            buf[index++] = 'l';
            buf[index++] = 's';
            buf[index] = 'e';
            size += 5;
        }
        return this;
    }

    /**
     * Inserts the value into this builder.
     *
     * @param index  the index to add at, must be valid
     * @param value  the value to insert
     * @return this, to enable chaining
     * @throws StringIndexOutOfBoundsException if the index is invalid
     */
    public StrBuilder insert(int index, char value) {
        validateIndex(index);
        ensureCapacity(size + 1);
        System.arraycopy(buf, index, buf, index + 1, size - index);
        buf[index] = value;
        size++;
        return this;
    }

    /**
     * Inserts the value into this builder.
     *
     * @param index  the index to add at, must be valid
     * @param value  the value to insert
     * @return this, to enable chaining
     * @throws StringIndexOutOfBoundsException if the index is invalid
     */
    public StrBuilder insert(int index, int value) {
        return insert(index, String.valueOf(value));
    }

    /**
     * Inserts the value into this builder.
     *
     * @param index  the index to add at, must be valid
     * @param value  the value to insert
     * @return this, to enable chaining
     * @throws StringIndexOutOfBoundsException if the index is invalid
     */
    public StrBuilder insert(int index, long value) {
        return insert(index, String.valueOf(value));
    }

    /**
     * Inserts the value into this builder.
     *
     * @param index  the index to add at, must be valid
     * @param value  the value to insert
     * @return this, to enable chaining
     * @throws StringIndexOutOfBoundsException if the index is invalid
     */
    public StrBuilder insert(int index, float value) {
        return insert(index, String.valueOf(value));
    }

    /**
     * Inserts the value into this builder.
     *
     * @param index  the index to add at, must be valid
     * @param value  the value to insert
     * @return this, to enable chaining
     * @throws StringIndexOutOfBoundsException if the index is invalid
     */
    public StrBuilder insert(int index, double value) {
        return insert(index, String.valueOf(value));
    }

    //-----------------------------------------------------------------------
    /**
     * Deletes the characters between the two specified indices.
     *
     * @param startIndex  the start index, inclusive, must be valid
     * @param endIndex  the end index, exclusive, must be valid except
     *  that if too large it is treated as end of string
     * @return this, to enable chaining
     * @throws StringIndexOutOfBoundsException if the index is invalid
     */
    public StrBuilder delete(int startIndex, int endIndex) {
        endIndex = validateRange(startIndex, endIndex);
        int len = endIndex - startIndex;
        if (len > 0) {
            System.arraycopy(buf, endIndex, buf, startIndex, size - endIndex);
            size -= len;
        }
        return this;
    }

    /**
     * Deletes the character at the specified index.
     *
     * @param index  the index to delete
     * @return this, to enable chaining
     * @throws StringIndexOutOfBoundsException if the index is invalid
     */
    public StrBuilder deleteCharAt(int index) {
        if (index < 0 || index >= size) {
            throw new StringIndexOutOfBoundsException(index);
        }
        System.arraycopy(buf, index + 1, buf, index, size - index - 1);
        size--;
        return this;
    }

    /**
     * Deletes the character wherever it occurs in the builder.
     * 
     * @param ch  the character to delete
     * @return this, to enable chaining
     */
    public StrBuilder delete(char ch) {
        for (int i = 0; i < size; i++) {
            if (buf[i] == ch) {
                int start = i;
                while (++i < size) {
                    if (buf[i] != ch) {
                        break;
                    }
                }
                System.arraycopy(buf, i, buf, start, size - i);
                size -= (i - start);
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
    public StrBuilder delete(String str) {
        int len = (str == null ? 0 : str.length());
        if (len > 0) {
            int index = indexOf(str, 0);
            while (index >= 0) {
                delete(index, index + len);
                index = indexOf(str, index);
            }
        }
        return this;
    }

    //-----------------------------------------------------------------------
    /**
     * Replaces a portion of the string builder with another string.
     * The length of the inserted string does not have to match the removed length.
     * 
     * @param startIndex  the start index, inclusive, must be valid
     * @param endIndex  the end index, exclusive, must be valid except
     *  that if too large it is treated as end of string
     * @param str  the string to replace with
     * @return this, to enable chaining
     * @throws StringIndexOutOfBoundsException if the index is invalid
     */
    public StrBuilder replace(int startIndex, int endIndex, String str) {
        endIndex = validateRange(startIndex, endIndex);
        int insertLen = str.length();
        int removeLen = endIndex = startIndex;
        int newSize = size - removeLen + insertLen;
        if (insertLen > removeLen) {
            ensureCapacity(newSize);
        }
        if (insertLen != removeLen) {
            System.arraycopy(buf, endIndex, buf, startIndex + insertLen, size - endIndex);
            size = newSize;
        }
        str.getChars(0, insertLen, buf, startIndex);
        return this;
    }

    /**
     * Replaces a portion of the string builder with another string builder.
     * The length of the inserted string does not have to match the removed length.
     * 
     * @param startIndex  the start index, inclusive, must be valid
     * @param endIndex  the end index, exclusive, must be valid except
     *  that if too large it is treated as end of string
     * @param builder  the string builder to replace with
     * @return this, to enable chaining
     * @throws StringIndexOutOfBoundsException if the index is invalid
     */
    public StrBuilder replace(int startIndex, int endIndex, StrBuilder builder) {
        endIndex = validateRange(startIndex, endIndex);
        int insertLen = builder.length();
        int removeLen = endIndex = startIndex;
        if (insertLen > removeLen) {
            ensureCapacity(size - removeLen + insertLen);
        }
        if (insertLen != removeLen) {
            System.arraycopy(buf, endIndex, buf, startIndex + insertLen, size - endIndex);
        }
        builder.getChars(0, insertLen, buf, startIndex);
        return this;
    }

    /**
     * Replaces the search character with the replace character throughout the builder.
     * 
     * @param search  the search string, null causes no action to occur
     * @param replace  the replace string, null is equivalent to an empty string
     * @return this, to enable chaining
     */
    public StrBuilder replace(char search, char replace) {
        if (search != replace) {
            for (int i = 0; i < size; i++) {
                if (buf[i] == search) {
                    buf[i] = replace;
                }
            }
        }
        return this;
    }

    /**
     * Replaces the search string with the replace string throughout the builder.
     * 
     * @param searchStr  the search string, null causes no action to occur
     * @param replaceStr  the replace string, null is equivalent to an empty string
     * @return this, to enable chaining
     */
    public StrBuilder replace(String searchStr, String replaceStr) {
        int searchLen = (searchStr == null ? 0 : searchStr.length());
        if (searchLen > 0) {
            replaceStr = (replaceStr == null ? "" : replaceStr);
            int index = indexOf(searchStr, 0);
            while (index >= 0) {
                replace(index, index + searchLen, replaceStr);
                index = indexOf(searchStr, index);
            }
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
    public boolean startsWith(String str) {
        if (str == null) {
            return false;
        }
        int len = str.length();
        if (len == 0) {
            return true;
        }
        if (len > size) {
            return false;
        }
        for (int i = 0; i < len; i++) {
            if (buf[i] != str.charAt(i)) {
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
    public boolean endsWith(String str) {
        if (str == null) {
            return false;
        }
        int len = str.length();
        if (len == 0) {
            return true;
        }
        if (len > size) {
            return false;
        }
        int pos = size - len;
        for (int i = 0; i < len; i++,pos++) {
            if (buf[pos] != str.charAt(i)) {
                return false;
            }
        }
        return true;
    }

    //-----------------------------------------------------------------------
    /**
     * Extracts a portion of this string builder as a string.
     * 
     * @param start  the start index, inclusive, must be valid
     * @return the new string
     * @throws StringIndexOutOfBoundsException if the index is invalid
     */
    public String substring(int start) {
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
     * @throws StringIndexOutOfBoundsException if the index is invalid
     */
    public String substring(int startIndex, int endIndex) {
        endIndex = validateRange(startIndex, endIndex);
        return new String(buf, startIndex, endIndex - startIndex);
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
    public String leftString(int length) {
        if (length <= 0) {
            return "";
        } else if (length >= size) {
            return new String(buf, 0, size);
        } else {
            return new String(buf, 0, length);
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
    public String rightString(int length) {
        if (length <= 0) {
            return "";
        } else if (length >= size) {
            return new String(buf, 0, size);
        } else {
            return new String(buf, size - length, size);
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
    public String midString(int index, int length) {
        if (index < 0) {
            index = 0;
        }
        if (length <= 0 || index >= size) {
            return "";
        }
        if (size <= index + length) {
            return new String(buf, index, size - index);
        } else {
            return new String(buf, index, length);
        }
    }

    //-----------------------------------------------------------------------
    /**
     * Checks of the string builder contains the specified char.
     * 
     * @param ch  the character to find
     * @return true if the builder contains the character
     */
    public boolean contains(char ch) {
        char[] thisBuf = buf;
        for (int i = 0; i < thisBuf.length; i++) {
            if (thisBuf[i] == ch) {
                return true;
            }
        }
        return false;
    }

    /**
     * Checks of the string builder contains the specified string.
     * 
     * @param str  the string to find
     * @return true if the builder contains the string
     */
    public boolean contains(String str) {
        return indexOf(str, 0) >= 0;
    }

    //-----------------------------------------------------------------------
    /**
     * Searches the string builder to find the first reference to the specified char.
     * 
     * @param ch  the character to find
     * @return the first index of the character, or -1 if not found
     */
    public int indexOf(char ch) {
        return indexOf(ch, 0);
    }

    /**
     * Searches the string builder to find the first reference to the specified char.
     * 
     * @param ch  the character to find
     * @param startIndex  the index to start at, must be valid
     * @return the first index of the character, or -1 if not found
     */
    public int indexOf(char ch, int startIndex) {
        startIndex = (startIndex < 0 ? 0 : startIndex);
        if (startIndex >= size) {
            return -1;
        }
        char[] thisBuf = buf;
        for (int i = startIndex; i < thisBuf.length; i++) {
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
    public int indexOf(String str) {
        return indexOf(str, 0);
    }

    /**
     * Searches the string builder to find the first reference to the specified
     * string starting searching from the given index.
     * <p>
     * Note that a null input string will return -1, whereas the JDK throws an exception.
     * 
     * @param str  the string to find, null returns -1
     * @param startIndex  the index to start at, must be valid
     * @return the first index of the string, or -1 if not found
     */
    public int indexOf(String str, int startIndex) {
        startIndex = (startIndex < 0 ? 0 : startIndex);
        if (startIndex >= size) {
            return -1;
        }
        int strLen = (str == null ? 0 : str.length());
        if (strLen > 0 && strLen <= size) {
            if (strLen == 1) {
                return indexOf(str.charAt(0), startIndex);
            }
            char[] thisBuf = buf;
            outer:
            for (int i = startIndex; i < thisBuf.length - strLen; i++) {
                for (int j = 0; j < strLen; j++) {
                    if (str.charAt(j) != thisBuf[i + j]) {
                        continue outer;
                    }
                }
                return i;
            }
            
        } else if (strLen == 0) {
            return 0;
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
    public int lastIndexOf(char ch) {
        return lastIndexOf(ch, 0);
    }

    /**
     * Searches the string builder to find the last reference to the specified char.
     * 
     * @param ch  the character to find
     * @param startIndex  the index to start at, invalid index rounded to edge
     * @return the last index of the character, or -1 if not found
     */
    public int lastIndexOf(char ch, int startIndex) {
        startIndex = (startIndex >= size ? size - 1 : startIndex);
        if (startIndex < 0) {
            return -1;
        }
        char[] thisBuf = buf;
        for (int i = startIndex; i >= 0; i--) {
            if (thisBuf[i] == ch) {
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
    public int lastIndexOf(String str) {
        return lastIndexOf(str, size);
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
    public int lastIndexOf(String str, int startIndex) {
        startIndex = (startIndex >= size ? size - 1 : startIndex);
        if (startIndex < 0) {
            return -1;
        }
        int strLen = (str == null ? 0 : str.length());
        if (strLen > 0 && strLen <= size) {
            if (strLen == 1) {
                return lastIndexOf(str.charAt(0), startIndex);
            }
            char[] thisBuf = buf;
            outer:
            for (int i = startIndex - strLen; i >= 0; i--) {
                for (int j = 0; j < strLen; j++) {
                    if (str.charAt(j) != thisBuf[i + j]) {
                        continue outer;
                    }
                }
                return i;
            }
            
        } else if (strLen == 0) {
            return startIndex;
        }
        return -1;
    }

    /**
     * Reverses the string builder placing each character in the opposite index.
     * 
     * @return this, to enable chaining
     */
    public StrBuilder reverse() {
        int half = size / 2;
        char swap;
        for (int i = 0; i < half; i++) {
            swap = buf[i];
            buf[i] = buf[size - i];
            buf[size - i] = swap;
        }
        return this;
    }

//    /**
//     * Gets a String version of the string builder by calling the internal
//     * constructor of String by reflection.
//     * <p>
//     * WARNING: You must not use the StrBuilder after calling this method
//     * as the buffer is now shared with the String object. To ensure this,
//     * the internal character array is set to null, so you will get
//     * NullPointerExceptions on all method calls.
//     *
//     * @return the builder as a String
//     */
//    public String toSharedString() {
//        try {
//            Constructor con = String.class.getDeclaredConstructor(
//                new Class[] {int.class, int.class, char[].class});
//            con.setAccessible(true);
//            char[] buffer = buf;
//            buf = null;
//            size = -1;
//            nullText = null;
//            return (String) con.newInstance(
//                new Object[] {new Integer(0), new Integer(size), buffer});
//            
//        } catch (Exception ex) {
//            ex.printStackTrace();
//            throw new UnsupportedOperationException("StrBuilder.toSharedString is unsupported: " + ex.getMessage());
//        }
//    }

    /**
     * Gets a String version of the string builder, creating a new instance
     * each time the method is called.
     * <p>
     * Note that unlike StringBuffer, the string version returned is
     * independent of the string builder.
     *
     * @return the builder as a String
     */
    public String toString() {
        return new String(buf, 0, size);
    }

    /**
     * Gets a StringBuffer version of the string builder, creating a
     * new instance each time the method is called.
     *
     * @return the builder as a StringBuffer
     */
    public StringBuffer toStringBuffer() {
        return new StringBuffer(size).append(buf, 0, size);
    }

    //-----------------------------------------------------------------------
    /**
     * Validates parameters defining a range of the builder.
     * 
     * @param startIndex  the start index, inclusive, must be valid
     * @param endIndex  the end index, exclusive, must be valid except
     *  that if too large it is treated as end of string
     * @return the new string
     * @throws StringIndexOutOfBoundsException if the index is invalid
     */
    protected int validateRange(int startIndex, int endIndex) {
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
     * @throws StringIndexOutOfBoundsException if the index is invalid
     */
    protected void validateIndex(int index) {
        if (index < 0 || index > size) {
            throw new StringIndexOutOfBoundsException(index);
        }
    }

}
