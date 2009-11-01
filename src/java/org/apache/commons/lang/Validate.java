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

import java.util.Collection;
import java.util.Iterator;
import java.util.Map;

/**
 * <p>Assists in validating arguments.</p>
 * 
 * <p>The class is based along the lines of JUnit. If an argument value is 
 * deemed invalid, an IllegalArgumentException is thrown. For example:</p>
 * 
 * <pre>
 * Validate.isTrue( i > 0, "The value must be greater than zero: ", i);
 * Validate.notNull( surname, "The surname must not be null");
 * </pre>
 *
 * @author Apache Software Foundation
 * @author <a href="mailto:ola.berg@arkitema.se">Ola Berg</a>
 * @author Gary Gregory
 * @author Norm Deane
 * @since 2.0
 * @version $Id$
 */
public class Validate {

    /**
     * Constructor. This class should not normally be instantiated.
     */
    public Validate() {
      super();
    }

    // isTrue
    //---------------------------------------------------------------------------------

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code>
     * if the test result is <code>false</code>.</p>
     * 
     * <p>This is used when validating according to an arbitrary boolean expression,
     * such as validating a primitive number or using your own custom validation 
     * expression.</p>
     *
     * <pre>
     * Validate.isTrue( myObject.isOk(), "The object is not OK: ", myObject);
     * </pre>
     *
     * <p>For performance reasons, the object is passed as a separate parameter and
     * appended to the message string only in the case of an error.</p>
     * 
     * @param expression  a boolean expression
     * @param message  the exception message you would like to see if the
     *  expression is <code>false</code>
     * @param value  the value to append to the message in case of error
     * @throws IllegalArgumentException if expression is <code>false</code>
     */
    public static void isTrue(boolean expression, String message, Object value) {
        if (expression == false) {
            throw new IllegalArgumentException(message + value);
        }
    }

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code>
     * if the test result is <code>false</code>.</p>
     * 
     * <p>This is used when validating according to an arbitrary boolean expression,
     * such as validating a primitive number or using your own custom validation 
     * expression.</p>
     *
     * <pre>
     * Validate.isTrue( i > 0, "The value must be greater than zero: ", i);
     * </pre>
     *
     * <p>For performance reasons, the long value is passed as a separate parameter and
     * appended to the message string only in the case of an error.</p>
     * 
     * @param expression  a boolean expression
     * @param message  the exception message you would like to see if the expression is <code>false</code>
     * @param value  the value to append to the message in case of error
     * @throws IllegalArgumentException if expression is <code>false</code>
     */
    public static void isTrue(boolean expression, String message, long value) {
        if (expression == false) {
            throw new IllegalArgumentException(message + value);
        }
    }

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code>
     * if the test result is <code>false</code>.</p>
     * 
     * <p>This is used when validating according to an arbitrary boolean expression,
     * such as validating a primitive number or using your own custom validation 
     * expression.</p>
     *
     * <pre>
     * Validate.isTrue( d > 0.0, "The value must be greater than zero: ", d);
     * </pre>
     *
     * <p>For performance reasons, the double value is passed as a separate parameter and
     * appended to the message string only in the case of an error.</p>
     * 
     * @param expression  a boolean expression
     * @param message  the exception message you would like to see if the expression
     *  is <code>false</code>
     * @param value  the value to append to the message in case of error
     * @throws IllegalArgumentException if expression is <code>false</code>
     */
    public static void isTrue(boolean expression, String message, double value) {
        if (expression == false) {
            throw new IllegalArgumentException(message + value);
        }
    }

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code>
     * if the test result is <code>false</code>.</p>
     * 
     * <p>This is used when validating according to an arbitrary boolean expression,
     * such as validating a primitive number or using your own custom validation 
     * expression.</p>
     *
     * <pre>
     * Validate.isTrue( (i > 0), "The value must be greater than zero");
     * Validate.isTrue( myObject.isOk(), "The object is not OK");
     * </pre>
     *
     * <p>For performance reasons, the message string should not involve a string append,
     * instead use the {@link #isTrue(boolean, String, Object)} method.</p>
     * 
     * @param expression  a boolean expression
     * @param message  the exception message you would like to see if the expression
     *  is <code>false</code>
     * @throws IllegalArgumentException if expression is <code>false</code>
     */
    public static void isTrue(boolean expression, String message) {
        if (expression == false) {
            throw new IllegalArgumentException(message);
        }
    }

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code>
     * if the test result is <code>false</code>.</p>
     * 
     * <p>This is used when validating according to an arbitrary boolean expression,
     * such as validating a primitive number or using your own custom validation 
     * expression.</p>
     *
     * <pre>
     * Validate.isTrue( i > 0 );
     * Validate.isTrue( myObject.isOk() );
     * </pre>
     *
     * <p>The message in the exception is 'The validated expression is false'.</p>
     * 
     * @param expression  a boolean expression
     * @throws IllegalArgumentException if expression is <code>false</code>
     */
    public static void isTrue(boolean expression) {
        if (expression == false) {
            throw new IllegalArgumentException("The validated expression is false");
        }
    }

    // notNull
    //---------------------------------------------------------------------------------

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code>
     * if the argument is <code>null</code>.</p>
     *
     * <pre>
     * Validate.notNull(myObject, "The object must not be null");
     * </pre>
     * 
     * @param object  the object to check is not <code>null</code>
     * @param message  the exception message you would like to see
     *  if the object is <code>null</code>
     * @return the input object, never <code>null</code>, for chaining
     * @throws IllegalArgumentException if the object is <code>null</code>
     */
    public static <T> T notNull(T object, String message) {
        if (object == null) {
            throw new IllegalArgumentException(message);
        }
        return object;
    }

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code>
     * if the argument is <code>null</code>.</p>
     *
     * <pre>
     * Validate.notNull(myObject);
     * </pre>
     *
     * <p>The message in the exception is 'The validated object is null'.</p>
     * 
     * @param object  the object to check is not <code>null</code>
     * @return the input object, never <code>null</code>, for chaining
     * @throws IllegalArgumentException if the object is <code>null</code>
     */
    public static <T> T notNull(T object) {
        return notNull(object, "The validated object is null");
    }

    // notEmpty array
    //---------------------------------------------------------------------------------

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code>
     * if the argument array is empty (<code>null</code> or no elements).</p>
     *
     * <pre>
     * Validate.notEmpty(myArray, "The array must not be empty");
     * </pre>
     * 
     * @param array  the array to check is not empty
     * @param message  the exception message you would like to see if the array is empty
     * @return the input array, never <code>null</code> or empty, for chaining
     * @throws IllegalArgumentException if the array is empty
     */
    public static <T> T[] notEmpty(T[] array, String message) {
        if (array == null || array.length == 0) {
            throw new IllegalArgumentException(message);
        }
        return array;
    }

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code>
     * if the argument array is empty (<code>null</code> or no elements).</p>
     *
     * <pre>
     * Validate.notEmpty(myArray);
     * </pre>
     *
     * <p>The message in the exception is 'The validated array is empty'.
     * 
     * @param array  the array to check is not empty
     * @return the input array, never <code>null</code> or empty, for chaining
     * @throws IllegalArgumentException if the array is empty
     */
    public static <T> T[] notEmpty(T[] array) {
        return notEmpty(array, "The validated array is empty");
    }

    // notEmpty collection
    //---------------------------------------------------------------------------------

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code>
     * if the argument Collection is empty (<code>null</code> or no elements).</p>
     *
     * <pre>
     * Validate.notEmpty(myCollection, "The collection must not be empty");
     * </pre>
     * 
     * @param collection  the collection to check is not empty
     * @param message  the exception message you would like to see if the collection is empty
     * @return the input collection, never <code>null</code> or empty, for chaining
     * @throws IllegalArgumentException if the collection is empty
     */
    public static <T extends Collection<?>> T notEmpty(T collection, String message) {
        if (collection == null || collection.size() == 0) {
            throw new IllegalArgumentException(message);
        }
        return collection;
    }

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code>
     * if the argument Collection is empty (<code>null</code> or no elements).</p>
     *
     * <pre>
     * Validate.notEmpty(myCollection);
     * </pre>
     *
     * <p>The message in the exception is 'The validated collection is empty'.</p>
     * 
     * @param collection  the collection to check is not empty
     * @return the input collection, never <code>null</code> or empty, for chaining
     * @throws IllegalArgumentException if the collection is empty
     */
    public static <T extends Collection<?>> T notEmpty(T collection) {
        return notEmpty(collection, "The validated collection is empty");
    }

    // notEmpty map
    //---------------------------------------------------------------------------------

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code>
     * if the argument Map is empty (<code>null</code> or no elements).</p>
     *
     * <pre>
     * Validate.notEmpty(myMap, "The map must not be empty");
     * </pre>
     * 
     * @param map  the map to check is not empty
     * @param message  the exception message you would like to see if the map is empty
     * @return the input map, never <code>null</code> or empty, for chaining
     * @throws IllegalArgumentException if the map is empty
     */
    public static <T extends Map<?, ?>> T notEmpty(T map, String message) {
        if (map == null || map.size() == 0) {
            throw new IllegalArgumentException(message);
        }
        return map;
    }

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code>
     * if the argument Map is empty (<code>null</code> or no elements).</p>
     *
     * <pre>
     * Validate.notEmpty(myMap);
     * </pre>
     *
     * <p>The message in the exception is 'The validated map is empty'.</p>
     * 
     * @param map  the map to check is not empty
     * @return the input map, never <code>null</code> or empty, for chaining
     * @throws IllegalArgumentException if the map is empty
     */
    public static <T extends Map<?, ?>> T notEmpty(T map) {
        return notEmpty(map, "The validated map is empty");
    }

    // notEmpty string
    //---------------------------------------------------------------------------------

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code>
     * if the argument String is empty (<code>null</code> or zero length).</p>
     *
     * <pre>
     * Validate.notEmpty(myString, "The string must not be empty");
     * </pre>
     * 
     * @param string  the string to check is not empty
     * @param message  the exception message you would like to see if the string is empty
     * @return the input string, never <code>null</code> or empty, for chaining
     * @throws IllegalArgumentException if the string is empty
     */
    public static <T extends CharSequence> T notEmpty(T string, String message) {
        if (string == null || string.length() == 0) {
            throw new IllegalArgumentException(message);
        }
        return string;
    }

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code>
     * if the argument String is empty (<code>null</code> or zero length).</p>
     *
     * <pre>
     * Validate.notEmpty(myString);
     * </pre>
     *
     * <p>The message in the exception is 'The validated string is empty'.</p>
     * 
     * @param string  the string to check is not empty
     * @return the input string, never <code>null</code> or empty, for chaining
     * @throws IllegalArgumentException if the string is empty
     */
    public static <T extends CharSequence> T notEmpty(T string) {
        return notEmpty(string, "The validated string is empty");
    }

    // notBlank string
    //---------------------------------------------------------------------------------

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code>
     * if the argument String is blank (<code>null</code>, empty or whitespace).</p>
     *
     * <pre>
     * Validate.notBlank(myString, "The string must not be blank");
     * </pre>
     *
     * @param string  the string to check is not blank
     * @param message  the exception message you would like to see if the string is blank
     * @return the input string, never <code>null</code> or blank, for chaining
     * @throws IllegalArgumentException if the string is blank
     * @see StringUtils#isBlank(CharSequence)
     */
    public static <T extends CharSequence> T notBlank(T string, String message) {
        if (StringUtils.isBlank(string)) {
            throw new IllegalArgumentException(message);
        }
        return string;
    }

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code>
     * if the argument String is blank (<code>null</code>, empty or whitespace).</p>
     *
     * <pre>
     * Validate.notBlank(myString);
     * </pre>
     *
     * <p>The message in the exception is 'The validated string is blank'.</p>
     *
     * @param string  the string to check is not blank
     * @return the input string, never <code>null</code> or blank, for chaining
     * @throws IllegalArgumentException if the string is blank
     * @see StringUtils#isBlank(CharSequence)
     */
    public static <T extends CharSequence> T notBlank(T string) {
        return notBlank(string, "The validated string is blank");
    }

    // notNullElements array
    //---------------------------------------------------------------------------------

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code>
     * if the argument array has <code>null</code> elements or is
     * <code>null</code>.</p>
     *
     * <pre>
     * Validate.noNullElements(myArray, "The array must not contain null elements");
     * </pre>
     * 
     * <p>If the array is null then the message in the exception is 'The validated object is null'.</p>
     *
     * @param array  the array to check
     * @param message  the exception message if the array has
     *  <code>null</code> elements
     * @return the validated input array, never <code>null</code>, for chaining
     * @throws IllegalArgumentException if the array has <code>null</code>
     *  elements or is <code>null</code>
     */
    public static <T> T[] noNullElements(T[] array, String message) {
        Validate.notNull(array);
        for (int i = 0; i < array.length; i++) {
            if (array[i] == null) {
                throw new IllegalArgumentException(message);
            }
        }
        return array;
    }

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code>
     * if the argument array has <code>null</code> elements or is
     * <code>null</code>.</p>
     *
     * <pre>
     * Validate.noNullElements(myArray);
     * </pre>
     *
     * <p>If the array has a null element the message in the exception is
     * 'The validated array contains null element at index: '.</p>
     *
     * <p>If the array is null then the message in the exception is 'The validated object is null'.</p>
     * 
     * @param array  the array to check
     * @return the validated input array, never <code>null</code>, for chaining
     * @throws IllegalArgumentException if the array has <code>null</code>
     *  elements or is <code>null</code>
     */
    public static <T> T[] noNullElements(T[] array) {
        Validate.notNull(array);
        for (int i = 0; i < array.length; i++) {
            if (array[i] == null) {
                throw new IllegalArgumentException("The validated array contains null element at index: " + i);
            }
        }
        return array;
    }

    // notNullElements collection
    //---------------------------------------------------------------------------------

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code>
     * if the argument Collection has <code>null</code> elements or is
     * <code>null</code>.</p>
     *
     * <pre>
     * Validate.noNullElements(myCollection, "The collection must not contain null elements");
     * </pre>
     *
     * <p>If the collection is null then the message in the exception is 'The validated object is null'.</p>
     * 
     * @param collection  the collection to check
     * @param message  the exception message if the collection has
     *  <code>null</code> elements
     * @return the validated input collection, never <code>null</code>, for chaining
     * @throws IllegalArgumentException if the collection has
     *  <code>null</code> elements or is <code>null</code>
     */
    public static <T extends Collection<?>> T noNullElements(T collection, String message) {
        Validate.notNull(collection);
        for (Iterator<?> it = collection.iterator(); it.hasNext();) {
            if (it.next() == null) {
                throw new IllegalArgumentException(message);
            }
        }
        return collection;
    }

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code>
     * if the argument Collection has <code>null</code> elements or is
     * <code>null</code>.</p>
     *
     * <pre>
     * Validate.noNullElements(myCollection);
     * </pre>
     *
     * <p>The message in the exception is 'The validated collection contains null element at index: '.</p>
     *
     * <p>If the collection is null then the message in the exception is 'The validated object is null'.</p>
     * 
     * @param collection  the collection to check
     * @return the validated input collection, never <code>null</code>, for chaining
     * @throws IllegalArgumentException if the collection has
     *  <code>null</code> elements or is <code>null</code>
     */
    public static <T extends Collection<?>> T noNullElements(T collection) {
        Validate.notNull(collection);
        int i = 0;
        for (Iterator<?> it = collection.iterator(); it.hasNext(); i++) {
            if (it.next() == null) {
                throw new IllegalArgumentException("The validated collection contains null element at index: " + i);
            }
        }
        return collection;
    }

    // validIndex array
    //---------------------------------------------------------------------------------

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code> if the
     * index for the argument array is invalid or if the array is <code>null</code>.</p>
     *
     * <pre>
     * Validate.validIndex(myArray, 2, "The array index is invalid: ");
     * </pre>
     * 
     * <p>If the array is null then the message in the exception is 'The validated object is null'.</p>
     *
     * @param array  the array to check, not null
     * @param message  the exception message if the array index is invalid
     * @return the validated input array, never <code>null</code>, for chaining
     * @throws IllegalArgumentException if the array index is invalid or null
     */
    public static <T> T[] validIndex(T[] array, int index, String message) {
        Validate.notNull(array);
        if (index < 0 || index >= array.length) {
            throw new IllegalArgumentException(message + index);
        }
        return array;
    }

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code> if the
     * index for the argument array is invalid or if the array is <code>null</code>.</p>
     *
     * <pre>
     * Validate.validIndex(myArray, 2);
     * </pre>
     *
     * <p>If the array index is invalid the message in the exception is
     * 'The validated array index is invalid: ' followed by the index.</p>
     *
     * <p>If the array is null then the message in the exception is 'The validated object is null'.</p>
     * 
     * @param array  the array to check, not null
     * @return the validated input array, never <code>null</code>, for chaining
     * @throws IllegalArgumentException if the array index is invalid or null
     */
    public static <T> T[] validIndex(T[] array, int index) {
        return validIndex(array, index, "The validated array index is invalid: ");
    }

    // validIndex collection
    //---------------------------------------------------------------------------------

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code> if the
     * index for the argument collection is invalid or if the collection is <code>null</code>.</p>
     *
     * <pre>
     * Validate.validIndex(myCollection, 2, "The collection index is invalid: ");
     * </pre>
     * 
     * <p>If the collection is null then the message in the exception is 'The validated object is null'.</p>
     *
     * @param coll  the collection to check, not null
     * @param message  the exception message if the collection index is invalid
     * @return the validated input collection, never <code>null</code>, for chaining
     * @throws IllegalArgumentException if the collection index is invalid or null
     */
    public static <T extends Collection<?>> T validIndex(T coll, int index, String message) {
        Validate.notNull(coll);
        if (index < 0 || index >= coll.size()) {
            throw new IllegalArgumentException(message + index);
        }
        return coll;
    }

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code> if the
     * index for the argument collection is invalid or if the collection is <code>null</code>.</p>
     *
     * <pre>
     * Validate.validIndex(myCollection, 2);
     * </pre>
     *
     * <p>If the collection index is invalid the message in the exception is
     * 'The validated collection index is invalid: ' followed by the index.</p>
     *
     * <p>If the collection is null then the message in the exception is 'The validated object is null'.</p>
     * 
     * @param coll  the collection to check, not null
     * @return the validated input collection, never <code>null</code>, for chaining
     * @throws IllegalArgumentException if the collection index is invalid or null
     */
    public static <T extends Collection<?>> T validIndex(T coll, int index) {
        return validIndex(coll, index, "The validated collection index is invalid: ");
    }

    // validIndex string
    //---------------------------------------------------------------------------------

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code> if the
     * index for the argument character sequence (including String and StringBuffer)
     * is invalid or if the string is <code>null</code>.</p>
     *
     * <pre>
     * Validate.validIndex(myStr, 2, "The string index is invalid: ");
     * </pre>
     * 
     * <p>If the string is null then the message in the exception is 'The validated object is null'.</p>
     *
     * @param str  the string to check, not null
     * @param message  the exception message if the string index is invalid
     * @return the validated input string, never <code>null</code>, for chaining
     * @throws IllegalArgumentException if the string index is invalid or null
     */
    public static <T extends CharSequence> T validIndex(T str, int index, String message) {
        Validate.notNull(str);
        if (index < 0 || index >= str.length()) {
            throw new IllegalArgumentException(message + index);
        }
        return str;
    }

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code> if the
     * index for the argument character sequence (including String and StringBuffer)
     * is invalid or if the string is <code>null</code>.</p>
     *
     * <pre>
     * Validate.validIndex(myStr, 2);
     * </pre>
     *
     * <p>If the string index is invalid the message in the exception is
     * 'The validated string index is invalid: ' followed by the index.</p>
     *
     * <p>If the string is null then the message in the exception is 'The validated object is null'.</p>
     * 
     * @param str  the string to check, not null
     * @return the validated input string, never <code>null</code>, for chaining
     * @throws IllegalArgumentException if the string index is invalid or null
     */
    public static <T extends CharSequence> T validIndex(T str, int index) {
        return validIndex(str, index, "The validated string index is invalid: ");
    }

}
