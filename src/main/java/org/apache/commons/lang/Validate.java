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
 * @author <a href="mailto:ola.berg@arkitema.se">Ola Berg</a>
 * @author Stephen Colebourne
 * @author Gary Gregory
 * @author Norm Deane
 * @since 2.0
 * @version $Id$
 */
public class Validate {
    // Validate has no dependencies on other classes in Commons Lang at present
    
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
     * @throws IllegalArgumentException if the object is <code>null</code>
     */
    public static void notNull(Object object, String message) {
        if (object == null) {
            throw new IllegalArgumentException(message);
        }
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
     * @throws IllegalArgumentException if the object is <code>null</code>
     */
    public static void notNull(Object object) {
        if (object == null) {
            throw new IllegalArgumentException("The validated object is null");
        }
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
     * @throws IllegalArgumentException if the array is empty
     */
    public static void notEmpty(Object[] array, String message) {
        if (array == null || array.length == 0) {
            throw new IllegalArgumentException(message);
        }
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
     * @throws IllegalArgumentException if the array is empty
     */
    public static void notEmpty(Object[] array) {
        if (array == null || array.length == 0) {
            throw new IllegalArgumentException("The validated array is empty");
        }
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
     * @throws IllegalArgumentException if the collection is empty
     */
    public static void notEmpty(Collection collection, String message) {
        if (collection == null || collection.size() == 0) {
            throw new IllegalArgumentException(message);
        }
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
     * @throws IllegalArgumentException if the collection is empty
     */
    public static void notEmpty(Collection collection) {
        if (collection == null || collection.size() == 0) {
            throw new IllegalArgumentException("The validated collection is empty");
        }
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
     * @throws IllegalArgumentException if the map is empty
     */
    public static void notEmpty(Map map, String message) {
        if (map == null || map.size() == 0) {
            throw new IllegalArgumentException(message);
        }
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
     * @throws IllegalArgumentException if the map is empty
     */
    public static void notEmpty(Map map) {
        if (map == null || map.size() == 0) {
            throw new IllegalArgumentException("The validated map is empty");
        }
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
     * @throws IllegalArgumentException if the string is empty
     */
    public static void notEmpty(String string, String message) {
        if (string == null || string.length() == 0) {
            throw new IllegalArgumentException(message);
        }
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
     * @throws IllegalArgumentException if the string is empty
     */
    public static void notEmpty(String string) {
        if (string == null || string.length() == 0) {
            throw new IllegalArgumentException("The validated string is empty");
        }
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
     * @throws IllegalArgumentException if the array has <code>null</code>
     *  elements or is <code>null</code>
     */
    public static void noNullElements(Object[] array, String message) {
        Validate.notNull(array);
        for (int i = 0; i < array.length; i++) {
            if (array[i] == null) {
                throw new IllegalArgumentException(message);
            }
        }
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
     * @throws IllegalArgumentException if the array has <code>null</code>
     *  elements or is <code>null</code>
     */
    public static void noNullElements(Object[] array) {
        Validate.notNull(array);
        for (int i = 0; i < array.length; i++) {
            if (array[i] == null) {
                throw new IllegalArgumentException("The validated array contains null element at index: " + i);
            }
        }
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
     * @throws IllegalArgumentException if the collection has
     *  <code>null</code> elements or is <code>null</code>
     */
    public static void noNullElements(Collection collection, String message) {
        Validate.notNull(collection);
        for (Iterator it = collection.iterator(); it.hasNext();) {
            if (it.next() == null) {
                throw new IllegalArgumentException(message);
            }
        }
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
     * @throws IllegalArgumentException if the collection has
     *  <code>null</code> elements or is <code>null</code>
     */
    public static void noNullElements(Collection collection) {
        Validate.notNull(collection);
        int i = 0;
        for (Iterator it = collection.iterator(); it.hasNext(); i++) {
            if (it.next() == null) {
                throw new IllegalArgumentException("The validated collection contains null element at index: " + i);
            }
        }
    }

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code>
     * if the argument collection  is <code>null</code> or has elements that
     * are not of type <code>clazz</code> or a subclass.</p>
     *
     * <pre>
     * Validate.allElementsOfType(collection, String.class, "Collection has invalid elements");
     * </pre>
     *
     * @param collection  the collection to check, not null
     * @param clazz  the <code>Class</code> which the collection's elements are expected to be, not null
     * @param message  the exception message if the <code>Collection</code> has elements not of type <code>clazz</code>
     * @since 2.1
     */
    public static void allElementsOfType(Collection collection, Class clazz, String message) {
        Validate.notNull(collection);
        Validate.notNull(clazz);
        for (Iterator it = collection.iterator(); it.hasNext(); ) {
            if (clazz.isInstance(it.next()) == false) {
                throw new IllegalArgumentException(message);
            }
        }
    }

    /**
     * <p>
     * Validate an argument, throwing <code>IllegalArgumentException</code> if the argument collection is
     * <code>null</code> or has elements that are not of type <code>clazz</code> or a subclass.
     * </p>
     * 
     * <pre>
     * Validate.allElementsOfType(collection, String.class);
     * </pre>
     * 
     * <p>
     * The message in the exception is 'The validated collection contains an element not of type clazz at index: '.
     * </p>
     * 
     * @param collection
     *            the collection to check, not null
     * @param clazz
     *            the <code>Class</code> which the collection's elements are expected to be, not null
     * @since 2.1
     */
    public static void allElementsOfType(Collection collection, Class clazz) {
        Validate.notNull(collection);
        Validate.notNull(clazz);
        int i = 0;
        for (Iterator it = collection.iterator(); it.hasNext(); i++) {
            if (clazz.isInstance(it.next()) == false) {
                throw new IllegalArgumentException("The validated collection contains an element not of type "
                    + clazz.getName() + " at index: " + i);
            }
        }
    }

}
