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
 * <p>This class assists in validating arguments.</p>
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
     * <p>Validate that the argument condition is <code>true</code>; otherwise 
     * throwing an exception with the specified message. This method is useful when
     * validating according to an arbitrary boolean expression, such as validating an 
     * object or using your own custom validation expression.</p>
     *
     * <pre>Validate.isTrue( myObject.isOk(), "The object is not OK: ", myObject);</pre>
     *
     * <p>For performance reasons, the object value is passed as a separate parameter and
     * appended to the exception message only in the case of an error.</p>
     * 
     * @param expression the boolean expression to check 
     * @param message the exception message if invalid
     * @param value the value to append to the message when invalid
     * @throws IllegalArgumentException if expression is <code>false</code>
     */
    public static void isTrue(boolean expression, String message, Object value) {
        if (expression == false) {
            throw new IllegalArgumentException(message + value);
        }
    }

    /**
     * <p>Validate that the argument condition is <code>true</code>; otherwise 
     * throwing an exception with the specified message. This method is useful when
     * validating according to an arbitrary boolean expression, such as validating a 
     * primitive number or using your own custom validation expression.</p>
     *
     * <pre>Validate.isTrue(i > 0.0, "The value must be greater than zero: ", i);</pre>
     *
     * <p>For performance reasons, the long value is passed as a separate parameter and
     * appended to the exception message only in the case of an error.</p>
     * 
     * @param expression the boolean expression to check 
     * @param message the exception message if invalid
     * @param value the value to append to the message when invalid
     * @throws IllegalArgumentException if expression is <code>false</code>
     */
    public static void isTrue(boolean expression, String message, long value) {
        if (expression == false) {
            throw new IllegalArgumentException(message + value);
        }
    }

    /**
     * <p>Validate that the argument condition is <code>true</code>; otherwise 
     * throwing an exception with the specified message. This method is useful when
     * validating according to an arbitrary boolean expression, such as validating a 
     * primitive number or using your own custom validation expression.</p>
     *
     * <pre>Validate.isTrue(d > 0.0, "The value must be greater than zero: ", d);</pre>
     *
     * <p>For performance reasons, the double value is passed as a separate parameter and
     * appended to the exception message only in the case of an error.</p>
     * 
     * @param expression the boolean expression to check 
     * @param message the exception message if invalid
     * @param value the value to append to the message when invalid
     * @throws IllegalArgumentException if expression is <code>false</code>
     */
    public static void isTrue(boolean expression, String message, double value) {
        if (expression == false) {
            throw new IllegalArgumentException(message + value);
        }
    }

    /**
     * <p>Validate that the argument condition is <code>true</code>; otherwise 
     * throwing an exception with the specified message. This method is useful when
     * validating according to an arbitrary boolean expression, such as validating a 
     * primitive number or using your own custom validation expression.</p>
     *
     * <pre>
     * Validate.isTrue( (i > 0), "The value must be greater than zero");
     * Validate.isTrue( myObject.isOk(), "The object is not OK");
     * </pre>
     *
     * @param expression the boolean expression to check 
     * @param message the exception message if invalid
     * @throws IllegalArgumentException if expression is <code>false</code>
     */
    public static void isTrue(boolean expression, String message) {
        if (expression == false) {
            throw new IllegalArgumentException(message);
        }
    }

    /**
     * <p>Validate that the argument condition is <code>true</code>; otherwise 
     * throwing an exception. This method is useful when validating according 
     * to an arbitrary boolean expression, such as validating a 
     * primitive number or using your own custom validation expression.</p>
     *
     * <pre>
     * Validate.isTrue(i > 0);
     * Validate.isTrue(myObject.isOk());</pre>
     *
     * <p>The message of the exception is &quot;The validated expression is 
     * false&quot;.</p>
     * 
     * @param expression the boolean expression to check 
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
     * <p>Validate that the specified argument is not <code>null</code>; 
     * otherwise throwing an exception.
     *
     * <pre>Validate.notNull(myObject);</pre>
     *
     * <p>The message of the exception is &quot;The validated object is 
     * null&quot;.</p>
     * 
     * @param object the object to check
     * @throws IllegalArgumentException if the object is <code>null</code>
     */
    public static void notNull(Object object) {
        notNull(object, "The validated object is null");
    }

    /**
     * <p>Validate that the specified argument is not <code>null</code>; 
     * otherwise throwing an exception with the specified message.
     *
     * <pre>Validate.notNull(myObject, "The object must not be null");</pre>
     * 
     * @param object the object to check
     * @param message the exception message if invalid
     */
    public static void notNull(Object object, String message) {
        if (object == null) {
            throw new IllegalArgumentException(message);
        }
    }

    // notEmpty array
    //---------------------------------------------------------------------------------

    /**
     * <p>Validate that the specified argument array is neither <code>null</code> 
     * nor a length of zero (no elements); otherwise throwing an exception 
     * with the specified message.
     *
     * <pre>Validate.notEmpty(myArray, "The array must not be empty");</pre>
     * 
     * @param array the array to check
     * @param message the exception message if invalid
     * @throws IllegalArgumentException if the array is empty
     */
    public static void notEmpty(Object[] array, String message) {
        if (array == null || array.length == 0) {
            throw new IllegalArgumentException(message);
        }
    }

    /**
     * <p>Validate that the specified argument array is neither <code>null</code> 
     * nor a length of zero (no elements); otherwise throwing an exception. 
     *
     * <pre>Validate.notEmpty(myArray);</pre>
     * 
     * <p>The message in the exception is &quot;The validated array is 
     * empty&quot;.
     * 
     * @param array the array to check
     * @throws IllegalArgumentException if the array is empty
     */
    public static void notEmpty(Object[] array) {
        notEmpty(array, "The validated array is empty");
    }

    // notEmpty collection
    //---------------------------------------------------------------------------------

    /**
     * <p>Validate that the specified argument collection is neither <code>null</code> 
     * nor a size of zero (no elements); otherwise throwing an exception 
     * with the specified message.
     *
     * <pre>Validate.notEmpty(myCollection, "The collection must not be empty");</pre>
     * 
     * @param collection the collection to check
     * @param message the exception message if invalid
     * @throws IllegalArgumentException if the collection is empty
     */
    public static void notEmpty(Collection collection, String message) {
        if (collection == null || collection.size() == 0) {
            throw new IllegalArgumentException(message);
        }
    }

    /**
     * <p>Validate that the specified argument collection is neither <code>null</code> 
     * nor a size of zero (no elements); otherwise throwing an exception. 
     *
     * <pre>Validate.notEmpty(myCollection);</pre>
     * 
     * <p>The message in the exception is &quot;The validated collection is 
     * empty&quot;.</p>
     * 
     * @param collection the collection to check
     * @throws IllegalArgumentException if the collection is empty
     */
    public static void notEmpty(Collection collection) {
        notEmpty(collection, "The validated collection is empty");
    }

    // notEmpty map
    //---------------------------------------------------------------------------------

    /**
     * <p>Validate that the specified argument map is neither <code>null</code> 
     * nor a size of zero (no elements); otherwise throwing an exception 
     * with the specified message.
     *
     * <pre>Validate.notEmpty(myMap, "The map must not be empty");</pre>
     * 
     * @param map the map to check
     * @param message the exception message if invalid
     * @throws IllegalArgumentException if the map is empty
     */
    public static void notEmpty(Map map, String message) {
        if (map == null || map.size() == 0) {
            throw new IllegalArgumentException(message);
        }
    }

    /**
     * <p>Validate that the specified argument map is neither <code>null</code> 
     * nor a size of zero (no elements); otherwise throwing an exception. 
     *
     * <pre>Validate.notEmpty(myMap);</pre>
     * 
     * <p>The message in the exception is &quot;The validated map is 
     * empty&quot;.</p>
     * 
     * @param map the map to check
     * @throws IllegalArgumentException if the map is empty
     * @see #notEmpty(Map, String)
     */
    public static void notEmpty(Map map) {
        notEmpty(map, "The validated map is empty");
    }

    // notEmpty string
    //---------------------------------------------------------------------------------

    /**
     * <p>Validate that the specified argument string is 
     * neither <code>null</code> nor a length of zero (no characters); 
     * otherwise throwing an exception with the specified message.
     *
     * <pre>Validate.notEmpty(myString, "The string must not be empty");</pre>
     * 
     * @param string the string to check
     * @param message the exception message if invalid
     * @throws IllegalArgumentException if the string is empty
     */
    public static void notEmpty(String string, String message) {
        if (string == null || string.length() == 0) {
            throw new IllegalArgumentException(message);
        }
    }

    /**
     * <p>Validate that the specified argument string is 
     * neither <code>null</code> nor a length of zero (no characters); 
     * otherwise throwing an exception with the specified message.
     *
     * <pre>Validate.notEmpty(myString);</pre>
     * 
     * <p>The message in the exception is &quot;The validated 
     * string is empty&quot;.</p>
     * 
     * @param string the string to check
     * @throws IllegalArgumentException if the string is empty
     */
    public static void notEmpty(String string) {
        notEmpty(string, "The validated string is empty");
    }

    // notNullElements array
    //---------------------------------------------------------------------------------

    /**
     * <p>Validate that the specified argument array is neither 
     * <code>null</code> nor contains any elements that are <code>null</code>;
     * otherwise throwing an exception with the specified message.
     *
     * <pre>Validate.noNullElements(myArray, "The array contain null at position %d");</pre>
     * 
     * <p>If the array is <code>null</code>, then the message in the exception 
     * is &quot;The validated object is null&quot;.</p>
     *
     * @param array the array to check
     * @param message the exception message if the collection has <code>null</code> elements
     * @throws IllegalArgumentException if the array is <code>null</code> or
     * an element in the array is <code>null</code>
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
     * <p>Validate that the specified argument array is neither 
     * <code>null</code> nor contains any elements that are <code>null</code>;
     * otherwise throwing an exception.
     *
     * <pre>Validate.noNullElements(myArray);</pre>
     * 
     * <p>If the array is <code>null</code>, then the message in the exception 
     * is &quot;The validated object is null&quot;.</p>
     * 
     * <p>If the array has a <code>null</code> element, then the message in the
     * exception is &quot;The validated array contains null element at index: 
     * &quot followed by the index.</p>
     *
     * @param array the array to check
     * @throws IllegalArgumentException if the array is <code>null</code> or
     * an element in the array is <code>null</code>
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
     * <p>Validate that the specified argument collection is neither 
     * <code>null</code> nor contains any elements that are <code>null</code>;
     * otherwise throwing an exception with the specified message.
     *
     * <pre>Validate.noNullElements(myCollection, "The collection contains null elements");</pre>
     * 
     * <p>If the collection is <code>null</code>, then the message in the exception 
     * is &quot;The validated object is null&quot;.</p>
     * 
     *
     * @param collection  the collection to check
     * @param message  the exception message if the collection has
     * @throws IllegalArgumentException if the collection is <code>null</code> or
     * an element in the collection is <code>null</code>
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
     * <p>Validate that the specified argument collection is neither 
     * <code>null</code> nor contains any elements that are <code>null</code>;
     * otherwise throwing an exception.
     *
     * <pre>Validate.noNullElements(myCollection);</pre>
     * 
     * <p>If the collection is <code>null</code>, then the message in the exception 
     * is &quot;The validated object is null&quot;.</p>
     * 
     * <p>If the collection has a <code>null</code> element, then the message in the
     * exception is &quot;The validated collection contains null element at index: 
     * &quot followed by the index.</p>
     *
     * @param collection  the collection to check
     * @throws IllegalArgumentException if the collection is <code>null</code> or
     * an element in the collection is <code>null</code>
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
     * @param collection  the collection to check, not null
     * @param clazz  the <code>Class</code> which the collection's elements are expected to be, not null
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
