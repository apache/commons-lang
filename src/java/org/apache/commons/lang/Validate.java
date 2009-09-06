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

import java.text.MessageFormat;
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
 * All validate functions exist in 4 variants: 
 * 
 * <p>1st function with only the validation option</p>
 * <pre>
 * Validate.isNotNull(surName);
 * </pre>
 * 
 * <p>2nd function with an additional String message parameter. This should
 * be used only if no additional parameters have to be provided. Instead of using
 * String operations to create the message String, the following 3rd variant 
 * should be used.</p>
 * <pre>
 * Validate.isNotNull(surName, "surname must be set");
 * </pre>
 * 
 * <p>Since commons-lang-3.0, for each validation function a similar 3rd validation function exists
 * with a list of additional message parameters as Objects in ellipsis notation. 
 * This is used instead of simply passing a message String due to performance reasons!
 * When using a message string, all parameters would have to be string concatenated
 * before the call, even if no problem arises which would cost performance.</br>
 * Instead of this, we will concatenate (with spaces) all given msgObjects.toString() 
 * only in case of a failed validation! If the first parameter of the msgObject is a
 * String, it will be taken as the format string for {@code MessageFormat}.</p>
 * 
 * <h3>Examples:</h3>
 * <p>
 * Simply validating an Argument without further message:
 * <pre>
 * public void myFn(String argString, Integer argInt) {
 *     Validate.notNull(argString);
 *     Validate.notNull(argInt);
 *     Validate.isTrue(argInt.intValue > 3);
 * }
 * </pre>
 * <p>
 * 
 * <p>
 * Validating an Argument and adding a message to the IllegalArgumentException:
 * <pre>
 * public void myFn(String argString, Integer argInt) {
 *     Validate.notNull(argInt, "Integer parameter must be set);
 *     Validate.isTrue(argInt.intValue > 3, "Integer parameter must be <=3!");
 * }
 * </pre>
 * <p>
 * 
 * <p>
 * If the first parameter of the msgObject is a String {@code MessageFormat} will be used:
 * <pre>
 *     Validate.isTrue(argInt1.intValue > argInt2.intValue, "param2 actually is {1} but must larger than param1 {0} !", argInt1, argInt2);
 * </pre>
 * </p>
 * 
 * <p>The same function sometimes exists multiple times in a 4th form with a single message String parameter 
 * and an additional value parameter. This is essentially the same like the 3rd form, but with fixed
 * object values to preserve backward compatibility with Validate 2.0!<p> 
 * <p>If the message String contains a <code>&quot;{0}&quot;</code>, it will be passed to 
 * {@code MessageFormat} with the value parameter as single Object parameter. If not, the value parameter 
 * will simply get concatenated to the message String separated with a space.
 *  </p>

 * @see MessageFormat
 * 
 * @author <a href="mailto:ola.berg@arkitema.se">Ola Berg</a>
 * @author Stephen Colebourne
 * @author Gary Gregory
 * @author Norm Deane
 * @author <a href="mailto:struberg@yahoo.de">Mark Struberg</a>
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
            throw new IllegalArgumentException(getMessage(message, value));
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
            
            throw new IllegalArgumentException(getMessage(message, value));
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
            throw new IllegalArgumentException(getMessage(message, value));
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

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code>
     * if the test result is <code>false</code>.</p>
     * 
     * <p>This is used when validating according to an arbitrary boolean expression,
     * such as validating a primitive number or using your own custom validation 
     * expression.</p>
     *
     * <pre>
     * Validate.isTrue(argInt1.intValue > argInt2.intValue, 
     *                 "param2 actually is {1} but must larger than param1 {0} !", argInt1, argInt2);
     * </pre>
     *
     * <p>If the first msgObject is a String, the {@code MessageFormat} will be used to construct the message</p>
     * <p>Otherwise the message in the exception is 'Validation failed: ' followed by all given  
     * parameters delimited with spaces.</p>
     * 
     * @param expression  a boolean expression
     * @throws IllegalArgumentException if expression is <code>false</code>
     */
    public static void isTrue(boolean expression, Object... msgObjects) {
        if (expression == false) {
            throw new IllegalArgumentException(getMessage(msgObjects));
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
        notNull(object, "The validated object is null");
    }


    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code>
     * if the argument is <code>null</code>.</p>
     * 
     * <pre>
     * Validate.notNull(myObject, "This happens while processing user {0}, currentUser);
     * </pre>
     * 
     * <p>If the first msgObject is a String, the {@code MessageFormat} will be used to construct the message</p>
     * <p>Otherwise the message in the exception is 'Validation failed: ' followed by all given  
     * parameters delimited with spaces.</p>
     * 
     * @param object Object to validate
     * @param msgObjects additional Objects added as text message to the InvalidArgumentException
     */
    public static void notNull(Object object, Object... msgObjects) {
        if (object == null) {
            throw new IllegalArgumentException(getMessage(msgObjects));
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
        notEmpty(array, "The validated array is empty");
    }

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code>
     * if the argument array is empty (<code>null</code> or no elements).</p>
     *
     * <pre>
     * Validate.notEmpty(myArray, "This happens while processing user {0}, currentUser);
     * </pre>
     *
     * <p>If the first msgObject is a String, the {@code MessageFormat} will be used to construct the message</p>
     * <p>Otherwise the message in the exception is 'Validation failed: ' followed by all given  
     * parameters delimited with spaces.</p>
     * 
     * @param array  the array to check is not empty
     * @param msgObjects additional Objects added as text message to the InvalidArgumentException
     * @throws IllegalArgumentException if the array is empty
     */
    public static void notEmpty(Object[] array, Object... msgObjects) {
        notEmpty(array, getMessage(msgObjects));
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
    public static void notEmpty(Collection<?> collection, String message) {
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
    public static void notEmpty(Collection<?> collection) {
        notEmpty(collection, "The validated collection is empty");
    }

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code>
     * if the argument Collection is empty (<code>null</code> or no elements).</p>
     *
     * <pre>
     * Validate.notEmpty(myCollection, "This happens while processing user {0}, currentUser);
     * </pre>
     *
     * <p>If the first msgObject is a String, the {@code MessageFormat} will be used to construct the message</p>
     * <p>Otherwise the message in the exception is 'Validation failed: ' followed by all given  
     * parameters delimited with spaces.</p>
     * 
     * @param collection  the collection to check is not empty
     * @param msgObjects additional Objects added as text message to the InvalidArgumentException
     * @throws IllegalArgumentException if the collection is empty
     */
    public static void notEmpty(Collection<?> collection, Object... msgObjects) {
        notEmpty(collection, getMessage(msgObjects));
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
    public static void notEmpty(Map<?,?> map, String message) {
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
    public static void notEmpty(Map<?,?> map) {
        notEmpty(map, "The validated map is empty");
    }

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code>
     * if the argument Map is empty (<code>null</code> or no elements).</p>
     *
     * <pre>
     * Validate.notEmpty(myMap, "This happens while processing user {0}, currentUser);
     * </pre>
     *
     * <p>If the first msgObject is a String, the {@code MessageFormat} will be used to construct the message</p>
     * <p>Otherwise the message in the exception is 'Validation failed: ' followed by all given  
     * parameters delimited with spaces.</p>
     * 
     * @param map  the map to check is not empty
     * @param msgObjects additional Objects added as text message to the InvalidArgumentException
     * @throws IllegalArgumentException if the map is empty
     */
    public static void notEmpty(Map<?,?> map, Object... msgObjects) {
        notEmpty(map, getMessage(msgObjects));
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
        notEmpty(string, "The validated string is empty");
    }

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code>
     * if the argument String is empty (<code>null</code> or zero length).</p>
     *
     * <pre>
     * Validate.notEmpty(myString);
     * </pre>
     *
     * <p>If the first msgObject is a String, the {@code MessageFormat} will be used to construct the message</p>
     * <p>Otherwise the message in the exception is 'Validation failed: ' followed by all given  
     * parameters delimited with spaces.</p>
     * 
     * @param string  the string to check is not empty
     * @param msgObjects additional Objects added as text message to the InvalidArgumentException
     * @throws IllegalArgumentException if the string is empty
     */
    public static void notEmpty(String string, Object... msgObjects) {
        notEmpty(string, getMessage(msgObjects));
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

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code>
     * if the argument array has <code>null</code> elements or is
     * <code>null</code>.</p>
     *
     * <pre>
     * Validate.noNullElements(myArray);
     * </pre>
     *
     * <p>If the first msgObject is a String, the {@code MessageFormat} will be used to construct the message</p>
     * <p>Otherwise the message in the exception is 'Validation failed: ' followed by all given  
     * parameters delimited with spaces.</p>
     *
     * <p>If the array is null then the message in the exception is 'The validated object is null'.</p>
     * 
     * @param array  the array to check
     * @param msgObjects additional Objects added as text message to the InvalidArgumentException
     * @throws IllegalArgumentException if the array has <code>null</code>
     *  elements or is <code>null</code>
     */
    public static void noNullElements(Object[] array, Object... msgObjects) {
        Validate.notNull(array);
        for (int i = 0; i < array.length; i++) {
            if (array[i] == null) {
                //X TODO maybe we can add 'i' as 0-th element? 
                throw new IllegalArgumentException(getMessage(msgObjects));
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
     * if the argument Collection has <code>null</code> elements or is
     * <code>null</code>.</p>
     *
     * <pre>
     * Validate.noNullElements(myCollection);
     * </pre>
     *
     * <p>If the first msgObject is a String, the {@code MessageFormat} will be used to construct the message</p>
     * <p>Otherwise the message in the exception is 'Validation failed: ' followed by all given  
     * parameters delimited with spaces.</p>
     *
     * <p>If the collection is null then the message in the exception is 'The validated object is null'.</p>
     * 
     * @param collection  the collection to check
     * @param msgObjects additional Objects added as text message to the InvalidArgumentException
     * @throws IllegalArgumentException if the collection has
     *  <code>null</code> elements or is <code>null</code>
     */
    public static void noNullElements(Collection collection, Object... msgObjects) {
        Validate.notNull(collection);
        int i = 0;
        for (Iterator it = collection.iterator(); it.hasNext(); i++) {
            if (it.next() == null) {
                //X TODO how about adding 'i' as 0-th element?
                throw new IllegalArgumentException(getMessage(msgObjects));
            }
        }
    }


    // allElementsOfType collection
    //---------------------------------------------------------------------------------

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
    public static void allElementsOfType(Collection collection, Class clazz, Object... msgObjects) {
        Validate.notNull(collection);
        Validate.notNull(clazz);
        int i = 0;
        for (Iterator it = collection.iterator(); it.hasNext(); i++) {
            if (clazz.isInstance(it.next()) == false) {
                //X TODO how to add clazz.getName() and i?
                throw new IllegalArgumentException(getMessage(msgObjects));
            }
        }
    }

    // private helper functions
    //---------------------------------------------------------------------------------

    
    /**
     * private helper function to create an error message from the given Objects
     * If the first object in msgObjects is of type {@code String} then 
     * {@code MessageFormat} will be used to format the output message.
     * 
     * @param msgObjects
     * @return concatenated String representation of all the objects
     */
    private static String getMessage(Object... msgObjects) {
        if (msgObjects.length > 0 && msgObjects[0] instanceof String) {
            String message = (String) msgObjects[0];
            if (msgObjects.length == 2 && !message.matches("[^\\{]*\\{\\d*\\}.*")) {
                // if it doesn't contain {0}, {1} etc we simply use string concatenation 
                return message + msgObjects[1]; // no space between to act like original function!
            }
            
            MessageFormat form = new MessageFormat((String) msgObjects[0]);
            Object[] params = new Object[msgObjects.length - 1];
            System.arraycopy(msgObjects, 1, params, 0, msgObjects.length - 1);
            return form.format(params);
        }
        else {
            StringBuffer sb = new StringBuffer("Validation failed: [");
            for(int i = 0; i < msgObjects.length; i++) {
                if (i > 0) {
                    sb.append(' ');
                }
                sb.append(msgObjects[i]);
            }
            sb.append(']');
            return sb.toString();
        }
    }

}
