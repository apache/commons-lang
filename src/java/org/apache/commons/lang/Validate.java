/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002-2003 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowledgement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowledgement may appear in the software itself,
 *    if and wherever such third-party acknowledgements normally appear.
 *
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 */
package org.apache.commons.lang;

import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
/**
 * <p>Assists in alidating arguments.</p>
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
 * @since 2.0
 * @version $Id: Validate.java,v 1.5 2003/08/22 17:25:33 ggregory Exp $
 */
public class Validate {
    
    /**
     * Constructor. This class should not normally be instantiated.
     */
    public Validate() {
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
     * <p>For performance reasons, the object is passed as a separate parameter and
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
     * <p>For performance reasons, the object is passed as a separate parameter and
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
     * Validate.notEmpty(myMap, "The collection must not be empty");
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
     * Validate.notEmpty(myArray, "The array must not contain null elements");
     * </pre>
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
     * Validate.notEmpty(myArray);
     * </pre>
     *
     * <p>The message in the exception is 'The validated array contains null element at index: '.</p>
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
     * if the argument collection has <code>null</code> elements or is
     * <code>null</code>.</p>
     *
     * <pre>
     * Validate.notEmpty(myCollection, "The collection must not contain null elements");
     * </pre>
     * 
     * @param collection  the collection to check
     * @param message  the exception message if the array has
     *  <code>null</code> elements
     * @throws IllegalArgumentException if the collection has
     *  <code>null</code> elements or is <code>null</code>
     */
    public static void noNullElements(Collection collection, String message) {
        Validate.notNull(collection);
        int i = 0;
        for (Iterator it = collection.iterator(); it.hasNext(); i++) {
            if (it.next() == null) {
                throw new IllegalArgumentException(message);
            }
        }
    }

    /**
     * <p>Validate an argument, throwing <code>IllegalArgumentException</code>
     * if the argument collection has <code>null</code> elements or is
     * <code>null</code>.</p>
     *
     * <pre>
     * Validate.notEmpty(myCollection);
     * </pre>
     *
     * <p>The message in the exception is 'The validated collection contains null element at index: '.</p>
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

}
