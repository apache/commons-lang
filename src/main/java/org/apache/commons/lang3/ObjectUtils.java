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
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.apache.commons.lang3.exception.CloneFailedException;

/**
 * <p>Operations on <code>Object</code>.</p>
 * 
 * <p>This class tries to handle <code>null</code> input gracefully.
 * An exception will generally not be thrown for a <code>null</code> input.
 * Each method documents its behaviour in more detail.</p>
 *
 * <p>#ThreadSafe#</p>
 * @author Apache Software Foundation
 * @author <a href="mailto:nissim@nksystems.com">Nissim Karpenstein</a>
 * @author <a href="mailto:janekdb@yahoo.co.uk">Janek Bogucki</a>
 * @author Daniel L. Rall
 * @author Gary Gregory
 * @author Mario Winterer
 * @author <a href="mailto:david@davidkarlsen.com">David J. M. Karlsen</a>
 * @since 1.0
 * @version $Id$
 */
//@Immutable
public class ObjectUtils {

    /**
     * <p>Singleton used as a <code>null</code> placeholder where
     * <code>null</code> has another meaning.</p>
     *
     * <p>For example, in a <code>HashMap</code> the
     * {@link java.util.HashMap#get(java.lang.Object)} method returns
     * <code>null</code> if the <code>Map</code> contains
     * <code>null</code> or if there is no matching key. The
     * <code>Null</code> placeholder can be used to distinguish between
     * these two cases.</p>
     *
     * <p>Another example is <code>Hashtable</code>, where <code>null</code>
     * cannot be stored.</p>
     *
     * <p>This instance is Serializable.</p>
     */
    public static final Null NULL = new Null();
    
    /**
     * <p><code>ObjectUtils</code> instances should NOT be constructed in
     * standard programming. Instead, the class should be used as
     * <code>ObjectUtils.defaultIfNull("a","b");</code>.</p>
     *
     * <p>This constructor is public to permit tools that require a JavaBean instance
     * to operate.</p>
     */
    public ObjectUtils() {
        super();
    }

    // Defaulting
    //-----------------------------------------------------------------------
    /**
     * <p>Returns a default value if the object passed is
     * <code>null</code>.</p>
     * 
     * <pre>
     * ObjectUtils.defaultIfNull(null, null)      = null
     * ObjectUtils.defaultIfNull(null, "")        = ""
     * ObjectUtils.defaultIfNull(null, "zz")      = "zz"
     * ObjectUtils.defaultIfNull("abc", *)        = "abc"
     * ObjectUtils.defaultIfNull(Boolean.TRUE, *) = Boolean.TRUE
     * </pre>
     *
     * @param object  the <code>Object</code> to test, may be <code>null</code>
     * @param defaultValue  the default value to return, may be <code>null</code>
     * @return <code>object</code> if it is not <code>null</code>, defaultValue otherwise
     */
    public static Object defaultIfNull(Object object, Object defaultValue) {
        return object != null ? object : defaultValue;
    }

    /**
     * <p>Returns the first value in the array which is not <code>null</code>.
     * If all the values are <code>null</code> or the array is <code>null</code>
     * or empty then <code>null</code> is returned.</p>
     * 
     * <pre>
     * ObjectUtils.firstNonNull(null, null)      = null
     * ObjectUtils.firstNonNull(null, "")        = ""
     * ObjectUtils.firstNonNull(null, null, "")  = ""
     * ObjectUtils.firstNonNull(null, "zz")      = "zz"
     * ObjectUtils.firstNonNull("abc", *)        = "abc"
     * ObjectUtils.firstNonNull(null, "xyz", *)  = "xyz"
     * ObjectUtils.firstNonNull(Boolean.TRUE, *) = Boolean.TRUE
     * ObjectUtils.firstNonNull()                = null
     * </pre>
     *
     * @param values  the values to test, may be <code>null</code> or empty
     * @return the first value from <code>values</code> which is not <code>null</code>,
     *  or <code>null</code> if there are no non-null values
     */
    public static <T> T firstNonNull(T... values) {
        if (values != null) {
            for (T val : values) {
                if (val != null) {
                    return val;
                }
            }
        }
        return null;
    }

    // Null-safe equals/hashCode
    //-----------------------------------------------------------------------
    /**
     * <p>Compares two objects for equality, where either one or both
     * objects may be <code>null</code>.</p>
     *
     * <pre>
     * ObjectUtils.equals(null, null)                  = true
     * ObjectUtils.equals(null, "")                    = false
     * ObjectUtils.equals("", null)                    = false
     * ObjectUtils.equals("", "")                      = true
     * ObjectUtils.equals(Boolean.TRUE, null)          = false
     * ObjectUtils.equals(Boolean.TRUE, "true")        = false
     * ObjectUtils.equals(Boolean.TRUE, Boolean.TRUE)  = true
     * ObjectUtils.equals(Boolean.TRUE, Boolean.FALSE) = false
     * </pre>
     *
     * @param object1  the first object, may be <code>null</code>
     * @param object2  the second object, may be <code>null</code>
     * @return <code>true</code> if the values of both objects are the same
     */
    public static boolean equals(Object object1, Object object2) {
        if (object1 == object2) {
            return true;
        }
        if ((object1 == null) || (object2 == null)) {
            return false;
        }
        return object1.equals(object2);
    }

    /**
     * <p>Gets the hash code of an object returning zero when the
     * object is <code>null</code>.</p>
     *
     * <pre>
     * ObjectUtils.hashCode(null)   = 0
     * ObjectUtils.hashCode(obj)    = obj.hashCode()
     * </pre>
     *
     * @param obj  the object to obtain the hash code of, may be <code>null</code>
     * @return the hash code of the object, or zero if null
     * @since 2.1
     */
    public static int hashCode(Object obj) {
        return (obj == null) ? 0 : obj.hashCode();
    }

    // Identity ToString
    //-----------------------------------------------------------------------
    /**
     * <p>Gets the toString that would be produced by <code>Object</code>
     * if a class did not override toString itself. <code>null</code>
     * will return <code>null</code>.</p>
     *
     * <pre>
     * ObjectUtils.identityToString(null)         = null
     * ObjectUtils.identityToString("")           = "java.lang.String@1e23"
     * ObjectUtils.identityToString(Boolean.TRUE) = "java.lang.Boolean@7fa"
     * </pre>
     *
     * @param object  the object to create a toString for, may be
     *  <code>null</code>
     * @return the default toString text, or <code>null</code> if
     *  <code>null</code> passed in
     */
    public static String identityToString(Object object) {
        if (object == null) {
            return null;
        }
        StringBuffer buffer = new StringBuffer();
        identityToString(buffer, object);
        return buffer.toString();
    }

    /**
     * <p>Appends the toString that would be produced by <code>Object</code>
     * if a class did not override toString itself. <code>null</code>
     * will throw a NullPointerException for either of the two parameters. </p>
     *
     * <pre>
     * ObjectUtils.identityToString(buf, "")            = buf.append("java.lang.String@1e23"
     * ObjectUtils.identityToString(buf, Boolean.TRUE)  = buf.append("java.lang.Boolean@7fa"
     * ObjectUtils.identityToString(buf, Boolean.TRUE)  = buf.append("java.lang.Boolean@7fa")
     * </pre>
     *
     * @param buffer  the buffer to append to
     * @param object  the object to create a toString for
     * @since 2.4
     */
    public static void identityToString(StringBuffer buffer, Object object) {
        if (object == null) {
            throw new NullPointerException("Cannot get the toString of a null identity");
        }
        buffer.append(object.getClass().getName())
              .append('@')
              .append(Integer.toHexString(System.identityHashCode(object)));
    }

    // ToString
    //-----------------------------------------------------------------------
    /**
     * <p>Gets the <code>toString</code> of an <code>Object</code> returning
     * an empty string ("") if <code>null</code> input.</p>
     * 
     * <pre>
     * ObjectUtils.toString(null)         = ""
     * ObjectUtils.toString("")           = ""
     * ObjectUtils.toString("bat")        = "bat"
     * ObjectUtils.toString(Boolean.TRUE) = "true"
     * </pre>
     * 
     * @see StringUtils#defaultString(String)
     * @see String#valueOf(Object)
     * @param obj  the Object to <code>toString</code>, may be null
     * @return the passed in Object's toString, or nullStr if <code>null</code> input
     * @since 2.0
     */
    public static String toString(Object obj) {
        return obj == null ? "" : obj.toString();
    }

    /**
     * <p>Gets the <code>toString</code> of an <code>Object</code> returning
     * a specified text if <code>null</code> input.</p>
     * 
     * <pre>
     * ObjectUtils.toString(null, null)           = null
     * ObjectUtils.toString(null, "null")         = "null"
     * ObjectUtils.toString("", "null")           = ""
     * ObjectUtils.toString("bat", "null")        = "bat"
     * ObjectUtils.toString(Boolean.TRUE, "null") = "true"
     * </pre>
     * 
     * @see StringUtils#defaultString(String,String)
     * @see String#valueOf(Object)
     * @param obj  the Object to <code>toString</code>, may be null
     * @param nullStr  the String to return if <code>null</code> input, may be null
     * @return the passed in Object's toString, or nullStr if <code>null</code> input
     * @since 2.0
     */
    public static String toString(Object obj, String nullStr) {
        return obj == null ? nullStr : obj.toString();
    }

    // Min/Max
    //-----------------------------------------------------------------------
    /**
     * Null safe comparison of Comparables.
     * 
     * @param c1  the first comparable, may be null
     * @param c2  the second comparable, may be null
     * @return
     *  <ul>
     *   <li>If both objects are non-null and unequal, the lesser object.
     *   <li>If both objects are non-null and equal, c1.
     *   <li>If one of the comparables is null, the non-null object.
     *   <li>If both the comparables are null, null is returned.
     *  </ul>
     */
    public static <T extends Comparable<? super T>> T min(T c1, T c2) {
        if (c1 != null && c2 != null) {
            return c1.compareTo(c2) < 1 ? c1 : c2;
        } else {
            return c1 != null ? c1 : c2;
        }                              
    }

    /**
     * Null safe comparison of Comparables.
     * 
     * @param c1  the first comparable, may be null
     * @param c2  the second comparable, may be null
     * @return
     *  <ul>
     *   <li>If both objects are non-null and unequal, the greater object.
     *   <li>If both objects are non-null and equal, c1.
     *   <li>If one of the comparables is null, the non-null object.
     *   <li>If both the comparables are null, null is returned.
     *  </ul>
     */
    public static <T extends Comparable<? super T>> T max(T c1, T c2) {
        if (c1 != null && c2 != null) {
            return c1.compareTo(c2) >= 0 ? c1 : c2;
        } else {
            return c1 != null ? c1 : c2;
        }
    }
    
    /**
     * Clone an object.
     * 
     * @param <T> the type of the object
     * @param o the object to clone
     * @return the clone if the object implements {@link Cloneable} otherwise <code>null</code>
     * @throws CloneFailedException if the object is cloneable and the clone operation fails
     * @since 3.0
     */
    public static <T> T clone(final T o) {
        if (o instanceof Cloneable) {
            try {
                final Method clone = o.getClass().getMethod("clone", (Class[])null);
                @SuppressWarnings("unchecked")
                final T result = (T)clone.invoke(o, (Object[])null);
                return result;
            } catch (final NoSuchMethodException e) {
                throw new CloneFailedException("Cloneable type has no clone method", e);
            } catch (final IllegalAccessException e) {
                throw new CloneFailedException("Cannot clone Cloneable type", e);
            } catch (final InvocationTargetException e) {
                throw new CloneFailedException("Exception cloning Cloneable type", e.getCause());
            }
        }

        return null;
    }

    /**
     * Clone an object if possible. This method is similar to {@link #clone(Object)}, but will
     * return the provided instance as the return value instead of <code>null</code> if the instance
     * is not cloneable. This is more convenient if the caller uses different
     * implementations (e.g. of a service) and some of the implementations do not allow concurrent
     * processing or have state. In such cases the implementation can simply provide a proper
     * clone implementation and the caller's code does not have to change.
     * 
     * @param <T> the type of the object
     * @param o the object to clone
     * @return the clone if the object implements {@link Cloneable} otherwise the object itself
     * @throws CloneFailedException if the object is cloneable and the clone operation fails
     * @since 3.0
     */
    public static <T> T cloneIfPossible(final T o) {
        final T clone = clone(o);
        return clone == null ? o : clone;
    }

    // Null
    //-----------------------------------------------------------------------
    /**
     * <p>Class used as a null placeholder where <code>null</code>
     * has another meaning.</p>
     *
     * <p>For example, in a <code>HashMap</code> the
     * {@link java.util.HashMap#get(java.lang.Object)} method returns
     * <code>null</code> if the <code>Map</code> contains
     * <code>null</code> or if there is no matching key. The
     * <code>Null</code> placeholder can be used to distinguish between
     * these two cases.</p>
     *
     * <p>Another example is <code>Hashtable</code>, where <code>null</code>
     * cannot be stored.</p>
     */
    public static class Null implements Serializable {
        /**
         * Required for serialization support. Declare serialization compatibility with Commons Lang 1.0
         * 
         * @see java.io.Serializable
         */
        private static final long serialVersionUID = 7092611880189329093L;
        
        /**
         * Restricted constructor - singleton.
         */
        Null() {
            super();
        }
        
        /**
         * <p>Ensure singleton.</p>
         * 
         * @return the singleton value
         */
        private Object readResolve() {
            return ObjectUtils.NULL;
        }
    }

}
