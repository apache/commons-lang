/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002 The Apache Software Foundation.  All rights
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
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
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

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.apache.commons.lang.builder.ToStringStyle;
/**
 * <p><code>ArrayUtils</code> contains utility methods for working for
 * arrays.</p>
 *
 * @author <a href="mailto:scolebourne@apache.org">Stephen Colebourne</a>
 * @author Moritz Petersen
 * @author <a href="mailto:fredrik@westermarck.com">Fredrik Westermarck</a>
 * @version $Id: ArrayUtils.java,v 1.6 2002/11/24 16:50:38 scolebourne Exp $
 */
public class ArrayUtils {

    /** An empty immutable object array */
    public static final Object[] EMPTY_OBJECT_ARRAY = new Object[0];
    /** An empty immutable class array */
    public static final Class[] EMPTY_CLASS_ARRAY = new Class[0];
    /** An empty immutable string array */
    public static final String[] EMPTY_STRING_ARRAY = new String[0];
    /** An empty immutable long array */
    public static final long[] EMPTY_LONG_ARRAY = new long[0];
    /** An empty immutable int array */
    public static final int[] EMPTY_INT_ARRAY = new int[0];
    /** An empty immutable short array */
    public static final short[] EMPTY_SHORT_ARRAY = new short[0];
    /** An empty immutable byte array */
    public static final byte[] EMPTY_BYTE_ARRAY = new byte[0];
    /** An empty immutable double array */
    public static final double[] EMPTY_DOUBLE_ARRAY = new double[0];
    /** An empty immutable float array */
    public static final float[] EMPTY_FLOAT_ARRAY = new float[0];
    /** An empty immutable boolean array */
    public static final boolean[] EMPTY_BOOLEAN_ARRAY = new boolean[0];
    
    /**
     * <p>ArrayUtils instances should NOT be constructed in standard programming.
     * Instead, the class should be used as <code>ArrayUtils.clone(new int[] {2})</code>.</p>
     *
     * <p>This constructor is public to permit tools that require a JavaBean instance
     * to operate.</p>
     */
    public ArrayUtils() {
    }

    // Basic methods handling multi-dimensional arrays
    //--------------------------------------------------------------------------
    
    /**
     * <p>Outputs an array as a String, treating <code>null</code> as an empty array.</p>
     *
     * <p>Multi-dimensional arrays are handled correctly, including
     * multi-dimensional primitive arrays.</p>
     *
     * <p>The format is that of Java source code, for example {a,b}.</p>
     * 
     * @param array  the array to get a toString for, may be <code>null</code>
     * @return a String representation of the array, '{}' if <code>null</code> passed in
     */
    public static String toString(Object array) {
        return toString(array, "{}");
    }
    
    /**
     * <p>Outputs an array as a String handling <code>null</code>s.</p>
     *
     * <p>Multi-dimensional arrays are handled correctly, including
     * multi-dimensional primitive arrays.</p>
     *
     * <p>The format is that of Java source code, for example {a,b}.</p>
     * 
     * @param array  the array to get a toString for, may be <code>null</code>
     * @param stringIfNull  the String to return if the array is <code>null</code>
     * @return a String representation of the array
     */    
    public static String toString(Object array, String stringIfNull) {
        if (array == null) {
            return stringIfNull;
        }
        return new ToStringBuilder(array, ToStringStyle.SIMPLE_STYLE).append(array).toString();
    }
    
    /**
     * <p>Get a hashCode for an array handling multi-dimensional arrays correctly.</p>
     * 
     * <p>Multi-dimensional primitive arrays are also handled correctly by this method.</p>
     * 
     * @param array  the array to get a hashCode for, may be <code>null</code>
     * @return a hashCode for the array
     */
    public static int hashCode(Object array) {
        return new HashCodeBuilder().append(array).toHashCode();
    }
    
    /**
     * <p>Compares two arrays, using equals(), handling multi-dimensional arrays
     * correctly.</p>
     * 
     * <p>Multi-dimensional primitive arrays are also handled correctly by this method.</p>
     * 
     * @param array1  the array to get a hashCode for, may be <code>null</code>
     * @param array2  the array to get a hashCode for, may be <code>null</code>
     * @return <code>true</code> if the arrays are equal
     */
    public static boolean isEquals(Object array1, Object array2) {
        return new EqualsBuilder().append(array1, array2).isEquals();
    }
    
    //--------------------------------------------------------------------------
    
    /**
     * <p>Converts the given array into a {@link Map}. Each element of the array
     * must be either a {@link Map.Entry} or an Array, containing at least two
     * elements, where the first element is used as key and the second as
     * value.</p>
     *
     * <p>This method can be used to initialize:</p>
     * <pre>
     * // Create a Map mapping colors.
     * Map colorMap = MapUtils.toMap(new String[][] {{
     *     {"RED", "#FF0000"},
     *     {"GREEN", "#00FF00"},
     *     {"BLUE", "#0000FF"}});
     * </pre>
     *
     * @param array  an array whose elements are either a {@link Map.Entry} or 
     *  an Array containing at least two elements
     * @return a <code>Map</code> that was created from the array
     * @throws IllegalArgumentException  if the array is <code>null</code>
     * @throws IllegalArgumentException  if one element of this Array is
     *  itself an Array containing less then two elements
     * @throws IllegalArgumentException  if the array contains elements other
     *  than {@link Map.Entry} and an Array
     */
    public static Map toMap(Object[] array) {
        if (array == null) {
            throw new IllegalArgumentException("The array must not be null");            
        }
        Map map = new HashMap((int) (array.length * 1.5));
        for (int i = 0; i < array.length; i++) {
            Object object = array[i];
            if (object instanceof Map.Entry) {
                Map.Entry entry = (Map.Entry) object;
                map.put(entry.getKey(), entry.getValue());
            } else if (object instanceof Object[]) {
                Object[] entry = (Object[]) object;
                if (entry.length < 2) {
                    throw new IllegalArgumentException("Array element " + i + ", '" 
                        + object
                        + "', has a length less than 2");
                }
                map.put(entry[0], entry[1]);
            } else {
                throw new IllegalArgumentException("Array element " + i + ", '" 
                        + object
                        + "', is neither of type Map.Entry nor an Array");
            }
        }
        return map;
    }
    
//    /**
//     * <p>Output the array as a String.</p>
//     *
//     * <p>Multi-dimensional arrays are handled by the Object[] method.</p>
//     *
//     * <p>The format is that of Java source code, for example {1,2}.</p>
//     * 
//     * @param array  the array to get a toString for, must not be <code>null</code>
//     * @return a String representation of the array
//     * @throws IllegalArgumentException if the array is <code>null</code>
//     */
//    public static String toString(long[] array) {
//        return new ToStringBuilder(array, ToStringStyle.SIMPLE_STYLE).append(array).toString();
//    }
//    
//    /**
//     * <p>Output the array as a String.</p>
//     *
//     * <p>Multi-dimensional arrays are handled by the Object[] method.</p>
//     *
//     * <p>The format is that of Java source code, for example {1,2}.</p>
//     * 
//     * @param array  the array to get a toString for, must not be <code>null</code>
//     * @return a String representation of the array
//     * @throws IllegalArgumentException if the array is <code>null</code>
//     */
//    public static String toString(int[] array) {
//        return new ToStringBuilder(array, ToStringStyle.SIMPLE_STYLE).append(array).toString();
//    }
//    
//    /**
//     * <p>Output the array as a String.</p>
//     *
//     * <p>Multi-dimensional arrays are handled by the Object[] method.</p>
//     *
//     * <p>The format is that of Java source code, for example {1,2}.</p>
//     * 
//     * @param array  the array to get a toString for, must not be <code>null</code>
//     * @return a String representation of the array
//     * @throws IllegalArgumentException if the array is <code>null</code>
//     */
//    public static String toString(short[] array) {
//        return new ToStringBuilder(array, ToStringStyle.SIMPLE_STYLE).append(array).toString();
//    }
//    
//    /**
//     * <p>Output the array as a String.</p>
//     *
//     * <p>Multi-dimensional arrays are handled by the Object[] method.</p>
//     *
//     * <p>The format is that of Java source code, for example {1,2}.</p>
//     * 
//     * @param array  the array to get a toString for, must not be <code>null</code>
//     * @return a String representation of the array
//     * @throws IllegalArgumentException if the array is <code>null</code>
//     */
//    public static String toString(byte[] array) {
//        return new ToStringBuilder(array, ToStringStyle.SIMPLE_STYLE).append(array).toString();
//    }
//    
//    /**
//     * <p>Output the array as a String.</p>
//     *
//     * <p>Multi-dimensional arrays are handled by the Object[] method.</p>
//     *
//     * <p>The format is that of Java source code, for example {1.0,2.0}.</p>
//     * 
//     * @param array  the array to get a toString for, must not be <code>null</code>
//     * @return a String representation of the array
//     * @throws IllegalArgumentException if the array is <code>null</code>
//     */
//    public static String toString(double[] array) {
//        return new ToStringBuilder(array, ToStringStyle.SIMPLE_STYLE).append(array).toString();
//    }
//    
//    /**
//     * <p>Output the array as a String.</p>
//     *
//     * <p>Multi-dimensional arrays are handled by the Object[] method.</p>
//     *
//     * <p>The format is that of Java source code, for example {1.0,2.0}.</p>
//     *
//     * @param array  the array to get a toString for, must not be <code>null</code>
//     * @return a String representation of the array
//     * @throws IllegalArgumentException if the array is <code>null</code>
//     */
//    public static String toString(float[] array) {
//        return new ToStringBuilder(array, ToStringStyle.SIMPLE_STYLE).append(array).toString();
//    }
//    
//    /**
//     * <p>Output the array as a String.</p>
//     *
//     * <p>Multi-dimensional arrays are handled by the Object[] method.</p>
//     *
//     * <p>The format is that of Java source code, for example {true,false}.</p>
//     * 
//     * @param array  the array to get a toString for, must not be <code>null</code>
//     * @return a String representation of the array
//     * @throws IllegalArgumentException if the array is <code>null</code>
//     */
//    public static String toString(boolean[] array) {
//        return new ToStringBuilder(array, ToStringStyle.SIMPLE_STYLE).append(array).toString();
//    }
    
    //--------------------------------------------------------------------------

    /**
     * <p>Shallow clones an array returning a typecast result and handling
     * <code>null</code>.</p>
     *
     * <p>The objecs in the array are not cloned, thus there is no special
     * handling for multi-dimensional arrays.</p>
     * 
     * @param array  the array to shallow clone, may be <code>null</code>
     * @return the cloned array, or <code>null</code> if <code>null</code>
     *  passed in
     */
    public static Object[] clone(Object[] array) {
        if (array == null) {
            return null;
        }
        return (Object[]) array.clone();
    }
    
    /**
     * <p>Clones an array returning a typecast result and handling
     * <code>null</code>.</p>
     *
     * <p>Any multi-dimensional aspects of the arrays are ignored.</p>
     * 
     * @param array  the array to clone, may be <code>null</code>
     * @return the cloned array, or <code>null</code> if <code>null</code>
     *  passed in
     */
    public static long[] clone(long[] array) {
        if (array == null) {
            return null;
        }
        return (long[]) array.clone();
    }
    
    /**
     * <p>Clones an array returning a typecast result and handling
     * <code>null</code>.</p>
     *
     * <p>Any multi-dimensional aspects of the arrays are ignored.</p>
     * 
     * @param array  the array to clone, may be <code>null</code>
     * @return the cloned array, or <code>null</code> if <code>null</code>
     *  passed in
     */
    public static int[] clone(int[] array) {
        if (array == null) {
            return null;
        }
        return (int[]) array.clone();
    }
    
    /**
     * <p>Clones an array returning a typecast result and handling
     * <code>null</code>.</p>
     *
     * <p>Any multi-dimensional aspects of the arrays are ignored.</p>
     * 
     * @param array  the array to clone, may be <code>null</code>
     * @return the cloned array, or <code>null</code> if <code>null</code>
     *  passed in
     */
    public static short[] clone(short[] array) {
        if (array == null) {
            return null;
        }
        return (short[]) array.clone();
    }
    
    /**
     * <p>Clones an array returning a typecast result and handling
     * <code>null</code>.</p>
     *
     * <p>Any multi-dimensional aspects of the arrays are ignored.</p>
     * 
     * @param array  the array to clone, may be <code>null</code>
     * @return the cloned array, or <code>null</code> if <code>null</code>
     *  passed in
     */
    public static char[] clone(char[] array) {
        if (array == null) {
            return null;
        }
        return (char[]) array.clone();
    }
    
    /**
     * <p>Clones an array returning a typecast result and handling
     * <code>null</code>.</p>
     *
     * <p>Any multi-dimensional aspects of the arrays are ignored.</p>
     * 
     * @param array  the array to clone, may be <code>null</code>
     * @return the cloned array, or <code>null</code> if <code>null</code>
     *  passed in
     */
    public static byte[] clone(byte[] array) {
        if (array == null) {
            return null;
        }
        return (byte[]) array.clone();
    }
    
    /**
     * <p>Clones an array returning a typecast result and handling
     * <code>null</code>.</p>
     *
     * <p>Any multi-dimensional aspects of the arrays are ignored.</p>
     * 
     * @param array  the array to clone, may be <code>null</code>
     * @return the cloned array, or <code>null</code> if <code>null</code>
     *  passed in
     */
    public static double[] clone(double[] array) {
        if (array == null) {
            return null;
        }
        return (double[]) array.clone();
    }
    
    /**
     * <p>Clones an array returning a typecast result and handling
     * <code>null</code>.</p>
     *
     * <p>Any multi-dimensional aspects of the arrays are ignored.</p>
     * 
     * @param array  the array to clone, may be <code>null</code>
     * @return the cloned array, or <code>null</code> if <code>null</code>
     *  passed in
     */
    public static float[] clone(float[] array) {
        if (array == null) {
            return null;
        }
        return (float[]) array.clone();
    }
    
    /**
     * <p>Clones an array returning a typecast result and handling
     * <code>null</code>.</p>
     *
     * <p>Any multi-dimensional aspects of the arrays are ignored.</p>
     * 
     * @param array  the array to clone, may be <code>null</code>
     * @return the cloned array, or <code>null</code> if <code>null</code>
     *  passed in
     */
    public static boolean[] clone(boolean[] array) {
        if (array == null) {
            return null;
        }
        return (boolean[]) array.clone();
    }

    //--------------------------------------------------------------------------

    /**
     * <p>Checks whether two arrays are the same length, treating
     * <code>null</code> arrays as length <code>0</code>.
     *
     * <p>Any multi-dimensional aspects of the arrays are ignored.</p>
     * 
     * @param array1 the first array, may be <code>null</code>
     * @param array2 the second array, may be <code>null</code>
     * @return <code>true</code> if length of arrays matches, treating
     *  <code>null</code> as an empty array
     */    
    public static boolean isSameLength(Object[] array1, Object[] array2) {
        if ((array1 == null && array2 != null && array2.length > 0) ||
            (array2 == null && array1 != null && array1.length > 0) ||
            (array1 != null && array2 != null && array1.length != array2.length)) {
                return false;
        }
        return true;
    }
    
    /**
     * <p>Checks whether two arrays are the same length, treating
     * <code>null</code> arrays as length <code>0</code>.</p>
     *
     * <p>Any multi-dimensional aspects of the arrays are ignored.</p>
     * 
     * @param array1 the first array, may be <code>null</code>
     * @param array2 the second array, may be <code>null</code>
     * @return <code>true</code> if length of arrays matches, treating
     *  <code>null</code> as an empty array
     */
    public static boolean isSameLength(long[] array1, long[] array2) {
        if ((array1 == null && array2 != null && array2.length > 0) ||
            (array2 == null && array1 != null && array1.length > 0) ||
            (array1 != null && array2 != null && array1.length != array2.length)) {
                return false;
        }
        return true;
    }
    
    /**
     * <p>Checks whether two arrays are the same length, treating
     * <code>null</code> arrays as length <code>0</code>.</p>
     *
     * <p>Any multi-dimensional aspects of the arrays are ignored.</p>
     * 
     * @param array1 the first array, may be <code>null</code>
     * @param array2 the second array, may be <code>null</code>
     * @return <code>true</code> if length of arrays matches, treating
     *  <code>null</code> as an empty array
     */
    public static boolean isSameLength(int[] array1, int[] array2) {
        if ((array1 == null && array2 != null && array2.length > 0) ||
            (array2 == null && array1 != null && array1.length > 0) ||
            (array1 != null && array2 != null && array1.length != array2.length)) {
                return false;
        }
        return true;
    }
    
    /**
     * <p>Checks whether two arrays are the same length, treating
     * <code>null</code> arrays as length <code>0</code>.</p>
     *
     * <p>Any multi-dimensional aspects of the arrays are ignored.</p>
     * 
     * @param array1 the first array, may be <code>null</code>
     * @param array2 the second array, may be <code>null</code>
     * @return <code>true</code> if length of arrays matches, treating
     *  <code>null</code> as an empty array
     */
    public static boolean isSameLength(short[] array1, short[] array2) {
        if ((array1 == null && array2 != null && array2.length > 0) ||
            (array2 == null && array1 != null && array1.length > 0) ||
            (array1 != null && array2 != null && array1.length != array2.length)) {
                return false;
        }
        return true;
    }
    
    /**
     * <p>Checks whether two arrays are the same length, treating
     * <code>null</code> arrays as length <code>0</code>.</p>
     *
     * <p>Any multi-dimensional aspects of the arrays are ignored.</p>
     * 
     * @param array1 the first array, may be <code>null</code>
     * @param array2 the second array, may be <code>null</code>
     * @return <code>true</code> if length of arrays matches, treating
     *  <code>null</code> as an empty array
     */
    public static boolean isSameLength(char[] array1, char[] array2) {
        if ((array1 == null && array2 != null && array2.length > 0) ||
            (array2 == null && array1 != null && array1.length > 0) ||
            (array1 != null && array2 != null && array1.length != array2.length)) {
                return false;
        }
        return true;
    }
    
    /**
     * <p>Checks whether two arrays are the same length, treating
     * <code>null</code> arrays as length <code>0</code>.</p>
     *
     * <p>Any multi-dimensional aspects of the arrays are ignored.</p>
     * 
     * @param array1 the first array, may be <code>null</code>
     * @param array2 the second array, may be <code>null</code>
     * @return <code>true</code> if length of arrays matches, treating
     *  <code>null</code> as an empty array
     */
    public static boolean isSameLength(byte[] array1, byte[] array2) {
        if ((array1 == null && array2 != null && array2.length > 0) ||
            (array2 == null && array1 != null && array1.length > 0) ||
            (array1 != null && array2 != null && array1.length != array2.length)) {
                return false;
        }
        return true;
    }
    
    /**
     * <p>Checks whether two arrays are the same length, treating
     * <code>null</code> arrays as length <code>0</code>.</p>
     *
     * <p>Any multi-dimensional aspects of the arrays are ignored.</p>
     * 
     * @param array1 the first array, may be <code>null</code>
     * @param array2 the second array, may be <code>null</code>
     * @return <code>true</code> if length of arrays matches, treating
     *  <code>null</code> as an empty array
     */
    public static boolean isSameLength(double[] array1, double[] array2) {
        if ((array1 == null && array2 != null && array2.length > 0) ||
            (array2 == null && array1 != null && array1.length > 0) ||
            (array1 != null && array2 != null && array1.length != array2.length)) {
                return false;
        }
        return true;
    }
    
    /**
     * <p>Checks whether two arrays are the same length, treating
     * <code>null</code> arrays as length <code>0</code>.</p>
     *
     * <p>Any multi-dimensional aspects of the arrays are ignored</p>.
     * 
     * @param array1 the first array, may be <code>null</code>
     * @param array2 the second array, may be <code>null</code>
     * @return <code>true</code> if length of arrays matches, treating
     *  <code>null</code> as an empty array
     */
    public static boolean isSameLength(float[] array1, float[] array2) {
        if ((array1 == null && array2 != null && array2.length > 0) ||
            (array2 == null && array1 != null && array1.length > 0) ||
            (array1 != null && array2 != null && array1.length != array2.length)) {
                return false;
        }
        return true;
    }
    
    /**
     * <p>Checks whether two arrays are the same length, treating
     * <code>null</code> arrays as length <code>0</code>.</p>
     *
     * <p>Any multi-dimensional aspects of the arrays are ignored.</p>
     * 
     * @param array1 the first array, may be <code>null</code>
     * @param array2 the second array, may be <code>null</code>
     * @return <code>true</code> if length of arrays matches, treating
     *  <code>null</code> as an empty array
     */
    public static boolean isSameLength(boolean[] array1, boolean[] array2) {
        if ((array1 == null && array2 != null && array2.length > 0) ||
            (array2 == null && array1 != null && array1.length > 0) ||
            (array1 != null && array2 != null && array1.length != array2.length)) {
                return false;
        }
        return true;
    }
    
    /**
     * <p>Checks whether two arrays are the same type taking into account
     * multi-dimensional arrays.</p>
     * 
     * <p>Primitive arrays may be compared using this method too.</p>
     * 
     * @param array1 the first array, must not be <code>null</code>
     * @param array2 the second array, must not be <code>null</code>
     * @return <code>true</code> if type of arrays matches
     * @throws IllegalArgumentException if either array is <code>null</code>
     */    
    public static boolean isSameType(Object array1, Object array2) {
        if (array1 == null || array2 == null) {
            throw new IllegalArgumentException("The array must not be null");
        }
        return array1.getClass().getName().equals(array2.getClass().getName());
    }
    
    /** 
     * Reverses the order of the given array.
     * <p>
     * There is no special handling for multi-dimensional arrays.
     * <p>
     * The method does nothing if <code>null</code> is passed in.
     * 
     * @param array  the array to reverse
     */
    public static void reverse(Object[] array) {
        if (array == null) {
            return;
        }
        int i = 0;
        int j = array.length - 1;
        Object tmp;
        while (j > i) {
            tmp = array[j];
            array[j] = array[i];
            array[i] = tmp;
            j--;
            i++;
        }
    }

    /**
     * Reverses the order of the given array.
     * <p>
     * The method does nothing if <code>null</code> is passed in.
     * 
     * @param array  the array to reverse
     */
    public static void reverse(long[] array) {
        if (array == null) {
            return;
        }
        int i = 0;
        int j = array.length - 1;
        long tmp;
        while (j > i) {
            tmp = array[j];
            array[j] = array[i];
            array[i] = tmp;
            j--;
            i++;
        }
    }

    /**
     * Reverses the order of the given array.
     * <p>
     * The method does nothing if <code>null</code> is passed in.
     * 
     * @param array  the array to reverse
     */
    public static void reverse(int[] array) {
        if (array == null) {
            return;
        }
        int i = 0;
        int j = array.length - 1;
        int tmp;
        while (j > i) {
            tmp = array[j];
            array[j] = array[i];
            array[i] = tmp;
            j--;
            i++;
        }
    }

    /**
     * Reverses the order of the given array.
     * <p>
     * There is no special handling for multi-dimensional arrays.
     * 
     * @param array  the array to reverse
     */
    public static void reverse(short[] array) {
        if (array == null) {
            return;
        }
        int i = 0;
        int j = array.length - 1;
        short tmp;
        while (j > i) {
            tmp = array[j];
            array[j] = array[i];
            array[i] = tmp;
            j--;
            i++;
        }
    }

    /**
     * Reverses the order of the given array.
     * <p>
     * The method does nothing if <code>null</code> is passed in.
     * 
     * @param array  the array to reverse
     */
    public static void reverse(char[] array) {
        if (array == null) {
            return;
        }
        int i = 0;
        int j = array.length - 1;
        char tmp;
        while (j > i) {
            tmp = array[j];
            array[j] = array[i];
            array[i] = tmp;
            j--;
            i++;
        }
    }

    /**
     * Reverses the order of the given array.
     * <p>
     * The method does nothing if <code>null</code> is passed in.
     * 
     * @param array  the array to reverse
     */
    public static void reverse(byte[] array) {
        if (array == null) {
            return;
        }
        int i = 0;
        int j = array.length - 1;
        byte tmp;
        while (j > i) {
            tmp = array[j];
            array[j] = array[i];
            array[i] = tmp;
            j--;
            i++;
        }
    }

    /**
     * Reverses the order of the given array.
     * <p>
     * The method does nothing if <code>null</code> is passed in.
     * 
     * @param array  the array to reverse
     */
    public static void reverse(double[] array) {
        if (array == null) {
            return;
        }
        int i = 0;
        int j = array.length - 1;
        double tmp;
        while (j > i) {
            tmp = array[j];
            array[j] = array[i];
            array[i] = tmp;
            j--;
            i++;
        }
    }

    /**
     * Reverses the order of the given array.
     * <p>
     * The method does nothing if <code>null</code> is passed in.
     * 
     * @param array  the array to reverse
     */
    public static void reverse(float[] array) {
        if (array == null) {
            return;
        }
        int i = 0;
        int j = array.length - 1;
        float tmp;
        while (j > i) {
            tmp = array[j];
            array[j] = array[i];
            array[i] = tmp;
            j--;
            i++;
        }
    }

    /**
     * Reverses the order of the given array.
     * <p>
     * The method does nothing if <code>null</code> is passed in.
     * 
     * @param array  the array to reverse
     */
    public static void reverse(boolean[] array) {
        if (array == null) {
            return;
        }
        int i = 0;
        int j = array.length - 1;
        boolean tmp;
        while (j > i) {
            tmp = array[j];
            array[j] = array[i];
            array[i] = tmp;
            j--;
            i++;
        }
    }

}
