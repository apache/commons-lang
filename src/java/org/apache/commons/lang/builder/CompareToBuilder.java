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
package org.apache.commons.lang.builder;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

import org.apache.commons.lang.NumberUtils;
/** 
 * <code>CompareTo</code> generation routines.
 * <p>
 * This class provides methods to build a good <comde>compareTo()</code> method for any class.
 * It is consistent with the <code>equals</code> and <code>hashcode</code> built
 * with EqualsBuilder and HashCodeBuilder.
 * <p>
 * Two object that compare equal using equals should compare equals using
 * compareTo.
 * <p>
 * All relevant fields should be included in the calculation of the comparison. Derived
 * fields may be ignored. The same fields, in the same order, should be used in
 * both <code>compareTo</code> and <code>equals</code>.
 * <p>
 * Typical use for the code is as follows:
 *
 * <pre>
 *  public int comapareTo(Object o) {
 *    MyClass rhs = (MyClass) o;
 *    return new CompareToBuilder()
 *                 .append(field1, rhs.field1)
 *                 .append(field2, rhs.field2)
 *                 .appendb(field3, rhs.field3)
 *                 .toComparison();
 *  }
 * </pre>
 * <p>
 * Alternatively, there is a method that uses reflection to determine
 * the fields to test. Because these fields are usually private, the method,
 * <code>reflectionCompare</code>, uses <code>Field.setAccessible</code> to change
 * the visibility of the fields. This will fail under a security manager,
 * unless the appropriate permissions are set. It is also slower than testing
 * explicitly.
 * <p>
 * A typical invocation for this method would look like:
 * <pre>
 * public int compareTo(Object o) {
 *   return CompareToBuilder.reflectionCompare(this, obj);
 * }
 * </pre>
 * @author <a href="mailto:steve.downey@netfolio.com">Steve Downey</a>
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @version $Id: CompareToBuilder.java,v 1.3 2002/09/22 09:18:32 scolebourne Exp $
 */
public class CompareToBuilder {
    /**
     * If the fields tested are equals.
     */
    private int comparison;

    /**
     * Constructor for CompareToBuilder.
     * Starts off assuming that the objects are equal.
     * @see java.lang.Object#Object()
     */
    public CompareToBuilder() {
        super();
        comparison = 0;
    }

    //-------------------------------------------------------------------------
    
    /** 
     * This method uses reflection to determine the ordering between two objects.
     * <p>
     * It uses Field.setAccessible to gain access to private fields. This means
     * that it will throw a security exception if run under a security manger, if
     * the permissions are not set up.
     * It is also not as efficient as testing explicitly.
     * Transient members will be not be tested, as they are likely derived
     * fields, and not part of the value of the object.
     * Static fields will not be tested.
     * @param lhs  Left Hand Side
     * @param rhs  Right Hand Side
     * @return int  a negative integer, zero, or a positive integer as this 
     * object is less than, equal to, or greater than the specified object.
     * @throws ClassCastException - if the specified object's type prevents it 
     * from being compared to this Object.
     */
    public static int reflectionCompare(Object lhs, Object rhs) {
        return reflectionCompare(lhs, rhs, false);
    }

    /**
     * This method uses reflection to determine if the two object are equal. 
     * <p>
     * It uses Field.setAccessible to gain access to private fields. This means
     * that it will throw a security exception if run under a security manger, if
     * the permissions are not set up.
     * It is also not as efficient as testing explicitly. 
     * If the TestTransients parameter is set to true, transient members will be
     * tested, otherwise they are ignored, as they are likely derived fields, and
     * not part of the value of the object. 
     * Static fields will not be tested.
     * 
     * @param lhs  Left Hand Side
     * @param rhs  Right Hand Side
     * @param testTransients  whether to include transient fields
     * @return int - a negative integer, zero, or a positive integer as this 
     * object is less than, equal to, or greater than the specified object.
     * @throws ClassCastException - if the specified object's type prevents it 
     * from being compared to this Object.
     */
    public static int reflectionCompare(Object lhs, Object rhs, 
            boolean testTransients) {
        if (lhs == rhs) {
            return 0;
        }
        if (lhs == null || rhs == null) {
            throw new NullPointerException();
        }
        Class c1 = lhs.getClass();
        if (!c1.isInstance(rhs)) {
            throw new ClassCastException();
        }
        Field[] fields = c1.getDeclaredFields();
        Field.setAccessible(fields, true);
        CompareToBuilder compareToBuilder = new CompareToBuilder();
        for (int i = 0; i < fields.length && compareToBuilder.comparison == 0; ++i) {
            Field f = fields[i];
            if (testTransients || !Modifier.isTransient(f.getModifiers())) {
                if (!Modifier.isStatic(f.getModifiers())) {
                    try {
                        compareToBuilder.append(f.get(lhs), f.get(rhs));
                    } catch (IllegalAccessException e) {
                        //this can't happen. Would get a Security exception instead
                        //throw a runtime exception in case the impossible happens.
                        throw new InternalError("Unexpected IllegalAccessException");
                    }
                }
            }
        }
        return compareToBuilder.toComparison();
    }

    //-------------------------------------------------------------------------
    
    /** Test if two <code>Object</code>s are equal using either the
     * <code>compareTo</code> method, or native comparison if the Objects are
     * actually arrays.
     * <p>
     * The objects must be <code>Comparable</code>. If they are not, the method
     * will throw a <code>ClassCastException</code>.
     * @param lhs - Left Hand Side
     * @param rhs - Right Hand Side
     * @return CompareToBuilder - used to chain calls.
     * @throws ClassCastException - if the specified object's type prevents it
     * from being compared to this Object.
     */
    public CompareToBuilder append(Object lhs, Object rhs) {
        if (comparison != 0) {
            return this;
        }
        if (lhs == rhs) {
            return this;
        }
        if (lhs == null || rhs == null) {
            throw new NullPointerException();
        }
        Class lhsClass = lhs.getClass();
        if (!lhsClass.isArray()) {
            //the simple case, not an array, just test the element 
            comparison = ((Comparable) lhs).compareTo(rhs);
        } else {
            //'Switch' on type of array, to dispatch to the correct handler
            // This handles multi dimensional arrays
            if (lhs instanceof long[]) {
                append((long[]) lhs, (long[]) rhs);
            } else if (lhs instanceof int[]) {
                append((int[]) lhs, (int[]) rhs);
            } else if (lhs instanceof short[]) {
                append((short[]) lhs, (short[]) rhs);
            } else if (lhs instanceof char[]) {
                append((char[]) lhs, (char[]) rhs);
            } else if (lhs instanceof byte[]) {
                append((byte[]) lhs, (byte[]) rhs);
            } else if (lhs instanceof double[]) {
                append((double[]) lhs, (double[]) rhs);
            } else if (lhs instanceof float[]) {
                append((float[]) lhs, (float[]) rhs);
            } else if (lhs instanceof boolean[]) {
                append((boolean[]) lhs, (boolean[]) rhs);
            } else {
                // Not an array of primitives
                append((Object[]) lhs, (Object[]) rhs);
            }
        }
        return this;
    }

    /**
     * Test if two <code>long</code>s are <, > or ==.
     * @param lhs - Left Hand Side
     * @param rhs - Right Hand Side
     * @return CompareToBuilder - used to chain calls.
     */
    public CompareToBuilder append(long lhs, long rhs) {
        if (comparison != 0) {
            return this;
        }
        comparison = ((lhs < rhs) ? -1 : ((lhs > rhs) ? 1 : 0));
        return this;
    }

    /**
     * Test if two <code>int</code>s are <, > or ==.
     * @param lhs - Left Hand Side
     * @param rhs - Right Hand Side
     * @return CompareToBuilder - used to chain calls.
     */
    public CompareToBuilder append(int lhs, int rhs) {
        if (comparison != 0) {
            return this;
        }
        comparison = ((lhs < rhs) ? -1 : ((lhs > rhs) ? 1 : 0));
        return this;
    }

    /**
     * Test if two <code>short</code>s are <, > or ==.
     * @param lhs - Left Hand Side
     * @param rhs - Right Hand Side
     * @return CompareToBuilder - used to chain calls.
     */
    public CompareToBuilder append(short lhs, short rhs) {
        if (comparison != 0) {
            return this;
        }
        comparison = ((lhs < rhs) ? -1 : ((lhs > rhs) ? 1 : 0));
        return this;
    }

    /**
     * Test if two <code>char</code>s are <, > or ==.
     * @param lhs - Left Hand Side
     * @param rhs - Right Hand Side
     * @return CompareToBuilder - used to chain calls.
     */
    public CompareToBuilder append(char lhs, char rhs) {
        if (comparison != 0) {
            return this;
        }
        comparison = ((lhs < rhs) ? -1 : ((lhs > rhs) ? 1 : 0));
        return this;
    }

    /**
     * Test if two <code>byte</code>s are <, > or ==.
     * @param lhs - Left Hand Side
     * @param rhs - Right Hand Side
     * @return CompareToBuilder - used to chain calls.
     */
    public CompareToBuilder append(byte lhs, byte rhs) {
        if (comparison != 0) {
            return this;
        }
        comparison = ((lhs < rhs) ? -1 : ((lhs > rhs) ? 1 : 0));
        return this;
    }

    /**
     * Test if two <code>double</code>s are <, > or ==. This handles NaNs, 
     * Infinties, and -0.0. It is compatible with the hash code generated by 
     * <code>HashCodeBuilder</code>.
     * @param lhs - Left Hand Side
     * @param rhs - Right Hand Side
     * @return CompareToBuilder - used to chain calls.
     */
    public CompareToBuilder append(double lhs, double rhs) {
        if (comparison != 0) {
            return this;
        }
        comparison = NumberUtils.compare(lhs, rhs);
        return this;
    }

    /**
     * Test if two <code>double</code>s are <, > or ==. This handles NaNs, 
     * Infinties, and -0.0. It is compatible with the hash code generated by 
     * <code>HashCodeBuilder</code>.
     * @param lhs - Left Hand Side
     * @param rhs - Right Hand Side
     * @return CompareToBuilder - used to chain calls.
     */
    public CompareToBuilder append(float lhs, float rhs) {
        if (comparison != 0) {
            return this;
        }
        comparison = NumberUtils.compare(lhs, rhs);
        return this;
    }

    /**
     * Test if two <code>booleans</code>s are <, > or ==.
     * @param lhs - Left Hand Side
     * @param rhs - Right Hand Side
     * @return CompareToBuilder - used to chain calls.
      */
    public CompareToBuilder append(boolean lhs, boolean rhs) {
        if (comparison != 0) {
            return this;
        }
        if (lhs == rhs) {
            return this;
        }
        if (lhs == false) {
            comparison = -1;
        } else {
            comparison = +1;
        }
        return this;
    }

    /**
     * Performs a deep comparison of two object arrays. This also will be
     * called for the top level of multi-dimensional, ragged, and multi-typed
     * arrays. If two arrays are of different lengths, and all elements of the
     * shorter array are equal to the elements in the longer array, the longer
     * array is the greater. This is dictionary, or lexical, ordering.
     * @param lhs - Left Hand Side
     * @param rhs - Right Hand Side
     * @return CompareToBuilder - used to chain calls.
     * @throws ClassCastException - if the specified object's type prevents it 
     * from being compared to this Object.
     */
    public CompareToBuilder append(Object[] lhs, Object[] rhs) {
        if (comparison != 0) {
            return this;
        }
        if (lhs == rhs) {
            return this;
        }
        if (lhs == null || rhs == null) {
            throw new NullPointerException();
        }

        int length = (lhs.length < rhs.length) ? lhs.length : rhs.length;
        for (int i = 0; i < length && comparison == 0; ++i) {
            Class lhsClass = lhs[i].getClass();
            if (!lhsClass.isInstance(rhs[i])) {
                throw new ClassCastException();
            }
            append(lhs[i], rhs[i]);
        }
        if (comparison == 0 && lhs.length != rhs.length) {
            comparison = (lhs.length < rhs.length) ? -1 : +1;
        }
        return this;
    }

    /**
     * Deep comparison of array of <code>long</code> Length and all values
     *  are compared. The method append(long, long) is used.
     * @param lhs - Left Hand Side
     * @param rhs - Right Hand Side
     * @return CompareToBuilder - used to chain calls.
     */
    public CompareToBuilder append(long[] lhs, long[] rhs) {
        if (comparison != 0) {
            return this;
        }
        if (lhs == rhs) {
            return this;
        }
        if (lhs == null || rhs == null) {
            throw new NullPointerException();
        }
        int length = (lhs.length < rhs.length) ? lhs.length : rhs.length;
        for (int i = 0; i < length && comparison == 0; ++i) {
            append(lhs[i], rhs[i]);
        }
        if (comparison == 0 && lhs.length != rhs.length) {
            comparison = (lhs.length < rhs.length) ? -1 : +1;
        }
        return this;
    }

    /**
     * Deep comparison of array of <code>int</code> Length and all values
     *  are compared. The method append(int, int) is used.
     * @param lhs - Left Hand Side
     * @param rhs - Right Hand Side
     * @return CompareToBuilder - used to chain calls.
     */
    public CompareToBuilder append(int[] lhs, int[] rhs) {
        if (comparison != 0) {
            return this;
        }
        if (lhs == rhs) {
            return this;
        }
        if (lhs == null || rhs == null) {
            throw new NullPointerException();
        }
        int length = (lhs.length < rhs.length) ? lhs.length : rhs.length;
        for (int i = 0; i < length && comparison == 0; ++i) {
            append(lhs[i], rhs[i]);
        }
        if (comparison == 0 && lhs.length != rhs.length) {
            comparison = (lhs.length < rhs.length) ? -1 : +1;
        }
        return this;
    }

    /**
     * Deep comparison of array of <code>short</code> Length and all values
     *  are compared. The method append(short, short) is used.
     * @param lhs - Left Hand Side
     * @param rhs - Right Hand Side
     * @return CompareToBuilder - used to chain calls.
     */
    public CompareToBuilder append(short[] lhs, short[] rhs) {
        if (comparison != 0) {
            return this;
        }
        if (lhs == rhs) {
            return this;
        }
        if (lhs == null || rhs == null) {
            throw new NullPointerException();
        }
        int length = (lhs.length < rhs.length) ? lhs.length : rhs.length;
        for (int i = 0; i < length && comparison == 0; ++i) {
            append(lhs[i], rhs[i]);
        }
        if (comparison == 0 && lhs.length != rhs.length) {
            comparison = (lhs.length < rhs.length) ? -1 : +1;
        }
        return this;
    }

    /**
     * Deep comparison of array of <code>char</code> Length and all values
     *  are compared. The method append(char, char) is used.
     * @param lhs - Left Hand Side
     * @param rhs - Right Hand Side
     * @return CompareToBuilder - used to chain calls.
     */
    public CompareToBuilder append(char[] lhs, char[] rhs) {
        if (comparison != 0) {
            return this;
        }
        if (lhs == rhs) {
            return this;
        }
        if (lhs == null || rhs == null) {
            throw new NullPointerException();
        }
        int length = (lhs.length < rhs.length) ? lhs.length : rhs.length;
        for (int i = 0; i < length && comparison == 0; ++i) {
            append(lhs[i], rhs[i]);
        }
        if (comparison == 0 && lhs.length != rhs.length) {
            comparison = (lhs.length < rhs.length) ? -1 : +1;
        }
        return this;
    }

    /**
     * Deep comparison of array of <code>byte</code> Length and all values
     *  are compared. The method append(byte, byte) is used.
     * @param lhs - Left Hand Side
     * @param rhs - Right Hand Side
     * @return CompareToBuilder - used to chain calls.
     */
    public CompareToBuilder append(byte[] lhs, byte[] rhs) {
        if (comparison != 0) {
            return this;
        }
        if (lhs == rhs) {
            return this;
        }
        if (lhs == null || rhs == null) {
            throw new NullPointerException();
        }
        int length = (lhs.length < rhs.length) ? lhs.length : rhs.length;
        for (int i = 0; i < length && comparison == 0; ++i) {
            append(lhs[i], rhs[i]);
        }
        if (comparison == 0 && lhs.length != rhs.length) {
            comparison = (lhs.length < rhs.length) ? -1 : +1;
        }
        return this;
    }

    /**
     * Deep comparison of array of <code>double</code> Length and all values
     *  are compared. The method append(double, double) is used.
     * @param lhs - Left Hand Side
     * @param rhs - Right Hand Side
     * @return CompareToBuilder - used to chain calls.
     */
    public CompareToBuilder append(double[] lhs, double[] rhs) {
        if (comparison != 0) {
            return this;
        }
        if (lhs == rhs) {
            return this;
        }
        if (lhs == null || rhs == null) {
            throw new NullPointerException();
        }
        int length = (lhs.length < rhs.length) ? lhs.length : rhs.length;
        for (int i = 0; i < length && comparison == 0; ++i) {
            append(lhs[i], rhs[i]);
        }
        if (comparison == 0 && lhs.length != rhs.length) {
            comparison = (lhs.length < rhs.length) ? -1 : +1;
        }
        return this;
    }

    /**
     * Deep comparison of array of <code>float</code> Length and all values
     *  are compared. The method append(float, float) is used.
     * @param lhs - Left Hand Side
     * @param rhs - Right Hand Side
     * @return CompareToBuilder - used to chain calls.
     */
    public CompareToBuilder append(float[] lhs, float[] rhs) {
        if (comparison != 0) {
            return this;
        }
        if (lhs == rhs) {
            return this;
        }
        if (lhs == null || rhs == null) {
            throw new NullPointerException();
        }
        int length = (lhs.length < rhs.length) ? lhs.length : rhs.length;
        for (int i = 0; i < length && comparison == 0; ++i) {
            append(lhs[i], rhs[i]);
        }
        if (comparison == 0 && lhs.length != rhs.length) {
            comparison = (lhs.length < rhs.length) ? -1 : +1;
        }
        return this;
    }

    /**
     * Deep comparison of array of <code>boolean</code> Length and all values
     *  are compared. The method append(boolean, boolean) is used.
     * @param lhs - Left Hand Side
     * @param rhs - Right Hand Side
     * @return CompareToBuilder - used to chain calls.
     */
    public CompareToBuilder append(boolean[] lhs, boolean[] rhs) {
        if (comparison != 0) {
            return this;
        }
        if (lhs == rhs) {
            return this;
        }
        if (lhs == null || rhs == null) {
            throw new NullPointerException();
        }
        int length = (lhs.length < rhs.length) ? lhs.length : rhs.length;
        for (int i = 0; i < length && comparison == 0; ++i) {
            append(lhs[i], rhs[i]);
        }
        if (comparison == 0 && lhs.length != rhs.length) {
            comparison = (lhs.length < rhs.length) ? -1 : +1;
        }
        return this;
    }

    /**
     * Return a negative integer if the object is less than, a positive 
     * integer if the object is greater than, or 0 if the object is equal.
     * 
     * @return int - a negative integer, zero, or a positive integer as this 
     * object is less than, equal to, or greater than the specified object.
     */
    public int toComparison() {
        return comparison;
    }

}

