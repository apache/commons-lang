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

import java.lang.reflect.AccessibleObject;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.Comparator;

import org.apache.commons.lang.math.NumberUtils;

/** 
 * <p><code>CompareTo</code> generation routines.</p>
 *
 * <p>This class provides methods to build a good <code>compareTo</code>
 * method for any class. It is consistent with the <code>equals()</code> and
 * <code>hashcode()</code> built with {@link EqualsBuilder} and
 * {@link HashCodeBuilder}.</p>
 *
 * <p>Two Objects that compare equal using <code>equals()</code> should normally
 * also compare equal using <code>compareTo()</code></p>.
 *
 * <p>All relevant fields should be included in the calculation of the
 * comparison. Derived fields may be ignored. The same fields, in the same
 * order, should be used in both <code>compareTo()</code> and
 * <code>equals()</code>.</p>
 *
 * <p>Typical use for the code is as follows:</p>
 *
 * <pre>
 *  public int compareTo(Object o) {
 *    MyClass rhs = (MyClass) o;
 *    return new CompareToBuilder()
 *                 .appendSuper(super.compareTo(o)
 *                 .append(field1, rhs.field1)
 *                 .append(field2, rhs.field2)
 *                 .append(field3, rhs.field3)
 *                 .toComparison();
 *  }
 * </pre>
 *
 * <p>Alternatively, there is a method that uses reflection to determine
 * the fields to test. Because these fields are usually private, the method,
 * <code>reflectionCompare</code>, uses <code>AccessibleObject.setAccessible</code> to change
 * the visibility of the fields. This will fail under a security manager,
 * unless the appropriate permissions are set. It is also slower than testing
 * explicitly.</p>
 *
 * <p>A typical invocation for this method would look like:</p>
 * <pre>
 * public int compareTo(Object o) {
 *   return CompareToBuilder.reflectionCompare(this, obj);
 * }
 * </pre>
 *
 * @author <a href="mailto:steve.downey@netfolio.com">Steve Downey</a>
 * @author Stephen Colebourne
 * @author Gary Gregory
 * @author Pete Gieser
 * @since 1.0
 * @version $Id: CompareToBuilder.java,v 1.19 2003/07/21 23:32:41 scolebourne Exp $
 */
public class CompareToBuilder {
    
    /**
     * If the fields tested are equals.
     */
    private int comparison;

    /**
     * <p>Constructor for CompareToBuilder.</p>
     *
     * <p>Starts off assuming that the objects are equal. Multiple calls are 
     * then made to the various append methods, followed by a call to 
     * {@link #toComparison} to get the result.</p>
     */
    public CompareToBuilder() {
        super();
        comparison = 0;
    }

    //-------------------------------------------------------------------------
    
    /** 
     * <p>This method uses reflection to determine the ordering between two
     * Objects.</p>
     *
     * <p>It uses <code>AccessibleObject.setAccessible</code> to gain access to private
     * fields. This means that it will throw a security exception if run under
     * a security manager, if the permissions are not set up correctly. It is
     * also not as efficient as testing explicitly.</p>
     *
     * <p>Transient members will be not be tested, as they are likely derived
     * fields, and not part of the value of the object.</p>
     *
     * <p>Static fields will not be tested. Superclass fields will be included.</p>
     *
     * @param lhs  <code>this</code> object
     * @param rhs  the other object
     * @return a negative integer, zero, or a positive integer as this 
     *  Object is less than, equal to, or greater than the specified Object.
     * @throws NullPointerException  if either (but not both) parameter is
     *  <code>null</code>
     * @throws ClassCastException  if the specified Object's type prevents it
     *  from being compared to this Object.
     */
    public static int reflectionCompare(Object lhs, Object rhs) {
        return reflectionCompare(lhs, rhs, false, null);
    }

    /**
     * <p>This method uses reflection to determine if the two Objects are
     * equal.</p>
     *
     * <p>It uses <code>AccessibleObject.setAccessible</code> to gain access to private
     * fields. This means that it will throw a security exception if run under
     * a security manager, if  the permissions are not set up correctly. It is
     * also not as efficient as testing explicitly.</p>
     *
     * <p>If the <code>testTransients</code> is set to <code>true</code>,
     * transient members will be tested, otherwise they are ignored, as they
     * are likely derived fields, and not part of the value of the object.</p>
     *
     * <p>Static fields will not be tested. Superclass fields will be included.</p>
     * 
     * @param lhs  <code>this</code> object
     * @param rhs  the other object
     * @param testTransients  whether to include transient fields
     * @return a negative integer, zero, or a positive integer as this 
     *  Object is less than, equal to, or greater than the specified Object.
     * @throws NullPointerException  if either (but not both) parameter is
     *  <code>null</code>
     * @throws ClassCastException  if the specified Object's type prevents it
     *  from being compared to this Object.
     */
    public static int reflectionCompare(Object lhs, Object rhs, boolean testTransients) {
        return reflectionCompare(lhs, rhs, testTransients, null);
    }

    /**
     * <p>This method uses reflection to determine if the two Objects are
     * equal.</p>
     *
     * <p>It uses <code>AccessibleObject.setAccessible</code> to gain access to private
     * fields. This means that it will throw a security exception if run under
     * a security manager, if  the permissions are not set up correctly. It is
     * also not as efficient as testing explicitly.</p>
     *
     * <p>If the <code>testTransients</code> is set to <code>true</code>,
     * transient members will be tested, otherwise they are ignored, as they
     * are likely derived fields, and not part of the value of the object.</p>
     *
     * <p>Static fields will not be included. Superclass fields will be appended
     * up to and including the specified superclass. A null superclass is treated
     * as java.lang.Object.</p>
     * 
     * @param lhs  <code>this</code> object
     * @param rhs  the other object
     * @param testTransients  whether to include transient fields
     * @param reflectUpToClass  the superclass to reflect up to (inclusive), may be null
     * @return a negative integer, zero, or a positive integer as this 
     *  Object is less than, equal to, or greater than the specified Object.
     * @throws NullPointerException  if either (but not both) parameter is
     *  <code>null</code>
     * @throws ClassCastException  if the specified Object's type prevents it
     *  from being compared to this Object.
     */
    public static int reflectionCompare(Object lhs, Object rhs, boolean testTransients, Class reflectUpToClass) {
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
        CompareToBuilder compareToBuilder = new CompareToBuilder();
        reflectionAppend(lhs, rhs, c1, compareToBuilder, testTransients);
        while (c1.getSuperclass() != null && c1 != reflectUpToClass) {
            c1 = c1.getSuperclass();
            reflectionAppend(lhs, rhs, c1, compareToBuilder, testTransients);
        }
        return compareToBuilder.toComparison();
    }

    /**
     * <p>Appends the fields and values defined by the given object of the
     * given Class.</p>
     * 
     * @param lhs  the left hand object
     * @param rhs  the right hand object
     * @param clazz  the class to append details of
     * @param builder  the builder to append to
     * @param useTransients  whether to test transient fields
     */
    private static void reflectionAppend(
        Object lhs,
        Object rhs,
        Class clazz,
        CompareToBuilder builder,
        boolean useTransients) {
        Field[] fields = clazz.getDeclaredFields();
        AccessibleObject.setAccessible(fields, true);
        for (int i = 0; i < fields.length && builder.comparison == 0; i++) {
            Field f = fields[i];
            if ((f.getName().indexOf('$') == -1)
                && (useTransients || !Modifier.isTransient(f.getModifiers()))
                && (!Modifier.isStatic(f.getModifiers()))) {
                try {
                    builder.append(f.get(lhs), f.get(rhs));
                } catch (IllegalAccessException e) {
                    //this can't happen. Would get a Security exception instead
                    //throw a runtime exception in case the impossible happens.
                    throw new InternalError("Unexpected IllegalAccessException");
                }
            }
        }
    }

    //-------------------------------------------------------------------------
    
    /**
     * <p>Adds the result of super.hashCode() to this builder.</p>
     *
     * @param superCompareTo  the result of calling <code>super.compareTo()</code>
     * @return CompareToBuilder - used to chain calls.
     */
    public CompareToBuilder appendSuper(int superCompareTo) {
        if (comparison != 0) {
            return this;
        }
        comparison = superCompareTo;
        return this;
    }
    
    //-------------------------------------------------------------------------
    
    /**
     * <p>Comparison of two Object references.</p>
     *
     * <ol>
     *  <li>Check if Objects are same using <code>==</code></li>
     *  <li>Check if either is null, a null object is less than a non-null</li>
     *  <li>Check the object contents</li>
     * </ol>
     * 
     * <p>The first parameter to be compared must either be an array or implement
     * <code>Comparable</code>.</p>
     *
     * @param lhs  the Object from <code>this</code> object
     * @param rhs  the Object from the other object
     * @return CompareToBuilder - used to chain calls.
     * @throws ClassCastException if the specified Object's type prevents it
     * from being compared to this Object.
     */
    public CompareToBuilder append(Object lhs, Object rhs) {
        return append(lhs, rhs, null);
    }

    /**
     * <p>Comparison of two Object references.</p>
     * <ol>
     *  <li>Check if Objects are same using <code>==</code></li>
     *  <li>Check if either is null, a null object is less than a non-null</li>
     *  <li>Check the object contents</li>
     * </ol>
     * 
     * <p>If the first parameter to be compared is an array, the array methods will
     * be used. Otherwise the comparator will be used. If the comparator is null, 
     * the <code>lhs</code> will be cast to <code>Comparable</code>.</p>
     *
     * @param lhs  the Object from <code>this</code> object
     * @param rhs  the Object from the other object
     * @param comparator  the comparator to use to compare the objects,
     *  <code>null</code> means to treat the <code>lhs</code> as <code>Comparable</code>.
     * @return CompareToBuilder - used to chain calls.
     * @throws ClassCastException if the specified Object's type prevents it
     * from being compared to this Object.
     */
    public CompareToBuilder append(Object lhs, Object rhs, Comparator comparator) {
        if (comparison != 0) {
            return this;
        }
        if (lhs == rhs) {
            return this;
        }
        if (lhs == null) {
            comparison = -1;
            return this;
        }
        if (rhs == null) {
            comparison = +1;
            return this;
        }
        Class lhsClass = lhs.getClass();
        if (lhsClass.isArray()) {
            // 'Switch' on type of array, to dispatch to the correct handler
            // This handles multi dimensional arrays
            // this could throw a ClassCastException is rhs is not the correct array type
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
                // this could throw a ClassCastException is rhs is not an array
                append((Object[]) lhs, (Object[]) rhs, comparator);
            }
        } else {
            // the simple case, not an array, just test the element
            if (comparator == null) {
                comparison = ((Comparable) lhs).compareTo(rhs);
            } else {
                comparison = comparator.compare(lhs, rhs);
            }
        }
        return this;
    }

    /**
     * <p>Test if two <code>long</code>s are &lt;, &gt; or ==.</p>
     *
     * @param lhs  the <code>long</code> from <code>this</code> object
     * @param rhs  the <code>long</code> from the other object
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
     * <p>Test if two <code>int</code>s are &lt;, &gt; or ==.</p>
     *
     * @param lhs  the <code>int</code> from <code>this</code> object
     * @param rhs  the <code>int</code> from the other object
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
     * <p>Test if two <code>short</code>s are &lt;, &gt; or ==.</p>
     * 
     * @param lhs  the <code>short</code> from <code>this</code> object
     * @param rhs  the <code>short</code> from the other object
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
     * <p>Test if two <code>char</code>s are &lt;, &gt; or ==.</p>
     *
     * @param lhs  the <code>char</code> from <code>this</code> object
     * @param rhs  the <code>char</code> from the other object
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
     * <p>Test if two <code>byte</code>s are &lt, &gt; or ==.</p>
     * 
     * @param lhs  the <code>byte</code> from <code>this</code> object
     * @param rhs  the <code>byte</code> from the other object
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
     * <p>Test if two <code>double</code>s are &lt;, &gt; or ==.</p>
     *
     * <p>This handles NaNs, Infinties, and <code>-0.0</code>.</p>
     *
     * <p>It is compatible with the hash code generated by
     * <code>HashCodeBuilder</code>.</p>
     *
     * @param lhs  the <code>double</code> from <code>this</code> object
     * @param rhs  the <code>double</code> from the other object
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
     * <p>Test if two <code>float</code>s are &lt;, &gt; or ==.</p>
     *
     * <p>This handles NaNs, Infinties, and <code>-0.0</code>.</p>
     *
     * <p>It is compatible with the hash code generated by
     * <code>HashCodeBuilder</code>.</p>
     *
     * @param lhs  the <code>float</code> from <code>this</code> object
     * @param rhs  the <code>float</code> from the other object
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
     * <p>Test if two <code>booleans</code>s are &lt;, &gt; or ==.</p>
     *
     * @param lhs  the <code>boolean</code> from <code>this</code> object
     * @param rhs  the <code>boolean</code> from the other object
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
     * <p>Deep comparison of an <code>Object</code> array.</p>
     *
     * <ol>
     *  <li>Check if arrays are the same using <code>==</code></li>
     *  <li>Check if either is <code>null</code>, a null array is less than a non-null</li>
     *  <li>Check array length, a short length array is less than a long length array</li>
     *  <li>Check array contents element by element using {@link #append(long, long)}</li>
     * </ol>
     *
     * <p>This method will also will be called for the top level of multi-dimensional,
     * ragged, and multi-typed arrays.</p>
     *
     * @param lhs  array from <code>this</code> object
     * @param rhs  array from the other object
     * @return CompareToBuilder - used to chain calls.
     * @throws ClassCastException  if the specified Object's type prevents it
     *  from being compared to this Object.
     */
    public CompareToBuilder append(Object[] lhs, Object[] rhs) {
        return append(lhs, rhs, null);
    }
    
    /**
     * <p>Deep comparison of an <code>Object</code> array.</p>
     *
     * <ol>
     *  <li>Check if arrays are the same using <code>==</code></li>
     *  <li>Check if either is <code>null</code>, a null array is less than a non-null</li>
     *  <li>Check array length, a shorter length array is less than a longer length array</li>
     *  <li>Check array contents element by element using {@link #append(Object, Object, Comparator)}</li>
     * </ol>
     *
     * <p>This method will also will be called for the top level of multi-dimensional,
     * ragged, and multi-typed arrays.</p>
     *
     * @param lhs  array from <code>this</code> object
     * @param rhs  array from the other object
     * @param comparator  the comparator to use to compare the objects,
     *  <code>null</code> means to treat the <code>lhs</code> as <code>Comparable</code>.
     * @return CompareToBuilder - used to chain calls.
     * @throws ClassCastException  if the specified Object's type prevents it
     *  from being compared to this Object.
     */
    public CompareToBuilder append(Object[] lhs, Object[] rhs, Comparator comparator) {
        if (comparison != 0) {
            return this;
        }
        if (lhs == rhs) {
            return this;
        }
        if (lhs == null) {
            comparison = -1;
            return this;
        }
        if (rhs == null) {
            comparison = +1;
            return this;
        }
        if (lhs.length != rhs.length) {
            comparison = (lhs.length < rhs.length) ? -1 : +1;
            return this;
        }
        for (int i = 0; i < lhs.length && comparison == 0; i++) {
            append(lhs[i], rhs[i], comparator);
        }
        return this;
    }

    /**
     * <p>Deep comparison of a <code>long</code> array.</p>
     *
     * <ol>
     *  <li>Check if arrays are the same using <code>==</code></li>
     *  <li>Check if either is <code>null</code>, a null array is less than a non-null</li>
     *  <li>Check array length, a shorter length array is less than a longer length array</li>
     *  <li>Check array contents element by element using {@link #append(long, long)}</li>
     * </ol>
     *
     * @param lhs  array from <code>this</code> object
     * @param rhs  array from the other object
     * @return CompareToBuilder - used to chain calls.
     */
    public CompareToBuilder append(long[] lhs, long[] rhs) {
        if (comparison != 0) {
            return this;
        }
        if (lhs == rhs) {
            return this;
        }
        if (lhs == null) {
            comparison = -1;
            return this;
        }
        if (rhs == null) {
            comparison = +1;
            return this;
        }
        if (lhs.length != rhs.length) {
            comparison = (lhs.length < rhs.length) ? -1 : +1;
            return this;
        }
        for (int i = 0; i < lhs.length && comparison == 0; i++) {
            append(lhs[i], rhs[i]);
        }
        return this;
    }

    /**
     * <p>Deep comparison of an <code>int</code> array.</p>
     *
     * <ol>
     *  <li>Check if arrays are the same using <code>==</code></li>
     *  <li>Check if either is <code>null</code>, a null array is less than a non-null</li>
     *  <li>Check array length, a shorter length array is less than a longer length array</li>
     *  <li>Check array contents element by element using {@link #append(int, int)}</li>
     * </ol>
     *
     * @param lhs  array from <code>this</code> object
     * @param rhs  array from the other object
     * @return CompareToBuilder - used to chain calls.
     */
    public CompareToBuilder append(int[] lhs, int[] rhs) {
        if (comparison != 0) {
            return this;
        }
        if (lhs == rhs) {
            return this;
        }
        if (lhs == null) {
            comparison = -1;
            return this;
        }
        if (rhs == null) {
            comparison = +1;
            return this;
        }
        if (lhs.length != rhs.length) {
            comparison = (lhs.length < rhs.length) ? -1 : +1;
            return this;
        }
        for (int i = 0; i < lhs.length && comparison == 0; i++) {
            append(lhs[i], rhs[i]);
        }
        return this;
    }

    /**
     * <p>Deep comparison of a <code>short</code> array.</p>
     *
     * <ol>
     *  <li>Check if arrays are the same using <code>==</code></li>
     *  <li>Check if either is <code>null</code>, a null array is less than a non-null</li>
     *  <li>Check array length, a shorter length array is less than a longer length array</li>
     *  <li>Check array contents element by element using {@link #append(short, short)}</li>
     * </ol>
     *
     * @param lhs  array from <code>this</code> object
     * @param rhs  array from the other object
     * @return CompareToBuilder - used to chain calls.
     */
    public CompareToBuilder append(short[] lhs, short[] rhs) {
        if (comparison != 0) {
            return this;
        }
        if (lhs == rhs) {
            return this;
        }
        if (lhs == null) {
            comparison = -1;
            return this;
        }
        if (rhs == null) {
            comparison = +1;
            return this;
        }
        if (lhs.length != rhs.length) {
            comparison = (lhs.length < rhs.length) ? -1 : +1;
            return this;
        }
        for (int i = 0; i < lhs.length && comparison == 0; i++) {
            append(lhs[i], rhs[i]);
        }
        return this;
    }

    /**
     * <p>Deep comparison of a <code>char</code> array.</p>
     *
     * <ol>
     *  <li>Check if arrays are the same using <code>==</code></li>
     *  <li>Check if either is <code>null</code>, a null array is less than a non-null</li>
     *  <li>Check array length, a shorter length array is less than a longer length array</li>
     *  <li>Check array contents element by element using {@link #append(char, char)}</li>
     * </ol>
     *
     * @param lhs  array from <code>this</code> object
     * @param rhs  array from the other object
     * @return CompareToBuilder - used to chain calls.
     */
    public CompareToBuilder append(char[] lhs, char[] rhs) {
        if (comparison != 0) {
            return this;
        }
        if (lhs == rhs) {
            return this;
        }
        if (lhs == null) {
            comparison = -1;
            return this;
        }
        if (rhs == null) {
            comparison = +1;
            return this;
        }
        if (lhs.length != rhs.length) {
            comparison = (lhs.length < rhs.length) ? -1 : +1;
            return this;
        }
        for (int i = 0; i < lhs.length && comparison == 0; i++) {
            append(lhs[i], rhs[i]);
        }
        return this;
    }

    /**
     * <p>Deep comparison of a <code>byte</code> array.</p>
     *
     * <ol>
     *  <li>Check if arrays are the same using <code>==</code></li>
     *  <li>Check if either is <code>null</code>, a null array is less than a non-null</li>
     *  <li>Check array length, a shorter length array is less than a longer length array</li>
     *  <li>Check array contents element by element using {@link #append(byte, byte)}</li>
     * </ol>
     *
     * @param lhs  array from <code>this</code> object
     * @param rhs  array from the other object
     * @return CompareToBuilder - used to chain calls.
     */
    public CompareToBuilder append(byte[] lhs, byte[] rhs) {
        if (comparison != 0) {
            return this;
        }
        if (lhs == rhs) {
            return this;
        }
        if (lhs == null) {
            comparison = -1;
            return this;
        }
        if (rhs == null) {
            comparison = +1;
            return this;
        }
        if (lhs.length != rhs.length) {
            comparison = (lhs.length < rhs.length) ? -1 : +1;
            return this;
        }
        for (int i = 0; i < lhs.length && comparison == 0; i++) {
            append(lhs[i], rhs[i]);
        }
        return this;
    }

    /**
     * <p>Deep comparison of a <code>double</code> array.</p>
     *
     * <ol>
     *  <li>Check if arrays are the same using <code>==</code></li>
     *  <li>Check if either is <code>null</code>, a null array is less than a non-null</li>
     *  <li>Check array length, a shorter length array is less than a longer length array</li>
     *  <li>Check array contents element by element using {@link #append(double, double)}</li>
     * </ol>
     *
     * @param lhs  array from <code>this</code> object
     * @param rhs  array from the other object
     * @return CompareToBuilder - used to chain calls.
     */
    public CompareToBuilder append(double[] lhs, double[] rhs) {
        if (comparison != 0) {
            return this;
        }
        if (lhs == rhs) {
            return this;
        }
        if (lhs == null) {
            comparison = -1;
            return this;
        }
        if (rhs == null) {
            comparison = +1;
            return this;
        }
        if (lhs.length != rhs.length) {
            comparison = (lhs.length < rhs.length) ? -1 : +1;
            return this;
        }
        for (int i = 0; i < lhs.length && comparison == 0; i++) {
            append(lhs[i], rhs[i]);
        }
        return this;
    }

    /**
     * <p>Deep comparison of a <code>float</code> array.</p>
     *
     * <ol>
     *  <li>Check if arrays are the same using <code>==</code></li>
     *  <li>Check if either is <code>null</code>, a null array is less than a non-null</li>
     *  <li>Check array length, a shorter length array is less than a longer length array</li>
     *  <li>Check array contents element by element using {@link #append(float, float)}
     * </ol>
     *
     * @param lhs  array from <code>this</code> object
     * @param rhs  array from the other object
     * @return CompareToBuilder - used to chain calls.
     */
    public CompareToBuilder append(float[] lhs, float[] rhs) {
        if (comparison != 0) {
            return this;
        }
        if (lhs == rhs) {
            return this;
        }
        if (lhs == null) {
            comparison = -1;
            return this;
        }
        if (rhs == null) {
            comparison = +1;
            return this;
        }
        if (lhs.length != rhs.length) {
            comparison = (lhs.length < rhs.length) ? -1 : +1;
            return this;
        }
        for (int i = 0; i < lhs.length && comparison == 0; i++) {
            append(lhs[i], rhs[i]);
        }
        return this;
    }

    /**
     * <p>Deep comparison of a <code>boolean/code> array.</p>
     *
     * <ol>
     *  <li>Check if arrays are the same using <code>==</code></li>
     *  <li>Check if either is <code>null</code>, a null array is less than a non-null</li>
     *  <li>Check array length, a shorter length array is less than a longer length array</li>
     *  <li>Check array contents element by element using {@link #append(boolean, boolean)}</li>
     * </ol>
     *
     * @param lhs  array from <code>this</code> object
     * @param rhs  array from the other object
     * @return CompareToBuilder - used to chain calls.
     */
    public CompareToBuilder append(boolean[] lhs, boolean[] rhs) {
        if (comparison != 0) {
            return this;
        }
        if (lhs == rhs) {
            return this;
        }
        if (lhs == null) {
            comparison = -1;
            return this;
        }
        if (rhs == null) {
            comparison = +1;
            return this;
        }
        if (lhs.length != rhs.length) {
            comparison = (lhs.length < rhs.length) ? -1 : +1;
            return this;
        }
        for (int i = 0; i < lhs.length && comparison == 0; i++) {
            append(lhs[i], rhs[i]);
        }
        return this;
    }

    /**
     * <p>Return a negative integer if this <code>Object</code> is less
     * than, a positive integer if this <code>Object</code> is greater than,
     * or <code>0</code> if this <code>Object</code> is equal to the specified
     * Object.</p>
     * 
     * @return int - a negative integer, zero, or a positive integer as this 
     *  Object is less than, equal to, or greater than the specified Object.
     */
    public int toComparison() {
        return comparison;
    }

}

