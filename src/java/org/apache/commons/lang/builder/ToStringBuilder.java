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
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;

/**
 * <p><code>ToString</code> generation routine.</p>
 *
 * <p>This class enables a good <code>toString</code> to be built for any
 * class. This class aims to simplify the process by:</p>
 * <ul>
 * <li>allowing field names
 * <li>handling all types consistently
 * <li>handling nulls consistently
 * <li>outputting arrays and multi-dimensional arrays
 * <li>enabling the detail level to be controlled for Objects and Collections
 * </ul>
 *
 * <p>To use this class write code as follows:
 * <pre>
 * public class Person {
 *   String name;
 *   int age;
 *   boolean isSmoker;
 * 
 *   ...
 * 
 *   public String toString() {
 *     return new ToStringBuilder(this).
 *       append("name", name).
 *       append("age", age).
 *       append("smoker", smoker).
 *       toString();
 *   }
 * }
 * </pre>
 * <p>This will produce a toString of the format:
 * <code>Person@7f54[name=Stephen,age=29,smoker=false]</code></p>
 * 
 * <p>To add the superclass <code>toString</code>, use {@link #appendSuper}.
 * To append the <code>toString</code> from an object that is delegated
 * to (or any other object), use {@link #appendToString}.</p>
 *
 * <p>Alternatively, there is a method that uses reflection to determine
 * the fields to test. Because these fields are usually private, the method, 
 * <code>reflectionToString</code>, uses <code>Field.setAccessible</code> to
 * change the visibility of the fields. This will fail under a security manager,
 * unless the appropriate permissions are set up correctly. It is also
 * slower than testing explicitly.</p>
 *
 * <p>A typical invocation for this method would look like:</p>
 * <pre>
 * public String toString() {
 *   return ToStringBuilder.reflectionToString(this);
 * }
 * </pre>
 *
 * <p>The exact format of the <code>toString</code> is determined by
 * the {@link ToStringStyle} passed into the constructor.</p>
 *
 * @author Stephen Colebourne
 * @author Gary Gregory
 * @since 1.0
 * @version $Id: ToStringBuilder.java,v 1.15 2003/03/20 05:32:11 ggregory Exp $
 */
public class ToStringBuilder {

    /**
     * The default style of output to use
     */
    private static ToStringStyle defaultStyle = ToStringStyle.DEFAULT_STYLE;
    /**
     * Current toString buffer
     */
    private final StringBuffer buffer;
    /**
     * The style of output to use
     */
    private final ToStringStyle style;
    /**
     * The object being output
     */
    private final Object object;

    /**
     * <p>Constructor for <code>ToStringBuilder</code>.</p>
     *
     * <p>This constructor outputs using the default style set with
     * <code>setDefaultStyle</code>.</p>
     * 
     * @param object  the Object to build a <code>toString</code> for,
     *  must not be <code>null</code>
     * @throws IllegalArgumentException  if the Object passed in is
     *  <code>null</code>
     */
    public ToStringBuilder(Object object) {
        this(object, getDefaultStyle(), null);
    }

    /**
     * <p>Constructor for <code>ToStringBuilder</code> specifying the
     * output style.</p>
     *
     * <p>If the style is <code>null</code>, the default style is used.</p>
     * 
     * @param object  the Object to build a <code>toString</code> for,
     *  must not be <code>null</code>
     * @param style  the style of the <code>toString</code> to create,
     *  may be <code>null</code>
     * @throws IllegalArgumentException  if the Object passed in is
     *  <code>null</code>
     */
    public ToStringBuilder(Object object, ToStringStyle style) {
        this(object, style, null);
    }

    /**
     * <p>Constructor for <code>ToStringBuilder</code>.</p>
     *
     * <p>If the style is <code>null</code>, the default style is used.</p>
     *
     * <p>If the buffer is <code>null</code>, a new one is created.</p>
     * 
     * @param object  the Object to build a <code>toString</code> for,
     *  must not be <code>null</code>
     * @param style  the style of the <code>toString</code> to create,
     *  may be <code>null</code>
     * @param buffer  the <code>StringBuffer</code> to populate, may be
     *  <code>null</code>
     * @throws IllegalArgumentException  if the Object passed in is
     *  <code>null</code>
     */
    public ToStringBuilder(Object object, ToStringStyle style, StringBuffer buffer) {
        super();
        if (object == null) {
            throw new IllegalArgumentException("The object to create a toString for must not be null");
        }
        if (style == null) {
            style = getDefaultStyle();
        }
        if (buffer == null) {
            buffer = new StringBuffer(512);
        }
        this.buffer = buffer;
        this.style = style;
        this.object = object;

        style.appendStart(buffer, object);
    }

    //----------------------------------------------------------------------------

    /**
     * <p>Gets the default <code>ToStringStyle</code> to use.</p>
     *
     * <p>This could allow the <code>ToStringStyle</code> to be
     * controlled for an entire application with one call.</p>
     *
     * <p>This might be used to have a verbose
     * <code>ToStringStyle</code> during development and a compact
     * <code>ToStringStyle</code> in production.</p>
     * 
     * @return the default <code>ToStringStyle</code>
     */
    public static ToStringStyle getDefaultStyle() {
        return defaultStyle;
    }

    /**
     * <p>Sets the default <code>ToStringStyle</code> to use.</p>
     * 
     * @param style  the default <code>ToStringStyle</code>
     * @throws IllegalArgumentException if the style is <code>null</code>
     */
    public static void setDefaultStyle(ToStringStyle style) {
        if (style == null) {
            throw new IllegalArgumentException("The style must not be null");
        }
        defaultStyle = style;
    }

    //-------------------------------------------------------------------------

    /**
     * <p>This method uses reflection to build a suitable
     * <code>toString<code> using the default <code>ToStringStyle</code>.
     *
     * <p>It uses <code>Field.setAccessible</code> to gain access to private
     * fields. This means that it will throw a security exception if run
     * under a security manger, if the permissions are not set up correctly.
     * It is also not as efficient as testing explicitly.</p>
     *
     * <p>Transient members will be not be included, as they are likely derived.</p>
     *
     * <p>Static fields will not be included. Superclass fields will be appended.</p>
     *
     * @param object  the Object to be output
     * @return the String result
     * @throws IllegalArgumentException if the Object is <code>null</code>
     */
    public static String reflectionToString(Object object) {
        return reflectionToString(object, null, false, null);
    }

    /**
     * <p>This method uses reflection to build a suitable
     * <code>toString</code>.</p>
     *
     * <p>It uses <code>Field.setAccessible</code> to gain access to private
     * fields. This means that it will throw a security exception if run
     * under a security manger, if the permissions are not set up correctly.
     * It is also not as efficient as testing explicitly.</p>
     *
     * <p>Transient members will be not be included, as they are likely
     * derived.</p>
     *
     * <p>Static fields will not be included. Superclass fields will be appended.</p>
     *
     * <p>If the style is <code>null</code>, the default
     * <code>ToStringStyle</code> is used.</p>
     * 
     * @param object  the Object to be output
     * @param style  the style of the <code>toString</code> to create,
     *  may be <code>null</code>
     * @return the String result
     * @throws IllegalArgumentException if the Object or
     *  <code>ToStringStyle</code> is <code>null</code>
     */
    public static String reflectionToString(Object object, ToStringStyle style) {
        return reflectionToString(object, style, false, null);
    }

    /**
     * <p>This method uses reflection to build a suitable
     * <code>toString</code>.</p>
     *
     * <p>It uses <code>Field.setAccessible</code> to gain access to private
     * fields. This means that it will throw a security exception if run
     * under a security manger, if the permissions are not set up correctly.
     * It is also not as efficient as testing explicitly. </p>
     *
     * <p>If the <code>outputTransients</code> is <code>true</code>,
     * transient members will be output, otherwise they are ignored,
     * as they are likely derived fields, and not part of the value of the
     * Object.</p>
     *
     * <p>Static fields will not be included. Superclass fields will be appended.</p>
     *
     * <p>
     * If the style is <code>null</code>, the default
     * <code>ToStringStyle</code> is used.</p>
     * 
     * @param object  the Object to be output
     * @param style  the style of the <code>toString</code> to create,
     *  may be <code>null</code>
     * @param outputTransients  whether to include transient fields
     * @return the String result
     * @throws IllegalArgumentException if the Object is <code>null</code>
     */
    public static String reflectionToString(Object object, ToStringStyle style, boolean outputTransients) {
        return reflectionToString(object, style, outputTransients, null);
    }

    /**
     * <p>This method uses reflection to build a suitable
     * <code>toString</code>.</p>
     *
     * <p>It uses <code>Field.setAccessible</code> to gain access to private
     * fields. This means that it will throw a security exception if run
     * under a security manger, if the permissions are not set up correctly.
     * It is also not as efficient as testing explicitly. </p>
     *
     * <p>If the <code>outputTransients</code> is <code>true</code>,
     * transient members will be output, otherwise they are ignored,
     * as they are likely derived fields, and not part of the value of the
     * Object.</p>
     *
     * <p>Static fields will not be included. Superclass fields will be appended
     * up to and including the specified superclass. A null superclass is treated
     * as java.lang.Object.</p>
     *
     * <p>If the style is <code>null</code>, the default
     * <code>ToStringStyle</code> is used.</p>
     * 
     * @param object  the Object to be output
     * @param style  the style of the <code>toString</code> to create,
     *  may be <code>null</code>
     * @param outputTransients  whether to include transient fields
     * @param reflectUpToClass  the superclass to reflect up to (inclusive), may be null
     * @return the String result
     * @throws IllegalArgumentException if the Object is <code>null</code>
     */
    public static String reflectionToString(Object object, ToStringStyle style, boolean outputTransients, Class reflectUpToClass) {
        if (style == null) {
            style = getDefaultStyle();
        }
        if (object == null) {
            return style.getNullText();
        }
        if (style == null) {
            style = getDefaultStyle();
        }
        ToStringBuilder builder = new ToStringBuilder(object, style);
        Class clazz = object.getClass();
        reflectionAppend(object, clazz, builder, outputTransients);
        while (clazz.getSuperclass() != null && clazz != reflectUpToClass) {
            clazz = clazz.getSuperclass();
            reflectionAppend(object, clazz, builder, outputTransients);
        }
        return builder.toString();
    }

    /**
     * Appends the fields and values defined by the given object of the
     * given Class.
     * 
     * @param object  the object to append details of
     * @param clazz  the class of object parameter
     * @param builder  the builder to append to
     * @param useTransients  whether to output transient fields
     */
    private static void reflectionAppend(Object object, Class clazz, ToStringBuilder builder, boolean useTransients) {
        if (clazz.isArray()) {
            reflectionAppendArray(object, clazz, builder);
            return;
        }
        Field[] fields = clazz.getDeclaredFields();
        Field.setAccessible(fields, true);
        for (int i = 0; i < fields.length; i++) {
            Field f = fields[i];
            if ((f.getName().indexOf('$') == -1)
                && (useTransients || !Modifier.isTransient(f.getModifiers()))
                && (!Modifier.isStatic(f.getModifiers()))) {
                try {
                    builder.append(f.getName(), f.get(object));
                } catch (IllegalAccessException ex) {
                    //this can't happen. Would get a Security exception instead
                    //throw a runtime exception in case the impossible happens.
                    throw new InternalError("Unexpected IllegalAccessException: " + ex.getMessage());
                }
            }
        }
    }

    /**
     * Appends the array elements in the given <code>Object</code> of the
     * given <code>Class</code> to a <code>ToStringBuilder</code>.
     * 
     * @param object  the array object to append details of
     * @param clazz  the array class of the object parameter
     * @param builder  the builder to append to
     */
    private static void reflectionAppendArray(Object object, Class clazz, ToStringBuilder builder) {
        try {
            // A multi-dimension array invokes the append(Object) method.
            // A single-dimension array of primitive type pt invokes the append(pt[]) method.
            builder.getClass().getDeclaredMethod("append", new Class[] { clazz.getComponentType().isArray() ? Object.class : clazz }).invoke(
                builder,
                new Object[] { object });
        } catch (SecurityException e) {
            // "This cannot happen"
            throw new InternalError("Unexpected SecurityException: " + e.getMessage());
        } catch (NoSuchMethodException e) {
            // "This cannot happen"
            throw new InternalError("Unexpected NoSuchMethodException: " + e.getMessage());
        } catch (IllegalArgumentException e) {
            // Method.invoke exception
            // "This cannot happen"
            throw new InternalError("Unexpected IllegalArgumentException: " + e.getMessage());
        } catch (IllegalAccessException e) {
            // Method.invoke exception
            // "This cannot happen"
            throw new InternalError("Unexpected IllegalAccessException: " + e.getMessage());
        } catch (InvocationTargetException e) {
            // Method.invoke exception
            // "This cannot happen"
            throw new InternalError("Unexpected InvocationTargetException: " + e.getMessage());
        }
    }

    //----------------------------------------------------------------------------

    /**
     * <p>Append the <code>toString</code> from the superclass.</p>
     * 
     * <p>This method asumes that the superclass uses the same <code>ToStringStyle</code>
     * as this one.</p>
     * 
     * <p>If the <code>superToString</code> is null, no change is made.</p>
     *
     * @param superToString  the result of <code>super.toString()</code>
     * @return this
     */
    public ToStringBuilder appendSuper(String superToString) {
        if (superToString != null) {
            style.appendSuper(buffer, superToString);
        }
        return this;
    }

    /**
     * <p>Append the <code>toString</code> from another object.</p>
     * 
     * <p>This method is useful where a class delegates most of the implementation of
     * it's properties to another class. You can then call toString() on the other 
     * class and pass the result into this method.</p>
     * 
     * <pre>
     *   private AnotherObject delegate;
     *   private String fieldInThisClass;
     * 
     *   public String toString() {
     *     return new ToStringBuilder(this).
     *       appendToString(delegate.toString()).
     *       append(fieldInThisClass).
     *       toString();
     *   }</pre>
     * 
     * <p>This method asumes that the other object uses the same <code>ToStringStyle</code>
     * as this one.</p>
     * 
     * <p>If the <code>toString</code> is null, no change is made.</p>
     *
     * @param toString  the result of <code>toString()</code> on another object
     * @return this
     */
    public ToStringBuilder appendToString(String toString) {
        if (toString != null) {
            style.appendToString(buffer, toString);
        }
        return this;
    }

    //----------------------------------------------------------------------------

    /**
     * <p>Append to the <code>toString</code> an <code>Object</code>
     * value.</p>
     *
     * @param object  the value to add to the <code>toString</code>
     * @return this
     */
    public ToStringBuilder append(Object object) {
        style.append(buffer, null, object, null);
        return this;
    }

    /**
     * <p>Append to the <code>toString</code> an <code>Object</code>
     * value.</p>
     *
     * @param fieldName  the field name
     * @param object  the value to add to the <code>toString</code>
     * @return this
     */
    public ToStringBuilder append(String fieldName, Object object) {
        style.append(buffer, fieldName, object, null);
        return this;
    }

    /**
     * <p>Append to the <code>toString</code> an <code>Object</code>
     * value.</p>
     *
     * @param fieldName  the field name
     * @param object  the value to add to the <code>toString</code>
     * @param fullDetail  <code>true</code> for detail,
     *  <code>false</code> for summary info
     * @return this
     */
    public ToStringBuilder append(String fieldName, Object object, boolean fullDetail) {
        style.append(buffer, fieldName, object, new Boolean(fullDetail));
        return this;
    }

    //----------------------------------------------------------------------------

    /**
     * <p>Append to the <code>toString</code> a <code>long</code>
     * value.</p>
     *
     * @param value  the value to add to the <code>toString</code>
     * @return this
     */
    public ToStringBuilder append(long value) {
        style.append(buffer, null, value);
        return this;
    }

    /**
     * <p>Append to the <code>toString</code> a <code>long</code>
     * value.</p>
     *
     * @param fieldName  the field name
     * @param value  the value to add to the <code>toString</code>
     * @return this
     */
    public ToStringBuilder append(String fieldName, long value) {
        style.append(buffer, fieldName, value);
        return this;
    }

    //----------------------------------------------------------------------------

    /**
     * <p>Append to the <code>toString</code> an <code>int</code>
     * value.</p>
     *
     * @param value  the value to add to the <code>toString</code>
     * @return this
     */
    public ToStringBuilder append(int value) {
        style.append(buffer, null, value);
        return this;
    }

    /**
     * <p>Append to the <code>toString</code> an <code>int</code>
     * value.</p>
     *
     * @param fieldName  the field name
     * @param value  the value to add to the <code>toString</code>
     * @return this
     */
    public ToStringBuilder append(String fieldName, int value) {
        style.append(buffer, fieldName, value);
        return this;
    }

    //----------------------------------------------------------------------------

    /**
     * <p>Append to the <code>toString</code> an <code>short</code>
     * value.</p>
     *
     * @param value  the value to add to the <code>toString</code>
     * @return this
     */
    public ToStringBuilder append(short value) {
        style.append(buffer, null, value);
        return this;
    }

    /**
     * <p>Append to the <code>toString</code> an <code>short</code>
     * value.</p>
     *
     * @param fieldName  the field name
     * @param value  the value to add to the <code>toString</code>
     * @return this
     */
    public ToStringBuilder append(String fieldName, short value) {
        style.append(buffer, fieldName, value);
        return this;
    }

    //----------------------------------------------------------------------------

    /**
     * <p>Append to the <code>toString</code> an <code>char</code>
     * value.</p>
     *
     * @param value  the value to add to the <code>toString</code>
     * @return this
     */
    public ToStringBuilder append(char value) {
        style.append(buffer, null, value);
        return this;
    }

    /**
     * <p>Append to the <code>toString</code> an <code>char</code>
     * value.</p>
     *
     * @param fieldName  the field name
     * @param value  the value to add to the <code>toString</code>
     * @return this
     */
    public ToStringBuilder append(String fieldName, char value) {
        style.append(buffer, fieldName, value);
        return this;
    }

    //----------------------------------------------------------------------------

    /**
     * <p>Append to the <code>toString</code> an <code>byte</code>
     * value.</p>
     *
     * @param value  the value to add to the <code>toString</code>
     * @return this
     */
    public ToStringBuilder append(byte value) {
        style.append(buffer, null, value);
        return this;
    }

    /**
     * <p>Append to the <code>toString</code> an <code>byte</code>
     * value.</p>
     *
     * @param fieldName  the field name
     * @param value  the value to add to the <code>toString</code>
     * @return this
     */
    public ToStringBuilder append(String fieldName, byte value) {
        style.append(buffer, fieldName, value);
        return this;
    }

    //----------------------------------------------------------------------------

    /**
     * <p>Append to the <code>toString</code> an <code>double</code>
     * value.</p>
     *
     * @param value  the value to add to the <code>toString</code>
     * @return this
     */
    public ToStringBuilder append(double value) {
        style.append(buffer, null, value);
        return this;
    }

    /**
     * <p>Append to the <code>toString</code> an <code>double</code>
     * value.</p>
     *
     * @param fieldName  the field name
     * @param value  the value to add to the <code>toString</code>
     * @return this
     */
    public ToStringBuilder append(String fieldName, double value) {
        style.append(buffer, fieldName, value);
        return this;
    }

    //----------------------------------------------------------------------------

    /**
     * <p>Append to the <code>toString</code> an <code>float</code>
     * value.</p>
     *
     * @param value  the value to add to the <code>toString</code>
     * @return this
     */
    public ToStringBuilder append(float value) {
        style.append(buffer, null, value);
        return this;
    }

    /**
     * <p>Append to the <code>toString</code> an <code>float</code>
     * value.</p>
     *
     * @param fieldName  the field name
     * @param value  the value to add to the <code>toString</code>
     * @return this
     */
    public ToStringBuilder append(String fieldName, float value) {
        style.append(buffer, fieldName, value);
        return this;
    }

    //----------------------------------------------------------------------------

    /**
     * <p>Append to the <code>toString</code> an <code>boolean</code>
     * value.</p>
     *
     * @param value  the value to add to the <code>toString</code>
     * @return this
     */
    public ToStringBuilder append(boolean value) {
        style.append(buffer, null, value);
        return this;
    }

    /**
     * <p>Append to the <code>toString</code> an <code>boolean</code>
     * value.</p>
     *
     * @param fieldName  the field name
     * @param value  the value to add to the <code>toString</code>
     * @return this
     */
    public ToStringBuilder append(String fieldName, boolean value) {
        style.append(buffer, fieldName, value);
        return this;
    }

    //----------------------------------------------------------------------------

    /**
     * <p>Append to the <code>toString</code> an <code>Object</code>
     * array.</p>
     *
     * @param array  the array to add to the <code>toString</code>
     * @return this
     */
    public ToStringBuilder append(Object[] array) {
        style.append(buffer, null, array, null);
        return this;
    }

    /**
     * <p>Append to the <code>toString</code> an <code>Object</code>
     * array.</p>
     *
     * @param fieldName  the field name
     * @param array  the array to add to the <code>toString</code>
     * @return this
     */
    public ToStringBuilder append(String fieldName, Object[] array) {
        style.append(buffer, fieldName, array, null);
        return this;
    }

    /**
     * <p>Append to the <code>toString</code> an <code>Object</code>
     * array.</p>
     *
     * <p>A boolean parameter controls the level of detail to show.
     * Setting <code>true</code> will output the array in full. Setting
     * <code>false</code> will output a summary, typically the size of
     * the array.</p>
     *
     * @param fieldName  the field name
     * @param array  the array to add to the <code>toString</code>
     * @param fullDetail  <code>true</code> for detail, <code>false</code>
     *  for summary info
     * @return this
     */
    public ToStringBuilder append(String fieldName, Object[] array, boolean fullDetail) {
        style.append(buffer, fieldName, array, new Boolean(fullDetail));
        return this;
    }

    //----------------------------------------------------------------------------

    /**
     * <p>Append to the <code>toString</code> a <code>long</code>
     * array.</p>
     *
     * @param array  the array to add to the <code>toString</code>
     * @return this
     */
    public ToStringBuilder append(long[] array) {
        style.append(buffer, null, array, null);
        return this;
    }

    /**
     * <p>Append a <code>hashCode</code> for a <code>long</code>
     * array.</p>
     *
     * @param fieldName  the field name
     * @param array  the array to add to the <code>hashCode</code>
     * @return this
     */
    public ToStringBuilder append(String fieldName, long[] array) {
        style.append(buffer, fieldName, array, null);
        return this;
    }

    /**
     * <p>Append to the <code>toString</code> a <code>long</code>
     * array.</p>
     *
     * <p>A boolean parameter controls the level of detail to show.
     * Setting <code>true</code> will output the array in full. Setting
     * <code>false</code> will output a summary, typically the size of
     * the array.</p>
     *
     * @param fieldName  the field name
     * @param array  the array to add to the <code>toString</code>
     * @param fullDetail  <code>true</code> for detail, <code>false</code>
     *  for summary info
     * @return this
     */
    public ToStringBuilder append(String fieldName, long[] array, boolean fullDetail) {
        style.append(buffer, fieldName, array, new Boolean(fullDetail));
        return this;
    }

    //----------------------------------------------------------------------------

    /**
     * <p>Append to the <code>toString</code> a <code>int</code>
     * array.</p>
     *
     * @param array  the array to add to the <code>toString</code>
     * @return this
     */
    public ToStringBuilder append(int[] array) {
        style.append(buffer, null, array, null);
        return this;
    }

    /**
     * <p>Append a <code>hashCode</code> for an <code>int</code>
     * array.</p>
     *
     * @param fieldName  the field name
     * @param array  the array to add to the <code>hashCode</code>
     * @return this
     */
    public ToStringBuilder append(String fieldName, int[] array) {
        style.append(buffer, fieldName, array, null);
        return this;
    }

    /**
     * <p>Append to the <code>toString</code> an <code>int</code>
     * array.</p>
     *
     * <p>A boolean parameter controls the level of detail to show.
     * Setting <code>true</code> will output the array in full. Setting
     * <code>false</code> will output a summary, typically the size of
     * the array.</p>
     *
     * @param fieldName  the field name
     * @param array  the array to add to the <code>toString</code>
     * @param fullDetail  <code>true</code> for detail, <code>false</code>
     *  for summary info
     * @return this
     */
    public ToStringBuilder append(String fieldName, int[] array, boolean fullDetail) {
        style.append(buffer, fieldName, array, new Boolean(fullDetail));
        return this;
    }

    //----------------------------------------------------------------------------

    /**
     * <p>Append to the <code>toString</code> a <code>short</code>
     * array.</p>
     *
     * @param array  the array to add to the <code>toString</code>
     * @return this
     */
    public ToStringBuilder append(short[] array) {
        style.append(buffer, null, array, null);
        return this;
    }

    /**
     * <p>Append a <code>hashCode</code> for a <code>short</code>
     * array.</p>
     *
     * @param fieldName  the field name
     * @param array  the array to add to the <code>hashCode</code>
     * @return this
     */
    public ToStringBuilder append(String fieldName, short[] array) {
        style.append(buffer, fieldName, array, null);
        return this;
    }

    /**
     * <p>Append to the <code>toString</code> a <code>short</code>
     * array.</p>
     *
     * <p>A boolean parameter controls the level of detail to show.
     * Setting <code>true</code> will output the array in full. Setting
     * <code>false</code> will output a summary, typically the size of
     * the array.
     *
     * @param fieldName  the field name
     * @param array  the array to add to the <code>toString</code>
     * @param fullDetail  <code>true</code> for detail, <code>false</code>
     *  for summary info
     * @return this
     */
    public ToStringBuilder append(String fieldName, short[] array, boolean fullDetail) {
        style.append(buffer, fieldName, array, new Boolean(fullDetail));
        return this;
    }

    //----------------------------------------------------------------------------

    /**
     * <p>Append to the <code>toString</code> a <code>char</code>
     * array.</p>
     *
     * @param array  the array to add to the <code>toString</code>
     * @return this
     */
    public ToStringBuilder append(char[] array) {
        style.append(buffer, null, array, null);
        return this;
    }

    /**
     * <p>Append a <code>hashCode</code> for a <code>char</code>
     * array.</p>
     *
     * @param fieldName  the field name
     * @param array  the array to add to the <code>hashCode</code>
     * @return this
     */
    public ToStringBuilder append(String fieldName, char[] array) {
        style.append(buffer, fieldName, array, null);
        return this;
    }

    /**
     * <p>Append to the <code>toString</code> a <code>char</code>
     * array.</p>
     *
     * <p>A boolean parameter controls the level of detail to show.
     * Setting <code>true</code> will output the array in full. Setting
     * <code>false</code> will output a summary, typically the size of
     * the array.</p>
     *
     * @param fieldName  the field name
     * @param array  the array to add to the <code>toString</code>
     * @param fullDetail  <code>true</code> for detail, <code>false</code>
     *  for summary info
     * @return this
     */
    public ToStringBuilder append(String fieldName, char[] array, boolean fullDetail) {
        style.append(buffer, fieldName, array, new Boolean(fullDetail));
        return this;
    }

    //----------------------------------------------------------------------------

    /**
     * <p>Append to the <code>toString</code> a <code>byte</code>
     * array.</p>
     *
     * @param array  the array to add to the <code>toString</code>
     * @return this
     */
    public ToStringBuilder append(byte[] array) {
        style.append(buffer, null, array, null);
        return this;
    }

    /**
     * <p>Append a <code>hashCode</code> for a <code>byte</code>
     * array.</p>
     *
     * @param fieldName  the field name
     * @param array  the array to add to the <code>hashCode</code>
     * @return this
     */
    public ToStringBuilder append(String fieldName, byte[] array) {
        style.append(buffer, fieldName, array, null);
        return this;
    }

    /**
     * <p>Append to the <code>toString</code> a <code>byte</code>
     * array.</p>
     *
     * <p>A boolean parameter controls the level of detail to show.
     * Setting <code>true</code> will output the array in full. Setting
     * <code>false</code> will output a summary, typically the size of
     * the array.
     *
     * @param fieldName  the field name
     * @param array  the array to add to the <code>toString</code>
     * @param fullDetail  <code>true</code> for detail, <code>false</code>
     *  for summary info
     * @return this
     */
    public ToStringBuilder append(String fieldName, byte[] array, boolean fullDetail) {
        style.append(buffer, fieldName, array, new Boolean(fullDetail));
        return this;
    }

    //----------------------------------------------------------------------------

    /**
     * <p>Append to the <code>toString</code> a <code>double</code>
     * array.</p>
     *
     * @param array  the array to add to the <code>toString</code>
     * @return this
     */
    public ToStringBuilder append(double[] array) {
        style.append(buffer, null, array, null);
        return this;
    }

    /**
     * <p>Append a <code>hashCode</code> for a <code>double</code>
     * array.</p>
     *
     * @param fieldName  the field name
     * @param array  the array to add to the <code>hashCode</code>
     * @return this
     */
    public ToStringBuilder append(String fieldName, double[] array) {
        style.append(buffer, fieldName, array, null);
        return this;
    }

    /**
     * <p>Append to the <code>toString</code> a <code>double</code>
     * array.</p>
     *
     * <p>A boolean parameter controls the level of detail to show.
     * Setting <code>true</code> will output the array in full. Setting
     * <code>false</code> will output a summary, typically the size of
     * the array.</p>
     *
     * @param fieldName  the field name
     * @param array  the array to add to the <code>toString</code>
     * @param fullDetail  <code>true</code> for detail, <code>false</code>
     *  for summary info
     * @return this
     */
    public ToStringBuilder append(String fieldName, double[] array, boolean fullDetail) {
        style.append(buffer, fieldName, array, new Boolean(fullDetail));
        return this;
    }

    //----------------------------------------------------------------------------

    /**
     * <p>Append to the <code>toString</code> a <code>float</code>
     * array.</p>
     *
     * @param array  the array to add to the <code>toString</code>
     * @return this
     */
    public ToStringBuilder append(float[] array) {
        style.append(buffer, null, array, null);
        return this;
    }

    /**
     * <p>Append a <code>hashCode</code> for a <code>float</code>
     * array.</p>
     *
     * @param fieldName  the field name
     * @param array  the array to add to the <code>hashCode</code>
     * @return this
     */
    public ToStringBuilder append(String fieldName, float[] array) {
        style.append(buffer, fieldName, array, null);
        return this;
    }

    /**
     * <p>Append to the <code>toString</code> a <code>float</code>
     * array.</p>
     *
     * <p>A boolean parameter controls the level of detail to show.
     * Setting <code>true</code> will output the array in full. Setting
     * <code>false</code> will output a summary, typically the size of
     * the array.</p>
     *
     * @param fieldName  the field name
     * @param array  the array to add to the <code>toString</code>
     * @param fullDetail  <code>true</code> for detail, <code>false</code>
     *  for summary info
     * @return this
     */
    public ToStringBuilder append(String fieldName, float[] array, boolean fullDetail) {
        style.append(buffer, fieldName, array, new Boolean(fullDetail));
        return this;
    }

    //----------------------------------------------------------------------------

    /**
     * <p>Append to the <code>toString</code> a <code>boolean</code>
     * array.</p>
     *
     * @param array  the array to add to the <code>toString</code>
     * @return this
     */
    public ToStringBuilder append(boolean[] array) {
        style.append(buffer, null, array, null);
        return this;
    }

    /**
     * <p>Append a <code>hashCode</code> for a <code>boolean</code>
     * array.</p>
     *
     * @param fieldName  the field name
     * @param array  the array to add to the <code>hashCode</code>
     * @return this
     */
    public ToStringBuilder append(String fieldName, boolean[] array) {
        style.append(buffer, fieldName, array, null);
        return this;
    }

    /**
     * <p>Append to the <code>toString</code> a <code>boolean</code>
     * array.</p>
     *
     * <p>A boolean parameter controls the level of detail to show.
     * Setting <code>true</code> will output the array in full. Setting
     * <code>false</code> will output a summary, typically the size of
     * the array.</p>
     *
     * @param fieldName  the field name
     * @param array  the array to add to the <code>toString</code>
     * @param fullDetail  <code>true</code> for detail, <code>false</code>
     *  for summary info
     * @return this
     */
    public ToStringBuilder append(String fieldName, boolean[] array, boolean fullDetail) {
        style.append(buffer, fieldName, array, new Boolean(fullDetail));
        return this;
    }

    //----------------------------------------------------------------------------

    /**
     * <p>Gets the <code>ToStringStyle</code> being used.</p>
     * 
     * @return the <code>ToStringStyle</code> being used
     */
    public ToStringStyle getStyle() {
        return style;
    }

    /**
     * <p>Gets the <code>StringBuffer</code> being populated.</p>
     * 
     * @return the <code>StringBuffer</code> being populated
     */
    public StringBuffer getStringBuffer() {
        return buffer;
    }

    /**
     * <p>Returns the built <code>toString</code>.</p>
     * 
     * <p>This method appends the end of the buffer, and can only be called once.
     * Use {@link #getStringBuffer} to get the current string state.</p>
     * 
     * @return the String <code>toString</code>
     */
    public String toString() {
        style.appendEnd(buffer, object);
        return buffer.toString();
    }

}
