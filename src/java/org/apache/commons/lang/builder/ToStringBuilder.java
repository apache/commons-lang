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
/**
 * <code>ToString</code> generation routine.
 * <p>
 * This class enables a good toString to be built for any class. This class aims 
 * to simplify the process by:
 * <ul>
 * <li>allowing field names
 * <li>handling all types consistently
 * <li>handling nulls consistently
 * <li>outputting arrays and multi-dimensional arrays
 * <li>enabling the detail level to be controlled for objects and collections
 * </ul>
 * <p>
 * To use this class write code as follows:
 * <code>
 * public class Person {
 *   String name;
 *   int age;
 *   boolean isSmoker;
 * 
 *   ...
 * 
 *   public String toString() {
 *     return new ToStringBuilder(this).
 *       append(name, "name").
 *       append(age, "age").
 *       append(smoker, "smoker").
 *       toString();
 *   }
 * }
 * </code>
 * This will produce a toString of the format:
 * <code>Person@7f54[name=Stephen,age=29,smoker=false]</code>
 * <p>
 * The exact format of the toString is determined by the {@link ToStringStyle}
 * passed into the constructor.
 *
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @version $Id: ToStringBuilder.java,v 1.3 2002/09/22 09:18:32 scolebourne Exp $
 */
public class ToStringBuilder {
    
    /**
     * The default style of output to use
     */
    private static ToStringStyle defaultStyle;
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
     * Constructor for ToStringBuilder.
     * This constructor outputs using the default style set with 
     * <code>setDefaultStyle</code>.
     * 
     * @param object  the object to build a toString for, must not be null
     * @throws IllegalArgumentException  if the object passed in is null
     */
    public ToStringBuilder(Object object) {
        this(object, getDefaultStyle(), null);
    }
    
    /**
     * Constructor for ToStringBuilder specifying the output style.
     * <p>
     * If the style is null, the default style is used.
     * 
     * @param object  the object to build a toString for, must not be null
     * @param style  the style of the toString to create, may be null
     * @throws IllegalArgumentException  if the object passed in is null
     */
    public ToStringBuilder(Object object, ToStringStyle style) {
        this(object, style, null);
    }
    
    /**
     * Constructor for ToStringBuilder.
     * <p>
     * If the style is null, the default style is used.
     * If the buffer is null, a new one is created.
     * 
     * @param object  the object to build a toString for, must not be null
     * @param style  the style of the toString to create, may be null
     * @param buffer  the string buffer to populate, may be null
     * @throws IllegalArgumentException  if the object passed in is null
     */
    public ToStringBuilder(Object object, ToStringStyle style, StringBuffer buffer) {
        super();
        if (object == null) {
            throw new IllegalArgumentException("The object to create a toString for must not be null");
        }
        if (style == null) {
            style = ToStringStyle.DEFAULT_STYLE;
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
     * Gets the default style to use.
     * <p>
     * This could allow the toString style to be controlled for an entire
     * application with one call. This might be used to have a verbose toString
     * during development and a compact toString in production.
     * 
     * @return the default toString style
     */
    public static ToStringStyle getDefaultStyle() {
        return defaultStyle;
    }
    
    /**
     * Sets the default style to use.
     * 
     * @param style  the default toString style
     * @throws IllegalArgumentException if the style is null
     */
    public static void setDefaultStyle(ToStringStyle style) {
        if (style == null) {
            throw new IllegalArgumentException("The style must not be null");
        }
        defaultStyle = style;
    }
    
    //-------------------------------------------------------------------------
    
    /**
     * This method uses reflection to build a suitable toString using the default style.
     * <p>
     * It uses Field.setAccessible to gain access to private fields. This means
     * that it will throw a security exception if run under a security manger, if
     * the permissions are not set up.
     * It is also not as efficient as testing explicitly. 
     * Transient members will be not be included, as they are likely derived.
     * Static fields will be not be included.
     * fields, and not part of the value of the object. 
     * 
     * @param object  the object to be output
     * @return the String result
     * @throws IllegalArgumentException if the object is null
     */
    public static String reflectionToString(Object object) {
        return reflectionToString(object, null, false);
    }

    /**
     * This method uses reflection to build a suitable toString.
     * <p>
     * It uses Field.setAccessible to gain access to private fields. This means
     * that it will throw a security exception if run under a security manger, if
     * the permissions are not set up.
     * It is also not as efficient as testing explicitly. 
     * Transient members will be not be included, as they are likely derived.
     * Static fields will be not be included.
     * fields, and not part of the value of the object. 
     * <p>
     * If the style is null, the default style is used.
     * 
     * @param object  the object to be output
     * @param style  the style of the toString to create, may be null
     * @return the String result
     * @throws IllegalArgumentException if the object or style is null
     */
    public static String reflectionToString(Object object, ToStringStyle style) {
        return reflectionToString(object, style, false);
    }

    /**
     * This method uses reflection to build a suitable toString.
     * <p>
     * It uses Field.setAccessible to gain access to private fields. This means
     * that it will throw a security exception if run under a security manger, if
     * the permissions are not set up.
     * It is also not as efficient as testing explicitly. 
     * If the outputTransients parameter is set to true, transient members will be
     * output, otherwise they are ignored, as they are likely derived fields, and
     * not part of the value of the object. 
     * Static fields will not be tested.
     * <p>
     * If the style is null, the default style is used.
     * 
     * @param object  the object to be output
     * @param style  the style of the toString to create, may be null
     * @param outputTransients  whether to include transient fields
     * @return the String result
     * @throws IllegalArgumentException if the object or style is null
     */
    public static String reflectionToString(Object object, ToStringStyle style, 
            boolean outputTransients) {
        if (object == null) {
            throw new IllegalArgumentException("The object must not be null");
        }
        if (style == null) {
            style = getDefaultStyle();
        }
        Field[] fields = object.getClass().getDeclaredFields();
        Field.setAccessible(fields, true);
        ToStringBuilder builder = new ToStringBuilder(object, style);
        for (int i = 0; i < fields.length; ++i) {
            Field f = fields[i];
            if (outputTransients || !Modifier.isTransient(f.getModifiers())) {
                if (!Modifier.isStatic(f.getModifiers())) {
                    try {
                        builder.append(f.getName(), f.get(object));
                        
                    } catch (IllegalAccessException ex) {
                        //this can't happen. Would get a Security exception instead
                        //throw a runtime exception in case the impossible happens.
                        throw new InternalError("Unexpected IllegalAccessException");
                    }
                }
            }
        }
        return builder.toString();
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString an Object value.
     *
     * @param object  the value to add to the toString
     * @return this
     */
    public ToStringBuilder append(Object object) {
        style.append(buffer, null, object, null);
        return this;
    }

    /**
     * Append to the toString an Object value.
     *
     * @param object  the value to add to the toString
     * @param fieldName  the field name
     * @return this
     */
    public ToStringBuilder append(String fieldName, Object object) {
        style.append(buffer, fieldName, object, null);
        return this;
    }

    /**
     * Append to the toString an Object value.
     *
     * @param object  the value to add to the toString
     * @param fieldName  the field name
     * @param fullDetail  true for detail, false for summary info
     * @return this
     */
    public ToStringBuilder append(String fieldName, Object object, boolean fullDetail) {
        style.append(buffer, fieldName, object, new Boolean(fullDetail));
        return this;
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString a long value.
     *
     * @param value  the value to add to the toString
     * @return this
     */
    public ToStringBuilder append(long value) {
        style.append(buffer, null, value);
        return this;
    }

    /**
     * Append to the toString a long value.
     *
     * @param value  the value to add to the toString
     * @param fieldName  the field name
     * @return this
     */
    public ToStringBuilder append(String fieldName, long value) {
        style.append(buffer, fieldName, value);
        return this;
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString an int value.
     *
     * @param value  the value to add to the toString
     * @return this
     */
    public ToStringBuilder append(int value) {
        style.append(buffer, null, value);
        return this;
    }

    /**
     * Append to the toString an int value.
     *
     * @param value  the value to add to the toString
     * @param fieldName  the field name
     * @return this
     */
    public ToStringBuilder append(String fieldName, int value) {
        style.append(buffer, fieldName, value);
        return this;
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString a short value.
     *
     * @param value  the value to add to the toString
     * @return this
     */
    public ToStringBuilder append(short value) {
        style.append(buffer, null, value);
        return this;
    }

    /**
     * Append to the toString a short value.
     *
     * @param value  the value to add to the toString
     * @param fieldName  the field name
     * @return this
     */
    public ToStringBuilder append(String fieldName, short value) {
        style.append(buffer, fieldName, value);
        return this;
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString a char value.
     *
     * @param value  the value to add to the toString
     * @return this
     */
    public ToStringBuilder append(char value) {
        style.append(buffer, null, value);
        return this;
    }

    /**
     * Append to the toString a char value.
     *
     * @param value  the value to add to the toString
     * @param fieldName  the field name
     * @return this
     */
    public ToStringBuilder append(String fieldName, char value) {
        style.append(buffer, fieldName, value);
        return this;
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString a byte value.
     *
     * @param value  the value to add to the toString
     * @return this
     */
    public ToStringBuilder append(byte value) {
        style.append(buffer, null, value);
        return this;
    }

    /**
     * Append to the toString a byte value.
     *
     * @param value  the value to add to the toString
     * @param fieldName  the field name
     * @return this
     */
    public ToStringBuilder append(String fieldName, byte value) {
        style.append(buffer, fieldName, value);
        return this;
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString a double value.
     *
     * @param value  the value to add to the toString
     * @return this
     */
    public ToStringBuilder append(double value) {
        style.append(buffer, null, value);
        return this;
    }

    /**
     * Append to the toString a double value.
     *
     * @param value  the value to add to the toString
     * @param fieldName  the field name
     * @return this
     */
    public ToStringBuilder append(String fieldName, double value) {
        style.append(buffer, fieldName, value);
        return this;
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString a float value.
     *
     * @param value  the value to add to the toString
     * @return this
     */
    public ToStringBuilder append(float value) {
        style.append(buffer, null, value);
        return this;
    }

    /**
     * Append to the toString a float value.
     *
     * @param value  the value to add to the toString
     * @param fieldName  the field name
     * @return this
     */
    public ToStringBuilder append(String fieldName, float value) {
        style.append(buffer, fieldName, value);
        return this;
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString a boolean value.
     *
     * @param value  the value to add to the toString
     * @return this
     */
    public ToStringBuilder append(boolean value) {
        style.append(buffer, null, value);
        return this;
    }

    /**
     * Append to the toString a boolean value.
     *
     * @param value  the value to add to the toString
     * @param fieldName  the field name
     * @return this
     */
    public ToStringBuilder append(String fieldName, boolean value) {
        style.append(buffer, fieldName, value);
        return this;
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString an Object array.
     *
     * @param array  the array to add to the toString
     * @return this
     */
    public ToStringBuilder append(Object[] array) {
        style.append(buffer, null, array, null);
        return this;
    }

    /**
     * Append to the toString an Object array.
     *
     * @param fieldName  the field name
     * @param array  the array to add to the toString
     * @return this
     */
    public ToStringBuilder append(String fieldName, Object[] array) {
        style.append(buffer, fieldName, array, null);
        return this;
    }

    /**
     * Append to the toString an Object array.
     * <p>
     * A boolean parameter controls the level of detail to show. Setting true
     * will output the array in full. Setting false will output a summary,
     * typically the size of the array.
     *
     * @param fieldName  the field name
     * @param array  the array to add to the toString
     * @param fullDetail  true for detail, false for summary info
     * @return this
     */
    public ToStringBuilder append(String fieldName, Object[] array, boolean fullDetail) {
        style.append(buffer, fieldName, array, new Boolean(fullDetail));
        return this;
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString a long array.
     *
     * @param array  the array to add to the toString
     * @return this
     */
    public ToStringBuilder append(long[] array) {
        style.append(buffer, null, array, null);
        return this;
    }

    /**
     * Append a hashCode for a long array.
     *
     * @param fieldName  the field name
     * @param array  the array to add to the hashCode
     * @return this
     */
    public ToStringBuilder append(String fieldName, long[] array) {
        style.append(buffer, fieldName, array, null);
        return this;
    }

    /**
     * Append to the toString a long array.
     * <p>
     * A boolean parameter controls the level of detail to show. Setting true
     * will output the array in full. Setting false will output a summary,
     * typically the size of the array.
     *
     * @param fieldName  the field name
     * @param array  the array to add to the toString
     * @param fullDetail  true for detail, false for summary info
     * @return this
     */
    public ToStringBuilder append(String fieldName, long[] array, boolean fullDetail) {
        style.append(buffer, fieldName, array, new Boolean(fullDetail));
        return this;
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString a int array.
     *
     * @param array  the array to add to the toString
     * @return this
     */
    public ToStringBuilder append(int[] array) {
        style.append(buffer, null, array, null);
        return this;
    }

    /**
     * Append a hashCode for an int array.
     *
     * @param fieldName  the field name
     * @param array  the array to add to the hashCode
     * @return this
     */
    public ToStringBuilder append(String fieldName, int[] array) {
        style.append(buffer, fieldName, array, null);
        return this;
    }

    /**
     * Append to the toString an int array.
     * <p>
     * A boolean parameter controls the level of detail to show. Setting true
     * will output the array in full. Setting false will output a summary,
     * typically the size of the array.
     *
     * @param fieldName  the field name
     * @param array  the array to add to the toString
     * @param fullDetail  true for detail, false for summary info
     * @return this
     */
    public ToStringBuilder append(String fieldName, int[] array, boolean fullDetail) {
        style.append(buffer, fieldName, array, new Boolean(fullDetail));
        return this;
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString a short array.
     *
     * @param array  the array to add to the toString
     * @return this
     */
    public ToStringBuilder append(short[] array) {
        style.append(buffer, null, array, null);
        return this;
    }

    /**
     * Append a hashCode for a short array.
     *
     * @param fieldName  the field name
     * @param array  the array to add to the hashCode
     * @return this
     */
    public ToStringBuilder append(String fieldName, short[] array) {
        style.append(buffer, fieldName, array, null);
        return this;
    }

    /**
     * Append to the toString a short array.
     * <p>
     * A boolean parameter controls the level of detail to show. Setting true
     * will output the array in full. Setting false will output a summary,
     * typically the size of the array.
     *
     * @param fieldName  the field name
     * @param array  the array to add to the toString
     * @param fullDetail  true for detail, false for summary info
     * @return this
     */
    public ToStringBuilder append(String fieldName, short[] array, boolean fullDetail) {
        style.append(buffer, fieldName, array, new Boolean(fullDetail));
        return this;
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString a char array.
     *
     * @param array  the array to add to the toString
     * @return this
     */
    public ToStringBuilder append(char[] array) {
        style.append(buffer, null, array, null);
        return this;
    }

    /**
     * Append a hashCode for a char array.
     *
     * @param fieldName  the field name
     * @param array  the array to add to the hashCode
     * @return this
     */
    public ToStringBuilder append(String fieldName, char[] array) {
        style.append(buffer, fieldName, array, null);
        return this;
    }

    /**
     * Append to the toString a char array.
     * <p>
     * A boolean parameter controls the level of detail to show. Setting true
     * will output the array in full. Setting false will output a summary,
     * typically the size of the array.
     *
     * @param fieldName  the field name
     * @param array  the array to add to the toString
     * @param fullDetail  true for detail, false for summary info
     * @return this
     */
    public ToStringBuilder append(String fieldName, char[] array, boolean fullDetail) {
        style.append(buffer, fieldName, array, new Boolean(fullDetail));
        return this;
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString a byte array.
     *
     * @param array  the array to add to the toString
     * @return this
     */
    public ToStringBuilder append(byte[] array) {
        style.append(buffer, null, array, null);
        return this;
    }

    /**
     * Append a hashCode for a byte array.
     *
     * @param fieldName  the field name
     * @param array  the array to add to the hashCode
     * @return this
     */
    public ToStringBuilder append(String fieldName, byte[] array) {
        style.append(buffer, fieldName, array, null);
        return this;
    }

    /**
     * Append to the toString a byte array.
     * <p>
     * A boolean parameter controls the level of detail to show. Setting true
     * will output the array in full. Setting false will output a summary,
     * typically the size of the array.
     *
     * @param fieldName  the field name
     * @param array  the array to add to the toString
     * @param fullDetail  true for detail, false for summary info
     * @return this
     */
    public ToStringBuilder append(String fieldName, byte[] array, boolean fullDetail) {
        style.append(buffer, fieldName, array, new Boolean(fullDetail));
        return this;
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString a double array.
     *
     * @param array  the array to add to the toString
     * @return this
     */
    public ToStringBuilder append(double[] array) {
        style.append(buffer, null, array, null);
        return this;
    }

    /**
     * Append a hashCode for a double array.
     *
     * @param fieldName  the field name
     * @param array  the array to add to the hashCode
     * @return this
     */
    public ToStringBuilder append(String fieldName, double[] array) {
        style.append(buffer, fieldName, array, null);
        return this;
    }

    /**
     * Append to the toString a double array.
     * <p>
     * A boolean parameter controls the level of detail to show. Setting true
     * will output the array in full. Setting false will output a summary,
     * typically the size of the array.
     *
     * @param fieldName  the field name
     * @param array  the array to add to the toString
     * @param fullDetail  true for detail, false for summary info
     * @return this
     */
    public ToStringBuilder append(String fieldName, double[] array, boolean fullDetail) {
        style.append(buffer, fieldName, array, new Boolean(fullDetail));
        return this;
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString a float array.
     *
     * @param array  the array to add to the toString
     * @return this
     */
    public ToStringBuilder append(float[] array) {
        style.append(buffer, null, array, null);
        return this;
    }

    /**
     * Append a hashCode for a float array.
     *
     * @param fieldName  the field name
     * @param array  the array to add to the hashCode
     * @return this
     */
    public ToStringBuilder append(String fieldName, float[] array) {
        style.append(buffer, fieldName, array, null);
        return this;
    }

    /**
     * Append to the toString a float array.
     * <p>
     * A boolean parameter controls the level of detail to show. Setting true
     * will output the array in full. Setting false will output a summary,
     * typically the size of the array.
     *
     * @param fieldName  the field name
     * @param array  the array to add to the toString
     * @param fullDetail  true for detail, false for summary info
     * @return this
     */
    public ToStringBuilder append(String fieldName, float[] array, boolean fullDetail) {
        style.append(buffer, fieldName, array, new Boolean(fullDetail));
        return this;
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString a boolean array.
     *
     * @param array  the array to add to the toString
     * @return this
     */
    public ToStringBuilder append(boolean[] array) {
        style.append(buffer, null, array, null);
        return this;
    }

    /**
     * Append a hashCode for a boolean array.
     *
     * @param fieldName  the field name
     * @param array  the array to add to the hashCode
     * @return this
     */
    public ToStringBuilder append(String fieldName, boolean[] array) {
        style.append(buffer, fieldName, array, null);
        return this;
    }

    /**
     * Append to the toString a boolean array.
     * <p>
     * A boolean parameter controls the level of detail to show. Setting true
     * will output the array in full. Setting false will output a summary,
     * typically the size of the array.
     *
     * @param fieldName  the field name
     * @param array  the array to add to the toString
     * @param fullDetail  true for detail, false for summary info
     * @return this
     */
    public ToStringBuilder append(String fieldName, boolean[] array, boolean fullDetail) {
        style.append(buffer, fieldName, array, new Boolean(fullDetail));
        return this;
    }

    //----------------------------------------------------------------------------
    
    /**
     * Gets the buffer being populated
     * 
     * @return the StringBuffer being populated
     */    
    public StringBuffer getStringBuffer() {
        return buffer;
    }

    /**
     * Returns the built toString
     * 
     * @return the String toString
     */    
    public String toString() {
        style.appendEnd(buffer, object);
        return buffer.toString();
    }

}
