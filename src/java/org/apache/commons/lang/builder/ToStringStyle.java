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
// package org.apache.commons.lang.builder

package org.apache.commons.lang.builder;
import java.io.Serializable;
import java.util.Collection;
import java.util.Map;

import org.apache.commons.lang.SystemUtils;
/**
 * <code>ToStringStyle</code> works with ToStringBuilder to create a
 * toString.
 * <p>
 * These classes are intended to be used as singletons. There is no need 
 * to instantiate a new style each time. A prohram will generally use one
 * of the predefined constants on this class. Alternatively, the 
 * {@link StandardToStringStyle} class can be used to set the individual
 * settings.
 * <P>
 * If necessary, this class can be subclassed to provide specific toString
 * designs. The main public interface is always via ToStringBuilder however.
 *
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @version $Id: ToStringStyle.java,v 1.1 2002/09/17 22:07:20 scolebourne Exp $
 */
public abstract class ToStringStyle implements Serializable {
    
    /**
     * The default toString style.
     */
    public static final ToStringStyle DEFAULT_STYLE = new DefaultToStringStyle();
    /**
     * The multi line toString style.
     */
    public static final ToStringStyle MULTI_LINE_STYLE = new MultiLineToStringStyle();
    /**
     * The no field names toString style.
     */
    public static final ToStringStyle NO_FIELD_NAMES_STYLE = new NoFieldNameToStringStyle();
    /**
     * The simple toString style.
     */
    public static final ToStringStyle SIMPLE_STYLE = new SimpleToStringStyle();

    /**
     * Whether to use the field names 'true'
     */
    protected boolean useFieldNames = true;
    /**
     * Whether to use the class name 'true'
     */
    protected boolean useClassName = true;
    /**
     * Whether to use short class names 'false'
     */
    protected boolean useShortClassName = false;
    /**
     * Whether to use the identity hash code 'true'
     */
    protected boolean useIdentityHashCode = true;
    
    /**
     * The content start '['
     */
    protected String contentStart = "[";
    /**
     * The content end ']'
     */
    protected String contentEnd = "]";
    /**
     * The field name value separator '='
     */
    protected String fieldNameValueSeparator = "=";
    /**
     * The field separator ','
     */
    protected String fieldSeparator = ",";
    /**
     * The array start '{'
     */
    protected String arrayStart = "{";
    /**
     * The array separator ','
     */
    protected String arraySeparator = ",";
    /**
     * The detail for array content
     */
    protected boolean arrayContentDetail = true;
    /**
     * The array end '}'
     */
    protected String arrayEnd = "}";
    /**
     * The value to use when fullDetail is null 'true'
     */
    protected boolean defaultFullDetail = true;
    /**
     * The null text '<null>'
     */
    protected String nullText = "<null>";
    /**
     * The summary size text start '<size'
     */
    protected String sizeStartText = "<size=";
    /**
     * The summary size text start '>'
     */
    protected String sizeEndText = ">";
    /**
     * The summary object text start '<'
     */
    protected String summaryObjectStartText = "<";
    /**
     * The summary object text start '>'
     */
    protected String summaryObjectEndText = ">";
    
    //----------------------------------------------------------------------------
    
    /**
     * Constructor.
     */
    protected ToStringStyle() {
        super();
    }
    
    //----------------------------------------------------------------------------
    
    /**
     * Append the start of data indicator.
     * 
     * @param buffer  the StringBuffer to populate
     * @param object  the object to build a toString for, must not be null
     */
    public void appendStart(StringBuffer buffer, Object object) {
        appendClassName(buffer, object);
        appendIdentityHashCode(buffer, object);
        appendContentStart(buffer);
    }

    /**
     * Append the end of data indicator.
     * 
     * @param buffer  the StringBuffer to populate
     * @param object  the object to build a toString for, must not be null
     */
    public void appendEnd(StringBuffer buffer, Object object) {
        appendContentEnd(buffer);
    }
    
    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString an Object value, printing the full 
     * toString of the object passed in.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name
     * @param value  the value to add to the toString
     * @param fullDetail  true for detail, false for summary info, null for style decides
     */
    public void append(StringBuffer buffer, String fieldName, Object value, Boolean fullDetail) {
        appendFieldStart(buffer, fieldName);
        
        if (value == null) {
            appendNullText(buffer, fieldName);
            
        } else {
            appendInternal(buffer, fieldName, value, isFullDetail(fullDetail));
        }
        
        appendFieldEnd(buffer, fieldName);
    }
    
    /**
     * Append to the toString an Object, correctly interpretting its type.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param value  the value to add to the toString, not null
     * @param detail  output detail or not
     */
    protected void appendInternal(StringBuffer buffer, String fieldName, Object value, boolean detail) {
        if (value instanceof Collection) {
            if (detail) {
                appendDetail(buffer, fieldName, (Collection) value);
            } else {
                appendSummarySize(buffer, fieldName, ((Collection) value).size());
            }
            
        } else if (value instanceof Map) {
            if (detail) {
                appendDetail(buffer, fieldName, (Map) value);
            } else {
                appendSummarySize(buffer, fieldName, ((Map) value).size());
            }
            
        } else if (value instanceof long[]) {
            if (detail) {
                appendDetail(buffer, fieldName, (long[]) value);
            } else {
                appendSummary(buffer, fieldName, (long[]) value);
            }
            
        } else if (value instanceof int[]) {
            if (detail) {
                appendDetail(buffer, fieldName, (int[]) value);
            } else {
                appendSummary(buffer, fieldName, (int[]) value);
            }
            
        } else if (value instanceof short[]) {
            if (detail) {
                appendDetail(buffer, fieldName, (short[]) value);
            } else {
                appendSummary(buffer, fieldName, (short[]) value);
            }
            
        } else if (value instanceof byte[]) {
            if (detail) {
                appendDetail(buffer, fieldName, (byte[]) value);
            } else {
                appendSummary(buffer, fieldName, (byte[]) value);
            }
            
        } else if (value instanceof char[]) {
            if (detail) {
                appendDetail(buffer, fieldName, (char[]) value);
            } else {
                appendSummary(buffer, fieldName, (char[]) value);
            }
            
        } else if (value instanceof double[]) {
            if (detail) {
                appendDetail(buffer, fieldName, (double[]) value);
            } else {
                appendSummary(buffer, fieldName, (double[]) value);
            }
            
        } else if (value instanceof float[]) {
            if (detail) {
                appendDetail(buffer, fieldName, (float[]) value);
            } else {
                appendSummary(buffer, fieldName, (float[]) value);
            }
            
        } else if (value instanceof boolean[]) {
            if (detail) {
                appendDetail(buffer, fieldName, (boolean[]) value);
            } else {
                appendSummary(buffer, fieldName, (boolean[]) value);
            }
        
        } else if (value.getClass().isArray()) {
            if (detail) {
                appendDetail(buffer, fieldName, (Object[]) value);
            } else {
                appendSummary(buffer, fieldName, (Object[]) value);
            }
            
        } else {
            if (detail) {
                appendDetail(buffer, fieldName, (Object) value);
            } else {
                appendSummary(buffer, fieldName, (Object) value);
            }
        }
    }
    
    /**
     * Append to the toString an Object value, printing the full detail of the object.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param value  the value to add to the toString, not null
     */
    protected void appendDetail(StringBuffer buffer, String fieldName, Object value) {
        buffer.append(value);
    }
    
    /**
     * Append to the toString a Collection.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param coll  the collection to add to the toString, not null
     */
    protected void appendDetail(StringBuffer buffer, String fieldName, Collection coll) {
        buffer.append(coll);
    }
    
    /**
     * Append to the toString a Map.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param map  the maps to add to the toString, not null
     */
    protected void appendDetail(StringBuffer buffer, String fieldName, Map map) {
        buffer.append(map);
    }
    
    /**
     * Append to the toString an Object value, printing a summary of the object.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param value  the value to add to the toString, not null
     */
    protected void appendSummary(StringBuffer buffer, String fieldName, Object value) {
        buffer.append(summaryObjectStartText);
        buffer.append(getShortClassName(value.getClass()));
        buffer.append(summaryObjectEndText);
    }
    
    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString a long value.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name
     * @param value  the value to add to the toString
     */
    public void append(StringBuffer buffer, String fieldName, long value) {
        appendFieldStart(buffer, fieldName);
        appendDetail(buffer, fieldName, value);
        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Append to the toString a long value.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param value  the value to add to the toString
     */
    protected void appendDetail(StringBuffer buffer, String fieldName, long value) {
        buffer.append(value);
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString an int value.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name
     * @param value  the value to add to the toString
     */
    public void append(StringBuffer buffer, String fieldName, int value) {
        appendFieldStart(buffer, fieldName);
        appendDetail(buffer, fieldName, value);
        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Append to the toString an int value.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param value  the value to add to the toString
     */
    protected void appendDetail(StringBuffer buffer, String fieldName, int value) {
        buffer.append(value);
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString a short value.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name
     * @param value  the value to add to the toString
     */
    public void append(StringBuffer buffer, String fieldName, short value) {
        appendFieldStart(buffer, fieldName);
        appendDetail(buffer, fieldName, value);
        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Append to the toString a short value.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param value  the value to add to the toString
     */
    protected void appendDetail(StringBuffer buffer, String fieldName, short value) {
        buffer.append(value);
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString a byte value.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name
     * @param value  the value to add to the toString
     */
    public void append(StringBuffer buffer, String fieldName, byte value) {
        appendFieldStart(buffer, fieldName);
        appendDetail(buffer, fieldName, value);
        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Append to the toString a byte value.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param value  the value to add to the toString
     */
    protected void appendDetail(StringBuffer buffer, String fieldName, byte value) {
        buffer.append(value);
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString a char value.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name
     * @param value  the value to add to the toString
     */
    public void append(StringBuffer buffer, String fieldName, char value) {
        appendFieldStart(buffer, fieldName);
        appendDetail(buffer, fieldName, value);
        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Append to the toString a char value.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param value  the value to add to the toString
     */
    protected void appendDetail(StringBuffer buffer, String fieldName, char value) {
        buffer.append(value);
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString a double value.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name
     * @param value  the value to add to the toString
     */
    public void append(StringBuffer buffer, String fieldName, double value) {
        appendFieldStart(buffer, fieldName);
        appendDetail(buffer, fieldName, value);
        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Append to the toString a double value.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param value  the value to add to the toString
     */
    protected void appendDetail(StringBuffer buffer, String fieldName, double value) {
        buffer.append(value);
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString a float value.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name
     * @param value  the value to add to the toString
     */
    public void append(StringBuffer buffer, String fieldName, float value) {
        appendFieldStart(buffer, fieldName);
        appendDetail(buffer, fieldName, value);
        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Append to the toString a float value.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param value  the value to add to the toString
     */
    protected void appendDetail(StringBuffer buffer, String fieldName, float value) {
        buffer.append(value);
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString a boolean value.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name
     * @param value  the value to add to the toString
     */
    public void append(StringBuffer buffer, String fieldName, boolean value) {
        appendFieldStart(buffer, fieldName);
        appendDetail(buffer, fieldName, value);
        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Append to the toString a boolean value.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param value  the value to add to the toString
     */
    protected void appendDetail(StringBuffer buffer, String fieldName, boolean value) {
        buffer.append(value);
    }

    /**
     * Append to the toString an Object array.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name
     * @param array  the array to add to the toString
     * @param fullDetail  true for detail, false for summary info, null for style decides
     */
    public void append(StringBuffer buffer, String fieldName, Object[] array, Boolean fullDetail) {
        appendFieldStart(buffer, fieldName);
        
        if (array == null) {
            appendNullText(buffer, fieldName);
            
        } else if (isFullDetail(fullDetail)) {
            appendDetail(buffer, fieldName, array);
            
        } else {
            appendSummary(buffer, fieldName, array);
        }

        appendFieldEnd(buffer, fieldName);
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString the detail of an Object array.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the toString, not null
     */
    protected void appendDetail(StringBuffer buffer, String fieldName, Object[] array) {
        buffer.append(arrayStart);
        for (int i = 0; i < array.length; i++) {
            Object item = array[i];
            if (i > 0) {
                buffer.append(arraySeparator);
            }
            if (item == null) {
                appendNullText(buffer, fieldName);
                
            } else {
                appendInternal(buffer, fieldName, item, arrayContentDetail);
            }
        }
        buffer.append(arrayEnd);
    }

    /**
     * Append to the toString a summary of an Object array.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the toString, not null
     */
    protected void appendSummary(StringBuffer buffer, String fieldName, Object[] array) {
        appendSummarySize(buffer, fieldName, array.length);
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString a long array.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name
     * @param array  the array to add to the toString
     * @param fullDetail  true for detail, false for summary info, null for style decides
     */
    public void append(StringBuffer buffer, String fieldName, long[] array, Boolean fullDetail) {
        appendFieldStart(buffer, fieldName);
        
        if (array == null) {
            appendNullText(buffer, fieldName);
            
        } else if (isFullDetail(fullDetail)) {
            appendDetail(buffer, fieldName, array);
            
        } else {
            appendSummary(buffer, fieldName, array);
        }

        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Append to the toString the detail of a long array.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the toString, not null
     */
    protected void appendDetail(StringBuffer buffer, String fieldName, long[] array) {
        buffer.append(arrayStart);
        for (int i = 0; i < array.length; i++) {
            if (i > 0) {
                buffer.append(arraySeparator);
            }
            appendDetail(buffer, fieldName, array[i]);
        }
        buffer.append(arrayEnd);
    }

    /**
     * Append to the toString a summary of a long array.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the toString, not null
     */
    protected void appendSummary(StringBuffer buffer, String fieldName, long[] array) {
        appendSummarySize(buffer, fieldName, array.length);
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString an int array.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name
     * @param array  the array to add to the toString
     * @param fullDetail  true for detail, false for summary info, null for style decides
     */
    public void append(StringBuffer buffer, String fieldName, int[] array, Boolean fullDetail) {
        appendFieldStart(buffer, fieldName);
        
        if (array == null) {
            appendNullText(buffer, fieldName);
            
        } else if (isFullDetail(fullDetail)) {
            appendDetail(buffer, fieldName, array);
            
        } else {
            appendSummary(buffer, fieldName, array);
        }

        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Append to the toString the detail of an int array.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the toString, not null
     */
    protected void appendDetail(StringBuffer buffer, String fieldName, int[] array) {
        buffer.append(arrayStart);
        for (int i = 0; i < array.length; i++) {
            if (i > 0) {
                buffer.append(arraySeparator);
            }
            appendDetail(buffer, fieldName, array[i]);
        }
        buffer.append(arrayEnd);
    }

    /**
     * Append to the toString a summary of an int array.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the toString, not null
     */
    protected void appendSummary(StringBuffer buffer, String fieldName, int[] array) {
        appendSummarySize(buffer, fieldName, array.length);
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString a short array.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name
     * @param array  the array to add to the toString
     * @param fullDetail  true for detail, false for summary info, null for style decides
     */
    public void append(StringBuffer buffer, String fieldName, short[] array, Boolean fullDetail) {
        appendFieldStart(buffer, fieldName);
        
        if (array == null) {
            appendNullText(buffer, fieldName);
            
        } else if (isFullDetail(fullDetail)) {
            appendDetail(buffer, fieldName, array);
            
        } else {
            appendSummary(buffer, fieldName, array);
        }

        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Append to the toString the detail of a short array.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the toString, not null
     */
    protected void appendDetail(StringBuffer buffer, String fieldName, short[] array) {
        buffer.append(arrayStart);
        for (int i = 0; i < array.length; i++) {
            if (i > 0) {
                buffer.append(arraySeparator);
            }
            appendDetail(buffer, fieldName, array[i]);
        }
        buffer.append(arrayEnd);
    }

    /**
     * Append to the toString a summary of a short array.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the toString, not null
     */
    protected void appendSummary(StringBuffer buffer, String fieldName, short[] array) {
        appendSummarySize(buffer, fieldName, array.length);
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString a byte array.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name
     * @param array  the array to add to the toString
     * @param fullDetail  true for detail, false for summary info, null for style decides
     */
    public void append(StringBuffer buffer, String fieldName, byte[] array, Boolean fullDetail) {
        appendFieldStart(buffer, fieldName);
        
        if (array == null) {
            appendNullText(buffer, fieldName);
            
        } else if (isFullDetail(fullDetail)) {
            appendDetail(buffer, fieldName, array);
            
        } else {
            appendSummary(buffer, fieldName, array);
        }

        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Append to the toString the detail of a byte array.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the toString, not null
     */
    protected void appendDetail(StringBuffer buffer, String fieldName, byte[] array) {
        buffer.append(arrayStart);
        for (int i = 0; i < array.length; i++) {
            if (i > 0) {
                buffer.append(arraySeparator);
            }
            appendDetail(buffer, fieldName, array[i]);
        }
        buffer.append(arrayEnd);
    }

    /**
     * Append to the toString a summary of a byte array.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the toString, not null
     */
    protected void appendSummary(StringBuffer buffer, String fieldName, byte[] array) {
        appendSummarySize(buffer, fieldName, array.length);
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString a char array.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name
     * @param array  the array to add to the toString
     * @param fullDetail  true for detail, false for summary info, null for style decides
     */
    public void append(StringBuffer buffer, String fieldName, char[] array, Boolean fullDetail) {
        appendFieldStart(buffer, fieldName);
        
        if (array == null) {
            appendNullText(buffer, fieldName);
            
        } else if (isFullDetail(fullDetail)) {
            appendDetail(buffer, fieldName, array);
            
        } else {
            appendSummary(buffer, fieldName, array);
        }

        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Append to the toString the detail of a char array.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the toString, not null
     */
    protected void appendDetail(StringBuffer buffer, String fieldName, char[] array) {
        buffer.append(arrayStart);
        for (int i = 0; i < array.length; i++) {
            if (i > 0) {
                buffer.append(arraySeparator);
            }
            appendDetail(buffer, fieldName, array[i]);
        }
        buffer.append(arrayEnd);
    }

    /**
     * Append to the toString a summary of a char array.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the toString, not null
     */
    protected void appendSummary(StringBuffer buffer, String fieldName, char[] array) {
        appendSummarySize(buffer, fieldName, array.length);
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString a double array.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name
     * @param array  the array to add to the toString
     * @param fullDetail  true for detail, false for summary info, null for style decides
     */
    public void append(StringBuffer buffer, String fieldName, double[] array, Boolean fullDetail) {
        appendFieldStart(buffer, fieldName);
        
        if (array == null) {
            appendNullText(buffer, fieldName);
            
        } else if (isFullDetail(fullDetail)) {
            appendDetail(buffer, fieldName, array);
            
        } else {
            appendSummary(buffer, fieldName, array);
        }

        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Append to the toString the detail of a double array.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the toString, not null
     */
    protected void appendDetail(StringBuffer buffer, String fieldName, double[] array) {
        buffer.append(arrayStart);
        for (int i = 0; i < array.length; i++) {
            if (i > 0) {
                buffer.append(arraySeparator);
            }
            appendDetail(buffer, fieldName, array[i]);
        }
        buffer.append(arrayEnd);
    }

    /**
     * Append to the toString a summary of a double array.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the toString, not null
     */
    protected void appendSummary(StringBuffer buffer, String fieldName, double[] array) {
        appendSummarySize(buffer, fieldName, array.length);
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString a float array.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name
     * @param array  the array to add to the toString
     * @param fullDetail  true for detail, false for summary info, null for style decides
     */
    public void append(StringBuffer buffer, String fieldName, float[] array, Boolean fullDetail) {
        appendFieldStart(buffer, fieldName);
        
        if (array == null) {
            appendNullText(buffer, fieldName);
            
        } else if (isFullDetail(fullDetail)) {
            appendDetail(buffer, fieldName, array);
            
        } else {
            appendSummary(buffer, fieldName, array);
        }

        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Append to the toString the detail of a float array.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the toString, not null
     */
    protected void appendDetail(StringBuffer buffer, String fieldName, float[] array) {
        buffer.append(arrayStart);
        for (int i = 0; i < array.length; i++) {
            if (i > 0) {
                buffer.append(arraySeparator);
            }
            appendDetail(buffer, fieldName, array[i]);
        }
        buffer.append(arrayEnd);
    }

    /**
     * Append to the toString a summary of a float array.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the toString, not null
     */
    protected void appendSummary(StringBuffer buffer, String fieldName, float[] array) {
        appendSummarySize(buffer, fieldName, array.length);
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append to the toString a boolean array.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name
     * @param array  the array to add to the toString
     * @param fullDetail  true for detail, false for summary info, null for style decides
     */
    public void append(StringBuffer buffer, String fieldName, boolean[] array, Boolean fullDetail) {
        appendFieldStart(buffer, fieldName);
        
        if (array == null) {
            appendNullText(buffer, fieldName);
            
        } else if (isFullDetail(fullDetail)) {
            appendDetail(buffer, fieldName, array);
            
        } else {
            appendSummary(buffer, fieldName, array);
        }

        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Append to the toString the detail of a boolean array.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the toString, not null
     */
    protected void appendDetail(StringBuffer buffer, String fieldName, boolean[] array) {
        buffer.append(arrayStart);
        for (int i = 0; i < array.length; i++) {
            if (i > 0) {
                buffer.append(arraySeparator);
            }
            appendDetail(buffer, fieldName, array[i]);
        }
        buffer.append(arrayEnd);
    }

    /**
     * Append to the toString a summary of a boolean array.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the toString, not null
     */
    protected void appendSummary(StringBuffer buffer, String fieldName, boolean[] array) {
        appendSummarySize(buffer, fieldName, array.length);
    }

    //----------------------------------------------------------------------------
    
    /**
     * Append the class name.
     * 
     * @param object  the object whose name to output
     */
    protected void appendClassName(StringBuffer buffer, Object object) {
        if (useClassName) {
            if (useShortClassName) {
                buffer.append(getShortClassName(object.getClass()));
            } else {
                buffer.append(object.getClass().getName());
            }
        }
    }

    /**
     * Append the IdentityHashCode.
     * 
     * @param object  the object whose id to output
     */
    protected void appendIdentityHashCode(StringBuffer buffer, Object object) {
        if (useIdentityHashCode) {
            buffer.append('@');
            buffer.append(Integer.toHexString(System.identityHashCode(object)));
        }
    }

    /**
     * Append the content start to the buffer.
     * 
     * @param buffer  the StringBuffer to populate
     */
    protected void appendContentStart(StringBuffer buffer) {
        buffer.append(contentStart);
    }
    
    /**
     * Append the content end to the buffer.
     * 
     * @param buffer  the StringBuffer to populate
     */
    protected void appendContentEnd(StringBuffer buffer) {
        int len = buffer.length();
        int sepLen = fieldSeparator.length();
        if (len > 0 && sepLen > 0 && len >= sepLen && buffer.charAt(len - 1) == fieldSeparator.charAt(sepLen - 1)) {
            buffer.setLength(len - sepLen);
        }
        buffer.append(contentEnd);
    }
    
    /**
     * Append an indicator for null to the buffer.
     * Default output is '<null>'.
     * 
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     */
    protected void appendNullText(StringBuffer buffer, String fieldName) {
        buffer.append(nullText);
    }
    
    /**
     * Append the field separator to the buffer.
     * 
     * @param buffer  the StringBuffer to populate
     */
    protected void appendFieldSeparator(StringBuffer buffer) {
        buffer.append(fieldSeparator);
    }
    
    /**
     * Append the field start to the buffer.
     * 
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name
     */
    protected void appendFieldStart(StringBuffer buffer, String fieldName) {
        if (useFieldNames && fieldName != null) {
            buffer.append(fieldName);
            buffer.append(fieldNameValueSeparator);
        }
    }
    
    /**
     * Append the field end to the buffer.
     * 
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     */
    protected void appendFieldEnd(StringBuffer buffer, String fieldName) {
        appendFieldSeparator(buffer);
    }
    
    /**
     * Append to the toString a size summary.
     *
     * @param buffer  the StringBuffer to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param size  the size to append
     */
    protected void appendSummarySize(StringBuffer buffer, String fieldName, int size) {
        buffer.append(sizeStartText);
        buffer.append(size);
        buffer.append(sizeEndText);
    }

    /**
     * Is this field to be output in full detail.
     * 
     * @param fullDetail  the detail level requested
     * @return whether full detail is to be shown
     */
    protected boolean isFullDetail(Boolean fullDetailRequest) {
        if (fullDetailRequest == null) {
            return defaultFullDetail;
        }
        return fullDetailRequest.booleanValue();
    }
    
    /**
     * Gets the short class name for a class.
     *
     * @param cls  the class to get the short name of
     * @return the short name
     */
    protected String getShortClassName(Class cls) {
        String name = cls.getName();
        int pos = name.lastIndexOf('.');
        if (pos == -1) {
            return name;
        }
        return name.substring(pos + 1);
    }

    //----------------------------------------------------------------------------
    
    /**
     * Default ToStringStyle.
     * This is an inner class rather than using StandardToStringStyle to
     * ensure its immutability.
     */
    private static class DefaultToStringStyle extends ToStringStyle {
        
        /**
         * Constructor - use the static constant rather than instantiating.
         */
        private DefaultToStringStyle() {
            super();
        }
        
        /**
         * Ensure singleton after serialization.
         */
        private Object readResolve() {
            return ToStringStyle.DEFAULT_STYLE;
        }
        
    }
    
    //----------------------------------------------------------------------------
    
    /**
     * ToStringStyle that does not print out the field names.
     * This is an inner class rather than using StandardToStringStyle to
     * ensure its immutability.
     */
    private static class NoFieldNameToStringStyle extends ToStringStyle {
        
        /**
         * Constructor - use the static constant rather than instantiating.
         */
        private NoFieldNameToStringStyle() {
            super();
            useFieldNames = false;
        }
        
        /**
         * Ensure singleton after serialization.
         */
        private Object readResolve() {
            return ToStringStyle.NO_FIELD_NAMES_STYLE;
        }
        
    }
    
    //----------------------------------------------------------------------------
    
    /**
     * ToStringStyle that does not print out the classname, identity hashcode,
     * content start or field name.
     * This is an inner class rather than using StandardToStringStyle to
     * ensure its immutability.
     */
    private static class SimpleToStringStyle extends ToStringStyle {
        
        /**
         * Constructor - use the static constant rather than instantiating.
         */
        private SimpleToStringStyle() {
            super();
            useClassName = false;
            useIdentityHashCode = false;
            useFieldNames = false;
            contentStart = "";
            contentEnd = "";
        }
        
        /**
         * Ensure singleton after serialization.
         */
        private Object readResolve() {
            return ToStringStyle.SIMPLE_STYLE;
        }
        
    }
    
    //----------------------------------------------------------------------------
    
    /**
     * ToStringStyle that outputs on multiple lines.
     * This is an inner class rather than using StandardToStringStyle to
     * ensure its immutability.
     */
    private static class MultiLineToStringStyle extends ToStringStyle {

        /**
         * Constructor - use the static constant rather than instantiating.
         */
        private MultiLineToStringStyle() {
            super();
            contentStart = "[" + SystemUtils.LINE_SEPARATOR + "  ";
            fieldSeparator = SystemUtils.LINE_SEPARATOR + "  ";
            contentEnd = SystemUtils.LINE_SEPARATOR + "]";
        }
        
        /**
         * Ensure singleton after serialization.
         */
        private Object readResolve() {
            return ToStringStyle.MULTI_LINE_STYLE;
        }
        
    }
    
    //----------------------------------------------------------------------------
    
    // Removed, as the XML style needs more work for escaping characters, arrays,
    // collections, maps and embedded beans.
//    /**
//     * ToStringStyle that outputs in XML style
//     */
//    private static class XMLToStringStyle extends ToStringStyle {
//        
//        /**
//         * Constructor - use the static constant rather than instantiating.
//         */
//        private XMLToStringStyle() {
//            super();
//            nullText = "null";
//            sizeStartText = "size=";
//            sizeEndText = "";
//        }
//        
//        /**
//         * @see ToStringStyle#appendStart(StringBuffer, Object)
//         */
//        public void appendStart(StringBuffer buffer, Object object) {
//            buffer.append('<');
//            buffer.append(getShortClassName(object.getClass()));
//            buffer.append(" class=\"");
//            appendClassName(buffer, object);
//            buffer.append("\" hashCode=\"");
//            appendIdentityHashCode(buffer, object);
//            buffer.append("\">");
//            buffer.append(SystemUtils.LINE_SEPARATOR);
//            buffer.append("  ");
//        }
//
//        /**
//         * @see ToStringStyle#appendFieldStart(StringBuffer, String)
//         */
//        protected void appendFieldStart(StringBuffer buffer, String fieldName) {
//            buffer.append('<');
//            buffer.append(fieldName);
//            buffer.append('>');
//        }
//
//        /**
//         * @see ToStringStyle#appendFieldEnd(StringBuffer, String)
//         */
//        protected void appendFieldEnd(StringBuffer buffer, String fieldName) {
//            buffer.append("</");
//            buffer.append(fieldName);
//            buffer.append('>');
//            buffer.append(SystemUtils.LINE_SEPARATOR);
//            buffer.append("  ");
//        }
//
//        /**
//         * @see ToStringStyle#appendEnd(StringBuffer, Object)
//         */
//        public void appendEnd(StringBuffer buffer, Object object) {
//            int len = buffer.length();
//            if (len > 2 && buffer.charAt(len - 1) == ' ' && buffer.charAt(len - 2) == ' ') {
//                buffer.setLength(len - 2);
//            }
//            buffer.append("</");
//            buffer.append(getShortClassName(object.getClass()));
//            buffer.append("\">");
//        }
//
//    }
    
}
