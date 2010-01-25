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
package org.apache.commons.lang.builder;

/**
 * <p>Works with {@link ToStringBuilder} to create a <code>toString</code>.</p>
 *
 * <p>This class is intended to be used as a singleton.
 * There is no need to instantiate a new style each time.
 * Simply instantiate the class once, customize the values as required, and
 * store the result in a public static final variable for the rest of the
 * program to access.</p>
 *
 * @author Stephen Colebourne
 * @author Pete Gieser
 * @author Gary Gregory
 * @since 1.0
 * @version $Id$
 */
public class StandardToStringStyle extends ToStringStyle {
    
    /**
     * Required for serialization support.
     * 
     * @see java.io.Serializable
     */
    private static final long serialVersionUID = 1L;

    /**
     * <p>Constructor.</p>
     */
    public StandardToStringStyle() {
        super();
    }
    
    //---------------------------------------------------------------------
    
    /**
     * <p>Gets whether to use the class name.</p>
     *
     * @return the current useClassName flag
     */
    public boolean isUseClassName() {
        return super.isUseClassName();
    }

    /**
     * <p>Sets whether to use the class name.</p>
     *
     * @param useClassName  the new useClassName flag
     */
    public void setUseClassName(boolean useClassName) {
        super.setUseClassName(useClassName);
    }

    //---------------------------------------------------------------------
    
    /**
     * <p>Gets whether to output short or long class names.</p>
     *
     * @return the current useShortClassName flag
     * @since 2.0
     */
    public boolean isUseShortClassName() {
        return super.isUseShortClassName();
    }

    /**
     * <p>Gets whether to output short or long class names.</p>
     *
     * @return the current shortClassName flag
     * @deprecated Use {@link #isUseShortClassName()}
     *             Method will be removed in Commons Lang 3.0.
     */
    public boolean isShortClassName() {
        return super.isUseShortClassName();
    }

    /**
     * <p>Sets whether to output short or long class names.</p>
     *
     * @param useShortClassName  the new useShortClassName flag
     * @since 2.0
     */
    public void setUseShortClassName(boolean useShortClassName) {
        super.setUseShortClassName(useShortClassName);
    }

    /**
     * <p>Sets whether to output short or long class names.</p>
     *
     * @param shortClassName  the new shortClassName flag
     * @deprecated Use {@link #setUseShortClassName(boolean)}
     *             Method will be removed in Commons Lang 3.0.
     */
    public void setShortClassName(boolean shortClassName) {
        super.setUseShortClassName(shortClassName);
    }

    //---------------------------------------------------------------------
    
    /**
     * <p>Gets whether to use the identity hash code.</p>
     * @return the current useIdentityHashCode flag
     */
    public boolean isUseIdentityHashCode() {
        return super.isUseIdentityHashCode();
    }

    /**
     * <p>Sets whether to use the identity hash code.</p>
     *
     * @param useIdentityHashCode  the new useIdentityHashCode flag
     */
    public void setUseIdentityHashCode(boolean useIdentityHashCode) {
        super.setUseIdentityHashCode(useIdentityHashCode);
    }

    //---------------------------------------------------------------------
    
    /**
     * <p>Gets whether to use the field names passed in.</p>
     *
     * @return the current useFieldNames flag
     */
    public boolean isUseFieldNames() {
        return super.isUseFieldNames();
    }

    /**
     * <p>Sets whether to use the field names passed in.</p>
     *
     * @param useFieldNames  the new useFieldNames flag
     */
    public void setUseFieldNames(boolean useFieldNames) {
        super.setUseFieldNames(useFieldNames);
    }

    //---------------------------------------------------------------------
    
    /**
     * <p>Gets whether to use full detail when the caller doesn't
     * specify.</p>
     *
     * @return the current defaultFullDetail flag
     */
    public boolean isDefaultFullDetail() {
        return super.isDefaultFullDetail();
    }

    /**
     * <p>Sets whether to use full detail when the caller doesn't
     * specify.</p>
     *
     * @param defaultFullDetail  the new defaultFullDetail flag
     */
    public void setDefaultFullDetail(boolean defaultFullDetail) {
        super.setDefaultFullDetail(defaultFullDetail);
    }

    //---------------------------------------------------------------------
    
    /**
     * <p>Gets whether to output array content detail.</p>
     *
     * @return the current array content detail setting
     */
    public boolean isArrayContentDetail() {
        return super.isArrayContentDetail();
    }
    
    /**
     * <p>Sets whether to output array content detail.</p>
     *
     * @param arrayContentDetail  the new arrayContentDetail flag
     */
    public void setArrayContentDetail(boolean arrayContentDetail) {
        super.setArrayContentDetail(arrayContentDetail);
    }

    //---------------------------------------------------------------------
    
    /**
     * <p>Gets the array start text.</p>
     *
     * @return the current array start text
     */
    public String getArrayStart() {
        return super.getArrayStart();
    }

    /**
     * <p>Sets the array start text.</p>
     *
     * <p><code>null</code> is accepted, but will be converted
     * to an empty String.</p>
     *
     * @param arrayStart  the new array start text
     */
    public void setArrayStart(String arrayStart) {
        super.setArrayStart(arrayStart);
    }

    //---------------------------------------------------------------------
    
    /**
     * <p>Gets the array end text.</p>
     *
     * @return the current array end text
     */
    public String getArrayEnd() {
        return super.getArrayEnd();
    }

    /**
     * <p>Sets the array end text.</p>
     *
     * <p><code>null</code> is accepted, but will be converted
     * to an empty String.</p>
     *
     * @param arrayEnd  the new array end text
     */
    public void setArrayEnd(String arrayEnd) {
        super.setArrayEnd(arrayEnd);
    }

    //---------------------------------------------------------------------
    
    /**
     * <p>Gets the array separator text.</p>
     *
     * @return the current array separator text
     */
    public String getArraySeparator() {
        return super.getArraySeparator();
    }

    /**
     * <p>Sets the array separator text.</p>
     *
     * <p><code>null</code> is accepted, but will be converted
     * to an empty String.</p>
     *
     * @param arraySeparator  the new array separator text
     */
    public void setArraySeparator(String arraySeparator) {
        super.setArraySeparator(arraySeparator);
    }

    //---------------------------------------------------------------------
    
    /**
     * <p>Gets the content start text.</p>
     *
     * @return the current content start text
     */
    public String getContentStart() {
        return super.getContentStart();
    }

    /**
     * <p>Sets the content start text.</p>
     *
     * <p><code>null</code> is accepted, but will be converted
     * to an empty String.</p>
     *
     * @param contentStart  the new content start text
     */
    public void setContentStart(String contentStart) {
        super.setContentStart(contentStart);
    }

    //---------------------------------------------------------------------
    
    /**
     * <p>Gets the content end text.</p>
     *
     * @return the current content end text
     */
    public String getContentEnd() {
        return super.getContentEnd();
    }

    /**
     * <p>Sets the content end text.</p>
     *
     * <p><code>null</code> is accepted, but will be converted
     * to an empty String.</p>
     *
     * @param contentEnd  the new content end text
     */
    public void setContentEnd(String contentEnd) {
        super.setContentEnd(contentEnd);
    }

    //---------------------------------------------------------------------
    
    /**
     * <p>Gets the field name value separator text.</p>
     *
     * @return the current field name value separator text
     */
    public String getFieldNameValueSeparator() {
        return super.getFieldNameValueSeparator();
    }

    /**
     * <p>Sets the field name value separator text.</p>
     *
     * <p><code>null</code> is accepted, but will be converted
     * to an empty String.</p>
     *
     * @param fieldNameValueSeparator  the new field name value separator text
     */
    public void setFieldNameValueSeparator(String fieldNameValueSeparator) {
        super.setFieldNameValueSeparator(fieldNameValueSeparator);
    }

    //---------------------------------------------------------------------
    
    /**
     * <p>Gets the field separator text.</p>
     *
     * @return the current field separator text
     */
    public String getFieldSeparator() {
        return super.getFieldSeparator();
    }

    /**
     * <p>Sets the field separator text.</p>
     *
     * <p><code>null</code> is accepted, but will be converted
     * to an empty String.</p>
     *
     * @param fieldSeparator  the new field separator text
     */
    public void setFieldSeparator(String fieldSeparator) {
        super.setFieldSeparator(fieldSeparator);
    }

    //---------------------------------------------------------------------
    
    /**
     * <p>Gets whether the field separator should be added at the start 
     * of each buffer.</p>
     * 
     * @return the fieldSeparatorAtStart flag
     * @since 2.0
     */
    public boolean isFieldSeparatorAtStart() {
        return super.isFieldSeparatorAtStart();
    }

    /**
     * <p>Sets whether the field separator should be added at the start 
     * of each buffer.</p>
     * 
     * @param fieldSeparatorAtStart  the fieldSeparatorAtStart flag
     * @since 2.0
     */
    public void setFieldSeparatorAtStart(boolean fieldSeparatorAtStart) {
        super.setFieldSeparatorAtStart(fieldSeparatorAtStart);
    }

    //---------------------------------------------------------------------
    
    /**
     * <p>Gets whether the field separator should be added at the end 
     * of each buffer.</p>
     * 
     * @return fieldSeparatorAtEnd flag
     * @since 2.0
     */
    public boolean isFieldSeparatorAtEnd() {
        return super.isFieldSeparatorAtEnd();
    }

    /**
     * <p>Sets whether the field separator should be added at the end 
     * of each buffer.</p>
     * 
     * @param fieldSeparatorAtEnd  the fieldSeparatorAtEnd flag
     * @since 2.0
     */
    public void setFieldSeparatorAtEnd(boolean fieldSeparatorAtEnd) {
        super.setFieldSeparatorAtEnd(fieldSeparatorAtEnd);
    }

    //---------------------------------------------------------------------
    
    /**
     * <p>Gets the text to output when <code>null</code> found.</p>
     *
     * @return the current text to output when <code>null</code> found
     */
    public String getNullText() {
        return super.getNullText();
    }

    /**
     * <p>Sets the text to output when <code>null</code> found.</p>
     *
     * <p><code>null</code> is accepted, but will be converted
     * to an empty String.</p>
     *
     * @param nullText  the new text to output when <code>null</code> found
     */
    public void setNullText(String nullText) {
        super.setNullText(nullText);
    }

    //---------------------------------------------------------------------
    
    /**
     * <p>Gets the text to output when a <code>Collection</code>,
     * <code>Map</code> or <code>Array</code> size is output.</p>
     *
     * <p>This is output before the size value.</p>
     *
     * @return the current start of size text
     */
    public String getSizeStartText() {
        return super.getSizeStartText();
    }

    /**
     * <p>Sets the start text to output when a <code>Collection</code>,
     * <code>Map</code> or <code>Array</code> size is output.</p>
     *
     * <p>This is output before the size value.</p>
     *
     * <p><code>null</code> is accepted, but will be converted to
     * an empty String.</p>
     *
     * @param sizeStartText  the new start of size text
     */
    public void setSizeStartText(String sizeStartText) {
        super.setSizeStartText(sizeStartText);
    }

    //---------------------------------------------------------------------
    
    /**
     * Gets the end text to output when a <code>Collection</code>,
     * <code>Map</code> or <code>Array</code> size is output.</p>
     *
     * <p>This is output after the size value.</p>
     *
     * @return the current end of size text
     */
    public String getSizeEndText() {
        return super.getSizeEndText();
    }

    /**
     * <p>Sets the end text to output when a <code>Collection</code>,
     * <code>Map</code> or <code>Array</code> size is output.</p>
     *
     * <p>This is output after the size value.</p>
     *
     * <p><code>null</code> is accepted, but will be converted
     * to an empty String.</p>
     *
     * @param sizeEndText  the new end of size text
     */
    public void setSizeEndText(String sizeEndText) {
        super.setSizeEndText(sizeEndText);
    }

    //---------------------------------------------------------------------
    
    /**
     * <p>Gets the start text to output when an <code>Object</code> is
     * output in summary mode.</p>
     *
     * <P>This is output before the size value.</p>
     *
     * @return the current start of summary text
     */
    public String getSummaryObjectStartText() {
        return super.getSummaryObjectStartText();
    }

    /**
     * <p>Sets the start text to output when an <code>Object</code> is
     * output in summary mode.</p>
     *
     * <p>This is output before the size value.</p>
     *
     * <p><code>null</code> is accepted, but will be converted to
     * an empty String.</p>
     *
     * @param summaryObjectStartText  the new start of summary text
     */
    public void setSummaryObjectStartText(String summaryObjectStartText) {
        super.setSummaryObjectStartText(summaryObjectStartText);
    }

    //---------------------------------------------------------------------
    
    /**
     * <p>Gets the end text to output when an <code>Object</code> is
     * output in summary mode.</p>
     *
     * <p>This is output after the size value.</p>
     *
     * @return the current end of summary text
     */
    public String getSummaryObjectEndText() {
        return super.getSummaryObjectEndText();
    }

    /**
     * <p>Sets the end text to output when an <code>Object</code> is
     * output in summary mode.</p>
     *
     * <p>This is output after the size value.</p>
     *
     * <p><code>null</code> is accepted, but will be converted to
     * an empty String.</p>
     *
     * @param summaryObjectEndText  the new end of summary text
     */
    public void setSummaryObjectEndText(String summaryObjectEndText) {
        super.setSummaryObjectEndText(summaryObjectEndText);
    }

    //---------------------------------------------------------------------
    
}
