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

/**
 * <code>StandardToStringStyle</code> works with ToStringBuilder to create a
 * toString.
 * <p>
 * This class is intended to be used as a singleton. There is no need 
 * to instantiate a new style each time. Your code should instantiate the class
 * once, customize the values as required, and then store the result in a 
 * public static final variable for the rest of the program to access.
 *
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @version $Id: StandardToStringStyle.java,v 1.3 2002/09/19 19:45:22 scolebourne Exp $
 */
public class StandardToStringStyle extends ToStringStyle {
    
    /**
     * Constructor.
     */
    public StandardToStringStyle() {
        super();
    }
    
    //---------------------------------------------------------------------
    
    /**
     * Gets whether to use the class name.
     * @return the current useClassName flag
     */
    public boolean isUseClassName() {
        return super.isUseClassName();
    }

    /**
     * Sets whether to use the class name.
     * @param useClassName  the new useClassName flag
     */
    public void setUseClassName(boolean useClassName) {
        super.setUseClassName(useClassName);
    }

    //---------------------------------------------------------------------
    
    /**
     * Gets whether to output short or long class names.
     * @return the current shortClassName flag
     */
    public boolean isShortClassName() {
        return super.isShortClassName();
    }

    /**
     * Sets whether to output short or long class names.
     * @param shortClassName  the new shortClassName flag
     */
    public void setShortClassName(boolean shortClassName) {
        super.setShortClassName(shortClassName);
    }

    //---------------------------------------------------------------------
    
    /**
     * Gets whether to use the identity hash code.
     * @return the current useIdentityHashCode flag
     */
    public boolean isUseIdentityHashCode() {
        return super.isUseIdentityHashCode();
    }

    /**
     * Sets whether to use the identity hash code.
     * @param useFieldNames  the new useIdentityHashCode flag
     */
    public void setUseIdentityHashCode(boolean useIdentityHashCode) {
        super.setUseIdentityHashCode(useIdentityHashCode);
    }

    //---------------------------------------------------------------------
    
    /**
     * Gets whether to use the field names passed in.
     * @return the current useFieldNames flag
     */
    public boolean isUseFieldNames() {
        return super.isUseFieldNames();
    }

    /**
     * Sets whether to use the field names passed in.
     * @param useFieldNames  the new useFieldNames flag
     */
    public void setUseFieldNames(boolean useFieldNames) {
        super.setUseFieldNames(useFieldNames);
    }

    //---------------------------------------------------------------------
    
    /**
     * Gets whether to use full detail when the caller doesn't specify.
     * @return the current defaultFullDetail flag
     */
    public boolean isDefaultFullDetail() {
        return super.isDefaultFullDetail();
    }

    /**
     * Sets whether to use full detail when the caller doesn't specify.
     * @param defaultFullDetail  the new defaultFullDetail flag
     */
    public void setDefaultFullDetail(boolean defaultFullDetail) {
        super.setDefaultFullDetail(defaultFullDetail);
    }

    //---------------------------------------------------------------------
    
    /**
     * Gets whether to output array content detail.
     * @return the current array content detail setting
     */
    public boolean isArrayContentDetail() {
        return super.isArrayContentDetail();
    }
    
    /**
     * Sets whether to output array content detail.
     * @param arrayContentDetail  the new arrayContentDetail flag
     */
    public void setArrayContentDetail(boolean arrayContentDetail) {
        super.setArrayContentDetail(arrayContentDetail);
    }

    //---------------------------------------------------------------------
    
    /**
     * Gets the array start text.
     * @return the current array start text
     */
    public String getArrayStart() {
        return super.getArrayStart();
    }

    /**
     * Sets the array start text.
     * Null is accepted, but will be converted to a blank string.
     * @param arrayStart  the new array start text
     */
    public void setArrayStart(String arrayStart) {
        super.setArrayStart(arrayStart);
    }

    //---------------------------------------------------------------------
    
    /**
     * Gets the array end text.
     * @return the current array end text
     */
    public String getArrayEnd() {
        return super.getArrayEnd();
    }

    /**
     * Sets the array end text.
     * Null is accepted, but will be converted to a blank string.
     * @param arrayEnd  the new array end text
     */
    public void setArrayEnd(String arrayEnd) {
        super.setArrayEnd(arrayEnd);
    }

    //---------------------------------------------------------------------
    
    /**
     * Gets the array separator text.
     * @return the current array separator text
     */
    public String getArraySeparator() {
        return super.getArraySeparator();
    }

    /**
     * Sets the array separator text.
     * Null is accepted, but will be converted to a blank string.
     * @param arraySeparator  the new array separator text
     */
    public void setArraySeparator(String arraySeparator) {
        super.setArraySeparator(arraySeparator);
    }

    //---------------------------------------------------------------------
    
    /**
     * Gets the content start text.
     * @return the current content start text
     */
    public String getContentStart() {
        return super.getContentStart();
    }

    /**
     * Sets the content start text.
     * Null is accepted, but will be converted to a blank string.
     * @param contentStart  the new content start text
     */
    public void setContentStart(String contentStart) {
        super.setContentStart(contentStart);
    }

    //---------------------------------------------------------------------
    
    /**
     * Gets the content end text.
     * @return the current content end text
     */
    public String getContentEnd() {
        return super.getContentEnd();
    }

    /**
     * Sets the content end text.
     * Null is accepted, but will be converted to a blank string.
     * @param contentEnd  the new content end text
     */
    public void setContentEnd(String contentEnd) {
        super.setContentEnd(contentEnd);
    }

    //---------------------------------------------------------------------
    
    /**
     * Gets the field name value separator text.
     * @return the current field name value separator text
     */
    public String getFieldNameValueSeparator() {
        return super.getFieldNameValueSeparator();
    }

    /**
     * Sets the field name value separator text.
     * Null is accepted, but will be converted to a blank string.
     * @param fieldNameValueSeparator  the new field name value separator text
     */
    public void setFieldNameValueSeparator(String fieldNameValueSeparator) {
        super.setFieldNameValueSeparator(fieldNameValueSeparator);
    }

    //---------------------------------------------------------------------
    
    /**
     * Gets the field separator text.
     * @return the current field separator text
     */
    public String getFieldSeparator() {
        return super.getFieldSeparator();
    }

    /**
     * Sets the field separator text.
     * Null is accepted, but will be converted to a blank string.
     * @param fieldSeparator  the new field separator text
     */
    public void setFieldSeparator(String fieldSeparator) {
        super.setFieldSeparator(fieldSeparator);
    }

    //---------------------------------------------------------------------
    
    /**
     * Gets the text to output when null found.
     * @return the current text to output when null found
     */
    public String getNullText() {
        return super.getNullText();
    }

    /**
     * Sets the text to output when null found.
     * Null is accepted, but will be converted to a blank string.
     * @param nullText  the new text to output when null found
     */
    public void setNullText(String nullText) {
        super.setNullText(nullText);
    }

    //---------------------------------------------------------------------
    
    /**
     * Gets the text to output when a Collection, Map or Array size is output.
     * This is output before the size value.
     * @return the current start of size text
     */
    public String getSizeStartText() {
        return super.getSizeStartText();
    }

    /**
     * Sets the text to output when a Collection, Map or Array size is output.
     * This is output before the size value.
     * Null is accepted, but will be converted to a blank string.
     * @param sizeStartText  the new start of size text
     */
    public void setSizeStartText(String sizeStartText) {
        super.setSizeStartText(sizeStartText);
    }

    //---------------------------------------------------------------------
    
    /**
     * Gets the text to output when a Collection, Map or Array size is output.
     * This is output after the size value.
     * @return the current end of size text
     */
    public String getSizeEndText() {
        return super.getSizeEndText();
    }

    /**
     * Sets the text to output when a Collection, Map or Array size is output.
     * This is output after the size value.
     * Null is accepted, but will be converted to a blank string.
     * @param sizeEndText  the new end of size text
     */
    public void setSizeEndText(String sizeEndText) {
        super.setSizeEndText(sizeEndText);
    }

    //---------------------------------------------------------------------
    
    /**
     * Gets the text to output when an Object is output in summary mode.
     * This is output before the size value.
     * @return the current start of summary text
     */
    public String getSummaryObjectStartText() {
        return super.getSummaryObjectStartText();
    }

    /**
     * Sets the text to output when an Object is output in summary mode.
     * This is output before the size value.
     * Null is accepted, but will be converted to a blank string.
     * @param summaryObjectStartText  the new start of summary text
     */
    public void setSummaryObjectStartText(String summaryObjectStartText) {
        super.setSummaryObjectStartText(summaryObjectStartText);
    }

    //---------------------------------------------------------------------
    
    /**
     * Gets the text to output when an Object is output in summary mode.
     * This is output after the size value.
     * @return the current end of summary text
     */
    public String getSummaryObjectEndText() {
        return super.getSummaryObjectEndText();
    }

    /**
     * Sets the text to output when an Object is output in summary mode.
     * This is output after the size value.
     * Null is accepted, but will be converted to a blank string.
     * @param summaryObjectEndText  the new end of summary text
     */
    public void setSummaryObjectEndText(String summaryObjectEndText) {
        super.setSummaryObjectEndText(summaryObjectEndText);
    }

    //---------------------------------------------------------------------
    
}
