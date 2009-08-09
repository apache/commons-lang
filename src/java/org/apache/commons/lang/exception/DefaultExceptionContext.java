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
package org.apache.commons.lang.exception;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.SystemUtils;

/**
 * Provides context feature for exceptions.  Used by both checked and unchecked version of the contexted exceptions.
 * @see ContextedRuntimeException
 * @author D. Ashmore
 * @since 3.0
 */
public class DefaultExceptionContext implements ExceptionContext {
    
    private static final long serialVersionUID = 293747957535772807L;
    
    /*
     * This value list could really be obtained from the Map, however, some
     * callers want to control the order of the list as it appears in the 
     * Message.  The list allows that.  name/value pairs will appear in
     * the order that they're provided.   D. Ashmore
     */
    private List<String> contextKeyList = new ArrayList<String>();
    private Map<String, Serializable> contextValueMap = new HashMap<String, Serializable>();
    
    /**
     * Adds information helpful to a developer in diagnosing and correcting
     * the problem.  
     * @see ContextedException#addLabeledValue(String, Serializable)
     * @param label  a textual label associated with information
     * @param value  information needed to understand exception.  May be null.
     * @return this
     * @since 3.0
     */
    public ExceptionContext addLabeledValue(String label, Serializable value) {        
        this.contextKeyList.add(label);
        this.contextValueMap.put(label, value);
        
        return this;
    }
    
    /**
     * Retrieves the value for a given label.
     * @param label  a textual label associated with information
     * @return value  information needed to understand exception.  May be null.
     * @since 3.0
     */
    public Serializable getLabeledValue(String label) {
        return this.contextValueMap.get(label);
    }
    
    /**
     * Retrieves currently defined labels.
     * @return labelSet
     * @since 3.0
     */
    public Set<String> getLabelSet() {
        return this.contextValueMap.keySet();
    }
    
    /**
     * Centralized message logic for both checked and unchecked version of
     * context exceptions
     * @param baseMessage message retained by super class
     * @return message -- exception message
     * @since 3.0
     */
    public String getFormattedExceptionMessage(String baseMessage){
        StringBuffer buffer = new StringBuffer(256);
        if (baseMessage != null) {
            buffer.append(baseMessage);
        }
        
        if (contextKeyList.size() > 0) {
            if (buffer.length() > 0l) {
                buffer.append(SystemUtils.LINE_SEPARATOR);
            }
            buffer.append("Exception Context:");
            buffer.append(SystemUtils.LINE_SEPARATOR); 
            buffer.append("\t");  
            
            Object value;
            String valueStr;
            for (String label: this.contextKeyList) {
                buffer.append("[");
                buffer.append(label);
                buffer.append("=");
                value = this.contextValueMap.get(label);
                if (value == null) {
                    buffer.append("null");
                }
                else {
                    try {valueStr = value.toString();}
                    catch (Throwable t) {
                        valueStr = "Excepted on toString(): " + 
                            ExceptionUtils.getStackTrace(t);
                    }
                    buffer.append(valueStr);
                }
                buffer.append("]");
                buffer.append(SystemUtils.LINE_SEPARATOR);  
                buffer.append("\t");  
            }
            buffer.append("---------------------------------");
        }
        return buffer.toString();
    }
    
}