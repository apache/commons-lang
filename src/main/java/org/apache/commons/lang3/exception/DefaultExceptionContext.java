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
package org.apache.commons.lang3.exception;

import java.io.Serializable;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang3.SystemUtils;

/**
 * Default implementation of the context storing the label-value pairs for contexted exceptions.
 * <p>
 * This implementation is serializable, however this is dependent on the values that
 * are added also being serializable.
 * 
 * @author Apache Software Foundation
 * @author D. Ashmore
 * @since 3.0
 */
class DefaultExceptionContext implements ExceptionContext, Serializable {

    /** The serialization version. */
    private static final long serialVersionUID = 293747957535772807L;
    /** The ordered map storing the label-data pairs. */
    private Map<String, Object> contextValueMap = new LinkedHashMap<String, Object>();

    /**
     * Adds a contextual label-value pair into this context.
     * <p>
     * This label-value pair provides information useful for debugging. If the
     * label already exists and the provided information is different, the 
     * label will be added with an appended index.
     * </p>
     * 
     * @param label  the label of the item to add, null not recommended
     * @param value  the value of item to add, may be null
     * @return this, for method chaining
     */
    public ExceptionContext addValue(String label, Object value) {        
        String key = label;
        int i = 0;
        while (contextValueMap.containsKey(key)) {
            Object information = contextValueMap.get(key);
            if ((value == null && information == null)
                    || (value != null && value.equals(information)))
                return this;
            key = label + "[" + ++i +"]";
        }
        contextValueMap.put(key, value);
        return this;
    }

    /**
     * Replaces a contextual label-value pair of this context.
     * <p>
     * This label-value pair provides information useful for debugging. If the
     * label does not yet exists, a simply add operation is performed.
     * </p>
     * 
     * @param label  the label of the item to add, null not recommended
     * @param value  the value of item to add, may be null
     * @return this, for method chaining
     */
    public ExceptionContext replaceValue(String label, Object value) {        
        contextValueMap.put(label, value);
        return this;
    }

    /**
     * Retrieves a contextual data value associated with the label.
     * 
     * @param label  the label to get the contextual value for, may be null
     * @return the contextual value associated with the label, may be null
     */
    public Object getValue(String label) {
        return contextValueMap.get(label);
    }

    /**
     * Retrieves the labels defined in the contextual data.
     * 
     * @return the set of labels, never null
     */
    public Set<String> getLabelSet() {
        return contextValueMap.keySet();
    }

    /**
     * Builds the message containing the contextual information.
     * 
     * @param baseMessage  the base exception message <b>without</b> context information appended
     * @return the exception message <b>with</b> context information appended, never null
     */
    public String getFormattedExceptionMessage(String baseMessage){
        StringBuilder buffer = new StringBuilder(256);
        if (baseMessage != null) {
            buffer.append(baseMessage);
        }
        
        if (contextValueMap.size() > 0) {
            if (buffer.length() > 0l) {
                buffer.append(SystemUtils.LINE_SEPARATOR);
            }
            buffer.append("Exception Context:");
            buffer.append(SystemUtils.LINE_SEPARATOR); 
            buffer.append("\t");  
            
            Object value;
            String valueStr;
            for (String label : contextValueMap.keySet()) {
                buffer.append("[");
                buffer.append(label);
                buffer.append("=");
                value = this.contextValueMap.get(label);
                if (value == null) {
                    buffer.append("null");
                }
                else {
                    try {
                        valueStr = value.toString();
                    }
                    catch (Exception e) {
                        valueStr = "Exception thrown on toString(): " + ExceptionUtils.getStackTrace(e);
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
