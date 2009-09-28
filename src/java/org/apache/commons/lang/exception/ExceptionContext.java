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
import java.util.Set;


/**
 * Provides context information for exceptions. It is available as separate interface to allow
 * it usage independently from the {@link ContextedException} and
 * {@link ContextedRuntimeException}.
 * 
 * @see ContextedException
 * @see ContextedRuntimeException
 * @author D. Ashmore
 * @since 3.0
 */
public interface ExceptionContext extends Serializable {
    
    /**
     * Adds a context item along with a label.  
     * @param label label of item
     * @param value value of item
     * @return context itself to allow method chaining
     * @since 3.0
     */
    public ExceptionContext addLabeledValue(String label, Serializable value);
    
    /**
     * Provides context information associated with the given label.
     * @param label label of item
     * @return value value associated with label
     * @since 3.0
     */
    public Serializable getLabeledValue(String label);
    
    /**
     * Provides a set of labels that are currently in the context.
     * @return labelSet labels currently used by the context
     * @since 3.0
     */
    public Set<String> getLabelSet();
    
    /**
     * Implementors provide the given base message with context label/value item 
     * information appended.
     * @param baseMessage exception message <b>without</b> context information appended
     * @return formattedMessage exception message <b>with</b> context information appended
     * @since 3.0
     */
    public String getFormattedExceptionMessage(String baseMessage);

}