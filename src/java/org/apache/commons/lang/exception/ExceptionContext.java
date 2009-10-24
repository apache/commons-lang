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
     * Adds a contextual label-value pair into this context.
     * <p>
     * This label-value pair provides information useful for debugging.
     * 
     * @param label  the label of the item to add, null not recommended
     * @param value  the value of item to add, may be null
     * @return context itself to allow method chaining
     */
    public ExceptionContext addLabeledValue(String label, Serializable value);

    /**
     * Retrieves a contextual data value associated with the label.
     * 
     * @param label  the label to get the contextual value for, may be null
     * @return the contextual value associated with the label, may be null
     */
    public Serializable getLabeledValue(String label);

    /**
     * Retrieves the labels defined in the contextual data.
     * 
     * @return the set of labels, never null
     */
    public Set<String> getLabelSet();

    /**
     * Implementors provide the given base message with context label/value item 
     * information appended.
     * 
     * @param baseMessage  the base exception message <b>without</b> context information appended
     * @return the exception message <b>with</b> context information appended, never null
     */
    public String getFormattedExceptionMessage(String baseMessage);

}
