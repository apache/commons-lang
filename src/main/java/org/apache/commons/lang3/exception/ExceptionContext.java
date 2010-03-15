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

import java.util.Set;

/**
 * Allows the storage and retrieval of contextual information based on label-value
 * pairs for exceptions.
 * 
 * @see ContextedException
 * @see ContextedRuntimeException
 * @author Apache Software Foundation
 * @author D. Ashmore
 * @since 3.0
 */
public interface ExceptionContext {

    /**
     * Adds a contextual label-value pair into this context.
     * <p>
     * This label-value pair provides information useful for debugging. If the
     * provided label already exists, it depends on the implementation what
     * happens with the new value. 
     * </p>
     * 
     * @param label  the label of the item to add, null not recommended
     * @param value  the value of item to add, may be null
     * @return context itself to allow method chaining
     */
    public ExceptionContext addValue(String label, Object value);

    /**
     * Replaces a contextual label-value pair of this context.
     * <p>
     * This label-value pair provides information useful for debugging. If the
     * label does not exist yet, it depends on the implementation what happens
     * with the provided value.
     * </p>
     * 
     * @param label  the label of the item to add, null not recommended
     * @param value  the value of item to add, may be null
     * @return context itself to allow method chaining
     */
    public ExceptionContext replaceValue(String label, Object value);

    /**
     * Retrieves a contextual data value associated with the label.
     * 
     * @param label  the label to get the contextual value for, may be null
     * @return the contextual value associated with the label, may be null
     */
    public Object getValue(String label);

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
