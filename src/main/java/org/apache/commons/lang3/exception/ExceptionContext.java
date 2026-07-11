/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang3.exception;

import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.tuple.Pair;

/**
 * Allows the storage and retrieval of contextual information based on label-value
 * pairs for exceptions.
 * <p>
 * Implementations are expected to manage the pairs in a list-style collection
 * that keeps the pairs in the sequence of their addition.
 * </p>
 *
 * @see ContextedException
 * @see ContextedRuntimeException
 * @since 3.0
 */
public interface ExceptionContext {

    /**
     * Adds a contextual label-value pair into this context.
     * <p>
     * The pair will be added to the context, independently of an already
     * existing pair with the same label.
     * </p>
     *
     * @param label  The label of the item to add, {@code null} not recommended
     * @param value  The value of item to add, may be {@code null}
     * @return {@code this}, for method chaining, not {@code null}
     */
    ExceptionContext addContextValue(String label, Object value);

    /**
     * Gets the full list of label-value pairs defined in the contextual data.
     *
     * @return The list of pairs, not {@code null}
     */
    List<Pair<String, Object>> getContextEntries();

    /**
     * Gets the full set of labels defined in the contextual data.
     *
     * @return The set of labels, not {@code null}
     */
    Set<String> getContextLabels();

    /**
     * Gets all the contextual data values associated with the label.
     *
     * @param label  The label to get the contextual values for, may be {@code null}
     * @return The contextual values associated with the label, never {@code null}
     */
    List<Object> getContextValues(String label);

    /**
     * Gets the first available contextual data value associated with the label.
     *
     * @param label  The label to get the contextual value for, may be {@code null}
     * @return The first contextual value associated with the label, may be {@code null}
     */
    Object getFirstContextValue(String label);

    /**
     * Gets the contextualized error message based on a base message.
     * This will add the context label-value pairs to the message.
     *
     * @param baseMessage  The base exception message <strong>without</strong> context information appended
     * @return The exception message <strong>with</strong> context information appended, not {@code null}
     */
    String getFormattedExceptionMessage(String baseMessage);

    /**
     * Sets a contextual label-value pair into this context.
     * <p>
     * The pair will be added normally, but any existing label-value pair with
     * the same label is removed from the context.
     * </p>
     *
     * @param label  The label of the item to add, {@code null} not recommended
     * @param value  The value of item to add, may be {@code null}
     * @return {@code this}, for method chaining, not {@code null}
     */
    ExceptionContext setContextValue(String label, Object value);

}
