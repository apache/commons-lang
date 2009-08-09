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
 * Provides an unchecked version of ContextedException
 * @see ContextedException
 * @author D. Ashmore
 * @author J&ouml;rg Schaible
 * @since 3.0
 *
 */
public class ContextedRuntimeException extends RuntimeException implements ExceptionContext {

    private static final long serialVersionUID = 1459691936045811817L;
    private ExceptionContext exceptionContext = new DefaultExceptionContext();
    
    /**
     * Instantiates ContextedRuntimeException without message or cause.
     * <p>DefaultExceptionContext used to store and format context information.</p>
     * 
     */
    public ContextedRuntimeException() {
    }

    /**
     * Instantiates ContextedRuntimeException with message, but without cause.
     * <p>DefaultExceptionContext used to store and format context information.</p>
     * @param message The exception message
     * @since 3.0
     */
    public ContextedRuntimeException(String message) {
        super(message);
    }

    /**
     * Instantiates ContextedRuntimeException with cause, but without message.
     * <p>DefaultExceptionContext used to store and format context information.</p>
     * @param cause Exception creating need for ContextedRuntimeException
     * @since 3.0
     */
    public ContextedRuntimeException(Throwable cause) {
        super(cause);
    }

    /**
     * Instantiates ContextedRuntimeException with cause and message.
     * <p>DefaultExceptionContext used to store and format context information.</p>
     * @param message The exception message
     * @param cause Exception creating need for ContextedException
     * @since 3.0
     */
    public ContextedRuntimeException(String message, Throwable cause) {
        this(message, cause, cause instanceof ExceptionContext ? (ExceptionContext)cause : null);
    }
    
    /**
     * Instantiates ContextedRuntimeException with cause, message, and ExceptionContext.
     * @param message The exception message
     * @param cause Exception creating need for ContextedRuntimeException
     * @param context Context used to store additional information
     * @since 3.0
     */
    public ContextedRuntimeException(String message, Throwable cause, ExceptionContext context) {
        super(message, cause);
        if (context != null) {
            this.exceptionContext = context;
        }
    }
    
    /**
     * Adds information helpful to a developer in diagnosing and correcting
     * the problem.  
     * @see ContextedException#addLabeledValue(String, Serializable)
     * @param label  a textual label associated with information
     * @param value  information needed to understand exception.  May be <code>null</code>.
     * @return this
     * @since 3.0
     */
    public ContextedRuntimeException addLabeledValue(String label, Serializable value) {        
        this.exceptionContext.addLabeledValue(label, value);
        return this;
    }
    
    /**
     * Convenience method to retrieve a value from the underlying ExceptionContext.
     * @param label  a textual label associated with information
     * @return value  information needed to understand exception.  May be <code>null</code>.
     * @since 3.0
     */
    public Serializable getLabeledValue(String label) {
        return  this.exceptionContext.getLabeledValue(label);
    }
    
    /**
     * Convenience method to retrieve  currently defined labels from the underlying ExceptionContext.
     * @return labelSet
     * @since 3.0
     */
    public Set<String> getLabelSet() {
        return this.exceptionContext.getLabelSet();
    }
    
    /**
     * Provides message pertaining to exception.
     * @see java.lang.Throwable#getMessage()
     * @return message
     * @since 3.0
     */
    @Override
    public String getMessage(){
        return getFormattedExceptionMessage(super.getMessage());
    }

    /**
     * {@inheritDoc}
     */
    public String getFormattedExceptionMessage(String baseMessage) {
        return this.exceptionContext.getFormattedExceptionMessage(baseMessage);
    }
}
