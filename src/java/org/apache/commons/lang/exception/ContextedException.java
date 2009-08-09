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
 * Provides an easier and safer way for developers to provide context when
 * generating checked exceptions.  Often, additional information, besides what's
 * embedded in the exception cause, is needed for developers to debug and correct 
 * a bug.  Often, this additional information can reduce the time it takes
 * to replicate and fix a bug.
 * 
 * <p>ContextedException is easier as developers don't need to be concerned 
 * with formatting the exception message to include additional information 
 * with the exception.  Additional information is automatically included
 * in the message and printed stack trace.  This often thins out exception
 * handling code.</p>
 * 
 * <p>ContextedException is safer as the additional code needed to embed additional
 * information in a normal exception tends to be tested less and is more vulnerable
 * to errors such as null pointer exceptions.</p>
 * 
 * <p>An unchecked version of this exception is provided by ContextedRuntimeException.</p>
 * 
 * <p>To use this class write code as follows:</p>
 *
 * <pre>
 *   try {
 * 
 *   ...
 * 
 *   } catch (Throwable e) {
 *     throw new ContextedException("Error posting account transaction", e)
 *          .addLabeledValue("accountNumber", accountNumber)
 *          .addLabeledValue("amountPosted", amountPosted)
 *          .addLabeledValue("previousBalance", previousBalance)
 *   }
 * }
 * </pre>
 * 
 * <p>The output in a printStacktrace() (which often is written to a log) would look something like the following:
 * <pre>
 * org.apache.commons.lang.exception.ContextedException: java.lang.Exception: Error posting account transaction
 *  Exception Context:
 *  [accountNumber=null]
 *  [amountPosted=100.00]
 *  [previousBalance=-2.17]
 *
 *  ---------------------------------
 *  at org.apache.commons.lang.exception.ContextedExceptionTest.testAddLabeledValue(ContextedExceptionTest.java:88)
 *  ..... (rest of trace)
 * </pre>
 * 
 * @see ContextedRuntimeException
 * @author D. Ashmore
 * @since 3.0
 *
 */
public class ContextedException extends Exception implements ExceptionContext {

    private static final long serialVersionUID = 8940917952810290164L;
    private ExceptionContext exceptionContext = new DefaultExceptionContext();

    /**
     * Instantiates ContextedException without message or cause.
     * <p>DefaultExceptionContext used to store and format context information.</p>
     */
    public ContextedException() {
    }

    /**
     * Instantiates ContextedException with message, but without cause.
     * <p>DefaultExceptionContext used to store and format context information.</p>
     * @param message The exception message
     */
    public ContextedException(String message) {
        super(message);
    }

    /**
     * Instantiates ContextedException with cause, but without message.
     * <p>DefaultExceptionContext used to store and format context information.</p>
     * @param cause Exception creating need for ContextedException
     */
    public ContextedException(Throwable cause) {
        super(cause);
    }

    /**
     * Instantiates ContextedException with cause and message.
     * <p>DefaultExceptionContext used to store and format context information.</p>
     * @param message The exception message
     * @param cause Exception creating need for ContextedException
     */
    public ContextedException(String message, Throwable cause) {
        super(message, cause);
    }
    
    /**
     * Instantiates ContextedException with cause, message, and ExceptionContext.
     * @param message The exception message
     * @param cause Exception creating need for ContextedException
     * @param context Context used to store additional information
     * @since 3.0
     */
    public ContextedException(String message, Throwable cause, ExceptionContext context) {
        super(message, cause);
        if (context != null) {
            this.exceptionContext = context;
        }
    }
    
    /**
     * Adds information helpful to a developer in diagnosing and correcting
     * the problem.  For the information to be meaningful, the value passed
     * should have a reasonable toString() implementation.
     * 
     * <p>Note:  If the value provided isn't Serializable, one solution would be
     * to provide its toString() if it has a meaningful implementation or 
     * individual properties of the value object instead.</p>
     * @param label  a textual label associated with information
     * @param value  information needed to understand exception.  May be <code>null</code>.
     * @return this
     * @since 3.0
     */
    public ContextedException addLabeledValue(String label, Serializable value) {        
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
     * Convenience method to retrieve currently defined labels from the underlying ExceptionContext.
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
