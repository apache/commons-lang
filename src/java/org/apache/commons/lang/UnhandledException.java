/*
 * Copyright 2002-2004 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang;

import org.apache.commons.lang.exception.NestableRuntimeException;

/**
 * Thrown when it is impossible or undesirable to consume
 * or throw a checked exception.
 *
 * @author Matthew Hawthorne
 * @since 2.0
 * @version $Id: UnhandledException.java,v 1.5 2004/02/18 22:59:50 ggregory Exp $
 */
public class UnhandledException extends NestableRuntimeException {

    /**
     * Constructs the exception using a cause.
     *
     * @param cause  the underlying cause
     */
    public UnhandledException(Throwable cause) {
        super(cause);
    }

    /**
     * Constructs the exception using a message and cause.
     *
     * @param message  the message to use
     * @param cause  the underlying cause
     */
    public UnhandledException(String message, Throwable cause) {
        super(message, cause);
    }

}
