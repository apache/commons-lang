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

/**
 * <p>Thrown to indicate that a method has not been implemented.</p>
 * 
 * @author Matthew Hawthorne
 * @since 2.0
 * @version $Id: NotImplementedException.java,v 1.6 2004/02/18 22:59:49 ggregory Exp $
 */
public class NotImplementedException extends UnsupportedOperationException {

    /**
     * <p>Constructs the exception with the specified class.</p>
     * 
     * @param clazz  the <code>Class</code> that has not implemented the method
     */
    public NotImplementedException(Class clazz) {
        super(
            "Method is not implemented in class "
                + ((clazz == null) ? null : clazz.getName()));
    }

    /**
     * <p>Constructs the exception with the specified message.</p>
     * 
     * @param msg  the exception message.
     */
    public NotImplementedException(String msg) {
        super(msg);
    }

}
