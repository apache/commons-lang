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
 * <p>Thrown when an object is an instance of an unexpected type (a class or interface).</p>
 * 
 * @author Matthew Hawthorne
 * @author Gary Gregory
 * @since 2.0
 * @version $Id: IllegalClassException.java,v 1.6 2004/02/18 22:59:50 ggregory Exp $
 */
public class IllegalClassException extends IllegalArgumentException {

    /**
     * <p>Instantiates with the specified types (classes or interfaces).</p>
     * 
     * @param expected  the expected type
     * @param actual  the actual type
     */
    public IllegalClassException(Class expected, Class actual) {
        super(
            "Expected: "
                + safeGetClassName(expected)
                + ", actual: "
                + safeGetClassName(actual));
    }

    /**
     * <p>Instantiates with the specified message.</p>
     * 
     * @param message  the exception message
     */
    public IllegalClassException(String message) {
        super(message);
    }

    /**
     * <p>Returns the class name or <code>null</code> if the class is
     * <code>null</code>.</p>
     * 
     * @param cls  a <code>Class</code>
     * @return the name of <code>cls</code>, or <code>null</code> if if <code>cls</code> is <code>null</code>.
     */
    private static final String safeGetClassName(Class cls) {
        return cls == null ? null : cls.getName();
    }

}
