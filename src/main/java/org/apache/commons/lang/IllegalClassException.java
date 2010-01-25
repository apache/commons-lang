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
package org.apache.commons.lang;

/**
 * <p>Thrown when an object is an instance of an unexpected type (a class or interface).
 * This exception supplements the standard <code>IllegalArgumentException</code>
 * by providing a more semantically rich description of the problem.</p>
 * 
 * <p><code>IllegalClassException</code> represents the case where a method takes
 * in a genericly typed parameter like Object (typically because it has to due to some
 * other interface it implements), but this implementation only actually accepts a specific
 * type, for example String. This exception would be used in place of
 * <code>IllegalArgumentException</code>, yet it still extends it.</p>
 * 
 * <pre>
 * public void foo(Object obj) {
 *   if (obj instanceof String == false) {
 *     throw new IllegalClassException(String.class, obj);
 *   }
 *   // do something with the string
 * }
 * </pre>
 * 
 * @author Matthew Hawthorne
 * @author Gary Gregory
 * @author Stephen Colebourne
 * @since 2.0
 * @version $Id$
 */
public class IllegalClassException extends IllegalArgumentException {

    /**
     * Required for serialization support.
     * 
     * @see java.io.Serializable
     */
    private static final long serialVersionUID = 8063272569377254819L;

    /**
     * <p>Instantiates with the expected type, and actual object.</p>
     * 
     * @param expected  the expected type
     * @param actual  the actual object
     * @since 2.1
     */
    public IllegalClassException(Class expected, Object actual) {
        super(
            "Expected: "
                + safeGetClassName(expected)
                + ", actual: "
                + (actual == null ? "null" : actual.getClass().getName()));
    }

    /**
     * <p>Instantiates with the expected and actual types.</p>
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
