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

import java.util.Arrays;

/**
 * <p>Thrown to indicate an incomplete argument to a method.
 * This exception supplements the standard <code>IllegalArgumentException</code>
 * by providing a more semantically rich description of the problem.</p>
 * 
 * <p><code>IncompleteArgumentException</code> represents the case where a method takes
 * in a parameter that has a number of properties, some of which have not been set.
 * A case might be a search requirements bean that must have three properties set
 * in order for the method to run, but only one is actually set.
 * This exception would be used in place of
 * <code>IllegalArgumentException</code>, yet it still extends it.</p>
 * 
 * <pre>
 * public void foo(PersonSearcher search) {
 *   if (search.getSurname() == null ||
 *       search.getForename() == null ||
 *       search.getSex() == null) {
 *     throw new IncompleteArgumentException("search");
 *   }
 *   // do something with the searcher
 * }
 * </pre>
 * 
 * @author Matthew Hawthorne
 * @since 2.0
 * @version $Id$
 */
public class IncompleteArgumentException extends IllegalArgumentException {

    /**
     * Required for serialization support.
     * 
     * @see java.io.Serializable
     */
    private static final long serialVersionUID = 4954193403612068178L;

    /**
     * <p>Instantiates with the specified description.</p>
     * 
     * @param argName  a description of the incomplete argument
     */
    public IncompleteArgumentException(String argName) {
        super(argName + " is incomplete.");
    }

    /**
     * <p>Instantiates with the specified description.</p>
     * 
     * @param argName  a description of the incomplete argument
     * @param items  an array describing the arguments missing
     */
    public IncompleteArgumentException(String argName, String[] items) {
        super(
            argName
                + " is missing the following items: "
                + safeArrayToString(items));
    }

    /**
     * <p>Converts an array to a string without throwing an exception.</p>
     * 
     * @param array  an array
     * @return the array as a string
     */
    private static final String safeArrayToString(Object[] array) {
        return array == null ? null : Arrays.asList(array).toString();
    }

}
