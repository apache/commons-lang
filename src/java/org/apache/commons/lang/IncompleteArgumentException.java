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

import java.util.Arrays;

/**
 * <p>Thrown to indicate an incomplete argument to a method.</p>
 * 
 * @author Matthew Hawthorne
 * @since 2.0
 * @version $Id: IncompleteArgumentException.java,v 1.6 2004/02/18 22:59:50 ggregory Exp $
 */
public class IncompleteArgumentException extends IllegalArgumentException {

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
