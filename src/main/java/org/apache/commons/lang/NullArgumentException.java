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
 * <p>Thrown to indicate that an argument was <code>null</code> and should
 * not have been.
 * This exception supplements the standard <code>IllegalArgumentException</code>
 * by providing a more semantically rich description of the problem.</p>
 * 
 * <p><code>NullArgumentException</code> represents the case where a method takes
 * in a parameter that must not be <code>null</code>.
 * Some coding standards would use <code>NullPointerException</code> for this case,
 * others will use <code>IllegalArgumentException</code>.
 * Thus this exception would be used in place of
 * <code>IllegalArgumentException</code>, yet it still extends it.</p>
 * 
 * <pre>
 * public void foo(String str) {
 *   if (str == null) {
 *     throw new NullArgumentException("str");
 *   }
 *   // do something with the string
 * }
 * </pre>
 * 
 * @author Matthew Hawthorne
 * @author Stephen Colebourne
 * @since 2.0
 * @version $Id$
 */
public class NullArgumentException extends IllegalArgumentException {

    /**
     * Required for serialization support.
     * 
     * @see java.io.Serializable
     */
    private static final long serialVersionUID = 1174360235354917591L;

    /**
     * <p>Instantiates with the given argument name.</p>
     *
     * @param argName  the name of the argument that was <code>null</code>.
     */
    public NullArgumentException(String argName) {
        super((argName == null ? "Argument" : argName) + " must not be null.");
    }

}
