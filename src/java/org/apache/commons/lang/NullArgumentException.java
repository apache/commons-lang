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
 * <p>Thrown to indicate that an argument was <code>null</code> and should
 * not have been.</p>
 * 
 * @author Matthew Hawthorne
 * @author Stephen Colebourne
 * @since 2.0
 * @version $Id: NullArgumentException.java,v 1.7 2004/02/18 22:59:50 ggregory Exp $
 */
public class NullArgumentException extends IllegalArgumentException {

   /**
    * <p>Instantiates with the given argument name.</p>
    *
    * @param argName  the name of the argument that was <code>null</code>.
    */
    public NullArgumentException(String argName) {
        super(argName + " must not be null.");
    }
}
