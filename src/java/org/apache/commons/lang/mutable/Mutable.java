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
package org.apache.commons.lang.mutable;

/**
 * A mutable object.
 *
 * @author Matthew Hawthorne
 * @since 2.1
 * @version $Id: Mutable.java,v 1.1 2004/06/11 02:26:32 matth Exp $
 */
public interface Mutable {

    /**
     * Sets the value of this object.
     *
     * @param value the value of this object.
     */
    public void setValue(Object value);

    /**
     * Gets the value of this object.
     *
     * @return a value.
     */
    public Object getValue();

}
