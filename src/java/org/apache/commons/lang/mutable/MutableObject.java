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

package org.apache.commons.lang.mutable;

import java.io.Serializable;

/**
 * A mutable <code>Object</code> wrapper.
 * 
 * @since 2.1
 * @version $Id$
 */
public class MutableObject implements Mutable, Serializable {

    /**
     * Required for serialization support.
     * 
     * @see java.io.Serializable
     */
    private static final long serialVersionUID = 86241875189L;

    /** The mutable value. */
    private Object value;

    /**
     * Constructs a new MutableObject with the default value of <code>null</code>.
     */
    public MutableObject() {
        super();
    }

    /**
     * Constructs a new MutableObject with the specified value.
     * 
     * @param value
     *            a value.
     */
    public MutableObject(Object value) {
        super();
        this.value = value;
    }

    //-----------------------------------------------------------------------
    /**
     * Gets the value.
     * 
     * @return the value
     */
    public Object getValue() {
        return this.value;
    }

    /**
     * Sets the value.
     * 
     * @param value
     *            the value to set
     */
    public void setValue(Object value) {
        this.value = value;
    }

    //-----------------------------------------------------------------------
    /**
     * Compares this object against the specified object. The result is <code>true</code> if and only if the argument
     * is not <code>null</code> and is a <code>MutableObject</code> object that contains the same <code>Object</code>
     * value as this object.
     * 
     * @param obj
     *            the object to compare with.
     * @return <code>true</code> if the objects are the same; <code>false</code> otherwise.
     */
    public boolean equals(Object obj) {
        if (obj instanceof MutableObject) {
            Object other = ((MutableObject) obj).value;
            return value == other || (value != null && value.equals(other));
        }
        return false;
    }

    /**
     * Returns the value's hash code or <code>0</code> if the value is <code>null</code>.
     * 
     * @return the value's hash code or <code>0</code> if the value is <code>null</code>.
     */
    public int hashCode() {
        return value == null ? 0 : value.hashCode();
    }

    /**
     * Returns the String value of this mutable.
     * 
     * @return the mutable value as a string
     */
    public String toString() {
        return value == null ? "null" : value.toString();
    }

}
