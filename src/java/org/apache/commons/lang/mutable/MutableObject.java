/*
 * Copyright 2004 The Apache Software Foundation.
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

import java.io.Serializable;

/**
 * A mutable <code>Object</code>.
 * 
 * @since 2.1
 * @version $Id: MutableObject.java,v 1.1 2004/07/08 00:02:35 scolebourne Exp $
 */
public class MutableObject
        implements Mutable, Serializable {

    /** Serialization lock. */
    private static final long serialVersionUID = 86241875189L;

    /** The mutable value. */
    private Object value;

    /**
     * Constructs a new MutableObject with the default value of null.
     */
    public MutableObject() {
        super();
    }

    /**
     * Constructs a new MutableObject with the specified value.
     * 
     * @param value a value.
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
     * @param value  the value to set
     */
    public void setValue(Object value) {
        this.value = value;;
    }

    //-----------------------------------------------------------------------
    /**
     * Checks if this object equals the specified object.
     * <p>
     * The object must be a MutableObject with an equal value to be equal.
     *
     * @param obj  the object to compare to
     * @return true if equal
     */
    public boolean equals(Object obj) {
        if (obj instanceof MutableObject) {
            Object other = ((MutableObject) obj).value;
            return (value == other || (value != null && value.equals(other)));
        }
        return false;
    }

    /**
     * Returns a suitable hashcode for this mutable.
     *
     * @return a suitable hashcode
     */
    public int hashCode() {
        return (value == null ? 0 : value.hashCode());
    }

    /**
     * Returns the String value of this mutable.
     *
     * @return the mutable value as a string
     */
    public String toString() {
        return (value == null ? "null" : value.toString());
    }

}
