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

import java.io.Serializable;

/**
 * A mutable <code>Number</code>.
 *
 * @since 2.1
 * @version $Id: MutableNumber.java,v 1.3 2004/06/24 04:20:46 bayard Exp $
 */
public abstract class MutableNumber
    extends Number
    implements Comparable, Mutable, Serializable {

    MutableNumber() {
        super();
    }

    // ----------------------------------------------------------------
    // Number overrides
    // ----------------------------------------------------------------

    public float floatValue() {
        return (float)doubleValue();
    }
    
    // ----------------------------------------------------------------
    // Object overrides
    // ----------------------------------------------------------------

    public String toString() {
        return String.valueOf(doubleValue()).intern();
    }

    public int hashCode() {
        return super.hashCode();
    }

    /**
     * Compares <code>this</code> to another object.
     * 
     * @param obj an object to compare to
     * @return <code>true</code> if <code>this</code> is equal to 
     * <code>obj</code>.
     * @see #compareTo(Object)
     */
    public boolean equals(Object obj) {
        return super.equals(obj);
    }

    // ----------------------------------------------------------------
    //  Comparable overrides
    // ----------------------------------------------------------------

    /**
     * Compares to another object
     * 
     * @param o an object to compare to
     * @return -1 if <code>this < o</code>, 0 if <code>this.equals(o)</code>, 
     *  1 if <code>this > o<code>
     * @throws ClassCastException if <code>o</code> is not a 
     * <code>Number</code>.
     */
    public int compareTo(Object o) {
        final double d = ((Number)o).doubleValue();
        return (doubleValue() < d) ? -1 : (doubleValue() > d) ? 1 : 0;
    }

}
