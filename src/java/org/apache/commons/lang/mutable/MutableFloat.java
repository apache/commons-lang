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
 * A mutable <code>Float</code>
 * 
 * @since 2.1
 * @version $Id: MutableFloat.java,v 1.3 2004/06/24 04:20:46 bayard Exp $
 */
public class MutableFloat extends MutableNumber {

    /**
     * Internal value.
     */
    private float value;

    /**
     * Instantiates with the specified value
     * 
     * @param value a value.
     */
    public MutableFloat(float value) {
        super();
        this.value = value;
    }

    public void setValue(float value) {
        this.value = value;
    }

    public double doubleValue() {
        return this.value;
    }

    public int intValue() {
        return (int)this.value;
    }

    public long longValue() {
        return (long)this.value;
    }

    public Object getValue() {
        return new Float(this.value);
    }

    public void setValue(Object value) {
        setValue(((Number)value).floatValue());
    }

}
