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
 * A mutable <code>Byte</code>.
 * 
 * @since 2.1
 * @version $Id: MutableByte.java,v 1.2 2004/06/13 06:18:49 bayard Exp $
 */
public class MutableByte extends MutableNumber {

    /**
     * Instantiates with the specified value
     * @param value a value.
     */
    public MutableByte(byte value) {
        super();
        setValue(new Byte(value));
    }

    public void setValue(byte value) {
        setValue(new Byte(value));
    }

    /**
     * @param value a <code>Number</code>
     */
    public void setValue(Object value) {
        setValue(((Number)value).byteValue());
    }

}
