/*
 * Copyright 2002-2005 The Apache Software Foundation.
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

import java.io.PrintWriter;
import java.io.StringWriter;

/**
 * <p>A PrintWriter that maintains a String as its backing store.</p>
 *
 * <p>Usage:
 * <pre>
 * StringPrintWriter out = new StringPrintWriter();
 * printTo(out);
 * System.out.println( out.getString() );
 * </pre>
 * </p>
 *
 * @author Alex Chaffee
 * @author Scott Stanchfield
 * @author Gary D. Gregory
 * @since 2.0
 */
class StringPrintWriter extends PrintWriter {
    
    /**
     * Constructs a new instance.
     */
    public StringPrintWriter() {
        super(new StringWriter());
    }

    /**
     * Constructs a new instance using the specified initial string-buffer
     * size.
     *
     * @param initialSize  an int specifying the initial size of the buffer.
     */
    public StringPrintWriter(int initialSize) {
        super(new StringWriter(initialSize));
    }

    /**
     * <p>Since toString() returns information *about* this object, we
     * want a separate method to extract just the contents of the
     * internal buffer as a String.</p>
     *
     * @return the contents of the internal string buffer
     */
    public String getString() {
        flush();
        return ((StringWriter) this.out).toString();
    }
    
}

