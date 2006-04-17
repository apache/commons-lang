/*
 * Copyright 2006 The Apache Software Foundation.
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
package org.apache.commons.lang.text;

import java.text.Format;
import java.text.FieldPosition;
import java.text.ParsePosition;

/**
 * Formats using one formatter and parses using a different formatter.
 * An example of use for this would be a webapp where data is taken in one way
 * and stored in a database another way.
 *
 * @author Archimedes Trajano
 */
public class CompositeFormat extends Format {

    private final Format parser;
    private final Format formatter;

    /**
     * Create a format that points its parseObject method to one implementation 
     * and its format method to another. 
     *
     * @param Format parser implementation
     * @param Format formatter implementation
     */
    public CompositeFormat(Format parser, Format formatter) {
        this.parser = parser;
        this.formatter = formatter;
    }

    /**
     * Uses the formatter Format instance. 
     *
     * @see Format.format(Object, StringBuffer, FieldPosition)
     */
    public StringBuffer format(Object obj, StringBuffer toAppendTo, FieldPosition pos) {
        return formatter.format(obj,toAppendTo,pos);
    }

    /**
     * Uses the parser Format instance. 
     *
     * @see Format.parseObject(String, ParsePosition)
     */
    public Object parseObject(String source, ParsePosition pos) {
        return parser.parseObject(source,pos);
    }

    /**
     * Provides access to the parser Format implementation. 
     *
     * @return Parser Format implementation
     */
    public Format getParser() {
        return this.parser;
    }

    /**
     * Provides access to the parser Format implementation. 
     *
     * @return Formatter Format implementation
     */
    public Format getFormatter() {
        return this.formatter;
    }

}
