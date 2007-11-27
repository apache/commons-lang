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
package org.apache.commons.lang.text;

import java.text.FieldPosition;
import java.text.Format;
import java.text.ParsePosition;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * Support class for implementing Formats that parse/format other Formats, with
 * specific support for interoperability with ExtendedMessageFormat.
 * 
 * @see ExtendedMessageFormat
 * @author Matt Benson
 * @since 2.4
 * @version $Id$
 */
public abstract class MetaFormatSupport extends Format {

    private static final char END_FE = '}';
    private static final char START_FE = '{';
    private static final char QUOTE = '\'';

    /**
     * Invert the specified Map.
     * 
     * @param map the Map to invert.
     * @return a new Map instance.
     * @throws NullPointerException if <code>map</code> is <code>null</code>.
     */
    protected Map invert(Map map) {
        Map result = new HashMap(map.size());
        for (Iterator iter = map.entrySet().iterator(); iter.hasNext();) {
            Map.Entry entry = (Map.Entry) iter.next();
            result.put(entry.getValue(), entry.getKey());
        }
        return result;
    }

    /**
     * Find the end of the subformat.
     * 
     * @param source String
     * @param pos current parse position
     */
    protected void seekFormatElementEnd(String source, ParsePosition pos) {
        int depth = 1;
        boolean quote = false;
        for (; pos.getIndex() < source.length(); next(pos)) {
            switch (source.charAt(pos.getIndex())) {
            case QUOTE:
                quote ^= true;
                break;
            case START_FE:
                depth += quote ? 0 : 1;
                break;
            case END_FE:
                depth -= quote ? 0 : 1;
                if (depth == 0) {
                    return;
                }
                break;
            }
        }
    }

    /**
     * Advance the parse index by 1.
     * 
     * @param pos the ParsePosition to advance.
     * @return <code>pos</code>
     */
    protected ParsePosition next(ParsePosition pos) {
        pos.setIndex(pos.getIndex() + 1);
        return pos;
    }

    // provide default javadoc >;)
    /**
     * Parse an object from the specified String and ParsePosition. If an error
     * occurs <code>pos.getErrorIndex()</code> will contain a value >= zero,
     * indicating the index at which the parse error occurred.
     * 
     * @param source String to parse
     * @param pos ParsePosition marking index into <code>source</code>
     * @return Object parsed
     */
    public abstract Object parseObject(String source, ParsePosition pos);

    /**
     * Format the specified object, appending to the given StringBuffer, and
     * optionally respecting the specified FieldPosition.
     * 
     * @param obj the object to format
     * @param toAppendTo the StringBuffer to which the formatted object should
     *            be appended
     * @param pos FieldPosition associated with <code>obj</code>
     * @return <code>toAppendTo</code>
     * @throws NullPointerException if <code>toAppendTo</code> or
     *             <code>pos</code> is <code>null</code>
     * @throws IllegalArgumentException if unable to format <code>obj</code>
     */
    public abstract StringBuffer format(Object obj, StringBuffer toAppendTo,
            FieldPosition pos);
}
