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

import org.apache.commons.lang.ObjectUtils;

/**
 * Basic metaFormat that requires enough configuration information to
 * parse/format other Formats for use by ExtendedMessageFormat.
 * 
 * @see ExtendedMessageFormat
 * @author Matt Benson
 * @since 2.4
 * @version $Id$
 */
public class NameKeyedMetaFormat extends MetaFormatSupport {
    private static final long serialVersionUID = 5963121202601122213L;

    private static final char TRIGGER_END = '}';
    private static final char TRIGGER_SUBFORMAT = ',';

    /**
     * Provides a builder with a fluent interface. Example:
     * <p>
     * <code>
     * <pre>
     * NameKeyedMetaFormat nkmf = new NameKeyedMetaFormat.Builder().put(&quot;foo&quot;,
     *         new FooFormat()).put(&quot;bar&quot;, new BarFormat())
     *         .put(&quot;baz&quot;, new BazFormat()).toNameKeyedMetaFormat();
     * </pre></code>
     * </p>
     */
    public static class Builder {
        private HashMap keyedFormats = new HashMap();

        /**
         * Add the specified format with the specified string key.
         * 
         * @param key String
         * @param format Format
         * @return Builder reference to this object
         */
        public Builder put(String key, Format format) {
            keyedFormats.put(key, format);
            return this;
        }

        /**
         * Render the {@link NameKeyedMetaFormat} instance from this Builder.
         * 
         * @return NameKeyedMetaFormat
         */
        public NameKeyedMetaFormat toNameKeyedMetaFormat() {
            return new NameKeyedMetaFormat(keyedFormats);
        }
    }

    private Map/* <String, Format> */keyedFormats = new HashMap();

    /**
     * Create a new NameKeyedMetaFormat.
     * 
     * @param keyedFormats String->Format map.
     */
    public NameKeyedMetaFormat(Map keyedFormats) {
        this.keyedFormats = keyedFormats;
    }

    /**
     * {@inheritDoc}
     */
    public StringBuffer format(Object obj, StringBuffer toAppendTo,
            FieldPosition pos) {
        int start = toAppendTo.length();
        // first try to match a sans-subformat format:
        for (Iterator iter = iterateKeys(); iter.hasNext();) {
            Object key = iter.next();
            if (ObjectUtils.equals(keyedFormats.get(key), obj)) {
                return toAppendTo.append(key);
            }
        }
        // now try again with subformats:
        for (Iterator iter = iterateKeys(); iter.hasNext();) {
            Object key = iter.next();
            try {
                ((Format) keyedFormats.get(key)).format(obj, toAppendTo, pos);
                if (toAppendTo.length() > start) {
                    toAppendTo.insert(start, ',');
                }
                return toAppendTo.insert(start, key);
            } catch (Exception e) {
                continue;
            }
        }
        throw new IllegalArgumentException("Cannot format " + obj);
    }

    /**
     * {@inheritDoc}
     */
    public Object parseObject(String source, ParsePosition pos) {
        int start = pos.getIndex();
        boolean subformat = false;
        for (; pos.getIndex() < source.length(); next(pos)) {
            char c = source.charAt(pos.getIndex());
            if (c == TRIGGER_SUBFORMAT) {
                subformat = true;
                break;
            }
            if (c == TRIGGER_END) {
                break;
            }
        }
        String key = source.substring(start, pos.getIndex());
        Format format = (Format) keyedFormats.get(key);
        if (format == null) {
            format = (Format) keyedFormats.get(key.trim());
            if (format == null) {
                pos.setErrorIndex(start);
                return null;
            }
        }
        if (subformat) {
            return format.parseObject(source, next(pos));
        }
        return format;
    }

    /**
     * Extension point to alter the iteration order of the delegate format keys.
     * 
     * @return Iterator.
     */
    protected Iterator iterateKeys() {
        return keyedFormats.keySet().iterator();
    }

}
