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

import java.text.DateFormat;
import java.text.DateFormatSymbols;
import java.text.FieldPosition;
import java.text.Format;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;

/**
 * date/time metaFormat support.
 * 
 * @see ExtendedMessageFormat
 * @author Matt Benson
 * @since 2.4
 * @version $Id$
 */
public abstract class DateMetaFormatSupport extends MetaFormatSupport {
    /** "Default" subformat name */
    protected static final String DEFAULT = "";

    /** "Short" subformat name */
    protected static final String SHORT = "short";

    /** "Medium" subformat name */
    protected static final String MEDIUM = "medium";

    /** "Long" subformat name */
    protected static final String LONG = "long";

    /** "Full" subformat name */
    protected static final String FULL = "full";

    private Locale locale;
    private boolean handlePatterns = true;

    private transient boolean initialized;
    private transient Map styleMap;
    private transient Map inverseStyleMap;
    private transient Map subformats;
    private transient Map reverseSubformats;
    private transient DateFormatSymbols dateFormatSymbols;

    /**
     * Create a new AbstractDateMetaFormat.
     */
    public DateMetaFormatSupport() {
        this(Locale.getDefault());
    }

    /**
     * Create a new AbstractDateMetaFormat.
     * 
     * @param locale Locale
     */
    public DateMetaFormatSupport(Locale locale) {
        super();
        this.locale = locale;
    }

    /**
     * {@inheritDoc}
     */
    public StringBuffer format(Object obj, StringBuffer toAppendTo,
            FieldPosition pos) {
        String subformat = getSubformatName(obj);
        if (subformat != null) {
            return toAppendTo.append(subformat);
        }
        if (isHandlePatterns() && obj instanceof SimpleDateFormat) {
            SimpleDateFormat sdf = (SimpleDateFormat) obj;
            if (sdf.getDateFormatSymbols().equals(dateFormatSymbols)) {
                return toAppendTo.append(sdf.toPattern());
            }
        }
        throw new IllegalArgumentException(String.valueOf(obj));
    }

    /**
     * Get the subformat name for the given object.
     * 
     * @param subformat Object
     * @return subformat name.
     */
    private String getSubformatName(Object subformat) {
        initialize();
        if (reverseSubformats.containsKey(subformat)) {
            return (String) inverseStyleMap.get(reverseSubformats
                    .get(subformat));
        }
        return null;
    }

    /**
     * {@inheritDoc}
     */
    public Object parseObject(String source, ParsePosition pos) {
        int start = pos.getIndex();
        seekFormatElementEnd(source, pos);
        if (pos.getErrorIndex() >= 0) {
            return null;
        }
        String subformat = source.substring(start, pos.getIndex()).trim();
        Object result = getSubformat(subformat);
        if (result != null) {
            return result;
        }
        if (isHandlePatterns()) {
            return new SimpleDateFormat(subformat, getLocale());
        }
        pos.setErrorIndex(start);
        return null;
    }

    /**
     * Get the named subformat.
     * 
     * @param subformat name
     * @return Format designated by <code>name</code>, if any
     */
    private Format getSubformat(String subformat) {
        initialize();
        if (!styleMap.containsKey(subformat)) {
            return null;
        }
        initialize();
        return (Format) subformats.get(styleMap.get(subformat));
    }

    /**
     * Get the locale in use by this DateMetaFormatSupport.
     * 
     * @return Locale
     */
    public Locale getLocale() {
        return locale;
    }

    /**
     * Initialize this DateMetaFormatSupport.
     */
    private synchronized void initialize() {
        if (!initialized) {
            styleMap = createStyleMap();
            inverseStyleMap = createInverseStyleMap();
            subformats = new HashMap();
            reverseSubformats = new HashMap();
            for (Iterator iter = styleMap.values().iterator(); iter.hasNext();) {
                Integer style = (Integer) iter.next();
                if (subformats.containsKey(style)) {
                    continue;
                }
                Format sf = createSubformatInstance(style.intValue());
                subformats.put(style, sf);
                if (inverseStyleMap.containsKey(style)) {
                    reverseSubformats.put(sf, style);
                }
            }
            dateFormatSymbols = new DateFormatSymbols(getLocale());
        }
        initialized = true;
    }

    /**
     * Create a subformat for the given <code>DateFormat</code> style
     * constant.
     * 
     * @param style DateFormat style constant
     * @return a DateFormat instance.
     */
    protected abstract DateFormat createSubformatInstance(int style);

    /**
     * Get whether this metaformat can parse date/time pattern formats in
     * addition to named formats.
     * 
     * @return boolean.
     */
    public boolean isHandlePatterns() {
        return handlePatterns;
    }

    /**
     * Set whether this metaformat can parse date/time pattern formats in
     * addition to named formats.
     * 
     * @param handlePatterns the boolean handlePatterns to set.
     * @return <code>this</code> for fluent usage.
     */
    public DateMetaFormatSupport setHandlePatterns(boolean handlePatterns) {
        this.handlePatterns = handlePatterns;
        return this;
    }

    /**
     * Create the style map.
     * 
     * @return Map
     */
    protected Map createStyleMap() {
        HashMap result = new HashMap();
        result.put(SHORT, new Integer(DateFormat.SHORT));
        result.put(MEDIUM, new Integer(DateFormat.MEDIUM));
        result.put(LONG, new Integer(DateFormat.LONG));
        result.put(FULL, new Integer(DateFormat.FULL));
        result.put(DEFAULT, new Integer(DateFormat.DEFAULT));
        return result;
    }

    /**
     * Create the inverse style map.
     * 
     * @return Map
     */
    protected Map createInverseStyleMap() {
        Map invertMe = createStyleMap();
        invertMe.remove(DEFAULT);
        return invert(invertMe);
    }
}
