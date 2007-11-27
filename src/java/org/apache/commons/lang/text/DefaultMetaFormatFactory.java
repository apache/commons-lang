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

import java.text.Format;
import java.text.ParsePosition;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.Validate;

/**
 * Factory methods to produce metaformat instances that behave like
 * java.text.MessageFormat.
 * 
 * @author Matt Benson
 * @since 2.4
 * @version $Id: DefaultMetaFormatFactory.java 592077 2007-11-05 16:47:10Z
 *          mbenson $
 */
class DefaultMetaFormatFactory {

    /** Number key */
    public static final String NUMBER_KEY = "number";

    /** Date key */
    public static final String DATE_KEY = "date";

    /** Time key */
    public static final String TIME_KEY = "time";

    /** Choice key */
    public static final String CHOICE_KEY = "choice";

    private static final String[] NO_SUBFORMAT_KEYS = new String[] {
            NUMBER_KEY, DATE_KEY, TIME_KEY };

    private static final String[] NO_PATTERN_KEYS = new String[] { NUMBER_KEY,
            DATE_KEY, TIME_KEY, CHOICE_KEY };

    private static final String[] PATTERN_KEYS = new String[] { DATE_KEY,
            TIME_KEY };

    /**
     * Ordered NameKeyedMetaFormat
     */
    private static class OrderedNameKeyedMetaFormat extends NameKeyedMetaFormat {
        private static final long serialVersionUID = -7688772075239431055L;

        private List keys;

        /**
         * Construct a new OrderedNameKeyedMetaFormat.
         * 
         * @param names String[]
         * @param formats Format[]
         */
        private OrderedNameKeyedMetaFormat(String[] names, Format[] formats) {
            super(createMap(names, formats));
            this.keys = Arrays.asList(names);
        }

        /**
         * Create a map from the specified key/value parameters.
         * 
         * @param names keys
         * @param formats values
         * @return Map
         */
        private static Map createMap(String[] names, Format[] formats) {
            Validate.isTrue(ArrayUtils.isSameLength(names, formats));
            HashMap result = new HashMap(names.length);
            for (int i = 0; i < names.length; i++) {
                result.put(names[i], formats[i]);
            }
            return result;
        }

        /**
         * {@inheritDoc}
         */
        protected Iterator iterateKeys() {
            return keys.iterator();
        }
    }

    /**
     * Get a default metaformat for the specified Locale.
     * 
     * @param locale the Locale for the resulting Format instance.
     * @return Format
     */
    public static Format getFormat(final Locale locale) {
        Format nmf = new NumberMetaFormat(locale);
        Format dmf = new DateMetaFormat(locale).setHandlePatterns(false);
        Format tmf = new TimeMetaFormat(locale).setHandlePatterns(false);

        return new MultiFormat(new Format[] {
                new OrderedNameKeyedMetaFormat(NO_SUBFORMAT_KEYS, new Format[] {
                        getDefaultFormat(nmf), getDefaultFormat(dmf),
                        getDefaultFormat(tmf) }),
                new OrderedNameKeyedMetaFormat(NO_PATTERN_KEYS, new Format[] {
                        nmf, dmf, tmf, ChoiceMetaFormat.INSTANCE }),
                new OrderedNameKeyedMetaFormat(PATTERN_KEYS,
                        new Format[] { new DateMetaFormat(locale),
                                new TimeMetaFormat(locale) }) });
    }

    /**
     * Get the default format supported by a given metaformat.
     * 
     * @param metaformat Format to handle parsing.
     * @return the default format, if any.
     */
    private static Format getDefaultFormat(Format metaformat) {
        ParsePosition pos = new ParsePosition(0);
        Object o = metaformat.parseObject("", pos);
        return pos.getErrorIndex() < 0 ? (Format) o : null;
    }
}
