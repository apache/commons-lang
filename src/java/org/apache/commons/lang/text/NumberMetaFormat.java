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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.FieldPosition;
import java.text.NumberFormat;
import java.text.ParsePosition;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

/**
 * Stock "number" MetaFormat.
 * 
 * @see ExtendedMessageFormat
 * @author Matt Benson
 * @since 2.4
 * @version $Id$
 */
public class NumberMetaFormat extends MetaFormatSupport {
    private static final long serialVersionUID = -5876397363537288952L;
    private static final String DEFAULT = "";
    private static final String INTEGER = "integer";
    private static final String CURRENCY = "currency";
    private static final String PERCENT = "percent";
    private static final Method GET_INTEGER_INSTANCE;

    static {
        Method m = null;
        try {
            Method mm = NumberFormat.class.getDeclaredMethod("getIntegerInstance", new Class[] { Locale.class });
            if (Modifier.isStatic(mm.getModifiers())) {
                m = mm;
            }
        } catch (Exception e) {
            // leave null
        }
        GET_INTEGER_INSTANCE = m;
    }

    private Locale locale;

    private transient Map subformats;
    private transient Map reverseSubformats;
    private transient DecimalFormatSymbols decimalFormatSymbols;

    /**
     * Create a new NumberMetaFormat.
     */
    public NumberMetaFormat() {
        this(Locale.getDefault());
    }

    /**
     * Create a new NumberMetaFormat.
     * 
     * @param locale Locale
     */
    public NumberMetaFormat(Locale locale) {
        super();
        this.locale = locale;
    }

    /**
     * {@inheritDoc}
     */
    public StringBuffer format(Object obj, StringBuffer toAppendTo,
            FieldPosition pos) {
        initialize();
        String subformat = (String) reverseSubformats.get(obj);
        if (subformat != null) {
            return toAppendTo.append(subformat);
        }
        if (obj instanceof DecimalFormat) {
            DecimalFormat df = (DecimalFormat) obj;
            if (df.getDecimalFormatSymbols().equals(decimalFormatSymbols)) {
                return toAppendTo.append(df.toPattern());
            }
        }
        throw new IllegalArgumentException();
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
        initialize();
        Object result = subformats.get(subformat);
        if (result != null) {
            return result;
        }
        return new DecimalFormat(subformat, decimalFormatSymbols);
    }

    /**
     * Get the locale in use by this <code>NumberMetaFormat</code>.
     * 
     * @return Locale
     */
    public Locale getLocale() {
        return locale;
    }

    /**
     * Initialize this NumberMetaFormat.
     */
    private synchronized void initialize() {
        if (subformats == null) {
            subformats = new HashMap();
            subformats.put(DEFAULT, NumberFormat.getInstance(getLocale()));
            subformats.put(INTEGER, createIntegerInstance(getLocale()));
            subformats.put(CURRENCY, NumberFormat
                    .getCurrencyInstance(getLocale()));
            subformats.put(PERCENT, NumberFormat
                    .getPercentInstance(getLocale()));

            reverseSubformats = invert(subformats);
            decimalFormatSymbols = new DecimalFormatSymbols(getLocale());
        }
    }

    /**
     * Create the "integer" NumberFormat instance for the specified Locale.
     * 
     * @param locale the Locale to use
     * @return integer NumberFormat
     */
    private static NumberFormat createIntegerInstance(Locale locale) {
        if (GET_INTEGER_INSTANCE != null) {
            try {
                 return (NumberFormat) GET_INTEGER_INSTANCE.invoke(null, new Object[] { locale });
            } catch (IllegalAccessException e) {
                //fall through
            } catch (InvocationTargetException e) {
                //fall through
            }
        }
        NumberFormat result = NumberFormat.getInstance(locale);
        result.setMaximumFractionDigits(0);
        result.setParseIntegerOnly(true);
        return result;
    }
}
