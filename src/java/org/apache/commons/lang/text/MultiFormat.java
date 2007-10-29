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
import java.util.ArrayList;

import org.apache.commons.lang.Validate;

/**
 * Format that tries a number of delegates in turn until one is successful.
 * Contrast to {@link CompositeFormat}.
 * 
 * @author Matt Benson
 * @since 2.4
 * @version $Id$
 */
public class MultiFormat extends Format {
    private static final long serialVersionUID = -6128683973856547540L;

    /**
     * Provides a builder with a fluent interface. Example:
     * <p>
     * <code>
     * <pre>
     * MultiFormat mf = new MultiFormat.Builder().add(new FooFormat()).add(
     *         new BarFormat()).add(new BazFormat()).toMultiFormat();
     * </pre></code>
     * </p>
     */
    public static class Builder {
        private ArrayList delegates = new ArrayList();

        /**
         * Add a delegate format.
         * 
         * @param delegate
         * @return the builder
         */
        public Builder add(Format delegate) {
            Validate.notNull(delegate, "delegate format is null");
            delegates.add(delegate);
            return this;
        }

        /**
         * Render the {@link MultiFormat} instance from this Builder.
         * 
         * @return MultiFormat
         */
        public MultiFormat toMultiFormat() {
            return new MultiFormat((Format[]) delegates
                    .toArray(new Format[delegates.size()]));
        }

    }

    private Format[] delegates;

    /**
     * Create a new MultiFormat.
     */
    public MultiFormat() {
    }

    /**
     * Create a new MultiFormat.
     * 
     * @param delegates
     */
    public MultiFormat(Format[] delegates) {
        setDelegates(delegates);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.text.Format#format(java.lang.Object, java.lang.StringBuffer,
     *      java.text.FieldPosition)
     */
    public StringBuffer format(Object obj, StringBuffer toAppendTo,
            FieldPosition pos) {
        Format[] d = getValidDelegates();
        for (int i = 0; i < d.length; i++) {
            try {
                return d[i].format(obj, toAppendTo, pos);
            } catch (IllegalArgumentException e) {
                continue;
            }
        }
        throw new IllegalArgumentException("No delegate Format can parse "
                + obj);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.text.Format#parseObject(java.lang.String,
     *      java.text.ParsePosition)
     */
    public Object parseObject(String source, ParsePosition pos) {
        int start = pos.getIndex();
        Format[] d = getDelegates();
        for (int i = 0; i < d.length; i++) {
            Object o = d[i].parseObject(source, pos);
            if (pos.getErrorIndex() < 0) {
                return o;
            }
            // set up for next attempt:
            pos.setIndex(start);
            pos.setErrorIndex(-1);
        }
        pos.setErrorIndex(start);
        return null;
    }

    /**
     * Set the delegates.
     * 
     * @param delegates
     *            the Format[] delegates to set.
     */
    public void setDelegates(Format[] delegates) {
        Validate.noNullElements(delegates,
                "Null elements present in delegates Format[]");
        this.delegates = delegates;
    }

    /**
     * Get the delegates.
     * 
     * @return Format[].
     */
    public Format[] getDelegates() {
        return delegates;
    }

    private Format[] getValidDelegates() {
        Format[] result = getDelegates();
        Validate.notEmpty(result, "No delegate Formats configured");
        return result;
    }
}
