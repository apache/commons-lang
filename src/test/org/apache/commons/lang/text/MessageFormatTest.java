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

import java.text.MessageFormat;
import java.util.Locale;

/**
 * Baseline tests for java.text.MessageFormat.
 * 
 * @author Matt Benson
 * @since 2.4
 * @version $Id$
 */
public abstract class MessageFormatTest extends AbstractMessageFormatTest {

    /**
     * Tests for <code>Locale.US</code>
     * 
     * @author mbenson
     */
    public static class US extends MessageFormatTest {
        /**
         * {@inheritDoc}
         */
        protected Locale getLocale() {
            return Locale.US;
        }
    }

    /**
     * Tests for <code>Locale.UK</code>
     * 
     * @author mbenson
     */
    public static class UK extends MessageFormatTest {
        /**
         * {@inheritDoc}
         */
        protected Locale getLocale() {
            return Locale.UK;
        }
    }

    /**
     * Tests for <code>Locale.GERMANY</code>
     * 
     * @author mbenson
     */
    public static class DE extends MessageFormatTest {
        /**
         * {@inheritDoc}
         */
        protected Locale getLocale() {
            return Locale.GERMANY;
        }
    }

    /**
     * Tests for <code>Locale.ITALY</code>
     * 
     * @author mbenson
     */
    public static class IT extends MessageFormatTest {
        /**
         * {@inheritDoc}
         */
        protected Locale getLocale() {
            return Locale.ITALY;
        }
    }

    /**
     * Tests for <code>Locale.JAPAN</code>
     * 
     * @author mbenson
     */
    public static class JP extends MessageFormatTest {
        /**
         * {@inheritDoc}
         */
        protected Locale getLocale() {
            return Locale.JAPAN;
        }
    }

    /**
     * Tests for <code>Locale.CHINA</code>
     * 
     * @author mbenson
     */
    public static class CN extends MessageFormatTest {
        /**
         * {@inheritDoc}
         */
        protected Locale getLocale() {
            return Locale.CHINA;
        }
    }

    /**
     * Tests for <code>Locale.CANADA</code>
     * 
     * @author mbenson
     */
    public static class CA extends MessageFormatTest {
        /**
         * {@inheritDoc}
         */
        protected Locale getLocale() {
            return Locale.CANADA;
        }
    }

    /**
     * Tests for <code>Locale.FRANCE</code>
     * 
     * @author mbenson
     */
    public static class FR extends MessageFormatTest {
        /**
         * {@inheritDoc}
         */
        protected Locale getLocale() {
            return Locale.FRANCE;
        }
    }

    /**
     * Tests for <code>Locale.KOREA</code>
     * 
     * @author mbenson
     */
    public static class KR extends MessageFormatTest {
        /**
         * {@inheritDoc}
         */
        protected Locale getLocale() {
            return Locale.KOREA;
        }
    }

    /**
     * Tests for <code>Locale.TAIWAN</code>
     * 
     * @author mbenson
     */
    public static class TW extends MessageFormatTest {
        /**
         * {@inheritDoc}
         */
        protected Locale getLocale() {
            return Locale.TAIWAN;
        }
    }

    /**
     * {@inheritDoc}
     */
    protected MessageFormat createMessageFormat(String pattern, Locale locale) {
        MessageFormat result = new MessageFormat(pattern);
        if (!Locale.getDefault().equals(locale)) {
            result.setLocale(locale);
            result.applyPattern(pattern);
        }
        return result;
    }
}
