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
