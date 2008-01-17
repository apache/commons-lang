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
import java.text.MessageFormat;
import java.text.ParsePosition;
import java.util.ArrayList;
import java.util.Locale;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.Validate;

/**
 * Extends <code>MessageFormat</code> to allow pluggable/additional formatting
 * options for embedded format elements; requires a "meta-format", that is a
 * <code>Format</code> capable of parsing and formatting other
 * <code>Format</code>s.
 * 
 * Limitations:
 * <ul>
 * <li><code>toPattern()</code> results are tailored to JDK 1.4+ output and
 * will produce fairly drastically different results on earlier JDKs.</li>
 * <li>Recursive choice formats do not inherit knowledge of the extended
 * formatters and are limited to those available with
 * <code>java.text.MessageFormat</code>.</li>
 * </ul>
 * 
 * @author Matt Benson
 * @since 2.4
 * @version $Id$
 */
public class ExtendedMessageFormat extends MessageFormat {
    private static final long serialVersionUID = -2362048321261811743L;

    /**
     * Get a default meta-format for the default Locale. This will produce
     * behavior identical to a <code>java.lang.MessageFormat</code> using the
     * default locale.
     * 
     * @return Format
     */
    public static Format createDefaultMetaFormat() {
        return createDefaultMetaFormat(Locale.getDefault());
    }

    /**
     * Get a default meta-format for the specified Locale. This will produce
     * behavior identical to a <code>java.lang.MessageFormat</code> using
     * <code>locale</code>.
     * 
     * @param locale the Locale for the resulting Format instance.
     * @return Format
     */
    public static Format createDefaultMetaFormat(Locale locale) {
        return DefaultMetaFormatFactory.getFormat(locale);
    }

    /**
     * Conceptual demarcation of methods to parse the pattern.
     */
    private static class Parser {
        private static final String ESCAPED_QUOTE = "''";
        private static final char START_FMT = ',';
        private static final char END_FE = '}';
        private static final char START_FE = '{';
        private static final char QUOTE = '\'';

        /**
         * Strip all formats from the pattern.
         * 
         * @param pattern String to strip
         * @return stripped pattern
         */
        private String stripFormats(String pattern) {
            StringBuffer sb = new StringBuffer(pattern.length());
            ParsePosition pos = new ParsePosition(0);
            char[] c = pattern.toCharArray();
            while (pos.getIndex() < pattern.length()) {
                switch (c[pos.getIndex()]) {
                case QUOTE:
                    appendQuotedString(pattern, pos, sb, true);
                    break;
                case START_FE:
                    int start = pos.getIndex();
                    readArgumentIndex(pattern, next(pos));
                    sb.append(c, start, pos.getIndex() - start);
                    if (c[pos.getIndex()] == START_FMT) {
                        eatFormat(pattern, next(pos));
                    }
                    if (c[pos.getIndex()] != END_FE) {
                        throw new IllegalArgumentException(
                                "Unreadable format element at position "
                                        + start);
                    }
                    // fall through
                default:
                    sb.append(c[pos.getIndex()]);
                    next(pos);
                }
            }
            return sb.toString();
        }

        /**
         * Insert formats back into the pattern for toPattern() support.
         * 
         * @param pattern source
         * @param formats the Formats to insert
         * @param metaFormat Format to format the Formats
         * @return full pattern
         */
        private String insertFormats(String pattern, Format[] formats,
                Format metaFormat) {
            if (formats == null || formats.length == 0) {
                return pattern;
            }
            StringBuffer sb = new StringBuffer(pattern.length() * 2);
            ParsePosition pos = new ParsePosition(0);
            int fe = -1;
            while (pos.getIndex() < pattern.length()) {
                char c = pattern.charAt(pos.getIndex());
                switch (c) {
                case QUOTE:
                    appendQuotedString(pattern, pos, sb, false);
                    break;
                case START_FE:
                    fe++;
                    sb.append(START_FE).append(
                            readArgumentIndex(pattern, next(pos)));
                    if (formats[fe] != null) {
                        String formatName = metaFormat.format(formats[fe]);
                        if (StringUtils.isNotEmpty(formatName)) {
                            sb.append(START_FMT).append(formatName);
                        }
                    }
                    break;
                default:
                    sb.append(pattern.charAt(pos.getIndex()));
                    next(pos);
                }
            }
            return sb.toString();
        }

        /**
         * Parse the formats from the given pattern.
         * 
         * @param pattern String to parse
         * @param metaFormat Format to parse the Formats
         * @return array of parsed Formats
         */
        private Format[] parseFormats(String pattern, Format metaFormat) {
            ArrayList result = new ArrayList();
            ParsePosition pos = new ParsePosition(0);
            while (pos.getIndex() < pattern.length()) {
                switch (pattern.charAt(pos.getIndex())) {
                case QUOTE:
                    getQuotedString(pattern, next(pos), true);
                    break;
                case START_FE:
                    int start = pos.getIndex();
                    readArgumentIndex(pattern, next(pos));
                    if (pattern.charAt(pos.getIndex()) == START_FMT) {
                        seekNonWs(pattern, next(pos));
                    }
                    result.add(metaFormat.parseObject(pattern, pos));
                    seekNonWs(pattern, pos);
                    if (pattern.charAt(pos.getIndex()) != END_FE) {
                        throw new IllegalArgumentException(
                                "Unreadable format element at position "
                                        + start);
                    }
                    // fall through
                default:
                    next(pos);
                }
            }
            return (Format[]) result.toArray(new Format[result.size()]);
        }

        /**
         * Consume whitespace from the current parse position.
         * 
         * @param pattern String to read
         * @param pos current position
         */
        private void seekNonWs(String pattern, ParsePosition pos) {
            int len = 0;
            char[] buffer = pattern.toCharArray();
            do {
                len = StrMatcher.splitMatcher().isMatch(buffer, pos.getIndex());
                pos.setIndex(pos.getIndex() + len);
            } while (len > 0 && pos.getIndex() < pattern.length());
        }

        /**
         * Convenience method to advance parse position by 1
         * 
         * @param pos ParsePosition
         * @return <code>pos</code>
         */
        private ParsePosition next(ParsePosition pos) {
            pos.setIndex(pos.getIndex() + 1);
            return pos;
        }

        /**
         * Read the argument index from the current format element
         * 
         * @param pattern pattern to parse
         * @param pos current parse position
         * @return argument index as string
         */
        private String readArgumentIndex(String pattern, ParsePosition pos) {
            int start = pos.getIndex();
            for (; pos.getIndex() < pattern.length(); next(pos)) {
                char c = pattern.charAt(pos.getIndex());
                if (c == START_FMT || c == END_FE) {
                    return pattern.substring(start, pos.getIndex());
                }
                if (!Character.isDigit(c)) {
                    throw new IllegalArgumentException(
                            "Invalid format argument index at position "
                                    + start);
                }
            }
            throw new IllegalArgumentException(
                    "Unterminated format element at position " + start);
        }

        /**
         * Consume a quoted string, adding it to <code>appendTo</code> if
         * specified.
         * 
         * @param pattern pattern to parse
         * @param pos current parse position
         * @param appendTo optional StringBuffer to append
         * @param escapingOn whether to process escaped quotes
         * @return <code>appendTo</code>
         */
        private StringBuffer appendQuotedString(String pattern,
                ParsePosition pos, StringBuffer appendTo, boolean escapingOn) {
            int start = pos.getIndex();
            char[] c = pattern.toCharArray();
            if (escapingOn && c[start] == QUOTE) {
                return appendTo == null ? null : appendTo.append(QUOTE);
            }
            int lastHold = start;
            for (int i = pos.getIndex(); i < pattern.length(); i++) {
                if (escapingOn
                        && pattern.substring(i).startsWith(ESCAPED_QUOTE)) {
                    appendTo.append(c, lastHold, pos.getIndex() - lastHold)
                            .append(QUOTE);
                    pos.setIndex(i + ESCAPED_QUOTE.length());
                    lastHold = pos.getIndex();
                    continue;
                }
                switch (c[pos.getIndex()]) {
                case QUOTE:
                    next(pos);
                    return appendTo == null ? null : appendTo.append(c,
                            lastHold, pos.getIndex() - lastHold);
                default:
                    next(pos);
                }
            }
            throw new IllegalArgumentException(
                    "Unterminated quoted string at position " + start);
        }

        /**
         * Consume quoted string only
         * 
         * @param pattern pattern to parse
         * @param pos current parse position
         * @param escapingOn whether to process escaped quotes
         */
        private void getQuotedString(String pattern, ParsePosition pos,
                boolean escapingOn) {
            appendQuotedString(pattern, pos, null, escapingOn);
        }

        /**
         * Consume the entire format found at the current position.
         * 
         * @param pattern string to parse
         * @param pos current parse position
         */
        private void eatFormat(String pattern, ParsePosition pos) {
            int start = pos.getIndex();
            int depth = 1;
            for (; pos.getIndex() < pattern.length(); next(pos)) {
                switch (pattern.charAt(pos.getIndex())) {
                case START_FE:
                    depth++;
                    break;
                case END_FE:
                    depth--;
                    if (depth == 0) {
                        return;
                    }
                    break;
                case QUOTE:
                    getQuotedString(pattern, pos, false);
                    break;
                }
            }
            throw new IllegalArgumentException(
                    "Unterminated format element at position " + start);
        }
    }

    private static final Parser PARSER = new Parser();

    private Format metaFormat;
    private String strippedPattern;

    /**
     * Create a new ExtendedMessageFormat for the default locale.
     * 
     * @param pattern String
     * @param metaFormat Format
     * @throws IllegalArgumentException if <code>metaFormat</code> is
     *             <code>null</code> or in case of a bad pattern.
     */
    public ExtendedMessageFormat(String pattern, Format metaFormat) {
        this(pattern, Locale.getDefault(), metaFormat);
    }

    /**
     * Create a new ExtendedMessageFormat.
     * 
     * @param pattern String
     * @param locale Locale
     * @param metaFormat Format
     * @throws IllegalArgumentException if <code>metaFormat</code> is
     *             <code>null</code> or in case of a bad pattern.
     */
    public ExtendedMessageFormat(String pattern, Locale locale,
            Format metaFormat) {
        /*
         * We have to do some acrobatics here: the call to the super constructor
         * will invoke applyPattern(), but we don't want to apply the pattern
         * until we've installed our custom metaformat. So we check for that in
         * our (final) applyPattern implementation, and re-call at the end of
         * this constructor.
         */
        super(pattern);
        setLocale(locale);
        setMetaFormat(metaFormat);
        applyPattern(pattern);
    }

    /**
     * Apply the specified pattern.
     * 
     * @param pattern String
     */
    public final void applyPattern(String pattern) {
        if (metaFormat == null) {
            return;
        }
        applyPatternPre(pattern);
        strippedPattern = PARSER.stripFormats(pattern);
        super.applyPattern(strippedPattern);
        setFormats(PARSER.parseFormats(pattern, metaFormat));
        applyPatternPost(pattern);
    }

    /**
     * Pre-execution hook by means of which a subclass can customize the
     * behavior of the final applyPattern implementation.
     * 
     * @param pattern String
     */
    protected void applyPatternPre(String pattern) {
        // noop
    }

    /**
     * Post-execution hook by means of which a subclass can customize the
     * behavior of the final applyPattern implementation.
     * 
     * @param pattern String
     */
    protected void applyPatternPost(String pattern) {
        // noop
    }

    /**
     * Render the pattern from the current state of the
     * <code>ExtendedMessageFormat</code>.
     * 
     * @return pattern String
     */
    public String toPattern() {
        return PARSER.insertFormats(strippedPattern, getFormats(), metaFormat);
    }

    /**
     * Get the meta-format currently configured.
     * 
     * @return Format.
     */
    public synchronized Format getMetaFormat() {
        return metaFormat;
    }

    /**
     * Set the meta-format. Has no effect until a subsequent call to
     * {@link #applyPattern(String)}.
     * 
     * @param metaFormat the Format metaFormat to set.
     */
    public synchronized void setMetaFormat(Format metaFormat) {
        Validate.notNull(metaFormat, "metaFormat is null");
        this.metaFormat = metaFormat;
    }

}
