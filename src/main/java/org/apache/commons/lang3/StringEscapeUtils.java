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
package org.apache.commons.lang3;

import java.io.IOException;
import java.io.Writer;

import org.apache.commons.lang3.text.translate.AggregateTranslator;
import org.apache.commons.lang3.text.translate.CharSequenceTranslator;
import org.apache.commons.lang3.text.translate.EntityArrays;
import org.apache.commons.lang3.text.translate.JavaUnicodeEscaper;
import org.apache.commons.lang3.text.translate.LookupTranslator;
import org.apache.commons.lang3.text.translate.NumericEntityEscaper;
import org.apache.commons.lang3.text.translate.NumericEntityUnescaper;
import org.apache.commons.lang3.text.translate.OctalUnescaper;
import org.apache.commons.lang3.text.translate.UnicodeUnescaper;
import org.apache.commons.lang3.text.translate.UnicodeUnpairedSurrogateRemover;

/**
 * <p>Escapes and unescapes {@code String}s for
 * Java, Java Script, HTML and XML.</p>
 *
 * <p>#ThreadSafe#</p>
 * @since 2.0
 */
public class StringEscapeUtils {

    /* ESCAPE TRANSLATORS */

    /**
     * Translator object for escaping Java. 
     * 
     * While {@link #escapeJava(String)} is the expected method of use, this 
     * object allows the Java escaping functionality to be used 
     * as the foundation for a custom translator. 
     *
     * @since 3.0
     */
    public static final CharSequenceTranslator ESCAPE_JAVA = 
          new LookupTranslator(
            new String[][] { 
              {"\"", "\\\""},
              {"\\", "\\\\"},
          }).with(
            new LookupTranslator(EntityArrays.JAVA_CTRL_CHARS_ESCAPE())
          ).with(
            JavaUnicodeEscaper.outsideOf(32, 0x7f) 
        );

    /**
     * Translator object for escaping EcmaScript/JavaScript. 
     * 
     * While {@link #escapeEcmaScript(String)} is the expected method of use, this 
     * object allows the EcmaScript escaping functionality to be used 
     * as the foundation for a custom translator. 
     *
     * @since 3.0
     */
    public static final CharSequenceTranslator ESCAPE_ECMASCRIPT = 
        new AggregateTranslator(
            new LookupTranslator(
                      new String[][] { 
                            {"'", "\\'"},
                            {"\"", "\\\""},
                            {"\\", "\\\\"},
                            {"/", "\\/"}
                      }),
            new LookupTranslator(EntityArrays.JAVA_CTRL_CHARS_ESCAPE()),
            JavaUnicodeEscaper.outsideOf(32, 0x7f) 
        );

    /**
     * Translator object for escaping Json.
     *
     * While {@link #escapeJson(String)} is the expected method of use, this
     * object allows the Json escaping functionality to be used
     * as the foundation for a custom translator.
     *
     * @since 3.2
     */
    public static final CharSequenceTranslator ESCAPE_JSON =
        new AggregateTranslator(
            new LookupTranslator(
                      new String[][] {
                            {"\"", "\\\""},
                            {"\\", "\\\\"},
                            {"/", "\\/"}
                      }),
            new LookupTranslator(EntityArrays.JAVA_CTRL_CHARS_ESCAPE()),
            JavaUnicodeEscaper.outsideOf(32, 0x7f)
        );

    /**
     * Translator object for escaping XML.
     * 
     * While {@link #escapeXml(String)} is the expected method of use, this 
     * object allows the XML escaping functionality to be used 
     * as the foundation for a custom translator. 
     *
     * @since 3.0
     * @deprecated use {@link #ESCAPE_XML10} or {@link #ESCAPE_XML11} instead.
     */
    @Deprecated
    public static final CharSequenceTranslator ESCAPE_XML = 
        new AggregateTranslator(
            new LookupTranslator(EntityArrays.BASIC_ESCAPE()),
            new LookupTranslator(EntityArrays.APOS_ESCAPE())
        );
    
    /**
     * Translator object for escaping XML 1.0.
     * 
     * While {@link #escapeXml10(String)} is the expected method of use, this
     * object allows the XML escaping functionality to be used
     * as the foundation for a custom translator.
     *
     * @since 3.3
     */
    public static final CharSequenceTranslator ESCAPE_XML10 =
        new AggregateTranslator(
            new LookupTranslator(EntityArrays.BASIC_ESCAPE()),
            new LookupTranslator(EntityArrays.APOS_ESCAPE()),
            new LookupTranslator(
                    new String[][] {
                            { "\u0000", StringUtils.EMPTY },
                            { "\u0001", StringUtils.EMPTY },
                            { "\u0002", StringUtils.EMPTY },
                            { "\u0003", StringUtils.EMPTY },
                            { "\u0004", StringUtils.EMPTY },
                            { "\u0005", StringUtils.EMPTY },
                            { "\u0006", StringUtils.EMPTY },
                            { "\u0007", StringUtils.EMPTY },
                            { "\u0008", StringUtils.EMPTY },
                            { "\u000b", StringUtils.EMPTY },
                            { "\u000c", StringUtils.EMPTY },
                            { "\u000e", StringUtils.EMPTY },
                            { "\u000f", StringUtils.EMPTY },
                            { "\u0010", StringUtils.EMPTY },
                            { "\u0011", StringUtils.EMPTY },
                            { "\u0012", StringUtils.EMPTY },
                            { "\u0013", StringUtils.EMPTY },
                            { "\u0014", StringUtils.EMPTY },
                            { "\u0015", StringUtils.EMPTY },
                            { "\u0016", StringUtils.EMPTY },
                            { "\u0017", StringUtils.EMPTY },
                            { "\u0018", StringUtils.EMPTY },
                            { "\u0019", StringUtils.EMPTY },
                            { "\u001a", StringUtils.EMPTY },
                            { "\u001b", StringUtils.EMPTY },
                            { "\u001c", StringUtils.EMPTY },
                            { "\u001d", StringUtils.EMPTY },
                            { "\u001e", StringUtils.EMPTY },
                            { "\u001f", StringUtils.EMPTY },
                            { "\ufffe", StringUtils.EMPTY },
                            { "\uffff", StringUtils.EMPTY }
                    }),
            NumericEntityEscaper.between(0x7f, 0x84),
            NumericEntityEscaper.between(0x86, 0x9f),
            new UnicodeUnpairedSurrogateRemover()
        );
    
    /**
     * Translator object for escaping XML 1.1.
     * 
     * While {@link #escapeXml11(String)} is the expected method of use, this
     * object allows the XML escaping functionality to be used
     * as the foundation for a custom translator.
     *
     * @since 3.3
     */
    public static final CharSequenceTranslator ESCAPE_XML11 =
        new AggregateTranslator(
            new LookupTranslator(EntityArrays.BASIC_ESCAPE()),
            new LookupTranslator(EntityArrays.APOS_ESCAPE()),
            new LookupTranslator(
                    new String[][] {
                            { "\u0000", StringUtils.EMPTY },
                            { "\u000b", "&#11;" },
                            { "\u000c", "&#12;" },
                            { "\ufffe", StringUtils.EMPTY },
                            { "\uffff", StringUtils.EMPTY }
                    }),
            NumericEntityEscaper.between(0x1, 0x8),
            NumericEntityEscaper.between(0xe, 0x1f),
            NumericEntityEscaper.between(0x7f, 0x84),
            NumericEntityEscaper.between(0x86, 0x9f),
            new UnicodeUnpairedSurrogateRemover()
        );

    /**
     * Translator object for escaping HTML version 3.0.
     * 
     * While {@link #escapeHtml3(String)} is the expected method of use, this 
     * object allows the HTML escaping functionality to be used 
     * as the foundation for a custom translator. 
     *
     * @since 3.0
     */
    public static final CharSequenceTranslator ESCAPE_HTML3 = 
        new AggregateTranslator(
            new LookupTranslator(EntityArrays.BASIC_ESCAPE()),
            new LookupTranslator(EntityArrays.ISO8859_1_ESCAPE())
        );

    /**
     * Translator object for escaping HTML version 4.0.
     * 
     * While {@link #escapeHtml4(String)} is the expected method of use, this 
     * object allows the HTML escaping functionality to be used 
     * as the foundation for a custom translator. 
     *
     * @since 3.0
     */
    public static final CharSequenceTranslator ESCAPE_HTML4 = 
        new AggregateTranslator(
            new LookupTranslator(EntityArrays.BASIC_ESCAPE()),
            new LookupTranslator(EntityArrays.ISO8859_1_ESCAPE()),
            new LookupTranslator(EntityArrays.HTML40_EXTENDED_ESCAPE())
        );

    /**
     * Translator object for escaping individual Comma Separated Values. 
     * 
     * While {@link #escapeCsv(String)} is the expected method of use, this 
     * object allows the CSV escaping functionality to be used 
     * as the foundation for a custom translator. 
     *
     * @since 3.0
     */
    public static final CharSequenceTranslator ESCAPE_CSV = new CsvEscaper();

    // TODO: Create a parent class - 'SinglePassTranslator' ?
    //       It would handle the index checking + length returning, 
    //       and could also have an optimization check method.
    static class CsvEscaper extends CharSequenceTranslator {

        private static final char CSV_DELIMITER = ',';
        private static final char CSV_QUOTE = '"';
        private static final String CSV_QUOTE_STR = String.valueOf(CSV_QUOTE);
        private static final char[] CSV_SEARCH_CHARS = 
            new char[] {CSV_DELIMITER, CSV_QUOTE, CharUtils.CR, CharUtils.LF};

        @Override
        public int translate(final CharSequence input, final int index, final Writer out) throws IOException {

            if(index != 0) {
                throw new IllegalStateException("CsvEscaper should never reach the [1] index");
            }

            if (StringUtils.containsNone(input.toString(), CSV_SEARCH_CHARS)) {
                out.write(input.toString());
            } else {
                out.write(CSV_QUOTE);
                out.write(StringUtils.replace(input.toString(), CSV_QUOTE_STR, CSV_QUOTE_STR + CSV_QUOTE_STR));
                out.write(CSV_QUOTE);
            }
            return Character.codePointCount(input, 0, input.length());
        }
    }

    /* UNESCAPE TRANSLATORS */

    /**
     * Translator object for unescaping escaped Java. 
     * 
     * While {@link #unescapeJava(String)} is the expected method of use, this 
     * object allows the Java unescaping functionality to be used 
     * as the foundation for a custom translator. 
     *
     * @since 3.0
     */
    // TODO: throw "illegal character: \92" as an Exception if a \ on the end of the Java (as per the compiler)?
    public static final CharSequenceTranslator UNESCAPE_JAVA = 
        new AggregateTranslator(
            new OctalUnescaper(),     // .between('\1', '\377'),
            new UnicodeUnescaper(),
            new LookupTranslator(EntityArrays.JAVA_CTRL_CHARS_UNESCAPE()),
            new LookupTranslator(
                      new String[][] { 
                            {"\\\\", "\\"},
                            {"\\\"", "\""},
                            {"\\'", "'"},
                            {"\\", ""}
                      })
        );

    /**
     * Translator object for unescaping escaped EcmaScript. 
     * 
     * While {@link #unescapeEcmaScript(String)} is the expected method of use, this 
     * object allows the EcmaScript unescaping functionality to be used 
     * as the foundation for a custom translator. 
     *
     * @since 3.0
     */
    public static final CharSequenceTranslator UNESCAPE_ECMASCRIPT = UNESCAPE_JAVA;

    /**
     * Translator object for unescaping escaped Json.
     *
     * While {@link #unescapeJson(String)} is the expected method of use, this
     * object allows the Json unescaping functionality to be used
     * as the foundation for a custom translator.
     *
     * @since 3.2
     */
    public static final CharSequenceTranslator UNESCAPE_JSON = UNESCAPE_JAVA;

    /**
     * Translator object for unescaping escaped HTML 3.0. 
     * 
     * While {@link #unescapeHtml3(String)} is the expected method of use, this 
     * object allows the HTML unescaping functionality to be used 
     * as the foundation for a custom translator. 
     *
     * @since 3.0
     */
    public static final CharSequenceTranslator UNESCAPE_HTML3 = 
        new AggregateTranslator(
            new LookupTranslator(EntityArrays.BASIC_UNESCAPE()),
            new LookupTranslator(EntityArrays.ISO8859_1_UNESCAPE()),
            new NumericEntityUnescaper()
        );

    /**
     * Translator object for unescaping escaped HTML 4.0. 
     * 
     * While {@link #unescapeHtml4(String)} is the expected method of use, this 
     * object allows the HTML unescaping functionality to be used 
     * as the foundation for a custom translator. 
     *
     * @since 3.0
     */
    public static final CharSequenceTranslator UNESCAPE_HTML4 = 
        new AggregateTranslator(
            new LookupTranslator(EntityArrays.BASIC_UNESCAPE()),
            new LookupTranslator(EntityArrays.ISO8859_1_UNESCAPE()),
            new LookupTranslator(EntityArrays.HTML40_EXTENDED_UNESCAPE()),
            new NumericEntityUnescaper()
        );

    /**
     * Translator object for unescaping escaped XML.
     * 
     * While {@link #unescapeXml(String)} is the expected method of use, this 
     * object allows the XML unescaping functionality to be used 
     * as the foundation for a custom translator. 
     *
     * @since 3.0
     */
    public static final CharSequenceTranslator UNESCAPE_XML = 
        new AggregateTranslator(
            new LookupTranslator(EntityArrays.BASIC_UNESCAPE()),
            new LookupTranslator(EntityArrays.APOS_UNESCAPE()),
            new NumericEntityUnescaper()
        );

    /**
     * Translator object for unescaping escaped Comma Separated Value entries.
     * 
     * While {@link #unescapeCsv(String)} is the expected method of use, this 
     * object allows the CSV unescaping functionality to be used 
     * as the foundation for a custom translator. 
     *
     * @since 3.0
     */
    public static final CharSequenceTranslator UNESCAPE_CSV = new CsvUnescaper();

    static class CsvUnescaper extends CharSequenceTranslator {

        private static final char CSV_DELIMITER = ',';
        private static final char CSV_QUOTE = '"';
        private static final String CSV_QUOTE_STR = String.valueOf(CSV_QUOTE);
        private static final char[] CSV_SEARCH_CHARS = 
            new char[] {CSV_DELIMITER, CSV_QUOTE, CharUtils.CR, CharUtils.LF};

        @Override
        public int translate(final CharSequence input, final int index, final Writer out) throws IOException {

            if(index != 0) {
                throw new IllegalStateException("CsvUnescaper should never reach the [1] index");
            }

            if ( input.charAt(0) != CSV_QUOTE || input.charAt(input.length() - 1) != CSV_QUOTE ) {
                out.write(input.toString());
                return Character.codePointCount(input, 0, input.length());
            }

            // strip quotes
            final String quoteless = input.subSequence(1, input.length() - 1).toString();

            if ( StringUtils.containsAny(quoteless, CSV_SEARCH_CHARS) ) {
                // deal with escaped quotes; ie) ""
                out.write(StringUtils.replace(quoteless, CSV_QUOTE_STR + CSV_QUOTE_STR, CSV_QUOTE_STR));
            } else {
                out.write(input.toString());
            }
            return Character.codePointCount(input, 0, input.length());
        }
    }

    /* Helper functions */

    /**
     * <p>{@code StringEscapeUtils} instances should NOT be constructed in
     * standard programming.</p>
     *
     * <p>Instead, the class should be used as:</p>
     * <pre>StringEscapeUtils.escapeJava("foo");</pre>
     *
     * <p>This constructor is public to permit tools that require a JavaBean
     * instance to operate.</p>
     */
    public StringEscapeUtils() {
      super();
    }

    // Java and JavaScript
    //--------------------------------------------------------------------------
    /**
     * <p>Escapes the characters in a {@code String} using Java String rules.</p>
     *
     * <p>Deals correctly with quotes and control-chars (tab, backslash, cr, ff, etc.) </p>
     *
     * <p>So a tab becomes the characters {@code '\\'} and
     * {@code 't'}.</p>
     *
     * <p>The only difference between Java strings and JavaScript strings
     * is that in JavaScript, a single quote and forward-slash (/) are escaped.</p>
     *
     * <p>Example:</p>
     * <pre>
     * input string: He didn't say, "Stop!"
     * output string: He didn't say, \"Stop!\"
     * </pre>
     *
     * @param input  String to escape values in, may be null
     * @return String with escaped values, {@code null} if null string input
     */
    public static final String escapeJava(final String input) {
        return ESCAPE_JAVA.translate(input);
    }

    /**
     * <p>Escapes the characters in a {@code String} using EcmaScript String rules.</p>
     * <p>Escapes any values it finds into their EcmaScript String form.
     * Deals correctly with quotes and control-chars (tab, backslash, cr, ff, etc.) </p>
     *
     * <p>So a tab becomes the characters {@code '\\'} and
     * {@code 't'}.</p>
     *
     * <p>The only difference between Java strings and EcmaScript strings
     * is that in EcmaScript, a single quote and forward-slash (/) are escaped.</p>
     *
     * <p>Note that EcmaScript is best known by the JavaScript and ActionScript dialects. </p>
     *
     * <p>Example:</p>
     * <pre>
     * input string: He didn't say, "Stop!"
     * output string: He didn\'t say, \"Stop!\"
     * </pre>
     *
     * @param input  String to escape values in, may be null
     * @return String with escaped values, {@code null} if null string input
     *
     * @since 3.0
     */
    public static final String escapeEcmaScript(final String input) {
        return ESCAPE_ECMASCRIPT.translate(input);
    }

    /**
     * <p>Escapes the characters in a {@code String} using Json String rules.</p>
     * <p>Escapes any values it finds into their Json String form.
     * Deals correctly with quotes and control-chars (tab, backslash, cr, ff, etc.) </p>
     *
     * <p>So a tab becomes the characters {@code '\\'} and
     * {@code 't'}.</p>
     *
     * <p>The only difference between Java strings and Json strings
     * is that in Json, forward-slash (/) is escaped.</p>
     *
     * <p>See http://www.ietf.org/rfc/rfc4627.txt for further details. </p>
     *
     * <p>Example:</p>
     * <pre>
     * input string: He didn't say, "Stop!"
     * output string: He didn't say, \"Stop!\"
     * </pre>
     *
     * @param input  String to escape values in, may be null
     * @return String with escaped values, {@code null} if null string input
     *
     * @since 3.2
     */
    public static final String escapeJson(final String input) {
        return ESCAPE_JSON.translate(input);
    }

    /**
     * <p>Unescapes any Java literals found in the {@code String}.
     * For example, it will turn a sequence of {@code '\'} and
     * {@code 'n'} into a newline character, unless the {@code '\'}
     * is preceded by another {@code '\'}.</p>
     * 
     * @param input  the {@code String} to unescape, may be null
     * @return a new unescaped {@code String}, {@code null} if null string input
     */
    public static final String unescapeJava(final String input) {
        return UNESCAPE_JAVA.translate(input);
    }

    /**
     * <p>Unescapes any EcmaScript literals found in the {@code String}.</p>
     *
     * <p>For example, it will turn a sequence of {@code '\'} and {@code 'n'}
     * into a newline character, unless the {@code '\'} is preceded by another
     * {@code '\'}.</p>
     *
     * @see #unescapeJava(String)
     * @param input  the {@code String} to unescape, may be null
     * @return A new unescaped {@code String}, {@code null} if null string input
     *
     * @since 3.0
     */
    public static final String unescapeEcmaScript(final String input) {
        return UNESCAPE_ECMASCRIPT.translate(input);
    }

    /**
     * <p>Unescapes any Json literals found in the {@code String}.</p>
     *
     * <p>For example, it will turn a sequence of {@code '\'} and {@code 'n'}
     * into a newline character, unless the {@code '\'} is preceded by another
     * {@code '\'}.</p>
     *
     * @see #unescapeJava(String)
     * @param input  the {@code String} to unescape, may be null
     * @return A new unescaped {@code String}, {@code null} if null string input
     *
     * @since 3.2
     */
    public static final String unescapeJson(final String input) {
        return UNESCAPE_JSON.translate(input);
    }

    // HTML and XML
    //--------------------------------------------------------------------------
    /**
     * <p>Escapes the characters in a {@code String} using HTML entities.</p>
     *
     * <p>
     * For example:
     * </p> 
     * <p><code>"bread" &amp; "butter"</code></p>
     * becomes:
     * <p>
     * <code>&amp;quot;bread&amp;quot; &amp;amp; &amp;quot;butter&amp;quot;</code>.
     * </p>
     *
     * <p>Supports all known HTML 4.0 entities, including funky accents.
     * Note that the commonly used apostrophe escape character (&amp;apos;)
     * is not a legal entity and so is not supported). </p>
     *
     * @param input  the {@code String} to escape, may be null
     * @return a new escaped {@code String}, {@code null} if null string input
     * 
     * @see <a href="http://hotwired.lycos.com/webmonkey/reference/special_characters/">ISO Entities</a>
     * @see <a href="http://www.w3.org/TR/REC-html32#latin1">HTML 3.2 Character Entities for ISO Latin-1</a>
     * @see <a href="http://www.w3.org/TR/REC-html40/sgml/entities.html">HTML 4.0 Character entity references</a>
     * @see <a href="http://www.w3.org/TR/html401/charset.html#h-5.3">HTML 4.01 Character References</a>
     * @see <a href="http://www.w3.org/TR/html401/charset.html#code-position">HTML 4.01 Code positions</a>
     * 
     * @since 3.0
     */
    public static final String escapeHtml4(final String input) {
        return ESCAPE_HTML4.translate(input);
    }

    /**
     * <p>Escapes the characters in a {@code String} using HTML entities.</p>
     * <p>Supports only the HTML 3.0 entities. </p>
     *
     * @param input  the {@code String} to escape, may be null
     * @return a new escaped {@code String}, {@code null} if null string input
     * 
     * @since 3.0
     */
    public static final String escapeHtml3(final String input) {
        return ESCAPE_HTML3.translate(input);
    }

    //-----------------------------------------------------------------------
    /**
     * <p>Unescapes a string containing entity escapes to a string
     * containing the actual Unicode characters corresponding to the
     * escapes. Supports HTML 4.0 entities.</p>
     *
     * <p>For example, the string {@code "&lt;Fran&ccedil;ais&gt;"}
     * will become {@code "<Français>"}</p>
     *
     * <p>If an entity is unrecognized, it is left alone, and inserted
     * verbatim into the result string. e.g. {@code "&gt;&zzzz;x"} will
     * become {@code ">&zzzz;x"}.</p>
     *
     * @param input  the {@code String} to unescape, may be null
     * @return a new unescaped {@code String}, {@code null} if null string input
     * 
     * @since 3.0
     */
    public static final String unescapeHtml4(final String input) {
        return UNESCAPE_HTML4.translate(input);
    }

    /**
     * <p>Unescapes a string containing entity escapes to a string
     * containing the actual Unicode characters corresponding to the
     * escapes. Supports only HTML 3.0 entities.</p>
     *
     * @param input  the {@code String} to unescape, may be null
     * @return a new unescaped {@code String}, {@code null} if null string input
     * 
     * @since 3.0
     */
    public static final String unescapeHtml3(final String input) {
        return UNESCAPE_HTML3.translate(input);
    }

    //-----------------------------------------------------------------------
    /**
     * <p>Escapes the characters in a {@code String} using XML entities.</p>
     *
     * <p>For example: {@code "bread" & "butter"} =&gt;
     * {@code &quot;bread&quot; &amp; &quot;butter&quot;}.
     * </p>
     *
     * <p>Supports only the five basic XML entities (gt, lt, quot, amp, apos).
     * Does not support DTDs or external entities.</p>
     *
     * <p>Note that Unicode characters greater than 0x7f are as of 3.0, no longer 
     *    escaped. If you still wish this functionality, you can achieve it 
     *    via the following: 
     * {@code StringEscapeUtils.ESCAPE_XML.with( NumericEntityEscaper.between(0x7f, Integer.MAX_VALUE) );}</p>
     *
     * @param input  the {@code String} to escape, may be null
     * @return a new escaped {@code String}, {@code null} if null string input
     * @see #unescapeXml(java.lang.String)
     * @deprecated use {@link #escapeXml10(java.lang.String)} or {@link #escapeXml11(java.lang.String)} instead.
     */
    @Deprecated
    public static final String escapeXml(final String input) {
        return ESCAPE_XML.translate(input);
    }

    /**
     * <p>Escapes the characters in a {@code String} using XML entities.</p>
     *
     * <p>For example: {@code "bread" & "butter"} =&gt;
     * {@code &quot;bread&quot; &amp; &quot;butter&quot;}.
     * </p>
     *
     * <p>Note that XML 1.0 is a text-only format: it cannot represent control
     * characters or unpaired Unicode surrogate codepoints, even after escaping.
     * {@code escapeXml10} will remove characters that do not fit in the
     * following ranges:</p>
     * 
     * <p>{@code #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]}</p>
     * 
     * <p>Though not strictly necessary, {@code escapeXml10} will escape
     * characters in the following ranges:</p>
     * 
     * <p>{@code [#x7F-#x84] | [#x86-#x9F]}</p>
     * 
     * <p>The returned string can be inserted into a valid XML 1.0 or XML 1.1
     * document. If you want to allow more non-text characters in an XML 1.1
     * document, use {@link #escapeXml11(String)}.</p>
     *
     * @param input  the {@code String} to escape, may be null
     * @return a new escaped {@code String}, {@code null} if null string input
     * @see #unescapeXml(java.lang.String)
     * @since 3.3
     */
    public static String escapeXml10(final String input) {
        return ESCAPE_XML10.translate(input);
    }
    
    /**
     * <p>Escapes the characters in a {@code String} using XML entities.</p>
     *
     * <p>For example: {@code "bread" & "butter"} =&gt;
     * {@code &quot;bread&quot; &amp; &quot;butter&quot;}.
     * </p>
     *
     * <p>XML 1.1 can represent certain control characters, but it cannot represent
     * the null byte or unpaired Unicode surrogate codepoints, even after escaping.
     * {@code escapeXml11} will remove characters that do not fit in the following
     * ranges:</p>
     * 
     * <p>{@code [#x1-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]}</p>
     * 
     * <p>{@code escapeXml11} will escape characters in the following ranges:</p>
     * 
     * <p>{@code [#x1-#x8] | [#xB-#xC] | [#xE-#x1F] | [#x7F-#x84] | [#x86-#x9F]}</p>
     * 
     * <p>The returned string can be inserted into a valid XML 1.1 document. Do not
     * use it for XML 1.0 documents.</p>
     *
     * @param input  the {@code String} to escape, may be null
     * @return a new escaped {@code String}, {@code null} if null string input
     * @see #unescapeXml(java.lang.String)
     * @since 3.3
     */
    public static String escapeXml11(final String input) {
        return ESCAPE_XML11.translate(input);
    }

    //-----------------------------------------------------------------------
    /**
     * <p>Unescapes a string containing XML entity escapes to a string
     * containing the actual Unicode characters corresponding to the
     * escapes.</p>
     *
     * <p>Supports only the five basic XML entities (gt, lt, quot, amp, apos).
     * Does not support DTDs or external entities.</p>
     *
     * <p>Note that numerical \\u Unicode codes are unescaped to their respective 
     *    Unicode characters. This may change in future releases. </p>
     *
     * @param input  the {@code String} to unescape, may be null
     * @return a new unescaped {@code String}, {@code null} if null string input
     * @see #escapeXml(String)
     * @see #escapeXml10(String)
     * @see #escapeXml11(String)
     */
    public static final String unescapeXml(final String input) {
        return UNESCAPE_XML.translate(input);
    }

    //-----------------------------------------------------------------------

    /**
     * <p>Returns a {@code String} value for a CSV column enclosed in double quotes,
     * if required.</p>
     *
     * <p>If the value contains a comma, newline or double quote, then the
     *    String value is returned enclosed in double quotes.</p>
     *
     * <p>Any double quote characters in the value are escaped with another double quote.</p>
     *
     * <p>If the value does not contain a comma, newline or double quote, then the
     *    String value is returned unchanged.</p>
     *
     * see <a href="http://en.wikipedia.org/wiki/Comma-separated_values">Wikipedia</a> and
     * <a href="http://tools.ietf.org/html/rfc4180">RFC 4180</a>.
     *
     * @param input the input CSV column String, may be null
     * @return the input String, enclosed in double quotes if the value contains a comma,
     * newline or double quote, {@code null} if null string input
     * @since 2.4
     */
    public static final String escapeCsv(final String input) {
        return ESCAPE_CSV.translate(input);
    }

    /**
     * <p>Returns a {@code String} value for an unescaped CSV column. </p>
     *
     * <p>If the value is enclosed in double quotes, and contains a comma, newline 
     *    or double quote, then quotes are removed. 
     * </p>
     *
     * <p>Any double quote escaped characters (a pair of double quotes) are unescaped 
     *    to just one double quote. </p>
     *
     * <p>If the value is not enclosed in double quotes, or is and does not contain a 
     *    comma, newline or double quote, then the String value is returned unchanged.</p>
     *
     * see <a href="http://en.wikipedia.org/wiki/Comma-separated_values">Wikipedia</a> and
     * <a href="http://tools.ietf.org/html/rfc4180">RFC 4180</a>.
     *
     * @param input the input CSV column String, may be null
     * @return the input String, with enclosing double quotes removed and embedded double 
     * quotes unescaped, {@code null} if null string input
     * @since 2.4
     */
    public static final String unescapeCsv(final String input) {
        return UNESCAPE_CSV.translate(input);
    }

}
