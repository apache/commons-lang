/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang3;

import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Helpers to process Strings using regular expressions.
 *
 * @see java.util.regex.Pattern
 * @since 3.8
 */
public class RegExUtils {

    /**
     * The pattern to split version strings.
     */
    static final Pattern VERSION_SPLIT_PATTERN = Pattern.compile("\\.");

    /**
     * Compiles the given regular expression into a pattern with the {@link Pattern#DOTALL} flag.
     *
     * @param regex The expression to be compiled.
     * @return the given regular expression compiled into a pattern with the {@link Pattern#DOTALL} flag.
     * @since 3.13.0
     */
    public static Pattern dotAll(final String regex) {
        return Pattern.compile(regex, Pattern.DOTALL);
    }

    /**
     * Compiles the given regular expression into a pattern with the {@link Pattern#DOTALL} flag, then creates a matcher that will match the given text against
     * this pattern.
     *
     * @param regex The expression to be compiled.
     * @param text  The character sequence to be matched.
     * @return A new matcher for this pattern.
     * @since 3.18.0
     */
    public static Matcher dotAllMatcher(final String regex, final CharSequence text) {
        return dotAll(regex).matcher(text);
    }

    /**
     * Compiles the given regular expression into a pattern with the {@link Pattern#DOTALL} flag, then creates a matcher that will match the given text against
     * this pattern.
     *
     * @param regex The expression to be compiled.
     * @param text  The character sequence to be matched.
     * @return A new matcher for this pattern.
     * @since 3.13.0
     * @deprecated Use {@link #dotAllMatcher(String, CharSequence)}.
     */
    @Deprecated
    public static Matcher dotAllMatcher(final String regex, final String text) {
        return dotAll(regex).matcher(text);
    }

    /**
     * Removes each substring of the text String that matches the given regular expression pattern.
     *
     * This method is a {@code null} safe equivalent to:
     * <ul>
     *  <li>{@code pattern.matcher(text).replaceAll(StringUtils.EMPTY)}</li>
     * </ul>
     *
     * <p>A {@code null} reference passed to this method is a no-op.</p>
     *
     * <pre>{@code
     * StringUtils.removeAll(null, *)      = null
     * StringUtils.removeAll("any", (Pattern) null)  = "any"
     * StringUtils.removeAll("any", Pattern.compile(""))    = "any"
     * StringUtils.removeAll("any", Pattern.compile(".*"))  = ""
     * StringUtils.removeAll("any", Pattern.compile(".+"))  = ""
     * StringUtils.removeAll("abc", Pattern.compile(".?"))  = ""
     * StringUtils.removeAll("A<__>\n<__>B", Pattern.compile("<.*>"))      = "A\nB"
     * StringUtils.removeAll("A<__>\n<__>B", Pattern.compile("(?s)<.*>"))  = "AB"
     * StringUtils.removeAll("A<__>\n<__>B", Pattern.compile("<.*>", Pattern.DOTALL))  = "AB"
     * StringUtils.removeAll("ABCabc123abc", Pattern.compile("[a-z]"))     = "ABC123"
     * }</pre>
     *
     * @param text  text to remove from, may be null.
     * @param regex  the regular expression to which this string is to be matched.
     * @return  the text with any removes processed,
     *              {@code null} if null String input.
     *
     * @see #replaceAll(CharSequence, Pattern, String)
     * @see java.util.regex.Matcher#replaceAll(String)
     * @see java.util.regex.Pattern
     * @since 3.18.0
     */
    public static String removeAll(final CharSequence text, final Pattern regex) {
        return replaceAll(text, regex, StringUtils.EMPTY);
    }

    /**
     * Removes each substring of the text String that matches the given regular expression pattern.
     *
     * This method is a {@code null} safe equivalent to:
     * <ul>
     *  <li>{@code pattern.matcher(text).replaceAll(StringUtils.EMPTY)}</li>
     * </ul>
     *
     * <p>A {@code null} reference passed to this method is a no-op.</p>
     *
     * <pre>{@code
     * StringUtils.removeAll(null, *)      = null
     * StringUtils.removeAll("any", (Pattern) null)  = "any"
     * StringUtils.removeAll("any", Pattern.compile(""))    = "any"
     * StringUtils.removeAll("any", Pattern.compile(".*"))  = ""
     * StringUtils.removeAll("any", Pattern.compile(".+"))  = ""
     * StringUtils.removeAll("abc", Pattern.compile(".?"))  = ""
     * StringUtils.removeAll("A<__>\n<__>B", Pattern.compile("<.*>"))      = "A\nB"
     * StringUtils.removeAll("A<__>\n<__>B", Pattern.compile("(?s)<.*>"))  = "AB"
     * StringUtils.removeAll("A<__>\n<__>B", Pattern.compile("<.*>", Pattern.DOTALL))  = "AB"
     * StringUtils.removeAll("ABCabc123abc", Pattern.compile("[a-z]"))     = "ABC123"
     * }</pre>
     *
     * @param text  text to remove from, may be null.
     * @param regex  the regular expression to which this string is to be matched
     * @return  the text with any removes processed,
     *              {@code null} if null String input.
     *
     * @see #replaceAll(CharSequence, Pattern, String)
     * @see java.util.regex.Matcher#replaceAll(String)
     * @see java.util.regex.Pattern
     * @deprecated Use {@link #removeAll(CharSequence, Pattern)}.
     */
    @Deprecated
    public static String removeAll(final String text, final Pattern regex) {
        return replaceAll((CharSequence) text, regex, StringUtils.EMPTY);
    }

    /**
     * Removes each substring of the text String that matches the given regular expression.
     *
     * This method is a {@code null} safe equivalent to:
     * <ul>
     *  <li>{@code text.replaceAll(regex, StringUtils.EMPTY)}</li>
     *  <li>{@code Pattern.compile(regex).matcher(text).replaceAll(StringUtils.EMPTY)}</li>
     * </ul>
     *
     * <p>A {@code null} reference passed to this method is a no-op.</p>
     *
     * <p>Unlike in the {@link #removePattern(CharSequence, String)} method, the {@link Pattern#DOTALL} option
     * is NOT automatically added.
     * To use the DOTALL option prepend {@code "(?s)"} to the regex.
     * DOTALL is also known as single-line mode in Perl.</p>
     *
     * <pre>{@code
     * StringUtils.removeAll(null, *)      = null
     * StringUtils.removeAll("any", (String) null)  = "any"
     * StringUtils.removeAll("any", "")    = "any"
     * StringUtils.removeAll("any", ".*")  = ""
     * StringUtils.removeAll("any", ".+")  = ""
     * StringUtils.removeAll("abc", ".?")  = ""
     * StringUtils.removeAll("A<__>\n<__>B", "<.*>")      = "A\nB"
     * StringUtils.removeAll("A<__>\n<__>B", "(?s)<.*>")  = "AB"
     * StringUtils.removeAll("ABCabc123abc", "[a-z]")     = "ABC123"
     * }</pre>
     *
     * @param text  text to remove from, may be null
     * @param regex  the regular expression to which this string is to be matched
     * @return  the text with any removes processed,
     *              {@code null} if null String input.
     *
     * @throws  java.util.regex.PatternSyntaxException
     *              if the regular expression's syntax is invalid.
     *
     * @see #replaceAll(String, String, String)
     * @see #removePattern(CharSequence, String)
     * @see String#replaceAll(String, String)
     * @see java.util.regex.Pattern
     * @see java.util.regex.Pattern#DOTALL
     */
    public static String removeAll(final String text, final String regex) {
        return replaceAll(text, regex, StringUtils.EMPTY);
    }

    /**
     * Removes the first substring of the text string that matches the given regular expression pattern.
     *
     * This method is a {@code null} safe equivalent to:
     * <ul>
     *  <li>{@code pattern.matcher(text).replaceFirst(StringUtils.EMPTY)}</li>
     * </ul>
     *
     * <p>A {@code null} reference passed to this method is a no-op.</p>
     *
     * <pre>{@code
     * StringUtils.removeFirst(null, *)      = null
     * StringUtils.removeFirst("any", (Pattern) null)  = "any"
     * StringUtils.removeFirst("any", Pattern.compile(""))    = "any"
     * StringUtils.removeFirst("any", Pattern.compile(".*"))  = ""
     * StringUtils.removeFirst("any", Pattern.compile(".+"))  = ""
     * StringUtils.removeFirst("abc", Pattern.compile(".?"))  = "bc"
     * StringUtils.removeFirst("A<__>\n<__>B", Pattern.compile("<.*>"))      = "A\n<__>B"
     * StringUtils.removeFirst("A<__>\n<__>B", Pattern.compile("(?s)<.*>"))  = "AB"
     * StringUtils.removeFirst("ABCabc123", Pattern.compile("[a-z]"))          = "ABCbc123"
     * StringUtils.removeFirst("ABCabc123abc", Pattern.compile("[a-z]+"))      = "ABC123abc"
     * }</pre>
     *
     * @param text  text to remove from, may be null.
     * @param regex  the regular expression pattern to which this string is to be matched.
     * @return  the text with the first replacement processed,
     *              {@code null} if null String input.
     *
     * @see #replaceFirst(String, Pattern, String)
     * @see java.util.regex.Matcher#replaceFirst(String)
     * @see java.util.regex.Pattern
     * @since 3.18.0
     */
    public static String removeFirst(final CharSequence text, final Pattern regex) {
        return replaceFirst(text, regex, StringUtils.EMPTY);
    }

    /**
     * Removes the first substring of the text string that matches the given regular expression pattern.
     *
     * This method is a {@code null} safe equivalent to:
     * <ul>
     *  <li>{@code pattern.matcher(text).replaceFirst(StringUtils.EMPTY)}</li>
     * </ul>
     *
     * <p>A {@code null} reference passed to this method is a no-op.</p>
     *
     * <pre>{@code
     * StringUtils.removeFirst(null, *)      = null
     * StringUtils.removeFirst("any", (Pattern) null)  = "any"
     * StringUtils.removeFirst("any", Pattern.compile(""))    = "any"
     * StringUtils.removeFirst("any", Pattern.compile(".*"))  = ""
     * StringUtils.removeFirst("any", Pattern.compile(".+"))  = ""
     * StringUtils.removeFirst("abc", Pattern.compile(".?"))  = "bc"
     * StringUtils.removeFirst("A<__>\n<__>B", Pattern.compile("<.*>"))      = "A\n<__>B"
     * StringUtils.removeFirst("A<__>\n<__>B", Pattern.compile("(?s)<.*>"))  = "AB"
     * StringUtils.removeFirst("ABCabc123", Pattern.compile("[a-z]"))          = "ABCbc123"
     * StringUtils.removeFirst("ABCabc123abc", Pattern.compile("[a-z]+"))      = "ABC123abc"
     * }</pre>
     *
     * @param text  text to remove from, may be null.
     * @param regex  the regular expression pattern to which this string is to be matched.
     * @return  the text with the first replacement processed,
     *              {@code null} if null String input.
     *
     * @see #replaceFirst(String, Pattern, String)
     * @see java.util.regex.Matcher#replaceFirst(String)
     * @see java.util.regex.Pattern
     * @deprecated Use {@link #removeFirst(CharSequence, Pattern)}.
     */
    @Deprecated
    public static String removeFirst(final String text, final Pattern regex) {
        return replaceFirst(text, regex, StringUtils.EMPTY);
    }

    /**
     * Removes the first substring of the text string that matches the given regular expression.
     *
     * This method is a {@code null} safe equivalent to:
     * <ul>
     *  <li>{@code text.replaceFirst(regex, StringUtils.EMPTY)}</li>
     *  <li>{@code Pattern.compile(regex).matcher(text).replaceFirst(StringUtils.EMPTY)}</li>
     * </ul>
     *
     * <p>A {@code null} reference passed to this method is a no-op.</p>
     *
     * <p>The {@link Pattern#DOTALL} option is NOT automatically added.
     * To use the DOTALL option prepend {@code "(?s)"} to the regex.
     * DOTALL is also known as single-line mode in Perl.</p>
     *
     * <pre>{@code
     * StringUtils.removeFirst(null, *)      = null
     * StringUtils.removeFirst("any", (String) null)  = "any"
     * StringUtils.removeFirst("any", "")    = "any"
     * StringUtils.removeFirst("any", ".*")  = ""
     * StringUtils.removeFirst("any", ".+")  = ""
     * StringUtils.removeFirst("abc", ".?")  = "bc"
     * StringUtils.removeFirst("A<__>\n<__>B", "<.*>")      = "A\n<__>B"
     * StringUtils.removeFirst("A<__>\n<__>B", "(?s)<.*>")  = "AB"
     * StringUtils.removeFirst("ABCabc123", "[a-z]")          = "ABCbc123"
     * StringUtils.removeFirst("ABCabc123abc", "[a-z]+")      = "ABC123abc"
     * }</pre>
     *
     * @param text  text to remove from, may be null.
     * @param regex  the regular expression to which this string is to be matched.
     * @return  the text with the first replacement processed,
     *              {@code null} if null String input.
     *
     * @throws  java.util.regex.PatternSyntaxException
     *              if the regular expression's syntax is invalid.
     *
     * @see #replaceFirst(String, String, String)
     * @see String#replaceFirst(String, String)
     * @see java.util.regex.Pattern
     * @see java.util.regex.Pattern#DOTALL
     */
    public static String removeFirst(final String text, final String regex) {
        return replaceFirst(text, regex, StringUtils.EMPTY);
    }

    /**
     * Removes each substring of the source String that matches the given regular expression using the DOTALL option.
     *
     * This call is a {@code null} safe equivalent to:
     * <ul>
     * <li>{@code text.replaceAll(&quot;(?s)&quot; + regex, StringUtils.EMPTY)}</li>
     * <li>{@code Pattern.compile(regex, Pattern.DOTALL).matcher(text).replaceAll(StringUtils.EMPTY)}</li>
     * </ul>
     *
     * <p>A {@code null} reference passed to this method is a no-op.</p>
     *
     * <pre>{@code
     * StringUtils.removePattern(null, *)       = null
     * StringUtils.removePattern("any", (String) null)   = "any"
     * StringUtils.removePattern("A<__>\n<__>B", "<.*>")  = "AB"
     * StringUtils.removePattern("ABCabc123", "[a-z]")    = "ABC123"
     * }</pre>
     *
     * @param text
     *            the source string.
     * @param regex
     *            the regular expression to which this string is to be matched.
     * @return The resulting {@link String}.
     * @see #replacePattern(CharSequence, String, String)
     * @see String#replaceAll(String, String)
     * @see Pattern#DOTALL
     * @since 3.18.0
     */
    public static String removePattern(final CharSequence text, final String regex) {
        return replacePattern(text, regex, StringUtils.EMPTY);
    }

    /**
     * Removes each substring of the source String that matches the given regular expression using the DOTALL option.
     *
     * This call is a {@code null} safe equivalent to:
     * <ul>
     * <li>{@code text.replaceAll(&quot;(?s)&quot; + regex, StringUtils.EMPTY)}</li>
     * <li>{@code Pattern.compile(regex, Pattern.DOTALL).matcher(text).replaceAll(StringUtils.EMPTY)}</li>
     * </ul>
     *
     * <p>A {@code null} reference passed to this method is a no-op.</p>
     *
     * <pre>{@code
     * StringUtils.removePattern(null, *)       = null
     * StringUtils.removePattern("any", (String) null)   = "any"
     * StringUtils.removePattern("A<__>\n<__>B", "<.*>")  = "AB"
     * StringUtils.removePattern("ABCabc123", "[a-z]")    = "ABC123"
     * }</pre>
     *
     * @param text
     *            the source string.
     * @param regex
     *            the regular expression to which this string is to be matched.
     * @return The resulting {@link String}.
     * @see #replacePattern(CharSequence, String, String)
     * @see String#replaceAll(String, String)
     * @see Pattern#DOTALL
     * @deprecated use {@link #removePattern(CharSequence, String)}.
     */
    @Deprecated
    public static String removePattern(final String text, final String regex) {
        return replacePattern((CharSequence) text, regex, StringUtils.EMPTY);
    }

    /**
     * Replaces each substring of the text String that matches the given regular expression pattern with the given replacement.
     *
     * This method is a {@code null} safe equivalent to:
     * <ul>
     *  <li>{@code pattern.matcher(text).replaceAll(replacement)}</li>
     * </ul>
     *
     * <p>A {@code null} reference passed to this method is a no-op.</p>
     *
     * <pre>{@code
     * StringUtils.replaceAll(null, *, *)       = null
     * StringUtils.replaceAll("any", (Pattern) null, *)   = "any"
     * StringUtils.replaceAll("any", *, null)   = "any"
     * StringUtils.replaceAll("", Pattern.compile(""), "zzz")    = "zzz"
     * StringUtils.replaceAll("", Pattern.compile(".*"), "zzz")  = "zzz"
     * StringUtils.replaceAll("", Pattern.compile(".+"), "zzz")  = ""
     * StringUtils.replaceAll("abc", Pattern.compile(""), "ZZ")  = "ZZaZZbZZcZZ"
     * StringUtils.replaceAll("<__>\n<__>", Pattern.compile("<.*>"), "z")                 = "z\nz"
     * StringUtils.replaceAll("<__>\n<__>", Pattern.compile("<.*>", Pattern.DOTALL), "z") = "z"
     * StringUtils.replaceAll("<__>\n<__>", Pattern.compile("(?s)<.*>"), "z")             = "z"
     * StringUtils.replaceAll("ABCabc123", Pattern.compile("[a-z]"), "_")       = "ABC___123"
     * StringUtils.replaceAll("ABCabc123", Pattern.compile("[^A-Z0-9]+"), "_")  = "ABC_123"
     * StringUtils.replaceAll("ABCabc123", Pattern.compile("[^A-Z0-9]+"), "")   = "ABC123"
     * StringUtils.replaceAll("Lorem ipsum  dolor   sit", Pattern.compile("( +)([a-z]+)"), "_$2")  = "Lorem_ipsum_dolor_sit"
     * }</pre>
     *
     * @param text  text to search and replace in, may be null.
     * @param regex  the regular expression pattern to which this string is to be matched.
     * @param replacement  the string to be substituted for each match.
     * @return  the text with any replacements processed,
     *              {@code null} if null String input.
     * @see java.util.regex.Matcher#replaceAll(String)
     * @see java.util.regex.Pattern
     */
    public static String replaceAll(final CharSequence text, final Pattern regex, final String replacement) {
        if (ObjectUtils.anyNull(text, regex, replacement)) {
            return toStringOrNull(text);
        }
        return regex.matcher(text).replaceAll(replacement);
    }

    /**
     * Replaces each substring of the text String that matches the given regular expression pattern with the given replacement.
     *
     * This method is a {@code null} safe equivalent to:
     * <ul>
     *  <li>{@code pattern.matcher(text).replaceAll(replacement)}</li>
     * </ul>
     *
     * <p>A {@code null} reference passed to this method is a no-op.</p>
     *
     * <pre>{@code
     * StringUtils.replaceAll(null, *, *)       = null
     * StringUtils.replaceAll("any", (Pattern) null, *)   = "any"
     * StringUtils.replaceAll("any", *, null)   = "any"
     * StringUtils.replaceAll("", Pattern.compile(""), "zzz")    = "zzz"
     * StringUtils.replaceAll("", Pattern.compile(".*"), "zzz")  = "zzz"
     * StringUtils.replaceAll("", Pattern.compile(".+"), "zzz")  = ""
     * StringUtils.replaceAll("abc", Pattern.compile(""), "ZZ")  = "ZZaZZbZZcZZ"
     * StringUtils.replaceAll("<__>\n<__>", Pattern.compile("<.*>"), "z")                 = "z\nz"
     * StringUtils.replaceAll("<__>\n<__>", Pattern.compile("<.*>", Pattern.DOTALL), "z") = "z"
     * StringUtils.replaceAll("<__>\n<__>", Pattern.compile("(?s)<.*>"), "z")             = "z"
     * StringUtils.replaceAll("ABCabc123", Pattern.compile("[a-z]"), "_")       = "ABC___123"
     * StringUtils.replaceAll("ABCabc123", Pattern.compile("[^A-Z0-9]+"), "_")  = "ABC_123"
     * StringUtils.replaceAll("ABCabc123", Pattern.compile("[^A-Z0-9]+"), "")   = "ABC123"
     * StringUtils.replaceAll("Lorem ipsum  dolor   sit", Pattern.compile("( +)([a-z]+)"), "_$2")  = "Lorem_ipsum_dolor_sit"
     * }</pre>
     *
     * @param text  text to search and replace in, may be null.
     * @param regex  the regular expression pattern to which this string is to be matched.
     * @param replacement  the string to be substituted for each match.
     * @return  the text with any replacements processed,
     *              {@code null} if null String input.
     * @see java.util.regex.Matcher#replaceAll(String)
     * @see java.util.regex.Pattern
     * @deprecated Use {@link #replaceAll(CharSequence, Pattern, String)}.
     */
    @Deprecated
    public static String replaceAll(final String text, final Pattern regex, final String replacement) {
        return replaceAll((CharSequence) text, regex, replacement);
    }

    /**
     * Replaces each substring of the text String that matches the given regular expression
     * with the given replacement.
     *
     * This method is a {@code null} safe equivalent to:
     * <ul>
     *  <li>{@code text.replaceAll(regex, replacement)}</li>
     *  <li>{@code Pattern.compile(regex).matcher(text).replaceAll(replacement)}</li>
     * </ul>
     *
     * <p>A {@code null} reference passed to this method is a no-op.</p>
     *
     * <p>Unlike in the {@link #replacePattern(CharSequence, String, String)} method, the {@link Pattern#DOTALL} option
     * is NOT automatically added.
     * To use the DOTALL option prepend {@code "(?s)"} to the regex.
     * DOTALL is also known as single-line mode in Perl.</p>
     *
     * <pre>{@code
     * StringUtils.replaceAll(null, *, *)       = null
     * StringUtils.replaceAll("any", (String) null, *)   = "any"
     * StringUtils.replaceAll("any", *, null)   = "any"
     * StringUtils.replaceAll("", "", "zzz")    = "zzz"
     * StringUtils.replaceAll("", ".*", "zzz")  = "zzz"
     * StringUtils.replaceAll("", ".+", "zzz")  = ""
     * StringUtils.replaceAll("abc", "", "ZZ")  = "ZZaZZbZZcZZ"
     * StringUtils.replaceAll("<__>\n<__>", "<.*>", "z")      = "z\nz"
     * StringUtils.replaceAll("<__>\n<__>", "(?s)<.*>", "z")  = "z"
     * StringUtils.replaceAll("ABCabc123", "[a-z]", "_")       = "ABC___123"
     * StringUtils.replaceAll("ABCabc123", "[^A-Z0-9]+", "_")  = "ABC_123"
     * StringUtils.replaceAll("ABCabc123", "[^A-Z0-9]+", "")   = "ABC123"
     * StringUtils.replaceAll("Lorem ipsum  dolor   sit", "( +)([a-z]+)", "_$2")  = "Lorem_ipsum_dolor_sit"
     * }</pre>
     *
     * @param text  text to search and replace in, may be null.
     * @param regex  the regular expression to which this string is to be matched.
     * @param replacement  the string to be substituted for each match.
     * @return  the text with any replacements processed,
     *              {@code null} if null String input.
     * @throws  java.util.regex.PatternSyntaxException
     *              if the regular expression's syntax is invalid.
     * @see #replacePattern(String, String, String)
     * @see String#replaceAll(String, String)
     * @see java.util.regex.Pattern
     * @see java.util.regex.Pattern#DOTALL
     */
    public static String replaceAll(final String text, final String regex, final String replacement) {
        if (ObjectUtils.anyNull(text, regex, replacement)) {
            return text;
        }
        return text.replaceAll(regex, replacement);
    }

    /**
     * Replaces the first substring of the text string that matches the given regular expression pattern
     * with the given replacement.
     *
     * This method is a {@code null} safe equivalent to:
     * <ul>
     *  <li>{@code pattern.matcher(text).replaceFirst(replacement)}</li>
     * </ul>
     *
     * <p>A {@code null} reference passed to this method is a no-op.</p>
     *
     * <pre>{@code
     * StringUtils.replaceFirst(null, *, *)       = null
     * StringUtils.replaceFirst("any", (Pattern) null, *)   = "any"
     * StringUtils.replaceFirst("any", *, null)   = "any"
     * StringUtils.replaceFirst("", Pattern.compile(""), "zzz")    = "zzz"
     * StringUtils.replaceFirst("", Pattern.compile(".*"), "zzz")  = "zzz"
     * StringUtils.replaceFirst("", Pattern.compile(".+"), "zzz")  = ""
     * StringUtils.replaceFirst("abc", Pattern.compile(""), "ZZ")  = "ZZabc"
     * StringUtils.replaceFirst("<__>\n<__>", Pattern.compile("<.*>"), "z")      = "z\n<__>"
     * StringUtils.replaceFirst("<__>\n<__>", Pattern.compile("(?s)<.*>"), "z")  = "z"
     * StringUtils.replaceFirst("ABCabc123", Pattern.compile("[a-z]"), "_")          = "ABC_bc123"
     * StringUtils.replaceFirst("ABCabc123abc", Pattern.compile("[^A-Z0-9]+"), "_")  = "ABC_123abc"
     * StringUtils.replaceFirst("ABCabc123abc", Pattern.compile("[^A-Z0-9]+"), "")   = "ABC123abc"
     * StringUtils.replaceFirst("Lorem ipsum  dolor   sit", Pattern.compile("( +)([a-z]+)"), "_$2")  = "Lorem_ipsum  dolor   sit"
     * }</pre>
     *
     * @param text  text to search and replace in, may be null.
     * @param regex  the regular expression pattern to which this string is to be matched.
     * @param replacement  the string to be substituted for the first match
     * @return  the text with the first replacement processed,
     *              {@code null} if null String input.
     * @see java.util.regex.Matcher#replaceFirst(String)
     * @see java.util.regex.Pattern
     * @since 3.18.0
     */
    public static String replaceFirst(final CharSequence text, final Pattern regex, final String replacement) {
        if (text == null || regex == null || replacement == null) {
            return toStringOrNull(text);
        }
        return regex.matcher(text).replaceFirst(replacement);
    }

    /**
     * Replaces the first substring of the text string that matches the given regular expression pattern
     * with the given replacement.
     *
     * This method is a {@code null} safe equivalent to:
     * <ul>
     *  <li>{@code pattern.matcher(text).replaceFirst(replacement)}</li>
     * </ul>
     *
     * <p>A {@code null} reference passed to this method is a no-op.</p>
     *
     * <pre>{@code
     * StringUtils.replaceFirst(null, *, *)       = null
     * StringUtils.replaceFirst("any", (Pattern) null, *)   = "any"
     * StringUtils.replaceFirst("any", *, null)   = "any"
     * StringUtils.replaceFirst("", Pattern.compile(""), "zzz")    = "zzz"
     * StringUtils.replaceFirst("", Pattern.compile(".*"), "zzz")  = "zzz"
     * StringUtils.replaceFirst("", Pattern.compile(".+"), "zzz")  = ""
     * StringUtils.replaceFirst("abc", Pattern.compile(""), "ZZ")  = "ZZabc"
     * StringUtils.replaceFirst("<__>\n<__>", Pattern.compile("<.*>"), "z")      = "z\n<__>"
     * StringUtils.replaceFirst("<__>\n<__>", Pattern.compile("(?s)<.*>"), "z")  = "z"
     * StringUtils.replaceFirst("ABCabc123", Pattern.compile("[a-z]"), "_")          = "ABC_bc123"
     * StringUtils.replaceFirst("ABCabc123abc", Pattern.compile("[^A-Z0-9]+"), "_")  = "ABC_123abc"
     * StringUtils.replaceFirst("ABCabc123abc", Pattern.compile("[^A-Z0-9]+"), "")   = "ABC123abc"
     * StringUtils.replaceFirst("Lorem ipsum  dolor   sit", Pattern.compile("( +)([a-z]+)"), "_$2")  = "Lorem_ipsum  dolor   sit"
     * }</pre>
     *
     * @param text  text to search and replace in, may be null.
     * @param regex  the regular expression pattern to which this string is to be matched.
     * @param replacement  the string to be substituted for the first match.
     * @return  the text with the first replacement processed,
     *              {@code null} if null String input.
     * @see java.util.regex.Matcher#replaceFirst(String)
     * @see java.util.regex.Pattern
     * @deprecated Use {@link #replaceFirst(CharSequence, Pattern, String)}.
     */
    @Deprecated
    public static String replaceFirst(final String text, final Pattern regex, final String replacement) {
        return replaceFirst((CharSequence) text, regex, replacement);
    }

    /**
     * Replaces the first substring of the text string that matches the given regular expression
     * with the given replacement.
     *
     * This method is a {@code null} safe equivalent to:
     * <ul>
     *  <li>{@code text.replaceFirst(regex, replacement)}</li>
     *  <li>{@code Pattern.compile(regex).matcher(text).replaceFirst(replacement)}</li>
     * </ul>
     *
     * <p>A {@code null} reference passed to this method is a no-op.</p>
     *
     * <p>The {@link Pattern#DOTALL} option is NOT automatically added.
     * To use the DOTALL option prepend {@code "(?s)"} to the regex.
     * DOTALL is also known as single-line mode in Perl.</p>
     *
     * <pre>{@code
     * StringUtils.replaceFirst(null, *, *)       = null
     * StringUtils.replaceFirst("any", (String) null, *)   = "any"
     * StringUtils.replaceFirst("any", *, null)   = "any"
     * StringUtils.replaceFirst("", "", "zzz")    = "zzz"
     * StringUtils.replaceFirst("", ".*", "zzz")  = "zzz"
     * StringUtils.replaceFirst("", ".+", "zzz")  = ""
     * StringUtils.replaceFirst("abc", "", "ZZ")  = "ZZabc"
     * StringUtils.replaceFirst("<__>\n<__>", "<.*>", "z")      = "z\n<__>"
     * StringUtils.replaceFirst("<__>\n<__>", "(?s)<.*>", "z")  = "z"
     * StringUtils.replaceFirst("ABCabc123", "[a-z]", "_")          = "ABC_bc123"
     * StringUtils.replaceFirst("ABCabc123abc", "[^A-Z0-9]+", "_")  = "ABC_123abc"
     * StringUtils.replaceFirst("ABCabc123abc", "[^A-Z0-9]+", "")   = "ABC123abc"
     * StringUtils.replaceFirst("Lorem ipsum  dolor   sit", "( +)([a-z]+)", "_$2")  = "Lorem_ipsum  dolor   sit"
     * }</pre>
     *
     * @param text  text to search and replace in, may be null.
     * @param regex  the regular expression to which this string is to be matched.
     * @param replacement  the string to be substituted for the first match.
     * @return  the text with the first replacement processed,
     *              {@code null} if null String input.
     * @throws  java.util.regex.PatternSyntaxException
     *              if the regular expression's syntax is invalid.
     * @see String#replaceFirst(String, String)
     * @see java.util.regex.Pattern
     * @see java.util.regex.Pattern#DOTALL
     */
    public static String replaceFirst(final String text, final String regex, final String replacement) {
        if (text == null || regex == null || replacement == null) {
            return text;
        }
        return text.replaceFirst(regex, replacement);
    }

    /**
     * Replaces each substring of the source String that matches the given regular expression with the given
     * replacement using the {@link Pattern#DOTALL} option. DOTALL is also known as single-line mode in Perl.
     *
     * This call is a {@code null} safe equivalent to:
     * <ul>
     * <li>{@code text.replaceAll(&quot;(?s)&quot; + regex, replacement)}</li>
     * <li>{@code Pattern.compile(regex, Pattern.DOTALL).matcher(text).replaceAll(replacement)}</li>
     * </ul>
     *
     * <p>A {@code null} reference passed to this method is a no-op.</p>
     *
     * <pre>{@code
     * StringUtils.replacePattern(null, *, *)       = null
     * StringUtils.replacePattern("any", (String) null, *)   = "any"
     * StringUtils.replacePattern("any", *, null)   = "any"
     * StringUtils.replacePattern("", "", "zzz")    = "zzz"
     * StringUtils.replacePattern("", ".*", "zzz")  = "zzz"
     * StringUtils.replacePattern("", ".+", "zzz")  = ""
     * StringUtils.replacePattern("<__>\n<__>", "<.*>", "z")       = "z"
     * StringUtils.replacePattern("ABCabc123", "[a-z]", "_")       = "ABC___123"
     * StringUtils.replacePattern("ABCabc123", "[^A-Z0-9]+", "_")  = "ABC_123"
     * StringUtils.replacePattern("ABCabc123", "[^A-Z0-9]+", "")   = "ABC123"
     * StringUtils.replacePattern("Lorem ipsum  dolor   sit", "( +)([a-z]+)", "_$2")  = "Lorem_ipsum_dolor_sit"
     * }</pre>
     *
     * @param text
     *            the source string.
     * @param regex
     *            the regular expression to which this string is to be matched.
     * @param replacement
     *            the string to be substituted for each match.
     * @return The resulting {@link String}.
     * @see #replaceAll(String, String, String)
     * @see String#replaceAll(String, String)
     * @see Pattern#DOTALL
     * @since 3.18.0
     */
    public static String replacePattern(final CharSequence text, final String regex, final String replacement) {
        if (ObjectUtils.anyNull(text, regex, replacement)) {
            return toStringOrNull(text);
        }
        return dotAllMatcher(regex, text).replaceAll(replacement);
    }

    /**
     * Replaces each substring of the source String that matches the given regular expression with the given
     * replacement using the {@link Pattern#DOTALL} option. DOTALL is also known as single-line mode in Perl.
     *
     * This call is a {@code null} safe equivalent to:
     * <ul>
     * <li>{@code text.replaceAll(&quot;(?s)&quot; + regex, replacement)}</li>
     * <li>{@code Pattern.compile(regex, Pattern.DOTALL).matcher(text).replaceAll(replacement)}</li>
     * </ul>
     *
     * <p>A {@code null} reference passed to this method is a no-op.</p>
     *
     * <pre>{@code
     * StringUtils.replacePattern(null, *, *)       = null
     * StringUtils.replacePattern("any", (String) null, *)   = "any"
     * StringUtils.replacePattern("any", *, null)   = "any"
     * StringUtils.replacePattern("", "", "zzz")    = "zzz"
     * StringUtils.replacePattern("", ".*", "zzz")  = "zzz"
     * StringUtils.replacePattern("", ".+", "zzz")  = ""
     * StringUtils.replacePattern("<__>\n<__>", "<.*>", "z")       = "z"
     * StringUtils.replacePattern("ABCabc123", "[a-z]", "_")       = "ABC___123"
     * StringUtils.replacePattern("ABCabc123", "[^A-Z0-9]+", "_")  = "ABC_123"
     * StringUtils.replacePattern("ABCabc123", "[^A-Z0-9]+", "")   = "ABC123"
     * StringUtils.replacePattern("Lorem ipsum  dolor   sit", "( +)([a-z]+)", "_$2")  = "Lorem_ipsum_dolor_sit"
     * }</pre>
     *
     * @param text
     *            the source string.
     * @param regex
     *            the regular expression to which this string is to be matched.
     * @param replacement
     *            the string to be substituted for each match.
     * @return The resulting {@link String}.
     * @see #replaceAll(String, String, String)
     * @see String#replaceAll(String, String)
     * @see Pattern#DOTALL
     * @deprecated Use {@link #replacePattern(CharSequence, String, String)}.
     */
    @Deprecated
    public static String replacePattern(final String text, final String regex, final String replacement) {
        return replacePattern((CharSequence) text, regex, replacement);
    }

    private static String toStringOrNull(final CharSequence text) {
        return Objects.toString(text, null);
    }

    /**
     * Make private in 4.0.
     *
     * @deprecated TODO Make private in 4.0.
     */
    @Deprecated
    public RegExUtils() {
        // empty
    }
}
