/*
 * Copyright 2003-2004 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.ListIterator;

/**
 * Tokenizes a string based based on delimiters (separators)
 * and supporting quoting and ignored character concepts.
 * <p>
 * This class can split a String into many smaller strings.
 * It aims to do a similar job to java util StringTokenizer, however it offers
 * much more control and flexibility. By default, it is setup like StringTokenizer.
 * <p>
 * The input String is split into a number of <i>tokens</i>.
 * Each token is separated from the next String by a <i>delimiter</i>.
 * One or more delimiter characters must be specified.
 * <p>
 * The processing then strips all the <i>ignored</i> characters from each side of the token.
 * The token may also have <i>quotes</i> to mark an area not to be stripped or tokenized.
 * Empty tokens may be removed or returned as null.
 * This example is based on the CSV tokenizer.
 * <pre>
 * "a,b,c"       - Three tokens "a","b","c"   (comma delimiter)
 * "a, b , c"    - Three tokens "a","b","c"   (ignored space characters stripped)
 * "a, " b ", c" - Three tokens "a"," b ","c" (quoted text untouched)
 * </pre>
 * <p>
 *
 * This tokenizer has the following properties and options:
 *
 * <table>
 *  <tr>
 *   <th>Property</th><th>Type</th><th>Default</th>
 *  </tr>
 *  <tr>
 *   <td>delim</td><td>CharSetMatcher</td><td>{ \t\n\r\f}</td>
 *  </tr>
 *  <tr>
 *   <td>quote</td><td>NoneMatcher</td><td>{}</td>
 *  </tr>
 *  <tr>
 *   <td>ignore</td><td>NoneMatcher</td><td>{}</td>
 *  </tr>
 *  <tr>
 *   <td>emptyTokenAsNull</td><td>boolean</td><td>false</td>
 *  </tr>
 *  <tr>
 *   <td>ignoreEmptyTokens</td><td>boolean</td><td>true</td>
 *  </tr>
 * </table>
 *
 * @author Matthew Inger
 * @author Stephen Colebourne
 * @author Gary D. Gregory
 * @since 2.1
 * @version $Id: Tokenizer.java,v 1.5 2004/02/18 22:59:50 ggregory Exp $
 */
public class Tokenizer implements ListIterator, Cloneable {

    /**
     * A Matcher which matches the comma character.
     * Best used for <code>delimiter</code>.
     */
    public static final Matcher COMMA_MATCHER = new CharMatcher(',');
    /**
     * A Matcher which matches the tab character.
     * Best used for <code>delimiter</code>.
     */
    public static final Matcher TAB_MATCHER = new CharMatcher('\t');
    /**
     * A Matcher which matches the space character.
     * Best used for <code>delimiter</code>.
     */
    public static final Matcher SPACE_MATCHER = new CharMatcher(' ');
    /**
     * A Matcher which matches the same characters as StringTokenizer,
     * namely space, tab, newline, formfeed.
     * Best used for <code>delimiter</code>.
     */
    public static final Matcher SPLIT_MATCHER = new CharSetMatcher(" \t\n\r\f");
    /**
     * A Matcher which matches the double quote character.
     * Best used for <code>quote</code>.
     */
    public static final Matcher DOUBLE_QUOTE_MATCHER = new CharMatcher('"');
    /**
     * A Matcher which matches the String trim() whitespace characters.
     * Best used for <code>ignored</code>.
     */
    public static final Matcher TRIM_MATCHER = new TrimMatcher();
    /**
     * A Matcher that matches no characters. Don't use this for delimiters!
     * Best used for <code>ignored</code>.
     */
    public static final Matcher NONE_MATCHER = new NoMatcher();
    
    private static final Tokenizer CSV_TOKENIZER_PROTOTYPE;
    private static final Tokenizer TSV_TOKENIZER_PROTOTYPE;

    static {
        CSV_TOKENIZER_PROTOTYPE = new Tokenizer(StringUtils.EMPTY);
        CSV_TOKENIZER_PROTOTYPE.setDelimiterMatcher(COMMA_MATCHER);
        CSV_TOKENIZER_PROTOTYPE.setQuoteMatcher(DOUBLE_QUOTE_MATCHER);
        CSV_TOKENIZER_PROTOTYPE.setIgnoredMatcher(TRIM_MATCHER);
        CSV_TOKENIZER_PROTOTYPE.setEmptyTokenAsNull(false);
        CSV_TOKENIZER_PROTOTYPE.setIgnoreEmptyTokens(false);

        TSV_TOKENIZER_PROTOTYPE = new Tokenizer(StringUtils.EMPTY);
        TSV_TOKENIZER_PROTOTYPE.setDelimiterMatcher(TAB_MATCHER);
        TSV_TOKENIZER_PROTOTYPE.setQuoteMatcher(DOUBLE_QUOTE_MATCHER);
        TSV_TOKENIZER_PROTOTYPE.setIgnoredMatcher(TRIM_MATCHER);
        TSV_TOKENIZER_PROTOTYPE.setEmptyTokenAsNull(false);
        TSV_TOKENIZER_PROTOTYPE.setIgnoreEmptyTokens(false);
    }

    /** The text to work on */
    private char chars[];
    /** The input text, null if char[] input */
    private String text;
    /** The parsed tokens */
    private String tokens[];
    /** The current iteration position */
    private int tokenPos;

    /** The delimiter matcher */
    private Matcher delim = SPLIT_MATCHER;
    /** The quote matcher */
    private Matcher quote = NONE_MATCHER;
    /** The ignored matcher */
    private Matcher ignored = NONE_MATCHER;
    /** Whether to return empty tokens as null */
    private boolean emptyAsNull = false;
    /** Whether to ignore empty tokens */
    private boolean ignoreEmptyTokens = true;

    //-----------------------------------------------------------------------
    /**
     * Get a tokenizer instance which parses Comma Seperated Value
     * strings.  You must call a "reset" method to set the string which
     * you want to parse.
     */
    public static final Tokenizer getCSVInstance() {
        return (Tokenizer)(CSV_TOKENIZER_PROTOTYPE.clone());
    }

    /**
     * Get a tokenizer instance which parses Comma Seperated Value
     * strings, initializing it with the given input.
     * 
     * @param input  the string to parse
     */
    public static final Tokenizer getCSVInstance(String input) {
        Tokenizer tok = (Tokenizer)(CSV_TOKENIZER_PROTOTYPE.clone());
        tok.reset(input);
        return tok;
    }

    /**
     * Get a tokenizer instance which parses Comma Seperated Value
     * strings, initializing it with the given input.
     * 
     * @param input  the text to parse
     */
    public static final Tokenizer getCSVInstance(char[] input) {
        Tokenizer tok = (Tokenizer)(CSV_TOKENIZER_PROTOTYPE.clone());
        tok.reset(input);
        return tok;
    }

    /**
     * Get a tokenizer instance which parses Tab Seperated Value
     * strings.  You must call a "reset" method to set the string which
     * you want to parse.
     */
    public static final Tokenizer getTSVInstance() {
        return (Tokenizer)(TSV_TOKENIZER_PROTOTYPE.clone());
    }

    /**
     * Get a tokenizer instance which parses Tab Seperated Value
     * strings, initializing it with the given input.
     * 
     * @param input  the string to parse
     */
    public static final Tokenizer getTSVInstance(String input) {
        Tokenizer tok = (Tokenizer)(TSV_TOKENIZER_PROTOTYPE.clone());
        tok.reset(input);
        return tok;
    }

    /**
     * Get a tokenizer instance which parses Tab Seperated Value
     * strings, initializing it with the given input.
     * 
     * @param input  the text to parse
     */
    public static final Tokenizer getTSVInstance(char[] input) {
        Tokenizer tok = (Tokenizer)(TSV_TOKENIZER_PROTOTYPE.clone());
        tok.reset(input);
        return tok;
    }

    //-----------------------------------------------------------------------
    /**
     * Constructs a tokenizer splitting on space, tab, newline and formfeed
     * as per StringTokenizer.
     * 
     * @param input  the string which is to be parsed
     */
    public Tokenizer(String input) {
        super();
        this.text = input;
        this.chars = input.toCharArray();  // no clone as toCharArray() clones
    }

    /**
     * Constructs a tokenizer splitting on space, tab, newline and formfeed
     * as per StringTokenizer.
     * 
     * @param input  the string which is to be parsed
     * @param delim  the field delimiter character
     */
    public Tokenizer(String input, char delim) {
        this(input);
        setDelimiterChar(delim);
    }

    /**
     * Constructs a tokenizer splitting on space, tab, newline and formfeed
     * as per StringTokenizer.
     * 
     * @param input  the string which is to be parsed
     * @param delim  the field delimiter character
     */
    public Tokenizer(String input, CharSetMatcher delim) {
        this(input);
        setDelimiterMatcher(delim);
    }

    /**
     * Constructs a tokenizer splitting on space, tab, newline and formfeed
     * as per StringTokenizer.
     * 
     * @param input  the string which is to be parsed
     * @param delim  the field delimiter character
     * @param quote  the field quoted string character
     */
    public Tokenizer(String input, char delim, char quote) {
        this(input, delim);
        setQuoteChar(quote);
    }

    /**
     * Constructs a tokenizer splitting on space, tab, newline and formfeed
     * as per StringTokenizer.
     * 
     * @param input  the string which is to be parsed
     * @param delim  the field delimiter character
     * @param quote  the field quoted string character
     */
    public Tokenizer(String input, CharSetMatcher delim, CharSetMatcher quote) {
        this(input, delim);
        setQuoteMatcher(quote);
    }

    /**
     * Constructs a tokenizer splitting on space, tab, newline and formfeed
     * as per StringTokenizer.
     * 
     * @param input  the string which is to be parsed, cloned
     */
    public Tokenizer(char[] input) {
        super();
        this.text = null;
        this.chars = (char[]) input.clone();
    }

    /**
     * Constructs a tokenizer splitting on space, tab, newline and formfeed
     * as per StringTokenizer.
     * 
     * @param input  the string which is to be parsed, cloned
     * @param delim the field delimiter character
     */
    public Tokenizer(char[] input, char delim) {
        this(input);
        setDelimiterChar(delim);
    }

    /**
     * Constructs a tokenizer splitting on space, tab, newline and formfeed
     * as per StringTokenizer.
     * 
     * @param input  the string which is to be parsed, cloned
     * @param delim  the field delimiter character
     */
    public Tokenizer(char[] input, CharSetMatcher delim) {
        this(input);
        setDelimiterMatcher(delim);
    }

    /**
     * Constructs a tokenizer splitting on space, tab, newline and formfeed
     * as per StringTokenizer.
     * 
     * @param input  the string which is to be parsed, cloned
     * @param delim  the field delimiter character
     * @param quote  the field quoted string character
     */
    public Tokenizer(char[] input, char delim, char quote) {
        this(input, delim);
        setQuoteChar(quote);
    }

    /**
     * Constructs a tokenizer splitting on space, tab, newline and formfeed
     * as per StringTokenizer.
     * 
     * @param input  the string which is to be parsed, cloned
     * @param delim  the field delimiter character
     * @param quote  the field quoted string character
     */
    public Tokenizer(char[] input, CharSetMatcher delim, CharSetMatcher quote) {
        this(input, delim);
        setQuoteMatcher(quote);
    }

    // API
    //-----------------------------------------------------------------------
    /**
     * Gets the number of tokens found in the String.
     * 
     * @return the number of matched tokens
     */
    public int size() {
        tokenize();
        return tokens.length;
    }

    /**
     * Gets the next token from the String.
     * 
     * @return the next sequential token, or null when no more tokens are found
     */
    public String nextToken() {
        if (hasNext()) {
            return tokens[tokenPos++];
        } else {
            return null;
        }
    }

    /**
     * Gets the previous token from the String.
     * 
     * @return the previous sequential token, or null when no more tokens are found
     */
    public String previousToken() {
        if (hasPrevious()) {
            return tokens[--tokenPos];
        } else {
            return null;
        }
    }

    /**
     * Gets a copy of the full token list.
     * 
     * @return the tokens as a String array
     */
    public String[] getAllTokens() {
        tokenize();
        return (String[]) tokens.clone();
    }

    /**
     * Resets this tokenizer, forgetting all parsing and iteration already completed.
     * <p>
     * This method allows the same tokenizer to be reused for the same String.
     */
    public void reset() {
        tokenPos = 0;
        tokens = null;
    }

    /**
     * Reset this tokenizer, giving it a new input string to parse.
     * In this manner you can re-use a tokenizer with the same settings
     * on multiple input lines.
     * 
     * @param input  the new string to tokenize
     */
    public void reset(String input) {
        reset();
        this.text = input;
        chars = input.toCharArray();  // no clone as toCharArray() clones
    }

    /**
     * Reset this tokenizer, giving it a new input string to parse.
     * In this manner you can re-use a tokenizer with the same settings
     * on multiple input lines.
     * 
     * @param input  the new character array to tokenize, cloned
     */
    public void reset(char [] input) {
        reset();
        this.text = null;
        chars = (char[]) input.clone();
    }

    // ListIterator
    //-----------------------------------------------------------------------
    /**
     * Checks whether there are any more tokens.
     * 
     * @return true if there are more tokens
     */
    public boolean hasNext() {
        tokenize();
        return (tokenPos < tokens.length);
    }

    /**
     * Gets the next token. This method is equivalent to {@link #nextToken()}.
     * 
     * @return the next String token
     */
    public Object next() {
        return nextToken();
    }

    /**
     * Gets the index of the next token to return.
     * 
     * @return the next token index
     */
    public int nextIndex() {
        return tokenPos;
    }

    /**
     * Checks whether there are any previous tokens that can be iterated to.
     * 
     * @return true if there are previous tokens
     */
    public boolean hasPrevious() {
        tokenize();
        return (tokenPos > 0);
    }

    /**
     * Gets the token previous to the last returned token.
     * 
     * @return the previous token
     */
    public Object previous() {
        return previousToken();
    }

    /**
     * Gets the index of the previous token.
     * 
     * @return the previous token index
     */
    public int previousIndex() {
        return (tokenPos - 1);
    }

    /**
     * Unsupported ListIterator operation.
     *
     * @throws UnsupportedOperationException always
     */
    public void remove() {
        throw new UnsupportedOperationException("remove() is unsupported");
    }

    /**
     * Unsupported ListIterator operation.
     *
     * @throws UnsupportedOperationException always
     */
    public void set(Object obj) {
        throw new UnsupportedOperationException("set() is unsupported");
    }

    /**
     * Unsupported ListIterator operation.
     *
     * @throws UnsupportedOperationException always
     */
    public void add(Object obj) {
        throw new UnsupportedOperationException("add() is unsupported");
    }

    // Implementation
    //-----------------------------------------------------------------------
    /**
     * Performs the tokenization if it hasn't already been done.
     */
    private void tokenize() {
        if (tokens == null) {
            this.tokens = readTokens();
        }
    }

    /**
     * Read all the tokens.
     */
    private String[] readTokens() {
        int len = chars.length;
        char cbuf[] = new char[len];
        StringBuffer token = new StringBuffer();
        int start = 0;
        List tokens = new ArrayList();
        String tok = null;

        // Keep going until we run out of characters
        while (start < len) {
            // read the next token
            start = readNextToken(start, cbuf, token);
            tok = token.toString();

            // Add the token, following the rules
            // in this object
            addToken(tokens, tok);

            // Reset the string buffer to zero length
            token.setLength(0);

            // Handle the special case where the very last
            // character is a delimiter, in which case, we
            // need another empty string
            if (start == len && delim.isMatch(chars[start - 1])) {
                // Add the token, following the rules
                // in this object
                addToken(tokens, new String());
            }
        }

        return (String[]) tokens.toArray(new String[tokens.size()]);
    }

    /**
     * Adds a token to a list, paying attention to the parameters we've set.
     * 
     * @param list  the list to add to
     * @param tok  the token to add
     */
    private void addToken(List list, String tok) {
        if (tok == null || tok.length() == 0) {
            if (ignoreEmptyTokens) {
                return;
            }
            if (emptyAsNull) {
                tok = null;
            }
        }
        list.add(tok);
    }

    /**
     * Reads character by character through the String to get the next token.
     * 
     * @param start  the first character of field
     * @param cbuf  a character buffer for temporary computations (so we
     *  don't have to keep recreating one)
     * @param token  a StringBuffer where the output token will go
     * @return the starting position of the next field (the character
     *  immediately after the delimiter, or if end of string found,
     *  then the length of string
     */
    private int readNextToken(int start, char cbuf[], StringBuffer token) {
        token.setLength(0);
        int len = chars.length;

        // Skip all leading whitespace, unless it is the
        // field delimiter or the quote character
        int current = start;
        while (current < len &&
                ignored.isMatch(chars[current]) &&
                !delim.isMatch(chars[current]) &&
                !quote.isMatch(chars[current])) {
            current++;
        }

        start = current;

        // Read the token depending on what the first
        // character is like
        if (delim.isMatch(chars[start])) {
            start = readEmpty(start, token);
        } else if (quote.isMatch(chars[start])) {
            start = readQuoted(start, cbuf, token);
        } else {
            start = readUnquoted(start, token);
        }

        return start;
    }

    /**
     * Reads a quoted string token.
     * 
     * @param start The first character of field (this will be the quote
     *              character)
     * @param cbuf A character buffer for temporary computations (so we
     *             don't have to keep recreating one)
     * @param token A StringBuffer where the output token will go.
     * @return The starting position of the next field (the character
     *         immediately after the delimiter, or if end of string found,
     *         then the length of string.
     */
    private int readQuoted(int start, char cbuf[], StringBuffer token) {
        // Loop until we've found the end of the quoted
        // string or the end of the input
        int cbufcnt = 0;
        int nd = start + 1;
        boolean done = false;
        boolean quoting = true;
        int len = chars.length;

        while (nd < len && !done) {
            // Quoting mode can occur several times throughout
            // a given string, so must switch between quoting
            // and non-quoting until we encounter a non-quoted
            // delimiter, or end of string, which inidicates end
            // of token.
            if (quoting) {
                // If we've found a quote character, see if it's
                // followed by a second quote.  If so, then we need
                // to actually put the quote character into the token
                // rather than end the token.
                if (quote.isMatch(chars[nd]) &&
                        nd + 1 < len &&
                        chars[nd + 1] == chars[nd]) {
                    cbuf[cbufcnt++] = chars[nd];
                    nd++;
                }
                // End the quoting if we get to this condition
                else if (quote.isMatch(chars[nd])) {
                    quoting = false;
                }
                // Otherwise, just put the character into the token
                else {
                    cbuf[cbufcnt++] = chars[nd];
                }
                nd++;
            }
            // If we're not in quoting mode, if we encounter
            // a delimiter, the token is ended.  If we encounter
            // a quote, we start quoting mode, otherwise, just append
            // the character
            else {
                // If we're
                if (delim.isMatch(chars[nd])) {
                    done = true;
                } else {
                    if (quote.isMatch(chars[nd])) {
                        quoting = true;
                    } else {
                        cbuf[cbufcnt++] = chars[nd];
                    }
                    nd++;
                }
            }
        }

        token.append(cbuf, 0, cbufcnt);

        return nd + 1;
    }

    /**
     * Read an unquoted string until a delimiter is found.
     * 
     * @param start  the first character of field
     * @param token  a StringBuffer where the output token will go.
     * @return  the starting position of the next field (the character
     *  immediately after the delimiter, or if end of string found,
     *  then the length of string.
     */
    private int readUnquoted(int start, StringBuffer token) {
        int len = chars.length;
        // Skip ahead until we get to a delimiter character, or
        // the end of the input
        int nd = start + 1;
        while (nd < len && !delim.isMatch(chars[nd])) {
            nd++;
        }

        token.append(chars, start, Math.min(nd, len) - start);

        return nd + 1;
    }

    /**
     * Read an empty string (basically, if a delimiter is found right
     * after another delimiter).
     * 
     * @param start  the first character of field (this will be the delimiter
     *  character)
     * @param token  a StringBuffer where the output token will go.
     * @return The starting position of the next field (the character
     *  immediately after the delimiter, or if end of string found,
     *  then the length of string.
     */
    private int readEmpty(int start, StringBuffer token) {
        token.setLength(0);
        return start + 1;
    }

    // Delimiter
    //-----------------------------------------------------------------------
    /**
     * Gets the field delimiter matcher.
     * 
     * @return the delimiter matcher in use
     */
    public Matcher getDelimiterMatcher() {
        return delim;
    }

    /**
     * Sets the field delimiter matcher.
     * <p>
     * The delimitier is used to separate one token from another.
     * 
     * @param delim  the delimiter matcher to use, null ignored
     */
    public void setDelimiterMatcher(Matcher delim) {
        if (delim != null) {
            this.delim = delim;
        }
    }

    /**
     * Sets the field delimiter character
     * 
     * @param delim  the delimiter character to use
     */
    public void setDelimiterChar(char delim) {
        setDelimiterMatcher(new CharMatcher(delim));
    }

    // Quote
    //-----------------------------------------------------------------------
    /**
     * Gets the quote matcher currently in use.
     * <p>
     * The quote character is used to wrap data between the tokens.
     * This enables delimiters to be entered as data.
     * The default value is '"' (double quote).
     * 
     * @return the quote matcher in use
     */
    public Matcher getQuoteMatcher() {
        return quote;
    }

    /**
     * Set the quote matcher to use.
     * <p>
     * The quote character is used to wrap data between the tokens.
     * This enables delimiters to be entered as data.
     * 
     * @param quote  the quote matcher to use, null ignored
     */
    public void setQuoteMatcher(Matcher quote) {
        if (quote != null) {
            this.quote = quote;
        }
    }

    /**
     * Sets the quote character to use.
     * <p>
     * The quote character is used to wrap data between the tokens.
     * This enables delimiters to be entered as data.
     * 
     * @param quote  the quote character to use
     */
    public void setQuoteChar(char quote) {
        setQuoteMatcher(new CharMatcher(quote));
    }

    // Ignored
    //-----------------------------------------------------------------------
    /**
     * Gets the ignored character matcher.
     * <p>
     * These characters are ignored when parsing the String, unless they are
     * within a quoted region.
     * The default value is space (' ') and all char control characters (32 and less).
     * 
     * @return the ignored matcher in use
     */
    public Matcher getIgnoredMatcher() {
        return ignored;
    }

    /**
     * Set the matcher for characters to ignore.
     * <p>
     * These characters are ignored when parsing the String, unless they are
     * within a quoted region.
     * 
     * @param ignored  the ignored matcher to use, null ignored
     */
    public void setIgnoredMatcher(Matcher ignored) {
        if (ignored != null) {
            this.ignored = ignored;
        }
    }

    /**
     * Set the character to ignore.
     * <p>
     * This character is ignored when parsing the String, unless it is
     * within a quoted region.
     * 
     * @param ignored  the ignored character to use
     */
    public void setIgnoredChar(char ignored) {
        setIgnoredMatcher(new CharMatcher(ignored));
    }

    //-----------------------------------------------------------------------
    /**
     * Gets whether the tokenizer currently returns empty tokens as null.
     * The default for this property is false.
     * 
     * @return true if empty tokens are returned as null
     */
    public boolean isEmptyTokenAsNull() {
        return emptyAsNull;
    }

    /**
     * Sets whether the tokenizer should return empty tokens as null.
     * The default for this property is false.
     * 
     * @param emptyAsNull  whether empty tokens are returned as null
     */
    public void setEmptyTokenAsNull(boolean emptyAsNull) {
        this.emptyAsNull = emptyAsNull;
    }

    //-----------------------------------------------------------------------
    /**
     * Gets whether the tokenizer currently ignores empty tokens.
     * The default for this property is false.
     * 
     * @return true if empty tokens are not returned
     */
    public boolean isIgnoreEmptyTokens() {
        return ignoreEmptyTokens;
    }

    /**
     * Sets whether the tokenizer should ignore and not return empty tokens.
     * The default for this property is false.
     * 
     * @param ignoreEmptyTokens  whether empty tokens are not returned
     */
    public void setIgnoreEmptyTokens(boolean ignoreEmptyTokens) {
        this.ignoreEmptyTokens = ignoreEmptyTokens;
    }

    //-----------------------------------------------------------------------
    /**
     * Gets the String content that the tokenizer is parsing.
     * 
     * @return the string content being parsed
     */
    public String getContent() {
        if (text == null) {
            text = new String(chars);
        }
        return text;
    }
    
    //-----------------------------------------------------------------------
    /**
     * Create a new instance of this Tokenizer.
     * The new instance is reset so that it will be at the start of the token list.
     */
    public Object clone() {
        try {
            Tokenizer cloned = (Tokenizer) super.clone();
            // chars[] does not need additional clone as it is treated as immutable
            cloned.reset();
            return cloned;
            
        } catch (CloneNotSupportedException ex) {
            return null;
        }
    }

    //-----------------------------------------------------------------------    
    /**
     * Class used to define a set of characters for matching purposes.
     */
    public static interface Matcher {
        /**
         * Returns true if the specified character matches.
         * 
         * @param ch  the character to check for
         * @return true if matches
         */
        boolean isMatch(char ch);
    }
    
    //-----------------------------------------------------------------------    
    /**
     * Class used to define a set of characters for matching purposes.
     */
    public static final class CharSetMatcher implements Matcher {
        private char chars[];

        /**
         * Constructor that creates a matcher from a character array.
         * 
         * @param chars  the characters to match, must not be null
         */
        public CharSetMatcher(char chars[]) {
            super();
            this.chars = (char[]) chars.clone();
            Arrays.sort(this.chars);
        }

        /**
         * Constructor that creates a matcher from a String.
         * 
         * @param chars  the characters to match, must not be null
         */
        public CharSetMatcher(String chars) {
            super();
            this.chars = chars.toCharArray();
            Arrays.sort(this.chars);
        }

        /**
         * Gets the characters being matched.
         * 
         * @return the characters being matched
         */
        public char[] getChars() {
            return (char[]) chars.clone();
        }

        /**
         * Returns whether or not the given charatcer matches.
         * 
         * @param ch the character to match.
         * @return whether or not the given charatcer matches.
         */
        public boolean isMatch(char ch) {
            return (Arrays.binarySearch(chars, ch) >= 0);
        }
    }
    
    //-----------------------------------------------------------------------    
    /**
     * Class used to define a character for matching purposes.
     */
    public static final class CharMatcher implements Matcher {
        private char ch;

        /**
         * Constructor that creates a matcher that matches a single character.
         * 
         * @param ch  the character to match
         */
        public CharMatcher(char ch) {
            super();
            this.ch = ch;
        }

        /**
         * Gets the character being matched.
         * 
         * @return the character being matched
         */
        public char getChar() {
            return this.ch;
        }

        /**
         * Returns whether or not the given charatcer matches.
         * 
         * @param ch the character to match.
         * @return whether or not the given charatcer matches.
         */
        public boolean isMatch(char ch) {
            return (this.ch == ch);
        }
    }
    
    //-----------------------------------------------------------------------    
    /**
     * Class used to match no characters.
     */
    static final class NoMatcher implements Matcher {

        NoMatcher() {
            super();
        }

        /**
         * Always returns <code>false</code>.
         * 
         * @param ch the character to match.
         * @return Always returns <code>false</code>.
         */
        public boolean isMatch(char ch) {
            return false;
        }
    }
    
    //-----------------------------------------------------------------------    
    /**
     * Class used to match whitespace as per trim().
     */
    static final class TrimMatcher implements Matcher {

        TrimMatcher() {
            super();
        }

        /**
         * Returns whether or not the given charatcer matches.
         * 
         * @param ch the character to match.
         * @return whether or not the given charatcer matches.
         */
        public boolean isMatch(char ch) {
            return (ch <= 32);
        }
    }
}
