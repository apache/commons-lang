/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002-2003 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowledgement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowledgement may appear in the software itself,
 *    if and wherever such third-party acknowledgements normally appear.
 *
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
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
 * This class can split a String into many smaller strings. It aims to do a
 * similar job to java util StringTokenizer, however it offers much more
 * control and flexibility.
 * <p>
 * The input String is split into a number of <i>tokens</i>.
 * Each token is separated from the next String by a <i>delimiter</i>.
 * One or more delimiter characters must be specified.
 * <p>
 * The processing then strips all the <i>ignored</i> characters from each side of the token.
 * The token may also have <i>quotes</i> to mark an area not to be stripped or tokenized.
 * Empty tokens may be removed or returned as null.
 * <pre>
 * "a,b,c"       - Three tokens "a","b","c" (comma delimiter)
 * "a, b , c"    - Three tokens "a","b","c" (ignored space characters stripped)
 * "a, " b ", c" - Three tokens "a"," b ","c" (quoted text untouched)
 * </pre>
 * <p>
 * By default, this tokenizer has the following properties:
 * <pre>
 * Property                     Default
 * ---------                    -------
 * delimiter                    ,  (comma)
 * quote                        "  (double quote)
 * ignored                      char &lt;= 32 (as per trim)
 * emptyTokenAsNull             false
 * ignoreEmptyTokens            false
 * </pre>
 *
 * @author Matthew Inger
 * @author Stephen Colebourne
 */
public class Tokenizer implements ListIterator {
    // TODO: Constructors
    // TODO: Tests
    // TODO: Static factories CSV/StringTokenizer
    
    /**
     * A Matcher which matches the comma character.
     * Best used for <code>delimiter</code>.
     */
    public static final Matcher COMMA_MATCHER = new CharMatcher(',');
    /**
     * A Matcher which matches the double quote character.
     * Best used for <code>quote</code>.
     */
    public static final Matcher DOUBLE_QUOTE_MATCHER = new CharMatcher('"');
    /**
     * A Matcher which matches the String trim() whitespace characters.
     * Best used for <code>ignored</code>.
     */
    public static final Matcher SPACES_MATCHER = new TrimMatcher();
    /**
     * A Matcher that matches no characters. Don't use this for delimiters!
     * Best used for <code>ignored</code>.
     */
    public static final Matcher NONE_MATCHER = new NoMatcher();

    /** The text to work on */
    private char chars[];
    /** The parsed tokens */
    private String tokens[];
    /** The current iteration position */
    private int tokenPos;

    /** The delimiter matcher */
    private Matcher delim = COMMA_MATCHER;
    /** The quote matcher */
    private Matcher quote = DOUBLE_QUOTE_MATCHER;
    /** The ignored matcher */
    private Matcher ignored = SPACES_MATCHER;
    /** Whether to return empty tokens as null */
    private boolean emptyAsNull = false;
    /** Whether to ignore empty tokens */
    private boolean ignoreEmptyTokens = false;

    //-----------------------------------------------------------------------
    /**
     * Constructor.
     * 
     * @param input  the string which is to be parsed
     */
    public Tokenizer(String input) {
        this(input.toCharArray());
    }

    /**
     * Constructor.
     * 
     * @param input  the string which is to be parsed
     * @param delim  the field delimiter character
     */
    public Tokenizer(String input, char delim) {
        this(input.toCharArray(), delim);
    }

    /**
     * Constructor.
     * 
     * @param input  the string which is to be parsed
     * @param delim  the field delimiter character
     */
    public Tokenizer(String input, CharSetMatcher delim) {
        this(input.toCharArray(), delim);
    }

    /**
     * Constructor.
     * 
     * @param input  the string which is to be parsed
     * @param delim  the field delimiter character
     * @param quote  the field quoted string character
     */
    public Tokenizer(String input, char delim, char quote) {
        this(input.toCharArray(), delim, quote);
    }

    /**
     * Constructor.
     * 
     * @param input  the string which is to be parsed
     * @param delim  the field delimiter character
     * @param quote  the field quoted string character
     */
    public Tokenizer(String input, CharSetMatcher delim, CharSetMatcher quote) {
        this(input.toCharArray(), delim, quote);
    }

    /**
     * Constructor.
     * 
     * @param input  the string which is to be parsed
     */
    public Tokenizer(char[] input) {
        super();
        this.chars = (char[]) input.clone();
        this.tokenPos = 0;
    }

    /**
     * Constructor.
     * 
     * @param input  the string which is to be parsed
     * @param delim the field delimiter character
     */
    public Tokenizer(char[] input, char delim) {
        this(input);
        setDelimiterChar(delim);
    }

    /**
     * Constructor.
     * 
     * @param input  the string which is to be parsed
     * @param delim  the field delimiter character
     */
    public Tokenizer(char[] input, CharSetMatcher delim) {
        this(input);
        setDelimiterMatcher(delim);
    }

    /**
     * Constructor.
     * 
     * @param input  the string which is to be parsed
     * @param delim  the field delimiter character
     * @param quote  the field quoted string character
     */
    public Tokenizer(char[] input, char delim, char quote) {
        this(input, delim);
        setQuoteChar(quote);
    }

    /**
     * Constructor.
     * 
     * @param input  the string which is to be parsed
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

        // skip all leading whitespace, unless it is the
        // field delimiter or the quote character
        while (start < len &&
                ignored.isMatch(chars[start]) &&
                !delim.isMatch(chars[start]) &&
                !quote.isMatch(chars[start])) {
            start++;
        }

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
     * @param quote  the ignored character to use
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
     * @return emptyAsNull  whether empty tokens are returned as null
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
     * @return ignoreEmptyTokens  whether empty tokens are not returned
     */
    public void setIgnoreEmptyTokens(boolean ignoreEmptyTokens) {
        this.ignoreEmptyTokens = ignoreEmptyTokens;
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
            this(chars.toCharArray());
        }

        /**
         * Gets the characters being matched.
         * 
         * @return the characters being matched
         */
        public char[] getChars() {
            return (char[]) chars.clone();
        }

        public boolean isMatch(char c) {
            return (Arrays.binarySearch(chars, c) >= 0);
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
         * @param chars  the character to match
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

        public boolean isMatch(char ch) {
            return (ch <= 32);
        }
    }
}
