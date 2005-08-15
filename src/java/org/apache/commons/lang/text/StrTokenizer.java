/*
 * Copyright 2003-2005 The Apache Software Foundation.
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
package org.apache.commons.lang.text;

import java.util.ArrayList;
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
 * The processing then strips all the <i>ignored</i> characters from then entire string (this
 * is useful for removing things like carriage returns, and so forth)
 * <p>
 * The processing then strips all the <i>trimmer</i> characters from the ends of the string.
 * <p>
 * The token may also have <i>quotes</i> to mark an area not to be stripped or tokenized.
 * Empty tokens may be removed or returned as null.
 * <pre>
 * "a,b,c"       - Three tokens "a","b","c"   (comma delimiter)
 * " a, b , c "    - Three tokens "a","b","c"   (default CSV processing trims whitespace)
 * "a, ", b ,", c" - Three tokens "a, " , " b ", ", c" (quoted text untouched)
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
 * @since 2.2
 * @version $Id$
 */
public class StrTokenizer implements ListIterator, Cloneable {

    private static final StrTokenizer CSV_TOKENIZER_PROTOTYPE;
    private static final StrTokenizer TSV_TOKENIZER_PROTOTYPE;
    static {
        CSV_TOKENIZER_PROTOTYPE = new StrTokenizer();
        CSV_TOKENIZER_PROTOTYPE.setDelimiterMatcher(StrMatcher.commaMatcher());
        CSV_TOKENIZER_PROTOTYPE.setQuoteMatcher(StrMatcher.doubleQuoteMatcher());
        CSV_TOKENIZER_PROTOTYPE.setIgnoredMatcher(StrMatcher.noneMatcher());
        CSV_TOKENIZER_PROTOTYPE.setTrimmerMatcher(StrMatcher.trimMatcher());
        CSV_TOKENIZER_PROTOTYPE.setEmptyTokenAsNull(false);
        CSV_TOKENIZER_PROTOTYPE.setIgnoreEmptyTokens(false);

        TSV_TOKENIZER_PROTOTYPE = new StrTokenizer();
        TSV_TOKENIZER_PROTOTYPE.setDelimiterMatcher(StrMatcher.tabMatcher());
        TSV_TOKENIZER_PROTOTYPE.setQuoteMatcher(StrMatcher.doubleQuoteMatcher());
        TSV_TOKENIZER_PROTOTYPE.setIgnoredMatcher(StrMatcher.noneMatcher());
        TSV_TOKENIZER_PROTOTYPE.setTrimmerMatcher(StrMatcher.trimMatcher());
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
    private StrMatcher delim = StrMatcher.splitMatcher();
    /** The quote matcher */
    private StrMatcher quote = StrMatcher.noneMatcher();
    /** The ignored matcher */
    private StrMatcher ignored = StrMatcher.noneMatcher();
    /** The trimmer matcher */
    private StrMatcher trimmer = StrMatcher.noneMatcher();

    /** Whether to return empty tokens as null */
    private boolean emptyAsNull = false;
    /** Whether to ignore empty tokens */
    private boolean ignoreEmptyTokens = true;

    //-----------------------------------------------------------------------

    /**
     * Returns a clone of <code>CSV_TOKENIZER_PROTOTYPE</code>.
     * 
     * @return a clone of <code>CSV_TOKENIZER_PROTOTYPE</code>.
     */
    private static StrTokenizer getCSVClone() {
        return (StrTokenizer) CSV_TOKENIZER_PROTOTYPE.clone();
    }

    /**
     * Gets a new tokenizer instance which parses Comma Seperated Value strings
     * initializing it with the given input.  The default for CSV processing
     * will be trim whitespace from both ends (which can be overriden with
     * the setTrimmer method).
     * <p>
     * You must call a "reset" method to set the string which you want to parse.
     * @return a new tokenizer instance which parses Comma Seperated Value strings
     */
    public static StrTokenizer getCSVInstance() {
        return getCSVClone();
    }

    /**
     * Gets a new tokenizer instance which parses Comma Seperated Value strings
     * initializing it with the given input.  The default for CSV processing
     * will be trim whitespace from both ends (which can be overriden with
     * the setTrimmer method).
     *
     * @param input  the text to parse
     * @return a new tokenizer instance which parses Comma Seperated Value strings
     */
    public static StrTokenizer getCSVInstance(String input) {
        StrTokenizer tok = getCSVClone();
        tok.reset(input);
        return tok;
    }

    /**
     * Gets a new tokenizer instance which parses Comma Seperated Value strings
     * initializing it with the given input.  The default for CSV processing
     * will be trim whitespace from both ends (which can be overriden with
     * the setTrimmer method).
     *
     * @param input  the text to parse
     * @return a new tokenizer instance which parses Comma Seperated Value strings
     */
    public static StrTokenizer getCSVInstance(char[] input) {
        StrTokenizer tok = getCSVClone();
        tok.reset(input);
        return tok;
    }

    /**
     * Returns a clone of <code>TSV_TOKENIZER_PROTOTYPE</code>.
     * 
     * @return a clone of <code>TSV_TOKENIZER_PROTOTYPE</code>.
     */
    private static StrTokenizer getTSVClone() {
        return (StrTokenizer) TSV_TOKENIZER_PROTOTYPE.clone();
    }


    /**
     * Gets a new tokenizer instance which parses Tab Seperated Value strings.
     * The default for CSV processing will be trim whitespace from both ends
     * (which can be overriden with the setTrimmer method).
     * <p>
     * You must call a "reset" method to set the string which you want to parse.
     * @return a new tokenizer instance which parses Tab Seperated Value strings.
     */
    public static StrTokenizer getTSVInstance() {
        return getTSVClone();
    }

    /**
     * Gets a new tokenizer instance which parses Tab Seperated Value strings.
     * The default for CSV processing will be trim whitespace from both ends
     * (which can be overriden with the setTrimmer method).
     * @param input  the string to parse
     * @return a new tokenizer instance which parses Tab Seperated Value strings.
     */
    public static StrTokenizer getTSVInstance(String input) {
        StrTokenizer tok = getTSVClone();
        tok.reset(input);
        return tok;
    }

    /**
     * Gets a new tokenizer instance which parses Tab Seperated Value strings.
     * The default for CSV processing will be trim whitespace from both ends
     * (which can be overriden with the setTrimmer method).
     * @param input  the string to parse
     * @return a new tokenizer instance which parses Tab Seperated Value strings.
     */
    public static StrTokenizer getTSVInstance(char[] input) {
        StrTokenizer tok = getTSVClone();
        tok.reset(input);
        return tok;
    }

    //-----------------------------------------------------------------------
    /**
     * Constructs a tokenizer splitting on space, tab, newline and formfeed
     * as per StringTokenizer, but with no text to tokenize.
     * <p>
     * This constructor is normally used with {@link #reset(String)}.
     */
    public StrTokenizer() {
        super();
        this.text = "";
        this.chars = new char[0];
    }

    /**
     * Constructs a tokenizer splitting on space, tab, newline and formfeed
     * as per StringTokenizer.
     *
     * @param input  the string which is to be parsed
     */
    public StrTokenizer(String input) {
        super();
        this.text = input;
        this.chars = input.toCharArray();  // no clone as toCharArray() clones
    }

    /**
     * Constructs a tokenizer splitting on the specified delimiter character.
     *
     * @param input  the string which is to be parsed
     * @param delim  the field delimiter character
     */
    public StrTokenizer(String input, char delim) {
        this(input);
        setDelimiterChar(delim);
    }

    /**
     * Constructs a tokenizer splitting on the specified delimiter string.
     *
     * @param input  the string which is to be parsed
     * @param delim  the field delimiter string
     */
    public StrTokenizer(String input, String delim) {
        this(input);
        setDelimiterString(delim);
    }

    /**
     * Constructs a tokenizer splitting using the specified delimiter matcher.
     *
     * @param input  the string which is to be parsed
     * @param delim  the field delimiter matcher
     */
    public StrTokenizer(String input, StrMatcher delim) {
        this(input);
        setDelimiterMatcher(delim);
    }

    /**
     * Constructs a tokenizer splitting on the specified delimiter character
     * and handling quotes using the specified quote character.
     *
     * @param input  the string which is to be parsed
     * @param delim  the field delimiter character
     * @param quote  the field quoted string character
     */
    public StrTokenizer(String input, char delim, char quote) {
        this(input, delim);
        setQuoteChar(quote);
    }

    /**
     * Constructs a tokenizer splitting using the specified delimiter matcher
     * and handling quotes using the specified quote matcher.
     *
     * @param input  the string which is to be parsed
     * @param delim  the field delimiter matcher
     * @param quote  the field quoted string matcher
     */
    public StrTokenizer(String input, StrMatcher delim, StrMatcher quote) {
        this(input, delim);
        setQuoteMatcher(quote);
    }

    /**
     * Constructs a tokenizer splitting on space, tab, newline and formfeed
     * as per StringTokenizer.
     *
     * @param input  the string which is to be parsed, cloned
     */
    public StrTokenizer(char[] input) {
        super();
        this.text = null;
        this.chars = (char[]) input.clone();
    }

    /**
     * Constructs a tokenizer splitting on the specified character.
     *
     * @param input  the string which is to be parsed, cloned
     * @param delim the field delimiter character
     */
    public StrTokenizer(char[] input, char delim) {
        this(input);
        setDelimiterChar(delim);
    }

    /**
     * Constructs a tokenizer splitting on the specified string.
     *
     * @param input  the string which is to be parsed, cloned
     * @param delim the field delimiter string
     */
    public StrTokenizer(char[] input, String delim) {
        this(input);
        setDelimiterString(delim);
    }

    /**
     * Constructs a tokenizer splitting using the specified delimiter matcher.
     *
     * @param input  the string which is to be parsed, cloned
     * @param delim  the field delimiter matcher
     */
    public StrTokenizer(char[] input, StrMatcher delim) {
        this(input);
        setDelimiterMatcher(delim);
    }

    /**
     * Constructs a tokenizer splitting on the specified delimiter character
     * and handling quotes using the specified quote character.
     *
     * @param input  the string which is to be parsed, cloned
     * @param delim  the field delimiter character
     * @param quote  the field quoted string character
     */
    public StrTokenizer(char[] input, char delim, char quote) {
        this(input, delim);
        setQuoteChar(quote);
    }

    /**
     * Constructs a tokenizer splitting using the specified delimiter matcher
     * and handling quotes using the specified quote matcher.
     *
     * @param input  the string which is to be parsed, cloned
     * @param delim  the field delimiter character
     * @param quote  the field quoted string character
     */
    public StrTokenizer(char[] input, StrMatcher delim, StrMatcher quote) {
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
        }
        return null;
    }

    /**
     * Gets the previous token from the String.
     *
     * @return the previous sequential token, or null when no more tokens are found
     */
    public String previousToken() {
        if (hasPrevious()) {
            return tokens[--tokenPos];
        }
        return null;
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
        return tokenPos < tokens.length;
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
        return tokenPos > 0;
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
        return tokenPos - 1;
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
     * @param obj this parameter ignored.
     * @throws UnsupportedOperationException always
     */
    public void set(Object obj) {
        throw new UnsupportedOperationException("set() is unsupported");
    }

    /**
     * Unsupported ListIterator operation.
     * @param obj this parameter ignored.
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
     * 
     * @return array containing the tokens.
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
            if (start == len && delim.isMatch(chars, start - 1, 0, len) == 1) {
                // Add the token, following the rules
                // in this object
                addToken(tokens, "");
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
        int ignoreLen = 0;
        int delimLen = 0;
        int quoteLen = 0;
        while (start < len &&
                (ignoreLen = ignored.isMatch(chars, start, 0, len)) >= 1 &&
                (delimLen = delim.isMatch(chars, start, 0, len)) < 1 &&
                (quoteLen = quote.isMatch(chars, start, 0, len)) < 1) {
            start += ignoreLen;
        }

        if (start >= len) {
            return start;
        } else {
            // lengths not setup
            if ((delimLen = delim.isMatch(chars, start, 0, len)) >= 1) {
                start += delimLen;
            } else if ((quoteLen = quote.isMatch(chars, start, 0, len)) >= 1) {
                start = readQuoted(start + quoteLen, cbuf, token);
            } else {
                start = readUnquoted(start, token);
            }
        }
//
//            // lengths not setup
//            if ((delimLen = delim.isMatch(chars, start)) >= 1) {
//                start += delimLen;
//            } else if ((quoteLen = quote.isMatch(chars, start)) >= 1) {
//                start = readQuoted(start + quoteLen, cbuf, token);
//            } else {
//                start = readUnquoted(start, token);
//            }
//        } else {
//            if (delimLen > 0) {
//                start += delimLen;
//            } else if (quoteLen >= 1) {
//                start = readQuoted(start + quoteLen, cbuf, token);
//            } else {
//                start = readUnquoted(start, token);
//            }
//        }

        return start;
    }

    /**
     * Reads a quoted string token.
     *
     * @param start The first character of field, immediately after any quote
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
        int pos = start;
        boolean done = false;
        boolean quoting = true;
        int len = chars.length;
        int delimLen = 0;
        int quoteLen = 0;

        while (pos < len && !done) {
            // Quoting mode can occur several times throughout
            // a given string, so must switch between quoting
            // and non-quoting until we encounter a non-quoted
            // delimiter, or end of string, which indicates end
            // of token.
            if (quoting) {
                // If we've found a quote character, see if it's
                // followed by a second quote.  If so, then we need
                // to actually put the quote character into the token
                // rather than end the token.
                if ((quoteLen = quote.isMatch(chars, pos, 0, len)) >= 1) {
                    if (pos + 1 < len && chars[pos + 1] == chars[pos]) {
                        cbuf[cbufcnt++] = chars[pos];
                        pos += 2;
                    } else {
                        // End the quoting if we get to this condition
                        quoting = false;
                        pos += quoteLen;
                    }
                } else {
                    // Otherwise, just put the character into the token
                    cbuf[cbufcnt++] = chars[pos];
                    pos++;
                }
            }
            // If we're not in quoting mode, if we encounter
            // a delimiter, the token is ended.  If we encounter
            // a quote, we start quoting mode, otherwise, just append
            // the character
            else {
                // If we're
                if ((delimLen = delim.isMatch(chars, pos, 0, len)) >= 1) {
                    done = true;
                } else {
                    if ((quoteLen = quote.isMatch(chars, pos, 0, len)) >= 1) {
                        quoting = true;
                        pos += quoteLen;
                    } else {
                        cbuf[cbufcnt++] = chars[pos];
                        pos++;
                    }
                }
            }
        }

        token.append(cbuf, 0, cbufcnt);

        return pos + delimLen;
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
        // Find delimiter or end of string
        char[] chars = this.chars;
        int len = chars.length;
        int pos = start;
        int delimLen = 0;
        while (pos < len && (delimLen = delim.isMatch(chars, pos, 0, len)) < 1) {
            pos++;
        }

        /* Trim string based on the trimmer matcher */
        while (trimmer.isMatch(chars, start, 0, len) > 0) {
            start++;
        }

        int length = Math.min(pos, len) - start;

        while (trimmer.isMatch(chars, start + length - 1, 0, len) > 0) {
            length--;
        }

        for (int i=0;i<length;i++) {
            if (ignored.isMatch(chars, start + i, 0, len) == 0) {
                token.append(chars[start + i]);
            }
        }


        return pos + delimLen;
    }

    // Delimiter
    //-----------------------------------------------------------------------
    /**
     * Gets the field delimiter matcher.
     *
     * @return the delimiter matcher in use
     */
    public StrMatcher getDelimiterMatcher() {
        return delim;
    }

    /**
     * Sets the field delimiter matcher.
     * <p>
     * The delimitier is used to separate one token from another.
     *
     * @param delim  the delimiter matcher to use
     */
    public void setDelimiterMatcher(StrMatcher delim) {
        if (delim == null) {
            this.delim = StrMatcher.noneMatcher();
        } else {
            this.delim = delim;
        }
    }

    /**
     * Sets the field delimiter character
     *
     * @param delim  the delimiter character to use
     */
    public void setDelimiterChar(char delim) {
        setDelimiterMatcher(StrMatcher.charMatcher(delim));
    }

    /**
     * Sets the field delimiter character
     *
     * @param delim  the delimiter character to use
     */
    public void setDelimiterString(String delim) {
        setDelimiterMatcher(StrMatcher.stringMatcher(delim));
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
    public StrMatcher getQuoteMatcher() {
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
    public void setQuoteMatcher(StrMatcher quote) {
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
        setQuoteMatcher(StrMatcher.charMatcher(quote));
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
    public StrMatcher getIgnoredMatcher() {
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
    public void setIgnoredMatcher(StrMatcher ignored) {
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
        setIgnoredMatcher(StrMatcher.charMatcher(ignored));
    }

    // Trimmer
    //-----------------------------------------------------------------------
    /**
     * Gets the trimmer character matcher.
     * <p>
     * These characters are trimmed off the beginning and ending of an unquoted string.
     * The default value is space (' ') and all char control characters (32 and less).
     *
     * @return the trimmer matcher in use
     */
    public StrMatcher getTrimmerMatcher() {
        return trimmer;
    }

    /**
     * Set the matcher for characters to trim off the beginning and end of an
     * unquoted string.
     *
     * @param trimmer  the trimmer matcher to use, null ignored
     */
    public void setTrimmerMatcher(StrMatcher trimmer) {
        if (trimmer != null) {
            this.trimmer = trimmer;
        }
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
     * Creates a new instance of this Tokenizer.
     * The new instance is reset so that it will be at the start of the token list.
     * @return a new instance of this Tokenizer which has been reset.
     */
    public Object clone() {
        try {
            StrTokenizer cloned = (StrTokenizer) super.clone();
            // chars[] does not need additional clone as it is treated as immutable
            cloned.reset();
            return cloned;

        } catch (CloneNotSupportedException ex) {
            return null;
        }
    }

}
