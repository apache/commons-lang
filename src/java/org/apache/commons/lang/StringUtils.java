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
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
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
import java.util.Iterator;
import java.util.List;

import org.apache.commons.lang.math.NumberUtils;

/**
 * <p>Common <code>String</code> manipulation routines that are 
 * <code>null</code> safe.</p>
 *
 * <p>The <code>StringUtils</code> class defines certain words related to
 * String handling.</p>
 * 
 * <ul>
 *  <li>null - <code>null</code>
 *  <li>empty - a zero-length string (<code>""</code>)
 *  <li>space - the space character (<code>' '</code>)(32)
 *  <li>whitespace - the characters defined by {@link Character#isWhitespace(char)}
 * </ul>
 * 
 * <p><code>StringUtils</code> handles <code>null</code> input Strings quietly.
 * That is to say that a <code>null</code> input will return <code>null</code>.
 * Where a <code>boolean</code> or <code>int</code> is being returned exact
 * details vary by method.</p>
 * 
 * <p>A side effect of the <code>null</code> handling is that a 
 * NullPointerException should be considered a bug in <code>StringUtils</code>.
 * (Except for deprecated methods).</p>
 *
 * @author <a href="http://jakarta.apache.org/turbine/">Apache Jakarta Turbine</a>
 * @author GenerationJavaCore
 * @author <a href="mailto:jon@latchkey.com">Jon S. Stevens</a>
 * @author <a href="mailto:dlr@finemaltcoding.com">Daniel Rall</a>
 * @author <a href="mailto:gcoladonato@yahoo.com">Greg Coladonato</a>
 * @author <a href="mailto:bayard@generationjava.com">Henri Yandell</a>
 * @author <a href="mailto:ed@apache.org">Ed Korthof</a>
 * @author <a href="mailto:rand_mcneely@yahoo.com">Rand McNeely</a>
 * @author Stephen Colebourne
 * @author <a href="mailto:fredrik@westermarck.com">Fredrik Westermarck</a>
 * @author Holger Krauth
 * @author <a href="mailto:alex@purpletech.com">Alexander Day Chaffee</a>
 * @author <a href="mailto:hps@intermeta.de">Henning P. Schmiedehausen</a>
 * @author Arun Mammen Thomas
 * @author <a href="mailto:ggregory@seagullsw.com">Gary Gregory</a>
 * @since 1.0
 * @version $Id: StringUtils.java,v 1.65 2003/07/19 18:09:33 scolebourne Exp $
 */
public class StringUtils {
    // Performance testing notes (JDK 1.4, Jul03, scolebourne)
    // Whitespace:
    // Character.isWhitespace() is faster than WHITESPACE.indexOf()
    // where WHITESPACE is a string of all whitespace characters
    // 
    // Character access:
    // String.charAt(n) versus toCharArray(), then array[n]
    // String.charAt(n) is about 15% worse for a 10K string
    // They are about equal for a length 50 string
    // String.charAt(n) is about 4 times better for a length 3 string
    // String.charAt(n) is best bet overall
    //
    // Append:
    // String.concat about twice as fast as StringBuffer.append
    // (not sure who tested this)
    
    /**
     * <p>The maximum size to which the padding constant(s) can expand.</p>
     */
    private static int PAD_LIMIT = 8192;

    /**
     * <p>A <code>String</code> containing all space characters (' ').</p>
     *
     * <p>Used for efficient space padding.  The length of the String expands as needed.</p>
     */
    private static String spaces = new String(" ");

    /**
     * <p>An array of <code>String</code>s used for padding.</p>
     *
     * <p>Used for efficient space padding. The length of each String expands as needed.</p>
     */
    private final static String[] padding = new String[Character.MAX_VALUE];

    /**
     * <p><code>StringUtils<code> instances should NOT be constructed in
     * standard programming. Instead, the class should be used as
     * <code>StringUtils.trim(" foo ");</code>.</p>
     *
     * <p>This constructor is public to permit tools that require a JavaBean
     * instance to operate.</p>
     */
    public StringUtils() {
    }

    // Empty
    //-----------------------------------------------------------------------

    /**
     * <p>Removes control characters (char &lt;= 32) from both
     * ends of this String, handling <code>null</code> by returning
     * an empty String ("").</p>
     * 
     * <pre>
     * StringUtils.clean(null)          = ""
     * StringUtils.clean("abc")         = "abc"
     * StringUtils.clean("    abc    ") = "abc"
     * StringUtils.clean("     ")       = ""
     * StringUtils.clean("")            = ""
     * </pre>
     *
     * @see java.lang.String#trim()
     * @param str  the String to clean, may be null
     * @return the trimmed text, never <code>null</code>
     * @deprecated Use the clearer named {@link #trimToEmpty(String)}.
     *             Method will be removed in Commons Lang 3.0.
     */
    public static String clean(String str) {
        return (str == null ? "" : str.trim());
    }

    /**
     * <p>Removes control characters (char &lt;= 32) from both
     * ends of this String, handling <code>null</code> by returning
     * <code>null</code>.</p>
     * 
     * <p>The String is trimmed using {@link String#trim()}.
     * Trim removes start and end characters &lt;= 32.</p>
     * 
     * <p>To trim your choice of characters, use the
     * {@link #strip(String, String)} methods.</p>
     * 
     * <pre>
     * StringUtils.trim(null)          = null
     * StringUtils.trim("abc")         = "abc"
     * StringUtils.trim("    abc    ") = "abc"
     * StringUtils.trim("     ")       = ""
     * StringUtils.trim("")            = ""
     * </pre>
     *
     * @see java.lang.String#trim()
     * @param str  the String to be trimmed, may be null
     * @return the trimmed string, <code>null</code> if null String input
     */
    public static String trim(String str) {
        return (str == null ? null : str.trim());
    }

    /** 
     * <p>Removes control characters (char &lt;= 32) from both  
     * ends of this String returning <code>null</code> if the String is 
     * empty ("") after the trim or if it is <code>null</code>.
     * 
     * <p>The String is trimmed using {@link String#trim()}.
     * Trim removes start and end characters &lt;= 32.</p>
     * 
     * <pre>
     * StringUtils.trimToNull(null)          = null
     * StringUtils.trimToNull("abc")         = "abc"
     * StringUtils.trimToNull("    abc    ") = "abc"
     * StringUtils.trimToNull("     ")       = null
     * StringUtils.trimToNull("")            = null
     * </pre>
     *  
     * @see java.lang.String#trim()
     * @param str  the String to be trimmed, may be null
     * @return the trimmed String, 
     *  <code>null</code> if only chars &lt;= 32, empty or null String input
     */
    public static String trimToNull(String str) {
        String ts = trim(str);
        return (ts == null || ts.length() == 0 ? null : ts);
    }

    /** 
     * <p>Removes control characters (char &lt;= 32) from both 
     * ends of this String returning an empty String ("") if the String
     * is empty ("") after the trim or if it is <code>null</code>.
     * 
     * <p>The String is trimmed using {@link String#trim()}.
     * Trim removes start and end characters &lt;= 32.</p>
     * 
     * <pre>
     * StringUtils.trimToEmpty(null)          = ""
     * StringUtils.trimToEmpty("abc")         = "abc"
     * StringUtils.trimToEmpty("    abc    ") = "abc"
     * StringUtils.trimToEmpty("     ")       = ""
     * StringUtils.trimToEmpty("")            = ""
     * </pre>
     *  
     * @see java.lang.String#trim()
     * @param str  the String to be trimmed, may be null
     * @return the trimmed String, or an empty String if <code>null</code> input
     */
    public static String trimToEmpty(String str) {
        return (str == null ? "" : str.trim());
    }
    
    // Delete
    //-----------------------------------------------------------------------

    /**
     * <p>Deletes all 'space' characters from a String as defined by
     * {@link Character#isSpace(char)}.</p>
     * 
     * <p>This is the only StringUtils method that uses the 
     * <code>isSpace</code> definition. You are advised to use
     * {@link #deleteWhitespace(String)} instead as whitespace is much
     * better localized.</p>
     *
     * <pre>
     * StringUtils.deleteSpaces(null)           = null
     * StringUtils.deleteSpaces("abc")          = "abc"
     * StringUtils.deleteSpaces(" \t  abc \n ") = "abc"
     * StringUtils.deleteSpaces("ab  c")        = "abc"
     * StringUtils.deleteSpaces("a\nb\tc     ") = "abc"
     * </pre>
     *  
     * <p>Spaces are defined as <code>{' ', '\t', '\r', '\n', '\b'}</code>
     * in line with the deprecated <code>isSpace</code> method.</p>
     *
     * @param str  the String to delete spaces from, may be null
     * @return the String without 'spaces', <code>null</code> if null String input
     */
    public static String deleteSpaces(String str) {
        if (str == null) {
            return null;
        }
        return CharSetUtils.delete(str, " \t\r\n\b");
    }

    /**
     * <p>Deletes all whitespaces from a String as defined by
     * {@link Character#isWhitespace(char)}.</p>
     *
     * <pre>
     * StringUtils.deleteWhitespace(null)        = null
     * StringUtils.deleteWhitespace("abc")       = "abc"
     * StringUtils.deleteWhitespace("   abc  ")  = "abc"
     * </pre>
     *  
     * @param str  the String to delete whitespace from, may be null
     * @return the String without whitespaces, <code>null</code> if null String input
     */
    public static String deleteWhitespace(String str) {
        if (str == null) {
            return null;
        }
        int sz = str.length();
        StringBuffer buffer = new StringBuffer(sz);
        for (int i = 0; i < sz; i++) {
            if (!Character.isWhitespace(str.charAt(i))) {
                buffer.append(str.charAt(i));
            }
        }
        return buffer.toString();
    }

    // Empty checks
    //-----------------------------------------------------------------------

    /**
     * <p>Checks if a String is empty ("").
     * <code>null</code> returns <code>false</code></p>
     * 
     * <pre>
     * StringUtils.isEmpty(null)      = false
     * StringUtils.isEmpty("")        = true
     * StringUtils.isEmpty(" ")       = false
     * StringUtils.isEmpty("bob")     = false
     * StringUtils.isEmpty("  bob  ") = false
     * </pre>
     *
     * <p>NOTE: This method changed in version 2.0.
     * It no longer trims the String, and null is no longer true.
     * That functionality is available in isEmptyTrimmedOrNull().</p>
     * 
     * @param str  the String to check, may be null
     * @return <code>true</code> if the String is empty
     */
    public static boolean isEmpty(String str) {
        return (str != null && str.length() == 0);
    }

    /**
     * <p>Checks if a String is not empty ("").
     * <code>null</code> returns <code>true</code></p>
     * 
     * <pre>
     * StringUtils.isNotEmpty(null)      = true
     * StringUtils.isNotEmpty("")        = false
     * StringUtils.isNotEmpty(" ")       = true
     * StringUtils.isNotEmpty("bob")     = true
     * StringUtils.isNotEmpty("  bob  ") = true
     * </pre>
     *
     * <p>NOTE: This method changed in version 2.0.
     * It no longer returns false for null.
     * That functionality is available in isNotEmptyOrNull().</p>
     * 
     * @param str  the String to check, may be null
     * @return <code>true</code> if the String is not empty
     */
    public static boolean isNotEmpty(String str) {
        return (str == null || str.length() > 0);
    }

    /**
     * <p>Checks if a String is empty ("") or <code>null</code>.</p>
     * 
     * <pre>
     * StringUtils.isEmptyOrNull(null)      = true
     * StringUtils.isEmptyOrNull("")        = true
     * StringUtils.isEmptyOrNull(" ")       = false
     * StringUtils.isEmptyOrNull("bob")     = false
     * StringUtils.isEmptyOrNull("  bob  ") = false
     * </pre>
     *
     * @param str  the String to check, may be null
     * @return <code>true</code> if the String is empty or null
     */
    public static boolean isEmptyOrNull(String str) {
        return (str == null || str.length() == 0);
    }

    /**
     * <p>Checks if a String is not not empty ("") and not <code>null</code>.</p>
     * 
     * <pre>
     * StringUtils.isNotEmptyOrNull(null)      = false
     * StringUtils.isNotEmptyOrNull("")        = false
     * StringUtils.isNotEmptyOrNull(" ")       = true
     * StringUtils.isNotEmptyOrNull("bob")     = true
     * StringUtils.isNotEmptyOrNull("  bob  ") = true
     * </pre>
     *
     * @param str  the String to check, may be null
     * @return <code>true</code> if the String is neither empty nor null
     */
    public static boolean isNotEmptyOrNull(String str) {
        return (str != null && str.length() > 0);
    }

    /**
     * <p>Checks if a trimmed String is empty ("").
     * <code>null</code> returns <code>false</code></p>
     * 
     * <p>The String is trimmed using {@link String#trim()}.
     * Trim removes start and end characters &lt;= 32.</p>
     *
     * <pre>
     * StringUtils.isEmptyTrimmed(null)      = false
     * StringUtils.isEmptyTrimmed("")        = true
     * StringUtils.isEmptyTrimmed(" ")       = true
     * StringUtils.isEmptyTrimmed("bob")     = false
     * StringUtils.isEmptyTrimmed("  bob  ") = false
     * </pre>
     *
     * @see java.lang.String#trim()
     * @param str  the String to check, may be null
     * @return <code>true</code> if the String is empty after trim()
     */
    public static boolean isEmptyTrimmed(String str) {
        return (str != null && str.trim().length() == 0);
    }

    /**
     * <p>Checks if a trimmed String is not empty ("").</p>
     * <code>null</code> returns <code>true</code></p>
     * 
     * <p>The String is trimmed using {@link String#trim()}.
     * Trim removes start and end characters &lt;= 32.</p>
     *
     * <pre>
     * StringUtils.isNotEmptyTrimmed(null)      = true
     * StringUtils.isNotEmptyTrimmed("")        = false
     * StringUtils.isNotEmptyTrimmed(" ")       = false
     * StringUtils.isNotEmptyTrimmed("bob")     = true
     * StringUtils.isNotEmptyTrimmed("  bob  ") = true
     * </pre>
     *
     * @see java.lang.String#trim()
     * @param str  the String to check, may be null
     * @return <code>true</code> if the String is not empty after trim()
     */
    public static boolean isNotEmptyTrimmed(String str) {
        return (str == null || str.trim().length() > 0);
    }

    /**
     * <p>Checks if a trimmed String is empty ("") or <code>null</code>.</p>
     * 
     * <p>The String is trimmed using {@link String#trim()}.
     * Trim removes start and end characters &lt;= 32.</p>
     *
     * <pre>
     * StringUtils.isEmptyTrimmedOrNull(null)      = true
     * StringUtils.isEmptyTrimmedOrNull("")        = true
     * StringUtils.isEmptyTrimmedOrNull(" ")       = true
     * StringUtils.isEmptyTrimmedOrNull("bob")     = false
     * StringUtils.isEmptyTrimmedOrNull("  bob  ") = false
     * </pre>
     *
     * @see java.lang.String#trim()
     * @param str  the String to check, may be null
     * @return <code>true</code> if the String is <code>null</code>
     *  or empty after trim()
     */
    public static boolean isEmptyTrimmedOrNull(String str) {
        return (str == null || str.trim().length() == 0);
    }

    /**
     * <p>Checks if a trimmed String is not empty ("") and not <code>null</code>.</p>
     * 
     * <p>The String is trimmed using {@link String#trim()}.
     * Trim removes start and end characters &lt;= 32.</p>
     *
     * <pre>
     * StringUtils.isNotEmptyTrimmedOrNull(null)      = false
     * StringUtils.isNotEmptyTrimmedOrNull("")        = false
     * StringUtils.isNotEmptyTrimmedOrNull(" ")       = false
     * StringUtils.isNotEmptyTrimmedOrNull("bob")     = true
     * StringUtils.isNotEmptyTrimmedOrNull("  bob  ") = true
     * </pre>
     *
     * @see java.lang.String#trim()
     * @param str  the String to check, may be null
     * @return <code>true</code> if the String is not <code>null</code>
     *  and not empty after trim()
     */
    public static boolean isNotEmptyTrimmedOrNull(String str) {
        return (str != null && str.trim().length() > 0);
    }

    // Equals
    //-----------------------------------------------------------------------

    /**
     * <p>Compares two Strings, returning <code>true</code> if they are equal.</p>
     *
     * <p><code>null</code>s are handled without exceptions. Two <code>null</code>
     * references are considered to be equal. The comparison is case sensitive.</p>
     *
     * <pre>
     * StringUtils.equals(null, null)   = true
     * StringUtils.equals(null, "abc")  = false
     * StringUtils.equals("abc", null)  = false
     * StringUtils.equals("abc", "abc") = true
     * StringUtils.equals("abc", "ABC") = false
     * </pre>
     *  
     * @see java.lang.String#equals(Object)
     * @param str1  the first String, may be null
     * @param str2  the second String, may be null
     * @return <code>true</code> if the Strings are equal, case sensitive, or
     *  both <code>null</code>
     */
    public static boolean equals(String str1, String str2) {
        return (str1 == null ? str2 == null : str1.equals(str2));
    }

    /**
     * <p>Compares two Strings, returning <code>true</code> if they are equal ignoring
     * the case.</p>
     *
     * <p><code>null</code>s are handled without exceptions. Two <code>null</code>
     * references are considered equal. Comparison is case insensitive.</p>
     *
     * <pre>
     * StringUtils.equalsIgnoreCase(null, null)   = true
     * StringUtils.equalsIgnoreCase(null, "abc")  = false
     * StringUtils.equalsIgnoreCase("abc", null)  = false
     * StringUtils.equalsIgnoreCase("abc", "abc") = true
     * StringUtils.equalsIgnoreCase("abc", "ABC") = true
     * </pre>
     * 
     * @see java.lang.String#equalsIgnoreCase(String)
     * @param str1  the first String, may be null
     * @param str2  the second String, may be null
     * @return <code>true</code> if the Strings are equal, case insensitive, or
     *  both <code>null</code>
     */
    public static boolean equalsIgnoreCase(String str1, String str2) {
        return (str1 == null ? str2 == null : str1.equalsIgnoreCase(str2));
    }

    // IndexOf
    //-----------------------------------------------------------------------
    
    /**
     * <p>Find the first index of any of a set of potential substrings.</p>
     *
     * <p>A <code>null</code> String will return <code>-1</code>.
     * A <code>null</code> search array will return <code>-1</code>.
     * A <code>null</code> search array entry will be ignored.</p>
     * 
     * <pre>
     * StringUtils.indexOfAny(null, null)                = -1
     * StringUtils.indexOfAny(null, ["ab","cd"])         = -1
     * StringUtils.indexOfAny("zzabyycdxx", null)        = -1
     * StringUtils.indexOfAny("zzabyycdxx", [])          = -1
     * StringUtils.indexOfAny("zzabyycdxx", ["ab","cd"]) = 2
     * StringUtils.indexOfAny("zzabyycdxx", ["cd","ab"]) = 2
     * StringUtils.indexOfAny("zzabyycdxx", ["mn","op"]) = -1
     * </pre>
     * 
     * @param str  the String to check, may be null
     * @param searchStrs  the Strings to search for, may be null
     * @return the first index of any of the searchStrs in str, -1 if no match
     */
    public static int indexOfAny(String str, String[] searchStrs) {
        if ((str == null) || (searchStrs == null)) {
            return -1;
        }
        int sz = searchStrs.length;

        // String's can't have a MAX_VALUEth index.
        int ret = Integer.MAX_VALUE;

        int tmp = 0;
        for (int i = 0; i < sz; i++) {
            String search = searchStrs[i];
            if (search == null) {
                continue;
            }
            tmp = str.indexOf(search);
            if (tmp == -1) {
                continue;
            }

            if (tmp < ret) {
                ret = tmp;
            }
        }

        return (ret == Integer.MAX_VALUE) ? -1 : ret;
    }

    /**
     * <p>Find the latest index of any of a set of potential substrings.</p>
     *
     * <p>A <code>null</code> String will return <code>-1</code>.
     * A <code>null</code> search array will return <code>-1</code>.
     * A <code>null</code> search array entry will be ignored.</p>
     * 
     * <pre>
     * StringUtils.lastIndexOfAny(null, null)                = -1
     * StringUtils.lastIndexOfAny(null, ["ab","cd"])         = -1
     * StringUtils.lastIndexOfAny("zzabyycdxx", null)        = -1
     * StringUtils.lastIndexOfAny("zzabyycdxx", [])          = -1
     * StringUtils.lastIndexOfAny("zzabyycdxx", ["ab","cd"]) = 6
     * StringUtils.lastIndexOfAny("zzabyycdxx", ["cd","ab"]) = 6
     * StringUtils.lastIndexOfAny("zzabyycdxx", ["mn","op"]) = -1
     * </pre>
     * 
     * @param str  the String to check, may be null
     * @param searchStrs  the Strings to search for, may be null
     * @return the last index of any of the Strings, -1 if no match
     */
    public static int lastIndexOfAny(String str, String[] searchStrs) {
        if ((str == null) || (searchStrs == null)) {
            return -1;
        }
        int sz = searchStrs.length;
        int ret = -1;
        int tmp = 0;
        for (int i = 0; i < sz; i++) {
            String search = searchStrs[i];
            if (search == null) {
                continue;
            }
            tmp = str.lastIndexOf(search);
            if (tmp > ret) {
                ret = tmp;
            }
        }
        return ret;
    }

    /**
     * <p>Search a String to find the first index of any
     * character not in the given set of characters.</p>
     * 
     * @param str  the String to check, may be null
     * @param searchChars  the chars to search for, may be null
     * @return the index of any of the chars, -1 if no match or null input
     */
     public static int indexOfAnyBut(String str, char[] searchChars) {
         if (searchChars == null) {
             return -1;
         }
         return indexOfAnyBut(str, new String(searchChars));
     }

    /**
     * <p>Search a String to find the first index of any
     * character not in the given set of characters.</p>
     * 
     * @param str  the String to check, may be null
     * @param searchChars  the chars to search for, may be null
     * @return the index of any of the chars, -1 if no match or null input
     */
    public static int indexOfAnyBut(String str, String searchChars) {
        if (str == null || searchChars == null) {
            return -1;
        }

        for (int i = 0; i < str.length(); i ++) {
           if (searchChars.indexOf(str.charAt(i)) < 0) {
               return i;
           }
        }

        return -1;
    }

    // Contains
    //-----------------------------------------------------------------------
    
    /**
     * <p>Checks if the String contains only certain characters.</p>
     *
     * <pre>
     * StringUtils.containsOnly(null, 'abc')   = false
     * StringUtils.containsOnly("", null)      = false
     * StringUtils.containsOnly("", 'abc')     = true
     * StringUtils.containsOnly("", '')        = true
     * StringUtils.containsOnly("ab", '')      = false
     * StringUtils.containsOnly("abab", 'abc') = true
     * StringUtils.containsOnly("ab1", 'abc')  = false
     * StringUtils.containsOnly("abz", 'abc')  = false
     * </pre>
     * 
     * @param str  the String to check, may be null
     * @param valid  an array of valid chars, may be null
     * @return true if it only contains valid chars and is non-null
     */
    public static boolean containsOnly(String str, char[] valid) {
        // All these pre-checks are to maintain API with an older version
        if ( (valid == null) || (str == null) ) {
            return false;
        }
        if (str.length() == 0) {
            return true;
        }
        if (valid.length == 0) {
            return false;
        }
        return indexOfAnyBut(str, valid) == -1;
    }

    /**
     * <p>Checks if the String contains only certain characters.</p>
     *
     * <pre>
     * StringUtils.containsOnly(null, "abc")   = false
     * StringUtils.containsOnly("", null)      = false
     * StringUtils.containsOnly("", "abc")     = true
     * StringUtils.containsOnly("", "")        = true
     * StringUtils.containsOnly("ab", "")      = false
     * StringUtils.containsOnly("abab", "abc") = true
     * StringUtils.containsOnly("ab1", "abc")  = false
     * StringUtils.containsOnly("abz", "abc")  = false
     * </pre>
     * 
     * @param str  the String to check, may be null
     * @param validChars  a String of valid chars, may be null
     * @return true if it only contains valid chars and is non-null
     */
    public static boolean containsOnly(String str, String validChars) {
        if (str == null || validChars == null) {
            return false;
        }
        return containsOnly(str, validChars.toCharArray());
    }
    
    /**
     * <p>Checks that the String does not contain certain characters.</p>
     *
     * <pre>
     * StringUtils.containsOnly(null, 'xyz')   = true
     * StringUtils.containsOnly("", null)      = true
     * StringUtils.containsOnly("", 'xyz')     = true
     * StringUtils.containsOnly("", '')        = true
     * StringUtils.containsOnly("ab", '')      = true
     * StringUtils.containsOnly("abab", 'xyz') = true
     * StringUtils.containsOnly("ab1", 'xyz')  = true
     * StringUtils.containsOnly("abz", 'xyz')  = false
     * </pre>
     * 
     * @param str  the String to check, may be null
     * @param invalidChars  an array of invalid chars, may be null
     * @return true if it contains none of the invalid chars, or is null
     */
    public static boolean containsNone(String str, char[] invalidChars) {
        if (str == null || invalidChars == null) {
            return true;
        }
        int strSize = str.length();
        int validSize = invalidChars.length;
        for (int i = 0; i < strSize; i++) {
            char ch = str.charAt(i);
            for (int j = 0; j < validSize; j++) {
                if (invalidChars[j] == ch) {
                    return false;
                }
            }
        }
        return true;
    }

    /**
     * <p>Checks that the String does not contain certain characters.</p>
     *
     * <pre>
     * StringUtils.containsOnly(null, "xyz")   = true
     * StringUtils.containsOnly("", null)      = true
     * StringUtils.containsOnly("", "xyz")     = true
     * StringUtils.containsOnly("", "")        = true
     * StringUtils.containsOnly("ab", "")      = true
     * StringUtils.containsOnly("abab", "xyz") = true
     * StringUtils.containsOnly("ab1", "xyz")  = true
     * StringUtils.containsOnly("abz", "xyz")  = false
     * </pre>
     * 
     * @param str  the String to check, may be null
     * @param invalidChars  a String of invalid chars, may be null
     * @return true if it contains none of the invalid chars, or is null
     */
    public static boolean containsNone(String str, String invalidChars) {
        if (str == null || invalidChars == null) {
            return true;
        }
        return containsNone(str, invalidChars.toCharArray());
    }
    
    // Substring
    //-----------------------------------------------------------------------
    
    /**
     * <p>Gets a substring from the specified String avoiding exceptions.</p>
     *
     * <p>A negative start position can be used to start <code>n</code>
     * characters from the end of the String.</p>
     * 
     * <pre>
     * StringUtils.substring(null, 0)   = null
     * StringUtils.substring("abc", 0)  = "abc"
     * StringUtils.substring("abc", 2)  = "c"
     * StringUtils.substring("abc", 4)  = ""
     * StringUtils.substring("abc", -2) = "bc"
     * StringUtils.substring("abc", -4) = "abc"
     * </pre>
     * 
     * @param str  the String to get the substring from, may be null
     * @param start  the position to start from, negative means
     *  count back from the end of the String by this many characters
     * @return substring from start position, <code>null</code> if null String input
     */
    public static String substring(String str, int start) {
        if (str == null) {
            return null;
        }

        // handle negatives, which means last n characters
        if (start < 0) {
            start = str.length() + start; // remember start is negative
        }

        if (start < 0) {
            start = 0;
        }
        if (start > str.length()) {
            return "";
        }

        return str.substring(start);
    }
    
    /**
     * <p>Gets a substring from the specified String avoiding exceptions.</p>
     *
     * <p>A negative start position can be used to start/end <code>n</code>
     * characters from the end of the String.</p>
     * 
     * <pre>
     * StringUtils.substring(null, 0, 2)    = null
     * StringUtils.substring("abc", 0, 2)   = "ab"
     * StringUtils.substring("abc", 2, 0)   = ""
     * StringUtils.substring("abc", 2, 4)   = "c"
     * StringUtils.substring("abc", 4, 6)   = ""
     * StringUtils.substring("abc", -2, -1) = "b"
     * StringUtils.substring("abc", -4, 2)  = "ab"
     * </pre>
     * 
     * @param str  the String to get the substring from, may be null
     * @param start  the position to start from, negative means
     *  count back from the end of the String by this many characters
     * @param end  the position to end at (exclusive), negative means
     *  count back from the end of the String by this many characters
     * @return substring from start position to end positon,
     *  <code>null</code> if null String input
     */
    public static String substring(String str, int start, int end) {
        if (str == null) {
            return null;
        }

        // handle negatives
        if (end < 0) {
            end = str.length() + end; // remember end is negative
        }
        if (start < 0) {
            start = str.length() + start; // remember start is negative
        }

        // check length next
        if (end > str.length()) {
            // check this works.
            end = str.length();
        }

        // if start is greater than end, return ""
        if (start > end) {
            return "";
        }

        if (start < 0) {
            start = 0;
        }
        if (end < 0) {
            end = 0;
        }

        return str.substring(start, end);
    }

    // Left/Right/Mid
    //-----------------------------------------------------------------------
    
    /**
     * <p>Gets the leftmost <code>len</code> characters of a String.</p>
     *
     * <p>If <code>len</code> characters are not available, or the
     * String is <code>null</code>, the String will be returned without
     * an exception.</p>
     *
     * <pre>
     * StringUtils.left(null, 0)    = null
     * StringUtils.left("abc", 0)   = ""
     * StringUtils.left("abc", 2)   = "ab"
     * StringUtils.left("abc", 4)   = "abc"
     * StringUtils.left("abc", -2)  = IllegalArgumentException
     * </pre>
     * 
     * @param str  the String to get the leftmost characters from, may be null
     * @param len  the length of the required String, must be zero or positive
     * @return the leftmost characters, <code>null</code> if null String input
     * @throws IllegalArgumentException if len is less than zero
     */
    public static String left(String str, int len) {
        if (len < 0) {
            throw new IllegalArgumentException("Requested String length " + len + " is less than zero");
        }
        if ((str == null) || (str.length() <= len)) {
            return str;
        } else {
            return str.substring(0, len);
        }
    }

    /**
     * <p>Gets the rightmost <code>len</code> characters of a String.</p>
     *
     * <p>If <code>len</code> characters are not available, or the String
     * is <code>null</code>, the String will be returned without an
     * exception.</p>
     *
     * <pre>
     * StringUtils.right(null, 0)    = null
     * StringUtils.right("abc", 0)   = ""
     * StringUtils.right("abc", 2)   = "bc"
     * StringUtils.right("abc", 4)   = "abc"
     * StringUtils.right("abc", -2)  = IllegalArgumentException
     * </pre>
     * 
     * @param str  the String to get the rightmost characters from, may be null
     * @param len  the length of the required String, must be zero or positive
     * @return the rightmost characters, <code>null</code> if null String input
     * @throws IllegalArgumentException if len is less than zero
     */
    public static String right(String str, int len) {
        if (len < 0) {
            throw new IllegalArgumentException("Requested String length " + len + " is less than zero");
        }
        if ((str == null) || (str.length() <= len)) {
            return str;
        } else {
            return str.substring(str.length() - len);
        }
    }

    /**
     * <p>Gets <code>len</code> characters from the middle of a String.</p>
     *
     * <p>If <code>len</code> characters are not available, the remainder
     * of the String will be returned without an exception. If the
     * String is <code>null</code>, <code>null</code> will be returned.</p>
     *
     * <pre>
     * StringUtils.mid(null, 0, 0)    = null
     * StringUtils.mid("abc", 0, 2)   = "ab"
     * StringUtils.mid("abc", 0, 4)   = "abc"
     * StringUtils.mid("abc", 2, 4)   = "c"
     * StringUtils.mid("abc", 4, 2)   = StringIndexOutOfBoundsException
     * StringUtils.mid("abc", -2, -2) = StringIndexOutOfBoundsException
     * StringUtils.mid("abc", 0, -2)  = IllegalArgumentException
     * </pre>
     * 
     * @param str  the String to get the characters from, may be null
     * @param pos  the position to start from, must be valid
     * @param len  the length of the required String, must be zero or positive
     * @return the middle characters, <code>null</code> if null String input
     * @throws IndexOutOfBoundsException if pos is out of bounds
     * @throws IllegalArgumentException if len is less than zero
     */
    public static String mid(String str, int pos, int len) {
        if ((pos < 0) ||
            (str != null && pos > str.length())) {
            throw new StringIndexOutOfBoundsException("String index " + pos + " is out of bounds");
        }
        if (len < 0) {
            throw new IllegalArgumentException("Requested String length " + len + " is less than zero");
        }
        if (str == null) {
            return null;
        }
        if (str.length() <= (pos + len)) {
            return str.substring(pos);
        } else {
            return str.substring(pos, pos + len);
        }
    }

    // Splitting
    //-----------------------------------------------------------------------
    
    /**
     * <p>Splits the provided text into an array, using whitespace as the
     * separator.
     * Whitespace is defined by {@link Character#isWhitespace(char)}.</p>
     *
     * <p>The separator is not included in the returned String array.
     * Adjacent separators are treated as one separator.</p>
     * 
     * <p>A <code>null</code> input String returns <code>null</code>.</p>
     *
     * <pre>
     * StringUtils.split(null)       = null
     * StringUtils.split("")         = []
     * StringUtils.split("abc def")  = ["abc", "def"]
     * StringUtils.split("abc  def") = ["abc", "def"]
     * </pre>
     * 
     * @param str  the String to parse, may be null
     * @return an array of parsed Strings, <code>null</code> if null String input
     */
    public static String[] split(String str) {
        return split(str, null, -1);
    }

    /**
     * <p>Splits the provided text into an array, separator specified.
     * This is an alternative to using StringTokenizer.</p>
     *
     * <p>The separator is not included in the returned String array.
     * Adjacent separators are treated as one separator.</p>
     * 
     * <p>A <code>null</code> input String returns <code>null</code>.</p>
     *
     * <pre>
     * StringUtils.split(null, '.')     = null
     * StringUtils.split("", '.')       = []
     * StringUtils.split("a.b.c", '.')  = ["a", "b", "c"]
     * StringUtils.split("a..b.c", '.') = ["a", "b", "c"]
     * StringUtils.split("a:b:c", '.')  = ["a:b:c"]
     * </pre>
     * 
     * @param str  the String to parse, may be null
     * @param separatorChars  the characters used as the delimiters,
     *  <code>null</code> splits on whitespace
     * @return an array of parsed Strings, <code>null</code> if null String input
     */
    public static String[] split(String str, char separatorChar) {
        // Performance tuned for 2.0 (JDK1.4)
        
        if (str == null) {
            return null;
        }
        int len = str.length();
        if (len == 0) {
            return ArrayUtils.EMPTY_STRING_ARRAY;
        }
        List list = new ArrayList();
        int i =0, start = 0;
        boolean match = false;
        while (i < len) {
            if (str.charAt(i) == separatorChar) {
                if (match) {
                    list.add(str.substring(start, i));
                    match = false;
                }
                start = ++i;
                continue;
            }
            match = true;
            i++;
        }
        if (match) {
            list.add(str.substring(start, i));
        }
        return (String[]) list.toArray(new String[list.size()]);
    }

    /**
     * <p>Splits the provided text into an array, separators specified.
     * This is an alternative to using StringTokenizer.</p>
     *
     * <p>The separator is not included in the returned String array.
     * Adjacent separators are treated as one separator.</p>
     * 
     * <p>A <code>null</code> input String returns <code>null</code>.
     * A <code>null</code> separatorChars splits on whitespace.</p>
     *
     * <pre>
     * StringUtils.split(null, null)      = null
     * StringUtils.split("", null)        = []
     * StringUtils.split("abc def", null) = ["abc", "def"]
     * StringUtils.split("abc def", " ")  = ["abc", "def"]
     * StringUtils.split("abc  def", " ") = ["abc", "def"]
     * StringUtils.split("ab:cd:ef", ":") = ["ab", "cd", "ef"]
     * </pre>
     * 
     * @param str  the String to parse, may be null
     * @param separatorChars  the characters used as the delimiters,
     *  <code>null</code> splits on whitespace
     * @return an array of parsed Strings, <code>null</code> if null String input
     */
    public static String[] split(String str, String separatorChars) {
        return split(str, separatorChars, -1);
    }

    /**
     * <p>Splits the provided text into an array, separators specified.
     * This is an alternative to using StringTokenizer.</p>
     *
     * <p>The separator is not included in the returned String array.
     * Adjacent separators are treated as one separator.</p>
     *
     * <p>A <code>null</code> input String returns <code>null</code>.
     * A <code>null</code> separatorChars splits on whitespace.</p>
     * 
     * <pre>
     * StringUtils.split(null, null, 0)         = null
     * StringUtils.split("", null, 0)           = []
     * StringUtils.split("ab de fg", null, 0)   = ["ab", "cd", "ef"]
     * StringUtils.split("ab   de fg", null, 0) = ["ab", "cd", "ef"]
     * StringUtils.split("ab:cd:ef", ":", 0)    = ["ab", "cd", "ef"]
     * StringUtils.split("ab:cd:ef", ":", 2)    = ["ab", "cdef"]
     * </pre>
     * 
     * @param str  the String to parse, may be null
     * @param separatorChars  the characters used as the delimiters,
     *  <code>null</code> splits on whitespace
     * @param max  the maximum number of elements to include in the
     *  array. A zero or negative value implies no limit
     * @return an array of parsed Strings, <code>null</code> if null String input
     */
    public static String[] split(String str, String separatorChars, int max) {
        // Performance tuned for 2.0 (JDK1.4)
        // Direct code is quicker than StringTokenizer.
        // Also, StringTokenizer uses isSpace() not isWhitespace()
        
        if (str == null) {
            return null;
        }
        int len = str.length();
        if (len == 0) {
            return ArrayUtils.EMPTY_STRING_ARRAY;
        }
        List list = new ArrayList();
        int sizePlus1 = 1;
        int i =0, start = 0;
        boolean match = false;
        if (separatorChars == null) {
            // Null separator means use whitespace
            while (i < len) {
                if (Character.isWhitespace(str.charAt(i))) {
                    if (match) {
                        if (sizePlus1++ == max) {
                            i = len;
                        }
                        list.add(str.substring(start, i));
                        match = false;
                    }
                    start = ++i;
                    continue;
                }
                match = true;
                i++;
            }
        } else if (separatorChars.length() == 1) {
            // Optimise 1 character case
            char sep = separatorChars.charAt(0);
            while (i < len) {
                if (str.charAt(i) == sep) {
                    if (match) {
                        if (sizePlus1++ == max) {
                            i = len;
                        }
                        list.add(str.substring(start, i));
                        match = false;
                    }
                    start = ++i;
                    continue;
                }
                match = true;
                i++;
            }
        } else {
            // standard case
            while (i < len) {
                if (separatorChars.indexOf(str.charAt(i)) >= 0) {
                    if (match) {
                        if (sizePlus1++ == max) {
                            i = len;
                        }
                        list.add(str.substring(start, i));
                        match = false;
                    }
                    start = ++i;
                    continue;
                }
                match = true;
                i++;
            }
        }
        if (match) {
            list.add(str.substring(start, i));
        }
        return (String[]) list.toArray(new String[list.size()]);
    }

    // Joining
    //-----------------------------------------------------------------------
    /**
     * <p>Concatenates elements of an array into a single String.</p>
     *
     * <p>The difference from join is that concatenate has no delimiter.</p>
     * 
     * @param array  the array of values to concatenate, may be null
     * @return the concatenated String, <code>null</code> if null array input
     */
    public static String concatenate(Object[] array) {
        return join(array, null);
    }
    
    /**
     * <p>Joins the elements of the provided array into a single String
     * containing the provided list of elements.</p>
     *
     * <p>No delimiter is added before or after the list.
     *
     * @param array  the array of values to join together, may be null
     * @param separator  the separator character to use
     * @return the joined String, <code>null</code> if null array input
     */
    public static String join(Object[] array, char separator) {
        if (array == null) {
            return null;
        }
        int arraySize = array.length;
        int bufSize = (arraySize == 0 ? 0 : ((array[0] == null ? 16 : array[0].toString().length()) + 1) * arraySize);
        StringBuffer buf = new StringBuffer(bufSize);

        for (int i = 0; i < arraySize; i++) {
            if (i > 0) {
                buf.append(separator);
            }
            if (array[i] != null) {
                buf.append(array[i]);
            }
        }
        return buf.toString();
    }

    /**
     * <p>Joins the elements of the provided array into a single String
     * containing the provided list of elements.</p>
     *
     * <p>No delimiter is added before or after the list.
     * A <code>null</code> separator is the same as an empty String ("").</p>
     *
     * @param array  the array of values to join together, may be null
     * @param separator  the separator character to use, null treated as ""
     * @return the joined String, <code>null</code> if null array input
     */
    public static String join(Object[] array, String separator) {
        if (array == null) {
            return null;
        }
        if (separator == null) {
            separator = "";
        }
        int arraySize = array.length;

        // ArraySize ==  0: Len = 0
        // ArraySize > 0:   Len = NofStrings *(len(firstString) + len(separator))
        //           (Assuming that all Strings are roughly equally long)
        int bufSize 
            = ((arraySize == 0) ? 0 
                : arraySize * ((array[0] == null ? 16 : array[0].toString().length()) 
                    + ((separator != null) ? separator.length(): 0)));

        StringBuffer buf = new StringBuffer(bufSize);

        for (int i = 0; i < arraySize; i++) {
            if ((separator != null) && (i > 0)) {
                buf.append(separator);
            }
            if (array[i] != null) {
                buf.append(array[i]);
            }
        }
        return buf.toString();
    }

    /**
     * <p>Joins the elements of the provided <code>Iterator</code> into
     * a single String containing the provided elements.</p>
     *
     * <p>No delimiter is added before or after the list.
     *
     * @param iterator  the <code>Iterator</code> of values to join together, may be null
     * @param separator  the separator character to use
     * @return the joined String, <code>null</code> if null iterator input
     */
    public static String join(Iterator iterator, char separator) {
        if (iterator == null) {
            return null;
        }
        StringBuffer buf = new StringBuffer(256);  // Java default is 16, probably too small
        while (iterator.hasNext()) {
            Object obj = iterator.next();
            if (obj != null) {
                buf.append(obj);
            }
            if (iterator.hasNext()) {
                buf.append(separator);
            }
        }
        return buf.toString();
    }

    /**
     * <p>Joins the elements of the provided <code>Iterator</code> into
     * a single String containing the provided elements.</p>
     *
     * <p>No delimiter is added before or after the list.
     * A <code>null</code> separator is the same as an empty String ("").</p>
     *
     * @param iterator  the <code>Iterator</code> of values to join together, may be null
     * @param separator  the separator character to use, null treated as ""
     * @return the joined String, <code>null</code> if null iterator input
     */
    public static String join(Iterator iterator, String separator) {
        if (iterator == null) {
            return null;
        }
        StringBuffer buf = new StringBuffer(256);  // Java default is 16, probably too small
        while (iterator.hasNext()) {
            Object obj = iterator.next();
            if (obj != null) {
                buf.append(obj);
            }
            if ((separator != null) && iterator.hasNext()) {
                buf.append(separator);
            }
         }
        return buf.toString();
    }

    // Replacing
    //-----------------------------------------------------------------------
    
    /**
     * <p>Replace a String with another String inside a larger String, once.</p>
     * 
     * <p>A <code>null</code> reference passed to this method is a no-op.</p>
     * 
     * <pre>
     * StringUtils.replaceOnce(null, null, null)  = null
     * StringUtils.replaceOnce("aba", null, null) = "aba"
     * StringUtils.replaceOnce("aba", null, null) = "aba"
     * StringUtils.replaceOnce("aba", "a", null)  = "aba"
     * StringUtils.replaceOnce("aba", "a", "")    = "aba"
     * StringUtils.replaceOnce("aba", "a", "z")   = "zba"
     * </pre>
     * 
     * @see #replace(String text, String repl, String with, int max)
     * @param text  text to search and replace in, may be null
     * @param repl  the String to search for, may be null
     * @param with  the String to replace with, may be null
     * @return the text with any replacements processed,
     *  <code>null</code> if null String input
     */
    public static String replaceOnce(String text, String repl, String with) {
        return replace(text, repl, with, 1);
    }

    /**
     * <p>Replace all occurances of a String within another String.</p>
     *
     * <p>A <code>null</code> reference passed to this method is a no-op.</p>
     * 
     * <pre>
     * StringUtils.replace(null, null, null)  = null
     * StringUtils.replace("aba", null, null) = "aba"
     * StringUtils.replace("aba", null, null) = "aba"
     * StringUtils.replace("aba", "a", null)  = "aba"
     * StringUtils.replace("aba", "a", "")    = "aba"
     * StringUtils.replace("aba", "a", "z")   = "zbz"
     * </pre>
     * 
     * @see #replace(String text, String repl, String with, int max)
     * @param text  text to search and replace in, may be null
     * @param repl  the String to search for, may be null
     * @param with  the String to replace with, may be null
     * @return the text with any replacements processed,
     *  <code>null</code> if null String input
     */
    public static String replace(String text, String repl, String with) {
        return replace(text, repl, with, -1);
    }

    /**
     * <p>Replace a String with another String inside a larger String,
     * for the first <code>max</code> values of the search String.</p>
     *
     * <p>A <code>null</code> reference passed to this method is a no-op.</p>
     *
     * <pre>
     * StringUtils.replace(null, null, null, 1)   = null
     * StringUtils.replace("abaa", null, null, 1) = "abaa"
     * StringUtils.replace("abaa", null, null, 1) = "abaa"
     * StringUtils.replace("abaa", "a", null, 1)  = "abaa"
     * StringUtils.replace("abaa", "a", "", 1)    = "abaa"
     * StringUtils.replace("abaa", "a", "z", 0)   = "abaa"
     * StringUtils.replace("abaa", "a", "z", 1)   = "zbaa"
     * StringUtils.replace("abaa", "a", "z", 2)   = "zbza"
     * StringUtils.replace("abaa", "a", "z", -1)  = "zbzz"
     * </pre>
     * 
     * @param text  text to search and replace in, may be null
     * @param repl  the String to search for, may be null
     * @param with  the String to replace with, may be null
     * @param max  maximum number of values to replace, or <code>-1</code> if no maximum
     * @return the text with any replacements processed,
     *  <code>null</code> if null String input
     */
    public static String replace(String text, String repl, String with, int max) {
        if (text == null || repl == null || with == null || repl.length() == 0 || max == 0) {
            return text;
        }

        StringBuffer buf = new StringBuffer(text.length());
        int start = 0, end = 0;
        while ((end = text.indexOf(repl, start)) != -1) {
            buf.append(text.substring(start, end)).append(with);
            start = end + repl.length();

            if (--max == 0) {
                break;
            }
        }
        buf.append(text.substring(start));
        return buf.toString();
    }

    /**
     * <p>Overlay a part of a String with another String.</p>
     *
     * <pre>
     * StringUtils.overlayString(null, null, 2, 4)        = null
     * StringUtils.overlayString("abcdef", null, 2, 4)    = "abef"
     * StringUtils.overlayString("abcdef", "", 2, 4)      = "abef"
     * StringUtils.overlayString("abcdef", "zzzz", 2, 4)  = "abzzzzef"
     * StringUtils.overlayString("abcdef", "zzzz", 4, 2)  = "abcdzzzzcdef"
     * StringUtils.overlayString("abcdef", "zzzz", -1, 4) = IndexOutOfBoundsException
     * StringUtils.overlayString("abcdef", "zzzz", 2, 8)  = IndexOutOfBoundsException
     * </pre>
     * 
     * @param text  the String to do overlaying in, may be null
     * @param overlay  the String to overlay, may be null
     * @param start  the position to start overlaying at, must be valid
     * @param end  the position to stop overlaying before, must be valid
     * @return overlayed String, <code>null</code> if null String input
     * @throws IndexOutOfBoundsException if either position is invalid
     */
    public static String overlayString(String text, String overlay, int start, int end) {
        if (text == null) {
            return null;
        }
        if (overlay == null) {
            overlay = "";
        }
        return new StringBuffer(start + overlay.length() + text.length() - end + 1)
            .append(text.substring(0, start))
            .append(overlay)
            .append(text.substring(end))
            .toString();
    }

    // Chomping
    //-----------------------------------------------------------------------

    /**
     * <p>Remove one newline from end of a String if it's there,
     * otherwise leave it alone.  A newline is &quot;<code>\n</code>&quot;,
     * &quot;<code>\r</code>&quot;, or &quot;<code>\r\n</code>&quot;.</p>
     *
     * <p>NOTE: This method changed in 2.0.
     * It now more closely matches Perl chomp.
     * For the previous behavior, use {@link #slice(String)}.</p>
     *
     * @param str  the String to chomp a newline from, may be null
     * @return String without newline, <code>null</code> if null String input
     */
    public static String chomp(String str) {
        if (str == null || str.length() == 0) {
            return str;
        }

        if (str.length() == 1) {
            if ("\r".equals(str) || "\n".equals(str)) {
                return "";
            }
            else {
                return str;
            }
        }

        int lastIdx = str.length() - 1;
        char last = str.charAt(lastIdx);

        if (last == '\n') {
            if (str.charAt(lastIdx - 1) == '\r') {
                lastIdx--;
            }
        } else if (last == '\r') {

        } else {
            lastIdx++;
        }
        return str.substring(0, lastIdx);
    }

    /**
     * <p>Remove <code>separator</code> from the end of
     * <code>str</code> if it's there, otherwise leave it alone.</p>
     *
     * <p>NOTE: This method changed in version 2.0.
     * It now more closely matches Perl chomp.
     * For the previous behavior, use {@link #slice(String,String)}.</p>
     *
     * @param str  the String to chomp from, may be null
     * @param separator  separator String, may be null
     * @return String without trailing separator, <code>null</code> if null String input
     */
    public static String chomp(String str, String separator) {
        if (str == null || str.length() == 0 || separator == null) {
            return str;
        }
        if (str.endsWith(separator)) {
            return str.substring(0, str.length() - separator.length());
        }
        return str;
    }

    /**
     * <p>Remove any &quot;\n&quot; if and only if it is at the end
     * of the supplied String.</p>
     * 
     * @param str  the String to chomp from, must not be null
     * @return String without chomped ending
     * @throws NullPointerException if str is <code>null</code>
     * @deprecated Use {@link #chomp(String)} instead.
     *             Method will be removed in Commons Lang 3.0.
     */
    public static String chompLast(String str) {
        return chompLast(str, "\n");
    }
    
    /**
     * <p>Remove a value if and only if the String ends with that value.</p>
     * 
     * @param str  the String to chomp from, must not be null
     * @param sep  the String to chomp, must not be null
     * @return String without chomped ending
     * @throws NullPointerException if str or sep is <code>null</code>
     * @deprecated Use {@link #chomp(String,String)} instead.
     *             Method will be removed in Commons Lang 3.0.
     */
    public static String chompLast(String str, String sep) {
        if (str.length() == 0) {
            return str;
        }
        String sub = str.substring(str.length() - sep.length());
        if (sep.equals(sub)) {
            return str.substring(0, str.length() - sep.length());
        } else {
            return str;
        }
    }

    /** 
     * <p>Remove everything and return the last value of a supplied String, and
     * everything after it from a String.
     * [That makes no sense. Just use sliceRemainder() :-)]</p>
     *
     * @param str  the String to chomp from, must not be null
     * @param sep  the String to chomp, must not be null
     * @return String chomped
     * @throws NullPointerException if str or sep is <code>null</code>
     * @deprecated Use {@link #sliceRemainder(String,String)} instead.
     *             Method will be removed in Commons Lang 3.0.
     */
    public static String getChomp(String str, String sep) {
        int idx = str.lastIndexOf(sep);
        if (idx == str.length() - sep.length()) {
            return sep;
        } else if (idx != -1) {
            return str.substring(idx);
        } else {
            return "";
        }
    }

    /** 
     * <p>Remove the first value of a supplied String, and everything before it
     * from a String.</p>
     *
     * @param str  the String to chomp from, must not be null
     * @param sep  the String to chomp, must not be null
     * @return String without chomped beginning
     * @throws NullPointerException if str or sep is <code>null</code>
     * @deprecated Use {@link #sliceFirstRemainder(String,String)} instead.
     *             Method will be removed in Commons Lang 3.0.
     */
    public static String prechomp(String str, String sep) {
        int idx = str.indexOf(sep);
        if (idx != -1) {
            return str.substring(idx + sep.length());
        } else {
            return str;
        }
    }

    /** 
     * <p>Remove and return everything before the first value of a
     * supplied String from another String.</p>
     *
     * @param str  the String to chomp from, must not be null
     * @param sep  the String to chomp, must not be null
     * @return String prechomped
     * @throws NullPointerException if str or sep is <code>null</code>
     * @deprecated Use {@link #sliceFirst(String,String)} instead.
     *             Method will be removed in Commons Lang 3.0.
     */
    public static String getPrechomp(String str, String sep) {
        int idx = str.indexOf(sep);
        if (idx != -1) {
            return str.substring(0, idx + sep.length());
        } else {
            return "";
        }
    }

    // Chopping
    //-----------------------------------------------------------------------
    
    /**
     * <p>Remove the last character from a String.</p>
     *
     * <p>If the String ends in <code>\r\n</code>, then remove both
     * of them.</p>
     *
     * @param str  the String to chop last character from, may be null
     * @return String without last character, <code>null</code> if null String input
     */
    public static String chop(String str) {
        if (str == null) {
            return null;
        }
        int strLen = str.length();
        if (strLen < 2) {
            return "";
        }
        int lastIdx = strLen - 1;
        String ret = str.substring(0, lastIdx);
        char last = str.charAt(lastIdx);
        if (last == '\n') {
            if (ret.charAt(lastIdx - 1) == '\r') {
                return ret.substring(0, lastIdx - 1);
            }
        }
        return ret;
    }

    /**
     * <p>Remove <code>\n</code> from end of a String if it's there.
     * If a <code>\r</code> precedes it, then remove that too.</p>
     *
     * @param str  the String to chop a newline from, must not be null
     * @return String without newline
     * @throws NullPointerException if str is <code>null</code>
     * @deprecated Use {@link #chomp(String)} instead.
     *             Method will be removed in Commons Lang 3.0.
     */
    public static String chopNewline(String str) {
        int lastIdx = str.length() - 1;
        if (lastIdx <= 0) {
            return "";
        }
        char last = str.charAt(lastIdx);
        if (last == '\n') {
            if (str.charAt(lastIdx - 1) == '\r') {
                lastIdx--;
            }
        } else {
            lastIdx++;
        }
        return str.substring(0, lastIdx);
    }


    // Slicing
    //-----------------------------------------------------------------------

    /**
     * <p>Remove the last newline, and everything after it from a String.</p>
     *
     * <p><em>(This method was formerly named chomp or chopNewline.)</em></p>
     *
     * @param str  the String to slice the newline from, may be null
     * @return String without sliced newline, <code>null</code> if null String input
     */
    public static String slice(String str) {
        return slice(str, "\n");
    }

    /**
     * <p>Find the last occurence of a separator String;
     * remove it and everything after it.</p>
     *
     * <p><em>(This method was formerly named chomp.)</em></p>
     *
     * @param str  the String to slice from, may be null
     * @param separator  the String to slice, may be null
     * @return String without sliced ending, <code>null</code> if null String input
     */
    public static String slice(String str, String separator) {
        if (str == null || separator == null || str.length() == 0 || separator.length() == 0) {
            return str;
        }
        int idx = str.lastIndexOf(separator);
        if (idx != -1) {
            return str.substring(0, idx);
        } else {
            return str;
        }
    }

    /**
     * <p>Find the last occurence of a separator String, and return
     * everything after it.</p>
     *
     * <p><em>(This method was formerly named getchomp. Also, now it does not
     * include the separator in the return value.)</em></p>
     *
     * @param str  the String to slice from, may be null
     * @param separator  the String to slice, may be null
     * @return String sliced, <code>null</code> if null String input
     */
    public static String sliceRemainder(String str, String separator) {
        if (str == null || str.length() == 0) {
            return str;
        }
        if (separator == null || separator.length() == 0) {
            return "";
        }
        int idx = str.lastIndexOf(separator);
        if (idx == str.length() - separator.length()) {
            return "";
        } else if (idx != -1) {
            return str.substring(idx + separator.length());
        } else {
            return "";
        }
    }

    /**
     * <p>Find the first occurence of a separator String, and return
     * everything after it.</p>
     *
     * <p><em>(This method was formerly named prechomp.  Also, previously
     * it included the separator in the return value; now it does not.)</em></p>
     *
     * @param str  the String to slice from, may be null
     * @param separator  the String to slice, may be null
     * @return String without sliced beginning, <code>null</code> if null String input
     */
    public static String sliceFirstRemainder(String str, String separator) {
        if (str == null || str.length() == 0) {
            return str;
        }
        if (separator == null || separator.length() == 0) {
            return "";
        }
        int idx = str.indexOf(separator);
        if (idx != -1) {
            return str.substring(idx + separator.length());
        } else {
            return str;
        }
    }

    /**
     * <p>Find the first occurence of a separator String;
     * return everything before it (but not including the separator).</p>
     *
     * <p><em>(This method was formerly named getPrechomp.  Also, it used to
     * include the separator, but now it does not.)</em></p>
     *
     * @param str  the String to slice from, may be null
     * @param separator  the String to slice, may be null
     * @return sliced String, <code>null</code> if null String input
     */
    public static String sliceFirst(String str, String separator) {
        if (str == null || separator == null || str.length() == 0 || separator.length() == 0) {
            return str;
        }
        int idx = str.indexOf(separator);
        if (idx != -1) {
            return str.substring(0, idx);
        } else {
            return "";
        }
    }

    // Conversion
    //-----------------------------------------------------------------------
    
    /**
     * <p>Escapes any values it finds into their String form.</p>
     *
     * <p>So a tab becomes the characters <code>'\\'</code> and
     * <code>'t'</code>.</p>
     *
     * <p>As of Lang 2.0, this calls {@link StringEscapeUtils#escapeJava(String)}
     * behind the scenes.
     * </p>
     * @see StringEscapeUtils#escapeJava(java.lang.String)
     * @param str String to escape values in
     * @return String with escaped values
     * @throws NullPointerException if str is <code>null</code>
     * @deprecated Use {@link StringEscapeUtils#escapeJava(String)}
     *             This method will be removed in Commons Lang 3.0
     */
    public static String escape(String str) {
        return StringEscapeUtils.escapeJava(str);
    }

    // Padding
    //-----------------------------------------------------------------------
    
    /**
     * <p>Repeat a String <code>repeat</code> times to form a
     * new String.</p>
     *
     * <pre>
     * StringUtils.repeat(null, 2) = null
     * StringUtils.repeat("", 0)   = ""
     * StringUtils.repeat("", 2)   = ""
     * StringUtils.repeat("a", 3)  = "aaa"
     * StringUtils.repeat("ab", 2) = "abab"
     * StringUtils.repeat("a", -2) = ""
     * </pre>
     *
     * @param str  the String to repeat, may be null
     * @param repeat  number of times to repeat str, negative treated as zero
     * @return a new String consisting of the original String repeated,
     *  <code>null</code> if null String input
     */
    public static String repeat(String str, int repeat) {
        // Performance tuned for 2.0 (JDK1.4)
        
        if (str == null) {
            return null;
        }
        if (repeat <= 0) {
            return "";
        }
        int inputLength;
        if (repeat == 1 || (inputLength = str.length()) == 0) {
            return str;
        }
        if (inputLength == 1 && repeat <= PAD_LIMIT) {
           return padding(repeat, str.charAt(0));
        }

        int outputLength = inputLength * repeat;
        switch (inputLength) {
            case 1:
                char ch = str.charAt(0);
                char[] output1 = new char[outputLength];
                for (int i = repeat - 1; i >= 0; i--) {
                    output1[i] = ch;
                }
                return new String(output1);
            case 2:
                char ch0 = str.charAt(0);
                char ch1 = str.charAt(1);
                char[] output2 = new char[outputLength];
                for (int i = repeat * 2 - 2; i >= 0; i--,i--) {
                    output2[i] = ch0;
                    output2[i + 1] = ch1;
                }
                return new String(output2);
            default:
                StringBuffer buf = new StringBuffer(outputLength);
                for (int i = 0; i < repeat; i++) {
                    buf.append(str);
                }        
                return buf.toString();
        }
    }

    /**
     * <p>Returns a String containing the requested number of 
     * space characters (' ').</p>
     * 
     * <pre>
     * StringUtils.padding(0)  = ""
     * StringUtils.padding(3)  = "   "
     * StringUtils.padding(-2) = IndexOutOfBoundsException
     * </pre>
     *
     * @param repeat  number of times to repeat space
     * @return a String with <code>repeat</code> spaces
     * @throws IndexOutOfBoundsException if <code>repeat &lt; 0</code>
     */
    private static String padding(int repeat) {
        while (spaces.length() < repeat)  {
            spaces = spaces.concat(spaces);
        }
        return spaces.substring(0, repeat);
    }

    /**
     * <p>Returns padding using the specified delimiter repeated
     * to a given length.</p>
     *
     * <pre>
     * StringUtils.padding(0, 'e')  = ""
     * StringUtils.padding(3, 'e')  = "eee"
     * StringUtils.padding(-2, 'e') = IndexOutOfBoundsException
     * </pre>
     *
     * @param repeat  number of times to repeat delim
     * @param padChar  character to repeat
     * @return String with repeated character
     * @throws IndexOutOfBoundsException if <code>repeat &lt; 0</code>
     */
    private static String padding(int repeat, char padChar) {
        if (padding[padChar] == null) {
            padding[padChar] = String.valueOf(padChar);
        }
        while (padding[padChar].length() < repeat) {
            padding[padChar] = padding[padChar].concat(padding[padChar]);
        }
        return padding[padChar].substring(0, repeat);
    }

    /**
     * <p>Right pad a String with spaces (' ').</p>
     *
     * <p>The String is padded to the size of <code>size</code>.</p>
     * 
     * <pre>
     * StringUtils.rightPad(null, 1)   = null
     * StringUtils.rightPad("bat", 3)  = "bat"
     * StringUtils.rightPad("bat", 5)  = "bat  "
     * StringUtils.rightPad("bat", 1)  = "bat"
     * StringUtils.rightPad("bat", -1) = "bat"
     * </pre>
     *
     * @param str  the String to pad out, may be null
     * @param size  the size to pad to
     * @return right padded String or original String if no padding is necessary,
     *  <code>null</code> if null String input
     */
    public static String rightPad(String str, int size) {
        if (str == null) {
            return null;
        }
        int pads = size - str.length();
        if (pads <= 0) {
            return str; // returns original String when possible
        }
        if (pads > PAD_LIMIT) {
            return rightPad(str, size, ' ');
        }
        return str.concat(padding(pads));
    }

    /**
     * <p>Right pad a String with a specified character.</p>
     *
     * <p>The String is padded to the size of <code>size</code>.</p>
     *
     * <pre>
     * StringUtils.rightPad(null, 1, 'z')   = null
     * StringUtils.rightPad("bat", 3, 'z')  = "bat"
     * StringUtils.rightPad("bat", 5, 'z')  = "batzz"
     * StringUtils.rightPad("bat", 1, 'z')  = "bat"
     * StringUtils.rightPad("bat", -1, 'z') = "bat"
     * </pre>
     *
     * @param str  the String to pad out, may be null
     * @param size  the size to pad to
     * @param padChar  the character to pad with
     * @return right padded String or original String if no padding is necessary,
     *  <code>null</code> if null String input
     */
    public static String rightPad(String str, int size, char padChar) {
        if (str == null) {
            return null;
        }
        int pads = size - str.length();
        if (pads <= 0) {
            return str; // returns original String when possible
        }
        if (pads > PAD_LIMIT) {
            return rightPad(str, size, String.valueOf(padChar));
        }
        return str.concat(padding(pads, padChar));
    }

    /**
     * <p>Right pad a String with a specified String.</p>
     *
     * <p>The String is padded to the size of <code>size</code>.</p>
     *
     * <pre>
     * StringUtils.rightPad(null, 1, "yz")   = null
     * StringUtils.rightPad("bat", 3, "yz")  = "bat"
     * StringUtils.rightPad("bat", 5, "yz")  = "batyz"
     * StringUtils.rightPad("bat", 8, "yz")  = "batyzyzy"
     * StringUtils.rightPad("bat", 1, "yz")  = "bat"
     * StringUtils.rightPad("bat", -1, "yz") = "bat"
     * StringUtils.rightPad("bat", 1, null)  = IllegalArgumentException
     * StringUtils.rightPad("bat", 1, "")    = IllegalArgumentException
     * StringUtils.rightPad(null, 1, "")     = IllegalArgumentException
     * </pre>
     *
     * @param str  the String to pad out, may be null
     * @param size  the size to pad to
     * @param padStr  the String to pad with, must not be null
     * @return right padded String or original String if no padding is necessary,
     *  <code>null</code> if null String input
     * @throws IllegalArgumentException if padStr is the empty String or null
     */
    public static String rightPad(String str, int size, String padStr) {
        int padLen;
        if (padStr == null || (padLen = padStr.length()) == 0) {
            throw new IllegalArgumentException("Pad String must not be null or empty");
        }
        if (str == null) {
            return null;
        }
        int strLen = str.length();
        int pads = size - strLen;
        if (padLen == 1 && pads <= PAD_LIMIT) {
            return rightPad(str, size, padStr.charAt(0));
        }
        
        if (pads <= 0) {
            return str; // returns original String when possible
        }
        if (pads == padLen) {
            return str.concat(padStr);
        } else if (pads < padLen) {
            return str.concat(padStr.substring(0, pads));
        } else {
            char[] padding = new char[pads];
            char[] padChars = padStr.toCharArray();
            for (int i = 0; i < pads; i++) {
                padding[i] = padChars[i % padLen];
            }
            return str.concat(new String(padding));
        }
    }

    /**
     * <p>Left pad a String with spaces (' ').</p>
     *
     * <p>The String is padded to the size of <code>size<code>.</p>
     *
     * <pre>
     * StringUtils.leftPad(null, 1)   = null
     * StringUtils.leftPad("bat", 3)  = "bat"
     * StringUtils.leftPad("bat", 5)  = "  bat"
     * StringUtils.leftPad("bat", 1)  = "bat"
     * StringUtils.leftPad("bat", -1) = "bat"
     * </pre>
     *
     * @param str  the String to pad out, may be null
     * @param size  the size to pad to
     * @return left padded String or original String if no padding is necessary,
     *  <code>null</code> if null String input
     */
    public static String leftPad(String str, int size) {
        if (str == null) {
            return null;
        }
        int pads = size - str.length();
        if (pads <= 0) { 
            return str; // returns original String when possible
        }
        if (pads > PAD_LIMIT) {
            return leftPad(str, size, ' ');
        }
        return padding(pads).concat(str);
    }

    /**
     * <p>Left pad a String with a specified character.</p>
     *
     * <p>Pad to a size of <code>size</code>.</p>
     *
     * <pre>
     * StringUtils.leftPad(null, 1, 'z')   = null
     * StringUtils.leftPad("bat", 3, 'z')  = "bat"
     * StringUtils.leftPad("bat", 5, 'z')  = "zzbat"
     * StringUtils.leftPad("bat", 1, 'z')  = "bat"
     * StringUtils.leftPad("bat", -1, 'z') = "bat"
     * </pre>
     *
     * @param str  the String to pad out, may be null
     * @param size  the size to pad to
     * @param padChar  the character to pad with
     * @return left padded String or original String if no padding is necessary,
     *  <code>null</code> if null String input
     */
    public static String leftPad(String str, int size, char padChar) {
        if (str == null) {
            return null;
        }
        int pads = size - str.length();
        if (pads <= 0) {
            return str; // returns original String when possible
        }
        if (pads > PAD_LIMIT) {
            return leftPad(str, size, String.valueOf(padChar));
        }
        return padding(pads, padChar).concat(str);
    }

    /**
     * <p>Left pad a String with a specified String.</p>
     *
     * <p>Pad to a size of <code>size</code>.</p>
     *
     * <pre>
     * StringUtils.leftPad(null, 1, "yz")   = null
     * StringUtils.leftPad("bat", 3, "yz")  = "bat"
     * StringUtils.leftPad("bat", 5, "yz")  = "yzbat"
     * StringUtils.leftPad("bat", 8, "yz")  = "yzyzybat"
     * StringUtils.leftPad("bat", 1, "yz")  = "bat"
     * StringUtils.leftPad("bat", -1, "yz") = "bat"
     * StringUtils.leftPad("bat", 1, null)  = IllegalArgumentException
     * StringUtils.leftPad("bat", 1, "")    = IllegalArgumentException
     * StringUtils.leftPad(null, 1, "")     = IllegalArgumentException
     * </pre>
     *
     * @param str  the String to pad out, may be null
     * @param size  the size to pad to
     * @param padStr  the String to pad with, must not be null
     * @return left padded String or original String if no padding is necessary,
     *  <code>null</code> if null String input
     * @throws IllegalArgumentException if padStr is the empty String or null
     */
    public static String leftPad(String str, int size, String padStr) {
        int padLen;
        if (padStr == null || (padLen = padStr.length()) == 0) {
            throw new IllegalArgumentException("Pad String must not be null or empty");
        }
        if (str == null) {
            return null;
        }
        int strLen = str.length();
        int pads = size - strLen;
        if (padLen == 1 && pads <= PAD_LIMIT) {
            return leftPad(str, size, padStr.charAt(0));
        }
        
        if (pads <= 0) {
            return str; // returns original String when possible
        }
        if (pads == padLen) {
            return padStr.concat(str);
        } else if (pads < padLen) {
            return padStr.substring(0, pads).concat(str);
        } else {
            char[] padding = new char[pads];
            char[] padChars = padStr.toCharArray();
            for (int i = 0; i < pads; i++) {
                padding[i] = padChars[i % padLen];
            }
            return new String(padding).concat(str);
        }
    }

    // Centering
    //-----------------------------------------------------------------------
    
    /**
     * <p>Centers a String in a larger String of size <code>size</code>
     * using the space character (' ').<p>
     * 
     * <p>If the size is less than the String length, the String is returned.
     * A <code>null</code> String returns <code>null</code>.
     * A negative size is treated as zero.</p>
     *
     * <p>Equivalent to <code>center(str, size, " ")</code>.</p>
     *
     * <pre>
     * StringUtils.center(null, -1)  = null
     * StringUtils.center(null, 4)   = null
     * StringUtils.center("ab", -1)  = "ab"
     * StringUtils.center("", 4)     = "    "
     * StringUtils.center("ab", 4)   = " ab "
     * StringUtils.center("abcd", 2) = "abcd"
     * StringUtils.center("a", 4)    = " a  "
     * </pre>
     * 
     * @param str  the String to center, may be null
     * @param size  the int size of new String, negative treated as zero
     * @return centered String, <code>null</code> if null String input
     */
    public static String center(String str, int size) {
        if (str == null || size <= 0) {
            return str;
        }
        int strLen = str.length();
        int pads = size - strLen;
        if (pads <= 0) {
            return str;
        }
        str = leftPad(str, strLen + pads / 2, ' ');
        str = rightPad(str, size, ' ');
        return str;
    }

    /**
     * <p>Centers a String in a larger String of size <code>size</code>.</p>
     *
     * <p>Uses a supplied String as the value to pad the String with.</p>
     *
     * <p>If the size is less than the String length, the String is returned.
     * A <code>null</code> String returns <code>null</code>.
     * A negative size is treated as zero.</p>
     *
     * <pre>
     * StringUtils.center(null, -1, " ")  = null
     * StringUtils.center("ab", -1, " ")  = "ab"
     * StringUtils.center(null, 4, " ")   = null
     * StringUtils.center("", 4, " ")     = "    "
     * StringUtils.center("ab", 4, " ")   = " ab"
     * StringUtils.center("abcd", 2, " ") = " abcd"
     * StringUtils.center("a", 4, " ")    = " a  "
     * StringUtils.center("a", 4, "yz")    = "yayz"
     * StringUtils.center("abc", 4, null) = IllegalArgumentException
     * StringUtils.center("abc", 4, "")   = IllegalArgumentException
     * StringUtils.center(null, 4, "")    = IllegalArgumentException
     * </pre>
     * 
     * @param str  the String to center, may be null
     * @param size  the int size of new String, negative treated as zero
     * @param padStr  the String to pad the new String with, must not be null or empty
     * @return centered String, <code>null</code> if null String input
     * @throws IllegalArgumentException if padStr is <code>null</code> or empty
     */
    public static String center(String str, int size, String padStr) {
        if (padStr == null || padStr.length() == 0) {
            throw new IllegalArgumentException("Pad String must not be null or empty");
        }
        if (str == null || size <= 0) {
            return str;
        }
        int strLen = str.length();
        int pads = size - strLen;
        if (pads <= 0) {
            return str;
        }
        str = leftPad(str, strLen + pads / 2, padStr);
        str = rightPad(str, size, padStr);
        return str;
    }

    // Stripping
    //-----------------------------------------------------------------------
    
    /**
     * <p>Strips whitespace from the start and end of a String.
     * This is similar to {@link String#trim()} but instead removes whitespace.
     * Whitespace is defined by {@link Character#isWhitespace(char)}.</p>
     * 
     * <p>A <code>null</code> input String returns <code>null</code>.</p>
     * 
     * <pre>
     * StringUtils.strip(null)     = null
     * StringUtils.strip("abc")    = "abc"
     * StringUtils.strip("  abc")  = "abc"
     * StringUtils.strip("abc  ")  = "abc"
     * StringUtils.strip(" abc ")  = "abc"
     * StringUtils.strip(" ab c ") = "ab c"
     * </pre>
     * 
     * @param str  the String to remove whitespace from, may be null
     * @return the stripped String, <code>null</code> if null String input
     */
    public static String strip(String str) {
        return strip(str, null);
    }
    
    /**
     * <p>Strips any of a set of characters from the start and end of a String.
     * This is similar to {@link String#trim()} but allows the characters
     * to be stripped to be controlled.</p>
     *
     * <p>A <code>null</code> input String returns <code>null</code>.</p>
     * 
     * <p>If the stripChars String is <code>null</code>, whitespace is
     * stripped as defined by {@link Character#isWhitespace(char)}.
     * Alternatively use {@link #strip(String)}.</p>
     * 
     * <pre>
     * StringUtils.strip(null, null)       = null
     * StringUtils.strip("abc", null)      = "abc"
     * StringUtils.strip("  abc", null)    = "abc"
     * StringUtils.strip("abc  ", null)    = "abc"
     * StringUtils.strip(" abc ", null)    = "abc"
     * StringUtils.strip("  abcyx", "xyz") = "  abc"
     * </pre>
     * 
     * @param str  the String to remove characters from, may be null
     * @param stripChars  the characters to remove, null treated as whitespace
     * @return the stripped String, <code>null</code> if null String input
     */
    public static String strip(String str, String stripChars) {
        str = stripStart(str, stripChars);
        return stripEnd(str, stripChars);
    }

    /**
     * <p>Strips any of a set of characters from the start of a String.</p>
     *
     * <p>A <code>null</code> input String returns <code>null</code>.</p>
     * 
     * <p>If the stripChars String is <code>null</code>, whitespace is
     * stripped as defined by {@link Character#isWhitespace(char)}.</p>
     * 
     * <pre>
     * StringUtils.strip(null, null)       = null
     * StringUtils.strip("abc", null)      = "abc"
     * StringUtils.strip("  abc", null)    = "abc"
     * StringUtils.strip("abc  ", null)    = "abc  "
     * StringUtils.strip(" abc ", null)    = "abc "
     * StringUtils.strip("yxabc  ", "xyz") = "abc  "
     * </pre>
     * 
     * @param str  the String to remove characters from, may be null
     * @param stripChars  the characters to remove, null treated as whitespace
     * @return the stripped String, <code>null</code> if null String input
     */
    public static String stripStart(String str, String stripChars) {
        int strLen;
        if (str == null || (strLen = str.length()) == 0) {
            return str;
        }
        int start = 0;
        if (stripChars == null) {
            while ((start != strLen) && Character.isWhitespace(str.charAt(start))) {
                start++;
            }
        } else {
            while ((start != strLen) && (stripChars.indexOf(str.charAt(start)) != -1)) {
                start++;
            }
        }
        return str.substring(start);
    }

    /**
     * <p>Strips any of a set of characters from the end of a String.</p>
     *
     * <p>A <code>null</code> input String returns <code>null</code>.</p>
     * 
     * <p>If the stripChars String is <code>null</code>, whitespace is
     * stripped as defined by {@link Character#isWhitespace(char)}.</p>
     * 
     * <pre>
     * StringUtils.strip(null, null)       = null
     * StringUtils.strip("abc", null)      = "abc"
     * StringUtils.strip("  abc", null)    = "  abc"
     * StringUtils.strip("abc  ", null)    = "abc"
     * StringUtils.strip(" abc ", null)    = " abc"
     * StringUtils.strip("  abcyx", "xyz") = "  abc"
     * </pre>
     * 
     * @param str  the String to remove characters from, may be null
     * @param stripChars  the characters to remove, null treated as whitespace
     * @return the stripped String, <code>null</code> if null String input
     */
    public static String stripEnd(String str, String stripChars) {
        int end;
        if (str == null || (end = str.length()) == 0) {
            return str;
        }
 
        if (stripChars == null) {
            while ((end != 0) && Character.isWhitespace(str.charAt(end - 1))) {
                end--;
            }
        } else {
            while ((end != 0) && (stripChars.indexOf(str.charAt(end - 1)) != -1)) {
                end--;
            }
        }
        return str.substring(0, end);
    }

    /**
     * <p>Strips whitespace from the start and end of every String in an array.
     * Whitespace is defined by {@link Character#isWhitespace(char)}.</p>
     *
     * <p>A new array is returned each time, except for length zero.
     * A <code>null</code> array will return <code>null</code>.
     * An empty array will return itself.
     * A <code>null</code> array entry will be ignored.</p>
     * 
     * <pre>
     * StringUtils.stripAll(null)             = null
     * StringUtils.stripAll([])               = []
     * StringUtils.stripAll(["abc", "  abc"]) = ["abc", "abc"]
     * StringUtils.stripAll(["abc  ", null])  = ["abc", null]
     * </pre>
     * 
     * @param str  the array to remove whitespace from, may be null
     * @return the stripped Strings, <code>null</code> if null array input
     */
    public static String[] stripAll(String[] strs) {
        return stripAll(strs, null);
    }
 
    /**
     * <p>Strips any of a set of characters from the start and end of every
     * String in an array.</p>
     * Whitespace is defined by {@link Character#isWhitespace(char)}.</p>
     *
     * <p>A new array is returned each time, except for length zero.
     * A <code>null</code> array will return <code>null</code>.
     * An empty array will return itself.
     * A <code>null</code> array entry will be ignored.
     * A <code>null</code> stripChars will strip whitespace as defined by
     * {@link Character#isWhitespace(char)}.</p>
     * 
     * <pre>
     * StringUtils.stripAll(null, null)             = null
     * StringUtils.stripAll([], null)               = []
     * StringUtils.stripAll(["abc", "  abc"], null) = ["abc", "abc"]
     * StringUtils.stripAll(["abc  ", null], null)  = ["abc", null]
     * StringUtils.stripAll(["abc  ", null], "yz")  = ["abc  ", null]
     * StringUtils.stripAll(["yabcz", null], "yz")  = ["abc", null]
     * </pre>
     * 
     * @param str  the array to remove characters from, may be null
     * @param stripChars  the characters to remove, null treated as whitespace
     * @return the stripped Strings, <code>null</code> if null array input
     */
    public static String[] stripAll(String[] strs, String stripChars) {
        int strsLen;
        if (strs == null || (strsLen = strs.length) == 0) {
            return strs;
        }
        String[] newArr = new String[strsLen];
        for (int i = 0; i < strsLen; i++) {
            newArr[i] = strip(strs[i], stripChars);
        }
        return newArr;
    }   

    // Case conversion
    //-----------------------------------------------------------------------
    
    /**
     * <p>Converts a String to upper case as per {@link String#toUpperCase()}.</p>
     * 
     * <p>A <code>null</code> input String returns <code>null</code>.</p>
     * 
     * <pre>
     * StringUtils.upperCase(null)  = null
     * StringUtils.upperCase("aBc") = "ABC"
     * </pre>
     * 
     * @param str  the String to upper case, may be null
     * @return the upper cased String, <code>null</code> if null String input
     */
    public static String upperCase(String str) {
        if (str == null) {
            return null;
        }
        return str.toUpperCase();
    }

    /**
     * <p>Converts a String to lower case as per {@link String#toLowerCase()}.</p>
     * 
     * <p>A <code>null</code> input String returns <code>null</code>.</p>
     * 
     * <pre>
     * StringUtils.lowerCase(null)  = null
     * StringUtils.lowerCase("aBc") = "abc"
     * </pre>
     * 
     * @param str  the String to lower case, may be null
     * @return the lower cased String, <code>null</code> if null String input
     */
    public static String lowerCase(String str) {
        if (str == null) {
            return null;
        }
        return str.toLowerCase();
    }

    /**
     * <p>Capitalises a String changing the first letter to title case as
     * per {@link Character#toTitleCase()}. No other letters are changed.</p>
     * 
     * <p>A <code>null</code> input String returns <code>null</code>.</p>
     * 
     * <pre>
     * StringUtils.capitalise(null)  = null
     * StringUtils.capitalise("cat") = "Cat"
     * StringUtils.capitalise("cAt") = "CAt"
     * </pre>
     * 
     * @param str  the String to capitalise, may be null
     * @return the capitalised String, <code>null</code> if null String input
     */
    public static String capitalise(String str) {
        int strLen;
        if (str == null || (strLen = str.length()) == 0) {
            return str;
        }
        return new StringBuffer(strLen)
            .append(Character.toTitleCase(str.charAt(0)))
            .append(str.substring(1))
            .toString();
    }

    /**
     * <p>Uncapitalises a String changing the first letter to title case as
     * per {@link Character#toLowerCase()}. No other letters are changed.</p>
     * 
     * <p>A <code>null</code> input String returns <code>null</code>.</p>
     * 
     * <pre>
     * StringUtils.uncapitalise(null)  = null
     * StringUtils.uncapitalise("Cat") = "cat"
     * StringUtils.uncapitalise("CAT") = "cAT"
     * </pre>
     * 
     * @param str  the String to uncapitalise, may be null
     * @return the uncapitalised String, <code>null</code> if null String input
     */
    public static String uncapitalise(String str) {
        int strLen;
        if (str == null || (strLen = str.length()) == 0) {
            return str;
        }
        return new StringBuffer(strLen)
            .append(Character.toLowerCase(str.charAt(0)))
            .append(str.substring(1))
            .toString();
    }

    /**
     * <p>Swaps the case of a String using a word based algorithm.</p>
     * 
     * <ul>
     *  <li>Upper case character converts to Lower case
     *  <li>Title case character converts to Lower case
     *  <li>Lower case character after Whitespace or at start converts to Title case
     *  <li>Other Lower case character converts to Upper case
     * </ul>
     * 
     * <p>Whitespace is defined by {@link Character#isWhitespace(char)}.
     * A <code>null</code> input String returns <code>null</code>.</p>
     * 
     * <pre>
     * StringUtils.swapCase(null)                 = null
     * StringUtils.swapCase("The dog has a BONE") = "tHE DOG HAS A bone"
     * </pre>
     * 
     * @param str  the String to swap case, may be null
     * @return the changed String, <code>null</code> if null String input
     */
    public static String swapCase(String str) {
        int strLen;
        if (str == null || (strLen = str.length()) == 0) {
            return str;
        }
        StringBuffer buffer = new StringBuffer(strLen);

        boolean whitespace = true;
        char ch = 0;
        char tmp = 0;

        for (int i = 0; i < strLen; i++) {
            ch = str.charAt(i);
            if (Character.isUpperCase(ch)) {
                tmp = Character.toLowerCase(ch);
            } else if (Character.isTitleCase(ch)) {
                tmp = Character.toLowerCase(ch);
            } else if (Character.isLowerCase(ch)) {
                if (whitespace) {
                    tmp = Character.toTitleCase(ch);
                } else {
                    tmp = Character.toUpperCase(ch);
                }
            } else {
                tmp = ch;
            }
            buffer.append(tmp);
            whitespace = Character.isWhitespace(ch);
        }
        return buffer.toString();
    }

    /**
     * <p>Capitalises all the whitespace separated words in a String.
     * Only the first letter of each word is changed.</p>
     *
     * <p>Whitespace is defined by {@link Character#isWhitespace(char)}.
     * A <code>null</code> input String returns <code>null</code>.</p>
     *
     * <pre>
     * StringUtils.capitaliseAllWords(null)        = null
     * StringUtils.capitaliseAllWords("")          = ""
     * StringUtils.capitaliseAllWords("i am FINE") = "I Am FINE"
     * </pre>
     * 
     * @param str  the String to capitalise, may be null
     * @return capitalised String, <code>null</code> if null String input
     */
    public static String capitaliseAllWords(String str) {
        int strLen;
        if (str == null || (strLen = str.length()) == 0) {
            return str;
        }
        StringBuffer buffer = new StringBuffer(strLen);
        boolean whitespace = true;
        for (int i = 0; i < strLen; i++) {
            char ch = str.charAt(i);
            if (Character.isWhitespace(ch)) {
                buffer.append(ch);
                whitespace = true;
            } else if (whitespace) {
                buffer.append(Character.toTitleCase(ch));
                whitespace = false;
            } else {
                buffer.append(ch);
            }
        }
        return buffer.toString();
    }

    /**
     * <p>Uncapitalises all the whitespace separated words in a String.
     * Only the first letter of each word is changed.</p>
     *
     * <p>Whitespace is defined by {@link Character#isWhitespace(char)}.
     * A <code>null</code> input String returns <code>null</code>.</p>
     *
     * <pre>
     * StringUtils.uncapitaliseAllWords(null)        = null
     * StringUtils.uncapitaliseAllWords("")          = ""
     * StringUtils.uncapitaliseAllWords("I Am FINE") = "i am fINE"
     * </pre>
     * 
     * @param str  the String to uncapitalise, may be null
     * @return uncapitalised String, <code>null</code> if null String input
     */
    public static String uncapitaliseAllWords(String str) {
        int strLen;
        if (str == null || (strLen = str.length()) == 0) {
            return str;
        }
        StringBuffer buffer = new StringBuffer(strLen);
        boolean whitespace = true;
        for (int i = 0; i < strLen; i++) {
            char ch = str.charAt(i);
            if (Character.isWhitespace(ch)) {
                buffer.append(ch);
                whitespace = true;
            } else if (whitespace) {
                buffer.append(Character.toLowerCase(ch));
                whitespace = false;
            } else {
                buffer.append(ch);
            }
        }
        return buffer.toString();
    }

    // Nested extraction
    //-----------------------------------------------------------------------
    
    /**
     * <p>Gets the String that is nested in between two instances of the
     * same String.</p>
     *
     * <p>A <code>null</code> input String returns <code>null</code>.
     * A <code>null</code> tag returns <code>null</code>.</p>
     * 
     * <pre>
     * StringUtils.getNestedString(null, "tag")        = null
     * StringUtils.getNestedString("tagabctag", null)  = null
     * StringUtils.getNestedString("tagabctag", "")    = ""
     * StringUtils.getNestedString("tagabctag", "tag") = "abc"
     * </pre>
     *
     * @param str  the String containing nested-string, may be null
     * @param tag  the String before and after nested-string, may be null
     * @return the nested String, <code>null</code> if no match
     */
    public static String getNestedString(String str, String tag) {
        return getNestedString(str, tag, tag);
    }
    
    /**
     * <p>Gets the String that is nested in between two Strings.
     * Only the first match is returned.</p>
     * 
     * <p>A <code>null</code> input String returns <code>null</code>.
     * A <code>null</code> open/close returns <code>null</code> (no match).
     * An empty ("") open/close returns an empty string.</p>
     *
     * <pre>
     * StringUtils.getNestedString(null, "y", "z")      = null
     * StringUtils.getNestedString("yabcz", null, null) = null
     * StringUtils.getNestedString("yabcz", "", "")     = ""
     * StringUtils.getNestedString("yabcz", "y", "z")   = "abc"
     * StringUtils.getNestedString("yabczyabcz", "y", "z")   = "abc"
     * </pre>
     *
     * @param str  the String containing nested-string, may be null
     * @param open  the String before nested-string, may be null
     * @param close  the String after nested-string, may be null
     * @return the nested String, <code>null</code> if no match
     */
    public static String getNestedString(String str, String open, String close) {
        if (str == null || open == null || close == null) {
            return null;
        }
        int start = str.indexOf(open);
        if (start != -1) {
            int end = str.indexOf(close, start + open.length());
            if (end != -1) {
                return str.substring(start + open.length(), end);
            }
        }
        return null;
    }

    // Count matches
    //-----------------------------------------------------------------------
    
    /**
     * <p>How many times is the substring in the larger String.</p>
     *
     * <p><code>null</code> returns <code>0</code>.</p>
     * 
     * @param str  the String to check, may be null
     * @param sub  the substring to count, may be null
     * @return the number of occurances, 0 if either String is <code>null</code>
     */
    public static int countMatches(String str, String sub) {
        if (str == null || str.length() == 0 || sub == null || sub.length() == 0) {
            return 0;
        }
        int count = 0;
        int idx = 0;
        while ((idx = str.indexOf(sub, idx)) != -1) {
            count++;
            idx += sub.length();
        }
        return count;
    }

    // Character Tests
    //-----------------------------------------------------------------------
    
    /**
     * <p>Checks if the String contains only unicode letters.</p>
     *
     * <p><code>null</code> will return <code>false</code>.
     * An empty String ("") will return <code>true</code>.</p>
     * 
     * @param str  the String to check, may be null
     * @return <code>true</code> if only contains letters, and is non-null
     */
    public static boolean isAlpha(String str) {
        if (str == null) {
            return false;
        }
        int sz = str.length();
        for (int i = 0; i < sz; i++) {
            if (Character.isLetter(str.charAt(i)) == false) {
                return false;
            }
        }
        return true;
    }

    /**
     * <p>Checks if the String contains only whitespace.</p>
     *
     * <p><code>null</code> will return <code>false</code>.
     * An empty String ("") will return <code>true</code>.</p>
     * 
     * @param str  the String to check, may be null
     * @return <code>true</code> if only contains whitespace, and is non-null
     */
    public static boolean isWhitespace(String str) {
        if (str == null) {
            return false;
        }
        int sz = str.length();
        for (int i = 0; i < sz; i++) {
            if ((Character.isWhitespace(str.charAt(i)) == false) ) {
                return false;
            }
        }
        return true;
    }

    /**
     * <p>Checks if the String contains only unicode letters and
     * space (' ').</p>
     *
     * <p><code>null</code> will return <code>false</code>
     * An empty String ("") will return <code>true</code>.</p>
     * 
     * @param str  the String to check, may be null
     * @return <code>true</code> if only contains letters and space,
     *  and is non-null
     */
    public static boolean isAlphaSpace(String str) {
        if (str == null) {
            return false;
        }
        int sz = str.length();
        for (int i = 0; i < sz; i++) {
            if ((Character.isLetter(str.charAt(i)) == false) &&
                (str.charAt(i) != ' ')) {
                return false;
            }
        }
        return true;
    }

    /**
     * <p>Checks if the String contains only unicode letters or digits.</p>
     *
     * <p><code>null</code> will return <code>false</code>.
     * An empty String ("") will return <code>true</code>.</p>
     * 
     * @param str  the String to check, may be null
     * @return <code>true</code> if only contains letters or digits,
     *  and is non-null
     */
    public static boolean isAlphanumeric(String str) {
        if (str == null) {
            return false;
        }
        int sz = str.length();
        for (int i = 0; i < sz; i++) {
            if (Character.isLetterOrDigit(str.charAt(i)) == false) {
                return false;
            }
        }
        return true;
    }

    /**
     * <p>Checks if the String contains only unicode letters, digits
     * or space (<code>' '</code>).</p>
     *
     * <p><code>null</code> will return <code>false</code>.
     * An empty String ("") will return <code>true</code>.</p>
     * 
     * @param str  the String to check, may be null
     * @return <code>true</code> if only contains letters, digits or space,
     *  and is non-null
     */
    public static boolean isAlphanumericSpace(String str) {
        if (str == null) {
            return false;
        }
        int sz = str.length();
        for (int i = 0; i < sz; i++) {
            if ((Character.isLetterOrDigit(str.charAt(i)) == false) &&
                (str.charAt(i) != ' ')) {
                return false;
            }
        }
        return true;
    }

    /**
     * <p>Checks if the String contains only unicode digits.</p>
     *
     * <p><code>null</code> will return <code>false</code>.
     * An empty String ("") will return <code>true</code>.</p>
     * 
     * @param str  the String to check, may be null
     * @return <code>true</code> if only contains digits, and is non-null
     */
    public static boolean isNumeric(String str) {
        if (str == null) {
            return false;
        }
        int sz = str.length();
        for (int i = 0; i < sz; i++) {
            if (Character.isDigit(str.charAt(i)) == false) {
                return false;
            }
        }
        return true;
    }

    /**
     * <p>Checks if the String contains only unicode digits or space
     * (<code>' '</code>).</p>
     *
     * <p><code>null</code> will return <code>false</code>.
     * An empty String ("") will return <code>true</code>.</p>
     * 
     * @param str  the String to check, may be null
     * @return <code>true</code> if only contains digits or space,
     *  and is non-null
     */
    public static boolean isNumericSpace(String str) {
        if (str == null) {
            return false;
        }
        int sz = str.length();
        for (int i = 0; i < sz; i++) {
            if ((Character.isDigit(str.charAt(i)) == false) &&
                (str.charAt(i) != ' ')) {
                return false;
            }
        }
        return true;
    }

    // Defaults
    //-----------------------------------------------------------------------
    
    /**
     * <p>Returns either the passed in String, 
     * or if the String is <code>null</code>, an empty String ("").</p>
     * 
     * <pre>
     * StringUtils.defaultString(null)  = ""
     * StringUtils.defaultString("")    = ""
     * StringUtils.defaultString("bat") = "bat"
     * </pre>
     * 
     * @param str  the String to check, may be null
     * @return the passed in String, or the empty String if it
     *  was <code>null</code>
     */
    public static String defaultString(String str) {
        return (str == null ? "" : str);
    }

    /**
     * <p>Returns either the passed in <code>Object</code> as a String,
     * or, if the <code>Object</code> is <code>null</code>,
     * an empty String ("").</p>
     * 
     * <pre>
     * StringUtils.defaultString(null)         = "null"
     * StringUtils.defaultString("")           = ""
     * StringUtils.defaultString("bat")        = "bat"
     * StringUtils.defaultString(Boolean.TRUE) = "true"
     * </pre>
     * 
     * @param obj  the Object to check, using <code>toString()</code>, may be null
     * @return the passed in Object's toString, or the empty String if it
     *  was <code>null</code>
     */
    public static String defaultString(Object obj) {
        return (obj == null ? "" : obj.toString());
    }

    /**
     * <p>Returns either the passed in String, 
     * or if the String is <code>null</code>, an empty String ("").</p>
     * 
     * <pre>
     * StringUtils.defaultString(null, "null")  = "null"
     * StringUtils.defaultString("", "null")    = ""
     * StringUtils.defaultString("bat", "null") = "bat"
     * </pre>
     * 
     * @param str  the String to check, may be null
     * @param defaultStr  the default String to return 
     *  if the input is <code>null</code>, may be null
     * @return the passed in String, or the default if it was <code>null</code>
     */
    public static String defaultString(String str, String defaultStr) {
        return (str == null ? defaultStr : str);
    }

    /**
     * <p>Returns either the passed in <code>Object</code> as a String,
     * or, if the <code>Object</code> is <code>null</code>, a passed
     * in default String.</p>
     * 
     * <pre>
     * StringUtils.defaultString(null, "null")         = "null"
     * StringUtils.defaultString("", "null")           = ""
     * StringUtils.defaultString("bat", "null")        = "bat"
     * StringUtils.defaultString(Boolean.TRUE, "null") = "true"
     * </pre>
     * 
     * @param obj  the Object to check, using <code>toString()</code>, may be null
     * @param defaultStr  the default String to return 
     *  if the input is <code>null</code>, may be null
     * @return the passed in Object's toString, or the default if it was <code>null</code>
     */
    public static String defaultString(Object obj, String defaultStr) {
        return (obj == null ? defaultStr : obj.toString());
    }

    // Reversing
    //-----------------------------------------------------------------------

    /**
     * <p>Reverse a String as per {@link StringBuffer#reverse()}.</p>
     *
     * <p><A code>null</code> String returns <code>null</code>.</p>
     * 
     * <pre>
     * StringUtils.reverse(null)  = null
     * StringUtils.reverse("")    = ""
     * StringUtils.reverse("bat") = "tab"
     * </pre>
     * 
     * @param str  the String to reverse, may be null
     * @return the reversed String, <code>null</code> if null String input
     */
    public static String reverse(String str) {
        if (str == null) {
            return null;
        }
        return new StringBuffer(str).reverse().toString();
    }

    /**
     * <p>Reverses a String that is delimited by a specific character.</p>
     *
     * <p>The Strings between the delimiters are not reversed.
     * Thus java.lang.String becomes String.lang.java (if the delimiter
     * is <code>'.'</code>).</p>
     * 
     * <pre>
     * StringUtils.reverseDelimitedString(null, '.')    = null
     * StringUtils.reverseDelimitedString("", '.')      = ""
     * StringUtils.reverseDelimitedString("a.b.c", 'x') = "a.b.c"
     * StringUtils.reverseDelimitedString("a.b.c", ".") = "c.b.a"
     * </pre>
     * 
     * @param str  the String to reverse, may be null
     * @param separatorChar  the separator character to use
     * @return the reversed String, <code>null</code> if null String input
     */
    public static String reverseDelimitedString(String str, char separatorChar) {
        if (str == null) {
            return null;
        }
        // could implement manually, but simple way is to reuse other, 
        // probably slower, methods.
        String[] strs = split(str, separatorChar);
        ArrayUtils.reverse(strs);
        return join(strs, separatorChar);
    }

    /**
     * <p>Reverses a String that is delimited by a specific character.</p>
     *
     * <p>The Strings between the delimiters are not reversed.
     * Thus java.lang.String becomes String.lang.java (if the delimiter
     * is <code>"."</code>).</p>
     * 
     * <pre>
     * StringUtils.reverseDelimitedString(null, null)    = null
     * StringUtils.reverseDelimitedString("", null)      = ""
     * StringUtils.reverseDelimitedString("a.b.c", null) = "a.b.c"
     * StringUtils.reverseDelimitedString("a.b.c", ".")  = "c.b.a"
     * </pre>
     * 
     * @param str  the String to reverse, may be null
     * @param separatorChars  the separator characters to use, null treated as whitespace
     * @return the reversed String, <code>null</code> if null String input
     * @deprecated Use {@link #reverseDelimitedString(String, char)} instead.
     *      This method is broken as the join doesn't know which char to use.
     *      Method will be removed in Commons Lang 3.0.
     * 
     */
    public static String reverseDelimitedString(String str, String separatorChars) {
        if (str == null) {
            return null;
        }
        // could implement manually, but simple way is to reuse other, 
        // probably slower, methods.
        String[] strs = split(str, separatorChars);
        ArrayUtils.reverse(strs);
        if (separatorChars == null) {
            return join(strs, ' ');
        }
        return join(strs, separatorChars);
    }

    // Abbreviating
    //-----------------------------------------------------------------------

    /**
     * <p>Turn "Now is the time for all good men" into "Now is the time for..."</p>
     *
     * <p>Specifically:
     * <ul>
     *   <li>If <code>str</code> is less than <code>maxWidth</code> characters
     *       long, return it.</li>
     *   <li>Else abbreviate it to <code>(substring(str, 0, max-3) + "...")</code>.</li>
     *   <li>If <code>maxWidth</code> is less than </code>3, throw an
     *       <code>IllegalArgumentException</code>.</li>
     *   <li>In no case will it return a String of length greater than
     *       <code>maxWidth</code>.</li>
     * </ul>
     * </p>
     *
     * @param str  the String to check, may be null
     * @param maxWidth  maximum length of result String, must be at least 4
     * @return abbreviated String, <code>null</code> if null String input
     * @throws IllegalArgumentException if the width is too small
     */
    public static String abbreviate(String str, int maxWidth) {
        return abbreviate(str, 0, maxWidth);
    }

    /**
     * <p>Turn "Now is the time for all good men" into "...is the time for..."</p>
     *
     * <p>Works like <code>abbreviate(String, int)</code>, but allows you to specify
     * a "left edge" offset.  Note that this left edge is not necessarily going to
     * be the leftmost character in the result, or the first character following the
     * ellipses, but it will appear somewhere in the result.
     *
     * <p>In no case will it return a String of length greater than
     * <code>maxWidth</code>.</p>
     *
     * @param str  the String to check, may be null
     * @param offset  left edge of source String
     * @param maxWidth  maximum length of result String, must be at least 4
     * @return abbreviated String, <code>null</code> if null String input
     * @throws IllegalArgumentException if the width is too small
     */
    public static String abbreviate(String str, int offset, int maxWidth) {
        if (str == null) {
            return null;
        }
        if (maxWidth < 4) {
            throw new IllegalArgumentException("Minimum abbreviation width is 4");
        }
        if (str.length() <= maxWidth) {
            return str;
        }
        if (offset > str.length()) {
            offset = str.length();
        }
        if ((str.length() - offset) < (maxWidth - 3)) {
            offset = str.length() - (maxWidth - 3);
        }
        if (offset <= 4) {
            return str.substring(0, maxWidth - 3) + "...";
        }
        if (maxWidth < 7) {
            throw new IllegalArgumentException("Minimum abbreviation width with offset is 7");
        }
        if ((offset + (maxWidth - 3)) < str.length()) {
            return "..." + abbreviate(str.substring(offset), maxWidth - 3);
        }
        return "..." + str.substring(str.length() - (maxWidth - 3));
    }

    // Difference
    //-----------------------------------------------------------------------

    /**
     * <p>Compare two Strings, and return the portion where they differ.
     * (More precisely, return the remainder of the second String,
     * starting from where it's different from the first.)</p>
     *
     * <p>For example,
     * <code>difference("i am a machine", "i am a robot") -> "robot"</code>.</p>
     *
     * @param str1  the first String, may be null
     * @param str2  the second String, may be null
     * @return the portion of str2 where it differs from str1; returns the 
     * empty String if they are equal
     */
    public static String difference(String str1, String str2) {
        if (str1 == null) {
            return str2;
        }
        if (str2 == null) {
            return str1;
        }
        int at = differenceAt(str1, str2);
        if (at == -1) {
            return "";
        }
        return str2.substring(at);
    }

    /**
     * <p>Compare two Strings, and return the index at which the
     * Strings begin to differ.</p>
     * 
     * <p>For example, 
     * <code>differenceAt("i am a machine", "i am a robot") -> 7</code></p>
     *
     * @param str1  the first String, may be null
     * @param str2  the second String, may be null
     * @return the index where str2 and str1 begin to differ; -1 if they are equal
     */
    public static int differenceAt(String str1, String str2) {
        if (str1 == str2) {
            return -1;
        }
        if (str1 == null || str2 == null) {
            return 0;
        }
        int i;
        for (i = 0; i < str1.length() && i < str2.length(); ++i) {
            if (str1.charAt(i) != str2.charAt(i)) {
                break;
            }
        }
        if (i < str2.length() || i < str1.length()) {
            return i;
        }
        return -1;
    }


    // Misc
    //-----------------------------------------------------------------------

    /**
     * <p>Find the Levenshtein distance between two Strings.</p>
     *
     * <p>This is the number of changes needed to change one String into
     * another. Where each change is a single character modification.</p>
     *
     * <p>This implemmentation of the levenshtein distance algorithm
     * is from <a href="http://www.merriampark.com/ld.htm">http://www.merriampark.com/ld.htm</a></p>
     * 
     * @param s  the first String, must not be null
     * @param t  the second String, must not be null
     * @return result distance
     * @throws IllegalArgumentException if either String input <code>null</code>
     */
    public static int getLevenshteinDistance(String s, String t) {
        if (s == null || t == null) {
            throw new IllegalArgumentException("Strings must not be null");
        }
        int d[][]; // matrix
        int n; // length of s
        int m; // length of t
        int i; // iterates through s
        int j; // iterates through t
        char s_i; // ith character of s
        char t_j; // jth character of t
        int cost; // cost

        // Step 1
        n = s.length();
        m = t.length();
        if (n == 0) {
            return m;
        }
        if (m == 0) {
            return n;
        }
        d = new int[n + 1][m + 1];

        // Step 2
        for (i = 0; i <= n; i++) {
            d[i][0] = i;
        }

        for (j = 0; j <= m; j++) {
            d[0][j] = j;
        }

        // Step 3
        for (i = 1; i <= n; i++) {
            s_i = s.charAt(i - 1);

            // Step 4
            for (j = 1; j <= m; j++) {
                t_j = t.charAt(j - 1);

                // Step 5
                if (s_i == t_j) {
                    cost = 0;
                } else {
                    cost = 1;
                }

                // Step 6
                d[i][j] = NumberUtils.min(d[i - 1][j] + 1, d[i][j - 1] + 1, d[i - 1][j - 1] + cost);
            }
        }

        // Step 7
        return d[n][m];
    }

}

