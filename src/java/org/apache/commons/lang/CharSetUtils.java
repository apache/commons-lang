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

/**
 * <p>Numerous routines to manipulate a <code>CharSet</code>.</p>
 *
 * <p>This class handles <code>null</code> input gracefully.
 * An exception will not be thrown for a <code>null</code> input.
 * Each method documents its behaviour in more detail.</p>
 * 
 * @author <a href="bayard@generationjava.com">Henri Yandell</a>
 * @author Stephen Colebourne
 * @author Phil Steitz
 * @since 1.0
 * @version $Id: CharSetUtils.java,v 1.22 2003/08/06 00:02:15 stevencaswell Exp $
 */
public class CharSetUtils {

    /**
     * <p>CharSetUtils instances should NOT be constructed in standard programming.
     * Instead, the class should be used as <code>CharSetUtils.evaluateSet(null);</code>.</p>
     *
     * <p>This constructor is public to permit tools that require a JavaBean instance
     * to operate.</p>
     */
    public CharSetUtils() {
    }

    // Factory
    //-----------------------------------------------------------------------
    /**
     * <p>Creates a <code>CharSet</code> instance which allows a certain amount of
     * set logic to be performed.</p>
     * <p>The syntax is:</p>
     * <ul>
     *  <li>&quot;aeio&quot; which implies 'a','e',..</li>
     *  <li>&quot;^e&quot; implies not e.</li>
     *  <li>&quot;ej-m&quot; implies e,j->m. e,j,k,l,m.</li>
     * </ul>
     * 
     * <pre>
     * CharSetUtils.evaluateSet(null)  = null
     * CharSetUtils.evaluateSet("")    = CharSet matching nothing
     * CharSetUtils.evaluateSet("a-e") = CharSet matching a,b,c,d,e
     * CharSetUtils.evaluateSet("abe-g") = CharSet matching a,b,e,f,g
     * </pre>
     *
     * @param setStr  the set, may be null
     * @return a CharSet instance, <code>null</code> if null input
     * @deprecated Use {@link CharSet#getInstance(String)}.
     *             Method will be removed in Commons Lang 3.0.
     */
    public static CharSet evaluateSet(String setStr) {
        if (setStr == null) {
            return null;
        }
        return CharSet.getInstance(setStr); 
    }

    /**
     * <p>Creates a <code>CharSet</code> instance which allows a certain amount of
     * set logic to be performed.</p>
     * <p>The syntax is:</p>
     * <ul>
     *  <li>&quot;aeio&quot; which implies 'a','e',..</li>
     *  <li>&quot;^e&quot; implies not e.</li>
     *  <li>&quot;ej-m&quot; implies e,j->m. e,j,k,l,m.</li>
     * </ul>
     * 
     * <pre>
     * CharSetUtils.evaluateSet(null)    = null
     * CharSetUtils.evaluateSet([])      = CharSet matching nothing
     * CharSetUtils.evaluateSet(["a-e"]) = CharSet matching a,b,c,d,e
     * </pre>
     *
     * @param set  the set, may be null
     * @return a CharSet instance, <code>null</code> if null input
     * @deprecated Use {@link CharSet#getInstance(String)}.
     *             Method will be removed in Commons Lang 3.0.
     */
    public static CharSet evaluateSet(String[] set) {
        if (set == null) {
            return null;
        }
        return new CharSet(set); 
    }

    // Squeeze
    //-----------------------------------------------------------------------
    /**
     * <p>Squeezes any repititions of a character that is mentioned in the
     * supplied set.</p>
     *
     * <pre>
     * CharSetUtils.squeeze(null, *)        = null
     * CharSetUtils.squeeze("", *)          = ""
     * CharSetUtils.squeeze(*, null)        = *
     * CharSetUtils.squeeze(*, "")          = *
     * CharSetUtils.squeeze("hello", "k-p") = "helo"
     * CharSetUtils.squeeze("hello", "a-e") = "hello"
     * </pre>
     *
     * @see #evaluateSet(java.lang.String[]) for set-syntax.
     * @param str  the string to squeeze, may be null
     * @param set  the character set to use for manipulation, may be null
     * @return modified String, <code>null</code> if null string input
     */
    public static String squeeze(String str, String set) {
        if (str == null || str.length() == 0 || set == null || set.length() == 0) {
            return str;
        }
        String[] strs = new String[1];
        strs[0] = set;
        return squeeze(str, strs);
    }

    /**
     * <p>Squeezes any repititions of a character that is mentioned in the
     * supplied set.</p>
     *
     * <p>An example is:</p>
     * <ul>
     *   <li>squeeze(&quot;hello&quot;, {&quot;el&quot;}) => &quot;helo&quot;</li>
     * </ul>
     * 
     * @see #evaluateSet(java.lang.String[]) for set-syntax.
     * @param str  the string to squeeze, may be null
     * @param set  the character set to use for manipulation, may be null
     * @return modified String, <code>null</code> if null string input
     */
    public static String squeeze(String str, String[] set) {
        if (str == null || str.length() == 0 || set == null || set.length == 0) {
            return str;
        }
        CharSet chars = evaluateSet(set);
        StringBuffer buffer = new StringBuffer(str.length());
        char[] chrs = str.toCharArray();
        int sz = chrs.length;
        char lastChar = ' ';
        char ch = ' ';
        for (int i = 0; i < sz; i++) {
            ch = chrs[i];
            if (chars.contains(ch)) {
                if ((ch == lastChar) && (i != 0)) {
                    continue;
                }
            }
            buffer.append(ch);
            lastChar = ch;
        }
        return buffer.toString();
    }

    // Count
    //-----------------------------------------------------------------------
    /**
     * <p>Takes an argument in set-syntax, see evaluateSet,
     * and returns the number of characters present in the specified string.</p>
     *
     * <pre>
     * CharSetUtils.count(null, *)        = 0
     * CharSetUtils.count("", *)          = 0
     * CharSetUtils.count(*, null)        = 0
     * CharSetUtils.count(*, "")          = 0
     * CharSetUtils.count("hello", "k-p") = 3
     * CharSetUtils.count("hello", "a-e") = 1
     * </pre>
     *
     * @see #evaluateSet(java.lang.String[]) for set-syntax.
     * @param str  String to count characters in, may be null
     * @param set  String set of characters to count, may be null
     * @return character count, zero if null string input
     */
    public static int count(String str, String set) {
        if (str == null || str.length() == 0 || set == null || set.length() == 0) {
            return 0;
        }
        String[] strs = new String[1];
        strs[0] = set;
        return count(str, strs);
    }
    
    /**
     * <p>Takes an argument in set-syntax, see evaluateSet,
     * and returns the number of characters present in the specified string.</p>
     *
     * <p>An example would be:</p>
     * <ul>
     *  <li>count(&quot;hello&quot;, {&quot;c-f&quot;, &quot;o&quot;}) returns 2.</li>
     * </ul>
     *
     * @see #evaluateSet(java.lang.String[]) for set-syntax.
     * @param str  String to count characters in, may be null
     * @param set  String[] set of characters to count, may be null
     * @return character count, zero if null string input
     */
    public static int count(String str, String[] set) {
        if (str == null || str.length() == 0 || set == null || set.length == 0) {
            return 0;
        }
        CharSet chars = evaluateSet(set);
        int count = 0;
        char[] chrs = str.toCharArray();
        int sz = chrs.length;
        for(int i=0; i<sz; i++) {
            if(chars.contains(chrs[i])) {
                count++;
            }
        }
        return count;
    }

    // Keep
    //-----------------------------------------------------------------------
    /**
     * <p>Takes an argument in set-syntax, see evaluateSet,
     * and keeps any of characters present in the specified string.</p>
     *
     * <pre>
     * CharSetUtils.keep(null, *)        = null
     * CharSetUtils.keep("", *)          = ""
     * CharSetUtils.keep(*, null)        = ""
     * CharSetUtils.keep(*, "")          = ""
     * CharSetUtils.keep("hello", "hl") = "hll"
     * CharSetUtils.keep("hello", "le") = "ell"
     * </pre>
     *
     * @see #evaluateSet(java.lang.String[]) for set-syntax.
     * @param str  String to keep characters from, may be null
     * @param set  String set of characters to keep, may be null
     * @return modified String, <code>null</code> if null string input
     */
    public static String keep(String str, String set) {
        if (str == null) {
            return null;
        }
        if (str.length() == 0 || set == null || set.length() == 0) {
            return "";
        }
        String[] strs = new String[1];
        strs[0] = set;
        return keep(str, strs);
    }
    
    /**
     * <p>Takes an argument in set-syntax, see evaluateSet,
     * and keeps any of characters present in the specified string.</p>
     *
     * <p>An example would be:</p>
     * <ul>
     *  <li>keep(&quot;hello&quot;, {&quot;c-f&quot;, &quot;o&quot;})
     *   returns &quot;hll&quot;</li>
     * </ul>
     *
     * @see #evaluateSet(java.lang.String[]) for set-syntax.
     * @param str  String to keep characters from, may be null
     * @param set  String[] set of characters to keep, may be null
     * @return modified String, <code>null</code> if null string input
     */
    public static String keep(String str, String[] set) {
        if (str == null) {
            return null;
        }
        if (str.length() == 0 || set == null || set.length == 0) {
            return "";
        }
        return modify(str, set, true);
    }

    // Delete
    //-----------------------------------------------------------------------
    /**
     * <p>Takes an argument in set-syntax, see evaluateSet,
     * and deletes any of characters present in the specified string.</p>
     *
     * <pre>
     * CharSetUtils.delete(null, *)        = null
     * CharSetUtils.delete("", *)          = ""
     * CharSetUtils.delete(*, null)        = *
     * CharSetUtils.delete(*, "")          = *
     * CharSetUtils.delete("hello", "hl") = "hll"
     * CharSetUtils.delete("hello", "le") = "ell"
     * </pre>
     *
     * @see #evaluateSet(java.lang.String[]) for set-syntax.
     * @param str  String to delete characters from, may be null
     * @param set  String set of characters to delete, may be null
     * @return modified String, <code>null</code> if null string input
     */
    public static String delete(String str, String set) {
        if (str == null || str.length() == 0 || set == null || set.length() == 0) {
            return str;
        }
        String[] strs = new String[1];
        strs[0] = set;
        return delete(str, strs);
    }
    
    /**
     * <p>Takes an argument in set-syntax, see evaluateSet,
     * and deletes any of characters present in the specified string.</p>
     *
     * <p>An example would be:</p>
     * <ul>
     *  <li>delete(&quot;hello&quot;, {&quot;c-f&quot;, &quot;o&quot;}) returns
     *   &quot;hll&quot;</li>
     * </ul>
     *
     * @see #evaluateSet(java.lang.String[]) for set-syntax.
     * @param str  String to delete characters from, may be null
     * @param set  String[] set of characters to delete, may be null
     * @return modified String, <code>null</code> if null string input
     */
    public static String delete(String str, String[] set) {
        if (str == null || str.length() == 0 || set == null || set.length == 0) {
            return str;
        }
        return modify(str, set, false);
    }

    //-----------------------------------------------------------------------
    // Implementation of delete and keep
    private static String modify(String str, String[] set, boolean expect) {
        CharSet chars = evaluateSet(set);
        StringBuffer buffer = new StringBuffer(str.length());
        char[] chrs = str.toCharArray();
        int sz = chrs.length;
        for(int i=0; i<sz; i++) {
            if(chars.contains(chrs[i]) == expect) {
                buffer.append(chrs[i]);
            }
        }
        return buffer.toString();
    }

    // Translate
    //-----------------------------------------------------------------------
    /**
     * <p>Translate characters in a String.
     * This is a multi character search and replace routine.</p>
     *
     * <p>An example is:</p>
     * <ul>
     *   <li>translate(&quot;hello&quot;, &quot;ho&quot;, &quot;jy&quot;)
     *    => jelly</li>
     * </ul>
     *
     * <p>If the length of characters to search for is greater than the
     * length of characters to replace, then the last character is 
     * used.</p>
     * 
     * <pre>
     * CharSetUtils.translate(null, *, *) = null
     * CharSetUtils.translate("", *, *) = ""
     * </pre>
     *
     * @param str  String to replace characters in, may be null
     * @param searchChars   a set of characters to search for, must not be null
     * @param replaceChars  a set of characters to replace, must not be null or empty ("")
     * @return translated String, <code>null</code> if null string input
     * @throws NullPointerException if <code>with</code> or <code>repl</code> 
     *  is <code>null</code>
     * @throws ArrayIndexOutOfBoundsException if <code>with</code> is empty ("")
     * @deprecated Use {@link StringUtils#replaceChars(String, String, String)}.
     *             Method will be removed in Commons Lang 3.0.
     */
    public static String translate(String str, String searchChars, String replaceChars) {
        if (str == null || str.length() == 0) {
            return str;
        }
        StringBuffer buffer = new StringBuffer(str.length());
        char[] chrs = str.toCharArray();
        char[] withChrs = replaceChars.toCharArray();
        int sz = chrs.length;
        int withMax = replaceChars.length() - 1;
        for(int i=0; i<sz; i++) {
            int idx = searchChars.indexOf(chrs[i]);
            if(idx != -1) {
                if(idx > withMax) {
                    idx = withMax;
                }
                buffer.append(withChrs[idx]);
            } else {
                buffer.append(chrs[i]);
            }
        }
        return buffer.toString();
    }

}
