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
 * @author <a href="bayard@generationjava.com">Henri Yandell</a>
 * @author Stephen Colebourne
 * @since 1.0
 * @version $Id: CharSetUtils.java,v 1.11 2003/04/09 00:07:50 ggregory Exp $
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

    /**
     * <p>Creates a <code>CharSetUtils</code> object which allows a certain amount of
     * set logic to be performed.</p>
     * <p>The syntax is:</p>
     * <ul>
     *  <li>"aeio" which implies 'a','e',..
     *  <li>"^e" implies not e. However it only negates, it's not
     *   a set in itself due to the size of that set in unicode.
     *  <li>"ej-m" implies e,j->m. e,j,k,l,m.
     * </ul>
     *
     * @param set
     * @return CharSet
     * @throws NullPointerException if any of set[i] is null or if set is null
     */
    public static CharSet evaluateSet(String[] set) {
        return new CharSet(set); 
    }

    /**
     * <p>Squeezes any repititions of a character that is mentioned in the
     * supplied set.</p>
     *
     * <p>An example is:</p>
     * <ul>
     *  <li>squeeze("hello", "el")  => "helo"
     * </ul>
     * @see #evaluateSet(java.lang.String[]) for set-syntax.
     *
     * @param str  the string to work from
     * @param set  the character set to use for manipulation
     */
    public static String squeeze(String str, String set) {
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
     *   <li>squeeze("hello", {"el"})  => "helo"
     * </ul>
     * @see #evaluateSet(java.lang.String[]) for set-syntax.
     * 
     * @param str  the string to work from
     * @param set  the character set to use for manipulation
     * @throws NullPointerException if <code>str</code> is
     *  <code>null</code>
     */
    public static String squeeze(String str, String[] set) {
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

    /**
     * <p>Takes an argument in set-syntax, see evaluateSet,
     * and returns the number of characters present in the specified string.</p>
     *
     * <p>An example would be:</p>
     * <ul>
     *   <li>count("hello", {"c-f","o"}) returns 2.
     * </ul>
     *
     * @param str  String target to count characters in
     * @param set  String set of characters to count
     */
    public static int count(String str, String set) {
        String[] strs = new String[1];
        strs[0] = set;
        return count(str, strs);
    }
    
    /**
     * <p>Takes an argument in set-syntax, see evaluateSet,
     * and returns the number of characters present in the specified string.</p>
     *
     * An example would be:</p>
     * <ul>
     *  <li>count("hello", {"c-f","o"}) returns 2.
     * </ul>
     *
     * @param str  String target to count characters in
     * @param set  String[] set of characters to count
     */
    public static int count(String str, String[] set) {
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

    /**
     * <p>Takes an argument in set-syntax, see evaluateSet,
     * and keeps any of characters present in the specified string.</p>
     *
     * <p>An example would be:</p>
     * <ul>
     *   <li>keep("hello", {"c-fo"}) returns "hll"
     * </ul>
     *
     * @param str  String target to keep characters from
     * @param set  String set of characters to keep
     */
    public static String keep(String str, String set) {
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
     *  <li>keep("hello", {"c-f","o"}) returns "hll"
     * </ul>
     *
     * @param str  String target to keep characters from
     * @param set  String[] set of characters to keep
     * @throws NullPointerException of <code>str</code> is
     *  <code>null</code>
     */
    public static String keep(String str, String[] set) {
        return modify(str, set, true);
    }

    /**
     * <p>Takes an argument in set-syntax, see evaluateSet,
     * and deletes any of characters present in the specified string.</p>
     *
     * <p>An example would be:</p>
     * <ul>
     *   <li>delete("hello", {"c-fo"}) returns "hll"
     * </ul>
     *
     * @param str  String target to delete characters from
     * @param set  String set of characters to delete
     */
    public static String delete(String str, String set) {
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
     *  <li>delete("hello", {"c-f","o"}) returns "hll"
     * </ul>
     *
     * @param str  String target to delete characters from
     * @param set  String[] set of characters to delete
     * @throws NullPointerException of <code>str</code> is
     *  <code>null</code>
     */
    public static String delete(String str, String[] set) {
        return modify(str, set, false);
    }

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

    /**
     * <p>Translate characters in a String.</p>
     *
     * <p>An example is:</p>
     * <ul>
     *   <li>translate("hello", "ho", "jy") => jelly
     * </ul>
     *
     * <p>If the length of characters to search for is greater than the
     * length of characters to replace, then the last character is 
     * used.</p>
     *
     * @param target String to replace characters in
     * @param repl String to find that will be replaced
     * @param with String to put into the target String
     * @throws NullPointerException if <code>target</code>, with
     *  or <code>repl</code> is <code>null</code>
     */
    public static String translate(String target, String repl, String with) {
        StringBuffer buffer = new StringBuffer(target.length());
        char[] chrs = target.toCharArray();
        char[] withChrs = with.toCharArray();
        int sz = chrs.length;
        int withMax = with.length() - 1;
        for(int i=0; i<sz; i++) {
            int idx = repl.indexOf(chrs[i]);
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
