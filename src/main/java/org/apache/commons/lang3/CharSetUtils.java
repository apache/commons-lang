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

/**
 * <p>Operations on <code>CharSet</code>s.</p>
 *
 * <p>This class handles <code>null</code> input gracefully.
 * An exception will not be thrown for a <code>null</code> input.
 * Each method documents its behaviour in more detail.</p>
 * 
 * <p>#ThreadSafe#</p>
 * @see CharSet
 * @author Apache Software Foundation
 * @author Phil Steitz
 * @author Gary Gregory
 * @since 1.0
 * @version $Id$
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
      super();
    }

    // Squeeze
    //-----------------------------------------------------------------------
    /**
     * <p>Squeezes any repetitions of a character that is mentioned in the
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
     * @see CharSet#getInstance(java.lang.String) for set-syntax.
     * @param str  the string to squeeze, may be null
     * @param set  the character set to use for manipulation, may be null
     * @return modified String, <code>null</code> if null string input
     */
    public static String squeeze(String str, String set) {
        if (StringUtils.isEmpty(str) || StringUtils.isEmpty(set)) {
            return str;
        }
        String[] strs = new String[1];
        strs[0] = set;
        return squeeze(str, strs);
    }

    /**
     * <p>Squeezes any repetitions of a character that is mentioned in the
     * supplied set.</p>
     *
     * <p>An example is:</p>
     * <ul>
     *   <li>squeeze(&quot;hello&quot;, {&quot;el&quot;}) => &quot;helo&quot;</li>
     * </ul>
     * 
     * @see CharSet#getInstance(java.lang.String) for set-syntax.
     * @param str  the string to squeeze, may be null
     * @param set  the character set to use for manipulation, may be null
     * @return modified String, <code>null</code> if null string input
     */
    public static String squeeze(String str, String[] set) {
        if (StringUtils.isEmpty(str) || ArrayUtils.isEmpty(set)) {
            return str;
        }
        CharSet chars = CharSet.getInstance(set);
        StringBuilder buffer = new StringBuilder(str.length());
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
     * @see CharSet#getInstance(java.lang.String) for set-syntax.
     * @param str  String to count characters in, may be null
     * @param set  String set of characters to count, may be null
     * @return character count, zero if null string input
     */
    public static int count(String str, String set) {
        if (StringUtils.isEmpty(str) || StringUtils.isEmpty(set)) {
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
     * @see CharSet#getInstance(java.lang.String) for set-syntax.
     * @param str  String to count characters in, may be null
     * @param set  String[] set of characters to count, may be null
     * @return character count, zero if null string input
     */
    public static int count(String str, String[] set) {
        if (StringUtils.isEmpty(str) || ArrayUtils.isEmpty(set)) {
            return 0;
        }
        CharSet chars = CharSet.getInstance(set);
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
     * CharSetUtils.keep("hello", "hl")  = "hll"
     * CharSetUtils.keep("hello", "le")  = "ell"
     * </pre>
     *
     * @see CharSet#getInstance(java.lang.String) for set-syntax.
     * @param str  String to keep characters from, may be null
     * @param set  String set of characters to keep, may be null
     * @return modified String, <code>null</code> if null string input
     * @since 2.0
     */
    public static String keep(String str, String set) {
        if (str == null) {
            return null;
        }
        if (str.length() == 0 || StringUtils.isEmpty(set)) {
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
     *   returns &quot;eo&quot;</li>
     * </ul>
     *
     * @see CharSet#getInstance(java.lang.String) for set-syntax.
     * @param str  String to keep characters from, may be null
     * @param set  String[] set of characters to keep, may be null
     * @return modified String, <code>null</code> if null string input
     * @since 2.0
     */
    public static String keep(String str, String[] set) {
        if (str == null) {
            return null;
        }
        if (str.length() == 0 || ArrayUtils.isEmpty(set)) {
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
     * CharSetUtils.delete("hello", "hl")  = "eo"
     * CharSetUtils.delete("hello", "le")  = "ho"
     * </pre>
     *
     * @see CharSet#getInstance(java.lang.String) for set-syntax.
     * @param str  String to delete characters from, may be null
     * @param set  String set of characters to delete, may be null
     * @return modified String, <code>null</code> if null string input
     */
    public static String delete(String str, String set) {
        if (StringUtils.isEmpty(str) || StringUtils.isEmpty(set)) {
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
     * @see CharSet#getInstance(java.lang.String) for set-syntax.
     * @param str  String to delete characters from, may be null
     * @param set  String[] set of characters to delete, may be null
     * @return modified String, <code>null</code> if null string input
     */
    public static String delete(String str, String[] set) {
        if (StringUtils.isEmpty(str) || ArrayUtils.isEmpty(set)) {
            return str;
        }
        return modify(str, set, false);
    }

    //-----------------------------------------------------------------------
    /**
     * Implementation of delete and keep
     *
     * @param str String to modify characters within
     * @param set String[] set of characters to modify
     * @param expect whether to evaluate on match, or non-match
     * @return modified String
     */
    private static String modify(String str, String[] set, boolean expect) {
        CharSet chars = CharSet.getInstance(set);
        StringBuilder buffer = new StringBuilder(str.length());
        char[] chrs = str.toCharArray();
        int sz = chrs.length;
        for(int i=0; i<sz; i++) {
            if(chars.contains(chrs[i]) == expect) {
                buffer.append(chrs[i]);
            }
        }
        return buffer.toString();
    }

}
