package org.apache.commons.lang;

/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2001-2002 The Apache Software Foundation.  All rights
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
 * 3. The end-user documentation included with the redistribution,
 *    if any, must include the following acknowledgment:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowledgment may appear in the software itself,
 *    if and wherever such third-party acknowledgments normally appear.
 *
 * 4. The names "Apache" and "Apache Software Foundation" and
 *    "Commons" must not be used to endorse or promote products
 *    derived from this software without prior written permission. For
 *    written permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache",
 *    nor may "Apache" appear in their name, without
 *    prior written permission of the Apache Software Foundation.
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

import java.util.Random;
/**
 * <p>Common random <code>String</code> manipulation routines.</p>
 *
 * <p>Originally from the GenerationJava Core library.</p>
 *
 * @author <a href="mailto:bayard@generationjava.com">Henri Yandell</a>
 * @author <a href="mailto:steven@caswell.name">Steven Caswell</a>
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @version $Id: RandomStringUtils.java,v 1.6 2002/11/16 10:41:03 scolebourne Exp $
 */
public class RandomStringUtils {

    /**
     * <p>Random object used by random method. This has to be not local
     * to the random method so as to not return the same value in the 
     * same millisecond.</p>
     */
    private static final Random RANDOM = new Random();

    /**
     * <p><code>RandomStringUtils</code> instances should NOT be constructed in
     * standard programming. Instead, the class should be used as
     * <code>RandomStringUtils.random(5);</code>.</p>
     *
     * <p>This constructor is public to permit tools that require a JavaBean instance
     * to operate.</p>
     */
    public RandomStringUtils() {
    }

    /**
     * <p>Creates a random string whose length is the number of characters
     * specified.</p>
     *
     * <p>Characters will be chosen from the set of all characters.</p>
     *
     * @param count length of random string to create
     * @return the random string
     */
    public static String random(int count) {
        return random(count, false, false);
    }

    /**
     * <p>Creates a random string whose length is the number of characters
     * specified.</p>
     *
     * <p>Characters will be chosen from the set of characters whose
     * ASCII value is between <code>32</code> and <code>127</code>.</p>
     *
     * @param count length of random string to create
     * @return the random string
     */
    public static String randomAscii(int count) {
        return random(count, 32, 127, false, false);
    }
    
    /**
     * <p>Creates a random string whose length is the number of characters
     * specified.</p>
     *
     * <p>Characters will be chosen from the set of alphabetic
     * characters.</p>
     *
     * @param count length of random string to create
     * @return the random string
     */
    public static String randomAlphabetic(int count) {
        return random(count, true, false);
    }
    
    /**
     * <p>Creates a random string whose length is the number of characters
     * specified.</p>
     *
     * <u>Characters will be chosen from the set of alpha-numeric
     * characters.</p>
     *
     * @param count length of random string to create
     * @return the random string
     */
    public static String randomAlphanumeric(int count) {
        return random(count, true, true);
    }
    
    /**
     * <p>Creates a random string whose length is the number of characters
     * specified.</p>
     *
     * <p>Characters will be chosen from the set of numeric
     * characters.</p>
     *
     * @param count length of random string to create
     * @return the random string
     */
    public static String randomNumeric(int count) {
        return random(count, false, true);
    }

    /**
     * <p>Creates a random string whose length is the number of characters
     * specified.</p>
     *
     * <p>Characters will be chosen from the set of alpha-numeric
     * characters as indicated by the arguments.</p>
     *
     * @param count length of random string to create
     * @param letters if <code>true</code>, generated string will include
     *  alphabetic characters
     * @param numbers if <code>true</code>, generatd string will include
     *  numeric characters
     * @return the random string
     */
    public static String random(int count, boolean letters, boolean numbers) {
        return random(count, 0, 0, letters, numbers);
    }
    
    /**
     * <p>Creates a random string whose length is the number of characters
     * specified.</p>
     *
     * <p>Characters will be chosen from the set of alpha-numeric
     * characters as indicated by the arguments.</p>
     *
     * @param count length of random string to create
     * @param start position in set of chars to start at
     * @param end  position in set of chars to end before
     * @param letters if <code>true</code>, generated string will include
     *  alphabetic characters
     * @param numbers if <code>true</code>, generated string will include
     *  numeric characters
     * @return the random string
     */
    public static String random(int count, int start, int end, boolean letters, boolean numbers) {
        return random(count, start, end, letters, numbers, null);
    }
    
    /**
     * <p>Creates a random string based on a variety of options.</p>
     *
	 * <p>If start and end are both <code>0</code>, start and end are set
     * to <code>' '</code> and <code>'z'</code>, the ASCII printable
     * characters, will be used, unless letters and numbers are both
	 * <code>false</code>, in which case, start and end are set to
     * <code>0</code> and <code>Integer.MAX_VALUE</code>.
     *
	 * <p>If set is not <code>null</code>, characters between start and
     * end are chosen.</p>
     *
     * @param count length of random string to create
     * @param start position in set of chars to start at
     * @param end position in set of chars to end before
     * @param letters only allow letters?
     * @param numbers only allow numbers?
     * @param set set of chars to choose randoms from. If <code>null</code>,
     *  then it will use the set of all chars.
     * @return the random string
     * @throws ArrayIndexOutOfBoundsException if there are not
     *  <code>(end - start) + 1</code> characters in the set array.
     */
    public static String random(int count, int start, int end, boolean letters, boolean numbers, char[] set) {
        if( (start == 0) && (end == 0) ) {
            end = (int)'z';
            start = (int)' ';
            if(!letters && !numbers) {
                start = 0;
                end = Integer.MAX_VALUE;
            }
        }

        StringBuffer buffer = new StringBuffer();
        int gap = end - start;

        while(count-- != 0) {
            char ch;
            if(set == null) {
                ch = (char)(RANDOM.nextInt(gap) + start);
            } else {
                ch = set[RANDOM.nextInt(gap) + start];
            }
            if( (letters && numbers && Character.isLetterOrDigit(ch)) ||
                (letters && Character.isLetter(ch)) ||
                (numbers && Character.isDigit(ch)) ||
                (!letters && !numbers)
              ) 
            {
                buffer.append( ch );
            } else {
                count++;
            }
        }
        return buffer.toString();
    }

    /**
     * <p>Creates a random string whose length is the number of characters
     * specified.</p>
     *
     * <p>Characters will be chosen from the set of characters
     * specified.</p>
     *
     * @param count length of random string to create
     * @param set String containing the set of characters to use
     * @return the random string
     */
    public static String random(int count, String set) {
        return random(count, set.toCharArray());
    }

    /**
     * <p>Creates a random string whose length is the number of characters
     * specified.</p>
     *
     * <p>Characters will be chosen from the set of characters specified.</p>
     *
     * @param count length of random string to create
     * @param set character array containing the set of characters to use
     * @return the random string
     */
    public static String random(int count, char[] set) {
        return random(count, 0, set.length - 1, false, false, set);
    }
}
