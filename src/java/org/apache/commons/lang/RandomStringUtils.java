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
 * <p>Originally from 
 *  <a href="http://jakarta.apache.org/turbine/">Turbine</a> and the
 * GenerationJavaCore library.</p>
 *
 * @author <a href="mailto:jon@latchkey.com">Jon S. Stevens</a>
 * @author <a href="mailto:dlr@finemaltcoding.com">Daniel Rall</a>
 * @author <a href="mailto:gcoladonato@yahoo.com">Greg Coladonato</a>
 * @author <a href="mailto:bayard@generationjava.com">Bayard</a>
 * @author <a href="mailto:ed@apache.org">Ed Korthof</a>
 * @author <a href="mailto:steven@caswell.name">Steven Caswell</a>
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @version $Id: RandomStringUtils.java,v 1.1 2002/07/19 03:35:54 bayard Exp $
 */
public class RandomStringUtils {

    /**
     * Random object used by random method. This has to be not local 
     * to the random method so as to not return the same value in the 
     * same millisecond. 
     */
    private static final Random RANDOM = new Random();

    /**
     * Prevent construction of RandomStringUtils instances
     */
    private RandomStringUtils() {
    }

    /**
     * Creates a random string whose length is the number of characters
     * specified. Characters will be chosen from the set of all characters.
     *
     * @param count length of random string to create
     * @return the random string
     */
    public static String random(int count) {
        return random(count, false, false);
    }

    /**
     * Creates a random string whose length is the number of characters
     * specified. Characters will be chosen from the set of characters whose
     * ASCII value is between 32 and 127 .
     *
     * @param count length of random string to create
     * @return the random string
     */
    public static String randomAscii(int count) {
        return random(count, 32, 127, false, false);
    }
    
    /**
     * Creates a random string whose length is the number of characters
     * specified. Characters will be chosen from the set of alphabetic
     * characters.
     *
     * @param count length of random string to create
     * @return the random string
     */
    public static String randomAlphabetic(int count) {
        return random(count, true, false);
    }
    
    /**
     * Creates a random string whose length is the number of characters
     * specified. Characters will be chosen from the set of alpha-numeric
     * characters.
     *
     * @param count length of random string to create
     * @return the random string
     */
    public static String randomAlphanumeric(int count) {
        return random(count, true, true);
    }
    
    /**
     * Creates a random string whose length is the number of characters
     * specified. Characters will be chosen from the set of numeric
     * characters.
     *
     * @param count length of random string to create
     * @return the random string
     */
    public static String randomNumeric(int count) {
        return random(count, false, true);
    }

    /**
     * Creates a random string whose length is the number of characters
     * specified. Characters will be chosen from the set of alpha-numeric
     * characters as indicated by the arguments.
     *
     * @param count length of random string to create
     * @param letters if <code>true</code>, generated string will include
     * alphabetic characters
     * @param numbers if <code>true</code>, generatd string will include
     * numeric characters
     * @return the random string
     */
    public static String random(int count, boolean letters, boolean numbers) {
        return random(count, 0, 0, letters, numbers);
    }
    
    /**
     * Creates a random string whose length is the number of characters
     * specified. Characters will be chosen from the set of alpha-numeric
     * characters as indicated by the arguments.
     *
     * @param count length of random string to create
     * @param start int position in set of chars to start at
     * @param end int position in set of chars to end before
     * @param letters if <code>true</code>, generated string will include
     * alphabetic characters
     * @param numbers if <code>true</code>, generatd string will include
     * numeric characters
     * @return the random string
     */
    public static String random(int count, int start, int end, boolean letters, boolean numbers) {
        return random(count, start, end, letters, numbers, null);
    }
    
    /**
     * Creates a random string based on a variety of options.
     *
     * @param count int length of random string to create
     * @param start int position in set of chars to start at
     * @param end int position in set of chars to end before
     * @param letters boolean only allow letters?
     * @param numbers boolean only allow numbers?
     * @param set char[] set of chars to choose randoms from.
     *        If null, then it will use the set of all chars.
     * @return the random string
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
     * Creates a random string whose length is the number of characters
     * specified. Characters will be chosen from the set of characters
     * specified.
     *
     * @param count int length of random string to create
     * @param set String containing the set of characters to use
     * @return the random string
     */
    public static String random(int count, String set) {
        return random(count, set.toCharArray());
    }

    /**
     * Creates a random string whose length is the number of characters
     * specified. Characters will be chosen from the set of characters
     * specified.
     *
     * @param count int length of random string to create
     * @param set character array containing the set of characters to use
     * @return the random string
     */
    public static String random(int count, char[] set) {
        return random(count, 0, set.length - 1, false, false, set);
    }
}
