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
package org.apache.commons.lang.util;

import java.io.Serializable;
import java.util.Random;

/**
 * <p><code>IdentifierUtils</code> provides a number of different identifier
 * reference implementations.</p>
 * 
 * <p>All the identifer factories are serializable and synchronized.
 * The factories all implement one of the factory interfaces defined in this
 * package. This allows you to obtain and use multiple factories for 
 * different reasons.</p>
 *
 * @author Stephen Colebourne
 * @since 2.0
 * @version $Id: IdentifierUtils.java,v 1.5 2003/05/16 22:06:43 scolebourne Exp $
 */
public class IdentifierUtils {

    /**
     * <p>Singleton instance of the
     * <code>LongIdentifierFactory</code>.</p>
     * 
     * <p>The singleton instance will wrap, so in a long-lived server, the id
     * may be duplicated.</p>
     * 
     * <p>The objects returned are:</p>
     * <ul>
     * <li>new Long(0L)
     * <li>new Long(1L)
     * <li>new Long(2L)
     * <li>...
     * </ul>
	 */
    public static final LongIdentifierFactory LONG_IDENTIFIER_FACTORY = new LongNumericIdentifierFactory(true, 0L);
    /**
     * <p>Singleton instance of the <code>StringNumericIdentifierFactory</code>.
     * </p>
     * 
     * <p>The singleton instance will wrap, so in a long-lived server, the id
     * may be duplicated.</p>
     * 
     * <p>The objects returned are:</p>
     * <ul>
     * <li>"0"
     * <li>"1"
     * <li>"2"
     * <li>...
     * </ul>
     */
    public static final StringIdentifierFactory STRING_NUMERIC_IDENTIFIER_FACTORY = new StringNumericIdentifierFactory(true, 0L);
    /**
     * <p>Singleton instance of the
     * <code>StringAlphanumericIdentifierFactory</code>.</p>
     * 
     * <p>The singleton instance will wrap, so in a long-lived server, the id
     * may be duplicated. However, the length is 15 in base-36, so thats a
     * lot of identifiers.</p>
     * 
     * <p>The objects returned are:</p>
     * <ul>
     * <li>"000000000000001"
     * <li>"000000000000002"
     * <li>"000000000000003"
     * <li>...
     * <li>"00000000000000y"
     * <li>"00000000000000z"
     * <li>"000000000000010"
     * <li>"000000000000011"
     * <li>...
     * <li>"00000000000001z"
     * <li>"000000000000020"
     * <li>...
     * </ul>
     */
    public static final StringIdentifierFactory STRING_ALPHANUMERIC_IDENTIFIER_FACTORY = new StringAlphanumericIdentifierFactory(true, 15);
    /**
     * <p>Singleton instance of the
     * <code>StringSessionIdentifierFactory</code>.</p>
     * 
     * <p>The singleton instance may produce duplicates in a long-lived server,
     * but its unlikely.</p>
     * 
     * <p>The objects returned are 10 or more base-36 digits.</p>
     */
    public static final StringIdentifierFactory STRING_SESSION_IDENTIFIER_FACTORY = new StringSessionIdentifierFactory();

    //---------------------------------------------------------------------------------
    
    /**
     * <p><code>IdentifierUtils</code> instances should NOT be constructed in
     * standard programming.</p>
     *
     * <p>This constructor is public to permit tools that require a JavaBean instance
     * to operate.</p>
     */
    public IdentifierUtils() {
        super();
    }

    //---------------------------------------------------------------------------------
    
    /**
     * <p>Gets the next identifier using the singleton instance of the
     * Long factory.
     * </p>
     * 
     * <p>The singleton instance will wrap, so in a long-lived server, the id
     * may be duplicated.</p>
     * 
     * @return a new identifier
     */
    public static Long nextLongIdentifier() {
        return LONG_IDENTIFIER_FACTORY.nextLongIdentifier();
    }

    /**
     * <p>Gets a new identifier factory that returns a series of Long objects
     * increasing in size.</p>
     * 
     * <p>The factory will wrap when the maximum <code>long</code> is
     * reached and return negative numbers. It will start from zero.</p>
     * 
     * @return a new identifier factory
     */
    public static LongIdentifierFactory longIdentifierFactory() {
        return new LongNumericIdentifierFactory(true, 0L);
    }

    /**
     * <p>Gets a new identifier factory that returns a series of Long objects
     * increasing in size.</p>
     * 
     * @param wrap  should the factory wrap when it reaches the maximum 
     *  long value (or throw an IllegalStateException)
     * @param initialValue  the initial long value to start at
     * @return a new identifier factory
     */
    public static LongIdentifierFactory longIdentifierFactory(boolean wrap, long initialValue) {
        return new LongNumericIdentifierFactory(wrap, initialValue);
    }
    
    //---------------------------------------------------------------------------------
    
    /**
     * <p>Gets the next identifier using the singleton instance of the
     * String Numeric factory.
     * </p>
     * 
     * <p>The singleton instance will wrap, so in a long-lived server, the id
     * may be duplicated.</p>
     * 
     * @return a new identifier
     */
    public static String nextStringNumericIdentifier() {
        return STRING_NUMERIC_IDENTIFIER_FACTORY.nextStringIdentifier();
    }

    /**
     * <p>Gets a new identifier factory that returns a series of String objects
     * representing numbers increasing in size.</p>
     * 
     * <p>The factory will wrap when the maximum <code>long</code> is
     * reached and return negative numbers. It will start from zero.</p>
     * 
     * @return a new identifier factory
     */
    public static StringIdentifierFactory stringNumericIdentifierFactory() {
        return new StringNumericIdentifierFactory(true, 0L);
    }

    /**
     * <p>Gets a new identifier factory that returns a series of String objects
     * representing numbers increasing in size.</p>
     * 
     * @param wrap  should the factory wrap when it reaches the maximum 
     *  long value (or throw an IllegalStateException)
     * @param initialValue  the initial long value to start at
     * @return a new identifier factory
     */
    public static StringIdentifierFactory stringNumericIdentifierFactory(boolean wrap, long initialValue) {
        return new StringNumericIdentifierFactory(wrap, initialValue);
    }
    
    //---------------------------------------------------------------------------------
    
    /**
     * <p>Gets the next identifier using the singleton instance of the
     * String Alphanumeric factory.
     * </p>
     * 
     * <p>The singleton instance will wrap, so in a long-lived server, the id
     * may be duplicated.</p>
     * 
     * @return a new identifier
     */
    public static String nextStringAlphanumericIdentifier() {
        return STRING_ALPHANUMERIC_IDENTIFIER_FACTORY.nextStringIdentifier();
    }

    /**
     * <p>Gets a new identifier factory that returns a series of String objects
     * representing numbers increasing in size in base-36.</p>
     * 
     * <p>The factory will wrap when the maximum size (15) is reached.</p>
     * 
     * @return a new identifier factory
     */
    public static StringIdentifierFactory stringAlphanumericIdentifierFactory() {
        return new StringAlphanumericIdentifierFactory(true, 15);
    }

    /**
     * <p>Gets a new identifier factory that returns a series of String objects
     * representing numbers increasing in size in base-36.</p>
     * 
     * @param wrap  should the factory wrap when it reaches the maximum 
     *  size (or throw an IllegalStateException)
     * @param size  the number of characters the id should fill
     * @return a new identifier factory
     */
    public static StringIdentifierFactory stringAlphanumericIdentifierFactory(boolean wrap, int size) {
        return new StringAlphanumericIdentifierFactory(wrap, size);
    }
    
    //---------------------------------------------------------------------------------
    
    /**
     * <p>Gets the next identifier using the singleton instance of the
     * String Session factory.
     * </p>
     * 
     * <p>The generation routine is based on a random number and a counter
     * within a 2 second time interval.</p>
     * 
     * @return a new identifier
     */
    public static String nextStringSessionIdentifier() {
        return STRING_SESSION_IDENTIFIER_FACTORY.nextStringIdentifier();
    }

    /**
     * <p>Gets a new identifier factory that returns a series of String objects
     * that appear to be random and are suitable for use as session identifiers.
     * </p>
     * 
     * <p>The generation routine is based on a random number and a counter
     * within a 2 second time interval.</p>
     * 
     * @return a new identifier factory
     */
    public static StringIdentifierFactory stringSessionIdentifierFactory() {
        return new StringSessionIdentifierFactory();
    }

    //---------------------------------------------------------------------------------

    /**
     * <code>LongIdentifierFactory</code> is an Identifier Factory
     * that generates an incrementing number as a Long object.
     *
     * @author Stephen Colebourne
     */
    private static class LongNumericIdentifierFactory implements LongIdentifierFactory, Serializable {
    
        /** Should the counter wrap. */
        private final boolean wrap;
        /** The counter. */
        private long count = 0;
    
        /**
         * Constructor.
         * 
         * @param wrap  should the factory wrap when it reaches the maximum 
         *  long value (or throw an exception)
         * @param initialValue  the initial long value to start at
         */
        private LongNumericIdentifierFactory(boolean wrap, long initialValue) {
            super();
            this.wrap = wrap;
            this.count = initialValue;
        }

        /**
         * Gets the next new identifier.
         * 
         * @return a new identifier as a Long
         */
        public Object nextIdentifier() {
            return nextLongIdentifier();
        }
        
        /**
         * Gets the next new identifier.
         * 
         * @return a new identifier as a Long
         */
        public Long nextLongIdentifier() {
            long value = 0;
            if (wrap) {
                synchronized (this) {
                    value = count++;
                }
            } else {
                synchronized (this) {
                    if (count == Long.MAX_VALUE) {
                        throw new IllegalStateException("The maximum number of identifiers has been reached");
                    }
                    value = count++;
                }
            }
            return new Long(value);
        }
    }

    //---------------------------------------------------------------------------------

    /**
     * <code>StringNumericIdentifierFactory</code> is an Identifier Factory
     * that generates an incrementing number as a String object.
     *
     * @author Stephen Colebourne
     */
    private static class StringNumericIdentifierFactory implements StringIdentifierFactory, Serializable {
    
        /** Should the counter wrap. */
        private final boolean wrap;
        /** The counter. */
        private long count = 0;
        
        /**
         * Constructor.
         * 
         * @param wrap  should the factory wrap when it reaches the maximum 
         *  long value (or throw an exception)
         * @param initialValue  the initial long value to start at
         */
        private StringNumericIdentifierFactory(boolean wrap, long initialValue) {
            super();
            this.wrap = wrap;
            this.count = initialValue;
        }

        /**
         * Gets the next new identifier.
         * 
         * @return a new identifier as a String
         */
        public Object nextIdentifier() {
            return nextStringIdentifier();
        }
        
        /**
         * Gets the next new identifier.
         * 
         * @return a new identifier as a String
         */
        public String nextStringIdentifier() {
            long value = 0;
            if (wrap) {
                synchronized (this) {
                    value = count++;
                }
            } else {
                synchronized (this) {
                    if (count == Long.MAX_VALUE) {
                        throw new IllegalStateException("The maximum number of identifiers has been reached");
                    }
                    value = count++;
                }
            }
            return Long.toString(value);
        }

    }
    
    //---------------------------------------------------------------------------------

    /**
     * <code>StringAlphanumericIdentifierFactory</code> is an Identifier Factory
     * that generates an incrementing incrementing number in base 36 as a String
     * object.
     *
     * @author Stephen Colebourne
     */
    private static class StringAlphanumericIdentifierFactory implements StringIdentifierFactory, Serializable {
    
        /** Should the counter wrap. */
        private final boolean wrap;
        /** The counter. */
        private char[] count = null;
        
        /**
         * Constructor.
         * 
         * @param wrap  should the factory wrap when it reaches the maximum 
         *  long value (or throw an exception)
         * @param size  the size of the identifier
         */
        private StringAlphanumericIdentifierFactory(boolean wrap, int size) {
            super();
            this.wrap = wrap;
            if (size < 1) {
                throw new IllegalArgumentException("The size must be at least one");
            }
            this.count = new char[size];
            for (int i = 0; i < size; i++) {
                count[i] = '0';  // zero
            }
        }

        /**
         * Gets the next new identifier.
         * 
         * @return a new identifier as a String
         */
        public Object nextIdentifier() {
            return nextStringIdentifier();
        }
        
        /**
         * Gets the next new identifier.
         * 
         * @return a new identifier as a String
         */
        public synchronized String nextStringIdentifier() {
            for (int i = count.length - 1; i >= 0; i--) {
                switch (count[i]) {
                    case 122:  // z
                    count[i] = '0';
                    if (i == 0 && wrap == false) {
                        throw new IllegalStateException("The maximum number of identifiers has been reached");
                    }
                    break;
                    
                    case 57:  // 9
                    count[i] = 'a';
                    i = -1;
                    break;
                    
                    default:
                    count[i]++;
                    i = -1;
                    break;
                }
            }
            return new String(count);
        }

    }
    
    //---------------------------------------------------------------------------------
    
    /**
     * <p><code>StringSessionIdentifierFactory</code> is an Identifier
     * Factory that generates an alphanumeric 10+ character identifier. The
     * exact length depends on the number of ids requested per time period.</p>
     * 
     * <p>Originally designed for JServ sessions. Uses synchronized count and
     * time to ensure uniqueness. Not guaranteed unique across JVMs, but
     * fairly safe none the less.</p>
     *
     * @author Jon S. Stevens
     * @author Neeme Praks
     * @author Stephen Colebourne
     */
    private static class StringSessionIdentifierFactory implements StringIdentifierFactory, Serializable {

        /**
         * We want to have a random string with a length of 6 characters.
         * Since we encode it base-36, we modulo the random number with
         * this value.
         */
        private static final long MAX_RANDOM_LEN = 2176782336L; // 36 ** 6
        /**
         * <p>The identifier must be unique within the typical lifespan of a
         * session; the value can roll over after that.</p>3 characters:
         * (this means a roll over after over a day, which is much larger
         * than a typical lifespan).
         */
        private static final long MAX_TIME_SECTION_LEN = 46656L; // 36 ** 3
        /**
         * Milliseconds between different tics.  The 3-character time
         * string has a new value every 2 seconds.
         */
        private static final long TIC_DIFFERENCE = 2000;
        
        /** The incrementing counter. */
        private int counter = 0;
        /** The last time. */
        private long lastTimeValue = 0;
        /** The randmonizer. */
        private Random randomizer = new Random();

        /**
         * Constructor.
         */
        private StringSessionIdentifierFactory() {
            super();
        }
        
        /**
         * Gets the next identifier.
         * 
         * @return the next 10 char String identifier
         */
        public Object nextIdentifier() {
            return nextStringIdentifier();
        }

        /**
         * Gets the next new identifier. Only guaranteed unique within
         * this JVM, but fairly safe for cross JVM usage as well.
         * 
         * <p>Format of identifier is
         * [6 chars random][3 chars time][1+ chars count]</p>
         * 
         * @return the next 10 char String identifier
         */
        public String nextStringIdentifier() {
            // Random value
            //--------------
            long currentRandom = randomizer.nextLong();
            if (currentRandom < 0) {
                currentRandom = -currentRandom;
            }
            // force value into 6 char range, and add to pad with zeros
            // this gives a length of 7, when converted to base 36, and
            // the first character (always 1 from the add) is dropped
            currentRandom %= MAX_RANDOM_LEN;
            currentRandom += MAX_RANDOM_LEN;

            long currentTimeValue = 0;
            int currentCount = 0;
        
            synchronized (this) {
                // Time
                //--------------
                currentTimeValue = (System.currentTimeMillis() / TIC_DIFFERENCE);
    
                // force value into 3 char range, and add to pad with zeros
                // this gives a length of 4, when converted to base 36, and
                // the first character (always 1 from the add) is dropped
                currentTimeValue %= MAX_TIME_SECTION_LEN;
                currentTimeValue += MAX_TIME_SECTION_LEN;
    
                // Count
                //--------------
                // Make the string unique by appending the count since last
                // time flip.
    
                // Count sessions only within tics (so the 'real' counter
                // isn't exposed to the public).
                if (lastTimeValue != currentTimeValue) {
                    lastTimeValue = currentTimeValue;
                    counter = 0;
                }
                currentCount = counter++;
            }

            // build string        
            //--------------
            StringBuffer id = new StringBuffer(15);
            id.append(Long.toString(currentRandom, 36).substring(1));  // 6 chars
            id.append(Long.toString(currentTimeValue, 36).substring(1));  // 3 chars
            id.append(Long.toString(currentCount, 36));  // 1+ chars
            return id.toString();
        }

    }
    
}
