package org.apache.commons.lang;

/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002 The Apache Software Foundation.  All rights
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

/**
 * A range of characters. Able to understand the idea of a contiguous 
 * sublist of an alphabet, a negated concept, and a set of characters.
 * Used by CharSet to handle sets of characters.
 *
 * @author <a href="bayard@generationjava.com">Henri Yandell</a>
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @version $Id: CharRange.java,v 1.1 2002/07/19 03:35:54 bayard Exp $
 */
class CharRange {

    /**
     * Used internally to represent null in a char.
     */
    private static char UNSET;

    private char start;
    private char close;
    private boolean negated;

    /**
     * Construct a CharRange over a single character.
     *
     * @param start char over which this range is placed
     */
    public CharRange(char start) {
        this.start = start;
    }

    /**
     * Construct a CharRange over a set of characters.
     *
     * @param start  char start character in this range. inclusive
     * @param close  char close character in this range. inclusive
     */
    public CharRange(char start, char close) {
        this.start = start;
        this.close = close;
    }

    /**
     * Construct a CharRange over a set of characters.
     *
     * @param start  String start first character is in this range (inclusive).
     * @param close  String first character is close character in this
     * range (inclusive).
     */
    public CharRange(String start, String close) {
        this.start = start.charAt(0);
        this.close = close.charAt(0);
    }

    /**
     * Get the start character for this character range
     * 
     * @return start char (inclusive)
     */
    public char getStart() {
        return this.start;
    }

    /**
     * Get the end character for this character range
     * 
     * @return end char (inclusive)
     */
    public char getEnd() {
        return this.close;
    }

    /**
     * Set the start character for this character range
     * 
     * @param ch  start char (inclusive)
     */
    public void setStart(char ch) {
        this.start = ch;
    }

    /**
     * Set the end character for this character range
     * 
     * @param ch  start char (inclusive)
     */
    public void setEnd(char ch) {
        this.close = ch;
    }

    /**
     * Is this CharRange over many characters
     *
     * @return boolean true is many characters
     */
    public boolean isRange() {
        return this.close != UNSET;
    }

    /**
     * Is the passed in character inside this range
     *
     * @return boolean true is in range
     */
    public boolean inRange(char ch) {
        if(isRange()) {
            return ((ch >= start) && (ch <= close) );
        } else {
            return start == ch;
        }
    }

    /**
     * Checks if this CharRange is negated.
     *
     * @return boolean true is negated
     */
    public boolean isNegated() {
        return negated;
    }

    /**
     * Sets this character range to be negated or not. 
     * This implies that this CharRange is over all characters except 
     * the ones in this range.
     * 
     * @param negated  true to negate the range
     */
    public void setNegated(boolean negated) {
        this.negated = negated;
    }

    /**
     * Output a string representation of the character range
     * 
     * @return string representation
     */
    public String toString() {
        String str = "";
        if(isNegated()) {
            str += "^";
        }
        str += start;
        if(isRange()) {
            str += "-";
            str += close;
        }
        return str;
    }
    
}
