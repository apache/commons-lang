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
 * <p>A range of characters. Able to understand the idea of a contiguous
 * sublist of an alphabet, a negated concept, and a set of characters.</p>
 *
 * <p>Used by <code>CharSet</code> to handle sets of characters.</p>
 *
 * @author <a href="bayard@generationjava.com">Henri Yandell</a>
 * @author Stephen Colebourne
 * @since 1.0
 * @version $Id: CharRange.java,v 1.5 2003/03/23 17:59:09 scolebourne Exp $
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
     * <p>Construct a <code>CharRange</code> over a single character.</p>
     *
     * @param start char over which this range is placed
     */
    public CharRange(char start) {
        this.start = start;
    }

    /**
     * <p>Construct a <code>CharRange</code> over a set of characters.</p>
     *
     * @param start  char start character in this range. inclusive
     * @param close  char close character in this range. inclusive
     */
    public CharRange(char start, char close) {
        this.start = start;
        this.close = close;
    }

    /**
     * <p>Construct a <code>CharRange</code> over a set of characters.</p>
     *
     * @param start  String start first character is in this range (inclusive).
     * @param close  String first character is close character in this
     *  range (inclusive).
     */
    public CharRange(String start, String close) {
        this.start = start.charAt(0);
        this.close = close.charAt(0);
    }

    /**
     * <p>Get the start character for this character range.</p>
     * 
     * @return start char (inclusive)
     */
    public char getStart() {
        return this.start;
    }

    /**
     * <p>Get the end character for this character range.</p>
     * 
     * @return end char (inclusive)
     */
    public char getEnd() {
        return this.close;
    }

    /**
     * <p>Set the start character for this character range.</p>
     * 
     * @param ch  start char (inclusive)
     */
    public void setStart(char ch) {
        this.start = ch;
    }

    /**
     * <p>Set the end character for this character range.</p>
     * 
     * @param ch  start char (inclusive)
     */
    public void setEnd(char ch) {
        this.close = ch;
    }

    /**
     * <p>Is this <code>CharRange</code> over many characters.</p>
     *
     * @return boolean <code>true</code> is many characters
     */
    public boolean isRange() {
        return this.close != UNSET;
    }

    /**
     * <p>Is the passed in character <code>ch</code> inside
     * this range.</p>
     *
     * @param ch character to test for
     * @return boolean <code>true</code> is in range
     */
    public boolean inRange(char ch) {
        if( isRange() ) {
            return ((ch >= start) && (ch <= close));
        } else {
            return start == ch;
        }
    }

    /**
     * <p>Checks if this <code>CharRange</code> is negated.</p>
     *
     * @return boolean <code>true</code> is negated
     */
    public boolean isNegated() {
        return negated;
    }

    /**
     * <p>Sets this character range to be negated or not.</p>
     *
     * <p>This implies that this <code>CharRange</code> is over
     * all characters except the ones in this range.</p>
     * 
     * @param negated  <code>true</code> to negate the range
     */
    public void setNegated(boolean negated) {
        this.negated = negated;
    }

    /**
     * <p>Output a string representation of the character range.</p>
     * 
     * @return string representation
     */
    public String toString() {
        String str = "";
        if( isNegated() ) {
            str += "^";
        }
        str += start;
        if( isRange() ) {
            str += "-";
            str += close;
        }
        return str;
    }
    
}
