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
 *    any, must include the following acknowledgement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowledgement may appear in the software itself,
 *    if and wherever such third-party acknowledgements normally appear.
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

import java.io.Serializable;

/**
 * <p>A contiguous range of characters, optionally negated.</p>
 * 
 * <p>Instances are immutable.</p>
 *
 * @author Henri Yandell
 * @author Stephen Colebourne
 * @author Chris Feldhacker
 * @author Gary Gregory
 * @since 1.0
 * @version $Id: CharRange.java,v 1.16 2003/09/07 14:32:34 psteitz Exp $
 */
public final class CharRange implements Serializable {

    /** Serialization lock, Lang version 2.0. */
    private static final long serialVersionUID = 8270183163158333422L;
    
    /** The first character, inclusive, in the range. */
    private final char start;
    /** The last character, inclusive, in the range. */
    private final char end;
    /** True if the range is everything except the characters specified. */
    private final boolean negated;
    
    /** Cached toString. */
    private transient String iToString;

    //-----------------------------------------------------------------------
    /**
     * <p>Constructs a <code>CharRange</code> over a single character.</p>
     *
     * @param ch  only character in this range
     */
    public CharRange(char ch) {
        this(ch, ch, false);
    }

    /**
     * <p>Constructs a <code>CharRange</code> over a single character,
     * optionally negating the range.</p>
     *
     * <p>A negated range includes everything except the specified char.</p>
     *
     * @param ch  only character in this range
     * @param negated  true to express everything except the range
     */
    public CharRange(char ch, boolean negated) {
        this(ch, ch, negated);
    }

    /**
     * <p>Constructs a <code>CharRange</code> over a set of characters.</p>
     *
     * @param start  first character, inclusive, in this range
     * @param end  last character, inclusive, in this range
     */
    public CharRange(char start, char end) {
        this(start, end, false);
    }

    /**
     * <p>Constructs a <code>CharRange</code> over a set of characters,
     * optionally negating the range.</p>
     *
     * <p>A negated range includes everything except that defined by the
     * start and end characters.</p>
     * 
     * <p>If start and end are in the wrong order, they are reversed.
     * Thus <code>a-e</code> is the same as <code>e-a</code>.</p>
     *
     * @param start  first character, inclusive, in this range
     * @param end  last character, inclusive, in this range
     * @param negated  true to express everything except the range
     */
    public CharRange(char start, char end, boolean negated) {
        super();
        if (start > end) {
            char temp = start;
            start = end;
            end = temp;
        }
        
        this.start = start;
        this.end = end;
        this.negated = negated;
    }

    // Accessors
    //-----------------------------------------------------------------------
    /**
     * <p>Gets the start character for this character range.</p>
     * 
     * @return the start char (inclusive)
     */
    public char getStart() {
        return this.start;
    }

    /**
     * <p>Gets the end character for this character range.</p>
     * 
     * @return the end char (inclusive)
     */
    public char getEnd() {
        return this.end;
    }

    /**
     * <p>Is this <code>CharRange</code> negated.</p>
     * 
     * <p>A negated range includes everything except that defined by the
     * start and end characters.</p>
     *
     * @return <code>true</code> is negated
     */
    public boolean isNegated() {
        return negated;
    }

    // Contains
    //-----------------------------------------------------------------------
    /**
     * <p>Is the character specified contained in this range.</p>
     *
     * @param ch  the character to check
     * @return <code>true</code> if this range contains the input character
     */
    public boolean contains(char ch) {
        return ((ch >= start && ch <= end) != negated);
    }

    /**
     * <p>Are all the characters of the passed in range contained in
     * this range.</p>
     *
     * @param range  the range to check against
     * @return <code>true</code> if this range entirely contains the input range
     * @throws IllegalArgumentException if <code>null</code> input
     */
    public boolean contains(CharRange range) {
        if (range == null) {
            throw new IllegalArgumentException("The Range must not be null");
        }
        if (negated) {
            if (range.negated) {
                return (start >= range.start && end <= range.end);
            } else {
                return (range.end < start || range.start > end);
            }
        } else {
            if (range.negated) {
                return (start == 0 && end == Character.MAX_VALUE);
            } else {
                return (start <= range.start && end >= range.end);
            }
        }
    }

    // Basics
    //-----------------------------------------------------------------------
    /**
     * <p>Compares two CharRange objects, returning true if they represent
     * exactly the same range of characters defined in the same way.</p>
     * 
     * @param obj  the object to compare to
     * @return true if equal
     */
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        if (obj instanceof CharRange == false) {
            return false;
        }
        CharRange other = (CharRange) obj;
        return (start == other.start && end == other.end && negated == other.negated);
    }

    /**
     * <p>Gets a hashCode compatible with the equals method.</p>
     * 
     * @return a suitable hashCode
     */
    public int hashCode() {
        return 83 + start + 7 * end + (negated ? 1 : 0);
    }
    
    /**
     * <p>Gets a string representation of the character range.</p>
     * 
     * @return string representation of this range
     */
    public String toString() {
        if (iToString == null) {
            StringBuffer buf = new StringBuffer(4);
            if (isNegated()) {
                buf.append('^');
            }
            buf.append(start);
            if (start != end) {
                buf.append('-');
                buf.append(end);
            }
            iToString = buf.toString();
        }
        return iToString;
    }
    
}
