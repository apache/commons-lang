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

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

/**
 * <p>A set of characters. You can iterate over the characters in the
 * set.</p>
 *
 * @author <a href="bayard@generationjava.com">Henri Yandell</a>
 * @author Stephen Colebourne
 * @since 1.0
 * @version $Id: CharSet.java,v 1.8 2003/03/23 17:59:09 scolebourne Exp $
 */
public class CharSet {

    private List set = new LinkedList();

    /**
     * <p>Restricted constructor.</p>
     *
     * <p>Use the factory method
     * {@link CharSetUtils#evaluateSet(java.lang.String[])}.</p>
     *
     * @throws NullPointerException if any of set[i] is <code>null</code>
     *  or if set is <code>null</code>
     */
    protected CharSet(String[] set) {
        int sz = set.length;
        for (int i = 0; i < sz; i++) {
            add(set[i]);
        }
    }

    /**
     * <p>Does the <code>CharSet</code> contain the specified
     * character <code>ch</code>.</p>
     * 
     * @param ch  the character to check for
     * @return <code>true</code> if it does contain the character
     *  <code>ch</code>
     */
    public boolean contains(char ch) {
        Iterator iterator = set.iterator();
        boolean bool = false;
        while (iterator.hasNext()) {
            CharRange range = (CharRange) iterator.next();
            if (range.isNegated()) {
                if (!range.inRange(ch)) {
                    bool = true;
                }
            } else {
                if (range.inRange(ch)) {
                    bool = true;
                }
            }
        }
        return bool;
    }

    /**
     * <p>Add a set definition string to the <code>CharSet</code>.</p>
     * 
     * @param str  set definition string
     * @throws NullPointerException if <code>str</code> is <code>null</code>
     */
    protected void add(String str) {
        int sz = str.length();
        CharRange range = null;

        if("-".equals(str)) {
            range = new CharRange('-');
            set.add(range);
            return;
        } 

        boolean end = false;
        boolean negated = false;
        for(int i=0; i<sz; i++) {
            char ch = str.charAt(i);
            if(ch == '-') {
                end = true;
                continue;
            }
            if(end) {
                range.setEnd(ch);
                continue;
            }
            if(ch == '^') {
                negated = true;
                continue;
            }
            range = new CharRange(ch);
            range.setNegated(negated);
            set.add(range);
        }
    }

    /**
     * <p>Returns a string representation of the set.</p>
     * 
     * @return string representation
     */
    public String toString() {
        return set.toString();
    }

}
