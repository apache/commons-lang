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

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Unit tests {@link org.apache.commons.lang.StringUtils} - Substring methods
 *
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @version $Id: StringUtilsIsTest.java,v 1.5 2003/03/23 21:51:51 scolebourne Exp $
 */
public class StringUtilsIsTest extends TestCase {

    public StringUtilsIsTest(String name) {
        super(name);
    }

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
    	TestSuite suite = new TestSuite(StringUtilsIsTest.class);
    	suite.setName("StringUtilsIsXxx Tests");
        return suite;
    }

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    //-----------------------------------------------------------------------

    public void testIsAlpha() {
        assertEquals(false, StringUtils.isAlpha(null));
        assertEquals(true, StringUtils.isAlpha(""));
        assertEquals(false, StringUtils.isAlpha(" "));
        assertEquals(true, StringUtils.isAlpha("a"));
        assertEquals(true, StringUtils.isAlpha("A"));
        assertEquals(true, StringUtils.isAlpha("kgKgKgKgkgkGkjkjlJlOKLgHdGdHgl"));
        assertEquals(false, StringUtils.isAlpha("ham kso"));
        assertEquals(false, StringUtils.isAlpha("1"));
        assertEquals(false, StringUtils.isAlpha("hkHKHik6iUGHKJgU7tUJgKJGI87GIkug"));
        assertEquals(false, StringUtils.isAlpha("_"));
        assertEquals(false, StringUtils.isAlpha("hkHKHik*khbkuh"));
    }

    public void testIsAlphanumeric() {
        assertEquals(false, StringUtils.isAlphanumeric(null));
        assertEquals(true, StringUtils.isAlphanumeric(""));
        assertEquals(false, StringUtils.isAlphanumeric(" "));
        assertEquals(true, StringUtils.isAlphanumeric("a"));
        assertEquals(true, StringUtils.isAlphanumeric("A"));
        assertEquals(true, StringUtils.isAlphanumeric("kgKgKgKgkgkGkjkjlJlOKLgHdGdHgl"));
        assertEquals(false, StringUtils.isAlphanumeric("ham kso"));
        assertEquals(true, StringUtils.isAlphanumeric("1"));
        assertEquals(true, StringUtils.isAlphanumeric("hkHKHik6iUGHKJgU7tUJgKJGI87GIkug"));
        assertEquals(false, StringUtils.isAlphanumeric("_"));
        assertEquals(false, StringUtils.isAlphanumeric("hkHKHik*khbkuh"));
    }

    public void testIsWhitespace() {
        assertEquals(false, StringUtils.isWhitespace(null));
        assertEquals(true, StringUtils.isWhitespace(""));
        assertEquals(true, StringUtils.isWhitespace(" "));
        assertEquals(true, StringUtils.isWhitespace("\t \n \t"));
        assertEquals(false, StringUtils.isWhitespace("\t aa\n \t"));
        assertEquals(true, StringUtils.isWhitespace(" "));
        assertEquals(false, StringUtils.isWhitespace(" a "));
        assertEquals(false, StringUtils.isWhitespace("a  "));
        assertEquals(false, StringUtils.isWhitespace("  a"));
        assertEquals(false, StringUtils.isWhitespace("aba"));
    }

    public void testIsAlphaspace() {
        assertEquals(false, StringUtils.isAlphaSpace(null));
        assertEquals(true, StringUtils.isAlphaSpace(""));
        assertEquals(true, StringUtils.isAlphaSpace(" "));
        assertEquals(true, StringUtils.isAlphaSpace("a"));
        assertEquals(true, StringUtils.isAlphaSpace("A"));
        assertEquals(true, StringUtils.isAlphaSpace("kgKgKgKgkgkGkjkjlJlOKLgHdGdHgl"));
        assertEquals(true, StringUtils.isAlphaSpace("ham kso"));
        assertEquals(false, StringUtils.isAlphaSpace("1"));
        assertEquals(false, StringUtils.isAlphaSpace("hkHKHik6iUGHKJgU7tUJgKJGI87GIkug"));
        assertEquals(false, StringUtils.isAlphaSpace("_"));
        assertEquals(false, StringUtils.isAlphaSpace("hkHKHik*khbkuh"));
    }

    public void testIsAlphanumericSpace() {
        assertEquals(false, StringUtils.isAlphanumericSpace(null));
        assertEquals(true, StringUtils.isAlphanumericSpace(""));
        assertEquals(true, StringUtils.isAlphanumericSpace(" "));
        assertEquals(true, StringUtils.isAlphanumericSpace("a"));
        assertEquals(true, StringUtils.isAlphanumericSpace("A"));
        assertEquals(true, StringUtils.isAlphanumericSpace("kgKgKgKgkgkGkjkjlJlOKLgHdGdHgl"));
        assertEquals(true, StringUtils.isAlphanumericSpace("ham kso"));
        assertEquals(true, StringUtils.isAlphanumericSpace("1"));
        assertEquals(true, StringUtils.isAlphanumericSpace("hkHKHik6iUGHKJgU7tUJgKJGI87GIkug"));
        assertEquals(false, StringUtils.isAlphanumericSpace("_"));
        assertEquals(false, StringUtils.isAlphanumericSpace("hkHKHik*khbkuh"));
    }

    public void testIsNumeric() {
        assertEquals(false, StringUtils.isNumeric(null));
        assertEquals(true, StringUtils.isNumeric(""));
        assertEquals(false, StringUtils.isNumeric(" "));
        assertEquals(false, StringUtils.isNumeric("a"));
        assertEquals(false, StringUtils.isNumeric("A"));
        assertEquals(false, StringUtils.isNumeric("kgKgKgKgkgkGkjkjlJlOKLgHdGdHgl"));
        assertEquals(false, StringUtils.isNumeric("ham kso"));
        assertEquals(true, StringUtils.isNumeric("1"));
        assertEquals(true, StringUtils.isNumeric("1000"));
        assertEquals(false, StringUtils.isNumeric("2.3"));
        assertEquals(false, StringUtils.isNumeric("10 00"));
        assertEquals(false, StringUtils.isNumeric("hkHKHik6iUGHKJgU7tUJgKJGI87GIkug"));
        assertEquals(false, StringUtils.isNumeric("_"));
        assertEquals(false, StringUtils.isNumeric("hkHKHik*khbkuh"));
    }

    public void testIsNumericSpace() {
        assertEquals(false, StringUtils.isNumericSpace(null));
        assertEquals(true, StringUtils.isNumericSpace(""));
        assertEquals(true, StringUtils.isNumericSpace(" "));
        assertEquals(false, StringUtils.isNumericSpace("a"));
        assertEquals(false, StringUtils.isNumericSpace("A"));
        assertEquals(false, StringUtils.isNumericSpace("kgKgKgKgkgkGkjkjlJlOKLgHdGdHgl"));
        assertEquals(false, StringUtils.isNumericSpace("ham kso"));
        assertEquals(true, StringUtils.isNumericSpace("1"));
        assertEquals(true, StringUtils.isNumericSpace("1000"));
        assertEquals(false, StringUtils.isNumericSpace("2.3"));
        assertEquals(true, StringUtils.isNumericSpace("10 00"));
        assertEquals(false, StringUtils.isNumericSpace("hkHKHik6iUGHKJgU7tUJgKJGI87GIkug"));
        assertEquals(false, StringUtils.isNumericSpace("_"));
        assertEquals(false, StringUtils.isNumericSpace("hkHKHik*khbkuh"));
    }

}
