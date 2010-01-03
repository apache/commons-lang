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

import junit.framework.TestCase;

/**
 * Unit tests {@link org.apache.commons.lang3.StringUtils} - Substring methods
 *
 * @author Apache Software Foundation
 * @author Michael Davey
 * @version $Id$
 */
public class StringUtilsIsTest extends TestCase {

    public StringUtilsIsTest(String name) {
        super(name);
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
        assertEquals(true, StringUtils.isWhitespace(StringUtilsTest.WHITESPACE));
        assertEquals(false, StringUtils.isWhitespace(StringUtilsTest.NON_WHITESPACE));
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

    public void testIsAsciiPrintable_String() {
        assertEquals(false, StringUtils.isAsciiPrintable(null));
        assertEquals(true, StringUtils.isAsciiPrintable(""));
        assertEquals(true, StringUtils.isAsciiPrintable(" "));
        assertEquals(true, StringUtils.isAsciiPrintable("a"));
        assertEquals(true, StringUtils.isAsciiPrintable("A"));
        assertEquals(true, StringUtils.isAsciiPrintable("1"));
        assertEquals(true, StringUtils.isAsciiPrintable("Ceki"));
        assertEquals(true, StringUtils.isAsciiPrintable("!ab2c~"));
        assertEquals(true, StringUtils.isAsciiPrintable("1000"));
        assertEquals(true, StringUtils.isAsciiPrintable("10 00"));
        assertEquals(false, StringUtils.isAsciiPrintable("10\t00"));
        assertEquals(true, StringUtils.isAsciiPrintable("10.00"));
        assertEquals(true, StringUtils.isAsciiPrintable("10,00"));
        assertEquals(true, StringUtils.isAsciiPrintable("!ab-c~"));
        assertEquals(true, StringUtils.isAsciiPrintable("hkHK=Hik6i?UGH_KJgU7.tUJgKJ*GI87GI,kug"));
        assertEquals(true, StringUtils.isAsciiPrintable("\u0020"));
        assertEquals(true, StringUtils.isAsciiPrintable("\u0021"));
        assertEquals(true, StringUtils.isAsciiPrintable("\u007e"));
        assertEquals(false, StringUtils.isAsciiPrintable("\u007f"));
        assertEquals(true, StringUtils.isAsciiPrintable("G?lc?"));
        assertEquals(true, StringUtils.isAsciiPrintable("=?iso-8859-1?Q?G=FClc=FC?="));
        assertEquals(false, StringUtils.isAsciiPrintable("G\u00fclc\u00fc"));
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
