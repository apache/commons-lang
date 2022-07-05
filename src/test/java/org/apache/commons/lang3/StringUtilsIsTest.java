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

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

/**
 * Unit tests {@link org.apache.commons.lang3.StringUtils} - IsX methods
 */
public class StringUtilsIsTest extends AbstractLangTest {

    @Test
    public void testIsAlpha() {
        assertFalse(StringUtils.isAlpha(null));
        assertFalse(StringUtils.isAlpha(""));
        assertFalse(StringUtils.isAlpha(" "));
        assertTrue(StringUtils.isAlpha("a"));
        assertTrue(StringUtils.isAlpha("A"));
        assertTrue(StringUtils.isAlpha("kgKgKgKgkgkGkjkjlJlOKLgHdGdHgl"));
        assertFalse(StringUtils.isAlpha("ham kso"));
        assertFalse(StringUtils.isAlpha("1"));
        assertFalse(StringUtils.isAlpha("hkHKHik6iUGHKJgU7tUJgKJGI87GIkug"));
        assertFalse(StringUtils.isAlpha("_"));
        assertFalse(StringUtils.isAlpha("hkHKHik*khbkuh"));
    }

    @Test
    public void testIsAlphanumeric() {
        assertFalse(StringUtils.isAlphanumeric(null));
        assertFalse(StringUtils.isAlphanumeric(""));
        assertFalse(StringUtils.isAlphanumeric(" "));
        assertTrue(StringUtils.isAlphanumeric("a"));
        assertTrue(StringUtils.isAlphanumeric("A"));
        assertTrue(StringUtils.isAlphanumeric("kgKgKgKgkgkGkjkjlJlOKLgHdGdHgl"));
        assertFalse(StringUtils.isAlphanumeric("ham kso"));
        assertTrue(StringUtils.isAlphanumeric("1"));
        assertTrue(StringUtils.isAlphanumeric("hkHKHik6iUGHKJgU7tUJgKJGI87GIkug"));
        assertFalse(StringUtils.isAlphanumeric("_"));
        assertFalse(StringUtils.isAlphanumeric("hkHKHik*khbkuh"));
    }

    @Test
    public void testIsAlphanumericSpace() {
        assertFalse(StringUtils.isAlphanumericSpace(null));
        assertTrue(StringUtils.isAlphanumericSpace(""));
        assertTrue(StringUtils.isAlphanumericSpace(" "));
        assertTrue(StringUtils.isAlphanumericSpace("a"));
        assertTrue(StringUtils.isAlphanumericSpace("A"));
        assertTrue(StringUtils.isAlphanumericSpace("kgKgKgKgkgkGkjkjlJlOKLgHdGdHgl"));
        assertTrue(StringUtils.isAlphanumericSpace("ham kso"));
        assertTrue(StringUtils.isAlphanumericSpace("1"));
        assertTrue(StringUtils.isAlphanumericSpace("hkHKHik6iUGHKJgU7tUJgKJGI87GIkug"));
        assertFalse(StringUtils.isAlphanumericSpace("_"));
        assertFalse(StringUtils.isAlphanumericSpace("hkHKHik*khbkuh"));
    }

    @Test
    public void testIsAlphaspace() {
        assertFalse(StringUtils.isAlphaSpace(null));
        assertTrue(StringUtils.isAlphaSpace(""));
        assertTrue(StringUtils.isAlphaSpace(" "));
        assertTrue(StringUtils.isAlphaSpace("a"));
        assertTrue(StringUtils.isAlphaSpace("A"));
        assertTrue(StringUtils.isAlphaSpace("kgKgKgKgkgkGkjkjlJlOKLgHdGdHgl"));
        assertTrue(StringUtils.isAlphaSpace("ham kso"));
        assertFalse(StringUtils.isAlphaSpace("1"));
        assertFalse(StringUtils.isAlphaSpace("hkHKHik6iUGHKJgU7tUJgKJGI87GIkug"));
        assertFalse(StringUtils.isAlphaSpace("_"));
        assertFalse(StringUtils.isAlphaSpace("hkHKHik*khbkuh"));
    }

    @Test
    public void testIsAsciiPrintable_String() {
        assertFalse(StringUtils.isAsciiPrintable(null));
        assertTrue(StringUtils.isAsciiPrintable(""));
        assertTrue(StringUtils.isAsciiPrintable(" "));
        assertTrue(StringUtils.isAsciiPrintable("a"));
        assertTrue(StringUtils.isAsciiPrintable("A"));
        assertTrue(StringUtils.isAsciiPrintable("1"));
        assertTrue(StringUtils.isAsciiPrintable("Ceki"));
        assertTrue(StringUtils.isAsciiPrintable("!ab2c~"));
        assertTrue(StringUtils.isAsciiPrintable("1000"));
        assertTrue(StringUtils.isAsciiPrintable("10 00"));
        assertFalse(StringUtils.isAsciiPrintable("10\t00"));
        assertTrue(StringUtils.isAsciiPrintable("10.00"));
        assertTrue(StringUtils.isAsciiPrintable("10,00"));
        assertTrue(StringUtils.isAsciiPrintable("!ab-c~"));
        assertTrue(StringUtils.isAsciiPrintable("hkHK=Hik6i?UGH_KJgU7.tUJgKJ*GI87GI,kug"));
        assertTrue(StringUtils.isAsciiPrintable("\u0020"));
        assertTrue(StringUtils.isAsciiPrintable("\u0021"));
        assertTrue(StringUtils.isAsciiPrintable("\u007e"));
        assertFalse(StringUtils.isAsciiPrintable("\u007f"));
        assertTrue(StringUtils.isAsciiPrintable("G?lc?"));
        assertTrue(StringUtils.isAsciiPrintable("=?iso-8859-1?Q?G=FClc=FC?="));
        assertFalse(StringUtils.isAsciiPrintable("G\u00fclc\u00fc"));
    }

    @Test
    public void testIsNumeric() {
        assertFalse(StringUtils.isNumeric(null));
        assertFalse(StringUtils.isNumeric(""));
        assertFalse(StringUtils.isNumeric(" "));
        assertFalse(StringUtils.isNumeric("a"));
        assertFalse(StringUtils.isNumeric("A"));
        assertFalse(StringUtils.isNumeric("kgKgKgKgkgkGkjkjlJlOKLgHdGdHgl"));
        assertFalse(StringUtils.isNumeric("ham kso"));
        assertTrue(StringUtils.isNumeric("1"));
        assertTrue(StringUtils.isNumeric("1000"));
        assertTrue(StringUtils.isNumeric("\u0967\u0968\u0969"));
        assertFalse(StringUtils.isNumeric("\u0967\u0968 \u0969"));
        assertFalse(StringUtils.isNumeric("2.3"));
        assertFalse(StringUtils.isNumeric("10 00"));
        assertFalse(StringUtils.isNumeric("hkHKHik6iUGHKJgU7tUJgKJGI87GIkug"));
        assertFalse(StringUtils.isNumeric("_"));
        assertFalse(StringUtils.isNumeric("hkHKHik*khbkuh"));
        assertFalse(StringUtils.isNumeric("+123"));
        assertFalse(StringUtils.isNumeric("-123"));
    }

    @Test
    public void testIsNumericSpace() {
        assertFalse(StringUtils.isNumericSpace(null));
        assertTrue(StringUtils.isNumericSpace(""));
        assertTrue(StringUtils.isNumericSpace(" "));
        assertFalse(StringUtils.isNumericSpace("a"));
        assertFalse(StringUtils.isNumericSpace("A"));
        assertFalse(StringUtils.isNumericSpace("kgKgKgKgkgkGkjkjlJlOKLgHdGdHgl"));
        assertFalse(StringUtils.isNumericSpace("ham kso"));
        assertTrue(StringUtils.isNumericSpace("1"));
        assertTrue(StringUtils.isNumericSpace("1000"));
        assertFalse(StringUtils.isNumericSpace("2.3"));
        assertTrue(StringUtils.isNumericSpace("10 00"));
        assertTrue(StringUtils.isNumericSpace("\u0967\u0968\u0969"));
        assertTrue(StringUtils.isNumericSpace("\u0967\u0968 \u0969"));
        assertFalse(StringUtils.isNumericSpace("hkHKHik6iUGHKJgU7tUJgKJGI87GIkug"));
        assertFalse(StringUtils.isNumericSpace("_"));
        assertFalse(StringUtils.isNumericSpace("hkHKHik*khbkuh"));
    }

    @Test
    public void testIsWhitespace() {
        assertFalse(StringUtils.isWhitespace(null));
        assertTrue(StringUtils.isWhitespace(""));
        assertTrue(StringUtils.isWhitespace(" "));
        assertTrue(StringUtils.isWhitespace("\t \n \t"));
        assertFalse(StringUtils.isWhitespace("\t aa\n \t"));
        assertTrue(StringUtils.isWhitespace(" "));
        assertFalse(StringUtils.isWhitespace(" a "));
        assertFalse(StringUtils.isWhitespace("a  "));
        assertFalse(StringUtils.isWhitespace("  a"));
        assertFalse(StringUtils.isWhitespace("aba"));
        assertTrue(StringUtils.isWhitespace(StringUtilsTest.WHITESPACE));
        assertFalse(StringUtils.isWhitespace(StringUtilsTest.NON_WHITESPACE));
    }

}
