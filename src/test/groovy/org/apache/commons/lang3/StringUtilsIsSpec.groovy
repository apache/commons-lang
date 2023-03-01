/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.commons.lang3


import spock.lang.Title

@Title("org.apache.commons.lang3.StringUtilsIsTest")
class StringUtilsIsSpec extends AbstractLangSpec {

    def "testIsAlpha"() {
        expect:
        StringUtils.isAlpha("a")
        StringUtils.isAlpha("A")
        StringUtils.isAlpha("kgKgKgKgkgkGkjkjlJlOKLgHdGdHgl")
        !StringUtils.isAlpha(null)
        !StringUtils.isAlpha("")
        !StringUtils.isAlpha(" ")
        !StringUtils.isAlpha("ham kso")
        !StringUtils.isAlpha("1")
        !StringUtils.isAlpha("hkHKHik6iUGHKJgU7tUJgKJGI87GIkug")
        !StringUtils.isAlpha("_")
        !StringUtils.isAlpha("hkHKHik*khbkuh")
    }

    def "testIsAlphanumeric"() {
        expect:
        StringUtils.isAlphanumeric("a")
        StringUtils.isAlphanumeric("A")
        !StringUtils.isAlphanumeric(null)
        !StringUtils.isAlphanumeric("")
        !StringUtils.isAlphanumeric(" ")
        StringUtils.isAlphanumeric("kgKgKgKgkgkGkjkjlJlOKLgHdGdHgl")
        !StringUtils.isAlphanumeric("ham kso")
        StringUtils.isAlphanumeric("1")
        StringUtils.isAlphanumeric("hkHKHik6iUGHKJgU7tUJgKJGI87GIkug")
        !StringUtils.isAlphanumeric("_")
        !StringUtils.isAlphanumeric("hkHKHik*khbkuh")
    }

    def "testIsAlphanumericSpace"() {
        expect:
        !StringUtils.isAlphanumericSpace(null)
        StringUtils.isAlphanumericSpace("")
        StringUtils.isAlphanumericSpace(" ")
        StringUtils.isAlphanumericSpace("a")
        StringUtils.isAlphanumericSpace("A")
        StringUtils.isAlphanumericSpace("kgKgKgKgkgkGkjkjlJlOKLgHdGdHgl")
        StringUtils.isAlphanumericSpace("ham kso")
        StringUtils.isAlphanumericSpace("1")
        StringUtils.isAlphanumericSpace("hkHKHik6iUGHKJgU7tUJgKJGI87GIkug")
        !StringUtils.isAlphanumericSpace("_")
        !StringUtils.isAlphanumericSpace("hkHKHik*khbkuh")
    }

    def "testIsAlphaspace"() {
        expect:
        !StringUtils.isAlphaSpace(null)
        StringUtils.isAlphaSpace("")
        StringUtils.isAlphaSpace(" ")
        StringUtils.isAlphaSpace("a")
        StringUtils.isAlphaSpace("A")
        StringUtils.isAlphaSpace("kgKgKgKgkgkGkjkjlJlOKLgHdGdHgl")
        StringUtils.isAlphaSpace("ham kso")
        !StringUtils.isAlphaSpace("1")
        !StringUtils.isAlphaSpace("hkHKHik6iUGHKJgU7tUJgKJGI87GIkug")
        !StringUtils.isAlphaSpace("_")
        !StringUtils.isAlphaSpace("hkHKHik*khbkuh")
    }

    def "testIsAsciiPrintable_String"() {
        expect:
        !StringUtils.isAsciiPrintable(null)
        StringUtils.isAsciiPrintable("")
        StringUtils.isAsciiPrintable(" ")
        StringUtils.isAsciiPrintable("a")
        StringUtils.isAsciiPrintable("A")
        StringUtils.isAsciiPrintable("1")
        StringUtils.isAsciiPrintable("Ceki")
        StringUtils.isAsciiPrintable("!ab2c~")
        StringUtils.isAsciiPrintable("1000")
        StringUtils.isAsciiPrintable("10 00")
        !StringUtils.isAsciiPrintable("10\t00")
        StringUtils.isAsciiPrintable("10.00")
        StringUtils.isAsciiPrintable("10,00")
        StringUtils.isAsciiPrintable("!ab-c~")
        StringUtils.isAsciiPrintable("hkHK=Hik6i?UGH_KJgU7.tUJgKJ*GI87GI,kug")
        StringUtils.isAsciiPrintable("\u0020")
        StringUtils.isAsciiPrintable("\u0021")
        StringUtils.isAsciiPrintable("\u007e")
        !StringUtils.isAsciiPrintable("\u007f")
        StringUtils.isAsciiPrintable("G?lc?")
        StringUtils.isAsciiPrintable("=?iso-8859-1?Q?G=FClc=FC?=")
        !StringUtils.isAsciiPrintable("G\u00fclc\u00fc")
    }

    def "testIsNumeric"() {
        expect:
        !StringUtils.isNumeric(null)
        !StringUtils.isNumeric("")
        !StringUtils.isNumeric(" ")
        !StringUtils.isNumeric("a")
        !StringUtils.isNumeric("A")
        !StringUtils.isNumeric("kgKgKgKgkgkGkjkjlJlOKLgHdGdHgl")
        !StringUtils.isNumeric("ham kso")
        StringUtils.isNumeric("1")
        StringUtils.isNumeric("1000")
        StringUtils.isNumeric("\u0967\u0968\u0969")
        !StringUtils.isNumeric("\u0967\u0968 \u0969")
        !StringUtils.isNumeric("2.3")
        !StringUtils.isNumeric("10 00")
        !StringUtils.isNumeric("hkHKHik6iUGHKJgU7tUJgKJGI87GIkug")
        !StringUtils.isNumeric("_")
        !StringUtils.isNumeric("hkHKHik*khbkuh")
        !StringUtils.isNumeric("+123")
        !StringUtils.isNumeric("-123")
    }

    def "testIsNumericSpace"() {
        expect:
        !StringUtils.isNumericSpace(null)
        StringUtils.isNumericSpace("")
        StringUtils.isNumericSpace(" ")
        !StringUtils.isNumericSpace("a")
        !StringUtils.isNumericSpace("A")
        !StringUtils.isNumericSpace("kgKgKgKgkgkGkjkjlJlOKLgHdGdHgl")
        !StringUtils.isNumericSpace("ham kso")
        StringUtils.isNumericSpace("1")
        StringUtils.isNumericSpace("1000")
        !StringUtils.isNumericSpace("2.3")
        StringUtils.isNumericSpace("10 00")
        StringUtils.isNumericSpace("\u0967\u0968\u0969")
        StringUtils.isNumericSpace("\u0967\u0968 \u0969")
        !StringUtils.isNumericSpace("hkHKHik6iUGHKJgU7tUJgKJGI87GIkug")
        !StringUtils.isNumericSpace("_")
        !StringUtils.isNumericSpace("hkHKHik*khbkuh")
    }

    def "testIsWhitespace"() {
        expect:
        !StringUtils.isWhitespace(null)
        StringUtils.isWhitespace("")
        StringUtils.isWhitespace(" ")
        StringUtils.isWhitespace("\t \n \t")
        !StringUtils.isWhitespace("\t aa\n \t")
        StringUtils.isWhitespace(" ")
        !StringUtils.isWhitespace(" a ")
        !StringUtils.isWhitespace("a  ")
        !StringUtils.isWhitespace("  a")
        !StringUtils.isWhitespace("aba")
        StringUtils.isWhitespace(StringUtilsTest.WHITESPACE)
        !StringUtils.isWhitespace(StringUtilsTest.NON_WHITESPACE)
    }

}
