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

import java.util.Arrays;
import java.io.IOException;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Unit tests {@link StringUtils}.
 *
 * @author of original StringUtilsTest.testEscape = ?
 * @author <a href="mailto:alex@purpletech.com">Alexander Day Chaffee</a>
 * @version $Id: StringEscapeUtilsTest.java,v 1.1 2003/03/31 03:53:52 alex Exp $
 */
public class StringEscapeUtilsTest extends TestCase {
    private final static String FOO = "foo";

    public StringEscapeUtilsTest(String name) {
        super(name);
    }

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(StringEscapeUtilsTest.class);
        suite.setName("StringEscapeUtilsTest Tests");
        return suite;
    }

    //-----------------------------------------------------------------------

    public void testEscapeJava() throws IOException {
        assertEscapeJava("empty string", "", "");
        assertEscapeJava(FOO, FOO);
        assertEscapeJava("tab", "\\t", "\t");
        assertEscapeJava("backslash", "\\\\", "\\");
        assertEscapeJava("single quote should not be escaped", "'", "'");
        assertEscapeJava("\\\\\\b\\t\\r", "\\\b\t\r");
        assertEscapeJava("\\u1234", "\u1234");
        assertEscapeJava("\\u0234", "\u0234");
        assertEscapeJava("\\u00fd", "\u00fd");

        assertEscapeJava("He didn't say, \\\"stop!\\\"",
                "He didn't say, \"stop!\"");
        assertEscapeJava("non-breaking space", "This space is non-breaking:" + "\\u00a0",
                "This space is non-breaking:\u00a0");
        assertEscapeJava("\\uabcd\\u1234\\u012c",
                "\uABCD\u1234\u012C");
    }

    private void assertEscapeJava(String escaped, String original) throws IOException {
        assertEscapeJava(null, escaped, original);
    }

    private void assertEscapeJava(String message, String expected, String original) throws IOException {
        String converted = StringEscapeUtils.escapeJava(original);
        message = "escapeJava(String) failed" + (message == null ? "" : (": " + message));
        assertEquals(message, expected, converted);

        StringPrintWriter writer = new StringPrintWriter();
        StringEscapeUtils.escapeJava(writer, original);
        assertEquals(expected, writer.getString());
    }

    public void testUnescapeJava() throws IOException {
        assertUnescapeJava("", "");
        assertUnescapeJava("test", "test");
        assertUnescapeJava("\ntest\b", "\\ntest\\b");
        assertUnescapeJava("\u123425foo\ntest\b", "\\u123425foo\\ntest\\b");
    }

    private void assertUnescapeJava(String unescaped, String original) throws IOException {
        assertEquals("unescape(String) failed",
                unescaped, StringUtils.unescape(original));

        StringPrintWriter writer = new StringPrintWriter();
        StringEscapeUtils.unescapeJava(writer, original);
        assertEquals(unescaped, writer.getString());

    }

    public void testEscapeJavaScript() {
        assertEquals("He didn\\'t say, \\\"stop!\\\"", StringEscapeUtils.escapeJavaScript("He didn't say, \"stop!\""));
    }


    // HTML
    //--------------------------------------------------------------
    String[][] htmlEscapes = {
        {"no escaping", "plain text", "plain text"},
        {"no escaping", "plain text", "plain text"},
        {"empty string", "", ""},
        {"ampersand", "bread &amp; butter", "bread & butter"},
        {"quotes", "&quot;bread&quot; &amp; butter", "\"bread\" & butter"},
        {"final character only", "greater than &gt;", "greater than >"},
        {"first character only", "&lt; less than", "< less than"},
        {"apostrophe", "Huntington's chorea", "Huntington's chorea"},
        {"languages", "English,Fran&ccedil;ais,&#26085;&#26412;&#35486; (nihongo)", "English,Français,\u65E5\u672C\u8A9E (nihongo)"},
    };

    public void testEscapeHtml() {
        for (int i = 0; i < htmlEscapes.length; ++i) {
            assertEquals(htmlEscapes[i][0], htmlEscapes[i][1], StringEscapeUtils.escapeHtml(htmlEscapes[i][2]));
            // todo: add test for (and implement) Writer-based version
        }
    }

    public void testHtmlunescape() {
        for (int i = 0; i < htmlEscapes.length; ++i) {
            assertEquals(htmlEscapes[i][0], htmlEscapes[i][2], StringEscapeUtils.unescapeHtml(htmlEscapes[i][1]));
            // todo: add test for (and implement) Writer-based version
        }
        // should we unicode-escape the cedilla here, for 7-bit cleanliness?
        assertEquals("funny chars pass through OK", "Français", StringEscapeUtils.unescapeHtml("Français"));
    }

    // SQL
    // see http://www.jguru.com/faq/view.jsp?EID=8881
    //--------------------
//    public void testEscapeSQL() throws Exception {
    // sql doubles-up single-quotes
//    }
}

