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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.io.StringWriter;
import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;

import org.apache.commons.lang3.text.translate.CharSequenceTranslator;
import org.apache.commons.lang3.text.translate.NumericEntityEscaper;
import org.junit.jupiter.api.Test;

/**
 * Unit tests for {@link StringEscapeUtils}.
 */
@Deprecated
public class StringEscapeUtilsTest extends AbstractLangTest {
    private static final String FOO = "foo";

    @Test
    public void testConstructor() {
        assertNotNull(new StringEscapeUtils());
        final Constructor<?>[] cons = StringEscapeUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertTrue(Modifier.isPublic(cons[0].getModifiers()));
        assertTrue(Modifier.isPublic(StringEscapeUtils.class.getModifiers()));
        assertFalse(Modifier.isFinal(StringEscapeUtils.class.getModifiers()));
    }

    @Test
    public void testEscapeJava() throws IOException {
        assertNull(StringEscapeUtils.escapeJava(null));
        assertThrows(NullPointerException.class, () -> StringEscapeUtils.ESCAPE_JAVA.translate(null, null));
        assertThrows(NullPointerException.class, () -> StringEscapeUtils.ESCAPE_JAVA.translate("", null));

        assertEscapeJava("empty string", "", "");
        assertEscapeJava(FOO, FOO);
        assertEscapeJava("tab", "\\t", "\t");
        assertEscapeJava("backslash", "\\\\", "\\");
        assertEscapeJava("single quote should not be escaped", "'", "'");
        assertEscapeJava("\\\\\\b\\t\\r", "\\\b\t\r");
        assertEscapeJava("\\u1234", "\u1234");
        assertEscapeJava("\\u0234", "\u0234");
        assertEscapeJava("\\u00EF", "\u00ef");
        assertEscapeJava("\\u0001", "\u0001");
        assertEscapeJava("Should use capitalized Unicode hex", "\\uABCD", "\uabcd");

        assertEscapeJava("He didn't say, \\\"stop!\\\"",
                "He didn't say, \"stop!\"");
        assertEscapeJava("non-breaking space", "This space is non-breaking:" + "\\u00A0",
                "This space is non-breaking:\u00a0");
        assertEscapeJava("\\uABCD\\u1234\\u012C",
                "\uABCD\u1234\u012C");
    }

    /**
     * Tests https://issues.apache.org/jira/browse/LANG-421
     */
    @Test
    public void testEscapeJavaWithSlash() {
        final String input = "String with a slash (/) in it";

        final String expected = input;
        final String actual = StringEscapeUtils.escapeJava(input);

        /*
         * In 2.4 StringEscapeUtils.escapeJava(String) escapes '/' characters, which are not a valid character to escape
         * in a Java string.
         */
        assertEquals(expected, actual);
    }

    private void assertEscapeJava(final String escaped, final String original) throws IOException {
        assertEscapeJava(null, escaped, original);
    }

    private void assertEscapeJava(String message, final String expected, final String original) throws IOException {
        final String converted = StringEscapeUtils.escapeJava(original);
        message = "escapeJava(String) failed" + (message == null ? "" : (": " + message));
        assertEquals(expected, converted, message);

        final StringWriter writer = new StringWriter();
        StringEscapeUtils.ESCAPE_JAVA.translate(original, writer);
        assertEquals(expected, writer.toString());
    }

    @Test
    public void testUnescapeJava() throws IOException {
        assertNull(StringEscapeUtils.unescapeJava(null));
        assertThrows(NullPointerException.class, () -> StringEscapeUtils.UNESCAPE_JAVA.translate(null, null));
        assertThrows(NullPointerException.class, () -> StringEscapeUtils.UNESCAPE_JAVA.translate("", null));
        assertThrows(RuntimeException.class, () -> StringEscapeUtils.unescapeJava("\\u02-3"));

        assertUnescapeJava("", "");
        assertUnescapeJava("test", "test");
        assertUnescapeJava("\ntest\b", "\\ntest\\b");
        assertUnescapeJava("\u123425foo\ntest\b", "\\u123425foo\\ntest\\b");
        assertUnescapeJava("'\foo\teste\r", "\\'\\foo\\teste\\r");
        assertUnescapeJava("", "\\");
        //foo
        assertUnescapeJava("lowercase Unicode", "\uABCDx", "\\uabcdx");
        assertUnescapeJava("uppercase Unicode", "\uABCDx", "\\uABCDx");
        assertUnescapeJava("Unicode as final character", "\uABCD", "\\uabcd");
    }

    private void assertUnescapeJava(final String unescaped, final String original) throws IOException {
        assertUnescapeJava(null, unescaped, original);
    }

    private void assertUnescapeJava(final String message, final String unescaped, final String original) throws IOException {
        final String expected = unescaped;
        final String actual = StringEscapeUtils.unescapeJava(original);

        assertEquals(expected, actual,
                "unescape(String) failed" +
                (message == null ? "" : (": " + message)) +
                ": expected '" + StringEscapeUtils.escapeJava(expected) +
                // we escape this so we can see it in the error message
                "' actual '" + StringEscapeUtils.escapeJava(actual) + "'");

        final StringWriter writer = new StringWriter();
        StringEscapeUtils.UNESCAPE_JAVA.translate(original, writer);
        assertEquals(unescaped, writer.toString());

    }

    @Test
    public void testEscapeEcmaScript() {
        assertNull(StringEscapeUtils.escapeEcmaScript(null));
        assertThrows(NullPointerException.class, () -> StringEscapeUtils.ESCAPE_ECMASCRIPT.translate(null, null));
        assertThrows(NullPointerException.class, () -> StringEscapeUtils.ESCAPE_ECMASCRIPT.translate("", null));

        assertEquals("He didn\\'t say, \\\"stop!\\\"", StringEscapeUtils.escapeEcmaScript("He didn't say, \"stop!\""));
        assertEquals("document.getElementById(\\\"test\\\").value = \\'<script>alert(\\'aaa\\');<\\/script>\\';",
                StringEscapeUtils.escapeEcmaScript("document.getElementById(\"test\").value = '<script>alert('aaa');</script>';"));
    }

    @Test
    public void testUnescapeEcmaScript() {
        assertNull(StringEscapeUtils.escapeEcmaScript(null));
        assertThrows(NullPointerException.class, () -> StringEscapeUtils.UNESCAPE_ECMASCRIPT.translate(null, null));
        assertThrows(NullPointerException.class, () -> StringEscapeUtils.UNESCAPE_ECMASCRIPT.translate("", null));

        assertEquals("He didn't say, \"stop!\"", StringEscapeUtils.unescapeEcmaScript("He didn\\'t say, \\\"stop!\\\""));
        assertEquals("document.getElementById(\"test\").value = '<script>alert('aaa');</script>';",
                StringEscapeUtils.unescapeEcmaScript("document.getElementById(\\\"test\\\").value = \\'<script>alert(\\'aaa\\');<\\/script>\\';"));
    }


    /** HTML and XML */
    private static final String[][] HTML_ESCAPES = {
        {"no escaping", "plain text", "plain text"},
        {"no escaping", "plain text", "plain text"},
        {"empty string", "", ""},
        {"null", null, null},
        {"ampersand", "bread &amp; butter", "bread & butter"},
        {"quotes", "&quot;bread&quot; &amp; butter", "\"bread\" & butter"},
        {"final character only", "greater than &gt;", "greater than >"},
        {"first character only", "&lt; less than", "< less than"},
        {"apostrophe", "Huntington's chorea", "Huntington's chorea"},
        {"languages", "English,Fran&ccedil;ais,\u65E5\u672C\u8A9E (nihongo)", "English,Fran\u00E7ais,\u65E5\u672C\u8A9E (nihongo)"},
        {"8-bit ascii shouldn't number-escape", "\u0080\u009F", "\u0080\u009F"},
    };

    @Test
    public void testEscapeHtml() throws IOException {
        for (final String[] element : HTML_ESCAPES) {
            final String message = element[0];
            final String expected = element[1];
            final String original = element[2];
            assertEquals(expected, StringEscapeUtils.escapeHtml4(original), message);
            final StringWriter sw = new StringWriter();
            StringEscapeUtils.ESCAPE_HTML4.translate(original, sw);
            final String actual = original == null ? null : sw.toString();
            assertEquals(expected, actual, message);
        }
    }

    @Test
    public void testUnescapeHtml4() throws IOException {
        for (final String[] element : HTML_ESCAPES) {
            final String message = element[0];
            final String expected = element[2];
            final String original = element[1];
            assertEquals(expected, StringEscapeUtils.unescapeHtml4(original), message);

            final StringWriter sw = new StringWriter();
            StringEscapeUtils.UNESCAPE_HTML4.translate(original, sw);
            final String actual = original == null ? null : sw.toString();
            assertEquals(expected, actual, message);
        }
        // \u00E7 is a cedilla (c with wiggle under)
        // note that the test string must be 7-bit-clean (Unicode escaped) or else it will compile incorrectly
        // on some locales
        assertEquals("Fran\u00E7ais", StringEscapeUtils.unescapeHtml4("Fran\u00E7ais"), "funny chars pass through OK");

        assertEquals("Hello&;World", StringEscapeUtils.unescapeHtml4("Hello&;World"));
        assertEquals("Hello&#;World", StringEscapeUtils.unescapeHtml4("Hello&#;World"));
        assertEquals("Hello&# ;World", StringEscapeUtils.unescapeHtml4("Hello&# ;World"));
        assertEquals("Hello&##;World", StringEscapeUtils.unescapeHtml4("Hello&##;World"));
    }

    @Test
    public void testUnescapeHexCharsHtml() {
        // Simple easy to grok test
        assertEquals("\u0080\u009F", StringEscapeUtils.unescapeHtml4("&#x80;&#x9F;"), "hex number unescape");
        assertEquals("\u0080\u009F", StringEscapeUtils.unescapeHtml4("&#X80;&#X9F;"), "hex number unescape");
        // Test all Character values:
        for (char i = Character.MIN_VALUE; i < Character.MAX_VALUE; i++) {
            final Character c1 = Character.valueOf(i);
            final Character c2 = Character.valueOf((char) (i+1));
            final String expected = c1.toString() + c2.toString();
            final String escapedC1 = "&#x" + Integer.toHexString((c1.charValue())) + ";";
            final String escapedC2 = "&#x" + Integer.toHexString((c2.charValue())) + ";";
            assertEquals(expected, StringEscapeUtils.unescapeHtml4(escapedC1 + escapedC2), "hex number unescape index " + (int) i);
        }
    }

    @Test
    public void testUnescapeUnknownEntity() {
        assertEquals("&zzzz;", StringEscapeUtils.unescapeHtml4("&zzzz;"));
    }

    @Test
    public void testEscapeHtmlVersions() {
        assertEquals("&Beta;", StringEscapeUtils.escapeHtml4("\u0392"));
        assertEquals("\u0392", StringEscapeUtils.unescapeHtml4("&Beta;"));

        // TODO: refine API for escaping/unescaping specific HTML versions
    }

    @Test
    public void testEscapeXml() throws Exception {
        assertEquals("&lt;abc&gt;", StringEscapeUtils.escapeXml("<abc>"));
        assertEquals("<abc>", StringEscapeUtils.unescapeXml("&lt;abc&gt;"));

        assertEquals("\u00A1", StringEscapeUtils.escapeXml("\u00A1"), "XML should not escape >0x7f values");
        assertEquals("\u00A0", StringEscapeUtils.unescapeXml("&#160;"), "XML should be able to unescape >0x7f values");
        assertEquals("\u00A0", StringEscapeUtils.unescapeXml("&#0160;"),
                "XML should be able to unescape >0x7f values with one leading 0");
        assertEquals("\u00A0", StringEscapeUtils.unescapeXml("&#00160;"),
                "XML should be able to unescape >0x7f values with two leading 0s");
        assertEquals("\u00A0", StringEscapeUtils.unescapeXml("&#000160;"),
                "XML should be able to unescape >0x7f values with three leading 0s");

        assertEquals("ain't", StringEscapeUtils.unescapeXml("ain&apos;t"));
        assertEquals("ain&apos;t", StringEscapeUtils.escapeXml("ain't"));
        assertEquals("", StringEscapeUtils.escapeXml(""));
        assertNull(StringEscapeUtils.escapeXml(null));
        assertNull(StringEscapeUtils.unescapeXml(null));

        StringWriter sw = new StringWriter();
        StringEscapeUtils.ESCAPE_XML.translate("<abc>", sw);
        assertEquals("&lt;abc&gt;", sw.toString(), "XML was escaped incorrectly");

        sw = new StringWriter();
        StringEscapeUtils.UNESCAPE_XML.translate("&lt;abc&gt;", sw);
        assertEquals("<abc>", sw.toString(), "XML was unescaped incorrectly");
    }

    @Test
    public void testEscapeXml10() {
        assertEquals("a&lt;b&gt;c&quot;d&apos;e&amp;f", StringEscapeUtils.escapeXml10("a<b>c\"d'e&f"));
        assertEquals("a\tb\rc\nd", StringEscapeUtils.escapeXml10("a\tb\rc\nd"), "XML 1.0 should not escape \t \n \r");
        assertEquals("ab", StringEscapeUtils.escapeXml10("a\u0000\u0001\u0008\u000b\u000c\u000e\u001fb"),
                "XML 1.0 should omit most #x0-x8 | #xb | #xc | #xe-#x19");
        assertEquals("a\ud7ff  \ue000b", StringEscapeUtils.escapeXml10("a\ud7ff\ud800 \udfff \ue000b"),
                "XML 1.0 should omit #xd800-#xdfff");
        assertEquals("a\ufffdb", StringEscapeUtils.escapeXml10("a\ufffd\ufffe\uffffb"),
                "XML 1.0 should omit #xfffe | #xffff");
        assertEquals("a\u007e&#127;&#132;\u0085&#134;&#159;\u00a0b",
                StringEscapeUtils.escapeXml10("a\u007e\u007f\u0084\u0085\u0086\u009f\u00a0b"),
                "XML 1.0 should escape #x7f-#x84 | #x86 - #x9f, for XML 1.1 compatibility");
    }

    @Test
    public void testEscapeXml11() {
        assertEquals("a&lt;b&gt;c&quot;d&apos;e&amp;f", StringEscapeUtils.escapeXml11("a<b>c\"d'e&f"));
        assertEquals("a\tb\rc\nd", StringEscapeUtils.escapeXml11("a\tb\rc\nd"), "XML 1.1 should not escape \t \n \r");
        assertEquals("ab", StringEscapeUtils.escapeXml11("a\u0000b"), "XML 1.1 should omit #x0");
        assertEquals("a&#1;&#8;&#11;&#12;&#14;&#31;b",
                StringEscapeUtils.escapeXml11("a\u0001\u0008\u000b\u000c\u000e\u001fb"),
                "XML 1.1 should escape #x1-x8 | #xb | #xc | #xe-#x19");
        assertEquals("a\u007e&#127;&#132;\u0085&#134;&#159;\u00a0b",
                StringEscapeUtils.escapeXml11("a\u007e\u007f\u0084\u0085\u0086\u009f\u00a0b"),
                "XML 1.1 should escape #x7F-#x84 | #x86-#x9F");
        assertEquals("a\ud7ff  \ue000b", StringEscapeUtils.escapeXml11("a\ud7ff\ud800 \udfff \ue000b"),
                "XML 1.1 should omit #xd800-#xdfff");
        assertEquals("a\ufffdb", StringEscapeUtils.escapeXml11("a\ufffd\ufffe\uffffb"),
                "XML 1.1 should omit #xfffe | #xffff");
    }

    /**
     * Tests Supplementary characters.
     * <p>
     * From https://www.w3.org/International/questions/qa-escapes
     * </p>
     * <blockquote>
     * Supplementary characters are those Unicode characters that have code points higher than the characters in
     * the Basic Multilingual Plane (BMP). In UTF-16 a supplementary character is encoded using two 16-bit surrogate code points from the
     * BMP. Because of this, some people think that supplementary characters need to be represented using two escapes, but this is incorrect
     * - you must use the single, code point value for that character. For example, use &amp;&#35;x233B4&#59; rather than
     * &amp;&#35;xD84C&#59;&amp;&#35;xDFB4&#59;.
     * </blockquote>
     * @see <a href="https://www.w3.org/International/questions/qa-escapes">Using character escapes in markup and CSS</a>
     * @see <a href="https://issues.apache.org/jira/browse/LANG-728">LANG-728</a>
     */
    @Test
    public void testEscapeXmlSupplementaryCharacters() {
        final CharSequenceTranslator escapeXml =
            StringEscapeUtils.ESCAPE_XML.with( NumericEntityEscaper.between(0x7f, Integer.MAX_VALUE) );

        assertEquals("&#144308;", escapeXml.translate("\uD84C\uDFB4"),
                "Supplementary character must be represented using a single escape");

        assertEquals("a b c &#144308;", escapeXml.translate("a b c \uD84C\uDFB4"),
                "Supplementary characters mixed with basic characters should be encoded correctly");
    }

    @Test
    public void testEscapeXmlAllCharacters() {
        // https://www.w3.org/TR/xml/#charsets says:
        // Char ::= #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF] /* any Unicode character,
        // excluding the surrogate blocks, FFFE, and FFFF. */
        final CharSequenceTranslator escapeXml = StringEscapeUtils.ESCAPE_XML
                .with(NumericEntityEscaper.below(9), NumericEntityEscaper.between(0xB, 0xC), NumericEntityEscaper.between(0xE, 0x19),
                        NumericEntityEscaper.between(0xD800, 0xDFFF), NumericEntityEscaper.between(0xFFFE, 0xFFFF), NumericEntityEscaper.above(0x110000));

        assertEquals("&#0;&#1;&#2;&#3;&#4;&#5;&#6;&#7;&#8;", escapeXml.translate("\u0000\u0001\u0002\u0003\u0004\u0005\u0006\u0007\u0008"));
        assertEquals("\t", escapeXml.translate("\t")); // 0x9
        assertEquals("\n", escapeXml.translate("\n")); // 0xA
        assertEquals("&#11;&#12;", escapeXml.translate("\u000B\u000C"));
        assertEquals("\r", escapeXml.translate("\r")); // 0xD
        assertEquals("Hello World! Ain&apos;t this great?", escapeXml.translate("Hello World! Ain't this great?"));
        assertEquals("&#14;&#15;&#24;&#25;", escapeXml.translate("\u000E\u000F\u0018\u0019"));
    }

    /**
     * Reverse of the above.
     *
     * @see <a href="https://issues.apache.org/jira/browse/LANG-729">LANG-729</a>
     */
    @Test
    public void testUnescapeXmlSupplementaryCharacters() {
        assertEquals("\uD84C\uDFB4", StringEscapeUtils.unescapeXml("&#144308;"),
                "Supplementary character must be represented using a single escape");

        assertEquals("a b c \uD84C\uDFB4", StringEscapeUtils.unescapeXml("a b c &#144308;"),
                "Supplementary characters mixed with basic characters should be decoded correctly");
    }

    // Tests issue LANG-150
    // https://issues.apache.org/jira/browse/LANG-150
    @Test
    public void testStandaloneAmphersand() {
        assertEquals("<P&O>", StringEscapeUtils.unescapeHtml4("&lt;P&O&gt;"));
        assertEquals("test & <", StringEscapeUtils.unescapeHtml4("test & &lt;"));
        assertEquals("<P&O>", StringEscapeUtils.unescapeXml("&lt;P&O&gt;"));
        assertEquals("test & <", StringEscapeUtils.unescapeXml("test & &lt;"));
    }

    @Test
    public void testLang313() {
        assertEquals("& &", StringEscapeUtils.unescapeHtml4("& &amp;"));
    }

    @Test
    public void testEscapeCsvString() {
        assertEquals("foo.bar",            StringEscapeUtils.escapeCsv("foo.bar"));
        assertEquals("\"foo,bar\"",        StringEscapeUtils.escapeCsv("foo,bar"));
        assertEquals("\"foo\nbar\"",       StringEscapeUtils.escapeCsv("foo\nbar"));
        assertEquals("\"foo\rbar\"",       StringEscapeUtils.escapeCsv("foo\rbar"));
        assertEquals("\"foo\"\"bar\"",     StringEscapeUtils.escapeCsv("foo\"bar"));
        assertEquals("foo\uD84C\uDFB4bar", StringEscapeUtils.escapeCsv("foo\uD84C\uDFB4bar"));
        assertEquals("",   StringEscapeUtils.escapeCsv(""));
        assertNull(StringEscapeUtils.escapeCsv(null));
    }

    @Test
    public void testEscapeCsvWriter() throws Exception {
        checkCsvEscapeWriter("foo.bar",            "foo.bar");
        checkCsvEscapeWriter("\"foo,bar\"",        "foo,bar");
        checkCsvEscapeWriter("\"foo\nbar\"",       "foo\nbar");
        checkCsvEscapeWriter("\"foo\rbar\"",       "foo\rbar");
        checkCsvEscapeWriter("\"foo\"\"bar\"",     "foo\"bar");
        checkCsvEscapeWriter("foo\uD84C\uDFB4bar", "foo\uD84C\uDFB4bar");
        checkCsvEscapeWriter("", null);
        checkCsvEscapeWriter("", "");
    }

    private void checkCsvEscapeWriter(final String expected, final String value) throws IOException {
        final StringWriter writer = new StringWriter();
        StringEscapeUtils.ESCAPE_CSV.translate(value, writer);
        assertEquals(expected, writer.toString());
    }

    @Test
    public void testEscapeCsvIllegalStateException() {
        final StringWriter writer = new StringWriter();
        assertThrows(IllegalStateException.class, () -> StringEscapeUtils.ESCAPE_CSV.translate("foo", -1, writer));
    }

    @Test
    public void testUnescapeCsvString() {
        assertEquals("foo.bar",              StringEscapeUtils.unescapeCsv("foo.bar"));
        assertEquals("foo,bar",              StringEscapeUtils.unescapeCsv("\"foo,bar\""));
        assertEquals("foo\nbar",             StringEscapeUtils.unescapeCsv("\"foo\nbar\""));
        assertEquals("foo\rbar",             StringEscapeUtils.unescapeCsv("\"foo\rbar\""));
        assertEquals("foo\"bar",             StringEscapeUtils.unescapeCsv("\"foo\"\"bar\""));
        assertEquals("foo\uD84C\uDFB4bar",   StringEscapeUtils.unescapeCsv("foo\uD84C\uDFB4bar"));
        assertEquals("",   StringEscapeUtils.unescapeCsv(""));
        assertNull(StringEscapeUtils.unescapeCsv(null));

        assertEquals("\"foo.bar\"",          StringEscapeUtils.unescapeCsv("\"foo.bar\""));
    }

    @Test
    public void testUnescapeCsvWriter() throws Exception {
        checkCsvUnescapeWriter("foo.bar",            "foo.bar");
        checkCsvUnescapeWriter("foo,bar",            "\"foo,bar\"");
        checkCsvUnescapeWriter("foo\nbar",           "\"foo\nbar\"");
        checkCsvUnescapeWriter("foo\rbar",           "\"foo\rbar\"");
        checkCsvUnescapeWriter("foo\"bar",           "\"foo\"\"bar\"");
        checkCsvUnescapeWriter("foo\uD84C\uDFB4bar", "foo\uD84C\uDFB4bar");
        checkCsvUnescapeWriter("", null);
        checkCsvUnescapeWriter("", "");

        checkCsvUnescapeWriter("\"foo.bar\"",        "\"foo.bar\"");
    }

    private void checkCsvUnescapeWriter(final String expected, final String value) throws IOException {
        final StringWriter writer = new StringWriter();
        StringEscapeUtils.UNESCAPE_CSV.translate(value, writer);
        assertEquals(expected, writer.toString());
    }

    @Test
    public void testUnescapeCsvIllegalStateException() {
        final StringWriter writer = new StringWriter();
        assertThrows(IllegalStateException.class, () -> StringEscapeUtils.UNESCAPE_CSV.translate("foo", -1, writer));
    }

    /**
     * Tests // https://issues.apache.org/jira/browse/LANG-480
     */
    @Test
    public void testEscapeHtmlHighUnicode() {
        // this is the utf8 representation of the character:
        // COUNTING ROD UNIT DIGIT THREE
        // in Unicode
        // code point: U+1D362
        final byte[] data = { (byte) 0xF0, (byte) 0x9D, (byte) 0x8D, (byte) 0xA2 };

        final String original = new String(data, StandardCharsets.UTF_8);

        final String escaped = StringEscapeUtils.escapeHtml4( original );
        assertEquals(original, escaped, "High Unicode should not have been escaped");

        final String unescaped = StringEscapeUtils.unescapeHtml4( escaped );
        assertEquals(original, unescaped, "High Unicode should have been unchanged");

// TODO: I think this should hold, needs further investigation
//        String unescapedFromEntity = StringEscapeUtils.unescapeHtml4( "&#119650;" );
//        assertEquals( "High Unicode should have been unescaped", original, unescapedFromEntity);
    }

    /**
     * Tests https://issues.apache.org/jira/browse/LANG-339
     */
    @Test
    public void testEscapeHiragana() {
        // Some random Japanese Unicode characters
        final String original = "\u304B\u304C\u3068";
        final String escaped = StringEscapeUtils.escapeHtml4(original);
        assertEquals(original, escaped,
                "Hiragana character Unicode behavior should not be being escaped by escapeHtml4");

        final String unescaped = StringEscapeUtils.unescapeHtml4( escaped );

        assertEquals(escaped, unescaped, "Hiragana character Unicode behavior has changed - expected no unescaping");
    }

    /**
     * Tests https://issues.apache.org/jira/browse/LANG-708
     *
     * @throws IOException
     *             if an I/O error occurs
     */
    @Test
    public void testLang708() throws IOException {
        final byte[] inputBytes = Files.readAllBytes(Paths.get("src/test/resources/lang-708-input.txt"));
        final String input = new String(inputBytes, StandardCharsets.UTF_8);
        final String escaped = StringEscapeUtils.escapeEcmaScript(input);
        // just the end:
        assertTrue(escaped.endsWith("}]"), escaped);
        // a little more:
        assertTrue(escaped.endsWith("\"valueCode\\\":\\\"\\\"}]"), escaped);
    }

    /**
     * Tests https://issues.apache.org/jira/browse/LANG-720
     */
    @Test
    public void testLang720() {
        final String input = "\ud842\udfb7" + "A";
        final String escaped = StringEscapeUtils.escapeXml(input);
        assertEquals(input, escaped);
    }

    /**
     * Tests https://issues.apache.org/jira/browse/LANG-911
     */
    @Test
    public void testLang911() {
        final String bellsTest = "\ud83d\udc80\ud83d\udd14";
        final String value = StringEscapeUtils.escapeJava(bellsTest);
        final String valueTest = StringEscapeUtils.unescapeJava(value);
        assertEquals(bellsTest, valueTest);
    }

    @Test
    public void testEscapeJson() {
        assertNull(StringEscapeUtils.escapeJson(null));
        assertThrows(NullPointerException.class, () -> StringEscapeUtils.ESCAPE_JSON.translate(null, null));
        assertThrows(NullPointerException.class, () -> StringEscapeUtils.ESCAPE_JSON.translate("", null));

        assertEquals("He didn't say, \\\"stop!\\\"", StringEscapeUtils.escapeJson("He didn't say, \"stop!\""));

        final String expected = "\\\"foo\\\" isn't \\\"bar\\\". specials: \\b\\r\\n\\f\\t\\\\\\/";
        final String input ="\"foo\" isn't \"bar\". specials: \b\r\n\f\t\\/";

        assertEquals(expected, StringEscapeUtils.escapeJson(input));
    }

    @Test
    public void testUnescapeJson() {
        assertNull(StringEscapeUtils.unescapeJson(null));
        assertThrows(NullPointerException.class, () -> StringEscapeUtils.UNESCAPE_JSON.translate(null, null));
        assertThrows(NullPointerException.class, () -> StringEscapeUtils.UNESCAPE_JSON.translate("", null));

        assertEquals("He didn't say, \"stop!\"", StringEscapeUtils.unescapeJson("He didn't say, \\\"stop!\\\""));

        final String expected ="\"foo\" isn't \"bar\". specials: \b\r\n\f\t\\/";
        final String input = "\\\"foo\\\" isn't \\\"bar\\\". specials: \\b\\r\\n\\f\\t\\\\\\/";

        assertEquals(expected, StringEscapeUtils.unescapeJson(input));
    }
}
