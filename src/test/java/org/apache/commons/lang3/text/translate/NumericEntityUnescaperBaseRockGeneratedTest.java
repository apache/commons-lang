package org.apache.commons.lang3.text.translate;

import org.apache.commons.lang3.text.translate.NumericEntityUnescaper;

import java.util.Arrays;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import java.util.Collections;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import org.junit.jupiter.params.provider.CsvSource;
import static org.junit.jupiter.api.Assertions.*;
import java.io.StringWriter;
import java.util.EnumSet;
import java.io.IOException;
import java.io.Writer;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

class NumericEntityUnescaperBaseRockGeneratedTest {

    private NumericEntityUnescaper unescaper;

    @BeforeEach
    void setUp() {
        unescaper = new NumericEntityUnescaper();
    }

    //BaseRock generated method id: ${testConstructorWithNoOptions}, hash: BA3FA738B542B3A0D80CB796DB37F59F
    @Test
    void testConstructorWithNoOptions() {
        assertTrue(unescaper.isSet(NumericEntityUnescaper.OPTION.semiColonRequired));
    }

    //BaseRock generated method id: ${testConstructorWithOptions}, hash: 4993D7B7BD71CC2BC505D0BCEC4E46D6
    @Test
    void testConstructorWithOptions() {
        NumericEntityUnescaper customUnescaper = new NumericEntityUnescaper(NumericEntityUnescaper.OPTION.semiColonOptional, NumericEntityUnescaper.OPTION.errorIfNoSemiColon);
        assertTrue(customUnescaper.isSet(NumericEntityUnescaper.OPTION.semiColonOptional));
        assertTrue(customUnescaper.isSet(NumericEntityUnescaper.OPTION.errorIfNoSemiColon));
        assertFalse(customUnescaper.isSet(NumericEntityUnescaper.OPTION.semiColonRequired));
    }

    //BaseRock generated method id: ${testTranslateValidEntities}, hash: BAF7400EC1A7C7D22933D376101A2CBE
    @ParameterizedTest
    @CsvSource({ "&#65;, A", "&#x41;, A", "&#X41;, A", "&#65;&#66;, AB", "&#x41;&#x42;, AB", "Text &#65; more text, Text A more text", "&#65, &#65", "&#x41, &#x41", "&#65ABC;, AABC", "&#x41ABC;, AABC", "&#x10FFFF;, \uDBFF\uDFFF", "&#1114111;, \uDBFF\uDFFF" })
    void testTranslateValidEntities(String input, String expected) throws IOException {
        StringWriter writer = new StringWriter();
        unescaper.translate(input, 0, writer);
        assertEquals(expected, writer.toString());
    }

    //BaseRock generated method id: ${testTranslateInvalidEntity}, hash: CF3F4CFFB0B9D7CCF0A515F90663C910
    @Test
    void testTranslateInvalidEntity() throws IOException {
        StringWriter writer = new StringWriter();
        assertEquals(0, unescaper.translate("&", 0, writer));
        assertEquals(0, unescaper.translate("&#", 0, writer));
        assertEquals(0, unescaper.translate("&#x", 0, writer));
        assertEquals(0, unescaper.translate("&#xZ", 0, writer));
        assertEquals(0, unescaper.translate("&#65Z", 0, writer));
    }

    //BaseRock generated method id: ${testTranslateWithSemiColonRequired}, hash: 642A4D236E1A1A7FFD1945544464A968
    @Test
    void testTranslateWithSemiColonRequired() throws IOException {
        StringWriter writer = new StringWriter();
        assertEquals(0, unescaper.translate("&#65", 0, writer));
        assertEquals("", writer.toString());
    }

    //BaseRock generated method id: ${testTranslateWithSemiColonOptional}, hash: 4C7CEFF1B95C65D0801D0E08E59A7889
    @Test
    void testTranslateWithSemiColonOptional() throws IOException {
        NumericEntityUnescaper optionalSemicolonUnescaper = new NumericEntityUnescaper(NumericEntityUnescaper.OPTION.semiColonOptional);
        StringWriter writer = new StringWriter();
        assertEquals(4, optionalSemicolonUnescaper.translate("&#65", 0, writer));
        assertEquals("A", writer.toString());
    }

    //BaseRock generated method id: ${testTranslateWithErrorIfNoSemiColon}, hash: A98301C1A88B94E4A75BA4333A0A1328
    @Test
    void testTranslateWithErrorIfNoSemiColon() {
        NumericEntityUnescaper errorUnescaper = new NumericEntityUnescaper(NumericEntityUnescaper.OPTION.errorIfNoSemiColon);
        StringWriter writer = new StringWriter();
        assertThrows(IllegalArgumentException.class, () -> errorUnescaper.translate("&#65", 0, writer));
    }

    //BaseRock generated method id: ${testTranslateWithLargeCodePoint}, hash: 90782BC737367DBD7523DC78C59A219B
    @Test
    void testTranslateWithLargeCodePoint() throws IOException {
        StringWriter writer = new StringWriter();
        assertEquals(8, unescaper.translate("&#131072;", 0, writer));
        assertEquals(2, writer.toString().length());
        assertEquals(0x20000, Character.codePointAt(writer.toString(), 0));
    }

    //BaseRock generated method id: ${testTranslateWithInvalidCodePoint}, hash: A797FDA2F18E4AB10AF4BCC6628C3504
    @Test
    void testTranslateWithInvalidCodePoint() throws IOException {
        StringWriter writer = new StringWriter();
        assertEquals(0, unescaper.translate("&#x110000;", 0, writer));
        assertEquals("", writer.toString());
    }

    //BaseRock generated method id: ${testTranslateWithNonEntity}, hash: E0D5E8855CD836675F07388559DE3AFE
    @Test
    void testTranslateWithNonEntity() throws IOException {
        StringWriter writer = new StringWriter();
        assertEquals(0, unescaper.translate("Text", 0, writer));
        assertEquals("", writer.toString());
    }
}
