package org.apache.commons.lang3.text.translate;

import org.apache.commons.lang3.text.translate.UnicodeUnescaper;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.ParameterizedTest;
import java.io.StringWriter;
import static org.junit.jupiter.api.Assertions.*;
import java.io.IOException;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

class UnicodeUnescaperBaseRockGeneratedTest {

    private UnicodeUnescaper unescaper;

    @BeforeEach
    void setUp() {
        unescaper = new UnicodeUnescaper();
    }

    //BaseRock generated method id: ${testConstructor}, hash: 2476B6631354940F17DD829239559DAA
    @Test
    void testConstructor() {
        assertNotNull(unescaper);
    }

    //BaseRock generated method id: ${testTranslateValidUnicodeSequences}, hash: B81F99A593251779F97E8576D4E0D35C
    @ParameterizedTest
    @CsvSource({ "\\u0041,A", "\\u00A9,©", "\\u00ae,®", "\\u2122,™", "\\u0394,Δ" })
    void testTranslateValidUnicodeSequences(String input, String expected) throws IOException {
        StringWriter out = new StringWriter();
        int result = unescaper.translate(input, 0, out);
        assertEquals(6, result);
        assertEquals(expected, out.toString());
    }

    //BaseRock generated method id: ${testTranslateMultipleUnicodeSequences}, hash: 1096B8EAA975DB95CB87CD5B5473F7AC
    @Test
    void testTranslateMultipleUnicodeSequences() throws IOException {
        String input = "\\u0041\\u0042\\u0043";
        StringWriter out = new StringWriter();
        int result = unescaper.translate(input, 0, out);
        assertEquals(6, result);
        assertEquals("A", out.toString());
        result = unescaper.translate(input, 6, out);
        assertEquals(6, result);
        assertEquals("AB", out.toString());
        result = unescaper.translate(input, 12, out);
        assertEquals(6, result);
        assertEquals("ABC", out.toString());
    }

    //BaseRock generated method id: ${testTranslateWithMultipleUPrefix}, hash: 00EB7F0B6292344278B9E77067D5D4AC
    @Test
    void testTranslateWithMultipleUPrefix() throws IOException {
        String input = "\\uuuu0041";
        StringWriter out = new StringWriter();
        int result = unescaper.translate(input, 0, out);
        assertEquals(8, result);
        assertEquals("A", out.toString());
    }

    //BaseRock generated method id: ${testTranslateWithPlusSign}, hash: 1922C79A882FAD71B430154663F79317
    @Test
    void testTranslateWithPlusSign() throws IOException {
        String input = "\\uu+0041";
        StringWriter out = new StringWriter();
        int result = unescaper.translate(input, 0, out);
        assertEquals(8, result);
        assertEquals("A", out.toString());
    }

    //BaseRock generated method id: ${testTranslateInvalidUnicodeSequence}, hash: C16C4C0F4D25B963AA957DFB54CC61BE
    @Test
    void testTranslateInvalidUnicodeSequence() {
        String input = "\\u004G";
        StringWriter out = new StringWriter();
        assertThrows(IllegalArgumentException.class, () -> unescaper.translate(input, 0, out));
    }

    //BaseRock generated method id: ${testTranslateIncompleteUnicodeSequence}, hash: C508D01F7B1F63862CB009A92655A675
    @Test
    void testTranslateIncompleteUnicodeSequence() {
        String input = "\\u004";
        StringWriter out = new StringWriter();
        assertThrows(IllegalArgumentException.class, () -> unescaper.translate(input, 0, out));
    }

    //BaseRock generated method id: ${testTranslateNonUnicodeSequence}, hash: F1C1CBC7607C77E76A54E4088641ED1D
    @Test
    void testTranslateNonUnicodeSequence() throws IOException {
        String input = "ABC";
        StringWriter out = new StringWriter();
        int result = unescaper.translate(input, 0, out);
        assertEquals(0, result);
        assertEquals("", out.toString());
    }

    //BaseRock generated method id: ${testTranslateEmptyInput}, hash: 0D9489368A36683A6AC0F6AC679A813F
    @Test
    void testTranslateEmptyInput() throws IOException {
        String input = "";
        StringWriter out = new StringWriter();
        int result = unescaper.translate(input, 0, out);
        assertEquals(0, result);
        assertEquals("", out.toString());
    }

    //BaseRock generated method id: ${testTranslateNullInput}, hash: 66540D4F4A57D572D18690E98EB63ACD
    @Test
    void testTranslateNullInput() {
        StringWriter out = new StringWriter();
        assertThrows(NullPointerException.class, () -> unescaper.translate(null, 0, out));
    }

    //BaseRock generated method id: ${testTranslateWithInvalidIndex}, hash: 8034435D74B367E410F1BC630D63FCF1
    @Test
    void testTranslateWithInvalidIndex() {
        String input = "\\u0041";
        StringWriter out = new StringWriter();
        assertThrows(StringIndexOutOfBoundsException.class, () -> unescaper.translate(input, 10, out));
    }
}
