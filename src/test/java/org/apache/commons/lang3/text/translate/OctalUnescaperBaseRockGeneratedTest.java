package org.apache.commons.lang3.text.translate;

import org.apache.commons.lang3.text.translate.OctalUnescaper;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.provider.CsvSource;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.params.ParameterizedTest;
import java.io.StringWriter;
import static org.mockito.Mockito.*;
import java.io.IOException;
import java.io.Writer;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

class OctalUnescaperBaseRockGeneratedTest {

    private OctalUnescaper octalUnescaper;

    @BeforeEach
    void setUp() {
        octalUnescaper = new OctalUnescaper();
    }

    //BaseRock generated method id: ${testTranslateWithNoOctalSequence}, hash: E60FF3AC87AD7A2BDE6BD067795364FA
    @Test
    void testTranslateWithNoOctalSequence() throws IOException {
        Writer writer = new StringWriter();
        int result = octalUnescaper.translate("abc", 0, writer);
        assertEquals(0, result);
        assertEquals("", writer.toString());
    }

    //BaseRock generated method id: ${testTranslateWithValidOctalSequence}, hash: 34487ACF414FCA55F5063C741194537B
    @ParameterizedTest
    @CsvSource({ "\\1,1,1", "\\12,10,2", "\\123,83,3", "\\777,511,3" })
    void testTranslateWithValidOctalSequence(String input, int expected, int expectedLength) throws IOException {
        Writer writer = new StringWriter();
        int result = octalUnescaper.translate(input, 0, writer);
        assertEquals(expectedLength, result);
        assertEquals((char) expected, writer.toString().charAt(0));
    }

    //BaseRock generated method id: ${testTranslateWithInvalidOctalSequence}, hash: C0993EF6B7A9AFA5506ED65F45155643
    @Test
    void testTranslateWithInvalidOctalSequence() throws IOException {
        Writer writer = new StringWriter();
        int result = octalUnescaper.translate("\\8", 0, writer);
        assertEquals(0, result);
        assertEquals("", writer.toString());
    }

    //BaseRock generated method id: ${testTranslateWithIncompleteOctalSequence}, hash: 4FA0D2A884902492268D9F72B0DABA3D
    @Test
    void testTranslateWithIncompleteOctalSequence() throws IOException {
        Writer writer = new StringWriter();
        int result = octalUnescaper.translate("\\1", 0, writer);
        assertEquals(2, result);
        assertEquals("\1", writer.toString());
    }

    //BaseRock generated method id: ${testTranslateWithMultipleOctalSequences}, hash: D58838B84205EC230DDF2579BE48EB27
    @Test
    void testTranslateWithMultipleOctalSequences() throws IOException {
        Writer writer = new StringWriter();
        String input = "\\1\\12\\123";
        int result = octalUnescaper.translate(input, 0, writer);
        assertEquals(2, result);
        assertEquals("\1", writer.toString());
        writer = new StringWriter();
        result = octalUnescaper.translate(input, 2, writer);
        assertEquals(3, result);
        assertEquals("\12", writer.toString());
        writer = new StringWriter();
        result = octalUnescaper.translate(input, 5, writer);
        assertEquals(4, result);
        assertEquals("S", writer.toString());
    }

    //BaseRock generated method id: ${testTranslateWithMixedContent}, hash: 52BED230D8B2B4344E014457F34423A1
    @Test
    void testTranslateWithMixedContent() throws IOException {
        Writer writer = new StringWriter();
        String input = "abc\\123def";
        int result = octalUnescaper.translate(input, 0, writer);
        assertEquals(0, result);
        assertEquals("", writer.toString());
        writer = new StringWriter();
        result = octalUnescaper.translate(input, 3, writer);
        assertEquals(4, result);
        assertEquals("S", writer.toString());
    }

    //BaseRock generated method id: ${testTranslateWithEdgeCases}, hash: 9601580A91A4B1D9FEA043A641E31CA7
    @Test
    void testTranslateWithEdgeCases() throws IOException {
        Writer writer = new StringWriter();
        int result = octalUnescaper.translate("\\", 0, writer);
        assertEquals(0, result);
        assertEquals("", writer.toString());
        writer = new StringWriter();
        result = octalUnescaper.translate("\\7", 0, writer);
        assertEquals(2, result);
        assertEquals("\7", writer.toString());
        writer = new StringWriter();
        result = octalUnescaper.translate("\\400", 0, writer);
        assertEquals(3, result);
        assertEquals(" ", writer.toString());
    }

    //BaseRock generated method id: ${testTranslateWithIOException}, hash: 32237495D8D98BC29D7B098BE4775A3C
    @Test
    void testTranslateWithIOException() throws IOException {
        Writer mockWriter = mock(Writer.class);
        doThrow(new IOException("Test IOException")).when(mockWriter).write(anyInt());
        assertThrows(IOException.class, () -> octalUnescaper.translate("\\123", 0, mockWriter));
    }
}
