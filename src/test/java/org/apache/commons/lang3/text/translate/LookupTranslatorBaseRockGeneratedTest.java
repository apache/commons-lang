package org.apache.commons.lang3.text.translate;

import org.apache.commons.lang3.text.translate.LookupTranslator;

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

class LookupTranslatorBaseRockGeneratedTest {

    private LookupTranslator translator;

    @BeforeEach
    void setUp() {
        CharSequence[][] lookup = { { "abc", "123" }, { "def", "456" }, { "ghi", "789" } };
        translator = new LookupTranslator(lookup);
    }

    //BaseRock generated method id: ${testConstructor}, hash: 5098FF2302C74E577234A9DDFA937E68
    @Test
    void testConstructor() {
        assertNotNull(translator);
    }

    //BaseRock generated method id: ${testConstructorWithNullLookup}, hash: 979D1DE6CAE7A53A25F0A92519846B2F
    @Test
    void testConstructorWithNullLookup() {
        LookupTranslator nullTranslator = new LookupTranslator((CharSequence[][]) null);
        assertNotNull(nullTranslator);
    }

    //BaseRock generated method id: ${testTranslateValidInput}, hash: 27CE37FC5C2283F4CF17D457E313E051
    @ParameterizedTest
    @CsvSource({ "abc, 0, 123, 3", "def, 0, 456, 3", "ghi, 0, 789, 3", "abcdef, 0, 123, 3", "xabc, 1, 123, 3", "xdef, 1, 456, 3", "xghi, 1, 789, 3" })
    void testTranslateValidInput(String input, int index, String expected, int expectedReturn) throws IOException {
        Writer writer = new StringWriter();
        int result = translator.translate(input, index, writer);
        assertEquals(expected, writer.toString());
        assertEquals(expectedReturn, result);
    }

    //BaseRock generated method id: ${testTranslateInvalidInput}, hash: 9E3E4920D93D5CBA7ACE08AD5543F0A3
    @ParameterizedTest
    @CsvSource({ "xyz, 0, '', 0", "abx, 0, '', 0", "ab, 0, '', 0" })
    void testTranslateInvalidInput(String input, int index, String expected, int expectedReturn) throws IOException {
        Writer writer = new StringWriter();
        int result = translator.translate(input, index, writer);
        assertEquals(expected, writer.toString());
        assertEquals(expectedReturn, result);
    }

    //BaseRock generated method id: ${testTranslateWithLongerInput}, hash: D41E7D103B7160BCBFD8A611257EEF05
    @Test
    void testTranslateWithLongerInput() throws IOException {
        String input = "abcdefghi";
        Writer writer = new StringWriter();
        int result = translator.translate(input, 0, writer);
        assertEquals("123", writer.toString());
        assertEquals(3, result);
    }

    //BaseRock generated method id: ${testTranslateWithIndexOutOfBounds}, hash: C3618BCA65544D35232AEB8DA53C6A01
    @Test
    void testTranslateWithIndexOutOfBounds() throws IOException {
        String input = "abc";
        Writer writer = new StringWriter();
        assertThrows(IndexOutOfBoundsException.class, () -> translator.translate(input, 3, writer));
    }

    //BaseRock generated method id: ${testTranslateWithNegativeIndex}, hash: DE866A503B07BF3FF439E669AD7F36A3
    @Test
    void testTranslateWithNegativeIndex() throws IOException {
        String input = "abc";
        Writer writer = new StringWriter();
        assertThrows(IndexOutOfBoundsException.class, () -> translator.translate(input, -1, writer));
    }

    //BaseRock generated method id: ${testTranslateWithWriterIOException}, hash: E88AE1A541F257BC4A5102CAEAE44595
    @Test
    void testTranslateWithWriterIOException() throws IOException {
        String input = "abc";
        Writer writer = mock(Writer.class);
        doThrow(new IOException("Mocked IO Exception")).when(writer).write(anyString());
        assertThrows(IOException.class, () -> translator.translate(input, 0, writer));
    }
}
