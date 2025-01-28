package org.apache.commons.lang3.text.translate;

import org.apache.commons.lang3.text.translate.AggregateTranslator;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.provider.CsvSource;
import static org.junit.jupiter.api.Assertions.*;
import java.io.StringWriter;
import org.junit.jupiter.params.ParameterizedTest;
import static org.mockito.Mockito.*;
import java.io.IOException;
import java.io.Writer;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

class AggregateTranslatorBaseRockGeneratedTest {

    private AggregateTranslator aggregateTranslator;

    private CharSequenceTranslator mockTranslator1;

    private CharSequenceTranslator mockTranslator2;

    @BeforeEach
    void setUp() {
        mockTranslator1 = mock(CharSequenceTranslator.class);
        mockTranslator2 = mock(CharSequenceTranslator.class);
        aggregateTranslator = new AggregateTranslator(mockTranslator1, mockTranslator2);
    }

    //BaseRock generated method id: ${testConstructor}, hash: E309FA3A6B235BCBD5508486F3AB964E
    @Test
    void testConstructor() {
        assertNotNull(aggregateTranslator);
    }

    //BaseRock generated method id: ${testTranslateWithNoConsumption}, hash: 3933044A7E8C12E151EA65B9564DEB6E
    @Test
    void testTranslateWithNoConsumption() throws IOException {
        CharSequence input = "test";
        Writer out = new StringWriter();
        when(mockTranslator1.translate(input, 0, out)).thenReturn(0);
        when(mockTranslator2.translate(input, 0, out)).thenReturn(0);
        int result = aggregateTranslator.translate(input, 0, out);
        assertEquals(0, result);
        verify(mockTranslator1).translate(input, 0, out);
        verify(mockTranslator2).translate(input, 0, out);
    }

    //BaseRock generated method id: ${testTranslateWithFirstTranslatorConsumption}, hash: 957C562D7376CA9DF3A3ECAF390338A4
    @Test
    void testTranslateWithFirstTranslatorConsumption() throws IOException {
        CharSequence input = "test";
        Writer out = new StringWriter();
        when(mockTranslator1.translate(input, 0, out)).thenReturn(2);
        int result = aggregateTranslator.translate(input, 0, out);
        assertEquals(2, result);
        verify(mockTranslator1).translate(input, 0, out);
        verify(mockTranslator2, never()).translate(any(), anyInt(), any());
    }

    //BaseRock generated method id: ${testTranslateWithSecondTranslatorConsumption}, hash: 2AE7CA772466BE977A1A9822F9866F92
    @Test
    void testTranslateWithSecondTranslatorConsumption() throws IOException {
        CharSequence input = "test";
        Writer out = new StringWriter();
        when(mockTranslator1.translate(input, 0, out)).thenReturn(0);
        when(mockTranslator2.translate(input, 0, out)).thenReturn(3);
        int result = aggregateTranslator.translate(input, 0, out);
        assertEquals(3, result);
        verify(mockTranslator1).translate(input, 0, out);
        verify(mockTranslator2).translate(input, 0, out);
    }

    //BaseRock generated method id: ${testTranslateWithDifferentInputsAndIndices}, hash: 2D79795F1A6DC290088EE73E00841618
    @ParameterizedTest
    @CsvSource({ "test, 0", "another, 2", "thirdtest, 5" })
    void testTranslateWithDifferentInputsAndIndices(String input, int index) throws IOException {
        Writer out = new StringWriter();
        when(mockTranslator1.translate(eq(input), eq(index), any())).thenReturn(0);
        when(mockTranslator2.translate(eq(input), eq(index), any())).thenReturn(0);
        int result = aggregateTranslator.translate(input, index, out);
        assertEquals(0, result);
        verify(mockTranslator1).translate(eq(input), eq(index), any());
        verify(mockTranslator2).translate(eq(input), eq(index), any());
    }

    //BaseRock generated method id: ${testTranslateWithIOException}, hash: 28604794E975504A16E94698CD005086
    @Test
    void testTranslateWithIOException() throws IOException {
        CharSequence input = "test";
        Writer out = new StringWriter();
        when(mockTranslator1.translate(input, 0, out)).thenThrow(new IOException("Test exception"));
        assertThrows(IOException.class, () -> aggregateTranslator.translate(input, 0, out));
    }

    //BaseRock generated method id: ${testTranslateWithEmptyTranslatorArray}, hash: C0CDF8B1DEE82EB36836874AB188EEAC
    @Test
    void testTranslateWithEmptyTranslatorArray() throws IOException {
        AggregateTranslator emptyTranslator = new AggregateTranslator();
        CharSequence input = "test";
        Writer out = new StringWriter();
        int result = emptyTranslator.translate(input, 0, out);
        assertEquals(0, result);
    }
}
