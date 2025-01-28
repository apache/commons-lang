package org.apache.commons.lang3.text;

import org.apache.commons.lang3.text.ExtendedMessageFormat;

import java.text.MessageFormat;
import org.apache.commons.lang3.LocaleUtils;
import static org.hamcrest.MatcherAssert.assertThat;
import org.junit.jupiter.api.BeforeEach;
import static org.hamcrest.Matchers.*;
import java.text.Format;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.provider.CsvSource;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.params.ParameterizedTest;
import static org.mockito.Mockito.*;
import java.util.*;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

class ExtendedMessageFormatBaseRockGeneratedTest {

    private ExtendedMessageFormat extendedMessageFormat;

    private Map<String, FormatFactory> registry;

    @BeforeEach
    void setUp() {
        registry = new HashMap<>();
        extendedMessageFormat = new ExtendedMessageFormat("", Locale.getDefault(), registry);
    }

    //BaseRock generated method id: ${testConstructorWithPattern}, hash: 313CBF503D95154C92A3678F363C47D8
    @Test
    void testConstructorWithPattern() {
        extendedMessageFormat = new ExtendedMessageFormat("Test {0}");
        assertEquals("Test {0}", extendedMessageFormat.toPattern());
    }

    //BaseRock generated method id: ${testConstructorWithPatternAndLocale}, hash: 8703463E00317E79B85FCBEA69F5B2A7
    @Test
    void testConstructorWithPatternAndLocale() {
        extendedMessageFormat = new ExtendedMessageFormat("Test {0}", Locale.US);
        assertEquals("Test {0}", extendedMessageFormat.toPattern());
        assertEquals(Locale.US, extendedMessageFormat.getLocale());
    }

    //BaseRock generated method id: ${testConstructorWithPatternAndRegistry}, hash: 668EE30A6AB187FB343041C28F3A9F00
    @Test
    void testConstructorWithPatternAndRegistry() {
        extendedMessageFormat = new ExtendedMessageFormat("Test {0}", registry);
        assertEquals("Test {0}", extendedMessageFormat.toPattern());
    }

    //BaseRock generated method id: ${testApplyPatternWithoutRegistry}, hash: A7127053E26D63956D9EBDB201C48FA9
    @Test
    void testApplyPatternWithoutRegistry() {
        extendedMessageFormat = new ExtendedMessageFormat("");
        extendedMessageFormat.applyPattern("Test {0,number} {1,date}");
        assertEquals("Test {0,number} {1,date}", extendedMessageFormat.toPattern());
    }

    //BaseRock generated method id: ${testApplyPatternWithRegistry}, hash: 88441299DEFD38A081755B8E2CA4FC21
    @Test
    void testApplyPatternWithRegistry() throws Exception {
        FormatFactory factory = mock(FormatFactory.class);
        Format format = mock(Format.class);
        when(factory.getFormat(anyString(), anyString(), any())).thenReturn(format);
        registry.put("test", factory);
        extendedMessageFormat.applyPattern("Test {0,test}");
        assertEquals("Test {0,test}", extendedMessageFormat.toPattern());
        verify(factory).getFormat("test", "", Locale.getDefault());
    }

    //BaseRock generated method id: ${testApplyPatternWithQuotedString}, hash: 2B876F763855797FBCACCA63A626EC4A
    @Test
    void testApplyPatternWithQuotedString() {
        extendedMessageFormat.applyPattern("Test '{0}' {1}");
        assertEquals("Test '{0}' {1}", extendedMessageFormat.toPattern());
    }

    //BaseRock generated method id: ${testApplyPatternWithInvalidFormat}, hash: 21C46B4A3069F9C5D4AC1C6DC6B857C6
    @Test
    void testApplyPatternWithInvalidFormat() {
        assertThrows(IllegalArgumentException.class, () -> extendedMessageFormat.applyPattern("Test {0,invalid}"));
    }

    //BaseRock generated method id: ${testEquals}, hash: F5951257FC6EF8F03F537315D0C4AACF
    @Test
    void testEquals() {
        ExtendedMessageFormat format1 = new ExtendedMessageFormat("Test {0}", registry);
        ExtendedMessageFormat format2 = new ExtendedMessageFormat("Test {0}", registry);
        ExtendedMessageFormat format3 = new ExtendedMessageFormat("Test {1}", registry);
        assertEquals(format1, format2);
        assertNotEquals(format1, format3);
        assertNotEquals(format1, null);
        assertNotEquals(format1, new MessageFormat("Test {0}"));
    }

    //BaseRock generated method id: ${testHashCode}, hash: 2EBE74FC96E0557FEB278C017A12D2E6
    @Test
    void testHashCode() {
        ExtendedMessageFormat format1 = new ExtendedMessageFormat("Test {0}", registry);
        ExtendedMessageFormat format2 = new ExtendedMessageFormat("Test {0}", registry);
        ExtendedMessageFormat format3 = new ExtendedMessageFormat("Test {1}", registry);
        assertEquals(format1.hashCode(), format2.hashCode());
        assertNotEquals(format1.hashCode(), format3.hashCode());
    }

    //BaseRock generated method id: ${testToPattern}, hash: C5177F335F2E19EAB66E676E13528BEA
    @Test
    void testToPattern() {
        extendedMessageFormat.applyPattern("Test {0} {1,number}");
        assertEquals("Test {0} {1,number}", extendedMessageFormat.toPattern());
    }

    //BaseRock generated method id: ${testUnsupportedOperations}, hash: A49C53FF407F6ED0A86F649A31E3D5BA
    @ParameterizedTest
    @CsvSource({ "setFormat,         'Unsupported operation'", "setFormatByArgumentIndex, 'Unsupported operation'", "setFormats,        'Unsupported operation'", "setFormatsByArgumentIndex, 'Unsupported operation'" })
    void testUnsupportedOperations(String methodName, String expectedMessage) {
        assertThrows(UnsupportedOperationException.class, () -> {
            switch(methodName) {
                case "setFormat":
                    extendedMessageFormat.setFormat(0, null);
                    break;
                case "setFormatByArgumentIndex":
                    extendedMessageFormat.setFormatByArgumentIndex(0, null);
                    break;
                case "setFormats":
                    extendedMessageFormat.setFormats(new Format[0]);
                    break;
                case "setFormatsByArgumentIndex":
                    extendedMessageFormat.setFormatsByArgumentIndex(new Format[0]);
                    break;
            }
        }, expectedMessage);
    }

    //BaseRock generated method id: ${testComplexPattern}, hash: F4A04DF16F1560AFB643E3398789A226
    @Test
    void testComplexPattern() throws Exception {
        FormatFactory factory = mock(FormatFactory.class);
        Format format = mock(Format.class);
        when(factory.getFormat(anyString(), anyString(), any())).thenReturn(format);
        registry.put("custom", factory);
        String pattern = "This is a {0} test with a {1,number,#.##} and {2,date,yyyy-MM-dd} and {3,custom,argument}";
        extendedMessageFormat.applyPattern(pattern);
        assertEquals(pattern, extendedMessageFormat.toPattern());
        verify(factory).getFormat("custom", "argument", Locale.getDefault());
    }

    //BaseRock generated method id: ${testPatternWithNestedBraces}, hash: 9DCD79A6B6EF67BC6CF11E44E28124AF
    @Test
    void testPatternWithNestedBraces() {
        String pattern = "Nested {0} and {1,choice,0#{2}|1#{3}}";
        extendedMessageFormat.applyPattern(pattern);
        assertEquals(pattern, extendedMessageFormat.toPattern());
    }

    //BaseRock generated method id: ${testFormatWithCustomFormat}, hash: 73F285693C831C2C387F56E9EE83687D
    @Test
    void testFormatWithCustomFormat() throws Exception {
        FormatFactory factory = mock(FormatFactory.class);
        Format format = mock(Format.class);
        when(factory.getFormat(anyString(), anyString(), any())).thenReturn(format);
        when(format.format(any())).thenReturn("CUSTOM");
        registry.put("custom", factory);
        extendedMessageFormat.applyPattern("Test {0,custom}");
        String result = extendedMessageFormat.format(new Object[] { "input" });
        assertEquals("Test CUSTOM", result);
        verify(format).format("input");
    }
}
