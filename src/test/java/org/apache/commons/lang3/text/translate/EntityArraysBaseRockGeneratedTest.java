package org.apache.commons.lang3.text.translate;

import org.apache.commons.lang3.text.translate.EntityArrays;

import java.util.stream.Stream;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.params.provider.Arguments;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

class EntityArraysBaseRockGeneratedTest {

    //BaseRock generated method id: ${testConstructor}, hash: E82878644A320B59C8A01E8F9B576C0E
    @Test
    void testConstructor() {
        assertDoesNotThrow(EntityArrays::new);
    }

    //BaseRock generated method id: ${testAPOS_ESCAPE}, hash: 2A8A349B4F57046BC6A8F3AA0A61426D
    @Test
    void testAPOS_ESCAPE() {
        String[][] result = EntityArrays.APOS_ESCAPE();
        assertNotNull(result);
        assertEquals(1, result.length);
        assertArrayEquals(new String[] { "'", "&apos;" }, result[0]);
    }

    //BaseRock generated method id: ${testAPOS_UNESCAPE}, hash: B573A6D02AB1EF795A8022E6EF8A3C49
    @Test
    void testAPOS_UNESCAPE() {
        String[][] result = EntityArrays.APOS_UNESCAPE();
        assertNotNull(result);
        assertEquals(1, result.length);
        assertArrayEquals(new String[] { "&apos;", "'" }, result[0]);
    }

    //BaseRock generated method id: ${testBASIC_ESCAPE}, hash: 236BAC7DE6513AE05D02649E9CF8F671
    @Test
    void testBASIC_ESCAPE() {
        String[][] result = EntityArrays.BASIC_ESCAPE();
        assertNotNull(result);
        assertEquals(4, result.length);
        assertArrayEquals(new String[] { "\"", "&quot;" }, result[0]);
        assertArrayEquals(new String[] { "&", "&amp;" }, result[1]);
        assertArrayEquals(new String[] { "<", "&lt;" }, result[2]);
        assertArrayEquals(new String[] { ">", "&gt;" }, result[3]);
    }

    //BaseRock generated method id: ${testBASIC_UNESCAPE}, hash: CD0F1038C5A6DD752CFAAC61FDC86EF1
    @Test
    void testBASIC_UNESCAPE() {
        String[][] result = EntityArrays.BASIC_UNESCAPE();
        assertNotNull(result);
        assertEquals(4, result.length);
        assertArrayEquals(new String[] { "&quot;", "\"" }, result[0]);
        assertArrayEquals(new String[] { "&amp;", "&" }, result[1]);
        assertArrayEquals(new String[] { "&lt;", "<" }, result[2]);
        assertArrayEquals(new String[] { "&gt;", ">" }, result[3]);
    }

    //BaseRock generated method id: ${testHTML40_EXTENDED_ESCAPE}, hash: 075C2A570878D8A2AF155B037503BCA2
    @Test
    void testHTML40_EXTENDED_ESCAPE() {
        String[][] result = EntityArrays.HTML40_EXTENDED_ESCAPE();
        assertNotNull(result);
        assertTrue(result.length > 0);
        assertArrayEquals(new String[] { "\\u0192", "&fnof;" }, result[0]);
    }

    //BaseRock generated method id: ${testHTML40_EXTENDED_UNESCAPE}, hash: 996E9B6D7D22237FE17787E9DB10300F
    @Test
    void testHTML40_EXTENDED_UNESCAPE() {
        String[][] result = EntityArrays.HTML40_EXTENDED_UNESCAPE();
        assertNotNull(result);
        assertTrue(result.length > 0);
        assertArrayEquals(new String[] { "&fnof;", "\\u0192" }, result[0]);
    }

    //BaseRock generated method id: ${testISO8859_1_ESCAPE}, hash: C10B3802C38BAFC964E98D660F445FF1
    @Test
    void testISO8859_1_ESCAPE() {
        String[][] result = EntityArrays.ISO8859_1_ESCAPE();
        assertNotNull(result);
        assertTrue(result.length > 0);
        assertArrayEquals(new String[] { "\\u00A0", "&nbsp;" }, result[0]);
    }

    //BaseRock generated method id: ${testISO8859_1_UNESCAPE}, hash: 601A71FBF6DC15289F9F3CF58C022F29
    @Test
    void testISO8859_1_UNESCAPE() {
        String[][] result = EntityArrays.ISO8859_1_UNESCAPE();
        assertNotNull(result);
        assertTrue(result.length > 0);
        assertArrayEquals(new String[] { "&nbsp;", "\\u00A0" }, result[0]);
    }

    //BaseRock generated method id: ${testJAVA_CTRL_CHARS_ESCAPE}, hash: 6BB21B28EA2FEEFA2C4223B62DDF515F
    @Test
    void testJAVA_CTRL_CHARS_ESCAPE() {
        String[][] result = EntityArrays.JAVA_CTRL_CHARS_ESCAPE();
        assertNotNull(result);
        assertEquals(5, result.length);
        assertArrayEquals(new String[] { "\\b", "\\\\b" }, result[0]);
        assertArrayEquals(new String[] { "\\n", "\\\\n" }, result[1]);
        assertArrayEquals(new String[] { "\\t", "\\\\t" }, result[2]);
        assertArrayEquals(new String[] { "\\f", "\\\\f" }, result[3]);
        assertArrayEquals(new String[] { "\\r", "\\\\r" }, result[4]);
    }

    //BaseRock generated method id: ${testJAVA_CTRL_CHARS_UNESCAPE}, hash: F931A30217183593D358A5BAA0DBCB53
    @Test
    void testJAVA_CTRL_CHARS_UNESCAPE() {
        String[][] result = EntityArrays.JAVA_CTRL_CHARS_UNESCAPE();
        assertNotNull(result);
        assertEquals(5, result.length);
        assertArrayEquals(new String[] { "\\\\b", "\\b" }, result[0]);
        assertArrayEquals(new String[] { "\\\\n", "\\n" }, result[1]);
        assertArrayEquals(new String[] { "\\\\t", "\\t" }, result[2]);
        assertArrayEquals(new String[] { "\\\\f", "\\f" }, result[3]);
        assertArrayEquals(new String[] { "\\\\r", "\\r" }, result[4]);
    }

    //BaseRock generated method id: ${testInvert}, hash: BDCA9B2273F2777DA8F39271B4A76823
    @ParameterizedTest
    @MethodSource("provideArraysForInvert")
    void testInvert(String[][] input, String[][] expected) {
        assertArrayEquals(expected, EntityArrays.invert(input));
    }

    private static Stream<Arguments> provideArraysForInvert() {
        return Stream.of(Arguments.of(new String[][] { { "a", "1" }, { "b", "2" } }, new String[][] { { "1", "a" }, { "2", "b" } }), Arguments.of(new String[][] { { "x", "y" }, { "z", "w" } }, new String[][] { { "y", "x" }, { "w", "z" } }));
    }
}
