package org.apache.commons.lang3.csv;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Simple test to verify compilation works.
 */
class SimpleCompileTest {

    @Test
    void testBasicCompilation() throws Exception {
        String csv = "name,age\nJohn,25\nJane,30";
        String json = CsvToJson.fromString(csv).toJson();
        assertTrue(json.contains("John"));
        assertTrue(json.contains("25"));
    }

    @Test
    void testBuilderCompilation() throws Exception {
        String csv = "name;age;city\nJohn;25;New York";
        String json = CsvToJson.builder()
                .withDelimiter(';')
                .fromString(csv)
                .toJson();
        assertTrue(json.contains("John"));
    }
}