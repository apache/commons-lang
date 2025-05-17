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

import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import javax.annotation.RegEx;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests {@link RegExUtils}.
 */
public class RegExUtilsTest extends AbstractLangTest {

    @Test
    public void testDotAll() {
        assertTrue(RegExUtils.dotAll("<A>.*</A>").matcher("<A>\nxy\n</A>").matches());
    }

    @Test
    public void testDotAllMatcher() {
        assertTrue(RegExUtils.dotAllMatcher("<A>.*</A>", (CharSequence) "<A>\nxy\n</A>").matches());
    }

    @SuppressWarnings("deprecation")
    @Test
    public void testDotAllMatcherDeprecated() {
        assertTrue(RegExUtils.dotAllMatcher("<A>.*</A>", "<A>\nxy\n</A>").matches());
    }

    @Test
    public void testRemoveAll() {
        assertNull(RegExUtils.removeAll((CharSequence) null, Pattern.compile("")));
        assertEquals("any", RegExUtils.removeAll((CharSequence) "any", (Pattern) null));

        assertEquals("any", RegExUtils.removeAll((CharSequence) "any", Pattern.compile("")));
        assertEquals("", RegExUtils.removeAll((CharSequence) "any", Pattern.compile(".*")));
        assertEquals("", RegExUtils.removeAll((CharSequence) "any", Pattern.compile(".+")));
        assertEquals("", RegExUtils.removeAll((CharSequence) "any", Pattern.compile(".?")));

        assertEquals("A\nB", RegExUtils.removeAll((CharSequence) "A<__>\n<__>B", Pattern.compile("<.*>")));
        assertEquals("AB", RegExUtils.removeAll((CharSequence) "A<__>\n<__>B", Pattern.compile("(?s)<.*>")));
        assertEquals("ABC123", RegExUtils.removeAll((CharSequence) "ABCabc123abc", Pattern.compile("[a-z]")));

        assertEquals("AB", RegExUtils.removeAll((CharSequence) "A<__>\n<__>B", Pattern.compile("<.*>", Pattern.DOTALL)));
        assertEquals("AB", RegExUtils.removeAll((CharSequence) "A<__>\\n<__>B", Pattern.compile("<.*>")));
        assertEquals("", RegExUtils.removeAll((CharSequence) "<A>x\\ny</A>", Pattern.compile("<A>.*</A>")));
        assertEquals("", RegExUtils.removeAll((CharSequence) "<A>\nxy\n</A>", Pattern.compile("<A>.*</A>", Pattern.DOTALL)));
    }

    @Test
    public void testRemoveAllDeprecated() {
        assertNull(RegExUtils.removeAll(null, Pattern.compile("")));
        assertEquals("any", RegExUtils.removeAll("any", (Pattern) null));

        assertEquals("any", RegExUtils.removeAll("any", Pattern.compile("")));
        assertEquals("", RegExUtils.removeAll("any", Pattern.compile(".*")));
        assertEquals("", RegExUtils.removeAll("any", Pattern.compile(".+")));
        assertEquals("", RegExUtils.removeAll("any", Pattern.compile(".?")));

        assertEquals("A\nB", RegExUtils.removeAll("A<__>\n<__>B", Pattern.compile("<.*>")));
        assertEquals("AB", RegExUtils.removeAll("A<__>\n<__>B", Pattern.compile("(?s)<.*>")));
        assertEquals("ABC123", RegExUtils.removeAll("ABCabc123abc", Pattern.compile("[a-z]")));

        assertEquals("AB", RegExUtils.removeAll("A<__>\n<__>B", Pattern.compile("<.*>", Pattern.DOTALL)));
        assertEquals("AB", RegExUtils.removeAll("A<__>\\n<__>B", Pattern.compile("<.*>")));
        assertEquals("", RegExUtils.removeAll("<A>x\\ny</A>", Pattern.compile("<A>.*</A>")));
        assertEquals("", RegExUtils.removeAll("<A>\nxy\n</A>", Pattern.compile("<A>.*</A>", Pattern.DOTALL)));
    }

    @Test
    public void testRemoveAll_StringString() {
        assertNull(RegExUtils.removeAll(null, ""));
        assertEquals("any", RegExUtils.removeAll("any", (String) null));

        assertEquals("any", RegExUtils.removeAll("any", ""));
        assertEquals("", RegExUtils.removeAll("any", ".*"));
        assertEquals("", RegExUtils.removeAll("any", ".+"));
        assertEquals("", RegExUtils.removeAll("any", ".?"));

        assertEquals("A\nB", RegExUtils.removeAll("A<__>\n<__>B", "<.*>"));
        assertEquals("AB", RegExUtils.removeAll("A<__>\n<__>B", "(?s)<.*>"));
        assertEquals("ABC123", RegExUtils.removeAll("ABCabc123abc", "[a-z]"));

        assertThrows(
                PatternSyntaxException.class,
                () -> RegExUtils.removeAll("any", "{badRegexSyntax}"),
                "RegExUtils.removeAll expecting PatternSyntaxException");
    }

    @Test
    public void testRemoveFirst() {
        assertNull(RegExUtils.removeFirst((CharSequence) null, Pattern.compile("")));
        assertEquals("any", RegExUtils.removeFirst((CharSequence) "any", (Pattern) null));

        assertEquals("any", RegExUtils.removeFirst((CharSequence) "any", Pattern.compile("")));
        assertEquals("", RegExUtils.removeFirst((CharSequence) "any", Pattern.compile(".*")));
        assertEquals("", RegExUtils.removeFirst((CharSequence) "any", Pattern.compile(".+")));
        assertEquals("bc", RegExUtils.removeFirst((CharSequence) "abc", Pattern.compile(".?")));

        assertEquals("A\n<__>B", RegExUtils.removeFirst((CharSequence) "A<__>\n<__>B", Pattern.compile("<.*>")));
        assertEquals("AB", RegExUtils.removeFirst((CharSequence) "A<__>\n<__>B", Pattern.compile("(?s)<.*>")));
        assertEquals("ABCbc123", RegExUtils.removeFirst((CharSequence) "ABCabc123", Pattern.compile("[a-z]")));
        assertEquals("ABC123abc", RegExUtils.removeFirst((CharSequence) "ABCabc123abc", Pattern.compile("[a-z]+")));
    }

    @Test
    public void testRemoveFirstDeprecated() {
        assertNull(RegExUtils.removeFirst(null, Pattern.compile("")));
        assertEquals("any", RegExUtils.removeFirst("any", (Pattern) null));

        assertEquals("any", RegExUtils.removeFirst("any", Pattern.compile("")));
        assertEquals("", RegExUtils.removeFirst("any", Pattern.compile(".*")));
        assertEquals("", RegExUtils.removeFirst("any", Pattern.compile(".+")));
        assertEquals("bc", RegExUtils.removeFirst("abc", Pattern.compile(".?")));

        assertEquals("A\n<__>B", RegExUtils.removeFirst("A<__>\n<__>B", Pattern.compile("<.*>")));
        assertEquals("AB", RegExUtils.removeFirst("A<__>\n<__>B", Pattern.compile("(?s)<.*>")));
        assertEquals("ABCbc123", RegExUtils.removeFirst("ABCabc123", Pattern.compile("[a-z]")));
        assertEquals("ABC123abc", RegExUtils.removeFirst("ABCabc123abc", Pattern.compile("[a-z]+")));
    }

    @Test
    public void testRemoveFirst_StringString() {
        assertNull(RegExUtils.removeFirst(null, ""));
        assertEquals("any", RegExUtils.removeFirst("any", (String) null));

        assertEquals("any", RegExUtils.removeFirst("any", ""));
        assertEquals("", RegExUtils.removeFirst("any", ".*"));
        assertEquals("", RegExUtils.removeFirst("any", ".+"));
        assertEquals("bc", RegExUtils.removeFirst("abc", ".?"));

        assertEquals("A\n<__>B", RegExUtils.removeFirst("A<__>\n<__>B", "<.*>"));
        assertEquals("AB", RegExUtils.removeFirst("A<__>\n<__>B", "(?s)<.*>"));
        assertEquals("ABCbc123", RegExUtils.removeFirst("ABCabc123", "[a-z]"));
        assertEquals("ABC123abc", RegExUtils.removeFirst("ABCabc123abc", "[a-z]+"));

        assertThrows(
                PatternSyntaxException.class,
                () -> RegExUtils.removeFirst("any", "{badRegexSyntax}"),
                "RegExUtils.removeFirst expecting PatternSyntaxException");
    }

    @Test
    public void testRemovePattern() {
        assertNull(RegExUtils.removePattern((CharSequence) null, ""));
        assertEquals("any", RegExUtils.removePattern((CharSequence) "any", (String) null));

        assertEquals("", RegExUtils.removePattern((CharSequence) "", ""));
        assertEquals("", RegExUtils.removePattern((CharSequence) "", ".*"));
        assertEquals("", RegExUtils.removePattern((CharSequence) "", ".+"));

        assertEquals("AB", RegExUtils.removePattern((CharSequence) "A<__>\n<__>B", "<.*>"));
        assertEquals("AB", RegExUtils.removePattern((CharSequence) "A<__>\\n<__>B", "<.*>"));
        assertEquals("", RegExUtils.removePattern((CharSequence) "<A>x\\ny</A>", "<A>.*</A>"));
        assertEquals("", RegExUtils.removePattern((CharSequence) "<A>\nxy\n</A>", "<A>.*</A>"));

        assertEquals("ABC123", RegExUtils.removePattern((CharSequence) "ABCabc123", "[a-z]"));
    }

    @Test
    public void testRemovePatternDeprecated() {
        assertNull(RegExUtils.removePattern(null, ""));
        assertEquals("any", RegExUtils.removePattern("any", (String) null));

        assertEquals("", RegExUtils.removePattern("", ""));
        assertEquals("", RegExUtils.removePattern("", ".*"));
        assertEquals("", RegExUtils.removePattern("", ".+"));

        assertEquals("AB", RegExUtils.removePattern("A<__>\n<__>B", "<.*>"));
        assertEquals("AB", RegExUtils.removePattern("A<__>\\n<__>B", "<.*>"));
        assertEquals("", RegExUtils.removePattern("<A>x\\ny</A>", "<A>.*</A>"));
        assertEquals("", RegExUtils.removePattern("<A>\nxy\n</A>", "<A>.*</A>"));

        assertEquals("ABC123", RegExUtils.removePattern("ABCabc123", "[a-z]"));
    }

    @Test
    public void testReplaceAll() {
        assertNull(RegExUtils.replaceAll((CharSequence) null, Pattern.compile(""), ""));

        assertEquals("any", RegExUtils.replaceAll((CharSequence) "any", (Pattern) null, ""));
        assertEquals("any", RegExUtils.replaceAll((CharSequence) "any", Pattern.compile(""), null));

        assertEquals("zzz", RegExUtils.replaceAll((CharSequence) "", Pattern.compile(""), "zzz"));
        assertEquals("zzz", RegExUtils.replaceAll((CharSequence) "", Pattern.compile(".*"), "zzz"));
        assertEquals("", RegExUtils.replaceAll((CharSequence) "", Pattern.compile(".+"), "zzz"));
        assertEquals("ZZaZZbZZcZZ", RegExUtils.replaceAll((CharSequence) "abc", Pattern.compile(""), "ZZ"));

        assertEquals("z\nz", RegExUtils.replaceAll((CharSequence) "<__>\n<__>", Pattern.compile("<.*>"), "z"));
        assertEquals("z", RegExUtils.replaceAll((CharSequence) "<__>\n<__>", Pattern.compile("(?s)<.*>"), "z"));

        assertEquals("z", RegExUtils.replaceAll((CharSequence) "<__>\n<__>", Pattern.compile("<.*>", Pattern.DOTALL), "z"));
        assertEquals("z", RegExUtils.replaceAll((CharSequence) "<__>\\n<__>", Pattern.compile("<.*>"), "z"));
        assertEquals("X", RegExUtils.replaceAll((CharSequence) "<A>\nxy\n</A>", Pattern.compile("<A>.*</A>", Pattern.DOTALL), "X"));

        assertEquals("ABC___123", RegExUtils.replaceAll((CharSequence) "ABCabc123", Pattern.compile("[a-z]"), "_"));
        assertEquals("ABC_123", RegExUtils.replaceAll((CharSequence) "ABCabc123", Pattern.compile("[^A-Z0-9]+"), "_"));
        assertEquals("ABC123", RegExUtils.replaceAll((CharSequence) "ABCabc123", Pattern.compile("[^A-Z0-9]+"), ""));
        assertEquals("Lorem_ipsum_dolor_sit", RegExUtils.replaceAll((CharSequence) "Lorem ipsum  dolor   sit", Pattern.compile("( +)([a-z]+)"), "_$2"));
    }

    @Test
    public void testReplaceAllDeprecated() {
        assertNull(RegExUtils.replaceAll(null, Pattern.compile(""), ""));

        assertEquals("any", RegExUtils.replaceAll("any", (Pattern) null, ""));
        assertEquals("any", RegExUtils.replaceAll("any", Pattern.compile(""), null));

        assertEquals("zzz", RegExUtils.replaceAll("", Pattern.compile(""), "zzz"));
        assertEquals("zzz", RegExUtils.replaceAll("", Pattern.compile(".*"), "zzz"));
        assertEquals("", RegExUtils.replaceAll("", Pattern.compile(".+"), "zzz"));
        assertEquals("ZZaZZbZZcZZ", RegExUtils.replaceAll("abc", Pattern.compile(""), "ZZ"));

        assertEquals("z\nz", RegExUtils.replaceAll("<__>\n<__>", Pattern.compile("<.*>"), "z"));
        assertEquals("z", RegExUtils.replaceAll("<__>\n<__>", Pattern.compile("(?s)<.*>"), "z"));

        assertEquals("z", RegExUtils.replaceAll("<__>\n<__>", Pattern.compile("<.*>", Pattern.DOTALL), "z"));
        assertEquals("z", RegExUtils.replaceAll("<__>\\n<__>", Pattern.compile("<.*>"), "z"));
        assertEquals("X", RegExUtils.replaceAll("<A>\nxy\n</A>", Pattern.compile("<A>.*</A>", Pattern.DOTALL), "X"));

        assertEquals("ABC___123", RegExUtils.replaceAll("ABCabc123", Pattern.compile("[a-z]"), "_"));
        assertEquals("ABC_123", RegExUtils.replaceAll("ABCabc123", Pattern.compile("[^A-Z0-9]+"), "_"));
        assertEquals("ABC123", RegExUtils.replaceAll("ABCabc123", Pattern.compile("[^A-Z0-9]+"), ""));
        assertEquals("Lorem_ipsum_dolor_sit", RegExUtils.replaceAll("Lorem ipsum  dolor   sit", Pattern.compile("( +)([a-z]+)"), "_$2"));
    }

    @Test
    public void testReplaceAll_StringStringString() {
        assertNull(RegExUtils.replaceAll(null, "", ""));

        assertEquals("any", RegExUtils.replaceAll("any", (String) null, ""));
        assertEquals("any", RegExUtils.replaceAll("any", "", null));

        assertEquals("zzz", RegExUtils.replaceAll("", "", "zzz"));
        assertEquals("zzz", RegExUtils.replaceAll("", ".*", "zzz"));
        assertEquals("", RegExUtils.replaceAll("", ".+", "zzz"));
        assertEquals("ZZaZZbZZcZZ", RegExUtils.replaceAll("abc", "", "ZZ"));

        assertEquals("z\nz", RegExUtils.replaceAll("<__>\n<__>", "<.*>", "z"));
        assertEquals("z", RegExUtils.replaceAll("<__>\n<__>", "(?s)<.*>", "z"));

        assertEquals("ABC___123", RegExUtils.replaceAll("ABCabc123", "[a-z]", "_"));
        assertEquals("ABC_123", RegExUtils.replaceAll("ABCabc123", "[^A-Z0-9]+", "_"));
        assertEquals("ABC123", RegExUtils.replaceAll("ABCabc123", "[^A-Z0-9]+", ""));
        assertEquals("Lorem_ipsum_dolor_sit", RegExUtils.replaceAll("Lorem ipsum  dolor   sit", "( +)([a-z]+)", "_$2"));

        assertThrows(
                PatternSyntaxException.class,
                () -> RegExUtils.replaceAll("any", "{badRegexSyntax}", ""),
                "RegExUtils.replaceAll expecting PatternSyntaxException");
    }

    @Test
    public void testReplaceFirst() {
        assertNull(RegExUtils.replaceFirst((CharSequence) null, Pattern.compile(""), ""));

        assertEquals("any", RegExUtils.replaceFirst((CharSequence) "any", (Pattern) null, ""));
        assertEquals("any", RegExUtils.replaceFirst((CharSequence) "any", Pattern.compile(""), null));

        assertEquals("zzz", RegExUtils.replaceFirst((CharSequence) "", Pattern.compile(""), "zzz"));
        assertEquals("zzz", RegExUtils.replaceFirst((CharSequence) "", Pattern.compile(".*"), "zzz"));
        assertEquals("", RegExUtils.replaceFirst((CharSequence) "", Pattern.compile(".+"), "zzz"));
        assertEquals("ZZabc", RegExUtils.replaceFirst((CharSequence) "abc", Pattern.compile(""), "ZZ"));

        assertEquals("z\n<__>", RegExUtils.replaceFirst((CharSequence) "<__>\n<__>", Pattern.compile("<.*>"), "z"));
        assertEquals("z", RegExUtils.replaceFirst((CharSequence) "<__>\n<__>", Pattern.compile("(?s)<.*>"), "z"));

        assertEquals("ABC_bc123", RegExUtils.replaceFirst((CharSequence) "ABCabc123", Pattern.compile("[a-z]"), "_"));
        assertEquals("ABC_123abc", RegExUtils.replaceFirst((CharSequence) "ABCabc123abc", Pattern.compile("[^A-Z0-9]+"), "_"));
        assertEquals("ABC123abc", RegExUtils.replaceFirst((CharSequence) "ABCabc123abc", Pattern.compile("[^A-Z0-9]+"), ""));
        assertEquals("Lorem_ipsum  dolor   sit", RegExUtils.replaceFirst((CharSequence) "Lorem ipsum  dolor   sit", Pattern.compile("( +)([a-z]+)"), "_$2"));
    }

    @Test
    public void testReplaceFirstDeprecated() {
        assertNull(RegExUtils.replaceFirst(null, Pattern.compile(""), ""));

        assertEquals("any", RegExUtils.replaceFirst("any", (Pattern) null, ""));
        assertEquals("any", RegExUtils.replaceFirst("any", Pattern.compile(""), null));

        assertEquals("zzz", RegExUtils.replaceFirst("", Pattern.compile(""), "zzz"));
        assertEquals("zzz", RegExUtils.replaceFirst("", Pattern.compile(".*"), "zzz"));
        assertEquals("", RegExUtils.replaceFirst("", Pattern.compile(".+"), "zzz"));
        assertEquals("ZZabc", RegExUtils.replaceFirst("abc", Pattern.compile(""), "ZZ"));

        assertEquals("z\n<__>", RegExUtils.replaceFirst("<__>\n<__>", Pattern.compile("<.*>"), "z"));
        assertEquals("z", RegExUtils.replaceFirst("<__>\n<__>", Pattern.compile("(?s)<.*>"), "z"));

        assertEquals("ABC_bc123", RegExUtils.replaceFirst("ABCabc123", Pattern.compile("[a-z]"), "_"));
        assertEquals("ABC_123abc", RegExUtils.replaceFirst("ABCabc123abc", Pattern.compile("[^A-Z0-9]+"), "_"));
        assertEquals("ABC123abc", RegExUtils.replaceFirst("ABCabc123abc", Pattern.compile("[^A-Z0-9]+"), ""));
        assertEquals("Lorem_ipsum  dolor   sit", RegExUtils.replaceFirst("Lorem ipsum  dolor   sit", Pattern.compile("( +)([a-z]+)"), "_$2"));
    }

    @Test
    public void testReplaceFirst_StringStringString() {
        assertNull(RegExUtils.replaceFirst(null, "", ""));

        assertEquals("any", RegExUtils.replaceFirst("any", (String) null, ""));
        assertEquals("any", RegExUtils.replaceFirst("any", "", null));

        assertEquals("zzz", RegExUtils.replaceFirst("", "", "zzz"));
        assertEquals("zzz", RegExUtils.replaceFirst("", ".*", "zzz"));
        assertEquals("", RegExUtils.replaceFirst("", ".+", "zzz"));
        assertEquals("ZZabc", RegExUtils.replaceFirst("abc", "", "ZZ"));

        assertEquals("z\n<__>", RegExUtils.replaceFirst("<__>\n<__>", "<.*>", "z"));
        assertEquals("z", RegExUtils.replaceFirst("<__>\n<__>", "(?s)<.*>", "z"));

        assertEquals("ABC_bc123", RegExUtils.replaceFirst("ABCabc123", "[a-z]", "_"));
        assertEquals("ABC_123abc", RegExUtils.replaceFirst("ABCabc123abc", "[^A-Z0-9]+", "_"));
        assertEquals("ABC123abc", RegExUtils.replaceFirst("ABCabc123abc", "[^A-Z0-9]+", ""));
        assertEquals("Lorem_ipsum  dolor   sit",
                RegExUtils.replaceFirst("Lorem ipsum  dolor   sit", "( +)([a-z]+)", "_$2"));

        assertThrows(
                PatternSyntaxException.class,
                () -> RegExUtils.replaceFirst("any", "{badRegexSyntax}", ""),
                "RegExUtils.replaceFirst expecting PatternSyntaxException");
    }

    @Test
    public void testReplacePattern() {
        assertNull(RegExUtils.replacePattern((CharSequence) null, "", ""));
        assertEquals("any", RegExUtils.replacePattern((CharSequence) "any", (String) null, ""));
        assertEquals("any", RegExUtils.replacePattern((CharSequence) "any", "", null));

        assertEquals("zzz", RegExUtils.replacePattern((CharSequence) "", "", "zzz"));
        assertEquals("zzz", RegExUtils.replacePattern((CharSequence) "", ".*", "zzz"));
        assertEquals("", RegExUtils.replacePattern((CharSequence) "", ".+", "zzz"));

        assertEquals("z", RegExUtils.replacePattern((CharSequence) "<__>\n<__>", "<.*>", "z"));
        assertEquals("z", RegExUtils.replacePattern((CharSequence) "<__>\\n<__>", "<.*>", "z"));
        assertEquals("X", RegExUtils.replacePattern((CharSequence) "<A>\nxy\n</A>", "<A>.*</A>", "X"));

        assertEquals("ABC___123", RegExUtils.replacePattern((CharSequence) "ABCabc123", "[a-z]", "_"));
        assertEquals("ABC_123", RegExUtils.replacePattern((CharSequence) "ABCabc123", "[^A-Z0-9]+", "_"));
        assertEquals("ABC123", RegExUtils.replacePattern((CharSequence) "ABCabc123", "[^A-Z0-9]+", ""));
        assertEquals("Lorem_ipsum_dolor_sit", RegExUtils.replacePattern((CharSequence) "Lorem ipsum  dolor   sit", "( +)([a-z]+)", "_$2"));
    }

    @Test
    public void testReplacePatternDeprecated() {
        assertNull(RegExUtils.replacePattern(null, "", ""));
        assertEquals("any", RegExUtils.replacePattern("any", (String) null, ""));
        assertEquals("any", RegExUtils.replacePattern("any", "", null));

        assertEquals("zzz", RegExUtils.replacePattern("", "", "zzz"));
        assertEquals("zzz", RegExUtils.replacePattern("", ".*", "zzz"));
        assertEquals("", RegExUtils.replacePattern("", ".+", "zzz"));

        assertEquals("z", RegExUtils.replacePattern("<__>\n<__>", "<.*>", "z"));
        assertEquals("z", RegExUtils.replacePattern("<__>\\n<__>", "<.*>", "z"));
        assertEquals("X", RegExUtils.replacePattern("<A>\nxy\n</A>", "<A>.*</A>", "X"));

        assertEquals("ABC___123", RegExUtils.replacePattern("ABCabc123", "[a-z]", "_"));
        assertEquals("ABC_123", RegExUtils.replacePattern("ABCabc123", "[^A-Z0-9]+", "_"));
        assertEquals("ABC123", RegExUtils.replacePattern("ABCabc123", "[^A-Z0-9]+", ""));
        assertEquals("Lorem_ipsum_dolor_sit", RegExUtils.replacePattern("Lorem ipsum  dolor   sit", "( +)([a-z]+)", "_$2"));
    }

    private RegExUtils utils;

    @BeforeEach
    void setUp() {
        utils = new RegExUtils();
    }

    @Test
    public void testFindMultipleMatches() {
        Pattern pattern = Pattern.compile("\\d+");
        List<String> result = utils.findMatches("asd 123 fgd 456 zxc", pattern);
        assertEquals(Arrays.asList("123", "456"), result);
    }

    @Test
    public void testFindNoMatches() {
        Pattern pattern = Pattern.compile("\\d+");
        List<String> result = utils.findMatches("asd zxc", pattern);
        assertTrue(result.isEmpty());
    }

    @Test
    void testFindMatchesWhenTextIsEmpty() {
        String[] result = utils.findMatchesAsArray("", "\\w+");
        assertArrayEquals(new String[0], result);
    }
//    @Test
//    public void testShouldReturnArrayList() {
//        Pattern pattern = Pattern.compile("\\w+");
//        List<String> result = utils.findMatches("test", pattern);
//        assertEquals(Arrays.asList("test"), result);
//        assertTrue(result instanceof ArrayList);
//    }

    @Test
    public void testFindMatchesWhenTextIsNull() {
        Pattern pattern = Pattern.compile("test");
        List<String> result = utils.findMatches(null, pattern);
        assert (result).isEmpty();
    }

    @Test
    public void testFindMatchesWhenPatternIsNull() {
        List<String> result = utils.findMatches("test", null);
        assertTrue(result.isEmpty());
    }

    @Test
    public void testFindMatchesWithEmptyText() {
        Pattern pattern = Pattern.compile("\\d+");
        List<String> result = utils.findMatches("", pattern);
        assertTrue(result.isEmpty());
    }

    @Test
    public void testFindMultipleMatchesAsArray() {
        String[] result = utils.findMatchesAsArray("test 123 test", "\\w+");
        assertArrayEquals(new String[]{"test", "123", "test"}, result);
    }

    @Test
    void testFindMatchesAsArrayWhenNoMatches() {
        String[] result = utils.findMatchesAsArray("abc", "\\d");
        assertArrayEquals(new String[0], result);
    }

    @Test
    void testFindMatchesAsArrayWhenTextIsNull() {
        String[] result = utils.findMatchesAsArray(null, "\\w+");
        assertArrayEquals(new String[0], result);
    }

    @Test
    void testFindMatchesAsArrayWhenTextIsEmpty() {
        String[] result = utils.findMatchesAsArray("", "\\w+");
        assertArrayEquals(new String[0], result);
    }

    @Test
    void testFindMatchesAsArrayWithInvalidRegex() {
        String[] result = utils.findMatchesAsArray("text", "[a-z");
        assertArrayEquals(new String[0], result);
    }

    @Test
    public void testReturnedListIsUnmodifiable() {
        Pattern pattern = Pattern.compile("\\w+");
        List<String> result = utils.findMatches("test", pattern);

        try {
            result.add("should fail");
            fail("Expected UnsupportedOperationException");
        } catch (UnsupportedOperationException e) {}
    }

    @Test
    public void testFindMultipleMatchesAsArrayList(){
        Pattern pattern = Pattern.compile("\\d+");
        List<String> result = utils.findMatchesModifiable("asd 123 fgd 456 zxc", pattern);
        assertEquals(Arrays.asList("123", "456"), result);
    }

    @Test
    public void testShouldAddElementInFindMatchesModifiable(){
        Pattern pattern = Pattern.compile("\\w+");
        List<String> result = utils.findMatchesModifiable("asd test 123", pattern);
        result.add("text");
        assertEquals(Arrays.asList("asd", "test", "123", "text"), result);
    }

    @Test
    public void testShouldRemoveElementInFindMatchesModifiable(){
        Pattern pattern = Pattern.compile("\\w+");
        List<String> result = utils.findMatchesModifiable("asd test 123", pattern);
        result.remove("123");
        assertEquals(Arrays.asList("asd","test"), result);
    }

    @Test
    public void testFindNoMatchesAsArrayList() {
        Pattern pattern = Pattern.compile("\\d+");
        List<String> result = utils.findMatchesModifiable("asd zxc", pattern);
        assertTrue(result.isEmpty());
    }

    @Test
    void testFindMatchesAsArrayListWhenTextIsEmpty() {
        Pattern pattern = Pattern.compile("\\d+");
        List<String>result = utils.findMatchesModifiable("", pattern);
        assertTrue(result.isEmpty());
    }

    @Test
    void testFindMatchesAsArrayListWhenTextIsNull() {
        Pattern pattern = Pattern.compile("\\d+");
        List<String>result = utils.findMatchesModifiable(null, pattern);
        assertTrue(result.isEmpty());
    }

    @Test
    void testFindMatchesAsArrayListWhenPatternIsNull(){
        List<String>result = utils.findMatchesModifiable("asd 123", null);
        assertTrue(result.isEmpty());
    }

}
