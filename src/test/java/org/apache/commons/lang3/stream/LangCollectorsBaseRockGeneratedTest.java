package org.apache.commons.lang3.stream;

import org.apache.commons.lang3.stream.LangCollectors;

import org.junit.jupiter.params.provider.MethodSource;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import org.junit.jupiter.api.Test;
import java.util.function.Supplier;
import java.util.stream.Collector;
import static org.hamcrest.MatcherAssert.assertThat;
import org.apache.commons.lang3.stream.LangCollectors;
import java.util.function.BinaryOperator;
import java.util.Arrays;
import java.util.function.BiConsumer;
import java.util.List;
import java.util.StringJoiner;
import org.junit.jupiter.params.ParameterizedTest;
import java.util.Collections;
import static org.mockito.ArgumentMatchers.eq;
import java.util.Objects;
import java.util.stream.Stream;
import java.util.Set;
import java.util.function.Function;
import static org.hamcrest.Matchers.*;
import org.apache.commons.lang3.StringUtils;
import static org.junit.jupiter.api.Assertions.*;
import java.util.stream.Collectors;
import org.junit.jupiter.params.provider.Arguments;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

class LangCollectorsBaseRockGeneratedTest {

    //BaseRock generated method id: ${testJoiningNoArgs}, hash: 5311ED329FEFF1091F4E3C2505107B06
    @Test
    void testJoiningNoArgs() {
        String result = Stream.of("a", "b", "c").collect(LangCollectors.joining());
        assertEquals("abc", result);
    }

    //BaseRock generated method id: ${testJoiningWithDelimiter}, hash: 7BBD49AE7DD23DAE8F65E8488209DF2F
    @Test
    void testJoiningWithDelimiter() {
        String result = Stream.of("a", "b", "c").collect(LangCollectors.joining(", "));
        assertEquals("a, b, c", result);
    }

    //BaseRock generated method id: ${testJoiningWithDelimiterPrefixSuffix}, hash: 636BF0E49426D58843EFF62193C3A095
    @Test
    void testJoiningWithDelimiterPrefixSuffix() {
        String result = Stream.of("a", "b", "c").collect(LangCollectors.joining(", ", "[", "]"));
        assertEquals("[a, b, c]", result);
    }

    //BaseRock generated method id: ${testJoiningWithCustomToString}, hash: 9E80536E98FA89AA50CC057DEA0660FB
    @Test
    void testJoiningWithCustomToString() {
        String result = Stream.of(1, 2, 3).collect(LangCollectors.joining(", ", "(", ")", Object::toString));
        assertEquals("(1, 2, 3)", result);
    }

    //BaseRock generated method id: ${testCollectWithArray}, hash: 3380957155DD9EFD4299B0AA6099140B
    @Test
    void testCollectWithArray() {
        String result = LangCollectors.collect(Collectors.joining(", "), "a", "b", "c");
        assertEquals("a, b, c", result);
    }

    //BaseRock generated method id: ${testCollectWithEmptyArray}, hash: F185C8935C6CAB472BB2A2E6F7C52768
    @Test
    void testCollectWithEmptyArray() {
        String result = LangCollectors.collect(Collectors.joining());
        assertEquals("", result);
    }

    //BaseRock generated method id: ${testCollectWithVariousInputs}, hash: AE4EFF0EBFD40845B3823554850B94A6
    @ParameterizedTest
    @MethodSource("provideCollectTestCases")
    void testCollectWithVariousInputs(Object[] input, String expected) {
        String result = LangCollectors.collect(LangCollectors.joining(", "), input);
        assertEquals(expected, result);
    }

    private static Stream<Arguments> provideCollectTestCases() {
        return Stream.of(Arguments.of(new Object[] { 1, "two", 3.0 }, "1, two, 3.0"), Arguments.of(new Object[] { null, "test", "" }, "null, test, "), Arguments.of(new Object[] {}, ""), Arguments.of(new Object[] { new Object() }, new Object().toString()));
    }

    //BaseRock generated method id: ${testJoiningWithNullElements}, hash: 7C21ABFA42A165C2B64F70CD19BC13BC
    @Test
    void testJoiningWithNullElements() {
        String result = Stream.of("a", null, "c").collect(LangCollectors.joining(", "));
        assertEquals("a, null, c", result);
    }

    //BaseRock generated method id: ${testJoiningWithEmptyStream}, hash: 0B2EF2031FFE5258B9394C640E45AD25
    @Test
    void testJoiningWithEmptyStream() {
        String result = Stream.empty().collect(LangCollectors.joining());
        assertEquals("", result);
    }

    //BaseRock generated method id: ${testJoiningWithSingleElement}, hash: 9DC1F8DDF08439FB2890D2CEA5676A14
    @Test
    void testJoiningWithSingleElement() {
        String result = Stream.of("single").collect(LangCollectors.joining(", ", "Start: ", " :End"));
        assertEquals("Start: single :End", result);
    }

    //BaseRock generated method id: ${testJoiningWithCustomToStringForComplexObjects}, hash: 1F403B6E5CED2F40B822F17900D07EF7
    @Test
    void testJoiningWithCustomToStringForComplexObjects() {
        class TestObject {

            private final String value;

            TestObject(String value) {
                this.value = value;
            }

            @Override
            public String toString() {
                return "TestObject(" + value + ")";
            }
        }
        List<TestObject> objects = Arrays.asList(new TestObject("a"), new TestObject("b"), new TestObject("c"));
        String result = objects.stream().collect(LangCollectors.joining(", ", "{", "}", obj -> ((TestObject) obj).value));
        assertEquals("{a, b, c}", result);
    }
}
