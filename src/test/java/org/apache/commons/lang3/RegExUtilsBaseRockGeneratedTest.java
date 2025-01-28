package org.apache.commons.lang3;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.mockito.MockedStatic;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.hamcrest.Matchers.is;
import org.junit.jupiter.api.Disabled;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class RegExUtilsBaseRockGeneratedTest {

    //BaseRock generated method id: ${dotAllTest}, hash: 6CEF815A9B1FE019FAC7834FC95AFEAC
    @Test()
    void dotAllTest() {
        //Act Statement(s)
        Pattern result = RegExUtils.dotAll("A");
        //Assert statement(s)
        //TODO: Please implement equals method in Pattern for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${dotAllMatcherTest}, hash: 02D2671DE730C81567A71B2AF1ACB66B
    @Test()
    void dotAllMatcherTest() {
        //Act Statement(s)
        Matcher result = RegExUtils.dotAllMatcher("A", (CharSequence) "text1");
        //Assert statement(s)
        //TODO: Please implement equals method in Matcher for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${dotAllMatcher1Test}, hash: 57E9F9D35EED6A030EDA59F7897C3240
    @Test()
    void dotAllMatcher1Test() {
        //Act Statement(s)
        Matcher result = RegExUtils.dotAllMatcher("A", "B");
        //Assert statement(s)
        //TODO: Please implement equals method in Matcher for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${removeAllTest}, hash: 2599EE77BE5FAAB1F9592DC326DE0856
    @Disabled()
    @Test()
    void removeAllTest() {
        //Arrange Statement(s)
        try (MockedStatic<RegExUtils> regExUtils = mockStatic(RegExUtils.class, CALLS_REAL_METHODS)) {
            regExUtils.when(() -> RegExUtils.replaceAll(eq("text1"), (Pattern) any(), eq(""))).thenReturn("return_of_replaceAll1");
            Pattern pattern = Pattern.compile("regex1");
            //Act Statement(s)
            String result = RegExUtils.removeAll((CharSequence) "text1", pattern);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_replaceAll1"));
                regExUtils.verify(() -> RegExUtils.replaceAll(eq("text1"), (Pattern) any(), eq("")), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeAll1Test}, hash: CE6BEE3297E0E0560C5D10FE69FCFA1B
    @Disabled()
    @Test()
    void removeAll1Test() {
        //Arrange Statement(s)
        try (MockedStatic<RegExUtils> regExUtils = mockStatic(RegExUtils.class, CALLS_REAL_METHODS)) {
            regExUtils.when(() -> RegExUtils.replaceAll(eq("text1"), (Pattern) any(), eq(""))).thenReturn("return_of_replaceAll1");
            Pattern pattern = Pattern.compile("regex1");
            //Act Statement(s)
            String result = RegExUtils.removeAll("text1", pattern);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_replaceAll1"));
                regExUtils.verify(() -> RegExUtils.replaceAll(eq("text1"), (Pattern) any(), eq("")), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeAll2Test}, hash: 1D88C9846ECBA8B88106317A5DD9B58F
    @Test()
    void removeAll2Test() {
        //Arrange Statement(s)
        try (MockedStatic<RegExUtils> regExUtils = mockStatic(RegExUtils.class, CALLS_REAL_METHODS)) {
            regExUtils.when(() -> RegExUtils.replaceAll("text1", "regex1", "")).thenReturn("return_of_replaceAll1");
            //Act Statement(s)
            String result = RegExUtils.removeAll("text1", "regex1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_replaceAll1"));
                regExUtils.verify(() -> RegExUtils.replaceAll("text1", "regex1", ""), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeFirstTest}, hash: 8C6DE944D49F1D73B2B4A14847E04714
    @Disabled()
    @Test()
    void removeFirstTest() {
        //Arrange Statement(s)
        try (MockedStatic<RegExUtils> regExUtils = mockStatic(RegExUtils.class, CALLS_REAL_METHODS)) {
            regExUtils.when(() -> RegExUtils.replaceFirst(eq("text1"), (Pattern) any(), eq(""))).thenReturn("return_of_replaceFirst1");
            Pattern pattern = Pattern.compile("regex1");
            //Act Statement(s)
            String result = RegExUtils.removeFirst((CharSequence) "text1", pattern);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_replaceFirst1"));
                regExUtils.verify(() -> RegExUtils.replaceFirst(eq("text1"), (Pattern) any(), eq("")), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeFirst1Test}, hash: 6DD13F735EB318B16CC34DA06D3AEF2E
    @Test()
    void removeFirst1Test() {
        //Arrange Statement(s)
        try (MockedStatic<RegExUtils> regExUtils = mockStatic(RegExUtils.class, CALLS_REAL_METHODS)) {
            regExUtils.when(() -> RegExUtils.replaceFirst(eq("text1"), (Pattern) any(), eq(""))).thenReturn("return_of_replaceFirst1");
            Pattern pattern = Pattern.compile("regex1");
            //Act Statement(s)
            String result = RegExUtils.removeFirst("text1", pattern);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_replaceFirst1"));
                regExUtils.verify(() -> RegExUtils.replaceFirst(eq("text1"), (Pattern) any(), eq("")), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeFirst2Test}, hash: 75E53D3EC7E00A23F379B72AC3714821
    @Test()
    void removeFirst2Test() {
        //Arrange Statement(s)
        try (MockedStatic<RegExUtils> regExUtils = mockStatic(RegExUtils.class, CALLS_REAL_METHODS)) {
            regExUtils.when(() -> RegExUtils.replaceFirst("text1", "regex1", "")).thenReturn("return_of_replaceFirst1");
            //Act Statement(s)
            String result = RegExUtils.removeFirst("text1", "regex1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_replaceFirst1"));
                regExUtils.verify(() -> RegExUtils.replaceFirst("text1", "regex1", ""), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removePatternTest}, hash: C0B1FDAE774F2ECF9ECA8B78D3F2B770
    @Disabled()
    @Test()
    void removePatternTest() {
        //Arrange Statement(s)
        try (MockedStatic<RegExUtils> regExUtils = mockStatic(RegExUtils.class, CALLS_REAL_METHODS)) {
            regExUtils.when(() -> RegExUtils.replacePattern("text1", "regex1", "")).thenReturn("return_of_replacePattern1");
            //Act Statement(s)
            String result = RegExUtils.removePattern((CharSequence) "text1", "regex1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_replacePattern1"));
                regExUtils.verify(() -> RegExUtils.replacePattern("text1", "regex1", ""), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removePattern1Test}, hash: 44975F195689054BFBE93BF3351CC2A0
    @Disabled()
    @Test()
    void removePattern1Test() {
        //Arrange Statement(s)
        try (MockedStatic<RegExUtils> regExUtils = mockStatic(RegExUtils.class, CALLS_REAL_METHODS)) {
            regExUtils.when(() -> RegExUtils.replacePattern("text1", "regex1", "")).thenReturn("return_of_replacePattern1");
            //Act Statement(s)
            String result = RegExUtils.removePattern("text1", "regex1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_replacePattern1"));
                regExUtils.verify(() -> RegExUtils.replacePattern("text1", "regex1", ""), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${replaceAllWhenObjectUtilsAnyNullTextRegexReplacement}, hash: 8A62498B67408463D05284BBE1DDF393
    @Disabled()
    @Test()
    void replaceAllWhenObjectUtilsAnyNullTextRegexReplacement() {
        /* Branches:
         * (ObjectUtils.anyNull(text, regex, replacement)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Pattern pattern = Pattern.compile("regex1");
        //Act Statement(s)
        String result = RegExUtils.replaceAll((CharSequence) "text1", pattern, "replacement1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${replaceAllWhenObjectUtilsNotAnyNullTextRegexReplacement}, hash: 0FBA487B1A7053E09D315A5DEF88FF61
    @Disabled()
    @Test()
    void replaceAllWhenObjectUtilsNotAnyNullTextRegexReplacement() {
        /* Branches:
         * (ObjectUtils.anyNull(text, regex, replacement)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Pattern pattern = Pattern.compile("regex1");
        //Act Statement(s)
        String result = RegExUtils.replaceAll((CharSequence) "text1", pattern, "replacement1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${replaceAll1Test}, hash: EB5214381DEF0DDE8537F4A011F66183
    @Test()
    void replaceAll1Test() {
        //Arrange Statement(s)
        try (MockedStatic<RegExUtils> regExUtils = mockStatic(RegExUtils.class, CALLS_REAL_METHODS)) {
            regExUtils.when(() -> RegExUtils.replaceAll(eq("text1"), (Pattern) any(), eq("replacement1"))).thenReturn("return_of_replaceAll1");
            Pattern pattern = Pattern.compile("regex1");
            //Act Statement(s)
            String result = RegExUtils.replaceAll("text1", pattern, "replacement1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_replaceAll1"));
                regExUtils.verify(() -> RegExUtils.replaceAll(eq("text1"), (Pattern) any(), eq("replacement1")), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${replaceAll2WhenObjectUtilsAnyNullTextRegexReplacement}, hash: 1BD2F774AA2C912B6FF44EF7926219F3
    @Test()
    void replaceAll2WhenObjectUtilsAnyNullTextRegexReplacement() {
        /* Branches:
         * (ObjectUtils.anyNull(text, regex, replacement)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String result = RegExUtils.replaceAll("text1", "regex1", "replacement1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("text1")));
    }

    //BaseRock generated method id: ${replaceAll2WhenObjectUtilsNotAnyNullTextRegexReplacement}, hash: FFF4A546DACDC2DA059969CE6884728D
    @Test()
    void replaceAll2WhenObjectUtilsNotAnyNullTextRegexReplacement() {
        /* Branches:
         * (ObjectUtils.anyNull(text, regex, replacement)) : false
         */
        //Act Statement(s)
        String result = RegExUtils.replaceAll("C", "A", "B");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("C")));
    }

    //BaseRock generated method id: ${replaceFirstWhenReplacementIsNull}, hash: 6D912072F81B1411D3679E758AB7CE9D
    @Disabled()
    @Test()
    void replaceFirstWhenReplacementIsNull() {
        /* Branches:
         * (text == null) : false
         * (regex == null) : false
         * (replacement == null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Pattern pattern = Pattern.compile("regex1");
        //Act Statement(s)
        String result = RegExUtils.replaceFirst((CharSequence) "text1", pattern, (String) null);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${replaceFirstWhenReplacementIsNotNull}, hash: 0DCECE80DAB14FA09045E370D16A2B9A
    @Test()
    void replaceFirstWhenReplacementIsNotNull() {
        /* Branches:
         * (text == null) : false
         * (regex == null) : false
         * (replacement == null) : false
         */
        //Arrange Statement(s)
        Pattern pattern = Pattern.compile("regex1");
        //Act Statement(s)
        String result = RegExUtils.replaceFirst((CharSequence) "text1", pattern, "replacement1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("text1")));
    }

    //BaseRock generated method id: ${replaceFirst1Test}, hash: DD7882655B6EC27283413CEDA13437A0
    @Test()
    void replaceFirst1Test() {
        //Arrange Statement(s)
        try (MockedStatic<RegExUtils> regExUtils = mockStatic(RegExUtils.class, CALLS_REAL_METHODS)) {
            regExUtils.when(() -> RegExUtils.replaceFirst(eq("text1"), (Pattern) any(), eq("replacement1"))).thenReturn("return_of_replaceFirst1");
            Pattern pattern = Pattern.compile("regex1");
            //Act Statement(s)
            String result = RegExUtils.replaceFirst("text1", pattern, "replacement1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_replaceFirst1"));
                regExUtils.verify(() -> RegExUtils.replaceFirst(eq("text1"), (Pattern) any(), eq("replacement1")), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${replaceFirst2WhenReplacementIsNull}, hash: 071E446D2FB78B2F7B2CC00A9B799164
    @Test()
    void replaceFirst2WhenReplacementIsNull() {
        /* Branches:
         * (text == null) : false
         * (regex == null) : false
         * (replacement == null) : true
         */
        //Act Statement(s)
        String result = RegExUtils.replaceFirst("text1", "regex1", (String) null);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("text1")));
    }

    //BaseRock generated method id: ${replaceFirst2WhenReplacementIsNotNull}, hash: E8BDA68AADD2AA27497B85B3C05FA11A
    @Test()
    void replaceFirst2WhenReplacementIsNotNull() {
        /* Branches:
         * (text == null) : false
         * (regex == null) : false
         * (replacement == null) : false
         */
        //Act Statement(s)
        String result = RegExUtils.replaceFirst("C", "A", "B");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("C")));
    }

    //BaseRock generated method id: ${replacePatternWhenObjectUtilsAnyNullTextRegexReplacement}, hash: 85F6C73820AB5AD5EE3468159F6EF09F
    @Disabled()
    @Test()
    void replacePatternWhenObjectUtilsAnyNullTextRegexReplacement() {
        /* Branches:
         * (ObjectUtils.anyNull(text, regex, replacement)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String result = RegExUtils.replacePattern((CharSequence) "text1", "regex1", "replacement1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${replacePatternWhenObjectUtilsNotAnyNullTextRegexReplacement}, hash: 1E5CB7EBF2918CD0ABB26D255B36D79E
    @Disabled()
    @Test()
    void replacePatternWhenObjectUtilsNotAnyNullTextRegexReplacement() {
        /* Branches:
         * (ObjectUtils.anyNull(text, regex, replacement)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Matcher matcherMock = mock(Matcher.class);
        try (MockedStatic<RegExUtils> regExUtils = mockStatic(RegExUtils.class, CALLS_REAL_METHODS)) {
            regExUtils.when(() -> RegExUtils.dotAllMatcher("regex1", "text1")).thenReturn(matcherMock);
            //Act Statement(s)
            String result = RegExUtils.replacePattern((CharSequence) "text1", "regex1", "replacement1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                regExUtils.verify(() -> RegExUtils.dotAllMatcher("regex1", "text1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${replacePattern1Test}, hash: 25B95EC853972409DCBE7EE1E6CA14A4
    @Test()
    void replacePattern1Test() {
        //Arrange Statement(s)
        try (MockedStatic<RegExUtils> regExUtils = mockStatic(RegExUtils.class, CALLS_REAL_METHODS)) {
            regExUtils.when(() -> RegExUtils.replacePattern("text1", "regex1", "replacement1")).thenReturn("return_of_replacePattern1");
            //Act Statement(s)
            String result = RegExUtils.replacePattern("text1", "regex1", "replacement1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_replacePattern1"));
                regExUtils.verify(() -> RegExUtils.replacePattern("text1", "regex1", "replacement1"), atLeast(1));
            });
        }
    }
}
