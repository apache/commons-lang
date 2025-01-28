package org.apache.commons.lang3.text;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mockStatic;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class WordUtilsBaseRockGeneratedTest {

    //BaseRock generated method id: ${capitalizeTest}, hash: 4F0301E34F42A8D3E6240C9F04EFD1B7
    @Test()
    void capitalizeTest() {
        //Arrange Statement(s)
        try (MockedStatic<WordUtils> wordUtils = mockStatic(WordUtils.class, CALLS_REAL_METHODS)) {
            wordUtils.when(() -> WordUtils.capitalize("str1", (char[]) null)).thenReturn("return_of_capitalize1");
            //Act Statement(s)
            String result = WordUtils.capitalize("str1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_capitalize1"));
                wordUtils.verify(() -> WordUtils.capitalize("str1", (char[]) null), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${capitalize1WhenStringUtilsIsEmptyStr}, hash: 77C798008B35732EA35ADB4E17125D24
    @Test()
    void capitalize1WhenStringUtilsIsEmptyStr() {
        /* Branches:
         * (delimiters == null) : true
         * (StringUtils.isEmpty(str)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        char[] _char = null;
        
        //Act Statement(s)
        String result = WordUtils.capitalize("str1", _char);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("str1")));
    }

    //BaseRock generated method id: ${capitalize1WhenIsDelimiterChDelimiters}, hash: 8214B7FB5B0F6234EA9C3AF00ABC8412
    @Test()
    void capitalize1WhenIsDelimiterChDelimiters() {
        /* Branches:
         * (delimiters == null) : true
         * (StringUtils.isEmpty(str)) : false
         * (delimLen == 0) : false
         * (i < buffer.length) : true
         * (delimiters == null) : true  #  inside isDelimiter method
         * (delimiters == null ? Character.isWhitespace(ch) : ArrayUtils.contains(delimiters, ch)) : true  #  inside isDelimiter method
         * (isDelimiter(ch, delimiters)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        char[] _char = null;
        
        //Act Statement(s)
        String result = WordUtils.capitalize("str1", _char);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${capitalize1WhenCapitalizeNext}, hash: AE31EA4DB156CEB5F40EACF880251B36
    @Test()
    void capitalize1WhenCapitalizeNext() {
        /* Branches:
         * (delimiters == null) : true
         * (StringUtils.isEmpty(str)) : false
         * (delimLen == 0) : false
         * (i < buffer.length) : true
         * (delimiters == null) : true  #  inside isDelimiter method
         * (delimiters == null ? Character.isWhitespace(ch) : ArrayUtils.contains(delimiters, ch)) : false  #  inside isDelimiter method
         * (isDelimiter(ch, delimiters)) : false
         * (capitalizeNext) : true
         */
         //Arrange Statement(s)
        char[] _char = null;
        
        //Act Statement(s)
        String result = WordUtils.capitalize("B", _char);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("B")));
    }

    //BaseRock generated method id: ${capitalize1WhenIsDelimiterNotChDelimitersAndCapitalizeNext}, hash: 7AC798CB1377EF78EB9A34B2EA22DBA2
    @Test()
    void capitalize1WhenIsDelimiterNotChDelimitersAndCapitalizeNext() {
        /* Branches:
         * (delimiters == null) : false
         * (StringUtils.isEmpty(str)) : false
         * (delimLen == 0) : false
         * (i < buffer.length) : true
         * (delimiters == null) : false  #  inside isDelimiter method
         * (delimiters == null ? Character.isWhitespace(ch) : ArrayUtils.contains(delimiters, ch)) : false  #  inside isDelimiter method
         * (isDelimiter(ch, delimiters)) : false
         * (capitalizeNext) : true
         */
         //Arrange Statement(s)
        char[] charArray = new char[] { 'A' };
        
        //Act Statement(s)
        String result = WordUtils.capitalize("B", charArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("B")));
    }

    //BaseRock generated method id: ${capitalizeFullyTest}, hash: 731832B49369A679EAE0CA7C580F696E
    @Test()
    void capitalizeFullyTest() {
        //Arrange Statement(s)
        try (MockedStatic<WordUtils> wordUtils = mockStatic(WordUtils.class, CALLS_REAL_METHODS)) {
            wordUtils.when(() -> WordUtils.capitalizeFully("str1", (char[]) null)).thenReturn("return_of_capitalizeFully1");
            //Act Statement(s)
            String result = WordUtils.capitalizeFully("str1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_capitalizeFully1"));
                wordUtils.verify(() -> WordUtils.capitalizeFully("str1", (char[]) null), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${capitalizeFully1WhenDelimLenNotEquals0}, hash: 46957C20ACF400D3742639CA9EC9C35A
    @Test()
    void capitalizeFully1WhenDelimLenNotEquals0() {
        /* Branches:
         * (delimiters == null) : true
         * (StringUtils.isEmpty(str)) : false
         * (delimLen == 0) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<WordUtils> wordUtils = mockStatic(WordUtils.class, CALLS_REAL_METHODS)) {
            wordUtils.when(() -> WordUtils.capitalize("a", (char[]) null)).thenReturn("return_of_capitalize1");
            char[] _char = null;
            //Act Statement(s)
            String result = WordUtils.capitalizeFully("A", _char);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_capitalize1"));
                wordUtils.verify(() -> WordUtils.capitalize("a", (char[]) null), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${capitalizeFully1WhenDelimLenEquals0}, hash: 9F124CD3A041A67688E9C56FE2BC5F7D
    @Test()
    void capitalizeFully1WhenDelimLenEquals0() {
        /* Branches:
         * (delimiters == null) : false
         * (StringUtils.isEmpty(str)) : false
         * (delimLen == 0) : true
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        
        //Act Statement(s)
        String result = WordUtils.capitalizeFully("A", charArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${containsAllWordsWhenArrayUtilsIsEmptyWords}, hash: 0F13C0890ECE79CBCB82C934D5553EAD
    @Test()
    void containsAllWordsWhenArrayUtilsIsEmptyWords() {
        /* Branches:
         * (StringUtils.isEmpty(word)) : false
         * (ArrayUtils.isEmpty(words)) : true
         */
         //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] {};
        
        //Act Statement(s)
        boolean result = WordUtils.containsAllWords("word1", charSequenceArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${containsAllWordsWhenStringUtilsIsBlankW}, hash: 2F62B3EDC2D6D4B6B819D33E560BAB3E
    @Test()
    void containsAllWordsWhenStringUtilsIsBlankW() {
        /* Branches:
         * (StringUtils.isEmpty(word)) : false
         * (ArrayUtils.isEmpty(words)) : false
         * (for-each(words)) : true
         * (StringUtils.isBlank(w)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] { "charSequence1" };
        
        //Act Statement(s)
        boolean result = WordUtils.containsAllWords("word1", charSequenceArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${containsAllWordsWhenPMatcherWordNotMatches}, hash: 723C01F4400A452B33E028975B888DF9
    @Test()
    void containsAllWordsWhenPMatcherWordNotMatches() {
        /* Branches:
         * (StringUtils.isEmpty(word)) : false
         * (ArrayUtils.isEmpty(words)) : false
         * (for-each(words)) : true
         * (StringUtils.isBlank(w)) : false
         * (!p.matcher(word).matches()) : true
         */
         //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] { "charSequence1" };
        
        //Act Statement(s)
        boolean result = WordUtils.containsAllWords("word1", charSequenceArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${containsAllWordsWhenPMatcherWordMatches}, hash: 357D4F8C9D8E82F7453158EDF8379917
    @Test()
    void containsAllWordsWhenPMatcherWordMatches() {
        /* Branches:
         * (StringUtils.isEmpty(word)) : false
         * (ArrayUtils.isEmpty(words)) : false
         * (for-each(words)) : true
         * (StringUtils.isBlank(w)) : false
         * (!p.matcher(word).matches()) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] { "charSequence1" };
        
        //Act Statement(s)
        boolean result = WordUtils.containsAllWords("word1", charSequenceArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${initialsTest}, hash: 01E789E30866F350BDF1CCB4EA7C4BEA
    @Test()
    void initialsTest() {
        //Arrange Statement(s)
        try (MockedStatic<WordUtils> wordUtils = mockStatic(WordUtils.class, CALLS_REAL_METHODS)) {
            wordUtils.when(() -> WordUtils.initials("str1", (char[]) null)).thenReturn("return_of_initials1");
            //Act Statement(s)
            String result = WordUtils.initials("str1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_initials1"));
                wordUtils.verify(() -> WordUtils.initials("str1", (char[]) null), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${initials1WhenStringUtilsIsEmptyStr}, hash: E8F76E49971A52EFA06A3BC75E0CE69E
    @Test()
    void initials1WhenStringUtilsIsEmptyStr() {
        /* Branches:
         * (StringUtils.isEmpty(str)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        
        //Act Statement(s)
        String result = WordUtils.initials("str1", charArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("str1")));
    }

    //BaseRock generated method id: ${initials1WhenDelimitersLengthEquals0}, hash: 9D6963FC228A38C190F01574026C5B5A
    @Test()
    void initials1WhenDelimitersLengthEquals0() {
        /* Branches:
         * (StringUtils.isEmpty(str)) : false
         * (delimiters != null) : true
         * (delimiters.length == 0) : true
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        
        //Act Statement(s)
        String result = WordUtils.initials("A", charArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${initials1WhenIsDelimiterChDelimiters}, hash: E4C2C80D77A222D53D60B43BBD409613
    @Test()
    void initials1WhenIsDelimiterChDelimiters() {
        /* Branches:
         * (StringUtils.isEmpty(str)) : false
         * (delimiters != null) : true
         * (delimiters.length == 0) : false
         * (i < strLen) : true
         * (delimiters == null) : false  #  inside isDelimiter method
         * (delimiters == null ? Character.isWhitespace(ch) : ArrayUtils.contains(delimiters, ch)) : true  #  inside isDelimiter method
         * (isDelimiter(ch, delimiters)) : true
         */
         //Arrange Statement(s)
        char[] charArray = new char[] { 'A' };
        
        //Act Statement(s)
        String result = WordUtils.initials("A", charArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${initials1WhenLastWasGap}, hash: 5F85996C635CA173C3DDF31591321EC1
    @Test()
    void initials1WhenLastWasGap() {
        /* Branches:
         * (StringUtils.isEmpty(str)) : false
         * (delimiters != null) : false
         * (i < strLen) : true
         * (delimiters == null) : true  #  inside isDelimiter method
         * (delimiters == null ? Character.isWhitespace(ch) : ArrayUtils.contains(delimiters, ch)) : false  #  inside isDelimiter method
         * (isDelimiter(ch, delimiters)) : false
         * (lastWasGap) : true
         */
         //Arrange Statement(s)
        char[] _char = null;
        
        //Act Statement(s)
        String result = WordUtils.initials("A", _char);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${initials1WhenIsDelimiterNotChDelimitersAndLastWasGap}, hash: 00C19FB1F7AF0C58B0E521E2C14F483C
    @Test()
    void initials1WhenIsDelimiterNotChDelimitersAndLastWasGap() {
        /* Branches:
         * (StringUtils.isEmpty(str)) : false
         * (delimiters != null) : true
         * (delimiters.length == 0) : false
         * (i < strLen) : true
         * (delimiters == null) : false  #  inside isDelimiter method
         * (delimiters == null ? Character.isWhitespace(ch) : ArrayUtils.contains(delimiters, ch)) : false  #  inside isDelimiter method
         * (isDelimiter(ch, delimiters)) : false
         * (lastWasGap) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        char[] charArray = new char[] { 'A' };
        
        //Act Statement(s)
        String result = WordUtils.initials("A", charArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${swapCaseWhenStringUtilsIsEmptyStr}, hash: 2A60A36220674F4415FDBF0AEFFCAFA4
    @Test()
    void swapCaseWhenStringUtilsIsEmptyStr() {
        /* Branches:
         * (StringUtils.isEmpty(str)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        String result = WordUtils.swapCase("str1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("str1")));
    }

    //BaseRock generated method id: ${swapCaseWhenCharacterIsTitleCaseCh}, hash: 2C8891009F63CAF4F86CA792CFCE5F9A
    @Test()
    void swapCaseWhenCharacterIsTitleCaseCh() {
        /* Branches:
         * (StringUtils.isEmpty(str)) : false
         * (i < buffer.length) : true
         * (Character.isUpperCase(ch)) : false
         * (Character.isTitleCase(ch)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        String result = WordUtils.swapCase("str1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${uncapitalizeTest}, hash: 4EBD2E647A479B9B60DA54EAF6D050B6
    @Test()
    void uncapitalizeTest() {
        //Arrange Statement(s)
        try (MockedStatic<WordUtils> wordUtils = mockStatic(WordUtils.class, CALLS_REAL_METHODS)) {
            wordUtils.when(() -> WordUtils.uncapitalize("str1", (char[]) null)).thenReturn("return_of_uncapitalize1");
            //Act Statement(s)
            String result = WordUtils.uncapitalize("str1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_uncapitalize1"));
                wordUtils.verify(() -> WordUtils.uncapitalize("str1", (char[]) null), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${uncapitalize1WhenStringUtilsIsEmptyStr}, hash: 1F582A68D6FDDB31944E313B3A91CA4B
    @Test()
    void uncapitalize1WhenStringUtilsIsEmptyStr() {
        /* Branches:
         * (delimiters == null) : true
         * (StringUtils.isEmpty(str)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        char[] _char = null;
        
        //Act Statement(s)
        String result = WordUtils.uncapitalize("str1", _char);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("str1")));
    }

    //BaseRock generated method id: ${uncapitalize1WhenIsDelimiterChDelimiters}, hash: B22D9F1C98CC1E369F4B42A9993EDD64
    @Test()
    void uncapitalize1WhenIsDelimiterChDelimiters() {
        /* Branches:
         * (delimiters == null) : true
         * (StringUtils.isEmpty(str)) : false
         * (delimLen == 0) : false
         * (i < buffer.length) : true
         * (delimiters == null) : true  #  inside isDelimiter method
         * (delimiters == null ? Character.isWhitespace(ch) : ArrayUtils.contains(delimiters, ch)) : true  #  inside isDelimiter method
         * (isDelimiter(ch, delimiters)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        char[] _char = null;
        
        //Act Statement(s)
        String result = WordUtils.uncapitalize("str1", _char);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${uncapitalize1WhenUncapitalizeNext}, hash: 860917524D234107A1DEAB2E95F7F4DD
    @Test()
    void uncapitalize1WhenUncapitalizeNext() {
        /* Branches:
         * (delimiters == null) : true
         * (StringUtils.isEmpty(str)) : false
         * (delimLen == 0) : false
         * (i < buffer.length) : true
         * (delimiters == null) : true  #  inside isDelimiter method
         * (delimiters == null ? Character.isWhitespace(ch) : ArrayUtils.contains(delimiters, ch)) : false  #  inside isDelimiter method
         * (isDelimiter(ch, delimiters)) : false
         * (uncapitalizeNext) : true
         */
         //Arrange Statement(s)
        char[] _char = null;
        
        //Act Statement(s)
        String result = WordUtils.uncapitalize("B", _char);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("b")));
    }

    //BaseRock generated method id: ${uncapitalize1WhenIsDelimiterNotChDelimitersAndUncapitalizeNext}, hash: AD438A5119BFC004303F865D440C1A5A
    @Test()
    void uncapitalize1WhenIsDelimiterNotChDelimitersAndUncapitalizeNext() {
        /* Branches:
         * (delimiters == null) : false
         * (StringUtils.isEmpty(str)) : false
         * (delimLen == 0) : false
         * (i < buffer.length) : true
         * (delimiters == null) : false  #  inside isDelimiter method
         * (delimiters == null ? Character.isWhitespace(ch) : ArrayUtils.contains(delimiters, ch)) : false  #  inside isDelimiter method
         * (isDelimiter(ch, delimiters)) : false
         * (uncapitalizeNext) : true
         */
         //Arrange Statement(s)
        char[] charArray = new char[] { 'A' };
        
        //Act Statement(s)
        String result = WordUtils.uncapitalize("B", charArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("b")));
    }

    //BaseRock generated method id: ${wrapTest}, hash: 5CD1D58B4580B5F28AB3570ADC2362D1
    @Test()
    void wrapTest() {
        //Arrange Statement(s)
        try (MockedStatic<WordUtils> wordUtils = mockStatic(WordUtils.class, CALLS_REAL_METHODS)) {
            wordUtils.when(() -> WordUtils.wrap("str1", 0, (String) null, false)).thenReturn("return_of_wrap1");
            //Act Statement(s)
            String result = WordUtils.wrap("str1", 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_wrap1"));
                wordUtils.verify(() -> WordUtils.wrap("str1", 0, (String) null, false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${wrap1Test}, hash: F099D3609A3FCC85E010D056E0B60BBC
    @Test()
    void wrap1Test() {
        //Arrange Statement(s)
        try (MockedStatic<WordUtils> wordUtils = mockStatic(WordUtils.class, CALLS_REAL_METHODS)) {
            wordUtils.when(() -> WordUtils.wrap("str1", 0, "newLineStr1", false, " ")).thenReturn("return_of_wrap1");
            //Act Statement(s)
            String result = WordUtils.wrap("str1", 0, "newLineStr1", false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_wrap1"));
                wordUtils.verify(() -> WordUtils.wrap("str1", 0, "newLineStr1", false, " "), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${wrap2WhenStrIsNull}, hash: 49194959F24C799FE0A8AC8CD22070A8
    @Test()
    void wrap2WhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
         
        //Act Statement(s)
        String result = WordUtils.wrap((String) null, 0, "newLineStr1", false, "wrapOn1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${wrap2WhenSpaceToWrapAtGreaterThanOrEqualsToOffset}, hash: 2B1B177DC0B81EAE2B8CCDFD69981433
    @Test()
    void wrap2WhenSpaceToWrapAtGreaterThanOrEqualsToOffset() {
        /* Branches:
         * (str == null) : false
         * (newLineStr == null) : true
         * (wrapLength < 1) : true
         * (StringUtils.isBlank(wrapOn)) : true
         * (offset < inputLineLength) : true
         * (matcher.find()) : true
         * (matcher.start() == 0) : false
         * (inputLineLength - offset <= wrapLength) : false
         * (matcher.find()) : true
         * (spaceToWrapAt >= offset) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        String result = WordUtils.wrap("str1", 0, (String) null, false, "wrapOn1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("\n")));
    }

    //BaseRock generated method id: ${wrap2WhenWrapLongWordsAndMatcherStartEquals0}, hash: BF4CD7246612DB1F406D082CDD153B3B
    @Test()
    void wrap2WhenWrapLongWordsAndMatcherStartEquals0() {
        /* Branches:
         * (str == null) : false
         * (newLineStr == null) : true
         * (wrapLength < 1) : true
         * (StringUtils.isBlank(wrapOn)) : true
         * (offset < inputLineLength) : true
         * (matcher.find()) : false
         * (inputLineLength - offset <= wrapLength) : false
         * (matcher.find()) : false
         * (spaceToWrapAt >= offset) : false
         * (wrapLongWords) : true
         * (matcher.start() == 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        String result = WordUtils.wrap("str1", 0, (String) null, false, "wrapOn1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("\n")));
    }

    //BaseRock generated method id: ${wrap2WhenSpaceToWrapAtGreaterThanOrEqualsTo0}, hash: 35D0985FD6387D692CA3A9952A32C6DE
    @Test()
    void wrap2WhenSpaceToWrapAtGreaterThanOrEqualsTo0() {
        /* Branches:
         * (str == null) : false
         * (newLineStr == null) : true
         * (wrapLength < 1) : true
         * (StringUtils.isBlank(wrapOn)) : true
         * (offset < inputLineLength) : true
         * (matcher.find()) : true
         * (matcher.start() == 0) : false
         * (inputLineLength - offset <= wrapLength) : false
         * (matcher.find()) : true
         * (spaceToWrapAt >= offset) : false
         * (wrapLongWords) : false
         * (matcher.find()) : true
         * (spaceToWrapAt >= 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        String result = WordUtils.wrap("str1", 0, (String) null, false, "wrapOn1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("\n")));
    }

    //BaseRock generated method id: ${wrap2WhenSpaceToWrapAtLessThan0}, hash: A7F999811183207655216419C55653A2
    @Test()
    void wrap2WhenSpaceToWrapAtLessThan0() {
        /* Branches:
         * (str == null) : false
         * (newLineStr == null) : true
         * (wrapLength < 1) : true
         * (StringUtils.isBlank(wrapOn)) : true
         * (offset < inputLineLength) : true
         * (matcher.find()) : true
         * (matcher.start() == 0) : false
         * (inputLineLength - offset <= wrapLength) : false
         * (matcher.find()) : true
         * (spaceToWrapAt >= offset) : false
         * (wrapLongWords) : false
         * (matcher.find()) : true
         * (spaceToWrapAt >= 0) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        String result = WordUtils.wrap("str1", 0, (String) null, false, "wrapOn1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }
}
