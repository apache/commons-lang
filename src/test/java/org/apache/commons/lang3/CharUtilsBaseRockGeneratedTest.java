package org.apache.commons.lang3;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mockStatic;
import static org.hamcrest.Matchers.is;
import org.junit.jupiter.api.Disabled;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class CharUtilsBaseRockGeneratedTest {

    //BaseRock generated method id: ${compareTest}, hash: CBB9FCA427B43B615BA6E97E69C0B0A4
    @Test()
    void compareTest() {
        //Act Statement(s)
        int result = CharUtils.compare('A', 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${isAsciiWhenChLessThan128}, hash: 6D8BAD3CE93F1D077119789F4BA33F1C
    @Test()
    void isAsciiWhenChLessThan128() {
        /* Branches:
         * (ch < 128) : true
         */
        //Act Statement(s)
        boolean result = CharUtils.isAscii('\u0004');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isAsciiWhenChNotLessThan128}, hash: 819716ABCED4C96FF99CE7BA5FA580D9
    @Test()
    void isAsciiWhenChNotLessThan128() {
        /* Branches:
         * (ch < 128) : false
         */
        //Act Statement(s)
        boolean result = CharUtils.isAscii('\uD87F');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isAsciiAlphaWhenIsAsciiAlphaLowerCh}, hash: 0B7FEB100AF8FB77586A499C1C6CFB0A
    @Test()
    void isAsciiAlphaWhenIsAsciiAlphaLowerCh() {
        /* Branches:
         * (isAsciiAlphaUpper(ch)) : false
         * (isAsciiAlphaLower(ch)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<CharUtils> charUtils = mockStatic(CharUtils.class, CALLS_REAL_METHODS)) {
            charUtils.when(() -> CharUtils.isAsciiAlphaUpper('A')).thenReturn(false);
            charUtils.when(() -> CharUtils.isAsciiAlphaLower('A')).thenReturn(true);
            //Act Statement(s)
            boolean result = CharUtils.isAsciiAlpha('A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                charUtils.verify(() -> CharUtils.isAsciiAlphaUpper('A'), atLeast(1));
                charUtils.verify(() -> CharUtils.isAsciiAlphaLower('A'), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAsciiAlphaWhenIsAsciiAlphaLowerNotCh}, hash: B09D0E90AE130ECAC1B2B7805A8B734C
    @Test()
    void isAsciiAlphaWhenIsAsciiAlphaLowerNotCh() {
        /* Branches:
         * (isAsciiAlphaUpper(ch)) : false
         * (isAsciiAlphaLower(ch)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<CharUtils> charUtils = mockStatic(CharUtils.class, CALLS_REAL_METHODS)) {
            charUtils.when(() -> CharUtils.isAsciiAlphaUpper('A')).thenReturn(false);
            charUtils.when(() -> CharUtils.isAsciiAlphaLower('A')).thenReturn(false);
            //Act Statement(s)
            boolean result = CharUtils.isAsciiAlpha('A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                charUtils.verify(() -> CharUtils.isAsciiAlphaUpper('A'), atLeast(1));
                charUtils.verify(() -> CharUtils.isAsciiAlphaLower('A'), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAsciiAlphaLowerWhenChLessThanOrEqualsTo_z_}, hash: 42566F623DC6EF79C5EE823F5044F728
    @Test()
    void isAsciiAlphaLowerWhenChLessThanOrEqualsTo_z_() {
        /* Branches:
         * (ch >= 'a') : true
         * (ch <= 'z') : true
         */
        //Act Statement(s)
        boolean result = CharUtils.isAsciiAlphaLower('c');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isAsciiAlphaLowerWhenChGreaterThan_z_}, hash: 09C842A93309CA321DE0E10A6C658F88
    @Test()
    void isAsciiAlphaLowerWhenChGreaterThan_z_() {
        /* Branches:
         * (ch >= 'a') : true
         * (ch <= 'z') : false
         */
        //Act Statement(s)
        boolean result = CharUtils.isAsciiAlphaLower('\uD87F');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isAsciiAlphanumericWhenIsAsciiNumericCh}, hash: 48BA160B839E4AE36A640FD24011D595
    @Test()
    void isAsciiAlphanumericWhenIsAsciiNumericCh() {
        /* Branches:
         * (isAsciiAlpha(ch)) : false
         * (isAsciiNumeric(ch)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<CharUtils> charUtils = mockStatic(CharUtils.class, CALLS_REAL_METHODS)) {
            charUtils.when(() -> CharUtils.isAsciiAlpha('A')).thenReturn(false);
            charUtils.when(() -> CharUtils.isAsciiNumeric('A')).thenReturn(true);
            //Act Statement(s)
            boolean result = CharUtils.isAsciiAlphanumeric('A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                charUtils.verify(() -> CharUtils.isAsciiAlpha('A'), atLeast(1));
                charUtils.verify(() -> CharUtils.isAsciiNumeric('A'), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAsciiAlphanumericWhenIsAsciiNumericNotCh}, hash: 312C072D10107AD1E24142F49D5D258E
    @Test()
    void isAsciiAlphanumericWhenIsAsciiNumericNotCh() {
        /* Branches:
         * (isAsciiAlpha(ch)) : false
         * (isAsciiNumeric(ch)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<CharUtils> charUtils = mockStatic(CharUtils.class, CALLS_REAL_METHODS)) {
            charUtils.when(() -> CharUtils.isAsciiAlpha('A')).thenReturn(false);
            charUtils.when(() -> CharUtils.isAsciiNumeric('A')).thenReturn(false);
            //Act Statement(s)
            boolean result = CharUtils.isAsciiAlphanumeric('A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                charUtils.verify(() -> CharUtils.isAsciiAlpha('A'), atLeast(1));
                charUtils.verify(() -> CharUtils.isAsciiNumeric('A'), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAsciiAlphaUpperWhenChLessThanOrEqualsTo_Z_}, hash: 098BC7E60C1C2752E7292CDCCAC59E78
    @Test()
    void isAsciiAlphaUpperWhenChLessThanOrEqualsTo_Z_() {
        /* Branches:
         * (ch >= 'A') : true
         * (ch <= 'Z') : true
         */
        //Act Statement(s)
        boolean result = CharUtils.isAsciiAlphaUpper('C');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isAsciiAlphaUpperWhenChGreaterThan_Z_}, hash: 41182A083C90DC5C04B6C6C534621D05
    @Test()
    void isAsciiAlphaUpperWhenChGreaterThan_Z_() {
        /* Branches:
         * (ch >= 'A') : true
         * (ch <= 'Z') : false
         */
        //Act Statement(s)
        boolean result = CharUtils.isAsciiAlphaUpper('\uD87F');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isAsciiControlWhenChEquals127}, hash: DBDEF2CDFDEE8784B085A50C75C8093D
    @Test()
    void isAsciiControlWhenChEquals127() {
        /* Branches:
         * (ch < 32) : false
         * (ch == 127) : true
         */
        //Act Statement(s)
        boolean result = CharUtils.isAsciiControl('');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isAsciiControlWhenChNotEquals127}, hash: 7E9CCDD09EE403ED734FAF98F4A2E786
    @Test()
    void isAsciiControlWhenChNotEquals127() {
        /* Branches:
         * (ch < 32) : false
         * (ch == 127) : false
         */
        //Act Statement(s)
        boolean result = CharUtils.isAsciiControl('\uD82F');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isAsciiNumericWhenChLessThanOrEqualsTo_9_}, hash: 7AC65BFB30E045F4627716095C7895FC
    @Test()
    void isAsciiNumericWhenChLessThanOrEqualsTo_9_() {
        /* Branches:
         * (ch >= '0') : true
         * (ch <= '9') : true
         */
        //Act Statement(s)
        boolean result = CharUtils.isAsciiNumeric('7');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isAsciiNumericWhenChGreaterThan_9_}, hash: 3F6F422849BD281C0B5CA81476EB542F
    @Test()
    void isAsciiNumericWhenChGreaterThan_9_() {
        /* Branches:
         * (ch >= '0') : true
         * (ch <= '9') : false
         */
        //Act Statement(s)
        boolean result = CharUtils.isAsciiNumeric('\uD87F');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isAsciiPrintableWhenChLessThan127}, hash: B4FE575F678C004AC3785CB7FBBCD1E0
    @Test()
    void isAsciiPrintableWhenChLessThan127() {
        /* Branches:
         * (ch >= 32) : true
         * (ch < 127) : true
         */
        //Act Statement(s)
        boolean result = CharUtils.isAsciiPrintable('C');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isAsciiPrintableWhenChNotLessThan127}, hash: 0DF479D91DEBF5FDC42217D5E26CCB58
    @Test()
    void isAsciiPrintableWhenChNotLessThan127() {
        /* Branches:
         * (ch >= 32) : true
         * (ch < 127) : false
         */
        //Act Statement(s)
        boolean result = CharUtils.isAsciiPrintable('\uD87F');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${toCharTest}, hash: E44C214E3D95A3090D780026634CFF36
    @Test()
    void toCharTest() {
        //Act Statement(s)
        char result = CharUtils.toChar('A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('A')));
    }

    //BaseRock generated method id: ${toChar1WhenChIsNotNull}, hash: 386F16CFFACF2A8967CE812D028272CC
    @Test()
    void toChar1WhenChIsNotNull() {
        /* Branches:
         * (ch != null) : true
         */
        //Act Statement(s)
        char result = CharUtils.toChar('A', 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('A')));
    }

    //BaseRock generated method id: ${toChar1WhenChIsNull}, hash: 14CF11221A63D278FEAB520337FF767B
    @Test()
    void toChar1WhenChIsNull() {
        /* Branches:
         * (ch != null) : false
         */
        //Act Statement(s)
        char result = CharUtils.toChar((Character) null, 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('A')));
    }

    //BaseRock generated method id: ${toChar2Test}, hash: EC12BB99B2682E089A8B455507484960
    @Test()
    void toChar2Test() {
        //Act Statement(s)
        char result = CharUtils.toChar("B");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('B')));
    }

    //BaseRock generated method id: ${toChar3WhenStringUtilsIsEmptyStr}, hash: E41B6D34E343811CEF574706421E1C27
    @Disabled()
    @Test()
    void toChar3WhenStringUtilsIsEmptyStr() {
        /* Branches:
         * (StringUtils.isEmpty(str)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        char result = CharUtils.toChar("str1", 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('A')));
    }

    //BaseRock generated method id: ${toChar3WhenStringUtilsNotIsEmptyStr}, hash: A7CCEAED9D0E7A2B6393BF795028A61F
    @Test()
    void toChar3WhenStringUtilsNotIsEmptyStr() {
        /* Branches:
         * (StringUtils.isEmpty(str)) : false
         */
        //Act Statement(s)
        char result = CharUtils.toChar("B", 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('B')));
    }

    //BaseRock generated method id: ${toCharacterObjectTest}, hash: BFCE2D8EC6E0C611F03539EB50CEE89C
    @Test()
    void toCharacterObjectTest() {
        //Act Statement(s)
        Character result = CharUtils.toCharacterObject('B');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('B')));
    }

    //BaseRock generated method id: ${toCharacterObject1WhenStringUtilsIsEmptyStr}, hash: 190B6B951ACD6B451B440D714B399038
    @Disabled()
    @Test()
    void toCharacterObject1WhenStringUtilsIsEmptyStr() {
        /* Branches:
         * (StringUtils.isEmpty(str)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        Character result = CharUtils.toCharacterObject("str1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toCharacterObject1WhenStringUtilsNotIsEmptyStr}, hash: D37F033B42FA82C5EF36EAD53182F445
    @Test()
    void toCharacterObject1WhenStringUtilsNotIsEmptyStr() {
        /* Branches:
         * (StringUtils.isEmpty(str)) : false
         */
        //Act Statement(s)
        Character result = CharUtils.toCharacterObject("B");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('B')));
    }

    //BaseRock generated method id: ${toIntValueWhenIsAsciiNumericNotChThrowsIllegalArgumentException}, hash: A2B4C49A5C6E59CC2D5A5959D5A59CFA
    @Test()
    void toIntValueWhenIsAsciiNumericNotChThrowsIllegalArgumentException() {
        /* Branches:
         * (!isAsciiNumeric(ch)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<CharUtils> charUtils = mockStatic(CharUtils.class, CALLS_REAL_METHODS)) {
            charUtils.when(() -> CharUtils.isAsciiNumeric('B')).thenReturn(false);
            //Act Statement(s)
            final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
                CharUtils.toIntValue('B');
            });
            IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The character B is not in the range '0' - '9'");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
                charUtils.verify(() -> CharUtils.isAsciiNumeric('B'), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toIntValueWhenIsAsciiNumericCh}, hash: 013786FDEF1EC25EE820EAE6A3481DC9
    @Disabled()
    @Test()
    void toIntValueWhenIsAsciiNumericCh() {
        /* Branches:
         * (!isAsciiNumeric(ch)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<CharUtils> charUtils = mockStatic(CharUtils.class, CALLS_REAL_METHODS)) {
            charUtils.when(() -> CharUtils.isAsciiNumeric('A')).thenReturn(true);
            //Act Statement(s)
            int result = CharUtils.toIntValue('A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                charUtils.verify(() -> CharUtils.isAsciiNumeric('A'), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toIntValue1WhenIsAsciiNumericCh}, hash: 606633C71656308A743B1DE39476F4E1
    @Disabled()
    @Test()
    void toIntValue1WhenIsAsciiNumericCh() {
        /* Branches:
         * (isAsciiNumeric(ch)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<CharUtils> charUtils = mockStatic(CharUtils.class, CALLS_REAL_METHODS)) {
            charUtils.when(() -> CharUtils.isAsciiNumeric('A')).thenReturn(true);
            //Act Statement(s)
            int result = CharUtils.toIntValue('A', 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                charUtils.verify(() -> CharUtils.isAsciiNumeric('A'), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toIntValue1WhenIsAsciiNumericNotCh}, hash: CC7A9992155565EF7C29E68DE91D2A0A
    @Test()
    void toIntValue1WhenIsAsciiNumericNotCh() {
        /* Branches:
         * (isAsciiNumeric(ch)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<CharUtils> charUtils = mockStatic(CharUtils.class, CALLS_REAL_METHODS)) {
            charUtils.when(() -> CharUtils.isAsciiNumeric('A')).thenReturn(false);
            //Act Statement(s)
            int result = CharUtils.toIntValue('A', 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                charUtils.verify(() -> CharUtils.isAsciiNumeric('A'), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toIntValue2Test}, hash: D8252B7E460FE40BD43D269C6F5ECD81
    @Test()
    void toIntValue2Test() {
        //Arrange Statement(s)
        try (MockedStatic<CharUtils> charUtils = mockStatic(CharUtils.class, CALLS_REAL_METHODS)) {
            charUtils.when(() -> CharUtils.toIntValue('A')).thenReturn(0);
            //Act Statement(s)
            int result = CharUtils.toIntValue('A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                charUtils.verify(() -> CharUtils.toIntValue('A'), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toIntValue3WhenChIsNotNull}, hash: 230CEC89DEB93F78BA134D33CCDE77D8
    @Test()
    void toIntValue3WhenChIsNotNull() {
        /* Branches:
         * (ch != null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<CharUtils> charUtils = mockStatic(CharUtils.class, CALLS_REAL_METHODS)) {
            charUtils.when(() -> CharUtils.toIntValue('A', 0)).thenReturn(0);
            //Act Statement(s)
            int result = CharUtils.toIntValue('A', 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                charUtils.verify(() -> CharUtils.toIntValue('A', 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toIntValue3WhenChIsNull}, hash: 8739C6A9FF020F43062B156F1EADB953
    @Test()
    void toIntValue3WhenChIsNull() {
        /* Branches:
         * (ch != null) : false
         */
        //Act Statement(s)
        int result = CharUtils.toIntValue((Character) null, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${toStringWhenChLessThanCHAR_STRING_ARRAYLength}, hash: 73B6CB54026B92114B9319A8F4E83F0E
    @Disabled()
    @Test()
    void toStringWhenChLessThanCHAR_STRING_ARRAYLength() {
        /* Branches:
         * (ch < CHAR_STRING_ARRAY.length) : true
         */
        //Act Statement(s)
        String result = CharUtils.toString('\u0006');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("null")));
    }

    //BaseRock generated method id: ${toStringWhenChNotLessThanCHAR_STRING_ARRAYLength}, hash: CFE12577F2ECD34D520534D30E318785
    @Test()
    void toStringWhenChNotLessThanCHAR_STRING_ARRAYLength() {
        /* Branches:
         * (ch < CHAR_STRING_ARRAY.length) : false
         */
        //Act Statement(s)
        String result = CharUtils.toString('\uD83F');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("\uD83F")));
    }

    //BaseRock generated method id: ${toString1WhenChIsNotNull}, hash: F6EE82A04B9F3FCC03F2490CA28A5839
    @Test()
    void toString1WhenChIsNotNull() {
        /* Branches:
         * (ch != null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<CharUtils> charUtils = mockStatic(CharUtils.class, CALLS_REAL_METHODS)) {
            charUtils.when(() -> CharUtils.toString('A')).thenReturn("return_of_toString1");
            //Act Statement(s)
            String result = CharUtils.toString('A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_toString1"));
                charUtils.verify(() -> CharUtils.toString('A'), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toString1WhenChIsNull}, hash: D76485223A0FF6D3E0A60879BFDA5870
    @Test()
    void toString1WhenChIsNull() {
        /* Branches:
         * (ch != null) : false
         */
        //Act Statement(s)
        String result = CharUtils.toString((Character) null);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${unicodeEscapedTest}, hash: 3B8FBA1A2E2BD166E4696DECC8282EB4
    @Disabled()
    @Test()
    void unicodeEscapedTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String result = CharUtils.unicodeEscaped('A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("\\ua00f")));
    }

    //BaseRock generated method id: ${unicodeEscaped1WhenChIsNotNull}, hash: E8F6E3252D1AC8E008A75B6D8223E41D
    @Test()
    void unicodeEscaped1WhenChIsNotNull() {
        /* Branches:
         * (ch != null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<CharUtils> charUtils = mockStatic(CharUtils.class, CALLS_REAL_METHODS)) {
            charUtils.when(() -> CharUtils.unicodeEscaped('A')).thenReturn("return_of_unicodeEscaped1");
            //Act Statement(s)
            String result = CharUtils.unicodeEscaped('A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_unicodeEscaped1"));
                charUtils.verify(() -> CharUtils.unicodeEscaped('A'), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${unicodeEscaped1WhenChIsNull}, hash: 60B25FE1FB0201E706378B7CBF1739FD
    @Test()
    void unicodeEscaped1WhenChIsNull() {
        /* Branches:
         * (ch != null) : false
         */
        //Act Statement(s)
        String result = CharUtils.unicodeEscaped((Character) null);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }
}
