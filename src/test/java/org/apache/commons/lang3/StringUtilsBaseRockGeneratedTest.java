package org.apache.commons.lang3;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.io.UnsupportedEncodingException;
import java.util.List;
import java.util.Locale;
import java.util.Iterator;
import java.util.function.Supplier;
import java.nio.CharBuffer;
import org.mockito.MockedStatic;
import java.nio.charset.Charset;
import java.util.ArrayList;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.mock;
import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mockStatic;
import static org.hamcrest.Matchers.closeTo;
import org.junit.jupiter.api.Disabled;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class StringUtilsBaseRockGeneratedTest {

    private final Supplier<CharSequence> supplierMock = mock(Supplier.class);

    //BaseRock generated method id: ${abbreviateTest}, hash: 05BFF8B9D83605AB752C1CF73A945C47
    @Test()
    void abbreviateTest() {
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.abbreviate("str1", "...", 0, 0)).thenReturn("return_of_abbreviate1");
            //Act Statement(s)
            String result = StringUtils.abbreviate("str1", 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_abbreviate1"));
                stringUtils.verify(() -> StringUtils.abbreviate("str1", "...", 0, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${abbreviate1Test}, hash: 5F4FC3083C076BF30B69A19D3BC68AB7
    @Test()
    void abbreviate1Test() {
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.abbreviate("str1", "...", 0, 0)).thenReturn("return_of_abbreviate1");
            //Act Statement(s)
            String result = StringUtils.abbreviate("str1", 0, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_abbreviate1"));
                stringUtils.verify(() -> StringUtils.abbreviate("str1", "...", 0, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${abbreviate2Test}, hash: 0AE5FAC795FFB2297ECDB20015FBF304
    @Test()
    void abbreviate2Test() {
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.abbreviate("str1", "abbrevMarker1", 0, 0)).thenReturn("return_of_abbreviate1");
            //Act Statement(s)
            String result = StringUtils.abbreviate("str1", "abbrevMarker1", 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_abbreviate1"));
                stringUtils.verify(() -> StringUtils.abbreviate("str1", "abbrevMarker1", 0, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${abbreviate3WhenMaxWidthGreaterThan0}, hash: 0B59D51B46DA4C5867F96993DE979C03
    @Test()
    void abbreviate3WhenMaxWidthGreaterThan0() {
        /* Branches:
         * (isNotEmpty(str)) : true
         * (EMPTY.equals(abbrevMarker)) : true
         * (maxWidth > 0) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isNotEmpty("str1")).thenReturn(true);
            stringUtils.when(() -> StringUtils.substring("str1", 0, 1)).thenReturn("return_of_substring1");
            //Act Statement(s)
            String result = StringUtils.abbreviate("str1", "", 0, 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_substring1"));
                stringUtils.verify(() -> StringUtils.isNotEmpty("str1"), atLeast(1));
                stringUtils.verify(() -> StringUtils.substring("str1", 0, 1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${abbreviate3WhenMaxWidthNotGreaterThan0AndIsAnyEmptyStrAbbrevMarker}, hash: 7FE14AC97878D1044B0D3A4C30ACF606
    @Test()
    void abbreviate3WhenMaxWidthNotGreaterThan0AndIsAnyEmptyStrAbbrevMarker() {
        /* Branches:
         * (isNotEmpty(str)) : true
         * (EMPTY.equals(abbrevMarker)) : true
         * (maxWidth > 0) : false
         * (isAnyEmpty(str, abbrevMarker)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isNotEmpty("str1")).thenReturn(true);
            CharSequence[] charSequenceArray = new CharSequence[] { "str1", "" };
            stringUtils.when(() -> StringUtils.isAnyEmpty(charSequenceArray)).thenReturn(true);
            //Act Statement(s)
            String result = StringUtils.abbreviate("str1", "", 0, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("str1"));
                stringUtils.verify(() -> StringUtils.isNotEmpty("str1"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isAnyEmpty(charSequenceArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${abbreviate3WhenMaxWidthNotGreaterThan0AndIsAnyEmptyNotStrAbbrevMarkerAndMaxWidthLessThanMThrowsIllegalArgumentException}, hash: 0492043C11EA008E28629FF4AA0E27EF
    @Test()
    void abbreviate3WhenMaxWidthNotGreaterThan0AndIsAnyEmptyNotStrAbbrevMarkerAndMaxWidthLessThanMThrowsIllegalArgumentException() {
        /* Branches:
         * (isNotEmpty(str)) : true
         * (EMPTY.equals(abbrevMarker)) : true
         * (maxWidth > 0) : false
         * (isAnyEmpty(str, abbrevMarker)) : false
         * (maxWidth < minAbbrevWidth) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isNotEmpty("str1")).thenReturn(true);
            CharSequence[] charSequenceArray = new CharSequence[] { "str1", "" };
            stringUtils.when(() -> StringUtils.isAnyEmpty(charSequenceArray)).thenReturn(false);
            //Act Statement(s)
            final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
                StringUtils.abbreviate("str1", "", 0, 0);
            });
            IllegalArgumentException illegalArgumentException = new IllegalArgumentException("Minimum abbreviation width is 1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
                stringUtils.verify(() -> StringUtils.isNotEmpty("str1"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isAnyEmpty(charSequenceArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${abbreviate3WhenMaxWidthNotLessThanMinAbbrevWidthAndStrLenLessThanOrEqualsToMaxWidth}, hash: FEA2D026B82FAC5D702564652DFD610D
    @Test()
    void abbreviate3WhenMaxWidthNotLessThanMinAbbrevWidthAndStrLenLessThanOrEqualsToMaxWidth() {
        /* Branches:
         * (isNotEmpty(str)) : true
         * (EMPTY.equals(abbrevMarker)) : false
         * (isAnyEmpty(str, abbrevMarker)) : false
         * (maxWidth < minAbbrevWidth) : false
         * (strLen <= maxWidth) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isNotEmpty("A")).thenReturn(true);
            CharSequence[] charSequenceArray = new CharSequence[] { "A", "B" };
            stringUtils.when(() -> StringUtils.isAnyEmpty(charSequenceArray)).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.abbreviate("A", "B", 0, 2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("A"));
                stringUtils.verify(() -> StringUtils.isNotEmpty("A"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isAnyEmpty(charSequenceArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${abbreviate3WhenStrLenMinusOffsetLessThanMaxWidthMinusAbbrevMarkerLengthAndOffsetLessThanOrEqualsToAbbrevMarkerLengthPlu2}, hash: 77C08BCD6E391720FFCEE99BACB54019
    @Test()
    void abbreviate3WhenStrLenMinusOffsetLessThanMaxWidthMinusAbbrevMarkerLengthAndOffsetLessThanOrEqualsToAbbrevMarkerLengthPlu2() {
        /* Branches:
         * (isNotEmpty(str)) : true
         * (EMPTY.equals(abbrevMarker)) : false
         * (isAnyEmpty(str, abbrevMarker)) : false
         * (maxWidth < minAbbrevWidth) : false
         * (strLen <= maxWidth) : false
         * (offset > strLen) : true
         * (strLen - offset < maxWidth - abbrevMarkerLength) : true
         * (offset <= abbrevMarkerLength + 1) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isNotEmpty("CBD")).thenReturn(true);
            CharSequence[] charSequenceArray = new CharSequence[] { "CBD", "A" };
            stringUtils.when(() -> StringUtils.isAnyEmpty(charSequenceArray)).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.abbreviate("CBD", "A", 4, 2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("CA"));
                stringUtils.verify(() -> StringUtils.isNotEmpty("CBD"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isAnyEmpty(charSequenceArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${abbreviate3WhenOffsetGreaterThanAbbrevMarkerLengthPlus1AndMaxWidthLessThanMinAbbrevWidthO3ThrowsIllegalArgumentException}, hash: 3574AB8B29933106343257232A833737
    @Test()
    void abbreviate3WhenOffsetGreaterThanAbbrevMarkerLengthPlus1AndMaxWidthLessThanMinAbbrevWidthO3ThrowsIllegalArgumentException() {
        /* Branches:
         * (isNotEmpty(str)) : true
         * (EMPTY.equals(abbrevMarker)) : false
         * (isAnyEmpty(str, abbrevMarker)) : false
         * (maxWidth < minAbbrevWidth) : false
         * (strLen <= maxWidth) : false
         * (offset > strLen) : true
         * (strLen - offset < maxWidth - abbrevMarkerLength) : true
         * (offset <= abbrevMarkerLength + 1) : false
         * (maxWidth < minAbbrevWidthOffset) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isNotEmpty("CBDE")).thenReturn(true);
            CharSequence[] charSequenceArray = new CharSequence[] { "CBDE", "A" };
            stringUtils.when(() -> StringUtils.isAnyEmpty(charSequenceArray)).thenReturn(false);
            //Act Statement(s)
            final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
                StringUtils.abbreviate("CBDE", "A", 5, 2);
            });
            IllegalArgumentException illegalArgumentException = new IllegalArgumentException("Minimum abbreviation width with offset is 3");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
                stringUtils.verify(() -> StringUtils.isNotEmpty("CBDE"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isAnyEmpty(charSequenceArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${abbreviate3WhenMaxWidthNotLessThanMinAbbrevWidthOffsetAndOffsetPlusMaxWidthMinusAbbrevMarkerLengthNotLessThanStrLen2}, hash: 56D400391B3BA6A378244A67001A8DF5
    @Test()
    void abbreviate3WhenMaxWidthNotLessThanMinAbbrevWidthOffsetAndOffsetPlusMaxWidthMinusAbbrevMarkerLengthNotLessThanStrLen2() {
        /* Branches:
         * (isNotEmpty(str)) : true
         * (EMPTY.equals(abbrevMarker)) : false
         * (isAnyEmpty(str, abbrevMarker)) : false
         * (maxWidth < minAbbrevWidth) : false
         * (strLen <= maxWidth) : false
         * (offset > strLen) : true
         * (strLen - offset < maxWidth - abbrevMarkerLength) : true
         * (offset <= abbrevMarkerLength + 1) : false
         * (maxWidth < minAbbrevWidthOffset) : false
         * (offset + maxWidth - abbrevMarkerLength < strLen) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isNotEmpty("CBDEF")).thenReturn(true);
            CharSequence[] charSequenceArray = new CharSequence[] { "CBDEF", "A" };
            stringUtils.when(() -> StringUtils.isAnyEmpty(charSequenceArray)).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.abbreviate("CBDEF", "A", 6, 3);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("AEF"));
                stringUtils.verify(() -> StringUtils.isNotEmpty("CBDEF"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isAnyEmpty(charSequenceArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${abbreviate3WhenMaxWidthNotLessThanMinAbbrevWidthOffsetAndOffsetPlusMaxWidthMinusAbbrevMarkerLengthLessThanStrLen}, hash: 4B89E1359DB900FF465A8C0E42C42C44
    @Test()
    void abbreviate3WhenMaxWidthNotLessThanMinAbbrevWidthOffsetAndOffsetPlusMaxWidthMinusAbbrevMarkerLengthLessThanStrLen() {
        /* Branches:
         * (isNotEmpty(str)) : true
         * (EMPTY.equals(abbrevMarker)) : false
         * (isAnyEmpty(str, abbrevMarker)) : false
         * (maxWidth < minAbbrevWidth) : false
         * (strLen <= maxWidth) : false
         * (offset > strLen) : false
         * (strLen - offset < maxWidth - abbrevMarkerLength) : false
         * (offset <= abbrevMarkerLength + 1) : false
         * (maxWidth < minAbbrevWidthOffset) : false
         * (offset + maxWidth - abbrevMarkerLength < strLen) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isNotEmpty("CBDEFG")).thenReturn(true);
            CharSequence[] charSequenceArray = new CharSequence[] { "CBDEFG", "A" };
            stringUtils.when(() -> StringUtils.isAnyEmpty(charSequenceArray)).thenReturn(false);
            stringUtils.when(() -> StringUtils.abbreviate("EFG", "A", 2)).thenReturn("H");
            //Act Statement(s)
            String result = StringUtils.abbreviate("CBDEFG", "A", 3, 3);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("AH"));
                stringUtils.verify(() -> StringUtils.isNotEmpty("CBDEFG"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isAnyEmpty(charSequenceArray), atLeast(1));
                stringUtils.verify(() -> StringUtils.abbreviate("EFG", "A", 2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${abbreviateMiddleWhenLengthLessThanMiddleLengthPlus2}, hash: ABBC213BFBDE25EF7C63C6FF6877E9A9
    @Test()
    void abbreviateMiddleWhenLengthLessThanMiddleLengthPlus2() {
        /* Branches:
         * (isAnyEmpty(str, middle)) : false
         * (length >= str.length()) : false
         * (length < middle.length() + 2) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            CharSequence[] charSequenceArray = new CharSequence[] { "B", "A" };
            stringUtils.when(() -> StringUtils.isAnyEmpty(charSequenceArray)).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.abbreviateMiddle("B", "A", 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("B"));
                stringUtils.verify(() -> StringUtils.isAnyEmpty(charSequenceArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${abbreviateMiddleWhenLengthNotLessThanMiddleLengthPlus2}, hash: 1F18F416AD66D93C725FC20A72B481D2
    @Test()
    void abbreviateMiddleWhenLengthNotLessThanMiddleLengthPlus2() {
        /* Branches:
         * (isAnyEmpty(str, middle)) : false
         * (length >= str.length()) : false
         * (length < middle.length() + 2) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            CharSequence[] charSequenceArray = new CharSequence[] { "CBE", "" };
            stringUtils.when(() -> StringUtils.isAnyEmpty(charSequenceArray)).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.abbreviateMiddle("CBE", "", 2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("CE"));
                stringUtils.verify(() -> StringUtils.isAnyEmpty(charSequenceArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${appendIfMissingTest}, hash: F17969AC8C33822D61B5EE1E405DCAF5
    @Disabled()
    @Test()
    void appendIfMissingTest() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: CS - Method: appendIfMissing
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] {};
        //Act Statement(s)
        String result = StringUtils.appendIfMissing("str1", "suffix1", charSequenceArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${appendIfMissingIgnoreCaseTest}, hash: E73C37B8D791CA5FD442DFEAF8974F5B
    @Disabled()
    @Test()
    void appendIfMissingIgnoreCaseTest() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: CI - Method: appendIfMissing
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] {};
        //Act Statement(s)
        String result = StringUtils.appendIfMissingIgnoreCase("str1", "suffix1", charSequenceArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${capitalizeWhenStrLenEquals0}, hash: 28B80F5FD0B9929E0C09A1B5529614CC
    @Test()
    void capitalizeWhenStrLenEquals0() {
        /* Branches:
         * (strLen == 0) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.length("str1")).thenReturn(0);
            //Act Statement(s)
            String result = StringUtils.capitalize("str1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("str1"));
                stringUtils.verify(() -> StringUtils.length("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${capitalizeWhenFirstCodepointEqualsNewCodePoint}, hash: FCB552A1854B34661DC144FD01DD69E5
    @Test()
    void capitalizeWhenFirstCodepointEqualsNewCodePoint() {
        /* Branches:
         * (strLen == 0) : false
         * (firstCodepoint == newCodePoint) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.length("A")).thenReturn(1);
            //Act Statement(s)
            String result = StringUtils.capitalize("A");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("A"));
                stringUtils.verify(() -> StringUtils.length("A"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${capitalizeWhenInOffsetLessThanStrLen}, hash: 855601334377E7CD6DD7DD90D19FAE35
    @Disabled()
    @Test()
    void capitalizeWhenInOffsetLessThanStrLen() {
        /* Branches:
         * (strLen == 0) : false
         * (firstCodepoint == newCodePoint) : false
         * (inOffset < strLen) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.length("str1")).thenReturn(2);
            //Act Statement(s)
            String result = StringUtils.capitalize("str1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                stringUtils.verify(() -> StringUtils.length("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${centerTest}, hash: B386D8F600873DC9B457650B88D44EB7
    @Test()
    void centerTest() {
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.center("str1", 0, ' ')).thenReturn("return_of_center1");
            //Act Statement(s)
            String result = StringUtils.center("str1", 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_center1"));
                stringUtils.verify(() -> StringUtils.center("str1", 0, ' '), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${center1WhenSizeLessThanOrEqualsTo0}, hash: E2295D0B1D9FC08A7F2F87A10A0EA10D
    @Test()
    void center1WhenSizeLessThanOrEqualsTo0() {
        /* Branches:
         * (str == null) : false
         * (size <= 0) : true
         */
        //Act Statement(s)
        String result = StringUtils.center("str1", -1, 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("str1")));
    }

    //BaseRock generated method id: ${center1WhenPadsLessThanOrEqualsTo0}, hash: E9102FD6BE2085A639AE38524F51A47D
    @Test()
    void center1WhenPadsLessThanOrEqualsTo0() {
        /* Branches:
         * (str == null) : false
         * (size <= 0) : false
         * (pads <= 0) : true
         */
        //Act Statement(s)
        String result = StringUtils.center("A", 1, 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${center1WhenPadsGreaterThan0}, hash: 13848CCF79219D8FE6AD784866D1BB31
    @Test()
    void center1WhenPadsGreaterThan0() {
        /* Branches:
         * (str == null) : false
         * (size <= 0) : false
         * (pads <= 0) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.leftPad("A", 1, 'A')).thenReturn("return_of_leftPad1");
            stringUtils.when(() -> StringUtils.rightPad("return_of_leftPad1", 2, 'A')).thenReturn("return_of_rightPad1");
            //Act Statement(s)
            String result = StringUtils.center("A", 2, 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_rightPad1"));
                stringUtils.verify(() -> StringUtils.leftPad("A", 1, 'A'), atLeast(1));
                stringUtils.verify(() -> StringUtils.rightPad("return_of_leftPad1", 2, 'A'), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${center2WhenSizeLessThanOrEqualsTo0}, hash: 5D6161F110DA8A55DCC2C7EC8C72E5A4
    @Test()
    void center2WhenSizeLessThanOrEqualsTo0() {
        /* Branches:
         * (str == null) : false
         * (size <= 0) : true
         */
        //Act Statement(s)
        String result = StringUtils.center("str1", -1, "padStr1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("str1")));
    }

    //BaseRock generated method id: ${center2WhenPadsLessThanOrEqualsTo0}, hash: CA408484266BC5B2AFC4B74BA81F003C
    @Test()
    void center2WhenPadsLessThanOrEqualsTo0() {
        /* Branches:
         * (str == null) : false
         * (size <= 0) : false
         * (isEmpty(padStr)) : true
         * (pads <= 0) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("padStr1")).thenReturn(true);
            //Act Statement(s)
            String result = StringUtils.center("A", 1, "padStr1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("A"));
                stringUtils.verify(() -> StringUtils.isEmpty("padStr1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${center2WhenPadsGreaterThan0}, hash: EAB16EE48C806B33DA391DAE94DF0879
    @Test()
    void center2WhenPadsGreaterThan0() {
        /* Branches:
         * (str == null) : false
         * (size <= 0) : false
         * (isEmpty(padStr)) : true
         * (pads <= 0) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("padStr1")).thenReturn(true);
            stringUtils.when(() -> StringUtils.leftPad("A", 1, " ")).thenReturn("return_of_leftPad1");
            stringUtils.when(() -> StringUtils.rightPad("return_of_leftPad1", 2, " ")).thenReturn("return_of_rightPad1");
            //Act Statement(s)
            String result = StringUtils.center("A", 2, "padStr1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_rightPad1"));
                stringUtils.verify(() -> StringUtils.isEmpty("padStr1"), atLeast(1));
                stringUtils.verify(() -> StringUtils.leftPad("A", 1, " "), atLeast(1));
                stringUtils.verify(() -> StringUtils.rightPad("return_of_leftPad1", 2, " "), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${chompWhenIsEmptyStr}, hash: 2958978F6B63FB5B6666B8922E961F69
    @Test()
    void chompWhenIsEmptyStr() {
        /* Branches:
         * (isEmpty(str)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(true);
            //Act Statement(s)
            String result = StringUtils.chomp("str1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("str1"));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${chompWhenChEqualsCharUtilsLF}, hash: E3652460CBF7C1EE53A9F7367C34D4BE
    @Test()
    void chompWhenChEqualsCharUtilsLF() {
        /* Branches:
         * (isEmpty(str)) : false
         * (str.length() == 1) : true
         * (ch == CharUtils.CR) : false
         * (ch == CharUtils.LF) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("\n")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.chomp("\n");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(""));
                stringUtils.verify(() -> StringUtils.isEmpty("\n"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${chompWhenChNotEqualsCharUtilsLF}, hash: 3D382B5E284BE9EEC8D09BA0B4D49CFC
    @Test()
    void chompWhenChNotEqualsCharUtilsLF() {
        /* Branches:
         * (isEmpty(str)) : false
         * (str.length() == 1) : true
         * (ch == CharUtils.CR) : false
         * (ch == CharUtils.LF) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("B")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.chomp("B");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("B"));
                stringUtils.verify(() -> StringUtils.isEmpty("B"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${chompWhenStrCharAtLastIdxMinus1EqualsCharUtilsCR}, hash: BB03D501F9D76F8EF21E69B415414450
    @Test()
    void chompWhenStrCharAtLastIdxMinus1EqualsCharUtilsCR() {
        /* Branches:
         * (isEmpty(str)) : false
         * (str.length() == 1) : false
         * (last == CharUtils.LF) : true
         * (str.charAt(lastIdx - 1) == CharUtils.CR) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("\r\n")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.chomp("\r\n");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(""));
                stringUtils.verify(() -> StringUtils.isEmpty("\r\n"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${chompWhenLastNotEqualsCharUtilsCR}, hash: F5F1727D94668DCBDCEB879729BAC6CF
    @Test()
    void chompWhenLastNotEqualsCharUtilsCR() {
        /* Branches:
         * (isEmpty(str)) : false
         * (str.length() == 1) : false
         * (last == CharUtils.LF) : false
         * (last != CharUtils.CR) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("CB")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.chomp("CB");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("CB"));
                stringUtils.verify(() -> StringUtils.isEmpty("CB"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${chomp1Test}, hash: 628C8D18334F9F168123280DA9B40DF1
    @Test()
    void chomp1Test() {
        //Act Statement(s)
        String result = StringUtils.chomp("A", "B");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${chopWhenStrIsNull}, hash: C66B85A0B0A812F18CC32697EF6AE5DF
    @Test()
    void chopWhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
        //Act Statement(s)
        String result = StringUtils.chop((String) null);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${chopWhenStrLenLessThan2}, hash: 3394A8069738C8CD19C84710FBD7755F
    @Test()
    void chopWhenStrLenLessThan2() {
        /* Branches:
         * (str == null) : false
         * (strLen < 2) : true
         */
        //Act Statement(s)
        String result = StringUtils.chop("A");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${chopWhenRetCharAtLastIdxMinus1EqualsCharUtilsCR}, hash: 8BE9F0AEA34F0160D490E10A3DCA1E31
    @Test()
    void chopWhenRetCharAtLastIdxMinus1EqualsCharUtilsCR() {
        /* Branches:
         * (str == null) : false
         * (strLen < 2) : false
         * (last == CharUtils.LF) : true
         * (ret.charAt(lastIdx - 1) == CharUtils.CR) : true
         */
        //Act Statement(s)
        String result = StringUtils.chop("\r\n");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${chopWhenRetCharAtLastIdxMinus1NotEqualsCharUtilsCR}, hash: C76CCBA8EB091FE3A52D9C8FF8FAFB4D
    @Test()
    void chopWhenRetCharAtLastIdxMinus1NotEqualsCharUtilsCR() {
        /* Branches:
         * (str == null) : false
         * (strLen < 2) : false
         * (last == CharUtils.LF) : true
         * (ret.charAt(lastIdx - 1) == CharUtils.CR) : false
         */
        //Act Statement(s)
        String result = StringUtils.chop("A\n");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${compareTest}, hash: 9E1CD7568147CF35DB1F3F6EC1CD165A
    @Disabled()
    @Test()
    void compareTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        int result = StringUtils.compare("A", "B");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${compare1WhenStr1EqualsStr2}, hash: 1C5DE9F08F77DA51E50795B2A7B32094
    @Test()
    void compare1WhenStr1EqualsStr2() {
        /* Branches:
         * (str1 == str2) : true
         */
        //Act Statement(s)
        int result = StringUtils.compare("str1", "str1", false);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${compare1WhenNullIsLess}, hash: D88C43325CCD40B81C93579F12262BFB
    @Test()
    void compare1WhenNullIsLess() {
        /* Branches:
         * (str1 == str2) : false
         * (str1 == null) : true
         * (nullIsLess) : true
         */
        //Act Statement(s)
        int result = StringUtils.compare((String) null, "str2", true);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${compare1WhenNotNullIsLess}, hash: 860443C1C9FE9087634B7D098AC0F43B
    @Test()
    void compare1WhenNotNullIsLess() {
        /* Branches:
         * (str1 == str2) : false
         * (str1 == null) : true
         * (nullIsLess) : false
         */
        //Act Statement(s)
        int result = StringUtils.compare((String) null, "str2", false);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1)));
    }

    //BaseRock generated method id: ${compare1WhenStr2IsNotNull}, hash: B5FBEA1666C3272F3FE7AB0368DF03C2
    @Test()
    void compare1WhenStr2IsNotNull() {
        /* Branches:
         * (str1 == str2) : false
         * (str1 == null) : false
         * (str2 == null) : false
         */
        //Act Statement(s)
        int result = StringUtils.compare("B", "A", false);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1)));
    }

    //BaseRock generated method id: ${compare1WhenStr2IsNullAndNullIsLess}, hash: 60D8C20908E9F8E123C1D4CA78B32023
    @Test()
    void compare1WhenStr2IsNullAndNullIsLess() {
        /* Branches:
         * (str1 == str2) : false
         * (str1 == null) : false
         * (str2 == null) : true
         * (nullIsLess) : true
         */
        //Act Statement(s)
        int result = StringUtils.compare("str1", (String) null, true);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1)));
    }

    //BaseRock generated method id: ${compare1WhenStr2IsNullAndNotNullIsLess}, hash: A1AEAB82623F30C57524C1DE15F0223B
    @Test()
    void compare1WhenStr2IsNullAndNotNullIsLess() {
        /* Branches:
         * (str1 == str2) : false
         * (str1 == null) : false
         * (str2 == null) : true
         * (nullIsLess) : false
         */
        //Act Statement(s)
        int result = StringUtils.compare("str1", (String) null, false);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${compareIgnoreCaseTest}, hash: AA08C84F08DAD8604D6E5ED9976E8079
    @Disabled()
    @Test()
    void compareIgnoreCaseTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        int result = StringUtils.compareIgnoreCase("A", "B");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${compareIgnoreCase1WhenStr1EqualsStr2}, hash: 64AA16B64FB21975E012E61A8E6A10B8
    @Test()
    void compareIgnoreCase1WhenStr1EqualsStr2() {
        /* Branches:
         * (str1 == str2) : true
         */
        //Act Statement(s)
        int result = StringUtils.compareIgnoreCase("str1", "str1", false);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${compareIgnoreCase1WhenNullIsLess}, hash: 569DB4E49CEE01D9722D92BF06A64E38
    @Test()
    void compareIgnoreCase1WhenNullIsLess() {
        /* Branches:
         * (str1 == str2) : false
         * (str1 == null) : true
         * (nullIsLess) : true
         */
        //Act Statement(s)
        int result = StringUtils.compareIgnoreCase((String) null, "str2", true);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${compareIgnoreCase1WhenNotNullIsLess}, hash: 9D50AC9C5C43BFD4C0BEA6890B4CA7E5
    @Test()
    void compareIgnoreCase1WhenNotNullIsLess() {
        /* Branches:
         * (str1 == str2) : false
         * (str1 == null) : true
         * (nullIsLess) : false
         */
        //Act Statement(s)
        int result = StringUtils.compareIgnoreCase((String) null, "str2", false);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1)));
    }

    //BaseRock generated method id: ${compareIgnoreCase1WhenStr2IsNotNull}, hash: FC29FFD28A2DC67785163157077CCB44
    @Test()
    void compareIgnoreCase1WhenStr2IsNotNull() {
        /* Branches:
         * (str1 == str2) : false
         * (str1 == null) : false
         * (str2 == null) : false
         */
        //Act Statement(s)
        int result = StringUtils.compareIgnoreCase("B", "A", false);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1)));
    }

    //BaseRock generated method id: ${compareIgnoreCase1WhenStr2IsNullAndNullIsLess}, hash: A2D4E0D171F7B1D8C1E0171B5E7BA79E
    @Test()
    void compareIgnoreCase1WhenStr2IsNullAndNullIsLess() {
        /* Branches:
         * (str1 == str2) : false
         * (str1 == null) : false
         * (str2 == null) : true
         * (nullIsLess) : true
         */
        //Act Statement(s)
        int result = StringUtils.compareIgnoreCase("str1", (String) null, true);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1)));
    }

    //BaseRock generated method id: ${compareIgnoreCase1WhenStr2IsNullAndNotNullIsLess}, hash: F262F3BE680BE0D353844F4E9D754D1A
    @Test()
    void compareIgnoreCase1WhenStr2IsNullAndNotNullIsLess() {
        /* Branches:
         * (str1 == str2) : false
         * (str1 == null) : false
         * (str2 == null) : true
         * (nullIsLess) : false
         */
        //Act Statement(s)
        int result = StringUtils.compareIgnoreCase("str1", (String) null, false);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${containsWhenStringsCSContainsSeqSearchSeq}, hash: 0DD359A55594191A7F0DCEC1A9EBC364
    @Disabled()
    @Test()
    void containsWhenStringsCSContainsSeqSearchSeq() {
        /* Branches:
         * (Strings.CS.contains(seq, searchSeq)) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: CS - Method: contains
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        boolean result = StringUtils.contains((CharSequence) "seq1", (CharSequence) "searchSeq1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${containsWhenStringsCSNotContainsSeqSearchSeq}, hash: 14AE837B36E0148E87BCB93CC33F5E83
    @Test()
    void containsWhenStringsCSNotContainsSeqSearchSeq() {
        /* Branches:
         * (Strings.CS.contains(seq, searchSeq)) : false
         */
        //Act Statement(s)
        boolean result = StringUtils.contains((CharSequence) "seq1", (CharSequence) "searchSeq1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${contains1WhenIsEmptySeq}, hash: 088DCF1E269434B7F4C4D3E29E1F3F36
    @Test()
    void contains1WhenIsEmptySeq() {
        /* Branches:
         * (isEmpty(seq)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("seq1")).thenReturn(true);
            //Act Statement(s)
            boolean result = StringUtils.contains((CharSequence) "seq1", 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.isEmpty("seq1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${contains1WhenCharSequenceUtilsIndexOfSeqSearchChar0GreaterThanOrEqualsTo0}, hash: 59CAB9AEDAA3C67A8DED64A49E82D865
    @Disabled()
    @Test()
    void contains1WhenCharSequenceUtilsIndexOfSeqSearchChar0GreaterThanOrEqualsTo0() {
        /* Branches:
         * (isEmpty(seq)) : false
         * (CharSequenceUtils.indexOf(seq, searchChar, 0) >= 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("seq1")).thenReturn(false);
            //Act Statement(s)
            boolean result = StringUtils.contains((CharSequence) "seq1", 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                stringUtils.verify(() -> StringUtils.isEmpty("seq1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${contains1WhenCharSequenceUtilsIndexOfSeqSearchChar0LessThan0}, hash: 57D8D5CC598C4DD146CA4CB162EF944A
    @Test()
    void contains1WhenCharSequenceUtilsIndexOfSeqSearchChar0LessThan0() {
        /* Branches:
         * (isEmpty(seq)) : false
         * (CharSequenceUtils.indexOf(seq, searchChar, 0) >= 0) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("seq1")).thenReturn(false);
            //Act Statement(s)
            boolean result = StringUtils.contains((CharSequence) "seq1", 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.isEmpty("seq1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${containsAnyWhenArrayUtilsIsEmptySearchChars}, hash: 650AC21A5A94446D06522213F99D83DC
    @Test()
    void containsAnyWhenArrayUtilsIsEmptySearchChars() {
        /* Branches:
         * (isEmpty(cs)) : false
         * (ArrayUtils.isEmpty(searchChars)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("cs1")).thenReturn(false);
            char[] charArray = new char[] {};
            //Act Statement(s)
            boolean result = StringUtils.containsAny((CharSequence) "cs1", charArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.isEmpty("cs1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${containsAnyWhenJPlus1IndexOfSearchCharsEqualsCsCharAtIPlus1}, hash: DA7B517EE9BF8F7BDA364543D37C3452
    @Disabled()
    @Test()
    void containsAnyWhenJPlus1IndexOfSearchCharsEqualsCsCharAtIPlus1() {
        /* Branches:
         * (isEmpty(cs)) : false
         * (ArrayUtils.isEmpty(searchChars)) : false
         * (i < csLength) : true
         * (j < searchLength) : true
         * (searchChars[j] == ch) : true
         * (!Character.isHighSurrogate(ch)) : false
         * (j == searchLast) : false
         * (i < csLast) : true
         * (searchChars[j + 1] == cs.charAt(i + 1)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("cs1")).thenReturn(false);
            char[] charArray = new char[] { 'A', 'A' };
            //Act Statement(s)
            boolean result = StringUtils.containsAny((CharSequence) "cs1", charArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                stringUtils.verify(() -> StringUtils.isEmpty("cs1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${containsAnyWhenJPlus1IndexOfSearchCharsNotEqualsCsCharAtIPlus1}, hash: 5361E4EF77E88D21DB18389C4DBC9411
    @Test()
    void containsAnyWhenJPlus1IndexOfSearchCharsNotEqualsCsCharAtIPlus1() {
        /* Branches:
         * (isEmpty(cs)) : false
         * (ArrayUtils.isEmpty(searchChars)) : false
         * (i < csLength) : true
         * (j < searchLength) : true
         * (searchChars[j] == ch) : true
         * (!Character.isHighSurrogate(ch)) : false
         * (j == searchLast) : false
         * (i < csLast) : true
         * (searchChars[j + 1] == cs.charAt(i + 1)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("cs1")).thenReturn(false);
            char[] charArray = new char[] { 'A', 'A' };
            //Act Statement(s)
            boolean result = StringUtils.containsAny((CharSequence) "cs1", charArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.isEmpty("cs1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${containsAny1WhenSearchCharsIsNull}, hash: 7416A2F10150BCD16B36BD1F4836EED3
    @Test()
    void containsAny1WhenSearchCharsIsNull() {
        /* Branches:
         * (searchChars == null) : true
         */
        //Arrange Statement(s)
        CharSequence charSequence = null;
        //Act Statement(s)
        boolean result = StringUtils.containsAny((CharSequence) "cs1", charSequence);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${containsAny1WhenContainsAnyCsCharSequenceUtilsToCharArraySearchChars}, hash: C6BEE70CE775BC467B619E4F72DF7D99
    @Test()
    void containsAny1WhenContainsAnyCsCharSequenceUtilsToCharArraySearchChars() {
        /* Branches:
         * (searchChars == null) : false
         * (containsAny(cs, CharSequenceUtils.toCharArray(searchChars))) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] { 's', 'e', 'a', 'r', 'c', 'h', 'C', 'h', 'a', 'r', 's', '1' };
            stringUtils.when(() -> StringUtils.containsAny("cs1", charArray)).thenReturn(true);
            //Act Statement(s)
            boolean result = StringUtils.containsAny((CharSequence) "cs1", (CharSequence) "searchChars1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                stringUtils.verify(() -> StringUtils.containsAny("cs1", charArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${containsAny1WhenContainsAnyNotCsCharSequenceUtilsToCharArraySearchChars}, hash: 454062BD9F3AEDF7B946C53708CD1999
    @Test()
    void containsAny1WhenContainsAnyNotCsCharSequenceUtilsToCharArraySearchChars() {
        /* Branches:
         * (searchChars == null) : false
         * (containsAny(cs, CharSequenceUtils.toCharArray(searchChars))) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] { 's', 'e', 'a', 'r', 'c', 'h', 'C', 'h', 'a', 'r', 's', '1' };
            stringUtils.when(() -> StringUtils.containsAny("cs1", charArray)).thenReturn(false);
            //Act Statement(s)
            boolean result = StringUtils.containsAny((CharSequence) "cs1", (CharSequence) "searchChars1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.containsAny("cs1", charArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${containsAny2WhenStringsCSContainsAnyCsSearchCharSequences}, hash: CB376CC5AA4567A29CB7CE541FFE2CC8
    @Disabled()
    @Test()
    void containsAny2WhenStringsCSContainsAnyCsSearchCharSequences() {
        /* Branches:
         * (Strings.CS.containsAny(cs, searchCharSequences)) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: CS - Method: containsAny
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] {};
        //Act Statement(s)
        boolean result = StringUtils.containsAny((CharSequence) "cs1", charSequenceArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${containsAny2WhenStringsCSNotContainsAnyCsSearchCharSequences}, hash: D03A1350EDE3B6ABFCD26E31EBC94A60
    @Test()
    void containsAny2WhenStringsCSNotContainsAnyCsSearchCharSequences() {
        /* Branches:
         * (Strings.CS.containsAny(cs, searchCharSequences)) : false
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: CS - Method: containsAny
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] {};
        //Act Statement(s)
        boolean result = StringUtils.containsAny((CharSequence) "cs1", charSequenceArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${containsAnyIgnoreCaseWhenStringsCIContainsAnyCsSearchCharSequences}, hash: 7E49E2C18F7199247CB59BFFD1FFCE60
    @Disabled()
    @Test()
    void containsAnyIgnoreCaseWhenStringsCIContainsAnyCsSearchCharSequences() {
        /* Branches:
         * (Strings.CI.containsAny(cs, searchCharSequences)) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: CI - Method: containsAny
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] {};
        //Act Statement(s)
        boolean result = StringUtils.containsAnyIgnoreCase("cs1", charSequenceArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${containsAnyIgnoreCaseWhenStringsCINotContainsAnyCsSearchCharSequences}, hash: F730FA2F554035744B6C820AE7D3B006
    @Test()
    void containsAnyIgnoreCaseWhenStringsCINotContainsAnyCsSearchCharSequences() {
        /* Branches:
         * (Strings.CI.containsAny(cs, searchCharSequences)) : false
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: CI - Method: containsAny
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] {};
        //Act Statement(s)
        boolean result = StringUtils.containsAnyIgnoreCase("cs1", charSequenceArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${containsIgnoreCaseWhenStringsCIContainsStrSearchStr}, hash: 94BD1E4893396C751CFEA692E1ADD136
    @Disabled()
    @Test()
    void containsIgnoreCaseWhenStringsCIContainsStrSearchStr() {
        /* Branches:
         * (Strings.CI.contains(str, searchStr)) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: CI - Method: contains
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        boolean result = StringUtils.containsIgnoreCase("str1", "searchStr1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${containsIgnoreCaseWhenStringsCINotContainsStrSearchStr}, hash: 87FF9A57245B41C9CEC5B3AEB5290B38
    @Test()
    void containsIgnoreCaseWhenStringsCINotContainsStrSearchStr() {
        /* Branches:
         * (Strings.CI.contains(str, searchStr)) : false
         */
        //Act Statement(s)
        boolean result = StringUtils.containsIgnoreCase("str1", "searchStr1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${containsNoneWhenSearchCharsIsNull}, hash: E77115680BF8AC7B8DAF0D3F8D984079
    @Test()
    void containsNoneWhenSearchCharsIsNull() {
        /* Branches:
         * (cs == null) : false
         * (searchChars == null) : true
         */
        //Arrange Statement(s)
        char[] _char = null;
        //Act Statement(s)
        boolean result = StringUtils.containsNone((CharSequence) "cs1", _char);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${containsNoneWhenJPlus1IndexOfSearchCharsEqualsCsCharAtIPlus1}, hash: ABF2C371C7C26A2D0F37B588678A1BEA
    @Disabled()
    @Test()
    void containsNoneWhenJPlus1IndexOfSearchCharsEqualsCsCharAtIPlus1() {
        /* Branches:
         * (cs == null) : false
         * (searchChars == null) : false
         * (i < csLen) : true
         * (j < searchLen) : true
         * (searchChars[j] == ch) : true
         * (!Character.isHighSurrogate(ch)) : false
         * (j == searchLast) : false
         * (i < csLast) : true
         * (searchChars[j + 1] == cs.charAt(i + 1)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        //Act Statement(s)
        boolean result = StringUtils.containsNone((CharSequence) "cs1", charArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${containsNoneWhenJPlus1IndexOfSearchCharsNotEqualsCsCharAtIPlus1}, hash: D0DD5465ED901B01B27866B4C5978130
    @Test()
    void containsNoneWhenJPlus1IndexOfSearchCharsNotEqualsCsCharAtIPlus1() {
        /* Branches:
         * (cs == null) : false
         * (searchChars == null) : false
         * (i < csLen) : true
         * (j < searchLen) : true
         * (searchChars[j] == ch) : true
         * (!Character.isHighSurrogate(ch)) : false
         * (j == searchLast) : false
         * (i < csLast) : true
         * (searchChars[j + 1] == cs.charAt(i + 1)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        //Act Statement(s)
        boolean result = StringUtils.containsNone((CharSequence) "cs1", charArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${containsNone1WhenInvalidCharsIsNull}, hash: 4A68363CC846FF582E1DBAC95964123E
    @Test()
    void containsNone1WhenInvalidCharsIsNull() {
        /* Branches:
         * (invalidChars == null) : true
         */
        //Act Statement(s)
        boolean result = StringUtils.containsNone((CharSequence) "cs1", (String) null);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${containsNone1WhenContainsNoneCsInvalidCharsToCharArray}, hash: 7C6EB6AA5482F6207E8F709E1EA68B91
    @Test()
    void containsNone1WhenContainsNoneCsInvalidCharsToCharArray() {
        /* Branches:
         * (invalidChars == null) : false
         * (containsNone(cs, invalidChars.toCharArray())) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] { 'A' };
            stringUtils.when(() -> StringUtils.containsNone("cs1", charArray)).thenReturn(true);
            //Act Statement(s)
            boolean result = StringUtils.containsNone((CharSequence) "cs1", "A");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                stringUtils.verify(() -> StringUtils.containsNone("cs1", charArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${containsNone1WhenContainsNoneNotCsInvalidCharsToCharArray}, hash: F01A250A1FD145AA32918A5ECF14D2D0
    @Test()
    void containsNone1WhenContainsNoneNotCsInvalidCharsToCharArray() {
        /* Branches:
         * (invalidChars == null) : false
         * (containsNone(cs, invalidChars.toCharArray())) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] { 'A' };
            stringUtils.when(() -> StringUtils.containsNone("cs1", charArray)).thenReturn(false);
            //Act Statement(s)
            boolean result = StringUtils.containsNone((CharSequence) "cs1", "A");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.containsNone("cs1", charArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${containsOnlyWhenCsIsNull}, hash: 64932E1D4D53FA2C2C3A2A9428BA0696
    @Test()
    void containsOnlyWhenCsIsNull() {
        /* Branches:
         * (valid == null) : false
         * (cs == null) : true
         */
        //Arrange Statement(s)
        CharSequence charSequence = null;
        char[] charArray = new char[] {};
        //Act Statement(s)
        boolean result = StringUtils.containsOnly(charSequence, charArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${containsOnlyWhenCsLengthEquals0}, hash: 984EF0A04319712DC2791B82400F07B6
    @Disabled()
    @Test()
    void containsOnlyWhenCsLengthEquals0() {
        /* Branches:
         * (valid == null) : false
         * (cs == null) : false
         * (cs.length() == 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        //Act Statement(s)
        boolean result = StringUtils.containsOnly((CharSequence) "cs1", charArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${containsOnlyWhenValidLengthEquals0}, hash: E402508E55ED6772DAA41ACCFD197075
    @Test()
    void containsOnlyWhenValidLengthEquals0() {
        /* Branches:
         * (valid == null) : false
         * (cs == null) : false
         * (cs.length() == 0) : false
         * (valid.length == 0) : true
         */
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        //Act Statement(s)
        boolean result = StringUtils.containsOnly((CharSequence) "cs1", charArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${containsOnlyWhenIndexOfAnyButCsValidEqualsINDEX_NOT_FOUND}, hash: 185456E1A5974E1C2E20FF866BAAD1FF
    @Test()
    void containsOnlyWhenIndexOfAnyButCsValidEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (valid == null) : false
         * (cs == null) : false
         * (cs.length() == 0) : false
         * (valid.length == 0) : false
         * (indexOfAnyBut(cs, valid) == INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] { 'A' };
            stringUtils.when(() -> StringUtils.indexOfAnyBut("cs1", charArray)).thenReturn(-1);
            //Act Statement(s)
            boolean result = StringUtils.containsOnly((CharSequence) "cs1", charArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                stringUtils.verify(() -> StringUtils.indexOfAnyBut("cs1", charArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${containsOnlyWhenIndexOfAnyButCsValidNotEqualsINDEX_NOT_FOUND}, hash: 888579D2AAC09F7828392A9D0014549D
    @Test()
    void containsOnlyWhenIndexOfAnyButCsValidNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (valid == null) : false
         * (cs == null) : false
         * (cs.length() == 0) : false
         * (valid.length == 0) : false
         * (indexOfAnyBut(cs, valid) == INDEX_NOT_FOUND) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] { 'A' };
            stringUtils.when(() -> StringUtils.indexOfAnyBut("cs1", charArray)).thenReturn(1);
            //Act Statement(s)
            boolean result = StringUtils.containsOnly((CharSequence) "cs1", charArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.indexOfAnyBut("cs1", charArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${containsOnly1WhenValidCharsIsNull}, hash: 91EA0EA82DCBA7EE634C48166E3A1916
    @Test()
    void containsOnly1WhenValidCharsIsNull() {
        /* Branches:
         * (cs == null) : false
         * (validChars == null) : true
         */
        //Act Statement(s)
        boolean result = StringUtils.containsOnly((CharSequence) "cs1", (String) null);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${containsOnly1WhenContainsOnlyCsValidCharsToCharArray}, hash: A6E8369984C7AB9518C2F9B82A8A9B98
    @Test()
    void containsOnly1WhenContainsOnlyCsValidCharsToCharArray() {
        /* Branches:
         * (cs == null) : false
         * (validChars == null) : false
         * (containsOnly(cs, validChars.toCharArray())) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] { 'A' };
            stringUtils.when(() -> StringUtils.containsOnly("cs1", charArray)).thenReturn(true);
            //Act Statement(s)
            boolean result = StringUtils.containsOnly((CharSequence) "cs1", "A");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                stringUtils.verify(() -> StringUtils.containsOnly("cs1", charArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${containsOnly1WhenContainsOnlyNotCsValidCharsToCharArray}, hash: 3E431A10AF0CF3EA4A45B1902AF15B71
    @Test()
    void containsOnly1WhenContainsOnlyNotCsValidCharsToCharArray() {
        /* Branches:
         * (cs == null) : false
         * (validChars == null) : false
         * (containsOnly(cs, validChars.toCharArray())) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] { 'A' };
            stringUtils.when(() -> StringUtils.containsOnly("cs1", charArray)).thenReturn(false);
            //Act Statement(s)
            boolean result = StringUtils.containsOnly((CharSequence) "cs1", "A");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.containsOnly("cs1", charArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${containsWhitespaceWhenIsEmptySeq}, hash: 592F3ED73D45C174F583419C7FD466F4
    @Test()
    void containsWhitespaceWhenIsEmptySeq() {
        /* Branches:
         * (isEmpty(seq)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("seq1")).thenReturn(true);
            //Act Statement(s)
            boolean result = StringUtils.containsWhitespace("seq1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.isEmpty("seq1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${containsWhitespaceWhenCharacterIsWhitespaceSeqCharAtI}, hash: D807F1E2E9231A51BA1A4BA293AE6904
    @Disabled()
    @Test()
    void containsWhitespaceWhenCharacterIsWhitespaceSeqCharAtI() {
        /* Branches:
         * (isEmpty(seq)) : false
         * (i < strLen) : true
         * (Character.isWhitespace(seq.charAt(i))) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("seq1")).thenReturn(false);
            //Act Statement(s)
            boolean result = StringUtils.containsWhitespace("seq1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                stringUtils.verify(() -> StringUtils.isEmpty("seq1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${containsWhitespaceWhenCharacterNotIsWhitespaceSeqCharAtI}, hash: C1AEA4A26C8C6872DB127503EBAEEB15
    @Test()
    void containsWhitespaceWhenCharacterNotIsWhitespaceSeqCharAtI() {
        /* Branches:
         * (isEmpty(seq)) : false
         * (i < strLen) : true
         * (Character.isWhitespace(seq.charAt(i))) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("seq1")).thenReturn(false);
            //Act Statement(s)
            boolean result = StringUtils.containsWhitespace("seq1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.isEmpty("seq1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${countMatchesWhenIsEmptyStr}, hash: 0649BF5DCBB63299EF03F08651B753A9
    @Test()
    void countMatchesWhenIsEmptyStr() {
        /* Branches:
         * (isEmpty(str)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(true);
            //Act Statement(s)
            int result = StringUtils.countMatches((CharSequence) "str1", 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${countMatchesWhenChEqualsStrCharAtI}, hash: AB97194B323B7BD14F6FE1BEA182B8C2
    @Disabled()
    @Test()
    void countMatchesWhenChEqualsStrCharAtI() {
        /* Branches:
         * (isEmpty(str)) : false
         * (i < str.length()) : true
         * (ch == str.charAt(i)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(false);
            //Act Statement(s)
            int result = StringUtils.countMatches((CharSequence) "str1", 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(1));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${countMatches1WhenIsEmptySub}, hash: D2A562BF9D5A1EC5751A1C76F801C08B
    @Test()
    void countMatches1WhenIsEmptySub() {
        /* Branches:
         * (isEmpty(str)) : false
         * (isEmpty(sub)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("sub1")).thenReturn(true);
            //Act Statement(s)
            int result = StringUtils.countMatches((CharSequence) "str1", (CharSequence) "sub1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("sub1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${countMatches1WhenIdxAssignedCharSequenceUtilsIndexOfStrSubIdxNotEqualsINDEX_NOT_FOUND}, hash: E5D0A7F8E96CFF9E99730C63396167F0
    @Disabled()
    @Test()
    void countMatches1WhenIdxAssignedCharSequenceUtilsIndexOfStrSubIdxNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (isEmpty(str)) : false
         * (isEmpty(sub)) : false
         * ((idx = CharSequenceUtils.indexOf(str, sub, idx)) != INDEX_NOT_FOUND) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("sub1")).thenReturn(false);
            //Act Statement(s)
            int result = StringUtils.countMatches((CharSequence) "str1", (CharSequence) "sub1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(1));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("sub1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${defaultIfBlankWhenIsBlankStr}, hash: C50A1BC3586DB87A7BE57C0717F1C227
    @Test()
    void defaultIfBlankWhenIsBlankStr() {
        /* Branches:
         * (isBlank(str)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isBlank("str1")).thenReturn(true);
            //Act Statement(s)
            CharSequence result = StringUtils.defaultIfBlank("str1", "defaultStr1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("defaultStr1"));
                stringUtils.verify(() -> StringUtils.isBlank("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${defaultIfBlankWhenIsBlankNotStr}, hash: 8FD17B5EDA43AF6558140A4E17E6DA21
    @Test()
    void defaultIfBlankWhenIsBlankNotStr() {
        /* Branches:
         * (isBlank(str)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isBlank("str1")).thenReturn(false);
            //Act Statement(s)
            CharSequence result = StringUtils.defaultIfBlank("str1", "defaultStr1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("str1"));
                stringUtils.verify(() -> StringUtils.isBlank("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${defaultIfEmptyWhenIsEmptyStr}, hash: 60373EBFB3C368BF3A8F955E2F2432DD
    @Test()
    void defaultIfEmptyWhenIsEmptyStr() {
        /* Branches:
         * (isEmpty(str)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(true);
            //Act Statement(s)
            CharSequence result = StringUtils.defaultIfEmpty("str1", "defaultStr1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("defaultStr1"));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${defaultIfEmptyWhenIsEmptyNotStr}, hash: 51011D60C7529F91772E46B2C36CA697
    @Test()
    void defaultIfEmptyWhenIsEmptyNotStr() {
        /* Branches:
         * (isEmpty(str)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(false);
            //Act Statement(s)
            CharSequence result = StringUtils.defaultIfEmpty("str1", "defaultStr1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("str1"));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${defaultStringTest}, hash: 78F4A449A93E7BD97F65F75DE206A592
    @Test()
    void defaultStringTest() {
        //Act Statement(s)
        String result = StringUtils.defaultString("A");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${defaultString1Test}, hash: 580386B026E9D6F68CFA383D219E57AA
    @Test()
    void defaultString1Test() {
        //Act Statement(s)
        String result = StringUtils.defaultString("A", "B");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${deleteWhitespaceWhenIsEmptyStr}, hash: 9C624B4D4BC71C20A86FFD360D695FB0
    @Test()
    void deleteWhitespaceWhenIsEmptyStr() {
        /* Branches:
         * (isEmpty(str)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(true);
            //Act Statement(s)
            String result = StringUtils.deleteWhitespace("str1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("str1"));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${deleteWhitespaceWhenCharacterNotIsWhitespaceStrCharAtIAndCountEqualsSz}, hash: 62B6D9BBBA0E7DFAA0048CEA256E90CD
    @Test()
    void deleteWhitespaceWhenCharacterNotIsWhitespaceStrCharAtIAndCountEqualsSz() {
        /* Branches:
         * (isEmpty(str)) : false
         * (i < sz) : true
         * (!Character.isWhitespace(str.charAt(i))) : true
         * (count == sz) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("A")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.deleteWhitespace("A");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("A"));
                stringUtils.verify(() -> StringUtils.isEmpty("A"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${deleteWhitespaceWhenCountNotEquals0}, hash: 586C942CAB2E0363F985C95F37DF46BE
    @Disabled()
    @Test()
    void deleteWhitespaceWhenCountNotEquals0() {
        /* Branches:
         * (isEmpty(str)) : false
         * (i < sz) : true
         * (!Character.isWhitespace(str.charAt(i))) : true
         * (count == sz) : false
         * (count == 0) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("AB")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.deleteWhitespace("AB");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                stringUtils.verify(() -> StringUtils.isEmpty("AB"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${deleteWhitespaceWhenCountEquals0}, hash: 9A1E3FB520FCE43E315BF3D162CBA6CF
    @Disabled()
    @Test()
    void deleteWhitespaceWhenCountEquals0() {
        /* Branches:
         * (isEmpty(str)) : false
         * (i < sz) : true
         * (!Character.isWhitespace(str.charAt(i))) : false
         * (count == sz) : false
         * (count == 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.deleteWhitespace("str1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(""));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${differenceWhenStr1IsNull}, hash: 45BAF37924F9254698C8EFCFE292B90E
    @Test()
    void differenceWhenStr1IsNull() {
        /* Branches:
         * (str1 == null) : true
         */
        //Act Statement(s)
        String result = StringUtils.difference((String) null, "str2");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("str2")));
    }

    //BaseRock generated method id: ${differenceWhenStr2IsNull}, hash: CD9C1D9678C420E831E0F155537C9E4B
    @Test()
    void differenceWhenStr2IsNull() {
        /* Branches:
         * (str1 == null) : false
         * (str2 == null) : true
         */
        //Act Statement(s)
        String result = StringUtils.difference("str1", (String) null);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("str1")));
    }

    //BaseRock generated method id: ${differenceWhenAtEqualsINDEX_NOT_FOUND}, hash: 1AE9415AA70CC8A0C9FC3513496FE0B6
    @Test()
    void differenceWhenAtEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (str1 == null) : false
         * (str2 == null) : false
         * (at == INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.indexOfDifference("str1", "str2")).thenReturn(-1);
            //Act Statement(s)
            String result = StringUtils.difference("str1", "str2");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(""));
                stringUtils.verify(() -> StringUtils.indexOfDifference("str1", "str2"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${differenceWhenAtNotEqualsINDEX_NOT_FOUND}, hash: 43587A1FDF861D96CCA7DF93CD61F5EA
    @Test()
    void differenceWhenAtNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (str1 == null) : false
         * (str2 == null) : false
         * (at == INDEX_NOT_FOUND) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.indexOfDifference("str1", "A")).thenReturn(0);
            //Act Statement(s)
            String result = StringUtils.difference("str1", "A");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("A"));
                stringUtils.verify(() -> StringUtils.indexOfDifference("str1", "A"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${endsWithWhenStringsCSEndsWithStrSuffix}, hash: E508944FEA9B19873B48342BE1712F2F
    @Disabled()
    @Test()
    void endsWithWhenStringsCSEndsWithStrSuffix() {
        /* Branches:
         * (Strings.CS.endsWith(str, suffix)) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: CS - Method: endsWith
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        boolean result = StringUtils.endsWith("str1", "suffix1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${endsWithWhenStringsCSNotEndsWithStrSuffix}, hash: 74AD49947B4E899035711B3A257FF230
    @Test()
    void endsWithWhenStringsCSNotEndsWithStrSuffix() {
        /* Branches:
         * (Strings.CS.endsWith(str, suffix)) : false
         */
        //Act Statement(s)
        boolean result = StringUtils.endsWith("str1", "suffix1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${endsWithAnyWhenStringsCSEndsWithAnySequenceSearchStrings}, hash: F7D699B2B0F519F193BA4558643749C0
    @Disabled()
    @Test()
    void endsWithAnyWhenStringsCSEndsWithAnySequenceSearchStrings() {
        /* Branches:
         * (Strings.CS.endsWithAny(sequence, searchStrings)) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: CS - Method: endsWithAny
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] {};
        //Act Statement(s)
        boolean result = StringUtils.endsWithAny("sequence1", charSequenceArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${endsWithAnyWhenStringsCSNotEndsWithAnySequenceSearchStrings}, hash: B5968625E3FD14307178C08C189DDCDB
    @Test()
    void endsWithAnyWhenStringsCSNotEndsWithAnySequenceSearchStrings() {
        /* Branches:
         * (Strings.CS.endsWithAny(sequence, searchStrings)) : false
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: CS - Method: endsWithAny
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] {};
        //Act Statement(s)
        boolean result = StringUtils.endsWithAny("sequence1", charSequenceArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${endsWithIgnoreCaseWhenStringsCIEndsWithStrSuffix}, hash: 691BEC6F36CAD02D43B585FED18466A8
    @Disabled()
    @Test()
    void endsWithIgnoreCaseWhenStringsCIEndsWithStrSuffix() {
        /* Branches:
         * (Strings.CI.endsWith(str, suffix)) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: CI - Method: endsWith
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        boolean result = StringUtils.endsWithIgnoreCase("str1", "suffix1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${endsWithIgnoreCaseWhenStringsCINotEndsWithStrSuffix}, hash: 520E5D4A0703E0A661AD71B070F9B8CC
    @Test()
    void endsWithIgnoreCaseWhenStringsCINotEndsWithStrSuffix() {
        /* Branches:
         * (Strings.CI.endsWith(str, suffix)) : false
         */
        //Act Statement(s)
        boolean result = StringUtils.endsWithIgnoreCase("str1", "suffix1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${equalsWhenStringsCSEqualsCs1Cs2}, hash: 24C356636F4C1B1C1DA6C2477A41AA9C
    @Disabled()
    @Test()
    void equalsWhenStringsCSEqualsCs1Cs2() {
        /* Branches:
         * (Strings.CS.equals(cs1, cs2)) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: CS - Method: equals
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        boolean result = StringUtils.equals("cs1", "cs2");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${equalsWhenStringsCSNotEqualsCs1Cs2}, hash: FD1C7D9FCAC67AB12CE9644433AE6577
    @Test()
    void equalsWhenStringsCSNotEqualsCs1Cs2() {
        /* Branches:
         * (Strings.CS.equals(cs1, cs2)) : false
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: CS - Method: equals
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        boolean result = StringUtils.equals("cs1", "cs2");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${equalsAnyWhenStringsCSEqualsAnyStringSearchStrings}, hash: 9914CBC56EDD372134BEBF58A219BA76
    @Disabled()
    @Test()
    void equalsAnyWhenStringsCSEqualsAnyStringSearchStrings() {
        /* Branches:
         * (Strings.CS.equalsAny(string, searchStrings)) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: CS - Method: equalsAny
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] {};
        //Act Statement(s)
        boolean result = StringUtils.equalsAny("string1", charSequenceArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${equalsAnyWhenStringsCSNotEqualsAnyStringSearchStrings}, hash: 51B94F44E5DBA4AB3421269F872FF985
    @Test()
    void equalsAnyWhenStringsCSNotEqualsAnyStringSearchStrings() {
        /* Branches:
         * (Strings.CS.equalsAny(string, searchStrings)) : false
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: CS - Method: equalsAny
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] {};
        //Act Statement(s)
        boolean result = StringUtils.equalsAny("string1", charSequenceArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${equalsAnyIgnoreCaseWhenStringsCIEqualsAnyStringSearchStrings}, hash: 8A4BB9654FF909D4ECCBF05F0232DC02
    @Disabled()
    @Test()
    void equalsAnyIgnoreCaseWhenStringsCIEqualsAnyStringSearchStrings() {
        /* Branches:
         * (Strings.CI.equalsAny(string, searchStrings)) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: CI - Method: equalsAny
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] {};
        //Act Statement(s)
        boolean result = StringUtils.equalsAnyIgnoreCase("string1", charSequenceArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${equalsAnyIgnoreCaseWhenStringsCINotEqualsAnyStringSearchStrings}, hash: 59AFF0A43DD9F383BA245A7B54EDBA3D
    @Test()
    void equalsAnyIgnoreCaseWhenStringsCINotEqualsAnyStringSearchStrings() {
        /* Branches:
         * (Strings.CI.equalsAny(string, searchStrings)) : false
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: CI - Method: equalsAny
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] {};
        //Act Statement(s)
        boolean result = StringUtils.equalsAnyIgnoreCase("string1", charSequenceArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${equalsIgnoreCaseWhenStringsCIEqualsCs1Cs2}, hash: D46E99A2D35093374B30BD67F49668B5
    @Disabled()
    @Test()
    void equalsIgnoreCaseWhenStringsCIEqualsCs1Cs2() {
        /* Branches:
         * (Strings.CI.equals(cs1, cs2)) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: CI - Method: equals
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        boolean result = StringUtils.equalsIgnoreCase("cs1", "cs2");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${equalsIgnoreCaseWhenStringsCINotEqualsCs1Cs2}, hash: 731DFD33B7697A70E483B716A7EC6A68
    @Test()
    void equalsIgnoreCaseWhenStringsCINotEqualsCs1Cs2() {
        /* Branches:
         * (Strings.CI.equals(cs1, cs2)) : false
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: CI - Method: equals
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        boolean result = StringUtils.equalsIgnoreCase("cs1", "cs2");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${firstNonBlankWhenIsNotBlankVal}, hash: 80CA2C793E880B54D604424D6F81FC9C
    @Test()
    void firstNonBlankWhenIsNotBlankVal() {
        /* Branches:
         * (values != null) : true
         * (for-each(values)) : true
         * (isNotBlank(val)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isNotBlank("charSequence1")).thenReturn(true);
            CharSequence[] charSequenceArray = new CharSequence[] { "charSequence1" };
            //Act Statement(s)
            CharSequence result = StringUtils.firstNonBlank(charSequenceArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("charSequence1"));
                stringUtils.verify(() -> StringUtils.isNotBlank("charSequence1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${firstNonBlankWhenIsNotBlankNotVal}, hash: 13D4C0022DABB1BB315F003DDB88F3D6
    @Test()
    void firstNonBlankWhenIsNotBlankNotVal() {
        /* Branches:
         * (values != null) : true
         * (for-each(values)) : true
         * (isNotBlank(val)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isNotBlank("charSequence1")).thenReturn(false);
            CharSequence[] charSequenceArray = new CharSequence[] { "charSequence1" };
            //Act Statement(s)
            CharSequence result = StringUtils.firstNonBlank(charSequenceArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(nullValue()));
                stringUtils.verify(() -> StringUtils.isNotBlank("charSequence1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${firstNonEmptyWhenIsNotEmptyVal}, hash: 993C123004F949E3E5013F5F2C1F3B12
    @Test()
    void firstNonEmptyWhenIsNotEmptyVal() {
        /* Branches:
         * (values != null) : true
         * (for-each(values)) : true
         * (isNotEmpty(val)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isNotEmpty("charSequence1")).thenReturn(true);
            CharSequence[] charSequenceArray = new CharSequence[] { "charSequence1" };
            //Act Statement(s)
            CharSequence result = StringUtils.firstNonEmpty(charSequenceArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("charSequence1"));
                stringUtils.verify(() -> StringUtils.isNotEmpty("charSequence1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${firstNonEmptyWhenIsNotEmptyNotVal}, hash: 2A728FF6F558FE6D9F2DFA9B1B47AF97
    @Test()
    void firstNonEmptyWhenIsNotEmptyNotVal() {
        /* Branches:
         * (values != null) : true
         * (for-each(values)) : true
         * (isNotEmpty(val)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isNotEmpty("charSequence1")).thenReturn(false);
            CharSequence[] charSequenceArray = new CharSequence[] { "charSequence1" };
            //Act Statement(s)
            CharSequence result = StringUtils.firstNonEmpty(charSequenceArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(nullValue()));
                stringUtils.verify(() -> StringUtils.isNotEmpty("charSequence1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getBytesWhenStringIsNull}, hash: 3A3B36CC415644366E7D0FA1833773C4
    @Test()
    void getBytesWhenStringIsNull() {
        /* Branches:
         * (string == null) : true
         */
        //Arrange Statement(s)
        Charset charset = Charset.defaultCharset();
        //Act Statement(s)
        byte[] result = StringUtils.getBytes((String) null, charset);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${getBytesWhenStringIsNotNull}, hash: 77387EB87628766A113C1CF52AB7DD3C
    @Disabled()
    @Test()
    void getBytesWhenStringIsNotNull() {
        /* Branches:
         * (string == null) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Charset charset = Charset.defaultCharset();
        //Act Statement(s)
        byte[] result = StringUtils.getBytes("string1", charset);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${getBytes1WhenStringIsNull}, hash: 29B8275EC083F4F4CCB58C84736FEBD3
    @Test()
    void getBytes1WhenStringIsNull() throws UnsupportedEncodingException {
        /* Branches:
         * (string == null) : true
         */
        //Act Statement(s)
        byte[] result = StringUtils.getBytes((String) null, "charset1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${getBytes1WhenStringIsNotNull}, hash: C891C9C1AB59C32BFC202F1A401F65DF
    @Disabled()
    @Test()
    void getBytes1WhenStringIsNotNull() throws UnsupportedEncodingException {
        /* Branches:
         * (string == null) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        byte[] result = StringUtils.getBytes("C", "A");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${getCommonPrefixWhenArrayUtilsIsEmptyStrs}, hash: A5B68D6C62BD42C5484ACE0C070C706B
    @Test()
    void getCommonPrefixWhenArrayUtilsIsEmptyStrs() {
        /* Branches:
         * (ArrayUtils.isEmpty(strs)) : true
         */
        //Arrange Statement(s)
        String[] stringArray = new String[] {};
        //Act Statement(s)
        String result = StringUtils.getCommonPrefix(stringArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${getCommonPrefixWhen0IndexOfStrsIsNull}, hash: BFDBF0D06324B6CE6CE58AF84BA6E4BC
    @Test()
    void getCommonPrefixWhen0IndexOfStrsIsNull() {
        /* Branches:
         * (ArrayUtils.isEmpty(strs)) : false
         * (smallestIndexOfDiff == INDEX_NOT_FOUND) : true
         * (strs[0] == null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            String[] stringArray = new String[] { (String) null };
            stringUtils.when(() -> StringUtils.indexOfDifference(stringArray)).thenReturn(-1);
            //Act Statement(s)
            String result = StringUtils.getCommonPrefix(stringArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(""));
                stringUtils.verify(() -> StringUtils.indexOfDifference(stringArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getCommonPrefixWhen0IndexOfStrsIsNotNull}, hash: 8D3F7728DDB25A52516988A08E4346FC
    @Test()
    void getCommonPrefixWhen0IndexOfStrsIsNotNull() {
        /* Branches:
         * (ArrayUtils.isEmpty(strs)) : false
         * (smallestIndexOfDiff == INDEX_NOT_FOUND) : true
         * (strs[0] == null) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            String[] stringArray = new String[] { "strsItem1" };
            stringUtils.when(() -> StringUtils.indexOfDifference(stringArray)).thenReturn(-1);
            //Act Statement(s)
            String result = StringUtils.getCommonPrefix(stringArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("strsItem1"));
                stringUtils.verify(() -> StringUtils.indexOfDifference(stringArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getCommonPrefixWhenSmallestIndexOfDiffEquals0}, hash: 5F740CAB06116F8D30EC35FD9057F1F7
    @Disabled()
    @Test()
    void getCommonPrefixWhenSmallestIndexOfDiffEquals0() {
        /* Branches:
         * (ArrayUtils.isEmpty(strs)) : false
         * (smallestIndexOfDiff == INDEX_NOT_FOUND) : false
         * (smallestIndexOfDiff == 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            String[] stringArray = new String[] {};
            stringUtils.when(() -> StringUtils.indexOfDifference(stringArray)).thenReturn(0);
            //Act Statement(s)
            String result = StringUtils.getCommonPrefix(stringArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(""));
                stringUtils.verify(() -> StringUtils.indexOfDifference(stringArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getCommonPrefixWhenSmallestIndexOfDiffNotEquals0}, hash: B61695F4623E80B6B0D25094C2A47F6C
    @Test()
    void getCommonPrefixWhenSmallestIndexOfDiffNotEquals0() {
        /* Branches:
         * (ArrayUtils.isEmpty(strs)) : false
         * (smallestIndexOfDiff == INDEX_NOT_FOUND) : false
         * (smallestIndexOfDiff == 0) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            String[] stringArray = new String[] { "B" };
            stringUtils.when(() -> StringUtils.indexOfDifference(stringArray)).thenReturn(1);
            //Act Statement(s)
            String result = StringUtils.getCommonPrefix(stringArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("B"));
                stringUtils.verify(() -> StringUtils.indexOfDifference(stringArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getDigitsWhenIsEmptyStr}, hash: 831793484AB30E49686DB7C660FC8B5B
    @Test()
    void getDigitsWhenIsEmptyStr() {
        /* Branches:
         * (isEmpty(str)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(true);
            //Act Statement(s)
            String result = StringUtils.getDigits("str1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("str1"));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getDigitsWhenCharacterIsDigitTempChar}, hash: 41E3BC458BC623A22DED9A7DA755A4CD
    @Disabled()
    @Test()
    void getDigitsWhenCharacterIsDigitTempChar() {
        /* Branches:
         * (isEmpty(str)) : false
         * (i < sz) : true
         * (Character.isDigit(tempChar)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.getDigits("str1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getFuzzyDistanceWhenQueryIsNullThrowsIllegalArgumentException}, hash: 57EB611A8484CBEA2E094C84E2D48A62
    @Test()
    void getFuzzyDistanceWhenQueryIsNullThrowsIllegalArgumentException() {
        /* Branches:
         * (term == null) : false
         * (query == null) : true
         */
        //Arrange Statement(s)
        CharSequence charSequence = null;
        Locale locale = new Locale("language1");
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("Strings must not be null");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            StringUtils.getFuzzyDistance("term1", charSequence, locale);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getFuzzyDistanceWhenLocaleIsNullThrowsIllegalArgumentException}, hash: 8C068AC9E3346C24432D7168763A45C3
    @Test()
    void getFuzzyDistanceWhenLocaleIsNullThrowsIllegalArgumentException() {
        /* Branches:
         * (term == null) : false
         * (query == null) : false
         * (locale == null) : true
         */
        //Arrange Statement(s)
        Locale locale = null;
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("Locale must not be null");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            StringUtils.getFuzzyDistance("term1", "query1", locale);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getFuzzyDistanceWhenPreviousMatchingCharacterIndexPlus1NotEqualsTermIndex}, hash: ED4CCD8CE81FA69DFFE45E9C3CA34E6B
    @Disabled()
    @Test()
    void getFuzzyDistanceWhenPreviousMatchingCharacterIndexPlus1NotEqualsTermIndex() {
        /* Branches:
         * (term == null) : false
         * (query == null) : false
         * (locale == null) : false
         * (queryIndex < queryLowerCase.length()) : true
         * (termIndex < termLowerCase.length()) : true
         * (!termCharacterMatchFound) : true
         * (queryChar == termChar) : true
         * (previousMatchingCharacterIndex + 1 == termIndex) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Locale locale = new Locale("language1");
        //Act Statement(s)
        int result = StringUtils.getFuzzyDistance("term1", "query1", locale);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(4)));
    }

    //BaseRock generated method id: ${getIfBlankWhenIsBlankStr}, hash: 5A6F0A4AE820ADC0BB36C44D93FCF86C
    @Disabled()
    @Test()
    void getIfBlankWhenIsBlankStr() {
        /* Branches:
         * (isBlank(str)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isBlank("str1")).thenReturn(true);
            //Act Statement(s)
            CharSequence result = StringUtils.getIfBlank("str1", supplierMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("charSequence1"));
                stringUtils.verify(() -> StringUtils.isBlank("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getIfBlankWhenIsBlankNotStr}, hash: 5B7E3F0B4FA5F6A053A997161B11A47A
    @Test()
    void getIfBlankWhenIsBlankNotStr() {
        /* Branches:
         * (isBlank(str)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isBlank("str1")).thenReturn(false);
            //Act Statement(s)
            CharSequence result = StringUtils.getIfBlank("str1", supplierMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("str1"));
                stringUtils.verify(() -> StringUtils.isBlank("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getIfEmptyWhenIsEmptyStr}, hash: 8B2F303AF792EE0709E93F63DD15D31A
    @Disabled()
    @Test()
    void getIfEmptyWhenIsEmptyStr() {
        /* Branches:
         * (isEmpty(str)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(true);
            //Act Statement(s)
            CharSequence result = StringUtils.getIfEmpty("str1", supplierMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("charSequence1"));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getIfEmptyWhenIsEmptyNotStr}, hash: DBFC57E5C4ECF4164AF8328E6065A17D
    @Test()
    void getIfEmptyWhenIsEmptyNotStr() {
        /* Branches:
         * (isEmpty(str)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(false);
            //Act Statement(s)
            CharSequence result = StringUtils.getIfEmpty("str1", supplierMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("str1"));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getJaroWinklerDistanceWhenSecondIsNullThrowsIllegalArgumentException}, hash: C9195FF0404CD2BBE565FD202C9F7B14
    @Test()
    void getJaroWinklerDistanceWhenSecondIsNullThrowsIllegalArgumentException() {
        /* Branches:
         * (first == null) : false
         * (second == null) : true
         */
        //Arrange Statement(s)
        CharSequence charSequence = null;
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("Strings must not be null");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            StringUtils.getJaroWinklerDistance("first1", charSequence);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getJaroWinklerDistanceWhenMiLessThanMinLengthAndFirstCharAtMiNotEqualsSecondCharAtMiAndMEquals04}, hash: B337CABADE36544B2CE18BB77C68AA0F
    @Disabled()
    @Test()
    void getJaroWinklerDistanceWhenMiLessThanMinLengthAndFirstCharAtMiNotEqualsSecondCharAtMiAndMEquals04() {
        /* Branches:
         * (first == null) : false
         * (second == null) : false
         * (first.length() > second.length()) : true  #  inside matches method
         * (mi < min.length()) : true  #  inside matches method
         * (xi < xn) : true  #  inside matches method
         * (!matchFlags[xi]) : true  #  inside matches method
         * (c1 == max.charAt(xi)) : false  #  inside matches method
         * (i < min.length()) : true  #  inside matches method
         * (matchIndexes[i] != -1) : false  #  inside matches method
         * (i < max.length()) : true  #  inside matches method
         * (matchFlags[i]) : false  #  inside matches method
         * (mi < ms1.length) : false  #  inside matches method
         * (mi < min.length()) : true  #  inside matches method
         * (first.charAt(mi) != second.charAt(mi)) : true  #  inside matches method
         * (m == 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        double result = StringUtils.getJaroWinklerDistance("first1", "second1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, closeTo(Double.parseDouble("0.0"), 0.00001)));
    }

    //BaseRock generated method id: ${getJaroWinklerDistanceWhenJLessThan0_7D}, hash: 0FB7CE59B8D4DA79296D604E5CABB625
    @Disabled()
    @Test()
    void getJaroWinklerDistanceWhenJLessThan0_7D() {
        /* Branches:
         * (first == null) : false
         * (second == null) : false
         * (first.length() > second.length()) : true  #  inside matches method
         * (mi < min.length()) : true  #  inside matches method
         * (xi < xn) : true  #  inside matches method
         * (!matchFlags[xi]) : true  #  inside matches method
         * (c1 == max.charAt(xi)) : true  #  inside matches method
         * (i < min.length()) : true  #  inside matches method
         * (matchIndexes[i] != -1) : true  #  inside matches method
         * (i < max.length()) : true  #  inside matches method
         * (matchFlags[i]) : false  #  inside matches method
         * (mi < ms1.length) : true  #  inside matches method
         * (ms1[mi] != ms2[mi]) : true  #  inside matches method
         * (mi < min.length()) : true  #  inside matches method
         * (first.charAt(mi) != second.charAt(mi)) : true  #  inside matches method
         * (m == 0) : false
         * (j < 0.7D) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        double result = StringUtils.getJaroWinklerDistance("first1", "second1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, closeTo(Double.parseDouble("0.0"), 0.00001)));
    }

    //BaseRock generated method id: ${getJaroWinklerDistanceWhenMiIndexOfMs1NotEqualsMiIndexOfMs2AndMiLessThanMinLengthAndFirstCharAtMiEqualsSecondCharAtMiAn}, hash: FABFCA777E09531CE1FA48BCBB9FE35A
    @Disabled()
    @Test()
    void getJaroWinklerDistanceWhenMiIndexOfMs1NotEqualsMiIndexOfMs2AndMiLessThanMinLengthAndFirstCharAtMiEqualsSecondCharAtMiAn() {
        /* Branches:
         * (first == null) : false
         * (second == null) : false
         * (first.length() > second.length()) : true  #  inside matches method
         * (mi < min.length()) : true  #  inside matches method
         * (xi < xn) : true  #  inside matches method
         * (!matchFlags[xi]) : true  #  inside matches method
         * (c1 == max.charAt(xi)) : true  #  inside matches method
         * (i < min.length()) : true  #  inside matches method
         * (matchIndexes[i] != -1) : true  #  inside matches method
         * (i < max.length()) : true  #  inside matches method
         * (matchFlags[i]) : false  #  inside matches method
         * (mi < ms1.length) : true  #  inside matches method
         * (ms1[mi] != ms2[mi]) : true  #  inside matches method
         * (mi < min.length()) : true  #  inside matches method
         * (first.charAt(mi) != second.charAt(mi)) : false  #  inside matches method
         * (m == 0) : false
         * (j < 0.7D) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        double result = StringUtils.getJaroWinklerDistance("first1", "second1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, closeTo(Double.parseDouble("0.0"), 0.00001)));
    }

    //BaseRock generated method id: ${getLevenshteinDistanceWhenTIsNullThrowsIllegalArgumentException}, hash: 0AFA182FCC2D5B9A52ABB294C4210CF6
    @Test()
    void getLevenshteinDistanceWhenTIsNullThrowsIllegalArgumentException() {
        /* Branches:
         * (s == null) : false
         * (t == null) : true
         */
        //Arrange Statement(s)
        CharSequence charSequence = null;
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("Strings must not be null");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            StringUtils.getLevenshteinDistance("s1", charSequence);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getLevenshteinDistanceWhenNEquals0}, hash: 3D54F1BE5C5F619746154D73B08FAD1E
    @Disabled()
    @Test()
    void getLevenshteinDistanceWhenNEquals0() {
        /* Branches:
         * (s == null) : false
         * (t == null) : false
         * (n == 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        int result = StringUtils.getLevenshteinDistance("s1", "t1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${getLevenshteinDistanceWhenMEquals0}, hash: 69387CE22B0380E640D48518E0DF20F7
    @Test()
    void getLevenshteinDistanceWhenMEquals0() {
        /* Branches:
         * (s == null) : false
         * (t == null) : false
         * (n == 0) : false
         * (m == 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        int result = StringUtils.getLevenshteinDistance("s1", "t1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1)));
    }

    //BaseRock generated method id: ${getLevenshteinDistanceWhenSCharAtIMinus1EqualsJOfT}, hash: 334120AA208E454828821968F3CAFC8F
    @Disabled()
    @Test()
    void getLevenshteinDistanceWhenSCharAtIMinus1EqualsJOfT() {
        /* Branches:
         * (s == null) : false
         * (t == null) : false
         * (n == 0) : false
         * (m == 0) : false
         * (n > m) : true
         * (i <= n) : true
         * (j <= m) : true
         * (i <= n) : true
         * (s.charAt(i - 1) == jOfT) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        int result = StringUtils.getLevenshteinDistance("s1", "t1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${getLevenshteinDistance1WhenTIsNullThrowsIllegalArgumentException}, hash: D8F607538DD02D05B30413176BDDF24A
    @Test()
    void getLevenshteinDistance1WhenTIsNullThrowsIllegalArgumentException() {
        /* Branches:
         * (s == null) : false
         * (t == null) : true
         */
        //Arrange Statement(s)
        CharSequence charSequence = null;
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("Strings must not be null");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            StringUtils.getLevenshteinDistance("s1", charSequence, 0);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getLevenshteinDistance1WhenThresholdLessThan0ThrowsIllegalArgumentException}, hash: 66C0BA79DB90044D60C85437A6BBCE45
    @Test()
    void getLevenshteinDistance1WhenThresholdLessThan0ThrowsIllegalArgumentException() {
        /* Branches:
         * (s == null) : false
         * (t == null) : false
         * (threshold < 0) : true
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("Threshold must not be negative");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            StringUtils.getLevenshteinDistance("s1", "t1", -2147483648);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getLevenshteinDistance1WhenMLessThanOrEqualsToThreshold}, hash: DFAF16B508356470D95418EE9860A852
    @Disabled()
    @Test()
    void getLevenshteinDistance1WhenMLessThanOrEqualsToThreshold() {
        /* Branches:
         * (s == null) : false
         * (t == null) : false
         * (threshold < 0) : false
         * (n == 0) : true
         * (m <= threshold) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        int result = StringUtils.getLevenshteinDistance("s1", "t1", 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${getLevenshteinDistance1WhenMGreaterThanThreshold}, hash: 3B729F15D1CB0A6ECD1EEE532F6E7C1F
    @Test()
    void getLevenshteinDistance1WhenMGreaterThanThreshold() {
        /* Branches:
         * (s == null) : false
         * (t == null) : false
         * (threshold < 0) : false
         * (n == 0) : true
         * (m <= threshold) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        int result = StringUtils.getLevenshteinDistance("s1", "t1", 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${getLevenshteinDistance1WhenNLessThanOrEqualsToThreshold}, hash: EEB0BB45227F8E9766B0DFA01111E642
    @Test()
    void getLevenshteinDistance1WhenNLessThanOrEqualsToThreshold() {
        /* Branches:
         * (s == null) : false
         * (t == null) : false
         * (threshold < 0) : false
         * (n == 0) : false
         * (m == 0) : true
         * (n <= threshold) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        int result = StringUtils.getLevenshteinDistance("s1", "t1", 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1)));
    }

    //BaseRock generated method id: ${getLevenshteinDistance1WhenMathAbsNMinusMGreaterThanThreshold}, hash: FB0CA1F058A0572E710BAFB00EAA7A89
    @Disabled()
    @Test()
    void getLevenshteinDistance1WhenMathAbsNMinusMGreaterThanThreshold() {
        /* Branches:
         * (s == null) : false
         * (t == null) : false
         * (threshold < 0) : false
         * (n == 0) : false
         * (m == 0) : false
         * (Math.abs(n - m) > threshold) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        int result = StringUtils.getLevenshteinDistance("s1", "t1", 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${getLevenshteinDistance1WhenMinGreaterThanMax}, hash: 043A7E0AD6A82BD595FC921612BB744A
    @Test()
    void getLevenshteinDistance1WhenMinGreaterThanMax() {
        /* Branches:
         * (s == null) : false
         * (t == null) : false
         * (threshold < 0) : false
         * (n == 0) : false
         * (m == 0) : false
         * (Math.abs(n - m) > threshold) : false
         * (n > m) : true
         * (i < boundary) : true
         * (j <= m) : true
         * (j > Integer.MAX_VALUE - threshold) : false
         * (min > max) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        int result = StringUtils.getLevenshteinDistance("s1", "t1", 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${getLevenshteinDistance1WhenILessThanOrEqualsToMaxAndSCharAtIMinus1NotEqualsJOfTAndNIndexOfPLessThanOrEqualsToThreshold}, hash: A724C254B3B5CCB71ABA8436CCA4F11E
    @Disabled()
    @Test()
    void getLevenshteinDistance1WhenILessThanOrEqualsToMaxAndSCharAtIMinus1NotEqualsJOfTAndNIndexOfPLessThanOrEqualsToThreshold() {
        /* Branches:
         * (s == null) : false
         * (t == null) : false
         * (threshold < 0) : false
         * (n == 0) : false
         * (m == 0) : false
         * (Math.abs(n - m) > threshold) : false
         * (n > m) : true
         * (i < boundary) : true
         * (j <= m) : true
         * (j > Integer.MAX_VALUE - threshold) : false
         * (min > max) : false
         * (min > 1) : false
         * (i <= max) : true
         * (s.charAt(i - 1) == jOfT) : false
         * (p[n] <= threshold) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        int result = StringUtils.getLevenshteinDistance("s1", "t1", 7579);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${getLevenshteinDistance1WhenMinGreaterThan1AndILessThanOrEqualsToMaxAndSCharAtIMinus1EqualsJOfTAndNIndexOfPLessThanOrEqu}, hash: 1166E89BD86463656A451BF16B7CF029
    @Disabled()
    @Test()
    void getLevenshteinDistance1WhenMinGreaterThan1AndILessThanOrEqualsToMaxAndSCharAtIMinus1EqualsJOfTAndNIndexOfPLessThanOrEqu() {
        /* Branches:
         * (s == null) : false
         * (t == null) : false
         * (threshold < 0) : false
         * (n == 0) : false
         * (m == 0) : false
         * (Math.abs(n - m) > threshold) : false
         * (n > m) : true
         * (i < boundary) : false
         * (j <= m) : true
         * (j > Integer.MAX_VALUE - threshold) : true
         * (min > max) : false
         * (min > 1) : true
         * (i <= max) : true
         * (s.charAt(i - 1) == jOfT) : true
         * (p[n] <= threshold) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        int result = StringUtils.getLevenshteinDistance("s1", "t1", 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${indexOfTest}, hash: 1A4FDA3741ECBD4C261568DA3586BE57
    @Test()
    void indexOfTest() {
        //Act Statement(s)
        int result = StringUtils.indexOf((CharSequence) "seq1", (CharSequence) "searchSeq1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${indexOf1Test}, hash: A6D00B4C917EB16F92C24830CAB668BA
    @Test()
    void indexOf1Test() {
        //Act Statement(s)
        int result = StringUtils.indexOf((CharSequence) "seq1", (CharSequence) "searchSeq1", 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${indexOf2WhenIsEmptySeq}, hash: 2BDAB7BC2CA54EC0CC4BFBF6DBF95443
    @Test()
    void indexOf2WhenIsEmptySeq() {
        /* Branches:
         * (isEmpty(seq)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("seq1")).thenReturn(true);
            //Act Statement(s)
            int result = StringUtils.indexOf((CharSequence) "seq1", 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(-1));
                stringUtils.verify(() -> StringUtils.isEmpty("seq1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOf2WhenIsEmptyNotSeq}, hash: C24252B4718A21DE8EF9E2081C0A38B6
    @Disabled()
    @Test()
    void indexOf2WhenIsEmptyNotSeq() {
        /* Branches:
         * (isEmpty(seq)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("seq1")).thenReturn(false);
            //Act Statement(s)
            int result = StringUtils.indexOf((CharSequence) "seq1", 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                stringUtils.verify(() -> StringUtils.isEmpty("seq1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOf3WhenIsEmptySeq}, hash: BAE38B208D12F9949AC4556E2DF01C5B
    @Test()
    void indexOf3WhenIsEmptySeq() {
        /* Branches:
         * (isEmpty(seq)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("seq1")).thenReturn(true);
            //Act Statement(s)
            int result = StringUtils.indexOf((CharSequence) "seq1", 0, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(-1));
                stringUtils.verify(() -> StringUtils.isEmpty("seq1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOf3WhenIsEmptyNotSeq}, hash: B7388C2B9639BB2D84EEBED0DE985BEC
    @Disabled()
    @Test()
    void indexOf3WhenIsEmptyNotSeq() {
        /* Branches:
         * (isEmpty(seq)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("seq1")).thenReturn(false);
            //Act Statement(s)
            int result = StringUtils.indexOf((CharSequence) "seq1", 0, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                stringUtils.verify(() -> StringUtils.isEmpty("seq1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOfAnyWhenArrayUtilsIsEmptySearchChars}, hash: F6E1612BC09CE0AE85E5317D9193D25A
    @Test()
    void indexOfAnyWhenArrayUtilsIsEmptySearchChars() {
        /* Branches:
         * (isEmpty(cs)) : false
         * (ArrayUtils.isEmpty(searchChars)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("cs1")).thenReturn(false);
            char[] charArray = new char[] {};
            //Act Statement(s)
            int result = StringUtils.indexOfAny((CharSequence) "cs1", charArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(-1));
                stringUtils.verify(() -> StringUtils.isEmpty("cs1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOfAnyWhenJPlus1IndexOfSearchCharsEqualsCsCharAtIPlus1}, hash: 8680B22F71CAEF77BDA3CEF6B9CDB163
    @Disabled()
    @Test()
    void indexOfAnyWhenJPlus1IndexOfSearchCharsEqualsCsCharAtIPlus1() {
        /* Branches:
         * (isEmpty(cs)) : false
         * (ArrayUtils.isEmpty(searchChars)) : false
         * (i < csLen) : true
         * (j < searchLen) : true
         * (searchChars[j] == ch) : true
         * (i >= csLast) : false
         * (j >= searchLast) : false
         * (!Character.isHighSurrogate(ch)) : false
         * (searchChars[j + 1] == cs.charAt(i + 1)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("cs1")).thenReturn(false);
            char[] charArray = new char[] { 'A', 'A' };
            //Act Statement(s)
            int result = StringUtils.indexOfAny((CharSequence) "cs1", charArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                stringUtils.verify(() -> StringUtils.isEmpty("cs1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOfAnyWhenJPlus1IndexOfSearchCharsNotEqualsCsCharAtIPlus1}, hash: B7E555F28B82E53114700A9E8D2D2925
    @Test()
    void indexOfAnyWhenJPlus1IndexOfSearchCharsNotEqualsCsCharAtIPlus1() {
        /* Branches:
         * (isEmpty(cs)) : false
         * (ArrayUtils.isEmpty(searchChars)) : false
         * (i < csLen) : true
         * (j < searchLen) : true
         * (searchChars[j] == ch) : true
         * (i >= csLast) : false
         * (j >= searchLast) : false
         * (!Character.isHighSurrogate(ch)) : false
         * (searchChars[j + 1] == cs.charAt(i + 1)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("cs1")).thenReturn(false);
            char[] charArray = new char[] { 'A', 'A' };
            //Act Statement(s)
            int result = StringUtils.indexOfAny((CharSequence) "cs1", charArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(-1));
                stringUtils.verify(() -> StringUtils.isEmpty("cs1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOfAny1WhenSearchStrsIsNull}, hash: B4540270131A643C4F1354DDED5996D7
    @Test()
    void indexOfAny1WhenSearchStrsIsNull() {
        /* Branches:
         * (str == null) : false
         * (searchStrs == null) : true
         */
        //Arrange Statement(s)
        CharSequence[] charSequence = null;
        //Act Statement(s)
        int result = StringUtils.indexOfAny((CharSequence) "str1", charSequence);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${indexOfAny1WhenSearchIsNullAndRetEqualsIntegerMAX_VALUE}, hash: F2B3C4AF09890081CD769896EB8EB161
    @Test()
    void indexOfAny1WhenSearchIsNullAndRetEqualsIntegerMAX_VALUE() {
        /* Branches:
         * (str == null) : false
         * (searchStrs == null) : false
         * (for-each(searchStrs)) : true
         * (search == null) : true
         * (ret == Integer.MAX_VALUE) : true
         */
        //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] { (CharSequence) null };
        //Act Statement(s)
        int result = StringUtils.indexOfAny((CharSequence) "str1", charSequenceArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${indexOfAny1WhenTmpEqualsINDEX_NOT_FOUNDAndRetEqualsIntegerMAX_VALUE}, hash: 236C97A5C053732073318F8ACCC6EB2F
    @Test()
    void indexOfAny1WhenTmpEqualsINDEX_NOT_FOUNDAndRetEqualsIntegerMAX_VALUE() {
        /* Branches:
         * (str == null) : false
         * (searchStrs == null) : false
         * (for-each(searchStrs)) : true
         * (search == null) : false
         * (tmp == INDEX_NOT_FOUND) : true
         * (ret == Integer.MAX_VALUE) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] { "charSequence1" };
        //Act Statement(s)
        int result = StringUtils.indexOfAny((CharSequence) "str1", charSequenceArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${indexOfAny1WhenRetNotEqualsIntegerMAX_VALUE}, hash: 31C0816DB9A430F0914F6D7685371F16
    @Disabled()
    @Test()
    void indexOfAny1WhenRetNotEqualsIntegerMAX_VALUE() {
        /* Branches:
         * (str == null) : false
         * (searchStrs == null) : false
         * (for-each(searchStrs)) : true
         * (search == null) : false
         * (tmp == INDEX_NOT_FOUND) : false
         * (tmp < ret) : true
         * (ret == Integer.MAX_VALUE) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] { "charSequence1" };
        //Act Statement(s)
        int result = StringUtils.indexOfAny((CharSequence) "str1", charSequenceArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${indexOfAny2WhenIsEmptySearchChars}, hash: 23C3CC33FC4A4B33B1B7864C4E80EC40
    @Test()
    void indexOfAny2WhenIsEmptySearchChars() {
        /* Branches:
         * (isEmpty(cs)) : false
         * (isEmpty(searchChars)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("cs1")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("searchChars1")).thenReturn(true);
            //Act Statement(s)
            int result = StringUtils.indexOfAny((CharSequence) "cs1", "searchChars1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(-1));
                stringUtils.verify(() -> StringUtils.isEmpty("cs1"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("searchChars1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOfAny2WhenIsEmptyNotSearchChars}, hash: 2F54A60ED44787C766EAE259A27E56B6
    @Test()
    void indexOfAny2WhenIsEmptyNotSearchChars() {
        /* Branches:
         * (isEmpty(cs)) : false
         * (isEmpty(searchChars)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("cs1")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("A")).thenReturn(false);
            char[] charArray = new char[] { 'A' };
            stringUtils.when(() -> StringUtils.indexOfAny("cs1", charArray)).thenReturn(0);
            //Act Statement(s)
            int result = StringUtils.indexOfAny((CharSequence) "cs1", "A");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                stringUtils.verify(() -> StringUtils.isEmpty("cs1"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("A"), atLeast(1));
                stringUtils.verify(() -> StringUtils.indexOfAny("cs1", charArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOfAnyButWhenArrayUtilsIsEmptySearchChars}, hash: 6030BEB81001F3AE0CA54E1FCFA5029B
    @Test()
    void indexOfAnyButWhenArrayUtilsIsEmptySearchChars() {
        /* Branches:
         * (isEmpty(cs)) : false
         * (ArrayUtils.isEmpty(searchChars)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("cs1")).thenReturn(false);
            char[] charArray = new char[] {};
            //Act Statement(s)
            int result = StringUtils.indexOfAnyBut((CharSequence) "cs1", charArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(-1));
                stringUtils.verify(() -> StringUtils.isEmpty("cs1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOfAnyButWhenArrayUtilsNotIsEmptySearchChars}, hash: 39D64E8E2BC936DE210F7FFDAE8229D7
    @Disabled()
    @Test()
    void indexOfAnyButWhenArrayUtilsNotIsEmptySearchChars() {
        /* Branches:
         * (isEmpty(cs)) : false
         * (ArrayUtils.isEmpty(searchChars)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("cs1")).thenReturn(false);
            stringUtils.when(() -> StringUtils.indexOfAnyBut(eq("cs1"), (CharBuffer) any())).thenReturn(0);
            char[] charArray = new char[] {};
            //Act Statement(s)
            int result = StringUtils.indexOfAnyBut((CharSequence) "cs1", charArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                stringUtils.verify(() -> StringUtils.isEmpty("cs1"), atLeast(1));
                stringUtils.verify(() -> StringUtils.indexOfAnyBut(eq("cs1"), (CharBuffer) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOfAnyBut1WhenIsEmptySearchChars}, hash: D05B29F91F8A7B5CAC0607C4DE9C3039
    @Test()
    void indexOfAnyBut1WhenIsEmptySearchChars() {
        /* Branches:
         * (isEmpty(seq)) : false
         * (isEmpty(searchChars)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("seq1")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("searchChars1")).thenReturn(true);
            //Act Statement(s)
            int result = StringUtils.indexOfAnyBut((CharSequence) "seq1", (CharSequence) "searchChars1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(-1));
                stringUtils.verify(() -> StringUtils.isEmpty("seq1"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("searchChars1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOfAnyBut1WhenSearchSetCodePointsNotContainsCurSeqCodePoint}, hash: 4C99B811BA4EC97B66774D78397268A0
    @Disabled()
    @Test()
    void indexOfAnyBut1WhenSearchSetCodePointsNotContainsCurSeqCodePoint() {
        /* Branches:
         * (isEmpty(seq)) : false
         * (isEmpty(searchChars)) : false
         * (curSeqCharIdx < seq.length()) : true
         * (!searchSetCodePoints.contains(curSeqCodePoint)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("seq1")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("searchChars1")).thenReturn(false);
            //Act Statement(s)
            int result = StringUtils.indexOfAnyBut((CharSequence) "seq1", (CharSequence) "searchChars1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                stringUtils.verify(() -> StringUtils.isEmpty("seq1"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("searchChars1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOfAnyBut1WhenSearchSetCodePointsContainsCurSeqCodePoint}, hash: 4974480F0AE66D3A643F0B0590121CD3
    @Disabled()
    @Test()
    void indexOfAnyBut1WhenSearchSetCodePointsContainsCurSeqCodePoint() {
        /* Branches:
         * (isEmpty(seq)) : false
         * (isEmpty(searchChars)) : false
         * (curSeqCharIdx < seq.length()) : true
         * (!searchSetCodePoints.contains(curSeqCodePoint)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("seq1")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("searchChars1")).thenReturn(false);
            //Act Statement(s)
            int result = StringUtils.indexOfAnyBut((CharSequence) "seq1", (CharSequence) "searchChars1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(-1));
                stringUtils.verify(() -> StringUtils.isEmpty("seq1"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("searchChars1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOfDifferenceWhenArrayUtilsGetLengthCssLessThanOrEqualsTo1}, hash: 68E2E825DFC05A75120FD19A80A5CCFD
    @Test()
    void indexOfDifferenceWhenArrayUtilsGetLengthCssLessThanOrEqualsTo1() {
        /* Branches:
         * (ArrayUtils.getLength(css) <= 1) : true
         */
        //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] {};
        //Act Statement(s)
        int result = StringUtils.indexOfDifference(charSequenceArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${indexOfDifferenceWhenCsIsNullAndAllStringsNull}, hash: 400DA0DDAA5FB8A89FE24FD228660E15
    @Test()
    void indexOfDifferenceWhenCsIsNullAndAllStringsNull() {
        /* Branches:
         * (ArrayUtils.getLength(css) <= 1) : false
         * (for-each(css)) : true
         * (cs == null) : true
         * (allStringsNull) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] { (CharSequence) null };
        //Act Statement(s)
        int result = StringUtils.indexOfDifference(charSequenceArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${indexOfDifferenceWhenNotAnyStringNull}, hash: 34C644E7628B2BC02A8E40113B4BE1CE
    @Test()
    void indexOfDifferenceWhenNotAnyStringNull() {
        /* Branches:
         * (ArrayUtils.getLength(css) <= 1) : false
         * (for-each(css)) : true
         * (cs == null) : false
         * (allStringsNull) : false
         * (longestStrLen == 0) : true
         * (!anyStringNull) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] { "charSequence1" };
        //Act Statement(s)
        int result = StringUtils.indexOfDifference(charSequenceArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${indexOfDifferenceWhenShortestStrLenEquals0}, hash: 6CE32EA24EA69D6989DD9FD5D8E21FB9
    @Disabled()
    @Test()
    void indexOfDifferenceWhenShortestStrLenEquals0() {
        /* Branches:
         * (ArrayUtils.getLength(css) <= 1) : false
         * (for-each(css)) : true
         * (cs == null) : false
         * (allStringsNull) : false
         * (longestStrLen == 0) : false
         * (shortestStrLen == 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] { "charSequence1" };
        //Act Statement(s)
        int result = StringUtils.indexOfDifference(charSequenceArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${indexOfDifferenceWhenFirstDiffNotEqualsMinus1}, hash: 8D0B9CEAEEC72F6679DE0635DF692304
    @Disabled()
    @Test()
    void indexOfDifferenceWhenFirstDiffNotEqualsMinus1() {
        /* Branches:
         * (ArrayUtils.getLength(css) <= 1) : false
         * (for-each(css)) : true
         * (cs == null) : false
         * (allStringsNull) : false
         * (longestStrLen == 0) : false
         * (shortestStrLen == 0) : false
         * (stringPos < shortestStrLen) : true
         * (arrayPos < arrayLen) : true
         * (css[arrayPos].charAt(stringPos) != comparisonChar) : true
         * (firstDiff != -1) : true
         * (firstDiff == -1) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] { "charSequence1", "charSequence3" };
        //Act Statement(s)
        int result = StringUtils.indexOfDifference(charSequenceArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${indexOfDifferenceWhenFirstDiffEqualsMinus1AndFirstDiffEqualsMinus1AndShortestStrLenNotEqualsLongestStrLen}, hash: 74F73FC2EE7CD363898636F69A79211F
    @Disabled()
    @Test()
    void indexOfDifferenceWhenFirstDiffEqualsMinus1AndFirstDiffEqualsMinus1AndShortestStrLenNotEqualsLongestStrLen() {
        /* Branches:
         * (ArrayUtils.getLength(css) <= 1) : false
         * (for-each(css)) : true
         * (cs == null) : false
         * (allStringsNull) : false
         * (longestStrLen == 0) : false
         * (shortestStrLen == 0) : false
         * (stringPos < shortestStrLen) : true
         * (arrayPos < arrayLen) : true
         * (css[arrayPos].charAt(stringPos) != comparisonChar) : false
         * (firstDiff != -1) : false
         * (firstDiff == -1) : true
         * (shortestStrLen != longestStrLen) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] { "charSequence1", "charSequence3" };
        //Act Statement(s)
        int result = StringUtils.indexOfDifference(charSequenceArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1)));
    }

    //BaseRock generated method id: ${indexOfDifference1WhenCs1EqualsCs2}, hash: AC6DC8597481FCD9D843D4B26FBE52B7
    @Test()
    void indexOfDifference1WhenCs1EqualsCs2() {
        /* Branches:
         * (cs1 == cs2) : true
         */
        //Act Statement(s)
        int result = StringUtils.indexOfDifference("cs1", "cs1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${indexOfDifference1WhenCs2IsNull}, hash: B10F7BBAA19E1E2FDA4F54425505A159
    @Test()
    void indexOfDifference1WhenCs2IsNull() {
        /* Branches:
         * (cs1 == cs2) : false
         * (cs1 == null) : false
         * (cs2 == null) : true
         */
        //Arrange Statement(s)
        CharSequence charSequence = null;
        //Act Statement(s)
        int result = StringUtils.indexOfDifference("cs1", charSequence);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${indexOfDifference1WhenINotLessThanCs1Length}, hash: 5BC641C3F98AE8FE0CED4AE3C507F150
    @Disabled()
    @Test()
    void indexOfDifference1WhenINotLessThanCs1Length() {
        /* Branches:
         * (cs1 == cs2) : false
         * (cs1 == null) : false
         * (cs2 == null) : false
         * (i < cs1.length()) : true
         * (i < cs2.length()) : true
         * (cs1.charAt(i) != cs2.charAt(i)) : true
         * (i < cs2.length()) : false
         * (i < cs1.length()) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        int result = StringUtils.indexOfDifference("cs1", "cs2");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${indexOfDifference1WhenINotLessThanCs2LengthAndILessThanCs1Length}, hash: 99B7104372C261CE34751BE5A9E67765
    @Disabled()
    @Test()
    void indexOfDifference1WhenINotLessThanCs2LengthAndILessThanCs1Length() {
        /* Branches:
         * (cs1 == cs2) : false
         * (cs1 == null) : false
         * (cs2 == null) : false
         * (i < cs1.length()) : true
         * (i < cs2.length()) : true
         * (cs1.charAt(i) != cs2.charAt(i)) : false
         * (i < cs2.length()) : false
         * (i < cs1.length()) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        int result = StringUtils.indexOfDifference("cs1", "cs2");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1)));
    }

    //BaseRock generated method id: ${indexOfIgnoreCaseTest}, hash: 0DAF7B80AAB82F607E62114E2A1C4717
    @Test()
    void indexOfIgnoreCaseTest() {
        //Act Statement(s)
        int result = StringUtils.indexOfIgnoreCase("str1", "searchStr1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${indexOfIgnoreCase1Test}, hash: C306F00FF0A2E7A54BFF440EE4F9C1A5
    @Test()
    void indexOfIgnoreCase1Test() {
        //Act Statement(s)
        int result = StringUtils.indexOfIgnoreCase("str1", "searchStr1", 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${isAllBlankWhenArrayUtilsIsEmptyCss}, hash: 0A30C49A7034F160AA114EAD46F767C8
    @Test()
    void isAllBlankWhenArrayUtilsIsEmptyCss() {
        /* Branches:
         * (ArrayUtils.isEmpty(css)) : true
         */
        //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] {};
        //Act Statement(s)
        boolean result = StringUtils.isAllBlank(charSequenceArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isAllBlankWhenIsNotBlankCs}, hash: C60743FFA2E36E733BF02075B557BD0E
    @Test()
    void isAllBlankWhenIsNotBlankCs() {
        /* Branches:
         * (ArrayUtils.isEmpty(css)) : false
         * (for-each(css)) : true
         * (isNotBlank(cs)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isNotBlank("charSequence1")).thenReturn(true);
            CharSequence[] charSequenceArray = new CharSequence[] { "charSequence1" };
            //Act Statement(s)
            boolean result = StringUtils.isAllBlank(charSequenceArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.isNotBlank("charSequence1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAllBlankWhenIsNotBlankNotCs}, hash: 39EA701F4C1D5A6F6288444DBC750DBC
    @Test()
    void isAllBlankWhenIsNotBlankNotCs() {
        /* Branches:
         * (ArrayUtils.isEmpty(css)) : false
         * (for-each(css)) : true
         * (isNotBlank(cs)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isNotBlank("charSequence1")).thenReturn(false);
            CharSequence[] charSequenceArray = new CharSequence[] { "charSequence1" };
            //Act Statement(s)
            boolean result = StringUtils.isAllBlank(charSequenceArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                stringUtils.verify(() -> StringUtils.isNotBlank("charSequence1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAllEmptyWhenArrayUtilsIsEmptyCss}, hash: A1DBA05299508A29AE1411B2DE3FFDF3
    @Test()
    void isAllEmptyWhenArrayUtilsIsEmptyCss() {
        /* Branches:
         * (ArrayUtils.isEmpty(css)) : true
         */
        //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] {};
        //Act Statement(s)
        boolean result = StringUtils.isAllEmpty(charSequenceArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isAllEmptyWhenIsNotEmptyCs}, hash: 03738C2377D475BCC0E06410D7988D51
    @Test()
    void isAllEmptyWhenIsNotEmptyCs() {
        /* Branches:
         * (ArrayUtils.isEmpty(css)) : false
         * (for-each(css)) : true
         * (isNotEmpty(cs)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isNotEmpty("charSequence1")).thenReturn(true);
            CharSequence[] charSequenceArray = new CharSequence[] { "charSequence1" };
            //Act Statement(s)
            boolean result = StringUtils.isAllEmpty(charSequenceArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.isNotEmpty("charSequence1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAllEmptyWhenIsNotEmptyNotCs}, hash: EF5FAB6907549AF5712CAC16E0828D4D
    @Test()
    void isAllEmptyWhenIsNotEmptyNotCs() {
        /* Branches:
         * (ArrayUtils.isEmpty(css)) : false
         * (for-each(css)) : true
         * (isNotEmpty(cs)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isNotEmpty("charSequence1")).thenReturn(false);
            CharSequence[] charSequenceArray = new CharSequence[] { "charSequence1" };
            //Act Statement(s)
            boolean result = StringUtils.isAllEmpty(charSequenceArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                stringUtils.verify(() -> StringUtils.isNotEmpty("charSequence1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAllLowerCaseWhenIsEmptyCs}, hash: 4248A371A2A37CC1A3BBE2A685E8FACC
    @Test()
    void isAllLowerCaseWhenIsEmptyCs() {
        /* Branches:
         * (isEmpty(cs)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("cs1")).thenReturn(true);
            //Act Statement(s)
            boolean result = StringUtils.isAllLowerCase("cs1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.isEmpty("cs1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAllLowerCaseWhenCharacterNotIsLowerCaseCsCharAtI}, hash: C7177F31F2E1274F697743127C9F32B8
    @Test()
    void isAllLowerCaseWhenCharacterNotIsLowerCaseCsCharAtI() {
        /* Branches:
         * (isEmpty(cs)) : false
         * (i < sz) : true
         * (!Character.isLowerCase(cs.charAt(i))) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("cs1")).thenReturn(false);
            //Act Statement(s)
            boolean result = StringUtils.isAllLowerCase("cs1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.isEmpty("cs1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAllLowerCaseWhenCharacterIsLowerCaseCsCharAtI}, hash: 7EFA532727C6B9DA8311971C04AFFDDE
    @Test()
    void isAllLowerCaseWhenCharacterIsLowerCaseCsCharAtI() {
        /* Branches:
         * (isEmpty(cs)) : false
         * (i < sz) : true
         * (!Character.isLowerCase(cs.charAt(i))) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("cs1")).thenReturn(false);
            //Act Statement(s)
            boolean result = StringUtils.isAllLowerCase("cs1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.isEmpty("cs1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAllUpperCaseWhenIsEmptyCs}, hash: E4332C33F63017A15344C67D3C1798CB
    @Test()
    void isAllUpperCaseWhenIsEmptyCs() {
        /* Branches:
         * (isEmpty(cs)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("cs1")).thenReturn(true);
            //Act Statement(s)
            boolean result = StringUtils.isAllUpperCase("cs1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.isEmpty("cs1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAllUpperCaseWhenCharacterNotIsUpperCaseCsCharAtI}, hash: F493C0D8B88E02FA714AECE2E8D5B3D5
    @Test()
    void isAllUpperCaseWhenCharacterNotIsUpperCaseCsCharAtI() {
        /* Branches:
         * (isEmpty(cs)) : false
         * (i < sz) : true
         * (!Character.isUpperCase(cs.charAt(i))) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("cs1")).thenReturn(false);
            //Act Statement(s)
            boolean result = StringUtils.isAllUpperCase("cs1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.isEmpty("cs1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAllUpperCaseWhenCharacterIsUpperCaseCsCharAtI}, hash: 193076AB55E3F1B7CF7FE4849FD92971
    @Disabled()
    @Test()
    void isAllUpperCaseWhenCharacterIsUpperCaseCsCharAtI() {
        /* Branches:
         * (isEmpty(cs)) : false
         * (i < sz) : true
         * (!Character.isUpperCase(cs.charAt(i))) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("cs1")).thenReturn(false);
            //Act Statement(s)
            boolean result = StringUtils.isAllUpperCase("cs1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                stringUtils.verify(() -> StringUtils.isEmpty("cs1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAlphaWhenIsEmptyCs}, hash: 373E60A109EE501CE82883B76BFCC5E7
    @Test()
    void isAlphaWhenIsEmptyCs() {
        /* Branches:
         * (isEmpty(cs)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("cs1")).thenReturn(true);
            //Act Statement(s)
            boolean result = StringUtils.isAlpha("cs1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.isEmpty("cs1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAlphaWhenCharacterNotIsLetterCsCharAtI}, hash: 37CFF6C593462B3526BE5483820C5BE6
    @Test()
    void isAlphaWhenCharacterNotIsLetterCsCharAtI() {
        /* Branches:
         * (isEmpty(cs)) : false
         * (i < sz) : true
         * (!Character.isLetter(cs.charAt(i))) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("cs1")).thenReturn(false);
            //Act Statement(s)
            boolean result = StringUtils.isAlpha("cs1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.isEmpty("cs1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAlphaWhenCharacterIsLetterCsCharAtI}, hash: 134FA9C4032F9A3D7C8B45333D211656
    @Test()
    void isAlphaWhenCharacterIsLetterCsCharAtI() {
        /* Branches:
         * (isEmpty(cs)) : false
         * (i < sz) : true
         * (!Character.isLetter(cs.charAt(i))) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("cs1")).thenReturn(false);
            //Act Statement(s)
            boolean result = StringUtils.isAlpha("cs1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.isEmpty("cs1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAlphanumericWhenIsEmptyCs}, hash: E4CE2FC1E693919395346D38F55C833C
    @Test()
    void isAlphanumericWhenIsEmptyCs() {
        /* Branches:
         * (isEmpty(cs)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("cs1")).thenReturn(true);
            //Act Statement(s)
            boolean result = StringUtils.isAlphanumeric("cs1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.isEmpty("cs1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAlphanumericWhenCharacterNotIsLetterOrDigitCsCharAtI}, hash: 8BCDDA85A5449FABD335E5A28CE79CE8
    @Disabled()
    @Test()
    void isAlphanumericWhenCharacterNotIsLetterOrDigitCsCharAtI() {
        /* Branches:
         * (isEmpty(cs)) : false
         * (i < sz) : true
         * (!Character.isLetterOrDigit(cs.charAt(i))) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("cs1")).thenReturn(false);
            //Act Statement(s)
            boolean result = StringUtils.isAlphanumeric("cs1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.isEmpty("cs1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAlphanumericWhenCharacterIsLetterOrDigitCsCharAtI}, hash: 6D3559BC1BD54C370F96DE8959BE210B
    @Test()
    void isAlphanumericWhenCharacterIsLetterOrDigitCsCharAtI() {
        /* Branches:
         * (isEmpty(cs)) : false
         * (i < sz) : true
         * (!Character.isLetterOrDigit(cs.charAt(i))) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("cs1")).thenReturn(false);
            //Act Statement(s)
            boolean result = StringUtils.isAlphanumeric("cs1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                stringUtils.verify(() -> StringUtils.isEmpty("cs1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAlphanumericSpaceWhenCsIsNull}, hash: E916E831F1F9338390C8A22A81FAD231
    @Test()
    void isAlphanumericSpaceWhenCsIsNull() {
        /* Branches:
         * (cs == null) : true
         */
        //Arrange Statement(s)
        CharSequence charSequence = null;
        //Act Statement(s)
        boolean result = StringUtils.isAlphanumericSpace(charSequence);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isAlphanumericSpaceWhenCharacterNotIsLetterOrDigitNowChar}, hash: 0D45973F08B4BB9BD57B77DE7F1DDA2C
    @Disabled()
    @Test()
    void isAlphanumericSpaceWhenCharacterNotIsLetterOrDigitNowChar() {
        /* Branches:
         * (cs == null) : false
         * (i < sz) : true
         * (nowChar != ' ') : true
         * (!Character.isLetterOrDigit(nowChar)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        boolean result = StringUtils.isAlphanumericSpace("cs1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isAlphanumericSpaceWhenCharacterIsLetterOrDigitNowChar}, hash: 0250658F45CF0308BAABB909E9B5D902
    @Test()
    void isAlphanumericSpaceWhenCharacterIsLetterOrDigitNowChar() {
        /* Branches:
         * (cs == null) : false
         * (i < sz) : true
         * (nowChar != ' ') : true
         * (!Character.isLetterOrDigit(nowChar)) : false
         */
        //Act Statement(s)
        boolean result = StringUtils.isAlphanumericSpace("cs1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isAlphaSpaceWhenCsIsNull}, hash: FBB90A15C40D16E53BEC0BDA1A021E20
    @Test()
    void isAlphaSpaceWhenCsIsNull() {
        /* Branches:
         * (cs == null) : true
         */
        //Arrange Statement(s)
        CharSequence charSequence = null;
        //Act Statement(s)
        boolean result = StringUtils.isAlphaSpace(charSequence);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isAlphaSpaceWhenCharacterNotIsLetterNowChar}, hash: 0B13AD4442391AD3E353D416A4361ABB
    @Test()
    void isAlphaSpaceWhenCharacterNotIsLetterNowChar() {
        /* Branches:
         * (cs == null) : false
         * (i < sz) : true
         * (nowChar != ' ') : true
         * (!Character.isLetter(nowChar)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        boolean result = StringUtils.isAlphaSpace("cs1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isAlphaSpaceWhenCharacterIsLetterNowChar}, hash: 764CCE499EDBF4AB1D7F6473CFB92800
    @Test()
    void isAlphaSpaceWhenCharacterIsLetterNowChar() {
        /* Branches:
         * (cs == null) : false
         * (i < sz) : true
         * (nowChar != ' ') : true
         * (!Character.isLetter(nowChar)) : false
         */
        //Act Statement(s)
        boolean result = StringUtils.isAlphaSpace("cs1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isAnyBlankWhenArrayUtilsIsEmptyCss}, hash: 91D9C671EE7A2D18B216FF10B3A2F2CC
    @Test()
    void isAnyBlankWhenArrayUtilsIsEmptyCss() {
        /* Branches:
         * (ArrayUtils.isEmpty(css)) : true
         */
        //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] {};
        //Act Statement(s)
        boolean result = StringUtils.isAnyBlank(charSequenceArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isAnyBlankWhenIsBlankCs}, hash: BBC868872EDBC8E6F3DE6694D7CE5341
    @Test()
    void isAnyBlankWhenIsBlankCs() {
        /* Branches:
         * (ArrayUtils.isEmpty(css)) : false
         * (for-each(css)) : true
         * (isBlank(cs)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isBlank("charSequence1")).thenReturn(true);
            CharSequence[] charSequenceArray = new CharSequence[] { "charSequence1" };
            //Act Statement(s)
            boolean result = StringUtils.isAnyBlank(charSequenceArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                stringUtils.verify(() -> StringUtils.isBlank("charSequence1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAnyBlankWhenIsBlankNotCs}, hash: 81A14055086AAB66F687B683208BD944
    @Test()
    void isAnyBlankWhenIsBlankNotCs() {
        /* Branches:
         * (ArrayUtils.isEmpty(css)) : false
         * (for-each(css)) : true
         * (isBlank(cs)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isBlank("charSequence1")).thenReturn(false);
            CharSequence[] charSequenceArray = new CharSequence[] { "charSequence1" };
            //Act Statement(s)
            boolean result = StringUtils.isAnyBlank(charSequenceArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.isBlank("charSequence1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAnyEmptyWhenArrayUtilsIsEmptyCss}, hash: 8F21EC2FF6CF9D70BD74801410816E33
    @Test()
    void isAnyEmptyWhenArrayUtilsIsEmptyCss() {
        /* Branches:
         * (ArrayUtils.isEmpty(css)) : true
         */
        //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] {};
        //Act Statement(s)
        boolean result = StringUtils.isAnyEmpty(charSequenceArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isAnyEmptyWhenIsEmptyCs}, hash: FE268ED1AB04E3A50C9861FD2A3F6416
    @Test()
    void isAnyEmptyWhenIsEmptyCs() {
        /* Branches:
         * (ArrayUtils.isEmpty(css)) : false
         * (for-each(css)) : true
         * (isEmpty(cs)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("charSequence1")).thenReturn(true);
            CharSequence[] charSequenceArray = new CharSequence[] { "charSequence1" };
            //Act Statement(s)
            boolean result = StringUtils.isAnyEmpty(charSequenceArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                stringUtils.verify(() -> StringUtils.isEmpty("charSequence1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAnyEmptyWhenIsEmptyNotCs}, hash: BDC493F96B1B48FB50BA4F3230BECA35
    @Test()
    void isAnyEmptyWhenIsEmptyNotCs() {
        /* Branches:
         * (ArrayUtils.isEmpty(css)) : false
         * (for-each(css)) : true
         * (isEmpty(cs)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("charSequence1")).thenReturn(false);
            CharSequence[] charSequenceArray = new CharSequence[] { "charSequence1" };
            //Act Statement(s)
            boolean result = StringUtils.isAnyEmpty(charSequenceArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.isEmpty("charSequence1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAsciiPrintableWhenCsIsNull}, hash: C6177AD4162C9429722098C88ECC1EDB
    @Test()
    void isAsciiPrintableWhenCsIsNull() {
        /* Branches:
         * (cs == null) : true
         */
        //Arrange Statement(s)
        CharSequence charSequence = null;
        //Act Statement(s)
        boolean result = StringUtils.isAsciiPrintable(charSequence);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isAsciiPrintableWhenCharUtilsNotIsAsciiPrintableCsCharAtI}, hash: 3112CBD162157C0060A18B6936154B7C
    @Disabled()
    @Test()
    void isAsciiPrintableWhenCharUtilsNotIsAsciiPrintableCsCharAtI() {
        /* Branches:
         * (cs == null) : false
         * (i < sz) : true
         * (!CharUtils.isAsciiPrintable(cs.charAt(i))) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        boolean result = StringUtils.isAsciiPrintable("cs1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isAsciiPrintableWhenCharUtilsIsAsciiPrintableCsCharAtI}, hash: 92D9765DAFA00F747464220ACAB5DCC1
    @Test()
    void isAsciiPrintableWhenCharUtilsIsAsciiPrintableCsCharAtI() {
        /* Branches:
         * (cs == null) : false
         * (i < sz) : true
         * (!CharUtils.isAsciiPrintable(cs.charAt(i))) : false
         */
        //Act Statement(s)
        boolean result = StringUtils.isAsciiPrintable("cs1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isBlankWhenStrLenEquals0}, hash: A584403E4DD06D8F9C6C199860BC6767
    @Test()
    void isBlankWhenStrLenEquals0() {
        /* Branches:
         * (strLen == 0) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.length("cs1")).thenReturn(0);
            //Act Statement(s)
            boolean result = StringUtils.isBlank("cs1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                stringUtils.verify(() -> StringUtils.length("cs1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isBlankWhenCharacterNotIsWhitespaceCsCharAtI}, hash: 8C9C450D1DF1BD55AA5CBFC41BEFFF22
    @Test()
    void isBlankWhenCharacterNotIsWhitespaceCsCharAtI() {
        /* Branches:
         * (strLen == 0) : false
         * (i < strLen) : true
         * (!Character.isWhitespace(cs.charAt(i))) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.length("cs1")).thenReturn(1);
            //Act Statement(s)
            boolean result = StringUtils.isBlank("cs1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.length("cs1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isBlankWhenCharacterIsWhitespaceCsCharAtI}, hash: C2E4839A8270AEBED647AD64E9274FD9
    @Test()
    void isBlankWhenCharacterIsWhitespaceCsCharAtI() {
        /* Branches:
         * (strLen == 0) : false
         * (i < strLen) : true
         * (!Character.isWhitespace(cs.charAt(i))) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.length("cs1")).thenReturn(0);
            //Act Statement(s)
            boolean result = StringUtils.isBlank("cs1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                stringUtils.verify(() -> StringUtils.length("cs1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isEmptyWhenCsLengthEquals0}, hash: 0B224AE53E18E07E1C9F4A9D032B9248
    @Disabled()
    @Test()
    void isEmptyWhenCsLengthEquals0() {
        /* Branches:
         * (cs == null) : false
         * (cs.length() == 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        boolean result = StringUtils.isEmpty("cs1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isEmptyWhenCsLengthNotEquals0}, hash: 0480020F68B31AF81DCC15B97214B0CD
    @Test()
    void isEmptyWhenCsLengthNotEquals0() {
        /* Branches:
         * (cs == null) : false
         * (cs.length() == 0) : false
         */
        //Act Statement(s)
        boolean result = StringUtils.isEmpty("cs1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isMixedCaseWhenCsLengthEquals1}, hash: 76F9F9BEBABBCE8DDF0FC17440D519E9
    @Test()
    void isMixedCaseWhenCsLengthEquals1() {
        /* Branches:
         * (isEmpty(cs)) : false
         * (cs.length() == 1) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("cs1")).thenReturn(false);
            //Act Statement(s)
            boolean result = StringUtils.isMixedCase("cs1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.isEmpty("cs1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isMixedCaseWhenNotContainsUppercase}, hash: 1F08CDD63044B32DEDBB0E0508D80C22
    @Test()
    void isMixedCaseWhenNotContainsUppercase() {
        /* Branches:
         * (isEmpty(cs)) : false
         * (cs.length() == 1) : false
         * (i < sz) : true
         * (Character.isUpperCase(nowChar)) : false
         * (Character.isLowerCase(nowChar)) : true
         * (containsUppercase) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("cs1")).thenReturn(false);
            //Act Statement(s)
            boolean result = StringUtils.isMixedCase("cs1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.isEmpty("cs1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isNoneBlankWhenIsAnyBlankNotCss}, hash: 2CFC01197ECE5B8BCFC09E034734DA2F
    @Test()
    void isNoneBlankWhenIsAnyBlankNotCss() {
        /* Branches:
         * (!isAnyBlank(css)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            CharSequence[] charSequenceArray = new CharSequence[] {};
            stringUtils.when(() -> StringUtils.isAnyBlank(charSequenceArray)).thenReturn(false);
            //Act Statement(s)
            boolean result = StringUtils.isNoneBlank(charSequenceArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                stringUtils.verify(() -> StringUtils.isAnyBlank(charSequenceArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isNoneBlankWhenIsAnyBlankCss}, hash: 0A10194F9179A4E6ED75F352FBF646CB
    @Test()
    void isNoneBlankWhenIsAnyBlankCss() {
        /* Branches:
         * (!isAnyBlank(css)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            CharSequence[] charSequenceArray = new CharSequence[] {};
            stringUtils.when(() -> StringUtils.isAnyBlank(charSequenceArray)).thenReturn(true);
            //Act Statement(s)
            boolean result = StringUtils.isNoneBlank(charSequenceArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.isAnyBlank(charSequenceArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isNoneEmptyWhenIsAnyEmptyNotCss}, hash: AA8B28EA14A3317C17E8658AB128C7A2
    @Test()
    void isNoneEmptyWhenIsAnyEmptyNotCss() {
        /* Branches:
         * (!isAnyEmpty(css)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            CharSequence[] charSequenceArray = new CharSequence[] {};
            stringUtils.when(() -> StringUtils.isAnyEmpty(charSequenceArray)).thenReturn(false);
            //Act Statement(s)
            boolean result = StringUtils.isNoneEmpty(charSequenceArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                stringUtils.verify(() -> StringUtils.isAnyEmpty(charSequenceArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isNoneEmptyWhenIsAnyEmptyCss}, hash: FAEDDF9B41700525A9235F5D228198F2
    @Test()
    void isNoneEmptyWhenIsAnyEmptyCss() {
        /* Branches:
         * (!isAnyEmpty(css)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            CharSequence[] charSequenceArray = new CharSequence[] {};
            stringUtils.when(() -> StringUtils.isAnyEmpty(charSequenceArray)).thenReturn(true);
            //Act Statement(s)
            boolean result = StringUtils.isNoneEmpty(charSequenceArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.isAnyEmpty(charSequenceArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isNotBlankWhenIsBlankNotCs}, hash: C71F09FE81E5ED009BD01714F0CDE032
    @Test()
    void isNotBlankWhenIsBlankNotCs() {
        /* Branches:
         * (!isBlank(cs)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isBlank("cs1")).thenReturn(false);
            //Act Statement(s)
            boolean result = StringUtils.isNotBlank("cs1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                stringUtils.verify(() -> StringUtils.isBlank("cs1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isNotBlankWhenIsBlankCs}, hash: 9B7FC0A46087A470F4D216AD4FCDEB13
    @Test()
    void isNotBlankWhenIsBlankCs() {
        /* Branches:
         * (!isBlank(cs)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isBlank("cs1")).thenReturn(true);
            //Act Statement(s)
            boolean result = StringUtils.isNotBlank("cs1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.isBlank("cs1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isNotEmptyWhenIsEmptyNotCs}, hash: 9D36A30D9ECE0202156BA8A0C1ABCBB3
    @Test()
    void isNotEmptyWhenIsEmptyNotCs() {
        /* Branches:
         * (!isEmpty(cs)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("cs1")).thenReturn(false);
            //Act Statement(s)
            boolean result = StringUtils.isNotEmpty("cs1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                stringUtils.verify(() -> StringUtils.isEmpty("cs1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isNotEmptyWhenIsEmptyCs}, hash: 0E81BAC39562C7EEE2B9AC91DE1A0B26
    @Test()
    void isNotEmptyWhenIsEmptyCs() {
        /* Branches:
         * (!isEmpty(cs)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("cs1")).thenReturn(true);
            //Act Statement(s)
            boolean result = StringUtils.isNotEmpty("cs1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.isEmpty("cs1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isNumericWhenIsEmptyCs}, hash: 19FABC925C4F514C9EF9C57B0BDFFF3E
    @Test()
    void isNumericWhenIsEmptyCs() {
        /* Branches:
         * (isEmpty(cs)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("cs1")).thenReturn(true);
            //Act Statement(s)
            boolean result = StringUtils.isNumeric("cs1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.isEmpty("cs1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isNumericWhenCharacterNotIsDigitCsCharAtI}, hash: 6A83BC3F563F2D58C62BEBE51A3BC6F5
    @Test()
    void isNumericWhenCharacterNotIsDigitCsCharAtI() {
        /* Branches:
         * (isEmpty(cs)) : false
         * (i < sz) : true
         * (!Character.isDigit(cs.charAt(i))) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("cs1")).thenReturn(false);
            //Act Statement(s)
            boolean result = StringUtils.isNumeric("cs1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                stringUtils.verify(() -> StringUtils.isEmpty("cs1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isNumericWhenCharacterIsDigitCsCharAtI}, hash: 8C5237463F9043EB8A6EAE101C9FCC71
    @Disabled()
    @Test()
    void isNumericWhenCharacterIsDigitCsCharAtI() {
        /* Branches:
         * (isEmpty(cs)) : false
         * (i < sz) : true
         * (!Character.isDigit(cs.charAt(i))) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("cs1")).thenReturn(false);
            //Act Statement(s)
            boolean result = StringUtils.isNumeric("cs1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                stringUtils.verify(() -> StringUtils.isEmpty("cs1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isNumericSpaceWhenCsIsNull}, hash: 5677CB96A1A231106844260E906AD69D
    @Test()
    void isNumericSpaceWhenCsIsNull() {
        /* Branches:
         * (cs == null) : true
         */
        //Arrange Statement(s)
        CharSequence charSequence = null;
        //Act Statement(s)
        boolean result = StringUtils.isNumericSpace(charSequence);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isNumericSpaceWhenCharacterNotIsDigitNowChar}, hash: CCFA742A63F3B86ABF032300121D46E0
    @Test()
    void isNumericSpaceWhenCharacterNotIsDigitNowChar() {
        /* Branches:
         * (cs == null) : false
         * (i < sz) : true
         * (nowChar != ' ') : true
         * (!Character.isDigit(nowChar)) : true
         */
        //Act Statement(s)
        boolean result = StringUtils.isNumericSpace("cs1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isNumericSpaceWhenCharacterIsDigitNowChar}, hash: F181DDF20FF67B8F62ECC1495E3A665C
    @Disabled()
    @Test()
    void isNumericSpaceWhenCharacterIsDigitNowChar() {
        /* Branches:
         * (cs == null) : false
         * (i < sz) : true
         * (nowChar != ' ') : true
         * (!Character.isDigit(nowChar)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        boolean result = StringUtils.isNumericSpace("cs1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isWhitespaceWhenCsIsNull}, hash: 2DFC8264D6315BBA31671233B98E2929
    @Test()
    void isWhitespaceWhenCsIsNull() {
        /* Branches:
         * (cs == null) : true
         */
        //Arrange Statement(s)
        CharSequence charSequence = null;
        //Act Statement(s)
        boolean result = StringUtils.isWhitespace(charSequence);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isWhitespaceWhenCharacterNotIsWhitespaceCsCharAtI}, hash: B5553CE196BC12636D59BEA826E15786
    @Test()
    void isWhitespaceWhenCharacterNotIsWhitespaceCsCharAtI() {
        /* Branches:
         * (cs == null) : false
         * (i < sz) : true
         * (!Character.isWhitespace(cs.charAt(i))) : true
         */
        //Act Statement(s)
        boolean result = StringUtils.isWhitespace("cs1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isWhitespaceWhenCharacterIsWhitespaceCsCharAtI}, hash: FC80CD46F08B893878748D3C3D314B1B
    @Disabled()
    @Test()
    void isWhitespaceWhenCharacterIsWhitespaceCsCharAtI() {
        /* Branches:
         * (cs == null) : false
         * (i < sz) : true
         * (!Character.isWhitespace(cs.charAt(i))) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        boolean result = StringUtils.isWhitespace("cs1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${joinWhenArrayIsNull}, hash: 457943BB53EBD613C4450DEC9CADB938
    @Test()
    void joinWhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        boolean[] _boolean = null;
        //Act Statement(s)
        String result = StringUtils.join(_boolean, 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${joinWhenArrayIsNotNull}, hash: FE35F39A20D51FF2CDBC3F1DC4CA0147
    @Test()
    void joinWhenArrayIsNotNull() {
        /* Branches:
         * (array == null) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            stringUtils.when(() -> StringUtils.join(booleanArray, 'A', 0, 0)).thenReturn("return_of_join1");
            //Act Statement(s)
            String result = StringUtils.join(booleanArray, 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_join1"));
                stringUtils.verify(() -> StringUtils.join(booleanArray, 'A', 0, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${join1WhenArrayIsNull}, hash: 47DECDB6F7546EF0048898A5976EC3BA
    @Test()
    void join1WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        boolean[] _boolean = null;
        //Act Statement(s)
        String result = StringUtils.join(_boolean, 'A', 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${join1WhenEndIndexMinusStartIndexLessThanOrEqualsTo0}, hash: E01101D0DA4BBB1EE33E9E67B5285BFA
    @Test()
    void join1WhenEndIndexMinusStartIndexLessThanOrEqualsTo0() {
        /* Branches:
         * (array == null) : false
         * (endIndex - startIndex <= 0) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        //Act Statement(s)
        String result = StringUtils.join(booleanArray, 'A', 2, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${join1WhenILessThanEndIndex}, hash: 2F9242AC15A29AA055F3A58C3DB6BC57
    @Disabled()
    @Test()
    void join1WhenILessThanEndIndex() {
        /* Branches:
         * (array == null) : false
         * (endIndex - startIndex <= 0) : false
         * (i < endIndex) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false };
        //Act Statement(s)
        String result = StringUtils.join(booleanArray, 'A', 0, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${join2WhenArrayIsNull}, hash: 4ED91B187E0AD13FE8C45219F570FA2D
    @Test()
    void join2WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        byte[] _byte = null;
        //Act Statement(s)
        String result = StringUtils.join(_byte, 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${join2WhenArrayIsNotNull}, hash: 9FBD02C7E1C33F65973306254434DC84
    @Test()
    void join2WhenArrayIsNotNull() {
        /* Branches:
         * (array == null) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            stringUtils.when(() -> StringUtils.join(byteArray, 'A', 0, 0)).thenReturn("return_of_join1");
            //Act Statement(s)
            String result = StringUtils.join(byteArray, 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_join1"));
                stringUtils.verify(() -> StringUtils.join(byteArray, 'A', 0, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${join3WhenArrayIsNull}, hash: 8CA019E334F4BB78510B384A70372BD6
    @Test()
    void join3WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        byte[] _byte = null;
        //Act Statement(s)
        String result = StringUtils.join(_byte, 'A', 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${join3WhenEndIndexMinusStartIndexLessThanOrEqualsTo0}, hash: 75BB5E701278DDC2E4BE23D962CCF435
    @Test()
    void join3WhenEndIndexMinusStartIndexLessThanOrEqualsTo0() {
        /* Branches:
         * (array == null) : false
         * (endIndex - startIndex <= 0) : true
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        //Act Statement(s)
        String result = StringUtils.join(byteArray, 'A', 2, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${join3WhenILessThanEndIndex}, hash: 291CD9E2CA34CB3A955B42770819BF00
    @Disabled()
    @Test()
    void join3WhenILessThanEndIndex() {
        /* Branches:
         * (array == null) : false
         * (endIndex - startIndex <= 0) : false
         * (i < endIndex) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] { (byte) 2 };
        //Act Statement(s)
        String result = StringUtils.join(byteArray, 'A', 0, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${join4WhenArrayIsNull}, hash: 31366E35245A3547ECD6364D87E519E0
    @Test()
    void join4WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        char[] _char = null;
        //Act Statement(s)
        String result = StringUtils.join(_char, 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${join4WhenArrayIsNotNull}, hash: 97760A969A7F758FF02C4C953EF43F57
    @Test()
    void join4WhenArrayIsNotNull() {
        /* Branches:
         * (array == null) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            stringUtils.when(() -> StringUtils.join(charArray, 'A', 0, 0)).thenReturn("return_of_join1");
            //Act Statement(s)
            String result = StringUtils.join(charArray, 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_join1"));
                stringUtils.verify(() -> StringUtils.join(charArray, 'A', 0, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${join5WhenArrayIsNull}, hash: DC5BA89E38F7E881502B3E4D25FC651A
    @Test()
    void join5WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        char[] _char = null;
        //Act Statement(s)
        String result = StringUtils.join(_char, 'A', 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${join5WhenEndIndexMinusStartIndexLessThanOrEqualsTo0}, hash: B7BE65C0649FD53610B83D76DEF61C59
    @Test()
    void join5WhenEndIndexMinusStartIndexLessThanOrEqualsTo0() {
        /* Branches:
         * (array == null) : false
         * (endIndex - startIndex <= 0) : true
         */
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        //Act Statement(s)
        String result = StringUtils.join(charArray, 'A', 2, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${join5WhenILessThanEndIndex}, hash: 90D5418004147BE1BFF15388071E57EA
    @Disabled()
    @Test()
    void join5WhenILessThanEndIndex() {
        /* Branches:
         * (array == null) : false
         * (endIndex - startIndex <= 0) : false
         * (i < endIndex) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        char[] charArray = new char[] { 'A' };
        //Act Statement(s)
        String result = StringUtils.join(charArray, 'A', 0, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${join6WhenArrayIsNull}, hash: B26F039F5740B6CFDCF2CB39CAB738C8
    @Test()
    void join6WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        double[] _double = null;
        //Act Statement(s)
        String result = StringUtils.join(_double, 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${join6WhenArrayIsNotNull}, hash: 59F0222EA6BF871FD48014EA959BD58C
    @Test()
    void join6WhenArrayIsNotNull() {
        /* Branches:
         * (array == null) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            stringUtils.when(() -> StringUtils.join(doubleArray, 'A', 0, 0)).thenReturn("return_of_join1");
            //Act Statement(s)
            String result = StringUtils.join(doubleArray, 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_join1"));
                stringUtils.verify(() -> StringUtils.join(doubleArray, 'A', 0, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${join7WhenArrayIsNull}, hash: 538475586A460E729EFFF00A5198CFF2
    @Test()
    void join7WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        double[] _double = null;
        //Act Statement(s)
        String result = StringUtils.join(_double, 'A', 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${join7WhenEndIndexMinusStartIndexLessThanOrEqualsTo0}, hash: A063581756332147BB869A63B4943247
    @Test()
    void join7WhenEndIndexMinusStartIndexLessThanOrEqualsTo0() {
        /* Branches:
         * (array == null) : false
         * (endIndex - startIndex <= 0) : true
         */
        //Arrange Statement(s)
        double[] doubleArray = new double[] {};
        //Act Statement(s)
        String result = StringUtils.join(doubleArray, 'A', 2, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${join7WhenILessThanEndIndex}, hash: AFD18B78898D9EFEBE2B05EE4EA302C2
    @Disabled()
    @Test()
    void join7WhenILessThanEndIndex() {
        /* Branches:
         * (array == null) : false
         * (endIndex - startIndex <= 0) : false
         * (i < endIndex) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        double[] doubleArray = new double[] { Double.parseDouble("1.0") };
        //Act Statement(s)
        String result = StringUtils.join(doubleArray, 'A', 0, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${join8WhenArrayIsNull}, hash: FC8440C7A7D3DCF308A5B6B79DCEC8D2
    @Test()
    void join8WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        float[] _float = null;
        //Act Statement(s)
        String result = StringUtils.join(_float, 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${join8WhenArrayIsNotNull}, hash: 7DB2A2CE36B32DE36AA360EBD4098F83
    @Test()
    void join8WhenArrayIsNotNull() {
        /* Branches:
         * (array == null) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            stringUtils.when(() -> StringUtils.join(floatArray, 'A', 0, 0)).thenReturn("return_of_join1");
            //Act Statement(s)
            String result = StringUtils.join(floatArray, 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_join1"));
                stringUtils.verify(() -> StringUtils.join(floatArray, 'A', 0, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${join9WhenArrayIsNull}, hash: 88C4A5414E5153674F3C2A55F3C2872C
    @Test()
    void join9WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        float[] _float = null;
        //Act Statement(s)
        String result = StringUtils.join(_float, 'A', 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${join9WhenEndIndexMinusStartIndexLessThanOrEqualsTo0}, hash: 98EB9A0053847ED046E2DDECE98FD20E
    @Test()
    void join9WhenEndIndexMinusStartIndexLessThanOrEqualsTo0() {
        /* Branches:
         * (array == null) : false
         * (endIndex - startIndex <= 0) : true
         */
        //Arrange Statement(s)
        float[] floatArray = new float[] {};
        //Act Statement(s)
        String result = StringUtils.join(floatArray, 'A', 2, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${join9WhenILessThanEndIndex}, hash: 39E7E5F53DEAE6C40419514E8D28161D
    @Disabled()
    @Test()
    void join9WhenILessThanEndIndex() {
        /* Branches:
         * (array == null) : false
         * (endIndex - startIndex <= 0) : false
         * (i < endIndex) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        float[] floatArray = new float[] { Float.parseFloat("1.0") };
        //Act Statement(s)
        String result = StringUtils.join(floatArray, 'A', 0, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${join10WhenArrayIsNull}, hash: CDD56CF7CED0FCE1A467FFF062AEED54
    @Test()
    void join10WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        int[] _int = null;
        //Act Statement(s)
        String result = StringUtils.join(_int, 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${join10WhenArrayIsNotNull}, hash: D7A29FCDB42D09DB1F031C486D008D13
    @Test()
    void join10WhenArrayIsNotNull() {
        /* Branches:
         * (array == null) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            stringUtils.when(() -> StringUtils.join(intArray, 'A', 0, 0)).thenReturn("return_of_join1");
            //Act Statement(s)
            String result = StringUtils.join(intArray, 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_join1"));
                stringUtils.verify(() -> StringUtils.join(intArray, 'A', 0, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${join11WhenArrayIsNull}, hash: 1A6C0EBC25416F02B328114769CAC011
    @Test()
    void join11WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        int[] _int = null;
        //Act Statement(s)
        String result = StringUtils.join(_int, 'A', 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${join11WhenEndIndexMinusStartIndexLessThanOrEqualsTo0}, hash: 9F6303A06EA021FEA6085D112D6D68CD
    @Test()
    void join11WhenEndIndexMinusStartIndexLessThanOrEqualsTo0() {
        /* Branches:
         * (array == null) : false
         * (endIndex - startIndex <= 0) : true
         */
        //Arrange Statement(s)
        int[] intArray = new int[] {};
        //Act Statement(s)
        String result = StringUtils.join(intArray, 'A', 2, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${join11WhenILessThanEndIndex}, hash: 4516999D5E6A1BF4AB5654D531C2E6A7
    @Disabled()
    @Test()
    void join11WhenILessThanEndIndex() {
        /* Branches:
         * (array == null) : false
         * (endIndex - startIndex <= 0) : false
         * (i < endIndex) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        int[] intArray = new int[] { 2 };
        //Act Statement(s)
        String result = StringUtils.join(intArray, 'A', 0, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${join12WhenIterableIsNotNull}, hash: 4A63A2AC33439E62F5569EE520682AEF
    @Test()
    void join12WhenIterableIsNotNull() {
        /* Branches:
         * (iterable != null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.join((Iterator) any(), eq('A'))).thenReturn("return_of_join1");
            Iterable<Object> iterable = new ArrayList<>();
            //Act Statement(s)
            String result = StringUtils.join(iterable, 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_join1"));
                stringUtils.verify(() -> StringUtils.join((Iterator) any(), eq('A')), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${join12WhenIterableIsNull}, hash: 172F64B80E5B37D2A3708A66DBC403F8
    @Test()
    void join12WhenIterableIsNull() {
        /* Branches:
         * (iterable != null) : false
         */
        //Arrange Statement(s)
        Iterable<?> iterable = null;
        //Act Statement(s)
        String result = StringUtils.join(iterable, 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${join13WhenIterableIsNotNull}, hash: 544A3DA684363F309240EE2114C9E6AA
    @Test()
    void join13WhenIterableIsNotNull() {
        /* Branches:
         * (iterable != null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.join((Iterator) any(), eq("separator1"))).thenReturn("return_of_join1");
            Iterable<Object> iterable = new ArrayList<>();
            //Act Statement(s)
            String result = StringUtils.join(iterable, "separator1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_join1"));
                stringUtils.verify(() -> StringUtils.join((Iterator) any(), eq("separator1")), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${join13WhenIterableIsNull}, hash: 3C3924FD38A1E9753BECE9C8066B8DF0
    @Test()
    void join13WhenIterableIsNull() {
        /* Branches:
         * (iterable != null) : false
         */
        //Arrange Statement(s)
        Iterable<?> iterable = null;
        //Act Statement(s)
        String result = StringUtils.join(iterable, "separator1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${join14WhenIteratorIsNull}, hash: 4BF6588D067E3575639A07E1270ED7BB
    @Test()
    void join14WhenIteratorIsNull() {
        /* Branches:
         * (iterator == null) : true
         */
        //Arrange Statement(s)
        Iterator<?> iterator = null;
        //Act Statement(s)
        String result = StringUtils.join(iterator, 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${join14WhenIteratorNotHasNext}, hash: 2A0223D7182D9D7F42C4F6E1ECA2E712
    @Test()
    void join14WhenIteratorNotHasNext() {
        /* Branches:
         * (iterator == null) : false
         * (!iterator.hasNext()) : true
         */
        //Arrange Statement(s)
        List<Object> anyList = new ArrayList<>();
        Iterator<?> iteratorIterator = anyList.iterator();
        //Act Statement(s)
        String result = StringUtils.join(iteratorIterator, 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${join14WhenIteratorHasNext}, hash: A12373E98E657E60006E6BC3F5A165DE
    @Disabled()
    @Test()
    void join14WhenIteratorHasNext() {
        /* Branches:
         * (iterator == null) : false
         * (!iterator.hasNext()) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object object = new Object();
        List<Object> anyList = new ArrayList<>();
        anyList.add(object);
        Iterator<?> iteratorIterator = anyList.iterator();
        //Act Statement(s)
        String result = StringUtils.join(iteratorIterator, 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${join15WhenIteratorIsNull}, hash: E74E55163C37EE5CF4EAD2CD1717D784
    @Test()
    void join15WhenIteratorIsNull() {
        /* Branches:
         * (iterator == null) : true
         */
        //Arrange Statement(s)
        Iterator<?> iterator = null;
        //Act Statement(s)
        String result = StringUtils.join(iterator, "separator1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${join15WhenIteratorNotHasNext}, hash: 4430E9D218619268B6E97CB91AB1D746
    @Test()
    void join15WhenIteratorNotHasNext() {
        /* Branches:
         * (iterator == null) : false
         * (!iterator.hasNext()) : true
         */
        //Arrange Statement(s)
        List<Object> anyList = new ArrayList<>();
        Iterator<?> iteratorIterator = anyList.iterator();
        //Act Statement(s)
        String result = StringUtils.join(iteratorIterator, "separator1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${join15WhenIteratorHasNext}, hash: 84F44B6D3CB628255B71A0DF19BDC116
    @Disabled()
    @Test()
    void join15WhenIteratorHasNext() {
        /* Branches:
         * (iterator == null) : false
         * (!iterator.hasNext()) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object object = new Object();
        List<Object> anyList = new ArrayList<>();
        anyList.add(object);
        Iterator<?> iteratorIterator = anyList.iterator();
        //Act Statement(s)
        String result = StringUtils.join(iteratorIterator, "A");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${join16WhenListIsNull}, hash: FAB7924A50C006AD6A0300276762BEAD
    @Test()
    void join16WhenListIsNull() {
        /* Branches:
         * (list == null) : true
         */
        //Arrange Statement(s)
        List<?> list = null;
        //Act Statement(s)
        String result = StringUtils.join(list, 'A', 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${join16WhenNoOfItemsLessThanOrEqualsTo0}, hash: 198F56A3616C63A6BEAEE07C4DD59D3B
    @Test()
    void join16WhenNoOfItemsLessThanOrEqualsTo0() {
        /* Branches:
         * (list == null) : false
         * (noOfItems <= 0) : true
         */
        //Arrange Statement(s)
        List<Object> anyList = new ArrayList<>();
        //Act Statement(s)
        String result = StringUtils.join(anyList, 'A', 2, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${join16WhenNoOfItemsGreaterThan0}, hash: 2ACE95E3EDB65BABE3ABE60124C206E8
    @Test()
    void join16WhenNoOfItemsGreaterThan0() {
        /* Branches:
         * (list == null) : false
         * (noOfItems <= 0) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.join((Iterator) any(), eq('A'))).thenReturn("return_of_join1");
            Object object = new Object();
            List<Object> anyList = new ArrayList<>();
            anyList.add(object);
            //Act Statement(s)
            String result = StringUtils.join(anyList, 'A', 0, 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_join1"));
                stringUtils.verify(() -> StringUtils.join((Iterator) any(), eq('A')), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${join17WhenListIsNull}, hash: 0B57B78DB33D2E111BE7C096F507A830
    @Test()
    void join17WhenListIsNull() {
        /* Branches:
         * (list == null) : true
         */
        //Arrange Statement(s)
        List<?> list = null;
        //Act Statement(s)
        String result = StringUtils.join(list, "separator1", 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${join17WhenNoOfItemsLessThanOrEqualsTo0}, hash: F391E7116D9D6DF72EE79CE08997B3B4
    @Test()
    void join17WhenNoOfItemsLessThanOrEqualsTo0() {
        /* Branches:
         * (list == null) : false
         * (noOfItems <= 0) : true
         */
        //Arrange Statement(s)
        List<Object> anyList = new ArrayList<>();
        //Act Statement(s)
        String result = StringUtils.join(anyList, "separator1", 2, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${join17WhenNoOfItemsGreaterThan0}, hash: 993E48DC97036E11DB2C75DB85A09480
    @Test()
    void join17WhenNoOfItemsGreaterThan0() {
        /* Branches:
         * (list == null) : false
         * (noOfItems <= 0) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.join((Iterator) any(), eq("separator1"))).thenReturn("return_of_join1");
            Object object = new Object();
            List<Object> anyList = new ArrayList<>();
            anyList.add(object);
            //Act Statement(s)
            String result = StringUtils.join(anyList, "separator1", 0, 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_join1"));
                stringUtils.verify(() -> StringUtils.join((Iterator) any(), eq("separator1")), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${join18WhenArrayIsNull}, hash: 4F10A712CD7F992D196E32967A582095
    @Test()
    void join18WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        long[] _long = null;
        //Act Statement(s)
        String result = StringUtils.join(_long, 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${join18WhenArrayIsNotNull}, hash: D9B7C65C163EFED6E21BC295DDF5518F
    @Test()
    void join18WhenArrayIsNotNull() {
        /* Branches:
         * (array == null) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            stringUtils.when(() -> StringUtils.join(longArray, 'A', 0, 0)).thenReturn("return_of_join1");
            //Act Statement(s)
            String result = StringUtils.join(longArray, 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_join1"));
                stringUtils.verify(() -> StringUtils.join(longArray, 'A', 0, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${join19WhenArrayIsNull}, hash: 2558AAC1C11294505134C42E85B02B75
    @Test()
    void join19WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        long[] _long = null;
        //Act Statement(s)
        String result = StringUtils.join(_long, 'A', 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${join19WhenEndIndexMinusStartIndexLessThanOrEqualsTo0}, hash: 4CA430AF22FB02CA39CB774B9BAA6161
    @Test()
    void join19WhenEndIndexMinusStartIndexLessThanOrEqualsTo0() {
        /* Branches:
         * (array == null) : false
         * (endIndex - startIndex <= 0) : true
         */
        //Arrange Statement(s)
        long[] longArray = new long[] {};
        //Act Statement(s)
        String result = StringUtils.join(longArray, 'A', 2, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${join19WhenILessThanEndIndex}, hash: F94C765BA33E99966A0175B6C6354ACE
    @Disabled()
    @Test()
    void join19WhenILessThanEndIndex() {
        /* Branches:
         * (array == null) : false
         * (endIndex - startIndex <= 0) : false
         * (i < endIndex) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        long[] longArray = new long[] { 2L };
        //Act Statement(s)
        String result = StringUtils.join(longArray, 'A', 0, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${join20WhenArrayIsNull}, hash: 53BB8528B17AD5973CE41BC7E2DB5537
    @Test()
    void join20WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        Object[] object = null;
        //Act Statement(s)
        String result = StringUtils.join(object, 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${join20WhenArrayIsNotNull}, hash: B717A693D12C6E8500AB3F5D1C6BE0E7
    @Test()
    void join20WhenArrayIsNotNull() {
        /* Branches:
         * (array == null) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            stringUtils.when(() -> StringUtils.join(objectArray, 'A', 0, 0)).thenReturn("return_of_join1");
            //Act Statement(s)
            String result = StringUtils.join(objectArray, 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_join1"));
                stringUtils.verify(() -> StringUtils.join(objectArray, 'A', 0, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${join21Test}, hash: 91A3136455125E9E484C32322E0E517A
    @Test()
    void join21Test() {
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            stringUtils.when(() -> StringUtils.join(objectArray, "A", 0, 0)).thenReturn("return_of_join1");
            //Act Statement(s)
            String result = StringUtils.join(objectArray, 'A', 0, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_join1"));
                stringUtils.verify(() -> StringUtils.join(objectArray, "A", 0, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${join22WhenArrayIsNotNull}, hash: 84EE549724812EC0B07DF4A19C52EFCF
    @Test()
    void join22WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            stringUtils.when(() -> StringUtils.join(objectArray, "A", 0, 0)).thenReturn("return_of_join1");
            //Act Statement(s)
            String result = StringUtils.join(objectArray, "A");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_join1"));
                stringUtils.verify(() -> StringUtils.join(objectArray, "A", 0, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${join22WhenArrayIsNull}, hash: ECDECBA7A978B0268E12FF5B10016BC3
    @Test()
    void join22WhenArrayIsNull() {
        /* Branches:
         * (array != null) : false
         */
        //Arrange Statement(s)
        Object[] object = null;
        //Act Statement(s)
        String result = StringUtils.join(object, "delimiter1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${join23WhenArrayIsNotNull}, hash: 93A6C22E2565B424FBB7870EF3FAAEFA
    @Disabled()
    @Test()
    void join23WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        //Act Statement(s)
        String result = StringUtils.join(objectArray, "delimiter1", 1, 2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${join23WhenArrayIsNull}, hash: D8DEA458F63B777CF72A73870D9470E5
    @Test()
    void join23WhenArrayIsNull() {
        /* Branches:
         * (array != null) : false
         */
        //Arrange Statement(s)
        Object[] object = null;
        //Act Statement(s)
        String result = StringUtils.join(object, "delimiter1", 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${join24WhenArrayIsNull}, hash: 8214E5A1A999C8A0F38FAFF1CD5E8E2C
    @Test()
    void join24WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        short[] _short = null;
        //Act Statement(s)
        String result = StringUtils.join(_short, 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${join24WhenArrayIsNotNull}, hash: 8CF4487261A5BCC7293163D42E05CFEF
    @Test()
    void join24WhenArrayIsNotNull() {
        /* Branches:
         * (array == null) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            stringUtils.when(() -> StringUtils.join(shortArray, 'A', 0, 0)).thenReturn("return_of_join1");
            //Act Statement(s)
            String result = StringUtils.join(shortArray, 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_join1"));
                stringUtils.verify(() -> StringUtils.join(shortArray, 'A', 0, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${join25WhenArrayIsNull}, hash: B48F7F6B3BCAE1845E0F3E8696D5A3CE
    @Test()
    void join25WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        short[] _short = null;
        //Act Statement(s)
        String result = StringUtils.join(_short, 'A', 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${join25WhenEndIndexMinusStartIndexLessThanOrEqualsTo0}, hash: 6338A41976E6ED28B5AA4D91263151CF
    @Test()
    void join25WhenEndIndexMinusStartIndexLessThanOrEqualsTo0() {
        /* Branches:
         * (array == null) : false
         * (endIndex - startIndex <= 0) : true
         */
        //Arrange Statement(s)
        short[] shortArray = new short[] {};
        //Act Statement(s)
        String result = StringUtils.join(shortArray, 'A', 2, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${join25WhenILessThanEndIndex}, hash: 4022A63480F7356E27BF42D37ACB09EF
    @Disabled()
    @Test()
    void join25WhenILessThanEndIndex() {
        /* Branches:
         * (array == null) : false
         * (endIndex - startIndex <= 0) : false
         * (i < endIndex) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        short[] shortArray = new short[] { (short) 2 };
        //Act Statement(s)
        String result = StringUtils.join(shortArray, 'A', 0, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${join26Test}, hash: 1919C21B2F1B7FDE3B89BB56A076EFB6
    @Test()
    void join26Test() {
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            stringUtils.when(() -> StringUtils.join(objectArray, (String) null)).thenReturn("return_of_join1");
            //Act Statement(s)
            String result = StringUtils.join(objectArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_join1"));
                stringUtils.verify(() -> StringUtils.join(objectArray, (String) null), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${joinWithWhenArrayIsNullThrowsIllegalArgumentException}, hash: BDD93A4163ECA39A4C1DB149D6FC3566
    @Test()
    void joinWithWhenArrayIsNullThrowsIllegalArgumentException() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        Object[] object = null;
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("Object varargs must not be null");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            StringUtils.joinWith("delimiter1", object);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${joinWithWhenArrayIsNotNull}, hash: BC9D65035C027FEE7C279628BD4EEC54
    @Test()
    void joinWithWhenArrayIsNotNull() {
        /* Branches:
         * (array == null) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            stringUtils.when(() -> StringUtils.join(objectArray, "delimiter1")).thenReturn("return_of_join1");
            //Act Statement(s)
            String result = StringUtils.joinWith("delimiter1", objectArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_join1"));
                stringUtils.verify(() -> StringUtils.join(objectArray, "delimiter1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${lastIndexOfTest}, hash: EB4FD03032EE9690BCC37F523E25901F
    @Test()
    void lastIndexOfTest() {
        //Act Statement(s)
        int result = StringUtils.lastIndexOf((CharSequence) "seq1", (CharSequence) "searchSeq1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${lastIndexOf1Test}, hash: 15C9522F6124AED018F8A024FE5084C1
    @Test()
    void lastIndexOf1Test() {
        //Act Statement(s)
        int result = StringUtils.lastIndexOf((CharSequence) "seq1", (CharSequence) "searchSeq1", 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${lastIndexOf2WhenIsEmptySeq}, hash: 1A5C5E81F4E17C6516ED7A32A95AD067
    @Test()
    void lastIndexOf2WhenIsEmptySeq() {
        /* Branches:
         * (isEmpty(seq)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("seq1")).thenReturn(true);
            //Act Statement(s)
            int result = StringUtils.lastIndexOf((CharSequence) "seq1", 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(-1));
                stringUtils.verify(() -> StringUtils.isEmpty("seq1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${lastIndexOf2WhenIsEmptyNotSeq}, hash: 2574ABB56D16031FCB18AAA1FE8736B1
    @Disabled()
    @Test()
    void lastIndexOf2WhenIsEmptyNotSeq() {
        /* Branches:
         * (isEmpty(seq)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("seq1")).thenReturn(false);
            //Act Statement(s)
            int result = StringUtils.lastIndexOf((CharSequence) "seq1", 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                stringUtils.verify(() -> StringUtils.isEmpty("seq1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${lastIndexOf3WhenIsEmptySeq}, hash: 25D55773AE4F003CD50047759F083C55
    @Test()
    void lastIndexOf3WhenIsEmptySeq() {
        /* Branches:
         * (isEmpty(seq)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("seq1")).thenReturn(true);
            //Act Statement(s)
            int result = StringUtils.lastIndexOf((CharSequence) "seq1", 0, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(-1));
                stringUtils.verify(() -> StringUtils.isEmpty("seq1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${lastIndexOf3WhenIsEmptyNotSeq}, hash: ACA235F7AA15A1C23BC259408638476D
    @Disabled()
    @Test()
    void lastIndexOf3WhenIsEmptyNotSeq() {
        /* Branches:
         * (isEmpty(seq)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("seq1")).thenReturn(false);
            //Act Statement(s)
            int result = StringUtils.lastIndexOf((CharSequence) "seq1", 0, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                stringUtils.verify(() -> StringUtils.isEmpty("seq1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${lastIndexOfAnyWhenSearchStrsIsNull}, hash: E7A693F6F3063BA1F6B420CB093865DD
    @Test()
    void lastIndexOfAnyWhenSearchStrsIsNull() {
        /* Branches:
         * (str == null) : false
         * (searchStrs == null) : true
         */
        //Arrange Statement(s)
        CharSequence[] charSequence = null;
        //Act Statement(s)
        int result = StringUtils.lastIndexOfAny("str1", charSequence);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${lastIndexOfAnyWhenSearchIsNull}, hash: 21757463BEB6B2F563D16B093788B1F0
    @Test()
    void lastIndexOfAnyWhenSearchIsNull() {
        /* Branches:
         * (str == null) : false
         * (searchStrs == null) : false
         * (for-each(searchStrs)) : true
         * (search == null) : true
         */
        //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] { (CharSequence) null };
        //Act Statement(s)
        int result = StringUtils.lastIndexOfAny("str1", charSequenceArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${lastIndexOfAnyWhenTmpGreaterThanRet}, hash: 72160995660CBB5C7B5A3A40F007E236
    @Disabled()
    @Test()
    void lastIndexOfAnyWhenTmpGreaterThanRet() {
        /* Branches:
         * (str == null) : false
         * (searchStrs == null) : false
         * (for-each(searchStrs)) : true
         * (search == null) : false
         * (tmp > ret) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] { "charSequence1" };
        //Act Statement(s)
        int result = StringUtils.lastIndexOfAny("str1", charSequenceArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${lastIndexOfIgnoreCaseTest}, hash: 5052E48F68E9B4723672C1AC44A16B17
    @Test()
    void lastIndexOfIgnoreCaseTest() {
        //Act Statement(s)
        int result = StringUtils.lastIndexOfIgnoreCase("str1", "searchStr1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${lastIndexOfIgnoreCase1Test}, hash: 8D78EEECE14A55FB13091B3E3F491135
    @Test()
    void lastIndexOfIgnoreCase1Test() {
        //Act Statement(s)
        int result = StringUtils.lastIndexOfIgnoreCase("str1", "searchStr1", 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${lastOrdinalIndexOfWhenOrdinalLessThanOrEqualsTo0}, hash: 9BFE832DC03892F3BB4F66C97D12A839
    @Test()
    void lastOrdinalIndexOfWhenOrdinalLessThanOrEqualsTo0() {
        /* Branches:
         * (str == null) : false  #  inside ordinalIndexOf method
         * (searchStr == null) : false  #  inside ordinalIndexOf method
         * (ordinal <= 0) : true  #  inside ordinalIndexOf method
         */
        //Act Statement(s)
        int result = StringUtils.lastOrdinalIndexOf("str1", "searchStr1", -1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${lastOrdinalIndexOfWhenLastIndex}, hash: 648DF5D1EE1EB781B2027D94E559BE91
    @Disabled()
    @Test()
    void lastOrdinalIndexOfWhenLastIndex() {
        /* Branches:
         * (str == null) : false  #  inside ordinalIndexOf method
         * (searchStr == null) : false  #  inside ordinalIndexOf method
         * (ordinal <= 0) : false  #  inside ordinalIndexOf method
         * (searchStr.length() == 0) : true  #  inside ordinalIndexOf method
         * (lastIndex) : true  #  inside ordinalIndexOf method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        int result = StringUtils.lastOrdinalIndexOf("str1", "searchStr1", 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${lastOrdinalIndexOfWhenIndexLessThan0}, hash: A8E7A7D3C67DB6250F77E0E85A587EB8
    @Test()
    void lastOrdinalIndexOfWhenIndexLessThan0() {
        /* Branches:
         * (str == null) : false  #  inside ordinalIndexOf method
         * (searchStr == null) : false  #  inside ordinalIndexOf method
         * (ordinal <= 0) : false  #  inside ordinalIndexOf method
         * (searchStr.length() == 0) : false  #  inside ordinalIndexOf method
         * (lastIndex) : true  #  inside ordinalIndexOf method
         * (lastIndex) : true  #  inside ordinalIndexOf method
         * (index < 0) : true  #  inside ordinalIndexOf method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        int result = StringUtils.lastOrdinalIndexOf("str1", "searchStr1", 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${lastOrdinalIndexOfWhenFoundNotLessThanOrdinal}, hash: 3F6B19FF1F42A9A55BE993DBBB9FB17D
    @Disabled()
    @Test()
    void lastOrdinalIndexOfWhenFoundNotLessThanOrdinal() {
        /* Branches:
         * (str == null) : false  #  inside ordinalIndexOf method
         * (searchStr == null) : false  #  inside ordinalIndexOf method
         * (ordinal <= 0) : false  #  inside ordinalIndexOf method
         * (searchStr.length() == 0) : false  #  inside ordinalIndexOf method
         * (lastIndex) : true  #  inside ordinalIndexOf method
         * (lastIndex) : true  #  inside ordinalIndexOf method
         * (index < 0) : false  #  inside ordinalIndexOf method
         * (found < ordinal) : false  #  inside ordinalIndexOf method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        int result = StringUtils.lastOrdinalIndexOf("str1", "searchStr1", 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1)));
    }

    //BaseRock generated method id: ${leftWhenStrIsNull}, hash: 323DF1E5ABC5034454F3E32E5E8155BB
    @Test()
    void leftWhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
        //Act Statement(s)
        String result = StringUtils.left((String) null, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${leftWhenLenLessThan0}, hash: B2DA6ABB75112E0D455B4760542A5D3F
    @Test()
    void leftWhenLenLessThan0() {
        /* Branches:
         * (str == null) : false
         * (len < 0) : true
         */
        //Act Statement(s)
        String result = StringUtils.left("str1", -2147483648);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${leftWhenStrLengthLessThanOrEqualsToLen}, hash: 0982495F857BBB0030EDA047740A8931
    @Test()
    void leftWhenStrLengthLessThanOrEqualsToLen() {
        /* Branches:
         * (str == null) : false
         * (len < 0) : false
         * (str.length() <= len) : true
         */
        //Act Statement(s)
        String result = StringUtils.left("A", 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${leftWhenStrLengthGreaterThanLen}, hash: 887B891E19F73D01F6FAA43B0C64FD66
    @Test()
    void leftWhenStrLengthGreaterThanLen() {
        /* Branches:
         * (str == null) : false
         * (len < 0) : false
         * (str.length() <= len) : false
         */
        //Act Statement(s)
        String result = StringUtils.left("A", 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${leftPadTest}, hash: 55D6A472FA51BB3692B38CC1218137BD
    @Test()
    void leftPadTest() {
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.leftPad("str1", 0, ' ')).thenReturn("return_of_leftPad1");
            //Act Statement(s)
            String result = StringUtils.leftPad("str1", 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_leftPad1"));
                stringUtils.verify(() -> StringUtils.leftPad("str1", 0, ' '), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${leftPad1WhenStrIsNull}, hash: B185431614CDFF7CA0E82E6446688CC6
    @Test()
    void leftPad1WhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
        //Act Statement(s)
        String result = StringUtils.leftPad((String) null, 0, 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${leftPad1WhenPadsLessThanOrEqualsTo0}, hash: 9271DDDCBD22BA4542F7F7CE1F64D3EF
    @Test()
    void leftPad1WhenPadsLessThanOrEqualsTo0() {
        /* Branches:
         * (str == null) : false
         * (pads <= 0) : true
         */
        //Act Statement(s)
        String result = StringUtils.leftPad("A", 1, 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${leftPad1WhenPadsGreaterThanPAD_LIMIT}, hash: B24A5B2EE1C639F931533A157C7F1E44
    @Test()
    void leftPad1WhenPadsGreaterThanPAD_LIMIT() {
        /* Branches:
         * (str == null) : false
         * (pads <= 0) : false
         * (pads > PAD_LIMIT) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.leftPad("", 8193, "A")).thenReturn("return_of_leftPad1");
            //Act Statement(s)
            String result = StringUtils.leftPad("", 8193, 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_leftPad1"));
                stringUtils.verify(() -> StringUtils.leftPad("", 8193, "A"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${leftPad1WhenPadsNotGreaterThanPAD_LIMIT}, hash: AA80C15C82AF66E64ADAA216E4FE0EAD
    @Test()
    void leftPad1WhenPadsNotGreaterThanPAD_LIMIT() {
        /* Branches:
         * (str == null) : false
         * (pads <= 0) : false
         * (pads > PAD_LIMIT) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.repeat('A', 1)).thenReturn("B");
            //Act Statement(s)
            String result = StringUtils.leftPad("A", 2, 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("BA"));
                stringUtils.verify(() -> StringUtils.repeat('A', 1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${leftPad2WhenStrIsNull}, hash: 1102CA229DE30349D6B1AC9F5BC741B1
    @Test()
    void leftPad2WhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
        //Act Statement(s)
        String result = StringUtils.leftPad((String) null, 0, "padStr1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${leftPad2WhenPadsLessThanOrEqualsTo0}, hash: D8C454B81B1925CF87A7CB0EBA26E5A0
    @Test()
    void leftPad2WhenPadsLessThanOrEqualsTo0() {
        /* Branches:
         * (str == null) : false
         * (isEmpty(padStr)) : true
         * (pads <= 0) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("padStr1")).thenReturn(true);
            //Act Statement(s)
            String result = StringUtils.leftPad("A", 1, "padStr1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("A"));
                stringUtils.verify(() -> StringUtils.isEmpty("padStr1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${leftPad2WhenPadsLessThanOrEqualsToPAD_LIMIT}, hash: FC4057BA037A3B16097254C35FF8E614
    @Test()
    void leftPad2WhenPadsLessThanOrEqualsToPAD_LIMIT() {
        /* Branches:
         * (str == null) : false
         * (isEmpty(padStr)) : true
         * (pads <= 0) : false
         * (padLen == 1) : true
         * (pads <= PAD_LIMIT) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("padStr1")).thenReturn(true);
            stringUtils.when(() -> StringUtils.leftPad("A", 2, ' ')).thenReturn("return_of_leftPad1");
            //Act Statement(s)
            String result = StringUtils.leftPad("A", 2, "padStr1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_leftPad1"));
                stringUtils.verify(() -> StringUtils.isEmpty("padStr1"), atLeast(1));
                stringUtils.verify(() -> StringUtils.leftPad("A", 2, ' '), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${leftPad2WhenPadsEqualsPadLen}, hash: 039751154315A717F4599F84FAD9E1D0
    @Test()
    void leftPad2WhenPadsEqualsPadLen() {
        /* Branches:
         * (str == null) : false
         * (isEmpty(padStr)) : false
         * (pads <= 0) : false
         * (padLen == 1) : false
         * (pads == padLen) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("AB")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.leftPad("C", 3, "AB");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("ABC"));
                stringUtils.verify(() -> StringUtils.isEmpty("AB"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${leftPad2WhenPadsLessThanPadLen}, hash: 742848C867E253C620D79E7BD53311F6
    @Test()
    void leftPad2WhenPadsLessThanPadLen() {
        /* Branches:
         * (str == null) : false
         * (isEmpty(padStr)) : false
         * (pads <= 0) : false
         * (padLen == 1) : false
         * (pads == padLen) : false
         * (pads < padLen) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("CD")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.leftPad("A", 2, "CD");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("CA"));
                stringUtils.verify(() -> StringUtils.isEmpty("CD"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${leftPad2WhenILessThanPads}, hash: 67DE40400D5CB023F937A7F7396885CC
    @Disabled()
    @Test()
    void leftPad2WhenILessThanPads() {
        /* Branches:
         * (str == null) : false
         * (isEmpty(padStr)) : true
         * (pads <= 0) : false
         * (padLen == 1) : true
         * (pads <= PAD_LIMIT) : false
         * (pads == padLen) : false
         * (pads < padLen) : false
         * (i < pads) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("padStr1")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.leftPad("str1", 0, "padStr1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                stringUtils.verify(() -> StringUtils.isEmpty("padStr1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${lengthWhenCsIsNull}, hash: 77BB35DA04230CCD51A9C82C14CD1C10
    @Test()
    void lengthWhenCsIsNull() {
        /* Branches:
         * (cs == null) : true
         */
        //Arrange Statement(s)
        CharSequence charSequence = null;
        //Act Statement(s)
        int result = StringUtils.length(charSequence);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${lengthWhenCsIsNotNull}, hash: E937D45893629FD2A2A39DD4A33D710D
    @Test()
    void lengthWhenCsIsNotNull() {
        /* Branches:
         * (cs == null) : false
         */
        //Act Statement(s)
        int result = StringUtils.length("cs1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(3)));
    }

    //BaseRock generated method id: ${lowerCaseWhenStrIsNull}, hash: 64ADD56D49C5397A369594B8BAB779E1
    @Test()
    void lowerCaseWhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
        //Act Statement(s)
        String result = StringUtils.lowerCase((String) null);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${lowerCaseWhenStrIsNotNull}, hash: E02C14459AD30736E71A5A8FA8B69652
    @Test()
    void lowerCaseWhenStrIsNotNull() {
        /* Branches:
         * (str == null) : false
         */
        //Act Statement(s)
        String result = StringUtils.lowerCase("A");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("a")));
    }

    //BaseRock generated method id: ${lowerCase1WhenStrIsNull}, hash: 22A7E02A1C06939B44A450A3EA1F536B
    @Test()
    void lowerCase1WhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
        //Arrange Statement(s)
        Locale locale = new Locale("language1");
        //Act Statement(s)
        String result = StringUtils.lowerCase((String) null, locale);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${lowerCase1WhenStrIsNotNull}, hash: AB7C10DAD397A9A75A8F586E545ED16C
    @Test()
    void lowerCase1WhenStrIsNotNull() {
        /* Branches:
         * (str == null) : false
         */
        //Arrange Statement(s)
        Locale locale = new Locale("language1");
        //Act Statement(s)
        String result = StringUtils.lowerCase("A", locale);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("a")));
    }

    //BaseRock generated method id: ${midWhenStrIsNull}, hash: 5C9D0667388ACECD8BF47CBB8E32C0A3
    @Test()
    void midWhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
        //Act Statement(s)
        String result = StringUtils.mid((String) null, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${midWhenPosGreaterThanStrLength}, hash: 0CC7B23D97E7A2D0DC300CCCFB94F74C
    @Test()
    void midWhenPosGreaterThanStrLength() {
        /* Branches:
         * (str == null) : false
         * (len < 0) : false
         * (pos > str.length()) : true
         */
        //Act Statement(s)
        String result = StringUtils.mid("A", 2, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${midWhenStrLengthLessThanOrEqualsToPosPlusLen}, hash: 3616CA74128DD4DED4C3A4FB37821C32
    @Test()
    void midWhenStrLengthLessThanOrEqualsToPosPlusLen() {
        /* Branches:
         * (str == null) : false
         * (len < 0) : false
         * (pos > str.length()) : false
         * (pos < 0) : true
         * (str.length() <= pos + len) : true
         */
        //Act Statement(s)
        String result = StringUtils.mid("A", -1, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${midWhenStrLengthGreaterThanPosPlusLen}, hash: B6299A45E216E6DB37A104C74B3C6B54
    @Test()
    void midWhenStrLengthGreaterThanPosPlusLen() {
        /* Branches:
         * (str == null) : false
         * (len < 0) : false
         * (pos > str.length()) : false
         * (pos < 0) : true
         * (str.length() <= pos + len) : false
         */
        //Act Statement(s)
        String result = StringUtils.mid("A", -1, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${normalizeSpaceWhenIsEmptyStr}, hash: 87C9DFBA9FA1DE447AE5562AAEBD0A75
    @Test()
    void normalizeSpaceWhenIsEmptyStr() {
        /* Branches:
         * (isEmpty(str)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(true);
            //Act Statement(s)
            String result = StringUtils.normalizeSpace("str1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("str1"));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${normalizeSpaceWhenStartWhitespacesAndStartWhitespaces}, hash: 512C92862EBEA1A5206DD8FE2A6F59D6
    @Disabled()
    @Test()
    void normalizeSpaceWhenStartWhitespacesAndStartWhitespaces() {
        /* Branches:
         * (isEmpty(str)) : false
         * (i < size) : true
         * (isWhitespace) : true
         * (whitespacesCount == 0) : true
         * (!startWhitespaces) : false
         * (startWhitespaces) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.normalizeSpace("str1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(""));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${normalizeSpaceWhenWhitespacesCountNotGreaterThan0}, hash: C489A7BF9A3223D25C4662941627066B
    @Test()
    void normalizeSpaceWhenWhitespacesCountNotGreaterThan0() {
        /* Branches:
         * (isEmpty(str)) : false
         * (i < size) : true
         * (isWhitespace) : false
         * (actualChar == 160) : true
         * (startWhitespaces) : false
         * (whitespacesCount > 0) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("\u00A0")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.normalizeSpace("\u00A0");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(""));
                stringUtils.verify(() -> StringUtils.isEmpty("\u00A0"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${normalizeSpaceWhenNotStartWhitespacesAndWhitespacesCountNotGreaterThan0}, hash: FE31E5700FB84EAE5401BFDA0157E5BB
    @Test()
    void normalizeSpaceWhenNotStartWhitespacesAndWhitespacesCountNotGreaterThan0() {
        /* Branches:
         * (isEmpty(str)) : false
         * (i < size) : true
         * (isWhitespace) : false
         * (actualChar == 160) : false
         * (startWhitespaces) : false
         * (whitespacesCount > 0) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("A")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.normalizeSpace("A");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("A"));
                stringUtils.verify(() -> StringUtils.isEmpty("A"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${ordinalIndexOfWhenOrdinalLessThanOrEqualsTo0}, hash: 84D957DDC6511EB0D6D544A0CE475EA6
    @Test()
    void ordinalIndexOfWhenOrdinalLessThanOrEqualsTo0() {
        /* Branches:
         * (str == null) : false  #  inside ordinalIndexOf method
         * (searchStr == null) : false  #  inside ordinalIndexOf method
         * (ordinal <= 0) : true  #  inside ordinalIndexOf method
         */
        //Act Statement(s)
        int result = StringUtils.ordinalIndexOf("str1", "searchStr1", -1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${ordinalIndexOfWhenNotLastIndex}, hash: 4F294C0B650BDCE639EBA160C85B68FF
    @Disabled()
    @Test()
    void ordinalIndexOfWhenNotLastIndex() {
        /* Branches:
         * (str == null) : false  #  inside ordinalIndexOf method
         * (searchStr == null) : false  #  inside ordinalIndexOf method
         * (ordinal <= 0) : false  #  inside ordinalIndexOf method
         * (searchStr.length() == 0) : true  #  inside ordinalIndexOf method
         * (lastIndex) : false  #  inside ordinalIndexOf method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        int result = StringUtils.ordinalIndexOf("str1", "searchStr1", 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${ordinalIndexOfWhenIndexLessThan0}, hash: 15A31D69E6A35457455AC6DCDA426424
    @Test()
    void ordinalIndexOfWhenIndexLessThan0() {
        /* Branches:
         * (str == null) : false  #  inside ordinalIndexOf method
         * (searchStr == null) : false  #  inside ordinalIndexOf method
         * (ordinal <= 0) : false  #  inside ordinalIndexOf method
         * (searchStr.length() == 0) : false  #  inside ordinalIndexOf method
         * (lastIndex) : false  #  inside ordinalIndexOf method
         * (lastIndex) : false  #  inside ordinalIndexOf method
         * (index < 0) : true  #  inside ordinalIndexOf method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        int result = StringUtils.ordinalIndexOf("str1", "searchStr1", 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${ordinalIndexOfWhenFoundNotLessThanOrdinal}, hash: 07823F3DEB909D8B2FCE13C33C52A803
    @Disabled()
    @Test()
    void ordinalIndexOfWhenFoundNotLessThanOrdinal() {
        /* Branches:
         * (str == null) : false  #  inside ordinalIndexOf method
         * (searchStr == null) : false  #  inside ordinalIndexOf method
         * (ordinal <= 0) : false  #  inside ordinalIndexOf method
         * (searchStr.length() == 0) : false  #  inside ordinalIndexOf method
         * (lastIndex) : false  #  inside ordinalIndexOf method
         * (lastIndex) : false  #  inside ordinalIndexOf method
         * (index < 0) : false  #  inside ordinalIndexOf method
         * (found < ordinal) : false  #  inside ordinalIndexOf method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        int result = StringUtils.ordinalIndexOf("str1", "searchStr1", 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1)));
    }

    //BaseRock generated method id: ${overlayWhenStrIsNull}, hash: CCF7682742C91CC6C3552E39C621F642
    @Test()
    void overlayWhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
        //Act Statement(s)
        String result = StringUtils.overlay((String) null, "overlay1", 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${overlayWhenStartGreaterThanEnd}, hash: BF866BFD8E4E8699A53B4C9C2C1802F4
    @Disabled()
    @Test()
    void overlayWhenStartGreaterThanEnd() {
        /* Branches:
         * (str == null) : false
         * (overlay == null) : true
         * (start < 0) : true
         * (start > len) : true
         * (end < 0) : true
         * (end > len) : true
         * (start > end) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String result = StringUtils.overlay("str1", (String) null, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${prependIfMissingTest}, hash: 7625C3D95C7F7EE13DBDA549502FB1BB
    @Disabled()
    @Test()
    void prependIfMissingTest() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: CS - Method: prependIfMissing
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] {};
        //Act Statement(s)
        String result = StringUtils.prependIfMissing("str1", "prefix1", charSequenceArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${prependIfMissingIgnoreCaseTest}, hash: A705DBDE2F7010403D371581D11A0A29
    @Disabled()
    @Test()
    void prependIfMissingIgnoreCaseTest() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: CI - Method: prependIfMissing
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] {};
        //Act Statement(s)
        String result = StringUtils.prependIfMissingIgnoreCase("str1", "prefix1", charSequenceArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${removeWhenStrIndexOfRemoveEqualsINDEX_NOT_FOUND}, hash: E7EB87BAEF29A08F521802CD19A82072
    @Test()
    void removeWhenStrIndexOfRemoveEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (isEmpty(str)) : false
         * (str.indexOf(remove) == INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.remove("", 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(""));
                stringUtils.verify(() -> StringUtils.isEmpty(""), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeWhenIIndexOfCharsNotEqualsRemove}, hash: 40812C290FA6179A172998A2E70336EC
    @Disabled()
    @Test()
    void removeWhenIIndexOfCharsNotEqualsRemove() {
        /* Branches:
         * (isEmpty(str)) : false
         * (str.indexOf(remove) == INDEX_NOT_FOUND) : false
         * (i < chars.length) : true
         * (chars[i] != remove) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("B")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.remove("B", 'B');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                stringUtils.verify(() -> StringUtils.isEmpty("B"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${remove1Test}, hash: 56A83B2A61BFD4065E471E70A51DCD6C
    @Test()
    void remove1Test() {
        //Act Statement(s)
        String result = StringUtils.remove("A", "B");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${removeAllTest}, hash: 5ECDA86353E59D781960108B1CD4D2AD
    @Test()
    void removeAllTest() {
        //Act Statement(s)
        String result = StringUtils.removeAll("A", "B");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${removeEndTest}, hash: 4C96C342E215388BE80A2F3541F3BEA8
    @Test()
    void removeEndTest() {
        //Act Statement(s)
        String result = StringUtils.removeEnd("A", "B");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${removeEndIgnoreCaseTest}, hash: E7895114CA5CBDBC07DF3EF8815ACD16
    @Test()
    void removeEndIgnoreCaseTest() {
        //Act Statement(s)
        String result = StringUtils.removeEndIgnoreCase("A", "B");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${removeFirstTest}, hash: C87CD32F0DC2AD3E9AAFD58156F05770
    @Test()
    void removeFirstTest() {
        //Act Statement(s)
        String result = StringUtils.removeFirst("A", "B");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${removeIgnoreCaseTest}, hash: 335FBDC8F973D7B72FBD52E72A0CCFCF
    @Test()
    void removeIgnoreCaseTest() {
        //Act Statement(s)
        String result = StringUtils.removeIgnoreCase("A", "B");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${removePatternTest}, hash: F65F41E4B191C7CF21D672324338ABD6
    @Test()
    void removePatternTest() {
        //Act Statement(s)
        String result = StringUtils.removePattern("A", "B");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${removeStartWhenIsEmptyStr}, hash: BF9575B0A1CF3D57EBB986D5ABCE6E8B
    @Test()
    void removeStartWhenIsEmptyStr() {
        /* Branches:
         * (isEmpty(str)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(true);
            //Act Statement(s)
            String result = StringUtils.removeStart("str1", 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("str1"));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeStartWhenStrCharAt0EqualsRemove}, hash: 81B7461A73BBD5590BF7853AA271CD95
    @Test()
    void removeStartWhenStrCharAt0EqualsRemove() {
        /* Branches:
         * (isEmpty(str)) : false
         * (str.charAt(0) == remove) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("AB")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.removeStart("AB", 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("B"));
                stringUtils.verify(() -> StringUtils.isEmpty("AB"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeStartWhenStrCharAt0NotEqualsRemove}, hash: 68CA10BA05B6E06CDF9BCEE399351A96
    @Test()
    void removeStartWhenStrCharAt0NotEqualsRemove() {
        /* Branches:
         * (isEmpty(str)) : false
         * (str.charAt(0) == remove) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("B")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.removeStart("B", 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("B"));
                stringUtils.verify(() -> StringUtils.isEmpty("B"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeStart1Test}, hash: D3713D3E97A00B74F9B518FF94A66119
    @Test()
    void removeStart1Test() {
        //Act Statement(s)
        String result = StringUtils.removeStart("A", "B");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${removeStartIgnoreCaseTest}, hash: E8C4ECE542AAEF41763E4BF8A24302F1
    @Test()
    void removeStartIgnoreCaseTest() {
        //Act Statement(s)
        String result = StringUtils.removeStartIgnoreCase("A", "B");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${repeatWhenCountLessThanOrEqualsTo0}, hash: 8020A42D5DBFCC9AD20E0730AB46F01E
    @Test()
    void repeatWhenCountLessThanOrEqualsTo0() {
        /* Branches:
         * (count <= 0) : true
         */
        //Act Statement(s)
        String result = StringUtils.repeat('A', -1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${repeatWhenCountGreaterThan0}, hash: 63F913CD7D732130B9504F42658E300A
    @Test()
    void repeatWhenCountGreaterThan0() {
        /* Branches:
         * (count <= 0) : false
         */
        //Act Statement(s)
        String result = StringUtils.repeat('A', 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${repeat1WhenRepeatIsNull}, hash: C67EF6CEB54AD86D9F32AEBEC77DDB98
    @Test()
    void repeat1WhenRepeatIsNull() {
        /* Branches:
         * (repeat == null) : true
         */
        //Act Statement(s)
        String result = StringUtils.repeat((String) null, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${repeat1WhenCountLessThanOrEqualsTo0}, hash: 639E884841F3B017A2230595052D4EC9
    @Test()
    void repeat1WhenCountLessThanOrEqualsTo0() {
        /* Branches:
         * (repeat == null) : false
         * (count <= 0) : true
         */
        //Act Statement(s)
        String result = StringUtils.repeat("repeat1", -1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${repeat1WhenInputLengthEquals0}, hash: 5CF27AFDE74CC36F9A541EEC527894AA
    @Test()
    void repeat1WhenInputLengthEquals0() {
        /* Branches:
         * (repeat == null) : false
         * (count <= 0) : false
         * (count == 1) : false
         * (inputLength == 0) : true
         */
        //Act Statement(s)
        String result = StringUtils.repeat("", 2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${repeat1WhenCountLessThanOrEqualsToPAD_LIMIT}, hash: A4D02730772FF62FB07B763BF234BF48
    @Test()
    void repeat1WhenCountLessThanOrEqualsToPAD_LIMIT() {
        /* Branches:
         * (repeat == null) : false
         * (count <= 0) : false
         * (count == 1) : false
         * (inputLength == 0) : false
         * (inputLength == 1) : true
         * (count <= PAD_LIMIT) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.repeat('A', 2)).thenReturn("return_of_repeat1");
            //Act Statement(s)
            String result = StringUtils.repeat("A", 2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_repeat1"));
                stringUtils.verify(() -> StringUtils.repeat('A', 2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${repeat1WhenSwitchInputLengthCase1}, hash: 7051B26C17534FBC20839E854AC09377
    @Test()
    void repeat1WhenSwitchInputLengthCase1() {
        /* Branches:
         * (repeat == null) : false
         * (count <= 0) : false
         * (count == 1) : false
         * (inputLength == 0) : false
         * (inputLength == 1) : true
         * (count <= PAD_LIMIT) : false
         * (switch(inputLength) = 1) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.repeat('A', 8193)).thenReturn("return_of_repeat1");
            //Act Statement(s)
            String result = StringUtils.repeat("A", 8193);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_repeat1"));
                stringUtils.verify(() -> StringUtils.repeat('A', 8193), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${repeat1WhenIGreaterThanOrEqualsTo0}, hash: EEAEB0CF71A158E16BE3C4AA36EDB718
    @Test()
    void repeat1WhenIGreaterThanOrEqualsTo0() {
        /* Branches:
         * (repeat == null) : false
         * (count <= 0) : false
         * (count == 1) : false
         * (inputLength == 0) : false
         * (inputLength == 1) : false
         * (switch(inputLength) = 2) : true
         * (i >= 0) : true
         */
        //Act Statement(s)
        String result = StringUtils.repeat("CD", 2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("CDCD")));
    }

    //BaseRock generated method id: ${repeat1WhenSwitchInputLengthCaseDefaultAndILessThanCount}, hash: 8EB15975799980E7D71394F5618F8A77
    @Disabled()
    @Test()
    void repeat1WhenSwitchInputLengthCaseDefaultAndILessThanCount() {
        /* Branches:
         * (repeat == null) : false
         * (count <= 0) : false
         * (count == 1) : false
         * (inputLength == 0) : false
         * (inputLength == 1) : true
         * (count <= PAD_LIMIT) : false
         * (switch(inputLength) = default) : true
         * (i < count) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String result = StringUtils.repeat("A", 8193);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${repeat2WhenSeparatorIsNull}, hash: ADE2128EB9D4DAE2337852844BED517B
    @Test()
    void repeat2WhenSeparatorIsNull() {
        /* Branches:
         * (repeat == null) : false
         * (separator == null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.repeat("repeat1", 0)).thenReturn("return_of_repeat1");
            //Act Statement(s)
            String result = StringUtils.repeat("repeat1", (String) null, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_repeat1"));
                stringUtils.verify(() -> StringUtils.repeat("repeat1", 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${repeat2WhenSeparatorIsNotNull}, hash: A6F0868BA8A32FF1AA848D18A8342FE8
    @Test()
    void repeat2WhenSeparatorIsNotNull() {
        /* Branches:
         * (repeat == null) : false
         * (separator == null) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.repeat("AB", 0)).thenReturn("C");
            //Act Statement(s)
            String result = StringUtils.repeat("A", "B", 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("C"));
                stringUtils.verify(() -> StringUtils.repeat("AB", 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${replaceTest}, hash: 286FB203F4E88BD8A92E050AE1368838
    @Test()
    void replaceTest() {
        //Act Statement(s)
        String result = StringUtils.replace("A", "B", "C");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${replace1Test}, hash: 665E659713A09BC3F72BB40EE3CFE14D
    @Test()
    void replace1Test() {
        //Act Statement(s)
        String result = StringUtils.replace("A", "B", "C", 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${replaceAllTest}, hash: 43000E9058618BE9814CC1026848FE40
    @Test()
    void replaceAllTest() {
        //Act Statement(s)
        String result = StringUtils.replaceAll("A", "B", "C");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${replaceCharsWhenStrIsNull}, hash: 706B10E1E164F7F07CADBF8202D2A4C7
    @Test()
    void replaceCharsWhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
        //Act Statement(s)
        String result = StringUtils.replaceChars((String) null, 'A', 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${replaceCharsWhenStrIsNotNull}, hash: 88558D69F2108894B8142E8EE853CE7F
    @Test()
    void replaceCharsWhenStrIsNotNull() {
        /* Branches:
         * (str == null) : false
         */
        //Act Statement(s)
        String result = StringUtils.replaceChars("A", 'A', 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${replaceChars1WhenIsEmptySearchChars}, hash: 6690F38692BCE407D617C928A0D27EF8
    @Test()
    void replaceChars1WhenIsEmptySearchChars() {
        /* Branches:
         * (isEmpty(str)) : false
         * (isEmpty(searchChars)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("searchChars1")).thenReturn(true);
            //Act Statement(s)
            String result = StringUtils.replaceChars("str1", "searchChars1", "replaceChars1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("str1"));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("searchChars1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${replaceChars1WhenIndexLessThan0AndNotModified}, hash: 66F63BDDE55F7922807062E0A77A0704
    @Test()
    void replaceChars1WhenIndexLessThan0AndNotModified() {
        /* Branches:
         * (isEmpty(str)) : false
         * (isEmpty(searchChars)) : false
         * (i < strLength) : true
         * (index >= 0) : false
         * (modified) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("B")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("E")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.replaceChars("B", "E", "C");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("B"));
                stringUtils.verify(() -> StringUtils.isEmpty("B"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("E"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${replaceChars1WhenModified}, hash: 6DA260C31BB943982343F8C2181823F0
    @Test()
    void replaceChars1WhenModified() {
        /* Branches:
         * (isEmpty(str)) : false
         * (isEmpty(searchChars)) : false
         * (i < strLength) : true
         * (index >= 0) : true
         * (index < replaceCharsLength) : true
         * (modified) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("B")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.replaceChars("B", "B", "D");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("D"));
                stringUtils.verify(() -> StringUtils.isEmpty("B"), atLeast(2));
            });
        }
    }

    //BaseRock generated method id: ${replaceEachWhenArrayUtilsIsEmptyReplacementList}, hash: 629941514DC5DF50DF2A6AE228941C7F
    @Test()
    void replaceEachWhenArrayUtilsIsEmptyReplacementList() {
        /* Branches:
         * (isEmpty(text)) : false  #  inside replaceEach method
         * (ArrayUtils.isEmpty(searchList)) : false  #  inside replaceEach method
         * (ArrayUtils.isEmpty(replacementList)) : true  #  inside replaceEach method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("text1")).thenReturn(false);
            String[] stringArray = new String[] {};
            String[] stringArray2 = new String[] {};
            //Act Statement(s)
            String result = StringUtils.replaceEach("text1", stringArray, stringArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("text1"));
                stringUtils.verify(() -> StringUtils.isEmpty("text1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${replaceEachWhenSearchLengthNotEqualsReplacementLengthThrowsIllegalArgumentException}, hash: 1479F8DE1632FE7836B90C8220B89EDB
    @Disabled()
    @Test()
    void replaceEachWhenSearchLengthNotEqualsReplacementLengthThrowsIllegalArgumentException() {
        /* Branches:
         * (isEmpty(text)) : false  #  inside replaceEach method
         * (ArrayUtils.isEmpty(searchList)) : false  #  inside replaceEach method
         * (ArrayUtils.isEmpty(replacementList)) : false  #  inside replaceEach method
         * (timeToLive < 0) : false  #  inside replaceEach method
         * (searchLength != replacementLength) : true  #  inside replaceEach method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("text1")).thenReturn(false);
            String[] stringArray = new String[] {};
            String[] stringArray2 = new String[] {};
            //Act Statement(s)
            final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
                StringUtils.replaceEach("text1", stringArray, stringArray2);
            });
            IllegalArgumentException illegalArgumentException = new IllegalArgumentException("s1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
                stringUtils.verify(() -> StringUtils.isEmpty("text1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${replaceEachWhenTextIndexEqualsMinus1}, hash: C094614108B2D24C32C98A3096A5310C
    @Test()
    void replaceEachWhenTextIndexEqualsMinus1() {
        /* Branches:
         * (isEmpty(text)) : false  #  inside replaceEach method
         * (ArrayUtils.isEmpty(searchList)) : false  #  inside replaceEach method
         * (ArrayUtils.isEmpty(replacementList)) : false  #  inside replaceEach method
         * (timeToLive < 0) : false  #  inside replaceEach method
         * (searchLength != replacementLength) : false  #  inside replaceEach method
         * (i < searchLength) : true  #  inside replaceEach method
         * (noMoreMatchesForReplIndex[i]) : false  #  inside replaceEach method
         * (isEmpty(searchList[i])) : false  #  inside replaceEach method
         * (replacementList[i] == null) : true  #  inside replaceEach method
         * (textIndex == -1) : true  #  inside replaceEach method
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("text1")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("searchListItem1")).thenReturn(false);
            String[] stringArray = new String[] { "searchListItem1" };
            String[] stringArray2 = new String[] { (String) null };
            //Act Statement(s)
            String result = StringUtils.replaceEach("text1", stringArray, stringArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("text1"));
                stringUtils.verify(() -> StringUtils.isEmpty("text1"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("searchListItem1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${replaceEachWhenTempIndexEqualsMinus1AndTextIndexEqualsMinus1}, hash: 1FB2361B436BB2B90618E404C8B289B4
    @Test()
    void replaceEachWhenTempIndexEqualsMinus1AndTextIndexEqualsMinus1() {
        /* Branches:
         * (isEmpty(text)) : false  #  inside replaceEach method
         * (ArrayUtils.isEmpty(searchList)) : false  #  inside replaceEach method
         * (ArrayUtils.isEmpty(replacementList)) : false  #  inside replaceEach method
         * (timeToLive < 0) : false  #  inside replaceEach method
         * (searchLength != replacementLength) : false  #  inside replaceEach method
         * (i < searchLength) : true  #  inside replaceEach method
         * (noMoreMatchesForReplIndex[i]) : false  #  inside replaceEach method
         * (isEmpty(searchList[i])) : false  #  inside replaceEach method
         * (replacementList[i] == null) : false  #  inside replaceEach method
         * (tempIndex == -1) : true  #  inside replaceEach method
         * (textIndex == -1) : true  #  inside replaceEach method
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("B")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("A")).thenReturn(false);
            String[] stringArray = new String[] { "A" };
            String[] stringArray2 = new String[] { "replacementListItem1" };
            //Act Statement(s)
            String result = StringUtils.replaceEach("B", stringArray, stringArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("B"));
                stringUtils.verify(() -> StringUtils.isEmpty("B"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("A"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${replaceEachWhenTempIndexEqualsMinus1AndILessThanTextLengthAndNotRepeat}, hash: C87905DE86D614F64A899026AF75FAFA
    @Test()
    void replaceEachWhenTempIndexEqualsMinus1AndILessThanTextLengthAndNotRepeat() {
        /* Branches:
         * (isEmpty(text)) : false  #  inside replaceEach method
         * (ArrayUtils.isEmpty(searchList)) : false  #  inside replaceEach method
         * (ArrayUtils.isEmpty(replacementList)) : false  #  inside replaceEach method
         * (timeToLive < 0) : false  #  inside replaceEach method
         * (searchLength != replacementLength) : false  #  inside replaceEach method
         * (i < searchLength) : true  #  inside replaceEach method
         * (noMoreMatchesForReplIndex[i]) : false  #  inside replaceEach method
         * (isEmpty(searchList[i])) : false  #  inside replaceEach method
         * (replacementList[i] == null) : false  #  inside replaceEach method
         * (tempIndex == -1) : false  #  inside replaceEach method
         * (textIndex == -1) : true  #  inside replaceEach method
         * (textIndex == -1) : false  #  inside replaceEach method
         * (i < searchList.length) : true  #  inside replaceEach method
         * (searchList[i] == null) : false  #  inside replaceEach method
         * (replacementList[i] == null) : false  #  inside replaceEach method
         * (greater > 0) : true  #  inside replaceEach method
         * (textIndex != -1) : true  #  inside replaceEach method
         * (i < textIndex) : true  #  inside replaceEach method
         * (i < searchLength) : true  #  inside replaceEach method
         * (noMoreMatchesForReplIndex[i]) : false  #  inside replaceEach method
         * (isEmpty(searchList[i])) : false  #  inside replaceEach method
         * (replacementList[i] == null) : false  #  inside replaceEach method
         * (tempIndex == -1) : true  #  inside replaceEach method
         * (i < textLength) : true  #  inside replaceEach method
         * (!repeat) : true  #  inside replaceEach method
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("BHCG")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("C")).thenReturn(false);
            String[] stringArray = new String[] { "C" };
            String[] stringArray2 = new String[] { "AF" };
            //Act Statement(s)
            String result = StringUtils.replaceEach("BHCG", stringArray, stringArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("BHAFG"));
                stringUtils.verify(() -> StringUtils.isEmpty("BHCG"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("C"), atLeast(2));
            });
        }
    }

    //BaseRock generated method id: ${replaceEachWhenTextIndexEqualsMinus1AndILessThanTextLengthAndNotRepeat}, hash: 2B63544BF70F50074B9E1564B7453E3C
    @Disabled()
    @Test()
    void replaceEachWhenTextIndexEqualsMinus1AndILessThanTextLengthAndNotRepeat() {
        /* Branches:
         * (isEmpty(text)) : false  #  inside replaceEach method
         * (ArrayUtils.isEmpty(searchList)) : false  #  inside replaceEach method
         * (ArrayUtils.isEmpty(replacementList)) : false  #  inside replaceEach method
         * (timeToLive < 0) : false  #  inside replaceEach method
         * (searchLength != replacementLength) : false  #  inside replaceEach method
         * (i < searchLength) : true  #  inside replaceEach method
         * (noMoreMatchesForReplIndex[i]) : false  #  inside replaceEach method
         * (isEmpty(searchList[i])) : false  #  inside replaceEach method
         * (replacementList[i] == null) : false  #  inside replaceEach method
         * (tempIndex == -1) : false  #  inside replaceEach method
         * (textIndex == -1) : true  #  inside replaceEach method
         * (textIndex == -1) : false  #  inside replaceEach method
         * (i < searchList.length) : true  #  inside replaceEach method
         * (searchList[i] == null) : false  #  inside replaceEach method
         * (replacementList[i] == null) : false  #  inside replaceEach method
         * (greater > 0) : true  #  inside replaceEach method
         * (textIndex != -1) : true  #  inside replaceEach method
         * (i < textIndex) : true  #  inside replaceEach method
         * (i < searchLength) : true  #  inside replaceEach method
         * (noMoreMatchesForReplIndex[i]) : false  #  inside replaceEach method
         * (isEmpty(searchList[i])) : false  #  inside replaceEach method
         * (replacementList[i] == null) : false  #  inside replaceEach method
         * (tempIndex == -1) : false  #  inside replaceEach method
         * (textIndex == -1) : true  #  inside replaceEach method
         * (i < textLength) : true  #  inside replaceEach method
         * (!repeat) : true  #  inside replaceEach method
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("BGCC")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("C")).thenReturn(false);
            String[] stringArray = new String[] { "C" };
            String[] stringArray2 = new String[] { "AF" };
            //Act Statement(s)
            String result = StringUtils.replaceEach("BGCC", stringArray, stringArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("BGAFC"));
                stringUtils.verify(() -> StringUtils.isEmpty("BGCC"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("C"), atLeast(2));
            });
        }
    }

    //BaseRock generated method id: ${replaceEachRepeatedlyWhenArrayUtilsIsEmptyReplacementList}, hash: B7C699ED94F290E7C70C2AE5622EC49F
    @Test()
    void replaceEachRepeatedlyWhenArrayUtilsIsEmptyReplacementList() {
        /* Branches:
         * (isEmpty(text)) : false  #  inside replaceEach method
         * (ArrayUtils.isEmpty(searchList)) : false  #  inside replaceEach method
         * (ArrayUtils.isEmpty(replacementList)) : true  #  inside replaceEach method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("text1")).thenReturn(false);
            String[] stringArray = new String[] {};
            String[] stringArray2 = new String[] {};
            //Act Statement(s)
            String result = StringUtils.replaceEachRepeatedly("text1", stringArray, stringArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("text1"));
                stringUtils.verify(() -> StringUtils.isEmpty("text1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${replaceEachRepeatedlyWhenTimeToLiveLessThan0ThrowsIllegalStateException}, hash: 3EB8C86AE19D0850C6E28646CD77199B
    @Disabled()
    @Test()
    void replaceEachRepeatedlyWhenTimeToLiveLessThan0ThrowsIllegalStateException() {
        /* Branches:
         * (isEmpty(text)) : false  #  inside replaceEach method
         * (ArrayUtils.isEmpty(searchList)) : false  #  inside replaceEach method
         * (ArrayUtils.isEmpty(replacementList)) : false  #  inside replaceEach method
         * (timeToLive < 0) : true  #  inside replaceEach method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("text1")).thenReturn(false);
            String[] stringArray = new String[] {};
            String[] stringArray2 = new String[] {};
            //Act Statement(s)
            final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
                StringUtils.replaceEachRepeatedly("text1", stringArray, stringArray2);
            });
            IllegalStateException illegalStateException = new IllegalStateException("Aborting to protect against StackOverflowError - output of one loop is the input of another");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
                stringUtils.verify(() -> StringUtils.isEmpty("text1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${replaceEachRepeatedlyWhenSearchLengthNotEqualsReplacementLengthThrowsIllegalArgumentException}, hash: EEF51B88815CCE91C398823B59598499
    @Disabled()
    @Test()
    void replaceEachRepeatedlyWhenSearchLengthNotEqualsReplacementLengthThrowsIllegalArgumentException() {
        /* Branches:
         * (isEmpty(text)) : false  #  inside replaceEach method
         * (ArrayUtils.isEmpty(searchList)) : false  #  inside replaceEach method
         * (ArrayUtils.isEmpty(replacementList)) : false  #  inside replaceEach method
         * (timeToLive < 0) : false  #  inside replaceEach method
         * (searchLength != replacementLength) : true  #  inside replaceEach method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("text1")).thenReturn(false);
            String[] stringArray = new String[] {};
            String[] stringArray2 = new String[] { "replacementListItem1" };
            //Act Statement(s)
            final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
                StringUtils.replaceEachRepeatedly("text1", stringArray, stringArray2);
            });
            IllegalArgumentException illegalArgumentException = new IllegalArgumentException("Search and Replace array lengths don't match: 0 vs 1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
                stringUtils.verify(() -> StringUtils.isEmpty("text1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${replaceEachRepeatedlyWhenTextIndexEqualsMinus1}, hash: 3564B503E10307740CCF20860F5EC370
    @Test()
    void replaceEachRepeatedlyWhenTextIndexEqualsMinus1() {
        /* Branches:
         * (isEmpty(text)) : false  #  inside replaceEach method
         * (ArrayUtils.isEmpty(searchList)) : false  #  inside replaceEach method
         * (ArrayUtils.isEmpty(replacementList)) : false  #  inside replaceEach method
         * (timeToLive < 0) : false  #  inside replaceEach method
         * (searchLength != replacementLength) : false  #  inside replaceEach method
         * (i < searchLength) : true  #  inside replaceEach method
         * (noMoreMatchesForReplIndex[i]) : false  #  inside replaceEach method
         * (isEmpty(searchList[i])) : false  #  inside replaceEach method
         * (replacementList[i] == null) : true  #  inside replaceEach method
         * (textIndex == -1) : true  #  inside replaceEach method
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("text1")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("searchListItem1")).thenReturn(false);
            String[] stringArray = new String[] { "searchListItem1" };
            String[] stringArray2 = new String[] { (String) null };
            //Act Statement(s)
            String result = StringUtils.replaceEachRepeatedly("text1", stringArray, stringArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("text1"));
                stringUtils.verify(() -> StringUtils.isEmpty("text1"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("searchListItem1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${replaceEachRepeatedlyWhenTempIndexEqualsMinus1AndTextIndexEqualsMinus1}, hash: 20AA784361103B02E0BC728E2DBDFF05
    @Test()
    void replaceEachRepeatedlyWhenTempIndexEqualsMinus1AndTextIndexEqualsMinus1() {
        /* Branches:
         * (isEmpty(text)) : false  #  inside replaceEach method
         * (ArrayUtils.isEmpty(searchList)) : false  #  inside replaceEach method
         * (ArrayUtils.isEmpty(replacementList)) : false  #  inside replaceEach method
         * (timeToLive < 0) : false  #  inside replaceEach method
         * (searchLength != replacementLength) : false  #  inside replaceEach method
         * (i < searchLength) : true  #  inside replaceEach method
         * (noMoreMatchesForReplIndex[i]) : false  #  inside replaceEach method
         * (isEmpty(searchList[i])) : false  #  inside replaceEach method
         * (replacementList[i] == null) : false  #  inside replaceEach method
         * (tempIndex == -1) : true  #  inside replaceEach method
         * (textIndex == -1) : true  #  inside replaceEach method
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("E")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("D")).thenReturn(false);
            String[] stringArray = new String[] { "D" };
            String[] stringArray2 = new String[] { "replacementListItem1" };
            //Act Statement(s)
            String result = StringUtils.replaceEachRepeatedly("E", stringArray, stringArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("E"));
                stringUtils.verify(() -> StringUtils.isEmpty("E"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("D"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${replaceEachRepeatedlyWhenTempIndexEqualsMinus1AndILessThanTextLengthAndRepeat}, hash: 843EDD83AADB9ED15958336BB63A4E79
    @Disabled()
    @Test()
    void replaceEachRepeatedlyWhenTempIndexEqualsMinus1AndILessThanTextLengthAndRepeat() {
        /* Branches:
         * (isEmpty(text)) : false  #  inside replaceEach method
         * (ArrayUtils.isEmpty(searchList)) : false  #  inside replaceEach method
         * (ArrayUtils.isEmpty(replacementList)) : false  #  inside replaceEach method
         * (timeToLive < 0) : false  #  inside replaceEach method
         * (searchLength != replacementLength) : false  #  inside replaceEach method
         * (i < searchLength) : true  #  inside replaceEach method
         * (noMoreMatchesForReplIndex[i]) : false  #  inside replaceEach method
         * (isEmpty(searchList[i])) : false  #  inside replaceEach method
         * (replacementList[i] == null) : false  #  inside replaceEach method
         * (tempIndex == -1) : false  #  inside replaceEach method
         * (textIndex == -1) : true  #  inside replaceEach method
         * (textIndex == -1) : false  #  inside replaceEach method
         * (i < searchList.length) : true  #  inside replaceEach method
         * (searchList[i] == null) : false  #  inside replaceEach method
         * (replacementList[i] == null) : false  #  inside replaceEach method
         * (greater > 0) : true  #  inside replaceEach method
         * (textIndex != -1) : true  #  inside replaceEach method
         * (i < textIndex) : true  #  inside replaceEach method
         * (i < searchLength) : true  #  inside replaceEach method
         * (noMoreMatchesForReplIndex[i]) : false  #  inside replaceEach method
         * (isEmpty(searchList[i])) : false  #  inside replaceEach method
         * (replacementList[i] == null) : false  #  inside replaceEach method
         * (tempIndex == -1) : true  #  inside replaceEach method
         * (i < textLength) : true  #  inside replaceEach method
         * (!repeat) : false  #  inside replaceEach method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("DGF")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("G")).thenReturn(false);
            String[] stringArray = new String[] { "G" };
            String[] stringArray2 = new String[] { "BE" };
            //Act Statement(s)
            String result = StringUtils.replaceEachRepeatedly("DGF", stringArray, stringArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                stringUtils.verify(() -> StringUtils.isEmpty("DGF"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("G"), atLeast(2));
            });
        }
    }

    //BaseRock generated method id: ${replaceEachRepeatedlyWhenTextIndexEqualsMinus1AndILessThanTextLengthAndRepeat}, hash: 98DDE8F433ABFD56B0083F2C19E05864
    @Disabled()
    @Test()
    void replaceEachRepeatedlyWhenTextIndexEqualsMinus1AndILessThanTextLengthAndRepeat() {
        /* Branches:
         * (isEmpty(text)) : false  #  inside replaceEach method
         * (ArrayUtils.isEmpty(searchList)) : false  #  inside replaceEach method
         * (ArrayUtils.isEmpty(replacementList)) : false  #  inside replaceEach method
         * (timeToLive < 0) : false  #  inside replaceEach method
         * (searchLength != replacementLength) : false  #  inside replaceEach method
         * (i < searchLength) : true  #  inside replaceEach method
         * (noMoreMatchesForReplIndex[i]) : false  #  inside replaceEach method
         * (isEmpty(searchList[i])) : false  #  inside replaceEach method
         * (replacementList[i] == null) : false  #  inside replaceEach method
         * (tempIndex == -1) : false  #  inside replaceEach method
         * (textIndex == -1) : true  #  inside replaceEach method
         * (textIndex == -1) : false  #  inside replaceEach method
         * (i < searchList.length) : true  #  inside replaceEach method
         * (searchList[i] == null) : false  #  inside replaceEach method
         * (replacementList[i] == null) : false  #  inside replaceEach method
         * (greater > 0) : true  #  inside replaceEach method
         * (textIndex != -1) : true  #  inside replaceEach method
         * (i < textIndex) : true  #  inside replaceEach method
         * (i < searchLength) : true  #  inside replaceEach method
         * (noMoreMatchesForReplIndex[i]) : false  #  inside replaceEach method
         * (isEmpty(searchList[i])) : false  #  inside replaceEach method
         * (replacementList[i] == null) : false  #  inside replaceEach method
         * (tempIndex == -1) : false  #  inside replaceEach method
         * (textIndex == -1) : true  #  inside replaceEach method
         * (i < textLength) : true  #  inside replaceEach method
         * (!repeat) : false  #  inside replaceEach method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("DEE")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("E")).thenReturn(false);
            String[] stringArray = new String[] { "E" };
            String[] stringArray2 = new String[] { "BF" };
            //Act Statement(s)
            String result = StringUtils.replaceEachRepeatedly("DEE", stringArray, stringArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                stringUtils.verify(() -> StringUtils.isEmpty("DEE"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("E"), atLeast(2));
            });
        }
    }

    //BaseRock generated method id: ${replaceFirstTest}, hash: 03FAFCA578A574F449F35B9F29DF1863
    @Test()
    void replaceFirstTest() {
        //Act Statement(s)
        String result = StringUtils.replaceFirst("A", "B", "C");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${replaceIgnoreCaseTest}, hash: 9DCB6C60B20FA4EFD2F8FB8C070DFECD
    @Test()
    void replaceIgnoreCaseTest() {
        //Act Statement(s)
        String result = StringUtils.replaceIgnoreCase("A", "B", "C");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${replaceIgnoreCase1Test}, hash: 0ED098B99F52C80B797C48CB98B95339
    @Test()
    void replaceIgnoreCase1Test() {
        //Act Statement(s)
        String result = StringUtils.replaceIgnoreCase("A", "B", "C", 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${replaceOnceTest}, hash: 8E8DDBE077A4E748CCF175830EC79EAF
    @Test()
    void replaceOnceTest() {
        //Act Statement(s)
        String result = StringUtils.replaceOnce("A", "B", "C");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${replaceOnceIgnoreCaseTest}, hash: 4686BC6A7D96BACAB6347E198962D22C
    @Test()
    void replaceOnceIgnoreCaseTest() {
        //Act Statement(s)
        String result = StringUtils.replaceOnceIgnoreCase("A", "B", "C");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${replacePatternTest}, hash: 1099617C07664762A9DF28509E35B505
    @Test()
    void replacePatternTest() {
        //Act Statement(s)
        String result = StringUtils.replacePattern("A", "B", "C");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${reverseWhenStrIsNull}, hash: 40EA11E06D957D6AE7819B0B1184856F
    @Test()
    void reverseWhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
        //Act Statement(s)
        String result = StringUtils.reverse((String) null);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${reverseWhenStrIsNotNull}, hash: D70E0F12AF07DEDD1FB2958D3DA824A7
    @Disabled()
    @Test()
    void reverseWhenStrIsNotNull() {
        /* Branches:
         * (str == null) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String result = StringUtils.reverse("str1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${reverseDelimitedTest}, hash: 1F325096707588D02235ECC283C9668A
    @Test()
    void reverseDelimitedTest() {
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            String[] stringArray = new String[] {};
            stringUtils.when(() -> StringUtils.split("str1", 'A')).thenReturn(stringArray);
            stringUtils.when(() -> StringUtils.join(stringArray, 'A')).thenReturn("return_of_join1");
            //Act Statement(s)
            String result = StringUtils.reverseDelimited("str1", 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_join1"));
                stringUtils.verify(() -> StringUtils.split("str1", 'A'), atLeast(1));
                stringUtils.verify(() -> StringUtils.join(stringArray, 'A'), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${rightWhenStrIsNull}, hash: D108E54E466BE18A87A3DDC5A4E15962
    @Test()
    void rightWhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
        //Act Statement(s)
        String result = StringUtils.right((String) null, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${rightWhenLenLessThan0}, hash: 4EDCA794FA931A44C971187DC2661F15
    @Test()
    void rightWhenLenLessThan0() {
        /* Branches:
         * (str == null) : false
         * (len < 0) : true
         */
        //Act Statement(s)
        String result = StringUtils.right("str1", -2147483648);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${rightWhenStrLengthLessThanOrEqualsToLen}, hash: 90D90AEE2489F50EF2035C8D0FDA450F
    @Test()
    void rightWhenStrLengthLessThanOrEqualsToLen() {
        /* Branches:
         * (str == null) : false
         * (len < 0) : false
         * (str.length() <= len) : true
         */
        //Act Statement(s)
        String result = StringUtils.right("A", 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${rightWhenStrLengthGreaterThanLen}, hash: 1EDB3AC69C02E04CD45B0E6D5CECDE48
    @Test()
    void rightWhenStrLengthGreaterThanLen() {
        /* Branches:
         * (str == null) : false
         * (len < 0) : false
         * (str.length() <= len) : false
         */
        //Act Statement(s)
        String result = StringUtils.right("AB", 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("B")));
    }

    //BaseRock generated method id: ${rightPadTest}, hash: 4588D1B0E92BF38E6E9CFDCED235BE5B
    @Test()
    void rightPadTest() {
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.rightPad("str1", 0, ' ')).thenReturn("return_of_rightPad1");
            //Act Statement(s)
            String result = StringUtils.rightPad("str1", 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_rightPad1"));
                stringUtils.verify(() -> StringUtils.rightPad("str1", 0, ' '), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${rightPad1WhenStrIsNull}, hash: A3827FD9391C139F06A3A95B0D26EDFD
    @Test()
    void rightPad1WhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
        //Act Statement(s)
        String result = StringUtils.rightPad((String) null, 0, 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${rightPad1WhenPadsLessThanOrEqualsTo0}, hash: 8565B109EE1B24EC997518DA9D88E7FD
    @Test()
    void rightPad1WhenPadsLessThanOrEqualsTo0() {
        /* Branches:
         * (str == null) : false
         * (pads <= 0) : true
         */
        //Act Statement(s)
        String result = StringUtils.rightPad("A", 1, 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${rightPad1WhenPadsGreaterThanPAD_LIMIT}, hash: D69D215664BDE775D6F782E6A090141A
    @Test()
    void rightPad1WhenPadsGreaterThanPAD_LIMIT() {
        /* Branches:
         * (str == null) : false
         * (pads <= 0) : false
         * (pads > PAD_LIMIT) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.rightPad("", 8193, "A")).thenReturn("return_of_rightPad1");
            //Act Statement(s)
            String result = StringUtils.rightPad("", 8193, 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_rightPad1"));
                stringUtils.verify(() -> StringUtils.rightPad("", 8193, "A"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${rightPad1WhenPadsNotGreaterThanPAD_LIMIT}, hash: 0ED279410AD8122346342139E4D3E9A5
    @Test()
    void rightPad1WhenPadsNotGreaterThanPAD_LIMIT() {
        /* Branches:
         * (str == null) : false
         * (pads <= 0) : false
         * (pads > PAD_LIMIT) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.repeat('A', 1)).thenReturn("B");
            //Act Statement(s)
            String result = StringUtils.rightPad("A", 2, 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("AB"));
                stringUtils.verify(() -> StringUtils.repeat('A', 1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${rightPad2WhenStrIsNull}, hash: 91FDA30BC83D8DFB1F91AEFD8109FC20
    @Test()
    void rightPad2WhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
        //Act Statement(s)
        String result = StringUtils.rightPad((String) null, 0, "padStr1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${rightPad2WhenPadsLessThanOrEqualsTo0}, hash: 5158D92A29E18E4B67C28ABB3D2C0083
    @Test()
    void rightPad2WhenPadsLessThanOrEqualsTo0() {
        /* Branches:
         * (str == null) : false
         * (isEmpty(padStr)) : true
         * (pads <= 0) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("padStr1")).thenReturn(true);
            //Act Statement(s)
            String result = StringUtils.rightPad("A", 1, "padStr1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("A"));
                stringUtils.verify(() -> StringUtils.isEmpty("padStr1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${rightPad2WhenPadsLessThanOrEqualsToPAD_LIMIT}, hash: D3AB829AC44E742BA3BD511B32E973BA
    @Test()
    void rightPad2WhenPadsLessThanOrEqualsToPAD_LIMIT() {
        /* Branches:
         * (str == null) : false
         * (isEmpty(padStr)) : true
         * (pads <= 0) : false
         * (padLen == 1) : true
         * (pads <= PAD_LIMIT) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("padStr1")).thenReturn(true);
            stringUtils.when(() -> StringUtils.rightPad("A", 2, ' ')).thenReturn("return_of_rightPad1");
            //Act Statement(s)
            String result = StringUtils.rightPad("A", 2, "padStr1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_rightPad1"));
                stringUtils.verify(() -> StringUtils.isEmpty("padStr1"), atLeast(1));
                stringUtils.verify(() -> StringUtils.rightPad("A", 2, ' '), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${rightPad2WhenPadsEqualsPadLen}, hash: 9549B93F5167C42D65453BF3AF65E313
    @Test()
    void rightPad2WhenPadsEqualsPadLen() {
        /* Branches:
         * (str == null) : false
         * (isEmpty(padStr)) : false
         * (pads <= 0) : false
         * (padLen == 1) : false
         * (pads == padLen) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("AB")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.rightPad("C", 3, "AB");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("CAB"));
                stringUtils.verify(() -> StringUtils.isEmpty("AB"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${rightPad2WhenPadsLessThanPadLen}, hash: EC0A8D1B9365E7E557848F1807F6419B
    @Test()
    void rightPad2WhenPadsLessThanPadLen() {
        /* Branches:
         * (str == null) : false
         * (isEmpty(padStr)) : false
         * (pads <= 0) : false
         * (padLen == 1) : false
         * (pads == padLen) : false
         * (pads < padLen) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("CD")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.rightPad("A", 2, "CD");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("AC"));
                stringUtils.verify(() -> StringUtils.isEmpty("CD"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${rightPad2WhenILessThanPads}, hash: 47BBF1BDE6DA350FDF8C4F025998770F
    @Disabled()
    @Test()
    void rightPad2WhenILessThanPads() {
        /* Branches:
         * (str == null) : false
         * (isEmpty(padStr)) : true
         * (pads <= 0) : false
         * (padLen == 1) : true
         * (pads <= PAD_LIMIT) : false
         * (pads == padLen) : false
         * (pads < padLen) : false
         * (i < pads) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("padStr1")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.rightPad("str1", 0, "padStr1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                stringUtils.verify(() -> StringUtils.isEmpty("padStr1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${rotateWhenStrIsNull}, hash: 3202E55BFD53DE72563110B7A7E9AA9B
    @Test()
    void rotateWhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
        //Act Statement(s)
        String result = StringUtils.rotate((String) null, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${rotateWhenShiftModulusStrLenEquals0}, hash: 3BB3EF556385F71546BB2C8F70AC30CC
    @Test()
    void rotateWhenShiftModulusStrLenEquals0() {
        /* Branches:
         * (str == null) : false
         * (shift == 0) : false
         * (strLen == 0) : false
         * (shift % strLen == 0) : true
         */
        //Act Statement(s)
        String result = StringUtils.rotate("A", 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${rotateWhenShiftModulusStrLenNotEquals0}, hash: 6297F0EE17F19F596D58498B0E1E462F
    @Test()
    void rotateWhenShiftModulusStrLenNotEquals0() {
        /* Branches:
         * (str == null) : false
         * (shift == 0) : false
         * (strLen == 0) : false
         * (shift % strLen == 0) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.substring("AB", -1)).thenReturn("C");
            stringUtils.when(() -> StringUtils.substring("AB", 0, -1)).thenReturn("D");
            //Act Statement(s)
            String result = StringUtils.rotate("AB", 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("CD"));
                stringUtils.verify(() -> StringUtils.substring("AB", -1), atLeast(1));
                stringUtils.verify(() -> StringUtils.substring("AB", 0, -1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${splitTest}, hash: 1F4E0C277F8B4B4FE5F6B08949D9C486
    @Test()
    void splitTest() {
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            String[] stringArray = new String[] {};
            stringUtils.when(() -> StringUtils.split("str1", (String) null, -1)).thenReturn(stringArray);
            //Act Statement(s)
            String[] result = StringUtils.split("str1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(stringArray));
                stringUtils.verify(() -> StringUtils.split("str1", (String) null, -1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${split1WhenStrIsNull}, hash: 8496A54EFA11E1D7EC9418B71C20FA98
    @Test()
    void split1WhenStrIsNull() {
        /* Branches:
         * (str == null) : true  #  inside splitWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.split((String) null, 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${split1WhenLenEquals0}, hash: 2E58D0ACE4094458AC7EF7930F1F57BF
    @Test()
    void split1WhenLenEquals0() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : true  #  inside splitWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.split("", 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${split1WhenMatch}, hash: 259E5D6C87EF85AB61B51451100751E8
    @Test()
    void split1WhenMatch() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (str.charAt(i) == separatorChar) : false  #  inside splitWorker method
         * (match) : true  #  inside splitWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.split("A", 'B');
        String[] stringResultArray = new String[] { "A" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${split1WhenNotPreserveAllTokens}, hash: ADDB2356B2D0C83946C02600D5F91DBB
    @Test()
    void split1WhenNotPreserveAllTokens() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (str.charAt(i) == separatorChar) : true  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : false  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : false  #  inside splitWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.split("A", 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${split2WhenStrIsNull}, hash: 17A6538AB40878CF685862503671E9FE
    @Test()
    void split2WhenStrIsNull() {
        /* Branches:
         * (str == null) : true  #  inside splitWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.split((String) null, "separatorChars1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${split2WhenLenEquals0}, hash: 3B3AC4DC52B2251836C68EBCFE11D93B
    @Test()
    void split2WhenLenEquals0() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : true  #  inside splitWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.split("", "separatorChars1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${split2WhenCharacterNotIsWhitespaceStrCharAtIThrowsNullPointerException}, hash: 60D234153855D902407EBFDBA5A616BF
    @Disabled()
    @Test()
    void split2WhenCharacterNotIsWhitespaceStrCharAtIThrowsNullPointerException() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : true  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (Character.isWhitespace(str.charAt(i))) : false  #  inside splitWorker method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        final NullPointerException result = assertThrows(NullPointerException.class, () -> {
            StringUtils.split("A", (String) null);
        });
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${split2WhenMatch}, hash: EAA81E0B1095CDA5A4E5CEFB56E4ACC7
    @Test()
    void split2WhenMatch() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : false  #  inside splitWorker method
         * (separatorChars.length() == 1) : true  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (str.charAt(i) == sep) : false  #  inside splitWorker method
         * (match) : true  #  inside splitWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.split("C", "B");
        String[] stringResultArray = new String[] { "C" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${split2WhenSeparatorCharsIndexOfStrCharAtILessThan0AndMatch}, hash: 4EBA62D9880B183CE7E54E321E3A904B
    @Test()
    void split2WhenSeparatorCharsIndexOfStrCharAtILessThan0AndMatch() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : false  #  inside splitWorker method
         * (separatorChars.length() == 1) : false  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (separatorChars.indexOf(str.charAt(i)) >= 0) : false  #  inside splitWorker method
         * (match) : true  #  inside splitWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.split("A", "CB");
        String[] stringResultArray = new String[] { "A" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${split2WhenNotPreserveAllTokens}, hash: 4B31BB08482BC8281ADFEAC0F582E046
    @Disabled()
    @Test()
    void split2WhenNotPreserveAllTokens() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : true  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (Character.isWhitespace(str.charAt(i))) : true  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : false  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : false  #  inside splitWorker method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String[] result = StringUtils.split("str1", (String) null);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${split2WhenNotMatchAndNotPreserveAllTokens}, hash: 2B5B6CEC25D469767E651BBBCBCE93AA
    @Test()
    void split2WhenNotMatchAndNotPreserveAllTokens() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : false  #  inside splitWorker method
         * (separatorChars.length() == 1) : true  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (str.charAt(i) == sep) : true  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : false  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : false  #  inside splitWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.split("B", "B");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${split2WhenNotPreserveAllTokensAndNotMatchAndNotPreserveAllTokens}, hash: 957ADB4DC05567A950EFE70D6A306272
    @Test()
    void split2WhenNotPreserveAllTokensAndNotMatchAndNotPreserveAllTokens() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : false  #  inside splitWorker method
         * (separatorChars.length() == 1) : false  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (separatorChars.indexOf(str.charAt(i)) >= 0) : true  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : false  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : false  #  inside splitWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.split("A", "BA");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${split3WhenStrIsNull}, hash: 0C6B5EE1FB788D513E9B8E9952C8B32C
    @Test()
    void split3WhenStrIsNull() {
        /* Branches:
         * (str == null) : true  #  inside splitWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.split((String) null, "separatorChars1", 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${split3WhenLenEquals0}, hash: B0CE40CE4C46C4F99EFC95F8FD563E3D
    @Test()
    void split3WhenLenEquals0() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : true  #  inside splitWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.split("", "separatorChars1", 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${split3WhenCharacterNotIsWhitespaceStrCharAtIThrowsNullPointerException}, hash: 959AF6FC57E87D1D81CE9E86B9D6504E
    @Disabled()
    @Test()
    void split3WhenCharacterNotIsWhitespaceStrCharAtIThrowsNullPointerException() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : true  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (Character.isWhitespace(str.charAt(i))) : false  #  inside splitWorker method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        final NullPointerException result = assertThrows(NullPointerException.class, () -> {
            StringUtils.split("A", (String) null, 0);
        });
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${split3WhenMatch}, hash: 1D2D2F4D16B572AFA1F5CA17DFA724FF
    @Test()
    void split3WhenMatch() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : false  #  inside splitWorker method
         * (separatorChars.length() == 1) : true  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (str.charAt(i) == sep) : false  #  inside splitWorker method
         * (match) : true  #  inside splitWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.split("C", "B", 0);
        String[] stringResultArray = new String[] { "C" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${split3WhenSeparatorCharsIndexOfStrCharAtILessThan0AndMatch}, hash: F66F3E7220BE665385FEBCCD478D77D7
    @Test()
    void split3WhenSeparatorCharsIndexOfStrCharAtILessThan0AndMatch() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : false  #  inside splitWorker method
         * (separatorChars.length() == 1) : false  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (separatorChars.indexOf(str.charAt(i)) >= 0) : false  #  inside splitWorker method
         * (match) : true  #  inside splitWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.split("A", "CB", 0);
        String[] stringResultArray = new String[] { "A" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${split3WhenNotPreserveAllTokens}, hash: EB0B21727EF19F29AB953BF3C3A0607E
    @Disabled()
    @Test()
    void split3WhenNotPreserveAllTokens() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : true  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (Character.isWhitespace(str.charAt(i))) : true  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : false  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : false  #  inside splitWorker method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String[] result = StringUtils.split("str1", (String) null, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${split3WhenNotMatchAndNotPreserveAllTokens}, hash: 0FEF69682B58519A8F655DB65646B9C1
    @Test()
    void split3WhenNotMatchAndNotPreserveAllTokens() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : false  #  inside splitWorker method
         * (separatorChars.length() == 1) : true  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (str.charAt(i) == sep) : true  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : false  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : false  #  inside splitWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.split("B", "B", 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${split3WhenNotPreserveAllTokensAndNotMatchAndNotPreserveAllTokens}, hash: ABC042CBDF2F1F9D08BA39DD6946E0F3
    @Test()
    void split3WhenNotPreserveAllTokensAndNotMatchAndNotPreserveAllTokens() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : false  #  inside splitWorker method
         * (separatorChars.length() == 1) : false  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (separatorChars.indexOf(str.charAt(i)) >= 0) : true  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : false  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : false  #  inside splitWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.split("A", "BA", 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${splitByCharacterTypeWhenStrIsNull}, hash: FF694313A4F3D6C5D077ABA6720FE0F2
    @Test()
    void splitByCharacterTypeWhenStrIsNull() {
        /* Branches:
         * (str == null) : true  #  inside splitByCharacterType method
         */
        //Act Statement(s)
        String[] result = StringUtils.splitByCharacterType((String) null);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${splitByCharacterTypeWhenStrIsEmpty}, hash: 0A00E03F2FAA4A6683846581E7D0EDE8
    @Test()
    void splitByCharacterTypeWhenStrIsEmpty() {
        /* Branches:
         * (str == null) : false  #  inside splitByCharacterType method
         * (str.isEmpty()) : true  #  inside splitByCharacterType method
         */
        //Act Statement(s)
        String[] result = StringUtils.splitByCharacterType("");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${splitByCharacterTypeWhenTypeEqualsCurrentType}, hash: 83843298F33A747D8BEE2BBF19514EA3
    @Disabled()
    @Test()
    void splitByCharacterTypeWhenTypeEqualsCurrentType() {
        /* Branches:
         * (str == null) : false  #  inside splitByCharacterType method
         * (str.isEmpty()) : false  #  inside splitByCharacterType method
         * (pos < c.length) : true  #  inside splitByCharacterType method
         * (type == currentType) : true  #  inside splitByCharacterType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String[] result = StringUtils.splitByCharacterType("str1");
        String[] stringResultArray = new String[] { "resultItem1" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${splitByCharacterTypeWhenNotCamelCase}, hash: 7E7474661DE722B6ACE6CAE35AD73ED0
    @Disabled()
    @Test()
    void splitByCharacterTypeWhenNotCamelCase() {
        /* Branches:
         * (str == null) : false  #  inside splitByCharacterType method
         * (str.isEmpty()) : false  #  inside splitByCharacterType method
         * (pos < c.length) : true  #  inside splitByCharacterType method
         * (type == currentType) : false  #  inside splitByCharacterType method
         * (camelCase) : false  #  inside splitByCharacterType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String[] result = StringUtils.splitByCharacterType("str1");
        String[] stringResultArray = new String[] { "resultItem1", "resultItem1" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${splitByCharacterTypeCamelCaseWhenStrIsNull}, hash: 85C992E489B624E972A6C2C86001FF75
    @Test()
    void splitByCharacterTypeCamelCaseWhenStrIsNull() {
        /* Branches:
         * (str == null) : true  #  inside splitByCharacterType method
         */
        //Act Statement(s)
        String[] result = StringUtils.splitByCharacterTypeCamelCase((String) null);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${splitByCharacterTypeCamelCaseWhenStrIsEmpty}, hash: 17583E13A5F141C88BAF52E2E2AFA773
    @Test()
    void splitByCharacterTypeCamelCaseWhenStrIsEmpty() {
        /* Branches:
         * (str == null) : false  #  inside splitByCharacterType method
         * (str.isEmpty()) : true  #  inside splitByCharacterType method
         */
        //Act Statement(s)
        String[] result = StringUtils.splitByCharacterTypeCamelCase("");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${splitByCharacterTypeCamelCaseWhenTypeEqualsCurrentType}, hash: FF21288211C943117CDA433D801E9A67
    @Disabled()
    @Test()
    void splitByCharacterTypeCamelCaseWhenTypeEqualsCurrentType() {
        /* Branches:
         * (str == null) : false  #  inside splitByCharacterType method
         * (str.isEmpty()) : false  #  inside splitByCharacterType method
         * (pos < c.length) : true  #  inside splitByCharacterType method
         * (type == currentType) : true  #  inside splitByCharacterType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String[] result = StringUtils.splitByCharacterTypeCamelCase("str1");
        String[] stringResultArray = new String[] { "resultItem1" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${splitByCharacterTypeCamelCaseWhenCurrentTypeNotEqualsCharacterUPPERCASE_LETTER}, hash: D772D33C6A6918764017311DF2B788AF
    @Disabled()
    @Test()
    void splitByCharacterTypeCamelCaseWhenCurrentTypeNotEqualsCharacterUPPERCASE_LETTER() {
        /* Branches:
         * (str == null) : false  #  inside splitByCharacterType method
         * (str.isEmpty()) : false  #  inside splitByCharacterType method
         * (pos < c.length) : true  #  inside splitByCharacterType method
         * (type == currentType) : false  #  inside splitByCharacterType method
         * (camelCase) : true  #  inside splitByCharacterType method
         * (type == Character.LOWERCASE_LETTER) : true  #  inside splitByCharacterType method
         * (currentType == Character.UPPERCASE_LETTER) : false  #  inside splitByCharacterType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String[] result = StringUtils.splitByCharacterTypeCamelCase("str1");
        String[] stringResultArray = new String[] { "resultItem1", "resultItem1" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${splitByWholeSeparatorWhenStrIsNull}, hash: B22AFB59CC92C78815160C34F2EC18A4
    @Test()
    void splitByWholeSeparatorWhenStrIsNull() {
        /* Branches:
         * (str == null) : true  #  inside splitByWholeSeparatorWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.splitByWholeSeparator((String) null, "separator1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${splitByWholeSeparatorWhenLenEquals0}, hash: 47904B6E1882CCEC5566737242348425
    @Test()
    void splitByWholeSeparatorWhenLenEquals0() {
        /* Branches:
         * (str == null) : false  #  inside splitByWholeSeparatorWorker method
         * (len == 0) : true  #  inside splitByWholeSeparatorWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.splitByWholeSeparator("", "separator1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${splitByWholeSeparatorWhenStrIsNotNullAndLenEquals0}, hash: 1F92C7C6872D69F3274D1EA452D7AA5C
    @Disabled()
    @Test()
    void splitByWholeSeparatorWhenStrIsNotNullAndLenEquals0() {
        /* Branches:
         * (str == null) : false  #  inside splitByWholeSeparatorWorker method
         * (len == 0) : false  #  inside splitByWholeSeparatorWorker method
         * (separator == null) : false  #  inside splitByWholeSeparatorWorker method
         * (EMPTY.equals(separator)) : true  #  inside splitByWholeSeparatorWorker method
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : true  #  inside splitWorker method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String[] result = StringUtils.splitByWholeSeparator("str1", "separator1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${splitByWholeSeparatorWhenILessThanLenAndCharacterNotIsWhitespaceStrCharAtIThrowsNullPointerException}, hash: C03D8E80124F4E56F1025DB1DD5CD616
    @Disabled()
    @Test()
    void splitByWholeSeparatorWhenILessThanLenAndCharacterNotIsWhitespaceStrCharAtIThrowsNullPointerException() {
        /* Branches:
         * (str == null) : false  #  inside splitByWholeSeparatorWorker method
         * (len == 0) : false  #  inside splitByWholeSeparatorWorker method
         * (separator == null) : false  #  inside splitByWholeSeparatorWorker method
         * (EMPTY.equals(separator)) : true  #  inside splitByWholeSeparatorWorker method
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : true  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (Character.isWhitespace(str.charAt(i))) : false  #  inside splitWorker method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        final NullPointerException result = assertThrows(NullPointerException.class, () -> {
            StringUtils.splitByWholeSeparator("BC", "");
        });
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${splitByWholeSeparatorWhenNumberOfSubstringsNotEqualsMaxAndNotPreserveAllTokens}, hash: 8B6F38A32BD20D2DB30A2339A9C02320
    @Test()
    void splitByWholeSeparatorWhenNumberOfSubstringsNotEqualsMaxAndNotPreserveAllTokens() {
        /* Branches:
         * (str == null) : false  #  inside splitByWholeSeparatorWorker method
         * (len == 0) : false  #  inside splitByWholeSeparatorWorker method
         * (separator == null) : false  #  inside splitByWholeSeparatorWorker method
         * (EMPTY.equals(separator)) : false  #  inside splitByWholeSeparatorWorker method
         * (end < len) : true  #  inside splitByWholeSeparatorWorker method
         * (end > -1) : true  #  inside splitByWholeSeparatorWorker method
         * (end > beg) : true  #  inside splitByWholeSeparatorWorker method
         * (numberOfSubstrings == max) : false  #  inside splitByWholeSeparatorWorker method
         * (preserveAllTokens) : false  #  inside splitByWholeSeparatorWorker method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String[] result = StringUtils.splitByWholeSeparator("CBHI", "B");
        String[] stringResultArray = new String[] { "C", "HI" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${splitByWholeSeparatorWhenNotMatchAndNotPreserveAllTokens}, hash: 7DD527C0CCD8C36D794E933619AD6293
    @Disabled()
    @Test()
    void splitByWholeSeparatorWhenNotMatchAndNotPreserveAllTokens() {
        /* Branches:
         * (str == null) : false  #  inside splitByWholeSeparatorWorker method
         * (len == 0) : false  #  inside splitByWholeSeparatorWorker method
         * (separator == null) : false  #  inside splitByWholeSeparatorWorker method
         * (EMPTY.equals(separator)) : true  #  inside splitByWholeSeparatorWorker method
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : true  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (Character.isWhitespace(str.charAt(i))) : true  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : false  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : false  #  inside splitWorker method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String[] result = StringUtils.splitByWholeSeparator("str1", "separator1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${splitByWholeSeparator1WhenStrIsNull}, hash: ACE1A44963D306BAABAEDF9D46644267
    @Test()
    void splitByWholeSeparator1WhenStrIsNull() {
        /* Branches:
         * (str == null) : true  #  inside splitByWholeSeparatorWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.splitByWholeSeparator((String) null, "separator1", 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${splitByWholeSeparator1WhenLenEquals0}, hash: 43FE519ABD0094BC9636A6B895F3D65E
    @Test()
    void splitByWholeSeparator1WhenLenEquals0() {
        /* Branches:
         * (str == null) : false  #  inside splitByWholeSeparatorWorker method
         * (len == 0) : true  #  inside splitByWholeSeparatorWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.splitByWholeSeparator("", "separator1", 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${splitByWholeSeparator1WhenStrIsNotNullAndLenEquals0}, hash: D69CBC6EF5221D680E27A8E10011D472
    @Disabled()
    @Test()
    void splitByWholeSeparator1WhenStrIsNotNullAndLenEquals0() {
        /* Branches:
         * (str == null) : false  #  inside splitByWholeSeparatorWorker method
         * (len == 0) : false  #  inside splitByWholeSeparatorWorker method
         * (separator == null) : false  #  inside splitByWholeSeparatorWorker method
         * (EMPTY.equals(separator)) : true  #  inside splitByWholeSeparatorWorker method
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : true  #  inside splitWorker method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String[] result = StringUtils.splitByWholeSeparator("str1", "separator1", 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${splitByWholeSeparator1WhenILessThanLenAndCharacterNotIsWhitespaceStrCharAtIThrowsNullPointerException}, hash: 8ED0BB654F59069983827D5E75D5F738
    @Disabled()
    @Test()
    void splitByWholeSeparator1WhenILessThanLenAndCharacterNotIsWhitespaceStrCharAtIThrowsNullPointerException() {
        /* Branches:
         * (str == null) : false  #  inside splitByWholeSeparatorWorker method
         * (len == 0) : false  #  inside splitByWholeSeparatorWorker method
         * (separator == null) : false  #  inside splitByWholeSeparatorWorker method
         * (EMPTY.equals(separator)) : true  #  inside splitByWholeSeparatorWorker method
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : true  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (Character.isWhitespace(str.charAt(i))) : false  #  inside splitWorker method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        final NullPointerException result = assertThrows(NullPointerException.class, () -> {
            StringUtils.splitByWholeSeparator("BC", "", 0);
        });
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${splitByWholeSeparator1WhenNumberOfSubstringsEqualsMaxAndNotPreserveAllTokens}, hash: 56B5832BF7F6BADDB1DC1FE4517A7BA1
    @Disabled()
    @Test()
    void splitByWholeSeparator1WhenNumberOfSubstringsEqualsMaxAndNotPreserveAllTokens() {
        /* Branches:
         * (str == null) : false  #  inside splitByWholeSeparatorWorker method
         * (len == 0) : false  #  inside splitByWholeSeparatorWorker method
         * (separator == null) : false  #  inside splitByWholeSeparatorWorker method
         * (EMPTY.equals(separator)) : false  #  inside splitByWholeSeparatorWorker method
         * (end < len) : true  #  inside splitByWholeSeparatorWorker method
         * (end > -1) : true  #  inside splitByWholeSeparatorWorker method
         * (end > beg) : true  #  inside splitByWholeSeparatorWorker method
         * (numberOfSubstrings == max) : true  #  inside splitByWholeSeparatorWorker method
         * (preserveAllTokens) : false  #  inside splitByWholeSeparatorWorker method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String[] result = StringUtils.splitByWholeSeparator("str1", "separator1", 0);
        String[] stringResultArray = new String[] { "resultItem1", "resultItem1", "resultItem1" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${splitByWholeSeparator1WhenNotMatchAndNotPreserveAllTokens}, hash: FE6F58B059E4BD0A01FDE415A46D7735
    @Disabled()
    @Test()
    void splitByWholeSeparator1WhenNotMatchAndNotPreserveAllTokens() {
        /* Branches:
         * (str == null) : false  #  inside splitByWholeSeparatorWorker method
         * (len == 0) : false  #  inside splitByWholeSeparatorWorker method
         * (separator == null) : false  #  inside splitByWholeSeparatorWorker method
         * (EMPTY.equals(separator)) : true  #  inside splitByWholeSeparatorWorker method
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : true  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (Character.isWhitespace(str.charAt(i))) : true  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : false  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : false  #  inside splitWorker method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String[] result = StringUtils.splitByWholeSeparator("str1", "separator1", 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${splitByWholeSeparatorPreserveAllTokensWhenStrIsNull}, hash: 166BCA01FEBE502E2EE0F4C6275E916B
    @Test()
    void splitByWholeSeparatorPreserveAllTokensWhenStrIsNull() {
        /* Branches:
         * (str == null) : true  #  inside splitByWholeSeparatorWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.splitByWholeSeparatorPreserveAllTokens((String) null, "separator1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${splitByWholeSeparatorPreserveAllTokensWhenLenEquals0}, hash: 8C75F7744122DCEC8FB6A1441289DDD3
    @Test()
    void splitByWholeSeparatorPreserveAllTokensWhenLenEquals0() {
        /* Branches:
         * (str == null) : false  #  inside splitByWholeSeparatorWorker method
         * (len == 0) : true  #  inside splitByWholeSeparatorWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.splitByWholeSeparatorPreserveAllTokens("", "separator1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${splitByWholeSeparatorPreserveAllTokensWhenStrIsNotNullAndLenEquals0}, hash: 41BF2D88452EE63AC6E18A1F1E661D6F
    @Disabled()
    @Test()
    void splitByWholeSeparatorPreserveAllTokensWhenStrIsNotNullAndLenEquals0() {
        /* Branches:
         * (str == null) : false  #  inside splitByWholeSeparatorWorker method
         * (len == 0) : false  #  inside splitByWholeSeparatorWorker method
         * (separator == null) : false  #  inside splitByWholeSeparatorWorker method
         * (EMPTY.equals(separator)) : true  #  inside splitByWholeSeparatorWorker method
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : true  #  inside splitWorker method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String[] result = StringUtils.splitByWholeSeparatorPreserveAllTokens("str1", "separator1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${splitByWholeSeparatorPreserveAllTokensWhenILessThanLenAndCharacterNotIsWhitespaceStrCharAtIThrowsNullPointerException}, hash: 6198D6FF947127E47BE7EB59642041AC
    @Disabled()
    @Test()
    void splitByWholeSeparatorPreserveAllTokensWhenILessThanLenAndCharacterNotIsWhitespaceStrCharAtIThrowsNullPointerException() {
        /* Branches:
         * (str == null) : false  #  inside splitByWholeSeparatorWorker method
         * (len == 0) : false  #  inside splitByWholeSeparatorWorker method
         * (separator == null) : false  #  inside splitByWholeSeparatorWorker method
         * (EMPTY.equals(separator)) : true  #  inside splitByWholeSeparatorWorker method
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : true  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (Character.isWhitespace(str.charAt(i))) : false  #  inside splitWorker method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        final NullPointerException result = assertThrows(NullPointerException.class, () -> {
            StringUtils.splitByWholeSeparatorPreserveAllTokens("BC", "");
        });
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${splitByWholeSeparatorPreserveAllTokensWhenPreserveAllTokensAndNumberOfSubstringsNotEqualsMax}, hash: 19E4F8036ABDD9958D04B2A344FABF81
    @Disabled()
    @Test()
    void splitByWholeSeparatorPreserveAllTokensWhenPreserveAllTokensAndNumberOfSubstringsNotEqualsMax() {
        /* Branches:
         * (str == null) : false  #  inside splitByWholeSeparatorWorker method
         * (len == 0) : false  #  inside splitByWholeSeparatorWorker method
         * (separator == null) : false  #  inside splitByWholeSeparatorWorker method
         * (EMPTY.equals(separator)) : false  #  inside splitByWholeSeparatorWorker method
         * (end < len) : true  #  inside splitByWholeSeparatorWorker method
         * (end > -1) : true  #  inside splitByWholeSeparatorWorker method
         * (end > beg) : true  #  inside splitByWholeSeparatorWorker method
         * (numberOfSubstrings == max) : false  #  inside splitByWholeSeparatorWorker method
         * (preserveAllTokens) : true  #  inside splitByWholeSeparatorWorker method
         * (numberOfSubstrings == max) : false  #  inside splitByWholeSeparatorWorker method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String[] result = StringUtils.splitByWholeSeparatorPreserveAllTokens("CBHI", "B");
        String[] stringResultArray = new String[] { "C", "", "HI" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${splitByWholeSeparatorPreserveAllTokensWhenLastMatch}, hash: 3D22BBB9C17514D39EF6200A8914C9A4
    @Disabled()
    @Test()
    void splitByWholeSeparatorPreserveAllTokensWhenLastMatch() {
        /* Branches:
         * (str == null) : false  #  inside splitByWholeSeparatorWorker method
         * (len == 0) : false  #  inside splitByWholeSeparatorWorker method
         * (separator == null) : false  #  inside splitByWholeSeparatorWorker method
         * (EMPTY.equals(separator)) : true  #  inside splitByWholeSeparatorWorker method
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : true  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (Character.isWhitespace(str.charAt(i))) : true  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : true  #  inside splitWorker method
         * (sizePlus1++ == max) : false  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : true  #  inside splitWorker method
         * (lastMatch) : true  #  inside splitWorker method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String[] result = StringUtils.splitByWholeSeparatorPreserveAllTokens("str1", "separator1");
        String[] stringResultArray = new String[] { "resultItem1", "resultItem1" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${splitByWholeSeparatorPreserveAllTokens1WhenStrIsNull}, hash: E4140E14FF42452F50DDD350D478853A
    @Test()
    void splitByWholeSeparatorPreserveAllTokens1WhenStrIsNull() {
        /* Branches:
         * (str == null) : true  #  inside splitByWholeSeparatorWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.splitByWholeSeparatorPreserveAllTokens((String) null, "separator1", 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${splitByWholeSeparatorPreserveAllTokens1WhenLenEquals0}, hash: 8DD9B094B8AA2051811E2870BD7FCE69
    @Test()
    void splitByWholeSeparatorPreserveAllTokens1WhenLenEquals0() {
        /* Branches:
         * (str == null) : false  #  inside splitByWholeSeparatorWorker method
         * (len == 0) : true  #  inside splitByWholeSeparatorWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.splitByWholeSeparatorPreserveAllTokens("", "separator1", 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${splitByWholeSeparatorPreserveAllTokens1WhenStrIsNotNullAndLenEquals0}, hash: 6237A7ED4EAFB76462E4EAB7480918F7
    @Disabled()
    @Test()
    void splitByWholeSeparatorPreserveAllTokens1WhenStrIsNotNullAndLenEquals0() {
        /* Branches:
         * (str == null) : false  #  inside splitByWholeSeparatorWorker method
         * (len == 0) : false  #  inside splitByWholeSeparatorWorker method
         * (separator == null) : false  #  inside splitByWholeSeparatorWorker method
         * (EMPTY.equals(separator)) : true  #  inside splitByWholeSeparatorWorker method
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : true  #  inside splitWorker method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String[] result = StringUtils.splitByWholeSeparatorPreserveAllTokens("str1", "separator1", 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${splitByWholeSeparatorPreserveAllTokens1WhenILessThanLenAndCharacterNotIsWhitespaceStrCharAtIThrowsNullPointerException}, hash: 1CB5EB35A2E02AE706738C555F1F0639
    @Disabled()
    @Test()
    void splitByWholeSeparatorPreserveAllTokens1WhenILessThanLenAndCharacterNotIsWhitespaceStrCharAtIThrowsNullPointerException() {
        /* Branches:
         * (str == null) : false  #  inside splitByWholeSeparatorWorker method
         * (len == 0) : false  #  inside splitByWholeSeparatorWorker method
         * (separator == null) : false  #  inside splitByWholeSeparatorWorker method
         * (EMPTY.equals(separator)) : true  #  inside splitByWholeSeparatorWorker method
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : true  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (Character.isWhitespace(str.charAt(i))) : false  #  inside splitWorker method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        final NullPointerException result = assertThrows(NullPointerException.class, () -> {
            StringUtils.splitByWholeSeparatorPreserveAllTokens("BC", "", 0);
        });
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${splitByWholeSeparatorPreserveAllTokens1WhenPreserveAllTokensAndNumberOfSubstringsNotEqualsMax}, hash: CBFFFD01EA3108543DB4C20EB8A53349
    @Disabled()
    @Test()
    void splitByWholeSeparatorPreserveAllTokens1WhenPreserveAllTokensAndNumberOfSubstringsNotEqualsMax() {
        /* Branches:
         * (str == null) : false  #  inside splitByWholeSeparatorWorker method
         * (len == 0) : false  #  inside splitByWholeSeparatorWorker method
         * (separator == null) : false  #  inside splitByWholeSeparatorWorker method
         * (EMPTY.equals(separator)) : false  #  inside splitByWholeSeparatorWorker method
         * (end < len) : true  #  inside splitByWholeSeparatorWorker method
         * (end > -1) : true  #  inside splitByWholeSeparatorWorker method
         * (end > beg) : true  #  inside splitByWholeSeparatorWorker method
         * (numberOfSubstrings == max) : true  #  inside splitByWholeSeparatorWorker method
         * (preserveAllTokens) : true  #  inside splitByWholeSeparatorWorker method
         * (numberOfSubstrings == max) : false  #  inside splitByWholeSeparatorWorker method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String[] result = StringUtils.splitByWholeSeparatorPreserveAllTokens("str1", "separator1", 0);
        String[] stringResultArray = new String[] { "resultItem1", "resultItem1", "", "resultItem1" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${splitByWholeSeparatorPreserveAllTokens1WhenPreserveAllTokensAndNumberOfSubstringsEqualsMax}, hash: 97ECFE442044566A98D21F63BDDC3657
    @Disabled()
    @Test()
    void splitByWholeSeparatorPreserveAllTokens1WhenPreserveAllTokensAndNumberOfSubstringsEqualsMax() {
        /* Branches:
         * (str == null) : false  #  inside splitByWholeSeparatorWorker method
         * (len == 0) : false  #  inside splitByWholeSeparatorWorker method
         * (separator == null) : false  #  inside splitByWholeSeparatorWorker method
         * (EMPTY.equals(separator)) : false  #  inside splitByWholeSeparatorWorker method
         * (end < len) : true  #  inside splitByWholeSeparatorWorker method
         * (end > -1) : true  #  inside splitByWholeSeparatorWorker method
         * (end > beg) : true  #  inside splitByWholeSeparatorWorker method
         * (numberOfSubstrings == max) : false  #  inside splitByWholeSeparatorWorker method
         * (preserveAllTokens) : true  #  inside splitByWholeSeparatorWorker method
         * (numberOfSubstrings == max) : true  #  inside splitByWholeSeparatorWorker method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String[] result = StringUtils.splitByWholeSeparatorPreserveAllTokens("str1", "separator1", 0);
        String[] stringResultArray = new String[] { "resultItem1", "resultItem1", "resultItem1" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${splitByWholeSeparatorPreserveAllTokens1WhenNotLastMatch}, hash: 3A97108301578424F491304B91A77081
    @Disabled()
    @Test()
    void splitByWholeSeparatorPreserveAllTokens1WhenNotLastMatch() {
        /* Branches:
         * (str == null) : false  #  inside splitByWholeSeparatorWorker method
         * (len == 0) : false  #  inside splitByWholeSeparatorWorker method
         * (separator == null) : false  #  inside splitByWholeSeparatorWorker method
         * (EMPTY.equals(separator)) : true  #  inside splitByWholeSeparatorWorker method
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : true  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (Character.isWhitespace(str.charAt(i))) : true  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : true  #  inside splitWorker method
         * (sizePlus1++ == max) : true  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : true  #  inside splitWorker method
         * (lastMatch) : false  #  inside splitWorker method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String[] result = StringUtils.splitByWholeSeparatorPreserveAllTokens("str1", "separator1", 0);
        String[] stringResultArray = new String[] { "resultItem1" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${splitByWholeSeparatorPreserveAllTokens1WhenLastMatch}, hash: ABFA1F013210BD4F741EFD66C265CAE5
    @Disabled()
    @Test()
    void splitByWholeSeparatorPreserveAllTokens1WhenLastMatch() {
        /* Branches:
         * (str == null) : false  #  inside splitByWholeSeparatorWorker method
         * (len == 0) : false  #  inside splitByWholeSeparatorWorker method
         * (separator == null) : false  #  inside splitByWholeSeparatorWorker method
         * (EMPTY.equals(separator)) : true  #  inside splitByWholeSeparatorWorker method
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : true  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (Character.isWhitespace(str.charAt(i))) : true  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : true  #  inside splitWorker method
         * (sizePlus1++ == max) : false  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : true  #  inside splitWorker method
         * (lastMatch) : true  #  inside splitWorker method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String[] result = StringUtils.splitByWholeSeparatorPreserveAllTokens("str1", "separator1", 0);
        String[] stringResultArray = new String[] { "resultItem1", "resultItem1" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${splitPreserveAllTokensWhenStrIsNull}, hash: 7D02810EDB8A236EF7BD38CCEC3DDBA8
    @Test()
    void splitPreserveAllTokensWhenStrIsNull() {
        /* Branches:
         * (str == null) : true  #  inside splitWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.splitPreserveAllTokens((String) null);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${splitPreserveAllTokensWhenLenEquals0}, hash: 2AD76EB4A2EC6B3EE0876039555C2CF8
    @Test()
    void splitPreserveAllTokensWhenLenEquals0() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : true  #  inside splitWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.splitPreserveAllTokens("");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${splitPreserveAllTokensWhenCharacterNotIsWhitespaceStrCharAtIThrowsNullPointerException}, hash: D055EA5217C4E68F9F2259DAD0A9A55F
    @Disabled()
    @Test()
    void splitPreserveAllTokensWhenCharacterNotIsWhitespaceStrCharAtIThrowsNullPointerException() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : true  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (Character.isWhitespace(str.charAt(i))) : false  #  inside splitWorker method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        final NullPointerException result = assertThrows(NullPointerException.class, () -> {
            StringUtils.splitPreserveAllTokens("A");
        });
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${splitPreserveAllTokensWhenLastMatch}, hash: 360703C7419A8189F93E670EC9FD86A6
    @Disabled()
    @Test()
    void splitPreserveAllTokensWhenLastMatch() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : true  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (Character.isWhitespace(str.charAt(i))) : true  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : true  #  inside splitWorker method
         * (sizePlus1++ == max) : false  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : true  #  inside splitWorker method
         * (lastMatch) : true  #  inside splitWorker method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String[] result = StringUtils.splitPreserveAllTokens("str1");
        String[] stringResultArray = new String[] { "resultItem1", "resultItem1" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${splitPreserveAllTokens1WhenStrIsNull}, hash: A66532E584E355492985C8AC0631E63D
    @Test()
    void splitPreserveAllTokens1WhenStrIsNull() {
        /* Branches:
         * (str == null) : true  #  inside splitWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.splitPreserveAllTokens((String) null, 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${splitPreserveAllTokens1WhenLenEquals0}, hash: 19D47431E0AFF2B05C63E5AD16D8091E
    @Test()
    void splitPreserveAllTokens1WhenLenEquals0() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : true  #  inside splitWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.splitPreserveAllTokens("", 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${splitPreserveAllTokens1WhenMatch}, hash: A118DD73039256CCF5B925DDDF8670DB
    @Test()
    void splitPreserveAllTokens1WhenMatch() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (str.charAt(i) == separatorChar) : false  #  inside splitWorker method
         * (match) : true  #  inside splitWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.splitPreserveAllTokens("A", 'B');
        String[] stringResultArray = new String[] { "A" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${splitPreserveAllTokens1WhenLastMatch}, hash: 2ED51AD5F2315FAACF8A1B01DEB37D4B
    @Disabled()
    @Test()
    void splitPreserveAllTokens1WhenLastMatch() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (str.charAt(i) == separatorChar) : true  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : true  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : true  #  inside splitWorker method
         * (lastMatch) : true  #  inside splitWorker method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String[] result = StringUtils.splitPreserveAllTokens("AB", 'A');
        String[] stringResultArray = new String[] { "", "" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${splitPreserveAllTokens2WhenStrIsNull}, hash: DB63309C835FDCB58C39E29C61D19F92
    @Test()
    void splitPreserveAllTokens2WhenStrIsNull() {
        /* Branches:
         * (str == null) : true  #  inside splitWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.splitPreserveAllTokens((String) null, "separatorChars1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${splitPreserveAllTokens2WhenLenEquals0}, hash: B196281ECECABEC4FD78B6A15CD04541
    @Test()
    void splitPreserveAllTokens2WhenLenEquals0() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : true  #  inside splitWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.splitPreserveAllTokens("", "separatorChars1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${splitPreserveAllTokens2WhenCharacterNotIsWhitespaceStrCharAtIThrowsNullPointerException}, hash: 653482F8E6BA783EAF6EC2D97B19837E
    @Disabled()
    @Test()
    void splitPreserveAllTokens2WhenCharacterNotIsWhitespaceStrCharAtIThrowsNullPointerException() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : true  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (Character.isWhitespace(str.charAt(i))) : false  #  inside splitWorker method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        final NullPointerException result = assertThrows(NullPointerException.class, () -> {
            StringUtils.splitPreserveAllTokens("A", (String) null);
        });
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${splitPreserveAllTokens2WhenMatch}, hash: AA3C8E857505EEA5BCF82D9C91495E21
    @Test()
    void splitPreserveAllTokens2WhenMatch() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : false  #  inside splitWorker method
         * (separatorChars.length() == 1) : true  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (str.charAt(i) == sep) : false  #  inside splitWorker method
         * (match) : true  #  inside splitWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.splitPreserveAllTokens("C", "B");
        String[] stringResultArray = new String[] { "C" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${splitPreserveAllTokens2WhenSeparatorCharsIndexOfStrCharAtILessThan0AndMatch}, hash: CA593F0AC4842D70792AEA667D44E1CB
    @Test()
    void splitPreserveAllTokens2WhenSeparatorCharsIndexOfStrCharAtILessThan0AndMatch() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : false  #  inside splitWorker method
         * (separatorChars.length() == 1) : false  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (separatorChars.indexOf(str.charAt(i)) >= 0) : false  #  inside splitWorker method
         * (match) : true  #  inside splitWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.splitPreserveAllTokens("A", "CB");
        String[] stringResultArray = new String[] { "A" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${splitPreserveAllTokens2WhenLastMatch}, hash: 5100692A8C29AFF36C44966AA7A6366B
    @Disabled()
    @Test()
    void splitPreserveAllTokens2WhenLastMatch() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : true  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (Character.isWhitespace(str.charAt(i))) : true  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : true  #  inside splitWorker method
         * (sizePlus1++ == max) : false  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : true  #  inside splitWorker method
         * (lastMatch) : true  #  inside splitWorker method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String[] result = StringUtils.splitPreserveAllTokens("str1", (String) null);
        String[] stringResultArray = new String[] { "resultItem1", "resultItem1" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${splitPreserveAllTokens2WhenPreserveAllTokensAndLastMatch}, hash: 35E9BB01395493C3750E5F064C898BFD
    @Disabled()
    @Test()
    void splitPreserveAllTokens2WhenPreserveAllTokensAndLastMatch() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : false  #  inside splitWorker method
         * (separatorChars.length() == 1) : true  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (str.charAt(i) == sep) : true  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : true  #  inside splitWorker method
         * (sizePlus1++ == max) : false  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : true  #  inside splitWorker method
         * (lastMatch) : true  #  inside splitWorker method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String[] result = StringUtils.splitPreserveAllTokens("BC", "B");
        String[] stringResultArray = new String[] { "", "" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${splitPreserveAllTokens2WhenNotMatchAndPreserveAllTokensAndLastMatch}, hash: 773ED36B7B9EA6E75E8F935CC5AD564D
    @Disabled()
    @Test()
    void splitPreserveAllTokens2WhenNotMatchAndPreserveAllTokensAndLastMatch() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : false  #  inside splitWorker method
         * (separatorChars.length() == 1) : false  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (separatorChars.indexOf(str.charAt(i)) >= 0) : true  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : true  #  inside splitWorker method
         * (sizePlus1++ == max) : false  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : true  #  inside splitWorker method
         * (lastMatch) : true  #  inside splitWorker method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String[] result = StringUtils.splitPreserveAllTokens("AC", "BA");
        String[] stringResultArray = new String[] { "", "" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${splitPreserveAllTokens3WhenStrIsNull}, hash: 6F55238B748AE3F8042D6FB741231A6D
    @Test()
    void splitPreserveAllTokens3WhenStrIsNull() {
        /* Branches:
         * (str == null) : true  #  inside splitWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.splitPreserveAllTokens((String) null, "separatorChars1", 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${splitPreserveAllTokens3WhenLenEquals0}, hash: 2A125019DB68A9CE200DE7BCE9285E5F
    @Test()
    void splitPreserveAllTokens3WhenLenEquals0() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : true  #  inside splitWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.splitPreserveAllTokens("", "separatorChars1", 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${splitPreserveAllTokens3WhenCharacterNotIsWhitespaceStrCharAtIThrowsNullPointerException}, hash: 68D2541AA75B5816042ED780802D0E17
    @Disabled()
    @Test()
    void splitPreserveAllTokens3WhenCharacterNotIsWhitespaceStrCharAtIThrowsNullPointerException() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : true  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (Character.isWhitespace(str.charAt(i))) : false  #  inside splitWorker method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        final NullPointerException result = assertThrows(NullPointerException.class, () -> {
            StringUtils.splitPreserveAllTokens("A", (String) null, 0);
        });
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${splitPreserveAllTokens3WhenMatch}, hash: AC7DB53FE505CFCDF1CE03333A283582
    @Test()
    void splitPreserveAllTokens3WhenMatch() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : false  #  inside splitWorker method
         * (separatorChars.length() == 1) : true  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (str.charAt(i) == sep) : false  #  inside splitWorker method
         * (match) : true  #  inside splitWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.splitPreserveAllTokens("C", "B", 0);
        String[] stringResultArray = new String[] { "C" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${splitPreserveAllTokens3WhenSeparatorCharsIndexOfStrCharAtILessThan0AndMatch}, hash: 724D955DBFB95F96130F4538FB9A41F7
    @Test()
    void splitPreserveAllTokens3WhenSeparatorCharsIndexOfStrCharAtILessThan0AndMatch() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : false  #  inside splitWorker method
         * (separatorChars.length() == 1) : false  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (separatorChars.indexOf(str.charAt(i)) >= 0) : false  #  inside splitWorker method
         * (match) : true  #  inside splitWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.splitPreserveAllTokens("A", "CB", 0);
        String[] stringResultArray = new String[] { "A" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${splitPreserveAllTokens3WhenNotLastMatch}, hash: A3B03D8A57E479775FEA84C0C0D22BB5
    @Disabled()
    @Test()
    void splitPreserveAllTokens3WhenNotLastMatch() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : true  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (Character.isWhitespace(str.charAt(i))) : true  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : true  #  inside splitWorker method
         * (sizePlus1++ == max) : true  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : true  #  inside splitWorker method
         * (lastMatch) : false  #  inside splitWorker method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String[] result = StringUtils.splitPreserveAllTokens("str1", (String) null, 0);
        String[] stringResultArray = new String[] { "resultItem1" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${splitPreserveAllTokens3WhenPreserveAllTokensAndNotLastMatch}, hash: F8660D4FF6C60C0F3041AA908942E248
    @Test()
    void splitPreserveAllTokens3WhenPreserveAllTokensAndNotLastMatch() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : false  #  inside splitWorker method
         * (separatorChars.length() == 1) : true  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (str.charAt(i) == sep) : true  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : true  #  inside splitWorker method
         * (sizePlus1++ == max) : true  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : true  #  inside splitWorker method
         * (lastMatch) : false  #  inside splitWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.splitPreserveAllTokens("B", "B", 1);
        String[] stringResultArray = new String[] { "B" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${splitPreserveAllTokens3WhenPreserveAllTokensAndLastMatch}, hash: 18279B7D2B679657F40EC336E6B4D1B0
    @Disabled()
    @Test()
    void splitPreserveAllTokens3WhenPreserveAllTokensAndLastMatch() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : false  #  inside splitWorker method
         * (separatorChars.length() == 1) : true  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (str.charAt(i) == sep) : true  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : true  #  inside splitWorker method
         * (sizePlus1++ == max) : false  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : true  #  inside splitWorker method
         * (lastMatch) : true  #  inside splitWorker method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String[] result = StringUtils.splitPreserveAllTokens("BC", "B", 0);
        String[] stringResultArray = new String[] { "", "" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${splitPreserveAllTokens3WhenNotMatchAndPreserveAllTokensAndNotLastMatch}, hash: B06E3C0AC61CA3706D9AC9896263D2E4
    @Test()
    void splitPreserveAllTokens3WhenNotMatchAndPreserveAllTokensAndNotLastMatch() {
        /* Branches:
         * (str == null) : false  #  inside splitWorker method
         * (len == 0) : false  #  inside splitWorker method
         * (separatorChars == null) : false  #  inside splitWorker method
         * (separatorChars.length() == 1) : false  #  inside splitWorker method
         * (i < len) : true  #  inside splitWorker method
         * (separatorChars.indexOf(str.charAt(i)) >= 0) : true  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : true  #  inside splitWorker method
         * (sizePlus1++ == max) : true  #  inside splitWorker method
         * (match) : false  #  inside splitWorker method
         * (preserveAllTokens) : true  #  inside splitWorker method
         * (lastMatch) : false  #  inside splitWorker method
         */
        //Act Statement(s)
        String[] result = StringUtils.splitPreserveAllTokens("A", "BA", 1);
        String[] stringResultArray = new String[] { "A" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${startsWithWhenStringsCSStartsWithStrPrefix}, hash: 689E4C665CD85A834B636631A8ACDA97
    @Disabled()
    @Test()
    void startsWithWhenStringsCSStartsWithStrPrefix() {
        /* Branches:
         * (Strings.CS.startsWith(str, prefix)) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: CS - Method: startsWith
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        boolean result = StringUtils.startsWith("str1", "prefix1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${startsWithWhenStringsCSNotStartsWithStrPrefix}, hash: AD2A392BBEC3CC6AF7C4A7E164107A34
    @Test()
    void startsWithWhenStringsCSNotStartsWithStrPrefix() {
        /* Branches:
         * (Strings.CS.startsWith(str, prefix)) : false
         */
        //Act Statement(s)
        boolean result = StringUtils.startsWith("str1", "prefix1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${startsWithAnyWhenStringsCSStartsWithAnySequenceSearchStrings}, hash: 07B620441C76F89AD77A6F7F5316CE89
    @Disabled()
    @Test()
    void startsWithAnyWhenStringsCSStartsWithAnySequenceSearchStrings() {
        /* Branches:
         * (Strings.CS.startsWithAny(sequence, searchStrings)) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: CS - Method: startsWithAny
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] {};
        //Act Statement(s)
        boolean result = StringUtils.startsWithAny("sequence1", charSequenceArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${startsWithAnyWhenStringsCSNotStartsWithAnySequenceSearchStrings}, hash: DDD2757757509CE41918E178423C22B0
    @Test()
    void startsWithAnyWhenStringsCSNotStartsWithAnySequenceSearchStrings() {
        /* Branches:
         * (Strings.CS.startsWithAny(sequence, searchStrings)) : false
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: CS - Method: startsWithAny
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        CharSequence[] charSequenceArray = new CharSequence[] {};
        //Act Statement(s)
        boolean result = StringUtils.startsWithAny("sequence1", charSequenceArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${startsWithIgnoreCaseWhenStringsCIStartsWithStrPrefix}, hash: C26F4E27EAEF6C6569BC9D3A5A4EA9F6
    @Disabled()
    @Test()
    void startsWithIgnoreCaseWhenStringsCIStartsWithStrPrefix() {
        /* Branches:
         * (Strings.CI.startsWith(str, prefix)) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: CI - Method: startsWith
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        boolean result = StringUtils.startsWithIgnoreCase("str1", "prefix1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${startsWithIgnoreCaseWhenStringsCINotStartsWithStrPrefix}, hash: 27FADB4DD97C23EF85C6732582BEBB2B
    @Test()
    void startsWithIgnoreCaseWhenStringsCINotStartsWithStrPrefix() {
        /* Branches:
         * (Strings.CI.startsWith(str, prefix)) : false
         */
        //Act Statement(s)
        boolean result = StringUtils.startsWithIgnoreCase("str1", "prefix1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${stripTest}, hash: F0AB2844E742A4BB646A947B8DEC1BB3
    @Test()
    void stripTest() {
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.strip("str1", (String) null)).thenReturn("return_of_strip1");
            //Act Statement(s)
            String result = StringUtils.strip("str1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_strip1"));
                stringUtils.verify(() -> StringUtils.strip("str1", (String) null), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${strip1Test}, hash: 649830BDE28E505CA5CE39B2B560F495
    @Test()
    void strip1Test() {
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.stripStart("str1", "stripChars1")).thenReturn("return_of_stripStart1");
            stringUtils.when(() -> StringUtils.stripEnd("return_of_stripStart1", "stripChars1")).thenReturn("return_of_stripEnd1");
            //Act Statement(s)
            String result = StringUtils.strip("str1", "stripChars1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_stripEnd1"));
                stringUtils.verify(() -> StringUtils.stripStart("str1", "stripChars1"), atLeast(1));
                stringUtils.verify(() -> StringUtils.stripEnd("return_of_stripStart1", "stripChars1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${stripAccentsWhenIsEmptyInput}, hash: 2340131DCDC43225EE4AAFCF7984391F
    @Test()
    void stripAccentsWhenIsEmptyInput() {
        /* Branches:
         * (isEmpty(input)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("input1")).thenReturn(true);
            //Act Statement(s)
            String result = StringUtils.stripAccents("input1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("input1"));
                stringUtils.verify(() -> StringUtils.isEmpty("input1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${stripAccentsWhenSwitchCharAtCase__u0141_}, hash: BCC0605FD31A8DE20228176CEF4DAFF7
    @Disabled()
    @Test()
    void stripAccentsWhenSwitchCharAtCase__u0141_() {
        /* Branches:
         * (isEmpty(input)) : false
         * (i < decomposed.length()) : true  #  inside convertRemainingAccentCharacters method
         * (switch(charAt) = '\u0141') : true  #  inside convertRemainingAccentCharacters method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("B")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.stripAccents("B");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                stringUtils.verify(() -> StringUtils.isEmpty("B"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${stripAllTest}, hash: 5F37DA7E3BCB56A55FD57F69F721B411
    @Test()
    void stripAllTest() {
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            String[] stringArray = new String[] {};
            String[] stringArray2 = new String[] {};
            stringUtils.when(() -> StringUtils.stripAll(stringArray2, (String) null)).thenReturn(stringArray);
            //Act Statement(s)
            String[] result = StringUtils.stripAll(stringArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(stringArray));
                stringUtils.verify(() -> StringUtils.stripAll(stringArray2, (String) null), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${stripAll1WhenStrsLenEquals0}, hash: 62EB8020D4D19F9D766CDB27C3C6D216
    @Test()
    void stripAll1WhenStrsLenEquals0() {
        /* Branches:
         * (strsLen == 0) : true
         */
        //Arrange Statement(s)
        String[] stringArray = new String[] {};
        //Act Statement(s)
        String[] result = StringUtils.stripAll(stringArray, "stripChars1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringArray)));
    }

    //BaseRock generated method id: ${stripAll1WhenStrsLenNotEquals0}, hash: 603794AEECDACED3F06FCD2AE5BEB8AA
    @Disabled()
    @Test()
    void stripAll1WhenStrsLenNotEquals0() {
        /* Branches:
         * (strsLen == 0) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.strip("strsItem1", "stripChars1")).thenReturn("return_of_strip1");
            String[] stringArray = new String[] { "strsItem1" };
            //Act Statement(s)
            String[] result = StringUtils.stripAll(stringArray, "stripChars1");
            String[] stringResultArray = new String[] { (String) null };
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(stringResultArray));
                stringUtils.verify(() -> StringUtils.strip("strsItem1", "stripChars1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${stripEndWhenEndEquals0}, hash: 2749B539336A5B2385C731B74600A8B9
    @Test()
    void stripEndWhenEndEquals0() {
        /* Branches:
         * (end == 0) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.length("str1")).thenReturn(0);
            //Act Statement(s)
            String result = StringUtils.stripEnd("str1", "stripChars1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("str1"));
                stringUtils.verify(() -> StringUtils.length("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${stripEndWhenStripCharsIsEmpty}, hash: 9314A39A066457D27B397708A10603D8
    @Test()
    void stripEndWhenStripCharsIsEmpty() {
        /* Branches:
         * (end == 0) : false
         * (stripChars == null) : false
         * (stripChars.isEmpty()) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.length("str1")).thenReturn(1);
            //Act Statement(s)
            String result = StringUtils.stripEnd("str1", "");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("str1"));
                stringUtils.verify(() -> StringUtils.length("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${stripEndWhenCharacterIsWhitespaceStrCharAtEndMinus1ThrowsNullPointerException}, hash: 7581656AA9B22288C4134C4110D95631
    @Disabled()
    @Test()
    void stripEndWhenCharacterIsWhitespaceStrCharAtEndMinus1ThrowsNullPointerException() {
        /* Branches:
         * (end == 0) : false
         * (stripChars == null) : true
         * (end != 0) : true
         * (Character.isWhitespace(str.charAt(end - 1))) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.length("str1")).thenReturn(0);
            //Act Statement(s)
            final NullPointerException result = assertThrows(NullPointerException.class, () -> {
                StringUtils.stripEnd("str1", (String) null);
            });
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                stringUtils.verify(() -> StringUtils.length("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${stripEndWhenStripCharsIndexOfStrCharAtEndMinus1NotEqualsINDEX_NOT_FOUND}, hash: A761519857085FD6B4B16FCE015397DB
    @Test()
    void stripEndWhenStripCharsIndexOfStrCharAtEndMinus1NotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (end == 0) : false
         * (stripChars == null) : false
         * (stripChars.isEmpty()) : false
         * (end != 0) : true
         * (stripChars.indexOf(str.charAt(end - 1)) != INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.length("A")).thenReturn(1);
            //Act Statement(s)
            String result = StringUtils.stripEnd("A", "A");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(""));
                stringUtils.verify(() -> StringUtils.length("A"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${stripStartWhenStrLenEquals0}, hash: E9400BA4141A3C6A7993F5E17BA5C15F
    @Test()
    void stripStartWhenStrLenEquals0() {
        /* Branches:
         * (strLen == 0) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.length("str1")).thenReturn(0);
            //Act Statement(s)
            String result = StringUtils.stripStart("str1", "stripChars1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("str1"));
                stringUtils.verify(() -> StringUtils.length("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${stripStartWhenStripCharsIsEmpty}, hash: E9206B514EC20D0FA3A9D562A4C959F6
    @Test()
    void stripStartWhenStripCharsIsEmpty() {
        /* Branches:
         * (strLen == 0) : false
         * (stripChars == null) : false
         * (stripChars.isEmpty()) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.length("str1")).thenReturn(1);
            //Act Statement(s)
            String result = StringUtils.stripStart("str1", "");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("str1"));
                stringUtils.verify(() -> StringUtils.length("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${stripStartWhenCharacterIsWhitespaceStrCharAtStartThrowsNullPointerException}, hash: 36645E15AB30D00418D80E220CDA3A01
    @Disabled()
    @Test()
    void stripStartWhenCharacterIsWhitespaceStrCharAtStartThrowsNullPointerException() {
        /* Branches:
         * (strLen == 0) : false
         * (stripChars == null) : true
         * (start != strLen) : true
         * (Character.isWhitespace(str.charAt(start))) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.length("str1")).thenReturn(0);
            //Act Statement(s)
            final NullPointerException result = assertThrows(NullPointerException.class, () -> {
                StringUtils.stripStart("str1", (String) null);
            });
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                stringUtils.verify(() -> StringUtils.length("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${stripStartWhenStripCharsIndexOfStrCharAtStartNotEqualsINDEX_NOT_FOUND}, hash: 185B5D646052EEE27D8B79B1913338B4
    @Test()
    void stripStartWhenStripCharsIndexOfStrCharAtStartNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (strLen == 0) : false
         * (stripChars == null) : false
         * (stripChars.isEmpty()) : false
         * (start != strLen) : true
         * (stripChars.indexOf(str.charAt(start)) != INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.length("AB")).thenReturn(1);
            //Act Statement(s)
            String result = StringUtils.stripStart("AB", "A");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("B"));
                stringUtils.verify(() -> StringUtils.length("AB"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${stripToEmptyWhenStrIsNull}, hash: 109BFC33E35C64149E9F091BA240B460
    @Test()
    void stripToEmptyWhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
        //Act Statement(s)
        String result = StringUtils.stripToEmpty((String) null);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${stripToEmptyWhenStrIsNotNull}, hash: CD342019936AD3D4005B5DB9158D4EFC
    @Test()
    void stripToEmptyWhenStrIsNotNull() {
        /* Branches:
         * (str == null) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.strip("str1", (String) null)).thenReturn("return_of_strip1");
            //Act Statement(s)
            String result = StringUtils.stripToEmpty("str1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_strip1"));
                stringUtils.verify(() -> StringUtils.strip("str1", (String) null), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${stripToNullWhenStrIsNull}, hash: E3A5FF081F3A50A0D6FBF9369BA4199B
    @Test()
    void stripToNullWhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
        //Act Statement(s)
        String result = StringUtils.stripToNull((String) null);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${stripToNullWhenStrIsEmpty}, hash: 76927C9ED15348C940B36BB00E435E72
    @Test()
    void stripToNullWhenStrIsEmpty() {
        /* Branches:
         * (str == null) : false
         * (str.isEmpty()) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.strip("str1", (String) null)).thenReturn("");
            //Act Statement(s)
            String result = StringUtils.stripToNull("str1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(nullValue()));
                stringUtils.verify(() -> StringUtils.strip("str1", (String) null), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${stripToNullWhenStrNotIsEmpty}, hash: 7BBFFC766F87BF607D014EFE229B064B
    @Test()
    void stripToNullWhenStrNotIsEmpty() {
        /* Branches:
         * (str == null) : false
         * (str.isEmpty()) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.strip("str1", (String) null)).thenReturn("A");
            //Act Statement(s)
            String result = StringUtils.stripToNull("str1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("A"));
                stringUtils.verify(() -> StringUtils.strip("str1", (String) null), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${substringWhenStrIsNull}, hash: E0C212C5D88A8D4D20B50A01398A92D2
    @Test()
    void substringWhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
        //Act Statement(s)
        String result = StringUtils.substring((String) null, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${substringWhenStartGreaterThanStrLength}, hash: E4B6AC9AF55C24E03B01BD3FD6ED9188
    @Disabled()
    @Test()
    void substringWhenStartGreaterThanStrLength() {
        /* Branches:
         * (str == null) : false
         * (start < 0) : true
         * (start < 0) : true
         * (start > str.length()) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String result = StringUtils.substring("str1", 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${substringWhenStartNotGreaterThanStrLength}, hash: D323175A79C7B2C7E94F0B5FF6A18159
    @Test()
    void substringWhenStartNotGreaterThanStrLength() {
        /* Branches:
         * (str == null) : false
         * (start < 0) : true
         * (start < 0) : true
         * (start > str.length()) : false
         */
        //Act Statement(s)
        String result = StringUtils.substring("A", -2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${substring1WhenStrIsNull}, hash: 9FCC8AF4554193EEFDC7E615B06ABC74
    @Test()
    void substring1WhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
        //Act Statement(s)
        String result = StringUtils.substring((String) null, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${substring1WhenStartGreaterThanEnd}, hash: 4A5F79C08BFE8EF083DCEECAC6F1BDA9
    @Test()
    void substring1WhenStartGreaterThanEnd() {
        /* Branches:
         * (str == null) : false
         * (end < 0) : true
         * (start < 0) : true
         * (end > str.length()) : true
         * (start > end) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String result = StringUtils.substring("str1", 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${substring1WhenEndLessThan0}, hash: 00914F1AD6279FBF4ACBC3E809DF915A
    @Disabled()
    @Test()
    void substring1WhenEndLessThan0() {
        /* Branches:
         * (str == null) : false
         * (end < 0) : true
         * (start < 0) : true
         * (end > str.length()) : true
         * (start > end) : false
         * (start < 0) : true
         * (end < 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String result = StringUtils.substring("str1", 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${substringAfterWhenIsEmptyStr}, hash: 695E1233B4F7CC6F0B24F790B5FE0EA4
    @Test()
    void substringAfterWhenIsEmptyStr() {
        /* Branches:
         * (isEmpty(str)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(true);
            //Act Statement(s)
            String result = StringUtils.substringAfter("str1", 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("str1"));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${substringAfterWhenPosEqualsINDEX_NOT_FOUND}, hash: 84C60ED43D8EDBA30B51513322B79B01
    @Test()
    void substringAfterWhenPosEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (isEmpty(str)) : false
         * (pos == INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.substringAfter("", 65);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(""));
                stringUtils.verify(() -> StringUtils.isEmpty(""), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${substringAfterWhenPosNotEqualsINDEX_NOT_FOUND}, hash: D7218E145FD26C8BAAD509B026A2878F
    @Test()
    void substringAfterWhenPosNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (isEmpty(str)) : false
         * (pos == INDEX_NOT_FOUND) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("AC")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.substringAfter("AC", 65);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("C"));
                stringUtils.verify(() -> StringUtils.isEmpty("AC"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${substringAfter1WhenIsEmptyStr}, hash: 7D3FBDE8CF31C91C2BCC5959645C7E99
    @Test()
    void substringAfter1WhenIsEmptyStr() {
        /* Branches:
         * (isEmpty(str)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(true);
            //Act Statement(s)
            String result = StringUtils.substringAfter("str1", "separator1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("str1"));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${substringAfter1WhenSeparatorIsNull}, hash: EDFC4069C04C9E068335CC3FDA858CFD
    @Test()
    void substringAfter1WhenSeparatorIsNull() {
        /* Branches:
         * (isEmpty(str)) : false
         * (separator == null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.substringAfter("str1", (String) null);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(""));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${substringAfter1WhenPosEqualsINDEX_NOT_FOUND}, hash: 6AE58601777D3455AC7A5BC9E74129C6
    @Test()
    void substringAfter1WhenPosEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (isEmpty(str)) : false
         * (separator == null) : false
         * (pos == INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("B")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.substringAfter("B", "A");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(""));
                stringUtils.verify(() -> StringUtils.isEmpty("B"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${substringAfter1WhenPosNotEqualsINDEX_NOT_FOUND}, hash: 6910B815B633B1E7EB43E7537E32FA85
    @Test()
    void substringAfter1WhenPosNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (isEmpty(str)) : false
         * (separator == null) : false
         * (pos == INDEX_NOT_FOUND) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("F")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.substringAfter("F", "");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("F"));
                stringUtils.verify(() -> StringUtils.isEmpty("F"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${substringAfterLastWhenIsEmptyStr}, hash: 263162A3CA8B4FAF5A0F97E3E6AFA2EF
    @Test()
    void substringAfterLastWhenIsEmptyStr() {
        /* Branches:
         * (isEmpty(str)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(true);
            //Act Statement(s)
            String result = StringUtils.substringAfterLast("str1", 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("str1"));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${substringAfterLastWhenPosEqualsStrLengthMinus1}, hash: 2D9A2E5D0B9A562799527B6C973178F3
    @Test()
    void substringAfterLastWhenPosEqualsStrLengthMinus1() {
        /* Branches:
         * (isEmpty(str)) : false
         * (pos == INDEX_NOT_FOUND) : false
         * (pos == str.length() - 1) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.substringAfterLast("str1", 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(""));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${substringAfterLastWhenPosNotEqualsStrLengthMinus1}, hash: C6642DD7524CE8D534C7EC6A52B1160A
    @Disabled()
    @Test()
    void substringAfterLastWhenPosNotEqualsStrLengthMinus1() {
        /* Branches:
         * (isEmpty(str)) : false
         * (pos == INDEX_NOT_FOUND) : false
         * (pos == str.length() - 1) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.substringAfterLast("str1", 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${substringAfterLast1WhenIsEmptyStr}, hash: F64F82E9645DE83C788FB99A70ED0D11
    @Test()
    void substringAfterLast1WhenIsEmptyStr() {
        /* Branches:
         * (isEmpty(str)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(true);
            //Act Statement(s)
            String result = StringUtils.substringAfterLast("str1", "separator1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("str1"));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${substringAfterLast1WhenIsEmptySeparator}, hash: E5CFFE783EB7412DB68EC7AC05F2BAC1
    @Test()
    void substringAfterLast1WhenIsEmptySeparator() {
        /* Branches:
         * (isEmpty(str)) : false
         * (isEmpty(separator)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("separator1")).thenReturn(true);
            //Act Statement(s)
            String result = StringUtils.substringAfterLast("str1", "separator1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(""));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("separator1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${substringAfterLast1WhenPosEqualsStrLengthMinusSeparatorLength}, hash: 13912677AAA5565163078EA9246B1F0A
    @Test()
    void substringAfterLast1WhenPosEqualsStrLengthMinusSeparatorLength() {
        /* Branches:
         * (isEmpty(str)) : false
         * (isEmpty(separator)) : false
         * (pos == INDEX_NOT_FOUND) : false
         * (pos == str.length() - separator.length()) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("separator1")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.substringAfterLast("str1", "separator1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(""));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("separator1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${substringAfterLast1WhenPosNotEqualsStrLengthMinusSeparatorLength}, hash: DE7E3A863A7E0F31C7D2EA8C6A25B127
    @Disabled()
    @Test()
    void substringAfterLast1WhenPosNotEqualsStrLengthMinusSeparatorLength() {
        /* Branches:
         * (isEmpty(str)) : false
         * (isEmpty(separator)) : false
         * (pos == INDEX_NOT_FOUND) : false
         * (pos == str.length() - separator.length()) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("separator1")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.substringAfterLast("str1", "separator1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("separator1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${substringBeforeWhenIsEmptyStr}, hash: 9C84595DE030D2CCB2C583BB8FEE1DF1
    @Test()
    void substringBeforeWhenIsEmptyStr() {
        /* Branches:
         * (isEmpty(str)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(true);
            //Act Statement(s)
            String result = StringUtils.substringBefore("str1", 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("str1"));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${substringBeforeWhenPosEqualsINDEX_NOT_FOUND}, hash: A72362E7B54D1D85AFB138F6A0D94D25
    @Test()
    void substringBeforeWhenPosEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (isEmpty(str)) : false
         * (pos == INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.substringBefore("", 65);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(""));
                stringUtils.verify(() -> StringUtils.isEmpty(""), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${substringBeforeWhenPosNotEqualsINDEX_NOT_FOUND}, hash: C0DE7A510D91B3F0209B468302E48D9D
    @Test()
    void substringBeforeWhenPosNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (isEmpty(str)) : false
         * (pos == INDEX_NOT_FOUND) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("A")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.substringBefore("A", 65);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(""));
                stringUtils.verify(() -> StringUtils.isEmpty("A"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${substringBefore1WhenSeparatorIsNull}, hash: 13835A89AB9F367A316B9BF079ECA403
    @Test()
    void substringBefore1WhenSeparatorIsNull() {
        /* Branches:
         * (isEmpty(str)) : false
         * (separator == null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.substringBefore("str1", (String) null);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("str1"));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${substringBefore1WhenSeparatorIsEmpty}, hash: 68F0EE4BC3A2AB19BEAE1AB10753F81A
    @Test()
    void substringBefore1WhenSeparatorIsEmpty() {
        /* Branches:
         * (isEmpty(str)) : false
         * (separator == null) : false
         * (separator.isEmpty()) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.substringBefore("str1", "");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(""));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${substringBefore1WhenPosEqualsINDEX_NOT_FOUND}, hash: 28F292F820F8768BAF0BF65F00AD3031
    @Test()
    void substringBefore1WhenPosEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (isEmpty(str)) : false
         * (separator == null) : false
         * (separator.isEmpty()) : false
         * (pos == INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("B")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.substringBefore("B", "A");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("B"));
                stringUtils.verify(() -> StringUtils.isEmpty("B"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${substringBefore1WhenPosNotEqualsINDEX_NOT_FOUND}, hash: 28CC30ADDA47AC071CF269EC0FE4FC1C
    @Test()
    void substringBefore1WhenPosNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (isEmpty(str)) : false
         * (separator == null) : false
         * (separator.isEmpty()) : false
         * (pos == INDEX_NOT_FOUND) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("A")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.substringBefore("A", "A");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(""));
                stringUtils.verify(() -> StringUtils.isEmpty("A"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${substringBeforeLastWhenIsEmptySeparator}, hash: F4465BC1E6B809062F5B715691FD91A2
    @Test()
    void substringBeforeLastWhenIsEmptySeparator() {
        /* Branches:
         * (isEmpty(str)) : false
         * (isEmpty(separator)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("separator1")).thenReturn(true);
            //Act Statement(s)
            String result = StringUtils.substringBeforeLast("str1", "separator1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("str1"));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("separator1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${substringBeforeLastWhenPosEqualsINDEX_NOT_FOUND}, hash: 26EA00A2FC7A5C9ED534DD8F80871676
    @Test()
    void substringBeforeLastWhenPosEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (isEmpty(str)) : false
         * (isEmpty(separator)) : false
         * (pos == INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("B")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("A")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.substringBeforeLast("B", "A");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("B"));
                stringUtils.verify(() -> StringUtils.isEmpty("B"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("A"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${substringBeforeLastWhenPosNotEqualsINDEX_NOT_FOUND}, hash: 75D3FC05856359528E56D5136AE2DC8A
    @Disabled()
    @Test()
    void substringBeforeLastWhenPosNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (isEmpty(str)) : false
         * (isEmpty(separator)) : false
         * (pos == INDEX_NOT_FOUND) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("separator1")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.substringBeforeLast("str1", "separator1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("separator1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${substringBetweenTest}, hash: 56F942BC39E07066B0F4DC9F00B50DFA
    @Test()
    void substringBetweenTest() {
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.substringBetween("str1", "tag1", "tag1")).thenReturn("return_of_substringBetween1");
            //Act Statement(s)
            String result = StringUtils.substringBetween("str1", "tag1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_substringBetween1"));
                stringUtils.verify(() -> StringUtils.substringBetween("str1", "tag1", "tag1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${substringBetween1WhenObjectUtilsNotAllNotNullStrOpenClose}, hash: BB608B8B9984DC269B838DF026168F4D
    @Test()
    void substringBetween1WhenObjectUtilsNotAllNotNullStrOpenClose() {
        /* Branches:
         * (!ObjectUtils.allNotNull(str, open, close)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String result = StringUtils.substringBetween("str1", "open1", "close1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${substringBetween1WhenEndNotEqualsINDEX_NOT_FOUND}, hash: 39B5A0DAFA5F188A86CE6FD13D6D534B
    @Test()
    void substringBetween1WhenEndNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (!ObjectUtils.allNotNull(str, open, close)) : false
         * (start != INDEX_NOT_FOUND) : true
         * (end != INDEX_NOT_FOUND) : true
         */
        //Act Statement(s)
        String result = StringUtils.substringBetween("BD", "", "");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${substringBetween1WhenEndEqualsINDEX_NOT_FOUND}, hash: 232329FC8CECF994F73B16B82DC3193A
    @Test()
    void substringBetween1WhenEndEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (!ObjectUtils.allNotNull(str, open, close)) : false
         * (start != INDEX_NOT_FOUND) : true
         * (end != INDEX_NOT_FOUND) : false
         */
        //Act Statement(s)
        String result = StringUtils.substringBetween("A", "A", "B");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${substringsBetweenWhenIsEmptyClose}, hash: 041115ED54791BF9AAF7F3D6D5C3247C
    @Test()
    void substringsBetweenWhenIsEmptyClose() {
        /* Branches:
         * (str == null) : false
         * (isEmpty(open)) : false
         * (isEmpty(close)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("open1")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("close1")).thenReturn(true);
            //Act Statement(s)
            String[] result = StringUtils.substringsBetween("str1", "open1", "close1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(nullValue()));
                stringUtils.verify(() -> StringUtils.isEmpty("open1"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("close1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${substringsBetweenWhenStrLenEquals0}, hash: 9A7BF05F36ECA43230FBAA1704F2AE0E
    @Test()
    void substringsBetweenWhenStrLenEquals0() {
        /* Branches:
         * (str == null) : false
         * (isEmpty(open)) : false
         * (isEmpty(close)) : false
         * (strLen == 0) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("open1")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("close1")).thenReturn(false);
            //Act Statement(s)
            String[] result = StringUtils.substringsBetween("", "open1", "close1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.length, equalTo(0));
                stringUtils.verify(() -> StringUtils.isEmpty("open1"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("close1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${substringsBetweenWhenStartLessThan0AndListIsEmpty}, hash: FCD49BB8CF8DC8715D3591A82C6836F1
    @Test()
    void substringsBetweenWhenStartLessThan0AndListIsEmpty() {
        /* Branches:
         * (str == null) : false
         * (isEmpty(open)) : false
         * (isEmpty(close)) : false
         * (strLen == 0) : false
         * (pos < strLen - closeLen) : true
         * (start < 0) : true
         * (list.isEmpty()) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("B")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("")).thenReturn(false);
            //Act Statement(s)
            String[] result = StringUtils.substringsBetween("C", "B", "");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(nullValue()));
                stringUtils.verify(() -> StringUtils.isEmpty("B"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty(""), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${substringsBetweenWhenEndLessThan0AndListIsEmpty}, hash: D4FD5FAD3F336DB6FDB72F342569F0DB
    @Test()
    void substringsBetweenWhenEndLessThan0AndListIsEmpty() {
        /* Branches:
         * (str == null) : false
         * (isEmpty(open)) : false
         * (isEmpty(close)) : false
         * (strLen == 0) : false
         * (pos < strLen - closeLen) : true
         * (start < 0) : false
         * (end < 0) : true
         * (list.isEmpty()) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("A")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("B")).thenReturn(false);
            //Act Statement(s)
            String[] result = StringUtils.substringsBetween("AD", "A", "B");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(nullValue()));
                stringUtils.verify(() -> StringUtils.isEmpty("A"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("B"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${substringsBetweenWhenListNotIsEmpty}, hash: 50DF14EAEEE921D516AB1897DE2D1888
    @Disabled()
    @Test()
    void substringsBetweenWhenListNotIsEmpty() {
        /* Branches:
         * (str == null) : false
         * (isEmpty(open)) : false
         * (isEmpty(close)) : false
         * (strLen == 0) : false
         * (pos < strLen - closeLen) : true
         * (start < 0) : false
         * (end < 0) : false
         * (list.isEmpty()) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("")).thenReturn(false);
            //Act Statement(s)
            String[] result = StringUtils.substringsBetween("D", "", "");
            String[] stringResultArray = new String[] { "", "", "", "", "", "" };
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(stringResultArray));
                stringUtils.verify(() -> StringUtils.isEmpty(""), atLeast(2));
            });
        }
    }

    //BaseRock generated method id: ${swapCaseWhenIsEmptyStr}, hash: 14D89E6B4830B8820C643529161873E2
    @Test()
    void swapCaseWhenIsEmptyStr() {
        /* Branches:
         * (isEmpty(str)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(true);
            //Act Statement(s)
            String result = StringUtils.swapCase("str1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("str1"));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${swapCaseWhenCharacterIsTitleCaseOldCodepoint}, hash: BF414E7F4F7CAA7573A21DE845D1A462
    @Disabled()
    @Test()
    void swapCaseWhenCharacterIsTitleCaseOldCodepoint() {
        /* Branches:
         * (isEmpty(str)) : false
         * (i < strLen) : true
         * (Character.isUpperCase(oldCodepoint)) : false
         * (Character.isTitleCase(oldCodepoint)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.swapCase("str1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toCodePointsWhenCsIsNull}, hash: 6D78E4F0E7165A51886CE31198A6189D
    @Test()
    void toCodePointsWhenCsIsNull() {
        /* Branches:
         * (cs == null) : true
         */
        //Arrange Statement(s)
        CharSequence charSequence = null;
        //Act Statement(s)
        int[] result = StringUtils.toCodePoints(charSequence);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toCodePointsWhenCsLengthEquals0}, hash: BE55564CF60A3B0E00C91B97FC8C19C8
    @Disabled()
    @Test()
    void toCodePointsWhenCsLengthEquals0() {
        /* Branches:
         * (cs == null) : false
         * (cs.length() == 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        int[] result = StringUtils.toCodePoints("cs1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toCodePointsWhenILessThanResultLength}, hash: D065851AD6C943FD1E59E72417034AF9
    @Test()
    void toCodePointsWhenILessThanResultLength() {
        /* Branches:
         * (cs == null) : false
         * (cs.length() == 0) : false
         * (i < result.length) : true
         */
        //Act Statement(s)
        int[] result = StringUtils.toCodePoints("cs1");
        int[] intResultArray = new int[] { 99, 115, 49 };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(intResultArray)));
    }

    //BaseRock generated method id: ${toEncodedStringTest}, hash: 9217421922399739D7F707A99EE9704F
    @Disabled()
    @Test()
    void toEncodedStringTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        Charset charset = Charset.defaultCharset();
        //Act Statement(s)
        String result = StringUtils.toEncodedString(byteArray, charset);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${toRootLowerCaseWhenSourceIsNull}, hash: 84769E01A8EC59AD067718C701B2EC13
    @Test()
    void toRootLowerCaseWhenSourceIsNull() {
        /* Branches:
         * (source == null) : true
         */
        //Act Statement(s)
        String result = StringUtils.toRootLowerCase((String) null);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toRootLowerCaseWhenSourceIsNotNull}, hash: 58D4EEC0F60E400770D36DA4DD4ECA1F
    @Test()
    void toRootLowerCaseWhenSourceIsNotNull() {
        /* Branches:
         * (source == null) : false
         */
        //Act Statement(s)
        String result = StringUtils.toRootLowerCase("A");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("a")));
    }

    //BaseRock generated method id: ${toRootUpperCaseWhenSourceIsNull}, hash: 6F1246C4E3FCB1D863D0FEC76AF08B52
    @Test()
    void toRootUpperCaseWhenSourceIsNull() {
        /* Branches:
         * (source == null) : true
         */
        //Act Statement(s)
        String result = StringUtils.toRootUpperCase((String) null);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toRootUpperCaseWhenSourceIsNotNull}, hash: 6012314AF44F8628492B669CC1317886
    @Test()
    void toRootUpperCaseWhenSourceIsNotNull() {
        /* Branches:
         * (source == null) : false
         */
        //Act Statement(s)
        String result = StringUtils.toRootUpperCase("A");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${toStringTest}, hash: CA5879789BC28D7999EFF2EFBDA2B71C
    @Disabled()
    @Test()
    void toStringTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        //Act Statement(s)
        String result = StringUtils.toString(byteArray, "A");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${trimWhenStrIsNull}, hash: D1DC07DF29EE3EDEAEA6C2DBA3F9ED89
    @Test()
    void trimWhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
        //Act Statement(s)
        String result = StringUtils.trim((String) null);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${trimWhenStrIsNotNull}, hash: FE14A02278B4BD793F987ACCC93DA8C2
    @Test()
    void trimWhenStrIsNotNull() {
        /* Branches:
         * (str == null) : false
         */
        //Act Statement(s)
        String result = StringUtils.trim("A ");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${trimToEmptyWhenStrIsNull}, hash: FF6EFD5BD1CEE67B68169EB74BBC00CA
    @Test()
    void trimToEmptyWhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
        //Act Statement(s)
        String result = StringUtils.trimToEmpty((String) null);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${trimToEmptyWhenStrIsNotNull}, hash: 1946C297BED4C2158C083AFD9FCE6C74
    @Test()
    void trimToEmptyWhenStrIsNotNull() {
        /* Branches:
         * (str == null) : false
         */
        //Act Statement(s)
        String result = StringUtils.trimToEmpty("A ");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${trimToNullWhenIsEmptyTs}, hash: 8CC74CBB173F2F78D4C70397FAE17D88
    @Test()
    void trimToNullWhenIsEmptyTs() {
        /* Branches:
         * (isEmpty(ts)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.trim("str1")).thenReturn("return_of_trim1");
            stringUtils.when(() -> StringUtils.isEmpty("return_of_trim1")).thenReturn(true);
            //Act Statement(s)
            String result = StringUtils.trimToNull("str1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(nullValue()));
                stringUtils.verify(() -> StringUtils.trim("str1"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("return_of_trim1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${trimToNullWhenIsEmptyNotTs}, hash: DDFA82F38F8F08558F5A6FB3300A5BCC
    @Test()
    void trimToNullWhenIsEmptyNotTs() {
        /* Branches:
         * (isEmpty(ts)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.trim("str1")).thenReturn("return_of_trim1");
            stringUtils.when(() -> StringUtils.isEmpty("return_of_trim1")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.trimToNull("str1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_trim1"));
                stringUtils.verify(() -> StringUtils.trim("str1"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("return_of_trim1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${truncateTest}, hash: C6BD1B297328D69388B977D13F6D62C4
    @Test()
    void truncateTest() {
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.truncate("str1", 0, 0)).thenReturn("return_of_truncate1");
            //Act Statement(s)
            String result = StringUtils.truncate("str1", 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_truncate1"));
                stringUtils.verify(() -> StringUtils.truncate("str1", 0, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${truncate1WhenOffsetLessThan0ThrowsIllegalArgumentException}, hash: E9B2BFE5078B2B77F2FB63550939B3BB
    @Test()
    void truncate1WhenOffsetLessThan0ThrowsIllegalArgumentException() {
        /* Branches:
         * (offset < 0) : true
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("offset cannot be negative");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            StringUtils.truncate("str1", -2147483648, 0);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${truncate1WhenMaxWidthLessThan0ThrowsIllegalArgumentException}, hash: 59A0E9F76CAFBBCC86FE8961903E90AC
    @Test()
    void truncate1WhenMaxWidthLessThan0ThrowsIllegalArgumentException() {
        /* Branches:
         * (offset < 0) : false
         * (maxWidth < 0) : true
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("maxWith cannot be negative");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            StringUtils.truncate("str1", 1, -1);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${truncate1WhenStrIsNull}, hash: 09663E009C0C3680EEC7AACBCFFAC07F
    @Test()
    void truncate1WhenStrIsNull() {
        /* Branches:
         * (offset < 0) : false
         * (maxWidth < 0) : false
         * (str == null) : true
         */
        //Act Statement(s)
        String result = StringUtils.truncate((String) null, 1, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${truncate1WhenOffsetGreaterThanStrLength}, hash: 9BF653B022DFB93DF18394A4B9F91EE8
    @Test()
    void truncate1WhenOffsetGreaterThanStrLength() {
        /* Branches:
         * (offset < 0) : false
         * (maxWidth < 0) : false
         * (str == null) : false
         * (offset > str.length()) : true
         */
        //Act Statement(s)
        String result = StringUtils.truncate("", 2, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${truncate1WhenStrLengthGreaterThanMaxWidth}, hash: 5B3B03CEFE8130208FF1B8FEA109EDFE
    @Test()
    void truncate1WhenStrLengthGreaterThanMaxWidth() {
        /* Branches:
         * (offset < 0) : false
         * (maxWidth < 0) : false
         * (str == null) : false
         * (offset > str.length()) : false
         * (str.length() > maxWidth) : true
         */
        //Act Statement(s)
        String result = StringUtils.truncate("A", 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${truncate1WhenStrLengthNotGreaterThanMaxWidth}, hash: 5169ED1DE76445FBFE68AF209565163C
    @Test()
    void truncate1WhenStrLengthNotGreaterThanMaxWidth() {
        /* Branches:
         * (offset < 0) : false
         * (maxWidth < 0) : false
         * (str == null) : false
         * (offset > str.length()) : false
         * (str.length() > maxWidth) : false
         */
        //Act Statement(s)
        String result = StringUtils.truncate("A", 0, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${uncapitalizeWhenStrLenEquals0}, hash: E542121C1AE868D1C6819F46E5F5CCC3
    @Test()
    void uncapitalizeWhenStrLenEquals0() {
        /* Branches:
         * (strLen == 0) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.length("str1")).thenReturn(0);
            //Act Statement(s)
            String result = StringUtils.uncapitalize("str1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("str1"));
                stringUtils.verify(() -> StringUtils.length("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${uncapitalizeWhenFirstCodePointEqualsNewCodePoint}, hash: EF050A4B0DE45466BF52DE12BCEA2D68
    @Test()
    void uncapitalizeWhenFirstCodePointEqualsNewCodePoint() {
        /* Branches:
         * (strLen == 0) : false
         * (firstCodePoint == newCodePoint) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.length("str1")).thenReturn(-1);
            //Act Statement(s)
            String result = StringUtils.uncapitalize("str1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("str1"));
                stringUtils.verify(() -> StringUtils.length("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${uncapitalizeWhenInOffsetLessThanStrLen}, hash: EBBFAC094375A8F2A12BCF1C8B2D228A
    @Disabled()
    @Test()
    void uncapitalizeWhenInOffsetLessThanStrLen() {
        /* Branches:
         * (strLen == 0) : false
         * (firstCodePoint == newCodePoint) : false
         * (inOffset < strLen) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.length("str1")).thenReturn(0);
            //Act Statement(s)
            String result = StringUtils.uncapitalize("str1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                stringUtils.verify(() -> StringUtils.length("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${unwrapWhenStrLengthEquals1}, hash: 6B1D6D4FDC7306F3D210DB8F69C7864F
    @Test()
    void unwrapWhenStrLengthEquals1() {
        /* Branches:
         * (isEmpty(str)) : false
         * (wrapChar == CharUtils.NUL) : false
         * (str.length() == 1) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("B")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.unwrap("B", 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("B"));
                stringUtils.verify(() -> StringUtils.isEmpty("B"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${unwrapWhenStrCharAtStrLengthMinus1EqualsWrapChar}, hash: 6E730AC0F0001705EF7F70E0CD55B730
    @Test()
    void unwrapWhenStrCharAtStrLengthMinus1EqualsWrapChar() {
        /* Branches:
         * (isEmpty(str)) : false
         * (wrapChar == CharUtils.NUL) : false
         * (str.length() == 1) : false
         * (str.charAt(0) == wrapChar) : true
         * (str.charAt(str.length() - 1) == wrapChar) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("BCB")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.unwrap("BCB", 'B');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("C"));
                stringUtils.verify(() -> StringUtils.isEmpty("BCB"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${unwrapWhenStrCharAtStrLengthMinus1NotEqualsWrapChar}, hash: B0F7CEB94139FCE7349E53606AD02004
    @Test()
    void unwrapWhenStrCharAtStrLengthMinus1NotEqualsWrapChar() {
        /* Branches:
         * (isEmpty(str)) : false
         * (wrapChar == CharUtils.NUL) : false
         * (str.length() == 1) : false
         * (str.charAt(0) == wrapChar) : true
         * (str.charAt(str.length() - 1) == wrapChar) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("BC")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.unwrap("BC", 'B');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("BC"));
                stringUtils.verify(() -> StringUtils.isEmpty("BC"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${unwrap1WhenStrLengthLessThan2MultipliedByWrapTokenLength}, hash: D0ECB8D4284EB6A3B522445EAA5EFBC1
    @Test()
    void unwrap1WhenStrLengthLessThan2MultipliedByWrapTokenLength() {
        /* Branches:
         * (isEmpty(str)) : false
         * (isEmpty(wrapToken)) : false
         * (str.length() < 2 * wrapToken.length()) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("A")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("B")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.unwrap("A", "B");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("A"));
                stringUtils.verify(() -> StringUtils.isEmpty("A"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("B"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${unwrap1WhenStringsCSEndsWithStrWrapToken}, hash: F7244CD3BBE4BC7225AEC4E2E58B8B29
    @Disabled()
    @Test()
    void unwrap1WhenStringsCSEndsWithStrWrapToken() {
        /* Branches:
         * (isEmpty(str)) : false
         * (isEmpty(wrapToken)) : false
         * (str.length() < 2 * wrapToken.length()) : false
         * (Strings.CS.startsWith(str, wrapToken)) : true
         * (Strings.CS.endsWith(str, wrapToken)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("wrapToken1")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.unwrap("str1", "wrapToken1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("wrapToken1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${unwrap1WhenStringsCSNotEndsWithStrWrapToken}, hash: 9886ADC4919900C067EFF705D7861675
    @Test()
    void unwrap1WhenStringsCSNotEndsWithStrWrapToken() {
        /* Branches:
         * (isEmpty(str)) : false
         * (isEmpty(wrapToken)) : false
         * (str.length() < 2 * wrapToken.length()) : false
         * (Strings.CS.startsWith(str, wrapToken)) : true
         * (Strings.CS.endsWith(str, wrapToken)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("wrapToken1")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.unwrap("str1", "wrapToken1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("str1"));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("wrapToken1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${upperCaseWhenStrIsNull}, hash: 23D5801968EC02AA7E0E1ABC16AC776A
    @Test()
    void upperCaseWhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
        //Act Statement(s)
        String result = StringUtils.upperCase((String) null);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${upperCaseWhenStrIsNotNull}, hash: 7B756BF796EFDF935B788D7B1688133A
    @Test()
    void upperCaseWhenStrIsNotNull() {
        /* Branches:
         * (str == null) : false
         */
        //Act Statement(s)
        String result = StringUtils.upperCase("A");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${upperCase1WhenStrIsNull}, hash: 379C24D203D58DAE9BADF84D90FC8E15
    @Test()
    void upperCase1WhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
        //Arrange Statement(s)
        Locale locale = new Locale("language1");
        //Act Statement(s)
        String result = StringUtils.upperCase((String) null, locale);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${upperCase1WhenStrIsNotNull}, hash: 07A4827BD47CF5C2B560ECE9829B45AE
    @Test()
    void upperCase1WhenStrIsNotNull() {
        /* Branches:
         * (str == null) : false
         */
        //Arrange Statement(s)
        Locale locale = new Locale("language1");
        //Act Statement(s)
        String result = StringUtils.upperCase("A", locale);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${valueOfWhenValueIsNull}, hash: F2A8297738540ED8FCA8539F982EB2DA
    @Test()
    void valueOfWhenValueIsNull() {
        /* Branches:
         * (value == null) : true
         */
        //Arrange Statement(s)
        char[] _char = null;
        //Act Statement(s)
        String result = StringUtils.valueOf(_char);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${valueOfWhenValueIsNotNull}, hash: C84EE69AF2BE636648024A5F81737CD6
    @Disabled()
    @Test()
    void valueOfWhenValueIsNotNull() {
        /* Branches:
         * (value == null) : false
         */
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        //Act Statement(s)
        String result = StringUtils.valueOf(charArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("[]")));
    }

    //BaseRock generated method id: ${wrapWhenWrapWithEqualsCharUtilsNUL}, hash: A12C987A53DB570733CDE35A24683E2F
    @Test()
    void wrapWhenWrapWithEqualsCharUtilsNUL() {
        /* Branches:
         * (isEmpty(str)) : false
         * (wrapWith == CharUtils.NUL) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.wrap("str1", '\u0000');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("str1"));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${wrapWhenWrapWithNotEqualsCharUtilsNUL}, hash: 9FADF4163C97E2336919AA39B5BF19F3
    @Test()
    void wrapWhenWrapWithNotEqualsCharUtilsNUL() {
        /* Branches:
         * (isEmpty(str)) : false
         * (wrapWith == CharUtils.NUL) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("B")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.wrap("B", 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("ABA"));
                stringUtils.verify(() -> StringUtils.isEmpty("B"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${wrap1WhenIsEmptyWrapWith}, hash: 0882F2880D27A16A5EA87BF772B280B9
    @Test()
    void wrap1WhenIsEmptyWrapWith() {
        /* Branches:
         * (isEmpty(str)) : false
         * (isEmpty(wrapWith)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("wrapWith1")).thenReturn(true);
            //Act Statement(s)
            String result = StringUtils.wrap("str1", "wrapWith1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("str1"));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("wrapWith1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${wrap1WhenIsEmptyNotWrapWith}, hash: D12F3B280E404384D93FAD98E7092296
    @Test()
    void wrap1WhenIsEmptyNotWrapWith() {
        /* Branches:
         * (isEmpty(str)) : false
         * (isEmpty(wrapWith)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("B")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("A")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.wrap("B", "A");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("ABA"));
                stringUtils.verify(() -> StringUtils.isEmpty("B"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("A"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${wrapIfMissingWhenWrapWithEqualsCharUtilsNUL}, hash: 89925860E85609EC6A9D5588F2D840C8
    @Test()
    void wrapIfMissingWhenWrapWithEqualsCharUtilsNUL() {
        /* Branches:
         * (isEmpty(str)) : false
         * (wrapWith == CharUtils.NUL) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.wrapIfMissing("str1", '\u0000');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("str1"));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${wrapIfMissingWhenNotWrapEnd}, hash: 285111EC2004AEA83ADABFEB9B7EC606
    @Test()
    void wrapIfMissingWhenNotWrapEnd() {
        /* Branches:
         * (isEmpty(str)) : false
         * (wrapWith == CharUtils.NUL) : false
         * (str.charAt(0) != wrapWith) : false
         * (str.charAt(str.length() - 1) != wrapWith) : false
         * (!wrapStart) : true
         * (!wrapEnd) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("A")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.wrapIfMissing("A", 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("A"));
                stringUtils.verify(() -> StringUtils.isEmpty("A"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${wrapIfMissingWhenWrapEnd}, hash: 34171F3E2AE53C93326094D8E0C8B910
    @Test()
    void wrapIfMissingWhenWrapEnd() {
        /* Branches:
         * (isEmpty(str)) : false
         * (wrapWith == CharUtils.NUL) : false
         * (str.charAt(0) != wrapWith) : true
         * (str.charAt(str.length() - 1) != wrapWith) : true
         * (!wrapStart) : false
         * (wrapStart) : true
         * (wrapEnd) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("B")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.wrapIfMissing("B", 'C');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("CBC"));
                stringUtils.verify(() -> StringUtils.isEmpty("B"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${wrapIfMissing1WhenIsEmptyWrapWith}, hash: F83CDC72415F7E30F7AD0EF7378BCE67
    @Test()
    void wrapIfMissing1WhenIsEmptyWrapWith() {
        /* Branches:
         * (isEmpty(str)) : false
         * (isEmpty(wrapWith)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("str1")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("wrapWith1")).thenReturn(true);
            //Act Statement(s)
            String result = StringUtils.wrapIfMissing("str1", "wrapWith1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("str1"));
                stringUtils.verify(() -> StringUtils.isEmpty("str1"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("wrapWith1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${wrapIfMissing1WhenNotWrapEnd}, hash: 56AA72C4FCC510A4B4B9C86BE3218435
    @Test()
    void wrapIfMissing1WhenNotWrapEnd() {
        /* Branches:
         * (isEmpty(str)) : false
         * (isEmpty(wrapWith)) : false
         * (!str.startsWith(wrapWith)) : false
         * (!str.endsWith(wrapWith)) : false
         * (!wrapStart) : true
         * (!wrapEnd) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("B")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.wrapIfMissing("B", "B");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("B"));
                stringUtils.verify(() -> StringUtils.isEmpty("B"), atLeast(2));
            });
        }
    }

    //BaseRock generated method id: ${wrapIfMissing1WhenWrapEnd}, hash: 55E2B1AF7A6B03CED156C4B52FF7CBF5
    @Test()
    void wrapIfMissing1WhenWrapEnd() {
        /* Branches:
         * (isEmpty(str)) : false
         * (isEmpty(wrapWith)) : false
         * (!str.startsWith(wrapWith)) : true
         * (!str.endsWith(wrapWith)) : true
         * (!wrapStart) : false
         * (wrapStart) : true
         * (wrapEnd) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<StringUtils> stringUtils = mockStatic(StringUtils.class, CALLS_REAL_METHODS)) {
            stringUtils.when(() -> StringUtils.isEmpty("C")).thenReturn(false);
            stringUtils.when(() -> StringUtils.isEmpty("B")).thenReturn(false);
            //Act Statement(s)
            String result = StringUtils.wrapIfMissing("C", "B");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("BCB"));
                stringUtils.verify(() -> StringUtils.isEmpty("C"), atLeast(1));
                stringUtils.verify(() -> StringUtils.isEmpty("B"), atLeast(1));
            });
        }
    }
}
