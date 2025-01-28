package org.apache.commons.lang3.time;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import java.util.TimeZone;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class DurationFormatUtilsBaseRockGeneratedTest {

    private final DurationFormatUtils.Token tokenMock = mock(DurationFormatUtils.Token.class);

    //BaseRock generated method id: ${formatWhenIsLiteral}, hash: 16144079FFC3CA44C5800B201AC30CF0
    @Test()
    void formatWhenIsLiteral() {
        /* Branches:
         * (for-each(tokens)) : true
         * (optionalIndex != token.optionalIndex) : true
         * (optionalIndex > -1) : true
         * (isLiteral) : true
         * (!inOptional) : false
         * (!lastOutputZero) : true
         * (inOptional) : true
         * (!isLiteral) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        DurationFormatUtils.Token[] tokenArray = new DurationFormatUtils.Token[] { tokenMock };
        
        //Act Statement(s)
        String result = DurationFormatUtils.format(tokenArray, 0L, 0L, 0L, 0L, 0L, 0L, 0L, false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${formatWhenNotPadWithZerosAndNotInOptional}, hash: 09F62334A5E6B0674E0A0F6EAEA358ED
    @Test()
    void formatWhenNotPadWithZerosAndNotInOptional() {
        /* Branches:
         * (for-each(tokens)) : true
         * (optionalIndex != token.optionalIndex) : true
         * (optionalIndex > -1) : false
         * (isLiteral) : false
         * (value.equals(y)) : false
         * (value.equals(M)) : true
         * (months == 0) : true
         * (!inOptional) : true
         * (padWithZeros) : false  #  inside paddedValue method
         * (inOptional) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        DurationFormatUtils.Token[] tokenArray = new DurationFormatUtils.Token[] { tokenMock };
        
        //Act Statement(s)
        String result = DurationFormatUtils.format(tokenArray, 0L, 0L, 0L, 0L, 0L, 0L, 0L, false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("0")));
    }

    //BaseRock generated method id: ${formatWhenNotInOptionalAndNotPadWithZerosAndNotInOptional}, hash: E81BE7B406D8DA2B8AC169CDE18F5FEF
    @Test()
    void formatWhenNotInOptionalAndNotPadWithZerosAndNotInOptional() {
        /* Branches:
         * (for-each(tokens)) : true
         * (optionalIndex != token.optionalIndex) : true
         * (optionalIndex > -1) : false
         * (isLiteral) : false
         * (value.equals(y)) : false
         * (value.equals(M)) : false
         * (value.equals(d)) : true
         * (days == 0) : true
         * (!inOptional) : true
         * (padWithZeros) : false  #  inside paddedValue method
         * (inOptional) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        DurationFormatUtils.Token[] tokenArray = new DurationFormatUtils.Token[] { tokenMock };
        
        //Act Statement(s)
        String result = DurationFormatUtils.format(tokenArray, 0L, 0L, 0L, 0L, 0L, 0L, 0L, false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("0")));
    }

    //BaseRock generated method id: ${formatWhenLastOutputZero}, hash: F335D319625465A5FB6D7A6CF166715D
    @Test()
    void formatWhenLastOutputZero() {
        /* Branches:
         * (for-each(tokens)) : true
         * (optionalIndex != token.optionalIndex) : true
         * (optionalIndex > -1) : true
         * (isLiteral) : false
         * (value.equals(y)) : true
         * (years == 0) : true
         * (!inOptional) : false
         * (!lastOutputZero) : false
         * (inOptional) : true
         * (!isLiteral) : true
         * (!firstOptionalNonLiteral) : true
         * (lastOutputZero) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        DurationFormatUtils.Token[] tokenArray = new DurationFormatUtils.Token[] { tokenMock };
        
        //Act Statement(s)
        String result = DurationFormatUtils.format(tokenArray, 0L, 0L, 0L, 0L, 0L, 0L, 0L, false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${formatWhenHoursEquals0AndNotInOptionalAndNotPadWithZerosAndNotInOptional}, hash: 876293AEFC09A824BD54637B3A5AAEF6
    @Test()
    void formatWhenHoursEquals0AndNotInOptionalAndNotPadWithZerosAndNotInOptional() {
        /* Branches:
         * (for-each(tokens)) : true
         * (optionalIndex != token.optionalIndex) : true
         * (optionalIndex > -1) : false
         * (isLiteral) : false
         * (value.equals(y)) : false
         * (value.equals(M)) : false
         * (value.equals(d)) : false
         * (value.equals(H)) : true
         * (hours == 0) : true
         * (!inOptional) : true
         * (padWithZeros) : false  #  inside paddedValue method
         * (inOptional) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        DurationFormatUtils.Token[] tokenArray = new DurationFormatUtils.Token[] { tokenMock };
        
        //Act Statement(s)
        String result = DurationFormatUtils.format(tokenArray, 0L, 0L, 0L, 0L, 0L, 0L, 0L, false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("0")));
    }

    //BaseRock generated method id: ${formatWhenNotLastOutputZero}, hash: 4C3002DD4CA7D73717589070D8448403
    @Test()
    void formatWhenNotLastOutputZero() {
        /* Branches:
         * (for-each(tokens)) : true
         * (optionalIndex != token.optionalIndex) : true
         * (optionalIndex > -1) : true
         * (isLiteral) : false
         * (value.equals(y)) : true
         * (years == 0) : false
         * (!inOptional) : false
         * (!lastOutputZero) : true
         * (padWithZeros) : true  #  inside paddedValue method
         * (inOptional) : true
         * (!isLiteral) : true
         * (!firstOptionalNonLiteral) : true
         * (lastOutputZero) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        DurationFormatUtils.Token[] tokenArray = new DurationFormatUtils.Token[] { tokenMock };
        
        //Act Statement(s)
        String result = DurationFormatUtils.format(tokenArray, 2L, 0L, 0L, 0L, 0L, 0L, 0L, true);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("2")));
    }

    //BaseRock generated method id: ${formatWhenNotFirstOptionalNonLiteralAndNotLastOutputZero}, hash: 402DB0A7EF3EDC15712494319138F0A5
    @Test()
    void formatWhenNotFirstOptionalNonLiteralAndNotLastOutputZero() {
        /* Branches:
         * (for-each(tokens)) : true
         * (optionalIndex != token.optionalIndex) : true
         * (optionalIndex > -1) : true
         * (isLiteral) : false
         * (value.equals(y)) : true
         * (years == 0) : false
         * (!inOptional) : false
         * (!lastOutputZero) : true
         * (padWithZeros) : false  #  inside paddedValue method
         * (inOptional) : true
         * (!isLiteral) : true
         * (!firstOptionalNonLiteral) : true
         * (lastOutputZero) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        DurationFormatUtils.Token[] tokenArray = new DurationFormatUtils.Token[] { tokenMock };
        
        //Act Statement(s)
        String result = DurationFormatUtils.format(tokenArray, 2L, 0L, 0L, 0L, 0L, 0L, 0L, false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("2")));
    }

    //BaseRock generated method id: ${formatWhenMinutesEquals0AndNotInOptionalAndNotPadWithZerosAndNotInOptional}, hash: 721B5A789453A2E4860BBA9FFE4A0FC2
    @Test()
    void formatWhenMinutesEquals0AndNotInOptionalAndNotPadWithZerosAndNotInOptional() {
        /* Branches:
         * (for-each(tokens)) : true
         * (optionalIndex != token.optionalIndex) : true
         * (optionalIndex > -1) : false
         * (isLiteral) : false
         * (value.equals(y)) : false
         * (value.equals(M)) : false
         * (value.equals(d)) : false
         * (value.equals(H)) : false
         * (value.equals(m)) : true
         * (minutes == 0) : true
         * (!inOptional) : true
         * (padWithZeros) : false  #  inside paddedValue method
         * (inOptional) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        DurationFormatUtils.Token[] tokenArray = new DurationFormatUtils.Token[] { tokenMock };
        
        //Act Statement(s)
        String result = DurationFormatUtils.format(tokenArray, 0L, 0L, 0L, 0L, 0L, 0L, 0L, false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("0")));
    }

    //BaseRock generated method id: ${formatWhenNotIsLiteralAndNotFirstOptionalNonLiteralAndNotLastOutputZero}, hash: 29CF2AD878A595FC5B610AE26484BAE2
    @Test()
    void formatWhenNotIsLiteralAndNotFirstOptionalNonLiteralAndNotLastOutputZero() {
        /* Branches:
         * (for-each(tokens)) : true
         * (optionalIndex != token.optionalIndex) : true
         * (optionalIndex > -1) : true
         * (isLiteral) : false
         * (value.equals(y)) : false
         * (value.equals(M)) : true
         * (months == 0) : false
         * (!inOptional) : false
         * (!lastOutputZero) : true
         * (padWithZeros) : true  #  inside paddedValue method
         * (inOptional) : true
         * (!isLiteral) : true
         * (!firstOptionalNonLiteral) : true
         * (lastOutputZero) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        DurationFormatUtils.Token[] tokenArray = new DurationFormatUtils.Token[] { tokenMock };
        
        //Act Statement(s)
        String result = DurationFormatUtils.format(tokenArray, 0L, 2L, 0L, 0L, 0L, 0L, 0L, true);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("2")));
    }

    //BaseRock generated method id: ${formatWhenSecondsEquals0AndNotInOptionalAndNotPadWithZerosAndNotInOptional}, hash: 1077FE88C236B9522C78EAD511A6C0AC
    @Test()
    void formatWhenSecondsEquals0AndNotInOptionalAndNotPadWithZerosAndNotInOptional() {
        /* Branches:
         * (for-each(tokens)) : true
         * (optionalIndex != token.optionalIndex) : true
         * (optionalIndex > -1) : false
         * (isLiteral) : false
         * (value.equals(y)) : false
         * (value.equals(M)) : false
         * (value.equals(d)) : false
         * (value.equals(H)) : false
         * (value.equals(m)) : false
         * (value.equals(s)) : true
         * (seconds == 0) : true
         * (!inOptional) : true
         * (padWithZeros) : false  #  inside paddedValue method
         * (inOptional) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        DurationFormatUtils.Token[] tokenArray = new DurationFormatUtils.Token[] { tokenMock };
        
        //Act Statement(s)
        String result = DurationFormatUtils.format(tokenArray, 0L, 0L, 0L, 0L, 0L, 0L, 0L, false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("0")));
    }

    //BaseRock generated method id: ${formatWhenPadWithZerosAndInOptionalAndNotIsLiteralAndNotFirstOptionalNonLiteralAndNotLastOutputZero}, hash: D834755F1BC128BC0C7536710B657DC0
    @Test()
    void formatWhenPadWithZerosAndInOptionalAndNotIsLiteralAndNotFirstOptionalNonLiteralAndNotLastOutputZero() {
        /* Branches:
         * (for-each(tokens)) : true
         * (optionalIndex != token.optionalIndex) : true
         * (optionalIndex > -1) : true
         * (isLiteral) : false
         * (value.equals(y)) : false
         * (value.equals(M)) : false
         * (value.equals(d)) : true
         * (days == 0) : false
         * (!inOptional) : false
         * (!lastOutputZero) : true
         * (padWithZeros) : true  #  inside paddedValue method
         * (inOptional) : true
         * (!isLiteral) : true
         * (!firstOptionalNonLiteral) : true
         * (lastOutputZero) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        DurationFormatUtils.Token[] tokenArray = new DurationFormatUtils.Token[] { tokenMock };
        
        //Act Statement(s)
        String result = DurationFormatUtils.format(tokenArray, 0L, 0L, 2L, 0L, 0L, 0L, 0L, true);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("2")));
    }

    //BaseRock generated method id: ${formatWhenNotLastOutputZeroAndPadWithZerosAndInOptionalAndNotIsLiteralAndNotFirstOptionalNonLiteralAndNotLastOutputZero}, hash: DACC8EDC1D7E4703E85880830A044BB7
    @Test()
    void formatWhenNotLastOutputZeroAndPadWithZerosAndInOptionalAndNotIsLiteralAndNotFirstOptionalNonLiteralAndNotLastOutputZero() {
        /* Branches:
         * (for-each(tokens)) : true
         * (optionalIndex != token.optionalIndex) : true
         * (optionalIndex > -1) : true
         * (isLiteral) : false
         * (value.equals(y)) : false
         * (value.equals(M)) : false
         * (value.equals(d)) : false
         * (value.equals(H)) : true
         * (hours == 0) : false
         * (!inOptional) : false
         * (!lastOutputZero) : true
         * (padWithZeros) : true  #  inside paddedValue method
         * (inOptional) : true
         * (!isLiteral) : true
         * (!firstOptionalNonLiteral) : true
         * (lastOutputZero) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        DurationFormatUtils.Token[] tokenArray = new DurationFormatUtils.Token[] { tokenMock };
        
        //Act Statement(s)
        String result = DurationFormatUtils.format(tokenArray, 0L, 0L, 0L, 2L, 0L, 0L, 0L, true);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("2")));
    }

    //BaseRock generated method id: ${formatWhenInOptionalAndNotLastOutputZeroAndPadWithZerosAndInOptionalAndNotIsLiteralAndNotFirstOptionalNonLiteralAndNotL}, hash: 0BE6DF0DEE8C49D424FF67611B7F6D0D
    @Test()
    void formatWhenInOptionalAndNotLastOutputZeroAndPadWithZerosAndInOptionalAndNotIsLiteralAndNotFirstOptionalNonLiteralAndNotL() {
        /* Branches:
         * (for-each(tokens)) : true
         * (optionalIndex != token.optionalIndex) : true
         * (optionalIndex > -1) : true
         * (isLiteral) : false
         * (value.equals(y)) : false
         * (value.equals(M)) : false
         * (value.equals(d)) : false
         * (value.equals(H)) : false
         * (value.equals(m)) : true
         * (minutes == 0) : false
         * (!inOptional) : false
         * (!lastOutputZero) : true
         * (padWithZeros) : true  #  inside paddedValue method
         * (inOptional) : true
         * (!isLiteral) : true
         * (!firstOptionalNonLiteral) : true
         * (lastOutputZero) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        DurationFormatUtils.Token[] tokenArray = new DurationFormatUtils.Token[] { tokenMock };
        
        //Act Statement(s)
        String result = DurationFormatUtils.format(tokenArray, 0L, 0L, 0L, 0L, 2L, 0L, 0L, true);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("2")));
    }

    //BaseRock generated method id: ${formatWhenPadWithZerosAndInOptionalAndNotIsLiteralAndNotFirstOptionalNonLiteralAndNotLastOutputZero2}, hash: 9BDF3F38481FA04E6E5B16F2505A2DAC
    @Test()
    void formatWhenPadWithZerosAndInOptionalAndNotIsLiteralAndNotFirstOptionalNonLiteralAndNotLastOutputZero2() {
        /* Branches:
         * (for-each(tokens)) : true
         * (optionalIndex != token.optionalIndex) : true
         * (optionalIndex > -1) : true
         * (isLiteral) : false
         * (value.equals(y)) : false
         * (value.equals(M)) : false
         * (value.equals(d)) : false
         * (value.equals(H)) : false
         * (value.equals(m)) : false
         * (value.equals(s)) : true
         * (seconds == 0) : false
         * (!inOptional) : false
         * (!lastOutputZero) : true
         * (padWithZeros) : true  #  inside paddedValue method
         * (inOptional) : true
         * (!isLiteral) : true
         * (!firstOptionalNonLiteral) : true
         * (lastOutputZero) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        DurationFormatUtils.Token[] tokenArray = new DurationFormatUtils.Token[] { tokenMock };
        
        //Act Statement(s)
        String result = DurationFormatUtils.format(tokenArray, 0L, 0L, 0L, 0L, 0L, 2L, 0L, true);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("2")));
    }

    //BaseRock generated method id: ${formatWhenMillisecondsEquals0AndInOptionalAndLastOutputZeroAndInOptionalAndNotIsLiteralAndNotFirstOptionalNonLiteralAnd}, hash: 9A6062E899380139C8B81BCD07B02EA3
    @Test()
    void formatWhenMillisecondsEquals0AndInOptionalAndLastOutputZeroAndInOptionalAndNotIsLiteralAndNotFirstOptionalNonLiteralAnd() {
        /* Branches:
         * (for-each(tokens)) : true
         * (optionalIndex != token.optionalIndex) : true
         * (optionalIndex > -1) : true
         * (isLiteral) : false
         * (value.equals(y)) : false
         * (value.equals(M)) : false
         * (value.equals(d)) : false
         * (value.equals(H)) : false
         * (value.equals(m)) : false
         * (value.equals(s)) : false
         * (value.equals(S)) : true
         * (milliseconds == 0) : true
         * (!inOptional) : false
         * (!lastOutputZero) : false
         * (inOptional) : true
         * (!isLiteral) : true
         * (!firstOptionalNonLiteral) : true
         * (lastOutputZero) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        DurationFormatUtils.Token[] tokenArray = new DurationFormatUtils.Token[] { tokenMock };
        
        //Act Statement(s)
        String result = DurationFormatUtils.format(tokenArray, 0L, 0L, 0L, 0L, 0L, 0L, 0L, false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${formatWhenNotLastOutputSecondsAndPadWithZerosAndInOptionalAndNotIsLiteralAndNotFirstOptionalNonLiteralAndNotLastOutputZ}, hash: 212A5A74F4DA6A5927AE3BC1BCDCCBBF
    @Test()
    void formatWhenNotLastOutputSecondsAndPadWithZerosAndInOptionalAndNotIsLiteralAndNotFirstOptionalNonLiteralAndNotLastOutputZ() {
        /* Branches:
         * (for-each(tokens)) : true
         * (optionalIndex != token.optionalIndex) : true
         * (optionalIndex > -1) : true
         * (isLiteral) : false
         * (value.equals(y)) : false
         * (value.equals(M)) : false
         * (value.equals(d)) : false
         * (value.equals(H)) : false
         * (value.equals(m)) : false
         * (value.equals(s)) : false
         * (value.equals(S)) : true
         * (milliseconds == 0) : false
         * (!inOptional) : false
         * (!lastOutputZero) : true
         * (lastOutputSeconds) : false
         * (padWithZeros) : true  #  inside paddedValue method
         * (inOptional) : true
         * (!isLiteral) : true
         * (!firstOptionalNonLiteral) : true
         * (lastOutputZero) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        DurationFormatUtils.Token[] tokenArray = new DurationFormatUtils.Token[] { tokenMock };
        
        //Act Statement(s)
        String result = DurationFormatUtils.format(tokenArray, 0L, 0L, 0L, 0L, 0L, 0L, 2L, true);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("2")));
    }

    //BaseRock generated method id: ${formatWhenNotLastOutputSecondsAndNotPadWithZerosAndInOptionalAndNotIsLiteralAndNotFirstOptionalNonLiteralAndNotLastOutp}, hash: 6ED726979F502AA0E057F94C601AE555
    @Test()
    void formatWhenNotLastOutputSecondsAndNotPadWithZerosAndInOptionalAndNotIsLiteralAndNotFirstOptionalNonLiteralAndNotLastOutp() {
        /* Branches:
         * (for-each(tokens)) : true
         * (optionalIndex != token.optionalIndex) : true
         * (optionalIndex > -1) : true
         * (isLiteral) : false
         * (value.equals(y)) : false
         * (value.equals(M)) : false
         * (value.equals(d)) : false
         * (value.equals(H)) : false
         * (value.equals(m)) : false
         * (value.equals(s)) : false
         * (value.equals(S)) : true
         * (milliseconds == 0) : false
         * (!inOptional) : false
         * (!lastOutputZero) : true
         * (lastOutputSeconds) : false
         * (padWithZeros) : false  #  inside paddedValue method
         * (inOptional) : true
         * (!isLiteral) : true
         * (!firstOptionalNonLiteral) : true
         * (lastOutputZero) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        DurationFormatUtils.Token[] tokenArray = new DurationFormatUtils.Token[] { tokenMock };
        
        //Act Statement(s)
        String result = DurationFormatUtils.format(tokenArray, 0L, 0L, 0L, 0L, 0L, 0L, 2L, false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("2")));
    }

    //BaseRock generated method id: ${formatDurationTest}, hash: 279CCD1014612BBFCF0FE59ED79C5BE3
    @Test()
    void formatDurationTest() {
        //Arrange Statement(s)
        try (MockedStatic<DurationFormatUtils> durationFormatUtils = mockStatic(DurationFormatUtils.class, CALLS_REAL_METHODS)) {
            durationFormatUtils.when(() -> DurationFormatUtils.formatDuration(0L, "format1", true)).thenReturn("return_of_formatDuration1");
            //Act Statement(s)
            String result = DurationFormatUtils.formatDuration(0L, "format1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_formatDuration1"));
                durationFormatUtils.verify(() -> DurationFormatUtils.formatDuration(0L, "format1", true), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${formatDuration1WhenTokenContainsTokenWithValueTokensS}, hash: 7CB41DF9D35E1C25060066779782CEB6
    @Test()
    void formatDuration1WhenTokenContainsTokenWithValueTokensS() {
        /* Branches:
         * (Token.containsTokenWithValue(tokens, d)) : true
         * (Token.containsTokenWithValue(tokens, H)) : true
         * (Token.containsTokenWithValue(tokens, m)) : true
         * (Token.containsTokenWithValue(tokens, s)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<DurationFormatUtils> durationFormatUtils = mockStatic(DurationFormatUtils.class, CALLS_REAL_METHODS)) {
            DurationFormatUtils.Token[] tokenArray = new DurationFormatUtils.Token[] {};
            durationFormatUtils.when(() -> DurationFormatUtils.lexx("format1")).thenReturn(tokenArray);
            durationFormatUtils.when(() -> DurationFormatUtils.format(tokenArray, 0L, 0L, 0L, 0L, 0L, 0L, 0L, false)).thenReturn("return_of_format1");
            //Act Statement(s)
            String result = DurationFormatUtils.formatDuration(0L, "format1", false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_format1"));
                durationFormatUtils.verify(() -> DurationFormatUtils.lexx("format1"), atLeast(1));
                durationFormatUtils.verify(() -> DurationFormatUtils.format(tokenArray, 0L, 0L, 0L, 0L, 0L, 0L, 0L, false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${formatDurationHMSTest}, hash: 1623368DC11ECCD71BC97895CAD617CF
    @Test()
    void formatDurationHMSTest() {
        //Arrange Statement(s)
        try (MockedStatic<DurationFormatUtils> durationFormatUtils = mockStatic(DurationFormatUtils.class, CALLS_REAL_METHODS)) {
            durationFormatUtils.when(() -> DurationFormatUtils.formatDuration(0L, "HH:mm:ss.SSS")).thenReturn("return_of_formatDuration1");
            //Act Statement(s)
            String result = DurationFormatUtils.formatDurationHMS(0L);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_formatDuration1"));
                durationFormatUtils.verify(() -> DurationFormatUtils.formatDuration(0L, "HH:mm:ss.SSS"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${formatDurationISOTest}, hash: FD63167DC48889C66BF3FE49D4493A1E
    @Test()
    void formatDurationISOTest() {
        //Arrange Statement(s)
        try (MockedStatic<DurationFormatUtils> durationFormatUtils = mockStatic(DurationFormatUtils.class, CALLS_REAL_METHODS)) {
            durationFormatUtils.when(() -> DurationFormatUtils.formatDuration(0L, "'P'yyyy'Y'M'M'd'DT'H'H'm'M's.SSS'S'", false)).thenReturn("return_of_formatDuration1");
            //Act Statement(s)
            String result = DurationFormatUtils.formatDurationISO(0L);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_formatDuration1"));
                durationFormatUtils.verify(() -> DurationFormatUtils.formatDuration(0L, "'P'yyyy'Y'M'M'd'DT'H'H'm'M's.SSS'S'", false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${formatDurationWordsWhenTmpLengthNotEqualsDurationLength}, hash: 8E6174AFE05661F13D35B0104598F8EF
    @Test()
    void formatDurationWordsWhenTmpLengthNotEqualsDurationLength() {
        /* Branches:
         * (suppressLeadingZeroElements) : true
         * (tmp.length() != duration.length()) : true
         * (tmp.length() != duration.length()) : true
         * (!duration.isEmpty()) : true
         * (suppressTrailingZeroElements) : true
         * (tmp.length() != duration.length()) : true
         * (tmp.length() != duration.length()) : true
         * (tmp.length() != duration.length()) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<DurationFormatUtils> durationFormatUtils = mockStatic(DurationFormatUtils.class, CALLS_REAL_METHODS)) {
            durationFormatUtils.when(() -> DurationFormatUtils.formatDuration(0L, "d' days 'H' hours 'm' minutes 's' seconds'")).thenReturn("return_of_formatDuration1");
            //Act Statement(s)
            String result = DurationFormatUtils.formatDurationWords(0L, false, false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                durationFormatUtils.verify(() -> DurationFormatUtils.formatDuration(0L, "d' days 'H' hours 'm' minutes 's' seconds'"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${formatPeriodTest}, hash: 7C94C0BCEEB01BE60383702407CE1F00
    @Test()
    void formatPeriodTest() {
        //Arrange Statement(s)
        try (MockedStatic<DurationFormatUtils> durationFormatUtils = mockStatic(DurationFormatUtils.class, CALLS_REAL_METHODS)) {
            durationFormatUtils.when(() -> DurationFormatUtils.formatPeriod(eq(0L), eq(0L), eq("format1"), eq(true), (TimeZone) any())).thenReturn("return_of_formatPeriod1");
            //Act Statement(s)
            String result = DurationFormatUtils.formatPeriod(0L, 0L, "format1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_formatPeriod1"));
                durationFormatUtils.verify(() -> DurationFormatUtils.formatPeriod(eq(0L), eq(0L), eq("format1"), eq(true), (TimeZone) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${formatPeriod1WhenStartMillisLessThanOrEqualsToEndMillisThrowsIllegalArgumentException}, hash: 9804A03715137DA161B22290B7CF0001
    @Test()
    void formatPeriod1WhenStartMillisLessThanOrEqualsToEndMillisThrowsIllegalArgumentException() {
        /* Branches:
         * (startMillis <= endMillis) : true
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            DurationFormatUtils.formatPeriod(1L, 1L, "format1", false, timeZone);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${formatPeriod1WhenStartMillisGreaterThanEndMillisThrowsIllegalArgumentException}, hash: 3FCB4DE21307C385F6EA4285164C736D
    @Test()
    void formatPeriod1WhenStartMillisGreaterThanEndMillisThrowsIllegalArgumentException() {
        /* Branches:
         * (startMillis <= endMillis) : false
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            DurationFormatUtils.formatPeriod(2L, 1L, "format1", false, timeZone);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${formatPeriodISOTest}, hash: 74F8F27CF35215E29D13B52715363858
    @Test()
    void formatPeriodISOTest() {
        //Arrange Statement(s)
        try (MockedStatic<DurationFormatUtils> durationFormatUtils = mockStatic(DurationFormatUtils.class, CALLS_REAL_METHODS)) {
            durationFormatUtils.when(() -> DurationFormatUtils.formatPeriod(eq(0L), eq(0L), eq("'P'yyyy'Y'M'M'd'DT'H'H'm'M's.SSS'S'"), eq(false), (TimeZone) any())).thenReturn("return_of_formatPeriod1");
            //Act Statement(s)
            String result = DurationFormatUtils.formatPeriodISO(0L, 0L);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_formatPeriod1"));
                durationFormatUtils.verify(() -> DurationFormatUtils.formatPeriod(eq(0L), eq(0L), eq("'P'yyyy'Y'M'M'd'DT'H'H'm'M's.SSS'S'"), eq(false), (TimeZone) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${lexxWhenNotInOptionalThrowsIllegalArgumentException}, hash: 8E3B2A5636C7558D71E8F7899E6C6269
    @Test()
    void lexxWhenNotInOptionalThrowsIllegalArgumentException() {
        /* Branches:
         * (i < format.length()) : true
         * (inLiteral) : false
         * (switch(ch) = 'd') : true
         * (!inOptional) : true
         */
         //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("Attempting to close unopened optional block at index: 0");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            DurationFormatUtils.lexx("]");
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${lexxWhenInLiteralThrowsIllegalArgumentException}, hash: DB52E610F75AE2944766738BEA955156
    @Test()
    void lexxWhenInLiteralThrowsIllegalArgumentException() {
        /* Branches:
         * (i < format.length()) : true
         * (inLiteral) : false
         * (switch(ch) = '[') : true
         * (inLiteral) : false
         * (value != null) : false
         * (inLiteral) : true
         */
         //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("Unmatched quote in format: '");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            DurationFormatUtils.lexx("'");
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${lexxWhenNotInLiteralAndNotInOptional}, hash: 1AF7F13CF72105C8758620B5BFA65444
    @Test()
    void lexxWhenNotInLiteralAndNotInOptional() {
        /* Branches:
         * (i < format.length()) : true
         * (inLiteral) : false
         * (switch(ch) = 'H') : true
         * (value != null) : true
         * (previous != null) : false
         * (inLiteral) : false
         * (inOptional) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        DurationFormatUtils.Token[] result = DurationFormatUtils.lexx("d");
        DurationFormatUtils.Token durationFormatUtilsToken = new DurationFormatUtils.Token("d", false, -1);
        DurationFormatUtils.Token[] tokenResultArray = new DurationFormatUtils.Token[] { durationFormatUtilsToken };
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(tokenResultArray)));
    }

    //BaseRock generated method id: ${lexxWhenInOptionalThrowsIllegalArgumentException}, hash: 463CD44FFCBAB456A419065B791AAE41
    @Test()
    void lexxWhenInOptionalThrowsIllegalArgumentException() {
        /* Branches:
         * (i < format.length()) : true
         * (inLiteral) : false
         * (switch(ch) = 'M') : true
         * (inOptional) : false
         * (value != null) : false
         * (inLiteral) : false
         * (inOptional) : true
         */
         //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("Unmatched optional in format: [");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            DurationFormatUtils.lexx("[");
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${lexxWhenPreviousIsNullAndNotInLiteralAndNotInOptional}, hash: FA0A970AEA5CBF1E4DFC5109D5810F54
    @Test()
    void lexxWhenPreviousIsNullAndNotInLiteralAndNotInOptional() {
        /* Branches:
         * (i < format.length()) : true
         * (inLiteral) : false
         * (switch(ch) = 'S') : true
         * (value != null) : true
         * (previous != null) : false
         * (inLiteral) : false
         * (inOptional) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        DurationFormatUtils.Token[] result = DurationFormatUtils.lexx("y");
        DurationFormatUtils.Token durationFormatUtilsToken = new DurationFormatUtils.Token("y", false, -1);
        DurationFormatUtils.Token[] tokenResultArray = new DurationFormatUtils.Token[] { durationFormatUtilsToken };
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(tokenResultArray)));
    }

    //BaseRock generated method id: ${lexxWhenValueIsNotNullAndPreviousIsNullAndNotInLiteralAndNotInOptional}, hash: C272B34059139FC4C75B093ACBA4AB46
    @Test()
    void lexxWhenValueIsNotNullAndPreviousIsNullAndNotInLiteralAndNotInOptional() {
        /* Branches:
         * (i < format.length()) : true
         * (inLiteral) : false
         * (switch(ch) = '\'') : true
         * (value != null) : true
         * (previous != null) : false
         * (inLiteral) : false
         * (inOptional) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        DurationFormatUtils.Token[] result = DurationFormatUtils.lexx("M");
        DurationFormatUtils.Token durationFormatUtilsToken = new DurationFormatUtils.Token("M", false, -1);
        DurationFormatUtils.Token[] tokenResultArray = new DurationFormatUtils.Token[] { durationFormatUtilsToken };
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(tokenResultArray)));
    }

    //BaseRock generated method id: ${lexxWhenSwitchChCase___AndValueIsNotNullAndPreviousIsNullAndNotInLiteralAndNotInOptional}, hash: 2E16830E23220047422D92980EC98098
    @Test()
    void lexxWhenSwitchChCase___AndValueIsNotNullAndPreviousIsNullAndNotInLiteralAndNotInOptional() {
        /* Branches:
         * (i < format.length()) : true
         * (inLiteral) : false
         * (switch(ch) = ']') : true
         * (value != null) : true
         * (previous != null) : false
         * (inLiteral) : false
         * (inOptional) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        DurationFormatUtils.Token[] result = DurationFormatUtils.lexx("H");
        DurationFormatUtils.Token durationFormatUtilsToken = new DurationFormatUtils.Token("H", false, -1);
        DurationFormatUtils.Token[] tokenResultArray = new DurationFormatUtils.Token[] { durationFormatUtilsToken };
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(tokenResultArray)));
    }

    //BaseRock generated method id: ${lexxWhenSwitchChCase_m_AndValueIsNotNullAndPreviousIsNullAndNotInLiteralAndNotInOptional}, hash: AF156E169522B839BACB076F91DACE4E
    @Test()
    void lexxWhenSwitchChCase_m_AndValueIsNotNullAndPreviousIsNullAndNotInLiteralAndNotInOptional() {
        /* Branches:
         * (i < format.length()) : true
         * (inLiteral) : false
         * (switch(ch) = 'm') : true
         * (value != null) : true
         * (previous != null) : false
         * (inLiteral) : false
         * (inOptional) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        DurationFormatUtils.Token[] result = DurationFormatUtils.lexx("m");
        DurationFormatUtils.Token durationFormatUtilsToken = new DurationFormatUtils.Token("m", false, -1);
        DurationFormatUtils.Token[] tokenResultArray = new DurationFormatUtils.Token[] { durationFormatUtilsToken };
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(tokenResultArray)));
    }

    //BaseRock generated method id: ${lexxWhenSwitchChCase_s_AndValueIsNotNullAndPreviousIsNullAndNotInLiteralAndNotInOptional}, hash: DD7F32159C5DA64999334251E36B757B
    @Test()
    void lexxWhenSwitchChCase_s_AndValueIsNotNullAndPreviousIsNullAndNotInLiteralAndNotInOptional() {
        /* Branches:
         * (i < format.length()) : true
         * (inLiteral) : false
         * (switch(ch) = 's') : true
         * (value != null) : true
         * (previous != null) : false
         * (inLiteral) : false
         * (inOptional) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        DurationFormatUtils.Token[] result = DurationFormatUtils.lexx("s");
        DurationFormatUtils.Token durationFormatUtilsToken = new DurationFormatUtils.Token("s", false, -1);
        DurationFormatUtils.Token[] tokenResultArray = new DurationFormatUtils.Token[] { durationFormatUtilsToken };
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(tokenResultArray)));
    }

    //BaseRock generated method id: ${lexxWhenSwitchChCase_y_AndValueIsNotNullAndPreviousIsNullAndNotInLiteralAndNotInOptional}, hash: CDBB66FF565540FB9F41B667203101C7
    @Test()
    void lexxWhenSwitchChCase_y_AndValueIsNotNullAndPreviousIsNullAndNotInLiteralAndNotInOptional() {
        /* Branches:
         * (i < format.length()) : true
         * (inLiteral) : false
         * (switch(ch) = 'y') : true
         * (value != null) : true
         * (previous != null) : false
         * (inLiteral) : false
         * (inOptional) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        DurationFormatUtils.Token[] result = DurationFormatUtils.lexx("S");
        DurationFormatUtils.Token durationFormatUtilsToken = new DurationFormatUtils.Token("S", false, -1);
        DurationFormatUtils.Token[] tokenResultArray = new DurationFormatUtils.Token[] { durationFormatUtilsToken };
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(tokenResultArray)));
    }

    //BaseRock generated method id: ${lexxWhenValueIsNullAndNotInLiteralAndNotInOptional}, hash: 5083B1E24A2CB55A565F20B1063A9154
    @Test()
    void lexxWhenValueIsNullAndNotInLiteralAndNotInOptional() {
        /* Branches:
         * (i < format.length()) : true
         * (inLiteral) : false
         * (switch(ch) = default) : true
         * (buffer == null) : true
         * (value != null) : false
         * (inLiteral) : false
         * (inOptional) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        DurationFormatUtils.Token[] result = DurationFormatUtils.lexx("A");
        StringBuilder stringBuilder = new StringBuilder();
        DurationFormatUtils.Token durationFormatUtilsToken = new DurationFormatUtils.Token(stringBuilder, false, -1);
        DurationFormatUtils.Token[] tokenResultArray = new DurationFormatUtils.Token[] { durationFormatUtilsToken };
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(tokenResultArray)));
    }
}
