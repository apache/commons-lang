package org.apache.commons.lang3.text.translate;

import org.apache.commons.lang3.text.translate.NumericEntityEscaper;

import static org.junit.jupiter.api.Assertions.assertAll;
import org.junit.jupiter.api.Test;
import static org.hamcrest.Matchers.equalTo;
import org.junit.jupiter.api.Timeout;
import static org.hamcrest.Matchers.notNullValue;
import static org.mockito.Mockito.atLeast;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import java.io.StringWriter;
import org.mockito.MockedStatic;
import java.io.IOException;
import java.io.Writer;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class NumericEntityEscaperBaseRockGeneratedTest {

    //BaseRock generated method id: ${aboveTest}, hash: 9530407A1EA55AAE28E030B039D0306F
    @Test()
    void aboveTest() {
        //Arrange Statement(s)
        try (MockedStatic<NumericEntityEscaper> numericEntityEscaper = mockStatic(NumericEntityEscaper.class, CALLS_REAL_METHODS)) {
            NumericEntityEscaper numericEntityEscaper2 = new NumericEntityEscaper();
            numericEntityEscaper.when(() -> NumericEntityEscaper.outsideOf(0, 0)).thenReturn(numericEntityEscaper2);
            //Act Statement(s)
            NumericEntityEscaper result = NumericEntityEscaper.above(0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(numericEntityEscaper2));
                numericEntityEscaper.verify(() -> NumericEntityEscaper.outsideOf(0, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${belowTest}, hash: 2C22341855B76F80A49DB1F6FA400AFE
    @Test()
    void belowTest() {
        //Arrange Statement(s)
        try (MockedStatic<NumericEntityEscaper> numericEntityEscaper = mockStatic(NumericEntityEscaper.class, CALLS_REAL_METHODS)) {
            NumericEntityEscaper numericEntityEscaper2 = new NumericEntityEscaper();
            numericEntityEscaper.when(() -> NumericEntityEscaper.outsideOf(0, 2147483647)).thenReturn(numericEntityEscaper2);
            //Act Statement(s)
            NumericEntityEscaper result = NumericEntityEscaper.below(0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(numericEntityEscaper2));
                numericEntityEscaper.verify(() -> NumericEntityEscaper.outsideOf(0, 2147483647), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${betweenTest}, hash: BD26FE7F0DADF4FB3BF82D3069E88539
    @Test()
    void betweenTest() {
        //Act Statement(s)
        NumericEntityEscaper result = NumericEntityEscaper.between(0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${outsideOfTest}, hash: 060525F938981601BA8BE0CC6419DC2A
    @Test()
    void outsideOfTest() {
        //Act Statement(s)
        NumericEntityEscaper result = NumericEntityEscaper.outsideOf(0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${translate3WhenCodePointLessThanBelow}, hash: 9B709969F071FC7F7D12834AD9EBB7C2
    @Test()
    void translate3WhenCodePointLessThanBelow() throws IOException {
        /* Branches:
         * (between) : true
         * (codePoint < below) : true
         */
        //Arrange Statement(s)
        NumericEntityEscaper target = new NumericEntityEscaper();
        Writer writer = new StringWriter();
        //Act Statement(s)
        boolean result = target.translate(-1, writer);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${translate3WhenCodePointNotGreaterThanAbove}, hash: 94C058895F07FE931D3C8AA31A1B73CD
    @Test()
    void translate3WhenCodePointNotGreaterThanAbove() throws IOException {
        /* Branches:
         * (between) : true
         * (codePoint < below) : false
         * (codePoint > above) : false
         */
        //Arrange Statement(s)
        NumericEntityEscaper target = new NumericEntityEscaper();
        Writer writer = new StringWriter();
        //Act Statement(s)
        boolean result = target.translate(0, writer);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }
}
