package org.apache.commons.lang3.text.translate;

import org.apache.commons.lang3.text.translate.UnicodeEscaper;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;
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
import static org.mockito.Mockito.verify;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class UnicodeEscaperBaseRockGeneratedTest {

    //BaseRock generated method id: ${aboveTest}, hash: 014C64C453C4C4417A75AF20BF2319CC
    @Test
    void aboveTest() {
        //Arrange Statement(s)
        try (MockedStatic<UnicodeEscaper> unicodeEscaper = mockStatic(UnicodeEscaper.class, CALLS_REAL_METHODS)) {
            UnicodeEscaper unicodeEscaper2 = new UnicodeEscaper();
            unicodeEscaper.when(() -> UnicodeEscaper.outsideOf(0, 0)).thenReturn(unicodeEscaper2);
            //Act Statement(s)
            UnicodeEscaper result = UnicodeEscaper.above(0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(unicodeEscaper2));
                unicodeEscaper.verify(() -> UnicodeEscaper.outsideOf(0, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${belowTest}, hash: 8FC2E11020A6AB77B2B053B66CBCA565
    @Test
    void belowTest() {
        //Arrange Statement(s)
        try (MockedStatic<UnicodeEscaper> unicodeEscaper = mockStatic(UnicodeEscaper.class, CALLS_REAL_METHODS)) {
            UnicodeEscaper unicodeEscaper2 = new UnicodeEscaper();
            unicodeEscaper.when(() -> UnicodeEscaper.outsideOf(0, 2147483647)).thenReturn(unicodeEscaper2);
            //Act Statement(s)
            UnicodeEscaper result = UnicodeEscaper.below(0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(unicodeEscaper2));
                unicodeEscaper.verify(() -> UnicodeEscaper.outsideOf(0, 2147483647), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${betweenTest}, hash: 0BF3EF62FE70469F64B63A1904A2D103
    @Test
    void betweenTest() {
        //Act Statement(s)
        UnicodeEscaper result = UnicodeEscaper.between(0, 0);
        //Assert statement(s)
        //TODO: Please implement equals method in UnicodeEscaper for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${outsideOfTest}, hash: FE1F6B4C1C4834449D0D5B7E2E9B80C0
    @Test
    void outsideOfTest() {
        //Act Statement(s)
        UnicodeEscaper result = UnicodeEscaper.outsideOf(0, 0);
        //Assert statement(s)
        //TODO: Please implement equals method in UnicodeEscaper for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${toUtf16EscapeTest}, hash: D3EED74C0C89D52B66D0B394D27AFB2C
    @Test
    void toUtf16EscapeTest() {
        //Arrange Statement(s)
        UnicodeEscaper target = new UnicodeEscaper(0, 0, false);
        //Act Statement(s)
        String result = target.toUtf16Escape(0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("\\uA")));
    }

    //BaseRock generated method id: ${translate3WhenCodePointGreaterThanAbove}, hash: 4AE7ECD660467DFE7D4CE2C250ADA0CB
    @Test
    void translate3WhenCodePointGreaterThanAbove() throws IOException {
        /* Branches:
         * (between) : true
         * (codePoint < below) : false
         * (codePoint > above) : true
         */
        //Arrange Statement(s)
        UnicodeEscaper target = new UnicodeEscaper(1, 1, true);
        Writer writer = new StringWriter();
        //Act Statement(s)
        boolean result = target.translate(2, writer);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${translate3WhenCodePointNotGreaterThan65535}, hash: AB9697089B048ADCE2D5313044BD5764
    @Test
    void translate3WhenCodePointNotGreaterThan65535() throws IOException {
        /* Branches:
         * (between) : true
         * (codePoint < below) : false
         * (codePoint > above) : false
         * (codePoint > 0xffff) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        UnicodeEscaper target = new UnicodeEscaper(0, 65535, true);
        Writer writer = new StringWriter();
        //Act Statement(s)
        boolean result = target.translate(65535, writer);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${translate3WhenCodePointGreaterThanAboveAndCodePointGreaterThan65535}, hash: 1674E75FD3CB8D0B99559277E9DB780C
    @Test
    void translate3WhenCodePointGreaterThanAboveAndCodePointGreaterThan65535() throws IOException {
        /* Branches:
         * (between) : false
         * (codePoint >= below) : true
         * (codePoint <= above) : false
         * (codePoint > 0xffff) : true
         */
        //Arrange Statement(s)
        UnicodeEscaper target = spy(new UnicodeEscaper(65535, 65535, false));
        doReturn("A").when(target).toUtf16Escape(65536);
        Writer writer = new StringWriter();
        //Act Statement(s)
        boolean result = target.translate(65536, writer);
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.TRUE));
            verify(target).toUtf16Escape(65536);
        });
    }
}
