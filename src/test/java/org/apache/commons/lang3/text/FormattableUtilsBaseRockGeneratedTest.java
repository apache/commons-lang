package org.apache.commons.lang3.text;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.util.Formattable;
import java.util.Formatter;
import org.mockito.MockedStatic;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class FormattableUtilsBaseRockGeneratedTest {

    //BaseRock generated method id: ${appendTest}, hash: 6E660DCC837C23EB3F5B96AB1C520B81
    @Test()
    void appendTest() {
        //Arrange Statement(s)
        try (MockedStatic<FormattableUtils> formattableUtils = mockStatic(FormattableUtils.class, CALLS_REAL_METHODS)) {
            Formatter formatter = new Formatter();
            Formatter formatter2 = new Formatter();
            formattableUtils.when(() -> FormattableUtils.append("seq1", formatter2, 0, 0, 0, ' ', (CharSequence) null)).thenReturn(formatter);
            //Act Statement(s)
            Formatter result = FormattableUtils.append("seq1", formatter2, 0, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(formatter));
                formattableUtils.verify(() -> FormattableUtils.append("seq1", formatter2, 0, 0, 0, ' ', (CharSequence) null), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${append1Test}, hash: 176863796123E2E489D5BB28D37B6549
    @Test()
    void append1Test() {
        //Arrange Statement(s)
        try (MockedStatic<FormattableUtils> formattableUtils = mockStatic(FormattableUtils.class, CALLS_REAL_METHODS)) {
            Formatter formatter = new Formatter();
            Formatter formatter2 = new Formatter();
            formattableUtils.when(() -> FormattableUtils.append("seq1", formatter2, 0, 0, 0, 'A', (CharSequence) null)).thenReturn(formatter);
            //Act Statement(s)
            Formatter result = FormattableUtils.append((CharSequence) "seq1", formatter2, 0, 0, 0, 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(formatter));
                formattableUtils.verify(() -> FormattableUtils.append("seq1", formatter2, 0, 0, 0, 'A', (CharSequence) null), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${append2WhenPrecisionLessThanSeqLengthAndFlagsAndFormattableFlagsLEFT_JUSTIFYEqualsFormattableFlagsLEFT_JUSTIFYAndILessT}, hash: ACDB6B9BBA9E19B21127786EADB601EE
    @Test()
    void append2WhenPrecisionLessThanSeqLengthAndFlagsAndFormattableFlagsLEFT_JUSTIFYEqualsFormattableFlagsLEFT_JUSTIFYAndILessT() {
        /* Branches:
         * (ellipsis == null) : false
         * (precision < 0) : false
         * (ellipsis.length() <= precision) : true
         * (precision >= 0) : true
         * (precision < seq.length()) : true
         * ((flags & FormattableFlags.LEFT_JUSTIFY) == FormattableFlags.LEFT_JUSTIFY) : true
         * (i < width) : true
         * (leftJustify) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Formatter formatter = new Formatter();
        
        //Act Statement(s)
        Formatter result = FormattableUtils.append("seq1", formatter, 0, 1, 1, 'A', "ellipsis1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(formatter)));
    }

    //BaseRock generated method id: ${append2WhenILessThanWidthAndNotLeftJustify}, hash: 829D9BE1DC5A9315649665B72780B4AF
    @Test()
    void append2WhenILessThanWidthAndNotLeftJustify() {
        /* Branches:
         * (ellipsis == null) : false
         * (precision < 0) : false
         * (ellipsis.length() <= precision) : true
         * (precision >= 0) : true
         * (precision < seq.length()) : true
         * ((flags & FormattableFlags.LEFT_JUSTIFY) == FormattableFlags.LEFT_JUSTIFY) : false
         * (i < width) : true
         * (leftJustify) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Formatter formatter = new Formatter();
        
        //Act Statement(s)
        Formatter result = FormattableUtils.append("seq1", formatter, 0, 1, 0, 'A', "ellipsis1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(formatter)));
    }

    //BaseRock generated method id: ${append3Test}, hash: 18DEB0443F6EFE3C0A6E8856B20B7F65
    @Test()
    void append3Test() {
        //Arrange Statement(s)
        try (MockedStatic<FormattableUtils> formattableUtils = mockStatic(FormattableUtils.class, CALLS_REAL_METHODS)) {
            Formatter formatter = new Formatter();
            Formatter formatter2 = new Formatter();
            formattableUtils.when(() -> FormattableUtils.append("seq1", formatter2, 0, 0, 0, ' ', "ellipsis1")).thenReturn(formatter);
            //Act Statement(s)
            Formatter result = FormattableUtils.append((CharSequence) "seq1", formatter2, 0, 0, 0, (CharSequence) "ellipsis1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(formatter));
                formattableUtils.verify(() -> FormattableUtils.append("seq1", formatter2, 0, 0, 0, ' ', "ellipsis1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toStringTest}, hash: DDB5E3D445CA60E755E2F601ABA571B2
    @Test()
    void toStringTest() {
        //Arrange Statement(s)
        Formattable formattableMock = mock(Formattable.class, "formattable");
        
        //Act Statement(s)
        String result = FormattableUtils.toString(formattableMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }
}
