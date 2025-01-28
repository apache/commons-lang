package org.apache.commons.lang3;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.util.UUID;
import org.mockito.MockedStatic;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mockStatic;
import static org.hamcrest.Matchers.is;
import org.junit.jupiter.api.Disabled;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class ConversionBaseRockGeneratedTest {

    //BaseRock generated method id: ${binaryBeMsb0ToHexDigitTest}, hash: D007A5679BA28F62C6629F364B6FD84E
    @Test()
    void binaryBeMsb0ToHexDigitTest() {
        //Arrange Statement(s)
        try (MockedStatic<Conversion> conversion = mockStatic(Conversion.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            conversion.when(() -> Conversion.binaryBeMsb0ToHexDigit(booleanArray, 0)).thenReturn('A');
            //Act Statement(s)
            char result = Conversion.binaryBeMsb0ToHexDigit(booleanArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo('A'));
                conversion.verify(() -> Conversion.binaryBeMsb0ToHexDigit(booleanArray, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${binaryBeMsb0ToHexDigit1WhenSrcLengthEquals0ThrowsIllegalArgumentException}, hash: AF7A49F85388B3EBBB6BDE8E87676064
    @Test()
    void binaryBeMsb0ToHexDigit1WhenSrcLengthEquals0ThrowsIllegalArgumentException() {
        /* Branches:
         * (Integer.compareUnsigned(srcPos, src.length) >= 0) : true
         * (src.length == 0) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("Cannot convert an empty array.");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.binaryBeMsb0ToHexDigit(booleanArray, 0);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${binaryBeMsb0ToHexDigit1WhenSrcLengthNotEquals0ThrowsIndexOutOfBoundsException}, hash: 3154AE8000A4B3AADDF389050FF505DB
    @Test()
    void binaryBeMsb0ToHexDigit1WhenSrcLengthNotEquals0ThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (Integer.compareUnsigned(srcPos, src.length) >= 0) : true
         * (src.length == 0) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false };
        IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("2 is not within array length 1");
        //Act Statement(s)
        final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
            Conversion.binaryBeMsb0ToHexDigit(booleanArray, 2);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${binaryBeMsb0ToHexDigit1WhenPosIndexOfSrc}, hash: B01C75550DF6162FFC7A820FF25EBE3B
    @Test()
    void binaryBeMsb0ToHexDigit1WhenPosIndexOfSrc() {
        /* Branches:
         * (Integer.compareUnsigned(srcPos, src.length) >= 0) : false
         * (3 <= pos) : true
         * (src[pos - 3]) : true
         * (src[pos - 2]) : true
         * (src[pos - 1]) : true
         * (src[pos]) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { true, true, true, true };
        //Act Statement(s)
        char result = Conversion.binaryBeMsb0ToHexDigit(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('f')));
    }

    //BaseRock generated method id: ${binaryBeMsb0ToHexDigit1WhenPosNotIndexOfSrc}, hash: 6D23C358A44A5837E88DD1A910CA08E4
    @Test()
    void binaryBeMsb0ToHexDigit1WhenPosNotIndexOfSrc() {
        /* Branches:
         * (Integer.compareUnsigned(srcPos, src.length) >= 0) : false
         * (3 <= pos) : true
         * (src[pos - 3]) : true
         * (src[pos - 2]) : true
         * (src[pos - 1]) : true
         * (src[pos]) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { true, true, true, false };
        //Act Statement(s)
        char result = Conversion.binaryBeMsb0ToHexDigit(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('e')));
    }

    //BaseRock generated method id: ${binaryBeMsb0ToHexDigit1WhenPosMinus1NotIndexOfSrcAndPosIndexOfSrc}, hash: 2BB21F31885F763FEE787BE0381C582B
    @Test()
    void binaryBeMsb0ToHexDigit1WhenPosMinus1NotIndexOfSrcAndPosIndexOfSrc() {
        /* Branches:
         * (Integer.compareUnsigned(srcPos, src.length) >= 0) : false
         * (3 <= pos) : true
         * (src[pos - 3]) : true
         * (src[pos - 2]) : true
         * (src[pos - 1]) : false
         * (src[pos]) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { true, true, false, true };
        //Act Statement(s)
        char result = Conversion.binaryBeMsb0ToHexDigit(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('d')));
    }

    //BaseRock generated method id: ${binaryBeMsb0ToHexDigit1WhenPosMinus1NotIndexOfSrcAndPosNotIndexOfSrc}, hash: A6E4DB0355B4575881B8F7F09170185D
    @Test()
    void binaryBeMsb0ToHexDigit1WhenPosMinus1NotIndexOfSrcAndPosNotIndexOfSrc() {
        /* Branches:
         * (Integer.compareUnsigned(srcPos, src.length) >= 0) : false
         * (3 <= pos) : true
         * (src[pos - 3]) : true
         * (src[pos - 2]) : true
         * (src[pos - 1]) : false
         * (src[pos]) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { true, true, false, false };
        //Act Statement(s)
        char result = Conversion.binaryBeMsb0ToHexDigit(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('c')));
    }

    //BaseRock generated method id: ${binaryBeMsb0ToHexDigit1WhenPosMinus1IndexOfSrcAndPosIndexOfSrc}, hash: 901A5CADD58512F9AF3CCA275E0CBDBA
    @Test()
    void binaryBeMsb0ToHexDigit1WhenPosMinus1IndexOfSrcAndPosIndexOfSrc() {
        /* Branches:
         * (Integer.compareUnsigned(srcPos, src.length) >= 0) : false
         * (3 <= pos) : true
         * (src[pos - 3]) : true
         * (src[pos - 2]) : false
         * (src[pos - 1]) : true
         * (src[pos]) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { true, false, true, true };
        //Act Statement(s)
        char result = Conversion.binaryBeMsb0ToHexDigit(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('b')));
    }

    //BaseRock generated method id: ${binaryBeMsb0ToHexDigit1WhenPosMinus1IndexOfSrcAndPosNotIndexOfSrc}, hash: AC265FA5EBE94C9850222DB793C2A6F7
    @Test()
    void binaryBeMsb0ToHexDigit1WhenPosMinus1IndexOfSrcAndPosNotIndexOfSrc() {
        /* Branches:
         * (Integer.compareUnsigned(srcPos, src.length) >= 0) : false
         * (3 <= pos) : true
         * (src[pos - 3]) : true
         * (src[pos - 2]) : false
         * (src[pos - 1]) : true
         * (src[pos]) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { true, false, true, false };
        //Act Statement(s)
        char result = Conversion.binaryBeMsb0ToHexDigit(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('a')));
    }

    //BaseRock generated method id: ${binaryBeMsb0ToHexDigit1WhenPosMinus2NotIndexOfSrcAndPosMinus1NotIndexOfSrcAndPosIndexOfSrc}, hash: 5BE4518D2333FB74BF164224595324D8
    @Test()
    void binaryBeMsb0ToHexDigit1WhenPosMinus2NotIndexOfSrcAndPosMinus1NotIndexOfSrcAndPosIndexOfSrc() {
        /* Branches:
         * (Integer.compareUnsigned(srcPos, src.length) >= 0) : false
         * (3 <= pos) : true
         * (src[pos - 3]) : true
         * (src[pos - 2]) : false
         * (src[pos - 1]) : false
         * (src[pos]) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { true, false, false, true };
        //Act Statement(s)
        char result = Conversion.binaryBeMsb0ToHexDigit(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('9')));
    }

    //BaseRock generated method id: ${binaryBeMsb0ToHexDigit1WhenPosMinus2NotIndexOfSrcAndPosMinus1NotIndexOfSrcAndPosNotIndexOfSrc}, hash: 170866D2DFDEF6AF64BD25242CF17A85
    @Test()
    void binaryBeMsb0ToHexDigit1WhenPosMinus2NotIndexOfSrcAndPosMinus1NotIndexOfSrcAndPosNotIndexOfSrc() {
        /* Branches:
         * (Integer.compareUnsigned(srcPos, src.length) >= 0) : false
         * (3 <= pos) : true
         * (src[pos - 3]) : true
         * (src[pos - 2]) : false
         * (src[pos - 1]) : false
         * (src[pos]) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { true, false, false, false };
        //Act Statement(s)
        char result = Conversion.binaryBeMsb0ToHexDigit(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('8')));
    }

    //BaseRock generated method id: ${binaryBeMsb0ToHexDigit1When2LessThanOrEqualsToPosAndPosMinus2IndexOfSrcAndPosMinus1IndexOfSrcAndPosIndexOfSrc}, hash: B2EFB353A8DB5FADBD07E8539D84D314
    @Test()
    void binaryBeMsb0ToHexDigit1When2LessThanOrEqualsToPosAndPosMinus2IndexOfSrcAndPosMinus1IndexOfSrcAndPosIndexOfSrc() {
        /* Branches:
         * (Integer.compareUnsigned(srcPos, src.length) >= 0) : false
         * (3 <= pos) : true
         * (src[pos - 3]) : false
         * (2 <= pos) : true
         * (src[pos - 2]) : true
         * (src[pos - 1]) : true
         * (src[pos]) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false, true, true, true };
        //Act Statement(s)
        char result = Conversion.binaryBeMsb0ToHexDigit(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('7')));
    }

    //BaseRock generated method id: ${binaryBeMsb0ToHexDigit1When2LessThanOrEqualsToPosAndPosMinus2IndexOfSrcAndPosMinus1IndexOfSrcAndPosNotIndexOfSrc}, hash: 22CF7479D8260F31E89A2F7DAF31A121
    @Test()
    void binaryBeMsb0ToHexDigit1When2LessThanOrEqualsToPosAndPosMinus2IndexOfSrcAndPosMinus1IndexOfSrcAndPosNotIndexOfSrc() {
        /* Branches:
         * (Integer.compareUnsigned(srcPos, src.length) >= 0) : false
         * (3 <= pos) : true
         * (src[pos - 3]) : false
         * (2 <= pos) : true
         * (src[pos - 2]) : true
         * (src[pos - 1]) : true
         * (src[pos]) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false, true, true, false };
        //Act Statement(s)
        char result = Conversion.binaryBeMsb0ToHexDigit(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('6')));
    }

    //BaseRock generated method id: ${binaryBeMsb0ToHexDigit1When2LessThanOrEqualsToPosAndPosMinus2IndexOfSrcAndPosMinus1NotIndexOfSrcAndPosIndexOfSrc}, hash: 7DF683F8DFB35B5339ADBE6026BB0D71
    @Test()
    void binaryBeMsb0ToHexDigit1When2LessThanOrEqualsToPosAndPosMinus2IndexOfSrcAndPosMinus1NotIndexOfSrcAndPosIndexOfSrc() {
        /* Branches:
         * (Integer.compareUnsigned(srcPos, src.length) >= 0) : false
         * (3 <= pos) : true
         * (src[pos - 3]) : false
         * (2 <= pos) : true
         * (src[pos - 2]) : true
         * (src[pos - 1]) : false
         * (src[pos]) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false, true, false, true };
        //Act Statement(s)
        char result = Conversion.binaryBeMsb0ToHexDigit(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('5')));
    }

    //BaseRock generated method id: ${binaryBeMsb0ToHexDigit1When2LessThanOrEqualsToPosAndPosMinus2IndexOfSrcAndPosMinus1NotIndexOfSrcAndPosNotIndexOfSrc}, hash: 6B939E7CABB965DCB673F33CE15EF250
    @Test()
    void binaryBeMsb0ToHexDigit1When2LessThanOrEqualsToPosAndPosMinus2IndexOfSrcAndPosMinus1NotIndexOfSrcAndPosNotIndexOfSrc() {
        /* Branches:
         * (Integer.compareUnsigned(srcPos, src.length) >= 0) : false
         * (3 <= pos) : true
         * (src[pos - 3]) : false
         * (2 <= pos) : true
         * (src[pos - 2]) : true
         * (src[pos - 1]) : false
         * (src[pos]) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false, true, false, false };
        //Act Statement(s)
        char result = Conversion.binaryBeMsb0ToHexDigit(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('4')));
    }

    //BaseRock generated method id: ${binaryBeMsb0ToHexDigit1When2LessThanOrEqualsToPosAndPosMinus2NotIndexOfSrcAnd1LessThanOrEqualsToPosAndPosMinus1IndexOfS}, hash: 95939E6C47BA1BCA543BEB5A89D20245
    @Test()
    void binaryBeMsb0ToHexDigit1When2LessThanOrEqualsToPosAndPosMinus2NotIndexOfSrcAnd1LessThanOrEqualsToPosAndPosMinus1IndexOfS() {
        /* Branches:
         * (Integer.compareUnsigned(srcPos, src.length) >= 0) : false
         * (3 <= pos) : true
         * (src[pos - 3]) : false
         * (2 <= pos) : true
         * (src[pos - 2]) : false
         * (1 <= pos) : true
         * (src[pos - 1]) : true
         * (src[pos]) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false, false, true, true };
        //Act Statement(s)
        char result = Conversion.binaryBeMsb0ToHexDigit(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('3')));
    }

    //BaseRock generated method id: ${binaryBeMsb0ToHexDigit1WhenPosMinus2NotIndexOfSrcAnd1LessThanOrEqualsToPosAndPosMinus1IndexOfSrcAndPosNotIndexOfSrc2}, hash: C3C429D123BBFDD10209CC5E530CC667
    @Test()
    void binaryBeMsb0ToHexDigit1WhenPosMinus2NotIndexOfSrcAnd1LessThanOrEqualsToPosAndPosMinus1IndexOfSrcAndPosNotIndexOfSrc2() {
        /* Branches:
         * (Integer.compareUnsigned(srcPos, src.length) >= 0) : false
         * (3 <= pos) : true
         * (src[pos - 3]) : false
         * (2 <= pos) : true
         * (src[pos - 2]) : false
         * (1 <= pos) : true
         * (src[pos - 1]) : true
         * (src[pos]) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false, false, true, false };
        //Act Statement(s)
        char result = Conversion.binaryBeMsb0ToHexDigit(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('2')));
    }

    //BaseRock generated method id: ${binaryBeMsb0ToHexDigit1When2LessThanOrEqualsToPosAndPosMinus2NotIndexOfSrcAnd1LessThanOrEqualsToPosAndPosMinus1NotIndex}, hash: 21939C59CAF214C0FB0BCEB97865C755
    @Test()
    void binaryBeMsb0ToHexDigit1When2LessThanOrEqualsToPosAndPosMinus2NotIndexOfSrcAnd1LessThanOrEqualsToPosAndPosMinus1NotIndex() {
        /* Branches:
         * (Integer.compareUnsigned(srcPos, src.length) >= 0) : false
         * (3 <= pos) : true
         * (src[pos - 3]) : false
         * (2 <= pos) : true
         * (src[pos - 2]) : false
         * (1 <= pos) : true
         * (src[pos - 1]) : false
         * (src[pos]) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false, false, false, true };
        //Act Statement(s)
        char result = Conversion.binaryBeMsb0ToHexDigit(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('1')));
    }

    //BaseRock generated method id: ${binaryBeMsb0ToHexDigit1WhenPosMinus2NotIndexOfSrcAnd1LessThanOrEqualsToPosAndPosMinus1NotIndexOfSrcAndPosNotIndexOfSrc2}, hash: 930D29FD8F5B5503C0C099B75DE906C1
    @Test()
    void binaryBeMsb0ToHexDigit1WhenPosMinus2NotIndexOfSrcAnd1LessThanOrEqualsToPosAndPosMinus1NotIndexOfSrcAndPosNotIndexOfSrc2() {
        /* Branches:
         * (Integer.compareUnsigned(srcPos, src.length) >= 0) : false
         * (3 <= pos) : true
         * (src[pos - 3]) : false
         * (2 <= pos) : true
         * (src[pos - 2]) : false
         * (1 <= pos) : true
         * (src[pos - 1]) : false
         * (src[pos]) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false, false, false, false };
        //Act Statement(s)
        char result = Conversion.binaryBeMsb0ToHexDigit(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('0')));
    }

    //BaseRock generated method id: ${binaryToByteWhenSrcPosNotEquals0And0EqualsNBools}, hash: 147E4E229E031509C8D04A23A3095517
    @Test()
    void binaryToByteWhenSrcPosNotEquals0And0EqualsNBools() {
        /* Branches:
         * (src.length == 0) : true
         * (srcPos == 0) : false
         * (0 == nBools) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        //Act Statement(s)
        byte result = Conversion.binaryToByte(booleanArray, 1, (byte) 0, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((byte) 0)));
    }

    //BaseRock generated method id: ${binaryToByteWhen0NotEqualsNBoolsAndNBoolsMinus1PlusDstPosGreaterThanOrEqualsTo8ThrowsIllegalArgumentException}, hash: 2DCFD5750285F3EF3CEA2301137B8BEA
    @Test()
    void binaryToByteWhen0NotEqualsNBoolsAndNBoolsMinus1PlusDstPosGreaterThanOrEqualsTo8ThrowsIllegalArgumentException() {
        /* Branches:
         * (src.length == 0) : true
         * (srcPos == 0) : false
         * (0 == nBools) : false
         * (nBools - 1 + dstPos >= 8) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("nBools-1+dstPos is greater or equal to than 8");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.binaryToByte(booleanArray, 1, (byte) 0, 8, 1);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${binaryToByteWhenIPlusSrcPosNotIndexOfSrc}, hash: 0EBED90F18363F280F7D9EDE975B0D5E
    @Test()
    void binaryToByteWhenIPlusSrcPosNotIndexOfSrc() {
        /* Branches:
         * (src.length == 0) : false
         * (0 == nBools) : false
         * (nBools - 1 + dstPos >= 8) : false
         * (i < nBools) : true
         * (src[i + srcPos]) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false };
        //Act Statement(s)
        byte result = Conversion.binaryToByte(booleanArray, 0, (byte) 0, 0, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((byte) 0)));
    }

    //BaseRock generated method id: ${binaryToByteWhenILessThanNBoolsAndIPlusSrcPosIndexOfSrc}, hash: 08D15D5DD3204CF93B8CCA2E55857F07
    @Test()
    void binaryToByteWhenILessThanNBoolsAndIPlusSrcPosIndexOfSrc() {
        /* Branches:
         * (src.length == 0) : true
         * (srcPos == 0) : false
         * (0 == nBools) : false
         * (nBools - 1 + dstPos >= 8) : false
         * (i < nBools) : true
         * (src[i + srcPos]) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        //Act Statement(s)
        byte result = Conversion.binaryToByte(booleanArray, 0, (byte) 0, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((byte) 0)));
    }

    //BaseRock generated method id: ${binaryToHexDigitTest}, hash: D9A173D11BB60A70A8678B8B8B99EB81
    @Test()
    void binaryToHexDigitTest() {
        //Arrange Statement(s)
        try (MockedStatic<Conversion> conversion = mockStatic(Conversion.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            conversion.when(() -> Conversion.binaryToHexDigit(booleanArray, 0)).thenReturn('A');
            //Act Statement(s)
            char result = Conversion.binaryToHexDigit(booleanArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo('A'));
                conversion.verify(() -> Conversion.binaryToHexDigit(booleanArray, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${binaryToHexDigit1WhenSrcLengthEquals0ThrowsIllegalArgumentException}, hash: E9CFDFEA660B3A2D034DE9E13CA784FE
    @Test()
    void binaryToHexDigit1WhenSrcLengthEquals0ThrowsIllegalArgumentException() {
        /* Branches:
         * (src.length == 0) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("Cannot convert an empty array.");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.binaryToHexDigit(booleanArray, 0);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${binaryToHexDigit1WhenSrcPosPlus1IndexOfSrcAndSrcPosIndexOfSrc}, hash: 77A3A3F7AC3EE5080BFB444AC4A03624
    @Test()
    void binaryToHexDigit1WhenSrcPosPlus1IndexOfSrcAndSrcPosIndexOfSrc() {
        /* Branches:
         * (src.length == 0) : false
         * (src.length > srcPos + 3) : true
         * (src[srcPos + 3]) : true
         * (src[srcPos + 2]) : true
         * (src[srcPos + 1]) : true
         * (src[srcPos]) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { true, true, true, true };
        //Act Statement(s)
        char result = Conversion.binaryToHexDigit(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('f')));
    }

    //BaseRock generated method id: ${binaryToHexDigit1WhenSrcPosPlus1IndexOfSrcAndSrcPosNotIndexOfSrc}, hash: 3916F1F0F015B1530481185A31479489
    @Test()
    void binaryToHexDigit1WhenSrcPosPlus1IndexOfSrcAndSrcPosNotIndexOfSrc() {
        /* Branches:
         * (src.length == 0) : false
         * (src.length > srcPos + 3) : true
         * (src[srcPos + 3]) : true
         * (src[srcPos + 2]) : true
         * (src[srcPos + 1]) : true
         * (src[srcPos]) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false, true, true, true };
        //Act Statement(s)
        char result = Conversion.binaryToHexDigit(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('e')));
    }

    //BaseRock generated method id: ${binaryToHexDigit1WhenSrcPosPlus1NotIndexOfSrcAndSrcPosIndexOfSrc}, hash: 55357C7066238DD207BB49D408F42648
    @Test()
    void binaryToHexDigit1WhenSrcPosPlus1NotIndexOfSrcAndSrcPosIndexOfSrc() {
        /* Branches:
         * (src.length == 0) : false
         * (src.length > srcPos + 3) : true
         * (src[srcPos + 3]) : true
         * (src[srcPos + 2]) : true
         * (src[srcPos + 1]) : false
         * (src[srcPos]) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { true, false, true, true };
        //Act Statement(s)
        char result = Conversion.binaryToHexDigit(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('d')));
    }

    //BaseRock generated method id: ${binaryToHexDigit1WhenSrcPosPlus1NotIndexOfSrcAndSrcPosNotIndexOfSrc}, hash: 99E618E25BB0D2B3EC5CE2D06AE259F9
    @Test()
    void binaryToHexDigit1WhenSrcPosPlus1NotIndexOfSrcAndSrcPosNotIndexOfSrc() {
        /* Branches:
         * (src.length == 0) : false
         * (src.length > srcPos + 3) : true
         * (src[srcPos + 3]) : true
         * (src[srcPos + 2]) : true
         * (src[srcPos + 1]) : false
         * (src[srcPos]) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false, false, true, true };
        //Act Statement(s)
        char result = Conversion.binaryToHexDigit(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('c')));
    }

    //BaseRock generated method id: ${binaryToHexDigit1WhenSrcPosPlus2NotIndexOfSrcAndSrcPosPlus1IndexOfSrcAndSrcPosIndexOfSrc}, hash: B6DD53C208116B766B7237ADFAA97684
    @Test()
    void binaryToHexDigit1WhenSrcPosPlus2NotIndexOfSrcAndSrcPosPlus1IndexOfSrcAndSrcPosIndexOfSrc() {
        /* Branches:
         * (src.length == 0) : false
         * (src.length > srcPos + 3) : true
         * (src[srcPos + 3]) : true
         * (src[srcPos + 2]) : false
         * (src[srcPos + 1]) : true
         * (src[srcPos]) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { true, true, false, true };
        //Act Statement(s)
        char result = Conversion.binaryToHexDigit(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('b')));
    }

    //BaseRock generated method id: ${binaryToHexDigit1WhenSrcPosPlus2NotIndexOfSrcAndSrcPosPlus1IndexOfSrcAndSrcPosNotIndexOfSrc}, hash: F2BBC584BF4A4355C9315BC7AEFD7202
    @Test()
    void binaryToHexDigit1WhenSrcPosPlus2NotIndexOfSrcAndSrcPosPlus1IndexOfSrcAndSrcPosNotIndexOfSrc() {
        /* Branches:
         * (src.length == 0) : false
         * (src.length > srcPos + 3) : true
         * (src[srcPos + 3]) : true
         * (src[srcPos + 2]) : false
         * (src[srcPos + 1]) : true
         * (src[srcPos]) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false, true, false, true };
        //Act Statement(s)
        char result = Conversion.binaryToHexDigit(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('a')));
    }

    //BaseRock generated method id: ${binaryToHexDigit1WhenSrcPosPlus2NotIndexOfSrcAndSrcPosPlus1NotIndexOfSrcAndSrcPosIndexOfSrc}, hash: D733AA5CAFB36555047F25928F62F527
    @Test()
    void binaryToHexDigit1WhenSrcPosPlus2NotIndexOfSrcAndSrcPosPlus1NotIndexOfSrcAndSrcPosIndexOfSrc() {
        /* Branches:
         * (src.length == 0) : false
         * (src.length > srcPos + 3) : true
         * (src[srcPos + 3]) : true
         * (src[srcPos + 2]) : false
         * (src[srcPos + 1]) : false
         * (src[srcPos]) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { true, false, false, true };
        //Act Statement(s)
        char result = Conversion.binaryToHexDigit(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('9')));
    }

    //BaseRock generated method id: ${binaryToHexDigit1WhenSrcPosPlus2NotIndexOfSrcAndSrcPosPlus1NotIndexOfSrcAndSrcPosNotIndexOfSrc}, hash: CA73C147B7304703DCB9558505845F43
    @Test()
    void binaryToHexDigit1WhenSrcPosPlus2NotIndexOfSrcAndSrcPosPlus1NotIndexOfSrcAndSrcPosNotIndexOfSrc() {
        /* Branches:
         * (src.length == 0) : false
         * (src.length > srcPos + 3) : true
         * (src[srcPos + 3]) : true
         * (src[srcPos + 2]) : false
         * (src[srcPos + 1]) : false
         * (src[srcPos]) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false, false, false, true };
        //Act Statement(s)
        char result = Conversion.binaryToHexDigit(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('8')));
    }

    //BaseRock generated method id: ${binaryToHexDigit1WhenSrcLengthGreaterThanSrcPosPlus2AndSrcPosPlus2IndexOfSrcAndSrcPosPlus1IndexOfSrcAndSrcPosIndexOfSrc}, hash: 42CF9A94DF408C43C59B2960F9550F25
    @Test()
    void binaryToHexDigit1WhenSrcLengthGreaterThanSrcPosPlus2AndSrcPosPlus2IndexOfSrcAndSrcPosPlus1IndexOfSrcAndSrcPosIndexOfSrc() {
        /* Branches:
         * (src.length == 0) : false
         * (src.length > srcPos + 3) : true
         * (src[srcPos + 3]) : false
         * (src.length > srcPos + 2) : true
         * (src[srcPos + 2]) : true
         * (src[srcPos + 1]) : true
         * (src[srcPos]) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { true, true, true, false };
        //Act Statement(s)
        char result = Conversion.binaryToHexDigit(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('7')));
    }

    //BaseRock generated method id: ${binaryToHexDigit1WhenSrcLengthGreaterThanSrcPosPlus2AndSrcPosPlus2IndexOfSrcAndSrcPosPlus1IndexOfSrcAndSrcPosNotIndexOf}, hash: 88A87F3DA5051F8DCD23D62DE3538E7B
    @Test()
    void binaryToHexDigit1WhenSrcLengthGreaterThanSrcPosPlus2AndSrcPosPlus2IndexOfSrcAndSrcPosPlus1IndexOfSrcAndSrcPosNotIndexOf() {
        /* Branches:
         * (src.length == 0) : false
         * (src.length > srcPos + 3) : true
         * (src[srcPos + 3]) : false
         * (src.length > srcPos + 2) : true
         * (src[srcPos + 2]) : true
         * (src[srcPos + 1]) : true
         * (src[srcPos]) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false, true, true, false };
        //Act Statement(s)
        char result = Conversion.binaryToHexDigit(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('6')));
    }

    //BaseRock generated method id: ${binaryToHexDigit1WhenSrcLengthGreaterThanSrcPosPlus2AndSrcPosPlus2IndexOfSrcAndSrcPosPlus1NotIndexOfSrcAndSrcPosIndexOf}, hash: 2E079E7B8FF775DB60380C8B0C07A1C7
    @Test()
    void binaryToHexDigit1WhenSrcLengthGreaterThanSrcPosPlus2AndSrcPosPlus2IndexOfSrcAndSrcPosPlus1NotIndexOfSrcAndSrcPosIndexOf() {
        /* Branches:
         * (src.length == 0) : false
         * (src.length > srcPos + 3) : true
         * (src[srcPos + 3]) : false
         * (src.length > srcPos + 2) : true
         * (src[srcPos + 2]) : true
         * (src[srcPos + 1]) : false
         * (src[srcPos]) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { true, false, true, false };
        //Act Statement(s)
        char result = Conversion.binaryToHexDigit(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('5')));
    }

    //BaseRock generated method id: ${binaryToHexDigit1WhenSrcLengthGreaterThanSrcPosPlus2AndSrcPosPlus2IndexOfSrcAndSrcPosPlus1NotIndexOfSrcAndSrcPosNotInde}, hash: 2F5EA8105C3A3D27C953222C912AF387
    @Test()
    void binaryToHexDigit1WhenSrcLengthGreaterThanSrcPosPlus2AndSrcPosPlus2IndexOfSrcAndSrcPosPlus1NotIndexOfSrcAndSrcPosNotInde() {
        /* Branches:
         * (src.length == 0) : false
         * (src.length > srcPos + 3) : true
         * (src[srcPos + 3]) : false
         * (src.length > srcPos + 2) : true
         * (src[srcPos + 2]) : true
         * (src[srcPos + 1]) : false
         * (src[srcPos]) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false, false, true, false };
        //Act Statement(s)
        char result = Conversion.binaryToHexDigit(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('4')));
    }

    //BaseRock generated method id: ${binaryToHexDigit1WhenSrcLengthGreaterThanSrcPosPlus1AndSrcPosPlus1IndexOfSrcAndSrcPosIndexOfSrc2}, hash: D49B8C2FFEEFACE214EFFF28AA51298C
    @Test()
    void binaryToHexDigit1WhenSrcLengthGreaterThanSrcPosPlus1AndSrcPosPlus1IndexOfSrcAndSrcPosIndexOfSrc2() {
        /* Branches:
         * (src.length == 0) : false
         * (src.length > srcPos + 3) : true
         * (src[srcPos + 3]) : false
         * (src.length > srcPos + 2) : true
         * (src[srcPos + 2]) : false
         * (src.length > srcPos + 1) : true
         * (src[srcPos + 1]) : true
         * (src[srcPos]) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { true, true, false, false };
        //Act Statement(s)
        char result = Conversion.binaryToHexDigit(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('3')));
    }

    //BaseRock generated method id: ${binaryToHexDigit1WhenSrcLengthGreaterThanSrcPosPlus1AndSrcPosPlus1IndexOfSrcAndSrcPosNotIndexOfSrc3}, hash: 25377B3ED691ED433CD78D7EECC5A839
    @Test()
    void binaryToHexDigit1WhenSrcLengthGreaterThanSrcPosPlus1AndSrcPosPlus1IndexOfSrcAndSrcPosNotIndexOfSrc3() {
        /* Branches:
         * (src.length == 0) : false
         * (src.length > srcPos + 3) : true
         * (src[srcPos + 3]) : false
         * (src.length > srcPos + 2) : true
         * (src[srcPos + 2]) : false
         * (src.length > srcPos + 1) : true
         * (src[srcPos + 1]) : true
         * (src[srcPos]) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false, true, false, false };
        //Act Statement(s)
        char result = Conversion.binaryToHexDigit(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('2')));
    }

    //BaseRock generated method id: ${binaryToHexDigit1WhenSrcLengthGreaterThanSrcPosPlus1AndSrcPosPlus1NotIndexOfSrcAndSrcPosIndexOfSrc2}, hash: A24C753D52C89F8B2B1996504B26080F
    @Test()
    void binaryToHexDigit1WhenSrcLengthGreaterThanSrcPosPlus1AndSrcPosPlus1NotIndexOfSrcAndSrcPosIndexOfSrc2() {
        /* Branches:
         * (src.length == 0) : false
         * (src.length > srcPos + 3) : true
         * (src[srcPos + 3]) : false
         * (src.length > srcPos + 2) : true
         * (src[srcPos + 2]) : false
         * (src.length > srcPos + 1) : true
         * (src[srcPos + 1]) : false
         * (src[srcPos]) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { true, false, false, false };
        //Act Statement(s)
        char result = Conversion.binaryToHexDigit(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('1')));
    }

    //BaseRock generated method id: ${binaryToHexDigit1WhenSrcLengthGreaterThanSrcPosPlus1AndSrcPosPlus1NotIndexOfSrcAndSrcPosNotIndexOfSrc2}, hash: 642708BAEADDB99125FBC7604AC97898
    @Test()
    void binaryToHexDigit1WhenSrcLengthGreaterThanSrcPosPlus1AndSrcPosPlus1NotIndexOfSrcAndSrcPosNotIndexOfSrc2() {
        /* Branches:
         * (src.length == 0) : false
         * (src.length > srcPos + 3) : true
         * (src[srcPos + 3]) : false
         * (src.length > srcPos + 2) : true
         * (src[srcPos + 2]) : false
         * (src.length > srcPos + 1) : true
         * (src[srcPos + 1]) : false
         * (src[srcPos]) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false, false, false, false };
        //Act Statement(s)
        char result = Conversion.binaryToHexDigit(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('0')));
    }

    //BaseRock generated method id: ${binaryToHexDigitMsb0_4bitsTest}, hash: CE7D269E99F661B6C963849BBD352F47
    @Test()
    void binaryToHexDigitMsb0_4bitsTest() {
        //Arrange Statement(s)
        try (MockedStatic<Conversion> conversion = mockStatic(Conversion.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            conversion.when(() -> Conversion.binaryToHexDigitMsb0_4bits(booleanArray, 0)).thenReturn('A');
            //Act Statement(s)
            char result = Conversion.binaryToHexDigitMsb0_4bits(booleanArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo('A'));
                conversion.verify(() -> Conversion.binaryToHexDigitMsb0_4bits(booleanArray, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${binaryToHexDigitMsb0_4bits1WhenSrcLengthGreaterThan8ThrowsIllegalArgumentException}, hash: 9F8CA4DFD2248D765EBE2A7498458E1C
    @Test()
    void binaryToHexDigitMsb0_4bits1WhenSrcLengthGreaterThan8ThrowsIllegalArgumentException() {
        /* Branches:
         * (src.length > 8) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false, false, false, false, false, false, false, false, false };
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("src.length>8: src.length=9");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.binaryToHexDigitMsb0_4bits(booleanArray, 0);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${binaryToHexDigitMsb0_4bits1WhenSrcLengthMinusSrcPosLessThan4ThrowsIllegalArgumentException}, hash: 683F3C14E6A03DA4C9D1F35A80D13F70
    @Test()
    void binaryToHexDigitMsb0_4bits1WhenSrcLengthMinusSrcPosLessThan4ThrowsIllegalArgumentException() {
        /* Branches:
         * (src.length > 8) : false
         * (src.length - srcPos < 4) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false };
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("src.length-srcPos<4: src.length=1, srcPos=2");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.binaryToHexDigitMsb0_4bits(booleanArray, 2);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${binaryToHexDigitMsb0_4bits1WhenSrcPosIndexOfSrc}, hash: 57AD5B19B228B39AE506457EDBA0E25D
    @Test()
    void binaryToHexDigitMsb0_4bits1WhenSrcPosIndexOfSrc() {
        /* Branches:
         * (src.length > 8) : false
         * (src.length - srcPos < 4) : false
         * (src[srcPos + 3]) : true
         * (src[srcPos + 2]) : true
         * (src[srcPos + 1]) : true
         * (src[srcPos]) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { true, true, true, true };
        //Act Statement(s)
        char result = Conversion.binaryToHexDigitMsb0_4bits(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('f')));
    }

    //BaseRock generated method id: ${binaryToHexDigitMsb0_4bits1WhenSrcPosNotIndexOfSrc}, hash: A54F15B88F1DBE63EE2890A37975B276
    @Test()
    void binaryToHexDigitMsb0_4bits1WhenSrcPosNotIndexOfSrc() {
        /* Branches:
         * (src.length > 8) : false
         * (src.length - srcPos < 4) : false
         * (src[srcPos + 3]) : true
         * (src[srcPos + 2]) : true
         * (src[srcPos + 1]) : true
         * (src[srcPos]) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false, true, true, true };
        //Act Statement(s)
        char result = Conversion.binaryToHexDigitMsb0_4bits(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('7')));
    }

    //BaseRock generated method id: ${binaryToHexDigitMsb0_4bits1WhenSrcPosPlus1NotIndexOfSrcAndSrcPosIndexOfSrc}, hash: 38B237B11EF28ADD61329A5AA3746447
    @Test()
    void binaryToHexDigitMsb0_4bits1WhenSrcPosPlus1NotIndexOfSrcAndSrcPosIndexOfSrc() {
        /* Branches:
         * (src.length > 8) : false
         * (src.length - srcPos < 4) : false
         * (src[srcPos + 3]) : true
         * (src[srcPos + 2]) : true
         * (src[srcPos + 1]) : false
         * (src[srcPos]) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { true, false, true, true };
        //Act Statement(s)
        char result = Conversion.binaryToHexDigitMsb0_4bits(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('b')));
    }

    //BaseRock generated method id: ${binaryToHexDigitMsb0_4bits1WhenSrcPosPlus1NotIndexOfSrcAndSrcPosNotIndexOfSrc}, hash: 33373BFABF41969BF5E293C24A39FF7D
    @Test()
    void binaryToHexDigitMsb0_4bits1WhenSrcPosPlus1NotIndexOfSrcAndSrcPosNotIndexOfSrc() {
        /* Branches:
         * (src.length > 8) : false
         * (src.length - srcPos < 4) : false
         * (src[srcPos + 3]) : true
         * (src[srcPos + 2]) : true
         * (src[srcPos + 1]) : false
         * (src[srcPos]) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false, false, true, true };
        //Act Statement(s)
        char result = Conversion.binaryToHexDigitMsb0_4bits(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('3')));
    }

    //BaseRock generated method id: ${binaryToHexDigitMsb0_4bits1WhenSrcPosPlus1IndexOfSrcAndSrcPosIndexOfSrc}, hash: D85570DD18F89B33EF295949B66DA29E
    @Test()
    void binaryToHexDigitMsb0_4bits1WhenSrcPosPlus1IndexOfSrcAndSrcPosIndexOfSrc() {
        /* Branches:
         * (src.length > 8) : false
         * (src.length - srcPos < 4) : false
         * (src[srcPos + 3]) : true
         * (src[srcPos + 2]) : false
         * (src[srcPos + 1]) : true
         * (src[srcPos]) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { true, true, false, true };
        //Act Statement(s)
        char result = Conversion.binaryToHexDigitMsb0_4bits(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('d')));
    }

    //BaseRock generated method id: ${binaryToHexDigitMsb0_4bits1WhenSrcPosPlus1IndexOfSrcAndSrcPosNotIndexOfSrc}, hash: 2CA5A078450504031DBD8568B8BEE0B1
    @Test()
    void binaryToHexDigitMsb0_4bits1WhenSrcPosPlus1IndexOfSrcAndSrcPosNotIndexOfSrc() {
        /* Branches:
         * (src.length > 8) : false
         * (src.length - srcPos < 4) : false
         * (src[srcPos + 3]) : true
         * (src[srcPos + 2]) : false
         * (src[srcPos + 1]) : true
         * (src[srcPos]) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false, true, false, true };
        //Act Statement(s)
        char result = Conversion.binaryToHexDigitMsb0_4bits(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('5')));
    }

    //BaseRock generated method id: ${binaryToHexDigitMsb0_4bits1WhenSrcPosPlus2NotIndexOfSrcAndSrcPosPlus1NotIndexOfSrcAndSrcPosIndexOfSrc}, hash: 8F7C0F3695985F4F1406A54F8587E089
    @Test()
    void binaryToHexDigitMsb0_4bits1WhenSrcPosPlus2NotIndexOfSrcAndSrcPosPlus1NotIndexOfSrcAndSrcPosIndexOfSrc() {
        /* Branches:
         * (src.length > 8) : false
         * (src.length - srcPos < 4) : false
         * (src[srcPos + 3]) : true
         * (src[srcPos + 2]) : false
         * (src[srcPos + 1]) : false
         * (src[srcPos]) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { true, false, false, true };
        //Act Statement(s)
        char result = Conversion.binaryToHexDigitMsb0_4bits(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('9')));
    }

    //BaseRock generated method id: ${binaryToHexDigitMsb0_4bits1WhenSrcPosPlus2NotIndexOfSrcAndSrcPosPlus1NotIndexOfSrcAndSrcPosNotIndexOfSrc}, hash: 5D9188147A2CA8A6855C5D583396D89D
    @Test()
    void binaryToHexDigitMsb0_4bits1WhenSrcPosPlus2NotIndexOfSrcAndSrcPosPlus1NotIndexOfSrcAndSrcPosNotIndexOfSrc() {
        /* Branches:
         * (src.length > 8) : false
         * (src.length - srcPos < 4) : false
         * (src[srcPos + 3]) : true
         * (src[srcPos + 2]) : false
         * (src[srcPos + 1]) : false
         * (src[srcPos]) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false, false, false, true };
        //Act Statement(s)
        char result = Conversion.binaryToHexDigitMsb0_4bits(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('1')));
    }

    //BaseRock generated method id: ${binaryToHexDigitMsb0_4bits1WhenSrcPosPlus2IndexOfSrcAndSrcPosPlus1IndexOfSrcAndSrcPosIndexOfSrc}, hash: D686ACB7827272F01B8C857D8C2FAA3F
    @Test()
    void binaryToHexDigitMsb0_4bits1WhenSrcPosPlus2IndexOfSrcAndSrcPosPlus1IndexOfSrcAndSrcPosIndexOfSrc() {
        /* Branches:
         * (src.length > 8) : false
         * (src.length - srcPos < 4) : false
         * (src[srcPos + 3]) : false
         * (src[srcPos + 2]) : true
         * (src[srcPos + 1]) : true
         * (src[srcPos]) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { true, true, true, false };
        //Act Statement(s)
        char result = Conversion.binaryToHexDigitMsb0_4bits(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('e')));
    }

    //BaseRock generated method id: ${binaryToHexDigitMsb0_4bits1WhenSrcPosPlus2IndexOfSrcAndSrcPosPlus1IndexOfSrcAndSrcPosNotIndexOfSrc}, hash: F026EB7106CE61B5C7219F5D00B23E82
    @Test()
    void binaryToHexDigitMsb0_4bits1WhenSrcPosPlus2IndexOfSrcAndSrcPosPlus1IndexOfSrcAndSrcPosNotIndexOfSrc() {
        /* Branches:
         * (src.length > 8) : false
         * (src.length - srcPos < 4) : false
         * (src[srcPos + 3]) : false
         * (src[srcPos + 2]) : true
         * (src[srcPos + 1]) : true
         * (src[srcPos]) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false, true, true, false };
        //Act Statement(s)
        char result = Conversion.binaryToHexDigitMsb0_4bits(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('6')));
    }

    //BaseRock generated method id: ${binaryToHexDigitMsb0_4bits1WhenSrcPosPlus2IndexOfSrcAndSrcPosPlus1NotIndexOfSrcAndSrcPosIndexOfSrc}, hash: 11CE6A1F580EB8D5994A4C668D385314
    @Test()
    void binaryToHexDigitMsb0_4bits1WhenSrcPosPlus2IndexOfSrcAndSrcPosPlus1NotIndexOfSrcAndSrcPosIndexOfSrc() {
        /* Branches:
         * (src.length > 8) : false
         * (src.length - srcPos < 4) : false
         * (src[srcPos + 3]) : false
         * (src[srcPos + 2]) : true
         * (src[srcPos + 1]) : false
         * (src[srcPos]) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { true, false, true, false };
        //Act Statement(s)
        char result = Conversion.binaryToHexDigitMsb0_4bits(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('a')));
    }

    //BaseRock generated method id: ${binaryToHexDigitMsb0_4bits1WhenSrcPosPlus2IndexOfSrcAndSrcPosPlus1NotIndexOfSrcAndSrcPosNotIndexOfSrc}, hash: F63CF85B56F0C004CE21F4F000DD47D8
    @Test()
    void binaryToHexDigitMsb0_4bits1WhenSrcPosPlus2IndexOfSrcAndSrcPosPlus1NotIndexOfSrcAndSrcPosNotIndexOfSrc() {
        /* Branches:
         * (src.length > 8) : false
         * (src.length - srcPos < 4) : false
         * (src[srcPos + 3]) : false
         * (src[srcPos + 2]) : true
         * (src[srcPos + 1]) : false
         * (src[srcPos]) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false, false, true, false };
        //Act Statement(s)
        char result = Conversion.binaryToHexDigitMsb0_4bits(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('2')));
    }

    //BaseRock generated method id: ${binaryToHexDigitMsb0_4bits1WhenSrcPosPlus2NotIndexOfSrcAndSrcPosPlus1IndexOfSrcAndSrcPosIndexOfSrc}, hash: D4EB37CB785838C4F87E37604A41DBDC
    @Test()
    void binaryToHexDigitMsb0_4bits1WhenSrcPosPlus2NotIndexOfSrcAndSrcPosPlus1IndexOfSrcAndSrcPosIndexOfSrc() {
        /* Branches:
         * (src.length > 8) : false
         * (src.length - srcPos < 4) : false
         * (src[srcPos + 3]) : false
         * (src[srcPos + 2]) : false
         * (src[srcPos + 1]) : true
         * (src[srcPos]) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { true, true, false, false };
        //Act Statement(s)
        char result = Conversion.binaryToHexDigitMsb0_4bits(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('c')));
    }

    //BaseRock generated method id: ${binaryToHexDigitMsb0_4bits1WhenSrcPosPlus2NotIndexOfSrcAndSrcPosPlus1IndexOfSrcAndSrcPosNotIndexOfSrc}, hash: 104D846F70A0D6FDFE08E23819148CFC
    @Test()
    void binaryToHexDigitMsb0_4bits1WhenSrcPosPlus2NotIndexOfSrcAndSrcPosPlus1IndexOfSrcAndSrcPosNotIndexOfSrc() {
        /* Branches:
         * (src.length > 8) : false
         * (src.length - srcPos < 4) : false
         * (src[srcPos + 3]) : false
         * (src[srcPos + 2]) : false
         * (src[srcPos + 1]) : true
         * (src[srcPos]) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false, true, false, false };
        //Act Statement(s)
        char result = Conversion.binaryToHexDigitMsb0_4bits(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('4')));
    }

    //BaseRock generated method id: ${binaryToHexDigitMsb0_4bits1WhenSrcPosPlus3NotIndexOfSrcAndSrcPosPlus2NotIndexOfSrcAndSrcPosPlus1NotIndexOfSrcAndSrcPosI}, hash: 1AE473A2475D75888BB7F21C0B22AD11
    @Test()
    void binaryToHexDigitMsb0_4bits1WhenSrcPosPlus3NotIndexOfSrcAndSrcPosPlus2NotIndexOfSrcAndSrcPosPlus1NotIndexOfSrcAndSrcPosI() {
        /* Branches:
         * (src.length > 8) : false
         * (src.length - srcPos < 4) : false
         * (src[srcPos + 3]) : false
         * (src[srcPos + 2]) : false
         * (src[srcPos + 1]) : false
         * (src[srcPos]) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { true, false, false, false };
        //Act Statement(s)
        char result = Conversion.binaryToHexDigitMsb0_4bits(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('8')));
    }

    //BaseRock generated method id: ${binaryToHexDigitMsb0_4bits1WhenSrcPosPlus3NotIndexOfSrcAndSrcPosPlus2NotIndexOfSrcAndSrcPosPlus1NotIndexOfSrcAndSrcPosN}, hash: 938AB5D3251BFACEC41E972EE09530F9
    @Test()
    void binaryToHexDigitMsb0_4bits1WhenSrcPosPlus3NotIndexOfSrcAndSrcPosPlus2NotIndexOfSrcAndSrcPosPlus1NotIndexOfSrcAndSrcPosN() {
        /* Branches:
         * (src.length > 8) : false
         * (src.length - srcPos < 4) : false
         * (src[srcPos + 3]) : false
         * (src[srcPos + 2]) : false
         * (src[srcPos + 1]) : false
         * (src[srcPos]) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false, false, false, false };
        //Act Statement(s)
        char result = Conversion.binaryToHexDigitMsb0_4bits(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('0')));
    }

    //BaseRock generated method id: ${binaryToIntWhenSrcPosNotEquals0And0EqualsNBools}, hash: A252050CE807EF1590630388B3DB4321
    @Test()
    void binaryToIntWhenSrcPosNotEquals0And0EqualsNBools() {
        /* Branches:
         * (src.length == 0) : true
         * (srcPos == 0) : false
         * (0 == nBools) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        //Act Statement(s)
        int result = Conversion.binaryToInt(booleanArray, 1, 0, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${binaryToIntWhen0NotEqualsNBoolsAndNBoolsMinus1PlusDstPosGreaterThanOrEqualsTo32ThrowsIllegalArgumentException}, hash: 91853D395E0C24A0278CEE3FADF99F22
    @Test()
    void binaryToIntWhen0NotEqualsNBoolsAndNBoolsMinus1PlusDstPosGreaterThanOrEqualsTo32ThrowsIllegalArgumentException() {
        /* Branches:
         * (src.length == 0) : true
         * (srcPos == 0) : false
         * (0 == nBools) : false
         * (nBools - 1 + dstPos >= 32) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("nBools-1+dstPos is greater or equal to than 32");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.binaryToInt(booleanArray, 1, 0, 32, 1);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${binaryToIntWhenIPlusSrcPosNotIndexOfSrc}, hash: 6D3DB77C972D4D33837D32128C4E48C7
    @Test()
    void binaryToIntWhenIPlusSrcPosNotIndexOfSrc() {
        /* Branches:
         * (src.length == 0) : false
         * (0 == nBools) : false
         * (nBools - 1 + dstPos >= 32) : false
         * (i < nBools) : true
         * (src[i + srcPos]) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false };
        //Act Statement(s)
        int result = Conversion.binaryToInt(booleanArray, 0, 0, 0, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${binaryToIntWhenILessThanNBoolsAndIPlusSrcPosIndexOfSrc}, hash: 5CE0A5A6846B9F3EDAAE8DA15E34492A
    @Test()
    void binaryToIntWhenILessThanNBoolsAndIPlusSrcPosIndexOfSrc() {
        /* Branches:
         * (src.length == 0) : true
         * (srcPos == 0) : false
         * (0 == nBools) : false
         * (nBools - 1 + dstPos >= 32) : false
         * (i < nBools) : true
         * (src[i + srcPos]) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        //Act Statement(s)
        int result = Conversion.binaryToInt(booleanArray, 0, 0, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${binaryToLongWhenSrcPosNotEquals0And0EqualsNBools}, hash: 69A2C958B769BA6E16E26F63A9039A0B
    @Test()
    void binaryToLongWhenSrcPosNotEquals0And0EqualsNBools() {
        /* Branches:
         * (src.length == 0) : true
         * (srcPos == 0) : false
         * (0 == nBools) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        //Act Statement(s)
        long result = Conversion.binaryToLong(booleanArray, 1, 0L, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0L)));
    }

    //BaseRock generated method id: ${binaryToLongWhen0NotEqualsNBoolsAndNBoolsMinus1PlusDstPosGreaterThanOrEqualsTo64ThrowsIllegalArgumentException}, hash: CE01A1907960D61431E2CFAF6854207A
    @Test()
    void binaryToLongWhen0NotEqualsNBoolsAndNBoolsMinus1PlusDstPosGreaterThanOrEqualsTo64ThrowsIllegalArgumentException() {
        /* Branches:
         * (src.length == 0) : true
         * (srcPos == 0) : false
         * (0 == nBools) : false
         * (nBools - 1 + dstPos >= 64) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("nBools-1+dstPos is greater or equal to than 64");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.binaryToLong(booleanArray, 1, 0L, 64, 1);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${binaryToLongWhenIPlusSrcPosNotIndexOfSrc}, hash: 0AB8183A355DD547E14DB48D7F318817
    @Test()
    void binaryToLongWhenIPlusSrcPosNotIndexOfSrc() {
        /* Branches:
         * (src.length == 0) : false
         * (0 == nBools) : false
         * (nBools - 1 + dstPos >= 64) : false
         * (i < nBools) : true
         * (src[i + srcPos]) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false };
        //Act Statement(s)
        long result = Conversion.binaryToLong(booleanArray, 0, 0L, 0, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0L)));
    }

    //BaseRock generated method id: ${binaryToLongWhenILessThanNBoolsAndIPlusSrcPosIndexOfSrc}, hash: 9FA9FEC65E121A0AA570F60BAC540AFC
    @Test()
    void binaryToLongWhenILessThanNBoolsAndIPlusSrcPosIndexOfSrc() {
        /* Branches:
         * (src.length == 0) : true
         * (srcPos == 0) : false
         * (0 == nBools) : false
         * (nBools - 1 + dstPos >= 64) : false
         * (i < nBools) : true
         * (src[i + srcPos]) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        //Act Statement(s)
        long result = Conversion.binaryToLong(booleanArray, 0, 0L, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0L)));
    }

    //BaseRock generated method id: ${binaryToShortWhenSrcPosNotEquals0And0EqualsNBools}, hash: B3649FA7F4A4179A21FC7EDB744EB0FE
    @Test()
    void binaryToShortWhenSrcPosNotEquals0And0EqualsNBools() {
        /* Branches:
         * (src.length == 0) : true
         * (srcPos == 0) : false
         * (0 == nBools) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        //Act Statement(s)
        short result = Conversion.binaryToShort(booleanArray, 1, (short) 0, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((short) 0)));
    }

    //BaseRock generated method id: ${binaryToShortWhen0NotEqualsNBoolsAndNBoolsMinus1PlusDstPosGreaterThanOrEqualsTo16ThrowsIllegalArgumentException}, hash: 1D51F8B7A14E35C2253F42A65BC7BD61
    @Test()
    void binaryToShortWhen0NotEqualsNBoolsAndNBoolsMinus1PlusDstPosGreaterThanOrEqualsTo16ThrowsIllegalArgumentException() {
        /* Branches:
         * (src.length == 0) : true
         * (srcPos == 0) : false
         * (0 == nBools) : false
         * (nBools - 1 + dstPos >= 16) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("nBools-1+dstPos is greater or equal to than 16");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.binaryToShort(booleanArray, 1, (short) 0, 16, 1);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${binaryToShortWhenIPlusSrcPosNotIndexOfSrc}, hash: 7365FCFBF4B5477235EF71949C3A70DC
    @Test()
    void binaryToShortWhenIPlusSrcPosNotIndexOfSrc() {
        /* Branches:
         * (src.length == 0) : false
         * (0 == nBools) : false
         * (nBools - 1 + dstPos >= 16) : false
         * (i < nBools) : true
         * (src[i + srcPos]) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false };
        //Act Statement(s)
        short result = Conversion.binaryToShort(booleanArray, 0, (short) 0, 0, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((short) 0)));
    }

    //BaseRock generated method id: ${binaryToShortWhenILessThanNBoolsAndIPlusSrcPosIndexOfSrc}, hash: 145A0EDC35F820130B047B31B42F4619
    @Test()
    void binaryToShortWhenILessThanNBoolsAndIPlusSrcPosIndexOfSrc() {
        /* Branches:
         * (src.length == 0) : true
         * (srcPos == 0) : false
         * (0 == nBools) : false
         * (nBools - 1 + dstPos >= 16) : false
         * (i < nBools) : true
         * (src[i + srcPos]) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        //Act Statement(s)
        short result = Conversion.binaryToShort(booleanArray, 0, (short) 0, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((short) 0)));
    }

    //BaseRock generated method id: ${byteArrayToIntWhenSrcPosNotEquals0And0EqualsNBytes}, hash: C7263E79EAC9AEF81395E8DE69C5F189
    @Test()
    void byteArrayToIntWhenSrcPosNotEquals0And0EqualsNBytes() {
        /* Branches:
         * (src.length == 0) : true
         * (srcPos == 0) : false
         * (0 == nBytes) : true
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        //Act Statement(s)
        int result = Conversion.byteArrayToInt(byteArray, 1, 0, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${byteArrayToIntWhen0NotEqualsNBytesAndNBytesMinus1MultipliedBy8PlusDstPosGreaterThanOrEquaThrowsIllegalArgumentException}, hash: 934104B33F786E5800AF223CDB289E6E
    @Test()
    void byteArrayToIntWhen0NotEqualsNBytesAndNBytesMinus1MultipliedBy8PlusDstPosGreaterThanOrEquaThrowsIllegalArgumentException() {
        /* Branches:
         * (src.length == 0) : true
         * (srcPos == 0) : false
         * (0 == nBytes) : false
         * ((nBytes - 1) * 8 + dstPos >= 32) : true
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("(nBytes-1)*8+dstPos is greater or equal to than 32");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.byteArrayToInt(byteArray, 1, 0, 32, 1);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${byteArrayToIntWhenNBytesMinus1MultipliedBy8PlusDstPosLessThan32AndILessThanNBytes}, hash: 8F973D3F74FCCF027E81EA8C5EC9EB31
    @Test()
    void byteArrayToIntWhenNBytesMinus1MultipliedBy8PlusDstPosLessThan32AndILessThanNBytes() {
        /* Branches:
         * (src.length == 0) : true
         * (srcPos == 0) : false
         * (0 == nBytes) : false
         * ((nBytes - 1) * 8 + dstPos >= 32) : false
         * (i < nBytes) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        //Act Statement(s)
        int result = Conversion.byteArrayToInt(byteArray, 0, 0, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${byteArrayToLongWhenSrcPosNotEquals0And0EqualsNBytes}, hash: 2629DF2669A0119640A9A5BA158EF76A
    @Test()
    void byteArrayToLongWhenSrcPosNotEquals0And0EqualsNBytes() {
        /* Branches:
         * (src.length == 0) : true
         * (srcPos == 0) : false
         * (0 == nBytes) : true
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        //Act Statement(s)
        long result = Conversion.byteArrayToLong(byteArray, 1, 0L, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0L)));
    }

    //BaseRock generated method id: ${byteArrayToLongWhen0NotEqualsNBytesAndNBytesMinus1MultipliedBy8PlusDstPosGreaterThanOrEquThrowsIllegalArgumentException}, hash: 9199AE7FFBEB15FC53A5A00AAE5CA513
    @Test()
    void byteArrayToLongWhen0NotEqualsNBytesAndNBytesMinus1MultipliedBy8PlusDstPosGreaterThanOrEquThrowsIllegalArgumentException() {
        /* Branches:
         * (src.length == 0) : true
         * (srcPos == 0) : false
         * (0 == nBytes) : false
         * ((nBytes - 1) * 8 + dstPos >= 64) : true
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("(nBytes-1)*8+dstPos is greater or equal to than 64");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.byteArrayToLong(byteArray, 1, 0L, 64, 1);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${byteArrayToLongWhenNBytesMinus1MultipliedBy8PlusDstPosLessThan64AndILessThanNBytes}, hash: 78AFB54B5987AAA9C5AB433CA75A775D
    @Test()
    void byteArrayToLongWhenNBytesMinus1MultipliedBy8PlusDstPosLessThan64AndILessThanNBytes() {
        /* Branches:
         * (src.length == 0) : true
         * (srcPos == 0) : false
         * (0 == nBytes) : false
         * ((nBytes - 1) * 8 + dstPos >= 64) : false
         * (i < nBytes) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        //Act Statement(s)
        long result = Conversion.byteArrayToLong(byteArray, 0, 0L, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0L)));
    }

    //BaseRock generated method id: ${byteArrayToShortWhenSrcPosNotEquals0And0EqualsNBytes}, hash: AA637B28BC82063FD4237F5320A9F94A
    @Test()
    void byteArrayToShortWhenSrcPosNotEquals0And0EqualsNBytes() {
        /* Branches:
         * (src.length == 0) : true
         * (srcPos == 0) : false
         * (0 == nBytes) : true
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        //Act Statement(s)
        short result = Conversion.byteArrayToShort(byteArray, 1, (short) 0, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((short) 0)));
    }

    //BaseRock generated method id: ${byteArrayToShortWhen0NotEqualsNBytesAndNBytesMinus1MultipliedBy8PlusDstPosGreaterThanOrEqThrowsIllegalArgumentException}, hash: E3031EEAE51D8F6DD72E65E44F4EFB5F
    @Test()
    void byteArrayToShortWhen0NotEqualsNBytesAndNBytesMinus1MultipliedBy8PlusDstPosGreaterThanOrEqThrowsIllegalArgumentException() {
        /* Branches:
         * (src.length == 0) : true
         * (srcPos == 0) : false
         * (0 == nBytes) : false
         * ((nBytes - 1) * 8 + dstPos >= 16) : true
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("(nBytes-1)*8+dstPos is greater or equal to than 16");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.byteArrayToShort(byteArray, 1, (short) 0, 16, 1);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${byteArrayToShortWhenNBytesMinus1MultipliedBy8PlusDstPosLessThan16AndILessThanNBytes}, hash: 01B9FA953FF05A891B21FA1E52D1611D
    @Test()
    void byteArrayToShortWhenNBytesMinus1MultipliedBy8PlusDstPosLessThan16AndILessThanNBytes() {
        /* Branches:
         * (src.length == 0) : true
         * (srcPos == 0) : false
         * (0 == nBytes) : false
         * ((nBytes - 1) * 8 + dstPos >= 16) : false
         * (i < nBytes) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        //Act Statement(s)
        short result = Conversion.byteArrayToShort(byteArray, 0, (short) 0, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((short) 0)));
    }

    //BaseRock generated method id: ${byteArrayToUuidWhenSrcLengthMinusSrcPosLessThan16ThrowsIllegalArgumentException}, hash: FB7EC320A72DB2FD86B1B9D50DB9637C
    @Test()
    void byteArrayToUuidWhenSrcLengthMinusSrcPosLessThan16ThrowsIllegalArgumentException() {
        /* Branches:
         * (src.length - srcPos < 16) : true
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("Need at least 16 bytes for UUID");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.byteArrayToUuid(byteArray, 0);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${byteArrayToUuidWhenSrcLengthMinusSrcPosNotLessThan16}, hash: 4E4731A02CF96D991EBD5C4EBE407472
    @Test()
    void byteArrayToUuidWhenSrcLengthMinusSrcPosNotLessThan16() {
        /* Branches:
         * (src.length - srcPos < 16) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<Conversion> conversion = mockStatic(Conversion.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            conversion.when(() -> Conversion.byteArrayToLong(byteArray, -16, 0L, 0, 8)).thenReturn(0L);
            conversion.when(() -> Conversion.byteArrayToLong(byteArray, -8, 0L, 0, 8)).thenReturn(0L);
            //Act Statement(s)
            UUID result = Conversion.byteArrayToUuid(byteArray, -16);
            UUID uUID = new UUID(0L, 0L);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(uUID));
                conversion.verify(() -> Conversion.byteArrayToLong(byteArray, -16, 0L, 0, 8), atLeast(1));
                conversion.verify(() -> Conversion.byteArrayToLong(byteArray, -8, 0L, 0, 8), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${byteToBinaryWhen0EqualsNBools}, hash: 72133EBC8B6F142B80E8F21BA80D24EC
    @Test()
    void byteToBinaryWhen0EqualsNBools() {
        /* Branches:
         * (0 == nBools) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        //Act Statement(s)
        boolean[] result = Conversion.byteToBinary((byte) 0, 0, booleanArray, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanArray)));
    }

    //BaseRock generated method id: ${byteToBinaryWhenNBoolsMinus1PlusSrcPosGreaterThanOrEqualsTo8ThrowsIllegalArgumentException}, hash: E5B4F1F8FF712C4BAE55E5DFCEA518BA
    @Test()
    void byteToBinaryWhenNBoolsMinus1PlusSrcPosGreaterThanOrEqualsTo8ThrowsIllegalArgumentException() {
        /* Branches:
         * (0 == nBools) : false
         * (nBools - 1 + srcPos >= 8) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("nBools-1+srcPos is greater or equal to than 8");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.byteToBinary((byte) 0, 10, booleanArray, 0, -1);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${byteToBinaryWhenDstPosPlusI}, hash: FA5013B7E4B7FECD391BB1A87B6E2C23
    @Disabled()
    @Test()
    void byteToBinaryWhenDstPosPlusI() {
        /* Branches:
         * (0 == nBools) : false
         * (nBools - 1 + srcPos >= 8) : false
         * (i < nBools) : true
         * (dstPos + i) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false };
        //Act Statement(s)
        boolean[] result = Conversion.byteToBinary((byte) 0, 0, booleanArray, 0, 1);
        boolean[] booleanResultArray = new boolean[] { true };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${byteToBinaryWhenDstPosNotPlusI}, hash: 05F8DB29430C6482F053E485E28C1923
    @Test()
    void byteToBinaryWhenDstPosNotPlusI() {
        /* Branches:
         * (0 == nBools) : false
         * (nBools - 1 + srcPos >= 8) : false
         * (i < nBools) : true
         * (dstPos + i) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false };
        //Act Statement(s)
        boolean[] result = Conversion.byteToBinary((byte) 0, 0, booleanArray, 0, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanArray)));
    }

    //BaseRock generated method id: ${byteToHexWhen0EqualsNHexs}, hash: DCB636DA5294F1A7FB4A0FB990D79D0E
    @Test()
    void byteToHexWhen0EqualsNHexs() {
        /* Branches:
         * (0 == nHexs) : true
         */
        //Act Statement(s)
        String result = Conversion.byteToHex((byte) 0, 0, "dstInit1", 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("dstInit1")));
    }

    //BaseRock generated method id: ${byteToHexWhenNHexsMinus1MultipliedBy4PlusSrcPosGreaterThanOrEqualsTo8ThrowsIllegalArgumentException}, hash: 6A1FA168387FD2BA9D145FCF86A24F37
    @Test()
    void byteToHexWhenNHexsMinus1MultipliedBy4PlusSrcPosGreaterThanOrEqualsTo8ThrowsIllegalArgumentException() {
        /* Branches:
         * (0 == nHexs) : false
         * ((nHexs - 1) * 4 + srcPos >= 8) : true
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("(nHexs-1)*4+srcPos is greater or equal to than 8");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.byteToHex((byte) 0, 16, "dstInit1", 0, -1);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${byteToHexWhenDstPosPlusIEqualsAppend}, hash: DC56DF308A676EC33DBABEF097BF1E77
    @Test()
    void byteToHexWhenDstPosPlusIEqualsAppend() {
        /* Branches:
         * (0 == nHexs) : false
         * ((nHexs - 1) * 4 + srcPos >= 8) : false
         * (i < nHexs) : true
         * (dstPos + i == append) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<Conversion> conversion = mockStatic(Conversion.class, CALLS_REAL_METHODS)) {
            conversion.when(() -> Conversion.intToHexDigit(0)).thenReturn('A');
            //Act Statement(s)
            String result = Conversion.byteToHex((byte) 0, 1, "B", 1, 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("BA"));
                conversion.verify(() -> Conversion.intToHexDigit(0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${byteToHexWhenDstPosPlusINotEqualsAppend}, hash: 00BF93F2A073F0B9BCF70B788D76B80C
    @Disabled()
    @Test()
    void byteToHexWhenDstPosPlusINotEqualsAppend() {
        /* Branches:
         * (0 == nHexs) : false
         * ((nHexs - 1) * 4 + srcPos >= 8) : false
         * (i < nHexs) : true
         * (dstPos + i == append) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<Conversion> conversion = mockStatic(Conversion.class, CALLS_REAL_METHODS)) {
            conversion.when(() -> Conversion.intToHexDigit(0)).thenReturn('A');
            //Act Statement(s)
            String result = Conversion.byteToHex((byte) 0, 1, "A", 2, 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("A"));
                conversion.verify(() -> Conversion.intToHexDigit(0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${hexDigitMsb0ToBinaryWhenSwitchHexDigitCase_0_}, hash: 88C31E00239F2B44FE435ABBE5F94E08
    @Test()
    void hexDigitMsb0ToBinaryWhenSwitchHexDigitCase_0_() {
        /* Branches:
         * (switch(hexDigit) = '0') : true
         */
        //Act Statement(s)
        boolean[] result = Conversion.hexDigitMsb0ToBinary('0');
        boolean[] booleanResultArray = new boolean[] { false, false, false, false };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${hexDigitMsb0ToBinaryWhenSwitchHexDigitCase_1_}, hash: 3908B11273B465FFD2492D6711882E10
    @Test()
    void hexDigitMsb0ToBinaryWhenSwitchHexDigitCase_1_() {
        /* Branches:
         * (switch(hexDigit) = '1') : true
         */
        //Act Statement(s)
        boolean[] result = Conversion.hexDigitMsb0ToBinary('1');
        boolean[] booleanResultArray = new boolean[] { false, false, false, true };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${hexDigitMsb0ToBinaryWhenSwitchHexDigitCase_2_}, hash: F80C172A61225B4FEA1822FAA864719B
    @Test()
    void hexDigitMsb0ToBinaryWhenSwitchHexDigitCase_2_() {
        /* Branches:
         * (switch(hexDigit) = '2') : true
         */
        //Act Statement(s)
        boolean[] result = Conversion.hexDigitMsb0ToBinary('2');
        boolean[] booleanResultArray = new boolean[] { false, false, true, false };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${hexDigitMsb0ToBinaryWhenSwitchHexDigitCase_3_}, hash: 77C810EB4D54B6DE82E13389418354F6
    @Test()
    void hexDigitMsb0ToBinaryWhenSwitchHexDigitCase_3_() {
        /* Branches:
         * (switch(hexDigit) = '3') : true
         */
        //Act Statement(s)
        boolean[] result = Conversion.hexDigitMsb0ToBinary('3');
        boolean[] booleanResultArray = new boolean[] { false, false, true, true };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${hexDigitMsb0ToBinaryWhenSwitchHexDigitCase_4_}, hash: 434C65BA8D10440DF5E660FBC9FC08E2
    @Test()
    void hexDigitMsb0ToBinaryWhenSwitchHexDigitCase_4_() {
        /* Branches:
         * (switch(hexDigit) = '4') : true
         */
        //Act Statement(s)
        boolean[] result = Conversion.hexDigitMsb0ToBinary('4');
        boolean[] booleanResultArray = new boolean[] { false, true, false, false };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${hexDigitMsb0ToBinaryWhenSwitchHexDigitCase_5_}, hash: CF228251F976982B3B2D4F076A28AFAE
    @Test()
    void hexDigitMsb0ToBinaryWhenSwitchHexDigitCase_5_() {
        /* Branches:
         * (switch(hexDigit) = '5') : true
         */
        //Act Statement(s)
        boolean[] result = Conversion.hexDigitMsb0ToBinary('5');
        boolean[] booleanResultArray = new boolean[] { false, true, false, true };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${hexDigitMsb0ToBinaryWhenSwitchHexDigitCase_6_}, hash: FD44992841D6291BE06E13A9168D0D4D
    @Test()
    void hexDigitMsb0ToBinaryWhenSwitchHexDigitCase_6_() {
        /* Branches:
         * (switch(hexDigit) = '6') : true
         */
        //Act Statement(s)
        boolean[] result = Conversion.hexDigitMsb0ToBinary('6');
        boolean[] booleanResultArray = new boolean[] { false, true, true, false };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${hexDigitMsb0ToBinaryWhenSwitchHexDigitCase_7_}, hash: 49703F78D9DEE5D0BBC6C47168CEBD8D
    @Test()
    void hexDigitMsb0ToBinaryWhenSwitchHexDigitCase_7_() {
        /* Branches:
         * (switch(hexDigit) = '7') : true
         */
        //Act Statement(s)
        boolean[] result = Conversion.hexDigitMsb0ToBinary('7');
        boolean[] booleanResultArray = new boolean[] { false, true, true, true };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${hexDigitMsb0ToBinaryWhenSwitchHexDigitCase_8_}, hash: A620959291D4CFEF122A443A135E8F1F
    @Test()
    void hexDigitMsb0ToBinaryWhenSwitchHexDigitCase_8_() {
        /* Branches:
         * (switch(hexDigit) = '8') : true
         */
        //Act Statement(s)
        boolean[] result = Conversion.hexDigitMsb0ToBinary('8');
        boolean[] booleanResultArray = new boolean[] { true, false, false, false };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${hexDigitMsb0ToBinaryWhenSwitchHexDigitCase_9_}, hash: 85CF22857FFEE7D542C893A3BFC1460D
    @Test()
    void hexDigitMsb0ToBinaryWhenSwitchHexDigitCase_9_() {
        /* Branches:
         * (switch(hexDigit) = '9') : true
         */
        //Act Statement(s)
        boolean[] result = Conversion.hexDigitMsb0ToBinary('9');
        boolean[] booleanResultArray = new boolean[] { true, false, false, true };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${hexDigitMsb0ToBinaryWhenSwitchHexDigitCase_a_}, hash: ABFA9DE3A2D3F50C0F2006D01DB61C52
    @Test()
    void hexDigitMsb0ToBinaryWhenSwitchHexDigitCase_a_() {
        /* Branches:
         * (switch(hexDigit) = 'a' or switch(hexDigit) = 'A') : true
         */
        //Act Statement(s)
        boolean[] result = Conversion.hexDigitMsb0ToBinary('A');
        boolean[] booleanResultArray = new boolean[] { true, false, true, false };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${hexDigitMsb0ToBinaryWhenSwitchHexDigitCase_a_2}, hash: BE539949BF867C114C1BEC72C107FAF5
    @Test()
    void hexDigitMsb0ToBinaryWhenSwitchHexDigitCase_a_2() {
        /* Branches:
         * (switch(hexDigit) = 'a' or switch(hexDigit) = 'A') : true
         */
        //Act Statement(s)
        boolean[] result = Conversion.hexDigitMsb0ToBinary('B');
        boolean[] booleanResultArray = new boolean[] { true, false, true, true };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${hexDigitMsb0ToBinaryWhenSwitchHexDigitCase_b_}, hash: 50B8B5CB39BDD20F5F30061B4F439444
    @Test()
    void hexDigitMsb0ToBinaryWhenSwitchHexDigitCase_b_() {
        /* Branches:
         * (switch(hexDigit) = 'b' or switch(hexDigit) = 'B') : true
         */
        //Act Statement(s)
        boolean[] result = Conversion.hexDigitMsb0ToBinary('C');
        boolean[] booleanResultArray = new boolean[] { true, true, false, false };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${hexDigitMsb0ToBinaryWhenSwitchHexDigitCase_b_2}, hash: 063966A471BB9E29D2C229766B9EB0AD
    @Test()
    void hexDigitMsb0ToBinaryWhenSwitchHexDigitCase_b_2() {
        /* Branches:
         * (switch(hexDigit) = 'b' or switch(hexDigit) = 'B') : true
         */
        //Act Statement(s)
        boolean[] result = Conversion.hexDigitMsb0ToBinary('D');
        boolean[] booleanResultArray = new boolean[] { true, true, false, true };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${hexDigitMsb0ToBinaryWhenSwitchHexDigitCase_c_}, hash: D1EF45ABB4833D21328DA0BD6FFA3C35
    @Test()
    void hexDigitMsb0ToBinaryWhenSwitchHexDigitCase_c_() {
        /* Branches:
         * (switch(hexDigit) = 'c' or switch(hexDigit) = 'C') : true
         */
        //Act Statement(s)
        boolean[] result = Conversion.hexDigitMsb0ToBinary('E');
        boolean[] booleanResultArray = new boolean[] { true, true, true, false };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${hexDigitMsb0ToBinaryWhenSwitchHexDigitCase_c_2}, hash: 970F4EAF6FBB8E8CCE2420E785CEB4BF
    @Test()
    void hexDigitMsb0ToBinaryWhenSwitchHexDigitCase_c_2() {
        /* Branches:
         * (switch(hexDigit) = 'c' or switch(hexDigit) = 'C') : true
         */
        //Act Statement(s)
        boolean[] result = Conversion.hexDigitMsb0ToBinary('F');
        boolean[] booleanResultArray = new boolean[] { true, true, true, true };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${hexDigitMsb0ToBinaryWhenSwitchHexDigitCaseDefaultThrowsIllegalArgumentException}, hash: AD7FA7AFA0A986D67E8B04CD65F0DE87
    @Disabled()
    @Test()
    void hexDigitMsb0ToBinaryWhenSwitchHexDigitCaseDefaultThrowsIllegalArgumentException() {
        /* Branches:
         * (switch(hexDigit) = default) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("Cannot interpret 'A' as a hexadecimal digit");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.hexDigitMsb0ToBinary('A');
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${hexDigitMsb0ToIntWhenSwitchHexDigitCase_0_}, hash: 53A1438AEAD2F9B12875CB177F2177D9
    @Test()
    void hexDigitMsb0ToIntWhenSwitchHexDigitCase_0_() {
        /* Branches:
         * (switch(hexDigit) = '0') : true
         */
        //Act Statement(s)
        int result = Conversion.hexDigitMsb0ToInt('0');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${hexDigitMsb0ToIntWhenSwitchHexDigitCase_1_}, hash: FF641CC0D91414986F296FFA62E3B8DB
    @Test()
    void hexDigitMsb0ToIntWhenSwitchHexDigitCase_1_() {
        /* Branches:
         * (switch(hexDigit) = '1') : true
         */
        //Act Statement(s)
        int result = Conversion.hexDigitMsb0ToInt('1');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(8)));
    }

    //BaseRock generated method id: ${hexDigitMsb0ToIntWhenSwitchHexDigitCase_2_}, hash: 6AF85ED69F57BBFFFB3826B3E72E100C
    @Test()
    void hexDigitMsb0ToIntWhenSwitchHexDigitCase_2_() {
        /* Branches:
         * (switch(hexDigit) = '2') : true
         */
        //Act Statement(s)
        int result = Conversion.hexDigitMsb0ToInt('2');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(4)));
    }

    //BaseRock generated method id: ${hexDigitMsb0ToIntWhenSwitchHexDigitCase_3_}, hash: 5A062E0490F8A5EC1774592A93E1C344
    @Test()
    void hexDigitMsb0ToIntWhenSwitchHexDigitCase_3_() {
        /* Branches:
         * (switch(hexDigit) = '3') : true
         */
        //Act Statement(s)
        int result = Conversion.hexDigitMsb0ToInt('3');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(12)));
    }

    //BaseRock generated method id: ${hexDigitMsb0ToIntWhenSwitchHexDigitCase_4_}, hash: 9F61D350E8379679AC5EEDEF4A3790AE
    @Test()
    void hexDigitMsb0ToIntWhenSwitchHexDigitCase_4_() {
        /* Branches:
         * (switch(hexDigit) = '4') : true
         */
        //Act Statement(s)
        int result = Conversion.hexDigitMsb0ToInt('4');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(2)));
    }

    //BaseRock generated method id: ${hexDigitMsb0ToIntWhenSwitchHexDigitCase_5_}, hash: B385BD058B0E1FAED1AB95FD66F73D77
    @Test()
    void hexDigitMsb0ToIntWhenSwitchHexDigitCase_5_() {
        /* Branches:
         * (switch(hexDigit) = '5') : true
         */
        //Act Statement(s)
        int result = Conversion.hexDigitMsb0ToInt('5');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(10)));
    }

    //BaseRock generated method id: ${hexDigitMsb0ToIntWhenSwitchHexDigitCase_6_}, hash: D54B2B442B6B8D24215831BD11A12470
    @Test()
    void hexDigitMsb0ToIntWhenSwitchHexDigitCase_6_() {
        /* Branches:
         * (switch(hexDigit) = '6') : true
         */
        //Act Statement(s)
        int result = Conversion.hexDigitMsb0ToInt('6');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(6)));
    }

    //BaseRock generated method id: ${hexDigitMsb0ToIntWhenSwitchHexDigitCase_7_}, hash: 02903BE8E98C38B386510D0EDD42DCA0
    @Test()
    void hexDigitMsb0ToIntWhenSwitchHexDigitCase_7_() {
        /* Branches:
         * (switch(hexDigit) = '7') : true
         */
        //Act Statement(s)
        int result = Conversion.hexDigitMsb0ToInt('7');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(14)));
    }

    //BaseRock generated method id: ${hexDigitMsb0ToIntWhenSwitchHexDigitCase_8_}, hash: A723A1218B3B9040995848EC454C006E
    @Test()
    void hexDigitMsb0ToIntWhenSwitchHexDigitCase_8_() {
        /* Branches:
         * (switch(hexDigit) = '8') : true
         */
        //Act Statement(s)
        int result = Conversion.hexDigitMsb0ToInt('8');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1)));
    }

    //BaseRock generated method id: ${hexDigitMsb0ToIntWhenSwitchHexDigitCase_9_}, hash: B6D2C7AB3691E7253756AC19DF919F7C
    @Test()
    void hexDigitMsb0ToIntWhenSwitchHexDigitCase_9_() {
        /* Branches:
         * (switch(hexDigit) = '9') : true
         */
        //Act Statement(s)
        int result = Conversion.hexDigitMsb0ToInt('9');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(9)));
    }

    //BaseRock generated method id: ${hexDigitMsb0ToIntWhenSwitchHexDigitCase_a_}, hash: D06113CAA0F09991E8247AFD76C9C74D
    @Test()
    void hexDigitMsb0ToIntWhenSwitchHexDigitCase_a_() {
        /* Branches:
         * (switch(hexDigit) = 'a' or switch(hexDigit) = 'A') : true
         */
        //Act Statement(s)
        int result = Conversion.hexDigitMsb0ToInt('A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(5)));
    }

    //BaseRock generated method id: ${hexDigitMsb0ToIntWhenSwitchHexDigitCase_a_2}, hash: 89ADC928090A48A65A0E1FAEE0C40FCD
    @Test()
    void hexDigitMsb0ToIntWhenSwitchHexDigitCase_a_2() {
        /* Branches:
         * (switch(hexDigit) = 'a' or switch(hexDigit) = 'A') : true
         */
        //Act Statement(s)
        int result = Conversion.hexDigitMsb0ToInt('B');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(13)));
    }

    //BaseRock generated method id: ${hexDigitMsb0ToIntWhenSwitchHexDigitCase_b_}, hash: A62E66189ED36D3781302769101F95D3
    @Test()
    void hexDigitMsb0ToIntWhenSwitchHexDigitCase_b_() {
        /* Branches:
         * (switch(hexDigit) = 'b' or switch(hexDigit) = 'B') : true
         */
        //Act Statement(s)
        int result = Conversion.hexDigitMsb0ToInt('C');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(3)));
    }

    //BaseRock generated method id: ${hexDigitMsb0ToIntWhenSwitchHexDigitCase_b_2}, hash: 1FC8AA4328A461D65C36A7129C8ED2D1
    @Test()
    void hexDigitMsb0ToIntWhenSwitchHexDigitCase_b_2() {
        /* Branches:
         * (switch(hexDigit) = 'b' or switch(hexDigit) = 'B') : true
         */
        //Act Statement(s)
        int result = Conversion.hexDigitMsb0ToInt('D');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(11)));
    }

    //BaseRock generated method id: ${hexDigitMsb0ToIntWhenSwitchHexDigitCase_c_}, hash: 1830F8CBE3D4133D6A42E02BB5C0952D
    @Test()
    void hexDigitMsb0ToIntWhenSwitchHexDigitCase_c_() {
        /* Branches:
         * (switch(hexDigit) = 'c' or switch(hexDigit) = 'C') : true
         */
        //Act Statement(s)
        int result = Conversion.hexDigitMsb0ToInt('E');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(7)));
    }

    //BaseRock generated method id: ${hexDigitMsb0ToIntWhenSwitchHexDigitCase_c_2}, hash: DB3DE7CB1D6F59F446FAE95E41D10630
    @Test()
    void hexDigitMsb0ToIntWhenSwitchHexDigitCase_c_2() {
        /* Branches:
         * (switch(hexDigit) = 'c' or switch(hexDigit) = 'C') : true
         */
        //Act Statement(s)
        int result = Conversion.hexDigitMsb0ToInt('F');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(15)));
    }

    //BaseRock generated method id: ${hexDigitMsb0ToIntWhenSwitchHexDigitCaseDefaultThrowsIllegalArgumentException}, hash: 60ACE41BD73BB4B7DF85A64453D063EE
    @Disabled()
    @Test()
    void hexDigitMsb0ToIntWhenSwitchHexDigitCaseDefaultThrowsIllegalArgumentException() {
        /* Branches:
         * (switch(hexDigit) = default) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("Cannot interpret 'A' as a hexadecimal digit");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.hexDigitMsb0ToInt('A');
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${hexDigitToBinaryWhenSwitchHexDigitCase_0_}, hash: FA38A64B96200FC1C8AA5D6A9159FA16
    @Test()
    void hexDigitToBinaryWhenSwitchHexDigitCase_0_() {
        /* Branches:
         * (switch(hexDigit) = '0') : true
         */
        //Act Statement(s)
        boolean[] result = Conversion.hexDigitToBinary('0');
        boolean[] booleanResultArray = new boolean[] { false, false, false, false };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${hexDigitToBinaryWhenSwitchHexDigitCase_1_}, hash: 0283CD80C54914553B1AE7C2F62E68DE
    @Test()
    void hexDigitToBinaryWhenSwitchHexDigitCase_1_() {
        /* Branches:
         * (switch(hexDigit) = '1') : true
         */
        //Act Statement(s)
        boolean[] result = Conversion.hexDigitToBinary('1');
        boolean[] booleanResultArray = new boolean[] { true, false, false, false };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${hexDigitToBinaryWhenSwitchHexDigitCase_2_}, hash: 039A093465983BA72E50EB241EEA8DD2
    @Test()
    void hexDigitToBinaryWhenSwitchHexDigitCase_2_() {
        /* Branches:
         * (switch(hexDigit) = '2') : true
         */
        //Act Statement(s)
        boolean[] result = Conversion.hexDigitToBinary('2');
        boolean[] booleanResultArray = new boolean[] { false, true, false, false };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${hexDigitToBinaryWhenSwitchHexDigitCase_3_}, hash: 07381694F850B4722E298A8D57CA6947
    @Test()
    void hexDigitToBinaryWhenSwitchHexDigitCase_3_() {
        /* Branches:
         * (switch(hexDigit) = '3') : true
         */
        //Act Statement(s)
        boolean[] result = Conversion.hexDigitToBinary('3');
        boolean[] booleanResultArray = new boolean[] { true, true, false, false };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${hexDigitToBinaryWhenSwitchHexDigitCase_4_}, hash: 8E16D50DBFB0E1D47DB1BCC5EE313816
    @Test()
    void hexDigitToBinaryWhenSwitchHexDigitCase_4_() {
        /* Branches:
         * (switch(hexDigit) = '4') : true
         */
        //Act Statement(s)
        boolean[] result = Conversion.hexDigitToBinary('4');
        boolean[] booleanResultArray = new boolean[] { false, false, true, false };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${hexDigitToBinaryWhenSwitchHexDigitCase_5_}, hash: 48B355751C4037B696E06C2B51F6FFA1
    @Test()
    void hexDigitToBinaryWhenSwitchHexDigitCase_5_() {
        /* Branches:
         * (switch(hexDigit) = '5') : true
         */
        //Act Statement(s)
        boolean[] result = Conversion.hexDigitToBinary('5');
        boolean[] booleanResultArray = new boolean[] { true, false, true, false };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${hexDigitToBinaryWhenSwitchHexDigitCase_6_}, hash: 2DB9D88748E7E927C8E2D9395A10A50C
    @Test()
    void hexDigitToBinaryWhenSwitchHexDigitCase_6_() {
        /* Branches:
         * (switch(hexDigit) = '6') : true
         */
        //Act Statement(s)
        boolean[] result = Conversion.hexDigitToBinary('6');
        boolean[] booleanResultArray = new boolean[] { false, true, true, false };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${hexDigitToBinaryWhenSwitchHexDigitCase_7_}, hash: FB163DF983DD53D16BA6DAD7D43E9742
    @Test()
    void hexDigitToBinaryWhenSwitchHexDigitCase_7_() {
        /* Branches:
         * (switch(hexDigit) = '7') : true
         */
        //Act Statement(s)
        boolean[] result = Conversion.hexDigitToBinary('7');
        boolean[] booleanResultArray = new boolean[] { true, true, true, false };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${hexDigitToBinaryWhenSwitchHexDigitCase_8_}, hash: FC30AAC82A70EB7C0B7F937E3E7A9C4B
    @Test()
    void hexDigitToBinaryWhenSwitchHexDigitCase_8_() {
        /* Branches:
         * (switch(hexDigit) = '8') : true
         */
        //Act Statement(s)
        boolean[] result = Conversion.hexDigitToBinary('8');
        boolean[] booleanResultArray = new boolean[] { false, false, false, true };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${hexDigitToBinaryWhenSwitchHexDigitCase_9_}, hash: 5FF274E50129F4BEEDBF8819EEA30326
    @Test()
    void hexDigitToBinaryWhenSwitchHexDigitCase_9_() {
        /* Branches:
         * (switch(hexDigit) = '9') : true
         */
        //Act Statement(s)
        boolean[] result = Conversion.hexDigitToBinary('9');
        boolean[] booleanResultArray = new boolean[] { true, false, false, true };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${hexDigitToBinaryWhenSwitchHexDigitCase_a_}, hash: 52B301EDF4BE63134B3749E11EB9681E
    @Test()
    void hexDigitToBinaryWhenSwitchHexDigitCase_a_() {
        /* Branches:
         * (switch(hexDigit) = 'a' or switch(hexDigit) = 'A') : true
         */
        //Act Statement(s)
        boolean[] result = Conversion.hexDigitToBinary('A');
        boolean[] booleanResultArray = new boolean[] { false, true, false, true };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${hexDigitToBinaryWhenSwitchHexDigitCase_a_2}, hash: B8FD83BCAEFAE252F4594A692B5FAF8E
    @Test()
    void hexDigitToBinaryWhenSwitchHexDigitCase_a_2() {
        /* Branches:
         * (switch(hexDigit) = 'a' or switch(hexDigit) = 'A') : true
         */
        //Act Statement(s)
        boolean[] result = Conversion.hexDigitToBinary('B');
        boolean[] booleanResultArray = new boolean[] { true, true, false, true };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${hexDigitToBinaryWhenSwitchHexDigitCase_b_}, hash: D692088DFD6A7A71E574C6B55F5E8DC9
    @Test()
    void hexDigitToBinaryWhenSwitchHexDigitCase_b_() {
        /* Branches:
         * (switch(hexDigit) = 'b' or switch(hexDigit) = 'B') : true
         */
        //Act Statement(s)
        boolean[] result = Conversion.hexDigitToBinary('C');
        boolean[] booleanResultArray = new boolean[] { false, false, true, true };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${hexDigitToBinaryWhenSwitchHexDigitCase_b_2}, hash: 08264755708390ABA8C37F6AE8E50B62
    @Test()
    void hexDigitToBinaryWhenSwitchHexDigitCase_b_2() {
        /* Branches:
         * (switch(hexDigit) = 'b' or switch(hexDigit) = 'B') : true
         */
        //Act Statement(s)
        boolean[] result = Conversion.hexDigitToBinary('D');
        boolean[] booleanResultArray = new boolean[] { true, false, true, true };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${hexDigitToBinaryWhenSwitchHexDigitCase_c_}, hash: 299B153B8FE45775AFA3F6F28A004EC0
    @Test()
    void hexDigitToBinaryWhenSwitchHexDigitCase_c_() {
        /* Branches:
         * (switch(hexDigit) = 'c' or switch(hexDigit) = 'C') : true
         */
        //Act Statement(s)
        boolean[] result = Conversion.hexDigitToBinary('E');
        boolean[] booleanResultArray = new boolean[] { false, true, true, true };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${hexDigitToBinaryWhenSwitchHexDigitCase_c_2}, hash: F2AE6E623F6B0CFA01C1BF2D53F1C843
    @Test()
    void hexDigitToBinaryWhenSwitchHexDigitCase_c_2() {
        /* Branches:
         * (switch(hexDigit) = 'c' or switch(hexDigit) = 'C') : true
         */
        //Act Statement(s)
        boolean[] result = Conversion.hexDigitToBinary('F');
        boolean[] booleanResultArray = new boolean[] { true, true, true, true };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${hexDigitToBinaryWhenSwitchHexDigitCaseDefaultThrowsIllegalArgumentException}, hash: 3E5FEDEC651A0B7219CC5280CF09640F
    @Disabled()
    @Test()
    void hexDigitToBinaryWhenSwitchHexDigitCaseDefaultThrowsIllegalArgumentException() {
        /* Branches:
         * (switch(hexDigit) = default) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("Cannot interpret 'A' as a hexadecimal digit");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.hexDigitToBinary('A');
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${hexDigitToIntWhenDigitLessThan0ThrowsIllegalArgumentException}, hash: DC72A42B4376B94A97F6D7EEAF2F15BA
    @Disabled()
    @Test()
    void hexDigitToIntWhenDigitLessThan0ThrowsIllegalArgumentException() {
        /* Branches:
         * (digit < 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("s1");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.hexDigitToInt('A');
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${hexDigitToIntWhenDigitNotLessThan0}, hash: A5A44E1D4114A973DC532C7C6DC69A01
    @Test()
    void hexDigitToIntWhenDigitNotLessThan0() {
        /* Branches:
         * (digit < 0) : false
         */
        //Act Statement(s)
        int result = Conversion.hexDigitToInt('A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(10)));
    }

    //BaseRock generated method id: ${hexToByteWhen0EqualsNHex}, hash: DCBB7BCC911CABCB53B068DABAF419EE
    @Test()
    void hexToByteWhen0EqualsNHex() {
        /* Branches:
         * (0 == nHex) : true
         */
        //Act Statement(s)
        byte result = Conversion.hexToByte("src1", 0, (byte) 0, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((byte) 0)));
    }

    //BaseRock generated method id: ${hexToByteWhenNHexMinus1MultipliedBy4PlusDstPosGreaterThanOrEqualsTo8ThrowsIllegalArgumentException}, hash: 4F778907776A4EE84182B9B030706C80
    @Test()
    void hexToByteWhenNHexMinus1MultipliedBy4PlusDstPosGreaterThanOrEqualsTo8ThrowsIllegalArgumentException() {
        /* Branches:
         * (0 == nHex) : false
         * ((nHex - 1) * 4 + dstPos >= 8) : true
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("(nHex-1)*4+dstPos is greater than or equal to 8");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.hexToByte("src1", 0, (byte) 0, 16, -1);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${hexToByteWhenILessThanNHex}, hash: 56049350CF2E0E993B25954DBD0FE996
    @Test()
    void hexToByteWhenILessThanNHex() {
        /* Branches:
         * (0 == nHex) : false
         * ((nHex - 1) * 4 + dstPos >= 8) : false
         * (i < nHex) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<Conversion> conversion = mockStatic(Conversion.class, CALLS_REAL_METHODS)) {
            conversion.when(() -> Conversion.hexDigitToInt('B')).thenReturn(0);
            //Act Statement(s)
            byte result = Conversion.hexToByte("B", 0, (byte) 0, 0, 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo((byte) 0));
                conversion.verify(() -> Conversion.hexDigitToInt('B'), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${hexToIntWhen0EqualsNHex}, hash: 2E2A4603C566FE6665D8F31AF77429BE
    @Test()
    void hexToIntWhen0EqualsNHex() {
        /* Branches:
         * (0 == nHex) : true
         */
        //Act Statement(s)
        int result = Conversion.hexToInt("src1", 0, 0, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${hexToIntWhenNHexMinus1MultipliedBy4PlusDstPosGreaterThanOrEqualsTo32ThrowsIllegalArgumentException}, hash: B5D0B2348195625EBB7390D32E73624A
    @Test()
    void hexToIntWhenNHexMinus1MultipliedBy4PlusDstPosGreaterThanOrEqualsTo32ThrowsIllegalArgumentException() {
        /* Branches:
         * (0 == nHex) : false
         * ((nHex - 1) * 4 + dstPos >= 32) : true
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("(nHexs-1)*4+dstPos is greater or equal to than 32");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.hexToInt("src1", 0, 0, 40, -1);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${hexToIntWhenILessThanNHex}, hash: 18443B3DBD803BBD94D68AD550C72479
    @Test()
    void hexToIntWhenILessThanNHex() {
        /* Branches:
         * (0 == nHex) : false
         * ((nHex - 1) * 4 + dstPos >= 32) : false
         * (i < nHex) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<Conversion> conversion = mockStatic(Conversion.class, CALLS_REAL_METHODS)) {
            conversion.when(() -> Conversion.hexDigitToInt('B')).thenReturn(0);
            //Act Statement(s)
            int result = Conversion.hexToInt("B", 0, 0, 0, 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                conversion.verify(() -> Conversion.hexDigitToInt('B'), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${hexToLongWhen0EqualsNHex}, hash: 1AD5FF4F05CAAC03E83A2E2E255EB252
    @Test()
    void hexToLongWhen0EqualsNHex() {
        /* Branches:
         * (0 == nHex) : true
         */
        //Act Statement(s)
        long result = Conversion.hexToLong("src1", 0, 0L, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0L)));
    }

    //BaseRock generated method id: ${hexToLongWhenNHexMinus1MultipliedBy4PlusDstPosGreaterThanOrEqualsTo64ThrowsIllegalArgumentException}, hash: 60B197A0551AE2E84C36DF521B78DB3A
    @Test()
    void hexToLongWhenNHexMinus1MultipliedBy4PlusDstPosGreaterThanOrEqualsTo64ThrowsIllegalArgumentException() {
        /* Branches:
         * (0 == nHex) : false
         * ((nHex - 1) * 4 + dstPos >= 64) : true
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("(nHexs-1)*4+dstPos is greater or equal to than 64");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.hexToLong("src1", 0, 0L, 72, -1);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${hexToLongWhenILessThanNHex}, hash: C9A229DA1A465E02EA82BEA26F31FC98
    @Test()
    void hexToLongWhenILessThanNHex() {
        /* Branches:
         * (0 == nHex) : false
         * ((nHex - 1) * 4 + dstPos >= 64) : false
         * (i < nHex) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<Conversion> conversion = mockStatic(Conversion.class, CALLS_REAL_METHODS)) {
            conversion.when(() -> Conversion.hexDigitToInt('B')).thenReturn(0);
            //Act Statement(s)
            long result = Conversion.hexToLong("B", 0, 0L, 0, 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0L));
                conversion.verify(() -> Conversion.hexDigitToInt('B'), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${hexToShortWhen0EqualsNHex}, hash: 87F15893488811150594CC4D6736690D
    @Test()
    void hexToShortWhen0EqualsNHex() {
        /* Branches:
         * (0 == nHex) : true
         */
        //Act Statement(s)
        short result = Conversion.hexToShort("src1", 0, (short) 0, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((short) 0)));
    }

    //BaseRock generated method id: ${hexToShortWhenNHexMinus1MultipliedBy4PlusDstPosGreaterThanOrEqualsTo16ThrowsIllegalArgumentException}, hash: CA0932190F862FF6145092D664754090
    @Test()
    void hexToShortWhenNHexMinus1MultipliedBy4PlusDstPosGreaterThanOrEqualsTo16ThrowsIllegalArgumentException() {
        /* Branches:
         * (0 == nHex) : false
         * ((nHex - 1) * 4 + dstPos >= 16) : true
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("(nHexs-1)*4+dstPos is greater or equal to than 16");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.hexToShort("src1", 0, (short) 0, 24, -1);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${hexToShortWhenILessThanNHex}, hash: 7EB8CD81A4570806023372A74365A45A
    @Test()
    void hexToShortWhenILessThanNHex() {
        /* Branches:
         * (0 == nHex) : false
         * ((nHex - 1) * 4 + dstPos >= 16) : false
         * (i < nHex) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<Conversion> conversion = mockStatic(Conversion.class, CALLS_REAL_METHODS)) {
            conversion.when(() -> Conversion.hexDigitToInt('B')).thenReturn(0);
            //Act Statement(s)
            short result = Conversion.hexToShort("B", 0, (short) 0, 0, 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo((short) 0));
                conversion.verify(() -> Conversion.hexDigitToInt('B'), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${intArrayToLongWhenSrcPosNotEquals0And0EqualsNInts}, hash: 267152E3388D829C1279775CDF78B158
    @Test()
    void intArrayToLongWhenSrcPosNotEquals0And0EqualsNInts() {
        /* Branches:
         * (src.length == 0) : true
         * (srcPos == 0) : false
         * (0 == nInts) : true
         */
        //Arrange Statement(s)
        int[] intArray = new int[] {};
        //Act Statement(s)
        long result = Conversion.intArrayToLong(intArray, 1, 0L, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0L)));
    }

    //BaseRock generated method id: ${intArrayToLongWhen0NotEqualsNIntsAndNIntsMinus1MultipliedBy32PlusDstPosGreaterThanOrEqualThrowsIllegalArgumentException}, hash: 88A8CB142C9FD1ABBA895A01EBA1DA41
    @Test()
    void intArrayToLongWhen0NotEqualsNIntsAndNIntsMinus1MultipliedBy32PlusDstPosGreaterThanOrEqualThrowsIllegalArgumentException() {
        /* Branches:
         * (src.length == 0) : true
         * (srcPos == 0) : false
         * (0 == nInts) : false
         * ((nInts - 1) * 32 + dstPos >= 64) : true
         */
        //Arrange Statement(s)
        int[] intArray = new int[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("(nInts-1)*32+dstPos is greater or equal to than 64");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.intArrayToLong(intArray, 1, 0L, 64, 1);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${intArrayToLongWhenNIntsMinus1MultipliedBy32PlusDstPosLessThan64AndILessThanNInts}, hash: 3645EF563E3FD9A5508CBAFA3BE55D5D
    @Test()
    void intArrayToLongWhenNIntsMinus1MultipliedBy32PlusDstPosLessThan64AndILessThanNInts() {
        /* Branches:
         * (src.length == 0) : true
         * (srcPos == 0) : false
         * (0 == nInts) : false
         * ((nInts - 1) * 32 + dstPos >= 64) : false
         * (i < nInts) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        int[] intArray = new int[] {};
        //Act Statement(s)
        long result = Conversion.intArrayToLong(intArray, 0, 0L, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0L)));
    }

    //BaseRock generated method id: ${intToBinaryWhen0EqualsNBools}, hash: 5F6CC9B168F411E57D8D0D57471D49C1
    @Test()
    void intToBinaryWhen0EqualsNBools() {
        /* Branches:
         * (0 == nBools) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        //Act Statement(s)
        boolean[] result = Conversion.intToBinary(0, 0, booleanArray, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanArray)));
    }

    //BaseRock generated method id: ${intToBinaryWhenNBoolsMinus1PlusSrcPosGreaterThanOrEqualsTo32ThrowsIllegalArgumentException}, hash: FBFD1E2DF653FA4A183DD2217388F21E
    @Test()
    void intToBinaryWhenNBoolsMinus1PlusSrcPosGreaterThanOrEqualsTo32ThrowsIllegalArgumentException() {
        /* Branches:
         * (0 == nBools) : false
         * (nBools - 1 + srcPos >= 32) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("nBools-1+srcPos is greater or equal to than 32");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.intToBinary(0, 34, booleanArray, 0, -1);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${intToBinaryWhenDstPosPlusI}, hash: D307AE617DFFA3FC17B8B67091D7DF5F
    @Disabled()
    @Test()
    void intToBinaryWhenDstPosPlusI() {
        /* Branches:
         * (0 == nBools) : false
         * (nBools - 1 + srcPos >= 32) : false
         * (i < nBools) : true
         * (dstPos + i) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false };
        //Act Statement(s)
        boolean[] result = Conversion.intToBinary(0, 0, booleanArray, 0, 1);
        boolean[] booleanResultArray = new boolean[] { true };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${intToBinaryWhenDstPosNotPlusI}, hash: 7789C7E4E466F0D598FC8B6AFF5266E9
    @Test()
    void intToBinaryWhenDstPosNotPlusI() {
        /* Branches:
         * (0 == nBools) : false
         * (nBools - 1 + srcPos >= 32) : false
         * (i < nBools) : true
         * (dstPos + i) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false };
        //Act Statement(s)
        boolean[] result = Conversion.intToBinary(0, 0, booleanArray, 0, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanArray)));
    }

    //BaseRock generated method id: ${intToByteArrayWhen0EqualsNBytes}, hash: 3634E72302F28B5D2EAF8DE0F4B3D2DE
    @Test()
    void intToByteArrayWhen0EqualsNBytes() {
        /* Branches:
         * (0 == nBytes) : true
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        //Act Statement(s)
        byte[] result = Conversion.intToByteArray(0, 0, byteArray, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(byteArray)));
    }

    //BaseRock generated method id: ${intToByteArrayWhenNBytesMinus1MultipliedBy8PlusSrcPosGreaterThanOrEqualsTo32ThrowsIllegalArgumentException}, hash: 526107450141C08E2043641FD6A720B4
    @Test()
    void intToByteArrayWhenNBytesMinus1MultipliedBy8PlusSrcPosGreaterThanOrEqualsTo32ThrowsIllegalArgumentException() {
        /* Branches:
         * (0 == nBytes) : false
         * ((nBytes - 1) * 8 + srcPos >= 32) : true
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("(nBytes-1)*8+srcPos is greater or equal to than 32");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.intToByteArray(0, 48, byteArray, 0, -1);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${intToByteArrayWhenILessThanNBytes}, hash: 086FA7E85859F5C3762399D982C039EE
    @Test()
    void intToByteArrayWhenILessThanNBytes() {
        /* Branches:
         * (0 == nBytes) : false
         * ((nBytes - 1) * 8 + srcPos >= 32) : false
         * (i < nBytes) : true
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] { (byte) 0 };
        //Act Statement(s)
        byte[] result = Conversion.intToByteArray(0, 0, byteArray, 0, 1);
        byte[] byteResultArray = new byte[] { (byte) 0 };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(byteResultArray)));
    }

    //BaseRock generated method id: ${intToHexWhen0EqualsNHexs}, hash: A52074CF305F40274C5F5739DA582C4A
    @Test()
    void intToHexWhen0EqualsNHexs() {
        /* Branches:
         * (0 == nHexs) : true
         */
        //Act Statement(s)
        String result = Conversion.intToHex(0, 0, "dstInit1", 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("dstInit1")));
    }

    //BaseRock generated method id: ${intToHexWhenNHexsMinus1MultipliedBy4PlusSrcPosGreaterThanOrEqualsTo32ThrowsIllegalArgumentException}, hash: DC5E34C1975384A9CF28924C052C28BE
    @Test()
    void intToHexWhenNHexsMinus1MultipliedBy4PlusSrcPosGreaterThanOrEqualsTo32ThrowsIllegalArgumentException() {
        /* Branches:
         * (0 == nHexs) : false
         * ((nHexs - 1) * 4 + srcPos >= 32) : true
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("(nHexs-1)*4+srcPos is greater or equal to than 32");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.intToHex(0, 40, "dstInit1", 0, -1);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${intToHexWhenDstPosPlusIEqualsAppend}, hash: 6B5BCFE0DB7A33DE299EA950FBC6BF02
    @Test()
    void intToHexWhenDstPosPlusIEqualsAppend() {
        /* Branches:
         * (0 == nHexs) : false
         * ((nHexs - 1) * 4 + srcPos >= 32) : false
         * (i < nHexs) : true
         * (dstPos + i == append) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<Conversion> conversion = mockStatic(Conversion.class, CALLS_REAL_METHODS)) {
            conversion.when(() -> Conversion.intToHexDigit(0)).thenReturn('A');
            //Act Statement(s)
            String result = Conversion.intToHex(0, 1, "B", 1, 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("BA"));
                conversion.verify(() -> Conversion.intToHexDigit(0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${intToHexWhenDstPosPlusINotEqualsAppend}, hash: 980C9042B776460EFDB5A3DA338B4C94
    @Disabled()
    @Test()
    void intToHexWhenDstPosPlusINotEqualsAppend() {
        /* Branches:
         * (0 == nHexs) : false
         * ((nHexs - 1) * 4 + srcPos >= 32) : false
         * (i < nHexs) : true
         * (dstPos + i == append) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<Conversion> conversion = mockStatic(Conversion.class, CALLS_REAL_METHODS)) {
            conversion.when(() -> Conversion.intToHexDigit(0)).thenReturn('A');
            //Act Statement(s)
            String result = Conversion.intToHex(0, 1, "A", 2, 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("A"));
                conversion.verify(() -> Conversion.intToHexDigit(0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${intToHexDigitWhenCEqualsCharacterMIN_VALUEThrowsIllegalArgumentException}, hash: F90118B7CE1FE45C79F62672991F9A2E
    @Disabled()
    @Test()
    void intToHexDigitWhenCEqualsCharacterMIN_VALUEThrowsIllegalArgumentException() {
        /* Branches:
         * (c == Character.MIN_VALUE) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("s1");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.intToHexDigit(0);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${intToHexDigitWhenCNotEqualsCharacterMIN_VALUE}, hash: DE5C6E23D6ACBFA31DFD732571971365
    @Test()
    void intToHexDigitWhenCNotEqualsCharacterMIN_VALUE() {
        /* Branches:
         * (c == Character.MIN_VALUE) : false
         */
        //Act Statement(s)
        char result = Conversion.intToHexDigit(1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('1')));
    }

    //BaseRock generated method id: ${intToHexDigitMsb0WhenSwitchNibbleCase0}, hash: 0886F22D4BF33B18EFEDFD904741EF15
    @Test()
    void intToHexDigitMsb0WhenSwitchNibbleCase0() {
        /* Branches:
         * (switch(nibble) = 0x0) : true
         */
        //Act Statement(s)
        char result = Conversion.intToHexDigitMsb0(0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('0')));
    }

    //BaseRock generated method id: ${intToHexDigitMsb0WhenSwitchNibbleCase1}, hash: BF4E2CC6D916946904B88B68DDE74F32
    @Test()
    void intToHexDigitMsb0WhenSwitchNibbleCase1() {
        /* Branches:
         * (switch(nibble) = 0x1) : true
         */
        //Act Statement(s)
        char result = Conversion.intToHexDigitMsb0(1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('8')));
    }

    //BaseRock generated method id: ${intToHexDigitMsb0WhenSwitchNibbleCase2}, hash: 2E93317653B9F7BDB266CF395C19EFA1
    @Test()
    void intToHexDigitMsb0WhenSwitchNibbleCase2() {
        /* Branches:
         * (switch(nibble) = 0x2) : true
         */
        //Act Statement(s)
        char result = Conversion.intToHexDigitMsb0(2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('4')));
    }

    //BaseRock generated method id: ${intToHexDigitMsb0WhenSwitchNibbleCase3}, hash: 72759AA3CE15E02F71FEAD82F6FB916C
    @Test()
    void intToHexDigitMsb0WhenSwitchNibbleCase3() {
        /* Branches:
         * (switch(nibble) = 0x3) : true
         */
        //Act Statement(s)
        char result = Conversion.intToHexDigitMsb0(3);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('c')));
    }

    //BaseRock generated method id: ${intToHexDigitMsb0WhenSwitchNibbleCase4}, hash: A865E0A8A53F0A780368156D3CE23C36
    @Test()
    void intToHexDigitMsb0WhenSwitchNibbleCase4() {
        /* Branches:
         * (switch(nibble) = 0x4) : true
         */
        //Act Statement(s)
        char result = Conversion.intToHexDigitMsb0(4);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('2')));
    }

    //BaseRock generated method id: ${intToHexDigitMsb0WhenSwitchNibbleCase5}, hash: 07E4B1904ECBB20A2E6140F14FEB609A
    @Test()
    void intToHexDigitMsb0WhenSwitchNibbleCase5() {
        /* Branches:
         * (switch(nibble) = 0x5) : true
         */
        //Act Statement(s)
        char result = Conversion.intToHexDigitMsb0(5);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('a')));
    }

    //BaseRock generated method id: ${intToHexDigitMsb0WhenSwitchNibbleCase6}, hash: EB42D2C27A18A8FC4E88A8F3E510F091
    @Test()
    void intToHexDigitMsb0WhenSwitchNibbleCase6() {
        /* Branches:
         * (switch(nibble) = 0x6) : true
         */
        //Act Statement(s)
        char result = Conversion.intToHexDigitMsb0(6);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('6')));
    }

    //BaseRock generated method id: ${intToHexDigitMsb0WhenSwitchNibbleCase7}, hash: 220494E98271B441578B23B934012423
    @Test()
    void intToHexDigitMsb0WhenSwitchNibbleCase7() {
        /* Branches:
         * (switch(nibble) = 0x7) : true
         */
        //Act Statement(s)
        char result = Conversion.intToHexDigitMsb0(7);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('e')));
    }

    //BaseRock generated method id: ${intToHexDigitMsb0WhenSwitchNibbleCase8}, hash: 78D182B73438344AD722305617AB5BE4
    @Test()
    void intToHexDigitMsb0WhenSwitchNibbleCase8() {
        /* Branches:
         * (switch(nibble) = 0x8) : true
         */
        //Act Statement(s)
        char result = Conversion.intToHexDigitMsb0(8);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('1')));
    }

    //BaseRock generated method id: ${intToHexDigitMsb0WhenSwitchNibbleCase9}, hash: 8B03680B87BDF14C550A0CB1F02F6B6C
    @Test()
    void intToHexDigitMsb0WhenSwitchNibbleCase9() {
        /* Branches:
         * (switch(nibble) = 0x9) : true
         */
        //Act Statement(s)
        char result = Conversion.intToHexDigitMsb0(9);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('9')));
    }

    //BaseRock generated method id: ${intToHexDigitMsb0WhenSwitchNibbleCase10}, hash: B624FD5736E2C7EBD0E27E4DC7CC6BF4
    @Test()
    void intToHexDigitMsb0WhenSwitchNibbleCase10() {
        /* Branches:
         * (switch(nibble) = 0xA) : true
         */
        //Act Statement(s)
        char result = Conversion.intToHexDigitMsb0(10);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('5')));
    }

    //BaseRock generated method id: ${intToHexDigitMsb0WhenSwitchNibbleCase11}, hash: F8E34C48DF71C54079F3FFFE4CA9ACEE
    @Test()
    void intToHexDigitMsb0WhenSwitchNibbleCase11() {
        /* Branches:
         * (switch(nibble) = 0xB) : true
         */
        //Act Statement(s)
        char result = Conversion.intToHexDigitMsb0(11);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('d')));
    }

    //BaseRock generated method id: ${intToHexDigitMsb0WhenSwitchNibbleCase12}, hash: 481E66AC9681DF886E57D7DAF3606202
    @Test()
    void intToHexDigitMsb0WhenSwitchNibbleCase12() {
        /* Branches:
         * (switch(nibble) = 0xC) : true
         */
        //Act Statement(s)
        char result = Conversion.intToHexDigitMsb0(12);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('3')));
    }

    //BaseRock generated method id: ${intToHexDigitMsb0WhenSwitchNibbleCase13}, hash: 74CFC5F68C60D26ECBA3DAC5A0B87841
    @Test()
    void intToHexDigitMsb0WhenSwitchNibbleCase13() {
        /* Branches:
         * (switch(nibble) = 0xD) : true
         */
        //Act Statement(s)
        char result = Conversion.intToHexDigitMsb0(13);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('b')));
    }

    //BaseRock generated method id: ${intToHexDigitMsb0WhenSwitchNibbleCase14}, hash: 0F0B1563740B72DC0ADE5617D15554B4
    @Test()
    void intToHexDigitMsb0WhenSwitchNibbleCase14() {
        /* Branches:
         * (switch(nibble) = 0xE) : true
         */
        //Act Statement(s)
        char result = Conversion.intToHexDigitMsb0(14);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('7')));
    }

    //BaseRock generated method id: ${intToHexDigitMsb0WhenSwitchNibbleCase15}, hash: 60EE2ADE477722C26F4BADBD739910D3
    @Test()
    void intToHexDigitMsb0WhenSwitchNibbleCase15() {
        /* Branches:
         * (switch(nibble) = 0xF) : true
         */
        //Act Statement(s)
        char result = Conversion.intToHexDigitMsb0(15);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('f')));
    }

    //BaseRock generated method id: ${intToHexDigitMsb0WhenSwitchNibbleCaseDefaultThrowsIllegalArgumentException}, hash: 26E3E06C3B317D833B11CA4B82A0CCA0
    @Disabled()
    @Test()
    void intToHexDigitMsb0WhenSwitchNibbleCaseDefaultThrowsIllegalArgumentException() {
        /* Branches:
         * (switch(nibble) = default) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("nibble value not between 0 and 15: 2");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.intToHexDigitMsb0(2);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${intToShortArrayWhen0EqualsNShorts}, hash: 8AEAE9637C8B1321F88386F01B564B64
    @Test()
    void intToShortArrayWhen0EqualsNShorts() {
        /* Branches:
         * (0 == nShorts) : true
         */
        //Arrange Statement(s)
        short[] shortArray = new short[] {};
        //Act Statement(s)
        short[] result = Conversion.intToShortArray(0, 0, shortArray, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(shortArray)));
    }

    //BaseRock generated method id: ${intToShortArrayWhenNShortsMinus1MultipliedBy16PlusSrcPosGreaterThanOrEqualsTo32ThrowsIllegalArgumentException}, hash: F32D40DDC02A19F62F87282933DD3542
    @Test()
    void intToShortArrayWhenNShortsMinus1MultipliedBy16PlusSrcPosGreaterThanOrEqualsTo32ThrowsIllegalArgumentException() {
        /* Branches:
         * (0 == nShorts) : false
         * ((nShorts - 1) * 16 + srcPos >= 32) : true
         */
        //Arrange Statement(s)
        short[] shortArray = new short[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("(nShorts-1)*16+srcPos is greater or equal to than 32");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.intToShortArray(0, 64, shortArray, 0, -1);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${intToShortArrayWhenILessThanNShorts}, hash: 0E867BB33B7D75FB1DA2D450736EE8B0
    @Test()
    void intToShortArrayWhenILessThanNShorts() {
        /* Branches:
         * (0 == nShorts) : false
         * ((nShorts - 1) * 16 + srcPos >= 32) : false
         * (i < nShorts) : true
         */
        //Arrange Statement(s)
        short[] shortArray = new short[] { (short) 0 };
        //Act Statement(s)
        short[] result = Conversion.intToShortArray(0, 0, shortArray, 0, 1);
        short[] shortResultArray = new short[] { (short) 0 };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(shortResultArray)));
    }

    //BaseRock generated method id: ${longToBinaryWhen0EqualsNBools}, hash: 5E7B2E7D68DF6C6EDB3E2BFB808380E8
    @Test()
    void longToBinaryWhen0EqualsNBools() {
        /* Branches:
         * (0 == nBools) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        //Act Statement(s)
        boolean[] result = Conversion.longToBinary(0L, 0, booleanArray, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanArray)));
    }

    //BaseRock generated method id: ${longToBinaryWhenNBoolsMinus1PlusSrcPosGreaterThanOrEqualsTo64ThrowsIllegalArgumentException}, hash: 7673388C25558F667CF4F2C25CB5DED9
    @Test()
    void longToBinaryWhenNBoolsMinus1PlusSrcPosGreaterThanOrEqualsTo64ThrowsIllegalArgumentException() {
        /* Branches:
         * (0 == nBools) : false
         * (nBools - 1 + srcPos >= 64) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("nBools-1+srcPos is greater or equal to than 64");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.longToBinary(0L, 66, booleanArray, 0, -1);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${longToBinaryWhenDstPosPlusI}, hash: 381589F92AF459A2760413806F7B1BBD
    @Disabled()
    @Test()
    void longToBinaryWhenDstPosPlusI() {
        /* Branches:
         * (0 == nBools) : false
         * (nBools - 1 + srcPos >= 64) : false
         * (i < nBools) : true
         * (dstPos + i) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false };
        //Act Statement(s)
        boolean[] result = Conversion.longToBinary(0L, 0, booleanArray, 0, 1);
        boolean[] booleanResultArray = new boolean[] { true };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${longToBinaryWhenDstPosNotPlusI}, hash: E0226539E6ED43F5564D02FEC01C4B93
    @Test()
    void longToBinaryWhenDstPosNotPlusI() {
        /* Branches:
         * (0 == nBools) : false
         * (nBools - 1 + srcPos >= 64) : false
         * (i < nBools) : true
         * (dstPos + i) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false };
        //Act Statement(s)
        boolean[] result = Conversion.longToBinary(0L, 0, booleanArray, 0, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanArray)));
    }

    //BaseRock generated method id: ${longToByteArrayWhen0EqualsNBytes}, hash: 299AFDC994D11ADFD546109FAEBD99C9
    @Test()
    void longToByteArrayWhen0EqualsNBytes() {
        /* Branches:
         * (0 == nBytes) : true
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        //Act Statement(s)
        byte[] result = Conversion.longToByteArray(0L, 0, byteArray, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(byteArray)));
    }

    //BaseRock generated method id: ${longToByteArrayWhenNBytesMinus1MultipliedBy8PlusSrcPosGreaterThanOrEqualsTo64ThrowsIllegalArgumentException}, hash: 97EE4C0697AE1DE21E62A0737F3CABFC
    @Test()
    void longToByteArrayWhenNBytesMinus1MultipliedBy8PlusSrcPosGreaterThanOrEqualsTo64ThrowsIllegalArgumentException() {
        /* Branches:
         * (0 == nBytes) : false
         * ((nBytes - 1) * 8 + srcPos >= 64) : true
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("(nBytes-1)*8+srcPos is greater or equal to than 64");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.longToByteArray(0L, 80, byteArray, 0, -1);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${longToByteArrayWhenILessThanNBytes}, hash: BC5B084B47B249D2EB80859A74357CA4
    @Test()
    void longToByteArrayWhenILessThanNBytes() {
        /* Branches:
         * (0 == nBytes) : false
         * ((nBytes - 1) * 8 + srcPos >= 64) : false
         * (i < nBytes) : true
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] { (byte) 0 };
        //Act Statement(s)
        byte[] result = Conversion.longToByteArray(0L, 0, byteArray, 0, 1);
        byte[] byteResultArray = new byte[] { (byte) 0 };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(byteResultArray)));
    }

    //BaseRock generated method id: ${longToHexWhen0EqualsNHexs}, hash: 2A1E6A6C4A24741BC9C8BDEC17108EB9
    @Test()
    void longToHexWhen0EqualsNHexs() {
        /* Branches:
         * (0 == nHexs) : true
         */
        //Act Statement(s)
        String result = Conversion.longToHex(0L, 0, "dstInit1", 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("dstInit1")));
    }

    //BaseRock generated method id: ${longToHexWhenNHexsMinus1MultipliedBy4PlusSrcPosGreaterThanOrEqualsTo64ThrowsIllegalArgumentException}, hash: 19710D54D9CF9ABCBAB9C910CDD5C9E0
    @Test()
    void longToHexWhenNHexsMinus1MultipliedBy4PlusSrcPosGreaterThanOrEqualsTo64ThrowsIllegalArgumentException() {
        /* Branches:
         * (0 == nHexs) : false
         * ((nHexs - 1) * 4 + srcPos >= 64) : true
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("(nHexs-1)*4+srcPos is greater or equal to than 64");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.longToHex(0L, 72, "dstInit1", 0, -1);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${longToHexWhenDstPosPlusIEqualsAppend}, hash: FEE9A70021DA5A930CA18BBB00C97CED
    @Test()
    void longToHexWhenDstPosPlusIEqualsAppend() {
        /* Branches:
         * (0 == nHexs) : false
         * ((nHexs - 1) * 4 + srcPos >= 64) : false
         * (i < nHexs) : true
         * (dstPos + i == append) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<Conversion> conversion = mockStatic(Conversion.class, CALLS_REAL_METHODS)) {
            conversion.when(() -> Conversion.intToHexDigit(0)).thenReturn('A');
            //Act Statement(s)
            String result = Conversion.longToHex(0L, 1, "B", 1, 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("BA"));
                conversion.verify(() -> Conversion.intToHexDigit(0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${longToHexWhenDstPosPlusINotEqualsAppend}, hash: FDA133E62540B3A96618665C4D82254D
    @Disabled()
    @Test()
    void longToHexWhenDstPosPlusINotEqualsAppend() {
        /* Branches:
         * (0 == nHexs) : false
         * ((nHexs - 1) * 4 + srcPos >= 64) : false
         * (i < nHexs) : true
         * (dstPos + i == append) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<Conversion> conversion = mockStatic(Conversion.class, CALLS_REAL_METHODS)) {
            conversion.when(() -> Conversion.intToHexDigit(1)).thenReturn('A');
            //Act Statement(s)
            String result = Conversion.longToHex(0L, 1, "A", 2, 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("A"));
                conversion.verify(() -> Conversion.intToHexDigit(1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${longToIntArrayWhen0EqualsNInts}, hash: 988188EBF1934883FBB18B52F1FC2C7C
    @Test()
    void longToIntArrayWhen0EqualsNInts() {
        /* Branches:
         * (0 == nInts) : true
         */
        //Arrange Statement(s)
        int[] intArray = new int[] {};
        //Act Statement(s)
        int[] result = Conversion.longToIntArray(0L, 0, intArray, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(intArray)));
    }

    //BaseRock generated method id: ${longToIntArrayWhenNIntsMinus1MultipliedBy32PlusSrcPosGreaterThanOrEqualsTo64ThrowsIllegalArgumentException}, hash: E97741E90DF6D55A5D147B82B085DE49
    @Test()
    void longToIntArrayWhenNIntsMinus1MultipliedBy32PlusSrcPosGreaterThanOrEqualsTo64ThrowsIllegalArgumentException() {
        /* Branches:
         * (0 == nInts) : false
         * ((nInts - 1) * 32 + srcPos >= 64) : true
         */
        //Arrange Statement(s)
        int[] intArray = new int[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("(nInts-1)*32+srcPos is greater or equal to than 64");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.longToIntArray(0L, 128, intArray, 0, -1);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${longToIntArrayWhenILessThanNInts}, hash: 71F09BB85AA408C39B4C738F92CABA63
    @Test()
    void longToIntArrayWhenILessThanNInts() {
        /* Branches:
         * (0 == nInts) : false
         * ((nInts - 1) * 32 + srcPos >= 64) : false
         * (i < nInts) : true
         */
        //Arrange Statement(s)
        int[] intArray = new int[] { 0 };
        //Act Statement(s)
        int[] result = Conversion.longToIntArray(0L, 0, intArray, 0, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(intArray)));
    }

    //BaseRock generated method id: ${longToShortArrayWhen0EqualsNShorts}, hash: 219E8FDAB33DC40C236FFF7F44CA359C
    @Test()
    void longToShortArrayWhen0EqualsNShorts() {
        /* Branches:
         * (0 == nShorts) : true
         */
        //Arrange Statement(s)
        short[] shortArray = new short[] {};
        //Act Statement(s)
        short[] result = Conversion.longToShortArray(0L, 0, shortArray, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(shortArray)));
    }

    //BaseRock generated method id: ${longToShortArrayWhenNShortsMinus1MultipliedBy16PlusSrcPosGreaterThanOrEqualsTo64ThrowsIllegalArgumentException}, hash: 58A2AAA951630E99E538F3C16027CE91
    @Test()
    void longToShortArrayWhenNShortsMinus1MultipliedBy16PlusSrcPosGreaterThanOrEqualsTo64ThrowsIllegalArgumentException() {
        /* Branches:
         * (0 == nShorts) : false
         * ((nShorts - 1) * 16 + srcPos >= 64) : true
         */
        //Arrange Statement(s)
        short[] shortArray = new short[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("(nShorts-1)*16+srcPos is greater or equal to than 64");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.longToShortArray(0L, 96, shortArray, 0, -1);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${longToShortArrayWhenILessThanNShorts}, hash: 6F7A0A60A32FDDE9C9662E86C0623D3B
    @Test()
    void longToShortArrayWhenILessThanNShorts() {
        /* Branches:
         * (0 == nShorts) : false
         * ((nShorts - 1) * 16 + srcPos >= 64) : false
         * (i < nShorts) : true
         */
        //Arrange Statement(s)
        short[] shortArray = new short[] { (short) 0 };
        //Act Statement(s)
        short[] result = Conversion.longToShortArray(0L, 0, shortArray, 0, 1);
        short[] shortResultArray = new short[] { (short) 0 };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(shortResultArray)));
    }

    //BaseRock generated method id: ${shortArrayToIntWhenSrcPosNotEquals0And0EqualsNShorts}, hash: B3E4B07F4005955063BDC7D53E6BE460
    @Test()
    void shortArrayToIntWhenSrcPosNotEquals0And0EqualsNShorts() {
        /* Branches:
         * (src.length == 0) : true
         * (srcPos == 0) : false
         * (0 == nShorts) : true
         */
        //Arrange Statement(s)
        short[] shortArray = new short[] {};
        //Act Statement(s)
        int result = Conversion.shortArrayToInt(shortArray, 1, 0, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${shortArrayToIntWhen0NotEqualsNShortsAndNShortsMinus1MultipliedBy16PlusDstPosGreaterThanOrThrowsIllegalArgumentException}, hash: F78CC6E96F7F488B736F22F067F155FF
    @Test()
    void shortArrayToIntWhen0NotEqualsNShortsAndNShortsMinus1MultipliedBy16PlusDstPosGreaterThanOrThrowsIllegalArgumentException() {
        /* Branches:
         * (src.length == 0) : true
         * (srcPos == 0) : false
         * (0 == nShorts) : false
         * ((nShorts - 1) * 16 + dstPos >= 32) : true
         */
        //Arrange Statement(s)
        short[] shortArray = new short[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("(nShorts-1)*16+dstPos is greater or equal to than 32");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.shortArrayToInt(shortArray, 1, 0, 32, 1);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${shortArrayToIntWhenNShortsMinus1MultipliedBy16PlusDstPosLessThan32AndILessThanNShorts}, hash: 1CCE3606A571A615D6E1411C1ECD6ED8
    @Test()
    void shortArrayToIntWhenNShortsMinus1MultipliedBy16PlusDstPosLessThan32AndILessThanNShorts() {
        /* Branches:
         * (src.length == 0) : true
         * (srcPos == 0) : false
         * (0 == nShorts) : false
         * ((nShorts - 1) * 16 + dstPos >= 32) : false
         * (i < nShorts) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        short[] shortArray = new short[] {};
        //Act Statement(s)
        int result = Conversion.shortArrayToInt(shortArray, 0, 0, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${shortArrayToLongWhenSrcPosNotEquals0And0EqualsNShorts}, hash: 5AF4D010CD272A41F1A8AE94C88466F5
    @Test()
    void shortArrayToLongWhenSrcPosNotEquals0And0EqualsNShorts() {
        /* Branches:
         * (src.length == 0) : true
         * (srcPos == 0) : false
         * (0 == nShorts) : true
         */
        //Arrange Statement(s)
        short[] shortArray = new short[] {};
        //Act Statement(s)
        long result = Conversion.shortArrayToLong(shortArray, 1, 0L, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0L)));
    }

    //BaseRock generated method id: ${shortArrayToLongWhen0NotEqualsNShortsAndNShortsMinus1MultipliedBy16PlusDstPosGreaterThanOThrowsIllegalArgumentException}, hash: 682F0667F181ED61CA2650F3555D1726
    @Test()
    void shortArrayToLongWhen0NotEqualsNShortsAndNShortsMinus1MultipliedBy16PlusDstPosGreaterThanOThrowsIllegalArgumentException() {
        /* Branches:
         * (src.length == 0) : true
         * (srcPos == 0) : false
         * (0 == nShorts) : false
         * ((nShorts - 1) * 16 + dstPos >= 64) : true
         */
        //Arrange Statement(s)
        short[] shortArray = new short[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("(nShorts-1)*16+dstPos is greater or equal to than 64");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.shortArrayToLong(shortArray, 1, 0L, 64, 1);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${shortArrayToLongWhenNShortsMinus1MultipliedBy16PlusDstPosLessThan64AndILessThanNShorts}, hash: 2BE01A886DCDB486DE15EBEAE798EAF2
    @Test()
    void shortArrayToLongWhenNShortsMinus1MultipliedBy16PlusDstPosLessThan64AndILessThanNShorts() {
        /* Branches:
         * (src.length == 0) : true
         * (srcPos == 0) : false
         * (0 == nShorts) : false
         * ((nShorts - 1) * 16 + dstPos >= 64) : false
         * (i < nShorts) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        short[] shortArray = new short[] {};
        //Act Statement(s)
        long result = Conversion.shortArrayToLong(shortArray, 0, 0L, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0L)));
    }

    //BaseRock generated method id: ${shortToBinaryWhen0EqualsNBools}, hash: 45F52A78A117FFEA0BC87137B3638316
    @Test()
    void shortToBinaryWhen0EqualsNBools() {
        /* Branches:
         * (0 == nBools) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        //Act Statement(s)
        boolean[] result = Conversion.shortToBinary((short) 0, 0, booleanArray, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanArray)));
    }

    //BaseRock generated method id: ${shortToBinaryWhenNBoolsMinus1PlusSrcPosGreaterThanOrEqualsTo16ThrowsIllegalArgumentException}, hash: D3B02409E69D59977A05358716B7921B
    @Test()
    void shortToBinaryWhenNBoolsMinus1PlusSrcPosGreaterThanOrEqualsTo16ThrowsIllegalArgumentException() {
        /* Branches:
         * (0 == nBools) : false
         * (nBools - 1 + srcPos >= 16) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("nBools-1+srcPos is greater or equal to than 16");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.shortToBinary((short) 0, 18, booleanArray, 0, -1);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${shortToBinaryWhenNBoolsNotMinus1ThrowsAssertionError}, hash: 36D23196D89D33C2E443B2F236AF9850
    @Disabled()
    @Test()
    void shortToBinaryWhenNBoolsNotMinus1ThrowsAssertionError() {
        /* Branches:
         * (0 == nBools) : false
         * (nBools - 1 + srcPos >= 16) : false
         * (nBools - 1 < 16 - srcPos) : false
         * (nBools - 1) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        //Act Statement(s)
        final AssertionError result = assertThrows(AssertionError.class, () -> {
            Conversion.shortToBinary((short) 0, 0, booleanArray, 0, 0);
        });
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${shortToBinaryWhenDstPosPlusI}, hash: 6734349E21F2055081CC85D6678F017F
    @Disabled()
    @Test()
    void shortToBinaryWhenDstPosPlusI() {
        /* Branches:
         * (0 == nBools) : false
         * (nBools - 1 + srcPos >= 16) : false
         * (nBools - 1 < 16 - srcPos) : false
         * (nBools - 1) : false
         * (i < nBools) : true
         * (dstPos + i) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false };
        //Act Statement(s)
        boolean[] result = Conversion.shortToBinary((short) 0, 0, booleanArray, 0, 1);
        boolean[] booleanResultArray = new boolean[] { true };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${shortToBinaryWhenDstPosNotPlusI}, hash: 390B20F84836AE414409BC210894EED7
    @Test()
    void shortToBinaryWhenDstPosNotPlusI() {
        /* Branches:
         * (0 == nBools) : false
         * (nBools - 1 + srcPos >= 16) : false
         * (nBools - 1 < 16 - srcPos) : false
         * (nBools - 1) : false
         * (i < nBools) : true
         * (dstPos + i) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false };
        //Act Statement(s)
        boolean[] result = Conversion.shortToBinary((short) 0, 0, booleanArray, 0, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanArray)));
    }

    //BaseRock generated method id: ${shortToByteArrayWhen0EqualsNBytes}, hash: 72CEC1ABE2A481BBA6EA978967CFEC25
    @Test()
    void shortToByteArrayWhen0EqualsNBytes() {
        /* Branches:
         * (0 == nBytes) : true
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        //Act Statement(s)
        byte[] result = Conversion.shortToByteArray((short) 0, 0, byteArray, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(byteArray)));
    }

    //BaseRock generated method id: ${shortToByteArrayWhenNBytesMinus1MultipliedBy8PlusSrcPosGreaterThanOrEqualsTo16ThrowsIllegalArgumentException}, hash: 9A32D5DDB2552AE6863AA5B2DCEE4B73
    @Test()
    void shortToByteArrayWhenNBytesMinus1MultipliedBy8PlusSrcPosGreaterThanOrEqualsTo16ThrowsIllegalArgumentException() {
        /* Branches:
         * (0 == nBytes) : false
         * ((nBytes - 1) * 8 + srcPos >= 16) : true
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("(nBytes-1)*8+srcPos is greater or equal to than 16");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.shortToByteArray((short) 0, 32, byteArray, 0, -1);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${shortToByteArrayWhenILessThanNBytes}, hash: 3F5C1FB1AEF5E35F84EB18A17BEE4483
    @Test()
    void shortToByteArrayWhenILessThanNBytes() {
        /* Branches:
         * (0 == nBytes) : false
         * ((nBytes - 1) * 8 + srcPos >= 16) : false
         * (i < nBytes) : true
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] { (byte) 0 };
        //Act Statement(s)
        byte[] result = Conversion.shortToByteArray((short) 0, 0, byteArray, 0, 1);
        byte[] byteResultArray = new byte[] { (byte) 0 };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(byteResultArray)));
    }

    //BaseRock generated method id: ${shortToHexWhen0EqualsNHexs}, hash: 5C5C3A871BDB69009D26588803EF7BD9
    @Test()
    void shortToHexWhen0EqualsNHexs() {
        /* Branches:
         * (0 == nHexs) : true
         */
        //Act Statement(s)
        String result = Conversion.shortToHex((short) 0, 0, "dstInit1", 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("dstInit1")));
    }

    //BaseRock generated method id: ${shortToHexWhenNHexsMinus1MultipliedBy4PlusSrcPosGreaterThanOrEqualsTo16ThrowsIllegalArgumentException}, hash: 692079ACF8626D57B22CEDE64104E402
    @Test()
    void shortToHexWhenNHexsMinus1MultipliedBy4PlusSrcPosGreaterThanOrEqualsTo16ThrowsIllegalArgumentException() {
        /* Branches:
         * (0 == nHexs) : false
         * ((nHexs - 1) * 4 + srcPos >= 16) : true
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("(nHexs-1)*4+srcPos is greater or equal to than 16");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.shortToHex((short) 0, 24, "dstInit1", 0, -1);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${shortToHexWhenDstPosPlusIEqualsAppend}, hash: 9E76D8176B1C8DDB9B9AC71E11C0F034
    @Test()
    void shortToHexWhenDstPosPlusIEqualsAppend() {
        /* Branches:
         * (0 == nHexs) : false
         * ((nHexs - 1) * 4 + srcPos >= 16) : false
         * (i < nHexs) : true
         * (dstPos + i == append) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<Conversion> conversion = mockStatic(Conversion.class, CALLS_REAL_METHODS)) {
            conversion.when(() -> Conversion.intToHexDigit(0)).thenReturn('A');
            //Act Statement(s)
            String result = Conversion.shortToHex((short) 0, 1, "B", 1, 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("BA"));
                conversion.verify(() -> Conversion.intToHexDigit(0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${shortToHexWhenDstPosPlusINotEqualsAppend}, hash: 4C04E085FDABD73149C9EEBBBF0ACA59
    @Disabled()
    @Test()
    void shortToHexWhenDstPosPlusINotEqualsAppend() {
        /* Branches:
         * (0 == nHexs) : false
         * ((nHexs - 1) * 4 + srcPos >= 16) : false
         * (i < nHexs) : true
         * (dstPos + i == append) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<Conversion> conversion = mockStatic(Conversion.class, CALLS_REAL_METHODS)) {
            conversion.when(() -> Conversion.intToHexDigit(0)).thenReturn('A');
            //Act Statement(s)
            String result = Conversion.shortToHex((short) 0, 1, "A", 2, 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("A"));
                conversion.verify(() -> Conversion.intToHexDigit(0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${uuidToByteArrayWhen0EqualsNBytes}, hash: 1586F60F2D817D8D15E732089328D82E
    @Test()
    void uuidToByteArrayWhen0EqualsNBytes() {
        /* Branches:
         * (0 == nBytes) : true
         */
        //Arrange Statement(s)
        UUID uUID = UUID.fromString("12345678-0000-abcd-1234-abcdef123456");
        byte[] byteArray = new byte[] {};
        //Act Statement(s)
        byte[] result = Conversion.uuidToByteArray(uUID, byteArray, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(byteArray)));
    }

    //BaseRock generated method id: ${uuidToByteArrayWhenNBytesGreaterThan16ThrowsIllegalArgumentException}, hash: 49A297E2CBAB30BCEB11F7983285C129
    @Test()
    void uuidToByteArrayWhenNBytesGreaterThan16ThrowsIllegalArgumentException() {
        /* Branches:
         * (0 == nBytes) : false
         * (nBytes > 16) : true
         */
        //Arrange Statement(s)
        UUID uUID = UUID.fromString("12345678-0000-abcd-1234-abcdef123456");
        byte[] byteArray = new byte[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("nBytes is greater than 16");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Conversion.uuidToByteArray(uUID, byteArray, 0, 17);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${uuidToByteArrayWhenNBytesGreaterThanOrEqualsTo8}, hash: 97E6D1AFD8DD1F60ABB6E161D430EF5E
    @Test()
    void uuidToByteArrayWhenNBytesGreaterThanOrEqualsTo8() {
        /* Branches:
         * (0 == nBytes) : false
         * (nBytes > 16) : false
         * (nBytes >= 8) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<Conversion> conversion = mockStatic(Conversion.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            byte[] byteArray2 = new byte[] {};
            conversion.when(() -> Conversion.longToByteArray(1311768464867765197L, 0, byteArray2, 8, 8)).thenReturn(byteArray);
            byte[] byteArray3 = new byte[] {};
            conversion.when(() -> Conversion.longToByteArray(1311862292439250006L, 0, byteArray2, 16, 1)).thenReturn(byteArray3);
            UUID uUID = UUID.fromString("12345678-0000-abcd-1234-abcdef123456");
            //Act Statement(s)
            byte[] result = Conversion.uuidToByteArray(uUID, byteArray2, 8, 9);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(byteArray2));
                conversion.verify(() -> Conversion.longToByteArray(1311768464867765197L, 0, byteArray2, 8, 8), atLeast(1));
                conversion.verify(() -> Conversion.longToByteArray(1311862292439250006L, 0, byteArray2, 16, 1), atLeast(1));
            });
        }
    }
}
