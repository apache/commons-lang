package org.apache.commons.lang3.text;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.io.IOException;
import java.util.List;
import java.util.Arrays;
import java.io.Writer;
import java.util.Iterator;
import java.io.Reader;
import java.nio.CharBuffer;
import java.util.ArrayList;
import static org.mockito.Mockito.doNothing;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.doReturn;
import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class StrBuilderBaseRockGeneratedTest {

    private final Object objectMock = mock(Object.class, "obj");

    private final Readable readableMock = mock(Readable.class);

    //BaseRock generated method id: ${appendWhenValue}, hash: F04E404B5C5822C1649429929CBF0BF2
    @Test()
    void appendWhenValue() {
        /* Branches:
         * (value) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).ensureCapacity(4);
        
        //Act Statement(s)
        StrBuilder result = target.append(true);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).ensureCapacity(4);
        });
    }

    //BaseRock generated method id: ${appendWhenNotValue}, hash: CAAE0E10C9868EFBCD84EA0698E33381
    @Test()
    void appendWhenNotValue() {
        /* Branches:
         * (value) : false
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).ensureCapacity(5);
        
        //Act Statement(s)
        StrBuilder result = target.append(false);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).ensureCapacity(5);
        });
    }

    //BaseRock generated method id: ${append1Test}, hash: 6732BA76F1D850F8B0C6CFC463AABBB2
    @Test()
    void append1Test() {
        //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).ensureCapacity(1);
        
        //Act Statement(s)
        StrBuilder result = target.append('A');
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).ensureCapacity(1);
        });
    }

    //BaseRock generated method id: ${append2WhenCharsIsNull}, hash: F2013E1705D1C18739167FB9D8F48739
    @Test()
    void append2WhenCharsIsNull() {
        /* Branches:
         * (chars == null) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).appendNull();
        char[] _char = null;
        
        //Act Statement(s)
        StrBuilder result = target.append(_char);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).appendNull();
        });
    }

    //BaseRock generated method id: ${append2WhenStrLenGreaterThan0}, hash: 6E9448BA3FAE44367D7FE9EFCD60C2AE
    @Test()
    void append2WhenStrLenGreaterThan0() {
        /* Branches:
         * (chars == null) : false
         * (strLen > 0) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).ensureCapacity(1);
        char[] charArray = new char[] { 'A' };
        
        //Act Statement(s)
        StrBuilder result = target.append(charArray);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).ensureCapacity(1);
        });
    }

    //BaseRock generated method id: ${append3WhenCharsIsNull}, hash: 3A59467029147460F25D8631BE8736E1
    @Test()
    void append3WhenCharsIsNull() {
        /* Branches:
         * (chars == null) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).appendNull();
        char[] _char = null;
        
        //Act Statement(s)
        StrBuilder result = target.append(_char, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).appendNull();
        });
    }

    //BaseRock generated method id: ${append3WhenStartIndexGreaterThanCharsLengthThrowsStringIndexOutOfBoundsException}, hash: 4ABA7BD206AEF3D10BF735235FA5EB96
    @Test()
    void append3WhenStartIndexGreaterThanCharsLengthThrowsStringIndexOutOfBoundsException() {
        /* Branches:
         * (chars == null) : false
         * (startIndex < 0) : false
         * (startIndex > chars.length) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        char[] charArray = new char[] {};
        StringIndexOutOfBoundsException stringIndexOutOfBoundsException = new StringIndexOutOfBoundsException("Invalid startIndex: 2");
        //Act Statement(s)
        final StringIndexOutOfBoundsException result = assertThrows(StringIndexOutOfBoundsException.class, () -> {
            target.append(charArray, 1, 2);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(stringIndexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${append3WhenStartIndexPlusLengthGreaterThanCharsLengthThrowsStringIndexOutOfBoundsException}, hash: 1BA888DDDA8EFB2FE59C515F445EE41D
    @Test()
    void append3WhenStartIndexPlusLengthGreaterThanCharsLengthThrowsStringIndexOutOfBoundsException() {
        /* Branches:
         * (chars == null) : false
         * (startIndex < 0) : false
         * (startIndex > chars.length) : false
         * (length < 0) : false
         * (startIndex + length > chars.length) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        char[] charArray = new char[] {};
        StringIndexOutOfBoundsException stringIndexOutOfBoundsException = new StringIndexOutOfBoundsException("Invalid length: 2");
        //Act Statement(s)
        final StringIndexOutOfBoundsException result = assertThrows(StringIndexOutOfBoundsException.class, () -> {
            target.append(charArray, 0, 2);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(stringIndexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${append3WhenLengthGreaterThan0}, hash: 31F273BAFCF89E03D5114B74EAEF9727
    @Test()
    void append3WhenLengthGreaterThan0() {
        /* Branches:
         * (chars == null) : false
         * (startIndex < 0) : false
         * (startIndex > chars.length) : false
         * (length < 0) : false
         * (startIndex + length > chars.length) : false
         * (length > 0) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).ensureCapacity(1);
        char[] charArray = new char[] { 'A' };
        
        //Act Statement(s)
        StrBuilder result = target.append(charArray, 0, 1);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).ensureCapacity(1);
        });
    }

    //BaseRock generated method id: ${append4WhenBufIsNull}, hash: 8F29993C5279A09D0716C776EE192EA1
    @Test()
    void append4WhenBufIsNull() {
        /* Branches:
         * (buf == null) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).appendNull();
        CharBuffer charBuffer = null;
        
        //Act Statement(s)
        StrBuilder result = target.append(charBuffer);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).appendNull();
        });
    }

    //BaseRock generated method id: ${append4WhenBufHasArray}, hash: 7CB129474C33BC9313A246B4AC069589
    @Test()
    void append4WhenBufHasArray() {
        /* Branches:
         * (buf == null) : false
         * (buf.hasArray()) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).ensureCapacity(0);
        CharBuffer charBuffer = CharBuffer.allocate(0);
        
        //Act Statement(s)
        StrBuilder result = target.append(charBuffer);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).ensureCapacity(0);
        });
    }

    //BaseRock generated method id: ${append4WhenBufNotHasArray}, hash: 1B58E8EE5BC622F9DB786C440192B449
    @Test()
    void append4WhenBufNotHasArray() {
        /* Branches:
         * (buf == null) : false
         * (buf.hasArray()) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).append("buf");
        CharBuffer charBuffer = CharBuffer.allocate(0);
        
        //Act Statement(s)
        StrBuilder result = target.append(charBuffer);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append("buf");
        });
    }

    //BaseRock generated method id: ${append5WhenBufIsNull}, hash: 6E7228A132313F8D90097993A3F1B9F6
    @Test()
    void append5WhenBufIsNull() {
        /* Branches:
         * (buf == null) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).appendNull();
        CharBuffer charBuffer = null;
        
        //Act Statement(s)
        StrBuilder result = target.append(charBuffer, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).appendNull();
        });
    }

    //BaseRock generated method id: ${append5WhenBufNotHasArray}, hash: E914E81D7D148378E55841E9A2E3F34A
    @Test()
    void append5WhenBufNotHasArray() {
        /* Branches:
         * (buf == null) : false
         * (buf.hasArray()) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).append("buf", 0, 0);
        CharBuffer charBuffer = CharBuffer.allocate(0);
        
        //Act Statement(s)
        StrBuilder result = target.append(charBuffer, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append("buf", 0, 0);
        });
    }

    //BaseRock generated method id: ${append5WhenStartIndexGreaterThanTotalLengthThrowsStringIndexOutOfBoundsException}, hash: 86E6D57D1531103C4849643CDE441DBB
    @Test()
    void append5WhenStartIndexGreaterThanTotalLengthThrowsStringIndexOutOfBoundsException() {
        /* Branches:
         * (buf == null) : false
         * (buf.hasArray()) : true
         * (startIndex < 0) : false
         * (startIndex > totalLength) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        CharBuffer charBuffer = CharBuffer.allocate(0);
        StringIndexOutOfBoundsException stringIndexOutOfBoundsException = new StringIndexOutOfBoundsException("startIndex must be valid");
        //Act Statement(s)
        final StringIndexOutOfBoundsException result = assertThrows(StringIndexOutOfBoundsException.class, () -> {
            target.append(charBuffer, 1, 0);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(stringIndexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${append5WhenStartIndexPlusLengthGreaterThanTotalLengthThrowsStringIndexOutOfBoundsException}, hash: A661ABF11C7F8AA3FF3F275D7FE684F9
    @Test()
    void append5WhenStartIndexPlusLengthGreaterThanTotalLengthThrowsStringIndexOutOfBoundsException() {
        /* Branches:
         * (buf == null) : false
         * (buf.hasArray()) : true
         * (startIndex < 0) : false
         * (startIndex > totalLength) : false
         * (length < 0) : false
         * (startIndex + length > totalLength) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        CharBuffer charBuffer = CharBuffer.allocate(0);
        StringIndexOutOfBoundsException stringIndexOutOfBoundsException = new StringIndexOutOfBoundsException("length must be valid");
        //Act Statement(s)
        final StringIndexOutOfBoundsException result = assertThrows(StringIndexOutOfBoundsException.class, () -> {
            target.append(charBuffer, 0, 1);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(stringIndexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${append5WhenStartIndexPlusLengthNotGreaterThanTotalLength}, hash: 82DCF8CFAE7FB36FBB1358E9DB462C55
    @Test()
    void append5WhenStartIndexPlusLengthNotGreaterThanTotalLength() {
        /* Branches:
         * (buf == null) : false
         * (buf.hasArray()) : true
         * (startIndex < 0) : false
         * (startIndex > totalLength) : false
         * (length < 0) : false
         * (startIndex + length > totalLength) : false
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).ensureCapacity(0);
        CharBuffer charBuffer = CharBuffer.allocate(0);
        
        //Act Statement(s)
        StrBuilder result = target.append(charBuffer, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).ensureCapacity(0);
        });
    }

    //BaseRock generated method id: ${append6WhenSeqIsNull}, hash: B5D5DA099116AB09E808EF644394F71D
    @Test()
    void append6WhenSeqIsNull() {
        /* Branches:
         * (seq == null) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).appendNull();
        CharSequence charSequence = null;
        
        //Act Statement(s)
        StrBuilder result = target.append(charSequence);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).appendNull();
        });
    }

    //BaseRock generated method id: ${append6WhenSeqInstanceOfStrBuilder}, hash: 99B7191B76FA41FCC93B1C09E0D50EA6
    @Test()
    void append6WhenSeqInstanceOfStrBuilder() {
        /* Branches:
         * (seq == null) : false
         * (seq instanceof StrBuilder) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        StrBuilder strBuilder = new StrBuilder();
        
        //Act Statement(s)
        StrBuilder result = target.append((CharSequence) strBuilder);
        StrBuilder strBuilder2 = new StrBuilder();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(strBuilder2)));
    }

    //BaseRock generated method id: ${append6WhenSeqInstanceOfStringBuilder}, hash: 7D99285379DE1E9317501A02EFAEBE77
    @Test()
    void append6WhenSeqInstanceOfStringBuilder() {
        /* Branches:
         * (seq == null) : false
         * (seq instanceof StrBuilder) : false
         * (seq instanceof StringBuilder) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        StringBuilder stringBuilder = new StringBuilder();
        
        //Act Statement(s)
        StrBuilder result = target.append((CharSequence) stringBuilder);
        StrBuilder strBuilder = new StrBuilder();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(strBuilder)));
    }

    //BaseRock generated method id: ${append6WhenSeqInstanceOfStringBuffer}, hash: C830B4163688631F34929E4D97CD381C
    @Test()
    void append6WhenSeqInstanceOfStringBuffer() {
        /* Branches:
         * (seq == null) : false
         * (seq instanceof StrBuilder) : false
         * (seq instanceof StringBuilder) : false
         * (seq instanceof StringBuffer) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        StringBuffer stringBuffer = new StringBuffer();
        
        //Act Statement(s)
        StrBuilder result = target.append((CharSequence) stringBuffer);
        StrBuilder strBuilder = new StrBuilder();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(strBuilder)));
    }

    //BaseRock generated method id: ${append6WhenSeqInstanceOfCharBuffer}, hash: C4F8828D5072BA293901491FEC25DFD7
    @Test()
    void append6WhenSeqInstanceOfCharBuffer() {
        /* Branches:
         * (seq == null) : false
         * (seq instanceof StrBuilder) : false
         * (seq instanceof StringBuilder) : false
         * (seq instanceof StringBuffer) : false
         * (seq instanceof CharBuffer) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).append((CharBuffer) any());
        CharBuffer charBuffer = CharBuffer.allocate(0);
        
        //Act Statement(s)
        StrBuilder result = target.append((CharSequence) charBuffer);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).append((CharBuffer) any());
        });
    }

    //BaseRock generated method id: ${append6WhenSeqNotInstanceOfCharBuffer}, hash: B6298AC9C00EAB772A0FE63E79F46741
    @Test()
    void append6WhenSeqNotInstanceOfCharBuffer() {
        /* Branches:
         * (seq == null) : false
         * (seq instanceof StrBuilder) : false
         * (seq instanceof StringBuilder) : false
         * (seq instanceof StringBuffer) : false
         * (seq instanceof CharBuffer) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        StrBuilder result = target.append((CharSequence) "seq1");
        StrBuilder strBuilder = new StrBuilder();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(strBuilder)));
    }

    //BaseRock generated method id: ${append7WhenSeqIsNull}, hash: FBF303A325767FBC58FD260CE5B135CB
    @Test()
    void append7WhenSeqIsNull() {
        /* Branches:
         * (seq == null) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).appendNull();
        CharSequence charSequence = null;
        
        //Act Statement(s)
        StrBuilder result = target.append(charSequence, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).appendNull();
        });
    }

    //BaseRock generated method id: ${append7WhenSeqIsNotNull}, hash: C8E64D5F4005814FD01671C8FAE6EF6B
    @Test()
    void append7WhenSeqIsNotNull() {
        /* Branches:
         * (seq == null) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        StrBuilder result = target.append((CharSequence) "seq1", 0, 0);
        StrBuilder strBuilder = new StrBuilder();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(strBuilder)));
    }

    //BaseRock generated method id: ${append8Test}, hash: 52295AF64383EB8BAD42B4EFC457DEA1
    @Test()
    void append8Test() {
        //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).append("0.0");
        
        //Act Statement(s)
        StrBuilder result = target.append(Double.parseDouble("0.0"));
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).append("0.0");
        });
    }

    //BaseRock generated method id: ${append9Test}, hash: 25DB20F5E6722A1E3A00104EEBACA089
    @Test()
    void append9Test() {
        //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).append("0.0");
        
        //Act Statement(s)
        StrBuilder result = target.append(Float.parseFloat("0.0"));
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).append("0.0");
        });
    }

    //BaseRock generated method id: ${append10Test}, hash: 9A55A6C6B8D637DFA913E28FD765B094
    @Test()
    void append10Test() {
        //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).append("2");
        
        //Act Statement(s)
        StrBuilder result = target.append(2);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).append("2");
        });
    }

    //BaseRock generated method id: ${append11Test}, hash: EC2A55DAE1BCF919622FC8CD1A9325F9
    @Test()
    void append11Test() {
        //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).append("2");
        
        //Act Statement(s)
        StrBuilder result = target.append(2L);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).append("2");
        });
    }

    //BaseRock generated method id: ${append12WhenObjIsNull}, hash: 0A964BE21A55E9ADE094BE7133A95F2D
    @Test()
    void append12WhenObjIsNull() {
        /* Branches:
         * (obj == null) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).appendNull();
        Object object = null;
        
        //Act Statement(s)
        StrBuilder result = target.append(object);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).appendNull();
        });
    }

    //BaseRock generated method id: ${append12WhenObjInstanceOfCharSequence}, hash: 763DD2FC1F0A2BF3AF09761B490DFF1F
    @Test()
    void append12WhenObjInstanceOfCharSequence() {
        /* Branches:
         * (obj == null) : false
         * (obj instanceof CharSequence) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).append("obj1");
        
        //Act Statement(s)
        StrBuilder result = target.append((Object) "obj1");
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).append("obj1");
        });
    }

    //BaseRock generated method id: ${append12WhenObjNotInstanceOfCharSequence}, hash: 3A9AC2B89D6A884B9071486CBB8A74F3
    @Test()
    void append12WhenObjNotInstanceOfCharSequence() {
        /* Branches:
         * (obj == null) : false
         * (obj instanceof CharSequence) : false
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).append("obj");
        
        //Act Statement(s)
        StrBuilder result = target.append(objectMock);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).append("obj");
        });
    }

    //BaseRock generated method id: ${append13WhenStrIsNull}, hash: D87C30B8C4A5E6372EABFABEFBC344FD
    @Test()
    void append13WhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).appendNull();
        StrBuilder strBuilder2 = null;
        
        //Act Statement(s)
        StrBuilder result = target.append(strBuilder2);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).appendNull();
        });
    }

    //BaseRock generated method id: ${append13WhenStrLenGreaterThan0}, hash: DA47402C5EA47AC4AB3D4DFD171C83EA
    @Test()
    void append13WhenStrLenGreaterThan0() {
        /* Branches:
         * (str == null) : false
         * (strLen > 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).ensureCapacity(1);
        StrBuilder strBuilder2 = new StrBuilder();
        
        //Act Statement(s)
        StrBuilder result = target.append(strBuilder2);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).ensureCapacity(1);
        });
    }

    //BaseRock generated method id: ${append14WhenStrIsNull}, hash: BB245DAC832B509AA7340F6988EA3725
    @Test()
    void append14WhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).appendNull();
        StrBuilder strBuilder2 = null;
        
        //Act Statement(s)
        StrBuilder result = target.append(strBuilder2, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).appendNull();
        });
    }

    //BaseRock generated method id: ${append14WhenStartIndexGreaterThanStrLengthThrowsStringIndexOutOfBoundsException}, hash: F124BD93E4E24FA24736EB123FB57704
    @Test()
    void append14WhenStartIndexGreaterThanStrLengthThrowsStringIndexOutOfBoundsException() {
        /* Branches:
         * (str == null) : false
         * (startIndex < 0) : false
         * (startIndex > str.length()) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        StrBuilder strBuilder = new StrBuilder();
        StringIndexOutOfBoundsException stringIndexOutOfBoundsException = new StringIndexOutOfBoundsException("startIndex must be valid");
        //Act Statement(s)
        final StringIndexOutOfBoundsException result = assertThrows(StringIndexOutOfBoundsException.class, () -> {
            target.append(strBuilder, 1, 0);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(stringIndexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${append14WhenStartIndexPlusLengthGreaterThanStrLengthThrowsStringIndexOutOfBoundsException}, hash: 7D7E57C49BDBC52B75751D3F5B87E94A
    @Test()
    void append14WhenStartIndexPlusLengthGreaterThanStrLengthThrowsStringIndexOutOfBoundsException() {
        /* Branches:
         * (str == null) : false
         * (startIndex < 0) : false
         * (startIndex > str.length()) : false
         * (length < 0) : false
         * (startIndex + length > str.length()) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        StrBuilder strBuilder = new StrBuilder();
        StringIndexOutOfBoundsException stringIndexOutOfBoundsException = new StringIndexOutOfBoundsException("length must be valid");
        //Act Statement(s)
        final StringIndexOutOfBoundsException result = assertThrows(StringIndexOutOfBoundsException.class, () -> {
            target.append(strBuilder, 0, 1);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(stringIndexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${append14WhenLengthGreaterThan0}, hash: 8DD96BEE384AF344A70FA880CDC2DBDF
    @Test()
    void append14WhenLengthGreaterThan0() {
        /* Branches:
         * (str == null) : false
         * (startIndex < 0) : false
         * (startIndex > str.length()) : false
         * (length < 0) : false
         * (startIndex + length > str.length()) : false
         * (length > 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).ensureCapacity(1);
        StrBuilder strBuilder2 = new StrBuilder();
        
        //Act Statement(s)
        StrBuilder result = target.append(strBuilder2, 0, 1);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).ensureCapacity(1);
        });
    }

    //BaseRock generated method id: ${append15WhenStrIsNull}, hash: 05F88CE2A089E020A8B5FD991B53E4A6
    @Test()
    void append15WhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).appendNull();
        
        //Act Statement(s)
        StrBuilder result = target.append((String) null);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).appendNull();
        });
    }

    //BaseRock generated method id: ${append15WhenStrLenGreaterThan0}, hash: 22B8A04F7DD0D20A3DBA0246228D3255
    @Test()
    void append15WhenStrLenGreaterThan0() {
        /* Branches:
         * (str == null) : false
         * (strLen > 0) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).ensureCapacity(1);
        
        //Act Statement(s)
        StrBuilder result = target.append("A");
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).ensureCapacity(1);
        });
    }

    //BaseRock generated method id: ${append16WhenStrIsNull}, hash: 3B43CD5C63410075E32748BDDEFDBE7A
    @Test()
    void append16WhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).appendNull();
        
        //Act Statement(s)
        StrBuilder result = target.append((String) null, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).appendNull();
        });
    }

    //BaseRock generated method id: ${append16WhenStartIndexGreaterThanStrLengthThrowsStringIndexOutOfBoundsException}, hash: 82948766E79756ECE9B209DF1F82C507
    @Test()
    void append16WhenStartIndexGreaterThanStrLengthThrowsStringIndexOutOfBoundsException() {
        /* Branches:
         * (str == null) : false
         * (startIndex < 0) : false
         * (startIndex > str.length()) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        StringIndexOutOfBoundsException stringIndexOutOfBoundsException = new StringIndexOutOfBoundsException("startIndex must be valid");
        //Act Statement(s)
        final StringIndexOutOfBoundsException result = assertThrows(StringIndexOutOfBoundsException.class, () -> {
            target.append("", 2, 0);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(stringIndexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${append16WhenStartIndexPlusLengthGreaterThanStrLengthThrowsStringIndexOutOfBoundsException}, hash: F2B18443154E17D206FF7C818A809977
    @Test()
    void append16WhenStartIndexPlusLengthGreaterThanStrLengthThrowsStringIndexOutOfBoundsException() {
        /* Branches:
         * (str == null) : false
         * (startIndex < 0) : false
         * (startIndex > str.length()) : false
         * (length < 0) : false
         * (startIndex + length > str.length()) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        StringIndexOutOfBoundsException stringIndexOutOfBoundsException = new StringIndexOutOfBoundsException("length must be valid");
        //Act Statement(s)
        final StringIndexOutOfBoundsException result = assertThrows(StringIndexOutOfBoundsException.class, () -> {
            target.append("", 0, 1);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(stringIndexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${append16WhenLengthGreaterThan0}, hash: 4937D4CB07624050F9EBFC8924A0C17B
    @Test()
    void append16WhenLengthGreaterThan0() {
        /* Branches:
         * (str == null) : false
         * (startIndex < 0) : false
         * (startIndex > str.length()) : false
         * (length < 0) : false
         * (startIndex + length > str.length()) : false
         * (length > 0) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).ensureCapacity(1);
        
        //Act Statement(s)
        StrBuilder result = target.append("A", 0, 1);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).ensureCapacity(1);
        });
    }

    //BaseRock generated method id: ${append17Test}, hash: B2E2A8ED2E9A30225993C7D3E061A33D
    @Test()
    void append17Test() {
        //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).append("A");
        Object[] objectArray = new Object[] {};
        
        //Act Statement(s)
        StrBuilder result = target.append("A", objectArray);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).append("A");
        });
    }

    //BaseRock generated method id: ${append18WhenStrIsNull}, hash: 592FAC020C094F2392A2EE3BFAE0522D
    @Test()
    void append18WhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).appendNull();
        StringBuffer stringBuffer = null;
        
        //Act Statement(s)
        StrBuilder result = target.append(stringBuffer);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).appendNull();
        });
    }

    //BaseRock generated method id: ${append18WhenStrLenGreaterThan0}, hash: AA8B401112D0DB9D09293C0C5F8E23B0
    @Test()
    void append18WhenStrLenGreaterThan0() {
        /* Branches:
         * (str == null) : false
         * (strLen > 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).ensureCapacity(0);
        StringBuffer stringBuffer = new StringBuffer();
        
        //Act Statement(s)
        StrBuilder result = target.append(stringBuffer);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).ensureCapacity(0);
        });
    }

    //BaseRock generated method id: ${append19WhenStrIsNull}, hash: 0D9243C89C688865F2036A7B3A9D6B3B
    @Test()
    void append19WhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).appendNull();
        StringBuffer stringBuffer = null;
        
        //Act Statement(s)
        StrBuilder result = target.append(stringBuffer, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).appendNull();
        });
    }

    //BaseRock generated method id: ${append19WhenStartIndexGreaterThanStrLengthThrowsStringIndexOutOfBoundsException}, hash: 359D0E5EB2955D080183D75A2A2B7898
    @Test()
    void append19WhenStartIndexGreaterThanStrLengthThrowsStringIndexOutOfBoundsException() {
        /* Branches:
         * (str == null) : false
         * (startIndex < 0) : false
         * (startIndex > str.length()) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        StringBuffer stringBuffer = new StringBuffer();
        StringIndexOutOfBoundsException stringIndexOutOfBoundsException = new StringIndexOutOfBoundsException("startIndex must be valid");
        //Act Statement(s)
        final StringIndexOutOfBoundsException result = assertThrows(StringIndexOutOfBoundsException.class, () -> {
            target.append(stringBuffer, 1, 0);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(stringIndexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${append19WhenStartIndexPlusLengthGreaterThanStrLengthThrowsStringIndexOutOfBoundsException}, hash: 450B81F2385BFF6456657CC77292E8A0
    @Test()
    void append19WhenStartIndexPlusLengthGreaterThanStrLengthThrowsStringIndexOutOfBoundsException() {
        /* Branches:
         * (str == null) : false
         * (startIndex < 0) : false
         * (startIndex > str.length()) : false
         * (length < 0) : false
         * (startIndex + length > str.length()) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        StringBuffer stringBuffer = new StringBuffer();
        StringIndexOutOfBoundsException stringIndexOutOfBoundsException = new StringIndexOutOfBoundsException("length must be valid");
        //Act Statement(s)
        final StringIndexOutOfBoundsException result = assertThrows(StringIndexOutOfBoundsException.class, () -> {
            target.append(stringBuffer, 0, 1);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(stringIndexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${append19WhenLengthGreaterThan0}, hash: 875EA92A0AD09FD044700E85432BC5C4
    @Test()
    void append19WhenLengthGreaterThan0() {
        /* Branches:
         * (str == null) : false
         * (startIndex < 0) : false
         * (startIndex > str.length()) : false
         * (length < 0) : false
         * (startIndex + length > str.length()) : false
         * (length > 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).ensureCapacity(0);
        StringBuffer stringBuffer = new StringBuffer();
        
        //Act Statement(s)
        StrBuilder result = target.append(stringBuffer, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).ensureCapacity(0);
        });
    }

    //BaseRock generated method id: ${append20WhenStrIsNull}, hash: 0F45056F86EBEDF016C2AC6B60EA985F
    @Test()
    void append20WhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).appendNull();
        StringBuilder stringBuilder = null;
        
        //Act Statement(s)
        StrBuilder result = target.append(stringBuilder);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).appendNull();
        });
    }

    //BaseRock generated method id: ${append20WhenStrLenGreaterThan0}, hash: 91AFA697C13E058A26C0698667E54384
    @Test()
    void append20WhenStrLenGreaterThan0() {
        /* Branches:
         * (str == null) : false
         * (strLen > 0) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).ensureCapacity(1);
        StringBuilder stringBuilder = new StringBuilder("A");
        
        //Act Statement(s)
        StrBuilder result = target.append(stringBuilder);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).ensureCapacity(1);
        });
    }

    //BaseRock generated method id: ${append21WhenStrIsNull}, hash: 54C9B4010F4CFD965B02237B42CA898B
    @Test()
    void append21WhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).appendNull();
        StringBuilder stringBuilder = null;
        
        //Act Statement(s)
        StrBuilder result = target.append(stringBuilder, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).appendNull();
        });
    }

    //BaseRock generated method id: ${append21WhenStartIndexGreaterThanStrLengthThrowsStringIndexOutOfBoundsException}, hash: A862E355C1E8643D0560F66BFB4656CB
    @Test()
    void append21WhenStartIndexGreaterThanStrLengthThrowsStringIndexOutOfBoundsException() {
        /* Branches:
         * (str == null) : false
         * (startIndex < 0) : false
         * (startIndex > str.length()) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        StringBuilder stringBuilder = new StringBuilder("");
        StringIndexOutOfBoundsException stringIndexOutOfBoundsException = new StringIndexOutOfBoundsException("startIndex must be valid");
        //Act Statement(s)
        final StringIndexOutOfBoundsException result = assertThrows(StringIndexOutOfBoundsException.class, () -> {
            target.append(stringBuilder, 2, 0);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(stringIndexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${append21WhenStartIndexPlusLengthGreaterThanStrLengthThrowsStringIndexOutOfBoundsException}, hash: 26F0E2E56B87ADA98C23B908AAAAC898
    @Test()
    void append21WhenStartIndexPlusLengthGreaterThanStrLengthThrowsStringIndexOutOfBoundsException() {
        /* Branches:
         * (str == null) : false
         * (startIndex < 0) : false
         * (startIndex > str.length()) : false
         * (length < 0) : false
         * (startIndex + length > str.length()) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        StringBuilder stringBuilder = new StringBuilder("");
        StringIndexOutOfBoundsException stringIndexOutOfBoundsException = new StringIndexOutOfBoundsException("length must be valid");
        //Act Statement(s)
        final StringIndexOutOfBoundsException result = assertThrows(StringIndexOutOfBoundsException.class, () -> {
            target.append(stringBuilder, 0, 1);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(stringIndexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${append21WhenLengthGreaterThan0}, hash: F3D15B7C037A2A9C4804E80E67B49450
    @Test()
    void append21WhenLengthGreaterThan0() {
        /* Branches:
         * (str == null) : false
         * (startIndex < 0) : false
         * (startIndex > str.length()) : false
         * (length < 0) : false
         * (startIndex + length > str.length()) : false
         * (length > 0) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).ensureCapacity(1);
        StringBuilder stringBuilder = new StringBuilder("A");
        
        //Act Statement(s)
        StrBuilder result = target.append(stringBuilder, 0, 1);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).ensureCapacity(1);
        });
    }

    //BaseRock generated method id: ${appendAllWhenIterableIsNotNull}, hash: 5DAB17D8678851D6BDC8A44C01C4E8E8
    @Test()
    void appendAllWhenIterableIsNotNull() {
        /* Branches:
         * (iterable != null) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        Object object = new Object();
        doReturn(strBuilder).when(target).append(object);
        Iterable<Object> iterable = new ArrayList<>(Arrays.asList(object));
        
        //Act Statement(s)
        StrBuilder result = target.appendAll(iterable);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append(object);
        });
    }

    //BaseRock generated method id: ${appendAll1WhenItIsNotNull}, hash: 2395D501EDAC028C6C26E53D1F3A76EB
    @Test()
    void appendAll1WhenItIsNotNull() {
        /* Branches:
         * (it != null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        List<Object> anyList = new ArrayList<>();
        Iterator<?> iteratorIt = anyList.iterator();
        
        //Act Statement(s)
        StrBuilder result = target.appendAll(iteratorIt);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${appendAll2WhenArrayIsNotEmpty}, hash: 2267CBC391994E01802C5F1BFD4B5388
    @Test()
    void appendAll2WhenArrayIsNotEmpty() {
        /* Branches:
         * (ArrayUtils.isNotEmpty(array)) : true
         * (for-each(array)) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        Object object = new Object();
        doReturn(strBuilder).when(target).append(object);
        Object[] objectArray = new Object[] { object };
        
        //Act Statement(s)
        StrBuilder result = target.appendAll(objectArray);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append(object);
        });
    }

    //BaseRock generated method id: ${appendFixedWidthPadLeftTest}, hash: E56A1E5F8827878662586755C796F950
    @Test()
    void appendFixedWidthPadLeftTest() {
        //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).appendFixedWidthPadLeft("2", 0, 'A');
        
        //Act Statement(s)
        StrBuilder result = target.appendFixedWidthPadLeft(2, 0, 'A');
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).appendFixedWidthPadLeft("2", 0, 'A');
        });
    }

    //BaseRock generated method id: ${appendFixedWidthPadLeft1WhenILessThanPadLen}, hash: E53851D2C3CA01494DF6BF478619A963
    @Test()
    void appendFixedWidthPadLeft1WhenILessThanPadLen() {
        /* Branches:
         * (width > 0) : true
         * (str == null) : true
         * (strLen >= width) : false
         * (i < padLen) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).ensureCapacity(1);
        Object object = new Object();
        
        //Act Statement(s)
        StrBuilder result = target.appendFixedWidthPadLeft(object, 1, 'A');
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).ensureCapacity(1);
        });
    }

    //BaseRock generated method id: ${appendFixedWidthPadRightTest}, hash: 20940A6C5C6A4F82B40EEBD09A91E2D8
    @Test()
    void appendFixedWidthPadRightTest() {
        //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).appendFixedWidthPadRight("2", 0, 'A');
        
        //Act Statement(s)
        StrBuilder result = target.appendFixedWidthPadRight(2, 0, 'A');
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).appendFixedWidthPadRight("2", 0, 'A');
        });
    }

    //BaseRock generated method id: ${appendFixedWidthPadRight1WhenILessThanPadLen}, hash: F36DBA2B5FA73DC99C9AB88C2AD1D117
    @Test()
    void appendFixedWidthPadRight1WhenILessThanPadLen() {
        /* Branches:
         * (width > 0) : true
         * (str == null) : true
         * (strLen >= width) : false
         * (i < padLen) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).ensureCapacity(1);
        Object object = new Object();
        
        //Act Statement(s)
        StrBuilder result = target.appendFixedWidthPadRight(object, 1, 'A');
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).ensureCapacity(1);
        });
    }

    //BaseRock generated method id: ${appendlnTest}, hash: 2D4F06088FDA8CE863A9D30AA7E9B9FF
    @Test()
    void appendlnTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).append(false);
        
        //Act Statement(s)
        StrBuilder result = target.appendln(false);
        StrBuilder strBuilder2 = new StrBuilder();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder2));
            verify(target).append(false);
        });
    }

    //BaseRock generated method id: ${appendln1Test}, hash: 2BE239811AA61E36A5270CD3F2349D0A
    @Test()
    void appendln1Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).append('A');
        
        //Act Statement(s)
        StrBuilder result = target.appendln('A');
        StrBuilder strBuilder2 = new StrBuilder();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder2));
            verify(target).append('A');
        });
    }

    //BaseRock generated method id: ${appendln2Test}, hash: 06689071AE0E8059469E8E497927BB94
    @Test()
    void appendln2Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        char[] charArray = new char[] {};
        doReturn(strBuilder).when(target).append(charArray);
        
        //Act Statement(s)
        StrBuilder result = target.appendln(charArray);
        StrBuilder strBuilder2 = new StrBuilder();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder2));
            verify(target).append(charArray);
        });
    }

    //BaseRock generated method id: ${appendln3Test}, hash: E417C75D966B6101F18F017F43B7BA75
    @Test()
    void appendln3Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        char[] charArray = new char[] {};
        doReturn(strBuilder).when(target).append(charArray, 0, 0);
        
        //Act Statement(s)
        StrBuilder result = target.appendln(charArray, 0, 0);
        StrBuilder strBuilder2 = new StrBuilder();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder2));
            verify(target).append(charArray, 0, 0);
        });
    }

    //BaseRock generated method id: ${appendln4Test}, hash: 22B6A29D08A0450F5A32D080A11D02E9
    @Test()
    void appendln4Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).append(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        StrBuilder result = target.appendln(Double.parseDouble("0.0"));
        StrBuilder strBuilder2 = new StrBuilder();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder2));
            verify(target).append(Double.parseDouble("0.0"));
        });
    }

    //BaseRock generated method id: ${appendln5Test}, hash: F0F3FAE47F5A0B4B654020F9816F8D47
    @Test()
    void appendln5Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).append(Float.parseFloat("0.0"));
        
        //Act Statement(s)
        StrBuilder result = target.appendln(Float.parseFloat("0.0"));
        StrBuilder strBuilder2 = new StrBuilder();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder2));
            verify(target).append(Float.parseFloat("0.0"));
        });
    }

    //BaseRock generated method id: ${appendln6Test}, hash: F4199730DFF0D32C067B6904F9F79D59
    @Test()
    void appendln6Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).append(0);
        
        //Act Statement(s)
        StrBuilder result = target.appendln(0);
        StrBuilder strBuilder2 = new StrBuilder();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder2));
            verify(target).append(0);
        });
    }

    //BaseRock generated method id: ${appendln7Test}, hash: DFF548F6155FB21D6E437B8B4A21DB88
    @Test()
    void appendln7Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).append(0L);
        
        //Act Statement(s)
        StrBuilder result = target.appendln(0L);
        StrBuilder strBuilder2 = new StrBuilder();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder2));
            verify(target).append(0L);
        });
    }

    //BaseRock generated method id: ${appendln8Test}, hash: 9C9C78124892F64E9E9E367835BC9B58
    @Test()
    void appendln8Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        Object object = new Object();
        doReturn(strBuilder).when(target).append(object);
        
        //Act Statement(s)
        StrBuilder result = target.appendln(object);
        StrBuilder strBuilder2 = new StrBuilder();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder2));
            verify(target).append(object);
        });
    }

    //BaseRock generated method id: ${appendln9Test}, hash: BE96FD7F98D19DB0A90CF47205E32168
    @Test()
    void appendln9Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        StrBuilder strBuilder2 = new StrBuilder();
        doReturn(strBuilder).when(target).append(strBuilder2);
        
        //Act Statement(s)
        StrBuilder result = target.appendln(strBuilder2);
        StrBuilder strBuilder3 = new StrBuilder();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder3));
            verify(target).append(strBuilder2);
        });
    }

    //BaseRock generated method id: ${appendln10Test}, hash: 19ED4BE0D6A16D640F9B424FD76B2B10
    @Test()
    void appendln10Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        StrBuilder strBuilder2 = new StrBuilder();
        doReturn(strBuilder).when(target).append(strBuilder2, 0, 0);
        
        //Act Statement(s)
        StrBuilder result = target.appendln(strBuilder2, 0, 0);
        StrBuilder strBuilder3 = new StrBuilder();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder3));
            verify(target).append(strBuilder2, 0, 0);
        });
    }

    //BaseRock generated method id: ${appendln11Test}, hash: 01B2C50F2815B71FE603F6C0C77C0791
    @Test()
    void appendln11Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).append("str1");
        
        //Act Statement(s)
        StrBuilder result = target.appendln("str1");
        StrBuilder strBuilder2 = new StrBuilder();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder2));
            verify(target).append("str1");
        });
    }

    //BaseRock generated method id: ${appendln12Test}, hash: 23ADDE6F92FC8173380BD3FE982A0FA6
    @Test()
    void appendln12Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).append("str1", 0, 0);
        
        //Act Statement(s)
        StrBuilder result = target.appendln("str1", 0, 0);
        StrBuilder strBuilder2 = new StrBuilder();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder2));
            verify(target).append("str1", 0, 0);
        });
    }

    //BaseRock generated method id: ${appendln13Test}, hash: 453F931F255DD4FB1530FBD9504735CB
    @Test()
    void appendln13Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        Object[] objectArray = new Object[] {};
        doReturn(strBuilder).when(target).append("format1", objectArray);
        
        //Act Statement(s)
        StrBuilder result = target.appendln("format1", objectArray);
        StrBuilder strBuilder2 = new StrBuilder();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder2));
            verify(target).append("format1", objectArray);
        });
    }

    //BaseRock generated method id: ${appendln14Test}, hash: 96353817603F3B1F154C42DFBF45D7F2
    @Test()
    void appendln14Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        StringBuffer stringBuffer = new StringBuffer();
        doReturn(strBuilder).when(target).append(stringBuffer);
        
        //Act Statement(s)
        StrBuilder result = target.appendln(stringBuffer);
        StrBuilder strBuilder2 = new StrBuilder();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder2));
            verify(target).append(stringBuffer);
        });
    }

    //BaseRock generated method id: ${appendln15Test}, hash: 605EE158879A62A405246F34F9F18CC0
    @Test()
    void appendln15Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        StringBuffer stringBuffer = new StringBuffer();
        doReturn(strBuilder).when(target).append(stringBuffer, 0, 0);
        
        //Act Statement(s)
        StrBuilder result = target.appendln(stringBuffer, 0, 0);
        StrBuilder strBuilder2 = new StrBuilder();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder2));
            verify(target).append(stringBuffer, 0, 0);
        });
    }

    //BaseRock generated method id: ${appendln16Test}, hash: A49BF51A29E6424576BE0B6E0C941185
    @Test()
    void appendln16Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        StringBuilder stringBuilder = new StringBuilder();
        doReturn(strBuilder).when(target).append(stringBuilder);
        
        //Act Statement(s)
        StrBuilder result = target.appendln(stringBuilder);
        StrBuilder strBuilder2 = new StrBuilder();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder2));
            verify(target).append(stringBuilder);
        });
    }

    //BaseRock generated method id: ${appendln17Test}, hash: F0344FD892630B8D3C45914F1047A15B
    @Test()
    void appendln17Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        StringBuilder stringBuilder = new StringBuilder();
        doReturn(strBuilder).when(target).append(stringBuilder, 0, 0);
        
        //Act Statement(s)
        StrBuilder result = target.appendln(stringBuilder, 0, 0);
        StrBuilder strBuilder2 = new StrBuilder();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder2));
            verify(target).append(stringBuilder, 0, 0);
        });
    }

    //BaseRock generated method id: ${appendNewLineWhenNewLineIsNull}, hash: 5A412741E3B90946FA9DF1013169D4DF
    @Test()
    void appendNewLineWhenNewLineIsNull() {
        /* Branches:
         * (newLine == null) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).append("\n");
        
        //Act Statement(s)
        StrBuilder result = target.appendNewLine();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append("\n");
        });
    }

    //BaseRock generated method id: ${appendNullWhenNullTextIsNull}, hash: 7CCBE84503154FED7339329C6D6E0DDF
    @Test()
    void appendNullWhenNullTextIsNull() {
        /* Branches:
         * (nullText == null) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        StrBuilder result = target.appendNull();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${appendPaddingWhenILessThanLength}, hash: 7438DB4680B1A9008D3ED70500BF0342
    @Test()
    void appendPaddingWhenILessThanLength() {
        /* Branches:
         * (length >= 0) : true
         * (i < length) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).ensureCapacity(1);
        
        //Act Statement(s)
        StrBuilder result = target.appendPadding(1, 'A');
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).ensureCapacity(1);
        });
    }

    //BaseRock generated method id: ${appendSeparatorWhenIsNotEmpty}, hash: 97FDE511E2B13917A916FF057059F29B
    @Test()
    void appendSeparatorWhenIsNotEmpty() {
        /* Branches:
         * (isNotEmpty()) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        doReturn(true).when(target).isNotEmpty();
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).append('A');
        
        //Act Statement(s)
        StrBuilder result = target.appendSeparator('A');
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).isNotEmpty();
            verify(target).append('A');
        });
    }

    //BaseRock generated method id: ${appendSeparator1WhenIsNotEmpty}, hash: 6F4420AADFFD35FEB03DD62F951C264C
    @Test()
    void appendSeparator1WhenIsNotEmpty() {
        /* Branches:
         * (isNotEmpty()) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        doReturn(true).when(target).isNotEmpty();
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).append('A');
        
        //Act Statement(s)
        StrBuilder result = target.appendSeparator('A', 'A');
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).isNotEmpty();
            verify(target).append('A');
        });
    }

    //BaseRock generated method id: ${appendSeparator1WhenIsNotEmptyNot}, hash: F0C0744DDFF7AF5FFA54990DF2B5F1DD
    @Test()
    void appendSeparator1WhenIsNotEmptyNot() {
        /* Branches:
         * (isNotEmpty()) : false
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        doReturn(false).when(target).isNotEmpty();
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).append('A');
        
        //Act Statement(s)
        StrBuilder result = target.appendSeparator('A', 'A');
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).isNotEmpty();
            verify(target).append('A');
        });
    }

    //BaseRock generated method id: ${appendSeparator2WhenLoopIndexGreaterThan0}, hash: 8510AA2BF0B7FE16CBBCE9953D55347C
    @Test()
    void appendSeparator2WhenLoopIndexGreaterThan0() {
        /* Branches:
         * (loopIndex > 0) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).append('A');
        
        //Act Statement(s)
        StrBuilder result = target.appendSeparator('A', 1);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append('A');
        });
    }

    //BaseRock generated method id: ${appendSeparator3Test}, hash: 01D1884A66FB6DA3F6389D843FD5C62E
    @Test()
    void appendSeparator3Test() {
        //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).appendSeparator("separator1", (String) null);
        
        //Act Statement(s)
        StrBuilder result = target.appendSeparator("separator1");
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).appendSeparator("separator1", (String) null);
        });
    }

    //BaseRock generated method id: ${appendSeparator4WhenLoopIndexGreaterThan0}, hash: 4CC6DCFA45ABD9A535365D0F27D34CEF
    @Test()
    void appendSeparator4WhenLoopIndexGreaterThan0() {
        /* Branches:
         * (separator != null) : true
         * (loopIndex > 0) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).append("separator1");
        
        //Act Statement(s)
        StrBuilder result = target.appendSeparator("separator1", 1);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append("separator1");
        });
    }

    //BaseRock generated method id: ${appendSeparator5WhenStrIsNotNull}, hash: 60C270D454749B02A0782CBA1AD4A8E8
    @Test()
    void appendSeparator5WhenStrIsNotNull() {
        /* Branches:
         * (isEmpty()) : true
         * (str != null) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        doReturn(true).when(target).isEmpty();
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).append("defaultIfEmpty1");
        
        //Act Statement(s)
        StrBuilder result = target.appendSeparator("standard1", "defaultIfEmpty1");
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).isEmpty();
            verify(target).append("defaultIfEmpty1");
        });
    }

    //BaseRock generated method id: ${appendSeparator5WhenIsEmptyNotAndStrIsNotNull}, hash: 47CA413C064D680B2408899E7E2F8012
    @Test()
    void appendSeparator5WhenIsEmptyNotAndStrIsNotNull() {
        /* Branches:
         * (isEmpty()) : false
         * (str != null) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        doReturn(false).when(target).isEmpty();
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).append("standard1");
        
        //Act Statement(s)
        StrBuilder result = target.appendSeparator("standard1", "defaultIfEmpty1");
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).isEmpty();
            verify(target).append("standard1");
        });
    }

    //BaseRock generated method id: ${appendToWhenAppendableInstanceOfWriter}, hash: 4881D0EBA42B880BF138725BE64B9AC5
    @Test()
    void appendToWhenAppendableInstanceOfWriter() throws IOException {
        /* Branches:
         * (appendable instanceof Writer) : true
         */
        //Arrange Statement(s)
        //StrBuilder target = new StrBuilder(-1);
        //Writer writer = Writer.nullWriter();
        //Act Statement(s)
        //target.appendTo(writer);
    }

    //BaseRock generated method id: ${appendToWhenAppendableInstanceOfStringBuilder}, hash: 345AEC380AFD5205DBCA91483CFD20DD
    @Test()
    void appendToWhenAppendableInstanceOfStringBuilder() throws IOException {
        /* Branches:
         * (appendable instanceof Writer) : false
         * (appendable instanceof StringBuilder) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        StringBuilder stringBuilder = new StringBuilder();
        
        //Act Statement(s)
        target.appendTo(stringBuilder);
    }

    //BaseRock generated method id: ${appendToWhenAppendableInstanceOfStringBuffer}, hash: 41BBF042F85566C9BA625ACBAAE0653A
    @Test()
    void appendToWhenAppendableInstanceOfStringBuffer() throws IOException {
        /* Branches:
         * (appendable instanceof Writer) : false
         * (appendable instanceof StringBuilder) : false
         * (appendable instanceof StringBuffer) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        StringBuffer stringBuffer = new StringBuffer();
        
        //Act Statement(s)
        target.appendTo(stringBuffer);
    }

    //BaseRock generated method id: ${appendToWhenAppendableInstanceOfCharBuffer}, hash: F083C7A624327C21EC9A2AC24D7ECC66
    @Test()
    void appendToWhenAppendableInstanceOfCharBuffer() throws IOException {
        /* Branches:
         * (appendable instanceof Writer) : false
         * (appendable instanceof StringBuilder) : false
         * (appendable instanceof StringBuffer) : false
         * (appendable instanceof CharBuffer) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        CharBuffer charBuffer = CharBuffer.allocate(0);
        
        //Act Statement(s)
        target.appendTo(charBuffer);
    }

    //BaseRock generated method id: ${appendToWhenAppendableNotInstanceOfCharBuffer}, hash: B029F5BFE4D83D709988ECC8C74EAD7B
    @Test()
    void appendToWhenAppendableNotInstanceOfCharBuffer() throws IOException {
        /* Branches:
         * (appendable instanceof Writer) : false
         * (appendable instanceof StringBuilder) : false
         * (appendable instanceof StringBuffer) : false
         * (appendable instanceof CharBuffer) : false
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        Appendable appendableMock = mock(Appendable.class);
        Appendable appendableMock2 = mock(Appendable.class);
        doReturn(appendableMock2).when(appendableMock).append(target);
        
        //Act Statement(s)
        target.appendTo(appendableMock);
        
        //Assert statement(s)
        assertAll("result", () -> verify(appendableMock).append(target));
    }

    //BaseRock generated method id: ${appendWithSeparatorsWhenItHasNext}, hash: 30BBE8F2B6DE51903FF2E632C882F044
    @Test()
    void appendWithSeparatorsWhenItHasNext() {
        /* Branches:
         * (iterable != null) : true
         * (it.hasNext()) : true
         * (it.hasNext()) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        Object object = new Object();
        doReturn(strBuilder).when(target).append(object);
        StrBuilder strBuilder2 = new StrBuilder();
        doReturn(strBuilder2).when(target).append("A");
        Iterable<Object> iterable = new ArrayList<>(Arrays.asList(object));
        
        //Act Statement(s)
        StrBuilder result = target.appendWithSeparators(iterable, "A");
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append(object);
            verify(target).append("A");
        });
    }

    //BaseRock generated method id: ${appendWithSeparators1WhenItHasNext}, hash: 0EEA482325DDDD3EEC832CC67E674FBF
    @Test()
    void appendWithSeparators1WhenItHasNext() {
        /* Branches:
         * (it != null) : true
         * (it.hasNext()) : true
         * (it.hasNext()) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        Object object = new Object();
        doReturn(strBuilder).when(target).append(object);
        StrBuilder strBuilder2 = new StrBuilder();
        doReturn(strBuilder2).when(target).append("A");
        List<Object> anyList = new ArrayList<>();
        anyList.add(object);
        Iterator<?> iteratorIt = anyList.iterator();
        
        //Act Statement(s)
        StrBuilder result = target.appendWithSeparators(iteratorIt, "A");
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append(object);
            verify(target).append("A");
        });
    }

    //BaseRock generated method id: ${appendWithSeparators2WhenILessThanArrayLength}, hash: A533B63FC306EA4FFEE46D1CE623919C
    @Test()
    void appendWithSeparators2WhenILessThanArrayLength() {
        /* Branches:
         * (array != null) : true
         * (array.length > 0) : true
         * (i < array.length) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        Object object = new Object();
        doReturn(strBuilder).when(target).append(object);
        StrBuilder strBuilder2 = new StrBuilder();
        doReturn(strBuilder2).when(target).append("A");
        StrBuilder strBuilder3 = new StrBuilder();
        Object object2 = new Object();
        doReturn(strBuilder3).when(target).append(object2);
        Object[] objectArray = new Object[] { object, object2 };
        
        //Act Statement(s)
        StrBuilder result = target.appendWithSeparators(objectArray, "A");
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append(object);
            verify(target).append("A");
            verify(target).append(object2);
        });
    }

    //BaseRock generated method id: ${asReaderTest}, hash: 4C4FB6347B4FE766A89E6F13E92DBF5B
    @Test()
    void asReaderTest() {
        //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        Reader result = target.asReader();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${asTokenizerTest}, hash: 179589680C533C026FC85C8798ED8B0A
    @Test()
    void asTokenizerTest() {
        //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        StrTokenizer result = target.asTokenizer();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${asWriterTest}, hash: 3119F717BBDA4C0E6BB63665DD6544E3
    @Test()
    void asWriterTest() {
        //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        Writer result = target.asWriter();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${buildTest}, hash: 4F0263616B8FDB7AB6807E0C122A9065
    @Test()
    void buildTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        String result = target.build();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${capacityTest}, hash: DF8600C79F6D3C2D9C8BC0CE1EFD518C
    @Test()
    void capacityTest() {
        //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        int result = target.capacity();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(32)));
    }

    //BaseRock generated method id: ${charAtWhenIndexGreaterThanOrEqualsToLengthThrowsStringIndexOutOfBoundsException}, hash: 088937B8FFF31A6E8964F210750BAE69
    @Test()
    void charAtWhenIndexGreaterThanOrEqualsToLengthThrowsStringIndexOutOfBoundsException() {
        /* Branches:
         * (index < 0) : false
         * (index >= length()) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        //Act Statement(s)
        final StringIndexOutOfBoundsException result = assertThrows(StringIndexOutOfBoundsException.class, () -> {
            target.charAt(0);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${clearTest}, hash: B02C461B5B62705B5BA02535458C0EDB
    @Test()
    void clearTest() {
        //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        StrBuilder result = target.clear();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${containsWhenINotLessThanThisSize}, hash: E5057560A5721BE2D393D37C8F6825F6
    @Test()
    void containsWhenINotLessThanThisSize() {
        /* Branches:
         * (i < this.size) : false
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        boolean result = target.contains('A');
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${contains1WhenIndexOfStr0GreaterThanOrEqualsTo0}, hash: 897F4FF8B2253A2F6859C77957500A74
    @Test()
    void contains1WhenIndexOfStr0GreaterThanOrEqualsTo0() {
        /* Branches:
         * (indexOf(str, 0) >= 0) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: CS - Method: indexOf
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        boolean result = target.contains("str1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${contains1WhenIndexOfStr0LessThan0}, hash: B34CEA42A6C913AB71BD4EC16C7B03E6
    @Test()
    void contains1WhenIndexOfStr0LessThan0() {
        /* Branches:
         * (indexOf(str, 0) >= 0) : false
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        boolean result = target.contains("str1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${contains2WhenIndexOfMatcher0GreaterThanOrEqualsTo0}, hash: 1E598EA52479DEA3ABFCB710F0B4858E
    @Test()
    void contains2WhenIndexOfMatcher0GreaterThanOrEqualsTo0() {
        /* Branches:
         * (indexOf(matcher, 0) >= 0) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        doReturn(1).when(target).indexOf((StrMatcher) any(), eq(0));
        StrMatcher strMatcher = StrMatcher.commaMatcher();
        
        //Act Statement(s)
        boolean result = target.contains(strMatcher);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.TRUE));
            verify(target).indexOf((StrMatcher) any(), eq(0));
        });
    }

    //BaseRock generated method id: ${contains2WhenIndexOfMatcher0LessThan0}, hash: 3D00FC33083A779FC843B1705431D57B
    @Test()
    void contains2WhenIndexOfMatcher0LessThan0() {
        /* Branches:
         * (indexOf(matcher, 0) >= 0) : false
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        doReturn(-1).when(target).indexOf((StrMatcher) any(), eq(0));
        StrMatcher strMatcher = StrMatcher.commaMatcher();
        
        //Act Statement(s)
        boolean result = target.contains(strMatcher);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.FALSE));
            verify(target).indexOf((StrMatcher) any(), eq(0));
        });
    }

    //BaseRock generated method id: ${deleteWhenLenGreaterThan0}, hash: 01FC86E1B75D3C823B86281336F3BDBB
    @Test()
    void deleteWhenLenGreaterThan0() {
        /* Branches:
         * (len > 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        doReturn(1).when(target).validateRange(0, 0);
        
        //Act Statement(s)
        StrBuilder result = target.delete(0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).validateRange(0, 0);
        });
    }

    //BaseRock generated method id: ${deleteAllWhenINotLessThanSize}, hash: 4B95B4AC7C3F5127FB2A517782869CA0
    @Test()
    void deleteAllWhenINotLessThanSize() {
        /* Branches:
         * (i < size) : false
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        StrBuilder result = target.deleteAll('A');
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${deleteAll1WhenIndexGreaterThanOrEqualsTo0}, hash: 0B05C823F67A0D68569536202E24C6B3
    @Test()
    void deleteAll1WhenIndexGreaterThanOrEqualsTo0() {
        /* Branches:
         * (len > 0) : true
         * (index >= 0) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: CS - Method: indexOf
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        
        //Act Statement(s)
        StrBuilder result = target.deleteAll("A");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${deleteAll2Test}, hash: D8D455CC9B303583F7E944359DB3F73B
    @Test()
    void deleteAll2Test() {
        //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).replace((StrMatcher) any(), eq((String) null), eq(0), eq(0), eq(-1));
        StrMatcher strMatcher = StrMatcher.commaMatcher();
        
        //Act Statement(s)
        StrBuilder result = target.deleteAll(strMatcher);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).replace((StrMatcher) any(), eq((String) null), eq(0), eq(0), eq(-1));
        });
    }

    //BaseRock generated method id: ${deleteCharAtWhenIndexGreaterThanOrEqualsToSizeThrowsStringIndexOutOfBoundsException}, hash: EA6C64F5A8951B8FED5AE8CC4337CBEC
    @Test()
    void deleteCharAtWhenIndexGreaterThanOrEqualsToSizeThrowsStringIndexOutOfBoundsException() {
        /* Branches:
         * (index < 0) : false
         * (index >= size) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        //Act Statement(s)
        final StringIndexOutOfBoundsException result = assertThrows(StringIndexOutOfBoundsException.class, () -> {
            target.deleteCharAt(0);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${deleteFirstWhenINotLessThanSize}, hash: DBB5FB16B17073ED4276342F339E8B08
    @Test()
    void deleteFirstWhenINotLessThanSize() {
        /* Branches:
         * (i < size) : false
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        StrBuilder result = target.deleteFirst('A');
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${deleteFirst1WhenIndexGreaterThanOrEqualsTo0}, hash: 8CA89BFF1BB0D6C088D86287D71872AE
    @Test()
    void deleteFirst1WhenIndexGreaterThanOrEqualsTo0() {
        /* Branches:
         * (len > 0) : true
         * (index >= 0) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: CS - Method: indexOf
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        
        //Act Statement(s)
        StrBuilder result = target.deleteFirst("A");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${deleteFirst2Test}, hash: 2DCF7E2FE90CEA2CA44BFFDC181E1B49
    @Test()
    void deleteFirst2Test() {
        //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).replace((StrMatcher) any(), eq((String) null), eq(0), eq(0), eq(1));
        StrMatcher strMatcher = StrMatcher.commaMatcher();
        
        //Act Statement(s)
        StrBuilder result = target.deleteFirst(strMatcher);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).replace((StrMatcher) any(), eq((String) null), eq(0), eq(0), eq(1));
        });
    }

    //BaseRock generated method id: ${endsWithWhenStrIsNull}, hash: 5CCB0E19B4E9BC403B7647293A26F2AC
    @Test()
    void endsWithWhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        boolean result = target.endsWith((String) null);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${endsWithWhenLenEquals0}, hash: 50E4B44119BA8D604C6FCD0F6C63195A
    @Test()
    void endsWithWhenLenEquals0() {
        /* Branches:
         * (str == null) : false
         * (len == 0) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        
        //Act Statement(s)
        boolean result = target.endsWith("");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${endsWithWhenLenGreaterThanSize}, hash: 1606387ABFC9E6D37D910B942F061362
    @Test()
    void endsWithWhenLenGreaterThanSize() {
        /* Branches:
         * (str == null) : false
         * (len == 0) : false
         * (len > size) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        
        //Act Statement(s)
        boolean result = target.endsWith("A");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${ensureCapacityWhenCapacityGreaterThanBufferLength}, hash: 58CB77349A7D65C47CF75E654AB9CD2A
    @Test()
    void ensureCapacityWhenCapacityGreaterThanBufferLength() {
        /* Branches:
         * (capacity > buffer.length) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        
        //Act Statement(s)
        StrBuilder result = target.ensureCapacity(33);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${equalsWhenEqualsObj}, hash: F90ABB05015E192595CEFF668D5D85E8
    @Test()
    void equalsWhenEqualsObj() {
        /* Branches:
         * (obj instanceof StrBuilder) : true
         * (equals((StrBuilder) obj)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        StrBuilder strBuilder = new StrBuilder();
        
        //Act Statement(s)
        boolean result = target.equals((Object) strBuilder);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${equalsWhenEqualsNotObj}, hash: A51131238123B44797AAB069F55F6EE3
    @Test()
    void equalsWhenEqualsNotObj() {
        /* Branches:
         * (obj instanceof StrBuilder) : true
         * (equals((StrBuilder) obj)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        StrBuilder strBuilder = new StrBuilder();
        
        //Act Statement(s)
        boolean result = target.equals((Object) strBuilder);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${equals1WhenThisEqualsOther}, hash: AE7A7BCC1B580F4AA7836AE9BA0E9591
    @Test()
    void equals1WhenThisEqualsOther() {
        /* Branches:
         * (this == other) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        boolean result = target.equals(target);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${equals1WhenOtherIsNull}, hash: EAF2B9E87D557A6FD28D9ECB241719CE
    @Test()
    void equals1WhenOtherIsNull() {
        /* Branches:
         * (this == other) : false
         * (other == null) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        StrBuilder strBuilder = null;
        
        //Act Statement(s)
        boolean result = target.equals(strBuilder);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${equals1WhenILessThan0}, hash: 52E89DAF86859EB20E5ED69E00C43C3E
    @Test()
    void equals1WhenILessThan0() {
        /* Branches:
         * (this == other) : false
         * (other == null) : false
         * (this.size != other.size) : false
         * (i >= 0) : false
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        StrBuilder strBuilder = new StrBuilder();
        
        //Act Statement(s)
        boolean result = target.equals(strBuilder);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${equalsIgnoreCaseWhenThisEqualsOther}, hash: 476BF572722B0485F40ED35058917004
    @Test()
    void equalsIgnoreCaseWhenThisEqualsOther() {
        /* Branches:
         * (this == other) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        boolean result = target.equalsIgnoreCase(target);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${equalsIgnoreCaseWhenILessThan0}, hash: F59173DA72EEC37D15AB47CB543641B2
    @Test()
    void equalsIgnoreCaseWhenILessThan0() {
        /* Branches:
         * (this == other) : false
         * (this.size != other.size) : false
         * (i >= 0) : false
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        StrBuilder strBuilder = new StrBuilder();
        
        //Act Statement(s)
        boolean result = target.equalsIgnoreCase(strBuilder);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${getCharsWhenDestinationLengthLessThanLen}, hash: 1ACED6BB0D0B46683D4FFA0D1BDB7563
    @Test()
    void getCharsWhenDestinationLengthLessThanLen() {
        /* Branches:
         * (destination == null) : false
         * (destination.length < len) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        char[] charArray = new char[] {};
        
        //Act Statement(s)
        char[] result = target.getChars(charArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${getChars1WhenStartIndexLessThan0ThrowsStringIndexOutOfBoundsException}, hash: D3DB1503EAA7E38BA2CDFB639A5C8395
    @Test()
    void getChars1WhenStartIndexLessThan0ThrowsStringIndexOutOfBoundsException() {
        /* Branches:
         * (startIndex < 0) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        char[] charArray = new char[] {};
        //Act Statement(s)
        final StringIndexOutOfBoundsException result = assertThrows(StringIndexOutOfBoundsException.class, () -> {
            target.getChars(-1, 0, charArray, 0);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getChars1WhenEndIndexGreaterThanLengthThrowsStringIndexOutOfBoundsException}, hash: 048982D5D8A98BEC0D9F87AF12064B9A
    @Test()
    void getChars1WhenEndIndexGreaterThanLengthThrowsStringIndexOutOfBoundsException() {
        /* Branches:
         * (startIndex < 0) : false
         * (endIndex < 0) : false
         * (endIndex > length()) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        char[] charArray = new char[] {};
        //Act Statement(s)
        final StringIndexOutOfBoundsException result = assertThrows(StringIndexOutOfBoundsException.class, () -> {
            target.getChars(0, 1, charArray, 0);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getChars1WhenStartIndexGreaterThanEndIndexThrowsStringIndexOutOfBoundsException}, hash: 7CB215DB97135FA34DF02D2F8A2E1E3D
    @Test()
    void getChars1WhenStartIndexGreaterThanEndIndexThrowsStringIndexOutOfBoundsException() {
        /* Branches:
         * (startIndex < 0) : false
         * (endIndex < 0) : false
         * (endIndex > length()) : false
         * (startIndex > endIndex) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        char[] charArray = new char[] {};
        StringIndexOutOfBoundsException stringIndexOutOfBoundsException = new StringIndexOutOfBoundsException("end < start");
        //Act Statement(s)
        final StringIndexOutOfBoundsException result = assertThrows(StringIndexOutOfBoundsException.class, () -> {
            target.getChars(1, 0, charArray, 0);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(stringIndexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getChars1WhenStartIndexNotGreaterThanEndIndex}, hash: 56B213EAB22275FC5A2E2B7791EFFCAF
    @Test()
    void getChars1WhenStartIndexNotGreaterThanEndIndex() {
        /* Branches:
         * (startIndex < 0) : false
         * (endIndex < 0) : false
         * (endIndex > length()) : false
         * (startIndex > endIndex) : false
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        char[] charArray = new char[] {};
        
        //Act Statement(s)
        target.getChars(0, 0, charArray, 0);
    }

    //BaseRock generated method id: ${getNewLineTextTest}, hash: B13EEA1D3AD20A1C02D0E9C992194952
    @Test()
    void getNewLineTextTest() {
        //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        String result = target.getNewLineText();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getNullTextTest}, hash: 1F9AAC8617E179A61D737D064EB0DABB
    @Test()
    void getNullTextTest() {
        //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        String result = target.getNullText();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${indexOfTest}, hash: 16F9BBCBC5C96F22F4ECA632285FEAB6
    @Test()
    void indexOfTest() {
        //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        doReturn(0).when(target).indexOf('A', 0);
        
        //Act Statement(s)
        int result = target.indexOf('A');
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(0));
            verify(target).indexOf('A', 0);
        });
    }

    //BaseRock generated method id: ${indexOf1WhenStartIndexGreaterThanOrEqualsToSize}, hash: B04728570A845AB6DA7D8F5614210C78
    @Test()
    void indexOf1WhenStartIndexGreaterThanOrEqualsToSize() {
        /* Branches:
         * (startIndex >= size) : true
         */
        //Arrange Statement(s)
        //StrBuilder target = new StrBuilder(-1);
        //Act Statement(s)
        //int result = target.indexOf('A', -2147483649);
        //Assert statement(s)
        //assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${indexOf1WhenIIndexOfThisBufEqualsCh}, hash: 4E516C3F4090DB927DAD7DEAD07113B6
    @Test()
    void indexOf1WhenIIndexOfThisBufEqualsCh() {
        /* Branches:
         * (startIndex >= size) : false
         * (i < size) : true
         * (thisBuf[i] == ch) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        
        //Act Statement(s)
        int result = target.indexOf('A', 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${indexOf1WhenIIndexOfThisBufNotEqualsCh}, hash: 6569090DA96CC31242D7ED1089F20F25
    @Test()
    void indexOf1WhenIIndexOfThisBufNotEqualsCh() {
        /* Branches:
         * (startIndex >= size) : false
         * (i < size) : true
         * (thisBuf[i] == ch) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        
        //Act Statement(s)
        int result = target.indexOf('A', 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${indexOf2Test}, hash: 1BAA85D82D4A62B643FFF8A1825AD475
    @Test()
    void indexOf2Test() {
        //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        int result = target.indexOf("str1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${indexOf3Test}, hash: 04AB7B12D485E485D4D82AB846FA2404
    @Test()
    void indexOf3Test() {
        //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        int result = target.indexOf("str1", 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${indexOf4Test}, hash: F354B4892B81FA4DDD5B5E0BC985BD0B
    @Test()
    void indexOf4Test() {
        //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        doReturn(0).when(target).indexOf((StrMatcher) any(), eq(0));
        StrMatcher strMatcher = StrMatcher.commaMatcher();
        
        //Act Statement(s)
        int result = target.indexOf(strMatcher);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(0));
            verify(target).indexOf((StrMatcher) any(), eq(0));
        });
    }

    //BaseRock generated method id: ${indexOf5WhenStartIndexGreaterThanOrEqualsToSize}, hash: DBDF5275CAADBB9F65133716AFE17DF4
    @Test()
    void indexOf5WhenStartIndexGreaterThanOrEqualsToSize() {
        /* Branches:
         * (matcher == null) : false
         * (startIndex >= size) : true
         */
        //Arrange Statement(s)
        //StrBuilder target = new StrBuilder(-1);
        //StrMatcher strMatcher = StrMatcher.commaMatcher();
        //Act Statement(s)
        //int result = target.indexOf(strMatcher, -2147483649);
        //Assert statement(s)
        //assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${indexOf5WhenMatcherIsMatchBufIStartIndexLenGreaterThan0}, hash: 8DCD6EBA2D76A01E3F47A4A59C0265AD
    @Test()
    void indexOf5WhenMatcherIsMatchBufIStartIndexLenGreaterThan0() {
        /* Branches:
         * (matcher == null) : false
         * (startIndex >= size) : false
         * (i < len) : true
         * (matcher.isMatch(buf, i, startIndex, len) > 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        StrMatcher strMatcher = StrMatcher.commaMatcher();
        
        //Act Statement(s)
        int result = target.indexOf(strMatcher, 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${indexOf5WhenMatcherIsMatchBufIStartIndexLenNotGreaterThan0}, hash: 8D9EE0A559A0837DDB00164C54EE2783
    @Test()
    void indexOf5WhenMatcherIsMatchBufIStartIndexLenNotGreaterThan0() {
        /* Branches:
         * (matcher == null) : false
         * (startIndex >= size) : false
         * (i < len) : true
         * (matcher.isMatch(buf, i, startIndex, len) > 0) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        StrMatcher strMatcher = StrMatcher.commaMatcher();
        
        //Act Statement(s)
        int result = target.indexOf(strMatcher, 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${insertWhenValue}, hash: 6854AAAFE83BEAFC572AB941DCCBAA72
    @Test()
    void insertWhenValue() {
        /* Branches:
         * (value) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        doNothing().when(target).validateIndex(0);
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).ensureCapacity(4);
        
        //Act Statement(s)
        StrBuilder result = target.insert(0, true);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).validateIndex(0);
            verify(target).ensureCapacity(4);
        });
    }

    //BaseRock generated method id: ${insertWhenNotValue}, hash: BE7892EBF6E5D7CCC0C5AA1BA3F048CB
    @Test()
    void insertWhenNotValue() {
        /* Branches:
         * (value) : false
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        doNothing().when(target).validateIndex(0);
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).ensureCapacity(5);
        
        //Act Statement(s)
        StrBuilder result = target.insert(0, false);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).validateIndex(0);
            verify(target).ensureCapacity(5);
        });
    }

    //BaseRock generated method id: ${insert1Test}, hash: 186CD76876DB6646FC4E0235953C2ED7
    @Test()
    void insert1Test() {
        //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        doNothing().when(target).validateIndex(0);
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).ensureCapacity(1);
        
        //Act Statement(s)
        StrBuilder result = target.insert(0, 'A');
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).validateIndex(0);
            verify(target).ensureCapacity(1);
        });
    }

    //BaseRock generated method id: ${insert2WhenCharsIsNull}, hash: B07027D021C9A55380F449024C98695E
    @Test()
    void insert2WhenCharsIsNull() {
        /* Branches:
         * (chars == null) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        doNothing().when(target).validateIndex(0);
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).insert(0, (String) null);
        char[] _char = null;
        
        //Act Statement(s)
        StrBuilder result = target.insert(0, _char);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).validateIndex(0);
            verify(target).insert(0, (String) null);
        });
    }

    //BaseRock generated method id: ${insert2WhenLenGreaterThan0}, hash: DAC5D80234E064BB213DBCD08F0191BB
    @Test()
    void insert2WhenLenGreaterThan0() {
        /* Branches:
         * (chars == null) : false
         * (len > 0) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        doNothing().when(target).validateIndex(0);
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).ensureCapacity(1);
        char[] charArray = new char[] { 'A' };
        
        //Act Statement(s)
        StrBuilder result = target.insert(0, charArray);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).validateIndex(0);
            verify(target).ensureCapacity(1);
        });
    }

    //BaseRock generated method id: ${insert3WhenCharsIsNull}, hash: 8DB3E73E17BD5A70C48CE5B835CF9542
    @Test()
    void insert3WhenCharsIsNull() {
        /* Branches:
         * (chars == null) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        doNothing().when(target).validateIndex(0);
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).insert(0, (String) null);
        char[] _char = null;
        
        //Act Statement(s)
        StrBuilder result = target.insert(0, _char, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).validateIndex(0);
            verify(target).insert(0, (String) null);
        });
    }

    //BaseRock generated method id: ${insert3WhenOffsetGreaterThanCharsLengthThrowsStringIndexOutOfBoundsException}, hash: 3F5215FEBB4D751A2519F4C13A6F0E12
    @Test()
    void insert3WhenOffsetGreaterThanCharsLengthThrowsStringIndexOutOfBoundsException() {
        /* Branches:
         * (chars == null) : false
         * (offset < 0) : false
         * (offset > chars.length) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        doNothing().when(target).validateIndex(0);
        char[] charArray = new char[] {};
        StringIndexOutOfBoundsException stringIndexOutOfBoundsException = new StringIndexOutOfBoundsException("Invalid offset: 2");
        //Act Statement(s)
        final StringIndexOutOfBoundsException result = assertThrows(StringIndexOutOfBoundsException.class, () -> {
            target.insert(0, charArray, 2, 0);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(stringIndexOutOfBoundsException.getMessage()));
            verify(target).validateIndex(0);
        });
    }

    //BaseRock generated method id: ${insert3WhenOffsetPlusLengthGreaterThanCharsLengthThrowsStringIndexOutOfBoundsException}, hash: FED590E7AC8E789891D61761C44D6395
    @Test()
    void insert3WhenOffsetPlusLengthGreaterThanCharsLengthThrowsStringIndexOutOfBoundsException() {
        /* Branches:
         * (chars == null) : false
         * (offset < 0) : false
         * (offset > chars.length) : false
         * (length < 0) : false
         * (offset + length > chars.length) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        doNothing().when(target).validateIndex(0);
        char[] charArray = new char[] {};
        StringIndexOutOfBoundsException stringIndexOutOfBoundsException = new StringIndexOutOfBoundsException("Invalid length: 2");
        //Act Statement(s)
        final StringIndexOutOfBoundsException result = assertThrows(StringIndexOutOfBoundsException.class, () -> {
            target.insert(0, charArray, 0, 2);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(stringIndexOutOfBoundsException.getMessage()));
            verify(target).validateIndex(0);
        });
    }

    //BaseRock generated method id: ${insert3WhenLengthGreaterThan0}, hash: 7C90E5D9B5207737B398A6D1792784A2
    @Test()
    void insert3WhenLengthGreaterThan0() {
        /* Branches:
         * (chars == null) : false
         * (offset < 0) : false
         * (offset > chars.length) : false
         * (length < 0) : false
         * (offset + length > chars.length) : false
         * (length > 0) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        doNothing().when(target).validateIndex(0);
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).ensureCapacity(1);
        char[] charArray = new char[] { 'A' };
        
        //Act Statement(s)
        StrBuilder result = target.insert(0, charArray, 0, 1);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).validateIndex(0);
            verify(target).ensureCapacity(1);
        });
    }

    //BaseRock generated method id: ${insert4Test}, hash: B482F12E40EF443F8AC1CF7C10090A07
    @Test()
    void insert4Test() {
        //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).insert(0, "0.0");
        
        //Act Statement(s)
        StrBuilder result = target.insert(0, Double.parseDouble("0.0"));
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).insert(0, "0.0");
        });
    }

    //BaseRock generated method id: ${insert5Test}, hash: C7F3BC5715E63AB1F3BC1C6B8C295634
    @Test()
    void insert5Test() {
        //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).insert(0, "0.0");
        
        //Act Statement(s)
        StrBuilder result = target.insert(0, Float.parseFloat("0.0"));
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).insert(0, "0.0");
        });
    }

    //BaseRock generated method id: ${insert6Test}, hash: DEB884A6EFF07D4DD4092DBD854C4F7B
    @Test()
    void insert6Test() {
        //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).insert(0, "2");
        
        //Act Statement(s)
        StrBuilder result = target.insert(0, 2);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).insert(0, "2");
        });
    }

    //BaseRock generated method id: ${insert7Test}, hash: BA3740D942094E14CF3AE423E9F1D759
    @Test()
    void insert7Test() {
        //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).insert(0, "2");
        
        //Act Statement(s)
        StrBuilder result = target.insert(0, 2L);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).insert(0, "2");
        });
    }

    //BaseRock generated method id: ${insert8WhenObjIsNull}, hash: 4E8FE1B8FE4B2A313DDB2D53DC879D1A
    @Test()
    void insert8WhenObjIsNull() {
        /* Branches:
         * (obj == null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        Object object = null;
        
        //Act Statement(s)
        StrBuilder result = target.insert(0, object);
        StrBuilder strBuilder = new StrBuilder();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(strBuilder)));
    }

    //BaseRock generated method id: ${insert8WhenObjIsNotNull}, hash: C132680A98176E18D67B0B42E445BE66
    @Test()
    void insert8WhenObjIsNotNull() {
        /* Branches:
         * (obj == null) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        StrBuilder result = target.insert(0, objectMock);
        StrBuilder strBuilder = new StrBuilder();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(strBuilder)));
    }

    //BaseRock generated method id: ${insert9WhenStrIsNull}, hash: B20E74CCE05A96A2E7D868D66A88693D
    @Test()
    void insert9WhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         * (str != null) : false
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        doNothing().when(target).validateIndex(0);
        
        //Act Statement(s)
        StrBuilder result = target.insert(0, (String) null);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).validateIndex(0);
        });
    }

    //BaseRock generated method id: ${insert9WhenStrLenGreaterThan0}, hash: 7403C2EE67E5877CEA1E9FADE90CF6A1
    @Test()
    void insert9WhenStrLenGreaterThan0() {
        /* Branches:
         * (str == null) : false
         * (str != null) : true
         * (strLen > 0) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        doNothing().when(target).validateIndex(0);
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).ensureCapacity(1);
        
        //Act Statement(s)
        StrBuilder result = target.insert(0, "A");
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).validateIndex(0);
            verify(target).ensureCapacity(1);
        });
    }

    //BaseRock generated method id: ${isEmptyWhenSizeEquals0}, hash: 69664DDA2829B5BA0C6180DB7DA967D8
    @Test()
    void isEmptyWhenSizeEquals0() {
        /* Branches:
         * (size == 0) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        boolean result = target.isEmpty();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isNotEmptyWhenSizeNotGreaterThan0}, hash: F0991B4AC6D4981C9328ED74FAFAA360
    @Test()
    void isNotEmptyWhenSizeNotGreaterThan0() {
        /* Branches:
         * (size > 0) : false
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        boolean result = target.isNotEmpty();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${lastIndexOfTest}, hash: 51DBDED2D94808FF420608EC6C3389F6
    @Test()
    void lastIndexOfTest() {
        //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        doReturn(0).when(target).lastIndexOf('A', -1);
        
        //Act Statement(s)
        int result = target.lastIndexOf('A');
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(0));
            verify(target).lastIndexOf('A', -1);
        });
    }

    //BaseRock generated method id: ${lastIndexOf1WhenStartIndexLessThan0}, hash: 32358E8A3DE1AAEE29130C79DC70ECC0
    @Test()
    void lastIndexOf1WhenStartIndexLessThan0() {
        /* Branches:
         * (startIndex >= size) : true
         * (startIndex < 0) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        int result = target.lastIndexOf('A', 1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${lastIndexOf1WhenStartIndexLessThanSizeAndStartIndexLessThan0}, hash: 84A3450D6AA239AED6D5E031ECB8F4D7
    @Test()
    void lastIndexOf1WhenStartIndexLessThanSizeAndStartIndexLessThan0() {
        /* Branches:
         * (startIndex >= size) : false
         * (startIndex < 0) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        
        //Act Statement(s)
        int result = target.lastIndexOf('A', -1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${lastIndexOf2Test}, hash: C1EDD987CCB4F283DC27A878D55449AD
    @Test()
    void lastIndexOf2Test() {
        //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        int result = target.lastIndexOf("str1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${lastIndexOf3Test}, hash: 184FE18BC2F627F006E066998A792F22
    @Test()
    void lastIndexOf3Test() {
        //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        int result = target.lastIndexOf("str1", 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${lastIndexOf4Test}, hash: 594D138B5879A9BEAA651CAADCC972F1
    @Test()
    void lastIndexOf4Test() {
        //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        doReturn(0).when(target).lastIndexOf((StrMatcher) any(), eq(0));
        StrMatcher strMatcher = StrMatcher.commaMatcher();
        
        //Act Statement(s)
        int result = target.lastIndexOf(strMatcher);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(0));
            verify(target).lastIndexOf((StrMatcher) any(), eq(0));
        });
    }

    //BaseRock generated method id: ${lastIndexOf5WhenStartIndexLessThan0}, hash: D332C110481CB8305322FDC268093D78
    @Test()
    void lastIndexOf5WhenStartIndexLessThan0() {
        /* Branches:
         * (startIndex >= size) : true
         * (matcher == null) : false
         * (startIndex < 0) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        StrMatcher strMatcher = StrMatcher.commaMatcher();
        
        //Act Statement(s)
        int result = target.lastIndexOf(strMatcher, 1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${lastIndexOf5WhenMatcherIsNotNullAndStartIndexLessThan0}, hash: BB54C4EAE2E47B2A2BDD98ABBAD2F8D3
    @Test()
    void lastIndexOf5WhenMatcherIsNotNullAndStartIndexLessThan0() {
        /* Branches:
         * (startIndex >= size) : false
         * (matcher == null) : false
         * (startIndex < 0) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        StrMatcher strMatcher = StrMatcher.commaMatcher();
        
        //Act Statement(s)
        int result = target.lastIndexOf(strMatcher, -1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${leftStringWhenLengthLessThanOrEqualsTo0}, hash: 6A594AE217715D7599B2744087627B5B
    @Test()
    void leftStringWhenLengthLessThanOrEqualsTo0() {
        /* Branches:
         * (length <= 0) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        String result = target.leftString(-1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${leftStringWhenLengthGreaterThanOrEqualsToSize}, hash: 4B0DBF722A6783CC330D73890D0E3A77
    @Test()
    void leftStringWhenLengthGreaterThanOrEqualsToSize() {
        /* Branches:
         * (length <= 0) : false
         * (length >= size) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        
        //Act Statement(s)
        String result = target.leftString(1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${lengthTest}, hash: 87400C7D25C08941608A362ACACA9BC2
    @Test()
    void lengthTest() {
        //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        int result = target.length();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${midStringWhenIndexGreaterThanOrEqualsToSize}, hash: FDEBFB4BA9C30835FB90D0E410948C56
    @Test()
    void midStringWhenIndexGreaterThanOrEqualsToSize() {
        /* Branches:
         * (index < 0) : true
         * (length <= 0) : false
         * (index >= size) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        String result = target.midString(-1, 1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${minimizeCapacityWhenBufferLengthGreaterThanLength}, hash: 0089A2011A0D80AB870E5FDBE82FC4AC
    @Test()
    void minimizeCapacityWhenBufferLengthGreaterThanLength() {
        /* Branches:
         * (buffer.length > length()) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        StrBuilder result = target.minimizeCapacity();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${readFromWhenReadAssignedRReadBufferSizeBufferLengthMinusSizeNotEqualsMinus1}, hash: F187808C3FF9E745FD4666ADC28834A2
    @Test()
    void readFromWhenReadAssignedRReadBufferSizeBufferLengthMinusSizeNotEqualsMinus1() throws IOException {
        /* Branches:
         * (readable instanceof Reader) : true
         * ((read = r.read(buffer, size, buffer.length - size)) != -1) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        //StrBuilder target = spy(new StrBuilder(-1));
        //StrBuilder strBuilder = new StrBuilder();
        //doReturn(strBuilder).when(target).ensureCapacity(1);
        //StrBuilder strBuilder2 = new StrBuilder();
        //doReturn(strBuilder2).when(target).ensureCapacity(2);
        //Reader reader = Reader.nullReader();
        //Act Statement(s)
        //int result = target.readFrom(reader);
        //Assert statement(s)
        /*assertAll("result", () -> {
    assertThat(result, equalTo(1));
    verify(target).ensureCapacity(1);
    verify(target).ensureCapacity(2);
});*/
    }

    //BaseRock generated method id: ${readFromWhenReadableInstanceOfCharBuffer}, hash: 23F15A1C3AD77407095CC3B35A488B8F
    @Test()
    void readFromWhenReadableInstanceOfCharBuffer() throws IOException {
        /* Branches:
         * (readable instanceof Reader) : false
         * (readable instanceof CharBuffer) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).ensureCapacity(0);
        CharBuffer charBuffer = CharBuffer.allocate(0);
        
        //Act Statement(s)
        int result = target.readFrom(charBuffer);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(0));
            verify(target).ensureCapacity(0);
        });
    }

    //BaseRock generated method id: ${readFromWhenReadEqualsMinus1}, hash: 09F39183FEA9FAD9B03B6615BBAFBF37
    @Test()
    void readFromWhenReadEqualsMinus1() throws IOException {
        /* Branches:
         * (readable instanceof Reader) : false
         * (readable instanceof CharBuffer) : false
         * (read == -1) : true
         */
         //Arrange Statement(s)
        doReturn(-1).when(readableMock).read((CharBuffer) any());
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).ensureCapacity(1);
        
        //Act Statement(s)
        int result = target.readFrom(readableMock);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(0));
            verify(readableMock).read((CharBuffer) any());
            verify(target).ensureCapacity(1);
        });
    }

    //BaseRock generated method id: ${readFromWhenReadNotEqualsMinus1}, hash: 71F8C44A8CDCEA5CB48D5DBB53133E55
    @Test()
    void readFromWhenReadNotEqualsMinus1() throws IOException {
        /* Branches:
         * (readable instanceof Reader) : false
         * (readable instanceof CharBuffer) : false
         * (read == -1) : false
         */
        doReturn(1, 0).when(readableMock).read((CharBuffer) any());
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).ensureCapacity(1);
        StrBuilder strBuilder2 = new StrBuilder();
        doReturn(strBuilder2).when(target).ensureCapacity(2);
        
        //Act Statement(s)
        int result = target.readFrom(readableMock);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(1));
            verify(readableMock, atLeast(2)).read((CharBuffer) any());
            verify(target).ensureCapacity(1);
            verify(target).ensureCapacity(2);
        });
    }

    //BaseRock generated method id: ${replaceWhenInsertLenGreaterThan0}, hash: C9ABA544C8A3E3FBE47A321389AEB296
    @Test()
    void replaceWhenInsertLenGreaterThan0() {
        /* Branches:
         * (insertLen != removeLen) : true  #  inside replaceImpl method
         * (insertLen > 0) : true  #  inside replaceImpl method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        doReturn(0).when(target).validateRange(-2, 0);
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).ensureCapacity(-1);
        
        //Act Statement(s)
        StrBuilder result = target.replace(-2, 0, "A");
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).validateRange(-2, 0);
            verify(target).ensureCapacity(-1);
        });
    }

    //BaseRock generated method id: ${replace1WhenSizeEquals0}, hash: 7FB4B499FE88A48FA105047315756B6F
    @Test()
    void replace1WhenSizeEquals0() {
        /* Branches:
         * (matcher == null) : false  #  inside replaceImpl method
         * (size == 0) : true  #  inside replaceImpl method
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        doReturn(0).when(target).validateRange(0, 0);
        StrMatcher strMatcher = StrMatcher.commaMatcher();
        
        //Act Statement(s)
        StrBuilder result = target.replace(strMatcher, "replaceStr1", 0, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).validateRange(0, 0);
        });
    }

    //BaseRock generated method id: ${replaceAllWhenINotLessThanSize}, hash: 81FB980B0BA916BD722BEAAA62DC7D64
    @Test()
    void replaceAllWhenINotLessThanSize() {
        /* Branches:
         * (search != replace) : true
         * (i < size) : false
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        
        //Act Statement(s)
        StrBuilder result = target.replaceAll('A', 'B');
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${replaceAll1WhenInsertLenGreaterThan0}, hash: 36C872949F3470E7874DBEC626312141
    @Test()
    void replaceAll1WhenInsertLenGreaterThan0() {
        /* Branches:
         * (searchLen > 0) : true
         * (index >= 0) : true
         * (insertLen != removeLen) : true  #  inside replaceImpl method
         * (insertLen > 0) : true  #  inside replaceImpl method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: CS - Method: indexOf
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).ensureCapacity(0);
        
        //Act Statement(s)
        StrBuilder result = target.replaceAll("searchStr1", "replaceStr1");
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).ensureCapacity(0);
        });
    }

    //BaseRock generated method id: ${replaceAll2Test}, hash: 798C8E06098F21E1BAD49986589C23A3
    @Test()
    void replaceAll2Test() {
        //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).replace((StrMatcher) any(), eq("replaceStr1"), eq(0), eq(0), eq(-1));
        StrMatcher strMatcher = StrMatcher.commaMatcher();
        
        //Act Statement(s)
        StrBuilder result = target.replaceAll(strMatcher, "replaceStr1");
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).replace((StrMatcher) any(), eq("replaceStr1"), eq(0), eq(0), eq(-1));
        });
    }

    //BaseRock generated method id: ${replaceFirstWhenINotLessThanSize}, hash: 82D259E7540B9CDD5DBB0DBC96430FA3
    @Test()
    void replaceFirstWhenINotLessThanSize() {
        /* Branches:
         * (search != replace) : true
         * (i < size) : false
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        
        //Act Statement(s)
        StrBuilder result = target.replaceFirst('A', 'B');
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${replaceFirst1WhenInsertLenGreaterThan0}, hash: 624D1DA2F9C2DC840CEB38D0C32D54DE
    @Test()
    void replaceFirst1WhenInsertLenGreaterThan0() {
        /* Branches:
         * (searchLen > 0) : true
         * (index >= 0) : true
         * (insertLen != removeLen) : true  #  inside replaceImpl method
         * (insertLen > 0) : true  #  inside replaceImpl method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: CS - Method: indexOf
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).ensureCapacity(0);
        
        //Act Statement(s)
        StrBuilder result = target.replaceFirst("searchStr1", "replaceStr1");
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).ensureCapacity(0);
        });
    }

    //BaseRock generated method id: ${replaceFirst2Test}, hash: 925FF00EB5F302D1203816BC2E96F069
    @Test()
    void replaceFirst2Test() {
        //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).replace((StrMatcher) any(), eq("replaceStr1"), eq(0), eq(0), eq(1));
        StrMatcher strMatcher = StrMatcher.commaMatcher();
        
        //Act Statement(s)
        StrBuilder result = target.replaceFirst(strMatcher, "replaceStr1");
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strBuilder));
            verify(target).replace((StrMatcher) any(), eq("replaceStr1"), eq(0), eq(0), eq(1));
        });
    }

    //BaseRock generated method id: ${reverseWhenSizeEquals0}, hash: 3754020D3B6C0FB1A1C3BC8337C532F6
    @Test()
    void reverseWhenSizeEquals0() {
        /* Branches:
         * (size == 0) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        StrBuilder result = target.reverse();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${rightStringWhenLengthLessThanOrEqualsTo0}, hash: B4F10256531858C45849CD11C4F5857D
    @Test()
    void rightStringWhenLengthLessThanOrEqualsTo0() {
        /* Branches:
         * (length <= 0) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        String result = target.rightString(-1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${rightStringWhenLengthGreaterThanOrEqualsToSize}, hash: C74E7B20FB9E3DE2E205B3EB76217D17
    @Test()
    void rightStringWhenLengthGreaterThanOrEqualsToSize() {
        /* Branches:
         * (length <= 0) : false
         * (length >= size) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        
        //Act Statement(s)
        String result = target.rightString(1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${setCharAtWhenIndexGreaterThanOrEqualsToLengthThrowsStringIndexOutOfBoundsException}, hash: 72FFAD6B40502AB8F790A3745F40D6E2
    @Test()
    void setCharAtWhenIndexGreaterThanOrEqualsToLengthThrowsStringIndexOutOfBoundsException() {
        /* Branches:
         * (index < 0) : false
         * (index >= length()) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        //Act Statement(s)
        final StringIndexOutOfBoundsException result = assertThrows(StringIndexOutOfBoundsException.class, () -> {
            target.setCharAt(0, 'A');
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${setLengthWhenLengthLessThan0ThrowsStringIndexOutOfBoundsException}, hash: 95F113D56986BFA60DFB419163E620D2
    @Test()
    void setLengthWhenLengthLessThan0ThrowsStringIndexOutOfBoundsException() {
        /* Branches:
         * (length < 0) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        //Act Statement(s)
        final StringIndexOutOfBoundsException result = assertThrows(StringIndexOutOfBoundsException.class, () -> {
            target.setLength(-1);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${setLengthWhenILessThanLength}, hash: 38AA698D30C5BE3EAC7E25EC59B1381E
    @Test()
    void setLengthWhenILessThanLength() {
        /* Branches:
         * (length < 0) : false
         * (length < size) : false
         * (length > size) : true
         * (i < length) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(strBuilder).when(target).ensureCapacity(1);
        
        //Act Statement(s)
        StrBuilder result = target.setLength(1);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).ensureCapacity(1);
        });
    }

    //BaseRock generated method id: ${setNewLineTextTest}, hash: 8CBA0320E82DC764AF01179A2425695A
    @Test()
    void setNewLineTextTest() {
        //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        StrBuilder result = target.setNewLineText("newLine1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${setNullTextWhenStringUtilsIsEmptyNullText}, hash: AB1BBD27721E837FC8877C913A08AD77
    @Test()
    void setNullTextWhenStringUtilsIsEmptyNullText() {
        /* Branches:
         * (StringUtils.isEmpty(nullText)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        
        //Act Statement(s)
        StrBuilder result = target.setNullText("nullText1");
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            assertThat(target.getNullText(), is(nullValue()));
        });
    }

    //BaseRock generated method id: ${sizeTest}, hash: E623EDAD6A4442CC2477EE6D8AF7627C
    @Test()
    void sizeTest() {
        //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        int result = target.size();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${startsWithWhenStrIsNull}, hash: 042BB34EAACB59D2054BA9EC44F9E6AC
    @Test()
    void startsWithWhenStrIsNull() {
        /* Branches:
         * (str == null) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        boolean result = target.startsWith((String) null);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${startsWithWhenLenEquals0}, hash: 5D0EE335B39421D42BAF4665A9345513
    @Test()
    void startsWithWhenLenEquals0() {
        /* Branches:
         * (str == null) : false
         * (len == 0) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        
        //Act Statement(s)
        boolean result = target.startsWith("");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${startsWithWhenLenGreaterThanSize}, hash: AC0268B6DFD51FDE8472E5CF67128536
    @Test()
    void startsWithWhenLenGreaterThanSize() {
        /* Branches:
         * (str == null) : false
         * (len == 0) : false
         * (len > size) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        
        //Act Statement(s)
        boolean result = target.startsWith("A");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${subSequenceWhenStartIndexLessThan0ThrowsStringIndexOutOfBoundsException}, hash: 0CC8944582F1A2CFAA8FCA33F5291A00
    @Test()
    void subSequenceWhenStartIndexLessThan0ThrowsStringIndexOutOfBoundsException() {
        /* Branches:
         * (startIndex < 0) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        //Act Statement(s)
        final StringIndexOutOfBoundsException result = assertThrows(StringIndexOutOfBoundsException.class, () -> {
            target.subSequence(-1, 0);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${subSequenceWhenEndIndexGreaterThanSizeThrowsStringIndexOutOfBoundsException}, hash: 0A64932B4F79F002E87580EA76EA4112
    @Test()
    void subSequenceWhenEndIndexGreaterThanSizeThrowsStringIndexOutOfBoundsException() {
        /* Branches:
         * (startIndex < 0) : false
         * (endIndex > size) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        //Act Statement(s)
        final StringIndexOutOfBoundsException result = assertThrows(StringIndexOutOfBoundsException.class, () -> {
            target.subSequence(1, 1);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${subSequenceWhenStartIndexGreaterThanEndIndexThrowsStringIndexOutOfBoundsException}, hash: EFFFB83880CEEF94547FD95B07998F3C
    @Test()
    void subSequenceWhenStartIndexGreaterThanEndIndexThrowsStringIndexOutOfBoundsException() {
        /* Branches:
         * (startIndex < 0) : false
         * (endIndex > size) : false
         * (startIndex > endIndex) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        //Act Statement(s)
        final StringIndexOutOfBoundsException result = assertThrows(StringIndexOutOfBoundsException.class, () -> {
            target.subSequence(1, 0);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${subSequenceWhenStartIndexNotGreaterThanEndIndex}, hash: 08C714EE08F6D1D14956EADBCE21DB76
    @Test()
    void subSequenceWhenStartIndexNotGreaterThanEndIndex() {
        /* Branches:
         * (startIndex < 0) : false
         * (endIndex > size) : false
         * (startIndex > endIndex) : false
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        doReturn("return_of_substring1").when(target).substring(0, 0);
        
        //Act Statement(s)
        CharSequence result = target.subSequence(0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("return_of_substring1"));
            verify(target).substring(0, 0);
        });
    }

    //BaseRock generated method id: ${substringTest}, hash: 8EA915FBA269261116F4C8C0AE3103EF
    @Test()
    void substringTest() {
        //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        doReturn("return_of_substring1").when(target).substring(0, 0);
        
        //Act Statement(s)
        String result = target.substring(0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("return_of_substring1"));
            verify(target).substring(0, 0);
        });
    }

    //BaseRock generated method id: ${substring1Test}, hash: FD2C536537B6A171611000A7EC13DD4A
    @Test()
    void substring1Test() {
        //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        doReturn(-1).when(target).validateRange(-1, 0);
        
        //Act Statement(s)
        String result = target.substring(-1, 0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("result1"));
            verify(target).validateRange(-1, 0);
        });
    }

    //BaseRock generated method id: ${toCharArrayWhenSizeEquals0}, hash: C74DC6D6BF86544C12E63DAFAE8DE5BD
    @Test()
    void toCharArrayWhenSizeEquals0() {
        /* Branches:
         * (size == 0) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        char[] result = target.toCharArray();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toCharArray1WhenLenEquals0}, hash: 2B0720FFCCEE62694FA8BA4227086359
    @Test()
    void toCharArray1WhenLenEquals0() {
        /* Branches:
         * (len == 0) : true
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(-1));
        doReturn(-1).when(target).validateRange(-1, 0);
        
        //Act Statement(s)
        char[] result = target.toCharArray(-1, 0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result.length, equalTo(0));
            verify(target).validateRange(-1, 0);
        });
    }

    //BaseRock generated method id: ${toCharArray1WhenLenNotEquals0}, hash: 3CBE349ECFC7E63C1624BF5876902C34
    @Test()
    void toCharArray1WhenLenNotEquals0() {
        /* Branches:
         * (len == 0) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = spy(new StrBuilder(0));
        doReturn(1).when(target).validateRange(0, 0);
        
        //Act Statement(s)
        char[] result = target.toCharArray(0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result.length, equalTo(0));
            verify(target).validateRange(0, 0);
        });
    }

    //BaseRock generated method id: ${toStringTest}, hash: 17BEE966962B814835B22B44A827E0F7
    @Test()
    void toStringTest() {
        //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        String result = target.toString();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${toStringBufferTest}, hash: B7C91E021311EB24F6B421A4BD912D35
    @Test()
    void toStringBufferTest() {
        //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        StringBuffer result = target.toStringBuffer();
        
        //Assert statement(s)
        //TODO: Please implement equals method in StringBuffer for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${toStringBuilderTest}, hash: A3413EA0BC78B0666D12BF828E075488
    @Test()
    void toStringBuilderTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        StringBuilder result = target.toStringBuilder();
        
        //Assert statement(s)
        //TODO: Please implement equals method in StringBuilder for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${trimWhenSizeEquals0}, hash: 5D5578E86BE031C158CA63C48C39ED3A
    @Test()
    void trimWhenSizeEquals0() {
        /* Branches:
         * (size == 0) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        
        //Act Statement(s)
        StrBuilder result = target.trim();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${validateIndexWhenIndexGreaterThanSizeThrowsStringIndexOutOfBoundsException}, hash: 010EB1A861EA303788133812337B90D7
    @Test()
    void validateIndexWhenIndexGreaterThanSizeThrowsStringIndexOutOfBoundsException() {
        /* Branches:
         * (index < 0) : false
         * (index > size) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        //Act Statement(s)
        final StringIndexOutOfBoundsException result = assertThrows(StringIndexOutOfBoundsException.class, () -> {
            target.validateIndex(1);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${validateIndexWhenIndexNotGreaterThanSize}, hash: C9CE88829A42119813EA83EE427C9B68
    @Test()
    void validateIndexWhenIndexNotGreaterThanSize() {
        /* Branches:
         * (index < 0) : false
         * (index > size) : false
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        
        //Act Statement(s)
        target.validateIndex(0);
    }

    //BaseRock generated method id: ${validateRangeWhenStartIndexLessThan0ThrowsStringIndexOutOfBoundsException}, hash: C7E0E2A0E567ED582DDF3215A75DC40C
    @Test()
    void validateRangeWhenStartIndexLessThan0ThrowsStringIndexOutOfBoundsException() {
        /* Branches:
         * (startIndex < 0) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(-1);
        //Act Statement(s)
        final StringIndexOutOfBoundsException result = assertThrows(StringIndexOutOfBoundsException.class, () -> {
            target.validateRange(-1, 0);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${validateRangeWhenStartIndexGreaterThanEndIndexThrowsStringIndexOutOfBoundsException}, hash: 43707E1E4555585766BFAFF184A8195C
    @Test()
    void validateRangeWhenStartIndexGreaterThanEndIndexThrowsStringIndexOutOfBoundsException() {
        /* Branches:
         * (startIndex < 0) : false
         * (endIndex > size) : true
         * (startIndex > endIndex) : true
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        StringIndexOutOfBoundsException stringIndexOutOfBoundsException = new StringIndexOutOfBoundsException("end < start");
        //Act Statement(s)
        final StringIndexOutOfBoundsException result = assertThrows(StringIndexOutOfBoundsException.class, () -> {
            target.validateRange(1, 1);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(stringIndexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${validateRangeWhenStartIndexNotGreaterThanEndIndex}, hash: 6CB1BF606346C380D7A2BC5BD8C2CA90
    @Test()
    void validateRangeWhenStartIndexNotGreaterThanEndIndex() {
        /* Branches:
         * (startIndex < 0) : false
         * (endIndex > size) : true
         * (startIndex > endIndex) : false
         */
         //Arrange Statement(s)
        StrBuilder target = new StrBuilder(0);
        
        //Act Statement(s)
        int result = target.validateRange(0, 1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }
}