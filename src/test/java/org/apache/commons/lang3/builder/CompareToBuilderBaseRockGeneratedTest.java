package org.apache.commons.lang3.builder;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.util.Collection;
import org.mockito.MockedStatic;
import java.util.ArrayList;
import java.util.Comparator;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.verify;
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
import static org.mockito.Mockito.mockStatic;
import org.junit.jupiter.api.Disabled;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class CompareToBuilderBaseRockGeneratedTest {

    //BaseRock generated method id: ${reflectionCompareTest}, hash: 718F6F2825467622D2B750B3C0F307C6
    @Test()
    void reflectionCompareTest() {
        //Arrange Statement(s)
        try (MockedStatic<CompareToBuilder> compareToBuilder = mockStatic(CompareToBuilder.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            String[] stringArray = new String[] {};
            compareToBuilder.when(() -> CompareToBuilder.reflectionCompare(object, object2, false, (Class) null, stringArray)).thenReturn(0);
            //Act Statement(s)
            int result = CompareToBuilder.reflectionCompare(object, object2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                compareToBuilder.verify(() -> CompareToBuilder.reflectionCompare(object, object2, false, (Class) null, stringArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${reflectionCompare1Test}, hash: C37F62E56DD03F9F774986832ED63F97
    @Test()
    void reflectionCompare1Test() {
        //Arrange Statement(s)
        try (MockedStatic<CompareToBuilder> compareToBuilder = mockStatic(CompareToBuilder.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            String[] stringArray = new String[] {};
            compareToBuilder.when(() -> CompareToBuilder.reflectionCompare(object, object2, false, (Class) null, stringArray)).thenReturn(0);
            //Act Statement(s)
            int result = CompareToBuilder.reflectionCompare(object, object2, false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                compareToBuilder.verify(() -> CompareToBuilder.reflectionCompare(object, object2, false, (Class) null, stringArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${reflectionCompare2WhenLhsEqualsRhs}, hash: 5A663BB773009980A18FE4E0C7F7438A
    @Test()
    void reflectionCompare2WhenLhsEqualsRhs() {
        /* Branches:
         * (lhs == rhs) : true
         */
        //Arrange Statement(s)
        Object object = new Object();
        String[] stringArray = new String[] {};
        //Act Statement(s)
        int result = CompareToBuilder.reflectionCompare(object, object, false, Object.class, stringArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${reflectionCompare2WhenLhsClazzNotIsInstanceRhsThrowsClassCastException}, hash: FF87B976F1AAC474F9CE54CC3BB8E25F
    @Test()
    void reflectionCompare2WhenLhsClazzNotIsInstanceRhsThrowsClassCastException() {
        /* Branches:
         * (lhs == rhs) : false
         * (!lhsClazz.isInstance(rhs)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        String[] stringArray = new String[] {};
        //Act Statement(s)
        final ClassCastException result = assertThrows(ClassCastException.class, () -> {
            CompareToBuilder.reflectionCompare(object, object2, false, Object.class, stringArray);
        });
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${reflectionCompare2WhenLhsClazzNotEqualsReflectUpToClass}, hash: DF8B7EDD5931E80DDEB97DD5313CA343
    @Test()
    void reflectionCompare2WhenLhsClazzNotEqualsReflectUpToClass() {
        /* Branches:
         * (lhs == rhs) : false
         * (!lhsClazz.isInstance(rhs)) : false
         * (i < fields.length) : true  #  inside reflectionAppend method
         * (builder.comparison == 0) : true  #  inside reflectionAppend method
         * (!ArrayUtils.contains(excludeFields, field.getName())) : true  #  inside reflectionAppend method
         * (!field.getName().contains("$")) : true  #  inside reflectionAppend method
         * (useTransients) : false  #  inside reflectionAppend method
         * (!Modifier.isStatic(field.getModifiers())) : true  #  inside reflectionAppend method
         * (lhsClazz.getSuperclass() != null) : true
         * (lhsClazz != reflectUpToClass) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        String[] stringArray = new String[] {};
        //Act Statement(s)
        int result = CompareToBuilder.reflectionCompare(object, object2, false, Object.class, stringArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${reflectionCompare3Test}, hash: 9981A37C95FDBA69838D5FF58933A82E
    @Test()
    void reflectionCompare3Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<CompareToBuilder> compareToBuilder = mockStatic(CompareToBuilder.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            String[] stringArray = new String[] {};
            compareToBuilder.when(() -> CompareToBuilder.reflectionCompare(object, object2, stringArray)).thenReturn(0);
            Collection<String> collection = new ArrayList<>();
            //Act Statement(s)
            int result = CompareToBuilder.reflectionCompare(object, object2, collection);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                compareToBuilder.verify(() -> CompareToBuilder.reflectionCompare(object, object2, stringArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${reflectionCompare4Test}, hash: 0CB56E69E1C3CF70D65627FA047AE4B1
    @Test()
    void reflectionCompare4Test() {
        //Arrange Statement(s)
        try (MockedStatic<CompareToBuilder> compareToBuilder = mockStatic(CompareToBuilder.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            String[] stringArray = new String[] {};
            compareToBuilder.when(() -> CompareToBuilder.reflectionCompare(object, object2, false, (Class) null, stringArray)).thenReturn(0);
            //Act Statement(s)
            int result = CompareToBuilder.reflectionCompare(object, object2, stringArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                compareToBuilder.verify(() -> CompareToBuilder.reflectionCompare(object, object2, false, (Class) null, stringArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${appendWhenLhsEqualsRhs}, hash: 3D4A00F8CA564E1B4AF78600F3A1A6F2
    @Test()
    void appendWhenLhsEqualsRhs() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        //Act Statement(s)
        CompareToBuilder result = target.append(false, false);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${appendWhenLhs}, hash: 5AD3BAC6CC898531E3ADCAAC7FB48360
    @Test()
    void appendWhenLhs() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        //Act Statement(s)
        CompareToBuilder result = target.append(true, false);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${appendWhenNotLhs}, hash: 29145AE631EF37CEFED386164A03ECF5
    @Test()
    void appendWhenNotLhs() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs) : false
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        //Act Statement(s)
        CompareToBuilder result = target.append(false, true);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append1WhenLhsEqualsRhs}, hash: 5EA0642DB9714F5D611C5A0BC045779A
    @Test()
    void append1WhenLhsEqualsRhs() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        boolean[] booleanArray = new boolean[] {};
        //Act Statement(s)
        CompareToBuilder result = target.append(booleanArray, booleanArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append1WhenLhsIsNull}, hash: E07FE7063863A42F69B8B98B6580B0D5
    @Test()
    void append1WhenLhsIsNull() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        boolean[] _boolean = null;
        boolean[] booleanArray = new boolean[] {};
        //Act Statement(s)
        CompareToBuilder result = target.append(_boolean, booleanArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append1WhenRhsIsNull}, hash: 5124519B24DE9C5DF8E16EB857A9566B
    @Test()
    void append1WhenRhsIsNull() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        boolean[] booleanArray = new boolean[] {};
        boolean[] _boolean = null;
        //Act Statement(s)
        CompareToBuilder result = target.append(booleanArray, _boolean);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append1WhenLhsLengthLessThanRhsLength}, hash: CEF85234A7E366551C611BBCFC0C80A2
    @Test()
    void append1WhenLhsLengthLessThanRhsLength() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : true
         * (lhs.length < rhs.length) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        boolean[] booleanArray = new boolean[] {};
        boolean[] booleanArray2 = new boolean[] { false };
        //Act Statement(s)
        CompareToBuilder result = target.append(booleanArray, booleanArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append1WhenLhsLengthNotLessThanRhsLength}, hash: 064041FDE33CC6F86E2E9312930AA37B
    @Test()
    void append1WhenLhsLengthNotLessThanRhsLength() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : true
         * (lhs.length < rhs.length) : false
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        boolean[] booleanArray = new boolean[] { false };
        boolean[] booleanArray2 = new boolean[] {};
        //Act Statement(s)
        CompareToBuilder result = target.append(booleanArray, booleanArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append1WhenComparisonEquals0}, hash: DF7E2A55CBA82E1D96A49A053C9CDAE7
    @Disabled()
    @Test()
    void append1WhenComparisonEquals0() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : false
         * (i < lhs.length) : true
         * (comparison == 0) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = spy(new CompareToBuilder());
        CompareToBuilder compareToBuilder = new CompareToBuilder();
        doReturn(compareToBuilder).when(target).append(false, false);
        boolean[] booleanArray = new boolean[] { false };
        boolean[] booleanArray2 = new boolean[] { false };
        //Act Statement(s)
        CompareToBuilder result = target.append(booleanArray, booleanArray2);
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append(false, false);
        });
    }

    //BaseRock generated method id: ${append2WhenComparisonEquals0}, hash: FBA57B16848F1F16C0EE735CCC5AADB1
    @Test()
    void append2WhenComparisonEquals0() {
        /* Branches:
         * (comparison != 0) : false
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        //Act Statement(s)
        CompareToBuilder result = target.append((byte) 1, (byte) 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append3WhenLhsEqualsRhs}, hash: 2D898D4E19DE1588B601355F8621AE86
    @Test()
    void append3WhenLhsEqualsRhs() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        byte[] byteArray = new byte[] {};
        //Act Statement(s)
        CompareToBuilder result = target.append(byteArray, byteArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append3WhenLhsIsNull}, hash: 859AFE43D2405A47E53A572D2546C788
    @Test()
    void append3WhenLhsIsNull() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        byte[] _byte = null;
        byte[] byteArray = new byte[] {};
        //Act Statement(s)
        CompareToBuilder result = target.append(_byte, byteArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append3WhenRhsIsNull}, hash: 34B94EF7D1C74F99040D0652361B79CA
    @Test()
    void append3WhenRhsIsNull() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        byte[] byteArray = new byte[] {};
        byte[] _byte = null;
        //Act Statement(s)
        CompareToBuilder result = target.append(byteArray, _byte);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append3WhenLhsLengthLessThanRhsLength}, hash: 473EEA7095D63184659EF487D8CF9BCC
    @Test()
    void append3WhenLhsLengthLessThanRhsLength() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : true
         * (lhs.length < rhs.length) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        byte[] byteArray = new byte[] {};
        byte[] byteArray2 = new byte[] { (byte) 0 };
        //Act Statement(s)
        CompareToBuilder result = target.append(byteArray, byteArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append3WhenLhsLengthNotLessThanRhsLength}, hash: 1A7F9B5360EF81880893928677BD3AF6
    @Test()
    void append3WhenLhsLengthNotLessThanRhsLength() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : true
         * (lhs.length < rhs.length) : false
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        byte[] byteArray = new byte[] { (byte) 0 };
        byte[] byteArray2 = new byte[] {};
        //Act Statement(s)
        CompareToBuilder result = target.append(byteArray, byteArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append3WhenComparisonEquals0}, hash: 5A5E4356F9BD0FEC9ACBA2AD76C5C562
    @Test()
    void append3WhenComparisonEquals0() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : false
         * (i < lhs.length) : true
         * (comparison == 0) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = spy(new CompareToBuilder());
        CompareToBuilder compareToBuilder = new CompareToBuilder();
        doReturn(compareToBuilder).when(target).append((byte) 0, (byte) 0);
        byte[] byteArray = new byte[] { (byte) 0 };
        byte[] byteArray2 = new byte[] { (byte) 0 };
        //Act Statement(s)
        CompareToBuilder result = target.append(byteArray, byteArray2);
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append((byte) 0, (byte) 0);
        });
    }

    //BaseRock generated method id: ${append4WhenComparisonEquals0}, hash: 7E58A6AB2E03B4BE2C510651A7EFAD82
    @Test()
    void append4WhenComparisonEquals0() {
        /* Branches:
         * (comparison != 0) : false
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        //Act Statement(s)
        CompareToBuilder result = target.append('A', 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append5WhenLhsEqualsRhs}, hash: 2CD99B79D1F0D3128AFE7DABE0857834
    @Test()
    void append5WhenLhsEqualsRhs() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        char[] charArray = new char[] {};
        //Act Statement(s)
        CompareToBuilder result = target.append(charArray, charArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append5WhenLhsIsNull}, hash: 19B5184E30F1B6282FB6E408392EB839
    @Test()
    void append5WhenLhsIsNull() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        char[] _char = null;
        char[] charArray = new char[] {};
        //Act Statement(s)
        CompareToBuilder result = target.append(_char, charArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append5WhenRhsIsNull}, hash: 1344C4C65A01DBBB779325ED5671C971
    @Test()
    void append5WhenRhsIsNull() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        char[] charArray = new char[] {};
        char[] _char = null;
        //Act Statement(s)
        CompareToBuilder result = target.append(charArray, _char);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append5WhenLhsLengthLessThanRhsLength}, hash: 1934E58FF88F0B903548EB94A71D19B0
    @Test()
    void append5WhenLhsLengthLessThanRhsLength() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : true
         * (lhs.length < rhs.length) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        char[] charArray = new char[] {};
        char[] charArray2 = new char[] { 'A' };
        //Act Statement(s)
        CompareToBuilder result = target.append(charArray, charArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append5WhenLhsLengthNotLessThanRhsLength}, hash: DBE88CB1F7CDB9389B24C49C68D9F838
    @Test()
    void append5WhenLhsLengthNotLessThanRhsLength() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : true
         * (lhs.length < rhs.length) : false
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        char[] charArray = new char[] { 'A' };
        char[] charArray2 = new char[] {};
        //Act Statement(s)
        CompareToBuilder result = target.append(charArray, charArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append5WhenComparisonEquals0}, hash: 00E5B8D85DFF84A8AD85760CDCD829F3
    @Test()
    void append5WhenComparisonEquals0() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : false
         * (i < lhs.length) : true
         * (comparison == 0) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = spy(new CompareToBuilder());
        CompareToBuilder compareToBuilder = new CompareToBuilder();
        doReturn(compareToBuilder).when(target).append('A', 'A');
        char[] charArray = new char[] { 'A' };
        char[] charArray2 = new char[] { 'A' };
        //Act Statement(s)
        CompareToBuilder result = target.append(charArray, charArray2);
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append('A', 'A');
        });
    }

    //BaseRock generated method id: ${append6WhenComparisonEquals0}, hash: B925956D6D195DE76E339EC8996B2CDD
    @Test()
    void append6WhenComparisonEquals0() {
        /* Branches:
         * (comparison != 0) : false
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        //Act Statement(s)
        CompareToBuilder result = target.append(Double.parseDouble("1.0"), Double.parseDouble("1.0"));
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append7WhenLhsEqualsRhs}, hash: 48F842C94B7A87B4C8590DB519B0DB57
    @Test()
    void append7WhenLhsEqualsRhs() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        double[] doubleArray = new double[] {};
        //Act Statement(s)
        CompareToBuilder result = target.append(doubleArray, doubleArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append7WhenLhsIsNull}, hash: 0610617D7B4066206607C51625B441AB
    @Test()
    void append7WhenLhsIsNull() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        double[] _double = null;
        double[] doubleArray = new double[] {};
        //Act Statement(s)
        CompareToBuilder result = target.append(_double, doubleArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append7WhenRhsIsNull}, hash: 60E3CB227DB4740B98E9DA31C88D8353
    @Test()
    void append7WhenRhsIsNull() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        double[] doubleArray = new double[] {};
        double[] _double = null;
        //Act Statement(s)
        CompareToBuilder result = target.append(doubleArray, _double);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append7WhenLhsLengthLessThanRhsLength}, hash: F38A5553E3E8CFF9CA88753767A6C71A
    @Test()
    void append7WhenLhsLengthLessThanRhsLength() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : true
         * (lhs.length < rhs.length) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        double[] doubleArray = new double[] {};
        double[] doubleArray2 = new double[] { Double.parseDouble("0") };
        //Act Statement(s)
        CompareToBuilder result = target.append(doubleArray, doubleArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append7WhenLhsLengthNotLessThanRhsLength}, hash: 1C7D258455B88AEAC73A1F9C8BA11E86
    @Test()
    void append7WhenLhsLengthNotLessThanRhsLength() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : true
         * (lhs.length < rhs.length) : false
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        double[] doubleArray = new double[] { Double.parseDouble("0") };
        double[] doubleArray2 = new double[] {};
        //Act Statement(s)
        CompareToBuilder result = target.append(doubleArray, doubleArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append7WhenComparisonEquals0}, hash: 5B3A55767265E88976AF3DDA9CF93B44
    @Disabled()
    @Test()
    void append7WhenComparisonEquals0() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : false
         * (i < lhs.length) : true
         * (comparison == 0) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = spy(new CompareToBuilder());
        CompareToBuilder compareToBuilder = new CompareToBuilder();
        doReturn(compareToBuilder).when(target).append(Double.parseDouble("0"), Double.parseDouble("0"));
        double[] doubleArray = new double[] { Double.parseDouble("0") };
        double[] doubleArray2 = new double[] { Double.parseDouble("0") };
        //Act Statement(s)
        CompareToBuilder result = target.append(doubleArray, doubleArray2);
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append(Double.parseDouble("0"), Double.parseDouble("0"));
        });
    }

    //BaseRock generated method id: ${append8WhenComparisonEquals0}, hash: FD95A1C2AF3AECA83E1604515B711179
    @Test()
    void append8WhenComparisonEquals0() {
        /* Branches:
         * (comparison != 0) : false
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        //Act Statement(s)
        CompareToBuilder result = target.append(Float.parseFloat("1.0"), Float.parseFloat("1.0"));
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append9WhenLhsEqualsRhs}, hash: CAC1437A18E03F109DC8F2D45B4A96B2
    @Test()
    void append9WhenLhsEqualsRhs() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        float[] floatArray = new float[] {};
        //Act Statement(s)
        CompareToBuilder result = target.append(floatArray, floatArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append9WhenLhsIsNull}, hash: A3937BFE9A98412CBF0C7516D07A7C04
    @Test()
    void append9WhenLhsIsNull() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        float[] _float = null;
        float[] floatArray = new float[] {};
        //Act Statement(s)
        CompareToBuilder result = target.append(_float, floatArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append9WhenRhsIsNull}, hash: 8A1F53094B35918D4690DAD72CD3B292
    @Test()
    void append9WhenRhsIsNull() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        float[] floatArray = new float[] {};
        float[] _float = null;
        //Act Statement(s)
        CompareToBuilder result = target.append(floatArray, _float);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append9WhenLhsLengthLessThanRhsLength}, hash: 61A04A35C8F352D875CD330177404C6A
    @Disabled()
    @Test()
    void append9WhenLhsLengthLessThanRhsLength() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : true
         * (lhs.length < rhs.length) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        float[] floatArray = new float[] {};
        float[] floatArray2 = new float[] { Float.parseFloat("0") };
        //Act Statement(s)
        CompareToBuilder result = target.append(floatArray, floatArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append9WhenLhsLengthNotLessThanRhsLength}, hash: 67499AC9D776E00AADADAD14B800EDA7
    @Test()
    void append9WhenLhsLengthNotLessThanRhsLength() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : true
         * (lhs.length < rhs.length) : false
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        float[] floatArray = new float[] { Float.parseFloat("0") };
        float[] floatArray2 = new float[] {};
        //Act Statement(s)
        CompareToBuilder result = target.append(floatArray, floatArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append9WhenComparisonEquals0}, hash: A07F2929D7CE5669A4B11DA48E11F8D3
    @Test()
    void append9WhenComparisonEquals0() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : false
         * (i < lhs.length) : true
         * (comparison == 0) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = spy(new CompareToBuilder());
        CompareToBuilder compareToBuilder = new CompareToBuilder();
        doReturn(compareToBuilder).when(target).append(Float.parseFloat("0"), Float.parseFloat("0"));
        float[] floatArray = new float[] { Float.parseFloat("0") };
        float[] floatArray2 = new float[] { Float.parseFloat("0") };
        //Act Statement(s)
        CompareToBuilder result = target.append(floatArray, floatArray2);
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append(Float.parseFloat("0"), Float.parseFloat("0"));
        });
    }

    //BaseRock generated method id: ${append10WhenComparisonEquals0}, hash: F6878A2E88265A9BD45A4AC1D49EAF6A
    @Test()
    void append10WhenComparisonEquals0() {
        /* Branches:
         * (comparison != 0) : false
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        //Act Statement(s)
        CompareToBuilder result = target.append(1, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append11WhenLhsEqualsRhs}, hash: 2B0B7918103CBC74A180139605F716E7
    @Test()
    void append11WhenLhsEqualsRhs() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        int[] intArray = new int[] {};
        //Act Statement(s)
        CompareToBuilder result = target.append(intArray, intArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append11WhenLhsIsNull}, hash: 263A7D45D5CE3FDD644D5A3163F56305
    @Test()
    void append11WhenLhsIsNull() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        int[] _int = null;
        int[] intArray = new int[] {};
        //Act Statement(s)
        CompareToBuilder result = target.append(_int, intArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append11WhenRhsIsNull}, hash: C294BB26EA6F22B5A587F697863E6936
    @Test()
    void append11WhenRhsIsNull() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        int[] intArray = new int[] {};
        int[] _int = null;
        //Act Statement(s)
        CompareToBuilder result = target.append(intArray, _int);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append11WhenLhsLengthLessThanRhsLength}, hash: 1240ED74CDECA6CFC82211919A9D1BC0
    @Test()
    void append11WhenLhsLengthLessThanRhsLength() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : true
         * (lhs.length < rhs.length) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        int[] intArray = new int[] {};
        int[] intArray2 = new int[] { 0 };
        //Act Statement(s)
        CompareToBuilder result = target.append(intArray, intArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append11WhenLhsLengthNotLessThanRhsLength}, hash: 63EA0344C4066052B079538F76E4FC48
    @Test()
    void append11WhenLhsLengthNotLessThanRhsLength() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : true
         * (lhs.length < rhs.length) : false
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        int[] intArray = new int[] { 0 };
        int[] intArray2 = new int[] {};
        //Act Statement(s)
        CompareToBuilder result = target.append(intArray, intArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append11WhenComparisonEquals0}, hash: 0E74017EF85B8F56F1874E0766289EB2
    @Disabled()
    @Test()
    void append11WhenComparisonEquals0() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : false
         * (i < lhs.length) : true
         * (comparison == 0) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = spy(new CompareToBuilder());
        CompareToBuilder compareToBuilder = new CompareToBuilder();
        doReturn(compareToBuilder).when(target).append(0, 0);
        int[] intArray = new int[] { 0 };
        int[] intArray2 = new int[] { 0 };
        //Act Statement(s)
        CompareToBuilder result = target.append(intArray, intArray2);
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append(0, 0);
        });
    }

    //BaseRock generated method id: ${append12WhenComparisonEquals0}, hash: 80EF02D267A03BC9BF7399C7F0F66E55
    @Test()
    void append12WhenComparisonEquals0() {
        /* Branches:
         * (comparison != 0) : false
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        //Act Statement(s)
        CompareToBuilder result = target.append(1L, 1L);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append13WhenLhsEqualsRhs}, hash: 563EFCA8E12866EACD4C83111FE427B9
    @Test()
    void append13WhenLhsEqualsRhs() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        long[] longArray = new long[] {};
        //Act Statement(s)
        CompareToBuilder result = target.append(longArray, longArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append13WhenLhsIsNull}, hash: FB32D8B300794271F3A7C7E91D6E978A
    @Test()
    void append13WhenLhsIsNull() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        long[] _long = null;
        long[] longArray = new long[] {};
        //Act Statement(s)
        CompareToBuilder result = target.append(_long, longArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append13WhenRhsIsNull}, hash: 34B8CA34458DCFE1324383236A7FB61A
    @Test()
    void append13WhenRhsIsNull() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        long[] longArray = new long[] {};
        long[] _long = null;
        //Act Statement(s)
        CompareToBuilder result = target.append(longArray, _long);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append13WhenLhsLengthLessThanRhsLength}, hash: 7ECDE88F31DB1B27AFA41E597E932BCF
    @Test()
    void append13WhenLhsLengthLessThanRhsLength() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : true
         * (lhs.length < rhs.length) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        long[] longArray = new long[] {};
        long[] longArray2 = new long[] { 0L };
        //Act Statement(s)
        CompareToBuilder result = target.append(longArray, longArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append13WhenLhsLengthNotLessThanRhsLength}, hash: B9EC5BE99AFA7BD3D19FEB5098CB651B
    @Test()
    void append13WhenLhsLengthNotLessThanRhsLength() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : true
         * (lhs.length < rhs.length) : false
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        long[] longArray = new long[] { 0L };
        long[] longArray2 = new long[] {};
        //Act Statement(s)
        CompareToBuilder result = target.append(longArray, longArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append13WhenComparisonEquals0}, hash: 7C0DEDC7604B522CCA00A071E7BD6A41
    @Test()
    void append13WhenComparisonEquals0() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : false
         * (i < lhs.length) : true
         * (comparison == 0) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = spy(new CompareToBuilder());
        CompareToBuilder compareToBuilder = new CompareToBuilder();
        doReturn(compareToBuilder).when(target).append(0L, 0L);
        long[] longArray = new long[] { 0L };
        long[] longArray2 = new long[] { 0L };
        //Act Statement(s)
        CompareToBuilder result = target.append(longArray, longArray2);
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append(0L, 0L);
        });
    }

    //BaseRock generated method id: ${append14Test}, hash: A798A8E428B43A2B6F592CDA9622F8BE
    @Test()
    void append14Test() {
        //Arrange Statement(s)
        CompareToBuilder target = spy(new CompareToBuilder());
        CompareToBuilder compareToBuilder = new CompareToBuilder();
        Object object = new Object();
        Object object2 = new Object();
        doReturn(compareToBuilder).when(target).append(object, object2, (Comparator) null);
        //Act Statement(s)
        CompareToBuilder result = target.append(object, object2);
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(compareToBuilder));
            verify(target).append(object, object2, (Comparator) null);
        });
    }

    //BaseRock generated method id: ${append15WhenLhsEqualsRhs}, hash: EDC3A6BE98AB460EB0CF238C59266B45
    @Test()
    void append15WhenLhsEqualsRhs() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        Object object = new Object();
        Comparator comparator = Comparator.reverseOrder();
        //Act Statement(s)
        CompareToBuilder result = target.append(object, object, comparator);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append15WhenLhsIsNull}, hash: 2DC862377F36544659B81DC5A0776B8A
    @Test()
    void append15WhenLhsIsNull() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        Object object = null;
        Object object2 = new Object();
        Comparator comparator = Comparator.reverseOrder();
        //Act Statement(s)
        CompareToBuilder result = target.append(object, object2, comparator);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append15WhenRhsIsNull}, hash: DFBB70BAF9E6BFD2E13F8F22A45CADDA
    @Test()
    void append15WhenRhsIsNull() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        Object object = new Object();
        Object object2 = null;
        Comparator comparator = Comparator.reverseOrder();
        //Act Statement(s)
        CompareToBuilder result = target.append(object, object2, comparator);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append15WhenComparatorIsNull}, hash: 5B53E7ADF992080959F091B3151D8D35
    @Disabled()
    @Test()
    void append15WhenComparatorIsNull() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (ObjectUtils.isArray(lhs)) : false
         * (comparator == null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        Object object = new Object();
        Object object2 = new Object();
        Comparator<?> comparator = null;
        //Act Statement(s)
        CompareToBuilder result = target.append((Object) object, object2, comparator);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append15WhenComparatorIsNotNull}, hash: 8C195838F23DE88372AD9A1171D105EF
    @Test()
    void append15WhenComparatorIsNotNull() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (ObjectUtils.isArray(lhs)) : false
         * (comparator == null) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        Object object = new Object();
        Object object2 = new Object();
        Comparator comparator = Comparator.reverseOrder();
        //Act Statement(s)
        CompareToBuilder result = target.append(object, object2, comparator);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append15WhenLhsNotInstanceOfBooleanArray}, hash: D00F3F60BB0603FF886CDC25E91A7FC3
    @Test()
    void append15WhenLhsNotInstanceOfBooleanArray() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (ObjectUtils.isArray(lhs)) : true
         * (lhs instanceof long[]) : false  #  inside appendArray method
         * (lhs instanceof int[]) : false  #  inside appendArray method
         * (lhs instanceof short[]) : false  #  inside appendArray method
         * (lhs instanceof char[]) : false  #  inside appendArray method
         * (lhs instanceof byte[]) : false  #  inside appendArray method
         * (lhs instanceof double[]) : false  #  inside appendArray method
         * (lhs instanceof float[]) : false  #  inside appendArray method
         * (lhs instanceof boolean[]) : false  #  inside appendArray method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        Object[] objectArray = new Object[] {};
        Comparator comparator = Comparator.reverseOrder();
        //Act Statement(s)
        CompareToBuilder result = target.append((Object) null, (Object) objectArray, comparator);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append16Test}, hash: 3653B607D1DDD37BF6ACADA576B07187
    @Test()
    void append16Test() {
        //Arrange Statement(s)
        CompareToBuilder target = spy(new CompareToBuilder());
        CompareToBuilder compareToBuilder = new CompareToBuilder();
        Object[] objectArray = new Object[] {};
        Object[] objectArray2 = new Object[] {};
        doReturn(compareToBuilder).when(target).append(objectArray, objectArray2, (Comparator) null);
        //Act Statement(s)
        CompareToBuilder result = target.append(objectArray, objectArray2);
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(compareToBuilder));
            verify(target).append(objectArray, objectArray2, (Comparator) null);
        });
    }

    //BaseRock generated method id: ${append17WhenLhsEqualsRhs}, hash: 976990FC52505947CAAE0E8799343284
    @Test()
    void append17WhenLhsEqualsRhs() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        Object[] objectArray = new Object[] {};
        Comparator comparator = Comparator.reverseOrder();
        //Act Statement(s)
        CompareToBuilder result = target.append(objectArray, objectArray, comparator);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append17WhenLhsIsNull}, hash: 729CCEA838F6FB4589C926FBC9F050D7
    @Test()
    void append17WhenLhsIsNull() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        Object[] object = null;
        Object[] objectArray = new Object[] {};
        Comparator comparator = Comparator.reverseOrder();
        //Act Statement(s)
        CompareToBuilder result = target.append(object, objectArray, comparator);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append17WhenRhsIsNull}, hash: 540B58026DD8EA65082A6DC7702F5647
    @Disabled()
    @Test()
    void append17WhenRhsIsNull() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        Object[] objectArray = new Object[] {};
        Object[] object = null;
        Comparator comparator = Comparator.reverseOrder();
        //Act Statement(s)
        CompareToBuilder result = target.append(objectArray, object, comparator);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append17WhenLhsLengthLessThanRhsLength}, hash: 0C0A29F60E109C9D16B9B23B93821FC6
    @Test()
    void append17WhenLhsLengthLessThanRhsLength() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : true
         * (lhs.length < rhs.length) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        Object[] objectArray = new Object[] {};
        Object object = new Object();
        Object[] objectArray2 = new Object[] { object };
        Comparator comparator = Comparator.reverseOrder();
        //Act Statement(s)
        CompareToBuilder result = target.append(objectArray, objectArray2, comparator);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append17WhenLhsLengthNotLessThanRhsLength}, hash: FE79ECD901F6785CDA49CC498D0FD0F2
    @Test()
    void append17WhenLhsLengthNotLessThanRhsLength() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : true
         * (lhs.length < rhs.length) : false
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        Object object = new Object();
        Object[] objectArray = new Object[] { object };
        Object[] objectArray2 = new Object[] {};
        Comparator comparator = Comparator.reverseOrder();
        //Act Statement(s)
        CompareToBuilder result = target.append(objectArray, objectArray2, comparator);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append17WhenComparisonEquals0}, hash: D086C10BD5F2BACB94B82574007ADBB1
    @Test()
    void append17WhenComparisonEquals0() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : false
         * (i < lhs.length) : true
         * (comparison == 0) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = spy(new CompareToBuilder());
        CompareToBuilder compareToBuilder = new CompareToBuilder();
        Object object = new Object();
        Object object2 = new Object();
        doReturn(compareToBuilder).when(target).append(eq(object), eq(object2), (Comparator) any());
        Object[] objectArray = new Object[] { object };
        Object[] objectArray2 = new Object[] { object2 };
        Comparator comparator = Comparator.reverseOrder();
        //Act Statement(s)
        CompareToBuilder result = target.append(objectArray, objectArray2, comparator);
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append(eq(object), eq(object2), (Comparator) any());
        });
    }

    //BaseRock generated method id: ${append18WhenComparisonEquals0}, hash: 814EAC76C1EB728A137B2568D1C7C16D
    @Test()
    void append18WhenComparisonEquals0() {
        /* Branches:
         * (comparison != 0) : false
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        //Act Statement(s)
        CompareToBuilder result = target.append((short) 1, (short) 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append19WhenLhsEqualsRhs}, hash: 544DD8FDF9CCBAA774095976D3A7E75B
    @Test()
    void append19WhenLhsEqualsRhs() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        short[] shortArray = new short[] {};
        //Act Statement(s)
        CompareToBuilder result = target.append(shortArray, shortArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append19WhenLhsIsNull}, hash: 0313BB3E86BA654DE9DFB1FDFBBEA901
    @Test()
    void append19WhenLhsIsNull() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        short[] _short = null;
        short[] shortArray = new short[] {};
        //Act Statement(s)
        CompareToBuilder result = target.append(_short, shortArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append19WhenRhsIsNull}, hash: 0954327DF9E8ED60A9DCF9442CCDDE11
    @Test()
    void append19WhenRhsIsNull() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        short[] shortArray = new short[] {};
        short[] _short = null;
        //Act Statement(s)
        CompareToBuilder result = target.append(shortArray, _short);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append19WhenLhsLengthLessThanRhsLength}, hash: 7836ACB2244DF58A9677B829F80AC1A0
    @Test()
    void append19WhenLhsLengthLessThanRhsLength() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : true
         * (lhs.length < rhs.length) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        short[] shortArray = new short[] {};
        short[] shortArray2 = new short[] { (short) 0 };
        //Act Statement(s)
        CompareToBuilder result = target.append(shortArray, shortArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append19WhenLhsLengthNotLessThanRhsLength}, hash: 5B083EF91105AAA8E635B29E93BEDBBA
    @Test()
    void append19WhenLhsLengthNotLessThanRhsLength() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : true
         * (lhs.length < rhs.length) : false
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        short[] shortArray = new short[] { (short) 0 };
        short[] shortArray2 = new short[] {};
        //Act Statement(s)
        CompareToBuilder result = target.append(shortArray, shortArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append19WhenComparisonEquals0}, hash: 7C58342DA46EAEB5A01B3E9FC5FA6593
    @Test()
    void append19WhenComparisonEquals0() {
        /* Branches:
         * (comparison != 0) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : false
         * (i < lhs.length) : true
         * (comparison == 0) : true
         */
        //Arrange Statement(s)
        CompareToBuilder target = spy(new CompareToBuilder());
        CompareToBuilder compareToBuilder = new CompareToBuilder();
        doReturn(compareToBuilder).when(target).append((short) 0, (short) 0);
        short[] shortArray = new short[] { (short) 0 };
        short[] shortArray2 = new short[] { (short) 0 };
        //Act Statement(s)
        CompareToBuilder result = target.append(shortArray, shortArray2);
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append((short) 0, (short) 0);
        });
    }

    //BaseRock generated method id: ${appendSuperWhenComparisonEquals0}, hash: 91C1B89E5DD842F17BA4A0429B5C4B80
    @Test()
    void appendSuperWhenComparisonEquals0() {
        /* Branches:
         * (comparison != 0) : false
         */
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        //Act Statement(s)
        CompareToBuilder result = target.appendSuper(0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${buildTest}, hash: 2801B60E0A8E0FAB43063878A6500DBE
    @Test()
    void buildTest() {
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        //Act Statement(s)
        Integer result = target.build();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${toComparisonTest}, hash: 6CA3D175EB3A39136C9580C5E8C429D5
    @Test()
    void toComparisonTest() {
        //Arrange Statement(s)
        CompareToBuilder target = new CompareToBuilder();
        //Act Statement(s)
        int result = target.toComparison();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }
}
