package org.apache.commons.lang3;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.util.Arrays;
import org.mockito.stubbing.Answer;
import java.util.Map;
import java.util.HashMap;
import java.util.Collection;
import java.util.function.Supplier;
import org.mockito.MockedStatic;
import java.util.ArrayList;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.verify;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.doReturn;
import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.mockito.ArgumentMatchers.anyCollection;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mockStatic;
import org.junit.jupiter.api.Disabled;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class ValidateBaseRockGeneratedTest {

    private final Comparable<Object> comparableMock = mock(Comparable.class);

    private final Object objectMock = mock(Object.class, "start");

    private final Object objectMock2 = mock(Object.class, "end");

    private final Comparable<Object> valueMock = mock(Comparable.class);

    //BaseRock generated method id: ${exclusiveBetweenWhenValueGreaterThanOrEqualsToEndThrowsIllegalArgumentException}, hash: FB8DB42192C09B58FA9680BD45290EB5
    @Disabled()
    @Test()
    void exclusiveBetweenWhenValueGreaterThanOrEqualsToEndThrowsIllegalArgumentException() {
        /* Branches:
         * (value <= start) : false
         * (value >= end) : true
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The value null is not in the specified exclusive range of null to null");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.exclusiveBetween(Double.parseDouble("0.25"), Double.parseDouble("0.25"), Double.parseDouble("0.3125"));
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${exclusiveBetweenWhenValueLessThanEnd}, hash: B5D56629F7651F875818BECEB85A1E48
    @Test()
    void exclusiveBetweenWhenValueLessThanEnd() {
        /* Branches:
         * (value <= start) : false
         * (value >= end) : false
         */
        //Act Statement(s)
        Validate.exclusiveBetween(Double.parseDouble("-0.5"), Double.parseDouble("0.5"), Double.parseDouble("0.0"));
    }

    //BaseRock generated method id: ${exclusiveBetween1WhenValueGreaterThanOrEqualsToEndThrowsIllegalArgumentException}, hash: BA336F11DC253E9E77E7A0FF00B2DAB2
    @Test()
    void exclusiveBetween1WhenValueGreaterThanOrEqualsToEndThrowsIllegalArgumentException() {
        /* Branches:
         * (value <= start) : false
         * (value >= end) : true
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("message1");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.exclusiveBetween(Double.parseDouble("0.25"), Double.parseDouble("0.25"), Double.parseDouble("0.5"), "message1");
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${exclusiveBetween1WhenValueLessThanEnd}, hash: 5D1E3C6A82B6AD6A444B58C776F3FD0B
    @Test()
    void exclusiveBetween1WhenValueLessThanEnd() {
        /* Branches:
         * (value <= start) : false
         * (value >= end) : false
         */
        //Act Statement(s)
        Validate.exclusiveBetween(Double.parseDouble("-0.5"), Double.parseDouble("0.5"), Double.parseDouble("0.0"), "message1");
    }

    //BaseRock generated method id: ${exclusiveBetween2WhenValueGreaterThanOrEqualsToEndThrowsIllegalArgumentException}, hash: 602827FEC40F549E44F357517AE15E8B
    @Test()
    void exclusiveBetween2WhenValueGreaterThanOrEqualsToEndThrowsIllegalArgumentException() {
        /* Branches:
         * (value <= start) : false
         * (value >= end) : true
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The value 3 is not in the specified exclusive range of 2 to 2");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.exclusiveBetween(2L, 2L, 3L);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${exclusiveBetween2WhenValueLessThanEnd}, hash: 8E51779C45DADD7FA2F81C4D6E719A15
    @Test()
    void exclusiveBetween2WhenValueLessThanEnd() {
        /* Branches:
         * (value <= start) : false
         * (value >= end) : false
         */
        //Act Statement(s)
        Validate.exclusiveBetween(1L, 3L, 2L);
    }

    //BaseRock generated method id: ${exclusiveBetween3WhenValueGreaterThanOrEqualsToEndThrowsIllegalArgumentException}, hash: 6F673B2C4C4DC3899380F45E00E4F73B
    @Test()
    void exclusiveBetween3WhenValueGreaterThanOrEqualsToEndThrowsIllegalArgumentException() {
        /* Branches:
         * (value <= start) : false
         * (value >= end) : true
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("message1");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.exclusiveBetween(1L, 1L, 2L, "message1");
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${exclusiveBetween3WhenValueLessThanEnd}, hash: 8C9C3F33E8E31D33968ACF8E873CC305
    @Test()
    void exclusiveBetween3WhenValueLessThanEnd() {
        /* Branches:
         * (value <= start) : false
         * (value >= end) : false
         */
        //Act Statement(s)
        Validate.exclusiveBetween(1L, 3L, 2L, "message1");
    }

    //BaseRock generated method id: ${exclusiveBetween4WhenValueCompareToEndGreaterThanOrEqualsTo0ThrowsIllegalArgumentException}, hash: EBF4705045CBF7879C490C0CE1CCB316
    @Disabled()
    @Test()
    void exclusiveBetween4WhenValueCompareToEndGreaterThanOrEqualsTo0ThrowsIllegalArgumentException() {
        /* Branches:
         * (value.compareTo(start) <= 0) : false
         * (value.compareTo(end) >= 0) : true
         */
        //Arrange Statement(s)
        Comparable<Object> valueMock = mock(Comparable.class, "value");
        doReturn(1).when(valueMock).compareTo(objectMock);
        doReturn(1).when(valueMock).compareTo(objectMock2);
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The value value is not in the specified exclusive range of null to null");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.exclusiveBetween(objectMock, objectMock2, valueMock);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
            verify(valueMock).compareTo(objectMock);
            verify(valueMock).compareTo(objectMock2);
        });
    }

    //BaseRock generated method id: ${exclusiveBetween4WhenValueCompareToEndLessThan0}, hash: 3D12AD73853F95B5EE893CE0D2548F0D
    @Test()
    void exclusiveBetween4WhenValueCompareToEndLessThan0() {
        /* Branches:
         * (value.compareTo(start) <= 0) : false
         * (value.compareTo(end) >= 0) : false
         */
        //Arrange Statement(s)
        Object object = new Object();
        doReturn(1).when(valueMock).compareTo(object);
        Object object2 = new Object();
        doReturn(-1).when(valueMock).compareTo(object2);
        //Act Statement(s)
        Validate.exclusiveBetween(object, object2, valueMock);
        //Assert statement(s)
        assertAll("result", () -> {
            verify(valueMock).compareTo(object);
            verify(valueMock).compareTo(object2);
        });
    }

    //BaseRock generated method id: ${exclusiveBetween5WhenValueCompareToEndLessThan0}, hash: C20F7ED08CFE84A0EA574C590511ACA0
    @Test()
    void exclusiveBetween5WhenValueCompareToEndLessThan0() {
        /* Branches:
         * (value.compareTo(start) <= 0) : false
         * (value.compareTo(end) >= 0) : false
         */
        //Arrange Statement(s)
        Object object = new Object();
        doReturn(1).when(valueMock).compareTo(object);
        Object object2 = new Object();
        doReturn(-1).when(valueMock).compareTo(object2);
        Object[] objectArray = new Object[] {};
        //Act Statement(s)
        Validate.exclusiveBetween(object, object2, valueMock, "message1", objectArray);
        //Assert statement(s)
        assertAll("result", () -> {
            verify(valueMock).compareTo(object);
            verify(valueMock).compareTo(object2);
        });
    }

    //BaseRock generated method id: ${exclusiveBetween5WhenArrayUtilsIsEmptyValuesThrowsIllegalArgumentException}, hash: AEA16EA978B7829E1FE8914260E3E44C
    @Test()
    void exclusiveBetween5WhenArrayUtilsIsEmptyValuesThrowsIllegalArgumentException() {
        /* Branches:
         * (value.compareTo(start) <= 0) : false
         * (value.compareTo(end) >= 0) : true
         * (ArrayUtils.isEmpty(values)) : true  #  inside getMessage method
         */
        //Arrange Statement(s)
        Object object = new Object();
        doReturn(1).when(valueMock).compareTo(object);
        Object object2 = new Object();
        doReturn(1).when(valueMock).compareTo(object2);
        Object[] objectArray = new Object[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("message1");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.exclusiveBetween(object, object2, valueMock, "message1", objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
            verify(valueMock).compareTo(object);
            verify(valueMock).compareTo(object2);
        });
    }

    //BaseRock generated method id: ${exclusiveBetween5WhenArrayUtilsNotIsEmptyValuesThrowsIllegalArgumentException}, hash: FAD1DF8BF2F14AE32CE89B1F5DE0E1BD
    @Test()
    void exclusiveBetween5WhenArrayUtilsNotIsEmptyValuesThrowsIllegalArgumentException() {
        /* Branches:
         * (value.compareTo(start) <= 0) : false
         * (value.compareTo(end) >= 0) : true
         * (ArrayUtils.isEmpty(values)) : false  #  inside getMessage method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        Object[] objectArray = new Object[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("A");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.exclusiveBetween(object, object2, comparableMock, "A", objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${finiteTest}, hash: F60F93EB37653262A5CEEDB38C6446EA
    @Test()
    void finiteTest() {
        //Arrange Statement(s)
        try (MockedStatic<Validate> validate = mockStatic(Validate.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] { Double.parseDouble("0.0") };
            validate.when(() -> Validate.finite(Double.parseDouble("0.0"), "The value is invalid: %f", objectArray)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            Validate.finite(Double.parseDouble("0.0"));
            //Assert statement(s)
            assertAll("result", () -> validate.verify(() -> Validate.finite(Double.parseDouble("0.0"), "The value is invalid: %f", objectArray), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${finite1WhenDoubleNotIsInfiniteValue}, hash: E1484CFA24C893B2E0B9F8AFFBDD20F1
    @Test()
    void finite1WhenDoubleNotIsInfiniteValue() {
        /* Branches:
         * (Double.isNaN(value)) : false
         * (Double.isInfinite(value)) : false
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        //Act Statement(s)
        Validate.finite(Double.parseDouble("0.0"), "message1", objectArray);
    }

    //BaseRock generated method id: ${finite1WhenArrayUtilsIsEmptyValuesThrowsIllegalArgumentException}, hash: 29E48120CCF9B603FD7B7093FEC3B96C
    @Disabled()
    @Test()
    void finite1WhenArrayUtilsIsEmptyValuesThrowsIllegalArgumentException() {
        /* Branches:
         * (Double.isNaN(value)) : false
         * (Double.isInfinite(value)) : true
         * (ArrayUtils.isEmpty(values)) : true  #  inside getMessage method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("message1");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.finite(Double.parseDouble("0.0"), "message1", objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${finite1WhenArrayUtilsNotIsEmptyValuesThrowsIllegalArgumentException}, hash: 5D08BB0204FFB593CFAD497DE181BD44
    @Disabled()
    @Test()
    void finite1WhenArrayUtilsNotIsEmptyValuesThrowsIllegalArgumentException() {
        /* Branches:
         * (Double.isNaN(value)) : false
         * (Double.isInfinite(value)) : true
         * (ArrayUtils.isEmpty(values)) : false  #  inside getMessage method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("s1");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.finite(Double.parseDouble("0.0"), "message1", objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${inclusiveBetweenWhenValueGreaterThanEndThrowsIllegalArgumentException}, hash: 2A0B71D948F2E90160BAACA38CCE149E
    @Disabled()
    @Test()
    void inclusiveBetweenWhenValueGreaterThanEndThrowsIllegalArgumentException() {
        /* Branches:
         * (value < start) : false
         * (value > end) : true
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The value null is not in the specified inclusive range of null to null");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.inclusiveBetween(Double.parseDouble("0.25"), Double.parseDouble("0.25"), Double.parseDouble("0.3125"));
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${inclusiveBetweenWhenValueNotGreaterThanEnd}, hash: 88F958C9CF7F0B70B0C95F8707FA08FE
    @Test()
    void inclusiveBetweenWhenValueNotGreaterThanEnd() {
        /* Branches:
         * (value < start) : false
         * (value > end) : false
         */
        //Act Statement(s)
        Validate.inclusiveBetween(Double.parseDouble("0.5"), Double.parseDouble("0.5"), Double.parseDouble("0.5"));
    }

    //BaseRock generated method id: ${inclusiveBetween1WhenValueGreaterThanEndThrowsIllegalArgumentException}, hash: 656C8428E2F300B2ED5724E54AE5A547
    @Test()
    void inclusiveBetween1WhenValueGreaterThanEndThrowsIllegalArgumentException() {
        /* Branches:
         * (value < start) : false
         * (value > end) : true
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("message1");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.inclusiveBetween(Double.parseDouble("0.25"), Double.parseDouble("0.25"), Double.parseDouble("0.5"), "message1");
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${inclusiveBetween1WhenValueNotGreaterThanEnd}, hash: EB9C98B69CBFC6AFE20038ACF69D34E4
    @Test()
    void inclusiveBetween1WhenValueNotGreaterThanEnd() {
        /* Branches:
         * (value < start) : false
         * (value > end) : false
         */
        //Act Statement(s)
        Validate.inclusiveBetween(Double.parseDouble("0.5"), Double.parseDouble("0.5"), Double.parseDouble("0.5"), "message1");
    }

    //BaseRock generated method id: ${inclusiveBetween2WhenValueGreaterThanEndThrowsIllegalArgumentException}, hash: 58AD6B309A75C4725D205B8A947A26E3
    @Test()
    void inclusiveBetween2WhenValueGreaterThanEndThrowsIllegalArgumentException() {
        /* Branches:
         * (value < start) : false
         * (value > end) : true
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The value 3 is not in the specified inclusive range of 2 to 2");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.inclusiveBetween(2L, 2L, 3L);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${inclusiveBetween2WhenValueNotGreaterThanEnd}, hash: FC117A0A42CEA8514F14DA2E26B5DF94
    @Test()
    void inclusiveBetween2WhenValueNotGreaterThanEnd() {
        /* Branches:
         * (value < start) : false
         * (value > end) : false
         */
        //Act Statement(s)
        Validate.inclusiveBetween(1L, 2L, 2L);
    }

    //BaseRock generated method id: ${inclusiveBetween3WhenValueGreaterThanEndThrowsIllegalArgumentException}, hash: 3200510ADFC990AADAB6130C835B3060
    @Test()
    void inclusiveBetween3WhenValueGreaterThanEndThrowsIllegalArgumentException() {
        /* Branches:
         * (value < start) : false
         * (value > end) : true
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("message1");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.inclusiveBetween(1L, 1L, 2L, "message1");
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${inclusiveBetween3WhenValueNotGreaterThanEnd}, hash: D9105D264CBBB1DDBBBB51D2F16E5DDA
    @Test()
    void inclusiveBetween3WhenValueNotGreaterThanEnd() {
        /* Branches:
         * (value < start) : false
         * (value > end) : false
         */
        //Act Statement(s)
        Validate.inclusiveBetween(1L, 2L, 2L, "message1");
    }

    //BaseRock generated method id: ${inclusiveBetween4WhenValueCompareToEndGreaterThan0ThrowsIllegalArgumentException}, hash: F0E9228E611F60628D368301D9A15366
    @Disabled()
    @Test()
    void inclusiveBetween4WhenValueCompareToEndGreaterThan0ThrowsIllegalArgumentException() {
        /* Branches:
         * (value.compareTo(start) < 0) : false
         * (value.compareTo(end) > 0) : true
         */
        //Arrange Statement(s)
        Comparable<Object> valueMock = mock(Comparable.class, "value");
        doReturn(1).when(valueMock).compareTo(objectMock);
        doReturn(1).when(valueMock).compareTo(objectMock2);
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The value value is not in the specified inclusive range of null to null");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.inclusiveBetween(objectMock, objectMock2, valueMock);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
            verify(valueMock).compareTo(objectMock);
            verify(valueMock).compareTo(objectMock2);
        });
    }

    //BaseRock generated method id: ${inclusiveBetween4WhenValueCompareToEndNotGreaterThan0}, hash: EA4FDB93974228B474CE7034F82A658E
    @Test()
    void inclusiveBetween4WhenValueCompareToEndNotGreaterThan0() {
        /* Branches:
         * (value.compareTo(start) < 0) : false
         * (value.compareTo(end) > 0) : false
         */
        //Arrange Statement(s)
        Object object = new Object();
        doReturn(1).when(valueMock).compareTo(object);
        Object object2 = new Object();
        doReturn(-1).when(valueMock).compareTo(object2);
        //Act Statement(s)
        Validate.inclusiveBetween(object, object2, valueMock);
        //Assert statement(s)
        assertAll("result", () -> {
            verify(valueMock).compareTo(object);
            verify(valueMock).compareTo(object2);
        });
    }

    //BaseRock generated method id: ${inclusiveBetween5WhenValueCompareToEndNotGreaterThan0}, hash: BA53330E36C7C84D8D69297E0E15CF4B
    @Test()
    void inclusiveBetween5WhenValueCompareToEndNotGreaterThan0() {
        /* Branches:
         * (value.compareTo(start) < 0) : false
         * (value.compareTo(end) > 0) : false
         */
        //Arrange Statement(s)
        Object object = new Object();
        doReturn(1).when(valueMock).compareTo(object);
        Object object2 = new Object();
        doReturn(-1).when(valueMock).compareTo(object2);
        Object[] objectArray = new Object[] {};
        //Act Statement(s)
        Validate.inclusiveBetween(object, object2, valueMock, "message1", objectArray);
        //Assert statement(s)
        assertAll("result", () -> {
            verify(valueMock).compareTo(object);
            verify(valueMock).compareTo(object2);
        });
    }

    //BaseRock generated method id: ${inclusiveBetween5WhenArrayUtilsIsEmptyValuesThrowsIllegalArgumentException}, hash: 03A7CFE51C59B741246D3A9B58604309
    @Test()
    void inclusiveBetween5WhenArrayUtilsIsEmptyValuesThrowsIllegalArgumentException() {
        /* Branches:
         * (value.compareTo(start) < 0) : false
         * (value.compareTo(end) > 0) : true
         * (ArrayUtils.isEmpty(values)) : true  #  inside getMessage method
         */
        //Arrange Statement(s)
        Object object = new Object();
        doReturn(1).when(valueMock).compareTo(object);
        Object object2 = new Object();
        doReturn(1).when(valueMock).compareTo(object2);
        Object[] objectArray = new Object[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("message1");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.inclusiveBetween(object, object2, valueMock, "message1", objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
            verify(valueMock).compareTo(object);
            verify(valueMock).compareTo(object2);
        });
    }

    //BaseRock generated method id: ${inclusiveBetween5WhenArrayUtilsNotIsEmptyValuesThrowsIllegalArgumentException}, hash: BA574C1935CE26BCE2F2C62C25154320
    @Disabled()
    @Test()
    void inclusiveBetween5WhenArrayUtilsNotIsEmptyValuesThrowsIllegalArgumentException() {
        /* Branches:
         * (value.compareTo(start) < 0) : false
         * (value.compareTo(end) > 0) : true
         * (ArrayUtils.isEmpty(values)) : false  #  inside getMessage method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        Object[] objectArray = new Object[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("A");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.inclusiveBetween(object, object2, comparableMock, "A", objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${isAssignableFromWhenSuperTypeNotIsAssignableFromTypeThrowsIllegalArgumentException}, hash: 42F75383C0A8A15C2F084EC2942C795E
    @Disabled()
    @Test()
    void isAssignableFromWhenSuperTypeNotIsAssignableFromTypeThrowsIllegalArgumentException() {
        /* Branches:
         * (type == null) : false
         * (superType == null) : false
         * (!superType.isAssignableFrom(type)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("Cannot assign a A to a B");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.isAssignableFrom(Object.class, Object.class);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${isAssignableFromWhenSuperTypeIsAssignableFromType}, hash: 3E8601083A42020C50F2C4A07E091118
    @Test()
    void isAssignableFromWhenSuperTypeIsAssignableFromType() {
        /* Branches:
         * (type == null) : false
         * (superType == null) : false
         * (!superType.isAssignableFrom(type)) : false
         */
        //Act Statement(s)
        Validate.isAssignableFrom(Object.class, Object.class);
    }

    //BaseRock generated method id: ${isAssignableFrom1WhenSuperTypeIsAssignableFromType}, hash: 1DC7E09F733F272E9BE72FAF9D5CDAA9
    @Test()
    void isAssignableFrom1WhenSuperTypeIsAssignableFromType() {
        /* Branches:
         * (!superType.isAssignableFrom(type)) : false
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        //Act Statement(s)
        Validate.isAssignableFrom(Object.class, Object.class, "message1", objectArray);
    }

    //BaseRock generated method id: ${isAssignableFrom1WhenArrayUtilsIsEmptyValuesThrowsIllegalArgumentException}, hash: 45EEF0689D1CD4DEDE1D29C3428C9D62
    @Disabled()
    @Test()
    void isAssignableFrom1WhenArrayUtilsIsEmptyValuesThrowsIllegalArgumentException() {
        /* Branches:
         * (!superType.isAssignableFrom(type)) : true
         * (ArrayUtils.isEmpty(values)) : true  #  inside getMessage method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("message1");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.isAssignableFrom(Object.class, Object.class, "message1", objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${isAssignableFrom1WhenArrayUtilsNotIsEmptyValuesThrowsIllegalArgumentException}, hash: FB71CDC8380BB89D0E6EF4C41BFB3418
    @Disabled()
    @Test()
    void isAssignableFrom1WhenArrayUtilsNotIsEmptyValuesThrowsIllegalArgumentException() {
        /* Branches:
         * (!superType.isAssignableFrom(type)) : true
         * (ArrayUtils.isEmpty(values)) : false  #  inside getMessage method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("A");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.isAssignableFrom(Object.class, Object.class, "A", objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${isInstanceOfWhenTypeNotIsInstanceObjThrowsIllegalArgumentException}, hash: A3B65A7BBBA52ACFB29BDB0A2E82EECD
    @Disabled()
    @Test()
    void isInstanceOfWhenTypeNotIsInstanceObjThrowsIllegalArgumentException() {
        /* Branches:
         * (!type.isInstance(obj)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object object = new Object();
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("Expected type: , actual: A");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.isInstanceOf(Object.class, object);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${isInstanceOfWhenTypeIsInstanceObj}, hash: 0442427464C1F83795E0112FE03BCF4C
    @Test()
    void isInstanceOfWhenTypeIsInstanceObj() {
        /* Branches:
         * (!type.isInstance(obj)) : false
         */
        //Arrange Statement(s)
        Object object = new Object();
        //Act Statement(s)
        Validate.isInstanceOf(Object.class, object);
    }

    //BaseRock generated method id: ${isInstanceOf1WhenTypeIsInstanceObj}, hash: 902887067F008C759D0AC0CEC9919960
    @Test()
    void isInstanceOf1WhenTypeIsInstanceObj() {
        /* Branches:
         * (!type.isInstance(obj)) : false
         */
        //Arrange Statement(s)
        Object object = new Object();
        Object[] objectArray = new Object[] {};
        //Act Statement(s)
        Validate.isInstanceOf(Object.class, object, "message1", objectArray);
    }

    //BaseRock generated method id: ${isInstanceOf1WhenArrayUtilsIsEmptyValuesThrowsIllegalArgumentException}, hash: F1AF52DB1B96E44D0A5263455C8ADBB7
    @Disabled()
    @Test()
    void isInstanceOf1WhenArrayUtilsIsEmptyValuesThrowsIllegalArgumentException() {
        /* Branches:
         * (!type.isInstance(obj)) : true
         * (ArrayUtils.isEmpty(values)) : true  #  inside getMessage method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object object = new Object();
        Object[] objectArray = new Object[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("message1");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.isInstanceOf(Object.class, object, "message1", objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${isInstanceOf1WhenArrayUtilsNotIsEmptyValuesThrowsIllegalArgumentException}, hash: 900A3E5FB126C9683B61D00C998121AC
    @Disabled()
    @Test()
    void isInstanceOf1WhenArrayUtilsNotIsEmptyValuesThrowsIllegalArgumentException() {
        /* Branches:
         * (!type.isInstance(obj)) : true
         * (ArrayUtils.isEmpty(values)) : false  #  inside getMessage method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object object = new Object();
        Object[] objectArray = new Object[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("A");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.isInstanceOf(Object.class, object, "A", objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${isTrueWhenNotExpressionThrowsIllegalArgumentException}, hash: 5F301357F0EE1225CAC27502615CC7D4
    @Test()
    void isTrueWhenNotExpressionThrowsIllegalArgumentException() {
        /* Branches:
         * (!expression) : true
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The validated expression is false");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.isTrue(false);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${isTrueWhenExpression}, hash: 891BAE8E66FEBE35983C91FC7B522FEA
    @Test()
    void isTrueWhenExpression() {
        /* Branches:
         * (!expression) : false
         */
        //Act Statement(s)
        Validate.isTrue(true);
    }

    //BaseRock generated method id: ${isTrue1WhenNotExpressionThrowsIllegalArgumentException}, hash: FEF02CD1B01C7D2D6668B73BB2AEE834
    @Test()
    void isTrue1WhenNotExpressionThrowsIllegalArgumentException() {
        /* Branches:
         * (!expression) : true
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("A");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.isTrue(false, "A", Double.parseDouble("0.0"));
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${isTrue1WhenExpression}, hash: E3E063CFDE9E1EB76212091C08E36311
    @Test()
    void isTrue1WhenExpression() {
        /* Branches:
         * (!expression) : false
         */
        //Act Statement(s)
        Validate.isTrue(true, "message1", Double.parseDouble("0.0"));
    }

    //BaseRock generated method id: ${isTrue2WhenNotExpressionThrowsIllegalArgumentException}, hash: 5FF714663D919356F6A26BF1C7DB718D
    @Test()
    void isTrue2WhenNotExpressionThrowsIllegalArgumentException() {
        /* Branches:
         * (!expression) : true
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("A");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.isTrue(false, "A", 2L);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${isTrue2WhenExpression}, hash: 03EF7DC1A325BF76F2B3F66B072CAECA
    @Test()
    void isTrue2WhenExpression() {
        /* Branches:
         * (!expression) : false
         */
        //Act Statement(s)
        Validate.isTrue(true, "message1", 0L);
    }

    //BaseRock generated method id: ${isTrue3WhenExpression}, hash: 4E2221A12FF30EB4418C2F236E662B28
    @Test()
    void isTrue3WhenExpression() {
        /* Branches:
         * (!expression) : false
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        //Act Statement(s)
        Validate.isTrue(true, "message1", objectArray);
    }

    //BaseRock generated method id: ${isTrue3WhenArrayUtilsIsEmptyValuesThrowsIllegalArgumentException}, hash: 1A8D29B579F606CF5980A7AF7C144DA1
    @Test()
    void isTrue3WhenArrayUtilsIsEmptyValuesThrowsIllegalArgumentException() {
        /* Branches:
         * (!expression) : true
         * (ArrayUtils.isEmpty(values)) : true  #  inside getMessage method
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("message1");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.isTrue(false, "message1", objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${isTrue3WhenArrayUtilsNotIsEmptyValuesThrowsIllegalArgumentException}, hash: 992E859A5BB66208606B0EFA9B97C6DE
    @Test()
    void isTrue3WhenArrayUtilsNotIsEmptyValuesThrowsIllegalArgumentException() {
        /* Branches:
         * (!expression) : true
         * (ArrayUtils.isEmpty(values)) : false  #  inside getMessage method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("A");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.isTrue(false, "A", objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${isTrue4WhenNotExpressionThrowsIllegalArgumentException}, hash: 04B77C44F29ED1C7ABFE40FB6224825B
    @Test()
    void isTrue4WhenNotExpressionThrowsIllegalArgumentException() {
        /* Branches:
         * (!expression) : true
         */
        //Arrange Statement(s)
        Supplier<String> messageSupplierMock = mock(Supplier.class);
        doReturn("return_of_get1").when(messageSupplierMock).get();
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("return_of_get1");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.isTrue(false, messageSupplierMock);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
            verify(messageSupplierMock).get();
        });
    }

    //BaseRock generated method id: ${isTrue4WhenExpression}, hash: 765ACC085635C729FC7516B5D4237D7B
    @Test()
    void isTrue4WhenExpression() {
        /* Branches:
         * (!expression) : false
         */
        //Arrange Statement(s)
        Supplier<String> supplierMock = mock(Supplier.class);
        //Act Statement(s)
        Validate.isTrue(true, supplierMock);
    }

    //BaseRock generated method id: ${matchesPatternWhenPatternNotMatchesPatternInputThrowsIllegalArgumentException}, hash: D614925ECEFFB76F2F17FE6C10CAEF4D
    @Test()
    void matchesPatternWhenPatternNotMatchesPatternInputThrowsIllegalArgumentException() {
        /* Branches:
         * (!Pattern.matches(pattern, input)) : true
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The string input1 does not match the pattern A");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.matchesPattern("input1", "A");
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${matchesPatternWhenPatternMatchesPatternInput}, hash: C88853AC1B97E315E0F39A8361AB6EA9
    @Disabled()
    @Test()
    void matchesPatternWhenPatternMatchesPatternInput() {
        /* Branches:
         * (!Pattern.matches(pattern, input)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        Validate.matchesPattern("input1", "pattern1");
    }

    //BaseRock generated method id: ${matchesPattern1WhenPatternMatchesPatternInput}, hash: 81BB3A875CF77ABDBB14A619FCB8927B
    @Disabled()
    @Test()
    void matchesPattern1WhenPatternMatchesPatternInput() {
        /* Branches:
         * (!Pattern.matches(pattern, input)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        //Act Statement(s)
        Validate.matchesPattern("input1", "pattern1", "message1", objectArray);
    }

    //BaseRock generated method id: ${matchesPattern1WhenArrayUtilsIsEmptyValuesThrowsIllegalArgumentException}, hash: 8904AD1F84F6D65A721735C3795EDCEB
    @Test()
    void matchesPattern1WhenArrayUtilsIsEmptyValuesThrowsIllegalArgumentException() {
        /* Branches:
         * (!Pattern.matches(pattern, input)) : true
         * (ArrayUtils.isEmpty(values)) : true  #  inside getMessage method
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("message1");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.matchesPattern("input1", "pattern1", "message1", objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${matchesPattern1WhenArrayUtilsNotIsEmptyValuesThrowsIllegalArgumentException}, hash: 5239B4CD652EF964A08417DC5C7DFF76
    @Test()
    void matchesPattern1WhenArrayUtilsNotIsEmptyValuesThrowsIllegalArgumentException() {
        /* Branches:
         * (!Pattern.matches(pattern, input)) : true
         * (ArrayUtils.isEmpty(values)) : false  #  inside getMessage method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("A");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.matchesPattern("input1", "pattern1", "A", objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${noNullElementsTest}, hash: 9F3998C545F09BE3A4E0F7B803C08500
    @Test()
    void noNullElementsTest() {
        //Arrange Statement(s)
        try (MockedStatic<Validate> validate = mockStatic(Validate.class, CALLS_REAL_METHODS)) {
            Iterable iterable = new ArrayList<>();
            Object[] objectArray = new Object[] {};
            validate.when(() -> Validate.noNullElements((Iterable) any(), eq("The validated collection contains null element at index: %d"), eq(objectArray))).thenReturn(iterable);
            Iterable iterable2 = new ArrayList<>();
            //Act Statement(s)
            Iterable result = Validate.noNullElements(iterable2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(iterable));
                validate.verify(() -> Validate.noNullElements((Iterable) any(), eq("The validated collection contains null element at index: %d"), eq(objectArray)), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${noNullElements1WhenArrayUtilsIsEmptyValuesThrowsIllegalArgumentException}, hash: AB152B5A6D794E4E0397AB721A2414F8
    @Test()
    void noNullElements1WhenArrayUtilsIsEmptyValuesThrowsIllegalArgumentException() {
        /* Branches:
         * (e == null) : true  #  inside lambda$noNullElements$0 method
         * (ArrayUtils.isEmpty(values)) : true  #  inside getMessage method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Iterable iterable = new ArrayList<>(Arrays.asList((Object) null));
        Object[] objectArray = new Object[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("message1");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.noNullElements(iterable, "message1", objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${noNullElements1WhenArrayUtilsNotIsEmptyValuesThrowsIllegalArgumentException}, hash: B1F6BDAF4E4D44A63F849ECCFBCDFEB5
    @Test()
    void noNullElements1WhenArrayUtilsNotIsEmptyValuesThrowsIllegalArgumentException() {
        /* Branches:
         * (e == null) : true  #  inside lambda$noNullElements$0 method
         * (ArrayUtils.isEmpty(values)) : false  #  inside getMessage method
         */
        //Arrange Statement(s)
        Iterable iterable = new ArrayList<>(Arrays.asList((Object) null));
        Object[] objectArray = new Object[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("A");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.noNullElements(iterable, "A", objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${noNullElements2Test}, hash: 8619AD62822BC24926F0AD8080EC7974
    @Test()
    void noNullElements2Test() {
        //Arrange Statement(s)
        try (MockedStatic<Validate> validate = mockStatic(Validate.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            Object[] objectArray2 = new Object[] {};
            Object[] objectArray3 = new Object[] {};
            validate.when(() -> Validate.noNullElements(objectArray2, "The validated array contains null element at index: %d", objectArray3)).thenReturn(objectArray);
            //Act Statement(s)
            Object[] result = Validate.noNullElements(objectArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(objectArray));
                validate.verify(() -> Validate.noNullElements(objectArray2, "The validated array contains null element at index: %d", objectArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${noNullElements3WhenIIndexOfArrayIsNotNull}, hash: 50FF29E7696EEAFDC97305B83ABAEEB6
    @Test()
    void noNullElements3WhenIIndexOfArrayIsNotNull() {
        /* Branches:
         * (i < array.length) : true
         * (array[i] == null) : false
         */
        //Arrange Statement(s)
        Object object = new Object();
        Object[] objectArray = new Object[] { object };
        Object[] objectArray2 = new Object[] {};
        //Act Statement(s)
        Object[] result = Validate.noNullElements(objectArray, "message1", objectArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(objectArray)));
    }

    //BaseRock generated method id: ${noNullElements3WhenArrayUtilsIsEmptyValuesThrowsIllegalArgumentException}, hash: AE400B0813F0031822B18D5EAA4607DC
    @Test()
    void noNullElements3WhenArrayUtilsIsEmptyValuesThrowsIllegalArgumentException() {
        /* Branches:
         * (i < array.length) : true
         * (array[i] == null) : true
         * (ArrayUtils.isEmpty(values)) : true  #  inside getMessage method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] { (Object) null };
        Object[] objectArray2 = new Object[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("message1");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.noNullElements(objectArray, "message1", objectArray2);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${noNullElements3WhenArrayUtilsNotIsEmptyValuesThrowsIllegalArgumentException}, hash: B9BF9EE241FD513EED55B2ACAB356EA4
    @Disabled()
    @Test()
    void noNullElements3WhenArrayUtilsNotIsEmptyValuesThrowsIllegalArgumentException() {
        /* Branches:
         * (i < array.length) : true
         * (array[i] == null) : true
         * (ArrayUtils.isEmpty(values)) : false  #  inside getMessage method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        Object[] objectArray2 = new Object[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("s1");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.noNullElements(objectArray, "message1", objectArray2);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${notBlankTest}, hash: 6BD3EDFD5E4FCBBB0FE2E85D524DBFB6
    @Test()
    void notBlankTest() {
        //Arrange Statement(s)
        try (MockedStatic<Validate> validate = mockStatic(Validate.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            validate.when(() -> Validate.notBlank("chars1", "The validated character sequence is blank", objectArray)).thenReturn("charSequence1");
            //Act Statement(s)
            CharSequence result = Validate.notBlank("chars1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("charSequence1"));
                validate.verify(() -> Validate.notBlank("chars1", "The validated character sequence is blank", objectArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${notBlank1WhenStringUtilsNotIsBlankChars}, hash: 3CB30FA3BECE6117C66277D721A33D49
    @Test()
    void notBlank1WhenStringUtilsNotIsBlankChars() {
        /* Branches:
         * (StringUtils.isBlank(chars)) : false
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        //Act Statement(s)
        CharSequence result = Validate.notBlank("chars1", "message1", objectArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("chars1")));
    }

    //BaseRock generated method id: ${notBlank1WhenArrayUtilsIsEmptyValuesThrowsIllegalArgumentException}, hash: E50AEA5EF41A140466C25A3E452486C2
    @Disabled()
    @Test()
    void notBlank1WhenArrayUtilsIsEmptyValuesThrowsIllegalArgumentException() {
        /* Branches:
         * (StringUtils.isBlank(chars)) : true
         * (ArrayUtils.isEmpty(values)) : true  #  inside getMessage method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("message1");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.notBlank("chars1", "message1", objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${notBlank1WhenArrayUtilsNotIsEmptyValuesThrowsIllegalArgumentException}, hash: 80EF9F5821085280332FF74DB33466D6
    @Disabled()
    @Test()
    void notBlank1WhenArrayUtilsNotIsEmptyValuesThrowsIllegalArgumentException() {
        /* Branches:
         * (StringUtils.isBlank(chars)) : true
         * (ArrayUtils.isEmpty(values)) : false  #  inside getMessage method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("A");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.notBlank("chars1", "A", objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${notEmptyTest}, hash: 5B1C348A570A57B9DB1F02372048DFFD
    @Test()
    void notEmptyTest() {
        //Arrange Statement(s)
        try (MockedStatic<Validate> validate = mockStatic(Validate.class, CALLS_REAL_METHODS)) {
            Collection collection = new ArrayList<>();
            Object[] objectArray = new Object[] {};
            validate.when(() -> Validate.notEmpty(anyCollection(), eq("The validated collection is empty"), eq(objectArray))).thenReturn(collection);
            Collection collection2 = new ArrayList<>();
            //Act Statement(s)
            Collection result = Validate.notEmpty(collection2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(collection));
                validate.verify(() -> Validate.notEmpty(anyCollection(), eq("The validated collection is empty"), eq(objectArray)), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${notEmpty1Test}, hash: 7839DB5E9D935F0818E4878958F7FC3A
    @Test()
    void notEmpty1Test() {
        //Arrange Statement(s)
        try (MockedStatic<Validate> validate = mockStatic(Validate.class, CALLS_REAL_METHODS)) {
            Map map = new HashMap<>();
            Object[] objectArray = new Object[] {};
            validate.when(() -> Validate.notEmpty(anyMap(), eq("The validated map is empty"), eq(objectArray))).thenReturn(map);
            Map map2 = new HashMap<>();
            //Act Statement(s)
            Map result = Validate.notEmpty(map2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(map));
                validate.verify(() -> Validate.notEmpty(anyMap(), eq("The validated map is empty"), eq(objectArray)), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${notEmpty2Test}, hash: C4B768EF41F500F9F352D20300F24AEA
    @Disabled()
    @Test()
    void notEmpty2Test() {
        //Arrange Statement(s)
        try (MockedStatic<Validate> validate = mockStatic(Validate.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            validate.when(() -> Validate.notEmpty("chars1", "The validated character sequence is empty", objectArray)).thenReturn("charSequence1");
            //Act Statement(s)
            CharSequence result = Validate.notEmpty((CharSequence) "chars1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("charSequence1"));
                validate.verify(() -> Validate.notEmpty("chars1", "The validated character sequence is empty", objectArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${notEmpty3WhenCollectionNotIsEmpty}, hash: D47A9A183B48F3FE4A818A186C079E2E
    @Test()
    void notEmpty3WhenCollectionNotIsEmpty() {
        /* Branches:
         * (collection.isEmpty()) : false
         */
        //Arrange Statement(s)
        Collection collection = new ArrayList<>();
        collection.add(null);
        Object[] objectArray = new Object[] {};
        //Act Statement(s)
        Collection result = Validate.notEmpty(collection, "message1", objectArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(collection)));
    }

    //BaseRock generated method id: ${notEmpty3WhenArrayUtilsIsEmptyValuesThrowsIllegalArgumentException}, hash: 25453037423E8E31B8E2BBABABD613F3
    @Test()
    void notEmpty3WhenArrayUtilsIsEmptyValuesThrowsIllegalArgumentException() {
        /* Branches:
         * (collection.isEmpty()) : true
         * (ArrayUtils.isEmpty(values)) : true  #  inside getMessage method
         */
        //Arrange Statement(s)
        Collection collection = new ArrayList<>();
        Object[] objectArray = new Object[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("message1");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.notEmpty(collection, "message1", objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${notEmpty3WhenArrayUtilsNotIsEmptyValuesThrowsIllegalArgumentException}, hash: CDCCE140750679F3AFCEAFD49CDA2CE7
    @Test()
    void notEmpty3WhenArrayUtilsNotIsEmptyValuesThrowsIllegalArgumentException() {
        /* Branches:
         * (collection.isEmpty()) : true
         * (ArrayUtils.isEmpty(values)) : false  #  inside getMessage method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Collection collection = new ArrayList<>();
        Object[] objectArray = new Object[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("A");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.notEmpty(collection, "A", objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${notEmpty4WhenMapNotIsEmpty}, hash: AF82F9EDC3637BDA7E627BCCB6039D00
    @Test()
    void notEmpty4WhenMapNotIsEmpty() {
        /* Branches:
         * (map.isEmpty()) : false
         */
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        Map<Object, Object> objectObjectMap = new HashMap<>();
        objectObjectMap.put(object, object2);
        Object[] objectArray = new Object[] {};
        //Act Statement(s)
        Map result = Validate.notEmpty(objectObjectMap, "message1", objectArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(objectObjectMap)));
    }

    //BaseRock generated method id: ${notEmpty4WhenArrayUtilsIsEmptyValuesThrowsIllegalArgumentException}, hash: 440E9F6F352CFD7CA52DC3D919280FC8
    @Test()
    void notEmpty4WhenArrayUtilsIsEmptyValuesThrowsIllegalArgumentException() {
        /* Branches:
         * (map.isEmpty()) : true
         * (ArrayUtils.isEmpty(values)) : true  #  inside getMessage method
         */
        //Arrange Statement(s)
        Map map = new HashMap<>();
        Object[] objectArray = new Object[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("message1");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.notEmpty(map, "message1", objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${notEmpty4WhenArrayUtilsNotIsEmptyValuesThrowsIllegalArgumentException}, hash: 5348E1BE411FCC3647682A7FC0B4FF0C
    @Test()
    void notEmpty4WhenArrayUtilsNotIsEmptyValuesThrowsIllegalArgumentException() {
        /* Branches:
         * (map.isEmpty()) : true
         * (ArrayUtils.isEmpty(values)) : false  #  inside getMessage method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Map map = new HashMap<>();
        Object[] objectArray = new Object[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("A");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.notEmpty(map, "A", objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${notEmpty5WhenCharsLengthNotEquals0}, hash: 6AFFF5AA34C47F9BACA06728BF7BC5D0
    @Test()
    void notEmpty5WhenCharsLengthNotEquals0() {
        /* Branches:
         * (chars.length() == 0) : false
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        //Act Statement(s)
        CharSequence result = Validate.notEmpty((CharSequence) "chars1", "message1", objectArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("chars1")));
    }

    //BaseRock generated method id: ${notEmpty5WhenArrayUtilsIsEmptyValuesThrowsIllegalArgumentException}, hash: F7E5C14DC45C5BF88BEFC724D167939C
    @Disabled()
    @Test()
    void notEmpty5WhenArrayUtilsIsEmptyValuesThrowsIllegalArgumentException() {
        /* Branches:
         * (chars.length() == 0) : true
         * (ArrayUtils.isEmpty(values)) : true  #  inside getMessage method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("message1");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.notEmpty((CharSequence) "chars1", "message1", objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${notEmpty5WhenArrayUtilsNotIsEmptyValuesThrowsIllegalArgumentException}, hash: 2C6DA415557AA2F7148541DF54FD6677
    @Disabled()
    @Test()
    void notEmpty5WhenArrayUtilsNotIsEmptyValuesThrowsIllegalArgumentException() {
        /* Branches:
         * (chars.length() == 0) : true
         * (ArrayUtils.isEmpty(values)) : false  #  inside getMessage method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("A");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.notEmpty((CharSequence) "chars1", "A", objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${notEmpty6Test}, hash: 477D9117F139DDD1E8E67D996F938E82
    @Test()
    void notEmpty6Test() {
        //Arrange Statement(s)
        try (MockedStatic<Validate> validate = mockStatic(Validate.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            Object[] objectArray2 = new Object[] {};
            Object[] objectArray3 = new Object[] {};
            validate.when(() -> Validate.notEmpty(objectArray2, "The validated array is empty", objectArray3)).thenReturn(objectArray);
            //Act Statement(s)
            Object[] result = Validate.notEmpty(objectArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(objectArray));
                validate.verify(() -> Validate.notEmpty(objectArray2, "The validated array is empty", objectArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${notEmpty7WhenArrayLengthNotEquals0}, hash: D5AF539495B1DD850B0CB8F41821D59D
    @Test()
    void notEmpty7WhenArrayLengthNotEquals0() {
        /* Branches:
         * (array.length == 0) : false
         */
        //Arrange Statement(s)
        Object object = new Object();
        Object[] objectArray = new Object[] { object };
        Object[] objectArray2 = new Object[] {};
        //Act Statement(s)
        Object[] result = Validate.notEmpty(objectArray, "message1", objectArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(objectArray)));
    }

    //BaseRock generated method id: ${notEmpty7WhenArrayUtilsIsEmptyValuesThrowsIllegalArgumentException}, hash: DD30E4D14DBE0B914A62586C607EB195
    @Test()
    void notEmpty7WhenArrayUtilsIsEmptyValuesThrowsIllegalArgumentException() {
        /* Branches:
         * (array.length == 0) : true
         * (ArrayUtils.isEmpty(values)) : true  #  inside getMessage method
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        Object[] objectArray2 = new Object[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("message1");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.notEmpty(objectArray, "message1", objectArray2);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${notEmpty7WhenArrayUtilsNotIsEmptyValuesThrowsIllegalArgumentException}, hash: E40AF1F56A896D7A65A50CAA6A1E9B10
    @Test()
    void notEmpty7WhenArrayUtilsNotIsEmptyValuesThrowsIllegalArgumentException() {
        /* Branches:
         * (array.length == 0) : true
         * (ArrayUtils.isEmpty(values)) : false  #  inside getMessage method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        Object[] objectArray2 = new Object[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("A");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.notEmpty(objectArray, "A", objectArray2);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${notNaNTest}, hash: 5D6D355D424B095365B64D8AC56A9F17
    @Test()
    void notNaNTest() {
        //Arrange Statement(s)
        try (MockedStatic<Validate> validate = mockStatic(Validate.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            validate.when(() -> Validate.notNaN(Double.parseDouble("0.0"), "The validated value is not a number", objectArray)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            Validate.notNaN(Double.parseDouble("0.0"));
            //Assert statement(s)
            assertAll("result", () -> validate.verify(() -> Validate.notNaN(Double.parseDouble("0.0"), "The validated value is not a number", objectArray), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${notNaN1WhenDoubleNotIsNaNValue}, hash: 2FD7FF74FD56E1696F3DE73B2C523F51
    @Test()
    void notNaN1WhenDoubleNotIsNaNValue() {
        /* Branches:
         * (Double.isNaN(value)) : false
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        //Act Statement(s)
        Validate.notNaN(Double.parseDouble("0.0"), "message1", objectArray);
    }

    //BaseRock generated method id: ${notNaN1WhenArrayUtilsIsEmptyValuesThrowsIllegalArgumentException}, hash: 022DDA60EB23F9C951F5B71B4957E77F
    @Disabled()
    @Test()
    void notNaN1WhenArrayUtilsIsEmptyValuesThrowsIllegalArgumentException() {
        /* Branches:
         * (Double.isNaN(value)) : true
         * (ArrayUtils.isEmpty(values)) : true  #  inside getMessage method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("message1");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.notNaN(Double.parseDouble("0.0"), "message1", objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${notNaN1WhenArrayUtilsNotIsEmptyValuesThrowsIllegalArgumentException}, hash: 16D025359A483B7252542E59A35B9410
    @Disabled()
    @Test()
    void notNaN1WhenArrayUtilsNotIsEmptyValuesThrowsIllegalArgumentException() {
        /* Branches:
         * (Double.isNaN(value)) : true
         * (ArrayUtils.isEmpty(values)) : false  #  inside getMessage method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("s1");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            Validate.notNaN(Double.parseDouble("0.0"), "message1", objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${notNullTest}, hash: EFFDCB6917C550EC5566A0866A32FF7E
    @Test()
    void notNullTest() {
        //Arrange Statement(s)
        try (MockedStatic<Validate> validate = mockStatic(Validate.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            Object[] objectArray = new Object[] {};
            validate.when(() -> Validate.notNull(object2, "The validated object is null", objectArray)).thenReturn(object);
            //Act Statement(s)
            Object result = Validate.notNull(object2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(object));
                validate.verify(() -> Validate.notNull(object2, "The validated object is null", objectArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${notNull1Test}, hash: 7D740672338A7918469C3418C929B1B8
    @Test()
    void notNull1Test() {
        //Arrange Statement(s)
        Object object = new Object();
        Object[] objectArray = new Object[] {};
        //Act Statement(s)
        Object result = Validate.notNull(object, "message1", objectArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(object)));
    }

    //BaseRock generated method id: ${validIndexTest}, hash: 0C8CD76CA7D18452D5E993BEB90FA841
    @Disabled()
    @Test()
    void validIndexTest() {
        //Arrange Statement(s)
        try (MockedStatic<Validate> validate = mockStatic(Validate.class, CALLS_REAL_METHODS)) {
            Collection collection = new ArrayList<>();
            Object[] objectArray = new Object[] { 1 };
            validate.when(() -> Validate.validIndex(anyCollection(), eq(1), eq("The validated collection index is invalid: %d"), eq(objectArray))).thenReturn(collection);
            Collection collection2 = new ArrayList<>();
            //Act Statement(s)
            Collection result = Validate.validIndex(collection2, 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(collection));
                validate.verify(() -> Validate.validIndex(anyCollection(), eq(1), eq("The validated collection index is invalid: %d"), eq(objectArray)), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${validIndex1Test}, hash: F1DFA032B291942F2BEAB0582B85BBC7
    @Disabled()
    @Test()
    void validIndex1Test() {
        //Arrange Statement(s)
        try (MockedStatic<Validate> validate = mockStatic(Validate.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] { 1 };
            validate.when(() -> Validate.validIndex("chars1", 1, "The validated character sequence index is invalid: %d", objectArray)).thenReturn("charSequence1");
            //Act Statement(s)
            CharSequence result = Validate.validIndex((CharSequence) "chars1", 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("charSequence1"));
                validate.verify(() -> Validate.validIndex("chars1", 1, "The validated character sequence index is invalid: %d", objectArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${validIndex2WhenIndexLessThanCollectionSize}, hash: 0EF576FBD4D8B9AD74950A85B44875C0
    @Test()
    void validIndex2WhenIndexLessThanCollectionSize() {
        /* Branches:
         * (index < 0) : false
         * (index >= collection.size()) : false
         */
        //Arrange Statement(s)
        Collection collection = new ArrayList<>();
        collection.add(null);
        Object[] objectArray = new Object[] {};
        //Act Statement(s)
        Collection result = Validate.validIndex(collection, 0, "message1", objectArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(collection)));
    }

    //BaseRock generated method id: ${validIndex2WhenArrayUtilsIsEmptyValuesThrowsIndexOutOfBoundsException}, hash: C29F51F2B4CD3A7FEA2D7E3224970269
    @Test()
    void validIndex2WhenArrayUtilsIsEmptyValuesThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (index < 0) : false
         * (index >= collection.size()) : true
         * (ArrayUtils.isEmpty(values)) : true  #  inside getMessage method
         */
        //Arrange Statement(s)
        Collection collection = new ArrayList<>();
        Object[] objectArray = new Object[] {};
        IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("message1");
        //Act Statement(s)
        final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
            Validate.validIndex(collection, 0, "message1", objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${validIndex2WhenIndexGreaterThanOrEqualsToCollectionSizeAndArrayUtilsNotIsEmptyValuesThrowsIndexOutOfBoundsException}, hash: 4064C0B30CEEE157DA44CA0C1B8158F1
    @Test()
    void validIndex2WhenIndexGreaterThanOrEqualsToCollectionSizeAndArrayUtilsNotIsEmptyValuesThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (index < 0) : false
         * (index >= collection.size()) : true
         * (ArrayUtils.isEmpty(values)) : false  #  inside getMessage method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Collection collection = new ArrayList<>();
        Object[] objectArray = new Object[] {};
        IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("A");
        //Act Statement(s)
        final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
            Validate.validIndex(collection, 0, "A", objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${validIndex3WhenIndexLessThanCharsLength}, hash: 9D8D97BC9BA56F255FCB7739D81AB047
    @Test()
    void validIndex3WhenIndexLessThanCharsLength() {
        /* Branches:
         * (index < 0) : false
         * (index >= chars.length()) : false
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        //Act Statement(s)
        CharSequence result = Validate.validIndex((CharSequence) "chars1", 1, "message1", objectArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("chars1")));
    }

    //BaseRock generated method id: ${validIndex3WhenArrayUtilsIsEmptyValuesThrowsIndexOutOfBoundsException}, hash: 7E54BB40E6C813EA6AA3BD4085ADDB9C
    @Disabled()
    @Test()
    void validIndex3WhenArrayUtilsIsEmptyValuesThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (index < 0) : false
         * (index >= chars.length()) : true
         * (ArrayUtils.isEmpty(values)) : true  #  inside getMessage method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("message1");
        //Act Statement(s)
        final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
            Validate.validIndex((CharSequence) "chars1", 1, "message1", objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${validIndex3WhenIndexGreaterThanOrEqualsToCharsLengthAndArrayUtilsNotIsEmptyValuesThrowsIndexOutOfBoundsException}, hash: 300EE86F1AF1ADFCD53F23839EB216AC
    @Disabled()
    @Test()
    void validIndex3WhenIndexGreaterThanOrEqualsToCharsLengthAndArrayUtilsNotIsEmptyValuesThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (index < 0) : false
         * (index >= chars.length()) : true
         * (ArrayUtils.isEmpty(values)) : false  #  inside getMessage method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("A");
        //Act Statement(s)
        final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
            Validate.validIndex((CharSequence) "chars1", 1, "A", objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${validIndex4Test}, hash: 91A9DE5A5EC33286E8F9D69D65002EF5
    @Test()
    void validIndex4Test() {
        //Arrange Statement(s)
        try (MockedStatic<Validate> validate = mockStatic(Validate.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            Object[] objectArray2 = new Object[] {};
            Object[] objectArray3 = new Object[] { 1 };
            validate.when(() -> Validate.validIndex(objectArray2, 1, "The validated array index is invalid: %d", objectArray3)).thenReturn(objectArray);
            //Act Statement(s)
            Object[] result = Validate.validIndex(objectArray2, 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(objectArray));
                validate.verify(() -> Validate.validIndex(objectArray2, 1, "The validated array index is invalid: %d", objectArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${validIndex5WhenIndexLessThanArrayLength}, hash: 691083E10DBD0B97C81795D7BE8228BF
    @Test()
    void validIndex5WhenIndexLessThanArrayLength() {
        /* Branches:
         * (index < 0) : false
         * (index >= array.length) : false
         */
        //Arrange Statement(s)
        Object object = new Object();
        Object[] objectArray = new Object[] { object };
        Object[] objectArray2 = new Object[] {};
        //Act Statement(s)
        Object[] result = Validate.validIndex(objectArray, 0, "message1", objectArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(objectArray)));
    }

    //BaseRock generated method id: ${validIndex5WhenArrayUtilsIsEmptyValuesThrowsIndexOutOfBoundsException}, hash: FF51C15211341E7ED940742A5A578519
    @Test()
    void validIndex5WhenArrayUtilsIsEmptyValuesThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (index < 0) : false
         * (index >= array.length) : true
         * (ArrayUtils.isEmpty(values)) : true  #  inside getMessage method
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        Object[] objectArray2 = new Object[] {};
        IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("message1");
        //Act Statement(s)
        final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
            Validate.validIndex(objectArray, 0, "message1", objectArray2);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${validIndex5WhenIndexGreaterThanOrEqualsToArrayLengthAndArrayUtilsNotIsEmptyValuesThrowsIndexOutOfBoundsException}, hash: 70EFAA241EE80D0C7180C526D4C95A9F
    @Test()
    void validIndex5WhenIndexGreaterThanOrEqualsToArrayLengthAndArrayUtilsNotIsEmptyValuesThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (index < 0) : false
         * (index >= array.length) : true
         * (ArrayUtils.isEmpty(values)) : false  #  inside getMessage method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        Object[] objectArray2 = new Object[] {};
        IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("A");
        //Act Statement(s)
        final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
            Validate.validIndex(objectArray, 0, "A", objectArray2);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${validStateWhenNotExpressionThrowsIllegalStateException}, hash: 91BE71A57CE87AF2196B383483DA5B16
    @Test()
    void validStateWhenNotExpressionThrowsIllegalStateException() {
        /* Branches:
         * (!expression) : true
         */
        //Arrange Statement(s)
        IllegalStateException illegalStateException = new IllegalStateException("The validated state is false");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            Validate.validState(false);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${validStateWhenExpression}, hash: E38E2ED5497E1C9996F16E342B0786D7
    @Test()
    void validStateWhenExpression() {
        /* Branches:
         * (!expression) : false
         */
        //Act Statement(s)
        Validate.validState(true);
    }

    //BaseRock generated method id: ${validState1WhenExpression}, hash: 8252CD39CCF632B2FAE9DA61DDBFA79D
    @Test()
    void validState1WhenExpression() {
        /* Branches:
         * (!expression) : false
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        //Act Statement(s)
        Validate.validState(true, "message1", objectArray);
    }

    //BaseRock generated method id: ${validState1WhenArrayUtilsIsEmptyValuesThrowsIllegalStateException}, hash: 307DB51D0D058DDD041EA5A8E4B7415B
    @Test()
    void validState1WhenArrayUtilsIsEmptyValuesThrowsIllegalStateException() {
        /* Branches:
         * (!expression) : true
         * (ArrayUtils.isEmpty(values)) : true  #  inside getMessage method
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        IllegalStateException illegalStateException = new IllegalStateException("message1");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            Validate.validState(false, "message1", objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${validState1WhenArrayUtilsNotIsEmptyValuesThrowsIllegalStateException}, hash: 6410656E4B3A7C49657A43BC36FC28B0
    @Test()
    void validState1WhenArrayUtilsNotIsEmptyValuesThrowsIllegalStateException() {
        /* Branches:
         * (!expression) : true
         * (ArrayUtils.isEmpty(values)) : false  #  inside getMessage method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        IllegalStateException illegalStateException = new IllegalStateException("A");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            Validate.validState(false, "A", objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }
}
