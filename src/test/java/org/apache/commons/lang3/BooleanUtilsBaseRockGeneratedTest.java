package org.apache.commons.lang3;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.util.List;
import java.util.Arrays;
import org.mockito.MockedStatic;
import java.util.ArrayList;
import java.util.function.Consumer;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInRelativeOrder;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.hamcrest.Matchers.is;
import org.junit.jupiter.api.Disabled;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class BooleanUtilsBaseRockGeneratedTest {

    //BaseRock generated method id: ${andWhenNotElement}, hash: A955CA2A16D65050117395671CEAAF71
    @Test()
    void andWhenNotElement() {
        /* Branches:
         * (for-each(array)) : true
         * (!element) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false };
        //Act Statement(s)
        boolean result = BooleanUtils.and(booleanArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${andWhenElement}, hash: E7817D38734CC6F21B3B6FC4B8A94B30
    @Test()
    void andWhenElement() {
        /* Branches:
         * (for-each(array)) : true
         * (!element) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { true };
        //Act Statement(s)
        boolean result = BooleanUtils.and(booleanArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${and1WhenAndArrayUtilsToPrimitiveArray}, hash: CB1642469213CD20F548CDF69CE09D18
    @Disabled()
    @Test()
    void and1WhenAndArrayUtilsToPrimitiveArray() {
        /* Branches:
         * (and(ArrayUtils.toPrimitive(array))) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<BooleanUtils> booleanUtils = mockStatic(BooleanUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            booleanUtils.when(() -> BooleanUtils.and(booleanArray)).thenReturn(true);
            Boolean[] booleanArray2 = new Boolean[] {};
            //Act Statement(s)
            Boolean result = BooleanUtils.and(booleanArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                booleanUtils.verify(() -> BooleanUtils.and(booleanArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${and1WhenAndNotArrayUtilsToPrimitiveArray}, hash: C6BAFAA3AB9197DC7CA6086E6F858823
    @Disabled()
    @Test()
    void and1WhenAndNotArrayUtilsToPrimitiveArray() {
        /* Branches:
         * (and(ArrayUtils.toPrimitive(array))) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<BooleanUtils> booleanUtils = mockStatic(BooleanUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            booleanUtils.when(() -> BooleanUtils.and(booleanArray)).thenReturn(false);
            Boolean[] booleanArray2 = new Boolean[] {};
            //Act Statement(s)
            Boolean result = BooleanUtils.and(booleanArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                booleanUtils.verify(() -> BooleanUtils.and(booleanArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${booleanValuesTest}, hash: C55646B8A747AE39E2BE71D8CCE11859
    @Test()
    void booleanValuesTest() {
        //Act Statement(s)
        Boolean[] result = BooleanUtils.booleanValues();
        Boolean[] booleanResultArray = new Boolean[] { false, true };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${compareWhenXEqualsY}, hash: FFD60ED498E11F3F1F20D9734A35CB04
    @Test()
    void compareWhenXEqualsY() {
        /* Branches:
         * (x == y) : true
         */
        //Act Statement(s)
        int result = BooleanUtils.compare(false, false);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${compareWhenX}, hash: FD6DBCA10F9EB92B8D23EBF5F08C7512
    @Test()
    void compareWhenX() {
        /* Branches:
         * (x == y) : false
         * (x) : true
         */
        //Act Statement(s)
        int result = BooleanUtils.compare(true, false);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1)));
    }

    //BaseRock generated method id: ${compareWhenNotX}, hash: 624C26742DCBA157DFF7D393E353C85F
    @Test()
    void compareWhenNotX() {
        /* Branches:
         * (x == y) : false
         * (x) : false
         */
        //Act Statement(s)
        int result = BooleanUtils.compare(false, true);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${forEachTest}, hash: 5FD524B8C7E1A6B616ED10BD1FBA42F4
    @Test()
    void forEachTest() {
        //Arrange Statement(s)
        Consumer<Boolean> consumerMock = mock(Consumer.class);
        //Act Statement(s)
        BooleanUtils.forEach(consumerMock);
    }

    //BaseRock generated method id: ${isFalseWhenBooleanFALSEEqualsBool}, hash: 5AC12382CCCCA932C9F715FB6036F0DB
    @Test()
    void isFalseWhenBooleanFALSEEqualsBool() {
        /* Branches:
         * (Boolean.FALSE.equals(bool)) : true
         */
        //Act Statement(s)
        boolean result = BooleanUtils.isFalse(false);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isFalseWhenBooleanFALSENotEqualsBool}, hash: 1608958F5D48B82ADDD287D0F5BD333E
    @Test()
    void isFalseWhenBooleanFALSENotEqualsBool() {
        /* Branches:
         * (Boolean.FALSE.equals(bool)) : false
         */
        //Act Statement(s)
        boolean result = BooleanUtils.isFalse(true);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isNotFalseWhenIsFalseNotBool}, hash: 4608544DC8982A01AB4A4B931DA137C6
    @Test()
    void isNotFalseWhenIsFalseNotBool() {
        /* Branches:
         * (!isFalse(bool)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<BooleanUtils> booleanUtils = mockStatic(BooleanUtils.class, CALLS_REAL_METHODS)) {
            booleanUtils.when(() -> BooleanUtils.isFalse(false)).thenReturn(false);
            //Act Statement(s)
            boolean result = BooleanUtils.isNotFalse(false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                booleanUtils.verify(() -> BooleanUtils.isFalse(false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isNotFalseWhenIsFalseBool}, hash: 67123A088B8849A40D0A32B98C85AA1C
    @Test()
    void isNotFalseWhenIsFalseBool() {
        /* Branches:
         * (!isFalse(bool)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<BooleanUtils> booleanUtils = mockStatic(BooleanUtils.class, CALLS_REAL_METHODS)) {
            booleanUtils.when(() -> BooleanUtils.isFalse(false)).thenReturn(true);
            //Act Statement(s)
            boolean result = BooleanUtils.isNotFalse(false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                booleanUtils.verify(() -> BooleanUtils.isFalse(false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isNotTrueWhenIsTrueNotBool}, hash: 908B6044FD330AFE422432B4DC97D692
    @Test()
    void isNotTrueWhenIsTrueNotBool() {
        /* Branches:
         * (!isTrue(bool)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<BooleanUtils> booleanUtils = mockStatic(BooleanUtils.class, CALLS_REAL_METHODS)) {
            booleanUtils.when(() -> BooleanUtils.isTrue(false)).thenReturn(false);
            //Act Statement(s)
            boolean result = BooleanUtils.isNotTrue(false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                booleanUtils.verify(() -> BooleanUtils.isTrue(false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isNotTrueWhenIsTrueBool}, hash: C7D649E5AA0BE44E2AC88C1463DAA669
    @Test()
    void isNotTrueWhenIsTrueBool() {
        /* Branches:
         * (!isTrue(bool)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<BooleanUtils> booleanUtils = mockStatic(BooleanUtils.class, CALLS_REAL_METHODS)) {
            booleanUtils.when(() -> BooleanUtils.isTrue(false)).thenReturn(true);
            //Act Statement(s)
            boolean result = BooleanUtils.isNotTrue(false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                booleanUtils.verify(() -> BooleanUtils.isTrue(false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isTrueWhenBooleanTRUEEqualsBool}, hash: 1F85962B0FD44774E5CABB47E0A85184
    @Test()
    void isTrueWhenBooleanTRUEEqualsBool() {
        /* Branches:
         * (Boolean.TRUE.equals(bool)) : true
         */
        //Act Statement(s)
        boolean result = BooleanUtils.isTrue(true);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isTrueWhenBooleanTRUENotEqualsBool}, hash: 081A797C9147E514A751085DF0983637
    @Test()
    void isTrueWhenBooleanTRUENotEqualsBool() {
        /* Branches:
         * (Boolean.TRUE.equals(bool)) : false
         */
        //Act Statement(s)
        boolean result = BooleanUtils.isTrue(false);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${negateWhenBoolIsNull}, hash: 25E0BE4FA6ED821ADB4DE9B480F5A520
    @Test()
    void negateWhenBoolIsNull() {
        /* Branches:
         * (bool == null) : true
         */
        //Act Statement(s)
        Boolean result = BooleanUtils.negate((Boolean) null);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${negateWhenBoolBooleanValue}, hash: E873D1D9E17B779100390C458C202CB7
    @Test()
    void negateWhenBoolBooleanValue() {
        /* Branches:
         * (bool == null) : false
         * (bool.booleanValue()) : true
         */
        //Act Statement(s)
        Boolean result = BooleanUtils.negate(true);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${negateWhenBoolNotBooleanValue}, hash: 4CD0B3F31F4F622104E4469E8ED414A2
    @Test()
    void negateWhenBoolNotBooleanValue() {
        /* Branches:
         * (bool == null) : false
         * (bool.booleanValue()) : false
         */
        //Act Statement(s)
        Boolean result = BooleanUtils.negate(false);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${oneHotWhenNotElement}, hash: 04666AF9183B60828C2C1304B8E24559
    @Test()
    void oneHotWhenNotElement() {
        /* Branches:
         * (for-each(array)) : true
         * (element) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false };
        //Act Statement(s)
        boolean result = BooleanUtils.oneHot(booleanArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${oneHotWhenNotResult}, hash: 00D0B32078A1DB5083C10764148B0E99
    @Test()
    void oneHotWhenNotResult() {
        /* Branches:
         * (for-each(array)) : true
         * (element) : true
         * (result) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { true };
        //Act Statement(s)
        boolean result = BooleanUtils.oneHot(booleanArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${oneHot1Test}, hash: 5CC022B3AC3D287EFDC3B1133F5BF6B2
    @Test()
    void oneHot1Test() {
        //Arrange Statement(s)
        try (MockedStatic<BooleanUtils> booleanUtils = mockStatic(BooleanUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            booleanUtils.when(() -> BooleanUtils.oneHot(booleanArray)).thenReturn(false);
            Boolean[] booleanArray2 = new Boolean[] {};
            //Act Statement(s)
            Boolean result = BooleanUtils.oneHot(booleanArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                booleanUtils.verify(() -> BooleanUtils.oneHot(booleanArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${orWhenElement}, hash: 55C3781AADDC030AD8C9632D215E553E
    @Test()
    void orWhenElement() {
        /* Branches:
         * (for-each(array)) : true
         * (element) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { true };
        //Act Statement(s)
        boolean result = BooleanUtils.or(booleanArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${orWhenNotElement}, hash: 55F2CE276F55ABE73B77A558C1DC2A8D
    @Test()
    void orWhenNotElement() {
        /* Branches:
         * (for-each(array)) : true
         * (element) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false };
        //Act Statement(s)
        boolean result = BooleanUtils.or(booleanArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${or1WhenOrArrayUtilsToPrimitiveArray}, hash: 1CDA7FAE29147918999D3AB76883B809
    @Disabled()
    @Test()
    void or1WhenOrArrayUtilsToPrimitiveArray() {
        /* Branches:
         * (or(ArrayUtils.toPrimitive(array))) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<BooleanUtils> booleanUtils = mockStatic(BooleanUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            booleanUtils.when(() -> BooleanUtils.or(booleanArray)).thenReturn(true);
            Boolean[] booleanArray2 = new Boolean[] {};
            //Act Statement(s)
            Boolean result = BooleanUtils.or(booleanArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                booleanUtils.verify(() -> BooleanUtils.or(booleanArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${or1WhenOrNotArrayUtilsToPrimitiveArray}, hash: F3553035C100AF0E9DE893A8542F0F6D
    @Disabled()
    @Test()
    void or1WhenOrNotArrayUtilsToPrimitiveArray() {
        /* Branches:
         * (or(ArrayUtils.toPrimitive(array))) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<BooleanUtils> booleanUtils = mockStatic(BooleanUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            booleanUtils.when(() -> BooleanUtils.or(booleanArray)).thenReturn(false);
            Boolean[] booleanArray2 = new Boolean[] {};
            //Act Statement(s)
            Boolean result = BooleanUtils.or(booleanArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                booleanUtils.verify(() -> BooleanUtils.or(booleanArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${primitiveValuesTest}, hash: CB944DA9174E0501BEB9D64F9E3A82C0
    @Test()
    void primitiveValuesTest() {
        //Act Statement(s)
        boolean[] result = BooleanUtils.primitiveValues();
        boolean[] booleanResultArray = new boolean[] { false, true };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${toBooleanWhenBoolBooleanValue}, hash: D9648159B52512C44F83761E81A84625
    @Test()
    void toBooleanWhenBoolBooleanValue() {
        /* Branches:
         * (bool != null) : true
         * (bool.booleanValue()) : true
         */
        //Act Statement(s)
        boolean result = BooleanUtils.toBoolean(true);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${toBooleanWhenBoolNotBooleanValue}, hash: 431716BCCB29B21130880FECB1DE4023
    @Test()
    void toBooleanWhenBoolNotBooleanValue() {
        /* Branches:
         * (bool != null) : true
         * (bool.booleanValue()) : false
         */
        //Act Statement(s)
        boolean result = BooleanUtils.toBoolean(false);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${toBoolean1WhenValueNotEquals0}, hash: 738E79654262555FCBC106CE76DCB655
    @Test()
    void toBoolean1WhenValueNotEquals0() {
        /* Branches:
         * (value != 0) : true
         */
        //Act Statement(s)
        boolean result = BooleanUtils.toBoolean(-1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${toBoolean1WhenValueEquals0}, hash: 4E0C569148A68C01A0A37C5409E6A543
    @Test()
    void toBoolean1WhenValueEquals0() {
        /* Branches:
         * (value != 0) : false
         */
        //Act Statement(s)
        boolean result = BooleanUtils.toBoolean(0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${toBoolean2WhenValueEqualsTrueValue}, hash: 2A9C67D04AFB17C486934658DA1215B7
    @Test()
    void toBoolean2WhenValueEqualsTrueValue() {
        /* Branches:
         * (value == trueValue) : true
         */
        //Act Statement(s)
        boolean result = BooleanUtils.toBoolean(1, 1, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${toBoolean2WhenValueEqualsFalseValue}, hash: 49FE764BD64413097408FF37E693B3C6
    @Test()
    void toBoolean2WhenValueEqualsFalseValue() {
        /* Branches:
         * (value == trueValue) : false
         * (value == falseValue) : true
         */
        //Act Statement(s)
        boolean result = BooleanUtils.toBoolean(1, 2, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${toBoolean2WhenValueNotEqualsFalseValueThrowsIllegalArgumentException}, hash: E931E6EAE97DE7BE3A7A9CD6FFBF4BDD
    @Test()
    void toBoolean2WhenValueNotEqualsFalseValueThrowsIllegalArgumentException() {
        /* Branches:
         * (value == trueValue) : false
         * (value == falseValue) : false
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The Integer did not match either specified value");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            BooleanUtils.toBoolean(1, 2, 2);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${toBoolean3WhenTrueValueIsNull}, hash: 0FA73426D952981A40AD71290C4594F0
    @Test()
    void toBoolean3WhenTrueValueIsNull() {
        /* Branches:
         * (value == null) : true
         * (trueValue == null) : true
         */
        //Act Statement(s)
        boolean result = BooleanUtils.toBoolean((Integer) null, (Integer) null, (Integer) 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${toBoolean3WhenValueEqualsTrueValue}, hash: 19D437B40B85F115A45C7AF57BD572C5
    @Test()
    void toBoolean3WhenValueEqualsTrueValue() {
        /* Branches:
         * (value == null) : false
         * (value.equals(trueValue)) : true
         */
        //Act Statement(s)
        boolean result = BooleanUtils.toBoolean(1, 1, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${toBoolean3WhenFalseValueIsNull}, hash: 8E56B70416ECD5E5B32C0C3639992654
    @Test()
    void toBoolean3WhenFalseValueIsNull() {
        /* Branches:
         * (value == null) : true
         * (trueValue == null) : false
         * (falseValue == null) : true
         */
        //Act Statement(s)
        boolean result = BooleanUtils.toBoolean((Integer) null, (Integer) 0, (Integer) null);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${toBoolean3WhenFalseValueIsNotNullThrowsIllegalArgumentException}, hash: ADBB4530EF4D25156309DB764E19258C
    @Test()
    void toBoolean3WhenFalseValueIsNotNullThrowsIllegalArgumentException() {
        /* Branches:
         * (value == null) : true
         * (trueValue == null) : false
         * (falseValue == null) : false
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The Integer did not match either specified value");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            BooleanUtils.toBoolean((Integer) null, (Integer) 0, (Integer) 0);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${toBoolean3WhenValueEqualsFalseValue}, hash: 0B15345B894CAA85823CFABD62D48956
    @Test()
    void toBoolean3WhenValueEqualsFalseValue() {
        /* Branches:
         * (value == null) : false
         * (value.equals(trueValue)) : false
         * (value.equals(falseValue)) : true
         */
        //Act Statement(s)
        boolean result = BooleanUtils.toBoolean(2, 1, 2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${toBoolean4WhenToBooleanObjectStrEqualsBooleanTRUE}, hash: 800B1507C2DFC6BE511067458D53731D
    @Test()
    void toBoolean4WhenToBooleanObjectStrEqualsBooleanTRUE() {
        /* Branches:
         * (toBooleanObject(str) == Boolean.TRUE) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<BooleanUtils> booleanUtils = mockStatic(BooleanUtils.class, CALLS_REAL_METHODS)) {
            booleanUtils.when(() -> BooleanUtils.toBooleanObject("str1")).thenReturn(true);
            //Act Statement(s)
            boolean result = BooleanUtils.toBoolean("str1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                booleanUtils.verify(() -> BooleanUtils.toBooleanObject("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toBoolean4WhenToBooleanObjectStrNotEqualsBooleanTRUE}, hash: DA56E21138F77885C9FD1C60ECE042B0
    @Test()
    void toBoolean4WhenToBooleanObjectStrNotEqualsBooleanTRUE() {
        /* Branches:
         * (toBooleanObject(str) == Boolean.TRUE) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<BooleanUtils> booleanUtils = mockStatic(BooleanUtils.class, CALLS_REAL_METHODS)) {
            booleanUtils.when(() -> BooleanUtils.toBooleanObject("str1")).thenReturn(false);
            //Act Statement(s)
            boolean result = BooleanUtils.toBoolean("str1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                booleanUtils.verify(() -> BooleanUtils.toBooleanObject("str1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toBoolean5WhenStrEqualsTrueString}, hash: 52FBFA2E0978F974A56C2405768A0A7B
    @Test()
    void toBoolean5WhenStrEqualsTrueString() {
        /* Branches:
         * (str == trueString) : true
         */
        //Act Statement(s)
        boolean result = BooleanUtils.toBoolean("str1", "str1", "falseString1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${toBoolean5WhenStrEqualsFalseString}, hash: 27E3D18FE1FA24672C7648A4A0DC95A4
    @Test()
    void toBoolean5WhenStrEqualsFalseString() {
        /* Branches:
         * (str == trueString) : false
         * (str == falseString) : true
         */
        //Act Statement(s)
        boolean result = BooleanUtils.toBoolean("str1", "trueString1", "str1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${toBoolean5WhenStrIsNotNullAndStrEqualsTrueString}, hash: 0979E9FB01BF3F21DB6DBB969AA500D7
    @Test()
    void toBoolean5WhenStrIsNotNullAndStrEqualsTrueString() {
        /* Branches:
         * (str == trueString) : false
         * (str == falseString) : false
         * (str != null) : true
         * (str.equals(trueString)) : true
         */
        //Act Statement(s)
        boolean result = BooleanUtils.toBoolean("A", "A", "falseString1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${toBoolean5WhenStrNotEqualsTrueStringAndStrEqualsFalseString}, hash: EB13E95E98B54E46E79F768F93A00D12
    @Test()
    void toBoolean5WhenStrNotEqualsTrueStringAndStrEqualsFalseString() {
        /* Branches:
         * (str == trueString) : false
         * (str == falseString) : false
         * (str != null) : true
         * (str.equals(trueString)) : false
         * (str.equals(falseString)) : true
         */
        //Act Statement(s)
        boolean result = BooleanUtils.toBoolean("A", "B", "A");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${toBoolean5WhenStrNotEqualsFalseStringThrowsIllegalArgumentException}, hash: 4F936B756803892974113FDA667F5295
    @Test()
    void toBoolean5WhenStrNotEqualsFalseStringThrowsIllegalArgumentException() {
        /* Branches:
         * (str == trueString) : false
         * (str == falseString) : false
         * (str != null) : true
         * (str.equals(trueString)) : false
         * (str.equals(falseString)) : false
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The String did not match either specified value");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            BooleanUtils.toBoolean("A", "B", "C");
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${toBooleanDefaultIfNullWhenValueIfNull}, hash: 8AF103D3E831981FFCFADD4CCC01367E
    @Test()
    void toBooleanDefaultIfNullWhenValueIfNull() {
        /* Branches:
         * (bool == null) : true
         * (valueIfNull) : true
         */
        //Act Statement(s)
        boolean result = BooleanUtils.toBooleanDefaultIfNull((Boolean) null, true);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${toBooleanDefaultIfNullWhenNotValueIfNull}, hash: 2368342FD227D3A96CE7670D2A47BE20
    @Test()
    void toBooleanDefaultIfNullWhenNotValueIfNull() {
        /* Branches:
         * (bool == null) : true
         * (valueIfNull) : false
         */
        //Act Statement(s)
        boolean result = BooleanUtils.toBooleanDefaultIfNull((Boolean) null, false);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${toBooleanDefaultIfNullWhenBoolBooleanValue}, hash: 82AA5712EA325DF96CA1DFDEF8A4EE7D
    @Test()
    void toBooleanDefaultIfNullWhenBoolBooleanValue() {
        /* Branches:
         * (bool == null) : false
         * (bool.booleanValue()) : true
         */
        //Act Statement(s)
        boolean result = BooleanUtils.toBooleanDefaultIfNull(true, false);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${toBooleanDefaultIfNullWhenBoolNotBooleanValue}, hash: 78E73D667317DEBD4F009D95E041D1D9
    @Test()
    void toBooleanDefaultIfNullWhenBoolNotBooleanValue() {
        /* Branches:
         * (bool == null) : false
         * (bool.booleanValue()) : false
         */
        //Act Statement(s)
        boolean result = BooleanUtils.toBooleanDefaultIfNull(false, false);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${toBooleanObjectWhenValueEquals0}, hash: 46042CD394B1DDCB9E9E98C3292A1087
    @Test()
    void toBooleanObjectWhenValueEquals0() {
        /* Branches:
         * (value == 0) : true
         */
        //Act Statement(s)
        Boolean result = BooleanUtils.toBooleanObject(0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${toBooleanObjectWhenValueNotEquals0}, hash: 167F34C606A2D01E5FA952FCA8903FF8
    @Test()
    void toBooleanObjectWhenValueNotEquals0() {
        /* Branches:
         * (value == 0) : false
         */
        //Act Statement(s)
        Boolean result = BooleanUtils.toBooleanObject(-1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${toBooleanObject1WhenValueEqualsTrueValue}, hash: 3AF6C79CCAC35E6DA91012640C9825F5
    @Test()
    void toBooleanObject1WhenValueEqualsTrueValue() {
        /* Branches:
         * (value == trueValue) : true
         */
        //Act Statement(s)
        Boolean result = BooleanUtils.toBooleanObject(1, 1, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${toBooleanObject1WhenValueEqualsFalseValue}, hash: E30D8AA427480B4E93D8206FE065200F
    @Test()
    void toBooleanObject1WhenValueEqualsFalseValue() {
        /* Branches:
         * (value == trueValue) : false
         * (value == falseValue) : true
         */
        //Act Statement(s)
        Boolean result = BooleanUtils.toBooleanObject(1, 2, 1, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${toBooleanObject1WhenValueEqualsNullValue}, hash: 9DB73CD783CE0E72A34ADC83415B1C6E
    @Test()
    void toBooleanObject1WhenValueEqualsNullValue() {
        /* Branches:
         * (value == trueValue) : false
         * (value == falseValue) : false
         * (value == nullValue) : true
         */
        //Act Statement(s)
        Boolean result = BooleanUtils.toBooleanObject(1, 2, 2, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toBooleanObject1WhenValueNotEqualsNullValueThrowsIllegalArgumentException}, hash: 1AE92074659859F585C0F5769BE59B8E
    @Test()
    void toBooleanObject1WhenValueNotEqualsNullValueThrowsIllegalArgumentException() {
        /* Branches:
         * (value == trueValue) : false
         * (value == falseValue) : false
         * (value == nullValue) : false
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The Integer did not match any specified value");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            BooleanUtils.toBooleanObject(1, 2, 2, 2);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${toBooleanObject2WhenValueIsNull}, hash: 02EB6D929342A1E1D77141A07C3E8C2B
    @Test()
    void toBooleanObject2WhenValueIsNull() {
        /* Branches:
         * (value == null) : true
         */
        //Act Statement(s)
        Boolean result = BooleanUtils.toBooleanObject((Integer) null);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toBooleanObject2WhenValueIntValueEquals0}, hash: 7BC6B1B3A8C41019854ACDF36BDBD2C6
    @Test()
    void toBooleanObject2WhenValueIntValueEquals0() {
        /* Branches:
         * (value == null) : false
         * (value.intValue() == 0) : true
         */
        //Act Statement(s)
        Boolean result = BooleanUtils.toBooleanObject(0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${toBooleanObject2WhenValueIntValueNotEquals0}, hash: 77D92A4E19444A3C5AB044EF05AC46F5
    @Test()
    void toBooleanObject2WhenValueIntValueNotEquals0() {
        /* Branches:
         * (value == null) : false
         * (value.intValue() == 0) : false
         */
        //Act Statement(s)
        Boolean result = BooleanUtils.toBooleanObject(-1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${toBooleanObject3WhenTrueValueIsNull}, hash: F814824D46D41F09A2BF544F64D4B950
    @Test()
    void toBooleanObject3WhenTrueValueIsNull() {
        /* Branches:
         * (value == null) : true
         * (trueValue == null) : true
         */
        //Act Statement(s)
        Boolean result = BooleanUtils.toBooleanObject((Integer) null, (Integer) null, (Integer) 0, (Integer) 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${toBooleanObject3WhenValueEqualsTrueValue}, hash: 5FF348C2C8BDAC042C0A129007B2574F
    @Test()
    void toBooleanObject3WhenValueEqualsTrueValue() {
        /* Branches:
         * (value == null) : false
         * (value.equals(trueValue)) : true
         */
        //Act Statement(s)
        Boolean result = BooleanUtils.toBooleanObject(1, 1, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${toBooleanObject3WhenFalseValueIsNull}, hash: 6A1C4F6EB3F2D7E49748E569B823AABA
    @Test()
    void toBooleanObject3WhenFalseValueIsNull() {
        /* Branches:
         * (value == null) : true
         * (trueValue == null) : false
         * (falseValue == null) : true
         */
        //Act Statement(s)
        Boolean result = BooleanUtils.toBooleanObject((Integer) null, (Integer) 0, (Integer) null, (Integer) 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${toBooleanObject3WhenValueEqualsFalseValue}, hash: 7627B60543A4DD51658637DD7908D639
    @Test()
    void toBooleanObject3WhenValueEqualsFalseValue() {
        /* Branches:
         * (value == null) : false
         * (value.equals(trueValue)) : false
         * (value.equals(falseValue)) : true
         */
        //Act Statement(s)
        Boolean result = BooleanUtils.toBooleanObject(2, 1, 2, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${toBooleanObject3WhenNullValueIsNull}, hash: 562C09A13C5549664A714CF9BC1862A8
    @Test()
    void toBooleanObject3WhenNullValueIsNull() {
        /* Branches:
         * (value == null) : true
         * (trueValue == null) : false
         * (falseValue == null) : false
         * (nullValue == null) : true
         */
        //Act Statement(s)
        Boolean result = BooleanUtils.toBooleanObject((Integer) null, (Integer) 0, (Integer) 0, (Integer) null);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toBooleanObject3WhenValueEqualsNullValue}, hash: 95B24E80F62F4D7510D5AA1450AA35D4
    @Test()
    void toBooleanObject3WhenValueEqualsNullValue() {
        /* Branches:
         * (value == null) : false
         * (value.equals(trueValue)) : false
         * (value.equals(falseValue)) : false
         * (value.equals(nullValue)) : true
         */
        //Act Statement(s)
        Boolean result = BooleanUtils.toBooleanObject(2, 1, 1, 2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toBooleanObject3WhenValueNotEqualsNullValueThrowsIllegalArgumentException}, hash: 0DB58F4BABE4B2A782158B38909FBCE5
    @Test()
    void toBooleanObject3WhenValueNotEqualsNullValueThrowsIllegalArgumentException() {
        /* Branches:
         * (value == null) : false
         * (value.equals(trueValue)) : false
         * (value.equals(falseValue)) : false
         * (value.equals(nullValue)) : false
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The Integer did not match any specified value");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            BooleanUtils.toBooleanObject(1, 2, 2, 2);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${toBooleanObject4WhenStrEqualsTRUE}, hash: 098BF951596106B1608FC47AD49B054B
    @Test()
    void toBooleanObject4WhenStrEqualsTRUE() {
        /* Branches:
         * (str == TRUE) : true
         */
        //Act Statement(s)
        Boolean result = BooleanUtils.toBooleanObject("true");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${toBooleanObject4WhenStrIsNull}, hash: C4DF8D4BE76C7186B9F06AE33806E2F8
    @Test()
    void toBooleanObject4WhenStrIsNull() {
        /* Branches:
         * (str == TRUE) : false
         * (str == null) : true
         */
        //Act Statement(s)
        Boolean result = BooleanUtils.toBooleanObject((String) null);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toBooleanObject4WhenCh1NotEquals_n_AndCh1Equals_N_}, hash: 049761CD1D90C260648064F56FD5F7CE
    @Test()
    void toBooleanObject4WhenCh1NotEquals_n_AndCh1Equals_N_() {
        /* Branches:
         * (str == TRUE) : false
         * (str == null) : false
         * (switch(str.length()) = 2) : true
         * (ch0 == 'o') : false
         * (ch0 == 'O') : true
         * (ch1 == 'n') : false
         * (ch1 == 'N') : true
         */
        //Act Statement(s)
        Boolean result = BooleanUtils.toBooleanObject("ON");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${toBooleanObject4WhenCh0Equals_1_}, hash: AB17E000D8E805D33E7AB228B341BC9A
    @Test()
    void toBooleanObject4WhenCh0Equals_1_() {
        /* Branches:
         * (str == TRUE) : false
         * (str == null) : false
         * (switch(str.length()) = 1) : true
         * (ch0 == 'y') : false
         * (ch0 == 'Y') : false
         * (ch0 == 't') : false
         * (ch0 == 'T') : false
         * (ch0 == '1') : true
         */
        //Act Statement(s)
        Boolean result = BooleanUtils.toBooleanObject("1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${toBooleanObject4WhenCh1NotEquals_o_AndCh1Equals_O_}, hash: A6F4B0A12BD53B56A723AC5F8AA91142
    @Test()
    void toBooleanObject4WhenCh1NotEquals_o_AndCh1Equals_O_() {
        /* Branches:
         * (str == TRUE) : false
         * (str == null) : false
         * (switch(str.length()) = 2) : true
         * (ch0 == 'o') : false
         * (ch0 == 'O') : false
         * (ch0 == 'n') : false
         * (ch0 == 'N') : true
         * (ch1 == 'o') : false
         * (ch1 == 'O') : true
         */
        //Act Statement(s)
        Boolean result = BooleanUtils.toBooleanObject("NO");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${toBooleanObject4WhenCh2NotEquals_s_AndCh2Equals_S_}, hash: E02EC5250BB600FD0EA2C3B810448102
    @Test()
    void toBooleanObject4WhenCh2NotEquals_s_AndCh2Equals_S_() {
        /* Branches:
         * (str == TRUE) : false
         * (str == null) : false
         * (switch(str.length()) = 3) : true
         * (ch0 == 'y') : false
         * (ch0 == 'Y') : true
         * (ch1 == 'e') : false
         * (ch1 == 'E') : true
         * (ch2 == 's') : false
         * (ch2 == 'S') : true
         */
        //Act Statement(s)
        Boolean result = BooleanUtils.toBooleanObject("YES");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${toBooleanObject4WhenCh2NotEquals_s_AndCh2NotEquals_S_AndCh0NotEquals_o_AndCh0NotEquals_O_}, hash: D6193544405449E6DC043156C221BA4E
    @Test()
    void toBooleanObject4WhenCh2NotEquals_s_AndCh2NotEquals_S_AndCh0NotEquals_o_AndCh0NotEquals_O_() {
        /* Branches:
         * (str == TRUE) : false
         * (str == null) : false
         * (switch(str.length()) = 3) : true
         * (ch0 == 'y') : false
         * (ch0 == 'Y') : true
         * (ch1 == 'e') : false
         * (ch1 == 'E') : true
         * (ch2 == 's') : false
         * (ch2 == 'S') : false
         * (ch0 == 'o') : false
         * (ch0 == 'O') : false
         */
        //Act Statement(s)
        Boolean result = BooleanUtils.toBooleanObject("YED");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toBooleanObject4WhenCh1Equals_F_AndCh2NotEquals_f_AndCh2Equals_F_}, hash: F75BFFDE4DA79C83561F1840328E871D
    @Test()
    void toBooleanObject4WhenCh1Equals_F_AndCh2NotEquals_f_AndCh2Equals_F_() {
        /* Branches:
         * (str == TRUE) : false
         * (str == null) : false
         * (switch(str.length()) = 3) : true
         * (ch0 == 'y') : false
         * (ch0 == 'Y') : false
         * (ch0 == 'o') : false
         * (ch0 == 'O') : true
         * (ch1 == 'f') : false
         * (ch1 == 'F') : true
         * (ch2 == 'f') : false
         * (ch2 == 'F') : true
         */
        //Act Statement(s)
        Boolean result = BooleanUtils.toBooleanObject("OFF");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${toBooleanObject4WhenCh0Equals_0_}, hash: E5440D233D5CBF3EDDC5E669273117EE
    @Test()
    void toBooleanObject4WhenCh0Equals_0_() {
        /* Branches:
         * (str == TRUE) : false
         * (str == null) : false
         * (switch(str.length()) = 1) : true
         * (ch0 == 'y') : false
         * (ch0 == 'Y') : false
         * (ch0 == 't') : false
         * (ch0 == 'T') : false
         * (ch0 == '1') : false
         * (ch0 == 'n') : false
         * (ch0 == 'N') : false
         * (ch0 == 'f') : false
         * (ch0 == 'F') : false
         * (ch0 == '0') : true
         */
        //Act Statement(s)
        Boolean result = BooleanUtils.toBooleanObject("0");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${toBooleanObject5WhenTrueStringIsNull}, hash: 0955BD1C2919322F8D9A6F8745502C46
    @Test()
    void toBooleanObject5WhenTrueStringIsNull() {
        /* Branches:
         * (str == null) : true
         * (trueString == null) : true
         */
        //Act Statement(s)
        Boolean result = BooleanUtils.toBooleanObject((String) null, (String) null, "falseString1", "nullString1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${toBooleanObject5WhenStrEqualsTrueString}, hash: FC645C408E1A9ECF7C62786F280301AD
    @Test()
    void toBooleanObject5WhenStrEqualsTrueString() {
        /* Branches:
         * (str == null) : false
         * (str.equals(trueString)) : true
         */
        //Act Statement(s)
        Boolean result = BooleanUtils.toBooleanObject("A", "A", "falseString1", "nullString1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${toBooleanObject5WhenFalseStringIsNull}, hash: 522CD8465B7A86635D6FD9A2640ECF83
    @Test()
    void toBooleanObject5WhenFalseStringIsNull() {
        /* Branches:
         * (str == null) : true
         * (trueString == null) : false
         * (falseString == null) : true
         */
        //Act Statement(s)
        Boolean result = BooleanUtils.toBooleanObject((String) null, "trueString1", (String) null, "nullString1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${toBooleanObject5WhenStrEqualsFalseString}, hash: ED6F50ABB09936B3F1ED58564F758C7B
    @Test()
    void toBooleanObject5WhenStrEqualsFalseString() {
        /* Branches:
         * (str == null) : false
         * (str.equals(trueString)) : false
         * (str.equals(falseString)) : true
         */
        //Act Statement(s)
        Boolean result = BooleanUtils.toBooleanObject("A", "B", "A", "nullString1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${toBooleanObject5WhenNullStringIsNull}, hash: 46F466BB8572FCADD14EBD92EDDD9E87
    @Test()
    void toBooleanObject5WhenNullStringIsNull() {
        /* Branches:
         * (str == null) : true
         * (trueString == null) : false
         * (falseString == null) : false
         * (nullString == null) : true
         */
        //Act Statement(s)
        Boolean result = BooleanUtils.toBooleanObject((String) null, "trueString1", "falseString1", (String) null);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toBooleanObject5WhenStrEqualsNullString}, hash: 6E87D1169CBD6ABC0C819A463EC97934
    @Test()
    void toBooleanObject5WhenStrEqualsNullString() {
        /* Branches:
         * (str == null) : false
         * (str.equals(trueString)) : false
         * (str.equals(falseString)) : false
         * (str.equals(nullString)) : true
         */
        //Act Statement(s)
        Boolean result = BooleanUtils.toBooleanObject("A", "B", "C", "A");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toBooleanObject5WhenStrNotEqualsNullStringThrowsIllegalArgumentException}, hash: 38C6F42578D43ED66F46A5111000A79F
    @Test()
    void toBooleanObject5WhenStrNotEqualsNullStringThrowsIllegalArgumentException() {
        /* Branches:
         * (str == null) : false
         * (str.equals(trueString)) : false
         * (str.equals(falseString)) : false
         * (str.equals(nullString)) : false
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The String did not match any specified value");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            BooleanUtils.toBooleanObject("A", "B", "C", "D");
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${toIntegerWhenBool}, hash: 09ED6CD5C3122982FCBB9F141A367FC0
    @Test()
    void toIntegerWhenBool() {
        /* Branches:
         * (bool) : true
         */
        //Act Statement(s)
        int result = BooleanUtils.toInteger(true);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1)));
    }

    //BaseRock generated method id: ${toIntegerWhenNotBool}, hash: B5D38A61B311BE24D9D85BC84C7E69E2
    @Test()
    void toIntegerWhenNotBool() {
        /* Branches:
         * (bool) : false
         */
        //Act Statement(s)
        int result = BooleanUtils.toInteger(false);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${toInteger1WhenBool}, hash: D9A3C613984D4BC2DAC62C0580E20946
    @Test()
    void toInteger1WhenBool() {
        /* Branches:
         * (bool) : true
         */
        //Act Statement(s)
        int result = BooleanUtils.toInteger(true, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${toInteger1WhenNotBool}, hash: AAA87F7B2885D90D358A9CA3B6D1CEC1
    @Test()
    void toInteger1WhenNotBool() {
        /* Branches:
         * (bool) : false
         */
        //Act Statement(s)
        int result = BooleanUtils.toInteger(false, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${toInteger2WhenBoolIsNull}, hash: D55FD6CDA83A60D9B1ED6A4639B06588
    @Test()
    void toInteger2WhenBoolIsNull() {
        /* Branches:
         * (bool == null) : true
         */
        //Act Statement(s)
        int result = BooleanUtils.toInteger((Boolean) null, 0, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${toInteger2WhenBoolBooleanValue}, hash: BCB7001BCAB0E217668AFCD09135E917
    @Test()
    void toInteger2WhenBoolBooleanValue() {
        /* Branches:
         * (bool == null) : false
         * (bool.booleanValue()) : true
         */
        //Act Statement(s)
        int result = BooleanUtils.toInteger(true, 0, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${toInteger2WhenBoolNotBooleanValue}, hash: 4C56F00262C17C76F4B2401C88C0EFE9
    @Test()
    void toInteger2WhenBoolNotBooleanValue() {
        /* Branches:
         * (bool == null) : false
         * (bool.booleanValue()) : false
         */
        //Act Statement(s)
        int result = BooleanUtils.toInteger(false, 0, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${toIntegerObjectWhenBool}, hash: 252EF06EA5BA43AA8BB04ECAD6F37620
    @Test()
    void toIntegerObjectWhenBool() {
        /* Branches:
         * (bool) : true
         */
        //Act Statement(s)
        Integer result = BooleanUtils.toIntegerObject(true);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1)));
    }

    //BaseRock generated method id: ${toIntegerObjectWhenNotBool}, hash: C816DEF4B578D7EB81ED77BECF657058
    @Test()
    void toIntegerObjectWhenNotBool() {
        /* Branches:
         * (bool) : false
         */
        //Act Statement(s)
        Integer result = BooleanUtils.toIntegerObject(false);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${toIntegerObject1WhenBool}, hash: FBB6C68ACEED4114E6CEFADA5C085B5F
    @Test()
    void toIntegerObject1WhenBool() {
        /* Branches:
         * (bool) : true
         */
        //Act Statement(s)
        Integer result = BooleanUtils.toIntegerObject(true, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${toIntegerObject1WhenNotBool}, hash: B7C10EFDA9A222114145300DE42F7DEF
    @Test()
    void toIntegerObject1WhenNotBool() {
        /* Branches:
         * (bool) : false
         */
        //Act Statement(s)
        Integer result = BooleanUtils.toIntegerObject(false, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${toIntegerObject2WhenBoolIsNull}, hash: 98254C040DF9B9302196EB7EF656A9EF
    @Test()
    void toIntegerObject2WhenBoolIsNull() {
        /* Branches:
         * (bool == null) : true
         */
        //Act Statement(s)
        Integer result = BooleanUtils.toIntegerObject((Boolean) null);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toIntegerObject2WhenBoolBooleanValue}, hash: 8A5284C235C62D26CE5E24EA9D39E61B
    @Test()
    void toIntegerObject2WhenBoolBooleanValue() {
        /* Branches:
         * (bool == null) : false
         * (bool.booleanValue()) : true
         */
        //Act Statement(s)
        Integer result = BooleanUtils.toIntegerObject(true);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1)));
    }

    //BaseRock generated method id: ${toIntegerObject2WhenBoolNotBooleanValue}, hash: 17565605F271F4D2EAC76E983326404D
    @Test()
    void toIntegerObject2WhenBoolNotBooleanValue() {
        /* Branches:
         * (bool == null) : false
         * (bool.booleanValue()) : false
         */
        //Act Statement(s)
        Integer result = BooleanUtils.toIntegerObject(false);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${toIntegerObject3WhenBoolIsNull}, hash: 722FE71BE246BFA3CBA29668B34BD308
    @Test()
    void toIntegerObject3WhenBoolIsNull() {
        /* Branches:
         * (bool == null) : true
         */
        //Act Statement(s)
        Integer result = BooleanUtils.toIntegerObject((Boolean) null, 0, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${toIntegerObject3WhenBoolBooleanValue}, hash: E8EB8236DCF1D49777D8E932B0D5554C
    @Test()
    void toIntegerObject3WhenBoolBooleanValue() {
        /* Branches:
         * (bool == null) : false
         * (bool.booleanValue()) : true
         */
        //Act Statement(s)
        Integer result = BooleanUtils.toIntegerObject(true, 0, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${toIntegerObject3WhenBoolNotBooleanValue}, hash: 09DEF20DCB4A4B804A6B9C0A33DF21A9
    @Test()
    void toIntegerObject3WhenBoolNotBooleanValue() {
        /* Branches:
         * (bool == null) : false
         * (bool.booleanValue()) : false
         */
        //Act Statement(s)
        Integer result = BooleanUtils.toIntegerObject(false, 0, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${toStringWhenBool}, hash: 2215167BE29EAFD9B8978B05791D6B1C
    @Test()
    void toStringWhenBool() {
        /* Branches:
         * (bool) : true
         */
        //Act Statement(s)
        String result = BooleanUtils.toString(true, "trueString1", "falseString1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("trueString1")));
    }

    //BaseRock generated method id: ${toStringWhenNotBool}, hash: 0202F15D67C9F6FF36EE713B9DDC0408
    @Test()
    void toStringWhenNotBool() {
        /* Branches:
         * (bool) : false
         */
        //Act Statement(s)
        String result = BooleanUtils.toString(false, "trueString1", "falseString1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("falseString1")));
    }

    //BaseRock generated method id: ${toString1WhenBoolIsNull}, hash: 151A59437DD388CBB5DC0FCCA17D582B
    @Test()
    void toString1WhenBoolIsNull() {
        /* Branches:
         * (bool == null) : true
         */
        //Act Statement(s)
        String result = BooleanUtils.toString((Boolean) null, "trueString1", "falseString1", "nullString1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("nullString1")));
    }

    //BaseRock generated method id: ${toString1WhenBoolBooleanValue}, hash: CCFF7D0D5AE6BCFCEBD5DD152316D5D5
    @Test()
    void toString1WhenBoolBooleanValue() {
        /* Branches:
         * (bool == null) : false
         * (bool.booleanValue()) : true
         */
        //Act Statement(s)
        String result = BooleanUtils.toString(true, "trueString1", "falseString1", "nullString1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("trueString1")));
    }

    //BaseRock generated method id: ${toString1WhenBoolNotBooleanValue}, hash: 8A7D560574E224244C6048A64BF940A6
    @Test()
    void toString1WhenBoolNotBooleanValue() {
        /* Branches:
         * (bool == null) : false
         * (bool.booleanValue()) : false
         */
        //Act Statement(s)
        String result = BooleanUtils.toString(false, "trueString1", "falseString1", "nullString1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("falseString1")));
    }

    //BaseRock generated method id: ${toStringOnOffTest}, hash: 9C5EA9FAE4A10745BD2074FDE42704DA
    @Test()
    void toStringOnOffTest() {
        //Arrange Statement(s)
        try (MockedStatic<BooleanUtils> booleanUtils = mockStatic(BooleanUtils.class, CALLS_REAL_METHODS)) {
            booleanUtils.when(() -> BooleanUtils.toString(false, "on", "off")).thenReturn("return_of_toString1");
            //Act Statement(s)
            String result = BooleanUtils.toStringOnOff(false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_toString1"));
                booleanUtils.verify(() -> BooleanUtils.toString(false, "on", "off"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toStringOnOff1Test}, hash: 3D124244F072478E3A43F620CE5EC797
    @Disabled()
    @Test()
    void toStringOnOff1Test() {
        //Arrange Statement(s)
        try (MockedStatic<BooleanUtils> booleanUtils = mockStatic(BooleanUtils.class, CALLS_REAL_METHODS)) {
            booleanUtils.when(() -> BooleanUtils.toString(false, "on", "off", (String) null)).thenReturn("return_of_toString1");
            //Act Statement(s)
            String result = BooleanUtils.toStringOnOff(false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_toString1"));
                booleanUtils.verify(() -> BooleanUtils.toString(false, "on", "off", (String) null), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toStringTrueFalseTest}, hash: 5717D37A5EE711A4A2E888807533EB81
    @Test()
    void toStringTrueFalseTest() {
        //Arrange Statement(s)
        try (MockedStatic<BooleanUtils> booleanUtils = mockStatic(BooleanUtils.class, CALLS_REAL_METHODS)) {
            booleanUtils.when(() -> BooleanUtils.toString(false, "true", "false")).thenReturn("return_of_toString1");
            //Act Statement(s)
            String result = BooleanUtils.toStringTrueFalse(false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_toString1"));
                booleanUtils.verify(() -> BooleanUtils.toString(false, "true", "false"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toStringTrueFalse1Test}, hash: A34D2A86EBD8F4C099C66A698BBC699D
    @Disabled()
    @Test()
    void toStringTrueFalse1Test() {
        //Arrange Statement(s)
        try (MockedStatic<BooleanUtils> booleanUtils = mockStatic(BooleanUtils.class, CALLS_REAL_METHODS)) {
            booleanUtils.when(() -> BooleanUtils.toString(false, "true", "false", (String) null)).thenReturn("return_of_toString1");
            //Act Statement(s)
            String result = BooleanUtils.toStringTrueFalse(false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_toString1"));
                booleanUtils.verify(() -> BooleanUtils.toString(false, "true", "false", (String) null), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toStringYesNoTest}, hash: C7CE93CDC02CA4C4A0E37E5C730257F3
    @Test()
    void toStringYesNoTest() {
        //Arrange Statement(s)
        try (MockedStatic<BooleanUtils> booleanUtils = mockStatic(BooleanUtils.class, CALLS_REAL_METHODS)) {
            booleanUtils.when(() -> BooleanUtils.toString(false, "yes", "no")).thenReturn("return_of_toString1");
            //Act Statement(s)
            String result = BooleanUtils.toStringYesNo(false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_toString1"));
                booleanUtils.verify(() -> BooleanUtils.toString(false, "yes", "no"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toStringYesNo1Test}, hash: F8C0B91FF443365174E8553564BE9E61
    @Disabled()
    @Test()
    void toStringYesNo1Test() {
        //Arrange Statement(s)
        try (MockedStatic<BooleanUtils> booleanUtils = mockStatic(BooleanUtils.class, CALLS_REAL_METHODS)) {
            booleanUtils.when(() -> BooleanUtils.toString(false, "yes", "no", (String) null)).thenReturn("return_of_toString1");
            //Act Statement(s)
            String result = BooleanUtils.toStringYesNo(false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_toString1"));
                booleanUtils.verify(() -> BooleanUtils.toString(false, "yes", "no", (String) null), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${valuesTest}, hash: B8347E91DB4D77922CE3300A5AC055CA
    @Test()
    void valuesTest() {
        //Act Statement(s)
        List<Boolean> result = BooleanUtils.values();
        List<Boolean> booleanResultList = new ArrayList<>(Arrays.asList(false, true));
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result.size(), equalTo(2));
            assertThat(result, containsInRelativeOrder(booleanResultList.toArray()));
        });
    }

    //BaseRock generated method id: ${xorWhenResult}, hash: 8F21DD598F8126FB1D0D9868D0E87B37
    @Disabled()
    @Test()
    void xorWhenResult() {
        /* Branches:
         * (for-each(array)) : true
         * (result) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false };
        //Act Statement(s)
        boolean result = BooleanUtils.xor(booleanArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${xorWhenNotResult}, hash: 99F0EF57366668A636D8E28E5449D4DD
    @Test()
    void xorWhenNotResult() {
        /* Branches:
         * (for-each(array)) : true
         * (result) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false };
        //Act Statement(s)
        boolean result = BooleanUtils.xor(booleanArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${xor1WhenXorArrayUtilsToPrimitiveArray}, hash: F4E219CF660EB7867A6902F9FF0BA80D
    @Disabled()
    @Test()
    void xor1WhenXorArrayUtilsToPrimitiveArray() {
        /* Branches:
         * (xor(ArrayUtils.toPrimitive(array))) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<BooleanUtils> booleanUtils = mockStatic(BooleanUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            booleanUtils.when(() -> BooleanUtils.xor(booleanArray)).thenReturn(true);
            Boolean[] booleanArray2 = new Boolean[] {};
            //Act Statement(s)
            Boolean result = BooleanUtils.xor(booleanArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                booleanUtils.verify(() -> BooleanUtils.xor(booleanArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${xor1WhenXorNotArrayUtilsToPrimitiveArray}, hash: F603C74E7EC11FAD29BEDA6E7BFF057B
    @Disabled()
    @Test()
    void xor1WhenXorNotArrayUtilsToPrimitiveArray() {
        /* Branches:
         * (xor(ArrayUtils.toPrimitive(array))) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<BooleanUtils> booleanUtils = mockStatic(BooleanUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            booleanUtils.when(() -> BooleanUtils.xor(booleanArray)).thenReturn(false);
            Boolean[] booleanArray2 = new Boolean[] {};
            //Act Statement(s)
            Boolean result = BooleanUtils.xor(booleanArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                booleanUtils.verify(() -> BooleanUtils.xor(booleanArray), atLeast(1));
            });
        }
    }
}
