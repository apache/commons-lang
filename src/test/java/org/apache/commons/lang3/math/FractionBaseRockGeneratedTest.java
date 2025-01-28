package org.apache.commons.lang3.math;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.doReturn;
import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.any;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mockStatic;
import static org.hamcrest.Matchers.closeTo;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class FractionBaseRockGeneratedTest {

    private final Fraction fractionMock = mock(Fraction.class);

    //BaseRock generated method id: ${getFractionWhenValueNotLessThan0AndValueGreaterThanIntegerMAX_VALUEThrowsArithmeticException}, hash: 8127BBD011FEB01915F23EF3F6BC741C
    @Test()
    void getFractionWhenValueNotLessThan0AndValueGreaterThanIntegerMAX_VALUEThrowsArithmeticException() {
        /* Branches:
         * (value < 0) : false
         * (value > Integer.MAX_VALUE) : true
         */
         //Arrange Statement(s)
        ArithmeticException arithmeticException = new ArithmeticException("The value must not be greater than Integer.MAX_VALUE or NaN");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            Fraction.getFraction(Double.parseDouble("2.147483648E9"));
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getFractionWhenILessThan25AndINotEquals25}, hash: ABC1FF65A47F88B0E6A8567A1D2B4FBD
    @Test()
    void getFractionWhenILessThan25AndINotEquals25() {
        /* Branches:
         * (value < 0) : true
         * (value > Integer.MAX_VALUE) : false
         * (Double.isNaN(value)) : false
         * (delta1 > delta2) : true
         * (denom2 <= 10000) : true
         * (denom2 > 0) : true
         * (i < 25) : true
         * (i == 25) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<Fraction> fraction = mockStatic(Fraction.class, CALLS_REAL_METHODS)) {
            Fraction fraction2 = Fraction.getFraction(Double.parseDouble("0.0"));
            fraction.when(() -> Fraction.getReducedFraction(0, 0)).thenReturn(fraction2);
            //Act Statement(s)
            Fraction result = Fraction.getFraction(Double.parseDouble("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(fraction2));
                fraction.verify(() -> Fraction.getReducedFraction(0, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getFraction1WhenDenominatorEquals0ThrowsArithmeticException}, hash: FC5039BF2DC58520CC25C92F1F8713FB
    @Test()
    void getFraction1WhenDenominatorEquals0ThrowsArithmeticException() {
        /* Branches:
         * (denominator == 0) : true
         */
         //Arrange Statement(s)
        ArithmeticException arithmeticException = new ArithmeticException("The denominator must not be zero");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            Fraction.getFraction(0, 0);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getFraction1WhenDenominatorEqualsIntegerMIN_VALUEThrowsArithmeticException}, hash: F54E2C5ED981C02DF2660E41DCABAB59
    @Test()
    void getFraction1WhenDenominatorEqualsIntegerMIN_VALUEThrowsArithmeticException() {
        /* Branches:
         * (denominator == 0) : false
         * (denominator < 0) : true
         * (numerator == Integer.MIN_VALUE) : false
         * (denominator == Integer.MIN_VALUE) : true
         */
         //Arrange Statement(s)
        ArithmeticException arithmeticException = new ArithmeticException("overflow: can't negate");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            Fraction.getFraction(0, -2147483648);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getFraction1WhenDenominatorNotEqualsIntegerMIN_VALUE}, hash: DA6CA686FA6FBA630B0685BEFA59D189
    @Test()
    void getFraction1WhenDenominatorNotEqualsIntegerMIN_VALUE() {
        /* Branches:
         * (denominator == 0) : false
         * (denominator < 0) : true
         * (numerator == Integer.MIN_VALUE) : false
         * (denominator == Integer.MIN_VALUE) : false
         */
         
        //Act Statement(s)
        Fraction result = Fraction.getFraction(0, -1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getFraction2WhenDenominatorEquals0ThrowsArithmeticException}, hash: 4EA5C67D75D864E39A35693B49F6A83F
    @Test()
    void getFraction2WhenDenominatorEquals0ThrowsArithmeticException() {
        /* Branches:
         * (denominator == 0) : true
         */
         //Arrange Statement(s)
        ArithmeticException arithmeticException = new ArithmeticException("The denominator must not be zero");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            Fraction.getFraction(0, 0, 0);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getFraction2WhenDenominatorLessThan0ThrowsArithmeticException}, hash: F66054965465A8483557A40F4E6A65A6
    @Test()
    void getFraction2WhenDenominatorLessThan0ThrowsArithmeticException() {
        /* Branches:
         * (denominator == 0) : false
         * (denominator < 0) : true
         */
         //Arrange Statement(s)
        ArithmeticException arithmeticException = new ArithmeticException("The denominator must not be negative");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            Fraction.getFraction(0, 0, -1);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getFraction2WhenNumeratorLessThan0ThrowsArithmeticException}, hash: 092ED42B3D4A61367C77BFED358FD437
    @Test()
    void getFraction2WhenNumeratorLessThan0ThrowsArithmeticException() {
        /* Branches:
         * (denominator == 0) : false
         * (denominator < 0) : false
         * (numerator < 0) : true
         */
         //Arrange Statement(s)
        ArithmeticException arithmeticException = new ArithmeticException("The numerator must not be negative");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            Fraction.getFraction(0, -1, 1);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getFraction2WhenNumeratorValueNotGreaterThanIntegerMAX_VALUE}, hash: 303E46F3787D6112F48F421CB8C0F56C
    @Test()
    void getFraction2WhenNumeratorValueNotGreaterThanIntegerMAX_VALUE() {
        /* Branches:
         * (denominator == 0) : false
         * (denominator < 0) : false
         * (numerator < 0) : false
         * (whole < 0) : true
         * (numeratorValue < Integer.MIN_VALUE) : false
         * (numeratorValue > Integer.MAX_VALUE) : false
         */
         
        //Act Statement(s)
        Fraction result = Fraction.getFraction(-1, 0, 1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getFraction2WhenNumeratorValueNotLessThanIntegerMIN_VALUEAndNumeratorValueGreaterThanIntegerMAThrowsArithmeticException}, hash: EEF985D42268DE1CDCA00B57D0AD6C4E
    @Test()
    void getFraction2WhenNumeratorValueNotLessThanIntegerMIN_VALUEAndNumeratorValueGreaterThanIntegerMAThrowsArithmeticException() {
        /* Branches:
         * (denominator == 0) : false
         * (denominator < 0) : false
         * (numerator < 0) : false
         * (whole < 0) : false
         * (numeratorValue < Integer.MIN_VALUE) : false
         * (numeratorValue > Integer.MAX_VALUE) : true
         */
         //Arrange Statement(s)
        ArithmeticException arithmeticException = new ArithmeticException("Numerator too large to represent as an Integer.");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            Fraction.getFraction(1073741824, 1, 2);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getFraction3WhenPosGreaterThanOrEqualsTo0}, hash: 6DE3EFD0AA26B7D7411CA4856DE459C4
    @Test()
    void getFraction3WhenPosGreaterThanOrEqualsTo0() {
        /* Branches:
         * (pos >= 0) : true
         */
         //Arrange Statement(s)
        try (MockedStatic<Fraction> fraction = mockStatic(Fraction.class, CALLS_REAL_METHODS)) {
            Fraction fraction2 = Fraction.getFraction(Double.parseDouble("0.0"));
            fraction.when(() -> Fraction.getFraction(Double.parseDouble("1.0"))).thenReturn(fraction2);
            //Act Statement(s)
            Fraction result = Fraction.getFraction("1.0");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(fraction2));
                fraction.verify(() -> Fraction.getFraction(Double.parseDouble("1.0")), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getFraction3WhenPosLessThan0ThrowsNumberFormatException}, hash: AB28A4E98A11BCB568D082341A5B7F61
    @Test()
    void getFraction3WhenPosLessThan0ThrowsNumberFormatException() {
        /* Branches:
         * (pos >= 0) : false
         * (pos > 0) : true
         * (pos < 0) : true
         */
         //Arrange Statement(s)
        NumberFormatException numberFormatException = new NumberFormatException("The fraction could not be parsed as the format X Y/Z");
        //Act Statement(s)
        final NumberFormatException result = assertThrows(NumberFormatException.class, () -> {
            Fraction.getFraction("0  ");
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(numberFormatException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getFraction3WhenPosNotLessThan0}, hash: B73154341B16337E20411827830D04E3
    @Test()
    void getFraction3WhenPosNotLessThan0() {
        /* Branches:
         * (pos >= 0) : false
         * (pos > 0) : true
         * (pos < 0) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<Fraction> fraction = mockStatic(Fraction.class, CALLS_REAL_METHODS)) {
            Fraction fraction2 = Fraction.getFraction(Double.parseDouble("0.0"));
            fraction.when(() -> Fraction.getFraction(2, 1, 0)).thenReturn(fraction2);
            //Act Statement(s)
            Fraction result = Fraction.getFraction("2 1/0");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(fraction2));
                fraction.verify(() -> Fraction.getFraction(2, 1, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getFraction3WhenPosLessThan0}, hash: EBAC6B4A1AFE8DD5D4BECA39CB39AAE5
    @Test()
    void getFraction3WhenPosLessThan0() {
        /* Branches:
         * (pos >= 0) : false
         * (pos > 0) : false
         * (pos < 0) : true
         */
         //Arrange Statement(s)
        try (MockedStatic<Fraction> fraction = mockStatic(Fraction.class, CALLS_REAL_METHODS)) {
            Fraction fraction2 = Fraction.getFraction(Double.parseDouble("0.0"));
            fraction.when(() -> Fraction.getFraction(0, 1)).thenReturn(fraction2);
            //Act Statement(s)
            Fraction result = Fraction.getFraction("0");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(fraction2));
                fraction.verify(() -> Fraction.getFraction(0, 1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getFraction3WhenPosNotGreaterThan0AndPosNotLessThan0}, hash: BB754C39C078CDE447F6AA9DF0A9C1DF
    @Test()
    void getFraction3WhenPosNotGreaterThan0AndPosNotLessThan0() {
        /* Branches:
         * (pos >= 0) : false
         * (pos > 0) : false
         * (pos < 0) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<Fraction> fraction = mockStatic(Fraction.class, CALLS_REAL_METHODS)) {
            Fraction fraction2 = Fraction.getFraction(Double.parseDouble("0.0"));
            fraction.when(() -> Fraction.getFraction(0, 0)).thenReturn(fraction2);
            //Act Statement(s)
            Fraction result = Fraction.getFraction("0/0");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(fraction2));
                fraction.verify(() -> Fraction.getFraction(0, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getReducedFractionWhenDenominatorEquals0ThrowsArithmeticException}, hash: F872C66F92E599739FA6638E4190DFAF
    @Test()
    void getReducedFractionWhenDenominatorEquals0ThrowsArithmeticException() {
        /* Branches:
         * (denominator == 0) : true
         */
         //Arrange Statement(s)
        ArithmeticException arithmeticException = new ArithmeticException("The denominator must not be zero");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            Fraction.getReducedFraction(0, 0);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getReducedFractionWhenNumeratorEquals0}, hash: 3BEE3ED157DF663F0097389F317869DD
    @Test()
    void getReducedFractionWhenNumeratorEquals0() {
        /* Branches:
         * (denominator == 0) : false
         * (numerator == 0) : true
         */
         
        //Act Statement(s)
        Fraction result = Fraction.getReducedFraction(0, 1);
        Fraction fraction = Fraction.ZERO;
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(fraction)));
    }

    //BaseRock generated method id: ${getReducedFractionWhenVEqualsIntegerMIN_VALUEThrowsArithmeticException}, hash: 30DB3009A10189E51217D6EC7478B077
    @Test()
    void getReducedFractionWhenVEqualsIntegerMIN_VALUEThrowsArithmeticException() {
        /* Branches:
         * (denominator == 0) : false
         * (numerator == 0) : false
         * (denominator == Integer.MIN_VALUE) : true
         * ((numerator & 1) == 0) : true
         * (denominator < 0) : true
         * (numerator == Integer.MIN_VALUE) : false
         * (denominator == Integer.MIN_VALUE) : false
         * (u == 0) : false  #  inside greatestCommonDivisor method
         * (v == 0) : true  #  inside greatestCommonDivisor method
         * (u == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (v == Integer.MIN_VALUE) : true  #  inside greatestCommonDivisor method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        ArithmeticException arithmeticException = new ArithmeticException("overflow: gcd is 2^31");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            Fraction.getReducedFraction(0, 0);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getReducedFractionWhenVNotEqualsIntegerMIN_VALUE}, hash: FB4B60E7C217DD2ABAF4F03865A9AFB4
    @Test()
    void getReducedFractionWhenVNotEqualsIntegerMIN_VALUE() {
        /* Branches:
         * (denominator == 0) : false
         * (numerator == 0) : false
         * (denominator == Integer.MIN_VALUE) : true
         * ((numerator & 1) == 0) : true
         * (denominator < 0) : true
         * (numerator == Integer.MIN_VALUE) : false
         * (denominator == Integer.MIN_VALUE) : false
         * (u == 0) : false  #  inside greatestCommonDivisor method
         * (v == 0) : true  #  inside greatestCommonDivisor method
         * (u == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (v == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Fraction result = Fraction.getReducedFraction(0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getReducedFractionWhenMathAbsVEquals1}, hash: D5CF8385CCEFCAC542EA0C5DDFE8E7E2
    @Test()
    void getReducedFractionWhenMathAbsVEquals1() {
        /* Branches:
         * (denominator == 0) : false
         * (numerator == 0) : false
         * (denominator == Integer.MIN_VALUE) : true
         * ((numerator & 1) == 0) : true
         * (denominator < 0) : true
         * (numerator == Integer.MIN_VALUE) : false
         * (denominator == Integer.MIN_VALUE) : false
         * (u == 0) : false  #  inside greatestCommonDivisor method
         * (v == 0) : false  #  inside greatestCommonDivisor method
         * (Math.abs(u) == 1) : false  #  inside greatestCommonDivisor method
         * (Math.abs(v) == 1) : true  #  inside greatestCommonDivisor method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Fraction result = Fraction.getReducedFraction(0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getReducedFractionWhenKNotEquals31AndUAnd1Equals1AndTAnd1Equals0AndTNotGreaterThan0AndTEquals0}, hash: 1B98C70D1CE3AE70500EA4821B49BA9C
    @Test()
    void getReducedFractionWhenKNotEquals31AndUAnd1Equals1AndTAnd1Equals0AndTNotGreaterThan0AndTEquals0() {
        /* Branches:
         * (denominator == 0) : false
         * (numerator == 0) : false
         * (denominator == Integer.MIN_VALUE) : true
         * ((numerator & 1) == 0) : true
         * (denominator < 0) : true
         * (numerator == Integer.MIN_VALUE) : false
         * (denominator == Integer.MIN_VALUE) : false
         * (u == 0) : false  #  inside greatestCommonDivisor method
         * (v == 0) : false  #  inside greatestCommonDivisor method
         * (Math.abs(u) == 1) : false  #  inside greatestCommonDivisor method
         * (Math.abs(v) == 1) : false  #  inside greatestCommonDivisor method
         * (u > 0) : true  #  inside greatestCommonDivisor method
         * (v > 0) : true  #  inside greatestCommonDivisor method
         * ((u & 1) == 0) : true  #  inside greatestCommonDivisor method
         * ((v & 1) == 0) : true  #  inside greatestCommonDivisor method
         * (k < 31) : true  #  inside greatestCommonDivisor method
         * (k == 31) : false  #  inside greatestCommonDivisor method
         * ((u & 1) == 1) : true  #  inside greatestCommonDivisor method
         * ((t & 1) == 0) : true  #  inside greatestCommonDivisor method
         * (t > 0) : false  #  inside greatestCommonDivisor method
         * (t != 0) : false  #  inside greatestCommonDivisor method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Fraction result = Fraction.getReducedFraction(-2147483647, -2147483648);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getReducedFractionWhenKNotEquals31AndUAnd1NotEquals1AndTAnd1Equals0AndTGreaterThan0AndTEquals0}, hash: 558D5435894E1A0DCAE65A6EC432DC70
    @Test()
    void getReducedFractionWhenKNotEquals31AndUAnd1NotEquals1AndTAnd1Equals0AndTGreaterThan0AndTEquals0() {
        /* Branches:
         * (denominator == 0) : false
         * (numerator == 0) : false
         * (denominator == Integer.MIN_VALUE) : true
         * ((numerator & 1) == 0) : true
         * (denominator < 0) : true
         * (numerator == Integer.MIN_VALUE) : false
         * (denominator == Integer.MIN_VALUE) : false
         * (u == 0) : false  #  inside greatestCommonDivisor method
         * (v == 0) : false  #  inside greatestCommonDivisor method
         * (Math.abs(u) == 1) : false  #  inside greatestCommonDivisor method
         * (Math.abs(v) == 1) : false  #  inside greatestCommonDivisor method
         * (u > 0) : true  #  inside greatestCommonDivisor method
         * (v > 0) : true  #  inside greatestCommonDivisor method
         * ((u & 1) == 0) : true  #  inside greatestCommonDivisor method
         * ((v & 1) == 0) : true  #  inside greatestCommonDivisor method
         * (k < 31) : true  #  inside greatestCommonDivisor method
         * (k == 31) : false  #  inside greatestCommonDivisor method
         * ((u & 1) == 1) : false  #  inside greatestCommonDivisor method
         * ((t & 1) == 0) : true  #  inside greatestCommonDivisor method
         * (t > 0) : true  #  inside greatestCommonDivisor method
         * (t != 0) : false  #  inside greatestCommonDivisor method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Fraction result = Fraction.getReducedFraction(-28, -2147483648);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${absWhenNumeratorGreaterThanOrEqualsTo0}, hash: 5C44868CC181A1A40D59670E7752A1BF
    @Test()
    void absWhenNumeratorGreaterThanOrEqualsTo0() {
        /* Branches:
         * (numerator >= 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(0, 0, 0);
        
        //Act Statement(s)
        Fraction result = target.abs();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${absWhenNumeratorLessThan0}, hash: CFF87200E54908F9B1A5196E9C426359
    @Test()
    void absWhenNumeratorLessThan0() {
        /* Branches:
         * (numerator >= 0) : false
         */
         //Arrange Statement(s)
        Fraction target = spy(Fraction.getFraction(-1, 0, 1));
        Fraction fraction = Fraction.getFraction(Double.parseDouble("0.0"));
        doReturn(fraction).when(target).negate();
        
        //Act Statement(s)
        Fraction result = target.abs();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(fraction));
            verify(target).negate();
        });
    }

    //BaseRock generated method id: ${addWhenIsAdd}, hash: 0CB1B203D5A810FC108452FDB449508A
    @Test()
    void addWhenIsAdd() {
        /* Branches:
         * (numerator == 0) : true  #  inside addSub method
         * (isAdd) : true  #  inside addSub method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(0, 0, 0);
        Fraction fraction = Fraction.getFraction(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        Fraction result = target.add(fraction);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(fraction)));
    }

    //BaseRock generated method id: ${addWhenFractionNumeratorEquals0}, hash: D5841EB3D42310CB915FA0CB5E213BED
    @Test()
    void addWhenFractionNumeratorEquals0() {
        /* Branches:
         * (numerator == 0) : false  #  inside addSub method
         * (fraction.numerator == 0) : true  #  inside addSub method
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(-1, 0, 1);
        Fraction fraction = Fraction.getFraction(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        Fraction result = target.add(fraction);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${addWhenMGreaterThanIntegerMAX_VALUEThrowsArithmeticException}, hash: 6C72780639A9AA36C65BAEA9CD3E2A7E
    @Test()
    void addWhenMGreaterThanIntegerMAX_VALUEThrowsArithmeticException() {
        /* Branches:
         * (numerator == 0) : false  #  inside addSub method
         * (fraction.numerator == 0) : false  #  inside addSub method
         * (u == 0) : false  #  inside greatestCommonDivisor method
         * (v == 0) : true  #  inside greatestCommonDivisor method
         * (u == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (v == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (d1 == 1) : true  #  inside addSub method
         * (m < Integer.MIN_VALUE) : false  #  inside mulAndCheck method
         * (m > Integer.MAX_VALUE) : true  #  inside mulAndCheck method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(0, 0, 0);
        Fraction fraction = Fraction.getFraction(Double.parseDouble("0.0"));
        ArithmeticException arithmeticException = new ArithmeticException("overflow: mul");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            target.add(fraction);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${addWhenMNotGreaterThanIntegerMAX_VALUEThrowsArithmeticException}, hash: B942EC3E9860B71DB7B6B0ED5E4635ED
    @Test()
    void addWhenMNotGreaterThanIntegerMAX_VALUEThrowsArithmeticException() {
        /* Branches:
         * (numerator == 0) : false  #  inside addSub method
         * (fraction.numerator == 0) : false  #  inside addSub method
         * (u == 0) : false  #  inside greatestCommonDivisor method
         * (v == 0) : true  #  inside greatestCommonDivisor method
         * (u == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (v == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (d1 == 1) : true  #  inside addSub method
         * (m < Integer.MIN_VALUE) : false  #  inside mulAndCheck method
         * (m > Integer.MAX_VALUE) : false  #  inside mulAndCheck method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(0, 0, 0);
        Fraction fraction = Fraction.getFraction(Double.parseDouble("0.0"));
        ArithmeticException arithmeticException = new ArithmeticException("overflow: mul");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            target.add(fraction);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${addWhenWBitLengthGreaterThan31ThrowsArithmeticException}, hash: 56077B3DBD4EF7AE478126E1134CA6BF
    @Test()
    void addWhenWBitLengthGreaterThan31ThrowsArithmeticException() {
        /* Branches:
         * (numerator == 0) : false  #  inside addSub method
         * (fraction.numerator == 0) : false  #  inside addSub method
         * (u == 0) : false  #  inside greatestCommonDivisor method
         * (v == 0) : true  #  inside greatestCommonDivisor method
         * (u == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (v == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (d1 == 1) : false  #  inside addSub method
         * (isAdd) : true  #  inside addSub method
         * (tmodd1 == 0) : true  #  inside addSub method
         * (w.bitLength() > 31) : true  #  inside addSub method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(0, 0, 0);
        Fraction fraction = Fraction.getFraction(Double.parseDouble("0.0"));
        ArithmeticException arithmeticException = new ArithmeticException("overflow: numerator too large after multiply");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            target.add(fraction);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${addWhenWBitLengthNotGreaterThan31AndMGreaterThanIntegerMAX_VALUEThrowsArithmeticException}, hash: 1600323DCA3A21319903C776C477384B
    @Test()
    void addWhenWBitLengthNotGreaterThan31AndMGreaterThanIntegerMAX_VALUEThrowsArithmeticException() {
        /* Branches:
         * (numerator == 0) : false  #  inside addSub method
         * (fraction.numerator == 0) : false  #  inside addSub method
         * (u == 0) : false  #  inside greatestCommonDivisor method
         * (v == 0) : true  #  inside greatestCommonDivisor method
         * (u == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (v == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (d1 == 1) : false  #  inside addSub method
         * (isAdd) : true  #  inside addSub method
         * (tmodd1 == 0) : true  #  inside addSub method
         * (w.bitLength() > 31) : false  #  inside addSub method
         * (m > Integer.MAX_VALUE) : true  #  inside mulPosAndCheck method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(0, 0, 0);
        Fraction fraction = Fraction.getFraction(Double.parseDouble("0.0"));
        ArithmeticException arithmeticException = new ArithmeticException("overflow: mulPos");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            target.add(fraction);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${addWhenSGreaterThanIntegerMAX_VALUEThrowsArithmeticException}, hash: F4F20404C63F7375BA39927EFF694286
    @Test()
    void addWhenSGreaterThanIntegerMAX_VALUEThrowsArithmeticException() {
        /* Branches:
         * (numerator == 0) : false  #  inside addSub method
         * (fraction.numerator == 0) : false  #  inside addSub method
         * (u == 0) : false  #  inside greatestCommonDivisor method
         * (v == 0) : true  #  inside greatestCommonDivisor method
         * (u == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (v == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (d1 == 1) : true  #  inside addSub method
         * (m < Integer.MIN_VALUE) : false  #  inside mulAndCheck method
         * (m > Integer.MAX_VALUE) : false  #  inside mulAndCheck method
         * (isAdd) : true  #  inside addSub method
         * (s < Integer.MIN_VALUE) : false  #  inside addAndCheck method
         * (s > Integer.MAX_VALUE) : true  #  inside addAndCheck method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(0, 0, 0);
        Fraction fraction = Fraction.getFraction(Double.parseDouble("0.0"));
        ArithmeticException arithmeticException = new ArithmeticException("overflow: add");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            target.add(fraction);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${addWhenMathAbsVEquals1AndWBitLengthGreaterThan31ThrowsArithmeticException}, hash: 490243257413384DCCA4C48C35731123
    @Test()
    void addWhenMathAbsVEquals1AndWBitLengthGreaterThan31ThrowsArithmeticException() {
        /* Branches:
         * (numerator == 0) : false  #  inside addSub method
         * (fraction.numerator == 0) : false  #  inside addSub method
         * (u == 0) : false  #  inside greatestCommonDivisor method
         * (v == 0) : true  #  inside greatestCommonDivisor method
         * (u == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (v == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (d1 == 1) : false  #  inside addSub method
         * (isAdd) : true  #  inside addSub method
         * (tmodd1 == 0) : false  #  inside addSub method
         * (Math.abs(u) == 1) : false  #  inside greatestCommonDivisor method
         * (Math.abs(v) == 1) : true  #  inside greatestCommonDivisor method
         * (w.bitLength() > 31) : true  #  inside addSub method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(0, 0, 0);
        Fraction fraction = Fraction.getFraction(Double.parseDouble("0.0"));
        ArithmeticException arithmeticException = new ArithmeticException("overflow: numerator too large after multiply");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            target.add(fraction);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${addWhenSNotGreaterThanIntegerMAX_VALUEAndMGreaterThanIntegerMAX_VALUEThrowsArithmeticException}, hash: A9C4E3772804AC962E69B93587120B43
    @Test()
    void addWhenSNotGreaterThanIntegerMAX_VALUEAndMGreaterThanIntegerMAX_VALUEThrowsArithmeticException() {
        /* Branches:
         * (numerator == 0) : false  #  inside addSub method
         * (fraction.numerator == 0) : false  #  inside addSub method
         * (u == 0) : false  #  inside greatestCommonDivisor method
         * (v == 0) : true  #  inside greatestCommonDivisor method
         * (u == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (v == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (d1 == 1) : true  #  inside addSub method
         * (m < Integer.MIN_VALUE) : false  #  inside mulAndCheck method
         * (m > Integer.MAX_VALUE) : false  #  inside mulAndCheck method
         * (isAdd) : true  #  inside addSub method
         * (s < Integer.MIN_VALUE) : false  #  inside addAndCheck method
         * (s > Integer.MAX_VALUE) : false  #  inside addAndCheck method
         * (m > Integer.MAX_VALUE) : true  #  inside mulPosAndCheck method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(0, 0, 0);
        Fraction fraction = Fraction.getFraction(Double.parseDouble("0.0"));
        ArithmeticException arithmeticException = new ArithmeticException("overflow: mulPos");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            target.add(fraction);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${addWhenSNotGreaterThanIntegerMAX_VALUEAndMNotGreaterThanIntegerMAX_VALUE}, hash: 821ADB0C2188FAA1A59F21D8755C5691
    @Test()
    void addWhenSNotGreaterThanIntegerMAX_VALUEAndMNotGreaterThanIntegerMAX_VALUE() {
        /* Branches:
         * (numerator == 0) : false  #  inside addSub method
         * (fraction.numerator == 0) : false  #  inside addSub method
         * (u == 0) : false  #  inside greatestCommonDivisor method
         * (v == 0) : true  #  inside greatestCommonDivisor method
         * (u == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (v == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (d1 == 1) : true  #  inside addSub method
         * (m < Integer.MIN_VALUE) : false  #  inside mulAndCheck method
         * (m > Integer.MAX_VALUE) : false  #  inside mulAndCheck method
         * (isAdd) : true  #  inside addSub method
         * (s < Integer.MIN_VALUE) : false  #  inside addAndCheck method
         * (s > Integer.MAX_VALUE) : false  #  inside addAndCheck method
         * (m > Integer.MAX_VALUE) : false  #  inside mulPosAndCheck method
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(-1, 0, 1);
        Fraction fraction = Fraction.getFraction(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        Fraction result = target.add(fraction);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${addWhenTAnd1Equals0AndTNotGreaterThan0AndTEquals0AndWBitLengthNotGreaterThan31AndMNotGreaterThanIntegerMAX_VALUE2}, hash: DD21B6BE88ECD00A9AB42CBDFF83087F
    @Test()
    void addWhenTAnd1Equals0AndTNotGreaterThan0AndTEquals0AndWBitLengthNotGreaterThan31AndMNotGreaterThanIntegerMAX_VALUE2() {
        /* Branches:
         * (numerator == 0) : false  #  inside addSub method
         * (fraction.numerator == 0) : false  #  inside addSub method
         * (u == 0) : false  #  inside greatestCommonDivisor method
         * (v == 0) : true  #  inside greatestCommonDivisor method
         * (u == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (v == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (d1 == 1) : false  #  inside addSub method
         * (isAdd) : true  #  inside addSub method
         * (tmodd1 == 0) : false  #  inside addSub method
         * (Math.abs(u) == 1) : false  #  inside greatestCommonDivisor method
         * (Math.abs(v) == 1) : false  #  inside greatestCommonDivisor method
         * (u > 0) : true  #  inside greatestCommonDivisor method
         * (v > 0) : true  #  inside greatestCommonDivisor method
         * ((u & 1) == 0) : true  #  inside greatestCommonDivisor method
         * ((v & 1) == 0) : true  #  inside greatestCommonDivisor method
         * (k < 31) : true  #  inside greatestCommonDivisor method
         * (k == 31) : false  #  inside greatestCommonDivisor method
         * ((u & 1) == 1) : true  #  inside greatestCommonDivisor method
         * ((t & 1) == 0) : true  #  inside greatestCommonDivisor method
         * (t > 0) : false  #  inside greatestCommonDivisor method
         * (t != 0) : false  #  inside greatestCommonDivisor method
         * (w.bitLength() > 31) : false  #  inside addSub method
         * (m > Integer.MAX_VALUE) : false  #  inside mulPosAndCheck method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(0, 0, 0);
        Fraction fraction = Fraction.getFraction(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        Fraction result = target.add(fraction);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${addWhenTAnd1Equals0AndTGreaterThan0AndTEquals0AndWBitLengthNotGreaterThan31AndMNotGreaterThanIntegerMAX_VALUE3}, hash: 544E0C358277CCA5CA1565DD49F7EAB6
    @Test()
    void addWhenTAnd1Equals0AndTGreaterThan0AndTEquals0AndWBitLengthNotGreaterThan31AndMNotGreaterThanIntegerMAX_VALUE3() {
        /* Branches:
         * (numerator == 0) : false  #  inside addSub method
         * (fraction.numerator == 0) : false  #  inside addSub method
         * (u == 0) : false  #  inside greatestCommonDivisor method
         * (v == 0) : true  #  inside greatestCommonDivisor method
         * (u == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (v == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (d1 == 1) : false  #  inside addSub method
         * (isAdd) : true  #  inside addSub method
         * (tmodd1 == 0) : false  #  inside addSub method
         * (Math.abs(u) == 1) : false  #  inside greatestCommonDivisor method
         * (Math.abs(v) == 1) : false  #  inside greatestCommonDivisor method
         * (u > 0) : true  #  inside greatestCommonDivisor method
         * (v > 0) : true  #  inside greatestCommonDivisor method
         * ((u & 1) == 0) : true  #  inside greatestCommonDivisor method
         * ((v & 1) == 0) : true  #  inside greatestCommonDivisor method
         * (k < 31) : true  #  inside greatestCommonDivisor method
         * (k == 31) : false  #  inside greatestCommonDivisor method
         * ((u & 1) == 1) : false  #  inside greatestCommonDivisor method
         * ((t & 1) == 0) : true  #  inside greatestCommonDivisor method
         * (t > 0) : true  #  inside greatestCommonDivisor method
         * (t != 0) : false  #  inside greatestCommonDivisor method
         * (w.bitLength() > 31) : false  #  inside addSub method
         * (m > Integer.MAX_VALUE) : false  #  inside mulPosAndCheck method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(0, 0, 0);
        Fraction fraction = Fraction.getFraction(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        Fraction result = target.add(fraction);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${compareToWhenThisEqualsOther}, hash: 584ABBC3E84FD652917C6D795693F1D1
    @Test()
    void compareToWhenThisEqualsOther() {
        /* Branches:
         * (this == other) : true
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(-1, 0, 1);
        
        //Act Statement(s)
        int result = target.compareTo(target);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${compareToWhenDenominatorNotEqualsOtherDenominator}, hash: 1C24F4C9B246331EFDE45E63440C399A
    @Test()
    void compareToWhenDenominatorNotEqualsOtherDenominator() {
        /* Branches:
         * (this == other) : false
         * (numerator == other.numerator) : true
         * (denominator == other.denominator) : false
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(-1, 0, 1);
        
        //Act Statement(s)
        int result = target.compareTo(fractionMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1)));
    }

    //BaseRock generated method id: ${divideByWhenFractionNumeratorEquals0ThrowsArithmeticException}, hash: 4375BD13F095C808F1091FEB549BAEEB
    @Test()
    void divideByWhenFractionNumeratorEquals0ThrowsArithmeticException() {
        /* Branches:
         * (fraction.numerator == 0) : true
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(-1, 0, 1);
        ArithmeticException arithmeticException = new ArithmeticException("The fraction to divide by must not be zero");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            target.divideBy(fractionMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${divideByWhenFractionNumeratorNotEquals0}, hash: 1AEE01D579B178A9242369E37EFE90C4
    @Test()
    void divideByWhenFractionNumeratorNotEquals0() {
        /* Branches:
         * (fraction.numerator == 0) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = spy(Fraction.getFraction(-1, 0, 1));
        Fraction fraction = Fraction.getFraction(Double.parseDouble("0.0"));
        doReturn(fraction).when(target).multiplyBy((Fraction) any());
        Fraction fraction2 = Fraction.getFraction(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        Fraction result = target.divideBy(fraction2);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(fraction));
            verify(target).multiplyBy((Fraction) any());
        });
    }

    //BaseRock generated method id: ${doubleValueTest}, hash: 0102C3D634D27C7FBB20ED56F30B5EB7
    @Test()
    void doubleValueTest() {
        //Arrange Statement(s)
        Fraction target = Fraction.getFraction(-2147483647, 0, 1);
        
        //Act Statement(s)
        double result = target.doubleValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, closeTo(Double.parseDouble("-2.147483647E9"), 0.00001)));
    }

    //BaseRock generated method id: ${equalsWhenObjEqualsThis}, hash: D3EEE590F205A035C2D9DA6F81137629
    @Test()
    void equalsWhenObjEqualsThis() {
        /* Branches:
         * (obj == this) : true
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(-1, 0, 1);
        
        //Act Statement(s)
        boolean result = target.equals(target);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${equalsWhenObjNotInstanceOfFraction}, hash: 6FB164E64380F63A243405E171D83FBC
    @Test()
    void equalsWhenObjNotInstanceOfFraction() {
        /* Branches:
         * (obj == this) : false
         * (!(obj instanceof Fraction)) : true
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(-1, 0, 1);
        Object object = new Object();
        
        //Act Statement(s)
        boolean result = target.equals(object);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${equalsWhenGetDenominatorEqualsOtherGetDenominator}, hash: 364B07C17C08F95EA28184E9EA75328D
    @Test()
    void equalsWhenGetDenominatorEqualsOtherGetDenominator() {
        /* Branches:
         * (obj == this) : false
         * (!(obj instanceof Fraction)) : false
         * (getNumerator() == other.getNumerator()) : true
         * (getDenominator() == other.getDenominator()) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(-2147483647, 0, 1);
        
        //Act Statement(s)
        boolean result = target.equals(fractionMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${equalsWhenGetDenominatorNotEqualsOtherGetDenominator}, hash: 3AA9CE9005E59630BC394F7058BA8310
    @Test()
    void equalsWhenGetDenominatorNotEqualsOtherGetDenominator() {
        /* Branches:
         * (obj == this) : false
         * (!(obj instanceof Fraction)) : false
         * (getNumerator() == other.getNumerator()) : true
         * (getDenominator() == other.getDenominator()) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(-1, 0, 1);
        
        //Act Statement(s)
        boolean result = target.equals(fractionMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${floatValueTest}, hash: D67E25F42044FAC9F4282AB52226F43C
    @Test()
    void floatValueTest() {
        //Arrange Statement(s)
        Fraction target = Fraction.getFraction(-2147483647, 0, 1);
        
        //Act Statement(s)
        float result = target.floatValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Float.parseFloat("-2.1474836E9"))));
    }

    //BaseRock generated method id: ${getDenominatorTest}, hash: 99D8631E68E5C5B99645DFB3773598C4
    @Test()
    void getDenominatorTest() {
        //Arrange Statement(s)
        Fraction target = Fraction.getFraction(-1, 0, 1);
        
        //Act Statement(s)
        int result = target.getDenominator();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1)));
    }

    //BaseRock generated method id: ${getNumeratorTest}, hash: 0ACA2D4DBFDCA893C9EA1F5B6DD71748
    @Test()
    void getNumeratorTest() {
        //Arrange Statement(s)
        Fraction target = Fraction.getFraction(-1, 0, 1);
        
        //Act Statement(s)
        int result = target.getNumerator();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${getProperNumeratorTest}, hash: 7793D99FC51AD61311A82E23E7A29855
    @Test()
    void getProperNumeratorTest() {
        //Arrange Statement(s)
        Fraction target = Fraction.getFraction(-1, 1, 2);
        
        //Act Statement(s)
        int result = target.getProperNumerator();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1)));
    }

    //BaseRock generated method id: ${getProperWholeTest}, hash: 38E16394D5B4539F92188341E4C729AB
    @Test()
    void getProperWholeTest() {
        //Arrange Statement(s)
        Fraction target = Fraction.getFraction(-1, 0, 1);
        
        //Act Statement(s)
        int result = target.getProperWhole();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${intValueTest}, hash: E8D0EA41BA83E45F605E229EC25C7DEA
    @Test()
    void intValueTest() {
        //Arrange Statement(s)
        Fraction target = Fraction.getFraction(-1, 0, 1);
        
        //Act Statement(s)
        int result = target.intValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${invertWhenNumeratorEquals0ThrowsArithmeticException}, hash: CAA0AC09D5094B886CB3C5D3404B17F8
    @Test()
    void invertWhenNumeratorEquals0ThrowsArithmeticException() {
        /* Branches:
         * (numerator == 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(0, 0, 0);
        ArithmeticException arithmeticException = new ArithmeticException("Unable to invert zero.");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            target.invert();
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${invertWhenNumeratorEqualsIntegerMIN_VALUEThrowsArithmeticException}, hash: 000DC5C4B8637BB87706FBED7E5C49CF
    @Test()
    void invertWhenNumeratorEqualsIntegerMIN_VALUEThrowsArithmeticException() {
        /* Branches:
         * (numerator == 0) : false
         * (numerator == Integer.MIN_VALUE) : true
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(-1, 2147483647, 1);
        ArithmeticException arithmeticException = new ArithmeticException("overflow: can't negate numerator");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            target.invert();
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${invertWhenNumeratorLessThan0}, hash: F682210B834436A6C6D374E4AF52483E
    @Test()
    void invertWhenNumeratorLessThan0() {
        /* Branches:
         * (numerator == 0) : false
         * (numerator == Integer.MIN_VALUE) : false
         * (numerator < 0) : true
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(-1, 0, 1);
        
        //Act Statement(s)
        Fraction result = target.invert();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${longValueTest}, hash: 707B5A8F9F4A7DCDB4D58FCF3B181F48
    @Test()
    void longValueTest() {
        //Arrange Statement(s)
        Fraction target = Fraction.getFraction(-1, 0, 1);
        
        //Act Statement(s)
        long result = target.longValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1L)));
    }

    //BaseRock generated method id: ${multiplyByWhenFractionNumeratorEquals0}, hash: 813AE96A6C2C14CE2983EA6CDE1E2312
    @Test()
    void multiplyByWhenFractionNumeratorEquals0() {
        /* Branches:
         * (numerator == 0) : false
         * (fraction.numerator == 0) : true
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(-1, 0, 1);
        
        //Act Statement(s)
        Fraction result = target.multiplyBy(fractionMock);
        Fraction fraction = Fraction.ZERO;
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(fraction)));
    }

    //BaseRock generated method id: ${multiplyByWhenMNotGreaterThanIntegerMAX_VALUEAndMGreaterThanIntegerMAX_VALUEThrowsArithmeticException}, hash: C906CB833A6480E2A37844F8EED83E8E
    @Test()
    void multiplyByWhenMNotGreaterThanIntegerMAX_VALUEAndMGreaterThanIntegerMAX_VALUEThrowsArithmeticException() {
        /* Branches:
         * (numerator == 0) : false
         * (fraction.numerator == 0) : false
         * (u == 0) : false  #  inside greatestCommonDivisor method
         * (v == 0) : true  #  inside greatestCommonDivisor method
         * (u == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (v == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (Math.abs(u) == 1) : false  #  inside greatestCommonDivisor method
         * (Math.abs(v) == 1) : true  #  inside greatestCommonDivisor method
         * (m < Integer.MIN_VALUE) : false  #  inside mulAndCheck method
         * (m > Integer.MAX_VALUE) : false  #  inside mulAndCheck method
         * (m > Integer.MAX_VALUE) : true  #  inside mulPosAndCheck method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(0, 0, 0);
        ArithmeticException arithmeticException = new ArithmeticException("overflow: mulPos");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            target.multiplyBy(fractionMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${multiplyByWhenMNotGreaterThanIntegerMAX_VALUE}, hash: 8F9EA4992F520A236D8DCD0277DEFA6C
    @Test()
    void multiplyByWhenMNotGreaterThanIntegerMAX_VALUE() {
        /* Branches:
         * (numerator == 0) : false
         * (fraction.numerator == 0) : false
         * (u == 0) : false  #  inside greatestCommonDivisor method
         * (v == 0) : true  #  inside greatestCommonDivisor method
         * (u == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (v == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (Math.abs(u) == 1) : false  #  inside greatestCommonDivisor method
         * (Math.abs(v) == 1) : true  #  inside greatestCommonDivisor method
         * (m < Integer.MIN_VALUE) : false  #  inside mulAndCheck method
         * (m > Integer.MAX_VALUE) : false  #  inside mulAndCheck method
         * (m > Integer.MAX_VALUE) : false  #  inside mulPosAndCheck method
         */
         //Arrange Statement(s)
        try (MockedStatic<Fraction> fraction = mockStatic(Fraction.class, CALLS_REAL_METHODS)) {
            Fraction fraction2 = Fraction.getFraction(Double.parseDouble("0.0"));
            fraction.when(() -> Fraction.getReducedFraction(2, 0)).thenReturn(fraction2);
            Fraction target = Fraction.getFraction(-1, 0, 1);
            //Act Statement(s)
            Fraction result = target.multiplyBy(fractionMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(fraction2));
                fraction.verify(() -> Fraction.getReducedFraction(2, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${multiplyByWhenTAnd1Equals0AndTNotGreaterThan0AndTEquals0AndMLessThanIntegerMIN_VALUE2ThrowsArithmeticException}, hash: 281FC105C45854400F02090DBC8FB7A2
    @Test()
    void multiplyByWhenTAnd1Equals0AndTNotGreaterThan0AndTEquals0AndMLessThanIntegerMIN_VALUE2ThrowsArithmeticException() {
        /* Branches:
         * (numerator == 0) : false
         * (fraction.numerator == 0) : false
         * (u == 0) : false  #  inside greatestCommonDivisor method
         * (v == 0) : true  #  inside greatestCommonDivisor method
         * (u == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (v == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (Math.abs(u) == 1) : false  #  inside greatestCommonDivisor method
         * (Math.abs(v) == 1) : false  #  inside greatestCommonDivisor method
         * (u > 0) : true  #  inside greatestCommonDivisor method
         * (v > 0) : true  #  inside greatestCommonDivisor method
         * ((u & 1) == 0) : true  #  inside greatestCommonDivisor method
         * ((v & 1) == 0) : true  #  inside greatestCommonDivisor method
         * (k < 31) : true  #  inside greatestCommonDivisor method
         * (k == 31) : false  #  inside greatestCommonDivisor method
         * ((u & 1) == 1) : true  #  inside greatestCommonDivisor method
         * ((t & 1) == 0) : true  #  inside greatestCommonDivisor method
         * (t > 0) : false  #  inside greatestCommonDivisor method
         * (t != 0) : false  #  inside greatestCommonDivisor method
         * (m < Integer.MIN_VALUE) : true  #  inside mulAndCheck method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(0, 0, 0);
        ArithmeticException arithmeticException = new ArithmeticException("overflow: mul");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            target.multiplyBy(fractionMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${multiplyByWhenTAnd1Equals0AndTGreaterThan0AndTEquals0AndMLessThanIntegerMIN_VALUE3ThrowsArithmeticException}, hash: C72AF24B26E5C5EC4AE107A2FF9F0224
    @Test()
    void multiplyByWhenTAnd1Equals0AndTGreaterThan0AndTEquals0AndMLessThanIntegerMIN_VALUE3ThrowsArithmeticException() {
        /* Branches:
         * (numerator == 0) : false
         * (fraction.numerator == 0) : false
         * (u == 0) : false  #  inside greatestCommonDivisor method
         * (v == 0) : true  #  inside greatestCommonDivisor method
         * (u == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (v == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (Math.abs(u) == 1) : false  #  inside greatestCommonDivisor method
         * (Math.abs(v) == 1) : false  #  inside greatestCommonDivisor method
         * (u > 0) : true  #  inside greatestCommonDivisor method
         * (v > 0) : true  #  inside greatestCommonDivisor method
         * ((u & 1) == 0) : true  #  inside greatestCommonDivisor method
         * ((v & 1) == 0) : true  #  inside greatestCommonDivisor method
         * (k < 31) : true  #  inside greatestCommonDivisor method
         * (k == 31) : false  #  inside greatestCommonDivisor method
         * ((u & 1) == 1) : false  #  inside greatestCommonDivisor method
         * ((t & 1) == 0) : true  #  inside greatestCommonDivisor method
         * (t > 0) : true  #  inside greatestCommonDivisor method
         * (t != 0) : false  #  inside greatestCommonDivisor method
         * (m < Integer.MIN_VALUE) : true  #  inside mulAndCheck method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(0, 0, 0);
        ArithmeticException arithmeticException = new ArithmeticException("overflow: mul");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            target.multiplyBy(fractionMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${negateWhenNumeratorEqualsIntegerMIN_VALUEThrowsArithmeticException}, hash: 534653C6062FC7154ED8616196D6B0A7
    @Test()
    void negateWhenNumeratorEqualsIntegerMIN_VALUEThrowsArithmeticException() {
        /* Branches:
         * (numerator == Integer.MIN_VALUE) : true
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(-2147483648, 0, 1);
        ArithmeticException arithmeticException = new ArithmeticException("overflow: too large to negate");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            target.negate();
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${negateWhenNumeratorNotEqualsIntegerMIN_VALUE}, hash: 4F45626BD41B3EC21984EC92DDD1A072
    @Test()
    void negateWhenNumeratorNotEqualsIntegerMIN_VALUE() {
        /* Branches:
         * (numerator == Integer.MIN_VALUE) : false
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(-1, 0, 1);
        
        //Act Statement(s)
        Fraction result = target.negate();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${powWhenPowerEquals1}, hash: 362069B11CD4ECD2522489DA6BA406F4
    @Test()
    void powWhenPowerEquals1() {
        /* Branches:
         * (power == 1) : true
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(-1, 0, 1);
        
        //Act Statement(s)
        Fraction result = target.pow(1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${powWhenPowerEquals0}, hash: 34F729727EC08DC5FB3C10626E28910D
    @Test()
    void powWhenPowerEquals0() {
        /* Branches:
         * (power == 1) : false
         * (power == 0) : true
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(-1, 0, 1);
        
        //Act Statement(s)
        Fraction result = target.pow(0);
        Fraction fraction = Fraction.ONE;
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(fraction)));
    }

    //BaseRock generated method id: ${powWhenPowerEqualsIntegerMIN_VALUE}, hash: 4170471DF2D1BFBB085DA98CA7BBE24E
    @Test()
    void powWhenPowerEqualsIntegerMIN_VALUE() {
        /* Branches:
         * (power == 1) : false
         * (power == 0) : false
         * (power < 0) : true
         * (power == Integer.MIN_VALUE) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = spy(Fraction.getFraction(-1, 0, 1));
        Fraction fraction = Fraction.getFraction(Double.parseDouble("0.0"));
        doReturn(fraction).when(target).invert();
        
        //Act Statement(s)
        Fraction result = target.pow(-2147483648);
        Fraction fraction2 = Fraction.getFraction(Double.parseDouble("0.0"));
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(fraction2));
            verify(target).invert();
        });
    }

    //BaseRock generated method id: ${powWhenPowerNotEqualsIntegerMIN_VALUE}, hash: 49C76ECA6944EAB0BC7D60DEC5E17FED
    @Test()
    void powWhenPowerNotEqualsIntegerMIN_VALUE() {
        /* Branches:
         * (power == 1) : false
         * (power == 0) : false
         * (power < 0) : true
         * (power == Integer.MIN_VALUE) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = spy(Fraction.getFraction(-1, 0, 1));
        Fraction fraction = Fraction.getFraction(Double.parseDouble("0.0"));
        doReturn(fraction).when(target).invert();
        
        //Act Statement(s)
        Fraction result = target.pow(-1);
        Fraction fraction2 = Fraction.getFraction(Double.parseDouble("0.0"));
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(fraction2));
            verify(target).invert();
        });
    }

    //BaseRock generated method id: ${powWhenPowerModulus2Equals0}, hash: 86F3B1D97B7AB40C00DA3280E21E19AC
    @Test()
    void powWhenPowerModulus2Equals0() {
        /* Branches:
         * (power == 1) : false
         * (power == 0) : false
         * (power < 0) : false
         * (power % 2 == 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = spy(Fraction.getFraction(-1, 0, 1));
        Fraction fraction = Fraction.getFraction(Double.parseDouble("0.0"));
        doReturn(fraction).when(target).multiplyBy(target);
        
        //Act Statement(s)
        Fraction result = target.pow(2);
        Fraction fraction2 = Fraction.getFraction(Double.parseDouble("0.0"));
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(fraction2));
            verify(target).multiplyBy(target);
        });
    }

    //BaseRock generated method id: ${powWhenPowerModulus2NotEquals0}, hash: D44DAD107F23CE97AA6D2E69F3ADB478
    @Test()
    void powWhenPowerModulus2NotEquals0() {
        /* Branches:
         * (power == 1) : false
         * (power == 0) : false
         * (power < 0) : false
         * (power % 2 == 0) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = spy(Fraction.getFraction(-1, 0, 1));
        Fraction fraction = Fraction.getFraction(Double.parseDouble("0.0"));
        doReturn(fraction).when(target).multiplyBy(target);
        
        //Act Statement(s)
        Fraction result = target.pow(3);
        Fraction fraction2 = Fraction.getFraction(Double.parseDouble("0.0"));
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(fraction2));
            verify(target).multiplyBy(target);
        });
    }

    //BaseRock generated method id: ${reduceWhenEqualsZERO}, hash: F463D4023E436A8D6748E55EE18F5FB3
    @Test()
    void reduceWhenEqualsZERO() {
        /* Branches:
         * (numerator == 0) : true
         * (equals(ZERO)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(0, 0, 0);
        
        //Act Statement(s)
        Fraction result = target.reduce();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${reduceWhenEqualsNotZERO}, hash: C2884046D487A989BE7E317173B4D141
    @Test()
    void reduceWhenEqualsNotZERO() {
        /* Branches:
         * (numerator == 0) : true
         * (equals(ZERO)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(0, 0, 0);
        
        //Act Statement(s)
        Fraction result = target.reduce();
        Fraction fraction = Fraction.ZERO;
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(fraction)));
    }

    //BaseRock generated method id: ${reduceWhenGcdEquals1}, hash: 65F4354B9BB2F83BDD5BD4498211EE34
    @Test()
    void reduceWhenGcdEquals1() {
        /* Branches:
         * (numerator == 0) : false
         * (u == 0) : false  #  inside greatestCommonDivisor method
         * (v == 0) : false  #  inside greatestCommonDivisor method
         * (Math.abs(u) == 1) : false  #  inside greatestCommonDivisor method
         * (Math.abs(v) == 1) : true  #  inside greatestCommonDivisor method
         * (gcd == 1) : true
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(-1, 1, 1);
        
        //Act Statement(s)
        Fraction result = target.reduce();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${reduceWhenKNotEquals31AndUAnd1Equals1AndTAnd1Equals0AndTNotGreaterThan0AndTEquals0AndGcdNotEquals1}, hash: EDAA4BA0BE17BB747AA9E663095999F5
    @Test()
    void reduceWhenKNotEquals31AndUAnd1Equals1AndTAnd1Equals0AndTNotGreaterThan0AndTEquals0AndGcdNotEquals1() {
        /* Branches:
         * (numerator == 0) : false
         * (u == 0) : false  #  inside greatestCommonDivisor method
         * (v == 0) : false  #  inside greatestCommonDivisor method
         * (Math.abs(u) == 1) : false  #  inside greatestCommonDivisor method
         * (Math.abs(v) == 1) : false  #  inside greatestCommonDivisor method
         * (u > 0) : true  #  inside greatestCommonDivisor method
         * (v > 0) : true  #  inside greatestCommonDivisor method
         * ((u & 1) == 0) : true  #  inside greatestCommonDivisor method
         * ((v & 1) == 0) : true  #  inside greatestCommonDivisor method
         * (k < 31) : true  #  inside greatestCommonDivisor method
         * (k == 31) : false  #  inside greatestCommonDivisor method
         * ((u & 1) == 1) : true  #  inside greatestCommonDivisor method
         * ((t & 1) == 0) : true  #  inside greatestCommonDivisor method
         * (t > 0) : false  #  inside greatestCommonDivisor method
         * (t != 0) : false  #  inside greatestCommonDivisor method
         * (gcd == 1) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<Fraction> fraction = mockStatic(Fraction.class, CALLS_REAL_METHODS)) {
            Fraction fraction2 = Fraction.getFraction(Double.parseDouble("0.0"));
            fraction.when(() -> Fraction.getFraction(-1, 0)).thenReturn(fraction2);
            Fraction target = Fraction.getFraction(-1, 4, 2);
            //Act Statement(s)
            Fraction result = target.reduce();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(fraction2));
                fraction.verify(() -> Fraction.getFraction(-1, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${reduceWhenKNotEquals31AndUAnd1NotEquals1AndTAnd1Equals0AndTGreaterThan0AndTEquals0AndGcdNotEquals1}, hash: 947B266BCA289F3081965DE127C9BAAA
    @Test()
    void reduceWhenKNotEquals31AndUAnd1NotEquals1AndTAnd1Equals0AndTGreaterThan0AndTEquals0AndGcdNotEquals1() {
        /* Branches:
         * (numerator == 0) : false
         * (u == 0) : false  #  inside greatestCommonDivisor method
         * (v == 0) : false  #  inside greatestCommonDivisor method
         * (Math.abs(u) == 1) : false  #  inside greatestCommonDivisor method
         * (Math.abs(v) == 1) : false  #  inside greatestCommonDivisor method
         * (u > 0) : true  #  inside greatestCommonDivisor method
         * (v > 0) : true  #  inside greatestCommonDivisor method
         * ((u & 1) == 0) : true  #  inside greatestCommonDivisor method
         * ((v & 1) == 0) : true  #  inside greatestCommonDivisor method
         * (k < 31) : true  #  inside greatestCommonDivisor method
         * (k == 31) : false  #  inside greatestCommonDivisor method
         * ((u & 1) == 1) : false  #  inside greatestCommonDivisor method
         * ((t & 1) == 0) : true  #  inside greatestCommonDivisor method
         * (t > 0) : true  #  inside greatestCommonDivisor method
         * (t != 0) : false  #  inside greatestCommonDivisor method
         * (gcd == 1) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<Fraction> fraction = mockStatic(Fraction.class, CALLS_REAL_METHODS)) {
            Fraction fraction2 = Fraction.getFraction(Double.parseDouble("0.0"));
            fraction.when(() -> Fraction.getFraction(-4, 0)).thenReturn(fraction2);
            Fraction target = Fraction.getFraction(-715827882, 0, 3);
            //Act Statement(s)
            Fraction result = target.reduce();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(fraction2));
                fraction.verify(() -> Fraction.getFraction(-4, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${subtractWhenNotIsAdd}, hash: 7575DD6E858CAE0F143993B0852C290B
    @Test()
    void subtractWhenNotIsAdd() {
        /* Branches:
         * (numerator == 0) : true  #  inside addSub method
         * (isAdd) : false  #  inside addSub method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(0, 0, 0);
        Fraction fraction = Fraction.getFraction(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        Fraction result = target.subtract(fraction);
        Fraction fraction2 = Fraction.getFraction(Double.parseDouble("0.0"));
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(fraction2)));
    }

    //BaseRock generated method id: ${subtractWhenFractionNumeratorEquals0}, hash: CE41E7A44822CB13322312567746C261
    @Test()
    void subtractWhenFractionNumeratorEquals0() {
        /* Branches:
         * (numerator == 0) : false  #  inside addSub method
         * (fraction.numerator == 0) : true  #  inside addSub method
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(-1, 0, 1);
        Fraction fraction = Fraction.getFraction(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        Fraction result = target.subtract(fraction);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${subtractWhenMGreaterThanIntegerMAX_VALUEThrowsArithmeticException}, hash: FC07AF63AEFAAC674E7DB330FDC8E3C9
    @Test()
    void subtractWhenMGreaterThanIntegerMAX_VALUEThrowsArithmeticException() {
        /* Branches:
         * (numerator == 0) : false  #  inside addSub method
         * (fraction.numerator == 0) : false  #  inside addSub method
         * (u == 0) : false  #  inside greatestCommonDivisor method
         * (v == 0) : true  #  inside greatestCommonDivisor method
         * (u == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (v == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (d1 == 1) : true  #  inside addSub method
         * (m < Integer.MIN_VALUE) : false  #  inside mulAndCheck method
         * (m > Integer.MAX_VALUE) : true  #  inside mulAndCheck method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(0, 0, 0);
        Fraction fraction = Fraction.getFraction(Double.parseDouble("0.0"));
        ArithmeticException arithmeticException = new ArithmeticException("overflow: mul");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            target.subtract(fraction);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${subtractWhenMNotGreaterThanIntegerMAX_VALUEThrowsArithmeticException}, hash: B515305C7C6D2B812935B17F0EFFBF4B
    @Test()
    void subtractWhenMNotGreaterThanIntegerMAX_VALUEThrowsArithmeticException() {
        /* Branches:
         * (numerator == 0) : false  #  inside addSub method
         * (fraction.numerator == 0) : false  #  inside addSub method
         * (u == 0) : false  #  inside greatestCommonDivisor method
         * (v == 0) : true  #  inside greatestCommonDivisor method
         * (u == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (v == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (d1 == 1) : true  #  inside addSub method
         * (m < Integer.MIN_VALUE) : false  #  inside mulAndCheck method
         * (m > Integer.MAX_VALUE) : false  #  inside mulAndCheck method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(0, 0, 0);
        Fraction fraction = Fraction.getFraction(Double.parseDouble("0.0"));
        ArithmeticException arithmeticException = new ArithmeticException("overflow: mul");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            target.subtract(fraction);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${subtractWhenWBitLengthGreaterThan31ThrowsArithmeticException}, hash: 90AADF5AE146F907A7EFFC7CC0F8DA51
    @Test()
    void subtractWhenWBitLengthGreaterThan31ThrowsArithmeticException() {
        /* Branches:
         * (numerator == 0) : false  #  inside addSub method
         * (fraction.numerator == 0) : false  #  inside addSub method
         * (u == 0) : false  #  inside greatestCommonDivisor method
         * (v == 0) : true  #  inside greatestCommonDivisor method
         * (u == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (v == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (d1 == 1) : false  #  inside addSub method
         * (isAdd) : false  #  inside addSub method
         * (tmodd1 == 0) : true  #  inside addSub method
         * (w.bitLength() > 31) : true  #  inside addSub method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(0, 0, 0);
        Fraction fraction = Fraction.getFraction(Double.parseDouble("0.0"));
        ArithmeticException arithmeticException = new ArithmeticException("overflow: numerator too large after multiply");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            target.subtract(fraction);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${subtractWhenWBitLengthNotGreaterThan31AndMGreaterThanIntegerMAX_VALUEThrowsArithmeticException}, hash: 0BDD349D8CA54E4B10F83D5BC15268E3
    @Test()
    void subtractWhenWBitLengthNotGreaterThan31AndMGreaterThanIntegerMAX_VALUEThrowsArithmeticException() {
        /* Branches:
         * (numerator == 0) : false  #  inside addSub method
         * (fraction.numerator == 0) : false  #  inside addSub method
         * (u == 0) : false  #  inside greatestCommonDivisor method
         * (v == 0) : true  #  inside greatestCommonDivisor method
         * (u == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (v == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (d1 == 1) : false  #  inside addSub method
         * (isAdd) : false  #  inside addSub method
         * (tmodd1 == 0) : true  #  inside addSub method
         * (w.bitLength() > 31) : false  #  inside addSub method
         * (m > Integer.MAX_VALUE) : true  #  inside mulPosAndCheck method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(0, 0, 0);
        Fraction fraction = Fraction.getFraction(Double.parseDouble("0.0"));
        ArithmeticException arithmeticException = new ArithmeticException("overflow: mulPos");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            target.subtract(fraction);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${subtractWhenSGreaterThanIntegerMAX_VALUEThrowsArithmeticException}, hash: 86AA7B5AD3D8CE86E8367D5EB6C8F425
    @Test()
    void subtractWhenSGreaterThanIntegerMAX_VALUEThrowsArithmeticException() {
        /* Branches:
         * (numerator == 0) : false  #  inside addSub method
         * (fraction.numerator == 0) : false  #  inside addSub method
         * (u == 0) : false  #  inside greatestCommonDivisor method
         * (v == 0) : true  #  inside greatestCommonDivisor method
         * (u == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (v == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (d1 == 1) : true  #  inside addSub method
         * (m < Integer.MIN_VALUE) : false  #  inside mulAndCheck method
         * (m > Integer.MAX_VALUE) : false  #  inside mulAndCheck method
         * (isAdd) : false  #  inside addSub method
         * (s < Integer.MIN_VALUE) : false  #  inside subAndCheck method
         * (s > Integer.MAX_VALUE) : true  #  inside subAndCheck method
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(-1, 0, 1);
        Fraction fraction = Fraction.getFraction(Double.parseDouble("0.0"));
        ArithmeticException arithmeticException = new ArithmeticException("overflow: add");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            target.subtract(fraction);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${subtractWhenMathAbsVEquals1AndWBitLengthGreaterThan31ThrowsArithmeticException}, hash: 4F08CC7DAB02E9CAAF2A03FAD00977C8
    @Test()
    void subtractWhenMathAbsVEquals1AndWBitLengthGreaterThan31ThrowsArithmeticException() {
        /* Branches:
         * (numerator == 0) : false  #  inside addSub method
         * (fraction.numerator == 0) : false  #  inside addSub method
         * (u == 0) : false  #  inside greatestCommonDivisor method
         * (v == 0) : true  #  inside greatestCommonDivisor method
         * (u == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (v == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (d1 == 1) : false  #  inside addSub method
         * (isAdd) : false  #  inside addSub method
         * (tmodd1 == 0) : false  #  inside addSub method
         * (Math.abs(u) == 1) : false  #  inside greatestCommonDivisor method
         * (Math.abs(v) == 1) : true  #  inside greatestCommonDivisor method
         * (w.bitLength() > 31) : true  #  inside addSub method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(0, 0, 0);
        Fraction fraction = Fraction.getFraction(Double.parseDouble("0.0"));
        ArithmeticException arithmeticException = new ArithmeticException("overflow: numerator too large after multiply");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            target.subtract(fraction);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${subtractWhenSNotGreaterThanIntegerMAX_VALUEAndMGreaterThanIntegerMAX_VALUEThrowsArithmeticException}, hash: F659AE57DD8C9BFECFF13A3E568EBB1A
    @Test()
    void subtractWhenSNotGreaterThanIntegerMAX_VALUEAndMGreaterThanIntegerMAX_VALUEThrowsArithmeticException() {
        /* Branches:
         * (numerator == 0) : false  #  inside addSub method
         * (fraction.numerator == 0) : false  #  inside addSub method
         * (u == 0) : false  #  inside greatestCommonDivisor method
         * (v == 0) : true  #  inside greatestCommonDivisor method
         * (u == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (v == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (d1 == 1) : true  #  inside addSub method
         * (m < Integer.MIN_VALUE) : false  #  inside mulAndCheck method
         * (m > Integer.MAX_VALUE) : false  #  inside mulAndCheck method
         * (isAdd) : false  #  inside addSub method
         * (s < Integer.MIN_VALUE) : false  #  inside subAndCheck method
         * (s > Integer.MAX_VALUE) : false  #  inside subAndCheck method
         * (m > Integer.MAX_VALUE) : true  #  inside mulPosAndCheck method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(0, 0, 0);
        Fraction fraction = Fraction.getFraction(Double.parseDouble("0.0"));
        ArithmeticException arithmeticException = new ArithmeticException("overflow: mulPos");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            target.subtract(fraction);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${subtractWhenSNotGreaterThanIntegerMAX_VALUEAndMNotGreaterThanIntegerMAX_VALUE}, hash: E0645BF8598F8D49F748F56EC4BF38E2
    @Test()
    void subtractWhenSNotGreaterThanIntegerMAX_VALUEAndMNotGreaterThanIntegerMAX_VALUE() {
        /* Branches:
         * (numerator == 0) : false  #  inside addSub method
         * (fraction.numerator == 0) : false  #  inside addSub method
         * (u == 0) : false  #  inside greatestCommonDivisor method
         * (v == 0) : true  #  inside greatestCommonDivisor method
         * (u == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (v == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (d1 == 1) : true  #  inside addSub method
         * (m < Integer.MIN_VALUE) : false  #  inside mulAndCheck method
         * (m > Integer.MAX_VALUE) : false  #  inside mulAndCheck method
         * (isAdd) : false  #  inside addSub method
         * (s < Integer.MIN_VALUE) : false  #  inside subAndCheck method
         * (s > Integer.MAX_VALUE) : false  #  inside subAndCheck method
         * (m > Integer.MAX_VALUE) : false  #  inside mulPosAndCheck method
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(-1, 0, 1);
        Fraction fraction = Fraction.getFraction(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        Fraction result = target.subtract(fraction);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${subtractWhenTAnd1Equals0AndTNotGreaterThan0AndTEquals0AndWBitLengthNotGreaterThan31AndMNotGreaterThanIntegerMAX_VALUE2}, hash: 31813D1703D6FE87D72D51DDB7AE5FCD
    @Test()
    void subtractWhenTAnd1Equals0AndTNotGreaterThan0AndTEquals0AndWBitLengthNotGreaterThan31AndMNotGreaterThanIntegerMAX_VALUE2() {
        /* Branches:
         * (numerator == 0) : false  #  inside addSub method
         * (fraction.numerator == 0) : false  #  inside addSub method
         * (u == 0) : false  #  inside greatestCommonDivisor method
         * (v == 0) : true  #  inside greatestCommonDivisor method
         * (u == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (v == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (d1 == 1) : false  #  inside addSub method
         * (isAdd) : false  #  inside addSub method
         * (tmodd1 == 0) : false  #  inside addSub method
         * (Math.abs(u) == 1) : false  #  inside greatestCommonDivisor method
         * (Math.abs(v) == 1) : false  #  inside greatestCommonDivisor method
         * (u > 0) : true  #  inside greatestCommonDivisor method
         * (v > 0) : true  #  inside greatestCommonDivisor method
         * ((u & 1) == 0) : true  #  inside greatestCommonDivisor method
         * ((v & 1) == 0) : true  #  inside greatestCommonDivisor method
         * (k < 31) : true  #  inside greatestCommonDivisor method
         * (k == 31) : false  #  inside greatestCommonDivisor method
         * ((u & 1) == 1) : true  #  inside greatestCommonDivisor method
         * ((t & 1) == 0) : true  #  inside greatestCommonDivisor method
         * (t > 0) : false  #  inside greatestCommonDivisor method
         * (t != 0) : false  #  inside greatestCommonDivisor method
         * (w.bitLength() > 31) : false  #  inside addSub method
         * (m > Integer.MAX_VALUE) : false  #  inside mulPosAndCheck method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(0, 0, 0);
        Fraction fraction = Fraction.getFraction(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        Fraction result = target.subtract(fraction);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${subtractWhenTAnd1Equals0AndTGreaterThan0AndTEquals0AndWBitLengthNotGreaterThan31AndMNotGreaterThanIntegerMAX_VALUE3}, hash: 3D9690FEFB14AB84FF74571D8A5331C2
    @Test()
    void subtractWhenTAnd1Equals0AndTGreaterThan0AndTEquals0AndWBitLengthNotGreaterThan31AndMNotGreaterThanIntegerMAX_VALUE3() {
        /* Branches:
         * (numerator == 0) : false  #  inside addSub method
         * (fraction.numerator == 0) : false  #  inside addSub method
         * (u == 0) : false  #  inside greatestCommonDivisor method
         * (v == 0) : true  #  inside greatestCommonDivisor method
         * (u == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (v == Integer.MIN_VALUE) : false  #  inside greatestCommonDivisor method
         * (d1 == 1) : false  #  inside addSub method
         * (isAdd) : false  #  inside addSub method
         * (tmodd1 == 0) : false  #  inside addSub method
         * (Math.abs(u) == 1) : false  #  inside greatestCommonDivisor method
         * (Math.abs(v) == 1) : false  #  inside greatestCommonDivisor method
         * (u > 0) : true  #  inside greatestCommonDivisor method
         * (v > 0) : true  #  inside greatestCommonDivisor method
         * ((u & 1) == 0) : true  #  inside greatestCommonDivisor method
         * ((v & 1) == 0) : true  #  inside greatestCommonDivisor method
         * (k < 31) : true  #  inside greatestCommonDivisor method
         * (k == 31) : false  #  inside greatestCommonDivisor method
         * ((u & 1) == 1) : false  #  inside greatestCommonDivisor method
         * ((t & 1) == 0) : true  #  inside greatestCommonDivisor method
         * (t > 0) : true  #  inside greatestCommonDivisor method
         * (t != 0) : false  #  inside greatestCommonDivisor method
         * (w.bitLength() > 31) : false  #  inside addSub method
         * (m > Integer.MAX_VALUE) : false  #  inside mulPosAndCheck method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(0, 0, 0);
        Fraction fraction = Fraction.getFraction(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        Fraction result = target.subtract(fraction);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${toProperStringWhenNumeratorEquals0}, hash: BA289D5E3DC25CE048766C9282794101
    @Test()
    void toProperStringWhenNumeratorEquals0() {
        /* Branches:
         * (toProperString == null) : true
         * (numerator == 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(0, 0, 0);
        
        //Act Statement(s)
        String result = target.toProperString();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("0")));
    }

    //BaseRock generated method id: ${toProperStringWhenNumeratorEqualsMinus1MultipliedByDenominator}, hash: B7AB56523B4ECB93B19CE822F406FD73
    @Test()
    void toProperStringWhenNumeratorEqualsMinus1MultipliedByDenominator() {
        /* Branches:
         * (toProperString == null) : true
         * (numerator == 0) : false
         * (numerator == denominator) : false
         * (numerator == -1 * denominator) : true
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(-1, 0, 1);
        
        //Act Statement(s)
        String result = target.toProperString();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("-1")));
    }

    //BaseRock generated method id: ${toProperStringWhenProperNumeratorEquals0}, hash: 0A9CBA04FF38A19A86AD20BB417E5D50
    @Test()
    void toProperStringWhenProperNumeratorEquals0() {
        /* Branches:
         * (toProperString == null) : true
         * (numerator == 0) : false
         * (numerator == denominator) : false
         * (numerator == -1 * denominator) : false
         * (numerator > 0) : false
         * ((numerator > 0 ? -numerator : numerator) < -denominator) : true
         * (properNumerator == 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(0, 0, 0);
        
        //Act Statement(s)
        String result = target.toProperString();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${toProperStringWhenProperNumeratorNotEquals0}, hash: 591BBF0FE28E773A6E1A06A77BF85BAE
    @Test()
    void toProperStringWhenProperNumeratorNotEquals0() {
        /* Branches:
         * (toProperString == null) : true
         * (numerator == 0) : false
         * (numerator == denominator) : false
         * (numerator == -1 * denominator) : false
         * (numerator > 0) : false
         * ((numerator > 0 ? -numerator : numerator) < -denominator) : true
         * (properNumerator == 0) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(0, 0, 0);
        
        //Act Statement(s)
        String result = target.toProperString();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${toStringWhenToStringIsNull}, hash: 9D0D7AC5C70FE7DB4DDE97E0C643398C
    @Test()
    void toStringWhenToStringIsNull() {
        /* Branches:
         * (toString == null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Fraction target = Fraction.getFraction(0, 0, 0);
        
        //Act Statement(s)
        String result = target.toString();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }
}
