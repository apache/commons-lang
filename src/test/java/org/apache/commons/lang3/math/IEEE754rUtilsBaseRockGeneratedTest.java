package org.apache.commons.lang3.math;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mockStatic;
import static org.hamcrest.Matchers.closeTo;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class IEEE754rUtilsBaseRockGeneratedTest {

    //BaseRock generated method id: ${maxWhenArrayLengthNotEquals0ThrowsIllegalArgumentException}, hash: D97D6415FE592EFB094EDA1EC2041882
    @Test()
    void maxWhenArrayLengthNotEquals0ThrowsIllegalArgumentException() {
        /* Branches:
         * (array.length != 0) : true
         */
         //Arrange Statement(s)
        double[] doubleArray = new double[] { Double.parseDouble("0") };
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            IEEE754rUtils.max(doubleArray);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${maxWhenArrayLengthEquals0ThrowsIllegalArgumentException}, hash: 7343447DB0191F7624F61E6ADCC92E3E
    @Test()
    void maxWhenArrayLengthEquals0ThrowsIllegalArgumentException() {
        /* Branches:
         * (array.length != 0) : false
         */
         //Arrange Statement(s)
        double[] doubleArray = new double[] {};
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            IEEE754rUtils.max(doubleArray);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${max1WhenDoubleIsNaNA}, hash: 66A668F18AC5BCF28A5CDAA7D0D3E850
    @Test()
    void max1WhenDoubleIsNaNA() {
        /* Branches:
         * (Double.isNaN(a)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        double result = IEEE754rUtils.max(Double.parseDouble("0.0"), Double.parseDouble("0.0"));
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, closeTo(Double.parseDouble("0.0"), 0.00001)));
    }

    //BaseRock generated method id: ${max1WhenDoubleIsNaNB}, hash: 6084F634F8D4C59188A8FBBA4E0C9A86
    @Test()
    void max1WhenDoubleIsNaNB() {
        /* Branches:
         * (Double.isNaN(a)) : false
         * (Double.isNaN(b)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        double result = IEEE754rUtils.max(Double.parseDouble("0.0"), Double.parseDouble("0.0"));
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, closeTo(Double.parseDouble("0.0"), 0.00001)));
    }

    //BaseRock generated method id: ${max1WhenDoubleNotIsNaNB}, hash: 718C3FEE059757AFE458F06BB5954AFF
    @Test()
    void max1WhenDoubleNotIsNaNB() {
        /* Branches:
         * (Double.isNaN(a)) : false
         * (Double.isNaN(b)) : false
         */
         
        //Act Statement(s)
        double result = IEEE754rUtils.max(Double.parseDouble("0.0"), Double.parseDouble("0.0"));
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, closeTo(Double.parseDouble("0.0"), 0.00001)));
    }

    //BaseRock generated method id: ${max2Test}, hash: 7575755015AB4A7FEA29C770EE71459A
    @Test()
    void max2Test() {
        //Arrange Statement(s)
        try (MockedStatic<IEEE754rUtils> iEEE754rUtils = mockStatic(IEEE754rUtils.class, CALLS_REAL_METHODS)) {
            iEEE754rUtils.when(() -> IEEE754rUtils.max(Double.parseDouble("0.0"), Double.parseDouble("0.0"))).thenReturn(Double.parseDouble("0.0"));
            //Act Statement(s)
            double result = IEEE754rUtils.max(Double.parseDouble("0.0"), Double.parseDouble("0.0"), Double.parseDouble("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, closeTo(Double.parseDouble("0.0"), 0.00001));
                iEEE754rUtils.verify(() -> IEEE754rUtils.max(Double.parseDouble("0.0"), Double.parseDouble("0.0")), atLeast(2));
            });
        }
    }

    //BaseRock generated method id: ${max3WhenArrayLengthNotEquals0ThrowsIllegalArgumentException}, hash: 66C31F700BA35598EBEE22BA8CD0FF07
    @Test()
    void max3WhenArrayLengthNotEquals0ThrowsIllegalArgumentException() {
        /* Branches:
         * (array.length != 0) : true
         */
         //Arrange Statement(s)
        float[] floatArray = new float[] { Float.parseFloat("0") };
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            IEEE754rUtils.max(floatArray);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${max3WhenArrayLengthEquals0ThrowsIllegalArgumentException}, hash: 16ED13A87A41A71157E42CA86E4D71DE
    @Test()
    void max3WhenArrayLengthEquals0ThrowsIllegalArgumentException() {
        /* Branches:
         * (array.length != 0) : false
         */
         //Arrange Statement(s)
        float[] floatArray = new float[] {};
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            IEEE754rUtils.max(floatArray);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${max4WhenFloatIsNaNA}, hash: EB72F89B832A14B35AABEAE9381984A3
    @Test()
    void max4WhenFloatIsNaNA() {
        /* Branches:
         * (Float.isNaN(a)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        float result = IEEE754rUtils.max(Float.parseFloat("0.0"), Float.parseFloat("0.0"));
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Float.parseFloat("0.0"))));
    }

    //BaseRock generated method id: ${max4WhenFloatIsNaNB}, hash: C70B1989C98A3921A925F533DE8CA0CC
    @Test()
    void max4WhenFloatIsNaNB() {
        /* Branches:
         * (Float.isNaN(a)) : false
         * (Float.isNaN(b)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        float result = IEEE754rUtils.max(Float.parseFloat("0.0"), Float.parseFloat("0.0"));
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Float.parseFloat("0.0"))));
    }

    //BaseRock generated method id: ${max4WhenFloatNotIsNaNB}, hash: 33579BD28B269B14527AE3F0E555F50D
    @Test()
    void max4WhenFloatNotIsNaNB() {
        /* Branches:
         * (Float.isNaN(a)) : false
         * (Float.isNaN(b)) : false
         */
         
        //Act Statement(s)
        float result = IEEE754rUtils.max(Float.parseFloat("0.0"), Float.parseFloat("0.0"));
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Float.parseFloat("0.0"))));
    }

    //BaseRock generated method id: ${max5Test}, hash: AAD8618657BD5CA849842504F8D16778
    @Test()
    void max5Test() {
        //Arrange Statement(s)
        try (MockedStatic<IEEE754rUtils> iEEE754rUtils = mockStatic(IEEE754rUtils.class, CALLS_REAL_METHODS)) {
            iEEE754rUtils.when(() -> IEEE754rUtils.max(Float.parseFloat("0.0"), Float.parseFloat("0.0"))).thenReturn(Float.parseFloat("0.0"));
            //Act Statement(s)
            float result = IEEE754rUtils.max(Float.parseFloat("0.0"), Float.parseFloat("0.0"), Float.parseFloat("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Float.parseFloat("0.0")));
                iEEE754rUtils.verify(() -> IEEE754rUtils.max(Float.parseFloat("0.0"), Float.parseFloat("0.0")), atLeast(2));
            });
        }
    }

    //BaseRock generated method id: ${minWhenArrayLengthNotEquals0ThrowsIllegalArgumentException}, hash: C3D7222401A375E77B380786C981881C
    @Test()
    void minWhenArrayLengthNotEquals0ThrowsIllegalArgumentException() {
        /* Branches:
         * (array.length != 0) : true
         */
         //Arrange Statement(s)
        double[] doubleArray = new double[] { Double.parseDouble("0") };
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            IEEE754rUtils.min(doubleArray);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${minWhenArrayLengthEquals0ThrowsIllegalArgumentException}, hash: A05C8F685D264C73D866E95D13ABB294
    @Test()
    void minWhenArrayLengthEquals0ThrowsIllegalArgumentException() {
        /* Branches:
         * (array.length != 0) : false
         */
         //Arrange Statement(s)
        double[] doubleArray = new double[] {};
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            IEEE754rUtils.min(doubleArray);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${min1WhenDoubleIsNaNA}, hash: FABC3867640D41E11A7944A682F143D7
    @Test()
    void min1WhenDoubleIsNaNA() {
        /* Branches:
         * (Double.isNaN(a)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        double result = IEEE754rUtils.min(Double.parseDouble("0.0"), Double.parseDouble("0.0"));
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, closeTo(Double.parseDouble("0.0"), 0.00001)));
    }

    //BaseRock generated method id: ${min1WhenDoubleIsNaNB}, hash: AA07A9689E142987D4D0A3F9DF6970C2
    @Test()
    void min1WhenDoubleIsNaNB() {
        /* Branches:
         * (Double.isNaN(a)) : false
         * (Double.isNaN(b)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        double result = IEEE754rUtils.min(Double.parseDouble("0.0"), Double.parseDouble("0.0"));
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, closeTo(Double.parseDouble("0.0"), 0.00001)));
    }

    //BaseRock generated method id: ${min1WhenDoubleNotIsNaNB}, hash: C6E1508BED3B54BC8640586EE28B39E4
    @Test()
    void min1WhenDoubleNotIsNaNB() {
        /* Branches:
         * (Double.isNaN(a)) : false
         * (Double.isNaN(b)) : false
         */
         
        //Act Statement(s)
        double result = IEEE754rUtils.min(Double.parseDouble("0.0"), Double.parseDouble("0.0"));
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, closeTo(Double.parseDouble("0.0"), 0.00001)));
    }

    //BaseRock generated method id: ${min2Test}, hash: 8E53BA84C532AFA7C7CFF7C463C0C795
    @Test()
    void min2Test() {
        //Arrange Statement(s)
        try (MockedStatic<IEEE754rUtils> iEEE754rUtils = mockStatic(IEEE754rUtils.class, CALLS_REAL_METHODS)) {
            iEEE754rUtils.when(() -> IEEE754rUtils.min(Double.parseDouble("0.0"), Double.parseDouble("0.0"))).thenReturn(Double.parseDouble("0.0"));
            //Act Statement(s)
            double result = IEEE754rUtils.min(Double.parseDouble("0.0"), Double.parseDouble("0.0"), Double.parseDouble("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, closeTo(Double.parseDouble("0.0"), 0.00001));
                iEEE754rUtils.verify(() -> IEEE754rUtils.min(Double.parseDouble("0.0"), Double.parseDouble("0.0")), atLeast(2));
            });
        }
    }

    //BaseRock generated method id: ${min3WhenArrayLengthNotEquals0ThrowsIllegalArgumentException}, hash: F849761E0FDC0CF167D22F1E1FC6089D
    @Test()
    void min3WhenArrayLengthNotEquals0ThrowsIllegalArgumentException() {
        /* Branches:
         * (array.length != 0) : true
         */
         //Arrange Statement(s)
        float[] floatArray = new float[] { Float.parseFloat("0") };
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            IEEE754rUtils.min(floatArray);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${min3WhenArrayLengthEquals0ThrowsIllegalArgumentException}, hash: 60ABD4C93982EA4EDAC0B73235D6C1CE
    @Test()
    void min3WhenArrayLengthEquals0ThrowsIllegalArgumentException() {
        /* Branches:
         * (array.length != 0) : false
         */
         //Arrange Statement(s)
        float[] floatArray = new float[] {};
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            IEEE754rUtils.min(floatArray);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${min4WhenFloatIsNaNA}, hash: 0778EA3E041E3CC8D81D0D1EBFC42AA7
    @Test()
    void min4WhenFloatIsNaNA() {
        /* Branches:
         * (Float.isNaN(a)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        float result = IEEE754rUtils.min(Float.parseFloat("0.0"), Float.parseFloat("0.0"));
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Float.parseFloat("0.0"))));
    }

    //BaseRock generated method id: ${min4WhenFloatIsNaNB}, hash: 37B7928547033C2887653AAF9B0CC8C0
    @Test()
    void min4WhenFloatIsNaNB() {
        /* Branches:
         * (Float.isNaN(a)) : false
         * (Float.isNaN(b)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        float result = IEEE754rUtils.min(Float.parseFloat("0.0"), Float.parseFloat("0.0"));
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Float.parseFloat("0.0"))));
    }

    //BaseRock generated method id: ${min4WhenFloatNotIsNaNB}, hash: 0A0078757D23E70FF55AE3E04DC1F680
    @Test()
    void min4WhenFloatNotIsNaNB() {
        /* Branches:
         * (Float.isNaN(a)) : false
         * (Float.isNaN(b)) : false
         */
         
        //Act Statement(s)
        float result = IEEE754rUtils.min(Float.parseFloat("0.0"), Float.parseFloat("0.0"));
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Float.parseFloat("0.0"))));
    }

    //BaseRock generated method id: ${min5Test}, hash: BEA1E59C6FFE636BEB8D52A70F1F9A66
    @Test()
    void min5Test() {
        //Arrange Statement(s)
        try (MockedStatic<IEEE754rUtils> iEEE754rUtils = mockStatic(IEEE754rUtils.class, CALLS_REAL_METHODS)) {
            iEEE754rUtils.when(() -> IEEE754rUtils.min(Float.parseFloat("0.0"), Float.parseFloat("0.0"))).thenReturn(Float.parseFloat("0.0"));
            //Act Statement(s)
            float result = IEEE754rUtils.min(Float.parseFloat("0.0"), Float.parseFloat("0.0"), Float.parseFloat("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Float.parseFloat("0.0")));
                iEEE754rUtils.verify(() -> IEEE754rUtils.min(Float.parseFloat("0.0"), Float.parseFloat("0.0")), atLeast(2));
            });
        }
    }
}
