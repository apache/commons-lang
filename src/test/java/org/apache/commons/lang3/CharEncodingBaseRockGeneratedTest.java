package org.apache.commons.lang3;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertAll;
import org.junit.jupiter.api.Disabled;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class CharEncodingBaseRockGeneratedTest {

    //BaseRock generated method id: ${isSupportedWhenNameIsNull}, hash: EF32249337907D7D433557CD5FF6A745
    @Test()
    void isSupportedWhenNameIsNull() {
        /* Branches:
         * (name == null) : true
         */
        //Act Statement(s)
        boolean result = CharEncoding.isSupported((String) null);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isSupportedWhenCharsetIsSupportedName}, hash: C4B867090BB758978BEDB17B1605FD1F
    @Disabled()
    @Test()
    void isSupportedWhenCharsetIsSupportedName() {
        /* Branches:
         * (name == null) : false
         * (Charset.isSupported(name)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        boolean result = CharEncoding.isSupported("name1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isSupportedWhenCharsetNotIsSupportedName}, hash: C3E5E2681B70D2D31ECF164E0D37F8D2
    @Test()
    void isSupportedWhenCharsetNotIsSupportedName() {
        /* Branches:
         * (name == null) : false
         * (Charset.isSupported(name)) : false
         */
        //Act Statement(s)
        boolean result = CharEncoding.isSupported("A");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isSupportedWhenCaughtIllegalCharsetNameException}, hash: 52E48538AD0D63D6B8FD559472F5F036
    @Test()
    void isSupportedWhenCaughtIllegalCharsetNameException() {
        /* Branches:
         * (name == null) : false
         * (catch-exception (IllegalCharsetNameException)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        boolean result = CharEncoding.isSupported("A");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }
}
