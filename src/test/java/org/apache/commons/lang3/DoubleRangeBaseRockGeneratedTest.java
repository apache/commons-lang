package org.apache.commons.lang3;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mockStatic;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class DoubleRangeBaseRockGeneratedTest {

    //BaseRock generated method id: ${of2Test}, hash: 22D0117E49A6A64914CCF275996D6BB0
    @Test()
    void of2Test() {
        //Arrange Statement(s)
        try (MockedStatic<DoubleRange> doubleRange = mockStatic(DoubleRange.class, CALLS_REAL_METHODS)) {
            DoubleRange doubleRange2 = DoubleRange.of(Double.parseDouble("0.0"), Double.parseDouble("0.0"));
            doubleRange.when(() -> DoubleRange.of(Double.parseDouble("0.0"), Double.parseDouble("0.0"))).thenReturn(doubleRange2);
            //Act Statement(s)
            DoubleRange result = DoubleRange.of(Double.parseDouble("0.0"), Double.parseDouble("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(doubleRange2));
                doubleRange.verify(() -> DoubleRange.of(Double.parseDouble("0.0"), Double.parseDouble("0.0")), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${of3WhenDefaultBranch}, hash: 160F80E809D93A23C0FD5BEE477D2858
    @Test()
    void of3WhenDefaultBranch() {
        /* Branches:
         * (branch expression (line 224)) : false  #  inside <init> method
         * (branch expression (line 229)) : false  #  inside <init> method
         */
         
        //Act Statement(s)
        DoubleRange result = DoubleRange.of(Double.parseDouble("0.5"), Double.parseDouble("0.5"));
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${of3WhenDefaultBranchAndDefaultBranch}, hash: 7E21FE52D2F142DF41274C50114AE013
    @Test()
    void of3WhenDefaultBranchAndDefaultBranch() {
        /* Branches:
         * (branch expression (line 224)) : false  #  inside <init> method
         * (branch expression (line 229)) : false  #  inside <init> method
         */
         
        //Act Statement(s)
        DoubleRange result = DoubleRange.of(Double.parseDouble("0.0"), Double.parseDouble("0.0"));
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }
}
