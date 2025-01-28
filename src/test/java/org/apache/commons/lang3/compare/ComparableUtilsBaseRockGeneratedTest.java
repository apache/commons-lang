package org.apache.commons.lang3.compare;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.util.function.Predicate;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.mock;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class ComparableUtilsBaseRockGeneratedTest {

    private final Comparable comparableMock = mock(Comparable.class);

    private final Comparable comparableMock2 = mock(Comparable.class);

    //BaseRock generated method id: ${betweenTest}, hash: 08AD3C66CCBD1E1AD64733D779DBB9DF
    @Test()
    void betweenTest() {
        
        //Act Statement(s)
        Predicate result = ComparableUtils.between(comparableMock, comparableMock2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${betweenExclusiveTest}, hash: 041FEF331C7BBB742E55AEB74A05C179
    @Test()
    void betweenExclusiveTest() {
        
        //Act Statement(s)
        Predicate result = ComparableUtils.betweenExclusive(comparableMock, comparableMock2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${geTest}, hash: 967FB858A6C42E6C1E06FFEACF5DEFA7
    @Test()
    void geTest() {
        
        //Act Statement(s)
        Predicate result = ComparableUtils.ge(comparableMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${gtTest}, hash: 94C072E6C22C6B2F90EEEB830CFE0714
    @Test()
    void gtTest() {
        
        //Act Statement(s)
        Predicate result = ComparableUtils.gt(comparableMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${isTest}, hash: 4E4AB96CBE3B46A9496F1CA44FC7A34D
    @Test()
    void isTest() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  No constructor found to create an object without any exception for class org.apache.commons.lang3.compare.ComparableUtils$ComparableCheckBuilder
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        ComparableUtils.ComparableCheckBuilder result = ComparableUtils.is(comparableMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${leTest}, hash: 0E1BCE17364169D12AF3C060C6D508A9
    @Test()
    void leTest() {
        
        //Act Statement(s)
        Predicate result = ComparableUtils.le(comparableMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${ltTest}, hash: B9493E797E7DCFE5621AEFA5C6834010
    @Test()
    void ltTest() {
        
        //Act Statement(s)
        Predicate result = ComparableUtils.lt(comparableMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${maxWhenObjectUtilsCompareComparable1Comparable2FalseGreaterThan0}, hash: 7C3E543C3746377C9F4A17C7DDD83E4D
    @Test()
    void maxWhenObjectUtilsCompareComparable1Comparable2FalseGreaterThan0() {
        /* Branches:
         * (ObjectUtils.compare(comparable1, comparable2, false) > 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Comparable result = ComparableUtils.max(comparableMock, comparableMock2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(comparableMock)));
    }

    //BaseRock generated method id: ${maxWhenObjectUtilsCompareComparable1Comparable2FalseNotGreaterThan0}, hash: 08FCBF23B9B789FA91DB912CEE829DDC
    @Test()
    void maxWhenObjectUtilsCompareComparable1Comparable2FalseNotGreaterThan0() {
        /* Branches:
         * (ObjectUtils.compare(comparable1, comparable2, false) > 0) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Comparable result = ComparableUtils.max(comparableMock, comparableMock2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(comparableMock2)));
    }

    //BaseRock generated method id: ${minWhenObjectUtilsCompareComparable1Comparable2TrueLessThan0}, hash: E2FA3F0C83780F066F492D931A58CA0E
    @Test()
    void minWhenObjectUtilsCompareComparable1Comparable2TrueLessThan0() {
        /* Branches:
         * (ObjectUtils.compare(comparable1, comparable2, true) < 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Comparable result = ComparableUtils.min(comparableMock, comparableMock2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(comparableMock)));
    }

    //BaseRock generated method id: ${minWhenObjectUtilsCompareComparable1Comparable2TrueNotLessThan0}, hash: B725C02F1016C78DCB01D77DF6B2B9F6
    @Test()
    void minWhenObjectUtilsCompareComparable1Comparable2TrueNotLessThan0() {
        /* Branches:
         * (ObjectUtils.compare(comparable1, comparable2, true) < 0) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Comparable result = ComparableUtils.min(comparableMock, comparableMock2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(comparableMock2)));
    }
}
