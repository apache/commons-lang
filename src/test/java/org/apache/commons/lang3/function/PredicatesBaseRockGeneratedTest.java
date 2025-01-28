package org.apache.commons.lang3.function;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.util.function.Predicate;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class PredicatesBaseRockGeneratedTest {

    //BaseRock generated method id: ${falsePredicateTest}, hash: 794354FCFD881D720EBA5AB5E2934704
    @Test()
    void falsePredicateTest() {
        
        //Act Statement(s)
        Predicate result = Predicates.falsePredicate();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${truePredicateTest}, hash: 669A6431B39A88DBCB27E5AB17991DC4
    @Test()
    void truePredicateTest() {
        
        //Act Statement(s)
        Predicate result = Predicates.truePredicate();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }
}
