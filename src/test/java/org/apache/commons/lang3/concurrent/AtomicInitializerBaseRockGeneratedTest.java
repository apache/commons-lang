package org.apache.commons.lang3.concurrent;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class AtomicInitializerBaseRockGeneratedTest {

    //BaseRock generated method id: ${builderTest}, hash: 31C71A146B6D757E732613E2DBFAE463
    @Test()
    void builderTest() {
        
        //Act Statement(s)
        AtomicInitializer.Builder<AtomicInitializer<Object>, Object> result = AtomicInitializer.builder();
        
        //Assert statement(s)
        //TODO: Please implement equals method in Builder for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getTypedException1Test}, hash: EDC9FF66234A0792A00D68CDCD5DE9CF
    @Test()
    void getTypedException1Test() {
        //Arrange Statement(s)
        AtomicInitializer target = new AtomicInitializer();
        Exception exception = new Exception();
        
        //Act Statement(s)
        ConcurrentException result = target.getTypedException(exception);
        
        //Assert statement(s)
        //TODO: Please implement equals method in ConcurrentException for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }
}
