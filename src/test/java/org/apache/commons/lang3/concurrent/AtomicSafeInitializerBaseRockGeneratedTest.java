package org.apache.commons.lang3.concurrent;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class AtomicSafeInitializerBaseRockGeneratedTest {

    //BaseRock generated method id: ${builderTest}, hash: 2DAAF087E5209E05F7D7281ADF977CE5
    @Test()
    void builderTest() {
        
        //Act Statement(s)
        AtomicSafeInitializer.Builder<AtomicSafeInitializer<Object>, Object> result = AtomicSafeInitializer.builder();
        
        //Assert statement(s)
        //TODO: Please implement equals method in Builder for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getTypedException1Test}, hash: C69C177DF593DD84D4212CEAB6C97CCD
    @Test()
    void getTypedException1Test() {
        //Arrange Statement(s)
        AtomicSafeInitializer target = new AtomicSafeInitializer();
        Exception exception = new Exception();
        
        //Act Statement(s)
        ConcurrentException result = target.getTypedException(exception);
        
        //Assert statement(s)
        //TODO: Please implement equals method in ConcurrentException for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }
}
