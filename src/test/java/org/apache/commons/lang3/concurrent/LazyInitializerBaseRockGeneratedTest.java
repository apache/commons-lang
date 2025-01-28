package org.apache.commons.lang3.concurrent;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.mockito.Mockito.verify;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.doReturn;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class LazyInitializerBaseRockGeneratedTest {

    //BaseRock generated method id: ${builderTest}, hash: 52C297293AEF0C2432C5A4AADE51C7A0
    @Test()
    void builderTest() {
        
        //Act Statement(s)
        LazyInitializer.Builder<LazyInitializer<Object>, Object> result = LazyInitializer.builder();
        
        //Assert statement(s)
        //TODO: Please implement equals method in Builder for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getWhenResultEqualsNO_INIT}, hash: 88DDF3DCC035C0D5E128CA94771CA29C
    @Test()
    void getWhenResultEqualsNO_INIT() throws Exception {
        /* Branches:
         * (result == NO_INIT) : true
         * (result == NO_INIT) : true
         */
         //Arrange Statement(s)
        LazyInitializer target = spy(new LazyInitializer());
        Object object = new Object();
        doReturn(object).when(target).initialize();
        
        //Act Statement(s)
        Object result = target.get();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(object));
            verify(target).initialize();
        });
    }

    //BaseRock generated method id: ${getTypedException1Test}, hash: 44FAE28A18C9801B8D5786E6C84D9A7B
    @Test()
    void getTypedException1Test() {
        //Arrange Statement(s)
        LazyInitializer target = new LazyInitializer();
        Exception exception = new Exception();
        
        //Act Statement(s)
        ConcurrentException result = target.getTypedException(exception);
        
        //Assert statement(s)
        //TODO: Please implement equals method in ConcurrentException for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${isInitializedWhenObjectNotEqualsNO_INIT}, hash: 4A6E9FD497575308F319F836FE50786F
    @Test()
    void isInitializedWhenObjectNotEqualsNO_INIT() {
        /* Branches:
         * (object != NO_INIT) : true
         */
         //Arrange Statement(s)
        LazyInitializer target = new LazyInitializer();
        
        //Act Statement(s)
        boolean result = target.isInitialized();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isInitializedWhenObjectEqualsNO_INIT}, hash: 31BDA98D348260B5D6251AB7D00DBFD8
    @Test()
    void isInitializedWhenObjectEqualsNO_INIT() {
        /* Branches:
         * (object != NO_INIT) : false
         */
         //Arrange Statement(s)
        LazyInitializer target = new LazyInitializer();
        
        //Act Statement(s)
        boolean result = target.isInitialized();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }
}
