package org.apache.commons.lang3.builder;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.mock;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class IDKeyBaseRockGeneratedTest {

    private final IDKey iDKeyMock = mock(IDKey.class);

    //BaseRock generated method id: ${equalsWhenOtherNotInstanceOfIDKey}, hash: D52CE296D42995E2258E0ED4799BFE1D
    @Test()
    void equalsWhenOtherNotInstanceOfIDKey() {
        /* Branches:
         * (!(other instanceof IDKey)) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        IDKey target = new IDKey(object);
        Object object2 = new Object();
        
        //Act Statement(s)
        boolean result = target.equals(object2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${equalsWhenIdNotEqualsIdKeyId}, hash: 13C72227E4D8D3DBEDDC88A0DB011445
    @Test()
    void equalsWhenIdNotEqualsIdKeyId() {
        /* Branches:
         * (!(other instanceof IDKey)) : false
         * (id != idKey.id) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        IDKey target = new IDKey(object);
        
        //Act Statement(s)
        boolean result = target.equals(iDKeyMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${equalsWhenValueEqualsIdKeyValue}, hash: 7B722E86A4E33E682A7FF6434FCD6633
    @Test()
    void equalsWhenValueEqualsIdKeyValue() {
        /* Branches:
         * (!(other instanceof IDKey)) : false
         * (id != idKey.id) : false
         * (value == idKey.value) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        IDKey target = new IDKey((Object) null);
        
        //Act Statement(s)
        boolean result = target.equals(iDKeyMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }
}
