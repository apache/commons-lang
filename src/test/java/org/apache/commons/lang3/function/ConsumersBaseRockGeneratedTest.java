package org.apache.commons.lang3.function;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.util.function.Consumer;
import static org.mockito.Mockito.doNothing;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.mockito.Mockito.verify;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.mock;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class ConsumersBaseRockGeneratedTest {

    //BaseRock generated method id: ${acceptWhenConsumerIsNotNull}, hash: 223F2A41CD5C98252129611C0DF97232
    @Test()
    void acceptWhenConsumerIsNotNull() {
        /* Branches:
         * (consumer != null) : true
         */
         //Arrange Statement(s)
        Consumer<Object> consumerMock = mock(Consumer.class);
        Object object = new Object();
        doNothing().when(consumerMock).accept(object);
        
        //Act Statement(s)
        Consumers.accept(consumerMock, object);
        
        //Assert statement(s)
        assertAll("result", () -> verify(consumerMock).accept(object));
    }

    //BaseRock generated method id: ${nopTest}, hash: 52D6F5AE21F14801CEAD5250E27D6871
    @Test()
    void nopTest() {
        
        //Act Statement(s)
        Consumer result = Consumers.nop();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }
}
