package org.apache.commons.lang3.concurrent;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Callable;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.mockito.Mockito.verify;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.doReturn;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class CallableBackgroundInitializerBaseRockGeneratedTest {

    private final Callable<Object> callableMock = mock(Callable.class, "callable");

    private final ExecutorService executorServiceMock = mock(ExecutorService.class);

    //BaseRock generated method id: ${getTypedException1Test}, hash: D9261A437C9CC496DE642EE171DFBAB6
    @Test()
    void getTypedException1Test() {
        //Arrange Statement(s)
        CallableBackgroundInitializer target = new CallableBackgroundInitializer(callableMock, executorServiceMock);
        Exception exception = new Exception();
        
        //Act Statement(s)
        Exception result = target.getTypedException(exception);
        
        //Assert statement(s)
        //TODO: Please implement equals method in Exception for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${initialize1Test}, hash: 66461C4B9CBC4E32B2AA90335A81CBC0
    @Test()
    void initialize1Test() throws Exception {
        //Arrange Statement(s)
        Object object = new Object();
        doReturn(object).when(callableMock).call();
        CallableBackgroundInitializer target = new CallableBackgroundInitializer(callableMock, executorServiceMock);
        
        //Act Statement(s)
        Object result = target.initialize();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(object));
            verify(callableMock).call();
        });
    }
}
