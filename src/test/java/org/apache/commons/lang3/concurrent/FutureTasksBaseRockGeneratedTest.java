package org.apache.commons.lang3.concurrent;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import java.util.concurrent.Callable;
import java.util.concurrent.FutureTask;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.mock;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class FutureTasksBaseRockGeneratedTest {

    //BaseRock generated method id: ${runTest}, hash: 50C9D72B9FED366799DA8A9A86124D70
    @Disabled()
    @Test()
    void runTest() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  Potential harmful system call (FutureTask.run) detected; Learn more: https://github.com/Sapient-AI/docs#disabled-generated-tests
         *  Suggestions:
         *  This method should be avoided from unit testing. This can be covered during integration testing.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Callable<Object> callableMock = mock(Callable.class);
        
        //Act Statement(s)
        FutureTask result = FutureTasks.run(callableMock);
        
        //Assert statement(s)
        //TODO: Please implement equals method in FutureTask for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }
}
