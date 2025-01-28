package org.apache.commons.lang3.concurrent;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import org.apache.commons.lang3.exception.UncheckedInterruptedException;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.mock;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class UncheckedFutureImplBaseRockGeneratedTest {

    private final Future<Object> futureMock = mock(Future.class);

    //BaseRock generated method id: ${getTest}, hash: 4E06C4F21C98B8C46F38862CC6FC330B
    @Disabled()
    @Test()
    void getTest() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  Potential harmful system call (Future.get) detected; Learn more: https://github.com/Sapient-AI/docs#disabled-generated-tests
         *  Suggestions:
         *  This method should be avoided from unit testing. This can be covered during integration testing.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        UncheckedFutureImpl target = new UncheckedFutureImpl(futureMock);
        
        //Act Statement(s)
        Object result = target.get();
        
        //Assert statement(s)
        //TODO: Please implement equals method in Object for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getWhenCaughtInterruptedExceptionThrowsUncheckedInterruptedException}, hash: EEAE9F40916878697D1F7CFCE4D36F30
    @Disabled()
    @Test()
    void getWhenCaughtInterruptedExceptionThrowsUncheckedInterruptedException() {
        /* Branches:
         * (catch-exception (InterruptedException)) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  Potential harmful system call (Future.get) detected; Learn more: https://github.com/Sapient-AI/docs#disabled-generated-tests
         *  Suggestions:
         *  This method should be avoided from unit testing. This can be covered during integration testing.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        UncheckedFutureImpl target = new UncheckedFutureImpl(futureMock);
        InterruptedException interruptedException = new InterruptedException();
        //Act Statement(s)
        final UncheckedInterruptedException result = assertThrows(UncheckedInterruptedException.class, () -> {
            target.get();
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getCause(), is(instanceOf(interruptedException.getClass())));
        });
    }

    //BaseRock generated method id: ${getWhenCaughtExecutionExceptionThrowsUncheckedExecutionException}, hash: 0340F93ACB5F3D5E88DEF2C5148406FA
    @Disabled()
    @Test()
    void getWhenCaughtExecutionExceptionThrowsUncheckedExecutionException() {
        /* Branches:
         * (catch-exception (ExecutionException)) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  Potential harmful system call (Future.get) detected; Learn more: https://github.com/Sapient-AI/docs#disabled-generated-tests
         *  Suggestions:
         *  This method should be avoided from unit testing. This can be covered during integration testing.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        UncheckedFutureImpl target = new UncheckedFutureImpl(futureMock);
        Throwable throwable = new Throwable();
        ExecutionException executionException = new ExecutionException(throwable);
        //Act Statement(s)
        final UncheckedExecutionException result = assertThrows(UncheckedExecutionException.class, () -> {
            target.get();
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getCause(), is(instanceOf(executionException.getClass())));
        });
    }

    //BaseRock generated method id: ${get1Test}, hash: 197237E6F5CEFA980922C29CE23C80AD
    @Disabled()
    @Test()
    void get1Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  Potential harmful system call (Future.get) detected; Learn more: https://github.com/Sapient-AI/docs#disabled-generated-tests
         *  Suggestions:
         *  This method should be avoided from unit testing. This can be covered during integration testing.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        UncheckedFutureImpl target = new UncheckedFutureImpl(futureMock);
        
        //Act Statement(s)
        Object result = target.get(0L, TimeUnit.NANOSECONDS);
        
        //Assert statement(s)
        //TODO: Please implement equals method in Object for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${get1WhenCaughtInterruptedExceptionThrowsUncheckedInterruptedException}, hash: 5901CF1477D9ADAA966E6E7D075B6CD4
    @Disabled()
    @Test()
    void get1WhenCaughtInterruptedExceptionThrowsUncheckedInterruptedException() {
        /* Branches:
         * (catch-exception (InterruptedException)) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  Potential harmful system call (Future.get) detected; Learn more: https://github.com/Sapient-AI/docs#disabled-generated-tests
         *  Suggestions:
         *  This method should be avoided from unit testing. This can be covered during integration testing.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        UncheckedFutureImpl target = new UncheckedFutureImpl(futureMock);
        InterruptedException interruptedException = new InterruptedException();
        //Act Statement(s)
        final UncheckedInterruptedException result = assertThrows(UncheckedInterruptedException.class, () -> {
            target.get(0L, TimeUnit.NANOSECONDS);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getCause(), is(instanceOf(interruptedException.getClass())));
        });
    }

    //BaseRock generated method id: ${get1WhenCaughtExecutionExceptionThrowsUncheckedExecutionException}, hash: D3715844684A9BB4BFD8F1A4A1DBA044
    @Disabled()
    @Test()
    void get1WhenCaughtExecutionExceptionThrowsUncheckedExecutionException() {
        /* Branches:
         * (catch-exception (ExecutionException)) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  Potential harmful system call (Future.get) detected; Learn more: https://github.com/Sapient-AI/docs#disabled-generated-tests
         *  Suggestions:
         *  This method should be avoided from unit testing. This can be covered during integration testing.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        UncheckedFutureImpl target = new UncheckedFutureImpl(futureMock);
        Throwable throwable = new Throwable();
        ExecutionException executionException = new ExecutionException(throwable);
        //Act Statement(s)
        final UncheckedExecutionException result = assertThrows(UncheckedExecutionException.class, () -> {
            target.get(0L, TimeUnit.NANOSECONDS);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getCause(), is(instanceOf(executionException.getClass())));
        });
    }

    //BaseRock generated method id: ${get1WhenCaughtTimeoutExceptionThrowsUncheckedTimeoutException}, hash: A87088C7578470BD99C1CA96343F6071
    @Disabled()
    @Test()
    void get1WhenCaughtTimeoutExceptionThrowsUncheckedTimeoutException() {
        /* Branches:
         * (catch-exception (TimeoutException)) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  Potential harmful system call (Future.get) detected; Learn more: https://github.com/Sapient-AI/docs#disabled-generated-tests
         *  Suggestions:
         *  This method should be avoided from unit testing. This can be covered during integration testing.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        UncheckedFutureImpl target = new UncheckedFutureImpl(futureMock);
        TimeoutException timeoutException = new TimeoutException();
        //Act Statement(s)
        final UncheckedTimeoutException result = assertThrows(UncheckedTimeoutException.class, () -> {
            target.get(0L, TimeUnit.NANOSECONDS);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getCause(), is(instanceOf(timeoutException.getClass())));
        });
    }
}
