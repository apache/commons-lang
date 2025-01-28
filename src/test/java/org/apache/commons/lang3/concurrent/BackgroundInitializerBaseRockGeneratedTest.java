package org.apache.commons.lang3.concurrent;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Disabled;
import java.util.concurrent.ExecutionException;
import org.mockito.stubbing.Answer;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import org.mockito.MockedStatic;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.doReturn;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.mockito.Mockito.mockStatic;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class BackgroundInitializerBaseRockGeneratedTest {

    private final ExecutorService executorServiceMock = mock(ExecutorService.class);

    private final Future futureMock = mock(Future.class);

    //BaseRock generated method id: ${builderTest}, hash: 31D370464CCF502D1E474CF0876B3D99
    @Test()
    void builderTest() {
        
        //Act Statement(s)
        BackgroundInitializer.Builder<BackgroundInitializer<Object>, Object> result = BackgroundInitializer.builder();
        
        //Assert statement(s)
        //TODO: Please implement equals method in Builder for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getTest}, hash: CFCD9B9154B776ED7808F504A486A167
    @Disabled()
    @Test()
    void getTest() throws ConcurrentException {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  Potential harmful system call (Future.get) detected; Learn more: https://github.com/Sapient-AI/docs#disabled-generated-tests
         *  Suggestions:
         *  This method should be avoided from unit testing. This can be covered during integration testing.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        BackgroundInitializer target = spy(new BackgroundInitializer(executorServiceMock));
        doReturn(futureMock).when(target).getFuture();
        
        //Act Statement(s)
        Object result = target.get();
        
        //Assert statement(s)
        //TODO: Please implement equals method in Object for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            verify(target).getFuture();
        });
    }

    //BaseRock generated method id: ${getWhenCaughtExecutionException}, hash: 55E8C9B042AF504AF1AA2D86349962A2
    @Disabled()
    @Test()
    void getWhenCaughtExecutionException() throws ConcurrentException, InterruptedException, ExecutionException {
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
        try (MockedStatic<ConcurrentUtils> concurrentUtils = mockStatic(ConcurrentUtils.class)) {
            Throwable throwable = new Throwable();
            ExecutionException executionException = new ExecutionException(throwable);
            concurrentUtils.when(() -> ConcurrentUtils.handleCause(executionException)).thenAnswer((Answer<Void>) invocation -> null);
            BackgroundInitializer target = spy(new BackgroundInitializer(executorServiceMock));
            doReturn(futureMock).when(target).getFuture();
            Object object = new Object();
            doReturn(object).when(futureMock).get();
            //Act Statement(s)
            Object result = target.get();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(nullValue()));
                concurrentUtils.verify(() -> ConcurrentUtils.handleCause(executionException), atLeast(1));
                verify(target).getFuture();
                verify(futureMock).get();
            });
        }
    }

    //BaseRock generated method id: ${getWhenCaughtInterruptedExceptionThrowsConcurrentException}, hash: 3C002DCA75238286BD269713F366D4F7
    @Disabled()
    @Test()
    void getWhenCaughtInterruptedExceptionThrowsConcurrentException() throws ConcurrentException, InterruptedException, ExecutionException {
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
        BackgroundInitializer target = spy(new BackgroundInitializer(executorServiceMock));
        doReturn(futureMock).when(target).getFuture();
        Object object = new Object();
        doReturn(object).when(futureMock).get();
        InterruptedException interruptedException = new InterruptedException();
        //Act Statement(s)
        final ConcurrentException result = assertThrows(ConcurrentException.class, () -> {
            target.get();
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getCause(), is(instanceOf(interruptedException.getClass())));
            verify(target).getFuture();
            verify(futureMock).get();
        });
    }

    //BaseRock generated method id: ${getActiveExecutorTest}, hash: 89DD51D8A46A434DEE6BF5D44B986241
    @Test()
    void getActiveExecutorTest() {
        //Arrange Statement(s)
        BackgroundInitializer target = new BackgroundInitializer(executorServiceMock);
        
        //Act Statement(s)
        ExecutorService result = target.getActiveExecutor();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getExternalExecutorTest}, hash: 2DEB44BB831C29FC2EE78023FF9FFEB7
    @Test()
    void getExternalExecutorTest() {
        //Arrange Statement(s)
        BackgroundInitializer target = new BackgroundInitializer(executorServiceMock);
        
        //Act Statement(s)
        ExecutorService result = target.getExternalExecutor();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(executorServiceMock)));
    }

    //BaseRock generated method id: ${getFutureWhenFutureIsNullThrowsIllegalStateException}, hash: 1ACA9A6000A54154B1CFD731256FF8CD
    @Test()
    void getFutureWhenFutureIsNullThrowsIllegalStateException() {
        /* Branches:
         * (future == null) : true
         */
         //Arrange Statement(s)
        BackgroundInitializer target = new BackgroundInitializer(executorServiceMock);
        IllegalStateException illegalStateException = new IllegalStateException("start() must be called first!");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            target.getFuture();
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getTaskCountTest}, hash: 5156CF248E5CD1A6A2948DD74C4EF1FC
    @Test()
    void getTaskCountTest() {
        //Arrange Statement(s)
        BackgroundInitializer target = new BackgroundInitializer(executorServiceMock);
        
        //Act Statement(s)
        int result = target.getTaskCount();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1)));
    }

    //BaseRock generated method id: ${getTypedException1Test}, hash: 8A690821E0C488EA26E98BF424703C26
    @Test()
    void getTypedException1Test() {
        //Arrange Statement(s)
        BackgroundInitializer target = new BackgroundInitializer(executorServiceMock);
        Exception exception = new Exception();
        
        //Act Statement(s)
        Exception result = target.getTypedException(exception);
        
        //Assert statement(s)
        //TODO: Please implement equals method in Exception for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${isInitializedWhenFutureIsNull}, hash: 79CF90B1F7ABC0E0CF72BE42F06CAA8A
    @Test()
    void isInitializedWhenFutureIsNull() {
        /* Branches:
         * (future == null) : true
         */
         //Arrange Statement(s)
        BackgroundInitializer target = new BackgroundInitializer(executorServiceMock);
        
        //Act Statement(s)
        boolean result = target.isInitialized();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isStartedWhenFutureIsNull}, hash: E6901B487DD7617AA4F8DA8376883F5E
    @Test()
    void isStartedWhenFutureIsNull() {
        /* Branches:
         * (future != null) : false
         */
         //Arrange Statement(s)
        BackgroundInitializer target = new BackgroundInitializer(executorServiceMock);
        
        //Act Statement(s)
        boolean result = target.isStarted();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${setExternalExecutorWhenIsStartedThrowsIllegalStateException}, hash: 95B6A63C4139F00725BA0D048A8239DE
    @Test()
    void setExternalExecutorWhenIsStartedThrowsIllegalStateException() {
        /* Branches:
         * (isStarted()) : true
         */
         //Arrange Statement(s)
        BackgroundInitializer target = spy(new BackgroundInitializer(executorServiceMock));
        doReturn(true).when(target).isStarted();
        IllegalStateException illegalStateException = new IllegalStateException("Cannot set ExecutorService after start()!");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            target.setExternalExecutor(executorServiceMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
            verify(target).isStarted();
        });
    }

    //BaseRock generated method id: ${setExternalExecutorWhenIsStartedNot}, hash: AA2B81A23729913E0050412811D0499A
    @Test()
    void setExternalExecutorWhenIsStartedNot() {
        /* Branches:
         * (isStarted()) : false
         */
         //Arrange Statement(s)
        BackgroundInitializer target = spy(new BackgroundInitializer(executorServiceMock));
        doReturn(false).when(target).isStarted();
        
        //Act Statement(s)
        target.setExternalExecutor(executorServiceMock);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(target.getExternalExecutor(), is(notNullValue()));
            verify(target).isStarted();
        });
    }

    //BaseRock generated method id: ${startWhenIsStarted}, hash: 1895313B750BFD3E05093AFECD2026A5
    @Test()
    void startWhenIsStarted() {
        /* Branches:
         * (!isStarted()) : false
         */
         //Arrange Statement(s)
        BackgroundInitializer target = spy(new BackgroundInitializer(executorServiceMock));
        doReturn(true).when(target).isStarted();
        
        //Act Statement(s)
        boolean result = target.start();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.FALSE));
            verify(target).isStarted();
        });
    }

    //BaseRock generated method id: ${startWhenExecutorIsNull}, hash: 95E43F6D1240288864E5020F764C4A36
    @Disabled()
    @Test()
    void startWhenExecutorIsNull() {
        /* Branches:
         * (!isStarted()) : true
         * (executor == null) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  Potential harmful system call (ExecutorService.submit) detected; Learn more: https://github.com/Sapient-AI/docs#disabled-generated-tests
         *  Suggestions:
         *  This method should be avoided from unit testing. This can be covered during integration testing.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        BackgroundInitializer target = spy(new BackgroundInitializer((ExecutorService) null));
        doReturn(false).when(target).isStarted();
        
        //Act Statement(s)
        boolean result = target.start();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.TRUE));
            verify(target).isStarted();
        });
    }

    //BaseRock generated method id: ${startWhenExecutorIsNotNull}, hash: 913F7913919047640C9202DB7ACA42DA
    @Disabled()
    @Test()
    void startWhenExecutorIsNotNull() {
        /* Branches:
         * (!isStarted()) : true
         * (executor == null) : false
         *
         * TODO: Help needed! This method is not unit testable!
         *  Potential harmful system call (ExecutorService.submit) detected; Learn more: https://github.com/Sapient-AI/docs#disabled-generated-tests
         *  Suggestions:
         *  This method should be avoided from unit testing. This can be covered during integration testing.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        BackgroundInitializer target = spy(new BackgroundInitializer(executorServiceMock));
        doReturn(false).when(target).isStarted();
        
        //Act Statement(s)
        boolean result = target.start();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.TRUE));
            verify(target).isStarted();
        });
    }
}
