package org.apache.commons.lang3.concurrent;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.util.concurrent.ExecutorService;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.verify;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.doReturn;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class MultiBackgroundInitializerBaseRockGeneratedTest {

    private final BackgroundInitializer<?> backgroundInitializerMock = mock(BackgroundInitializer.class);

    private final ExecutorService executorServiceMock = mock(ExecutorService.class);

    //BaseRock generated method id: ${addInitializerWhenIsStartedThrowsIllegalStateException}, hash: 1189929F02EB1C652EF1D89457BACB9A
    @Test()
    void addInitializerWhenIsStartedThrowsIllegalStateException() {
        /* Branches:
         * (isStarted()) : true
         */
         //Arrange Statement(s)
        MultiBackgroundInitializer target = spy(new MultiBackgroundInitializer(executorServiceMock));
        doReturn(true).when(target).isStarted();
        IllegalStateException illegalStateException = new IllegalStateException("addInitializer() must not be called after start()!");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            target.addInitializer("name1", backgroundInitializerMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
            verify(target).isStarted();
        });
    }

    //BaseRock generated method id: ${addInitializerWhenIsStartedNot}, hash: 086199F63CED718F6910E031F8C5BB39
    @Test()
    void addInitializerWhenIsStartedNot() {
        /* Branches:
         * (isStarted()) : false
         */
         //Arrange Statement(s)
        MultiBackgroundInitializer target = spy(new MultiBackgroundInitializer(executorServiceMock));
        doReturn(false).when(target).isStarted();
        
        //Act Statement(s)
        target.addInitializer("name1", backgroundInitializerMock);
        
        //Assert statement(s)
        assertAll("result", () -> verify(target).isStarted());
    }

    //BaseRock generated method id: ${closeWhenExceptionIsNull}, hash: 287AA8E2BB2F96898C8B9D3ED79E6C82
    @Test()
    void closeWhenExceptionIsNull() throws ConcurrentException {
        /* Branches:
         * (for-each(childInitializers.values())) : false
         * (exception != null) : false
         */
         //Arrange Statement(s)
        MultiBackgroundInitializer target = new MultiBackgroundInitializer(executorServiceMock);
        
        //Act Statement(s)
        target.close();
    }

    //BaseRock generated method id: ${getTaskCountTest}, hash: CB57F13552AC5FB5767D94769D3D6E6A
    @Test()
    void getTaskCountTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        MultiBackgroundInitializer target = new MultiBackgroundInitializer(executorServiceMock);
        
        //Act Statement(s)
        int result = target.getTaskCount();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1)));
    }

    //BaseRock generated method id: ${initialize1Test}, hash: 138BC3459E797893D9BA1EB1C3BCE952
    @Test()
    void initialize1Test() throws Exception {
        //Arrange Statement(s)
        MultiBackgroundInitializer target = new MultiBackgroundInitializer(executorServiceMock);
        
        //Act Statement(s)
        MultiBackgroundInitializer.MultiBackgroundInitializerResults result = target.initialize();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${isInitializedWhenChildInitializersIsEmpty}, hash: F16102F675C22B6B1332770A2C999C65
    @Test()
    void isInitializedWhenChildInitializersIsEmpty() {
        /* Branches:
         * (childInitializers.isEmpty()) : true
         */
         //Arrange Statement(s)
        MultiBackgroundInitializer target = new MultiBackgroundInitializer(executorServiceMock);
        
        //Act Statement(s)
        boolean result = target.isInitialized();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }
}
