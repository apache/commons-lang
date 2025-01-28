package org.apache.commons.lang3.concurrent;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.verify;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.doReturn;
import static org.hamcrest.Matchers.closeTo;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class TimedSemaphoreBaseRockGeneratedTest {

    private final ScheduledExecutorService scheduledExecutorServiceMock = mock(ScheduledExecutorService.class);

    private final ScheduledFuture<?> scheduledFutureMock = mock(ScheduledFuture.class);

    //BaseRock generated method id: ${acquireWhenIsShutdownThrowsIllegalStateException}, hash: C5F51582286696F460D1D4119167C4E2
    @Test()
    void acquireWhenIsShutdownThrowsIllegalStateException() throws InterruptedException {
        /* Branches:
         * (isShutdown()) : true  #  inside prepareAcquire method
         */
         //Arrange Statement(s)
        TimedSemaphore target = spy(new TimedSemaphore(scheduledExecutorServiceMock, 1L, TimeUnit.NANOSECONDS, 0));
        doReturn(true).when(target).isShutdown();
        IllegalStateException illegalStateException = new IllegalStateException("TimedSemaphore is shut down!");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            target.acquire();
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
            verify(target).isShutdown();
        });
    }

    //BaseRock generated method id: ${acquireWhenCanPassAndCanPass}, hash: 00C6B82903AE2F09A47B5A9D50AE0148
    @Test()
    void acquireWhenCanPassAndCanPass() throws InterruptedException {
        /* Branches:
         * (isShutdown()) : false  #  inside prepareAcquire method
         * (task == null) : true  #  inside prepareAcquire method
         * (getLimit() <= NO_LIMIT) : false  #  inside acquirePermit method
         * (acquireCount < getLimit()) : true  #  inside acquirePermit method
         * (!canPass) : false
         * (!canPass) : false
         */
         //Arrange Statement(s)
        TimedSemaphore target = spy(new TimedSemaphore(scheduledExecutorServiceMock, 1L, TimeUnit.NANOSECONDS, 1));
        doReturn(false).when(target).isShutdown();
        doReturn(scheduledFutureMock).when(target).startTimer();
        
        //Act Statement(s)
        target.acquire();
        
        //Assert statement(s)
        assertAll("result", () -> {
            verify(target).isShutdown();
            verify(target).startTimer();
        });
    }

    //BaseRock generated method id: ${endOfPeriodTest}, hash: F88060125C426AD07FFB352C4A93DEBC
    @Test()
    void endOfPeriodTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimedSemaphore target = new TimedSemaphore(scheduledExecutorServiceMock, 1L, TimeUnit.NANOSECONDS, 0);
        
        //Act Statement(s)
        target.endOfPeriod();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(target.getAcquireCount(), equalTo(0)));
    }

    //BaseRock generated method id: ${getAcquireCountTest}, hash: 835648E86B2E1544B72ADFD1BEADEC8D
    @Test()
    void getAcquireCountTest() {
        //Arrange Statement(s)
        TimedSemaphore target = new TimedSemaphore(scheduledExecutorServiceMock, 1L, TimeUnit.NANOSECONDS, 0);
        
        //Act Statement(s)
        int result = target.getAcquireCount();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${getAvailablePermitsTest}, hash: 7D9525AAD352B3064CDC251F9A7499A5
    @Test()
    void getAvailablePermitsTest() {
        //Arrange Statement(s)
        TimedSemaphore target = new TimedSemaphore(scheduledExecutorServiceMock, 1L, TimeUnit.NANOSECONDS, 1);
        
        //Act Statement(s)
        int result = target.getAvailablePermits();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1)));
    }

    //BaseRock generated method id: ${getAverageCallsPerPeriodWhenPeriodCountEquals0}, hash: 561A7B5A5694FBA58E5DE28905177A07
    @Test()
    void getAverageCallsPerPeriodWhenPeriodCountEquals0() {
        /* Branches:
         * (periodCount == 0) : true
         */
         //Arrange Statement(s)
        TimedSemaphore target = new TimedSemaphore(scheduledExecutorServiceMock, 1L, TimeUnit.NANOSECONDS, 0);
        
        //Act Statement(s)
        double result = target.getAverageCallsPerPeriod();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, closeTo(Double.parseDouble("0.0"), 0.00001)));
    }

    //BaseRock generated method id: ${getExecutorServiceTest}, hash: 4DB6B4B8E874F263C752A2744FEDF2A0
    @Test()
    void getExecutorServiceTest() {
        //Arrange Statement(s)
        TimedSemaphore target = new TimedSemaphore(scheduledExecutorServiceMock, 1L, TimeUnit.NANOSECONDS, 0);
        
        //Act Statement(s)
        ScheduledExecutorService result = target.getExecutorService();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(scheduledExecutorServiceMock)));
    }

    //BaseRock generated method id: ${getLastAcquiresPerPeriodTest}, hash: 28D973F33A907D9F7952F34FBF9A851D
    @Test()
    void getLastAcquiresPerPeriodTest() {
        //Arrange Statement(s)
        TimedSemaphore target = new TimedSemaphore(scheduledExecutorServiceMock, 1L, TimeUnit.NANOSECONDS, 0);
        
        //Act Statement(s)
        int result = target.getLastAcquiresPerPeriod();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${getLimitTest}, hash: 832AD91305C2FD894C2A3CB84E82532D
    @Test()
    void getLimitTest() {
        //Arrange Statement(s)
        TimedSemaphore target = new TimedSemaphore(scheduledExecutorServiceMock, 1L, TimeUnit.NANOSECONDS, 0);
        
        //Act Statement(s)
        int result = target.getLimit();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${getPeriodTest}, hash: 7E34279DAAF344FB91DB5FE6C4AECA35
    @Test()
    void getPeriodTest() {
        //Arrange Statement(s)
        TimedSemaphore target = new TimedSemaphore(scheduledExecutorServiceMock, 1L, TimeUnit.NANOSECONDS, 0);
        
        //Act Statement(s)
        long result = target.getPeriod();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1L)));
    }

    //BaseRock generated method id: ${getUnitTest}, hash: 12DB5C2F04EF38B66C8CB443C5A1DB19
    @Test()
    void getUnitTest() {
        //Arrange Statement(s)
        TimedSemaphore target = new TimedSemaphore(scheduledExecutorServiceMock, 1L, TimeUnit.NANOSECONDS, 0);
        
        //Act Statement(s)
        TimeUnit result = target.getUnit();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(TimeUnit.NANOSECONDS)));
    }

    //BaseRock generated method id: ${isShutdownTest}, hash: 5C24519748FE7DA79176ABE8FE326F2E
    @Test()
    void isShutdownTest() {
        //Arrange Statement(s)
        TimedSemaphore target = new TimedSemaphore(scheduledExecutorServiceMock, 1L, TimeUnit.NANOSECONDS, 0);
        
        //Act Statement(s)
        boolean result = target.isShutdown();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${setLimitTest}, hash: A5CE5EC7CEED199725F9C23D942A673E
    @Test()
    void setLimitTest() {
        //Arrange Statement(s)
        TimedSemaphore target = new TimedSemaphore(scheduledExecutorServiceMock, 1L, TimeUnit.NANOSECONDS, 0);
        
        //Act Statement(s)
        target.setLimit(0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(target.getLimit(), equalTo(0)));
    }

    //BaseRock generated method id: ${shutdownWhenTaskIsNull}, hash: 9C3D8BA9C72BE83079AAD93E802C3A2D
    @Test()
    void shutdownWhenTaskIsNull() {
        /* Branches:
         * (!shutdown) : true
         * (ownExecutor) : false
         * (task != null) : false
         */
         //Arrange Statement(s)
        TimedSemaphore target = new TimedSemaphore(scheduledExecutorServiceMock, 1L, TimeUnit.NANOSECONDS, 0);
        
        //Act Statement(s)
        target.shutdown();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(target.isShutdown(), equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${startTimerTest}, hash: 70E5C212F20A9CEB32D0B06F9206ABE1
    @Test()
    void startTimerTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimedSemaphore target = new TimedSemaphore(scheduledExecutorServiceMock, 1L, TimeUnit.NANOSECONDS, 0);
        
        //Act Statement(s)
        ScheduledFuture<?> result = target.startTimer();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${tryAcquireWhenIsShutdownThrowsIllegalStateException}, hash: 6001FAB02FA6FC62DA16918405F434CA
    @Test()
    void tryAcquireWhenIsShutdownThrowsIllegalStateException() {
        /* Branches:
         * (isShutdown()) : true  #  inside prepareAcquire method
         */
         //Arrange Statement(s)
        TimedSemaphore target = spy(new TimedSemaphore(scheduledExecutorServiceMock, 1L, TimeUnit.NANOSECONDS, 0));
        doReturn(true).when(target).isShutdown();
        IllegalStateException illegalStateException = new IllegalStateException("TimedSemaphore is shut down!");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            target.tryAcquire();
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
            verify(target).isShutdown();
        });
    }

    //BaseRock generated method id: ${tryAcquireWhenAcquireCountLessThanGetLimit}, hash: 493686E1CFDFACBC767C96A530BBE70D
    @Test()
    void tryAcquireWhenAcquireCountLessThanGetLimit() {
        /* Branches:
         * (isShutdown()) : false  #  inside prepareAcquire method
         * (task == null) : true  #  inside prepareAcquire method
         * (getLimit() <= NO_LIMIT) : false  #  inside acquirePermit method
         * (acquireCount < getLimit()) : true  #  inside acquirePermit method
         */
         //Arrange Statement(s)
        TimedSemaphore target = spy(new TimedSemaphore(scheduledExecutorServiceMock, 1L, TimeUnit.NANOSECONDS, 1));
        doReturn(false).when(target).isShutdown();
        doReturn(scheduledFutureMock).when(target).startTimer();
        
        //Act Statement(s)
        boolean result = target.tryAcquire();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.TRUE));
            verify(target).isShutdown();
            verify(target).startTimer();
        });
    }
}
