package org.apache.commons.lang3.concurrent;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import static org.mockito.Mockito.doNothing;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.times;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class ThresholdCircuitBreakerBaseRockGeneratedTest {

    //BaseRock generated method id: ${checkStateWhenIsOpenNot}, hash: E669216CF7A7FF1F18CFE7D0EA89F32B
    @Test()
    void checkStateWhenIsOpenNot() {
        /* Branches:
         * (!isOpen()) : true
         */
         //Arrange Statement(s)
        ThresholdCircuitBreaker target = spy(new ThresholdCircuitBreaker(0L));
        doReturn(false).when(target).isOpen();
        
        //Act Statement(s)
        boolean result = target.checkState();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.TRUE));
            verify(target).isOpen();
        });
    }

    //BaseRock generated method id: ${checkStateWhenIsOpen}, hash: 6B0CAAA280BA0C506BA420FC448423DF
    @Test()
    void checkStateWhenIsOpen() {
        /* Branches:
         * (!isOpen()) : false
         */
         //Arrange Statement(s)
        ThresholdCircuitBreaker target = spy(new ThresholdCircuitBreaker(0L));
        doReturn(true).when(target).isOpen();
        
        //Act Statement(s)
        boolean result = target.checkState();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.FALSE));
            verify(target).isOpen();
        });
    }

    //BaseRock generated method id: ${closeTest}, hash: 70DF4F9551842493E2B646BC687E7145
    @Test()
    void closeTest() {
        //Arrange Statement(s)
        ThresholdCircuitBreaker target = spy(new ThresholdCircuitBreaker(0L));
        doNothing().when(target).changeState(AbstractCircuitBreaker.State.CLOSED);
        
        //Act Statement(s)
        target.close();
        
        //Assert statement(s)
        assertAll("result", () -> verify(target).changeState(AbstractCircuitBreaker.State.CLOSED));
    }

    //BaseRock generated method id: ${getThresholdTest}, hash: 89F19B6FBBF1164B9C34DB329A25990E
    @Test()
    void getThresholdTest() {
        //Arrange Statement(s)
        ThresholdCircuitBreaker target = new ThresholdCircuitBreaker(0L);
        
        //Act Statement(s)
        long result = target.getThreshold();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0L)));
    }

    //BaseRock generated method id: ${incrementAndCheckState1WhenCheckState}, hash: 1D71CE75F51501AEF4B3A0A6E4BF9113
    @Test()
    void incrementAndCheckState1WhenCheckState() {
        /* Branches:
         * (threshold == 0) : true
         * (used > threshold) : true
         * (checkState()) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        ThresholdCircuitBreaker target = spy(new ThresholdCircuitBreaker(0L));
        doNothing().when(target).open();
        doReturn(true).when(target).checkState();
        
        //Act Statement(s)
        boolean result = target.incrementAndCheckState(0L);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.TRUE));
            verify(target, times(2)).open();
            verify(target).checkState();
        });
    }

    //BaseRock generated method id: ${incrementAndCheckState1WhenCheckStateNot}, hash: F0D75A2B4DB01364EC1B705D30C7BED4
    @Test()
    void incrementAndCheckState1WhenCheckStateNot() {
        /* Branches:
         * (threshold == 0) : true
         * (used > threshold) : true
         * (checkState()) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        ThresholdCircuitBreaker target = spy(new ThresholdCircuitBreaker(0L));
        doNothing().when(target).open();
        doReturn(false).when(target).checkState();
        
        //Act Statement(s)
        boolean result = target.incrementAndCheckState(0L);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.FALSE));
            verify(target, times(2)).open();
            verify(target).checkState();
        });
    }
}
