package org.apache.commons.lang3.time;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Disabled;
import org.apache.commons.lang3.function.FailableSupplier;
import java.time.Duration;
import java.util.function.Supplier;
import org.apache.commons.lang3.function.FailableRunnable;
import java.util.concurrent.TimeUnit;
import org.mockito.MockedStatic;
import java.time.Instant;
import static org.mockito.Mockito.doNothing;
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
import static org.mockito.Mockito.mockStatic;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class StopWatchBaseRockGeneratedTest {

    private final FailableRunnable<Throwable> failableRunnableMock = mock(FailableRunnable.class);

    private final Runnable runnableMock = mock(Runnable.class);

    private final Supplier<Object> supplierMock = mock(Supplier.class);

    //BaseRock generated method id: ${createTest}, hash: 45463F709BFD70AB0507F63277B30000
    @Test()
    void createTest() {
        
        //Act Statement(s)
        StopWatch result = StopWatch.create();
        
        //Assert statement(s)
        //TODO: Please implement equals method in StopWatch for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${createStartedTest}, hash: B7AF9EDACE9E6460F75C62106491507D
    @Test()
    void createStartedTest() {
        
        //Act Statement(s)
        StopWatch result = StopWatch.createStarted();
        
        //Assert statement(s)
        //TODO: Please implement equals method in StopWatch for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${formatSplitTimeTest}, hash: D4411A036889D7B83A078ECC70587497
    @Test()
    void formatSplitTimeTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StopWatch target = spy(new StopWatch("message1"));
        Duration duration = Duration.ofDays(0L);
        doReturn(duration).when(target).getSplitDuration();
        
        //Act Statement(s)
        String result = target.formatSplitTime();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("00:00:00.001"));
            verify(target).getSplitDuration();
        });
    }

    //BaseRock generated method id: ${formatTimeTest}, hash: 14318977B37F079878E0BD4613D14E4A
    @Test()
    void formatTimeTest() {
        //Arrange Statement(s)
        StopWatch target = spy(new StopWatch("message1"));
        doReturn(1L).when(target).getTime();
        
        //Act Statement(s)
        String result = target.formatTime();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("00:00:00.001"));
            verify(target).getTime();
        });
    }

    //BaseRock generated method id: ${getWhenIsStopped}, hash: 588F6ED3441CCEEFD38DB2A3FFEA9AF3
    @Test()
    void getWhenIsStopped() {
        /* Branches:
         * (isStopped()) : true  #  inside startResume method
         */
         //Arrange Statement(s)
        Object object = new Object();
        doReturn(object).when(supplierMock).get();
        StopWatch target = spy(new StopWatch("message1"));
        doReturn(true).when(target).isStopped();
        doNothing().when(target).start();
        doNothing().when(target).suspend();
        
        //Act Statement(s)
        Object result = target.get(supplierMock);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(object));
            verify(supplierMock).get();
            verify(target).isStopped();
            verify(target).start();
            verify(target).suspend();
        });
    }

    //BaseRock generated method id: ${getWhenIsSuspended}, hash: 4F63FBC9FD168BC90ABA2DD01E84C781
    @Test()
    void getWhenIsSuspended() {
        /* Branches:
         * (isStopped()) : false  #  inside startResume method
         * (isSuspended()) : true  #  inside startResume method
         */
         //Arrange Statement(s)
        Object object = new Object();
        doReturn(object).when(supplierMock).get();
        StopWatch target = spy(new StopWatch("message1"));
        doReturn(false).when(target).isStopped();
        doReturn(true).when(target).isSuspended();
        doNothing().when(target).resume();
        doNothing().when(target).suspend();
        
        //Act Statement(s)
        Object result = target.get(supplierMock);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(object));
            verify(supplierMock).get();
            verify(target).isStopped();
            verify(target).isSuspended();
            verify(target).resume();
            verify(target).suspend();
        });
    }

    //BaseRock generated method id: ${getDurationTest}, hash: E1E1BD36BA4ABDB38BEC73F964A2F6BC
    @Test()
    void getDurationTest() {
        //Arrange Statement(s)
        StopWatch target = spy(new StopWatch("message1"));
        doReturn(1L).when(target).getNanoTime();
        
        //Act Statement(s)
        Duration result = target.getDuration();
        Duration duration = Duration.ofNanos(1L);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(duration));
            verify(target).getNanoTime();
        });
    }

    //BaseRock generated method id: ${getMessageTest}, hash: 1A0CAAF3AE8833B1E394A6E07D3049AE
    @Test()
    void getMessageTest() {
        //Arrange Statement(s)
        StopWatch target = new StopWatch("message1");
        
        //Act Statement(s)
        String result = target.getMessage();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("message1")));
    }

    //BaseRock generated method id: ${getNanoTimeWhenSwitchRunningStateCaseUNSTARTED}, hash: 3F3BD0E224E19787A14FD5F1105589EF
    @Test()
    void getNanoTimeWhenSwitchRunningStateCaseUNSTARTED() {
        /* Branches:
         * (switch(runningState) = UNSTARTED) : true
         */
         //Arrange Statement(s)
        StopWatch target = new StopWatch("message1");
        
        //Act Statement(s)
        long result = target.getNanoTime();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0L)));
    }

    //BaseRock generated method id: ${getSplitDurationTest}, hash: F2ED1733FA60C193FE95A948EF530EE0
    @Test()
    void getSplitDurationTest() {
        //Arrange Statement(s)
        StopWatch target = spy(new StopWatch("message1"));
        doReturn(1L).when(target).getSplitNanoTime();
        
        //Act Statement(s)
        Duration result = target.getSplitDuration();
        Duration duration = Duration.ofNanos(1L);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(duration));
            verify(target).getSplitNanoTime();
        });
    }

    //BaseRock generated method id: ${getSplitNanoTimeWhenSplitStateNotEqualsSplitStateSPLITThrowsIllegalStateException}, hash: 8A3041C075C8E38A8822B22562825B51
    @Test()
    void getSplitNanoTimeWhenSplitStateNotEqualsSplitStateSPLITThrowsIllegalStateException() {
        /* Branches:
         * (splitState != SplitState.SPLIT) : true
         */
         //Arrange Statement(s)
        StopWatch target = new StopWatch("message1");
        IllegalStateException illegalStateException = new IllegalStateException("Stopwatch must be split to get the split time.");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            target.getSplitNanoTime();
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getSplitTimeTest}, hash: C02FAE259A0427CF7FF2A5DC81C29B0E
    @Test()
    void getSplitTimeTest() {
        //Arrange Statement(s)
        StopWatch target = spy(new StopWatch("message1"));
        doReturn(1L).when(target).getSplitNanoTime();
        
        //Act Statement(s)
        long result = target.getSplitTime();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(0L));
            verify(target).getSplitNanoTime();
        });
    }

    //BaseRock generated method id: ${getStartInstantWhenRunningStateEqualsStateUNSTARTEDThrowsIllegalStateException}, hash: 8DB7CE3D61E03398E81363BB0E60CDF9
    @Test()
    void getStartInstantWhenRunningStateEqualsStateUNSTARTEDThrowsIllegalStateException() {
        /* Branches:
         * (runningState == State.UNSTARTED) : true
         */
         //Arrange Statement(s)
        StopWatch target = new StopWatch("message1");
        IllegalStateException illegalStateException = new IllegalStateException("Stopwatch has not been started");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            target.getStartInstant();
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getStartTimeTest}, hash: DC01E33C186D242A54F3B8F2B3886338
    @Test()
    void getStartTimeTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StopWatch target = spy(new StopWatch("message1"));
        Instant instant = Instant.ofEpochSecond(1700000000L);
        doReturn(instant).when(target).getStartInstant();
        
        //Act Statement(s)
        long result = target.getStartTime();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(0L));
            verify(target).getStartInstant();
        });
    }

    //BaseRock generated method id: ${getStopInstantWhenRunningStateEqualsStateUNSTARTEDThrowsIllegalStateException}, hash: 2D8D030D77CF903BE4327E8EC77C3742
    @Test()
    void getStopInstantWhenRunningStateEqualsStateUNSTARTEDThrowsIllegalStateException() {
        /* Branches:
         * (runningState == State.UNSTARTED) : true
         */
         //Arrange Statement(s)
        StopWatch target = new StopWatch("message1");
        IllegalStateException illegalStateException = new IllegalStateException("Stopwatch has not been started");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            target.getStopInstant();
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getStopTimeTest}, hash: B7DF6B6882AC09B7F8C4B58D081353BD
    @Test()
    void getStopTimeTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StopWatch target = spy(new StopWatch("message1"));
        Instant instant = Instant.ofEpochSecond(1700000000L);
        doReturn(instant).when(target).getStopInstant();
        
        //Act Statement(s)
        long result = target.getStopTime();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(0L));
            verify(target).getStopInstant();
        });
    }

    //BaseRock generated method id: ${getTWhenIsStopped}, hash: A326BD82B77332C1B4D292E0AABE20E0
    @Test()
    void getTWhenIsStopped() throws Throwable {
        /* Branches:
         * (isStopped()) : true  #  inside startResume method
         */
         //Arrange Statement(s)
        StopWatch target = spy(new StopWatch("message1"));
        doReturn(true).when(target).isStopped();
        doNothing().when(target).start();
        doNothing().when(target).suspend();
        FailableSupplier failableSupplier = FailableSupplier.nul();
        
        //Act Statement(s)
        Object result = target.getT(failableSupplier);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(nullValue()));
            verify(target).isStopped();
            verify(target).start();
            verify(target).suspend();
        });
    }

    //BaseRock generated method id: ${getTWhenIsSuspended}, hash: 543C5D83F451121BE089017CEC7CEF50
    @Test()
    void getTWhenIsSuspended() throws Throwable {
        /* Branches:
         * (isStopped()) : false  #  inside startResume method
         * (isSuspended()) : true  #  inside startResume method
         */
         //Arrange Statement(s)
        StopWatch target = spy(new StopWatch("message1"));
        doReturn(false).when(target).isStopped();
        doReturn(true).when(target).isSuspended();
        doNothing().when(target).resume();
        doNothing().when(target).suspend();
        FailableSupplier failableSupplier = FailableSupplier.nul();
        
        //Act Statement(s)
        Object result = target.getT(failableSupplier);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(nullValue()));
            verify(target).isStopped();
            verify(target).isSuspended();
            verify(target).resume();
            verify(target).suspend();
        });
    }

    //BaseRock generated method id: ${getTimeTest}, hash: 9386700825EEEDB406BB7978516ACFFE
    @Test()
    void getTimeTest() {
        //Arrange Statement(s)
        StopWatch target = spy(new StopWatch("message1"));
        doReturn(1L).when(target).getNanoTime();
        
        //Act Statement(s)
        long result = target.getTime();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(0L));
            verify(target).getNanoTime();
        });
    }

    //BaseRock generated method id: ${getTime1Test}, hash: B6A1315C9CA98A66EE52030500385F84
    @Test()
    void getTime1Test() {
        //Arrange Statement(s)
        StopWatch target = spy(new StopWatch("message1"));
        doReturn(0L).when(target).getNanoTime();
        
        //Act Statement(s)
        long result = target.getTime(TimeUnit.NANOSECONDS);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(0L));
            verify(target).getNanoTime();
        });
    }

    //BaseRock generated method id: ${isStartedWhenRunningStateIsStarted}, hash: 1220A258E3DC8ECCF7114DE38300AF2B
    @Test()
    void isStartedWhenRunningStateIsStarted() {
        /* Branches:
         * (runningState.isStarted()) : true
         */
         //Arrange Statement(s)
        StopWatch target = new StopWatch("message1");
        
        //Act Statement(s)
        boolean result = target.isStarted();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isStartedWhenRunningStateNotIsStarted}, hash: F95CB268AC240BBCD6BE84D71A686D5B
    @Test()
    void isStartedWhenRunningStateNotIsStarted() {
        /* Branches:
         * (runningState.isStarted()) : false
         */
         //Arrange Statement(s)
        StopWatch target = new StopWatch("message1");
        
        //Act Statement(s)
        boolean result = target.isStarted();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isStoppedWhenRunningStateIsStopped}, hash: 74286203750BD9A8585844094A0E5CEC
    @Test()
    void isStoppedWhenRunningStateIsStopped() {
        /* Branches:
         * (runningState.isStopped()) : true
         */
         //Arrange Statement(s)
        StopWatch target = new StopWatch("message1");
        
        //Act Statement(s)
        boolean result = target.isStopped();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isStoppedWhenRunningStateNotIsStopped}, hash: 1D568B880CDCB16D5083560D223D7611
    @Test()
    void isStoppedWhenRunningStateNotIsStopped() {
        /* Branches:
         * (runningState.isStopped()) : false
         */
         //Arrange Statement(s)
        StopWatch target = new StopWatch("message1");
        
        //Act Statement(s)
        boolean result = target.isStopped();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isSuspendedWhenRunningStateIsSuspended}, hash: CB0F26482D458DC3D54B9140A604DD4C
    @Test()
    void isSuspendedWhenRunningStateIsSuspended() {
        /* Branches:
         * (runningState.isSuspended()) : true
         */
         //Arrange Statement(s)
        StopWatch target = new StopWatch("message1");
        
        //Act Statement(s)
        boolean result = target.isSuspended();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isSuspendedWhenRunningStateNotIsSuspended}, hash: CAC7BDA7FDF54D63CE8FA1819DC49C00
    @Test()
    void isSuspendedWhenRunningStateNotIsSuspended() {
        /* Branches:
         * (runningState.isSuspended()) : false
         */
         //Arrange Statement(s)
        StopWatch target = new StopWatch("message1");
        
        //Act Statement(s)
        boolean result = target.isSuspended();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${resetTest}, hash: 8E4C20034D585807C63FEB91EE9A1054
    @Test()
    void resetTest() {
        //Arrange Statement(s)
        StopWatch target = new StopWatch("message1");
        
        //Act Statement(s)
        target.reset();
    }

    //BaseRock generated method id: ${resumeWhenRunningStateNotEqualsStateSUSPENDEDThrowsIllegalStateException}, hash: 29D2FBF3CB9A3DCBB4B5BC370A9A0A71
    @Test()
    void resumeWhenRunningStateNotEqualsStateSUSPENDEDThrowsIllegalStateException() {
        /* Branches:
         * (runningState != State.SUSPENDED) : true
         */
         //Arrange Statement(s)
        StopWatch target = new StopWatch("message1");
        IllegalStateException illegalStateException = new IllegalStateException("Stopwatch must be suspended to resume.");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            target.resume();
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${runWhenIsStopped}, hash: 9006610F64DD9894B0B42A8C1F68003A
    @Disabled()
    @Test()
    void runWhenIsStopped() {
        /* Branches:
         * (isStopped()) : true  #  inside startResume method
         *
         * TODO: Help needed! This method is not unit testable!
         *  Potential harmful system call (Runnable.run) detected; Learn more: https://github.com/Sapient-AI/docs#disabled-generated-tests
         *  Suggestions:
         *  This method should be avoided from unit testing. This can be covered during integration testing.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StopWatch target = spy(new StopWatch("message1"));
        doReturn(true).when(target).isStopped();
        doNothing().when(target).start();
        doNothing().when(target).suspend();
        
        //Act Statement(s)
        target.run(runnableMock);
        
        //Assert statement(s)
        assertAll("result", () -> {
            verify(target).isStopped();
            verify(target).start();
            verify(target).suspend();
        });
    }

    //BaseRock generated method id: ${runWhenIsSuspended}, hash: CDD8FF05060FEEDEB1ECC98B88D97701
    @Disabled()
    @Test()
    void runWhenIsSuspended() {
        /* Branches:
         * (isStopped()) : false  #  inside startResume method
         * (isSuspended()) : true  #  inside startResume method
         *
         * TODO: Help needed! This method is not unit testable!
         *  Potential harmful system call (Runnable.run) detected; Learn more: https://github.com/Sapient-AI/docs#disabled-generated-tests
         *  Suggestions:
         *  This method should be avoided from unit testing. This can be covered during integration testing.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StopWatch target = spy(new StopWatch("message1"));
        doReturn(false).when(target).isStopped();
        doReturn(true).when(target).isSuspended();
        doNothing().when(target).resume();
        doNothing().when(target).suspend();
        
        //Act Statement(s)
        target.run(runnableMock);
        
        //Assert statement(s)
        assertAll("result", () -> {
            verify(target).isStopped();
            verify(target).isSuspended();
            verify(target).resume();
            verify(target).suspend();
        });
    }

    //BaseRock generated method id: ${runTWhenIsStopped}, hash: 24E68CEE3272CE9F00D2CE2AE83A3996
    @Test()
    void runTWhenIsStopped() throws Throwable {
        /* Branches:
         * (isStopped()) : true  #  inside startResume method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StopWatch target = spy(new StopWatch("message1"));
        doReturn(true).when(target).isStopped();
        doNothing().when(target).start();
        doNothing().when(target).suspend();
        
        //Act Statement(s)
        target.runT(failableRunnableMock);
        
        //Assert statement(s)
        assertAll("result", () -> {
            verify(target).isStopped();
            verify(target).start();
            verify(target).suspend();
        });
    }

    //BaseRock generated method id: ${runTWhenIsSuspended}, hash: 09AEC0A96ECEA64638552A68F1D07843
    @Test()
    void runTWhenIsSuspended() throws Throwable {
        /* Branches:
         * (isStopped()) : false  #  inside startResume method
         * (isSuspended()) : true  #  inside startResume method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StopWatch target = spy(new StopWatch("message1"));
        doReturn(false).when(target).isStopped();
        doReturn(true).when(target).isSuspended();
        doNothing().when(target).resume();
        doNothing().when(target).suspend();
        
        //Act Statement(s)
        target.runT(failableRunnableMock);
        
        //Assert statement(s)
        assertAll("result", () -> {
            verify(target).isStopped();
            verify(target).isSuspended();
            verify(target).resume();
            verify(target).suspend();
        });
    }

    //BaseRock generated method id: ${splitWhenRunningStateNotEqualsStateRUNNINGThrowsIllegalStateException}, hash: 12F2A360591CD79C12078C70A76CB213
    @Test()
    void splitWhenRunningStateNotEqualsStateRUNNINGThrowsIllegalStateException() {
        /* Branches:
         * (runningState != State.RUNNING) : true
         */
         //Arrange Statement(s)
        StopWatch target = new StopWatch("message1");
        IllegalStateException illegalStateException = new IllegalStateException("Stopwatch is not running.");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            target.split();
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${startWhenRunningStateEqualsStateUNSTARTED}, hash: A33A5C7C45C1D711F200ED583FE7444C
    @Test()
    void startWhenRunningStateEqualsStateUNSTARTED() {
        /* Branches:
         * (runningState == State.STOPPED) : false
         * (runningState != State.UNSTARTED) : false
         *
         * TODO: Help needed! This method is not unit testable!
         *  Method java.lang.System::nanoTime has a unrepeatable behavior
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<Instant> instant = mockStatic(Instant.class)) {
            Instant instant2 = Instant.ofEpochSecond(1700000000L);
            instant.when(() -> Instant.now()).thenReturn(instant2);
            StopWatch target = new StopWatch("message1");
            //Act Statement(s)
            target.start();
            //Assert statement(s)
            assertAll("result", () -> instant.verify(() -> Instant.now(), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${stopWhenRunningStateNotEqualsStateSUSPENDEDThrowsIllegalStateException}, hash: 6A4CBF4DE3FE9E8C125E0E9A95918270
    @Test()
    void stopWhenRunningStateNotEqualsStateSUSPENDEDThrowsIllegalStateException() {
        /* Branches:
         * (runningState != State.RUNNING) : true
         * (runningState != State.SUSPENDED) : true
         */
         //Arrange Statement(s)
        StopWatch target = new StopWatch("message1");
        IllegalStateException illegalStateException = new IllegalStateException("Stopwatch is not running.");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            target.stop();
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${suspendWhenRunningStateNotEqualsStateRUNNINGThrowsIllegalStateException}, hash: 2D316120481BD6392A2A529DD052060C
    @Test()
    void suspendWhenRunningStateNotEqualsStateRUNNINGThrowsIllegalStateException() {
        /* Branches:
         * (runningState != State.RUNNING) : true
         */
         //Arrange Statement(s)
        StopWatch target = new StopWatch("message1");
        IllegalStateException illegalStateException = new IllegalStateException("Stopwatch must be running to suspend.");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            target.suspend();
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${toSplitStringWhenMsgStrIsEmpty}, hash: 521BB28C52F80BA54D39E7F0C9F94377
    @Test()
    void toSplitStringWhenMsgStrIsEmpty() {
        /* Branches:
         * (msgStr.isEmpty()) : true
         */
         //Arrange Statement(s)
        StopWatch target = spy(new StopWatch(""));
        doReturn("return_of_formatSplitTime1").when(target).formatSplitTime();
        
        //Act Statement(s)
        String result = target.toSplitString();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("return_of_formatSplitTime1"));
            verify(target).formatSplitTime();
        });
    }

    //BaseRock generated method id: ${toSplitStringWhenMsgStrNotIsEmpty}, hash: 93E9DEF85422586A040F3342AAFE33FF
    @Test()
    void toSplitStringWhenMsgStrNotIsEmpty() {
        /* Branches:
         * (msgStr.isEmpty()) : false
         */
         //Arrange Statement(s)
        StopWatch target = spy(new StopWatch("C"));
        doReturn("A").when(target).formatSplitTime();
        
        //Act Statement(s)
        String result = target.toSplitString();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("C A"));
            verify(target).formatSplitTime();
        });
    }

    //BaseRock generated method id: ${toStringWhenMsgStrIsEmpty}, hash: 50B5BAF685E7E2D5E04C13F0C02E5397
    @Test()
    void toStringWhenMsgStrIsEmpty() {
        /* Branches:
         * (msgStr.isEmpty()) : true
         */
         //Arrange Statement(s)
        StopWatch target = spy(new StopWatch(""));
        doReturn("return_of_formatTime1").when(target).formatTime();
        
        //Act Statement(s)
        String result = target.toString();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("return_of_formatTime1"));
            verify(target).formatTime();
        });
    }

    //BaseRock generated method id: ${toStringWhenMsgStrNotIsEmpty}, hash: 8DBDC3C8E5418DA3FB130C444A6F4F12
    @Test()
    void toStringWhenMsgStrNotIsEmpty() {
        /* Branches:
         * (msgStr.isEmpty()) : false
         */
         //Arrange Statement(s)
        StopWatch target = spy(new StopWatch("C"));
        doReturn("A").when(target).formatTime();
        
        //Act Statement(s)
        String result = target.toString();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("C A"));
            verify(target).formatTime();
        });
    }

    //BaseRock generated method id: ${unsplitWhenSplitStateNotEqualsSplitStateSPLITThrowsIllegalStateException}, hash: 6DC2786DFCF5E675552BE026E377BAA4
    @Test()
    void unsplitWhenSplitStateNotEqualsSplitStateSPLITThrowsIllegalStateException() {
        /* Branches:
         * (splitState != SplitState.SPLIT) : true
         */
         //Arrange Statement(s)
        StopWatch target = new StopWatch("message1");
        IllegalStateException illegalStateException = new IllegalStateException("Stopwatch has not been split.");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            target.unsplit();
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }
}
