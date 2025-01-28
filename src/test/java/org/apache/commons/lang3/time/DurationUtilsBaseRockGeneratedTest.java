package org.apache.commons.lang3.time;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import org.apache.commons.lang3.function.FailableConsumer;
import java.time.Duration;
import java.time.temporal.ChronoUnit;
import org.apache.commons.lang3.function.FailableRunnable;
import org.apache.commons.lang3.ObjectUtils;
import java.util.concurrent.TimeUnit;
import org.mockito.MockedStatic;
import org.apache.commons.lang3.function.FailableBiConsumer;
import java.time.temporal.Temporal;
import java.time.Instant;
import static org.mockito.ArgumentMatchers.any;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class DurationUtilsBaseRockGeneratedTest {

    //BaseRock generated method id: ${acceptWhenDurationIsNotNull}, hash: 8295FE37A1D615716BC0D835F453D95E
    @Test()
    void acceptWhenDurationIsNotNull() throws Throwable {
        /* Branches:
         * (consumer != null) : true
         * (duration != null) : true
         */
         //Arrange Statement(s)
        try (MockedStatic<DurationUtils> durationUtils = mockStatic(DurationUtils.class, CALLS_REAL_METHODS)) {
            durationUtils.when(() -> DurationUtils.getNanosOfMilli((Duration) any())).thenReturn(1);
            FailableBiConsumer failableBiConsumer = FailableBiConsumer.nop();
            Duration duration = Duration.ofDays(0L);
            //Act Statement(s)
            DurationUtils.accept(failableBiConsumer, duration);
            //Assert statement(s)
            assertAll("result", () -> durationUtils.verify(() -> DurationUtils.getNanosOfMilli((Duration) any()), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${getNanosOfMiiliTest}, hash: 1CB0869886166D28BEA3DEB5C713DC8B
    @Test()
    void getNanosOfMiiliTest() {
        //Arrange Statement(s)
        try (MockedStatic<DurationUtils> durationUtils = mockStatic(DurationUtils.class, CALLS_REAL_METHODS)) {
            durationUtils.when(() -> DurationUtils.getNanosOfMilli((Duration) any())).thenReturn(0);
            Duration duration = Duration.ofDays(0L);
            //Act Statement(s)
            int result = DurationUtils.getNanosOfMiili(duration);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                durationUtils.verify(() -> DurationUtils.getNanosOfMilli((Duration) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getNanosOfMilliTest}, hash: 53FC89ED6B4D9494BD751F075F404EA0
    @Test()
    void getNanosOfMilliTest() {
        //Arrange Statement(s)
        Duration duration = Duration.ofDays(0L);
        
        //Act Statement(s)
        int result = DurationUtils.getNanosOfMilli(duration);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${isPositiveWhenDurationNotIsZero}, hash: BB91890D2D74CCEF63AD2BA02909B943
    @Test()
    void isPositiveWhenDurationNotIsZero() {
        /* Branches:
         * (!duration.isNegative()) : true
         * (!duration.isZero()) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Duration duration = Duration.ofDays(0L);
        
        //Act Statement(s)
        boolean result = DurationUtils.isPositive(duration);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isPositiveWhenDurationIsZero}, hash: 9FF794A8B960F7B786074D97FF820E7F
    @Test()
    void isPositiveWhenDurationIsZero() {
        /* Branches:
         * (!duration.isNegative()) : true
         * (!duration.isZero()) : false
         */
         //Arrange Statement(s)
        Duration duration = Duration.ofDays(0L);
        
        //Act Statement(s)
        boolean result = DurationUtils.isPositive(duration);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${ofTest}, hash: A834A2084C84E8EF714C09CEB50E3879
    @Test()
    void ofTest() throws Throwable {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: nowConsumer - Method: accept
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<Instant> instant = mockStatic(Instant.class)) {
            Instant instant2 = Instant.now();
            Instant instant3 = Instant.now();
            instant.when(() -> Instant.now()).thenReturn(instant2).thenReturn(instant3);
            FailableConsumer failableConsumer = FailableConsumer.nop();
            //Act Statement(s)
            Duration result = DurationUtils.of(failableConsumer);
            Duration duration = Duration.ofDays(0L);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(duration));
                instant.verify(() -> Instant.now(), atLeast(2));
            });
        }
    }

    //BaseRock generated method id: ${of1Test}, hash: 65849301C5F9ADDD44F2ECF7AF1490AD
    @Test()
    void of1Test() throws Throwable {
        //Arrange Statement(s)
        FailableRunnable<Throwable> failableRunnableMock = mock(FailableRunnable.class);
        try (MockedStatic<DurationUtils> durationUtils = mockStatic(DurationUtils.class, CALLS_REAL_METHODS)) {
            Duration duration = Duration.ofDays(0L);
            durationUtils.when(() -> DurationUtils.of((FailableConsumer) any())).thenReturn(duration);
            //Act Statement(s)
            Duration result = DurationUtils.of(failableRunnableMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(duration));
                durationUtils.verify(() -> DurationUtils.of((FailableConsumer) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${sinceTest}, hash: 39719471A4FA0B4276939A05FD89FF0F
    @Test()
    void sinceTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Temporal temporalMock = mock(Temporal.class);
        try (MockedStatic<Instant> instant = mockStatic(Instant.class)) {
            Instant instant2 = Instant.now();
            instant.when(() -> Instant.now()).thenReturn(instant2);
            //Act Statement(s)
            Duration result = DurationUtils.since(temporalMock);
            Duration duration = Duration.ofDays(0L);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(duration));
                instant.verify(() -> Instant.now(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toChronoUnitWhenSwitchObjectsRequireNonNullTimeUnitCaseDAYS}, hash: 2EFF359DE3A7E4E1219302487FA1D713
    @Test()
    void toChronoUnitWhenSwitchObjectsRequireNonNullTimeUnitCaseDAYS() {
        /* Branches:
         * (switch(Objects.requireNonNull(timeUnit)) = DAYS) : true
         */
         
        //Act Statement(s)
        ChronoUnit result = DurationUtils.toChronoUnit(TimeUnit.DAYS);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(ChronoUnit.DAYS)));
    }

    //BaseRock generated method id: ${toChronoUnitWhenSwitchObjectsRequireNonNullTimeUnitCaseHOURS}, hash: A299BB75CFF0893981266AE0F1B9A595
    @Test()
    void toChronoUnitWhenSwitchObjectsRequireNonNullTimeUnitCaseHOURS() {
        /* Branches:
         * (switch(Objects.requireNonNull(timeUnit)) = HOURS) : true
         */
         
        //Act Statement(s)
        ChronoUnit result = DurationUtils.toChronoUnit(TimeUnit.HOURS);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(ChronoUnit.HOURS)));
    }

    //BaseRock generated method id: ${toChronoUnitWhenSwitchObjectsRequireNonNullTimeUnitCaseMICROSECONDS}, hash: B8DDDFE8EDF67EC4A0769541B9BA6E57
    @Test()
    void toChronoUnitWhenSwitchObjectsRequireNonNullTimeUnitCaseMICROSECONDS() {
        /* Branches:
         * (switch(Objects.requireNonNull(timeUnit)) = MICROSECONDS) : true
         */
         
        //Act Statement(s)
        ChronoUnit result = DurationUtils.toChronoUnit(TimeUnit.MICROSECONDS);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(ChronoUnit.MICROS)));
    }

    //BaseRock generated method id: ${toChronoUnitWhenSwitchObjectsRequireNonNullTimeUnitCaseMILLISECONDS}, hash: 8F4DFBB9F9BB90C4C48FB41202290458
    @Test()
    void toChronoUnitWhenSwitchObjectsRequireNonNullTimeUnitCaseMILLISECONDS() {
        /* Branches:
         * (switch(Objects.requireNonNull(timeUnit)) = MILLISECONDS) : true
         */
         
        //Act Statement(s)
        ChronoUnit result = DurationUtils.toChronoUnit(TimeUnit.MILLISECONDS);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(ChronoUnit.MILLIS)));
    }

    //BaseRock generated method id: ${toChronoUnitWhenSwitchObjectsRequireNonNullTimeUnitCaseMINUTES}, hash: 8D7AB70519CC986FEDB6740AE37D4845
    @Test()
    void toChronoUnitWhenSwitchObjectsRequireNonNullTimeUnitCaseMINUTES() {
        /* Branches:
         * (switch(Objects.requireNonNull(timeUnit)) = MINUTES) : true
         */
         
        //Act Statement(s)
        ChronoUnit result = DurationUtils.toChronoUnit(TimeUnit.MINUTES);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(ChronoUnit.MINUTES)));
    }

    //BaseRock generated method id: ${toChronoUnitWhenSwitchObjectsRequireNonNullTimeUnitCaseNANOSECONDS}, hash: 7D9CC897471E46F4D50A291146A3C579
    @Test()
    void toChronoUnitWhenSwitchObjectsRequireNonNullTimeUnitCaseNANOSECONDS() {
        /* Branches:
         * (switch(Objects.requireNonNull(timeUnit)) = NANOSECONDS) : true
         */
         
        //Act Statement(s)
        ChronoUnit result = DurationUtils.toChronoUnit(TimeUnit.NANOSECONDS);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(ChronoUnit.NANOS)));
    }

    //BaseRock generated method id: ${toChronoUnitWhenSwitchObjectsRequireNonNullTimeUnitCaseSECONDS}, hash: 1F8B9D866B96B6745EF381BE9172AA04
    @Test()
    void toChronoUnitWhenSwitchObjectsRequireNonNullTimeUnitCaseSECONDS() {
        /* Branches:
         * (switch(Objects.requireNonNull(timeUnit)) = SECONDS) : true
         */
         
        //Act Statement(s)
        ChronoUnit result = DurationUtils.toChronoUnit(TimeUnit.SECONDS);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(ChronoUnit.SECONDS)));
    }

    //BaseRock generated method id: ${toChronoUnitWhenSwitchObjectsRequireNonNullTimeUnitCaseDefaultThrowsIllegalArgumentException}, hash: 39B1512AA7EF73D6D9FA89E30CFAB7FB
    @Test()
    void toChronoUnitWhenSwitchObjectsRequireNonNullTimeUnitCaseDefaultThrowsIllegalArgumentException() {
        /* Branches:
         * (switch(Objects.requireNonNull(timeUnit)) = default) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("DAYS");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            DurationUtils.toChronoUnit(TimeUnit.DAYS);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${toDurationTest}, hash: 1B1444A1393D8DA12E6FE2C8B0130FEC
    @Test()
    void toDurationTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<DurationUtils> durationUtils = mockStatic(DurationUtils.class, CALLS_REAL_METHODS)) {
            durationUtils.when(() -> DurationUtils.toChronoUnit(TimeUnit.NANOSECONDS)).thenReturn(ChronoUnit.FOREVER);
            //Act Statement(s)
            Duration result = DurationUtils.toDuration(1L, TimeUnit.NANOSECONDS);
            Duration duration = Duration.ofDays(0L);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(duration));
                durationUtils.verify(() -> DurationUtils.toChronoUnit(TimeUnit.NANOSECONDS), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toMillisIntTest}, hash: A6C3327D68EABBCD646754F5A6BF55A7
    @Test()
    void toMillisIntTest() {
        //Arrange Statement(s)
        Duration duration = Duration.ofDays(0L);
        
        //Act Statement(s)
        int result = DurationUtils.toMillisInt(duration);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${zeroIfNullTest}, hash: F0BB00619C03D11690003D8DFA7A4A40
    @Test()
    void zeroIfNullTest() {
        //Arrange Statement(s)
        Duration duration = Duration.ofDays(0L);
        
        //Act Statement(s)
        Duration result = DurationUtils.zeroIfNull(duration);
        Duration duration3 = Duration.ZERO;
        Duration duration2 = ObjectUtils.defaultIfNull(duration, duration3);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(duration2)));
    }
}
