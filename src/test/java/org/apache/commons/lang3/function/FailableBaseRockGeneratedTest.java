package org.apache.commons.lang3.function;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.util.function.BiConsumer;
import java.util.concurrent.Callable;
import org.apache.commons.lang3.stream.Streams;
import java.util.function.Function;
import java.util.function.BiPredicate;
import java.util.ArrayList;
import java.util.function.Consumer;
import java.util.function.BiFunction;
import org.mockito.stubbing.Answer;
import java.util.stream.Stream;
import java.lang.reflect.UndeclaredThrowableException;
import java.util.function.Predicate;
import java.util.Collection;
import java.util.function.Supplier;
import org.mockito.MockedStatic;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.doReturn;
import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.any;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.atLeast;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.mockStatic;
import static org.hamcrest.Matchers.closeTo;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class FailableBaseRockGeneratedTest {

    private final FailableRunnable<Throwable> actionMock = mock(FailableRunnable.class);

    private final FailableBooleanSupplier<Throwable> failableBooleanSupplierMock = mock(FailableBooleanSupplier.class);

    private final FailableRunnable<Throwable> failableRunnableMock = mock(FailableRunnable.class);

    private final FailableRunnable<?> failableRunnableMock2 = mock(FailableRunnable.class);

    private final FailableRunnable<?> runnableMock = mock(FailableRunnable.class);

    //BaseRock generated method id: ${acceptWhenTestIsNotNull}, hash: FF90E4875895B79B49972480EF86D955
    @Test()
    void acceptWhenTestIsNotNull() {
        /* Branches:
         * (runnable != null) : true  #  inside run method
         * (test != null) : true  #  inside run method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: runnable - Method: run
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        FailableBiConsumer<Object, Object, Throwable> failableBiConsumer = FailableBiConsumer.nop();
        Object object = new Object();
        Object object2 = new Object();
        
        //Act Statement(s)
        Failable.accept(failableBiConsumer, object, object2);
    }

    //BaseRock generated method id: ${acceptWhenCaughtThrowableThrowsRuntimeException}, hash: BC0D6E4FDF0C966CE41D29CC3A536F43
    @Test()
    void acceptWhenCaughtThrowableThrowsRuntimeException() {
        /* Branches:
         * (runnable != null) : true  #  inside run method
         * (test != null) : true  #  inside run method
         * (catch-exception (Throwable)) : true  #  inside run method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: runnable - Method: run
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<Failable> failable = mockStatic(Failable.class, CALLS_REAL_METHODS)) {
            RuntimeException runtimeException = new RuntimeException();
            Throwable throwable = new Throwable();
            failable.when(() -> Failable.rethrow(throwable)).thenThrow(runtimeException);
            FailableBiConsumer<Object, Object, Throwable> failableBiConsumer = FailableBiConsumer.nop();
            Object object = new Object();
            Object object2 = new Object();
            //Act Statement(s)
            final RuntimeException result = assertThrows(RuntimeException.class, () -> {
                Failable.accept(failableBiConsumer, object, object2);
            });
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(runtimeException));
                failable.verify(() -> Failable.rethrow(throwable), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${accept1WhenTestIsNotNull}, hash: 172928D56627AF61C707B27D7E314939
    @Test()
    void accept1WhenTestIsNotNull() {
        /* Branches:
         * (runnable != null) : true  #  inside run method
         * (test != null) : true  #  inside run method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: runnable - Method: run
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        FailableConsumer<Object, Throwable> failableConsumer = FailableConsumer.nop();
        Object object = new Object();
        
        //Act Statement(s)
        Failable.accept(failableConsumer, object);
    }

    //BaseRock generated method id: ${accept1WhenCaughtThrowableThrowsRuntimeException}, hash: 8622E4A12F56CB42114A6A1D4EFA8109
    @Test()
    void accept1WhenCaughtThrowableThrowsRuntimeException() {
        /* Branches:
         * (runnable != null) : true  #  inside run method
         * (test != null) : true  #  inside run method
         * (catch-exception (Throwable)) : true  #  inside run method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: runnable - Method: run
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<Failable> failable = mockStatic(Failable.class, CALLS_REAL_METHODS)) {
            RuntimeException runtimeException = new RuntimeException();
            Throwable throwable = new Throwable();
            failable.when(() -> Failable.rethrow(throwable)).thenThrow(runtimeException);
            FailableConsumer<Object, Throwable> failableConsumer = FailableConsumer.nop();
            Object object = new Object();
            //Act Statement(s)
            final RuntimeException result = assertThrows(RuntimeException.class, () -> {
                Failable.accept(failableConsumer, object);
            });
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(runtimeException));
                failable.verify(() -> Failable.rethrow(throwable), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${accept2WhenTestIsNotNull}, hash: CF4167682B32584BA2E39E9C943CAA36
    @Test()
    void accept2WhenTestIsNotNull() {
        /* Branches:
         * (runnable != null) : true  #  inside run method
         * (test != null) : true  #  inside run method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: runnable - Method: run
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        FailableDoubleConsumer<Throwable> failableDoubleConsumer = FailableDoubleConsumer.nop();
        
        //Act Statement(s)
        Failable.accept(failableDoubleConsumer, Double.parseDouble("0.0"));
    }

    //BaseRock generated method id: ${accept2WhenCaughtThrowableThrowsRuntimeException}, hash: 038E21BDB9C88F7AB5CCC729871E8601
    @Test()
    void accept2WhenCaughtThrowableThrowsRuntimeException() {
        /* Branches:
         * (runnable != null) : true  #  inside run method
         * (test != null) : true  #  inside run method
         * (catch-exception (Throwable)) : true  #  inside run method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: runnable - Method: run
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<Failable> failable = mockStatic(Failable.class, CALLS_REAL_METHODS)) {
            RuntimeException runtimeException = new RuntimeException();
            Throwable throwable = new Throwable();
            failable.when(() -> Failable.rethrow(throwable)).thenThrow(runtimeException);
            FailableDoubleConsumer<Throwable> failableDoubleConsumer = FailableDoubleConsumer.nop();
            //Act Statement(s)
            final RuntimeException result = assertThrows(RuntimeException.class, () -> {
                Failable.accept(failableDoubleConsumer, Double.parseDouble("0.0"));
            });
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(runtimeException));
                failable.verify(() -> Failable.rethrow(throwable), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${accept3WhenTestIsNotNull}, hash: 0FEFA09CAC22535262E6B1CDF404095C
    @Test()
    void accept3WhenTestIsNotNull() {
        /* Branches:
         * (runnable != null) : true  #  inside run method
         * (test != null) : true  #  inside run method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: runnable - Method: run
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        FailableIntConsumer<Throwable> failableIntConsumer = FailableIntConsumer.nop();
        
        //Act Statement(s)
        Failable.accept(failableIntConsumer, 0);
    }

    //BaseRock generated method id: ${accept3WhenCaughtThrowableThrowsRuntimeException}, hash: 602554D31F9837731520382F851EEB68
    @Test()
    void accept3WhenCaughtThrowableThrowsRuntimeException() {
        /* Branches:
         * (runnable != null) : true  #  inside run method
         * (test != null) : true  #  inside run method
         * (catch-exception (Throwable)) : true  #  inside run method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: runnable - Method: run
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<Failable> failable = mockStatic(Failable.class, CALLS_REAL_METHODS)) {
            RuntimeException runtimeException = new RuntimeException();
            Throwable throwable = new Throwable();
            failable.when(() -> Failable.rethrow(throwable)).thenThrow(runtimeException);
            FailableIntConsumer<Throwable> failableIntConsumer = FailableIntConsumer.nop();
            //Act Statement(s)
            final RuntimeException result = assertThrows(RuntimeException.class, () -> {
                Failable.accept(failableIntConsumer, 0);
            });
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(runtimeException));
                failable.verify(() -> Failable.rethrow(throwable), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${accept4WhenTestIsNotNull}, hash: CABD6D64E1DFBBED49E7A299A9262344
    @Test()
    void accept4WhenTestIsNotNull() {
        /* Branches:
         * (runnable != null) : true  #  inside run method
         * (test != null) : true  #  inside run method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: runnable - Method: run
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        FailableLongConsumer<Throwable> failableLongConsumer = FailableLongConsumer.nop();
        
        //Act Statement(s)
        Failable.accept(failableLongConsumer, 0L);
    }

    //BaseRock generated method id: ${accept4WhenCaughtThrowableThrowsRuntimeException}, hash: E51B8AD78385AF364968D335F5AA388E
    @Test()
    void accept4WhenCaughtThrowableThrowsRuntimeException() {
        /* Branches:
         * (runnable != null) : true  #  inside run method
         * (test != null) : true  #  inside run method
         * (catch-exception (Throwable)) : true  #  inside run method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: runnable - Method: run
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<Failable> failable = mockStatic(Failable.class, CALLS_REAL_METHODS)) {
            RuntimeException runtimeException = new RuntimeException();
            Throwable throwable = new Throwable();
            failable.when(() -> Failable.rethrow(throwable)).thenThrow(runtimeException);
            FailableLongConsumer<Throwable> failableLongConsumer = FailableLongConsumer.nop();
            //Act Statement(s)
            final RuntimeException result = assertThrows(RuntimeException.class, () -> {
                Failable.accept(failableLongConsumer, 0L);
            });
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(runtimeException));
                failable.verify(() -> Failable.rethrow(throwable), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${applyTest}, hash: 7249E095361168DC15003D07FB8EC6ED
    @Test()
    void applyTest() {
        //Arrange Statement(s)
        try (MockedStatic<Failable> failable = mockStatic(Failable.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            failable.when(() -> Failable.get((FailableSupplier) any())).thenReturn(object);
            FailableBiFunction<Object, Object, Object, Throwable> failableBiFunction = FailableBiFunction.nop();
            Object object2 = new Object();
            Object object3 = new Object();
            //Act Statement(s)
            Object result = Failable.apply(failableBiFunction, object2, object3);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(object));
                failable.verify(() -> Failable.get((FailableSupplier) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${apply1Test}, hash: 87FF35ED6B6EBCBEBF9171A2E402D498
    @Test()
    void apply1Test() {
        //Arrange Statement(s)
        try (MockedStatic<Failable> failable = mockStatic(Failable.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            failable.when(() -> Failable.get((FailableSupplier) any())).thenReturn(object);
            FailableFunction<Object, Object, Throwable> failableFunction = FailableFunction.identity();
            Object object2 = new Object();
            //Act Statement(s)
            Object result = Failable.apply(failableFunction, object2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(object));
                failable.verify(() -> Failable.get((FailableSupplier) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${applyAsDoubleTest}, hash: 0B4E3E0ED7716182000A3A9A9E7524C2
    @Test()
    void applyAsDoubleTest() {
        //Arrange Statement(s)
        FailableDoubleBinaryOperator<Throwable> failableDoubleBinaryOperatorMock = mock(FailableDoubleBinaryOperator.class);
        try (MockedStatic<Failable> failable = mockStatic(Failable.class, CALLS_REAL_METHODS)) {
            failable.when(() -> Failable.getAsDouble((FailableDoubleSupplier) any())).thenReturn(Double.parseDouble("0.0"));
            //Act Statement(s)
            double result = Failable.applyAsDouble(failableDoubleBinaryOperatorMock, Double.parseDouble("0.0"), Double.parseDouble("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, closeTo(Double.parseDouble("0.0"), 0.00001));
                failable.verify(() -> Failable.getAsDouble((FailableDoubleSupplier) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${asBiConsumerTest}, hash: B786BFBAFC69BF7AA676EF6F335A2F35
    @Test()
    void asBiConsumerTest() {
        //Arrange Statement(s)
        FailableBiConsumer failableBiConsumer = FailableBiConsumer.nop();
        
        //Act Statement(s)
        BiConsumer<Object, Object> result = Failable.asBiConsumer(failableBiConsumer);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${asBiFunctionTest}, hash: D5DEE0546ED777525B2B0012AE54BAC0
    @Test()
    void asBiFunctionTest() {
        //Arrange Statement(s)
        FailableBiFunction failableBiFunction = FailableBiFunction.nop();
        
        //Act Statement(s)
        BiFunction<Object, Object, Object> result = Failable.asBiFunction(failableBiFunction);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${asBiPredicateTest}, hash: 9B9C9C9CFA254D4FAB6FC901BDB7E49B
    @Test()
    void asBiPredicateTest() {
        //Arrange Statement(s)
        FailableBiPredicate failableBiPredicate = FailableBiPredicate.falsePredicate();
        
        //Act Statement(s)
        BiPredicate<Object, Object> result = Failable.asBiPredicate(failableBiPredicate);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${asCallableTest}, hash: 08500A8828CF7F281D03FA0B8F12B8B1
    @Test()
    void asCallableTest() {
        //Arrange Statement(s)
        FailableCallable<Object, ?> failableCallableMock = mock(FailableCallable.class);
        
        //Act Statement(s)
        Callable result = Failable.asCallable(failableCallableMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${asConsumerTest}, hash: 1743F0BA9F740998F089F3C216FFA66A
    @Test()
    void asConsumerTest() {
        //Arrange Statement(s)
        FailableConsumer failableConsumer = FailableConsumer.nop();
        
        //Act Statement(s)
        Consumer result = Failable.asConsumer(failableConsumer);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${asFunctionTest}, hash: 69CA459F36113BA16A36D07326D395C2
    @Test()
    void asFunctionTest() {
        //Arrange Statement(s)
        FailableFunction failableFunction = FailableFunction.identity();
        
        //Act Statement(s)
        Function<Object, Object> result = Failable.asFunction(failableFunction);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${asPredicateTest}, hash: A85820FE58F0A48AAB03F8A2CE4A65A0
    @Test()
    void asPredicateTest() {
        //Arrange Statement(s)
        FailablePredicate failablePredicate = FailablePredicate.falsePredicate();
        
        //Act Statement(s)
        Predicate result = Failable.asPredicate(failablePredicate);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${asRunnableTest}, hash: CD942C7909BADA50DED570F78E2CA454
    @Test()
    void asRunnableTest() {
        //Arrange Statement(s)
        FailableRunnable<?> failableRunnableMock = mock(FailableRunnable.class);
        
        //Act Statement(s)
        Runnable result = Failable.asRunnable(failableRunnableMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${asSupplierTest}, hash: 138A0A3B9C0DE650FE0B1B6D29F63F0B
    @Test()
    void asSupplierTest() {
        //Arrange Statement(s)
        FailableSupplier failableSupplier = FailableSupplier.nul();
        
        //Act Statement(s)
        Supplier result = Failable.asSupplier(failableSupplier);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${callTest}, hash: 26D865A1F4BC0BDBB0559B70056B5987
    @Test()
    void callTest() {
        //Arrange Statement(s)
        FailableCallable<Object, Throwable> failableCallableMock = mock(FailableCallable.class);
        try (MockedStatic<Failable> failable = mockStatic(Failable.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            failable.when(() -> Failable.get((FailableSupplier) any())).thenReturn(object);
            //Act Statement(s)
            Object result = Failable.call(failableCallableMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(object));
                failable.verify(() -> Failable.get((FailableSupplier) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getTest}, hash: 88A77FD28B6A809DDB8D4D3209882D6E
    @Test()
    void getTest() {
        //Arrange Statement(s)
        FailableSupplier failableSupplier = FailableSupplier.nul();
        
        //Act Statement(s)
        Object result = Failable.get(failableSupplier);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getWhenCaughtThrowableThrowsRuntimeException}, hash: 89F986C9A5D8A69186319AB8C5E0C27F
    @Test()
    void getWhenCaughtThrowableThrowsRuntimeException() {
        /* Branches:
         * (catch-exception (Throwable)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<Failable> failable = mockStatic(Failable.class, CALLS_REAL_METHODS)) {
            RuntimeException runtimeException = new RuntimeException();
            Throwable throwable = new Throwable();
            failable.when(() -> Failable.rethrow(throwable)).thenThrow(runtimeException);
            FailableSupplier failableSupplier = FailableSupplier.nul();
            //Act Statement(s)
            final RuntimeException result = assertThrows(RuntimeException.class, () -> {
                Failable.get(failableSupplier);
            });
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(runtimeException));
                failable.verify(() -> Failable.rethrow(throwable), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getAsBooleanWhenSupplierGetAsBoolean}, hash: 6EBC63E22A8424194F626685A972C523
    @Test()
    void getAsBooleanWhenSupplierGetAsBoolean() {
        /* Branches:
         * (supplier.getAsBoolean()) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        boolean result = Failable.getAsBoolean(failableBooleanSupplierMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${getAsBooleanWhenSupplierNotGetAsBoolean}, hash: 176490CC1FCB82F76E2FF5294F8A1A73
    @Test()
    void getAsBooleanWhenSupplierNotGetAsBoolean() {
        /* Branches:
         * (supplier.getAsBoolean()) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        boolean result = Failable.getAsBoolean(failableBooleanSupplierMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${getAsBooleanWhenCaughtThrowableThrowsRuntimeException}, hash: 17D55F7D7F6D178D7E5793718376BAD2
    @Test()
    void getAsBooleanWhenCaughtThrowableThrowsRuntimeException() throws Throwable {
        /* Branches:
         * (catch-exception (Throwable)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        FailableBooleanSupplier<Throwable> supplierMock = mock(FailableBooleanSupplier.class);
        try (MockedStatic<Failable> failable = mockStatic(Failable.class, CALLS_REAL_METHODS)) {
            doReturn(false).when(supplierMock).getAsBoolean();
            RuntimeException runtimeException = new RuntimeException();
            Throwable throwable = new Throwable();
            failable.when(() -> Failable.rethrow(throwable)).thenThrow(runtimeException);
            //Act Statement(s)
            final RuntimeException result = assertThrows(RuntimeException.class, () -> {
                Failable.getAsBoolean(supplierMock);
            });
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(runtimeException));
                verify(supplierMock, atLeast(1)).getAsBoolean();
                failable.verify(() -> Failable.rethrow(throwable), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getAsDoubleTest}, hash: F44172A1E19998EC704B6F6531F77C2C
    @Test()
    void getAsDoubleTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        FailableDoubleSupplier<Throwable> failableDoubleSupplierMock = mock(FailableDoubleSupplier.class);
        
        //Act Statement(s)
        double result = Failable.getAsDouble(failableDoubleSupplierMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, closeTo(Double.parseDouble("0.0"), 0.00001)));
    }

    //BaseRock generated method id: ${getAsDoubleWhenCaughtThrowableThrowsRuntimeException}, hash: 0F699E127E2AF7548AFD70FB7735C32C
    @Test()
    void getAsDoubleWhenCaughtThrowableThrowsRuntimeException() throws Throwable {
        /* Branches:
         * (catch-exception (Throwable)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        FailableDoubleSupplier<Throwable> supplierMock = mock(FailableDoubleSupplier.class);
        try (MockedStatic<Failable> failable = mockStatic(Failable.class, CALLS_REAL_METHODS)) {
            doReturn(Double.parseDouble("0.0")).when(supplierMock).getAsDouble();
            RuntimeException runtimeException = new RuntimeException();
            Throwable throwable = new Throwable();
            failable.when(() -> Failable.rethrow(throwable)).thenThrow(runtimeException);
            //Act Statement(s)
            final RuntimeException result = assertThrows(RuntimeException.class, () -> {
                Failable.getAsDouble(supplierMock);
            });
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(runtimeException));
                verify(supplierMock, atLeast(1)).getAsDouble();
                failable.verify(() -> Failable.rethrow(throwable), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getAsIntTest}, hash: FA9CF09B2FD2775429853AD526BB014E
    @Test()
    void getAsIntTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        FailableIntSupplier<Throwable> failableIntSupplierMock = mock(FailableIntSupplier.class);
        
        //Act Statement(s)
        int result = Failable.getAsInt(failableIntSupplierMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${getAsIntWhenCaughtThrowableThrowsRuntimeException}, hash: F9561F459692F4A37EB2BE84AF0467F9
    @Test()
    void getAsIntWhenCaughtThrowableThrowsRuntimeException() throws Throwable {
        /* Branches:
         * (catch-exception (Throwable)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        FailableIntSupplier<Throwable> supplierMock = mock(FailableIntSupplier.class);
        try (MockedStatic<Failable> failable = mockStatic(Failable.class, CALLS_REAL_METHODS)) {
            doReturn(0).when(supplierMock).getAsInt();
            RuntimeException runtimeException = new RuntimeException();
            Throwable throwable = new Throwable();
            failable.when(() -> Failable.rethrow(throwable)).thenThrow(runtimeException);
            //Act Statement(s)
            final RuntimeException result = assertThrows(RuntimeException.class, () -> {
                Failable.getAsInt(supplierMock);
            });
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(runtimeException));
                verify(supplierMock, atLeast(1)).getAsInt();
                failable.verify(() -> Failable.rethrow(throwable), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getAsLongTest}, hash: 4379A4DFDA54467D87B4227B801B1A9B
    @Test()
    void getAsLongTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        FailableLongSupplier<Throwable> failableLongSupplierMock = mock(FailableLongSupplier.class);
        
        //Act Statement(s)
        long result = Failable.getAsLong(failableLongSupplierMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0L)));
    }

    //BaseRock generated method id: ${getAsLongWhenCaughtThrowableThrowsRuntimeException}, hash: 83992E61400FBAC0B5BAB645299EE1C8
    @Test()
    void getAsLongWhenCaughtThrowableThrowsRuntimeException() throws Throwable {
        /* Branches:
         * (catch-exception (Throwable)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        FailableLongSupplier<Throwable> supplierMock = mock(FailableLongSupplier.class);
        try (MockedStatic<Failable> failable = mockStatic(Failable.class, CALLS_REAL_METHODS)) {
            doReturn(0L).when(supplierMock).getAsLong();
            RuntimeException runtimeException = new RuntimeException();
            Throwable throwable = new Throwable();
            failable.when(() -> Failable.rethrow(throwable)).thenThrow(runtimeException);
            //Act Statement(s)
            final RuntimeException result = assertThrows(RuntimeException.class, () -> {
                Failable.getAsLong(supplierMock);
            });
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(runtimeException));
                verify(supplierMock, atLeast(1)).getAsLong();
                failable.verify(() -> Failable.rethrow(throwable), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getAsShortTest}, hash: 5A9A8853836DFF79044F7D28B5546E41
    @Test()
    void getAsShortTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        FailableShortSupplier<Throwable> failableShortSupplierMock = mock(FailableShortSupplier.class);
        
        //Act Statement(s)
        short result = Failable.getAsShort(failableShortSupplierMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((short) 0)));
    }

    //BaseRock generated method id: ${getAsShortWhenCaughtThrowableThrowsRuntimeException}, hash: 74F1E320CA82C98D575CC53ADFD003B2
    @Test()
    void getAsShortWhenCaughtThrowableThrowsRuntimeException() throws Throwable {
        /* Branches:
         * (catch-exception (Throwable)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        FailableShortSupplier<Throwable> supplierMock = mock(FailableShortSupplier.class);
        try (MockedStatic<Failable> failable = mockStatic(Failable.class, CALLS_REAL_METHODS)) {
            doReturn((short) 0).when(supplierMock).getAsShort();
            RuntimeException runtimeException = new RuntimeException();
            Throwable throwable = new Throwable();
            failable.when(() -> Failable.rethrow(throwable)).thenThrow(runtimeException);
            //Act Statement(s)
            final RuntimeException result = assertThrows(RuntimeException.class, () -> {
                Failable.getAsShort(supplierMock);
            });
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(runtimeException));
                verify(supplierMock, atLeast(1)).getAsShort();
                failable.verify(() -> Failable.rethrow(throwable), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${rethrowWhenThrowableNotInstanceOfIOExceptionThrowsUndeclaredThrowableException}, hash: 584987626D06BA615B272679A80A4F09
    @Test()
    void rethrowWhenThrowableNotInstanceOfIOExceptionThrowsUndeclaredThrowableException() {
        /* Branches:
         * (throwable instanceof IOException) : false
         */
         //Arrange Statement(s)
        Throwable throwable = new Throwable();
        //Act Statement(s)
        final UndeclaredThrowableException result = assertThrows(UndeclaredThrowableException.class, () -> {
            Failable.rethrow(throwable);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${runWhenRunnableIsNotNull}, hash: E1A21614F947F24F46DF6243F0810599
    @Test()
    void runWhenRunnableIsNotNull() {
        /* Branches:
         * (runnable != null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Failable.run(failableRunnableMock);
    }

    //BaseRock generated method id: ${runWhenCaughtThrowableThrowsRuntimeException}, hash: CDDCAA5E8B2A35CCEA0E9321D3210165
    @Test()
    void runWhenCaughtThrowableThrowsRuntimeException() throws Throwable {
        /* Branches:
         * (runnable != null) : true
         * (catch-exception (Throwable)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        FailableRunnable<Throwable> runnableMock = mock(FailableRunnable.class);
        try (MockedStatic<Failable> failable = mockStatic(Failable.class, CALLS_REAL_METHODS)) {
            doNothing().when(runnableMock).run();
            RuntimeException runtimeException = new RuntimeException();
            Throwable throwable = new Throwable();
            failable.when(() -> Failable.rethrow(throwable)).thenThrow(runtimeException);
            //Act Statement(s)
            final RuntimeException result = assertThrows(RuntimeException.class, () -> {
                Failable.run(runnableMock);
            });
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(runtimeException));
                verify(runnableMock, atLeast(1)).run();
                failable.verify(() -> Failable.rethrow(throwable), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${streamTest}, hash: 3111FE82B5BB9235EAE64EE386738BB8
    @Test()
    void streamTest() {
        //Arrange Statement(s)
        Collection<Object> collection = new ArrayList<>();
        
        //Act Statement(s)
        Streams.FailableStream result = Failable.stream(collection);
        
        //Assert statement(s)
        //TODO: Please implement equals method in FailableStream for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${stream1Test}, hash: 5045C9BE3F308DAAEEDA510D3ED6E2DB
    @Test()
    void stream1Test() {
        //Arrange Statement(s)
        Stream<Object> stream = Stream.empty();
        
        //Act Statement(s)
        Streams.FailableStream result = Failable.stream(stream);
        
        //Assert statement(s)
        //TODO: Please implement equals method in FailableStream for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${testWhenGetAsBooleanPredicateTestObject1Object2}, hash: ED535862EB37CBD98C0C4C069A399CB7
    @Test()
    void testWhenGetAsBooleanPredicateTestObject1Object2() {
        /* Branches:
         * (getAsBoolean(() -> predicate.test(object1, object2))) : true
         */
         //Arrange Statement(s)
        try (MockedStatic<Failable> failable = mockStatic(Failable.class, CALLS_REAL_METHODS)) {
            failable.when(() -> Failable.getAsBoolean((FailableBooleanSupplier) any())).thenReturn(true);
            FailableBiPredicate<Object, Object, Throwable> failableBiPredicate = FailableBiPredicate.falsePredicate();
            Object object = new Object();
            Object object2 = new Object();
            //Act Statement(s)
            boolean result = Failable.test(failableBiPredicate, object, object2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                failable.verify(() -> Failable.getAsBoolean((FailableBooleanSupplier) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${testWhenGetAsBooleanNotPredicateTestObject1Object2}, hash: 3438178D2239E1659F1C4E2CDB69A8A9
    @Test()
    void testWhenGetAsBooleanNotPredicateTestObject1Object2() {
        /* Branches:
         * (getAsBoolean(() -> predicate.test(object1, object2))) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<Failable> failable = mockStatic(Failable.class, CALLS_REAL_METHODS)) {
            failable.when(() -> Failable.getAsBoolean((FailableBooleanSupplier) any())).thenReturn(false);
            FailableBiPredicate<Object, Object, Throwable> failableBiPredicate = FailableBiPredicate.falsePredicate();
            Object object = new Object();
            Object object2 = new Object();
            //Act Statement(s)
            boolean result = Failable.test(failableBiPredicate, object, object2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                failable.verify(() -> Failable.getAsBoolean((FailableBooleanSupplier) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${test1WhenGetAsBooleanPredicateTestObject}, hash: 92D2743316E4235B3BDB4F9C61512407
    @Test()
    void test1WhenGetAsBooleanPredicateTestObject() {
        /* Branches:
         * (getAsBoolean(() -> predicate.test(object))) : true
         */
         //Arrange Statement(s)
        try (MockedStatic<Failable> failable = mockStatic(Failable.class, CALLS_REAL_METHODS)) {
            failable.when(() -> Failable.getAsBoolean((FailableBooleanSupplier) any())).thenReturn(true);
            FailablePredicate<Object, Throwable> failablePredicate = FailablePredicate.falsePredicate();
            Object object = new Object();
            //Act Statement(s)
            boolean result = Failable.test(failablePredicate, object);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                failable.verify(() -> Failable.getAsBoolean((FailableBooleanSupplier) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${test1WhenGetAsBooleanNotPredicateTestObject}, hash: 11DCE996A6628786715565C7207F2B71
    @Test()
    void test1WhenGetAsBooleanNotPredicateTestObject() {
        /* Branches:
         * (getAsBoolean(() -> predicate.test(object))) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<Failable> failable = mockStatic(Failable.class, CALLS_REAL_METHODS)) {
            failable.when(() -> Failable.getAsBoolean((FailableBooleanSupplier) any())).thenReturn(false);
            FailablePredicate<Object, Throwable> failablePredicate = FailablePredicate.falsePredicate();
            Object object = new Object();
            //Act Statement(s)
            boolean result = Failable.test(failablePredicate, object);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                failable.verify(() -> Failable.getAsBoolean((FailableBooleanSupplier) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${tryWithResourcesWhenResourcesIsNotEmptyAndThIsNull}, hash: EADE7B9B6DC3DA1037C917BBB5583B18
    @Test()
    void tryWithResourcesWhenResourcesIsNotEmptyAndThIsNull() {
        /* Branches:
         * (errorHandler == null) : true
         * (resources != null) : true
         * (for-each(resources)) : true
         * (th != null) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        FailableConsumer<Throwable, Throwable> failableConsumer = null;
        FailableRunnable<Throwable>[] failableRunnableArray = new FailableRunnable[] { failableRunnableMock2 };
        
        //Act Statement(s)
        Failable.tryWithResources(failableRunnableMock, failableConsumer, failableRunnableArray);
    }

    //BaseRock generated method id: ${tryWithResourcesWhenResourcesIsNotNullAndResourcesIsNotEmptyAndThIsNull}, hash: E051355AC878BF90AA42DC845C151B58
    @Test()
    void tryWithResourcesWhenResourcesIsNotNullAndResourcesIsNotEmptyAndThIsNull() {
        /* Branches:
         * (errorHandler == null) : false
         * (resources != null) : true
         * (for-each(resources)) : true
         * (th != null) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        FailableConsumer failableConsumer = FailableConsumer.nop();
        FailableRunnable<Throwable>[] failableRunnableArray = new FailableRunnable[] { failableRunnableMock2 };
        
        //Act Statement(s)
        Failable.tryWithResources(failableRunnableMock, failableConsumer, failableRunnableArray);
    }

    //BaseRock generated method id: ${tryWithResourcesWhenResourcesIsNotNullAndResourcesIsNotEmptyAndThIsNotNull}, hash: 00070B34E56E68ED8FC4BEF54F747807
    @Test()
    void tryWithResourcesWhenResourcesIsNotNullAndResourcesIsNotEmptyAndThIsNotNull() throws Throwable {
        /* Branches:
         * (errorHandler == null) : false
         * (catch-exception (Throwable)) : true
         * (resources != null) : true
         * (for-each(resources)) : true
         * (th != null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        doNothing().when(actionMock).run();
        doNothing().when(runnableMock).run();
        FailableConsumer failableConsumer = FailableConsumer.nop();
        FailableRunnable<Throwable>[] failableRunnableArray = new FailableRunnable[] { runnableMock };
        
        //Act Statement(s)
        Failable.tryWithResources(actionMock, failableConsumer, failableRunnableArray);
        
        //Assert statement(s)
        assertAll("result", () -> {
            verify(actionMock).run();
            verify(runnableMock).run();
        });
    }

    //BaseRock generated method id: ${tryWithResourcesWhenThIsNullAndThIsNotNull}, hash: 8C47BAF9C637387BC308D177BB595A46
    @Test()
    void tryWithResourcesWhenThIsNullAndThIsNotNull() throws Throwable {
        /* Branches:
         * (errorHandler == null) : true
         * (resources != null) : true
         * (for-each(resources)) : true
         * (catch-exception (Throwable)) : true
         * (th == null) : true
         * (th != null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        doNothing().when(actionMock).run();
        doNothing().when(runnableMock).run();
        FailableConsumer<Throwable, Throwable> failableConsumer = null;
        FailableRunnable<Throwable>[] failableRunnableArray = new FailableRunnable[] { runnableMock };
        
        //Act Statement(s)
        Failable.tryWithResources(actionMock, failableConsumer, failableRunnableArray);
        
        //Assert statement(s)
        assertAll("result", () -> {
            verify(actionMock).run();
            verify(runnableMock).run();
        });
    }

    //BaseRock generated method id: ${tryWithResources1Test}, hash: A7595CFAECF2DCD3D9F1D02BA858E7B4
    @Test()
    void tryWithResources1Test() {
        //Arrange Statement(s)
        try (MockedStatic<Failable> failable = mockStatic(Failable.class, CALLS_REAL_METHODS)) {
            FailableRunnable<Throwable>[] failableRunnableArray = new FailableRunnable[] {};
            failable.when(() -> Failable.tryWithResources(failableRunnableMock, (FailableConsumer) null, failableRunnableArray)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            Failable.tryWithResources(failableRunnableMock, failableRunnableArray);
            //Assert statement(s)
            assertAll("result", () -> failable.verify(() -> Failable.tryWithResources(failableRunnableMock, (FailableConsumer) null, failableRunnableArray), atLeast(1)));
        }
    }
}
