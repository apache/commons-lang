package org.apache.commons.lang3;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.util.function.BiConsumer;
import java.util.concurrent.Callable;
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
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.doReturn;
import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.any;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mockStatic;
import org.junit.jupiter.api.Disabled;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class FunctionsBaseRockGeneratedTest {

    private final Functions.FailableBiPredicate<Object, Object, Throwable> functionsFailableBiPredicateMock = mock(Functions.FailableBiPredicate.class);

    private final Functions.FailablePredicate<Object, Throwable> functionsFailablePredicateMock = mock(Functions.FailablePredicate.class);

    private final Functions.FailableRunnable<Throwable> functionsFailableRunnableMock = mock(Functions.FailableRunnable.class);

    //BaseRock generated method id: ${acceptTest}, hash: C60414D3EE2AC0587CFB2AA96F228D65
    @Test()
    void acceptTest() {
        //Arrange Statement(s)
        Functions.FailableBiConsumer<Object, Object, Throwable> functionsFailableBiConsumerMock = mock(Functions.FailableBiConsumer.class);
        try (MockedStatic<Functions> functions = mockStatic(Functions.class, CALLS_REAL_METHODS)) {
            functions.when(() -> Functions.run((Functions.FailableRunnable) any())).thenAnswer((Answer<Void>) invocation -> null);
            Object object = new Object();
            Object object2 = new Object();
            //Act Statement(s)
            Functions.accept(functionsFailableBiConsumerMock, object, object2);
            //Assert statement(s)
            assertAll("result", () -> functions.verify(() -> Functions.run((Functions.FailableRunnable) any()), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${accept1Test}, hash: 99EDE3E9459BD760D9DAED0E23A913B6
    @Test()
    void accept1Test() {
        //Arrange Statement(s)
        Functions.FailableConsumer<Object, Throwable> functionsFailableConsumerMock = mock(Functions.FailableConsumer.class);
        try (MockedStatic<Functions> functions = mockStatic(Functions.class, CALLS_REAL_METHODS)) {
            functions.when(() -> Functions.run((Functions.FailableRunnable) any())).thenAnswer((Answer<Void>) invocation -> null);
            Object object = new Object();
            //Act Statement(s)
            Functions.accept(functionsFailableConsumerMock, object);
            //Assert statement(s)
            assertAll("result", () -> functions.verify(() -> Functions.run((Functions.FailableRunnable) any()), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${applyTest}, hash: B5E105B2CAF70C29CA93A1E74669677B
    @Test()
    void applyTest() {
        //Arrange Statement(s)
        Functions.FailableBiFunction<Object, Object, Object, Throwable> functionsFailableBiFunctionMock = mock(Functions.FailableBiFunction.class);
        try (MockedStatic<Functions> functions = mockStatic(Functions.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            functions.when(() -> Functions.get((Functions.FailableSupplier) any())).thenReturn(object);
            Object object2 = new Object();
            Object object3 = new Object();
            //Act Statement(s)
            Object result = Functions.apply(functionsFailableBiFunctionMock, object2, object3);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(object));
                functions.verify(() -> Functions.get((Functions.FailableSupplier) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${apply1Test}, hash: 9C766E5622DF0E6FAC0BD26E0FE5640F
    @Test()
    void apply1Test() {
        //Arrange Statement(s)
        Functions.FailableFunction<Object, Object, Throwable> functionsFailableFunctionMock = mock(Functions.FailableFunction.class);
        try (MockedStatic<Functions> functions = mockStatic(Functions.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            functions.when(() -> Functions.get((Functions.FailableSupplier) any())).thenReturn(object);
            Object object2 = new Object();
            //Act Statement(s)
            Object result = Functions.apply(functionsFailableFunctionMock, object2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(object));
                functions.verify(() -> Functions.get((Functions.FailableSupplier) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${asBiConsumerTest}, hash: DC57E5A5493984EC2570A5936F2F0D9E
    @Test()
    void asBiConsumerTest() {
        //Arrange Statement(s)
        Functions.FailableBiConsumer<Object, Object, ?> functionsFailableBiConsumerMock = mock(Functions.FailableBiConsumer.class);
        //Act Statement(s)
        BiConsumer<Object, Object> result = Functions.asBiConsumer(functionsFailableBiConsumerMock);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${asBiFunctionTest}, hash: 56C48FD3A7AED7ED9B6FB785DF0333EB
    @Test()
    void asBiFunctionTest() {
        //Arrange Statement(s)
        Functions.FailableBiFunction<Object, Object, Object, ?> functionsFailableBiFunctionMock = mock(Functions.FailableBiFunction.class);
        //Act Statement(s)
        BiFunction<Object, Object, Object> result = Functions.asBiFunction(functionsFailableBiFunctionMock);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${asBiPredicateTest}, hash: D35F5E28596E412BB13AED670844EFD9
    @Test()
    void asBiPredicateTest() {
        //Arrange Statement(s)
        Functions.FailableBiPredicate<Object, Object, ?> functionsFailableBiPredicateMock = mock(Functions.FailableBiPredicate.class);
        //Act Statement(s)
        BiPredicate<Object, Object> result = Functions.asBiPredicate(functionsFailableBiPredicateMock);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${asCallableTest}, hash: 4596ED1734EA20B12491A53BECA064E8
    @Test()
    void asCallableTest() {
        //Arrange Statement(s)
        Functions.FailableCallable<Object, ?> functionsFailableCallableMock = mock(Functions.FailableCallable.class);
        //Act Statement(s)
        Callable result = Functions.asCallable(functionsFailableCallableMock);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${asConsumerTest}, hash: 803023458E3BB0F2C5D704D119218480
    @Test()
    void asConsumerTest() {
        //Arrange Statement(s)
        Functions.FailableConsumer<Object, ?> functionsFailableConsumerMock = mock(Functions.FailableConsumer.class);
        //Act Statement(s)
        Consumer result = Functions.asConsumer(functionsFailableConsumerMock);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${asFunctionTest}, hash: 0FD1B0E98DBE446BD06526CEC5737C7D
    @Test()
    void asFunctionTest() {
        //Arrange Statement(s)
        Functions.FailableFunction<Object, Object, ?> functionsFailableFunctionMock = mock(Functions.FailableFunction.class);
        //Act Statement(s)
        Function<Object, Object> result = Functions.asFunction(functionsFailableFunctionMock);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${asPredicateTest}, hash: 44BAED0FD07DEB5C11DBD8D4E2ABC775
    @Test()
    void asPredicateTest() {
        //Arrange Statement(s)
        Functions.FailablePredicate<Object, ?> functionsFailablePredicateMock = mock(Functions.FailablePredicate.class);
        //Act Statement(s)
        Predicate result = Functions.asPredicate(functionsFailablePredicateMock);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${asRunnableTest}, hash: 23657F2019F1BCAC1BB9B774A7F62B59
    @Test()
    void asRunnableTest() {
        //Arrange Statement(s)
        Functions.FailableRunnable<?> functionsFailableRunnableMock = mock(Functions.FailableRunnable.class);
        //Act Statement(s)
        Runnable result = Functions.asRunnable(functionsFailableRunnableMock);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${asSupplierTest}, hash: 76407461F92585183E612499F5652F73
    @Test()
    void asSupplierTest() {
        //Arrange Statement(s)
        Functions.FailableSupplier<Object, ?> functionsFailableSupplierMock = mock(Functions.FailableSupplier.class);
        //Act Statement(s)
        Supplier result = Functions.asSupplier(functionsFailableSupplierMock);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${callTest}, hash: E8152EEC65F90686954A70C5E1E00F2E
    @Test()
    void callTest() {
        //Arrange Statement(s)
        Functions.FailableCallable<Object, Throwable> functionsFailableCallableMock = mock(Functions.FailableCallable.class);
        try (MockedStatic<Functions> functions = mockStatic(Functions.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            functions.when(() -> Functions.get((Functions.FailableSupplier) any())).thenReturn(object);
            //Act Statement(s)
            Object result = Functions.call(functionsFailableCallableMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(object));
                functions.verify(() -> Functions.get((Functions.FailableSupplier) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getTest}, hash: 9B83D283A6AB958FC4F98C4B7BA808D5
    @Disabled()
    @Test()
    void getTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Functions.FailableSupplier<Object, Throwable> functionsFailableSupplierMock = mock(Functions.FailableSupplier.class);
        //Act Statement(s)
        Object result = Functions.get(functionsFailableSupplierMock);
        //Assert statement(s)
        //TODO: Please implement equals method in Object for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getWhenCaughtThrowableThrowsRuntimeException}, hash: C326AFD8D4803E3C65CA3F4AA761B597
    @Disabled()
    @Test()
    void getWhenCaughtThrowableThrowsRuntimeException() throws Throwable {
        /* Branches:
         * (catch-exception (Throwable)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Functions.FailableSupplier<Object, Throwable> supplierMock = mock(Functions.FailableSupplier.class);
        try (MockedStatic<Functions> functions = mockStatic(Functions.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            doReturn(object).when(supplierMock).get();
            RuntimeException runtimeException = new RuntimeException();
            Throwable throwable = new Throwable();
            functions.when(() -> Functions.rethrow(throwable)).thenThrow(runtimeException);
            //Act Statement(s)
            final RuntimeException result = assertThrows(RuntimeException.class, () -> {
                Functions.get(supplierMock);
            });
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(runtimeException));
                verify(supplierMock, atLeast(1)).get();
                functions.verify(() -> Functions.rethrow(throwable), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${rethrowWhenThrowableNotInstanceOfIOExceptionThrowsUndeclaredThrowableException}, hash: 8AB0169ED14BE9BFD2DCAC3A463AE060
    @Test()
    void rethrowWhenThrowableNotInstanceOfIOExceptionThrowsUndeclaredThrowableException() {
        /* Branches:
         * (throwable instanceof IOException) : false
         */
        //Arrange Statement(s)
        Throwable throwable = new Throwable();
        //Act Statement(s)
        final UndeclaredThrowableException result = assertThrows(UndeclaredThrowableException.class, () -> {
            Functions.rethrow(throwable);
        });
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${runTest}, hash: C4000F37B880EB7E480D77FE1A6FC301
    @Test()
    void runTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        Functions.run(functionsFailableRunnableMock);
    }

    //BaseRock generated method id: ${runWhenCaughtThrowableThrowsRuntimeException}, hash: 3DE9D667F1C5157AA5DB777ACA86D418
    @Disabled()
    @Test()
    void runWhenCaughtThrowableThrowsRuntimeException() throws Throwable {
        /* Branches:
         * (catch-exception (Throwable)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Functions.FailableRunnable<Throwable> runnableMock = mock(Functions.FailableRunnable.class);
        try (MockedStatic<Functions> functions = mockStatic(Functions.class, CALLS_REAL_METHODS)) {
            doNothing().when(runnableMock).run();
            RuntimeException runtimeException = new RuntimeException();
            Throwable throwable = new Throwable();
            functions.when(() -> Functions.rethrow(throwable)).thenThrow(runtimeException);
            //Act Statement(s)
            final RuntimeException result = assertThrows(RuntimeException.class, () -> {
                Functions.run(runnableMock);
            });
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(runtimeException));
                verify(runnableMock, atLeast(1)).run();
                functions.verify(() -> Functions.rethrow(throwable), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${streamTest}, hash: C23ED60D7764D3975CA14154060367BC
    @Test()
    void streamTest() {
        //Arrange Statement(s)
        Collection<Object> collection = new ArrayList<>();
        //Act Statement(s)
        Streams.FailableStream result = Functions.stream(collection);
        //Assert statement(s)
        //TODO: Please implement equals method in FailableStream for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${stream1Test}, hash: 2902FD393901E5AD576E82BB7A99287B
    @Test()
    void stream1Test() {
        //Arrange Statement(s)
        Stream<Object> stream = Stream.empty();
        //Act Statement(s)
        Streams.FailableStream result = Functions.stream(stream);
        //Assert statement(s)
        //TODO: Please implement equals method in FailableStream for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${testWhenSupplierGetAsBoolean}, hash: 19A517DBE3203A0C26E4165FFD29E170
    @Disabled()
    @Test()
    void testWhenSupplierGetAsBoolean() {
        /* Branches:
         * (supplier.getAsBoolean()) : true  #  inside getAsBoolean method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: supplier - Method: getAsBoolean
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        //Act Statement(s)
        boolean result = Functions.test(functionsFailableBiPredicateMock, object, object2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${testWhenSupplierNotGetAsBoolean}, hash: DC723E3C7C673FD94A08BB2575240303
    @Test()
    void testWhenSupplierNotGetAsBoolean() {
        /* Branches:
         * (supplier.getAsBoolean()) : false  #  inside getAsBoolean method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: supplier - Method: getAsBoolean
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        //Act Statement(s)
        boolean result = Functions.test(functionsFailableBiPredicateMock, object, object2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${testWhenCaughtThrowableThrowsRuntimeException}, hash: E52C14D2CC85E0676CAB049787BB0430
    @Disabled()
    @Test()
    void testWhenCaughtThrowableThrowsRuntimeException() {
        /* Branches:
         * (catch-exception (Throwable)) : true  #  inside getAsBoolean method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: supplier - Method: getAsBoolean
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<Functions> functions = mockStatic(Functions.class, CALLS_REAL_METHODS)) {
            RuntimeException runtimeException = new RuntimeException();
            Throwable throwable = new Throwable();
            functions.when(() -> Functions.rethrow(throwable)).thenThrow(runtimeException);
            Object object = new Object();
            Object object2 = new Object();
            //Act Statement(s)
            final RuntimeException result = assertThrows(RuntimeException.class, () -> {
                Functions.test(functionsFailableBiPredicateMock, object, object2);
            });
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(runtimeException));
                functions.verify(() -> Functions.rethrow(throwable), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${test1WhenSupplierGetAsBoolean}, hash: CF90A99D2AAF692B8A86C56B3DEAA049
    @Disabled()
    @Test()
    void test1WhenSupplierGetAsBoolean() {
        /* Branches:
         * (supplier.getAsBoolean()) : true  #  inside getAsBoolean method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: supplier - Method: getAsBoolean
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object object = new Object();
        //Act Statement(s)
        boolean result = Functions.test(functionsFailablePredicateMock, object);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${test1WhenSupplierNotGetAsBoolean}, hash: BD86CD0811D40F53E750EE88390EC645
    @Test()
    void test1WhenSupplierNotGetAsBoolean() {
        /* Branches:
         * (supplier.getAsBoolean()) : false  #  inside getAsBoolean method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: supplier - Method: getAsBoolean
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object object = new Object();
        //Act Statement(s)
        boolean result = Functions.test(functionsFailablePredicateMock, object);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${test1WhenCaughtThrowableThrowsRuntimeException}, hash: 94B209B48851C7536CBE8ADF987DF1D0
    @Disabled()
    @Test()
    void test1WhenCaughtThrowableThrowsRuntimeException() {
        /* Branches:
         * (catch-exception (Throwable)) : true  #  inside getAsBoolean method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: supplier - Method: getAsBoolean
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<Functions> functions = mockStatic(Functions.class, CALLS_REAL_METHODS)) {
            RuntimeException runtimeException = new RuntimeException();
            Throwable throwable = new Throwable();
            functions.when(() -> Functions.rethrow(throwable)).thenThrow(runtimeException);
            Object object = new Object();
            //Act Statement(s)
            final RuntimeException result = assertThrows(RuntimeException.class, () -> {
                Functions.test(functionsFailablePredicateMock, object);
            });
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(runtimeException));
                functions.verify(() -> Functions.rethrow(throwable), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${tryWithResourcesWhenErrorHandlerIsNotNull}, hash: C962B4A5F8840EE86406DE8286B028AD
    @Test()
    void tryWithResourcesWhenErrorHandlerIsNotNull() {
        /* Branches:
         * (errorHandler != null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Functions.FailableConsumer<Throwable, Throwable> functionsFailableConsumerMock = mock(Functions.FailableConsumer.class);
        Functions.FailableRunnable<Throwable>[] failableRunnableArray = new Functions.FailableRunnable[] {};
        //Act Statement(s)
        Functions.tryWithResources(functionsFailableRunnableMock, functionsFailableConsumerMock, failableRunnableArray);
    }

    //BaseRock generated method id: ${tryWithResourcesWhenErrorHandlerIsNull}, hash: A7DDB728F4276B5E3558BE3399172E5D
    @Test()
    void tryWithResourcesWhenErrorHandlerIsNull() {
        /* Branches:
         * (errorHandler != null) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Functions.FailableConsumer<Throwable, Throwable> functionsFailableConsumer = null;
        Functions.FailableRunnable<Throwable>[] failableRunnableArray = new Functions.FailableRunnable[] {};
        //Act Statement(s)
        Functions.tryWithResources(functionsFailableRunnableMock, functionsFailableConsumer, failableRunnableArray);
    }

    //BaseRock generated method id: ${tryWithResources1Test}, hash: 60224AA5BDC7EA822F53637BB4201A4B
    @Test()
    void tryWithResources1Test() {
        //Arrange Statement(s)
        try (MockedStatic<Functions> functions = mockStatic(Functions.class, CALLS_REAL_METHODS)) {
            Functions.FailableRunnable<Throwable>[] failableRunnableArray = new Functions.FailableRunnable[] {};
            functions.when(() -> Functions.tryWithResources(functionsFailableRunnableMock, (Functions.FailableConsumer) null, failableRunnableArray)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            Functions.tryWithResources(functionsFailableRunnableMock, failableRunnableArray);
            //Assert statement(s)
            assertAll("result", () -> functions.verify(() -> Functions.tryWithResources(functionsFailableRunnableMock, (Functions.FailableConsumer) null, failableRunnableArray), atLeast(1)));
        }
    }
}
