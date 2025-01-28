package org.apache.commons.lang3.function;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Disabled;
import java.lang.reflect.Method;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.Supplier;
import org.mockito.MockedStatic;
import java.util.function.BiFunction;
import org.apache.commons.lang3.exception.UncheckedIllegalAccessException;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mock;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.mockito.Mockito.mockStatic;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class MethodInvokersBaseRockGeneratedTest {

    private final Method methodMock = mock(Method.class);

    //BaseRock generated method id: ${asBiConsumerTest}, hash: CA6C597FFAC20CD2A87FAA3ABF764929
    @Test()
    void asBiConsumerTest() {
        //Arrange Statement(s)
        BiConsumer biConsumerMock = mock(BiConsumer.class);
        try (MockedStatic<MethodInvokers> methodInvokers = mockStatic(MethodInvokers.class, CALLS_REAL_METHODS)) {
            methodInvokers.when(() -> MethodInvokers.asInterfaceInstance(BiConsumer.class, methodMock)).thenReturn(biConsumerMock);
            //Act Statement(s)
            BiConsumer<Object, Object> result = MethodInvokers.asBiConsumer(methodMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(biConsumerMock));
                methodInvokers.verify(() -> MethodInvokers.asInterfaceInstance(BiConsumer.class, methodMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${asBiFunctionTest}, hash: C5E3331EC80CFE4C8A5E57001712FF7E
    @Test()
    void asBiFunctionTest() {
        //Arrange Statement(s)
        BiFunction biFunctionMock = mock(BiFunction.class);
        try (MockedStatic<MethodInvokers> methodInvokers = mockStatic(MethodInvokers.class, CALLS_REAL_METHODS)) {
            methodInvokers.when(() -> MethodInvokers.asInterfaceInstance(BiFunction.class, methodMock)).thenReturn(biFunctionMock);
            //Act Statement(s)
            BiFunction<Object, Object, Object> result = MethodInvokers.asBiFunction(methodMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(biFunctionMock));
                methodInvokers.verify(() -> MethodInvokers.asInterfaceInstance(BiFunction.class, methodMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${asFailableBiConsumerTest}, hash: 379202B25E6AB244373231012F2919A5
    @Test()
    void asFailableBiConsumerTest() {
        //Arrange Statement(s)
        try (MockedStatic<MethodInvokers> methodInvokers = mockStatic(MethodInvokers.class, CALLS_REAL_METHODS)) {
            FailableBiConsumer failableBiConsumer = FailableBiConsumer.nop();
            methodInvokers.when(() -> MethodInvokers.asInterfaceInstance(FailableBiConsumer.class, methodMock)).thenReturn(failableBiConsumer);
            //Act Statement(s)
            FailableBiConsumer<Object, Object, Throwable> result = MethodInvokers.asFailableBiConsumer(methodMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(failableBiConsumer));
                methodInvokers.verify(() -> MethodInvokers.asInterfaceInstance(FailableBiConsumer.class, methodMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${asFailableBiFunctionTest}, hash: CAC40EE683E1569217DBC91E1D78A1FF
    @Test()
    void asFailableBiFunctionTest() {
        //Arrange Statement(s)
        try (MockedStatic<MethodInvokers> methodInvokers = mockStatic(MethodInvokers.class, CALLS_REAL_METHODS)) {
            FailableBiFunction failableBiFunction = FailableBiFunction.nop();
            methodInvokers.when(() -> MethodInvokers.asInterfaceInstance(FailableBiFunction.class, methodMock)).thenReturn(failableBiFunction);
            //Act Statement(s)
            FailableBiFunction<Object, Object, Object, Throwable> result = MethodInvokers.asFailableBiFunction(methodMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(failableBiFunction));
                methodInvokers.verify(() -> MethodInvokers.asInterfaceInstance(FailableBiFunction.class, methodMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${asFailableFunctionTest}, hash: 11DE7CEF6941F0A5832590775366B2D7
    @Test()
    void asFailableFunctionTest() {
        //Arrange Statement(s)
        try (MockedStatic<MethodInvokers> methodInvokers = mockStatic(MethodInvokers.class, CALLS_REAL_METHODS)) {
            FailableFunction failableFunction = FailableFunction.identity();
            methodInvokers.when(() -> MethodInvokers.asInterfaceInstance(FailableFunction.class, methodMock)).thenReturn(failableFunction);
            //Act Statement(s)
            FailableFunction<Object, Object, Throwable> result = MethodInvokers.asFailableFunction(methodMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(failableFunction));
                methodInvokers.verify(() -> MethodInvokers.asInterfaceInstance(FailableFunction.class, methodMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${asFailableSupplierTest}, hash: D23D1D32F5CE74E20A5E83D052EABF3F
    @Test()
    void asFailableSupplierTest() {
        //Arrange Statement(s)
        try (MockedStatic<MethodInvokers> methodInvokers = mockStatic(MethodInvokers.class, CALLS_REAL_METHODS)) {
            FailableSupplier failableSupplier = FailableSupplier.nul();
            methodInvokers.when(() -> MethodInvokers.asInterfaceInstance(FailableSupplier.class, methodMock)).thenReturn(failableSupplier);
            //Act Statement(s)
            FailableSupplier<Object, Throwable> result = MethodInvokers.asFailableSupplier(methodMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(failableSupplier));
                methodInvokers.verify(() -> MethodInvokers.asInterfaceInstance(FailableSupplier.class, methodMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${asFunctionTest}, hash: 7E018F03B266BF1A959E74FABE90DAB4
    @Test()
    void asFunctionTest() {
        //Arrange Statement(s)
        Function functionMock = mock(Function.class);
        try (MockedStatic<MethodInvokers> methodInvokers = mockStatic(MethodInvokers.class, CALLS_REAL_METHODS)) {
            methodInvokers.when(() -> MethodInvokers.asInterfaceInstance(Function.class, methodMock)).thenReturn(functionMock);
            //Act Statement(s)
            Function<Object, Object> result = MethodInvokers.asFunction(methodMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(functionMock));
                methodInvokers.verify(() -> MethodInvokers.asInterfaceInstance(Function.class, methodMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${asInterfaceInstanceTest}, hash: 332A7B2699069EC1E492B764CE3CC001
    @Disabled()
    @Test()
    void asInterfaceInstanceTest() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  Potential harmful system call (MethodHandleProxies.asInterfaceInstance) detected; Learn more: https://github.com/Sapient-AI/docs#disabled-generated-tests
         *  Suggestions:
         *  This method should be avoided from unit testing. This can be covered during integration testing.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Object result = MethodInvokers.asInterfaceInstance(Object.class, methodMock);
        
        //Assert statement(s)
        //TODO: Please implement equals method in Object for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${asInterfaceInstanceWhenCaughtIllegalAccessExceptionThrowsUncheckedIllegalAccessException}, hash: AC3971D6C692EACEDA4EDB33F4EAC360
    @Test()
    void asInterfaceInstanceWhenCaughtIllegalAccessExceptionThrowsUncheckedIllegalAccessException() {
        /* Branches:
         * (catch-exception (IllegalAccessException)) : true  #  inside unreflectUnchecked method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: object of type MethodHandles.Lookup - Method: unreflect
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        IllegalAccessException illegalAccessException = new IllegalAccessException();
        //Act Statement(s)
        final UncheckedIllegalAccessException result = assertThrows(UncheckedIllegalAccessException.class, () -> {
            MethodInvokers.asInterfaceInstance(Object.class, methodMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getCause(), is(instanceOf(illegalAccessException.getClass())));
        });
    }

    //BaseRock generated method id: ${asSupplierTest}, hash: E4F6670EA5CB97424DB7C9ABD0D3418C
    @Test()
    void asSupplierTest() {
        //Arrange Statement(s)
        Supplier supplierMock = mock(Supplier.class);
        try (MockedStatic<MethodInvokers> methodInvokers = mockStatic(MethodInvokers.class, CALLS_REAL_METHODS)) {
            methodInvokers.when(() -> MethodInvokers.asInterfaceInstance(Supplier.class, methodMock)).thenReturn(supplierMock);
            //Act Statement(s)
            Supplier result = MethodInvokers.asSupplier(methodMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(supplierMock));
                methodInvokers.verify(() -> MethodInvokers.asInterfaceInstance(Supplier.class, methodMock), atLeast(1));
            });
        }
    }
}
