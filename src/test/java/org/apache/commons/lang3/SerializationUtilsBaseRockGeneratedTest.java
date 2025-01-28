package org.apache.commons.lang3;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import org.mockito.stubbing.Answer;
import java.io.ByteArrayInputStream;
import java.io.OutputStream;
import org.mockito.MockedStatic;
import java.io.IOException;
import java.io.Serializable;
import java.io.InputStream;
import java.io.ByteArrayOutputStream;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.mock;
import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.mockito.Mockito.mockStatic;
import org.junit.jupiter.api.Disabled;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class SerializationUtilsBaseRockGeneratedTest {

    private final Serializable serializableMock = mock(Serializable.class);

    //BaseRock generated method id: ${cloneWhenObjectIsNull}, hash: B39B59F3B93F24A59B5DA4F5D1DE2FE5
    @Test()
    void cloneWhenObjectIsNull() {
        /* Branches:
         * (object == null) : true
         */
        //Arrange Statement(s)
        Serializable serializable = null;
        //Act Statement(s)
        Serializable result = SerializationUtils.clone(serializable);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${cloneWhenObjectIsNotNull}, hash: 7CA31BD9C52C6CA68BABC4A5102B395B
    @Disabled()
    @Test()
    void cloneWhenObjectIsNotNull() {
        /* Branches:
         * (object == null) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<SerializationUtils> serializationUtils = mockStatic(SerializationUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            serializationUtils.when(() -> SerializationUtils.serialize(serializableMock)).thenReturn(byteArray);
            //Act Statement(s)
            Serializable result = SerializationUtils.clone(serializableMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                serializationUtils.verify(() -> SerializationUtils.serialize(serializableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${cloneWhenDefaultBranchThrowsThrowable}, hash: 75DC33CD0D19CB89B1AFE44965107DB7
    @Test()
    void cloneWhenDefaultBranchThrowsThrowable() {
        /* Branches:
         * (object == null) : false
         * (branch expression (line 125)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<SerializationUtils> serializationUtils = mockStatic(SerializationUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            serializationUtils.when(() -> SerializationUtils.serialize(serializableMock)).thenReturn(byteArray);
            //Act Statement(s)
            final Throwable result = assertThrows(Throwable.class, () -> {
                SerializationUtils.clone(serializableMock);
            });
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                serializationUtils.verify(() -> SerializationUtils.serialize(serializableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${cloneWhenCaughtClassNotFoundExceptionOrIOExceptionThrowsSerializationException}, hash: 2C564020F397000CE47E005B2A8107D2
    @Test()
    void cloneWhenCaughtClassNotFoundExceptionOrIOExceptionThrowsSerializationException() {
        /* Branches:
         * (object == null) : false
         * (catch-exception (ClassNotFoundException | IOException)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<SerializationUtils> serializationUtils = mockStatic(SerializationUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            serializationUtils.when(() -> SerializationUtils.serialize(serializableMock)).thenReturn(byteArray);
            //Act Statement(s)
            final SerializationException result = assertThrows(SerializationException.class, () -> {
                SerializationUtils.clone(serializableMock);
            });
            Exception exception = new Exception();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getCause(), is(instanceOf(exception.getClass())));
                serializationUtils.verify(() -> SerializationUtils.serialize(serializableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${deserializeTest}, hash: E2F861F66EE946F85B5142F10C2FD2A7
    @Disabled()
    @Test()
    void deserializeTest() {
        //Arrange Statement(s)
        try (MockedStatic<SerializationUtils> serializationUtils = mockStatic(SerializationUtils.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            ByteArrayInputStream byteArrayInputStream = mock(ByteArrayInputStream.class);
            serializationUtils.when(() -> SerializationUtils.deserialize(byteArrayInputStream)).thenReturn(object);
            byte[] byteArray = new byte[] {};
            //Act Statement(s)
            Object result = SerializationUtils.deserialize(byteArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(object));
                serializationUtils.verify(() -> SerializationUtils.deserialize(byteArrayInputStream), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${deserialize1Test}, hash: CEC5936BF1D58D72002C2AAE2583C6A1
    @Disabled()
    @Test()
    void deserialize1Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        InputStream inputStream = new ByteArrayInputStream(byteArrayOutputStream.toByteArray());
        //Act Statement(s)
        Object result = SerializationUtils.deserialize(inputStream);
        //Assert statement(s)
        //TODO: Please implement equals method in Object for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${deserialize1WhenDefaultBranchThrowsThrowable}, hash: CBD3F4BFF1660F001E29145B3048B266
    @Test()
    void deserialize1WhenDefaultBranchThrowsThrowable() {
        /* Branches:
         * (branch expression (line 185)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        InputStream inputStream = new ByteArrayInputStream(byteArrayOutputStream.toByteArray());
        //Act Statement(s)
        final Throwable result = assertThrows(Throwable.class, () -> {
            SerializationUtils.deserialize(inputStream);
        });
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${deserialize1WhenCaughtClassNotFoundExceptionOrIOExceptionOrNegativeArraySizeExceptionThrowsSerializationException}, hash: E91ED4E49B79CDCF6EF6DDADF20EEF8B
    @Test()
    void deserialize1WhenCaughtClassNotFoundExceptionOrIOExceptionOrNegativeArraySizeExceptionThrowsSerializationException() {
        /* Branches:
         * (catch-exception (ClassNotFoundException | IOException | NegativeArraySizeException)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        InputStream inputStream = new ByteArrayInputStream(byteArrayOutputStream.toByteArray());
        Exception exception = new Exception();
        //Act Statement(s)
        final SerializationException result = assertThrows(SerializationException.class, () -> {
            SerializationUtils.deserialize(inputStream);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getCause(), is(instanceOf(exception.getClass())));
        });
    }

    //BaseRock generated method id: ${roundtripTest}, hash: 08D111EEA25C23563F735874E7CBAFD5
    @Test()
    void roundtripTest() {
        //Arrange Statement(s)
        Serializable serializableMock2 = mock(Serializable.class);
        try (MockedStatic<SerializationUtils> serializationUtils = mockStatic(SerializationUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            serializationUtils.when(() -> SerializationUtils.serialize(serializableMock)).thenReturn(byteArray);
            serializationUtils.when(() -> SerializationUtils.deserialize(byteArray)).thenReturn(serializableMock2);
            //Act Statement(s)
            Serializable result = SerializationUtils.roundtrip(serializableMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(serializableMock2));
                serializationUtils.verify(() -> SerializationUtils.serialize(serializableMock), atLeast(1));
                serializationUtils.verify(() -> SerializationUtils.deserialize(byteArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${serializeTest}, hash: CC72D448971239428E9C9A426D178E0D
    @Test()
    void serializeTest() {
        //Arrange Statement(s)
        try (MockedStatic<SerializationUtils> serializationUtils = mockStatic(SerializationUtils.class, CALLS_REAL_METHODS)) {
            serializationUtils.when(() -> SerializationUtils.serialize(eq(serializableMock), (ByteArrayOutputStream) any())).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            byte[] result = SerializationUtils.serialize(serializableMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.length, equalTo(0));
                serializationUtils.verify(() -> SerializationUtils.serialize(eq(serializableMock), (ByteArrayOutputStream) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${serialize1Test}, hash: 245581EDC9B4C3BC139F8CDDCCA9435F
    @Test()
    void serialize1Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: out - Method: writeObject
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        //Act Statement(s)
        SerializationUtils.serialize(serializableMock, byteArrayOutputStream);
    }

    //BaseRock generated method id: ${serialize1WhenDefaultBranchThrowsThrowable}, hash: 25F3292DB28F6A048E172723035D2013
    @Disabled()
    @Test()
    void serialize1WhenDefaultBranchThrowsThrowable() {
        /* Branches:
         * (branch expression (line 242)) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: out - Method: writeObject
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        //Act Statement(s)
        final Throwable result = assertThrows(Throwable.class, () -> {
            SerializationUtils.serialize(serializableMock, byteArrayOutputStream);
        });
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${serialize1WhenDefaultBranch}, hash: 989AF958212A42E36D223F8544A6F155
    @Test()
    void serialize1WhenDefaultBranch() {
        /* Branches:
         * (branch expression (line 242)) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: out - Method: writeObject
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        //Act Statement(s)
        SerializationUtils.serialize(serializableMock, byteArrayOutputStream);
    }

    //BaseRock generated method id: ${serialize1WhenCaughtIOExceptionThrowsSerializationException}, hash: C191A04EAA10E1C81668F12E20AA0A61
    @Disabled()
    @Test()
    void serialize1WhenCaughtIOExceptionThrowsSerializationException() {
        /* Branches:
         * (catch-exception (IOException)) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: out - Method: writeObject
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        IOException iOException = new IOException();
        //Act Statement(s)
        final SerializationException result = assertThrows(SerializationException.class, () -> {
            SerializationUtils.serialize(serializableMock, byteArrayOutputStream);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getCause(), is(instanceOf(iOException.getClass())));
        });
    }
}
