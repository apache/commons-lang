package org.apache.commons.lang3.builder;

import org.apache.commons.lang3.builder.Reflection;

import java.util.stream.Stream;
import java.lang.reflect.Field;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.params.provider.Arguments;
import org.mockito.Mockito;
import java.util.Objects;
import static org.mockito.Mockito.*;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

class ReflectionBaseRockGeneratedTest {

    //BaseRock generated method id: ${testGetUncheckedWithValidField}, hash: 402024C7572E867B4CA5525E4ABB484F
    @Test
    void testGetUncheckedWithValidField() throws NoSuchFieldException {
        TestObject testObject = new TestObject();
        Field field = TestObject.class.getDeclaredField("testField");
        field.setAccessible(true);
        Object result = Reflection.getUnchecked(field, testObject);
        assertEquals("test", result);
    }

    //BaseRock generated method id: ${testGetUncheckedWithNullField}, hash: 0AC45560BF1B0B8F26AF8C70AB1344CA
    @Test
    void testGetUncheckedWithNullField() {
        TestObject testObject = new TestObject();
        assertThrows(NullPointerException.class, () -> Reflection.getUnchecked(null, testObject));
    }

    //BaseRock generated method id: ${testGetUncheckedWithIllegalAccessException}, hash: 4595F87B82CC56AD17ACFB9024D67998
    @ParameterizedTest
    @MethodSource("provideIllegalAccessExceptionCases")
    void testGetUncheckedWithIllegalAccessException(Field field, Object obj) throws IllegalAccessException {
        Field mockedField = mock(Field.class);
        when(mockedField.get(any())).thenThrow(new IllegalAccessException("Test exception"));
        assertThrows(IllegalArgumentException.class, () -> Reflection.getUnchecked(mockedField, obj));
    }

    private static Stream<Arguments> provideIllegalAccessExceptionCases() {
        return Stream.of(Arguments.of(mock(Field.class), new Object()), Arguments.of(mock(Field.class), null));
    }

    private static class TestObject {

        private final String testField = "test";
    }
}
