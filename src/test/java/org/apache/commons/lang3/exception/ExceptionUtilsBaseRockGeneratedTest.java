package org.apache.commons.lang3.exception;

import org.apache.commons.lang3.exception.ExceptionUtils;

import java.io.PrintWriter;
import java.io.PrintStream;
import static org.hamcrest.Matchers.nullValue;
import org.junit.jupiter.api.Test;
import java.io.OutputStream;
import static org.mockito.Mockito.atLeast;
import static org.hamcrest.MatcherAssert.assertThat;
import java.util.function.Consumer;
import org.mockito.MockedStatic;
import java.util.Arrays;
import static org.junit.jupiter.api.Assertions.assertAll;
import java.util.List;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.anyList;
import org.junit.jupiter.api.Timeout;
import java.util.stream.Stream;
import static org.mockito.Mockito.mock;
import static org.hamcrest.Matchers.notNullValue;
import org.mockito.stubbing.Answer;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import java.util.ArrayList;
import java.io.StringWriter;
import static org.hamcrest.Matchers.containsInRelativeOrder;
import java.lang.reflect.UndeclaredThrowableException;
import java.io.ByteArrayOutputStream;
import java.io.Writer;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class // ... [rest of the methods remain unchanged]
ExceptionUtilsBaseRockGeneratedTest {

    private final UndeclaredThrowableException undeclaredThrowableExceptionMock = mock(UndeclaredThrowableException.class);

    // ... [rest of the methods remain unchanged]
    //BaseRock generated method id: ${printRootCauseStackTrace2WhenThrowableIsNull}, hash: E04C2C3FE5D7A193DAE3033B0D3D3B8C
    @Test
    void printRootCauseStackTrace2WhenThrowableIsNull() {
        // Arrange Statement(s)
        Throwable throwable = null;
        StringWriter stringWriter = new StringWriter();
        PrintWriter printWriter = new PrintWriter(stringWriter);
        // Act Statement(s)
        ExceptionUtils.printRootCauseStackTrace(throwable, printWriter);
    }

    //BaseRock generated method id: ${printRootCauseStackTrace2WhenThrowableIsNotNull}, hash: ACC2FE6C7226D3F632436470A9052CDE
    @Test
    void printRootCauseStackTrace2WhenThrowableIsNotNull() {
        // Arrange Statement(s)
        try (MockedStatic<ExceptionUtils> exceptionUtils = mockStatic(ExceptionUtils.class, CALLS_REAL_METHODS)) {
            List<String> stringList = new ArrayList<>(Arrays.asList("A"));
            Throwable throwable = new Throwable();
            exceptionUtils.when(() -> ExceptionUtils.getRootCauseStackTraceList(throwable)).thenReturn(stringList);
            StringWriter stringWriter = new StringWriter();
            PrintWriter printWriter = new PrintWriter(stringWriter);
            // Act Statement(s)
            ExceptionUtils.printRootCauseStackTrace(throwable, printWriter);
            // Assert statement(s)
            assertAll("result", () -> exceptionUtils.verify(() -> ExceptionUtils.getRootCauseStackTraceList(throwable), atLeast(1)));
        }
    }
}
