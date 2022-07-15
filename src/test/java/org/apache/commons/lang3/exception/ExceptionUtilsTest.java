/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang3.exception;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.test.NotVisibleExceptionFactory;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link org.apache.commons.lang3.exception.ExceptionUtils}.
 *
 * @since 1.0
 */
public class ExceptionUtilsTest extends AbstractLangTest {

    /**
     * Provides a method with a well known chained/nested exception
     * name which matches the full signature (e.g. has a return value
     * of {@code Throwable}).
     */
    private static class ExceptionWithCause extends Exception {
        private static final long serialVersionUID = 1L;

        private Throwable cause;

        ExceptionWithCause(final String str, final Throwable cause) {
            super(str);
            setCause(cause);
        }

        ExceptionWithCause(final Throwable cause) {
            setCause(cause);
        }

        @Override
        public synchronized Throwable getCause() {
            return cause;
        }

        public void setCause(final Throwable cause) {
            this.cause = cause;
        }
    }

    /**
     * Provides a method with a well known chained/nested exception
     * name which does not match the full signature (e.g. lacks a
     * return value of {@code Throwable}).
     */
    private static class ExceptionWithoutCause extends Exception {
        private static final long serialVersionUID = 1L;

        @SuppressWarnings("unused")
        public void getTargetException() {
            // noop
        }
    }

    // Temporary classes to allow the nested exception code to be removed
    // prior to a rewrite of this test class.
    private static class NestableException extends Exception {
        private static final long serialVersionUID = 1L;

        @SuppressWarnings("unused")
        NestableException() {
        }

        NestableException(final Throwable t) {
            super(t);
        }
    }

    public static class TestThrowable extends Throwable {
        private static final long serialVersionUID = 1L;
    }

    private static int redeclareCheckedException() {
        return throwsCheckedException();
    }

    private static int throwsCheckedException() {
        try {
            throw new IOException();
        } catch (final Exception e) {
            return ExceptionUtils.<Integer>rethrow(e);
        }
    }

    private NestableException nested;


    private Throwable withCause;

    private Throwable withoutCause;

    private Throwable jdkNoCause;

    private ExceptionWithCause cyclicCause;

    private Throwable notVisibleException;

    private Throwable createExceptionWithCause() {
        try {
            try {
                throw new ExceptionWithCause(createExceptionWithoutCause());
            } catch (final Throwable t) {
                throw new ExceptionWithCause(t);
            }
        } catch (final Throwable t) {
            return t;
        }
    }

    private Throwable createExceptionWithoutCause() {
        try {
            throw new ExceptionWithoutCause();
        } catch (final Throwable t) {
            return t;
        }
    }

    @BeforeEach
    public void setUp() {
        withoutCause = createExceptionWithoutCause();
        nested = new NestableException(withoutCause);
        withCause = new ExceptionWithCause(nested);
        jdkNoCause = new NullPointerException();
        final ExceptionWithCause a = new ExceptionWithCause(null);
        final ExceptionWithCause b = new ExceptionWithCause(a);
        a.setCause(b);
        cyclicCause = new ExceptionWithCause(a);
        notVisibleException = NotVisibleExceptionFactory.createException(withoutCause);
    }

    @AfterEach
    public void tearDown() {
        withoutCause = null;
        nested = null;
        withCause = null;
        jdkNoCause = null;
        cyclicCause = null;
        notVisibleException = null;
    }

    @Test
    public void test_getMessage_Throwable() {
        Throwable th = null;
        assertEquals("", ExceptionUtils.getMessage(th));

        th = new IllegalArgumentException("Base");
        assertEquals("IllegalArgumentException: Base", ExceptionUtils.getMessage(th));

        th = new ExceptionWithCause("Wrapper", th);
        assertEquals("ExceptionUtilsTest.ExceptionWithCause: Wrapper", ExceptionUtils.getMessage(th));
    }

    @Test
    public void test_getRootCauseMessage_Throwable() {
        Throwable th = null;
        assertEquals("", ExceptionUtils.getRootCauseMessage(th));

        th = new IllegalArgumentException("Base");
        assertEquals("IllegalArgumentException: Base", ExceptionUtils.getRootCauseMessage(th));

        th = new ExceptionWithCause("Wrapper", th);
        assertEquals("IllegalArgumentException: Base", ExceptionUtils.getRootCauseMessage(th));
    }

    @Test
    public void testCatchTechniques() {
        IOException ioe = assertThrows(IOException.class, ExceptionUtilsTest::throwsCheckedException);
        assertEquals(1, ExceptionUtils.getThrowableCount(ioe));

        ioe = assertThrows(IOException.class, ExceptionUtilsTest::redeclareCheckedException);
        assertEquals(1, ExceptionUtils.getThrowableCount(ioe));
    }

    @Test
    public void testConstructor() {
        assertNotNull(new ExceptionUtils());
        final Constructor<?>[] cons = ExceptionUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertTrue(Modifier.isPublic(cons[0].getModifiers()));
        assertTrue(Modifier.isPublic(ExceptionUtils.class.getModifiers()));
        assertFalse(Modifier.isFinal(ExceptionUtils.class.getModifiers()));
    }

    @Test
    public void testForEach_jdkNoCause() {
        final List<Throwable> throwables = new ArrayList<>();
        ExceptionUtils.forEach(jdkNoCause, throwables::add);
        assertEquals(1, throwables.size());
        assertSame(jdkNoCause, throwables.get(0));
    }

    @Test
    public void testForEach_nested() {
        final List<Throwable> throwables = new ArrayList<>();
        ExceptionUtils.forEach(nested, throwables::add);
        assertEquals(2, throwables.size());
        assertSame(nested, throwables.get(0));
        assertSame(withoutCause, throwables.get(1));
    }

    @Test
    public void testForEach_null() {
        final List<Throwable> throwables = new ArrayList<>();
        ExceptionUtils.forEach(null, throwables::add);
        assertEquals(0, throwables.size());
    }

    @Test
    public void testForEach_recursiveCause() {
        final List<Throwable> throwables = new ArrayList<>();
        ExceptionUtils.forEach(cyclicCause, throwables::add);
        assertEquals(3, throwables.size());
        assertSame(cyclicCause, throwables.get(0));
        assertSame(cyclicCause.getCause(), throwables.get(1));
        assertSame(cyclicCause.getCause().getCause(), throwables.get(2));
    }

    @Test
    public void testForEach_withCause() {
        final List<Throwable> throwables = new ArrayList<>();
        ExceptionUtils.forEach(withCause, throwables::add);
        assertEquals(3, throwables.size());
        assertSame(withCause, throwables.get(0));
        assertSame(nested, throwables.get(1));
        assertSame(withoutCause, throwables.get(2));
    }

    @Test
    public void testForEach_withoutCause() {
        final List<Throwable> throwables = new ArrayList<>();
        ExceptionUtils.forEach(withoutCause, throwables::add);
        assertEquals(1, throwables.size());
        assertSame(withoutCause, throwables.get(0));
    }

    @SuppressWarnings("deprecation") // Specifically tests the deprecated methods
    @Test
    public void testGetCause_Throwable() {
        assertSame(null, ExceptionUtils.getCause(null));
        assertSame(null, ExceptionUtils.getCause(withoutCause));
        assertSame(withoutCause, ExceptionUtils.getCause(nested));
        assertSame(nested, ExceptionUtils.getCause(withCause));
        assertSame(null, ExceptionUtils.getCause(jdkNoCause));
        assertSame(cyclicCause.getCause(), ExceptionUtils.getCause(cyclicCause));
        assertSame(cyclicCause.getCause().getCause(), ExceptionUtils.getCause(cyclicCause.getCause()));
        assertSame(cyclicCause.getCause(), ExceptionUtils.getCause(cyclicCause.getCause().getCause()));
        assertSame(withoutCause, ExceptionUtils.getCause(notVisibleException));
    }

    @SuppressWarnings("deprecation") // Specifically tests the deprecated methods
    @Test
    public void testGetCause_ThrowableArray() {
        assertSame(null, ExceptionUtils.getCause(null, null));
        assertSame(null, ExceptionUtils.getCause(null, new String[0]));

        // not known type, so match on supplied method names
        assertSame(nested, ExceptionUtils.getCause(withCause, null));  // default names
        assertSame(null, ExceptionUtils.getCause(withCause, new String[0]));
        assertSame(null, ExceptionUtils.getCause(withCause, new String[]{null}));
        assertSame(nested, ExceptionUtils.getCause(withCause, new String[]{"getCause"}));

        // not known type, so match on supplied method names
        assertSame(null, ExceptionUtils.getCause(withoutCause, null));
        assertSame(null, ExceptionUtils.getCause(withoutCause, new String[0]));
        assertSame(null, ExceptionUtils.getCause(withoutCause, new String[]{null}));
        assertSame(null, ExceptionUtils.getCause(withoutCause, new String[]{"getCause"}));
        assertSame(null, ExceptionUtils.getCause(withoutCause, new String[]{"getTargetException"}));
    }

    @Test
    public void testGetRootCause_Throwable() {
        assertSame(null, ExceptionUtils.getRootCause(null));
        assertSame(withoutCause, ExceptionUtils.getRootCause(withoutCause));
        assertSame(withoutCause, ExceptionUtils.getRootCause(nested));
        assertSame(withoutCause, ExceptionUtils.getRootCause(withCause));
        assertSame(jdkNoCause, ExceptionUtils.getRootCause(jdkNoCause));
        assertSame(cyclicCause.getCause().getCause(), ExceptionUtils.getRootCause(cyclicCause));
    }

    @Test
    public void testGetRootCauseStackTrace_Throwable() {
        assertEquals(0, ExceptionUtils.getRootCauseStackTrace(null).length);

        final Throwable cause = createExceptionWithCause();
        String[] stackTrace = ExceptionUtils.getRootCauseStackTrace(cause);
        boolean match = false;
        for (final String element : stackTrace) {
            if (element.startsWith(ExceptionUtils.WRAPPED_MARKER)) {
                match = true;
                break;
            }
        }
        assertTrue(match);

        stackTrace = ExceptionUtils.getRootCauseStackTrace(withoutCause);
        match = false;
        for (final String element : stackTrace) {
            if (element.startsWith(ExceptionUtils.WRAPPED_MARKER)) {
                match = true;
                break;
            }
        }
        assertFalse(match);
    }

    @Test
    public void testGetRootCauseStackTraceList_Throwable() {
        assertEquals(0, ExceptionUtils.getRootCauseStackTraceList(null).size());

        final Throwable cause = createExceptionWithCause();
        List<String> stackTrace = ExceptionUtils.getRootCauseStackTraceList(cause);
        boolean match = false;
        for (final String element : stackTrace) {
            if (element.startsWith(ExceptionUtils.WRAPPED_MARKER)) {
                match = true;
                break;
            }
        }
        assertTrue(match);

        stackTrace = ExceptionUtils.getRootCauseStackTraceList(withoutCause);
        match = false;
        for (final String element : stackTrace) {
            if (element.startsWith(ExceptionUtils.WRAPPED_MARKER)) {
                match = true;
                break;
            }
        }
        assertFalse(match);
    }

    @Test
    @DisplayName("getStackFrames returns empty string array when the argument is null")
    public void testgetStackFramesHappyPath() {
        final String[] actual = ExceptionUtils.getStackFrames(new Throwable() {
            private static final long serialVersionUID = 1L;

            // provide static stack trace to make test stable
            @Override
            public void printStackTrace(final PrintWriter s) {
                s.write("org.apache.commons.lang3.exception.ExceptionUtilsTest$1\n" +
                    "\tat org.apache.commons.lang3.exception.ExceptionUtilsTest.testgetStackFramesGappyPath(ExceptionUtilsTest.java:706)\n" +
                    "\tat java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke0(Native Method)\n" +
                    "\tat com.intellij.rt.junit.JUnitStarter.prepareStreamsAndStart(JUnitStarter.java:230)\n" +
                    "\tat com.intellij.rt.junit.JUnitStarter.main(JUnitStarter.java:58)\n");
            }
        });

        assertArrayEquals(new String[]{
            "org.apache.commons.lang3.exception.ExceptionUtilsTest$1",
            "\tat org.apache.commons.lang3.exception.ExceptionUtilsTest.testgetStackFramesGappyPath(ExceptionUtilsTest.java:706)",
            "\tat java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke0(Native Method)",
            "\tat com.intellij.rt.junit.JUnitStarter.prepareStreamsAndStart(JUnitStarter.java:230)",
            "\tat com.intellij.rt.junit.JUnitStarter.main(JUnitStarter.java:58)"
        }, actual);
    }

    @Test
    @DisplayName("getStackFrames returns the string array of the stack frames when there is a real exception")
    public void testgetStackFramesNullArg() {
        final String[] actual = ExceptionUtils.getStackFrames((Throwable) null);
        assertEquals(0, actual.length);
    }

    @Test
    public void testGetThrowableCount_Throwable() {
        assertEquals(0, ExceptionUtils.getThrowableCount(null));
        assertEquals(1, ExceptionUtils.getThrowableCount(withoutCause));
        assertEquals(2, ExceptionUtils.getThrowableCount(nested));
        assertEquals(3, ExceptionUtils.getThrowableCount(withCause));
        assertEquals(1, ExceptionUtils.getThrowableCount(jdkNoCause));
        assertEquals(3, ExceptionUtils.getThrowableCount(cyclicCause));
    }

    @Test
    public void testGetThrowableList_Throwable_jdkNoCause() {
        final List<?> throwables = ExceptionUtils.getThrowableList(jdkNoCause);
        assertEquals(1, throwables.size());
        assertSame(jdkNoCause, throwables.get(0));
    }

    @Test
    public void testGetThrowableList_Throwable_nested() {
        final List<?> throwables = ExceptionUtils.getThrowableList(nested);
        assertEquals(2, throwables.size());
        assertSame(nested, throwables.get(0));
        assertSame(withoutCause, throwables.get(1));
    }

    @Test
    public void testGetThrowableList_Throwable_null() {
        final List<?> throwables = ExceptionUtils.getThrowableList(null);
        assertEquals(0, throwables.size());
    }

    @Test
    public void testGetThrowableList_Throwable_recursiveCause() {
        final List<?> throwables = ExceptionUtils.getThrowableList(cyclicCause);
        assertEquals(3, throwables.size());
        assertSame(cyclicCause, throwables.get(0));
        assertSame(cyclicCause.getCause(), throwables.get(1));
        assertSame(cyclicCause.getCause().getCause(), throwables.get(2));
    }

    @Test
    public void testGetThrowableList_Throwable_withCause() {
        final List<?> throwables = ExceptionUtils.getThrowableList(withCause);
        assertEquals(3, throwables.size());
        assertSame(withCause, throwables.get(0));
        assertSame(nested, throwables.get(1));
        assertSame(withoutCause, throwables.get(2));
    }

    @Test
    public void testGetThrowableList_Throwable_withoutCause() {
        final List<?> throwables = ExceptionUtils.getThrowableList(withoutCause);
        assertEquals(1, throwables.size());
        assertSame(withoutCause, throwables.get(0));
    }

    @Test
    public void testGetThrowables_Throwable_jdkNoCause() {
        final Throwable[] throwables = ExceptionUtils.getThrowables(jdkNoCause);
        assertEquals(1, throwables.length);
        assertSame(jdkNoCause, throwables[0]);
    }

    @Test
    public void testGetThrowables_Throwable_nested() {
        final Throwable[] throwables = ExceptionUtils.getThrowables(nested);
        assertEquals(2, throwables.length);
        assertSame(nested, throwables[0]);
        assertSame(withoutCause, throwables[1]);
    }

    @Test
    public void testGetThrowables_Throwable_null() {
        assertEquals(0, ExceptionUtils.getThrowables(null).length);
    }

    @Test
    public void testGetThrowables_Throwable_recursiveCause() {
        final Throwable[] throwables = ExceptionUtils.getThrowables(cyclicCause);
        assertEquals(3, throwables.length);
        assertSame(cyclicCause, throwables[0]);
        assertSame(cyclicCause.getCause(), throwables[1]);
        assertSame(cyclicCause.getCause().getCause(), throwables[2]);
    }

    @Test
    public void testGetThrowables_Throwable_withCause() {
        final Throwable[] throwables = ExceptionUtils.getThrowables(withCause);
        assertEquals(3, throwables.length);
        assertSame(withCause, throwables[0]);
        assertSame(nested, throwables[1]);
        assertSame(withoutCause, throwables[2]);
    }

    @Test
    public void testGetThrowables_Throwable_withoutCause() {
        final Throwable[] throwables = ExceptionUtils.getThrowables(withoutCause);
        assertEquals(1, throwables.length);
        assertSame(withoutCause, throwables[0]);
    }

    @Test
    public void testIndexOf_ThrowableClass() {
        assertEquals(-1, ExceptionUtils.indexOfThrowable(null, null));
        assertEquals(-1, ExceptionUtils.indexOfThrowable(null, NestableException.class));

        assertEquals(-1, ExceptionUtils.indexOfThrowable(withoutCause, null));
        assertEquals(-1, ExceptionUtils.indexOfThrowable(withoutCause, ExceptionWithCause.class));
        assertEquals(-1, ExceptionUtils.indexOfThrowable(withoutCause, NestableException.class));
        assertEquals(0, ExceptionUtils.indexOfThrowable(withoutCause, ExceptionWithoutCause.class));

        assertEquals(-1, ExceptionUtils.indexOfThrowable(nested, null));
        assertEquals(-1, ExceptionUtils.indexOfThrowable(nested, ExceptionWithCause.class));
        assertEquals(0, ExceptionUtils.indexOfThrowable(nested, NestableException.class));
        assertEquals(1, ExceptionUtils.indexOfThrowable(nested, ExceptionWithoutCause.class));

        assertEquals(-1, ExceptionUtils.indexOfThrowable(withCause, null));
        assertEquals(0, ExceptionUtils.indexOfThrowable(withCause, ExceptionWithCause.class));
        assertEquals(1, ExceptionUtils.indexOfThrowable(withCause, NestableException.class));
        assertEquals(2, ExceptionUtils.indexOfThrowable(withCause, ExceptionWithoutCause.class));

        assertEquals(-1, ExceptionUtils.indexOfThrowable(withCause, Exception.class));
        assertEquals(-1, ExceptionUtils.indexOfThrowable(withCause, Throwable.class));
    }

    @Test
    public void testIndexOf_ThrowableClassInt() {
        assertEquals(-1, ExceptionUtils.indexOfThrowable(null, null, 0));
        assertEquals(-1, ExceptionUtils.indexOfThrowable(null, NestableException.class, 0));

        assertEquals(-1, ExceptionUtils.indexOfThrowable(withoutCause, null));
        assertEquals(-1, ExceptionUtils.indexOfThrowable(withoutCause, ExceptionWithCause.class, 0));
        assertEquals(-1, ExceptionUtils.indexOfThrowable(withoutCause, NestableException.class, 0));
        assertEquals(0, ExceptionUtils.indexOfThrowable(withoutCause, ExceptionWithoutCause.class, 0));

        assertEquals(-1, ExceptionUtils.indexOfThrowable(nested, null, 0));
        assertEquals(-1, ExceptionUtils.indexOfThrowable(nested, ExceptionWithCause.class, 0));
        assertEquals(0, ExceptionUtils.indexOfThrowable(nested, NestableException.class, 0));
        assertEquals(1, ExceptionUtils.indexOfThrowable(nested, ExceptionWithoutCause.class, 0));

        assertEquals(-1, ExceptionUtils.indexOfThrowable(withCause, null));
        assertEquals(0, ExceptionUtils.indexOfThrowable(withCause, ExceptionWithCause.class, 0));
        assertEquals(1, ExceptionUtils.indexOfThrowable(withCause, NestableException.class, 0));
        assertEquals(2, ExceptionUtils.indexOfThrowable(withCause, ExceptionWithoutCause.class, 0));

        assertEquals(0, ExceptionUtils.indexOfThrowable(withCause, ExceptionWithCause.class, -1));
        assertEquals(0, ExceptionUtils.indexOfThrowable(withCause, ExceptionWithCause.class, 0));
        assertEquals(-1, ExceptionUtils.indexOfThrowable(withCause, ExceptionWithCause.class, 1));
        assertEquals(-1, ExceptionUtils.indexOfThrowable(withCause, ExceptionWithCause.class, 9));

        assertEquals(-1, ExceptionUtils.indexOfThrowable(withCause, Exception.class, 0));
        assertEquals(-1, ExceptionUtils.indexOfThrowable(withCause, Throwable.class, 0));
    }

    @Test
    public void testIndexOfType_ThrowableClass() {
        assertEquals(-1, ExceptionUtils.indexOfType(null, null));
        assertEquals(-1, ExceptionUtils.indexOfType(null, NestableException.class));

        assertEquals(-1, ExceptionUtils.indexOfType(withoutCause, null));
        assertEquals(-1, ExceptionUtils.indexOfType(withoutCause, ExceptionWithCause.class));
        assertEquals(-1, ExceptionUtils.indexOfType(withoutCause, NestableException.class));
        assertEquals(0, ExceptionUtils.indexOfType(withoutCause, ExceptionWithoutCause.class));

        assertEquals(-1, ExceptionUtils.indexOfType(nested, null));
        assertEquals(-1, ExceptionUtils.indexOfType(nested, ExceptionWithCause.class));
        assertEquals(0, ExceptionUtils.indexOfType(nested, NestableException.class));
        assertEquals(1, ExceptionUtils.indexOfType(nested, ExceptionWithoutCause.class));

        assertEquals(-1, ExceptionUtils.indexOfType(withCause, null));
        assertEquals(0, ExceptionUtils.indexOfType(withCause, ExceptionWithCause.class));
        assertEquals(1, ExceptionUtils.indexOfType(withCause, NestableException.class));
        assertEquals(2, ExceptionUtils.indexOfType(withCause, ExceptionWithoutCause.class));

        assertEquals(0, ExceptionUtils.indexOfType(withCause, Exception.class));
        assertEquals(0, ExceptionUtils.indexOfType(withCause, Throwable.class));
    }

    @Test
    public void testIndexOfType_ThrowableClassInt() {
        assertEquals(-1, ExceptionUtils.indexOfType(null, null, 0));
        assertEquals(-1, ExceptionUtils.indexOfType(null, NestableException.class, 0));

        assertEquals(-1, ExceptionUtils.indexOfType(withoutCause, null));
        assertEquals(-1, ExceptionUtils.indexOfType(withoutCause, ExceptionWithCause.class, 0));
        assertEquals(-1, ExceptionUtils.indexOfType(withoutCause, NestableException.class, 0));
        assertEquals(0, ExceptionUtils.indexOfType(withoutCause, ExceptionWithoutCause.class, 0));

        assertEquals(-1, ExceptionUtils.indexOfType(nested, null, 0));
        assertEquals(-1, ExceptionUtils.indexOfType(nested, ExceptionWithCause.class, 0));
        assertEquals(0, ExceptionUtils.indexOfType(nested, NestableException.class, 0));
        assertEquals(1, ExceptionUtils.indexOfType(nested, ExceptionWithoutCause.class, 0));

        assertEquals(-1, ExceptionUtils.indexOfType(withCause, null));
        assertEquals(0, ExceptionUtils.indexOfType(withCause, ExceptionWithCause.class, 0));
        assertEquals(1, ExceptionUtils.indexOfType(withCause, NestableException.class, 0));
        assertEquals(2, ExceptionUtils.indexOfType(withCause, ExceptionWithoutCause.class, 0));

        assertEquals(0, ExceptionUtils.indexOfType(withCause, ExceptionWithCause.class, -1));
        assertEquals(0, ExceptionUtils.indexOfType(withCause, ExceptionWithCause.class, 0));
        assertEquals(-1, ExceptionUtils.indexOfType(withCause, ExceptionWithCause.class, 1));
        assertEquals(-1, ExceptionUtils.indexOfType(withCause, ExceptionWithCause.class, 9));

        assertEquals(0, ExceptionUtils.indexOfType(withCause, Exception.class, 0));
        assertEquals(0, ExceptionUtils.indexOfType(withCause, Throwable.class, 0));
    }

    @Test
    public void testPrintRootCauseStackTrace_Throwable() {
        ExceptionUtils.printRootCauseStackTrace(null);
        // could pipe system.err to a known stream, but not much point as
        // internally this method calls stream method anyway
    }

    @Test
    public void testPrintRootCauseStackTrace_ThrowableStream() {
        ByteArrayOutputStream out = new ByteArrayOutputStream(1024);
        ExceptionUtils.printRootCauseStackTrace(null, (PrintStream) null);
        ExceptionUtils.printRootCauseStackTrace(null, new PrintStream(out));
        assertEquals(0, out.toString().length());

        out = new ByteArrayOutputStream(1024);
        assertThrows(
                NullPointerException.class,
                () -> ExceptionUtils.printRootCauseStackTrace(withCause, (PrintStream) null));

        out = new ByteArrayOutputStream(1024);
        final Throwable cause = createExceptionWithCause();
        ExceptionUtils.printRootCauseStackTrace(cause, new PrintStream(out));
        String stackTrace = out.toString();
        assertTrue(stackTrace.contains(ExceptionUtils.WRAPPED_MARKER));

        out = new ByteArrayOutputStream(1024);
        ExceptionUtils.printRootCauseStackTrace(withoutCause, new PrintStream(out));
        stackTrace = out.toString();
        assertFalse(stackTrace.contains(ExceptionUtils.WRAPPED_MARKER));
    }

    @Test
    public void testPrintRootCauseStackTrace_ThrowableWriter() {
        StringWriter writer = new StringWriter(1024);
        ExceptionUtils.printRootCauseStackTrace(null, (PrintWriter) null);
        ExceptionUtils.printRootCauseStackTrace(null, new PrintWriter(writer));
        assertEquals(0, writer.getBuffer().length());

        writer = new StringWriter(1024);
        assertThrows(
                NullPointerException.class,
                () -> ExceptionUtils.printRootCauseStackTrace(withCause, (PrintWriter) null));

        writer = new StringWriter(1024);
        final Throwable cause = createExceptionWithCause();
        ExceptionUtils.printRootCauseStackTrace(cause, new PrintWriter(writer));
        String stackTrace = writer.toString();
        assertTrue(stackTrace.contains(ExceptionUtils.WRAPPED_MARKER));

        writer = new StringWriter(1024);
        ExceptionUtils.printRootCauseStackTrace(withoutCause, new PrintWriter(writer));
        stackTrace = writer.toString();
        assertFalse(stackTrace.contains(ExceptionUtils.WRAPPED_MARKER));
    }

    @Test
    public void testRemoveCommonFrames_ListList() {
        assertThrows(NullPointerException.class, () -> ExceptionUtils.removeCommonFrames(null, null));
    }

    @Test
    public void testStream_jdkNoCause() {
        assertEquals(1, ExceptionUtils.stream(jdkNoCause).count());
        assertSame(jdkNoCause, ExceptionUtils.stream(jdkNoCause).toArray()[0]);
    }

    @Test
    public void testStream_nested() {
        assertEquals(2, ExceptionUtils.stream(nested).count());
        final Object[] array = ExceptionUtils.stream(nested).toArray();
        assertSame(nested, array[0]);
        assertSame(withoutCause, array[1]);
    }

    @Test
    public void testStream_null() {
        assertEquals(0, ExceptionUtils.stream(null).count());
    }

    @Test
    public void testStream_recursiveCause() {
        final List<?> throwables = ExceptionUtils.stream(cyclicCause).collect(Collectors.toList());
        assertEquals(3, throwables.size());
        assertSame(cyclicCause, throwables.get(0));
        assertSame(cyclicCause.getCause(), throwables.get(1));
        assertSame(cyclicCause.getCause().getCause(), throwables.get(2));
    }

    @Test
    public void testStream_withCause() {
        final List<?> throwables = ExceptionUtils.stream(withCause).collect(Collectors.toList());
        assertEquals(3, throwables.size());
        assertSame(withCause, throwables.get(0));
        assertSame(nested, throwables.get(1));
        assertSame(withoutCause, throwables.get(2));
    }

    @Test
    public void testStream_withoutCause() {
        final List<?> throwables = ExceptionUtils.stream(withoutCause).collect(Collectors.toList());
        assertEquals(1, throwables.size());
        assertSame(withoutCause, throwables.get(0));
    }

    @Test
    public void testThrow() {
        final Exception expected = new InterruptedException();
        final Exception actual = assertThrows(Exception.class, () -> ExceptionUtils.rethrow(expected));
        assertSame(expected, actual);
    }

    @Test
    public void testThrowableOf_ThrowableClass() {
        assertNull(ExceptionUtils.throwableOfThrowable(null, null));
        assertNull(ExceptionUtils.throwableOfThrowable(null, NestableException.class));

        assertNull(ExceptionUtils.throwableOfThrowable(withoutCause, null));
        assertNull(ExceptionUtils.throwableOfThrowable(withoutCause, ExceptionWithCause.class));
        assertNull(ExceptionUtils.throwableOfThrowable(withoutCause, NestableException.class));
        assertEquals(withoutCause, ExceptionUtils.throwableOfThrowable(withoutCause, ExceptionWithoutCause.class));

        assertNull(ExceptionUtils.throwableOfThrowable(nested, null));
        assertNull(ExceptionUtils.throwableOfThrowable(nested, ExceptionWithCause.class));
        assertEquals(nested, ExceptionUtils.throwableOfThrowable(nested, NestableException.class));
        assertEquals(nested.getCause(), ExceptionUtils.throwableOfThrowable(nested, ExceptionWithoutCause.class));

        assertNull(ExceptionUtils.throwableOfThrowable(withCause, null));
        assertEquals(withCause, ExceptionUtils.throwableOfThrowable(withCause, ExceptionWithCause.class));
        assertEquals(withCause.getCause(), ExceptionUtils.throwableOfThrowable(withCause, NestableException.class));
        assertEquals(withCause.getCause().getCause(), ExceptionUtils.throwableOfThrowable(withCause, ExceptionWithoutCause.class));

        assertNull(ExceptionUtils.throwableOfThrowable(withCause, Exception.class));
        assertNull(ExceptionUtils.throwableOfThrowable(withCause, Throwable.class));
    }

    @Test
    public void testThrowableOf_ThrowableClassInt() {
        assertNull(ExceptionUtils.throwableOfThrowable(null, null, 0));
        assertNull(ExceptionUtils.throwableOfThrowable(null, NestableException.class, 0));

        assertNull(ExceptionUtils.throwableOfThrowable(withoutCause, null));
        assertNull(ExceptionUtils.throwableOfThrowable(withoutCause, ExceptionWithCause.class, 0));
        assertNull(ExceptionUtils.throwableOfThrowable(withoutCause, NestableException.class, 0));
        assertEquals(withoutCause, ExceptionUtils.throwableOfThrowable(withoutCause, ExceptionWithoutCause.class, 0));

        assertNull(ExceptionUtils.throwableOfThrowable(nested, null, 0));
        assertNull(ExceptionUtils.throwableOfThrowable(nested, ExceptionWithCause.class, 0));
        assertEquals(nested, ExceptionUtils.throwableOfThrowable(nested, NestableException.class, 0));
        assertEquals(nested.getCause(), ExceptionUtils.throwableOfThrowable(nested, ExceptionWithoutCause.class, 0));

        assertNull(ExceptionUtils.throwableOfThrowable(withCause, null));
        assertEquals(withCause, ExceptionUtils.throwableOfThrowable(withCause, ExceptionWithCause.class, 0));
        assertEquals(withCause.getCause(), ExceptionUtils.throwableOfThrowable(withCause, NestableException.class, 0));
        assertEquals(withCause.getCause().getCause(), ExceptionUtils.throwableOfThrowable(withCause, ExceptionWithoutCause.class, 0));

        assertEquals(withCause, ExceptionUtils.throwableOfThrowable(withCause, ExceptionWithCause.class, -1));
        assertEquals(withCause, ExceptionUtils.throwableOfThrowable(withCause, ExceptionWithCause.class, 0));
        assertNull(ExceptionUtils.throwableOfThrowable(withCause, ExceptionWithCause.class, 1));
        assertNull(ExceptionUtils.throwableOfThrowable(withCause, ExceptionWithCause.class, 9));

        assertNull(ExceptionUtils.throwableOfThrowable(withCause, Exception.class, 0));
        assertNull(ExceptionUtils.throwableOfThrowable(withCause, Throwable.class, 0));
    }

    @Test
    public void testThrowableOfType_ThrowableClass() {
        assertNull(ExceptionUtils.throwableOfType(null, null));
        assertNull(ExceptionUtils.throwableOfType(null, NestableException.class));

        assertNull(ExceptionUtils.throwableOfType(withoutCause, null));
        assertNull(ExceptionUtils.throwableOfType(withoutCause, ExceptionWithCause.class));
        assertNull(ExceptionUtils.throwableOfType(withoutCause, NestableException.class));
        assertEquals(withoutCause, ExceptionUtils.throwableOfType(withoutCause, ExceptionWithoutCause.class));

        assertNull(ExceptionUtils.throwableOfType(nested, null));
        assertNull(ExceptionUtils.throwableOfType(nested, ExceptionWithCause.class));
        assertEquals(nested, ExceptionUtils.throwableOfType(nested, NestableException.class));
        assertEquals(nested.getCause(), ExceptionUtils.throwableOfType(nested, ExceptionWithoutCause.class));

        assertNull(ExceptionUtils.throwableOfType(withCause, null));
        assertEquals(withCause, ExceptionUtils.throwableOfType(withCause, ExceptionWithCause.class));
        assertEquals(withCause.getCause(), ExceptionUtils.throwableOfType(withCause, NestableException.class));
        assertEquals(withCause.getCause().getCause(), ExceptionUtils.throwableOfType(withCause, ExceptionWithoutCause.class));

        assertEquals(withCause, ExceptionUtils.throwableOfType(withCause, Exception.class));
        assertEquals(withCause, ExceptionUtils.throwableOfType(withCause, Throwable.class));
    }

    @Test
    public void testThrowableOfType_ThrowableClassInt() {
        assertNull(ExceptionUtils.throwableOfType(null, null, 0));
        assertNull(ExceptionUtils.throwableOfType(null, NestableException.class, 0));

        assertNull(ExceptionUtils.throwableOfType(withoutCause, null));
        assertNull(ExceptionUtils.throwableOfType(withoutCause, ExceptionWithCause.class, 0));
        assertNull(ExceptionUtils.throwableOfType(withoutCause, NestableException.class, 0));
        assertEquals(withoutCause, ExceptionUtils.throwableOfType(withoutCause, ExceptionWithoutCause.class, 0));

        assertNull(ExceptionUtils.throwableOfType(nested, null, 0));
        assertNull(ExceptionUtils.throwableOfType(nested, ExceptionWithCause.class, 0));
        assertEquals(nested, ExceptionUtils.throwableOfType(nested, NestableException.class, 0));
        assertEquals(nested.getCause(), ExceptionUtils.throwableOfType(nested, ExceptionWithoutCause.class, 0));

        assertNull(ExceptionUtils.throwableOfType(withCause, null));
        assertEquals(withCause, ExceptionUtils.throwableOfType(withCause, ExceptionWithCause.class, 0));
        assertEquals(withCause.getCause(), ExceptionUtils.throwableOfType(withCause, NestableException.class, 0));
        assertEquals(withCause.getCause().getCause(), ExceptionUtils.throwableOfType(withCause, ExceptionWithoutCause.class, 0));

        assertEquals(withCause, ExceptionUtils.throwableOfType(withCause, ExceptionWithCause.class, -1));
        assertEquals(withCause, ExceptionUtils.throwableOfType(withCause, ExceptionWithCause.class, 0));
        assertNull(ExceptionUtils.throwableOfType(withCause, ExceptionWithCause.class, 1));
        assertNull(ExceptionUtils.throwableOfType(withCause, ExceptionWithCause.class, 9));

        assertEquals(withCause, ExceptionUtils.throwableOfType(withCause, Exception.class, 0));
        assertEquals(withCause, ExceptionUtils.throwableOfType(withCause, Throwable.class, 0));
    }

    @Test
    public void testWrapAndUnwrapCheckedException() {
        final Throwable t = assertThrows(Throwable.class, () -> ExceptionUtils.wrapAndThrow(new IOException()));
        assertTrue(ExceptionUtils.hasCause(t, IOException.class));
    }

    @Test
    public void testWrapAndUnwrapError() {
        final Throwable t = assertThrows(Throwable.class, () -> ExceptionUtils.wrapAndThrow(new OutOfMemoryError()));
        assertTrue(ExceptionUtils.hasCause(t, Error.class));
    }

    @Test
    public void testWrapAndUnwrapRuntimeException() {
        final Throwable t = assertThrows(Throwable.class, () -> ExceptionUtils.wrapAndThrow(new IllegalArgumentException()));
        assertTrue(ExceptionUtils.hasCause(t, RuntimeException.class));
    }

    @Test
    public void testWrapAndUnwrapThrowable() {
        final Throwable t = assertThrows(Throwable.class, () -> ExceptionUtils.wrapAndThrow(new TestThrowable()));
        assertTrue(ExceptionUtils.hasCause(t, TestThrowable.class));
    }
}
