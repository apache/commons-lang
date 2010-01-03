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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;
import java.sql.SQLException;
import java.util.List;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.apache.commons.lang3.SystemUtils;

/**
 * Tests {@link org.apache.commons.lang3.exception.ExceptionUtils}.
 * 
 * <h3>Notes</h3>
 * <p>
 * Make sure this exception code does not depend on Java 1.4 nested exceptions. SVN revision 38990 does not compile with
 * Java 1.3.1.
 * </p>
 * <ul>
 * <li>Compiled with Sun Java 1.3.1_15</li>
 * <li>Tested with Sun Java 1.3.1_15</li>
 * <li>Tested with Sun Java 1.4.2_12</li>
 * <li>Tested with Sun Java 1.5.0_08</li>
 * <li>All of the above on Windows XP SP2 + patches.</li>
 * </ul>
 * <p>
 * Gary Gregory; August 16, 2006.
 * </p>
 * 
 * @author Apache Software Foundation
 * @author Daniel L. Rall
 * @author <a href="mailto:steven@caswell.name">Steven Caswell</a>
 * @author <a href="mailto:ggregory@seagullsw.com">Gary Gregory</a>
 * @since 1.0
 */
public class ExceptionUtilsTest extends TestCase {
    
    private NestableException nested;
    private Throwable withCause;
    private Throwable withoutCause;
    private Throwable jdkNoCause;
    private ExceptionWithCause cyclicCause;

    public ExceptionUtilsTest(String name) {
        super(name);
    }

    @Override
    public void setUp() {
        withoutCause = createExceptionWithoutCause();
        nested = new NestableException(withoutCause);
        withCause = new ExceptionWithCause(nested);
        jdkNoCause = new NullPointerException();
        ExceptionWithCause a = new ExceptionWithCause(null);
        ExceptionWithCause b = new ExceptionWithCause(a);
        a.setCause(b);
        cyclicCause = new ExceptionWithCause(a);
    }

    @Override
    protected void tearDown() throws Exception {
        withoutCause = null;
        nested = null;
        withCause = null;
        jdkNoCause = null;
        cyclicCause = null;
    }

    //-----------------------------------------------------------------------
    private Throwable createExceptionWithoutCause() {
        try {
            throw new ExceptionWithoutCause();
        } catch (Throwable t) {
            return t;
        }
    }

    private Throwable createExceptionWithCause() {
        try {
            try {
                throw new ExceptionWithCause(createExceptionWithoutCause());
            } catch (Throwable t) {
                throw new ExceptionWithCause(t);
            }
        } catch (Throwable t) {
            return t;
        }
    }

    //-----------------------------------------------------------------------
    
    public void testConstructor() {
        assertNotNull(new ExceptionUtils());
        Constructor<?>[] cons = ExceptionUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertEquals(true, Modifier.isPublic(cons[0].getModifiers()));
        assertEquals(true, Modifier.isPublic(ExceptionUtils.class.getModifiers()));
        assertEquals(false, Modifier.isFinal(ExceptionUtils.class.getModifiers()));
    }
    
    //-----------------------------------------------------------------------
    public void testGetCause_Throwable() {
        assertSame(null, ExceptionUtils.getCause(null));
        assertSame(null, ExceptionUtils.getCause(withoutCause));
        assertSame(withoutCause, ExceptionUtils.getCause(nested));
        assertSame(nested, ExceptionUtils.getCause(withCause));
        assertSame(null, ExceptionUtils.getCause(jdkNoCause));
        assertSame(cyclicCause.getCause(), ExceptionUtils.getCause(cyclicCause));
        assertSame(((ExceptionWithCause) cyclicCause.getCause()).getCause(), ExceptionUtils.getCause(cyclicCause.getCause()));
        assertSame(cyclicCause.getCause(), ExceptionUtils.getCause(((ExceptionWithCause) cyclicCause.getCause()).getCause()));
    }

    public void testGetCause_ThrowableArray() {
        assertSame(null, ExceptionUtils.getCause(null, null));
        assertSame(null, ExceptionUtils.getCause(null, new String[0]));

        // not known type, so match on supplied method names
        assertSame(nested, ExceptionUtils.getCause(withCause, null));  // default names
        assertSame(null, ExceptionUtils.getCause(withCause, new String[0]));
        assertSame(null, ExceptionUtils.getCause(withCause, new String[] {null}));
        assertSame(nested, ExceptionUtils.getCause(withCause, new String[] {"getCause"}));
        
        // not known type, so match on supplied method names
        assertSame(null, ExceptionUtils.getCause(withoutCause, null));
        assertSame(null, ExceptionUtils.getCause(withoutCause, new String[0]));
        assertSame(null, ExceptionUtils.getCause(withoutCause, new String[] {null}));
        assertSame(null, ExceptionUtils.getCause(withoutCause, new String[] {"getCause"}));
        assertSame(null, ExceptionUtils.getCause(withoutCause, new String[] {"getTargetException"}));
    }

    public void testGetRootCause_Throwable() {
        assertSame(null, ExceptionUtils.getRootCause(null));
        assertSame(null, ExceptionUtils.getRootCause(withoutCause));
        assertSame(withoutCause, ExceptionUtils.getRootCause(nested));
        assertSame(withoutCause, ExceptionUtils.getRootCause(withCause));
        assertSame(null, ExceptionUtils.getRootCause(jdkNoCause));
        assertSame(((ExceptionWithCause) cyclicCause.getCause()).getCause(), ExceptionUtils.getRootCause(cyclicCause));
    }

    //-----------------------------------------------------------------------
    public void testGetThrowableCount_Throwable() {
        assertEquals(0, ExceptionUtils.getThrowableCount(null));
        assertEquals(1, ExceptionUtils.getThrowableCount(withoutCause));
        assertEquals(2, ExceptionUtils.getThrowableCount(nested));
        assertEquals(3, ExceptionUtils.getThrowableCount(withCause));
        assertEquals(1, ExceptionUtils.getThrowableCount(jdkNoCause));
        assertEquals(3, ExceptionUtils.getThrowableCount(cyclicCause));
    }

    //-----------------------------------------------------------------------
    public void testGetThrowables_Throwable_null() {
        assertEquals(0, ExceptionUtils.getThrowables(null).length);
    }

    public void testGetThrowables_Throwable_withoutCause() {
        Throwable[] throwables = ExceptionUtils.getThrowables(withoutCause);
        assertEquals(1, throwables.length);
        assertSame(withoutCause, throwables[0]);
    }

    public void testGetThrowables_Throwable_nested() {
        Throwable[] throwables = ExceptionUtils.getThrowables(nested);
        assertEquals(2, throwables.length);
        assertSame(nested, throwables[0]);
        assertSame(withoutCause, throwables[1]);
    }

    public void testGetThrowables_Throwable_withCause() {
        Throwable[] throwables = ExceptionUtils.getThrowables(withCause);
        assertEquals(3, throwables.length);
        assertSame(withCause, throwables[0]);
        assertSame(nested, throwables[1]);
        assertSame(withoutCause, throwables[2]);
    }

    public void testGetThrowables_Throwable_jdkNoCause() {
        Throwable[] throwables = ExceptionUtils.getThrowables(jdkNoCause);
        assertEquals(1, throwables.length);
        assertSame(jdkNoCause, throwables[0]);
    }

    public void testGetThrowables_Throwable_recursiveCause() {
        Throwable[] throwables = ExceptionUtils.getThrowables(cyclicCause);
        assertEquals(3, throwables.length);
        assertSame(cyclicCause, throwables[0]);
        assertSame(cyclicCause.getCause(), throwables[1]);
        assertSame(((ExceptionWithCause) cyclicCause.getCause()).getCause(), throwables[2]);
    }

    //-----------------------------------------------------------------------
    public void testGetThrowableList_Throwable_null() {
        List<?> throwables = ExceptionUtils.getThrowableList(null);
        assertEquals(0, throwables.size());
    }

    public void testGetThrowableList_Throwable_withoutCause() {
        List<?> throwables = ExceptionUtils.getThrowableList(withoutCause);
        assertEquals(1, throwables.size());
        assertSame(withoutCause, throwables.get(0));
    }

    public void testGetThrowableList_Throwable_nested() {
        List<?> throwables = ExceptionUtils.getThrowableList(nested);
        assertEquals(2, throwables.size());
        assertSame(nested, throwables.get(0));
        assertSame(withoutCause, throwables.get(1));
    }

    public void testGetThrowableList_Throwable_withCause() {
        List<?> throwables = ExceptionUtils.getThrowableList(withCause);
        assertEquals(3, throwables.size());
        assertSame(withCause, throwables.get(0));
        assertSame(nested, throwables.get(1));
        assertSame(withoutCause, throwables.get(2));
    }

    public void testGetThrowableList_Throwable_jdkNoCause() {
        List<?> throwables = ExceptionUtils.getThrowableList(jdkNoCause);
        assertEquals(1, throwables.size());
        assertSame(jdkNoCause, throwables.get(0));
    }

    public void testGetThrowableList_Throwable_recursiveCause() {
        List<?> throwables = ExceptionUtils.getThrowableList(cyclicCause);
        assertEquals(3, throwables.size());
        assertSame(cyclicCause, throwables.get(0));
        assertSame(cyclicCause.getCause(), throwables.get(1));
        assertSame(((ExceptionWithCause) cyclicCause.getCause()).getCause(), throwables.get(2));
    }

    //-----------------------------------------------------------------------
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
    }

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
    }

    //-----------------------------------------------------------------------
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
    }

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
    }

    //-----------------------------------------------------------------------
    public void testPrintRootCauseStackTrace_Throwable() throws Exception {
        ExceptionUtils.printRootCauseStackTrace(null);
        // could pipe system.err to a known stream, but not much point as
        // internally this method calls stram method anyway
    }
    
    public void testPrintRootCauseStackTrace_ThrowableStream() throws Exception {
        ByteArrayOutputStream out = new ByteArrayOutputStream(1024);
        ExceptionUtils.printRootCauseStackTrace(null, (PrintStream) null);
        ExceptionUtils.printRootCauseStackTrace(null, new PrintStream(out));
        assertEquals(0, out.toString().length());
        
        out = new ByteArrayOutputStream(1024);
        try {
            ExceptionUtils.printRootCauseStackTrace(withCause, (PrintStream) null);
            fail();
        } catch (IllegalArgumentException ex) {
        }
        
        out = new ByteArrayOutputStream(1024);
        Throwable withCause = createExceptionWithCause();
        ExceptionUtils.printRootCauseStackTrace(withCause, new PrintStream(out));
        String stackTrace = out.toString();
        assertTrue(stackTrace.indexOf(ExceptionUtils.WRAPPED_MARKER) != -1);
        
        out = new ByteArrayOutputStream(1024);
        ExceptionUtils.printRootCauseStackTrace(withoutCause, new PrintStream(out));
        stackTrace = out.toString();
        assertTrue(stackTrace.indexOf(ExceptionUtils.WRAPPED_MARKER) == -1);
    }

    public void testPrintRootCauseStackTrace_ThrowableWriter() throws Exception {
        StringWriter writer = new StringWriter(1024);
        ExceptionUtils.printRootCauseStackTrace(null, (PrintWriter) null);
        ExceptionUtils.printRootCauseStackTrace(null, new PrintWriter(writer));
        assertEquals(0, writer.getBuffer().length());
        
        writer = new StringWriter(1024);
        try {
            ExceptionUtils.printRootCauseStackTrace(withCause, (PrintWriter) null);
            fail();
        } catch (IllegalArgumentException ex) {
        }
        
        writer = new StringWriter(1024);
        Throwable withCause = createExceptionWithCause();
        ExceptionUtils.printRootCauseStackTrace(withCause, new PrintWriter(writer));
        String stackTrace = writer.toString();
        assertTrue(stackTrace.indexOf(ExceptionUtils.WRAPPED_MARKER) != -1);
        
        writer = new StringWriter(1024);
        ExceptionUtils.printRootCauseStackTrace(withoutCause, new PrintWriter(writer));
        stackTrace = writer.toString();
        assertTrue(stackTrace.indexOf(ExceptionUtils.WRAPPED_MARKER) == -1);
    }

    //-----------------------------------------------------------------------
    public void testGetRootCauseStackTrace_Throwable() throws Exception {
        assertEquals(0, ExceptionUtils.getRootCauseStackTrace(null).length);
        
        Throwable withCause = createExceptionWithCause();
        String[] stackTrace = ExceptionUtils.getRootCauseStackTrace(withCause);
        boolean match = false;
        for (int i = 0; i < stackTrace.length; i++) {
            if (stackTrace[i].startsWith(ExceptionUtils.WRAPPED_MARKER)) {
                match = true;
                break;
            }
        }
        assertEquals(true, match);
        
        stackTrace = ExceptionUtils.getRootCauseStackTrace(withoutCause);
        match = false;
        for (int i = 0; i < stackTrace.length; i++) {
            if (stackTrace[i].startsWith(ExceptionUtils.WRAPPED_MARKER)) {
                match = true;
                break;
            }
        }
        assertEquals(false, match);
    }

    public void testRemoveCommonFrames_ListList() throws Exception {
        try {
            ExceptionUtils.removeCommonFrames(null, null);
            fail();
        } catch (IllegalArgumentException ex) {
        }
    }

    public void test_getMessage_Throwable() {
        Throwable th = null;
        assertEquals("", ExceptionUtils.getMessage(th));
        
        th = new IllegalArgumentException("Base");
        assertEquals("IllegalArgumentException: Base", ExceptionUtils.getMessage(th));
        
        th = new ExceptionWithCause("Wrapper", th);
        assertEquals("ExceptionUtilsTest.ExceptionWithCause: Wrapper", ExceptionUtils.getMessage(th));
    }

    public void test_getRootCauseMessage_Throwable() {
        Throwable th = null;
        assertEquals("", ExceptionUtils.getRootCauseMessage(th));
        
        th = new IllegalArgumentException("Base");
        assertEquals("IllegalArgumentException: Base", ExceptionUtils.getRootCauseMessage(th));
        
        th = new ExceptionWithCause("Wrapper", th);
        assertEquals("IllegalArgumentException: Base", ExceptionUtils.getRootCauseMessage(th));
    }

    //-----------------------------------------------------------------------
    /**
     * Provides a method with a well known chained/nested exception
     * name which matches the full signature (e.g. has a return value
     * of <code>Throwable</code>.
     */
    private static class ExceptionWithCause extends Exception {
        private Throwable cause;

        public ExceptionWithCause(String str, Throwable cause) {
            super(str);
            setCause(cause);
        }

        public ExceptionWithCause(Throwable cause) {
            super();
            setCause(cause);
        }

        @Override
        public Throwable getCause() {
            return cause;
        }

        public void setCause(Throwable cause) {
            this.cause = cause;
        }
    }

    /**
     * Provides a method with a well known chained/nested exception
     * name which does not match the full signature (e.g. lacks a
     * return value of <code>Throwable</code>.
     */
    private static class ExceptionWithoutCause extends Exception {
        @SuppressWarnings("unused")
        public void getTargetException() {
        }
    }

    // Temporary classes to allow the nested exception code to be removed 
    // prior to a rewrite of this test class. 
    private static class NestableException extends Exception { 
        @SuppressWarnings("unused")
        public NestableException() { super(); }
        public NestableException(Throwable t) { super(t); }
    }

}
