/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002-2003 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
 *
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 */
package org.apache.commons.lang.exception;

import java.io.PrintWriter;
import java.io.StringWriter;

import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * Tests {@link org.apache.commons.lang.exception.ExceptionUtils}.
 *
 * @author <a href="mailto:dlr@finemaltcoding.com">Daniel Rall</a>
 * @aithor <a href="mailto:steven@caswell.name">Steven Caswell</a>
 * @since 1.0
 */
public class ExceptionUtilsTestCase extends junit.framework.TestCase
{
    private NestableException nested;
    private Throwable withCause;
    private Throwable withoutCause;

    public ExceptionUtilsTestCase(String name)
    {
        super(name);
    }
    
    public static Test suite()
    {
        return new TestSuite(ExceptionUtilsTestCase.class);
    }

    public void setUp()
    {
        withoutCause = createExceptionWithoutCause();
        nested = new NestableException(withoutCause);
        withCause = new ExceptionWithCause(nested);
    }
    
    private Throwable createExceptionWithoutCause(){
        try {
            throw new ExceptionWithoutCause();
        }
        catch (Throwable t){
            return t;
        }
    }

    private Throwable createExceptionWithCause(){
        try {
            try {
                throw new ExceptionWithCause(createExceptionWithoutCause());
            }
            catch (Throwable t){
                throw new ExceptionWithCause(t);
            }
        }
        catch (Throwable t){
            return t;
        }
    } 
    
    public void testGetCause()
    {
        assertNull(ExceptionUtils.getCause(withoutCause));
        assertTrue(ExceptionUtils.getCause(nested) == withoutCause);
        assertTrue(ExceptionUtils.getCause(withCause) == nested);
    }

    public void testGetRootCause()
    {
        assertNull(ExceptionUtils.getRootCause(withoutCause));
        assertTrue(ExceptionUtils.getRootCause(withCause) == withoutCause);
        assertTrue(ExceptionUtils.getRootCause(withCause) == withoutCause);
    }

    public void testGetThrowableCount()
    {
        assertEquals(ExceptionUtils.getThrowableCount(null), 0);
    }

    public void testPrintThrowables()
        throws Exception
    {
        StringWriter writer = new StringWriter(1024);
        Throwable withCause = createExceptionWithCause();
        ExceptionUtils.printRootCauseStackTrace(withCause, 
            new PrintWriter(writer));
        String stackTrace = writer.toString();
        assertTrue("printRootCauseStackTrace(Throwable, PrintWriter) failed",
                   stackTrace.indexOf(ExceptionUtils.WRAPPED_MARKER) != -1);
        writer = new StringWriter(1024);
        ExceptionUtils.printRootCauseStackTrace(withoutCause, 
            new PrintWriter(writer));
        stackTrace = writer.toString();
        assertTrue("printRootCauseStackTrace(Throwable, PrintWriter) failed",
                   stackTrace.indexOf(ExceptionUtils.WRAPPED_MARKER) == -1);
    }

    public void testIsNestedThrowable() {
        assertTrue("SQLException not nested", 
                   ExceptionUtils.isNestedThrowable(new java.sql.SQLException() ) );
        assertTrue("InvocationTargetException not nested", 
                   ExceptionUtils.isNestedThrowable(new java.lang.reflect.InvocationTargetException( new Exception() ) ) );
        assertTrue("NestableRuntimeException not nested", 
                   ExceptionUtils.isNestedThrowable(new NestableRuntimeException() ) );
                   
        // TODO: Come up with a way to test if java.lang.Throwable is nested.
        // bearing in mind that in JDK 1.4 it is, and in 1.3 and previous 
        // it isn't.
    }
    
    /**
     * Provides a method with a well known chained/nested exception
     * name which matches the full signature (e.g. has a return value
     * of <code>Throwable</code>.
     */
    private static class ExceptionWithCause extends Exception
    {
        private Throwable cause;

        public ExceptionWithCause(Throwable cause)
        {
            this.cause = cause;
        }

        public Throwable getCause()
        {
            return cause;
        }
    }

    /**
     * Provides a method with a well known chained/nested exception
     * name which does not match the full signature (e.g. lacks a
     * return value of <code>Throwable</code>.
     */
    private static class ExceptionWithoutCause extends Exception
    {
        public void getTargetException()
        {
        }
    }
}
