/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002 The Apache Software Foundation.  All rights
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
package org.apache.commons.lang.functor;

import java.io.Serializable;
import java.util.Date;
import java.util.TimeZone;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.commons.lang.SerializationException;
/**
 * Tests the org.apache.commons.lang.functor.FactoryUtils class.
 *
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @version $Id: TestFactoryUtils.java,v 1.1 2002/11/05 16:45:13 bayard Exp $
 */
public class TestFactoryUtils extends junit.framework.TestCase {

    /**
     * Construct
     */
    public TestFactoryUtils(String name) {
        super(name);
    }

    /**
     * Return class aa a test suite.
     */
    public static Test suite() {
        return new TestSuite(TestFactoryUtils.class);
    }

    /**
     * Set up instance variables required by this test case.
     */
    public void setUp() {
    }

    /**
     * Tear down instance variables required by this test case.
     */
    public void tearDown() {
    }

    // exceptionFactory
    //------------------------------------------------------------------

    public void testExceptionFactory() {
        assertNotNull(FactoryUtils.exceptionFactory());
        assertSame(FactoryUtils.exceptionFactory(), FactoryUtils.exceptionFactory());
        try {
            FactoryUtils.exceptionFactory().create();
        } catch (FactoryException ex) {
            try {
                FactoryUtils.exceptionFactory().create();
            } catch (FactoryException ex2) {
                return;
            }
        }
        fail();
    }
    
    // nullFactory
    //------------------------------------------------------------------
    
    public void testNullFactory() {
        Factory factory = FactoryUtils.nullFactory();
        assertNotNull(factory);
        Object created = factory.create();
        assertNull(created);
    }

    // constantFactory
    //------------------------------------------------------------------
    
    public void testConstantFactoryNull() {
        Factory factory = FactoryUtils.constantFactory(null);
        assertNotNull(factory);
        Object created = factory.create();
        assertNull(created);
    }

    public void testConstantFactoryConstant() {
        Integer constant = new Integer(9);
        Factory factory = FactoryUtils.constantFactory(constant);
        assertNotNull(factory);
        Object created = factory.create();
        assertSame(constant, created);
    }

    // prototypeFactory
    //------------------------------------------------------------------
    
    public void testPrototypeFactoryNull() {
        try {
            Factory factory = FactoryUtils.prototypeFactory(null);
            
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }

    public void testPrototypeFactoryPublicCloneMethod() {
        Date proto = new Date();
        Factory factory = FactoryUtils.prototypeFactory(proto);
        assertNotNull(factory);
        Object created = factory.create();
        assertTrue(proto != created);
        assertEquals(proto, created);
    }

    public void testPrototypeFactoryPublicCopyConstructor() {
        Mock1 proto = new Mock1(6);
        Factory factory = FactoryUtils.prototypeFactory(proto);
        assertNotNull(factory);
        Object created = factory.create();
        assertTrue(proto != created);
        assertEquals(proto, created);
    }

    public void testPrototypeFactoryPublicSerialization() {
        Integer proto = new Integer(9);
        Factory factory = FactoryUtils.prototypeFactory(proto);
        assertNotNull(factory);
        Object created = factory.create();
        assertTrue(proto != created);
        assertEquals(proto, created);
    }

    public void testPrototypeFactoryPublicSerializationError() {
        Mock2 proto = new Mock2(new Object());
        Factory factory = FactoryUtils.prototypeFactory(proto);
        assertNotNull(factory);
        try {
            Object created = factory.create();
            
        } catch (FactoryException ex) {
            assertTrue(ex.getCause() instanceof SerializationException);
            return;
        }
        fail();
    }

    public void testPrototypeFactoryPublicBad() {
        Object proto = new Object();
        try {
            Factory factory = FactoryUtils.prototypeFactory(proto);
            
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }

    public static class Mock1 {
        private final int iVal;
        public Mock1(int val) {
            iVal = val;
        }
        public Mock1(Mock1 mock) {
            iVal = mock.iVal;
        }
        public boolean equals(Object obj) {
            if (obj instanceof Mock1) {
                if (iVal == ((Mock1) obj).iVal) {
                    return true;
                }
            }
            return false;
        }
    }
    
    public static class Mock2 implements Serializable {
        private final Object iVal;
        public Mock2(Object val) {
            iVal = val;
        }
        public boolean equals(Object obj) {
            if (obj instanceof Mock2) {
                if (iVal == ((Mock2) obj).iVal) {
                    return true;
                }
            }
            return false;
        }
    }
    
    // reflectionFactory
    //------------------------------------------------------------------
    
    public void testReflectionFactoryNull() {
        try {
            Factory factory = FactoryUtils.reflectionFactory(null);
            
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }

    public void testReflectionFactorySimple() {
        Factory factory = FactoryUtils.reflectionFactory(Date.class);
        assertNotNull(factory);
        Object created = factory.create();
        assertTrue(created instanceof Date);
        assertEquals((double) System.currentTimeMillis(), (double) ((Date) created).getTime(), 0.01d);
    }

    public void testReflectionFactoryMismatch() {
        try {
            Factory factory = FactoryUtils.reflectionFactory(Date.class, null, new Object[] {null});
            
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }

    public void testReflectionFactoryNoConstructor() {
        try {
            Factory factory = FactoryUtils.reflectionFactory(Date.class, new Class[] {Long.class}, new Object[] {null});
            
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }

    public void testReflectionFactoryComplex() {
        TimeZone.setDefault(TimeZone.getTimeZone("GMT"));
        Factory factory = FactoryUtils.reflectionFactory(Date.class,
            new Class[] {Integer.TYPE, Integer.TYPE, Integer.TYPE},
            new Object[] {new Integer(70), new Integer(0), new Integer(2)});
        assertNotNull(factory);
        Object created = factory.create();
        assertTrue(created instanceof Date);
        assertEquals(new Date(1000 * 60 * 60 * 24), created);
    }

}
