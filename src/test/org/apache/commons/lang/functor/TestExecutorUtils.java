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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.commons.lang.functor.Predicate;
import org.apache.commons.lang.functor.PredicateUtils;
/**
 * Tests the org.apache.commons.lang.functor.ExecutorUtils class.
 *
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @version $Id: TestExecutorUtils.java,v 1.2 2002/11/22 22:52:40 bayard Exp $
 */
public class TestExecutorUtils extends junit.framework.TestCase {

    // JDK 1.2 compliancy
    private static final Map EMPTY_MAP = Collections.unmodifiableMap(new HashMap());

    private static final Object cObject = new Object();
    private static final Object cString = "Hello";
    private static final Object cInteger = new Integer(6);

    /**
     * Construct
     */
    public TestExecutorUtils(String name) {
        super(name);
    }

    /**
     * Return class aa a test suite.
     */
    public static Test suite() {
        return new TestSuite(TestExecutorUtils.class);
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
    
    static class MockExecutor implements Executor {
        int count = 0;
        
        /**
         * @see org.apache.commons.lang.functor.Executor#execute(Object)
         */
        public void execute(Object object) {
            count++;
        }

    }

    // exceptionExecutor
    //------------------------------------------------------------------

    public void testExceptionExecutor() {
        assertNotNull(ExecutorUtils.exceptionExecutor());
        assertSame(ExecutorUtils.exceptionExecutor(), ExecutorUtils.exceptionExecutor());
        try {
            ExecutorUtils.exceptionExecutor().execute(null);
        } catch (ExecutorException ex) {
            try {
                ExecutorUtils.exceptionExecutor().execute(cString);
            } catch (ExecutorException ex2) {
                return;
            }
        }
        fail();
    }
    
    // nopExecutor
    //------------------------------------------------------------------

    public void testNopExecutor() {
        StringBuffer buf = new StringBuffer("Hello");
        ExecutorUtils.nopExecutor().execute(null);
        assertEquals("Hello", buf.toString());
        ExecutorUtils.nopExecutor().execute("Hello");
        assertEquals("Hello", buf.toString());
    }

    // invokeExecutor
    //------------------------------------------------------------------

    public void testInvokeExecutor() {
        StringBuffer buf = new StringBuffer("Hello");
        ExecutorUtils.invokerExecutor("reverse").execute(buf);
        assertEquals("olleH", buf.toString());
        buf = new StringBuffer("Hello");
        ExecutorUtils.invokerExecutor("setLength", new Class[] {Integer.TYPE}, new Object[] {new Integer(2)}).execute(buf);
        assertEquals("He", buf.toString());
    }

    // forExecutor
    //------------------------------------------------------------------

    public void testForExecutor() {
        MockExecutor cmd = new MockExecutor();
        ExecutorUtils.forExecutor(5, cmd).execute(null);
        assertEquals(5, cmd.count);
        try {
            ExecutorUtils.forExecutor(-1, new MockExecutor());
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }

    // whileExecutor
    //------------------------------------------------------------------

    public void testWhileExecutor() {
        MockExecutor cmd = new MockExecutor();
        ExecutorUtils.whileExecutor(PredicateUtils.falsePredicate(), cmd).execute(null);
        assertEquals(0, cmd.count);
        try {
            ExecutorUtils.whileExecutor(null, null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }

    // doWhileExecutor
    //------------------------------------------------------------------

    public void testDoWhileExecutor() {
        MockExecutor cmd = new MockExecutor();
        ExecutorUtils.doWhileExecutor(cmd, PredicateUtils.falsePredicate()).execute(null);
        assertEquals(1, cmd.count);
        try {
            ExecutorUtils.doWhileExecutor(null, null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }

    // chainedExecutor
    //------------------------------------------------------------------

    public void testChainedExecutor() {
        MockExecutor a = new MockExecutor();
        MockExecutor b = new MockExecutor();
        ExecutorUtils.chainedExecutor(a, b).execute(null);
        assertEquals(1, a.count);
        assertEquals(1, b.count);
        
        a = new MockExecutor();
        b = new MockExecutor();
        ExecutorUtils.chainedExecutor(new Executor[] {a, b, a}).execute(null);
        assertEquals(2, a.count);
        assertEquals(1, b.count);
        
        a = new MockExecutor();
        b = new MockExecutor();
        Collection coll = new ArrayList();
        coll.add(b);
        coll.add(a);
        coll.add(b);
        ExecutorUtils.chainedExecutor(coll).execute(null);
        assertEquals(1, a.count);
        assertEquals(2, b.count);
    }

    public void testChainedExecutorEx1a() {
        try {
            ExecutorUtils.chainedExecutor(null, null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testChainedExecutorEx1b() {
        try {
            ExecutorUtils.chainedExecutor((Executor[]) null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testChainedExecutorEx1c() {
        try {
            ExecutorUtils.chainedExecutor((Collection) null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testChainedExecutorEx2() {
        try {
            ExecutorUtils.chainedExecutor(new Executor[0]);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testChainedExecutorEx3() {
        try {
            ExecutorUtils.chainedExecutor(new Executor[] {null, null});
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testChainedExecutorEx4() {
        try {
            ExecutorUtils.chainedExecutor(Collections.EMPTY_LIST);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testChainedExecutorEx5() {
        try {
            Collection coll = new ArrayList();
            coll.add(null);
            coll.add(null);
            ExecutorUtils.chainedExecutor(coll);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    // switchExecutor
    //------------------------------------------------------------------

    public void testSwitchExecutor() {
        MockExecutor a = new MockExecutor();
        MockExecutor b = new MockExecutor();
        ExecutorUtils.switchExecutor(PredicateUtils.truePredicate(), a, b).execute(null);
        assertEquals(1, a.count);
        assertEquals(0, b.count);
        
        a = new MockExecutor();
        b = new MockExecutor();
        ExecutorUtils.switchExecutor(PredicateUtils.falsePredicate(), a, b).execute(null);
        assertEquals(0, a.count);
        assertEquals(1, b.count);
        
        a = new MockExecutor();
        b = new MockExecutor();
        ExecutorUtils.switchExecutor(
            new Predicate[] {PredicateUtils.equalPredicate("HELLO"), PredicateUtils.equalPredicate("THERE")}, 
            new Executor[] {a, b}).execute("WELL");
        assertEquals(0, a.count);
        assertEquals(0, b.count);
        
        a = new MockExecutor();
        b = new MockExecutor();
        ExecutorUtils.switchExecutor(
            new Predicate[] {PredicateUtils.equalPredicate("HELLO"), PredicateUtils.equalPredicate("THERE")}, 
            new Executor[] {a, b}).execute("HELLO");
        assertEquals(1, a.count);
        assertEquals(0, b.count);
        
        a = new MockExecutor();
        b = new MockExecutor();
        MockExecutor c = new MockExecutor();
        ExecutorUtils.switchExecutor(
            new Predicate[] {PredicateUtils.equalPredicate("HELLO"), PredicateUtils.equalPredicate("THERE")}, 
            new Executor[] {a, b}, c).execute("WELL");
        assertEquals(0, a.count);
        assertEquals(0, b.count);
        assertEquals(1, c.count);
        
        a = new MockExecutor();
        b = new MockExecutor();
        Map map = new HashMap();
        map.put(PredicateUtils.equalPredicate("HELLO"), a);
        map.put(PredicateUtils.equalPredicate("THERE"), b);
        ExecutorUtils.switchExecutor(map).execute(null);
        assertEquals(0, a.count);
        assertEquals(0, b.count);

        a = new MockExecutor();
        b = new MockExecutor();
        map = new HashMap();
        map.put(PredicateUtils.equalPredicate("HELLO"), a);
        map.put(PredicateUtils.equalPredicate("THERE"), b);
        ExecutorUtils.switchExecutor(map).execute("THERE");
        assertEquals(0, a.count);
        assertEquals(1, b.count);

        a = new MockExecutor();
        b = new MockExecutor();
        c = new MockExecutor();
        map = new HashMap();
        map.put(PredicateUtils.equalPredicate("HELLO"), a);
        map.put(PredicateUtils.equalPredicate("THERE"), b);
        map.put(null, c);
        ExecutorUtils.switchExecutor(map).execute("WELL");
        assertEquals(0, a.count);
        assertEquals(0, b.count);
        assertEquals(1, c.count);
    }

    public void testSwitchExecutorEx1a() {
        try {
            ExecutorUtils.switchExecutor(null, null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testSwitchExecutorEx1b() {
        try {
            ExecutorUtils.switchExecutor((Predicate[]) null, (Executor[]) null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testSwitchExecutorEx1c() {
        try {
            ExecutorUtils.switchExecutor((Map) null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testSwitchExecutorEx2() {
        try {
            ExecutorUtils.switchExecutor(new Predicate[0], new Executor[0]);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testSwitchExecutorEx3() {
        try {
            ExecutorUtils.switchExecutor(new Predicate[2], new Executor[2]);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testSwitchExecutorEx4() {
        try {
            ExecutorUtils.switchExecutor(EMPTY_MAP);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testSwitchExecutorEx5() {
        try {
            Map map = new HashMap();
            map.put(null, null);
            map.put(null, null);
            ExecutorUtils.switchExecutor(map);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testSwitchExecutorEx6() {
        try {
            ExecutorUtils.switchExecutor(new Predicate[2], new Executor[1]);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    // switchMapExecutor
    //------------------------------------------------------------------

    public void testSwitchMapExecutor() {
        MockExecutor a = new MockExecutor();
        MockExecutor b = new MockExecutor();
        Map map = new HashMap();
        map.put("HELLO", a);
        map.put("THERE", b);
        ExecutorUtils.switchMapExecutor(map).execute(null);
        assertEquals(0, a.count);
        assertEquals(0, b.count);

        a = new MockExecutor();
        b = new MockExecutor();
        map = new HashMap();
        map.put("HELLO", a);
        map.put("THERE", b);
        ExecutorUtils.switchMapExecutor(map).execute("THERE");
        assertEquals(0, a.count);
        assertEquals(1, b.count);

        a = new MockExecutor();
        b = new MockExecutor();
        MockExecutor c = new MockExecutor();
        map = new HashMap();
        map.put("HELLO", a);
        map.put("THERE", b);
        map.put(null, c);
        ExecutorUtils.switchMapExecutor(map).execute("WELL");
        assertEquals(0, a.count);
        assertEquals(0, b.count);
        assertEquals(1, c.count);
    }

    public void testSwitchMapExecutorEx1() {
        try {
            ExecutorUtils.switchMapExecutor(null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testSwitchMapExecutorEx2() {
        try {
            ExecutorUtils.switchMapExecutor(EMPTY_MAP);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    
}
