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
 * Tests the org.apache.commons.lang.functor.ClosureUtils class.
 *
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @version $Id: TestClosureUtils.java,v 1.1 2002/11/05 16:45:13 bayard Exp $
 */
public class TestClosureUtils extends junit.framework.TestCase {

    private static final Object cObject = new Object();
    private static final Object cString = "Hello";
    private static final Object cInteger = new Integer(6);

    /**
     * Construct
     */
    public TestClosureUtils(String name) {
        super(name);
    }

    /**
     * Return class aa a test suite.
     */
    public static Test suite() {
        return new TestSuite(TestClosureUtils.class);
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
    
    static class MockClosure implements Closure {
        int count = 0;
        
        /**
         * @see org.apache.commons.lang.functor.Closure#execute(Object)
         */
        public void execute(Object object) {
            count++;
        }

    }

    // exceptionClosure
    //------------------------------------------------------------------

    public void testExceptionClosure() {
        assertNotNull(ClosureUtils.exceptionClosure());
        assertSame(ClosureUtils.exceptionClosure(), ClosureUtils.exceptionClosure());
        try {
            ClosureUtils.exceptionClosure().execute(null);
        } catch (ClosureException ex) {
            try {
                ClosureUtils.exceptionClosure().execute(cString);
            } catch (ClosureException ex2) {
                return;
            }
        }
        fail();
    }
    
    // nopClosure
    //------------------------------------------------------------------

    public void testNopClosure() {
        StringBuffer buf = new StringBuffer("Hello");
        ClosureUtils.nopClosure().execute(null);
        assertEquals("Hello", buf.toString());
        ClosureUtils.nopClosure().execute("Hello");
        assertEquals("Hello", buf.toString());
    }

    // invokeClosure
    //------------------------------------------------------------------

    public void testInvokeClosure() {
        StringBuffer buf = new StringBuffer("Hello");
        ClosureUtils.invokerClosure("reverse").execute(buf);
        assertEquals("olleH", buf.toString());
        buf = new StringBuffer("Hello");
        ClosureUtils.invokerClosure("setLength", new Class[] {Integer.TYPE}, new Object[] {new Integer(2)}).execute(buf);
        assertEquals("He", buf.toString());
    }

    // forClosure
    //------------------------------------------------------------------

    public void testForClosure() {
        MockClosure cmd = new MockClosure();
        ClosureUtils.forClosure(5, cmd).execute(null);
        assertEquals(5, cmd.count);
        try {
            ClosureUtils.forClosure(-1, new MockClosure());
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }

    // whileClosure
    //------------------------------------------------------------------

    public void testWhileClosure() {
        MockClosure cmd = new MockClosure();
        ClosureUtils.whileClosure(PredicateUtils.falsePredicate(), cmd).execute(null);
        assertEquals(0, cmd.count);
        try {
            ClosureUtils.whileClosure(null, null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }

    // doWhileClosure
    //------------------------------------------------------------------

    public void testDoWhileClosure() {
        MockClosure cmd = new MockClosure();
        ClosureUtils.doWhileClosure(cmd, PredicateUtils.falsePredicate()).execute(null);
        assertEquals(1, cmd.count);
        try {
            ClosureUtils.doWhileClosure(null, null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }

    // chainedClosure
    //------------------------------------------------------------------

    public void testChainedClosure() {
        MockClosure a = new MockClosure();
        MockClosure b = new MockClosure();
        ClosureUtils.chainedClosure(a, b).execute(null);
        assertEquals(1, a.count);
        assertEquals(1, b.count);
        
        a = new MockClosure();
        b = new MockClosure();
        ClosureUtils.chainedClosure(new Closure[] {a, b, a}).execute(null);
        assertEquals(2, a.count);
        assertEquals(1, b.count);
        
        a = new MockClosure();
        b = new MockClosure();
        Collection coll = new ArrayList();
        coll.add(b);
        coll.add(a);
        coll.add(b);
        ClosureUtils.chainedClosure(coll).execute(null);
        assertEquals(1, a.count);
        assertEquals(2, b.count);
    }

    public void testChainedClosureEx1a() {
        try {
            ClosureUtils.chainedClosure(null, null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testChainedClosureEx1b() {
        try {
            ClosureUtils.chainedClosure((Closure[]) null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testChainedClosureEx1c() {
        try {
            ClosureUtils.chainedClosure((Collection) null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testChainedClosureEx2() {
        try {
            ClosureUtils.chainedClosure(new Closure[0]);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testChainedClosureEx3() {
        try {
            ClosureUtils.chainedClosure(new Closure[] {null, null});
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testChainedClosureEx4() {
        try {
            ClosureUtils.chainedClosure(Collections.EMPTY_LIST);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testChainedClosureEx5() {
        try {
            Collection coll = new ArrayList();
            coll.add(null);
            coll.add(null);
            ClosureUtils.chainedClosure(coll);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    // switchClosure
    //------------------------------------------------------------------

    public void testSwitchClosure() {
        MockClosure a = new MockClosure();
        MockClosure b = new MockClosure();
        ClosureUtils.switchClosure(PredicateUtils.truePredicate(), a, b).execute(null);
        assertEquals(1, a.count);
        assertEquals(0, b.count);
        
        a = new MockClosure();
        b = new MockClosure();
        ClosureUtils.switchClosure(PredicateUtils.falsePredicate(), a, b).execute(null);
        assertEquals(0, a.count);
        assertEquals(1, b.count);
        
        a = new MockClosure();
        b = new MockClosure();
        ClosureUtils.switchClosure(
            new Predicate[] {PredicateUtils.equalPredicate("HELLO"), PredicateUtils.equalPredicate("THERE")}, 
            new Closure[] {a, b}).execute("WELL");
        assertEquals(0, a.count);
        assertEquals(0, b.count);
        
        a = new MockClosure();
        b = new MockClosure();
        ClosureUtils.switchClosure(
            new Predicate[] {PredicateUtils.equalPredicate("HELLO"), PredicateUtils.equalPredicate("THERE")}, 
            new Closure[] {a, b}).execute("HELLO");
        assertEquals(1, a.count);
        assertEquals(0, b.count);
        
        a = new MockClosure();
        b = new MockClosure();
        MockClosure c = new MockClosure();
        ClosureUtils.switchClosure(
            new Predicate[] {PredicateUtils.equalPredicate("HELLO"), PredicateUtils.equalPredicate("THERE")}, 
            new Closure[] {a, b}, c).execute("WELL");
        assertEquals(0, a.count);
        assertEquals(0, b.count);
        assertEquals(1, c.count);
        
        a = new MockClosure();
        b = new MockClosure();
        Map map = new HashMap();
        map.put(PredicateUtils.equalPredicate("HELLO"), a);
        map.put(PredicateUtils.equalPredicate("THERE"), b);
        ClosureUtils.switchClosure(map).execute(null);
        assertEquals(0, a.count);
        assertEquals(0, b.count);

        a = new MockClosure();
        b = new MockClosure();
        map = new HashMap();
        map.put(PredicateUtils.equalPredicate("HELLO"), a);
        map.put(PredicateUtils.equalPredicate("THERE"), b);
        ClosureUtils.switchClosure(map).execute("THERE");
        assertEquals(0, a.count);
        assertEquals(1, b.count);

        a = new MockClosure();
        b = new MockClosure();
        c = new MockClosure();
        map = new HashMap();
        map.put(PredicateUtils.equalPredicate("HELLO"), a);
        map.put(PredicateUtils.equalPredicate("THERE"), b);
        map.put(null, c);
        ClosureUtils.switchClosure(map).execute("WELL");
        assertEquals(0, a.count);
        assertEquals(0, b.count);
        assertEquals(1, c.count);
    }

    public void testSwitchClosureEx1a() {
        try {
            ClosureUtils.switchClosure(null, null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testSwitchClosureEx1b() {
        try {
            ClosureUtils.switchClosure((Predicate[]) null, (Closure[]) null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testSwitchClosureEx1c() {
        try {
            ClosureUtils.switchClosure((Map) null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testSwitchClosureEx2() {
        try {
            ClosureUtils.switchClosure(new Predicate[0], new Closure[0]);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testSwitchClosureEx3() {
        try {
            ClosureUtils.switchClosure(new Predicate[2], new Closure[2]);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testSwitchClosureEx4() {
        try {
            ClosureUtils.switchClosure(Collections.EMPTY_MAP);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testSwitchClosureEx5() {
        try {
            Map map = new HashMap();
            map.put(null, null);
            map.put(null, null);
            ClosureUtils.switchClosure(map);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testSwitchClosureEx6() {
        try {
            ClosureUtils.switchClosure(new Predicate[2], new Closure[1]);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    // switchMapClosure
    //------------------------------------------------------------------

    public void testSwitchMapClosure() {
        MockClosure a = new MockClosure();
        MockClosure b = new MockClosure();
        Map map = new HashMap();
        map.put("HELLO", a);
        map.put("THERE", b);
        ClosureUtils.switchMapClosure(map).execute(null);
        assertEquals(0, a.count);
        assertEquals(0, b.count);

        a = new MockClosure();
        b = new MockClosure();
        map = new HashMap();
        map.put("HELLO", a);
        map.put("THERE", b);
        ClosureUtils.switchMapClosure(map).execute("THERE");
        assertEquals(0, a.count);
        assertEquals(1, b.count);

        a = new MockClosure();
        b = new MockClosure();
        MockClosure c = new MockClosure();
        map = new HashMap();
        map.put("HELLO", a);
        map.put("THERE", b);
        map.put(null, c);
        ClosureUtils.switchMapClosure(map).execute("WELL");
        assertEquals(0, a.count);
        assertEquals(0, b.count);
        assertEquals(1, c.count);
    }

    public void testSwitchMapClosureEx1() {
        try {
            ClosureUtils.switchMapClosure(null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testSwitchMapClosureEx2() {
        try {
            ClosureUtils.switchMapClosure(Collections.EMPTY_MAP);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    
}
