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
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.commons.lang.SerializationException;
import org.apache.commons.lang.functor.TransformerUtils;
/**
 * Tests the org.apache.commons.lang.functor.PredicateUtils class.
 *
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @version $Id: TestPredicateUtils.java,v 1.1 2002/11/05 16:45:13 bayard Exp $
 */
public class TestPredicateUtils extends junit.framework.TestCase {

    private static final Object cObject = new Object();
    private static final Object cString = "Hello";
    private static final Object cInteger = new Integer(6);

    /**
     * Construct
     */
    public TestPredicateUtils(String name) {
        super(name);
    }

    /**
     * Return class aa a test suite.
     */
    public static Test suite() {
        return new TestSuite(TestPredicateUtils.class);
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

    // exceptionPredicate
    //------------------------------------------------------------------

    public void testExceptionPredicate() {
        assertNotNull(PredicateUtils.exceptionPredicate());
        assertSame(PredicateUtils.exceptionPredicate(), PredicateUtils.exceptionPredicate());
        try {
            PredicateUtils.exceptionPredicate().evaluate(null);
        } catch (PredicateException ex) {
            try {
                PredicateUtils.exceptionPredicate().evaluate(cString);
            } catch (PredicateException ex2) {
                return;
            }
        }
        fail();
    }
    
    // nullPredicate
    //------------------------------------------------------------------

    public void testNullPredicate() {
        assertNotNull(PredicateUtils.nullPredicate());
        assertSame(PredicateUtils.nullPredicate(), PredicateUtils.nullPredicate());
        assertEquals(true, PredicateUtils.nullPredicate().evaluate(null));
        assertEquals(false, PredicateUtils.nullPredicate().evaluate(cObject));
        assertEquals(false, PredicateUtils.nullPredicate().evaluate(cString));
        assertEquals(false, PredicateUtils.nullPredicate().evaluate(cInteger));
    }

    // notNullPredicate
    //------------------------------------------------------------------

    public void testIsNotNullPredicate() {
        assertNotNull(PredicateUtils.notNullPredicate());
        assertSame(PredicateUtils.notNullPredicate(), PredicateUtils.notNullPredicate());
        assertEquals(false, PredicateUtils.notNullPredicate().evaluate(null));
        assertEquals(true, PredicateUtils.notNullPredicate().evaluate(cObject));
        assertEquals(true, PredicateUtils.notNullPredicate().evaluate(cString));
        assertEquals(true, PredicateUtils.notNullPredicate().evaluate(cInteger));
    }

    // equalPredicate
    //------------------------------------------------------------------

    public void testEqualPredicate() {
        assertSame(PredicateUtils.nullPredicate(), PredicateUtils.equalPredicate(null));
        assertNotNull(PredicateUtils.equalPredicate(new Integer(6)));
        assertEquals(false, PredicateUtils.equalPredicate(new Integer(6)).evaluate(null));
        assertEquals(false, PredicateUtils.equalPredicate(new Integer(6)).evaluate(cObject));
        assertEquals(false, PredicateUtils.equalPredicate(new Integer(6)).evaluate(cString));
        assertEquals(true, PredicateUtils.equalPredicate(new Integer(6)).evaluate(cInteger));
    }

    // identityPredicate
    //------------------------------------------------------------------

    public void testIdentityPredicate() {
        assertSame(PredicateUtils.nullPredicate(), PredicateUtils.identityPredicate(null));
        assertNotNull(PredicateUtils.identityPredicate(new Integer(6)));
        assertEquals(false, PredicateUtils.identityPredicate(new Integer(6)).evaluate(null));
        assertEquals(false, PredicateUtils.identityPredicate(new Integer(6)).evaluate(cObject));
        assertEquals(false, PredicateUtils.identityPredicate(new Integer(6)).evaluate(cString));
        assertEquals(false, PredicateUtils.identityPredicate(new Integer(6)).evaluate(cInteger));
        assertEquals(true, PredicateUtils.identityPredicate(cInteger).evaluate(cInteger));
    }

    // truePredicate
    //------------------------------------------------------------------

    public void testTruePredicate() {
        assertNotNull(PredicateUtils.truePredicate());
        assertSame(PredicateUtils.truePredicate(), PredicateUtils.truePredicate());
        assertEquals(true, PredicateUtils.truePredicate().evaluate(null));
        assertEquals(true, PredicateUtils.truePredicate().evaluate(cObject));
        assertEquals(true, PredicateUtils.truePredicate().evaluate(cString));
        assertEquals(true, PredicateUtils.truePredicate().evaluate(cInteger));
    }

    // falsePredicate
    //------------------------------------------------------------------

    public void testFalsePredicate() {
        assertNotNull(PredicateUtils.falsePredicate());
        assertSame(PredicateUtils.falsePredicate(), PredicateUtils.falsePredicate());
        assertEquals(false, PredicateUtils.falsePredicate().evaluate(null));
        assertEquals(false, PredicateUtils.falsePredicate().evaluate(cObject));
        assertEquals(false, PredicateUtils.falsePredicate().evaluate(cString));
        assertEquals(false, PredicateUtils.falsePredicate().evaluate(cInteger));
    }

    // notPredicate
    //------------------------------------------------------------------

    public void testNotPredicate() {
        assertNotNull(PredicateUtils.notPredicate(PredicateUtils.truePredicate()));
        assertEquals(false, PredicateUtils.notPredicate(PredicateUtils.truePredicate()).evaluate(null));
        assertEquals(false, PredicateUtils.notPredicate(PredicateUtils.truePredicate()).evaluate(cObject));
        assertEquals(false, PredicateUtils.notPredicate(PredicateUtils.truePredicate()).evaluate(cString));
        assertEquals(false, PredicateUtils.notPredicate(PredicateUtils.truePredicate()).evaluate(cInteger));
    }
    
    public void testNotPredicateEx() {
        try {
            PredicateUtils.notPredicate(null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    // andPredicate
    //------------------------------------------------------------------

    public void testAndPredicate() {
        assertEquals(true, PredicateUtils.andPredicate(PredicateUtils.truePredicate(), PredicateUtils.truePredicate()).evaluate(null));
        assertEquals(false, PredicateUtils.andPredicate(PredicateUtils.truePredicate(), PredicateUtils.falsePredicate()).evaluate(null));
        assertEquals(false, PredicateUtils.andPredicate(PredicateUtils.falsePredicate(), PredicateUtils.truePredicate()).evaluate(null));
        assertEquals(false, PredicateUtils.andPredicate(PredicateUtils.falsePredicate(), PredicateUtils.falsePredicate()).evaluate(null));
    }

    public void testAndPredicateEx() {
        try {
            PredicateUtils.andPredicate(null, null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    // allPredicate
    //------------------------------------------------------------------

    public void testAllPredicate() {
        assertEquals(true, PredicateUtils.allPredicate(new Predicate[] {
            PredicateUtils.truePredicate(), PredicateUtils.truePredicate(), PredicateUtils.truePredicate()}).evaluate(null));
        assertEquals(false, PredicateUtils.allPredicate(new Predicate[] {
            PredicateUtils.truePredicate(), PredicateUtils.falsePredicate(), PredicateUtils.truePredicate()}).evaluate(null));
        assertEquals(false, PredicateUtils.allPredicate(new Predicate[] {
            PredicateUtils.falsePredicate(), PredicateUtils.falsePredicate(), PredicateUtils.truePredicate()}).evaluate(null));
        assertEquals(false, PredicateUtils.allPredicate(new Predicate[] {
            PredicateUtils.falsePredicate(), PredicateUtils.falsePredicate(), PredicateUtils.falsePredicate()}).evaluate(null));
        Collection coll = new ArrayList();
        coll.add(PredicateUtils.truePredicate());
        coll.add(PredicateUtils.truePredicate());
        coll.add(PredicateUtils.truePredicate());
        assertEquals(true, PredicateUtils.allPredicate(coll).evaluate(null));
        coll.clear();
        coll.add(PredicateUtils.truePredicate());
        coll.add(PredicateUtils.falsePredicate());
        coll.add(PredicateUtils.truePredicate());
        assertEquals(false, PredicateUtils.allPredicate(coll).evaluate(null));
        coll.clear();
        coll.add(PredicateUtils.falsePredicate());
        coll.add(PredicateUtils.falsePredicate());
        coll.add(PredicateUtils.truePredicate());
        assertEquals(false, PredicateUtils.allPredicate(coll).evaluate(null));
        coll.clear();
        coll.add(PredicateUtils.falsePredicate());
        coll.add(PredicateUtils.falsePredicate());
        coll.add(PredicateUtils.falsePredicate());
        assertEquals(false, PredicateUtils.allPredicate(coll).evaluate(null));
    }

    public void testAllPredicateEx1() {
        try {
            PredicateUtils.allPredicate((Predicate[]) null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testAllPredicateEx2() {
        try {
            PredicateUtils.allPredicate(new Predicate[] {null});
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testAllPredicateEx3() {
        try {
            PredicateUtils.allPredicate(new Predicate[] {null, null});
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testAllPredicateEx4() {
        try {
            PredicateUtils.allPredicate((Collection) null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testAllPredicateEx5() {
        try {
            PredicateUtils.allPredicate(Collections.EMPTY_LIST);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testAllPredicateEx6() {
        try {
            Collection coll = new ArrayList();
            coll.add(null);
            coll.add(null);
            PredicateUtils.allPredicate(coll);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    // orPredicate
    //------------------------------------------------------------------

    public void testOrPredicate() {
        assertEquals(true, PredicateUtils.orPredicate(PredicateUtils.truePredicate(), PredicateUtils.truePredicate()).evaluate(null));
        assertEquals(true, PredicateUtils.orPredicate(PredicateUtils.truePredicate(), PredicateUtils.falsePredicate()).evaluate(null));
        assertEquals(true, PredicateUtils.orPredicate(PredicateUtils.falsePredicate(), PredicateUtils.truePredicate()).evaluate(null));
        assertEquals(false, PredicateUtils.orPredicate(PredicateUtils.falsePredicate(), PredicateUtils.falsePredicate()).evaluate(null));
    }
    
    public void testOrPredicateEx() {
        try {
            PredicateUtils.orPredicate(null, null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    // anyPredicate
    //------------------------------------------------------------------

    public void testAnyPredicate() {
        assertEquals(true, PredicateUtils.anyPredicate(new Predicate[] {
            PredicateUtils.truePredicate(), PredicateUtils.truePredicate(), PredicateUtils.truePredicate()}).evaluate(null));
        assertEquals(true, PredicateUtils.anyPredicate(new Predicate[] {
            PredicateUtils.truePredicate(), PredicateUtils.falsePredicate(), PredicateUtils.truePredicate()}).evaluate(null));
        assertEquals(true, PredicateUtils.anyPredicate(new Predicate[] {
            PredicateUtils.falsePredicate(), PredicateUtils.falsePredicate(), PredicateUtils.truePredicate()}).evaluate(null));
        assertEquals(false, PredicateUtils.anyPredicate(new Predicate[] {
            PredicateUtils.falsePredicate(), PredicateUtils.falsePredicate(), PredicateUtils.falsePredicate()}).evaluate(null));
        Collection coll = new ArrayList();
        coll.add(PredicateUtils.truePredicate());
        coll.add(PredicateUtils.truePredicate());
        coll.add(PredicateUtils.truePredicate());
        assertEquals(true, PredicateUtils.anyPredicate(coll).evaluate(null));
        coll.clear();
        coll.add(PredicateUtils.truePredicate());
        coll.add(PredicateUtils.falsePredicate());
        coll.add(PredicateUtils.truePredicate());
        assertEquals(true, PredicateUtils.anyPredicate(coll).evaluate(null));
        coll.clear();
        coll.add(PredicateUtils.falsePredicate());
        coll.add(PredicateUtils.falsePredicate());
        coll.add(PredicateUtils.truePredicate());
        assertEquals(true, PredicateUtils.anyPredicate(coll).evaluate(null));
        coll.clear();
        coll.add(PredicateUtils.falsePredicate());
        coll.add(PredicateUtils.falsePredicate());
        coll.add(PredicateUtils.falsePredicate());
        assertEquals(false, PredicateUtils.anyPredicate(coll).evaluate(null));
    }

    public void testAnyPredicateEx1() {
        try {
            PredicateUtils.anyPredicate((Predicate[]) null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testAnyPredicateEx2() {
        try {
            PredicateUtils.anyPredicate(new Predicate[] {null});
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testAnyPredicateEx3() {
        try {
            PredicateUtils.anyPredicate(new Predicate[] {null, null});
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testAnyPredicateEx4() {
        try {
            PredicateUtils.anyPredicate((Collection) null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testAnyPredicateEx5() {
        try {
            PredicateUtils.anyPredicate(Collections.EMPTY_LIST);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testAnyPredicateEx6() {
        try {
            Collection coll = new ArrayList();
            coll.add(null);
            coll.add(null);
            PredicateUtils.anyPredicate(coll);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    // eitherPredicate
    //------------------------------------------------------------------

    public void testEitherPredicate() {
        assertEquals(false, PredicateUtils.eitherPredicate(PredicateUtils.truePredicate(), PredicateUtils.truePredicate()).evaluate(null));
        assertEquals(true, PredicateUtils.eitherPredicate(PredicateUtils.truePredicate(), PredicateUtils.falsePredicate()).evaluate(null));
        assertEquals(true, PredicateUtils.eitherPredicate(PredicateUtils.falsePredicate(), PredicateUtils.truePredicate()).evaluate(null));
        assertEquals(false, PredicateUtils.eitherPredicate(PredicateUtils.falsePredicate(), PredicateUtils.falsePredicate()).evaluate(null));
    }

    public void testEitherPredicateEx() {
        try {
            PredicateUtils.eitherPredicate(null, null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    // onePredicate
    //------------------------------------------------------------------

    public void testOnePredicate() {
        assertEquals(false, PredicateUtils.onePredicate(new Predicate[] {
            PredicateUtils.truePredicate(), PredicateUtils.truePredicate(), PredicateUtils.truePredicate()}).evaluate(null));
        assertEquals(false, PredicateUtils.onePredicate(new Predicate[] {
            PredicateUtils.truePredicate(), PredicateUtils.falsePredicate(), PredicateUtils.truePredicate()}).evaluate(null));
        assertEquals(true, PredicateUtils.onePredicate(new Predicate[] {
            PredicateUtils.truePredicate(), PredicateUtils.falsePredicate(), PredicateUtils.falsePredicate()}).evaluate(null));
        assertEquals(true, PredicateUtils.onePredicate(new Predicate[] {
            PredicateUtils.falsePredicate(), PredicateUtils.truePredicate(), PredicateUtils.falsePredicate()}).evaluate(null));
        assertEquals(true, PredicateUtils.onePredicate(new Predicate[] {
            PredicateUtils.falsePredicate(), PredicateUtils.falsePredicate(), PredicateUtils.truePredicate()}).evaluate(null));
        assertEquals(false, PredicateUtils.onePredicate(new Predicate[] {
            PredicateUtils.falsePredicate(), PredicateUtils.falsePredicate(), PredicateUtils.falsePredicate()}).evaluate(null));
        Collection coll = new ArrayList();
        coll.add(PredicateUtils.truePredicate());
        coll.add(PredicateUtils.truePredicate());
        coll.add(PredicateUtils.truePredicate());
        assertEquals(false, PredicateUtils.onePredicate(coll).evaluate(null));
        coll.clear();
        coll.add(PredicateUtils.truePredicate());
        coll.add(PredicateUtils.falsePredicate());
        coll.add(PredicateUtils.truePredicate());
        assertEquals(false, PredicateUtils.onePredicate(coll).evaluate(null));
        coll.clear();
        coll.add(PredicateUtils.falsePredicate());
        coll.add(PredicateUtils.falsePredicate());
        coll.add(PredicateUtils.truePredicate());
        assertEquals(true, PredicateUtils.onePredicate(coll).evaluate(null));
        coll.clear();
        coll.add(PredicateUtils.falsePredicate());
        coll.add(PredicateUtils.falsePredicate());
        coll.add(PredicateUtils.falsePredicate());
        assertEquals(false, PredicateUtils.onePredicate(coll).evaluate(null));
    }

    public void testOnePredicateEx1() {
        try {
            PredicateUtils.onePredicate((Predicate[]) null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testOnePredicateEx2() {
        try {
            PredicateUtils.onePredicate(new Predicate[] {null});
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testOnePredicateEx3() {
        try {
            PredicateUtils.onePredicate(new Predicate[] {null, null});
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testOnePredicateEx4() {
        try {
            PredicateUtils.onePredicate((Collection) null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testOnePredicateEx5() {
        try {
            PredicateUtils.onePredicate(Collections.EMPTY_LIST);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testOnePredicateEx6() {
        try {
            Collection coll = new ArrayList();
            coll.add(null);
            coll.add(null);
            PredicateUtils.onePredicate(coll);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    // neitherPredicate
    //------------------------------------------------------------------

    public void testNeitherPredicate() {
        assertEquals(false, PredicateUtils.neitherPredicate(PredicateUtils.truePredicate(), PredicateUtils.truePredicate()).evaluate(null));
        assertEquals(false, PredicateUtils.neitherPredicate(PredicateUtils.truePredicate(), PredicateUtils.falsePredicate()).evaluate(null));
        assertEquals(false, PredicateUtils.neitherPredicate(PredicateUtils.falsePredicate(), PredicateUtils.truePredicate()).evaluate(null));
        assertEquals(true, PredicateUtils.neitherPredicate(PredicateUtils.falsePredicate(), PredicateUtils.falsePredicate()).evaluate(null));
    }

    public void testNeitherPredicateEx() {
        try {
            PredicateUtils.neitherPredicate(null, null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    // nonePredicate
    //------------------------------------------------------------------

    public void testNonePredicate() {
        assertEquals(false, PredicateUtils.nonePredicate(new Predicate[] {
            PredicateUtils.truePredicate(), PredicateUtils.truePredicate(), PredicateUtils.truePredicate()}).evaluate(null));
        assertEquals(false, PredicateUtils.nonePredicate(new Predicate[] {
            PredicateUtils.truePredicate(), PredicateUtils.falsePredicate(), PredicateUtils.truePredicate()}).evaluate(null));
        assertEquals(false, PredicateUtils.nonePredicate(new Predicate[] {
            PredicateUtils.falsePredicate(), PredicateUtils.falsePredicate(), PredicateUtils.truePredicate()}).evaluate(null));
        assertEquals(true, PredicateUtils.nonePredicate(new Predicate[] {
            PredicateUtils.falsePredicate(), PredicateUtils.falsePredicate(), PredicateUtils.falsePredicate()}).evaluate(null));
        Collection coll = new ArrayList();
        coll.add(PredicateUtils.truePredicate());
        coll.add(PredicateUtils.truePredicate());
        coll.add(PredicateUtils.truePredicate());
        assertEquals(false, PredicateUtils.nonePredicate(coll).evaluate(null));
        coll.clear();
        coll.add(PredicateUtils.truePredicate());
        coll.add(PredicateUtils.falsePredicate());
        coll.add(PredicateUtils.truePredicate());
        assertEquals(false, PredicateUtils.nonePredicate(coll).evaluate(null));
        coll.clear();
        coll.add(PredicateUtils.falsePredicate());
        coll.add(PredicateUtils.falsePredicate());
        coll.add(PredicateUtils.truePredicate());
        assertEquals(false, PredicateUtils.nonePredicate(coll).evaluate(null));
        coll.clear();
        coll.add(PredicateUtils.falsePredicate());
        coll.add(PredicateUtils.falsePredicate());
        coll.add(PredicateUtils.falsePredicate());
        assertEquals(true, PredicateUtils.nonePredicate(coll).evaluate(null));
    }

    public void testNonePredicateEx1() {
        try {
            PredicateUtils.nonePredicate((Predicate[]) null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testNonePredicateEx2() {
        try {
            PredicateUtils.nonePredicate(new Predicate[] {null});
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testNonePredicateEx3() {
        try {
            PredicateUtils.nonePredicate(new Predicate[] {null, null});
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testNonePredicateEx4() {
        try {
            PredicateUtils.nonePredicate((Collection) null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testNonePredicateEx5() {
        try {
            PredicateUtils.nonePredicate(Collections.EMPTY_LIST);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testNonePredicateEx6() {
        try {
            Collection coll = new ArrayList();
            coll.add(null);
            coll.add(null);
            PredicateUtils.nonePredicate(coll);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    // instanceofPredicate
    //------------------------------------------------------------------

    public void testInstanceOfPredicate() {
        assertNotNull(PredicateUtils.instanceofPredicate(String.class));
        assertEquals(false, PredicateUtils.instanceofPredicate(String.class).evaluate(null));
        assertEquals(false, PredicateUtils.instanceofPredicate(String.class).evaluate(cObject));
        assertEquals(true, PredicateUtils.instanceofPredicate(String.class).evaluate(cString));
        assertEquals(false, PredicateUtils.instanceofPredicate(String.class).evaluate(cInteger));
    }

    // uniquePredicate
    //------------------------------------------------------------------

    public void testUniquePredicate() {
        Predicate p = PredicateUtils.uniquePredicate();
        assertEquals(true, p.evaluate(new Object()));
        assertEquals(true, p.evaluate(new Object()));
        assertEquals(true, p.evaluate(new Object()));
        assertEquals(true, p.evaluate(cString));
        assertEquals(false, p.evaluate(cString));
        assertEquals(false, p.evaluate(cString));
    }
    
    // asPredicate(Transformer)
    //------------------------------------------------------------------

    public void testAsPredicateTransformer() {
        assertEquals(false, PredicateUtils.asPredicate(TransformerUtils.nopTransformer()).evaluate(Boolean.FALSE));
        assertEquals(true, PredicateUtils.asPredicate(TransformerUtils.nopTransformer()).evaluate(Boolean.TRUE));
    }

    public void testAsPredicateTransformerEx1() {
        try {
            PredicateUtils.asPredicate(null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testAsPredicateTransformerEx2() {
        try {
            PredicateUtils.asPredicate(TransformerUtils.nopTransformer()).evaluate(null);
        } catch (PredicateException ex) {
            return;
        }
        fail();
    }
    
    // invokerPredicate
    //------------------------------------------------------------------

    public void testInvokerPredicate() {
        List list = new ArrayList();
        assertEquals(true, PredicateUtils.invokerPredicate("isEmpty").evaluate(list));
        list.add(new Object());
        assertEquals(false, PredicateUtils.invokerPredicate("isEmpty").evaluate(list));
    }

    public void testInvokerPredicateEx1() {
        try {
            PredicateUtils.invokerPredicate(null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testInvokerPredicateEx2() {
        try {
            PredicateUtils.invokerPredicate("isEmpty").evaluate(null);
        } catch (PredicateException ex) {
            return;
        }
        fail();
    }
    
    public void testInvokerPredicateEx3() {
        try {
            PredicateUtils.invokerPredicate("noSuchMethod").evaluate(new Object());
        } catch (PredicateException ex) {
            return;
        }
        fail();
    }
    
    // invokerPredicate2
    //------------------------------------------------------------------

    public void testInvokerPredicate2() {
        List list = new ArrayList();
        assertEquals(false, PredicateUtils.invokerPredicate(
            "contains", new Class[] {Object.class}, new Object[] {cString}).evaluate(list));
        list.add(cString);
        assertEquals(true, PredicateUtils.invokerPredicate(
            "contains", new Class[] {Object.class}, new Object[] {cString}).evaluate(list));
    }

    public void testInvokerPredicate2Ex1() {
        try {
            PredicateUtils.invokerPredicate(null, null, null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    public void testInvokerPredicate2Ex2() {
        try {
            PredicateUtils.invokerPredicate("contains", new Class[] {Object.class}, new Object[] {cString}).evaluate(null);
        } catch (PredicateException ex) {
            return;
        }
        fail();
    }
    
    public void testInvokerPredicate2Ex3() {
        try {
            PredicateUtils.invokerPredicate(
                "noSuchMethod", new Class[] {Object.class}, new Object[] {cString}).evaluate(new Object());
        } catch (PredicateException ex) {
            return;
        }
        fail();
    }
    
    // nullIsException
    //------------------------------------------------------------------

    public void testNullIsExceptionPredicate() {
        assertEquals(true, PredicateUtils.nullIsExceptionPredicate(PredicateUtils.truePredicate()).evaluate(new Object()));
        try {
            PredicateUtils.nullIsExceptionPredicate(PredicateUtils.truePredicate()).evaluate(null);
        } catch (PredicateException ex) {
            return;
        }
        fail();
    }

    public void testNullIsExceptionPredicateEx1() {
        try {
            PredicateUtils.nullIsExceptionPredicate(null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    // nullIsTrue
    //------------------------------------------------------------------

    public void testNullIsTruePredicate() {
        assertEquals(true, PredicateUtils.nullIsTruePredicate(PredicateUtils.truePredicate()).evaluate(null));
        assertEquals(true, PredicateUtils.nullIsTruePredicate(PredicateUtils.truePredicate()).evaluate(new Object()));
        assertEquals(false, PredicateUtils.nullIsTruePredicate(PredicateUtils.falsePredicate()).evaluate(new Object()));
    }

    public void testNullIsTruePredicateEx1() {
        try {
            PredicateUtils.nullIsTruePredicate(null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    // nullIsFalse
    //------------------------------------------------------------------

    public void testNullIsFalsePredicate() {
        assertEquals(false, PredicateUtils.nullIsFalsePredicate(PredicateUtils.truePredicate()).evaluate(null));
        assertEquals(true, PredicateUtils.nullIsFalsePredicate(PredicateUtils.truePredicate()).evaluate(new Object()));
        assertEquals(false, PredicateUtils.nullIsFalsePredicate(PredicateUtils.falsePredicate()).evaluate(new Object()));
    }

    public void testNullIsFalsePredicateEx1() {
        try {
            PredicateUtils.nullIsFalsePredicate(null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
}
