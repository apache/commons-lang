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
 *    any, must include the following acknowledgement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowledgement may appear in the software itself,
 *    if and wherever such third-party acknowledgements normally appear.
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
package org.apache.commons.lang;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;
/**
 * Unit tests {@link org.apache.commons.lang.util.Validate}.
 *
 * @author Stephen Colebourne
 * @author Norm Deane
 * @version $Id: ValidateTest.java,v 1.4 2004/02/14 00:48:20 scolebourne Exp $
 */
public class ValidateTest extends TestCase {

    public ValidateTest(String name) {
        super(name);
    }

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
    	TestSuite suite = new TestSuite(ValidateTest.class);
    	suite.setName("Validate Tests");
        return suite;
    }

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    //-----------------------------------------------------------------------
    public void testIsTrue1() {
        Validate.isTrue(true);
        try {
            Validate.isTrue(false);
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("The validated expression is false", ex.getMessage());
        }
    }

    //-----------------------------------------------------------------------
    public void testIsTrue2() {
        Validate.isTrue(true, "MSG");
        try {
            Validate.isTrue(false, "MSG");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("MSG", ex.getMessage());
        }
    }

    //-----------------------------------------------------------------------
    public void testIsTrue3() {
        Validate.isTrue(true, "MSG", new Integer(6));
        try {
            Validate.isTrue(false, "MSG", new Integer(6));
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("MSG6", ex.getMessage());
        }
    }

    //-----------------------------------------------------------------------
    public void testIsTrue4() {
        Validate.isTrue(true, "MSG", 7);
        try {
            Validate.isTrue(false, "MSG", 7);
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("MSG7", ex.getMessage());
        }
    }

    //-----------------------------------------------------------------------
    public void testIsTrue5() {
        Validate.isTrue(true, "MSG", 7.4d);
        try {
            Validate.isTrue(false, "MSG", 7.4d);
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("MSG7.4", ex.getMessage());
        }
    }

    //-----------------------------------------------------------------------
    public void testNotNull1() {
        Validate.notNull(new Object());
        try {
            Validate.notNull(null);
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("The validated object is null", ex.getMessage());
        }
    }

    //-----------------------------------------------------------------------
    public void testNotNull2() {
        Validate.notNull(new Object(), "MSG");
        try {
            Validate.notNull(null, "MSG");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("MSG", ex.getMessage());
        }
    }

    //-----------------------------------------------------------------------
    public void testNotEmptyArray1() {
        Validate.notEmpty(new Object[] {null});
        try {
            Validate.notEmpty((Object[]) null);
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("The validated array is empty", ex.getMessage());
        }
        try {
            Validate.notEmpty(new Object[0]);
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("The validated array is empty", ex.getMessage());
        }
    }

    //-----------------------------------------------------------------------
    public void testNotEmptyArray2() {
        Validate.notEmpty(new Object[] {null}, "MSG");
        try {
            Validate.notEmpty((Object[]) null, "MSG");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("MSG", ex.getMessage());
        }
        try {
            Validate.notEmpty(new Object[0], "MSG");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("MSG", ex.getMessage());
        }
    }

    //-----------------------------------------------------------------------
    public void testNotEmptyCollection1() {
        Collection coll = new ArrayList();
        try {
            Validate.notEmpty((Collection) null);
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("The validated collection is empty", ex.getMessage());
        }
        try {
            Validate.notEmpty(coll);
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("The validated collection is empty", ex.getMessage());
        }
        coll.add(new Integer(8));
        Validate.notEmpty(coll);
    }

    //-----------------------------------------------------------------------
    public void testNotEmptyCollection2() {
        Collection coll = new ArrayList();
        try {
            Validate.notEmpty((Collection) null, "MSG");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("MSG", ex.getMessage());
        }
        try {
            Validate.notEmpty(coll, "MSG");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("MSG", ex.getMessage());
        }
        coll.add(new Integer(8));
        Validate.notEmpty(coll, "MSG");
    }

    //-----------------------------------------------------------------------
    public void testNotEmptyMap1() {
        Map map = new HashMap();
        try {
            Validate.notEmpty((Map) null);
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("The validated map is empty", ex.getMessage());
        }
        try {
            Validate.notEmpty(map);
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("The validated map is empty", ex.getMessage());
        }
        map.put("ll", new Integer(8));
        Validate.notEmpty(map);
    }

    //-----------------------------------------------------------------------
    public void testNotEmptyMap2() {
        Map map = new HashMap();
        try {
            Validate.notEmpty((Map) null, "MSG");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("MSG", ex.getMessage());
        }
        try {
            Validate.notEmpty(map, "MSG");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("MSG", ex.getMessage());
        }
        map.put("ll", new Integer(8));
        Validate.notEmpty(map, "MSG");
    }

    //-----------------------------------------------------------------------
    public void testNotEmptyString1() {
        Validate.notEmpty("hjl");
        try {
            Validate.notEmpty((String) null);
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("The validated string is empty", ex.getMessage());
        }
        try {
            Validate.notEmpty("");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("The validated string is empty", ex.getMessage());
        }
    }

    //-----------------------------------------------------------------------
    public void testNotEmptyString2() {
        Validate.notEmpty("a", "MSG");
        try {
            Validate.notEmpty((String) null, "MSG");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("MSG", ex.getMessage());
        }
        try {
            Validate.notEmpty("", "MSG");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("MSG", ex.getMessage());
        }
    }

    //-----------------------------------------------------------------------
    public void testNoNullElementsArray1() {
        String[] array = new String[] {"a", "b"};
        Validate.noNullElements(array);
        try {
            Validate.noNullElements((Object[]) null);
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("The validated object is null", ex.getMessage());
        }
        array[1] = null;
        try {
            Validate.noNullElements(array);
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("The validated array contains null element at index: 1", ex.getMessage());
        }
    }

    //-----------------------------------------------------------------------
    public void testNoNullElementsArray2() {
        String[] array = new String[] {"a", "b"};
        Validate.noNullElements(array, "MSG");
        try {
            Validate.noNullElements((Object[]) null, "MSG");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("The validated object is null", ex.getMessage());
        }
        array[1] = null;
        try {
            Validate.noNullElements(array, "MSG");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("MSG", ex.getMessage());
        }
    }

    //-----------------------------------------------------------------------
    public void testNoNullElementsCollection1() {
        List coll = new ArrayList();
        coll.add("a");
        coll.add("b");
        Validate.noNullElements(coll);
        try {
            Validate.noNullElements((Collection) null);
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("The validated object is null", ex.getMessage());
        }
        coll.set(1, null);
        try {
            Validate.noNullElements(coll);
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("The validated collection contains null element at index: 1", ex.getMessage());
        }
    }

    //-----------------------------------------------------------------------
    public void testNoNullElementsCollection2() {
        List coll = new ArrayList();
        coll.add("a");
        coll.add("b");
        Validate.noNullElements(coll, "MSG");
        try {
            Validate.noNullElements((Collection) null, "MSG");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("The validated object is null", ex.getMessage());
        }
        coll.set(1, null);
        try {
            Validate.noNullElements(coll, "MSG");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("MSG", ex.getMessage());
        }
    }

    //-----------------------------------------------------------------------
    public void testAllElementsOfClass() {
    	List coll = new ArrayList();
    	coll.add("a");
    	coll.add("b");
    	Validate.allElementsOfClass(coll, String.class, "MSG");
    	try {
    		Validate.allElementsOfClass(coll, Integer.class, "MSG");
    		fail("Expecting IllegalArgumentException");
    	} catch (IllegalArgumentException ex) {
    		assertEquals("MSG", ex.getMessage());
    	}
    	coll.set(1, Boolean.FALSE);
    	try {
    		Validate.allElementsOfClass(coll, String.class);
    		fail("Expecting IllegalArgumentException");
    	} catch (IllegalArgumentException ex) {
    		assertEquals("The validated collection contains an element not of type java.lang.String at index: 1", ex.getMessage());
    	}
    }
}
