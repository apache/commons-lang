/*
 * Copyright 2002,2004 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
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
 * @version $Id$
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
    public void testAllElementsOfType() {
        List coll = new ArrayList();
        coll.add("a");
        coll.add("b");
        Validate.allElementsOfType(coll, String.class, "MSG");
        Validate.allElementsOfType(coll, String.class);
        try {
            Validate.allElementsOfType(coll, Integer.class, "MSG");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("MSG", ex.getMessage());
        }
        coll.set(1, Boolean.FALSE);
        try {
            Validate.allElementsOfType(coll, String.class);
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("The validated collection contains an element not of type java.lang.String at index: 1", ex.getMessage());
        }
        
        coll = new ArrayList();
        coll.add(new Integer(5));
        coll.add(new Double(2.0d));
        Validate.allElementsOfType(coll, Number.class, "MSG");
        try {
            Validate.allElementsOfType(coll, Integer.class, "MSG");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("MSG", ex.getMessage());
        }
        try {
            Validate.allElementsOfType(coll, Double.class, "MSG");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("MSG", ex.getMessage());
        }
    }

    public void testConstructor() {
        assertNotNull(new Validate());
        Constructor[] cons = Validate.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertEquals(true, Modifier.isPublic(cons[0].getModifiers()));
        assertEquals(true, Modifier.isPublic(Validate.class.getModifiers()));
        assertEquals(false, Modifier.isFinal(Validate.class.getModifiers()));
    }
    
}
