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
package org.apache.commons.lang.enum;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.apache.commons.lang.SerializationUtils;

/**
 * Test cases for the {@link Enum} class.
 *
 * @author Stephen Colebourne
 * @version $Id: EnumTest.java,v 1.10 2003/08/05 00:24:02 scolebourne Exp $
 */

public final class EnumTest extends TestCase {

    public EnumTest(String name) {
        super(name);
    }

    public void setUp() {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(EnumTest.class);
        suite.setName("Enum Tests");
        return suite;
    }

    public void testName() {
        assertEquals("Red", ColorEnum.RED.getName());
        assertEquals("Green", ColorEnum.GREEN.getName());
        assertEquals("Blue", ColorEnum.BLUE.getName());
    }

    public void testCompareTo() {
        assertTrue(ColorEnum.BLUE.compareTo(ColorEnum.BLUE) == 0);
        assertTrue(ColorEnum.RED.compareTo(ColorEnum.BLUE) > 0);
        assertTrue(ColorEnum.BLUE.compareTo(ColorEnum.RED) < 0);
        try {
            ColorEnum.RED.compareTo(null);
            fail();
        } catch (NullPointerException ex) {}
        try {
            ColorEnum.RED.compareTo(new Object());
            fail();
        } catch (ClassCastException ex) {}
    }

    public void testEquals() {
        assertSame(ColorEnum.RED, ColorEnum.RED);
        assertSame(ColorEnum.getEnum("Red"), ColorEnum.RED);
        assertEquals(false, ColorEnum.RED.equals(null));
        assertEquals(true, ColorEnum.RED.equals(ColorEnum.RED));
        assertEquals(true, ColorEnum.RED.equals(ColorEnum.getEnum("Red")));
    }

    public void testHashCode() {
        assertEquals(ColorEnum.RED.hashCode(), ColorEnum.RED.hashCode());
        assertEquals(7 + ColorEnum.class.hashCode() + 3 * "Red".hashCode(), ColorEnum.RED.hashCode());
    }

    public void testToString() {
        String toString = ColorEnum.RED.toString();
        assertEquals("ColorEnum[Red]", toString);
        assertSame(toString, ColorEnum.RED.toString());
    }

    public void testIterator() {
        Iterator it = ColorEnum.iterator();
        assertSame(ColorEnum.RED, it.next());
        assertSame(ColorEnum.GREEN, it.next());
        assertSame(ColorEnum.BLUE, it.next());
    }

    public void testList() {
        List list = ColorEnum.getEnumList();
        
        assertNotNull(list);
        
        assertEquals( list.size(),
        				ColorEnum.getEnumMap().keySet().size());
        
        Iterator it = list.iterator();
        assertSame(ColorEnum.RED, it.next());
        assertSame(ColorEnum.GREEN, it.next());
        assertSame(ColorEnum.BLUE, it.next());
    }

    public void testMap() {
        Map map = ColorEnum.getEnumMap();
        
        assertNotNull(map);
        
        assertEquals( map.keySet().size(),
        				ColorEnum.getEnumList().size());
        
        assertTrue(map.containsValue(ColorEnum.RED));
        assertTrue(map.containsValue(ColorEnum.GREEN));
        assertTrue(map.containsValue(ColorEnum.BLUE));
        assertSame(ColorEnum.RED, map.get("Red"));
        assertSame(ColorEnum.GREEN, map.get("Green"));
        assertSame(ColorEnum.BLUE, map.get("Blue"));
    }

    public void testGet() {
        assertSame(ColorEnum.RED, ColorEnum.getEnum("Red"));
        assertSame(ColorEnum.GREEN, ColorEnum.getEnum("Green"));
        assertSame(ColorEnum.BLUE, ColorEnum.getEnum("Blue"));
        assertSame(null, ColorEnum.getEnum("Pink"));
    }

    public void testSerialization() {
        int hashCode = ColorEnum.RED.hashCode();
        assertSame(ColorEnum.RED, SerializationUtils.clone(ColorEnum.RED));
        assertEquals(hashCode, SerializationUtils.clone(ColorEnum.RED).hashCode());
        assertSame(ColorEnum.GREEN, SerializationUtils.clone(ColorEnum.GREEN));
        assertSame(ColorEnum.BLUE, SerializationUtils.clone(ColorEnum.BLUE));
    }

    public void testBroken1() {
        try {
            Broken1Enum.RED.getName();
            fail();
        } catch (ExceptionInInitializerError ex) {
            assertTrue(ex.getException() instanceof IllegalArgumentException);
        }
    }

    public void testBroken2() {
        try {
            Broken2Enum.RED.getName();
            fail();
        } catch (ExceptionInInitializerError ex) {
            assertTrue(ex.getException() instanceof IllegalArgumentException);
        }
    }

    public void testBroken3() {
        try {
            Broken3Enum.RED.getName();
            fail();
        } catch (ExceptionInInitializerError ex) {
            assertTrue(ex.getException() instanceof IllegalArgumentException);
        }
    }

    public void testBroken1Operation() {
        try {
            Broken1OperationEnum.PLUS.getName();
            fail();
        } catch (ExceptionInInitializerError ex) {
            assertTrue(ex.getException() instanceof IllegalArgumentException);
        }
    }

    public void testBroken2Operation() {
        try {
            Broken2OperationEnum.PLUS.getName();
            fail();
        } catch (ExceptionInInitializerError ex) {
            assertTrue(ex.getException() instanceof IllegalArgumentException);
        }
    }

    public void testBroken3Operation() {
        try {
            Broken3OperationEnum.PLUS.getName();
            fail();
        } catch (ExceptionInInitializerError ex) {
            assertTrue(ex.getException() instanceof IllegalArgumentException);
        }
    }

    public void testBroken4Operation() {
        try {
            Broken4OperationEnum.PLUS.getName();
            fail();
        } catch (ExceptionInInitializerError ex) {
            assertTrue(ex.getException() instanceof IllegalArgumentException);
        }
    }

    public void testBroken5Operation() {
        try {
            Broken5OperationEnum.PLUS.getName();
            fail();
        } catch (ExceptionInInitializerError ex) {
            assertTrue(ex.getException() instanceof IllegalArgumentException);
        }
    }

    public void testOperationGet() {
        assertSame(OperationEnum.PLUS, OperationEnum.getEnum("Plus"));
        assertSame(OperationEnum.MINUS, OperationEnum.getEnum("Minus"));
        assertSame(null, OperationEnum.getEnum("Pink"));
    }

    public void testOperationSerialization() {
        assertSame(OperationEnum.PLUS, SerializationUtils.clone(OperationEnum.PLUS));
        assertSame(OperationEnum.MINUS, SerializationUtils.clone(OperationEnum.MINUS));
    }

    public void testOperationToString() {
        assertEquals("OperationEnum[Plus]", OperationEnum.PLUS.toString());
    }

    public void testOperationList() {
        List list = OperationEnum.getEnumList();
        assertNotNull(list);
        assertEquals(2, list.size());
        assertEquals(list.size(), OperationEnum.getEnumMap().keySet().size());
        
        Iterator it = list.iterator();
        assertSame(OperationEnum.PLUS, it.next());
        assertSame(OperationEnum.MINUS, it.next());
    }

    public void testOperationMap() {
        Map map = OperationEnum.getEnumMap();
        assertNotNull(map);
        assertEquals(map.keySet().size(), OperationEnum.getEnumList().size());
        
        assertTrue(map.containsValue(OperationEnum.PLUS));
        assertTrue(map.containsValue(OperationEnum.MINUS));
        assertSame(OperationEnum.PLUS, map.get("Plus"));
        assertSame(OperationEnum.MINUS, map.get("Minus"));
    }

    public void testOperationCalculation() {
        assertEquals(3, OperationEnum.PLUS.eval(1, 2));
        assertEquals(-1, OperationEnum.MINUS.eval(1, 2));
    }
    
    //-----------------------------------------------------------------------
    public void testExtended1Get() {
        assertSame(Extended1Enum.ALPHA, Extended1Enum.getEnum("Alpha"));
        assertSame(Extended1Enum.BETA, Extended1Enum.getEnum("Beta"));
        assertSame(null, Extended1Enum.getEnum("Gamma"));
        assertSame(null, Extended1Enum.getEnum("Delta"));
    }
            
    public void testExtended2Get() {
        assertSame(Extended1Enum.ALPHA, Extended2Enum.ALPHA);
        assertSame(Extended1Enum.BETA, Extended2Enum.BETA);
        
        assertSame(Extended2Enum.ALPHA, Extended2Enum.getEnum("Alpha"));
        assertSame(Extended2Enum.BETA, Extended2Enum.getEnum("Beta"));
        assertSame(Extended2Enum.GAMMA, Extended2Enum.getEnum("Gamma"));
        assertSame(null, Extended2Enum.getEnum("Delta"));
    }

    public void testExtended3Get() {
        assertSame(Extended2Enum.ALPHA, Extended3Enum.ALPHA);
        assertSame(Extended2Enum.BETA, Extended3Enum.BETA);
        assertSame(Extended2Enum.GAMMA, Extended3Enum.GAMMA);
        
        assertSame(Extended3Enum.ALPHA, Extended3Enum.getEnum("Alpha"));
        assertSame(Extended3Enum.BETA, Extended3Enum.getEnum("Beta"));
        assertSame(Extended3Enum.GAMMA, Extended3Enum.getEnum("Gamma"));
        assertSame(Extended3Enum.DELTA, Extended3Enum.getEnum("Delta"));
    }

    public void testExtendedSerialization() {
        assertSame(Extended1Enum.ALPHA, SerializationUtils.clone(Extended1Enum.ALPHA));
        assertSame(Extended1Enum.BETA, SerializationUtils.clone(Extended1Enum.BETA));
        assertSame(Extended2Enum.GAMMA, SerializationUtils.clone(Extended2Enum.GAMMA));
        assertSame(Extended3Enum.DELTA, SerializationUtils.clone(Extended3Enum.DELTA));
    }

    public void testExtendedToString() {
        assertEquals("Extended1Enum[Alpha]", Extended1Enum.ALPHA.toString());
        assertEquals("Extended1Enum[Beta]", Extended1Enum.BETA.toString());
        
        assertEquals("Extended1Enum[Alpha]", Extended2Enum.ALPHA.toString());
        assertEquals("Extended1Enum[Beta]", Extended2Enum.BETA.toString());
        assertEquals("Extended2Enum[Gamma]", Extended2Enum.GAMMA.toString());
        
        assertEquals("Extended1Enum[Alpha]", Extended3Enum.ALPHA.toString());
        assertEquals("Extended1Enum[Beta]", Extended3Enum.BETA.toString());
        assertEquals("Extended2Enum[Gamma]", Extended3Enum.GAMMA.toString());
        assertEquals("Extended3Enum[Delta]", Extended3Enum.DELTA.toString());
    }

    public void testExtended1List() {
        List list = Extended1Enum.getEnumList();
        assertNotNull(list);
        assertEquals(2, list.size());
        assertEquals(list.size(), Extended1Enum.getEnumMap().keySet().size());
        
        Iterator it = list.iterator();
        assertSame(Extended1Enum.ALPHA, it.next());
        assertSame(Extended1Enum.BETA, it.next());
    }

    public void testExtended2List() {
        List list = Extended2Enum.getEnumList();
        assertNotNull(list);
        assertEquals(3, list.size());
        assertEquals(list.size(), Extended2Enum.getEnumMap().keySet().size());
        
        Iterator it = list.iterator();
        assertSame(Extended2Enum.ALPHA, it.next());
        assertSame(Extended2Enum.BETA, it.next());
        assertSame(Extended2Enum.GAMMA, it.next());
    }

    public void testExtended3List() {
        List list = Extended3Enum.getEnumList();
        assertNotNull(list);
        assertEquals(4, list.size());
        assertEquals(list.size(), Extended3Enum.getEnumMap().keySet().size());
        
        Iterator it = list.iterator();
        assertSame(Extended3Enum.ALPHA, it.next());
        assertSame(Extended3Enum.BETA, it.next());
        assertSame(Extended3Enum.GAMMA, it.next());
        assertSame(Extended3Enum.DELTA, it.next());
    }

    public void testExtended1Map() {
        Map map = Extended1Enum.getEnumMap();
        assertNotNull(map);
        assertEquals(map.keySet().size(), Extended1Enum.getEnumList().size());
        
        assertTrue(map.containsValue(Extended1Enum.ALPHA));
        assertTrue(map.containsValue(Extended1Enum.BETA));
        assertSame(Extended1Enum.ALPHA, map.get("Alpha"));
        assertSame(Extended1Enum.BETA, map.get("Beta"));
    }

    public void testExtended2Map() {
        Map map = Extended2Enum.getEnumMap();
        assertNotNull(map);
        assertEquals(map.keySet().size(), Extended2Enum.getEnumList().size());
        
        assertTrue(map.containsValue(Extended2Enum.ALPHA));
        assertTrue(map.containsValue(Extended2Enum.BETA));
        assertTrue(map.containsValue(Extended2Enum.GAMMA));
        assertSame(Extended2Enum.ALPHA, map.get("Alpha"));
        assertSame(Extended2Enum.BETA, map.get("Beta"));
        assertSame(Extended2Enum.GAMMA, map.get("Gamma"));
    }

    public void testExtended3Map() {
        Map map = Extended3Enum.getEnumMap();
        assertNotNull(map);
        assertEquals(map.keySet().size(), Extended3Enum.getEnumList().size());
        
        assertTrue(map.containsValue(Extended3Enum.ALPHA));
        assertTrue(map.containsValue(Extended3Enum.BETA));
        assertTrue(map.containsValue(Extended3Enum.GAMMA));
        assertTrue(map.containsValue(Extended3Enum.DELTA));
        assertSame(Extended3Enum.ALPHA, map.get("Alpha"));
        assertSame(Extended3Enum.BETA, map.get("Beta"));
        assertSame(Extended3Enum.GAMMA, map.get("Gamma"));
        assertSame(Extended3Enum.DELTA, map.get("Delta"));
    }

}
