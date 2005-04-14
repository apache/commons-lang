/*
 * Copyright 2002-2005 The Apache Software Foundation.
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
package org.apache.commons.lang.enums;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * Test cases for the {@link Enum} class.
 *
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @version $Id$
 */

public final class EnumUtilsTest extends TestCase {

    public EnumUtilsTest(String name) {
        super(name);
    }

    public void setUp() {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(EnumUtilsTest.class);
        suite.setName("EnumUtils Tests");
        return suite;
    }

    //-----------------------------------------------------------------------
    public void testConstructor() {
        assertNotNull(new EnumUtils());
        Constructor[] cons = EnumUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertEquals(true, Modifier.isPublic(cons[0].getModifiers()));
        assertEquals(true, Modifier.isPublic(EnumUtils.class.getModifiers()));
        assertEquals(false, Modifier.isFinal(EnumUtils.class.getModifiers()));
    }
    
    //-----------------------------------------------------------------------
    public void testIterator() {
        Iterator it = EnumUtils.iterator(ColorEnum.class);
        assertSame(ColorEnum.RED, it.next());
        assertSame(ColorEnum.GREEN, it.next());
        assertSame(ColorEnum.BLUE, it.next());
        it = EnumUtils.iterator(DummyEnum.class);
        assertEquals(false, it.hasNext());
    }

    public void testIteratorEx() {
        try {
            EnumUtils.iterator(null);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            EnumUtils.iterator(Object.class);
            fail();
        } catch (IllegalArgumentException ex) {}
    }

    //-----------------------------------------------------------------------
    public void testList() {
        List list = EnumUtils.getEnumList(ColorEnum.class);
        Iterator it = list.iterator();
        assertSame(ColorEnum.RED, it.next());
        assertSame(ColorEnum.GREEN, it.next());
        assertSame(ColorEnum.BLUE, it.next());
        list = EnumUtils.getEnumList(DummyEnum.class);
        assertEquals(0, list.size());
    }

    public void testListEx() {
        try {
            EnumUtils.getEnumList(null);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            EnumUtils.getEnumList(Object.class);
            fail();
        } catch (IllegalArgumentException ex) {}
    }

    //-----------------------------------------------------------------------
    public void testMap() {
        Map map = EnumUtils.getEnumMap(ColorEnum.class);
        assertTrue(map.containsValue(ColorEnum.RED));
        assertTrue(map.containsValue(ColorEnum.GREEN));
        assertTrue(map.containsValue(ColorEnum.BLUE));
        assertSame(ColorEnum.RED, map.get("Red"));
        assertSame(ColorEnum.GREEN, map.get("Green"));
        assertSame(ColorEnum.BLUE, map.get("Blue"));
        map = EnumUtils.getEnumMap(DummyEnum.class);
        assertEquals(0, map.size());
    }

    public void testMapEx() {
        try {
            EnumUtils.getEnumMap(null);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            EnumUtils.getEnumMap(Object.class);
            fail();
        } catch (IllegalArgumentException ex) {}
    }

    //-----------------------------------------------------------------------
    public void testGet() {
        assertSame(ColorEnum.RED, EnumUtils.getEnum(ColorEnum.class, "Red"));
        assertSame(ColorEnum.GREEN, EnumUtils.getEnum(ColorEnum.class, "Green"));
        assertSame(ColorEnum.BLUE, EnumUtils.getEnum(ColorEnum.class, "Blue"));
        assertSame(null, EnumUtils.getEnum(ColorEnum.class, "Pink"));
        assertSame(null, EnumUtils.getEnum(DummyEnum.class, "Pink"));
    }

    public void testGetEx() {
        try {
            EnumUtils.getEnum(null, "");
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            EnumUtils.getEnum(Object.class, "Red");
            fail();
        } catch (IllegalArgumentException ex) {}
    }

    //-----------------------------------------------------------------------
    public void testGetValue() {
        assertSame(ValuedColorEnum.RED, EnumUtils.getEnum(ValuedColorEnum.class, 1));
        assertSame(ValuedColorEnum.GREEN, EnumUtils.getEnum(ValuedColorEnum.class, 2));
        assertSame(ValuedColorEnum.BLUE, EnumUtils.getEnum(ValuedColorEnum.class, 3));
        assertSame(null, EnumUtils.getEnum(ValuedColorEnum.class, 4));
        assertSame(null, EnumUtils.getEnum(DummyEnum.class, 5));
    }

    public void testGetValueEx() {
        try {
            EnumUtils.getEnum(null, 0);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            EnumUtils.getEnum(Object.class, 2);
            fail();
        } catch (IllegalArgumentException ex) {}
    }

}
