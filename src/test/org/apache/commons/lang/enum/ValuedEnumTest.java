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
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @version $Id$
 */

public final class ValuedEnumTest extends TestCase {

    public ValuedEnumTest(String name) {
        super(name);
    }

    public void setUp() {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(ValuedEnumTest.class);
        suite.setName("ValuedEnum Tests");
        return suite;
    }

    public void testName() {
        assertEquals("Red", ValuedColorEnum.RED.getName());
        assertEquals("Green", ValuedColorEnum.GREEN.getName());
        assertEquals("Blue", ValuedColorEnum.BLUE.getName());
    }

    public void testValue() {
        assertEquals(1, ValuedColorEnum.RED.getValue());
        assertEquals(2, ValuedColorEnum.GREEN.getValue());
        assertEquals(3, ValuedColorEnum.BLUE.getValue());
    }

    public void testCompareTo() {
        assertTrue(ValuedColorEnum.BLUE.compareTo(ValuedColorEnum.BLUE) == 0);
        assertTrue(ValuedColorEnum.RED.compareTo(ValuedColorEnum.BLUE) < 0);
        assertTrue(ValuedColorEnum.BLUE.compareTo(ValuedColorEnum.RED) > 0);
    }

    public void testEquals() {
        assertSame(ValuedColorEnum.RED, ValuedColorEnum.RED);
        assertSame(ValuedColorEnum.getEnum("Red"), ValuedColorEnum.RED);
    }

    public void testToString() {
        String toString = ValuedColorEnum.RED.toString();
        assertEquals("ValuedColorEnum[Red=1]", toString);
        assertSame(toString, ValuedColorEnum.RED.toString());
    }

    public void testIterator() {
        Iterator it = ValuedColorEnum.iterator();
        assertSame(ValuedColorEnum.RED, it.next());
        assertSame(ValuedColorEnum.GREEN, it.next());
        assertSame(ValuedColorEnum.BLUE, it.next());
    }

    public void testList() {
        List list = ValuedColorEnum.getEnumList();
        
        assertNotNull(list);
        
        assertEquals( list.size(),
                     ValuedColorEnum.getEnumMap().keySet().size());
        
        Iterator it = list.iterator();
        assertSame(ValuedColorEnum.RED, it.next());
        assertSame(ValuedColorEnum.GREEN, it.next());
        assertSame(ValuedColorEnum.BLUE, it.next());
    }

    public void testMap() {
        Map map = ValuedColorEnum.getEnumMap();
        
        assertNotNull(map);
        
        assertEquals( map.keySet().size(),
                     ValuedColorEnum.getEnumList().size());
                     
        assertTrue(map.containsValue(ValuedColorEnum.RED));
        assertTrue(map.containsValue(ValuedColorEnum.GREEN));
        assertTrue(map.containsValue(ValuedColorEnum.BLUE));
        assertSame(ValuedColorEnum.RED, map.get("Red"));
        assertSame(ValuedColorEnum.GREEN, map.get("Green"));
        assertSame(ValuedColorEnum.BLUE, map.get("Blue"));
    }

    public void testGet() {
        assertSame(ValuedColorEnum.RED, ValuedColorEnum.getEnum("Red"));
        assertSame(ValuedColorEnum.GREEN, ValuedColorEnum.getEnum("Green"));
        assertSame(ValuedColorEnum.BLUE, ValuedColorEnum.getEnum("Blue"));
        assertSame(null, ValuedColorEnum.getEnum("Pink"));
    }

    public void testGetValue() {
        assertSame(ValuedColorEnum.RED, ValuedColorEnum.getEnum(1));
        assertSame(ValuedColorEnum.GREEN, ValuedColorEnum.getEnum(2));
        assertSame(ValuedColorEnum.BLUE, ValuedColorEnum.getEnum(3));
        assertSame(null, ValuedColorEnum.getEnum(4));
    }

    public void testSerialization() {
        assertSame(ValuedColorEnum.RED, SerializationUtils.clone(ValuedColorEnum.RED));
        assertSame(ValuedColorEnum.GREEN, SerializationUtils.clone(ValuedColorEnum.GREEN));
        assertSame(ValuedColorEnum.BLUE, SerializationUtils.clone(ValuedColorEnum.BLUE));
    }

}
