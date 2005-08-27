/*
 * Copyright 2005 The Apache Software Foundation.
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

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Tests for the IntHashMap class.
 *
 * @author  Steven Caswell
 * @version $Id$
 */
public class IntHashMapTest extends TestCase {

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(IntHashMapTest.class);
        suite.setName("IntHashMapTest Tests");
        return suite;
    }

    public void testConstructor() {
        try {
            new IntHashMap(-1, 0.0f);
            fail();
        } catch (IllegalArgumentException e) {
            assertEquals("Illegal Capacity: -1", e.getMessage());
        }
        try {
            new IntHashMap(1, 0.0f);
            fail();
        } catch (IllegalArgumentException e) {
            assertEquals("Illegal Load: 0.0", e.getMessage());
        }
        new IntHashMap(0, 1.0f);
        
        try {
            new IntHashMap(-1);
            fail();
        } catch (IllegalArgumentException e) {
           assertEquals("Illegal Capacity: -1", e.getMessage());
        }
        IntHashMap map1 = new IntHashMap(0);
        assertEquals(0, map1.size());
    }
    
    public void testClear() {
        IntHashMap map = new IntHashMap();
        assertNull(map.put(1, "hello"));
        assertNull(map.put(2, "world"));
        assertEquals(2, map.size());
        map.clear();
        assertEquals(0, map.size());
    }
  
    public void testContainsKey() {
        IntHashMap map = new IntHashMap();
        assertNull(map.put(1, "hello"));
        assertNull(map.put(2, "world"));
        assertEquals(2, map.size());
        assertTrue(map.containsKey(1));
        assertTrue(map.containsKey(2));
        assertFalse(map.containsKey(3));
    }

    public void testContains() {
        IntHashMap map = new IntHashMap();
        assertNull(map.put(1, "hello"));
        assertNull(map.put(2, "world"));
        assertEquals(2, map.size());
        assertTrue(map.containsValue("hello"));
        assertTrue(map.containsValue("world"));
        assertFalse(map.containsValue("goodbye"));
        try {
            map.containsValue(null);
            fail();
        } catch(NullPointerException e) {
      }
    }

    public void testContainsValue() {
        IntHashMap map = new IntHashMap();
        assertNull(map.put(1, "hello"));
        assertNull(map.put(2, "world"));
        assertEquals(2, map.size());
        assertTrue(map.containsValue("hello"));
        assertTrue(map.containsValue("world"));
        assertFalse(map.containsValue("goodbye"));
        try {
            map.containsValue(null);
            fail();
        } catch(NullPointerException e) {
        }
    }

    public void testIsEmpty() {
        IntHashMap map = new IntHashMap();
        assertTrue(map.isEmpty());
        assertNull(map.put(1, "hello"));
        assertEquals(1, map.size());
        assertFalse(map.isEmpty());
    }
  
    public void testPut() {
        IntHashMap map = new IntHashMap();
        assertNull(map.put(1, "hello"));
        assertNull(map.put(2, "world"));
        assertEquals(2, map.size());
        assertEquals("hello", map.put(1, "hellooooo"));
    }
    
    public void testRemove() {
        IntHashMap map = new IntHashMap();
        assertNull(map.put(1, "hello"));
        assertNull(map.put(2, "world"));
        assertEquals(2, map.size());
        assertEquals("hello", map.remove(1));
        assertEquals(1, map.size());
        assertNull(map.remove(3));
    }
    
}
