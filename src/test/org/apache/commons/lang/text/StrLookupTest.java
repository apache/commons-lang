/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.commons.lang.text;

import java.util.HashMap;
import java.util.Map;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Test class for StrLookup.
 *
 * @version $Id$
 */
public class StrLookupTest extends TestCase {

    /**
     * Main method.
     * 
     * @param args  command line arguments, ignored
     */
    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    /**
     * Return a new test suite containing this test case.
     * 
     * @return a new test suite containing this test case
     */
    public static Test suite() {
        TestSuite suite = new TestSuite(StrLookupTest.class);
        suite.setName("StrLookup Tests");
        return suite;
    }

    @Override
    protected void setUp() throws Exception {
        super.setUp();
    }

    @Override
    protected void tearDown() throws Exception {
        super.tearDown();
    }

    //-----------------------------------------------------------------------
    public void testNoneLookup() {
        assertEquals(null, StrLookup.noneLookup().lookup(null));
        assertEquals(null, StrLookup.noneLookup().lookup(""));
        assertEquals(null, StrLookup.noneLookup().lookup("any"));
    }

    public void testSystemProperiesLookup() {
        assertEquals(System.getProperty("os.name"), StrLookup.systemPropertiesLookup().lookup("os.name"));
        assertEquals(null, StrLookup.systemPropertiesLookup().lookup(""));
        assertEquals(null, StrLookup.systemPropertiesLookup().lookup("other"));
        try {
            StrLookup.systemPropertiesLookup().lookup(null);
            fail();
        } catch (NullPointerException ex) {
            // expected
        }
    }

    public void testMapLookup() {
        Map map = new HashMap();
        map.put("key", "value");
        map.put("number", new Integer(2));
        assertEquals("value", StrLookup.mapLookup(map).lookup("key"));
        assertEquals("2", StrLookup.mapLookup(map).lookup("number"));
        assertEquals(null, StrLookup.mapLookup(map).lookup(null));
        assertEquals(null, StrLookup.mapLookup(map).lookup(""));
        assertEquals(null, StrLookup.mapLookup(map).lookup("other"));
    }

    public void testMapLookup_nullMap() {
        Map map = null;
        assertEquals(null, StrLookup.mapLookup(map).lookup(null));
        assertEquals(null, StrLookup.mapLookup(map).lookup(""));
        assertEquals(null, StrLookup.mapLookup(map).lookup("any"));
    }

}
