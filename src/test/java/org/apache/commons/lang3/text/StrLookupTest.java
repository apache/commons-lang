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

package org.apache.commons.lang3.text;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.junit.Test;

/**
 * Test class for StrLookup.
 */
public class StrLookupTest  {

    //-----------------------------------------------------------------------
    @Test
    public void testNoneLookup() {
        assertEquals(null, StrLookup.noneLookup().lookup(null));
        assertEquals(null, StrLookup.noneLookup().lookup(""));
        assertEquals(null, StrLookup.noneLookup().lookup("any"));
    }

    @Test
    public void testSystemPropertiesLookup() {
        assertEquals(System.getProperty("os.name"), StrLookup.systemPropertiesLookup().lookup("os.name"));
        assertEquals(null, StrLookup.systemPropertiesLookup().lookup(""));
        assertEquals(null, StrLookup.systemPropertiesLookup().lookup("other"));
        try {
            StrLookup.systemPropertiesLookup().lookup(null);
            fail();
        } catch (final NullPointerException ex) {
            // expected
        }
    }

    /**
     * Tests that a lookup object for system properties can deal with a full
     * replacement of the system properties object. This test is related to
     * LANG-1055.
     */
    @Test
    public void testSystemPropertiesLookupReplacedProperties() {
        final Properties oldProperties = System.getProperties();
        final String osName = "os.name";
        final String newOsName = oldProperties.getProperty(osName) + "_changed";

        final StrLookup<String> sysLookup = StrLookup.systemPropertiesLookup();
        final Properties newProps = new Properties();
        newProps.setProperty(osName, newOsName);
        System.setProperties(newProps);
        try {
            assertEquals("Changed properties not detected", newOsName, sysLookup.lookup(osName));
        } finally {
            System.setProperties(oldProperties);
        }
    }

    /**
     * Tests that a lookup object for system properties sees changes on system
     * properties. This test is related to LANG-1141.
     */
    @Test
    public void testSystemPropertiesLookupUpdatedProperty() {
        final String osName = "os.name";
        final String oldOs = System.getProperty(osName);
        final String newOsName = oldOs + "_changed";

        final StrLookup<String> sysLookup = StrLookup.systemPropertiesLookup();
        System.setProperty(osName, newOsName);
        try {
            assertEquals("Changed properties not detected", newOsName, sysLookup.lookup(osName));
        } finally {
            System.setProperty(osName, oldOs);
        }
    }

    @Test
    public void testMapLookup() {
        final Map<String, Object> map = new HashMap<>();
        map.put("key", "value");
        map.put("number", Integer.valueOf(2));
        assertEquals("value", StrLookup.mapLookup(map).lookup("key"));
        assertEquals("2", StrLookup.mapLookup(map).lookup("number"));
        assertEquals(null, StrLookup.mapLookup(map).lookup(null));
        assertEquals(null, StrLookup.mapLookup(map).lookup(""));
        assertEquals(null, StrLookup.mapLookup(map).lookup("other"));
    }

    @Test
    public void testMapLookup_nullMap() {
        final Map<String, ?> map = null;
        assertEquals(null, StrLookup.mapLookup(map).lookup(null));
        assertEquals(null, StrLookup.mapLookup(map).lookup(""));
        assertEquals(null, StrLookup.mapLookup(map).lookup("any"));
    }

}
