/*
 * Copyright 2004-2006 The Apache Software Foundation.
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

import java.net.URLClassLoader;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * Test cases for the {@link Enum} class equals method.
 *
 * @author Matthias Eichel
 * @author Stephen Colebourne
 * @version $Id$
 */
public final class EnumEqualsTest extends TestCase {

    public EnumEqualsTest(String name) {
        super(name);
    }

    public void setUp() {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(EnumEqualsTest.class);
        suite.setName("Enum equals Tests");
        return suite;
    }

    //-----------------------------------------------------------------------
    static final class CarColorEnum extends Enum {
        public static final CarColorEnum BLACK = new CarColorEnum("black");
        public static final CarColorEnum BROWN = new CarColorEnum("brown");
        public static final CarColorEnum YELLOW = new CarColorEnum("yellow");
        public static final CarColorEnum BLUE = new CarColorEnum("blue");
        public static final CarColorEnum RED = new CarColorEnum("red");

        private CarColorEnum(String enumAsString) {
            super(enumAsString);
        }
    }

    static final class TrafficlightColorEnum extends Enum {
        public static final TrafficlightColorEnum RED = new TrafficlightColorEnum("red");
        public static final TrafficlightColorEnum YELLOW = new TrafficlightColorEnum("yellow");
        public static final TrafficlightColorEnum GREEN = new TrafficlightColorEnum("green");

        private TrafficlightColorEnum(String enumAsString) {
            super(enumAsString);
        }
    }

    static class TotallyUnrelatedClass {
        private final String name;

        public TotallyUnrelatedClass(final String name) {
            this.name = name;
        }

        public String getName() {
            return name;
        }
    }

    //-----------------------------------------------------------------------
    public void testEquals() {
        assertEquals(false, CarColorEnum.RED.equals(TrafficlightColorEnum.RED));
        assertEquals(false, CarColorEnum.YELLOW.equals(TrafficlightColorEnum.YELLOW));
        
        assertEquals(false, TrafficlightColorEnum.RED.equals(new TotallyUnrelatedClass("red")));
        assertEquals(false, CarColorEnum.RED.equals(new TotallyUnrelatedClass("red")));
        
        assertEquals(false, TrafficlightColorEnum.RED.equals(new TotallyUnrelatedClass("some")));
        assertEquals(false, CarColorEnum.RED.equals(new TotallyUnrelatedClass("some")));
    }

    public void testEquals_classloader_equal() throws Exception {
        ClassLoader cl = ColorEnum.class.getClassLoader();
        if (cl instanceof URLClassLoader) {
            URLClassLoader urlCL = (URLClassLoader) cl;
            URLClassLoader urlCL1 = new URLClassLoader(urlCL.getURLs(), null);
            URLClassLoader urlCL2 = new URLClassLoader(urlCL.getURLs(), null);
            Class otherEnumClass1 = urlCL1.loadClass("org.apache.commons.lang.enums.ColorEnum");
            Class otherEnumClass2 = urlCL2.loadClass("org.apache.commons.lang.enums.ColorEnum");
            Object blue1 = otherEnumClass1.getDeclaredField("BLUE").get(null);
            Object blue2 = otherEnumClass2.getDeclaredField("BLUE").get(null);
            assertEquals(true, blue1.equals(blue2));
        }
    }

    public void testEquals_classloader_different() throws Exception {
        ClassLoader cl = ColorEnum.class.getClassLoader();
        if (cl instanceof URLClassLoader) {
            URLClassLoader urlCL = (URLClassLoader) cl;
            URLClassLoader urlCL1 = new URLClassLoader(urlCL.getURLs(), null);
            URLClassLoader urlCL2 = new URLClassLoader(urlCL.getURLs(), null);
            Class otherEnumClass1 = urlCL1.loadClass("org.apache.commons.lang.enums.ColorEnum");
            Class otherEnumClass2 = urlCL2.loadClass("org.apache.commons.lang.enums.ColorEnum");
            Object blue1 = otherEnumClass1.getDeclaredField("BLUE").get(null);
            Object blue2 = otherEnumClass2.getDeclaredField("RED").get(null);
            assertEquals(false, blue1.equals(blue2));
        }
    }

    //-----------------------------------------------------------------------
    public void testCompareTo() {
        try {
            CarColorEnum.RED.compareTo(TrafficlightColorEnum.RED);
            fail();
        } catch (ClassCastException ex) {}
        try {
            CarColorEnum.YELLOW.compareTo(TrafficlightColorEnum.YELLOW);
            fail();
        } catch (ClassCastException ex) {}
        try {
            TrafficlightColorEnum.RED.compareTo(new TotallyUnrelatedClass("red"));
            fail();
        } catch (ClassCastException ex) {}
        try {
            CarColorEnum.RED.compareTo(new TotallyUnrelatedClass("red"));
            fail();
        } catch (ClassCastException ex) {}
        try {
            TrafficlightColorEnum.RED.compareTo(new TotallyUnrelatedClass("some"));
            fail();
        } catch (ClassCastException ex) {}
        try {
            CarColorEnum.RED.compareTo(new TotallyUnrelatedClass("some"));
            fail();
        } catch (ClassCastException ex) {}
    }

}
