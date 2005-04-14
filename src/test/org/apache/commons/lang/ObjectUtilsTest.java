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
package org.apache.commons.lang;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Unit tests {@link org.apache.commons.lang.ObjectUtils}.
 *
 * @author <a href="mailto:jmcnally@collab.net">John McNally</a>
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @author <a href="mailto:ridesmet@users.sourceforge.net">Ringo De Smet</a>
 * @author <a href="mailto:ggregory@seagullsw.com">Gary Gregory</a>
 * @version $Id$
 */
public class ObjectUtilsTest extends TestCase {
    private static final String FOO = "foo";
    private static final String BAR = "bar";

    public ObjectUtilsTest(String name) {
        super(name);
    }

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(ObjectUtilsTest.class);
        suite.setName("ObjectUtils Tests");
        return suite;
    }

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    //-----------------------------------------------------------------------
    public void testConstructor() {
        assertNotNull(new ObjectUtils());
        Constructor[] cons = ObjectUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertEquals(true, Modifier.isPublic(cons[0].getModifiers()));
        assertEquals(true, Modifier.isPublic(ObjectUtils.class.getModifiers()));
        assertEquals(false, Modifier.isFinal(ObjectUtils.class.getModifiers()));
    }
    
    //-----------------------------------------------------------------------
    public void testIsNull() {
        Object o = FOO;
        Object dflt = BAR;
        assertSame("dflt was not returned when o was null", dflt, ObjectUtils.defaultIfNull(null, dflt));
        assertSame("dflt was returned when o was not null", o, ObjectUtils.defaultIfNull(o, dflt));
    }

    public void testEquals() {
        assertTrue("ObjectUtils.equals(null, null) returned false", ObjectUtils.equals(null, null));
        assertTrue("ObjectUtils.equals(\"foo\", null) returned true", !ObjectUtils.equals(FOO, null));
        assertTrue("ObjectUtils.equals(null, \"bar\") returned true", !ObjectUtils.equals(null, BAR));
        assertTrue("ObjectUtils.equals(\"foo\", \"bar\") returned true", !ObjectUtils.equals(FOO, BAR));
        assertTrue("ObjectUtils.equals(\"foo\", \"foo\") returned false", ObjectUtils.equals(FOO, FOO));
    }

    public void testHashCode() {
        assertEquals(0, ObjectUtils.hashCode(null));
        assertEquals("a".hashCode(), ObjectUtils.hashCode("a"));
    }

//    /**
//     * Show that java.util.Date and java.sql.Timestamp are apples and oranges.
//     * Prompted by an email discussion. 
//     * 
//     * The behavior is different b/w Sun Java 1.3.1_10 and 1.4.2_03.
//     */
//    public void testDateEqualsJava() {
//        long now = 1076957313284L; // Feb 16, 2004 10:49... PST
//        java.util.Date date = new java.util.Date(now);
//        java.sql.Timestamp realTimestamp = new java.sql.Timestamp(now);
//        java.util.Date timestamp = realTimestamp;
//        // sanity check 1:
//        assertEquals(284000000, realTimestamp.getNanos());
//        assertEquals(1076957313284L, date.getTime());
//        //
//        // On Sun 1.3.1_10:
//        //junit.framework.AssertionFailedError: expected:<1076957313284> but was:<1076957313000>
//        //
//        //assertEquals(1076957313284L, timestamp.getTime());
//        //
//        //junit.framework.AssertionFailedError: expected:<1076957313284> but was:<1076957313000>
//        //
//        //assertEquals(1076957313284L, realTimestamp.getTime());
//        // sanity check 2:        
//        assertEquals(date.getDay(), realTimestamp.getDay());
//        assertEquals(date.getHours(), realTimestamp.getHours());
//        assertEquals(date.getMinutes(), realTimestamp.getMinutes());
//        assertEquals(date.getMonth(), realTimestamp.getMonth());
//        assertEquals(date.getSeconds(), realTimestamp.getSeconds());
//        assertEquals(date.getTimezoneOffset(), realTimestamp.getTimezoneOffset());
//        assertEquals(date.getYear(), realTimestamp.getYear());
//        //
//        // Time values are == and equals() on Sun 1.4.2_03 but NOT on Sun 1.3.1_10:
//        //
//        //assertFalse("Sanity check failed: date.getTime() == timestamp.getTime()", date.getTime() == timestamp.getTime());
//        //assertFalse("Sanity check failed: timestamp.equals(date)", timestamp.equals(date));
//        //assertFalse("Sanity check failed: date.equals(timestamp)", date.equals(timestamp));
//        // real test:
//        //assertFalse("java.util.Date and java.sql.Timestamp should be equal", ObjectUtils.equals(date, timestamp));
//    }
    
    public void testIdentityToString() {
        assertEquals(null, ObjectUtils.identityToString(null));
        assertEquals(
            "java.lang.String@" + Integer.toHexString(System.identityHashCode(FOO)),
            ObjectUtils.identityToString(FOO));
        Integer i = new Integer(90);
        assertEquals(
            "java.lang.Integer@" + Integer.toHexString(System.identityHashCode(i)),
            ObjectUtils.identityToString(i));
    }

    public void testAppendIdentityToString() {
        assertEquals(null, ObjectUtils.appendIdentityToString(null, null));
        assertEquals(null, ObjectUtils.appendIdentityToString(new StringBuffer(), null));
        assertEquals(
            "java.lang.String@" + Integer.toHexString(System.identityHashCode(FOO)),
            ObjectUtils.appendIdentityToString(null, FOO).toString());
        assertEquals(
            "java.lang.String@" + Integer.toHexString(System.identityHashCode(FOO)),
            ObjectUtils.appendIdentityToString(new StringBuffer(), FOO).toString());
        Integer val = new Integer(90);
        assertEquals(
            "java.lang.Integer@" + Integer.toHexString(System.identityHashCode(val)),
            ObjectUtils.appendIdentityToString(null, val).toString());
        assertEquals(
            "java.lang.Integer@" + Integer.toHexString(System.identityHashCode(val)),
            ObjectUtils.appendIdentityToString(new StringBuffer(), val).toString());
    }

    public void testToString_Object() {
        assertEquals("", ObjectUtils.toString((Object) null) );
        assertEquals(Boolean.TRUE.toString(), ObjectUtils.toString(Boolean.TRUE) );
    }
            
    public void testToString_ObjectString() {
        assertEquals(BAR, ObjectUtils.toString((Object) null, BAR) );
        assertEquals(Boolean.TRUE.toString(), ObjectUtils.toString(Boolean.TRUE, BAR) );
    }

    public void testNull() {
        assertTrue(ObjectUtils.NULL != null);
        assertTrue(ObjectUtils.NULL instanceof ObjectUtils.Null);
        assertSame(ObjectUtils.NULL, SerializationUtils.clone(ObjectUtils.NULL));
    }

}
