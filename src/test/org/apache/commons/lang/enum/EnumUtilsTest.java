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
package org.apache.commons.lang.enum;

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
 * @version $Id: EnumUtilsTest.java,v 1.6 2003/08/18 02:22:27 bayard Exp $
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
