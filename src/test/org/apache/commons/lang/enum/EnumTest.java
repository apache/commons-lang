/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002 The Apache Software Foundation.  All rights
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
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @version $Id: EnumTest.java,v 1.3 2002/11/02 13:17:06 scolebourne Exp $
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
    }

    public void testEquals() {
        assertSame(ColorEnum.RED, ColorEnum.RED);
        assertSame(ColorEnum.getEnum("Red"), ColorEnum.RED);
    }

    public void testToString() {
        assertEquals("ColorEnum[Red]", ColorEnum.RED.toString());
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
        assertSame(ColorEnum.RED, SerializationUtils.clone(ColorEnum.RED));
        assertSame(ColorEnum.GREEN, SerializationUtils.clone(ColorEnum.GREEN));
        assertSame(ColorEnum.BLUE, SerializationUtils.clone(ColorEnum.BLUE));
    }

    public void testBroken1() {
        try {
            Broken1Enum.RED.getName();
            fail();
        } catch (ExceptionInInitializerError ex) {
            assertTrue(ex.getCause() instanceof IllegalArgumentException);
        }
    }

    public void testBroken2() {
        try {
            Broken2Enum.RED.getName();
            fail();
        } catch (ExceptionInInitializerError ex) {
            assertTrue(ex.getCause() instanceof IllegalArgumentException);
        }
    }

    public void testBroken3() {
        try {
            Broken3Enum.RED.getName();
            fail();
        } catch (ExceptionInInitializerError ex) {
            assertTrue(ex.getCause() instanceof IllegalArgumentException);
        }
    }

}
