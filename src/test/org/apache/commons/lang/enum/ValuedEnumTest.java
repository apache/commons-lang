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
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @version $Id: ValuedEnumTest.java,v 1.4 2003/07/31 22:36:39 scolebourne Exp $
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
