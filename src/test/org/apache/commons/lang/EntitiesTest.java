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
package org.apache.commons.lang;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Unit tests for {@link StringEscapeUtils}.
 *
 * @author of original StringUtilsTest.testEscape = ?
 * @author <a href="mailto:alex@purpletech.com">Alexander Day Chaffee</a>
 * @version $Id: EntitiesTest.java,v 1.5 2003/05/24 19:24:15 ggregory Exp $
 */
public class EntitiesTest extends TestCase
{
    public EntitiesTest(String name) {
        super(name);
    }

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(EntitiesTest.class);
        suite.setName("EntitiesTest Tests");
        return suite;
    }

    Entities entities;

    public void setUp()
    {
        entities = new Entities();
        entities.addEntity("foo", 161);
        entities.addEntity("bar", 162);
    }

    public void testEscapeNamedEntity() throws Exception
    {
        assertEquals("&foo;", entities.escape("\u00A1"));
        assertEquals("x&foo;", entities.escape("x\u00A1"));
        assertEquals("&foo;x", entities.escape("\u00A1x"));
        assertEquals("x&foo;x", entities.escape("x\u00A1x"));
        assertEquals("&foo;&bar;", entities.escape("\u00A1\u00A2"));
    }

    public void testUnescapeNamedEntity() throws Exception
    {
        assertEquals("\u00A1", entities.unescape("&foo;"));
        assertEquals("x\u00A1", entities.unescape("x&foo;"));
        assertEquals("\u00A1x", entities.unescape("&foo;x"));
        assertEquals("x\u00A1x", entities.unescape("x&foo;x"));
        assertEquals("\u00A1\u00A2", entities.unescape("&foo;&bar;"));
    }

    public void testUnescapeUnknownEntity() throws Exception
    {
        assertEquals("&zzzz;", entities.unescape("&zzzz;"));
    }

    public void testAddEntitiesArray() throws Exception
    {
        String[][] array = {{"foo", "100"}, {"bar", "101"}};
        Entities e = new Entities();
        e.addEntities(array);
        assertEquals("foo", e.entityName(100));
        assertEquals("bar", e.entityName(101));
        assertEquals(100, e.entityValue("foo"));
        assertEquals(101, e.entityValue("bar"));
    }

    public void testEntitiesXmlObject() throws Exception
    {
        assertEquals("gt", Entities.XML.entityName('>'));
        assertEquals((int) '>', Entities.XML.entityValue("gt"));
        assertEquals(-1, Entities.XML.entityValue("xyzzy"));
    }

    public void testArrayIntMap() throws Exception
    {
        Entities.ArrayIntMap map = new Entities.ArrayIntMap(2);
        checkSomeIntMap(map);
    }

    public void testTreeIntMap() throws Exception
    {
        Entities.IntMap map = new Entities.TreeIntMap();
        checkSomeIntMap(map);
    }

    public void testHashIntMap() throws Exception
    {
        Entities.IntMap map = new Entities.HashIntMap();
        checkSomeIntMap(map);
    }

    public void testBinaryIntMap() throws Exception
    {
        Entities.BinaryIntMap map = new Entities.BinaryIntMap(2);
        checkSomeIntMap(map);
    }

    private void checkSomeIntMap(Entities.IntMap map) {
        map.add("foo", 1);
        assertEquals(1, map.value("foo"));
        assertEquals("foo", map.name(1));
        map.add("bar", 2);
        map.add("baz", 3);
        assertEquals(3, map.value("baz"));
        assertEquals("baz", map.name(3));
    }
}

