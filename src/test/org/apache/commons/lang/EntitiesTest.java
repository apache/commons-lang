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

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Unit tests for {@link StringEscapeUtils}.
 *
 * @author of original StringUtilsTest.testEscape = ?
 * @author <a href="mailto:alex@purpletech.com">Alexander Day Chaffee</a>
 * @author <a href="mailto:ggregory@seagullsw.com">Gary Gregory</a>
 * @version $Id$
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
        assertEquals('>', Entities.XML.entityValue("gt"));
        assertEquals(-1, Entities.XML.entityValue("xyzzy"));
    }

    public void testArrayIntMap() throws Exception
    {
        Entities.ArrayEntityMap map = new Entities.ArrayEntityMap(2);
        checkSomeEntityMap(map);
    }

    public void testTreeIntMap() throws Exception
    {
        Entities.EntityMap map = new Entities.TreeEntityMap();
        checkSomeEntityMap(map);
    }

    public void testHashIntMap() throws Exception
    {
        Entities.EntityMap map = new Entities.HashEntityMap();
        checkSomeEntityMap(map);
    }

    public void testBinaryIntMap() throws Exception
    {
        Entities.BinaryEntityMap map = new Entities.BinaryEntityMap(2);
        checkSomeEntityMap(map);
    }

    public void testPrimitiveIntMap() throws Exception
    {
        Entities.PrimitiveEntityMap map = new Entities.PrimitiveEntityMap();
        checkSomeEntityMap(map);
    }

    private void checkSomeEntityMap(Entities.EntityMap map) {
        map.add("foo", 1);
        assertEquals(1, map.value("foo"));
        assertEquals("foo", map.name(1));
        map.add("bar", 2);
        map.add("baz", 3);
        assertEquals(3, map.value("baz"));
        assertEquals("baz", map.name(3));
    }

    public void testHtml40Nbsp() throws Exception
    {
        assertEquals("&nbsp;", Entities.HTML40.escape("\u00A0"));
        Entities e = new Entities();
        e.map = new Entities.PrimitiveEntityMap();
        Entities.fillWithHtml40Entities(e);
        assertEquals("&nbsp;", e.escape("\u00A0"));
    }

}

