/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.commons.lang3;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.lang.reflect.Modifier;
import java.util.Iterator;
import java.util.NoSuchElementException;

import org.junit.Test;

/**
 * Unit tests {@link org.apache.commons.lang3.CharRange}.
 *
 * @version $Id$
 */
public class CharRangeTest  {

    //-----------------------------------------------------------------------
    @Test
    public void testClass() {
        // class changed to non-public in 3.0
        assertFalse(Modifier.isPublic(CharRange.class.getModifiers()));
        assertTrue(Modifier.isFinal(CharRange.class.getModifiers()));
    }

    //-----------------------------------------------------------------------
    @Test
    public void testConstructorAccessors_is() {
        CharRange rangea = CharRange.is('a');
        assertEquals('a', rangea.getStart());
        assertEquals('a', rangea.getEnd());
        assertFalse(rangea.isNegated());
        assertEquals("a", rangea.toString());
    }

    @Test
    public void testConstructorAccessors_isNot() {
        CharRange rangea = CharRange.isNot('a');
        assertEquals('a', rangea.getStart());
        assertEquals('a', rangea.getEnd());
        assertTrue(rangea.isNegated());
        assertEquals("^a", rangea.toString());
    }

    @Test
    public void testConstructorAccessors_isIn_Same() {
        CharRange rangea = CharRange.isIn('a', 'a');
        assertEquals('a', rangea.getStart());
        assertEquals('a', rangea.getEnd());
        assertFalse(rangea.isNegated());
        assertEquals("a", rangea.toString());
    }

    @Test
    public void testConstructorAccessors_isIn_Normal() {
        CharRange rangea = CharRange.isIn('a', 'e');
        assertEquals('a', rangea.getStart());
        assertEquals('e', rangea.getEnd());
        assertFalse(rangea.isNegated());
        assertEquals("a-e", rangea.toString());
    }

    @Test
    public void testConstructorAccessors_isIn_Reversed() {
        CharRange rangea = CharRange.isIn('e', 'a');
        assertEquals('a', rangea.getStart());
        assertEquals('e', rangea.getEnd());
        assertFalse(rangea.isNegated());
        assertEquals("a-e", rangea.toString());
    }

    @Test
    public void testConstructorAccessors_isNotIn_Same() {
        CharRange rangea = CharRange.isNotIn('a', 'a');
        assertEquals('a', rangea.getStart());
        assertEquals('a', rangea.getEnd());
        assertTrue(rangea.isNegated());
        assertEquals("^a", rangea.toString());
    }

    @Test
    public void testConstructorAccessors_isNotIn_Normal() {
        CharRange rangea = CharRange.isNotIn('a', 'e');
        assertEquals('a', rangea.getStart());
        assertEquals('e', rangea.getEnd());
        assertTrue(rangea.isNegated());
        assertEquals("^a-e", rangea.toString());
    }

    @Test
    public void testConstructorAccessors_isNotIn_Reversed() {
        CharRange rangea = CharRange.isNotIn('e', 'a');
        assertEquals('a', rangea.getStart());
        assertEquals('e', rangea.getEnd());
        assertTrue(rangea.isNegated());
        assertEquals("^a-e", rangea.toString());
    }

    //-----------------------------------------------------------------------
    @Test
    public void testEquals_Object() {
        CharRange rangea = CharRange.is('a');
        CharRange rangeae = CharRange.isIn('a', 'e');
        CharRange rangenotbf = CharRange.isIn('b', 'f');

        assertFalse(rangea.equals(null));

        assertTrue(rangea.equals(rangea));
        assertTrue(rangea.equals(CharRange.is('a')));
        assertTrue(rangeae.equals(rangeae));
        assertTrue(rangeae.equals(CharRange.isIn('a', 'e')));
        assertTrue(rangenotbf.equals(rangenotbf));
        assertTrue(rangenotbf.equals(CharRange.isIn('b', 'f')));

        assertFalse(rangea.equals(rangeae));
        assertFalse(rangea.equals(rangenotbf));
        assertFalse(rangeae.equals(rangea));
        assertFalse(rangeae.equals(rangenotbf));
        assertFalse(rangenotbf.equals(rangea));
        assertFalse(rangenotbf.equals(rangeae));
    }

    @Test
    public void testHashCode() {
        CharRange rangea = CharRange.is('a');
        CharRange rangeae = CharRange.isIn('a', 'e');
        CharRange rangenotbf = CharRange.isIn('b', 'f');

        assertTrue(rangea.hashCode() == rangea.hashCode());
        assertTrue(rangea.hashCode() == CharRange.is('a').hashCode());
        assertTrue(rangeae.hashCode() == rangeae.hashCode());
        assertTrue(rangeae.hashCode() == CharRange.isIn('a', 'e').hashCode());
        assertTrue(rangenotbf.hashCode() == rangenotbf.hashCode());
        assertTrue(rangenotbf.hashCode() == CharRange.isIn('b', 'f').hashCode());

        assertFalse(rangea.hashCode() == rangeae.hashCode());
        assertFalse(rangea.hashCode() == rangenotbf.hashCode());
        assertFalse(rangeae.hashCode() == rangea.hashCode());
        assertFalse(rangeae.hashCode() == rangenotbf.hashCode());
        assertFalse(rangenotbf.hashCode() == rangea.hashCode());
        assertFalse(rangenotbf.hashCode() == rangeae.hashCode());
    }

    //-----------------------------------------------------------------------
    @Test
    public void testContains_Char() {
        CharRange range = CharRange.is('c');
        assertFalse(range.contains('b'));
        assertTrue(range.contains('c'));
        assertFalse(range.contains('d'));
        assertFalse(range.contains('e'));

        range = CharRange.isIn('c', 'd');
        assertFalse(range.contains('b'));
        assertTrue(range.contains('c'));
        assertTrue(range.contains('d'));
        assertFalse(range.contains('e'));

        range = CharRange.isIn('d', 'c');
        assertFalse(range.contains('b'));
        assertTrue(range.contains('c'));
        assertTrue(range.contains('d'));
        assertFalse(range.contains('e'));

        range = CharRange.isNotIn('c', 'd');
        assertTrue(range.contains('b'));
        assertFalse(range.contains('c'));
        assertFalse(range.contains('d'));
        assertTrue(range.contains('e'));
        assertTrue(range.contains((char) 0));
        assertTrue(range.contains(Character.MAX_VALUE));
    }

    //-----------------------------------------------------------------------
    @Test
    public void testContains_Charrange() {
        CharRange a = CharRange.is('a');
        CharRange b = CharRange.is('b');
        CharRange c = CharRange.is('c');
        CharRange c2 = CharRange.is('c');
        CharRange d = CharRange.is('d');
        CharRange e = CharRange.is('e');
        CharRange cd = CharRange.isIn('c', 'd');
        CharRange bd = CharRange.isIn('b', 'd');
        CharRange bc = CharRange.isIn('b', 'c');
        CharRange ab = CharRange.isIn('a', 'b');
        CharRange de = CharRange.isIn('d', 'e');
        CharRange ef = CharRange.isIn('e', 'f');
        CharRange ae = CharRange.isIn('a', 'e');

        // normal/normal
        assertFalse(c.contains(b));
        assertTrue(c.contains(c));
        assertTrue(c.contains(c2));
        assertFalse(c.contains(d));

        assertFalse(c.contains(cd));
        assertFalse(c.contains(bd));
        assertFalse(c.contains(bc));
        assertFalse(c.contains(ab));
        assertFalse(c.contains(de));

        assertTrue(cd.contains(c));
        assertTrue(bd.contains(c));
        assertTrue(bc.contains(c));
        assertFalse(ab.contains(c));
        assertFalse(de.contains(c));

        assertTrue(ae.contains(b));
        assertTrue(ae.contains(ab));
        assertTrue(ae.contains(bc));
        assertTrue(ae.contains(cd));
        assertTrue(ae.contains(de));

        CharRange notb = CharRange.isNot('b');
        CharRange notc = CharRange.isNot('c');
        CharRange notd = CharRange.isNot('d');
        CharRange notab = CharRange.isNotIn('a', 'b');
        CharRange notbc = CharRange.isNotIn('b', 'c');
        CharRange notbd = CharRange.isNotIn('b', 'd');
        CharRange notcd = CharRange.isNotIn('c', 'd');
        CharRange notde = CharRange.isNotIn('d', 'e');
        CharRange notae = CharRange.isNotIn('a', 'e');
        CharRange all = CharRange.isIn((char) 0, Character.MAX_VALUE);
        CharRange allbutfirst = CharRange.isIn((char) 1, Character.MAX_VALUE);

        // normal/negated
        assertFalse(c.contains(notc));
        assertFalse(c.contains(notbd));
        assertTrue(all.contains(notc));
        assertTrue(all.contains(notbd));
        assertFalse(allbutfirst.contains(notc));
        assertFalse(allbutfirst.contains(notbd));

        // negated/normal
        assertTrue(notc.contains(a));
        assertTrue(notc.contains(b));
        assertFalse(notc.contains(c));
        assertTrue(notc.contains(d));
        assertTrue(notc.contains(e));

        assertTrue(notc.contains(ab));
        assertFalse(notc.contains(bc));
        assertFalse(notc.contains(bd));
        assertFalse(notc.contains(cd));
        assertTrue(notc.contains(de));
        assertFalse(notc.contains(ae));
        assertFalse(notc.contains(all));
        assertFalse(notc.contains(allbutfirst));

        assertTrue(notbd.contains(a));
        assertFalse(notbd.contains(b));
        assertFalse(notbd.contains(c));
        assertFalse(notbd.contains(d));
        assertTrue(notbd.contains(e));

        assertTrue(notcd.contains(ab));
        assertFalse(notcd.contains(bc));
        assertFalse(notcd.contains(bd));
        assertFalse(notcd.contains(cd));
        assertFalse(notcd.contains(de));
        assertFalse(notcd.contains(ae));
        assertTrue(notcd.contains(ef));
        assertFalse(notcd.contains(all));
        assertFalse(notcd.contains(allbutfirst));

        // negated/negated
        assertFalse(notc.contains(notb));
        assertTrue(notc.contains(notc));
        assertFalse(notc.contains(notd));

        assertFalse(notc.contains(notab));
        assertTrue(notc.contains(notbc));
        assertTrue(notc.contains(notbd));
        assertTrue(notc.contains(notcd));
        assertFalse(notc.contains(notde));

        assertFalse(notbd.contains(notb));
        assertFalse(notbd.contains(notc));
        assertFalse(notbd.contains(notd));

        assertFalse(notbd.contains(notab));
        assertFalse(notbd.contains(notbc));
        assertTrue(notbd.contains(notbd));
        assertFalse(notbd.contains(notcd));
        assertFalse(notbd.contains(notde));
        assertTrue(notbd.contains(notae));
    }

    @Test
    public void testContainsNullArg() {
        CharRange range = CharRange.is('a');
        try {
            @SuppressWarnings("unused")
            boolean contains = range.contains(null);
        } catch(IllegalArgumentException e) {
            assertEquals("The Range must not be null", e.getMessage());
        }
    }

    @Test
    public void testIterator() {
        CharRange a = CharRange.is('a');
        CharRange ad = CharRange.isIn('a', 'd');
        CharRange nota = CharRange.isNot('a');
        CharRange emptySet = CharRange.isNotIn((char) 0, Character.MAX_VALUE);
        CharRange notFirst = CharRange.isNotIn((char) 1, Character.MAX_VALUE);
        CharRange notLast = CharRange.isNotIn((char) 0, (char) (Character.MAX_VALUE - 1));

        Iterator<Character> aIt = a.iterator();
        assertNotNull(aIt);
        assertTrue(aIt.hasNext());
        assertEquals(Character.valueOf('a'), aIt.next());
        assertFalse(aIt.hasNext());

        Iterator<Character> adIt = ad.iterator();
        assertNotNull(adIt);
        assertTrue(adIt.hasNext());
        assertEquals(Character.valueOf('a'), adIt.next());
        assertEquals(Character.valueOf('b'), adIt.next());
        assertEquals(Character.valueOf('c'), adIt.next());
        assertEquals(Character.valueOf('d'), adIt.next());
        assertFalse(adIt.hasNext());

        Iterator<Character> notaIt = nota.iterator();
        assertNotNull(notaIt);
        assertTrue(notaIt.hasNext());
        while (notaIt.hasNext()) {
            Character c = notaIt.next();
            assertFalse('a' == c.charValue());
        }

        Iterator<Character> emptySetIt = emptySet.iterator();
        assertNotNull(emptySetIt);
        assertFalse(emptySetIt.hasNext());
        try {
            emptySetIt.next();
            fail("Should throw NoSuchElementException");
        } catch (NoSuchElementException e) {
            assertTrue(true);
        }

        Iterator<Character> notFirstIt = notFirst.iterator();
        assertNotNull(notFirstIt);
        assertTrue(notFirstIt.hasNext());
        assertEquals(Character.valueOf((char) 0), notFirstIt.next());
        assertFalse(notFirstIt.hasNext());
        try {
            notFirstIt.next();
            fail("Should throw NoSuchElementException");
        } catch (NoSuchElementException e) {
            assertTrue(true);
        }

        Iterator<Character> notLastIt = notLast.iterator();
        assertNotNull(notLastIt);
        assertTrue(notLastIt.hasNext());
        assertEquals(Character.valueOf(Character.MAX_VALUE), notLastIt.next());
        assertFalse(notLastIt.hasNext());
        try {
            notLastIt.next();
            fail("Should throw NoSuchElementException");
        } catch (NoSuchElementException e) {
            assertTrue(true);
        }
    }

    //-----------------------------------------------------------------------
    @Test
    public void testSerialization() {
        CharRange range = CharRange.is('a');
        assertEquals(range, SerializationUtils.clone(range)); 
        range = CharRange.isIn('a', 'e');
        assertEquals(range, SerializationUtils.clone(range)); 
        range = CharRange.isNotIn('a', 'e');
        assertEquals(range, SerializationUtils.clone(range)); 
    }

}
