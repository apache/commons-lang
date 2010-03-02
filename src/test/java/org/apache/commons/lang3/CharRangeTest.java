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

import java.lang.reflect.Modifier;
import java.util.Iterator;
import java.util.NoSuchElementException;

import junit.framework.TestCase;

/**
 * Unit tests {@link org.apache.commons.lang3.CharRange}.
 *
 * @author Apache Software Foundation
 * @version $Id$
 */
public class CharRangeTest extends TestCase {

    public CharRangeTest(String name) {
        super(name);
    }

    //-----------------------------------------------------------------------
    public void testClass() {
        assertEquals(true, Modifier.isPublic(CharRange.class.getModifiers()));
        assertEquals(true, Modifier.isFinal(CharRange.class.getModifiers()));
    }

    //-----------------------------------------------------------------------
    public void testConstructorAccessors_is() {
        CharRange rangea = CharRange.is('a');
        assertEquals('a', rangea.getStart());
        assertEquals('a', rangea.getEnd());
        assertEquals(false, rangea.isNegated());
        assertEquals("a", rangea.toString());
    }

    public void testConstructorAccessors_isNot() {
        CharRange rangea = CharRange.isNot('a');
        assertEquals('a', rangea.getStart());
        assertEquals('a', rangea.getEnd());
        assertEquals(true, rangea.isNegated());
        assertEquals("^a", rangea.toString());
    }

    public void testConstructorAccessors_isIn_Same() {
        CharRange rangea = CharRange.isIn('a', 'a');
        assertEquals('a', rangea.getStart());
        assertEquals('a', rangea.getEnd());
        assertEquals(false, rangea.isNegated());
        assertEquals("a", rangea.toString());
    }

    public void testConstructorAccessors_isIn_Normal() {
        CharRange rangea = CharRange.isIn('a', 'e');
        assertEquals('a', rangea.getStart());
        assertEquals('e', rangea.getEnd());
        assertEquals(false, rangea.isNegated());
        assertEquals("a-e", rangea.toString());
    }

    public void testConstructorAccessors_isIn_Reversed() {
        CharRange rangea = CharRange.isIn('e', 'a');
        assertEquals('a', rangea.getStart());
        assertEquals('e', rangea.getEnd());
        assertEquals(false, rangea.isNegated());
        assertEquals("a-e", rangea.toString());
    }

    public void testConstructorAccessors_isNotIn_Same() {
        CharRange rangea = CharRange.isNotIn('a', 'a');
        assertEquals('a', rangea.getStart());
        assertEquals('a', rangea.getEnd());
        assertEquals(true, rangea.isNegated());
        assertEquals("^a", rangea.toString());
    }

    public void testConstructorAccessors_isNotIn_Normal() {
        CharRange rangea = CharRange.isNotIn('a', 'e');
        assertEquals('a', rangea.getStart());
        assertEquals('e', rangea.getEnd());
        assertEquals(true, rangea.isNegated());
        assertEquals("^a-e", rangea.toString());
    }

    public void testConstructorAccessors_isNotIn_Reversed() {
        CharRange rangea = CharRange.isNotIn('e', 'a');
        assertEquals('a', rangea.getStart());
        assertEquals('e', rangea.getEnd());
        assertEquals(true, rangea.isNegated());
        assertEquals("^a-e", rangea.toString());
    }

    //-----------------------------------------------------------------------
    public void testEquals_Object() {
        CharRange rangea = CharRange.is('a');
        CharRange rangeae = CharRange.isIn('a', 'e');
        CharRange rangenotbf = CharRange.isIn('b', 'f');

        assertEquals(false, rangea.equals(null));

        assertEquals(true, rangea.equals(rangea));
        assertEquals(true, rangea.equals(CharRange.is('a')));
        assertEquals(true, rangeae.equals(rangeae));
        assertEquals(true, rangeae.equals(CharRange.isIn('a', 'e')));
        assertEquals(true, rangenotbf.equals(rangenotbf));
        assertEquals(true, rangenotbf.equals(CharRange.isIn('b', 'f')));

        assertEquals(false, rangea.equals(rangeae));
        assertEquals(false, rangea.equals(rangenotbf));
        assertEquals(false, rangeae.equals(rangea));
        assertEquals(false, rangeae.equals(rangenotbf));
        assertEquals(false, rangenotbf.equals(rangea));
        assertEquals(false, rangenotbf.equals(rangeae));
    }

    public void testHashCode() {
        CharRange rangea = CharRange.is('a');
        CharRange rangeae = CharRange.isIn('a', 'e');
        CharRange rangenotbf = CharRange.isIn('b', 'f');

        assertEquals(true, rangea.hashCode() == rangea.hashCode());
        assertEquals(true, rangea.hashCode() == CharRange.is('a').hashCode());
        assertEquals(true, rangeae.hashCode() == rangeae.hashCode());
        assertEquals(true, rangeae.hashCode() == CharRange.isIn('a', 'e').hashCode());
        assertEquals(true, rangenotbf.hashCode() == rangenotbf.hashCode());
        assertEquals(true, rangenotbf.hashCode() == CharRange.isIn('b', 'f').hashCode());

        assertEquals(false, rangea.hashCode() == rangeae.hashCode());
        assertEquals(false, rangea.hashCode() == rangenotbf.hashCode());
        assertEquals(false, rangeae.hashCode() == rangea.hashCode());
        assertEquals(false, rangeae.hashCode() == rangenotbf.hashCode());
        assertEquals(false, rangenotbf.hashCode() == rangea.hashCode());
        assertEquals(false, rangenotbf.hashCode() == rangeae.hashCode());
    }

    //-----------------------------------------------------------------------
    public void testContains_Char() {
        CharRange range = CharRange.is('c');
        assertEquals(false, range.contains('b'));
        assertEquals(true, range.contains('c'));
        assertEquals(false, range.contains('d'));
        assertEquals(false, range.contains('e'));

        range = CharRange.isIn('c', 'd');
        assertEquals(false, range.contains('b'));
        assertEquals(true, range.contains('c'));
        assertEquals(true, range.contains('d'));
        assertEquals(false, range.contains('e'));

        range = CharRange.isIn('d', 'c');
        assertEquals(false, range.contains('b'));
        assertEquals(true, range.contains('c'));
        assertEquals(true, range.contains('d'));
        assertEquals(false, range.contains('e'));

        range = CharRange.isNotIn('c', 'd');
        assertEquals(true, range.contains('b'));
        assertEquals(false, range.contains('c'));
        assertEquals(false, range.contains('d'));
        assertEquals(true, range.contains('e'));
        assertEquals(true, range.contains((char) 0));
        assertEquals(true, range.contains(Character.MAX_VALUE));
    }

    //-----------------------------------------------------------------------
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
        assertEquals(false, c.contains(b));
        assertEquals(true, c.contains(c));
        assertEquals(true, c.contains(c2));
        assertEquals(false, c.contains(d));

        assertEquals(false, c.contains(cd));
        assertEquals(false, c.contains(bd));
        assertEquals(false, c.contains(bc));
        assertEquals(false, c.contains(ab));
        assertEquals(false, c.contains(de));

        assertEquals(true, cd.contains(c));
        assertEquals(true, bd.contains(c));
        assertEquals(true, bc.contains(c));
        assertEquals(false, ab.contains(c));
        assertEquals(false, de.contains(c));

        assertEquals(true, ae.contains(b));
        assertEquals(true, ae.contains(ab));
        assertEquals(true, ae.contains(bc));
        assertEquals(true, ae.contains(cd));
        assertEquals(true, ae.contains(de));

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
        assertEquals(false, c.contains(notc));
        assertEquals(false, c.contains(notbd));
        assertEquals(true, all.contains(notc));
        assertEquals(true, all.contains(notbd));
        assertEquals(false, allbutfirst.contains(notc));
        assertEquals(false, allbutfirst.contains(notbd));

        // negated/normal
        assertEquals(true, notc.contains(a));
        assertEquals(true, notc.contains(b));
        assertEquals(false, notc.contains(c));
        assertEquals(true, notc.contains(d));
        assertEquals(true, notc.contains(e));

        assertEquals(true, notc.contains(ab));
        assertEquals(false, notc.contains(bc));
        assertEquals(false, notc.contains(bd));
        assertEquals(false, notc.contains(cd));
        assertEquals(true, notc.contains(de));
        assertEquals(false, notc.contains(ae));
        assertEquals(false, notc.contains(all));
        assertEquals(false, notc.contains(allbutfirst));

        assertEquals(true, notbd.contains(a));
        assertEquals(false, notbd.contains(b));
        assertEquals(false, notbd.contains(c));
        assertEquals(false, notbd.contains(d));
        assertEquals(true, notbd.contains(e));

        assertEquals(true, notcd.contains(ab));
        assertEquals(false, notcd.contains(bc));
        assertEquals(false, notcd.contains(bd));
        assertEquals(false, notcd.contains(cd));
        assertEquals(false, notcd.contains(de));
        assertEquals(false, notcd.contains(ae));
        assertEquals(true, notcd.contains(ef));
        assertEquals(false, notcd.contains(all));
        assertEquals(false, notcd.contains(allbutfirst));

        // negated/negated
        assertEquals(false, notc.contains(notb));
        assertEquals(true, notc.contains(notc));
        assertEquals(false, notc.contains(notd));

        assertEquals(false, notc.contains(notab));
        assertEquals(true, notc.contains(notbc));
        assertEquals(true, notc.contains(notbd));
        assertEquals(true, notc.contains(notcd));
        assertEquals(false, notc.contains(notde));

        assertEquals(false, notbd.contains(notb));
        assertEquals(false, notbd.contains(notc));
        assertEquals(false, notbd.contains(notd));

        assertEquals(false, notbd.contains(notab));
        assertEquals(false, notbd.contains(notbc));
        assertEquals(true, notbd.contains(notbd));
        assertEquals(false, notbd.contains(notcd));
        assertEquals(false, notbd.contains(notde));
        assertEquals(true, notbd.contains(notae));
    }

    public void testContainsNullArg() {
        CharRange range = CharRange.is('a');
        try {
            @SuppressWarnings("unused")
            boolean contains = range.contains(null);
        } catch(IllegalArgumentException e) {
            assertEquals("The Range must not be null", e.getMessage());
        }
    }

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
    public void testSerialization() {
        CharRange range = CharRange.is('a');
        assertEquals(range, SerializationUtils.clone(range)); 
        range = CharRange.isIn('a', 'e');
        assertEquals(range, SerializationUtils.clone(range)); 
        range = CharRange.isNotIn('a', 'e');
        assertEquals(range, SerializationUtils.clone(range)); 
    }

}
