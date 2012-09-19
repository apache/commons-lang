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
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.Modifier;

import org.junit.Test;

/**
 * Unit tests {@link org.apache.commons.lang3.CharSet}.
 *
 * @version $Id$
 */
public class CharSetTest  {

    //-----------------------------------------------------------------------
    @Test
    public void testClass() {
        assertTrue(Modifier.isPublic(CharSet.class.getModifiers()));
        assertFalse(Modifier.isFinal(CharSet.class.getModifiers()));
    }
    
    //-----------------------------------------------------------------------
    @Test
    public void testGetInstance() {
        assertSame(CharSet.EMPTY, CharSet.getInstance( (String) null));
        assertSame(CharSet.EMPTY, CharSet.getInstance(""));
        assertSame(CharSet.ASCII_ALPHA, CharSet.getInstance("a-zA-Z"));
        assertSame(CharSet.ASCII_ALPHA, CharSet.getInstance("A-Za-z"));
        assertSame(CharSet.ASCII_ALPHA_LOWER, CharSet.getInstance("a-z"));
        assertSame(CharSet.ASCII_ALPHA_UPPER, CharSet.getInstance("A-Z"));
        assertSame(CharSet.ASCII_NUMERIC, CharSet.getInstance("0-9"));
    }

    //-----------------------------------------------------------------------
    @Test
    public void testGetInstance_Stringarray() {
        assertEquals(null, CharSet.getInstance((String[]) null));
        assertEquals("[]", CharSet.getInstance(new String[0]).toString());
        assertEquals("[]", CharSet.getInstance(new String[] {null}).toString());
        assertEquals("[a-e]", CharSet.getInstance(new String[] {"a-e"}).toString());
    }
    
    //-----------------------------------------------------------------------
    @Test
    public void testConstructor_String_simple() {
        CharSet set;
        CharRange[] array;
        
        set = CharSet.getInstance((String) null);
        array = set.getCharRanges();
        assertEquals("[]", set.toString());
        assertEquals(0, array.length);
        
        set = CharSet.getInstance("");
        array = set.getCharRanges();
        assertEquals("[]", set.toString());
        assertEquals(0, array.length);
        
        set = CharSet.getInstance("a");
        array = set.getCharRanges();
        assertEquals("[a]", set.toString());
        assertEquals(1, array.length);
        assertEquals("a", array[0].toString());
        
        set = CharSet.getInstance("^a");
        array = set.getCharRanges();
        assertEquals("[^a]", set.toString());
        assertEquals(1, array.length);
        assertEquals("^a", array[0].toString());
        
        set = CharSet.getInstance("a-e");
        array = set.getCharRanges();
        assertEquals("[a-e]", set.toString());
        assertEquals(1, array.length);
        assertEquals("a-e", array[0].toString());
        
        set = CharSet.getInstance("^a-e");
        array = set.getCharRanges();
        assertEquals("[^a-e]", set.toString());
        assertEquals(1, array.length);
        assertEquals("^a-e", array[0].toString());
    }
    
    @Test
    public void testConstructor_String_combo() {
        CharSet set;
        CharRange[] array;
        
        set = CharSet.getInstance("abc");
        array = set.getCharRanges();
        assertEquals(3, array.length);
        assertTrue(ArrayUtils.contains(array, CharRange.is('a')));
        assertTrue(ArrayUtils.contains(array, CharRange.is('b')));
        assertTrue(ArrayUtils.contains(array, CharRange.is('c')));
        
        set = CharSet.getInstance("a-ce-f");
        array = set.getCharRanges();
        assertEquals(2, array.length);
        assertTrue(ArrayUtils.contains(array, CharRange.isIn('a', 'c')));
        assertTrue(ArrayUtils.contains(array, CharRange.isIn('e', 'f')));
        
        set = CharSet.getInstance("ae-f");
        array = set.getCharRanges();
        assertEquals(2, array.length);
        assertTrue(ArrayUtils.contains(array, CharRange.is('a')));
        assertTrue(ArrayUtils.contains(array, CharRange.isIn('e', 'f')));
        
        set = CharSet.getInstance("e-fa");
        array = set.getCharRanges();
        assertEquals(2, array.length);
        assertTrue(ArrayUtils.contains(array, CharRange.is('a')));
        assertTrue(ArrayUtils.contains(array, CharRange.isIn('e', 'f')));
        
        set = CharSet.getInstance("ae-fm-pz");
        array = set.getCharRanges();
        assertEquals(4, array.length);
        assertTrue(ArrayUtils.contains(array, CharRange.is('a')));
        assertTrue(ArrayUtils.contains(array, CharRange.isIn('e', 'f')));
        assertTrue(ArrayUtils.contains(array, CharRange.isIn('m', 'p')));
        assertTrue(ArrayUtils.contains(array, CharRange.is('z')));
    }
    
    @Test
    public void testConstructor_String_comboNegated() {
        CharSet set;
        CharRange[] array;
        
        set = CharSet.getInstance("^abc");
        array = set.getCharRanges();
        assertEquals(3, array.length);
        assertTrue(ArrayUtils.contains(array, CharRange.isNot('a')));
        assertTrue(ArrayUtils.contains(array, CharRange.is('b')));
        assertTrue(ArrayUtils.contains(array, CharRange.is('c')));
        
        set = CharSet.getInstance("b^ac");
        array = set.getCharRanges();
        assertEquals(3, array.length);
        assertTrue(ArrayUtils.contains(array, CharRange.is('b')));
        assertTrue(ArrayUtils.contains(array, CharRange.isNot('a')));
        assertTrue(ArrayUtils.contains(array, CharRange.is('c')));
        
        set = CharSet.getInstance("db^ac");
        array = set.getCharRanges();
        assertEquals(4, array.length);
        assertTrue(ArrayUtils.contains(array, CharRange.is('d')));
        assertTrue(ArrayUtils.contains(array, CharRange.is('b')));
        assertTrue(ArrayUtils.contains(array, CharRange.isNot('a')));
        assertTrue(ArrayUtils.contains(array, CharRange.is('c')));
        
        set = CharSet.getInstance("^b^a");
        array = set.getCharRanges();
        assertEquals(2, array.length);
        assertTrue(ArrayUtils.contains(array, CharRange.isNot('b')));
        assertTrue(ArrayUtils.contains(array, CharRange.isNot('a')));
        
        set = CharSet.getInstance("b^a-c^z");
        array = set.getCharRanges();
        assertEquals(3, array.length);
        assertTrue(ArrayUtils.contains(array, CharRange.isNotIn('a', 'c')));
        assertTrue(ArrayUtils.contains(array, CharRange.isNot('z')));
        assertTrue(ArrayUtils.contains(array, CharRange.is('b')));
    }

    @Test
    public void testConstructor_String_oddDash() {
        CharSet set;
        CharRange[] array;
        
        set = CharSet.getInstance("-");
        array = set.getCharRanges();
        assertEquals(1, array.length);
        assertTrue(ArrayUtils.contains(array, CharRange.is('-')));
        
        set = CharSet.getInstance("--");
        array = set.getCharRanges();
        assertEquals(1, array.length);
        assertTrue(ArrayUtils.contains(array, CharRange.is('-')));
        
        set = CharSet.getInstance("---");
        array = set.getCharRanges();
        assertEquals(1, array.length);
        assertTrue(ArrayUtils.contains(array, CharRange.is('-')));
        
        set = CharSet.getInstance("----");
        array = set.getCharRanges();
        assertEquals(1, array.length);
        assertTrue(ArrayUtils.contains(array, CharRange.is('-')));
        
        set = CharSet.getInstance("-a");
        array = set.getCharRanges();
        assertEquals(2, array.length);
        assertTrue(ArrayUtils.contains(array, CharRange.is('-')));
        assertTrue(ArrayUtils.contains(array, CharRange.is('a')));
        
        set = CharSet.getInstance("a-");
        array = set.getCharRanges();
        assertEquals(2, array.length);
        assertTrue(ArrayUtils.contains(array, CharRange.is('a')));
        assertTrue(ArrayUtils.contains(array, CharRange.is('-')));
        
        set = CharSet.getInstance("a--");
        array = set.getCharRanges();
        assertEquals(1, array.length);
        assertTrue(ArrayUtils.contains(array, CharRange.isIn('a', '-')));
        
        set = CharSet.getInstance("--a");
        array = set.getCharRanges();
        assertEquals(1, array.length);
        assertTrue(ArrayUtils.contains(array, CharRange.isIn('-', 'a')));
    }
    
    @Test
    public void testConstructor_String_oddNegate() {
        CharSet set;
        CharRange[] array;
        set = CharSet.getInstance("^");
        array = set.getCharRanges();
        assertEquals(1, array.length);
        assertTrue(ArrayUtils.contains(array, CharRange.is('^'))); // "^"
        
        set = CharSet.getInstance("^^");
        array = set.getCharRanges();
        assertEquals(1, array.length);
        assertTrue(ArrayUtils.contains(array, CharRange.isNot('^'))); // "^^"
        
        set = CharSet.getInstance("^^^");
        array = set.getCharRanges();
        assertEquals(2, array.length);
        assertTrue(ArrayUtils.contains(array, CharRange.isNot('^'))); // "^^"
        assertTrue(ArrayUtils.contains(array, CharRange.is('^'))); // "^"
        
        set = CharSet.getInstance("^^^^");
        array = set.getCharRanges();
        assertEquals(1, array.length);
        assertTrue(ArrayUtils.contains(array, CharRange.isNot('^'))); // "^^" x2
        
        set = CharSet.getInstance("a^");
        array = set.getCharRanges();
        assertEquals(2, array.length);
        assertTrue(ArrayUtils.contains(array, CharRange.is('a'))); // "a"
        assertTrue(ArrayUtils.contains(array, CharRange.is('^'))); // "^"
        
        set = CharSet.getInstance("^a-");
        array = set.getCharRanges();
        assertEquals(2, array.length);
        assertTrue(ArrayUtils.contains(array, CharRange.isNot('a'))); // "^a"
        assertTrue(ArrayUtils.contains(array, CharRange.is('-'))); // "-"
        
        set = CharSet.getInstance("^^-c");
        array = set.getCharRanges();
        assertEquals(1, array.length);
        assertTrue(ArrayUtils.contains(array, CharRange.isNotIn('^', 'c'))); // "^^-c"
        
        set = CharSet.getInstance("^c-^");
        array = set.getCharRanges();
        assertEquals(1, array.length);
        assertTrue(ArrayUtils.contains(array, CharRange.isNotIn('c', '^'))); // "^c-^"
        
        set = CharSet.getInstance("^c-^d");
        array = set.getCharRanges();
        assertEquals(2, array.length);
        assertTrue(ArrayUtils.contains(array, CharRange.isNotIn('c', '^'))); // "^c-^"
        assertTrue(ArrayUtils.contains(array, CharRange.is('d'))); // "d"
        
        set = CharSet.getInstance("^^-");
        array = set.getCharRanges();
        assertEquals(2, array.length);
        assertTrue(ArrayUtils.contains(array, CharRange.isNot('^'))); // "^^"
        assertTrue(ArrayUtils.contains(array, CharRange.is('-'))); // "-"
    }
    
    @Test
    public void testConstructor_String_oddCombinations() {
        CharSet set;
        CharRange[] array = null;
        
        set = CharSet.getInstance("a-^c");
        array = set.getCharRanges();
        assertTrue(ArrayUtils.contains(array, CharRange.isIn('a', '^'))); // "a-^"
        assertTrue(ArrayUtils.contains(array, CharRange.is('c'))); // "c"
        assertFalse(set.contains('b'));
        assertTrue(set.contains('^'));  
        assertTrue(set.contains('_')); // between ^ and a
        assertTrue(set.contains('c'));  
        
        set = CharSet.getInstance("^a-^c");
        array = set.getCharRanges();
        assertTrue(ArrayUtils.contains(array, CharRange.isNotIn('a', '^'))); // "^a-^"
        assertTrue(ArrayUtils.contains(array, CharRange.is('c'))); // "c"
        assertTrue(set.contains('b'));
        assertFalse(set.contains('^'));  
        assertFalse(set.contains('_')); // between ^ and a
        
        set = CharSet.getInstance("a- ^-- "); //contains everything
        array = set.getCharRanges();
        assertTrue(ArrayUtils.contains(array, CharRange.isIn('a', ' '))); // "a- "
        assertTrue(ArrayUtils.contains(array, CharRange.isNotIn('-', ' '))); // "^-- "
        assertTrue(set.contains('#'));
        assertTrue(set.contains('^'));
        assertTrue(set.contains('a'));
        assertTrue(set.contains('*'));
        assertTrue(set.contains('A'));
        
        set = CharSet.getInstance("^-b");
        array = set.getCharRanges();
        assertTrue(ArrayUtils.contains(array, CharRange.isIn('^','b'))); // "^-b"
        assertTrue(set.contains('b'));
        assertTrue(set.contains('_')); // between ^ and a
        assertFalse(set.contains('A'));
        assertTrue(set.contains('^')); 
        
        set = CharSet.getInstance("b-^");
        array = set.getCharRanges();
        assertTrue(ArrayUtils.contains(array, CharRange.isIn('^','b'))); // "b-^"
        assertTrue(set.contains('b'));
        assertTrue(set.contains('^'));
        assertTrue(set.contains('a')); // between ^ and b
        assertFalse(set.contains('c')); 
    }
        
    //-----------------------------------------------------------------------    
    @Test
    public void testEquals_Object() {
        CharSet abc = CharSet.getInstance("abc");
        CharSet abc2 = CharSet.getInstance("abc");
        CharSet atoc = CharSet.getInstance("a-c");
        CharSet atoc2 = CharSet.getInstance("a-c");
        CharSet notatoc = CharSet.getInstance("^a-c");
        CharSet notatoc2 = CharSet.getInstance("^a-c");
        
        assertFalse(abc.equals(null));
        
        assertTrue(abc.equals(abc));
        assertTrue(abc.equals(abc2));
        assertFalse(abc.equals(atoc));
        assertFalse(abc.equals(notatoc));
        
        assertFalse(atoc.equals(abc));
        assertTrue(atoc.equals(atoc));
        assertTrue(atoc.equals(atoc2));
        assertFalse(atoc.equals(notatoc));
        
        assertFalse(notatoc.equals(abc));
        assertFalse(notatoc.equals(atoc));
        assertTrue(notatoc.equals(notatoc));
        assertTrue(notatoc.equals(notatoc2));
    }
            
    @Test
    public void testHashCode() {
        CharSet abc = CharSet.getInstance("abc");
        CharSet abc2 = CharSet.getInstance("abc");
        CharSet atoc = CharSet.getInstance("a-c");
        CharSet atoc2 = CharSet.getInstance("a-c");
        CharSet notatoc = CharSet.getInstance("^a-c");
        CharSet notatoc2 = CharSet.getInstance("^a-c");
        
        assertEquals(abc.hashCode(), abc.hashCode());
        assertEquals(abc.hashCode(), abc2.hashCode());
        assertEquals(atoc.hashCode(), atoc.hashCode());
        assertEquals(atoc.hashCode(), atoc2.hashCode());
        assertEquals(notatoc.hashCode(), notatoc.hashCode());
        assertEquals(notatoc.hashCode(), notatoc2.hashCode());
    }
    
    //-----------------------------------------------------------------------    
    @Test
    public void testContains_Char() {
        CharSet btod = CharSet.getInstance("b-d");
        CharSet dtob = CharSet.getInstance("d-b");
        CharSet bcd = CharSet.getInstance("bcd");
        CharSet bd = CharSet.getInstance("bd");
        CharSet notbtod = CharSet.getInstance("^b-d");
        
        assertFalse(btod.contains('a'));
        assertTrue(btod.contains('b'));
        assertTrue(btod.contains('c'));
        assertTrue(btod.contains('d'));
        assertFalse(btod.contains('e'));
        
        assertFalse(bcd.contains('a'));
        assertTrue(bcd.contains('b'));
        assertTrue(bcd.contains('c'));
        assertTrue(bcd.contains('d'));
        assertFalse(bcd.contains('e'));
        
        assertFalse(bd.contains('a'));
        assertTrue(bd.contains('b'));
        assertFalse(bd.contains('c'));
        assertTrue(bd.contains('d'));
        assertFalse(bd.contains('e'));
        
        assertTrue(notbtod.contains('a'));
        assertFalse(notbtod.contains('b'));
        assertFalse(notbtod.contains('c'));
        assertFalse(notbtod.contains('d'));
        assertTrue(notbtod.contains('e'));
        
        assertFalse(dtob.contains('a'));
        assertTrue(dtob.contains('b'));
        assertTrue(dtob.contains('c'));
        assertTrue(dtob.contains('d'));
        assertFalse(dtob.contains('e'));
      
        CharRange[] array = dtob.getCharRanges();
        assertEquals("[b-d]", dtob.toString());
        assertEquals(1, array.length);
    }
    
    //-----------------------------------------------------------------------    
    @Test
    public void testSerialization() {
        CharSet set = CharSet.getInstance("a");
        assertEquals(set, SerializationUtils.clone(set)); 
        set = CharSet.getInstance("a-e");
        assertEquals(set, SerializationUtils.clone(set)); 
        set = CharSet.getInstance("be-f^a-z");
        assertEquals(set, SerializationUtils.clone(set)); 
    }
    
    //-----------------------------------------------------------------------    
    @Test
    public void testStatics() {
        CharRange[] array;
        
        array = CharSet.EMPTY.getCharRanges();
        assertEquals(0, array.length);
        
        array = CharSet.ASCII_ALPHA.getCharRanges();
        assertEquals(2, array.length);
        assertTrue(ArrayUtils.contains(array, CharRange.isIn('a', 'z')));
        assertTrue(ArrayUtils.contains(array, CharRange.isIn('A', 'Z')));
        
        array = CharSet.ASCII_ALPHA_LOWER.getCharRanges();
        assertEquals(1, array.length);
        assertTrue(ArrayUtils.contains(array, CharRange.isIn('a', 'z')));
        
        array = CharSet.ASCII_ALPHA_UPPER.getCharRanges();
        assertEquals(1, array.length);
        assertTrue(ArrayUtils.contains(array, CharRange.isIn('A', 'Z')));
        
        array = CharSet.ASCII_NUMERIC.getCharRanges();
        assertEquals(1, array.length);
        assertTrue(ArrayUtils.contains(array, CharRange.isIn('0', '9')));
    }
    
}
