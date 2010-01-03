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

import junit.framework.TestCase;

/**
 * Unit tests {@link org.apache.commons.lang3.CharSet}.
 *
 * @author Apache Software Foundation
 * @author Phil Steitz
 * @version $Id$
 */
public class CharSetTest extends TestCase {
    
    public CharSetTest(String name) {
        super(name);
    }

    //-----------------------------------------------------------------------
    public void testClass() {
        assertEquals(true, Modifier.isPublic(CharSet.class.getModifiers()));
        assertEquals(false, Modifier.isFinal(CharSet.class.getModifiers()));
    }
    
    //-----------------------------------------------------------------------
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
    public void testGetInstance_Stringarray() {
        assertEquals(null, CharSet.getInstance((String[]) null));
        assertEquals("[]", CharSet.getInstance(new String[0]).toString());
        assertEquals("[]", CharSet.getInstance(new String[] {null}).toString());
        assertEquals("[a-e]", CharSet.getInstance(new String[] {"a-e"}).toString());
    }
    
    //-----------------------------------------------------------------------
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
    
    public void testConstructor_String_combo() {
        CharSet set;
        CharRange[] array;
        
        set = CharSet.getInstance("abc");
        array = set.getCharRanges();
        assertEquals(3, array.length);
        assertEquals(true, ArrayUtils.contains(array, CharRange.is('a')));
        assertEquals(true, ArrayUtils.contains(array, CharRange.is('b')));
        assertEquals(true, ArrayUtils.contains(array, CharRange.is('c')));
        
        set = CharSet.getInstance("a-ce-f");
        array = set.getCharRanges();
        assertEquals(2, array.length);
        assertEquals(true, ArrayUtils.contains(array, CharRange.isIn('a', 'c')));
        assertEquals(true, ArrayUtils.contains(array, CharRange.isIn('e', 'f')));
        
        set = CharSet.getInstance("ae-f");
        array = set.getCharRanges();
        assertEquals(2, array.length);
        assertEquals(true, ArrayUtils.contains(array, CharRange.is('a')));
        assertEquals(true, ArrayUtils.contains(array, CharRange.isIn('e', 'f')));
        
        set = CharSet.getInstance("e-fa");
        array = set.getCharRanges();
        assertEquals(2, array.length);
        assertEquals(true, ArrayUtils.contains(array, CharRange.is('a')));
        assertEquals(true, ArrayUtils.contains(array, CharRange.isIn('e', 'f')));
        
        set = CharSet.getInstance("ae-fm-pz");
        array = set.getCharRanges();
        assertEquals(4, array.length);
        assertEquals(true, ArrayUtils.contains(array, CharRange.is('a')));
        assertEquals(true, ArrayUtils.contains(array, CharRange.isIn('e', 'f')));
        assertEquals(true, ArrayUtils.contains(array, CharRange.isIn('m', 'p')));
        assertEquals(true, ArrayUtils.contains(array, CharRange.is('z')));
    }
    
    public void testConstructor_String_comboNegated() {
        CharSet set;
        CharRange[] array;
        
        set = CharSet.getInstance("^abc");
        array = set.getCharRanges();
        assertEquals(3, array.length);
        assertEquals(true, ArrayUtils.contains(array, CharRange.isNot('a')));
        assertEquals(true, ArrayUtils.contains(array, CharRange.is('b')));
        assertEquals(true, ArrayUtils.contains(array, CharRange.is('c')));
        
        set = CharSet.getInstance("b^ac");
        array = set.getCharRanges();
        assertEquals(3, array.length);
        assertEquals(true, ArrayUtils.contains(array, CharRange.is('b')));
        assertEquals(true, ArrayUtils.contains(array, CharRange.isNot('a')));
        assertEquals(true, ArrayUtils.contains(array, CharRange.is('c')));
        
        set = CharSet.getInstance("db^ac");
        array = set.getCharRanges();
        assertEquals(4, array.length);
        assertEquals(true, ArrayUtils.contains(array, CharRange.is('d')));
        assertEquals(true, ArrayUtils.contains(array, CharRange.is('b')));
        assertEquals(true, ArrayUtils.contains(array, CharRange.isNot('a')));
        assertEquals(true, ArrayUtils.contains(array, CharRange.is('c')));
        
        set = CharSet.getInstance("^b^a");
        array = set.getCharRanges();
        assertEquals(2, array.length);
        assertEquals(true, ArrayUtils.contains(array, CharRange.isNot('b')));
        assertEquals(true, ArrayUtils.contains(array, CharRange.isNot('a')));
        
        set = CharSet.getInstance("b^a-c^z");
        array = set.getCharRanges();
        assertEquals(3, array.length);
        assertEquals(true, ArrayUtils.contains(array, CharRange.isNotIn('a', 'c')));
        assertEquals(true, ArrayUtils.contains(array, CharRange.isNot('z')));
        assertEquals(true, ArrayUtils.contains(array, CharRange.is('b')));
    }

    public void testConstructor_String_oddDash() {
        CharSet set;
        CharRange[] array;
        
        set = CharSet.getInstance("-");
        array = set.getCharRanges();
        assertEquals(1, array.length);
        assertEquals(true, ArrayUtils.contains(array, CharRange.is('-')));
        
        set = CharSet.getInstance("--");
        array = set.getCharRanges();
        assertEquals(1, array.length);
        assertEquals(true, ArrayUtils.contains(array, CharRange.is('-')));
        
        set = CharSet.getInstance("---");
        array = set.getCharRanges();
        assertEquals(1, array.length);
        assertEquals(true, ArrayUtils.contains(array, CharRange.is('-')));
        
        set = CharSet.getInstance("----");
        array = set.getCharRanges();
        assertEquals(1, array.length);
        assertEquals(true, ArrayUtils.contains(array, CharRange.is('-')));
        
        set = CharSet.getInstance("-a");
        array = set.getCharRanges();
        assertEquals(2, array.length);
        assertEquals(true, ArrayUtils.contains(array, CharRange.is('-')));
        assertEquals(true, ArrayUtils.contains(array, CharRange.is('a')));
        
        set = CharSet.getInstance("a-");
        array = set.getCharRanges();
        assertEquals(2, array.length);
        assertEquals(true, ArrayUtils.contains(array, CharRange.is('a')));
        assertEquals(true, ArrayUtils.contains(array, CharRange.is('-')));
        
        set = CharSet.getInstance("a--");
        array = set.getCharRanges();
        assertEquals(1, array.length);
        assertEquals(true, ArrayUtils.contains(array, CharRange.isIn('a', '-')));
        
        set = CharSet.getInstance("--a");
        array = set.getCharRanges();
        assertEquals(1, array.length);
        assertEquals(true, ArrayUtils.contains(array, CharRange.isIn('-', 'a')));
    }
    
    public void testConstructor_String_oddNegate() {
        CharSet set;
        CharRange[] array;
        set = CharSet.getInstance("^");
        array = set.getCharRanges();
        assertEquals(1, array.length);
        assertEquals(true, ArrayUtils.contains(array, CharRange.is('^'))); // "^"
        
        set = CharSet.getInstance("^^");
        array = set.getCharRanges();
        assertEquals(1, array.length);
        assertEquals(true, ArrayUtils.contains(array, CharRange.isNot('^'))); // "^^"
        
        set = CharSet.getInstance("^^^");
        array = set.getCharRanges();
        assertEquals(2, array.length);
        assertEquals(true, ArrayUtils.contains(array, CharRange.isNot('^'))); // "^^"
        assertEquals(true, ArrayUtils.contains(array, CharRange.is('^'))); // "^"
        
        set = CharSet.getInstance("^^^^");
        array = set.getCharRanges();
        assertEquals(1, array.length);
        assertEquals(true, ArrayUtils.contains(array, CharRange.isNot('^'))); // "^^" x2
        
        set = CharSet.getInstance("a^");
        array = set.getCharRanges();
        assertEquals(2, array.length);
        assertEquals(true, ArrayUtils.contains(array, CharRange.is('a'))); // "a"
        assertEquals(true, ArrayUtils.contains(array, CharRange.is('^'))); // "^"
        
        set = CharSet.getInstance("^a-");
        array = set.getCharRanges();
        assertEquals(2, array.length);
        assertEquals(true, ArrayUtils.contains(array, CharRange.isNot('a'))); // "^a"
        assertEquals(true, ArrayUtils.contains(array, CharRange.is('-'))); // "-"
        
        set = CharSet.getInstance("^^-c");
        array = set.getCharRanges();
        assertEquals(1, array.length);
        assertEquals(true, ArrayUtils.contains(array, CharRange.isNotIn('^', 'c'))); // "^^-c"
        
        set = CharSet.getInstance("^c-^");
        array = set.getCharRanges();
        assertEquals(1, array.length);
        assertEquals(true, ArrayUtils.contains(array, CharRange.isNotIn('c', '^'))); // "^c-^"
        
        set = CharSet.getInstance("^c-^d");
        array = set.getCharRanges();
        assertEquals(2, array.length);
        assertEquals(true, ArrayUtils.contains(array, CharRange.isNotIn('c', '^'))); // "^c-^"
        assertEquals(true, ArrayUtils.contains(array, CharRange.is('d'))); // "d"
        
        set = CharSet.getInstance("^^-");
        array = set.getCharRanges();
        assertEquals(2, array.length);
        assertEquals(true, ArrayUtils.contains(array, CharRange.isNot('^'))); // "^^"
        assertEquals(true, ArrayUtils.contains(array, CharRange.is('-'))); // "-"
    }
    
    public void testConstructor_String_oddCombinations() {
        CharSet set;
        CharRange[] array = null;
        
        set = CharSet.getInstance("a-^c");
        array = set.getCharRanges();
        assertEquals(true, ArrayUtils.contains(array, CharRange.isIn('a', '^'))); // "a-^"
        assertEquals(true, ArrayUtils.contains(array, CharRange.is('c'))); // "c"
        assertEquals(false, set.contains('b'));
        assertEquals(true, set.contains('^'));  
        assertEquals(true, set.contains('_')); // between ^ and a
        assertEquals(true, set.contains('c'));  
        
        set = CharSet.getInstance("^a-^c");
        array = set.getCharRanges();
        assertEquals(true, ArrayUtils.contains(array, CharRange.isNotIn('a', '^'))); // "^a-^"
        assertEquals(true, ArrayUtils.contains(array, CharRange.is('c'))); // "c"
        assertEquals(true, set.contains('b'));
        assertEquals(false, set.contains('^'));  
        assertEquals(false, set.contains('_')); // between ^ and a
        
        set = CharSet.getInstance("a- ^-- "); //contains everything
        array = set.getCharRanges();
        assertEquals(true, ArrayUtils.contains(array, CharRange.isIn('a', ' '))); // "a- "
        assertEquals(true, ArrayUtils.contains(array, CharRange.isNotIn('-', ' '))); // "^-- "
        assertEquals(true, set.contains('#'));
        assertEquals(true, set.contains('^'));
        assertEquals(true, set.contains('a'));
        assertEquals(true, set.contains('*'));
        assertEquals(true, set.contains('A'));
        
        set = CharSet.getInstance("^-b");
        array = set.getCharRanges();
        assertEquals(true, ArrayUtils.contains(array, CharRange.isIn('^','b'))); // "^-b"
        assertEquals(true, set.contains('b'));
        assertEquals(true, set.contains('_')); // between ^ and a
        assertEquals(false, set.contains('A'));
        assertEquals(true, set.contains('^')); 
        
        set = CharSet.getInstance("b-^");
        array = set.getCharRanges();
        assertEquals(true, ArrayUtils.contains(array, CharRange.isIn('^','b'))); // "b-^"
        assertEquals(true, set.contains('b'));
        assertEquals(true, set.contains('^'));
        assertEquals(true, set.contains('a')); // between ^ and b
        assertEquals(false, set.contains('c')); 
    }
        
    //-----------------------------------------------------------------------    
    public void testEquals_Object() {
        CharSet abc = CharSet.getInstance("abc");
        CharSet abc2 = CharSet.getInstance("abc");
        CharSet atoc = CharSet.getInstance("a-c");
        CharSet atoc2 = CharSet.getInstance("a-c");
        CharSet notatoc = CharSet.getInstance("^a-c");
        CharSet notatoc2 = CharSet.getInstance("^a-c");
        
        assertEquals(false, abc.equals(null));
        
        assertEquals(true, abc.equals(abc));
        assertEquals(true, abc.equals(abc2));
        assertEquals(false, abc.equals(atoc));
        assertEquals(false, abc.equals(notatoc));
        
        assertEquals(false, atoc.equals(abc));
        assertEquals(true, atoc.equals(atoc));
        assertEquals(true, atoc.equals(atoc2));
        assertEquals(false, atoc.equals(notatoc));
        
        assertEquals(false, notatoc.equals(abc));
        assertEquals(false, notatoc.equals(atoc));
        assertEquals(true, notatoc.equals(notatoc));
        assertEquals(true, notatoc.equals(notatoc2));
    }
            
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
    public void testContains_Char() {
        CharSet btod = CharSet.getInstance("b-d");
        CharSet dtob = CharSet.getInstance("d-b");
        CharSet bcd = CharSet.getInstance("bcd");
        CharSet bd = CharSet.getInstance("bd");
        CharSet notbtod = CharSet.getInstance("^b-d");
        
        assertEquals(false, btod.contains('a'));
        assertEquals(true, btod.contains('b'));
        assertEquals(true, btod.contains('c'));
        assertEquals(true, btod.contains('d'));
        assertEquals(false, btod.contains('e'));
        
        assertEquals(false, bcd.contains('a'));
        assertEquals(true, bcd.contains('b'));
        assertEquals(true, bcd.contains('c'));
        assertEquals(true, bcd.contains('d'));
        assertEquals(false, bcd.contains('e'));
        
        assertEquals(false, bd.contains('a'));
        assertEquals(true, bd.contains('b'));
        assertEquals(false, bd.contains('c'));
        assertEquals(true, bd.contains('d'));
        assertEquals(false, bd.contains('e'));
        
        assertEquals(true, notbtod.contains('a'));
        assertEquals(false, notbtod.contains('b'));
        assertEquals(false, notbtod.contains('c'));
        assertEquals(false, notbtod.contains('d'));
        assertEquals(true, notbtod.contains('e'));
        
        assertEquals(false, dtob.contains('a'));
        assertEquals(true, dtob.contains('b'));
        assertEquals(true, dtob.contains('c'));
        assertEquals(true, dtob.contains('d'));
        assertEquals(false, dtob.contains('e'));
      
        CharRange[] array = dtob.getCharRanges();
        assertEquals("[b-d]", dtob.toString());
        assertEquals(1, array.length);
    }
    
    //-----------------------------------------------------------------------    
    public void testSerialization() {
        CharSet set = CharSet.getInstance("a");
        assertEquals(set, SerializationUtils.clone(set)); 
        set = CharSet.getInstance("a-e");
        assertEquals(set, SerializationUtils.clone(set)); 
        set = CharSet.getInstance("be-f^a-z");
        assertEquals(set, SerializationUtils.clone(set)); 
    }
    
    //-----------------------------------------------------------------------    
    public void testStatics() {
        CharRange[] array;
        
        array = CharSet.EMPTY.getCharRanges();
        assertEquals(0, array.length);
        
        array = CharSet.ASCII_ALPHA.getCharRanges();
        assertEquals(2, array.length);
        assertEquals(true, ArrayUtils.contains(array, CharRange.isIn('a', 'z')));
        assertEquals(true, ArrayUtils.contains(array, CharRange.isIn('A', 'Z')));
        
        array = CharSet.ASCII_ALPHA_LOWER.getCharRanges();
        assertEquals(1, array.length);
        assertEquals(true, ArrayUtils.contains(array, CharRange.isIn('a', 'z')));
        
        array = CharSet.ASCII_ALPHA_UPPER.getCharRanges();
        assertEquals(1, array.length);
        assertEquals(true, ArrayUtils.contains(array, CharRange.isIn('A', 'Z')));
        
        array = CharSet.ASCII_NUMERIC.getCharRanges();
        assertEquals(1, array.length);
        assertEquals(true, ArrayUtils.contains(array, CharRange.isIn('0', '9')));
    }
    
}
