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
package org.apache.commons.lang;

import java.lang.reflect.Modifier;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Unit tests {@link org.apache.commons.lang.CharRange}.
 *
 * @author Stephen Colebourne
 * @version $Id$
 */
public class CharRangeTest extends TestCase {
    
    public CharRangeTest(String name) {
        super(name);
    }

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(CharRangeTest.class);
        suite.setName("CharRange Tests");
        return suite;
    }

    @Override
    protected void setUp() throws Exception {
        super.setUp();
    }

    @Override
    protected void tearDown() throws Exception {
        super.tearDown();
    }

    //-----------------------------------------------------------------------
    public void testClass() {
        assertEquals(true, Modifier.isPublic(CharRange.class.getModifiers()));
        assertEquals(true, Modifier.isFinal(CharRange.class.getModifiers()));
    }
    
    //-----------------------------------------------------------------------
    public void testConstructorAccessors_Char() {
        CharRange rangea = new CharRange('a');
        assertEquals('a', rangea.getStart());
        assertEquals('a', rangea.getEnd());
        assertEquals(false, rangea.isNegated());
        assertEquals("a", rangea.toString());
    }
    
    public void testConstructorAccessors_CharBoolean_Normal() {
        CharRange rangea = new CharRange('a');
        assertEquals('a', rangea.getStart());
        assertEquals('a', rangea.getEnd());
        assertEquals(false, rangea.isNegated());
        assertEquals("a", rangea.toString());
    }
    
    public void testConstructorAccessors_CharBoolean_Negated() {
        CharRange rangea = new CharRange('a', true);
        assertEquals('a', rangea.getStart());
        assertEquals('a', rangea.getEnd());
        assertEquals(true, rangea.isNegated());
        assertEquals("^a", rangea.toString());
    }
    
    public void testConstructorAccessors_CharChar_Same() {
        CharRange rangea = new CharRange('a', 'a');
        assertEquals('a', rangea.getStart());
        assertEquals('a', rangea.getEnd());
        assertEquals(false, rangea.isNegated());
        assertEquals("a", rangea.toString());
    }
    
    public void testConstructorAccessors_CharChar_Normal() {
        CharRange rangea = new CharRange('a', 'e');
        assertEquals('a', rangea.getStart());
        assertEquals('e', rangea.getEnd());
        assertEquals(false, rangea.isNegated());
        assertEquals("a-e", rangea.toString());
    }
    
    public void testConstructorAccessors_CharChar_Reversed() {
        CharRange rangea = new CharRange('e', 'a');
        assertEquals('a', rangea.getStart());
        assertEquals('e', rangea.getEnd());
        assertEquals(false, rangea.isNegated());
        assertEquals("a-e", rangea.toString());
    }
    
    public void testConstructorAccessors_CharCharBoolean_Same() {
        CharRange rangea = new CharRange('a', 'a', false);
        assertEquals('a', rangea.getStart());
        assertEquals('a', rangea.getEnd());
        assertEquals(false, rangea.isNegated());
        assertEquals("a", rangea.toString());
    }
    
    public void testConstructorAccessors_CharCharBoolean_Normal() {
        CharRange rangea = new CharRange('a', 'e', false);
        assertEquals('a', rangea.getStart());
        assertEquals('e', rangea.getEnd());
        assertEquals(false, rangea.isNegated());
        assertEquals("a-e", rangea.toString());
    }
    
    public void testConstructorAccessors_CharCharBoolean_Reversed() {
        CharRange rangea = new CharRange('e', 'a', false);
        assertEquals('a', rangea.getStart());
        assertEquals('e', rangea.getEnd());
        assertEquals(false, rangea.isNegated());
        assertEquals("a-e", rangea.toString());
    }
    
    public void testConstructorAccessors_CharCharBoolean_SameNegated() {
        CharRange rangea = new CharRange('a', 'a', true);
        assertEquals('a', rangea.getStart());
        assertEquals('a', rangea.getEnd());
        assertEquals(true, rangea.isNegated());
        assertEquals("^a", rangea.toString());
    }
    
    public void testConstructorAccessors_CharCharBoolean_NormalNegated() {
        CharRange rangea = new CharRange('a', 'e', true);
        assertEquals('a', rangea.getStart());
        assertEquals('e', rangea.getEnd());
        assertEquals(true, rangea.isNegated());
        assertEquals("^a-e", rangea.toString());
    }
    
    public void testConstructorAccessors_CharCharBoolean_ReversedNegated() {
        CharRange rangea = new CharRange('e', 'a', true);
        assertEquals('a', rangea.getStart());
        assertEquals('e', rangea.getEnd());
        assertEquals(true, rangea.isNegated());
        assertEquals("^a-e", rangea.toString());
    }

    //-----------------------------------------------------------------------    
    public void testEquals_Object() {
        CharRange rangea = new CharRange('a');
        CharRange rangeae = new CharRange('a', 'e');
        CharRange rangenotbf = new CharRange('b', 'f', false);
        
        assertEquals(false, rangea.equals(null));
        
        assertEquals(true, rangea.equals(rangea));
        assertEquals(true, rangea.equals(new CharRange('a')));
        assertEquals(true, rangeae.equals(rangeae));
        assertEquals(true, rangeae.equals(new CharRange('a', 'e')));
        assertEquals(true, rangenotbf.equals(rangenotbf));
        assertEquals(true, rangenotbf.equals(new CharRange('b', 'f', false)));
        
        assertEquals(false, rangea.equals(rangeae));
        assertEquals(false, rangea.equals(rangenotbf));
        assertEquals(false, rangeae.equals(rangea));
        assertEquals(false, rangeae.equals(rangenotbf));
        assertEquals(false, rangenotbf.equals(rangea));
        assertEquals(false, rangenotbf.equals(rangeae));
    }
            
    public void testHashCode() {
        CharRange rangea = new CharRange('a');
        CharRange rangeae = new CharRange('a', 'e');
        CharRange rangenotbf = new CharRange('b', 'f', false);
        
        assertEquals(true, rangea.hashCode() == rangea.hashCode());
        assertEquals(true, rangea.hashCode() == new CharRange('a').hashCode());
        assertEquals(true, rangeae.hashCode() == rangeae.hashCode());
        assertEquals(true, rangeae.hashCode() == new CharRange('a', 'e').hashCode());
        assertEquals(true, rangenotbf.hashCode() == rangenotbf.hashCode());
        assertEquals(true, rangenotbf.hashCode() == new CharRange('b', 'f', false).hashCode());
        
        assertEquals(false, rangea.hashCode() == rangeae.hashCode());
        assertEquals(false, rangea.hashCode() == rangenotbf.hashCode());
        assertEquals(false, rangeae.hashCode() == rangea.hashCode());
        assertEquals(false, rangeae.hashCode() == rangenotbf.hashCode());
        assertEquals(false, rangenotbf.hashCode() == rangea.hashCode());
        assertEquals(false, rangenotbf.hashCode() == rangeae.hashCode());
    }
    
    //-----------------------------------------------------------------------    
    public void testContains_Char() {
        CharRange range = new CharRange('c');
        assertEquals(false, range.contains('b'));
        assertEquals(true, range.contains('c'));
        assertEquals(false, range.contains('d'));
        assertEquals(false, range.contains('e'));
        
        range = new CharRange('c', 'd');
        assertEquals(false, range.contains('b'));
        assertEquals(true, range.contains('c'));
        assertEquals(true, range.contains('d'));
        assertEquals(false, range.contains('e'));
        
        range = new CharRange('d', 'c');
        assertEquals(false, range.contains('b'));
        assertEquals(true, range.contains('c'));
        assertEquals(true, range.contains('d'));
        assertEquals(false, range.contains('e'));
        
        range = new CharRange('c', 'd', false);
        assertEquals(false, range.contains('b'));
        assertEquals(true, range.contains('c'));
        assertEquals(true, range.contains('d'));
        assertEquals(false, range.contains('e'));
        
        range = new CharRange('c', 'd', true);
        assertEquals(true, range.contains('b'));
        assertEquals(false, range.contains('c'));
        assertEquals(false, range.contains('d'));
        assertEquals(true, range.contains('e'));
        assertEquals(true, range.contains((char) 0));
        assertEquals(true, range.contains(Character.MAX_VALUE));
    }
    
    //-----------------------------------------------------------------------    
    public void testContains_Charrange() {
        CharRange a = new CharRange('a');
        CharRange b = new CharRange('b');
        CharRange c = new CharRange('c');
        CharRange c2 = new CharRange('c');
        CharRange d = new CharRange('d');
        CharRange e = new CharRange('e');
        CharRange cd = new CharRange('c', 'd');
        CharRange bd = new CharRange('b', 'd');
        CharRange bc = new CharRange('b', 'c');
        CharRange ab = new CharRange('a', 'b');
        CharRange de = new CharRange('d', 'e');
        CharRange ef = new CharRange('e', 'f');
        CharRange ae = new CharRange('a', 'e');
        
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
        
        CharRange notb = new CharRange('b', 'b', true);
        CharRange notc = new CharRange('c', 'c', true);
        CharRange notd = new CharRange('d', 'd', true);
        CharRange notab = new CharRange('a', 'b', true);
        CharRange notbc = new CharRange('b', 'c', true);
        CharRange notbd = new CharRange('b', 'd', true);
        CharRange notcd = new CharRange('c', 'd', true);
        CharRange notde = new CharRange('d', 'e', true);
        CharRange notae = new CharRange('a', 'e', true);
        CharRange all = new CharRange((char) 0, Character.MAX_VALUE);
        CharRange allbutfirst = new CharRange((char) 1, Character.MAX_VALUE);
        
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
        CharRange range = new CharRange('a');
        try {
            @SuppressWarnings("unused")
            boolean contains = range.contains(null);
        } catch(IllegalArgumentException e) {
            assertEquals("The Range must not be null", e.getMessage());
        }
    }
    
    //-----------------------------------------------------------------------    
    public void testSerialization() {
        CharRange range = new CharRange('a');
        assertEquals(range, SerializationUtils.clone(range)); 
        range = new CharRange('a', 'e');
        assertEquals(range, SerializationUtils.clone(range)); 
        range = new CharRange('a', 'e', true);
        assertEquals(range, SerializationUtils.clone(range)); 
    }
    
}
