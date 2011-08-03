/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang3;

import static org.junit.Assert.*;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;

import org.junit.Test;

/**
 * Unit tests {@link org.apache.commons.lang3.BooleanUtils}.
 *
 * @version $Id$
 */
public class BooleanUtilsTest {

    //-----------------------------------------------------------------------
    @Test
    public void testConstructor() {
        assertNotNull(new BooleanUtils());
        Constructor<?>[] cons = BooleanUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertEquals(true, Modifier.isPublic(cons[0].getModifiers()));
        assertEquals(true, Modifier.isPublic(BooleanUtils.class.getModifiers()));
        assertEquals(false, Modifier.isFinal(BooleanUtils.class.getModifiers()));
    }
    
    //-----------------------------------------------------------------------
    @Test
    public void test_negate_Boolean() {
        assertSame(null, BooleanUtils.negate(null));
        assertSame(Boolean.TRUE, BooleanUtils.negate(Boolean.FALSE));
        assertSame(Boolean.FALSE, BooleanUtils.negate(Boolean.TRUE));
    }

    //-----------------------------------------------------------------------
    @Test
    public void test_isTrue_Boolean() {
        assertEquals(true, BooleanUtils.isTrue(Boolean.TRUE));
        assertEquals(false, BooleanUtils.isTrue(Boolean.FALSE));
        assertEquals(false, BooleanUtils.isTrue((Boolean) null));
    }

    @Test
    public void test_isNotTrue_Boolean() {
        assertEquals(false, BooleanUtils.isNotTrue(Boolean.TRUE));
        assertEquals(true, BooleanUtils.isNotTrue(Boolean.FALSE));
        assertEquals(true, BooleanUtils.isNotTrue((Boolean) null));
    }

    //-----------------------------------------------------------------------
    @Test
    public void test_isFalse_Boolean() {
        assertEquals(false, BooleanUtils.isFalse(Boolean.TRUE));
        assertEquals(true, BooleanUtils.isFalse(Boolean.FALSE));
        assertEquals(false, BooleanUtils.isFalse((Boolean) null));
    }

    @Test
    public void test_isNotFalse_Boolean() {
        assertEquals(true, BooleanUtils.isNotFalse(Boolean.TRUE));
        assertEquals(false, BooleanUtils.isNotFalse(Boolean.FALSE));
        assertEquals(true, BooleanUtils.isNotFalse((Boolean) null));
    }

    //-----------------------------------------------------------------------
    @Test
    public void test_toBoolean_Boolean() {
        assertEquals(true, BooleanUtils.toBoolean(Boolean.TRUE));
        assertEquals(false, BooleanUtils.toBoolean(Boolean.FALSE));
        assertEquals(false, BooleanUtils.toBoolean((Boolean) null));
    }

    @Test
    public void test_toBooleanDefaultIfNull_Boolean_boolean() {
        assertEquals(true, BooleanUtils.toBooleanDefaultIfNull(Boolean.TRUE, true));
        assertEquals(true, BooleanUtils.toBooleanDefaultIfNull(Boolean.TRUE, false));
        assertEquals(false, BooleanUtils.toBooleanDefaultIfNull(Boolean.FALSE, true));
        assertEquals(false, BooleanUtils.toBooleanDefaultIfNull(Boolean.FALSE, false));
        assertEquals(true, BooleanUtils.toBooleanDefaultIfNull((Boolean) null, true));
        assertEquals(false, BooleanUtils.toBooleanDefaultIfNull((Boolean) null, false));
    }

    //-----------------------------------------------------------------------
    //-----------------------------------------------------------------------
    @Test
    public void test_toBoolean_int() {
        assertEquals(true, BooleanUtils.toBoolean(1));
        assertEquals(true, BooleanUtils.toBoolean(-1));
        assertEquals(false, BooleanUtils.toBoolean(0));
    }
    
    @Test
    public void test_toBooleanObject_int() {
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject(1));
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject(-1));
        assertEquals(Boolean.FALSE, BooleanUtils.toBooleanObject(0));
    }
    
    @Test
    public void test_toBooleanObject_Integer() {
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject(Integer.valueOf(1)));
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject(Integer.valueOf(-1)));
        assertEquals(Boolean.FALSE, BooleanUtils.toBooleanObject(Integer.valueOf(0)));
        assertEquals(null, BooleanUtils.toBooleanObject((Integer) null));
    }
    
    //-----------------------------------------------------------------------
    @Test
    public void test_toBoolean_int_int_int() {
        assertEquals(true, BooleanUtils.toBoolean(6, 6, 7));
        assertEquals(false, BooleanUtils.toBoolean(7, 6, 7));
    }

    @Test(expected = IllegalArgumentException.class)
    public void test_toBoolean_int_int_int_noMatch() {
        BooleanUtils.toBoolean(8, 6, 7);
    }
    
    @Test
    public void test_toBoolean_Integer_Integer_Integer() {
        Integer six = Integer.valueOf(6);
        Integer seven = Integer.valueOf(7);

        assertEquals(true, BooleanUtils.toBoolean((Integer) null, null, seven));
        assertEquals(false, BooleanUtils.toBoolean((Integer) null, six, null));

        assertEquals(true, BooleanUtils.toBoolean(Integer.valueOf(6), six, seven));
        assertEquals(false, BooleanUtils.toBoolean(Integer.valueOf(7), six, seven));
    }

    @Test(expected = IllegalArgumentException.class)
    public void test_toBoolean_Integer_Integer_Integer_nullValue() {
        BooleanUtils.toBoolean(null, Integer.valueOf(6), Integer.valueOf(7));
    }

    @Test(expected = IllegalArgumentException.class)
    public void test_toBoolean_Integer_Integer_Integer_noMatch() {
        BooleanUtils.toBoolean(Integer.valueOf(8), Integer.valueOf(6), Integer.valueOf(7));
    }
    
    //-----------------------------------------------------------------------
    @Test
    public void test_toBooleanObject_int_int_int() {
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject(6, 6, 7, 8));
        assertEquals(Boolean.FALSE, BooleanUtils.toBooleanObject(7, 6, 7, 8));
        assertEquals(null, BooleanUtils.toBooleanObject(8, 6, 7, 8));
    }

    @Test(expected = IllegalArgumentException.class)
    public void test_toBooleanObject_int_int_int_noMatch() {
        BooleanUtils.toBooleanObject(9, 6, 7, 8);
    }
    
    @Test
    public void test_toBooleanObject_Integer_Integer_Integer_Integer() {
        Integer six = Integer.valueOf(6);
        Integer seven = Integer.valueOf(7);
        Integer eight = Integer.valueOf(8);

        assertSame(Boolean.TRUE, BooleanUtils.toBooleanObject((Integer) null, null, seven, eight));
        assertSame(Boolean.FALSE, BooleanUtils.toBooleanObject((Integer) null, six, null, eight));
        assertSame(null, BooleanUtils.toBooleanObject((Integer) null, six, seven, null));

        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject(Integer.valueOf(6), six, seven, eight));
        assertEquals(Boolean.FALSE, BooleanUtils.toBooleanObject(Integer.valueOf(7), six, seven, eight));
        assertEquals(null, BooleanUtils.toBooleanObject(Integer.valueOf(8), six, seven, eight));
    }

    @Test(expected = IllegalArgumentException.class)
    public void test_toBooleanObject_Integer_Integer_Integer_Integer_nullValue() {
        BooleanUtils.toBooleanObject(null, Integer.valueOf(6), Integer.valueOf(7), Integer.valueOf(8));
    }

    @Test(expected = IllegalArgumentException.class)
    public void test_toBooleanObject_Integer_Integer_Integer_Integer_noMatch() {
        BooleanUtils.toBooleanObject(Integer.valueOf(9), Integer.valueOf(6), Integer.valueOf(7), Integer.valueOf(8));
    }

    //-----------------------------------------------------------------------
    @Test
    public void test_toInteger_boolean() {
        assertEquals(1, BooleanUtils.toInteger(true));
        assertEquals(0, BooleanUtils.toInteger(false));
    }
    
    @Test
    public void test_toIntegerObject_boolean() {
        assertEquals(Integer.valueOf(1), BooleanUtils.toIntegerObject(true));
        assertEquals(Integer.valueOf(0), BooleanUtils.toIntegerObject(false));
    }
    
    @Test
    public void test_toIntegerObject_Boolean() {
        assertEquals(Integer.valueOf(1), BooleanUtils.toIntegerObject(Boolean.TRUE));
        assertEquals(Integer.valueOf(0), BooleanUtils.toIntegerObject(Boolean.FALSE));
        assertEquals(null, BooleanUtils.toIntegerObject((Boolean) null));
    }
    
    //-----------------------------------------------------------------------
    @Test
    public void test_toInteger_boolean_int_int() {
        assertEquals(6, BooleanUtils.toInteger(true, 6, 7));
        assertEquals(7, BooleanUtils.toInteger(false, 6, 7));
    }
    
    @Test
    public void test_toInteger_Boolean_int_int_int() {
        assertEquals(6, BooleanUtils.toInteger(Boolean.TRUE, 6, 7, 8));
        assertEquals(7, BooleanUtils.toInteger(Boolean.FALSE, 6, 7, 8));
        assertEquals(8, BooleanUtils.toInteger(null, 6, 7, 8));
    }
    
    @Test
    public void test_toIntegerObject_boolean_Integer_Integer() {
        Integer six = Integer.valueOf(6);
        Integer seven = Integer.valueOf(7);
        assertEquals(six, BooleanUtils.toIntegerObject(true, six, seven));
        assertEquals(seven, BooleanUtils.toIntegerObject(false, six, seven));
    }
    
    @Test
    public void test_toIntegerObject_Boolean_Integer_Integer_Integer() {
        Integer six = Integer.valueOf(6);
        Integer seven = Integer.valueOf(7);
        Integer eight = Integer.valueOf(8);
        assertEquals(six, BooleanUtils.toIntegerObject(Boolean.TRUE, six, seven, eight));
        assertEquals(seven, BooleanUtils.toIntegerObject(Boolean.FALSE, six, seven, eight));
        assertEquals(eight, BooleanUtils.toIntegerObject((Boolean) null, six, seven, eight));
        assertEquals(null, BooleanUtils.toIntegerObject((Boolean) null, six, seven, null));
    }
    
    //-----------------------------------------------------------------------
    //-----------------------------------------------------------------------
    @Test
    public void test_toBooleanObject_String() {
        assertEquals(null, BooleanUtils.toBooleanObject((String) null));
        assertEquals(null, BooleanUtils.toBooleanObject(""));
        assertEquals(Boolean.FALSE, BooleanUtils.toBooleanObject("false"));
        assertEquals(Boolean.FALSE, BooleanUtils.toBooleanObject("no"));
        assertEquals(Boolean.FALSE, BooleanUtils.toBooleanObject("off"));
        assertEquals(Boolean.FALSE, BooleanUtils.toBooleanObject("FALSE"));
        assertEquals(Boolean.FALSE, BooleanUtils.toBooleanObject("NO"));
        assertEquals(Boolean.FALSE, BooleanUtils.toBooleanObject("OFF"));
        assertEquals(null, BooleanUtils.toBooleanObject("oof"));
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject("true"));
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject("yes"));
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject("on"));
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject("TRUE"));
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject("ON"));
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject("YES"));
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject("TruE"));
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject("TruE"));

        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject("y"));
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject("Y"));
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject("t"));
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject("T"));
        assertEquals(Boolean.FALSE, BooleanUtils.toBooleanObject("f"));
        assertEquals(Boolean.FALSE, BooleanUtils.toBooleanObject("F"));
        assertEquals(Boolean.FALSE, BooleanUtils.toBooleanObject("n"));
        assertEquals(Boolean.FALSE, BooleanUtils.toBooleanObject("N"));
        assertEquals(null, BooleanUtils.toBooleanObject("z"));

        assertEquals(null, BooleanUtils.toBooleanObject("ab"));
        assertEquals(null, BooleanUtils.toBooleanObject("yoo"));
    }
    
    @Test
    public void test_toBooleanObject_String_String_String_String() {
        assertSame(Boolean.TRUE, BooleanUtils.toBooleanObject((String) null, null, "N", "U"));
        assertSame(Boolean.FALSE, BooleanUtils.toBooleanObject((String) null, "Y", null, "U"));
        assertSame(null, BooleanUtils.toBooleanObject((String) null, "Y", "N", null));

        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject("Y", "Y", "N", "U"));
        assertEquals(Boolean.FALSE, BooleanUtils.toBooleanObject("N", "Y", "N", "U"));
        assertEquals(null, BooleanUtils.toBooleanObject("U", "Y", "N", "U"));
    }

    @Test(expected = IllegalArgumentException.class)
    public void test_toBooleanObject_String_String_String_String_nullValue() {
        BooleanUtils.toBooleanObject((String) null, "Y", "N", "U");
    }

    @Test(expected = IllegalArgumentException.class)
    public void test_toBooleanObject_String_String_String_String_noMatch() {
        BooleanUtils.toBooleanObject("X", "Y", "N", "U");
    }

    //-----------------------------------------------------------------------
    @Test
    public void test_toBoolean_String() {
        assertEquals(false, BooleanUtils.toBoolean((String) null));
        assertEquals(false, BooleanUtils.toBoolean(""));
        assertEquals(false, BooleanUtils.toBoolean("off"));
        assertEquals(false, BooleanUtils.toBoolean("oof"));
        assertEquals(false, BooleanUtils.toBoolean("yep"));
        assertEquals(false, BooleanUtils.toBoolean("trux"));
        assertEquals(false, BooleanUtils.toBoolean("false"));
        assertEquals(false, BooleanUtils.toBoolean("a"));
        assertEquals(true, BooleanUtils.toBoolean("true")); // interned handled differently
        assertEquals(true, BooleanUtils.toBoolean(new StringBuffer("tr").append("ue").toString()));
        assertEquals(true, BooleanUtils.toBoolean("truE"));
        assertEquals(true, BooleanUtils.toBoolean("trUe"));
        assertEquals(true, BooleanUtils.toBoolean("trUE"));
        assertEquals(true, BooleanUtils.toBoolean("tRue"));
        assertEquals(true, BooleanUtils.toBoolean("tRuE"));
        assertEquals(true, BooleanUtils.toBoolean("tRUe"));
        assertEquals(true, BooleanUtils.toBoolean("tRUE"));
        assertEquals(true, BooleanUtils.toBoolean("TRUE"));
        assertEquals(true, BooleanUtils.toBoolean("TRUe"));
        assertEquals(true, BooleanUtils.toBoolean("TRuE"));
        assertEquals(true, BooleanUtils.toBoolean("TRue"));
        assertEquals(true, BooleanUtils.toBoolean("TrUE"));
        assertEquals(true, BooleanUtils.toBoolean("TrUe"));
        assertEquals(true, BooleanUtils.toBoolean("TruE"));
        assertEquals(true, BooleanUtils.toBoolean("True"));
        assertEquals(true, BooleanUtils.toBoolean("on"));
        assertEquals(true, BooleanUtils.toBoolean("oN"));
        assertEquals(true, BooleanUtils.toBoolean("On"));
        assertEquals(true, BooleanUtils.toBoolean("ON"));
        assertEquals(true, BooleanUtils.toBoolean("yes"));
        assertEquals(true, BooleanUtils.toBoolean("yeS"));
        assertEquals(true, BooleanUtils.toBoolean("yEs"));
        assertEquals(true, BooleanUtils.toBoolean("yES"));
        assertEquals(true, BooleanUtils.toBoolean("Yes"));
        assertEquals(true, BooleanUtils.toBoolean("YeS"));
        assertEquals(true, BooleanUtils.toBoolean("YEs"));
        assertEquals(true, BooleanUtils.toBoolean("YES"));
        assertEquals(false, BooleanUtils.toBoolean("yes?"));
        assertEquals(false, BooleanUtils.toBoolean("tru"));

        assertEquals(false, BooleanUtils.toBoolean("no"));
        assertEquals(false, BooleanUtils.toBoolean("off"));
        assertEquals(false, BooleanUtils.toBoolean("yoo"));
    }

    @Test
    public void test_toBoolean_String_String_String() {
        assertEquals(true, BooleanUtils.toBoolean((String) null, null, "N"));
        assertEquals(false, BooleanUtils.toBoolean((String) null, "Y", null));
        assertEquals(true, BooleanUtils.toBoolean("Y", "Y", "N"));
        assertEquals(true, BooleanUtils.toBoolean("Y", new String("Y"), new String("N")));
        assertEquals(false, BooleanUtils.toBoolean("N", "Y", "N"));
        assertEquals(false, BooleanUtils.toBoolean("N", new String("Y"), new String("N")));
        assertEquals(true, BooleanUtils.toBoolean((String) null, null, null));
        assertEquals(true, BooleanUtils.toBoolean("Y", "Y", "Y"));
        assertEquals(true, BooleanUtils.toBoolean("Y", new String("Y"), new String("Y")));
    }

    @Test(expected = IllegalArgumentException.class)
    public void test_toBoolean_String_String_String_nullValue() {
        BooleanUtils.toBoolean(null, "Y", "N");
    }

    @Test(expected = IllegalArgumentException.class)
    public void test_toBoolean_String_String_String_noMatch() {
        BooleanUtils.toBoolean("X", "Y", "N");
    }

    //-----------------------------------------------------------------------
    @Test
    public void test_toStringTrueFalse_Boolean() {
        assertEquals(null, BooleanUtils.toStringTrueFalse((Boolean) null));
        assertEquals("true", BooleanUtils.toStringTrueFalse(Boolean.TRUE));
        assertEquals("false", BooleanUtils.toStringTrueFalse(Boolean.FALSE));
    }
    
    @Test
    public void test_toStringOnOff_Boolean() {
        assertEquals(null, BooleanUtils.toStringOnOff((Boolean) null));
        assertEquals("on", BooleanUtils.toStringOnOff(Boolean.TRUE));
        assertEquals("off", BooleanUtils.toStringOnOff(Boolean.FALSE));
    }
    
    @Test
    public void test_toStringYesNo_Boolean() {
        assertEquals(null, BooleanUtils.toStringYesNo((Boolean) null));
        assertEquals("yes", BooleanUtils.toStringYesNo(Boolean.TRUE));
        assertEquals("no", BooleanUtils.toStringYesNo(Boolean.FALSE));
    }
    
    @Test
    public void test_toString_Boolean_String_String_String() {
        assertEquals("U", BooleanUtils.toString((Boolean) null, "Y", "N", "U"));
        assertEquals("Y", BooleanUtils.toString(Boolean.TRUE, "Y", "N", "U"));
        assertEquals("N", BooleanUtils.toString(Boolean.FALSE, "Y", "N", "U"));
    }
    
    //-----------------------------------------------------------------------
    @Test
    public void test_toStringTrueFalse_boolean() {
        assertEquals("true", BooleanUtils.toStringTrueFalse(true));
        assertEquals("false", BooleanUtils.toStringTrueFalse(false));
    }
    
    @Test
    public void test_toStringOnOff_boolean() {
        assertEquals("on", BooleanUtils.toStringOnOff(true));
        assertEquals("off", BooleanUtils.toStringOnOff(false));
    }
    
    @Test
    public void test_toStringYesNo_boolean() {
        assertEquals("yes", BooleanUtils.toStringYesNo(true));
        assertEquals("no", BooleanUtils.toStringYesNo(false));
    }
    
    @Test
    public void test_toString_boolean_String_String_String() {
        assertEquals("Y", BooleanUtils.toString(true, "Y", "N"));
        assertEquals("N", BooleanUtils.toString(false, "Y", "N"));
    }
    
    //  testXor
    //  -----------------------------------------------------------------------
    @Test(expected = IllegalArgumentException.class)
    public void testXor_primitive_nullInput() {
        BooleanUtils.xor((boolean[]) null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testXor_primitive_emptyInput() {
        BooleanUtils.xor(new boolean[] {});
    }

    @Test
    public void testXor_primitive_validInput_2items() {
        assertTrue(
            "True result for (true, true)",
            ! BooleanUtils.xor(new boolean[] { true, true }));

        assertTrue(
            "True result for (false, false)",
            ! BooleanUtils.xor(new boolean[] { false, false }));

        assertTrue(
            "False result for (true, false)",
            BooleanUtils.xor(new boolean[] { true, false }));

        assertTrue(
            "False result for (false, true)",
            BooleanUtils.xor(new boolean[] { false, true }));
    }

    @Test
    public void testXor_primitive_validInput_3items() {
        assertTrue(
            "False result for (false, false, true)",
            BooleanUtils.xor(new boolean[] { false, false, true }));

        assertTrue(
            "False result for (false, true, false)",
            BooleanUtils.xor(new boolean[] { false, true, false }));

        assertTrue(
            "False result for (true, false, false)",
            BooleanUtils.xor(new boolean[] { true, false, false }));

        assertTrue(
            "True result for (true, true, true)",
            ! BooleanUtils.xor(new boolean[] { true, true, true }));

        assertTrue(
            "True result for (false, false)",
            ! BooleanUtils.xor(new boolean[] { false, false, false }));

        assertTrue(
            "True result for (true, true, false)",
            ! BooleanUtils.xor(new boolean[] { true, true, false }));

        assertTrue(
            "True result for (true, false, true)",
            ! BooleanUtils.xor(new boolean[] { true, false, true }));

        assertTrue(
            "False result for (false, true, true)",
            ! BooleanUtils.xor(new boolean[] { false, true, true }));
    }

    @Test(expected = IllegalArgumentException.class)
    public void testXor_object_nullInput() {
        BooleanUtils.xor((Boolean[]) null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testXor_object_emptyInput() {
        BooleanUtils.xor(new Boolean[] {});
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testXor_object_nullElementInput() {
        BooleanUtils.xor(new Boolean[] {null});
    }

    @Test
    public void testXor_object_validInput_2items() {
        assertTrue(
            "True result for (true, true)",
            ! BooleanUtils
                .xor(new Boolean[] { Boolean.TRUE, Boolean.TRUE })
                .booleanValue());

        assertTrue(
            "True result for (false, false)",
            ! BooleanUtils
                .xor(new Boolean[] { Boolean.FALSE, Boolean.FALSE })
                .booleanValue());

        assertTrue(
            "False result for (true, false)",
            BooleanUtils
                .xor(new Boolean[] { Boolean.TRUE, Boolean.FALSE })
                .booleanValue());

        assertTrue(
            "False result for (false, true)",
            BooleanUtils
                .xor(new Boolean[] { Boolean.FALSE, Boolean.TRUE })
                .booleanValue());
    }

    @Test
    public void testXor_object_validInput_3items() {
        assertTrue(
            "False result for (false, false, true)",
            BooleanUtils
                .xor(
                    new Boolean[] {
                        Boolean.FALSE,
                        Boolean.FALSE,
                        Boolean.TRUE })
                .booleanValue());

        assertTrue(
            "False result for (false, true, false)",
            BooleanUtils
                .xor(
                    new Boolean[] {
                        Boolean.FALSE,
                        Boolean.TRUE,
                        Boolean.FALSE })
                .booleanValue());

        assertTrue(
            "False result for (true, false, false)",
            BooleanUtils
                .xor(
                    new Boolean[] {
                        Boolean.TRUE,
                        Boolean.FALSE,
                        Boolean.FALSE })
                .booleanValue());

        assertTrue(
            "True result for (true, true, true)",
            ! BooleanUtils
                .xor(new Boolean[] { Boolean.TRUE, Boolean.TRUE, Boolean.TRUE })
                .booleanValue());

        assertTrue(
            "True result for (false, false)",
            ! BooleanUtils.xor(
                    new Boolean[] {
                        Boolean.FALSE,
                        Boolean.FALSE,
                        Boolean.FALSE })
                .booleanValue());

        assertTrue(
            "True result for (true, true, false)",
            ! BooleanUtils.xor(
                    new Boolean[] {
                        Boolean.TRUE,
                        Boolean.TRUE,
                        Boolean.FALSE })
                .booleanValue());

        assertTrue(
            "True result for (true, false, true)",
            ! BooleanUtils.xor(
                    new Boolean[] {
                        Boolean.TRUE,
                        Boolean.FALSE,
                        Boolean.TRUE })
                .booleanValue());

        assertTrue(
            "False result for (false, true, true)",
            ! BooleanUtils.xor(
                    new Boolean[] {
                        Boolean.FALSE,
                        Boolean.TRUE,
                        Boolean.TRUE })
                .booleanValue());
    }

    //  testAnd
    //  -----------------------------------------------------------------------
    @Test(expected = IllegalArgumentException.class)
    public void testAnd_primitive_nullInput() {
        BooleanUtils.and((boolean[]) null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testAnd_primitive_emptyInput() {
        BooleanUtils.and(new boolean[] {});
    }
    
    @Test
    public void testAnd_primitive_validInput_2items() {
        assertTrue(
            "False result for (true, true)",
            BooleanUtils.and(new boolean[] { true, true }));
        
        assertTrue(
            "True result for (false, false)",
            ! BooleanUtils.and(new boolean[] { false, false }));
        
        assertTrue(
            "True result for (true, false)",
            ! BooleanUtils.and(new boolean[] { true, false }));
        
        assertTrue(
            "True result for (false, true)",
            ! BooleanUtils.and(new boolean[] { false, true }));
    }
    
    @Test
    public void testAnd_primitive_validInput_3items() {
        assertTrue(
            "True result for (false, false, true)",
            ! BooleanUtils.and(new boolean[] { false, false, true }));
        
        assertTrue(
            "True result for (false, true, false)",
            ! BooleanUtils.and(new boolean[] { false, true, false }));
        
        assertTrue(
            "True result for (true, false, false)",
            ! BooleanUtils.and(new boolean[] { true, false, false }));
        
        assertTrue(
            "False result for (true, true, true)",
            BooleanUtils.and(new boolean[] { true, true, true }));
        
        assertTrue(
            "True result for (false, false)",
            ! BooleanUtils.and(new boolean[] { false, false, false }));
        
        assertTrue(
            "True result for (true, true, false)",
            ! BooleanUtils.and(new boolean[] { true, true, false }));
        
        assertTrue(
            "True result for (true, false, true)",
            ! BooleanUtils.and(new boolean[] { true, false, true }));
        
        assertTrue(
            "True result for (false, true, true)",
            ! BooleanUtils.and(new boolean[] { false, true, true }));
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testAnd_object_nullInput() {
        BooleanUtils.and((Boolean[]) null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testAnd_object_emptyInput() {
        BooleanUtils.and(new Boolean[] {});
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testAnd_object_nullElementInput() {
        BooleanUtils.and(new Boolean[] {null});
    }
    
    @Test
    public void testAnd_object_validInput_2items() {
        assertTrue(
            "False result for (true, true)",
            BooleanUtils
            .and(new Boolean[] { Boolean.TRUE, Boolean.TRUE })
            .booleanValue());
        
        assertTrue(
            "True result for (false, false)",
            ! BooleanUtils
            .and(new Boolean[] { Boolean.FALSE, Boolean.FALSE })
            .booleanValue());
        
        assertTrue(
            "True result for (true, false)",
            ! BooleanUtils
            .and(new Boolean[] { Boolean.TRUE, Boolean.FALSE })
            .booleanValue());
        
        assertTrue(
            "True result for (false, true)",
            ! BooleanUtils
            .and(new Boolean[] { Boolean.FALSE, Boolean.TRUE })
            .booleanValue());
    }
    
    @Test
    public void testAnd_object_validInput_3items() {
        assertTrue(
            "True result for (false, false, true)",
            ! BooleanUtils
            .and(
                new Boolean[] {
                    Boolean.FALSE,
                    Boolean.FALSE,
                    Boolean.TRUE })
                    .booleanValue());
        
        assertTrue(
            "True result for (false, true, false)",
            ! BooleanUtils
            .and(
                new Boolean[] {
                    Boolean.FALSE,
                    Boolean.TRUE,
                    Boolean.FALSE })
                    .booleanValue());
        
        assertTrue(
            "True result for (true, false, false)",
            ! BooleanUtils
            .and(
                new Boolean[] {
                    Boolean.TRUE,
                    Boolean.FALSE,
                    Boolean.FALSE })
                    .booleanValue());
        
        assertTrue(
            "False result for (true, true, true)",
            BooleanUtils
            .and(new Boolean[] { Boolean.TRUE, Boolean.TRUE, Boolean.TRUE })
            .booleanValue());
        
        assertTrue(
            "True result for (false, false)",
            ! BooleanUtils.and(
                new Boolean[] {
                    Boolean.FALSE,
                    Boolean.FALSE,
                    Boolean.FALSE })
                    .booleanValue());
        
        assertTrue(
            "True result for (true, true, false)",
            ! BooleanUtils.and(
                new Boolean[] {
                    Boolean.TRUE,
                    Boolean.TRUE,
                    Boolean.FALSE })
                    .booleanValue());
        
        assertTrue(
            "True result for (true, false, true)",
            ! BooleanUtils.and(
                new Boolean[] {
                    Boolean.TRUE,
                    Boolean.FALSE,
                    Boolean.TRUE })
                    .booleanValue());
        
        assertTrue(
            "True result for (false, true, true)",
            ! BooleanUtils.and(
                new Boolean[] {
                    Boolean.FALSE,
                    Boolean.TRUE,
                    Boolean.TRUE })
                    .booleanValue());
    }
    
    //  testOr
    //  -----------------------------------------------------------------------
    @Test(expected = IllegalArgumentException.class)
    public void testOr_primitive_nullInput() {
        BooleanUtils.or((boolean[]) null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testOr_primitive_emptyInput() {
        BooleanUtils.or(new boolean[] {});
    }
    
    @Test
    public void testOr_primitive_validInput_2items() {
        assertTrue(
            "False result for (true, true)",
            BooleanUtils.or(new boolean[] { true, true }));
        
        assertTrue(
            "True result for (false, false)",
            ! BooleanUtils.or(new boolean[] { false, false }));
        
        assertTrue(
            "False result for (true, false)",
            BooleanUtils.or(new boolean[] { true, false }));
        
        assertTrue(
            "False result for (false, true)",
            BooleanUtils.or(new boolean[] { false, true }));
    }
    
    @Test
    public void testOr_primitive_validInput_3items() {
        assertTrue(
            "False result for (false, false, true)",
            BooleanUtils.or(new boolean[] { false, false, true }));
        
        assertTrue(
            "False result for (false, true, false)",
            BooleanUtils.or(new boolean[] { false, true, false }));
        
        assertTrue(
            "False result for (true, false, false)",
            BooleanUtils.or(new boolean[] { true, false, false }));
        
        assertTrue(
            "False result for (true, true, true)",
            BooleanUtils.or(new boolean[] { true, true, true }));
        
        assertTrue(
            "True result for (false, false)",
            ! BooleanUtils.or(new boolean[] { false, false, false }));
        
        assertTrue(
            "False result for (true, true, false)",
            BooleanUtils.or(new boolean[] { true, true, false }));
        
        assertTrue(
            "False result for (true, false, true)",
            BooleanUtils.or(new boolean[] { true, false, true }));
        
        assertTrue(
            "False result for (false, true, true)",
            BooleanUtils.or(new boolean[] { false, true, true }));
    
    }
    @Test(expected = IllegalArgumentException.class)
    public void testOr_object_nullInput() {
        BooleanUtils.or((Boolean[]) null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testOr_object_emptyInput() {
        BooleanUtils.or(new Boolean[] {});
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testOr_object_nullElementInput() {
        BooleanUtils.or(new Boolean[] {null});
    }
    
    @Test
    public void testOr_object_validInput_2items() {
        assertTrue(
            "False result for (true, true)",
            BooleanUtils
            .or(new Boolean[] { Boolean.TRUE, Boolean.TRUE })
            .booleanValue());
        
        assertTrue(
            "True result for (false, false)",
            ! BooleanUtils
            .or(new Boolean[] { Boolean.FALSE, Boolean.FALSE })
            .booleanValue());
        
        assertTrue(
            "False result for (true, false)",
            BooleanUtils
            .or(new Boolean[] { Boolean.TRUE, Boolean.FALSE })
            .booleanValue());
        
        assertTrue(
            "False result for (false, true)",
            BooleanUtils
            .or(new Boolean[] { Boolean.FALSE, Boolean.TRUE })
            .booleanValue());
    }
    
    @Test
    public void testOr_object_validInput_3items() {
        assertTrue(
            "False result for (false, false, true)",
            BooleanUtils
            .or(
                new Boolean[] {
                    Boolean.FALSE,
                    Boolean.FALSE,
                    Boolean.TRUE })
                    .booleanValue());
        
        assertTrue(
            "False result for (false, true, false)",
            BooleanUtils
            .or(
                new Boolean[] {
                    Boolean.FALSE,
                    Boolean.TRUE,
                    Boolean.FALSE })
                    .booleanValue());
        
        assertTrue(
            "False result for (true, false, false)",
            BooleanUtils
            .or(
                new Boolean[] {
                    Boolean.TRUE,
                    Boolean.FALSE,
                    Boolean.FALSE })
                    .booleanValue());
        
        assertTrue(
            "False result for (true, true, true)",
            BooleanUtils
            .or(new Boolean[] { Boolean.TRUE, Boolean.TRUE, Boolean.TRUE })
            .booleanValue());
        
        assertTrue(
            "True result for (false, false)",
            ! BooleanUtils.or(
                new Boolean[] {
                    Boolean.FALSE,
                    Boolean.FALSE,
                    Boolean.FALSE })
                    .booleanValue());
        
        assertTrue(
            "False result for (true, true, false)",
            BooleanUtils.or(
                new Boolean[] {
                    Boolean.TRUE,
                    Boolean.TRUE,
                    Boolean.FALSE })
                    .booleanValue());
        
        assertTrue(
            "False result for (true, false, true)",
            BooleanUtils.or(
                new Boolean[] {
                    Boolean.TRUE,
                    Boolean.FALSE,
                    Boolean.TRUE })
                    .booleanValue());
        
        assertTrue(
            "False result for (false, true, true)",
            BooleanUtils.or(
                new Boolean[] {
                    Boolean.FALSE,
                    Boolean.TRUE,
                    Boolean.TRUE })
                    .booleanValue());
    }
    
}
