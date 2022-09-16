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

import static org.apache.commons.lang3.ArraySorter.sort;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.Test;

/**
 * Unit tests {@link BooleanUtils}.
 */
public class BooleanUtilsTest extends AbstractLangTest {

    @Test
    public void test_booleanValues() {
        final Boolean[] expected = {Boolean.FALSE, Boolean.TRUE};
        assertArrayEquals(sort(expected), BooleanUtils.booleanValues());
    }

    @Test
    public void test_values() {
        final List<Boolean> expected = Arrays.asList(Boolean.FALSE, Boolean.TRUE);
        Collections.sort(expected);
        assertEquals(expected, BooleanUtils.values());
    }

    @Test
    public void test_forEach() {
        final List<Boolean> list = new ArrayList<>();
        BooleanUtils.forEach(list::add);
        assertEquals(Arrays.asList(Boolean.FALSE, Boolean.TRUE), list);
    }

    @Test
    public void test_isFalse_Boolean() {
        assertFalse(BooleanUtils.isFalse(Boolean.TRUE));
        assertTrue(BooleanUtils.isFalse(Boolean.FALSE));
        assertFalse(BooleanUtils.isFalse(null));
    }

    @Test
    public void test_isNotFalse_Boolean() {
        assertTrue(BooleanUtils.isNotFalse(Boolean.TRUE));
        assertFalse(BooleanUtils.isNotFalse(Boolean.FALSE));
        assertTrue(BooleanUtils.isNotFalse(null));
    }

    @Test
    public void test_isNotTrue_Boolean() {
        assertFalse(BooleanUtils.isNotTrue(Boolean.TRUE));
        assertTrue(BooleanUtils.isNotTrue(Boolean.FALSE));
        assertTrue(BooleanUtils.isNotTrue(null));
    }

    @Test
    public void test_isTrue_Boolean() {
        assertTrue(BooleanUtils.isTrue(Boolean.TRUE));
        assertFalse(BooleanUtils.isTrue(Boolean.FALSE));
        assertFalse(BooleanUtils.isTrue(null));
    }

    @Test
    public void test_negate_Boolean() {
        assertSame(null, BooleanUtils.negate(null));
        assertSame(Boolean.TRUE, BooleanUtils.negate(Boolean.FALSE));
        assertSame(Boolean.FALSE, BooleanUtils.negate(Boolean.TRUE));
    }

    @Test
    public void test_primitiveValues() {
        assertArrayEquals(new boolean[] {false, true}, BooleanUtils.primitiveValues());
    }

    @Test
    public void test_toBoolean_Boolean() {
        assertTrue(BooleanUtils.toBoolean(Boolean.TRUE));
        assertFalse(BooleanUtils.toBoolean(Boolean.FALSE));
        assertFalse(BooleanUtils.toBoolean((Boolean) null));
    }

    @Test
    public void test_toBoolean_int() {
        assertTrue(BooleanUtils.toBoolean(1));
        assertTrue(BooleanUtils.toBoolean(-1));
        assertFalse(BooleanUtils.toBoolean(0));
    }

    @Test
    public void test_toBoolean_int_int_int() {
        assertTrue(BooleanUtils.toBoolean(6, 6, 7));
        assertFalse(BooleanUtils.toBoolean(7, 6, 7));
    }

    @Test
    public void test_toBoolean_int_int_int_noMatch() {
        assertThrows(IllegalArgumentException.class, () -> BooleanUtils.toBoolean(8, 6, 7));
    }

    @Test
    public void test_toBoolean_Integer_Integer_Integer() {
        final Integer six = Integer.valueOf(6);
        final Integer seven = Integer.valueOf(7);

        assertTrue(BooleanUtils.toBoolean(null, null, seven));
        assertFalse(BooleanUtils.toBoolean(null, six, null));

        assertTrue(BooleanUtils.toBoolean(Integer.valueOf(6), six, seven));
        assertFalse(BooleanUtils.toBoolean(Integer.valueOf(7), six, seven));
    }

    @Test
    public void test_toBoolean_Integer_Integer_Integer_noMatch() {
        assertThrows(IllegalArgumentException.class,
                () -> BooleanUtils.toBoolean(Integer.valueOf(8), Integer.valueOf(6), Integer.valueOf(7)));
    }

    @Test
    public void test_toBoolean_Integer_Integer_Integer_nullValue() {
        assertThrows(IllegalArgumentException.class,
                () -> BooleanUtils.toBoolean(null, Integer.valueOf(6), Integer.valueOf(7)));
    }

    @Test
    public void test_toBoolean_String() {
        assertFalse(BooleanUtils.toBoolean((String) null));
        assertFalse(BooleanUtils.toBoolean(""));
        assertFalse(BooleanUtils.toBoolean("off"));
        assertFalse(BooleanUtils.toBoolean("oof"));
        assertFalse(BooleanUtils.toBoolean("yep"));
        assertFalse(BooleanUtils.toBoolean("trux"));
        assertFalse(BooleanUtils.toBoolean("false"));
        assertFalse(BooleanUtils.toBoolean("a"));
        assertTrue(BooleanUtils.toBoolean("true")); // interned handled differently
        assertTrue(BooleanUtils.toBoolean(new StringBuilder("tr").append("ue").toString()));
        assertTrue(BooleanUtils.toBoolean("truE"));
        assertTrue(BooleanUtils.toBoolean("trUe"));
        assertTrue(BooleanUtils.toBoolean("trUE"));
        assertTrue(BooleanUtils.toBoolean("tRue"));
        assertTrue(BooleanUtils.toBoolean("tRuE"));
        assertTrue(BooleanUtils.toBoolean("tRUe"));
        assertTrue(BooleanUtils.toBoolean("tRUE"));
        assertTrue(BooleanUtils.toBoolean("TRUE"));
        assertTrue(BooleanUtils.toBoolean("TRUe"));
        assertTrue(BooleanUtils.toBoolean("TRuE"));
        assertTrue(BooleanUtils.toBoolean("TRue"));
        assertTrue(BooleanUtils.toBoolean("TrUE"));
        assertTrue(BooleanUtils.toBoolean("TrUe"));
        assertTrue(BooleanUtils.toBoolean("TruE"));
        assertTrue(BooleanUtils.toBoolean("True"));
        assertTrue(BooleanUtils.toBoolean("on"));
        assertTrue(BooleanUtils.toBoolean("oN"));
        assertTrue(BooleanUtils.toBoolean("On"));
        assertTrue(BooleanUtils.toBoolean("ON"));
        assertTrue(BooleanUtils.toBoolean("yes"));
        assertTrue(BooleanUtils.toBoolean("yeS"));
        assertTrue(BooleanUtils.toBoolean("yEs"));
        assertTrue(BooleanUtils.toBoolean("yES"));
        assertTrue(BooleanUtils.toBoolean("Yes"));
        assertTrue(BooleanUtils.toBoolean("YeS"));
        assertTrue(BooleanUtils.toBoolean("YEs"));
        assertTrue(BooleanUtils.toBoolean("YES"));
        assertTrue(BooleanUtils.toBoolean("1"));
        assertFalse(BooleanUtils.toBoolean("yes?"));
        assertFalse(BooleanUtils.toBoolean("0"));
        assertFalse(BooleanUtils.toBoolean("tru"));

        assertFalse(BooleanUtils.toBoolean("no"));
        assertFalse(BooleanUtils.toBoolean("off"));
        assertFalse(BooleanUtils.toBoolean("yoo"));
    }

    @Test
    public void test_toBoolean_String_String_String() {
        assertTrue(BooleanUtils.toBoolean(null, null, "N"));
        assertFalse(BooleanUtils.toBoolean(null, "Y", null));
        assertTrue(BooleanUtils.toBoolean("Y", "Y", "N"));
        assertTrue(BooleanUtils.toBoolean("Y", "Y", "N"));
        assertFalse(BooleanUtils.toBoolean("N", "Y", "N"));
        assertFalse(BooleanUtils.toBoolean("N", "Y", "N"));
        assertTrue(BooleanUtils.toBoolean((String) null, null, null));
        assertTrue(BooleanUtils.toBoolean("Y", "Y", "Y"));
        assertTrue(BooleanUtils.toBoolean("Y", "Y", "Y"));
    }

    @Test
    public void test_toBoolean_String_String_String_noMatch() {
        assertThrows(IllegalArgumentException.class, () -> BooleanUtils.toBoolean("X", "Y", "N"));
    }

    @Test
    public void test_toBoolean_String_String_String_nullValue() {
        assertThrows(IllegalArgumentException.class, () -> BooleanUtils.toBoolean(null, "Y", "N"));
    }

    @Test
    public void test_toBooleanDefaultIfNull_Boolean_boolean() {
        assertTrue(BooleanUtils.toBooleanDefaultIfNull(Boolean.TRUE, true));
        assertTrue(BooleanUtils.toBooleanDefaultIfNull(Boolean.TRUE, false));
        assertFalse(BooleanUtils.toBooleanDefaultIfNull(Boolean.FALSE, true));
        assertFalse(BooleanUtils.toBooleanDefaultIfNull(Boolean.FALSE, false));
        assertTrue(BooleanUtils.toBooleanDefaultIfNull(null, true));
        assertFalse(BooleanUtils.toBooleanDefaultIfNull(null, false));
    }

    @Test
    public void test_toBooleanObject_int() {
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject(1));
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject(-1));
        assertEquals(Boolean.FALSE, BooleanUtils.toBooleanObject(0));
    }

    @Test
    public void test_toBooleanObject_int_int_int() {
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject(6, 6, 7, 8));
        assertEquals(Boolean.FALSE, BooleanUtils.toBooleanObject(7, 6, 7, 8));
        assertNull(BooleanUtils.toBooleanObject(8, 6, 7, 8));
    }

    @Test
    public void test_toBooleanObject_int_int_int_noMatch() {
        assertThrows(IllegalArgumentException.class, () -> BooleanUtils.toBooleanObject(9, 6, 7, 8));
    }

    @Test
    public void test_toBooleanObject_Integer() {
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject(Integer.valueOf(1)));
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject(Integer.valueOf(-1)));
        assertEquals(Boolean.FALSE, BooleanUtils.toBooleanObject(Integer.valueOf(0)));
        assertNull(BooleanUtils.toBooleanObject((Integer) null));
    }

    @Test
    public void test_toBooleanObject_Integer_Integer_Integer_Integer() {
        final Integer six = Integer.valueOf(6);
        final Integer seven = Integer.valueOf(7);
        final Integer eight = Integer.valueOf(8);

        assertSame(Boolean.TRUE, BooleanUtils.toBooleanObject(null, null, seven, eight));
        assertSame(Boolean.FALSE, BooleanUtils.toBooleanObject(null, six, null, eight));
        assertSame(null, BooleanUtils.toBooleanObject(null, six, seven, null));

        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject(Integer.valueOf(6), six, seven, eight));
        assertEquals(Boolean.FALSE, BooleanUtils.toBooleanObject(Integer.valueOf(7), six, seven, eight));
        assertNull(BooleanUtils.toBooleanObject(Integer.valueOf(8), six, seven, eight));
    }

    @Test
    public void test_toBooleanObject_Integer_Integer_Integer_Integer_noMatch() {
        assertThrows(IllegalArgumentException.class,
                () -> BooleanUtils.toBooleanObject(Integer.valueOf(9), Integer.valueOf(6), Integer.valueOf(7), Integer.valueOf(8)));
    }

    @Test
    public void test_toBooleanObject_Integer_Integer_Integer_Integer_nullValue() {
        assertThrows(IllegalArgumentException.class,
                () -> BooleanUtils.toBooleanObject(null, Integer.valueOf(6), Integer.valueOf(7), Integer.valueOf(8)));
    }

    @Test
    public void test_toBooleanObject_String() {
        assertNull(BooleanUtils.toBooleanObject((String) null));
        assertNull(BooleanUtils.toBooleanObject(""));
        assertEquals(Boolean.FALSE, BooleanUtils.toBooleanObject("false"));
        assertEquals(Boolean.FALSE, BooleanUtils.toBooleanObject("no"));
        assertEquals(Boolean.FALSE, BooleanUtils.toBooleanObject("off"));
        assertEquals(Boolean.FALSE, BooleanUtils.toBooleanObject("FALSE"));
        assertEquals(Boolean.FALSE, BooleanUtils.toBooleanObject("NO"));
        assertEquals(Boolean.FALSE, BooleanUtils.toBooleanObject("OFF"));
        assertNull(BooleanUtils.toBooleanObject("oof"));
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject("true"));
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject("yes"));
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject("on"));
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject("TRUE"));
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject("ON"));
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject("YES"));
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject("TruE"));
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject("TruE"));

        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject("y")); // yes
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject("Y"));
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject("t")); // true
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject("T"));
        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject("1"));
        assertEquals(Boolean.FALSE, BooleanUtils.toBooleanObject("f")); // false
        assertEquals(Boolean.FALSE, BooleanUtils.toBooleanObject("F"));
        assertEquals(Boolean.FALSE, BooleanUtils.toBooleanObject("n")); // No
        assertEquals(Boolean.FALSE, BooleanUtils.toBooleanObject("N"));
        assertEquals(Boolean.FALSE, BooleanUtils.toBooleanObject("0"));
        assertNull(BooleanUtils.toBooleanObject("z"));

        assertNull(BooleanUtils.toBooleanObject("ab"));
        assertNull(BooleanUtils.toBooleanObject("yoo"));
        assertNull(BooleanUtils.toBooleanObject("true "));
        assertNull(BooleanUtils.toBooleanObject("ono"));
    }

    @Test
    public void test_toBooleanObject_String_String_String_String() {
        assertSame(Boolean.TRUE, BooleanUtils.toBooleanObject(null, null, "N", "U"));
        assertSame(Boolean.FALSE, BooleanUtils.toBooleanObject(null, "Y", null, "U"));
        assertSame(null, BooleanUtils.toBooleanObject(null, "Y", "N", null));

        assertEquals(Boolean.TRUE, BooleanUtils.toBooleanObject("Y", "Y", "N", "U"));
        assertEquals(Boolean.FALSE, BooleanUtils.toBooleanObject("N", "Y", "N", "U"));
        assertNull(BooleanUtils.toBooleanObject("U", "Y", "N", "U"));
    }

    @Test
    public void test_toBooleanObject_String_String_String_String_noMatch() {
        assertThrows(IllegalArgumentException.class, () -> BooleanUtils.toBooleanObject("X", "Y", "N", "U"));
    }

    @Test
    public void test_toBooleanObject_String_String_String_String_nullValue() {
        assertThrows(IllegalArgumentException.class, () -> BooleanUtils.toBooleanObject(null, "Y", "N", "U"));
    }

    @Test
    public void test_toInteger_boolean() {
        assertEquals(1, BooleanUtils.toInteger(true));
        assertEquals(0, BooleanUtils.toInteger(false));
    }

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
    public void test_toIntegerObject_boolean() {
        assertEquals(Integer.valueOf(1), BooleanUtils.toIntegerObject(true));
        assertEquals(Integer.valueOf(0), BooleanUtils.toIntegerObject(false));
    }

    @Test
    public void test_toIntegerObject_Boolean() {
        assertEquals(Integer.valueOf(1), BooleanUtils.toIntegerObject(Boolean.TRUE));
        assertEquals(Integer.valueOf(0), BooleanUtils.toIntegerObject(Boolean.FALSE));
        assertNull(BooleanUtils.toIntegerObject(null));
    }

    @Test
    public void test_toIntegerObject_boolean_Integer_Integer() {
        final Integer six = Integer.valueOf(6);
        final Integer seven = Integer.valueOf(7);
        assertEquals(six, BooleanUtils.toIntegerObject(true, six, seven));
        assertEquals(seven, BooleanUtils.toIntegerObject(false, six, seven));
    }

    @Test
    public void test_toIntegerObject_Boolean_Integer_Integer_Integer() {
        final Integer six = Integer.valueOf(6);
        final Integer seven = Integer.valueOf(7);
        final Integer eight = Integer.valueOf(8);
        assertEquals(six, BooleanUtils.toIntegerObject(Boolean.TRUE, six, seven, eight));
        assertEquals(seven, BooleanUtils.toIntegerObject(Boolean.FALSE, six, seven, eight));
        assertEquals(eight, BooleanUtils.toIntegerObject(null, six, seven, eight));
        assertNull(BooleanUtils.toIntegerObject(null, six, seven, null));
    }

    @Test
    public void test_toString_boolean_String_String_String() {
        assertEquals("Y", BooleanUtils.toString(true, "Y", "N"));
        assertEquals("N", BooleanUtils.toString(false, "Y", "N"));
    }

    @Test
    public void test_toString_Boolean_String_String_String() {
        assertEquals("U", BooleanUtils.toString(null, "Y", "N", "U"));
        assertEquals("Y", BooleanUtils.toString(Boolean.TRUE, "Y", "N", "U"));
        assertEquals("N", BooleanUtils.toString(Boolean.FALSE, "Y", "N", "U"));
    }

    @Test
    public void test_toStringOnOff_boolean() {
        assertEquals("on", BooleanUtils.toStringOnOff(true));
        assertEquals("off", BooleanUtils.toStringOnOff(false));
    }

    @Test
    public void test_toStringOnOff_Boolean() {
        assertNull(BooleanUtils.toStringOnOff(null));
        assertEquals("on", BooleanUtils.toStringOnOff(Boolean.TRUE));
        assertEquals("off", BooleanUtils.toStringOnOff(Boolean.FALSE));
    }

    @Test
    public void test_toStringTrueFalse_boolean() {
        assertEquals("true", BooleanUtils.toStringTrueFalse(true));
        assertEquals("false", BooleanUtils.toStringTrueFalse(false));
    }

    @Test
    public void test_toStringTrueFalse_Boolean() {
        assertNull(BooleanUtils.toStringTrueFalse(null));
        assertEquals("true", BooleanUtils.toStringTrueFalse(Boolean.TRUE));
        assertEquals("false", BooleanUtils.toStringTrueFalse(Boolean.FALSE));
    }

    @Test
    public void test_toStringYesNo_boolean() {
        assertEquals("yes", BooleanUtils.toStringYesNo(true));
        assertEquals("no", BooleanUtils.toStringYesNo(false));
    }

    @Test
    public void test_toStringYesNo_Boolean() {
        assertNull(BooleanUtils.toStringYesNo(null));
        assertEquals("yes", BooleanUtils.toStringYesNo(Boolean.TRUE));
        assertEquals("no", BooleanUtils.toStringYesNo(Boolean.FALSE));
    }

    @Test
    public void testAnd_object_emptyInput() {
        assertThrows(IllegalArgumentException.class, () -> BooleanUtils.and(new Boolean[] {}));
    }

    @Test
    public void testAnd_object_nullElementInput() {
        assertEquals(Boolean.FALSE, BooleanUtils.and(new Boolean[] {null}));
    }

    @Test
    public void testAnd_object_nullInput() {
        assertThrows(NullPointerException.class, () -> BooleanUtils.and((Boolean[]) null));
    }

    @Test
    public void testAnd_object_validInput_2items() {
        assertTrue(
                BooleanUtils
                    .and(new Boolean[] { Boolean.TRUE, Boolean.TRUE })
                    .booleanValue(),
                "False result for (true, true)");

        assertFalse(
                BooleanUtils
                    .and(new Boolean[] { Boolean.FALSE, Boolean.FALSE })
                    .booleanValue(),
                "True result for (false, false)");

        assertFalse(
                BooleanUtils
                    .and(new Boolean[] { Boolean.TRUE, Boolean.FALSE })
                    .booleanValue(),
                "True result for (true, false)");

        assertFalse(
                BooleanUtils
                    .and(new Boolean[] { Boolean.FALSE, Boolean.TRUE })
                    .booleanValue(),
                "True result for (false, true)");
    }

    @Test
    public void testAnd_object_validInput_3items() {
        assertFalse(
                BooleanUtils
                    .and(
                        new Boolean[] {
                            Boolean.FALSE,
                            Boolean.FALSE,
                            Boolean.TRUE })
                            .booleanValue(),
                "True result for (false, false, true)");

        assertFalse(
                BooleanUtils
                    .and(
                        new Boolean[] {
                            Boolean.FALSE,
                            Boolean.TRUE,
                            Boolean.FALSE })
                            .booleanValue(),
                "True result for (false, true, false)");

        assertFalse(
                BooleanUtils
                    .and(
                        new Boolean[] {
                            Boolean.TRUE,
                            Boolean.FALSE,
                            Boolean.FALSE })
                            .booleanValue(),
                "True result for (true, false, false)");

        assertTrue(
                BooleanUtils
                    .and(new Boolean[] { Boolean.TRUE, Boolean.TRUE, Boolean.TRUE })
                    .booleanValue(),
                "False result for (true, true, true)");

        assertFalse(
                BooleanUtils.and(
                        new Boolean[] {
                            Boolean.FALSE,
                            Boolean.FALSE,
                            Boolean.FALSE })
                            .booleanValue(),
                "True result for (false, false)");

        assertFalse(
                BooleanUtils.and(
                        new Boolean[] {
                            Boolean.TRUE,
                            Boolean.TRUE,
                            Boolean.FALSE })
                            .booleanValue(),
                "True result for (true, true, false)");

        assertFalse(
                BooleanUtils.and(
                        new Boolean[] {
                            Boolean.TRUE,
                            Boolean.FALSE,
                            Boolean.TRUE })
                            .booleanValue(),
                "True result for (true, false, true)");

        assertFalse(
                BooleanUtils.and(
                        new Boolean[] {
                            Boolean.FALSE,
                            Boolean.TRUE,
                            Boolean.TRUE })
                            .booleanValue(),
                "True result for (false, true, true)");
    }

    @Test
    public void testAnd_primitive_emptyInput() {
        assertThrows(IllegalArgumentException.class, () -> BooleanUtils.and(new boolean[] {}));
    }

    @Test
    public void testAnd_primitive_nullInput() {
        assertThrows(NullPointerException.class, () -> BooleanUtils.and((boolean[]) null));
    }

    @Test
    public void testAnd_primitive_validInput_2items() {
        assertTrue(
                BooleanUtils.and(new boolean[] { true, true }),
                "False result for (true, true)");

        assertFalse(
                BooleanUtils.and(new boolean[] { false, false }),
                "True result for (false, false)");

        assertFalse(
                BooleanUtils.and(new boolean[] { true, false }),
                "True result for (true, false)");

        assertFalse(
                BooleanUtils.and(new boolean[] { false, true }),
                "True result for (false, true)");
    }

    @Test
    public void testAnd_primitive_validInput_3items() {
        assertFalse(
                BooleanUtils.and(new boolean[] { false, false, true }),
                "True result for (false, false, true)");

        assertFalse(
                BooleanUtils.and(new boolean[] { false, true, false }),
                "True result for (false, true, false)");

        assertFalse(
                BooleanUtils.and(new boolean[] { true, false, false }),
                "True result for (true, false, false)");

        assertTrue(
                BooleanUtils.and(new boolean[] { true, true, true }),
                "False result for (true, true, true)");

        assertFalse(
                BooleanUtils.and(new boolean[] { false, false, false }),
                "True result for (false, false)");

        assertFalse(
                BooleanUtils.and(new boolean[] { true, true, false }),
                "True result for (true, true, false)");

        assertFalse(
                BooleanUtils.and(new boolean[] { true, false, true }),
                "True result for (true, false, true)");

        assertFalse(
                BooleanUtils.and(new boolean[] { false, true, true }),
                "True result for (false, true, true)");
    }

    @Test
    public void testCompare() {
        assertTrue(BooleanUtils.compare(true, false) > 0);
        assertEquals(0, BooleanUtils.compare(true, true));
        assertEquals(0, BooleanUtils.compare(false, false));
        assertTrue(BooleanUtils.compare(false, true) < 0);
    }

    @Test
    public void testConstructor() {
        assertNotNull(new BooleanUtils());
        final Constructor<?>[] cons = BooleanUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertTrue(Modifier.isPublic(cons[0].getModifiers()));
        assertTrue(Modifier.isPublic(BooleanUtils.class.getModifiers()));
        assertFalse(Modifier.isFinal(BooleanUtils.class.getModifiers()));
    }

    @Test
    public void testOneHot_object_emptyInput() {
        assertThrows(IllegalArgumentException.class, () -> BooleanUtils.oneHot(new Boolean[] {}));
    }

    @Test
    public void testOneHot_object_nullElementInput() {
        assertEquals(Boolean.FALSE, BooleanUtils.oneHot(new Boolean[] {null}));
    }

    @Test
    public void testOneHot_object_nullInput() {
        assertThrows(NullPointerException.class, () -> BooleanUtils.oneHot((Boolean[]) null));
    }

    @Test
    public void testOneHot_object_validInput_1item() {
        assertTrue(BooleanUtils.oneHot(new Boolean[]{Boolean.TRUE}), "true");

        assertFalse(BooleanUtils.oneHot(new Boolean[]{Boolean.FALSE}), "false");

        assertFalse(BooleanUtils.oneHot(new Boolean[]{null}), "false");
    }

    @Test
    public void testOneHot_object_validInput_2items() {
        assertFalse(BooleanUtils.oneHot(new Boolean[]{true, true}), "both true");

        assertFalse(BooleanUtils.oneHot(new Boolean[]{false, false}), "both false");

        assertTrue(BooleanUtils.oneHot(new Boolean[]{true, false}), "first true");

        assertTrue(BooleanUtils.oneHot(new Boolean[]{false, true}), "last true");
    }

    @Test
    public void testOneHot_object_validInput_2ItemsNullsTreatedAsFalse() {
        assertFalse(BooleanUtils.oneHot(null, null), "both null");

        assertTrue(BooleanUtils.oneHot(true, null), "first true");

        assertTrue(BooleanUtils.oneHot(null, true), "last true");
    }

    @Test
    public void testOneHot_object_validInput_3items() {
        // none true
        assertFalse(BooleanUtils.oneHot(new Boolean[]{false, false, false}), "all false");

        // one true
        assertTrue(BooleanUtils.oneHot(new Boolean[]{true, false, false}), "first true");

        assertTrue(BooleanUtils.oneHot(new Boolean[]{false, true, false}), "middle true");

        assertTrue(BooleanUtils.oneHot(new Boolean[]{false, false, true}), "last true");

        // two true
        assertFalse(BooleanUtils.oneHot(new Boolean[]{false, true, true}), "first false");

        assertFalse(BooleanUtils.oneHot(new Boolean[]{true, false, true}), "middle false");

        assertFalse(BooleanUtils.oneHot(new Boolean[]{true, true, false}), "last false");

        // three true
        assertFalse(BooleanUtils.oneHot(new Boolean[]{true, true, true}), "all true");
    }

    @Test
    public void testOneHot_primitive_emptyInput() {
        assertThrows(IllegalArgumentException.class, () -> BooleanUtils.oneHot(new boolean[] {}));
    }

    @Test
    public void testOneHot_primitive_nullInput() {
        assertThrows(NullPointerException.class, () -> BooleanUtils.oneHot((boolean[]) null));
    }

    @Test
    public void testOneHot_primitive_validInput_1item() {
        assertTrue(BooleanUtils.oneHot(new boolean[]{true}), "true");

        assertFalse(BooleanUtils.oneHot(new boolean[]{false}), "false");
    }

    @Test
    public void testOneHot_primitive_validInput_2items() {
        assertFalse(BooleanUtils.oneHot(new boolean[]{true, true}), "both true");

        assertFalse(BooleanUtils.oneHot(new boolean[]{false, false}), "both false");

        assertTrue(BooleanUtils.oneHot(new boolean[]{true, false}), "first true");

        assertTrue(BooleanUtils.oneHot(new boolean[]{false, true}), "last true");
    }

    @Test
    public void testOneHot_primitive_validInput_3items() {
        // none true
        assertFalse(BooleanUtils.oneHot(new boolean[]{false, false, false}), "all false");

        // one true
        assertTrue(BooleanUtils.oneHot(new boolean[]{true, false, false}), "first true");

        assertTrue(BooleanUtils.oneHot(new boolean[]{false, true, false}), "middle true");

        assertTrue(BooleanUtils.oneHot(new boolean[]{false, false, true}), "last true");

        // two true
        assertFalse(BooleanUtils.oneHot(new boolean[]{false, true, true}), "first false");

        assertFalse(BooleanUtils.oneHot(new boolean[]{true, false, true}), "middle false");

        assertFalse(BooleanUtils.oneHot(new boolean[]{true, true, false}), "last false");

        // three true
        assertFalse(BooleanUtils.oneHot(new boolean[]{true, true, true}), "all true");
    }

    @Test
    public void testOr_object_emptyInput() {
        assertThrows(IllegalArgumentException.class, () -> BooleanUtils.or(new Boolean[] {}));
    }

    @Test
    public void testOr_object_nullElementInput() {
        assertEquals(Boolean.FALSE, BooleanUtils.or(new Boolean[] {null}));
    }

    @Test
    public void testOr_object_nullInput() {
        assertThrows(NullPointerException.class, () -> BooleanUtils.or((Boolean[]) null));
    }

    @Test
    public void testOr_object_validInput_2items() {
        assertTrue(
                BooleanUtils
                    .or(new Boolean[] { Boolean.TRUE, Boolean.TRUE })
                    .booleanValue(),
                "False result for (true, true)");

        assertFalse(
                BooleanUtils
                    .or(new Boolean[] { Boolean.FALSE, Boolean.FALSE })
                    .booleanValue(),
                "True result for (false, false)");

        assertTrue(
                BooleanUtils
                    .or(new Boolean[] { Boolean.TRUE, Boolean.FALSE })
                    .booleanValue(),
                "False result for (true, false)");

        assertTrue(
                BooleanUtils
                    .or(new Boolean[] { Boolean.FALSE, Boolean.TRUE })
                    .booleanValue(),
                "False result for (false, true)");
    }

    @Test
    public void testOr_object_validInput_3items() {
        assertTrue(
                BooleanUtils
                    .or(
                        new Boolean[] {
                            Boolean.FALSE,
                            Boolean.FALSE,
                            Boolean.TRUE })
                            .booleanValue(),
                "False result for (false, false, true)");

        assertTrue(
                BooleanUtils
                    .or(
                        new Boolean[] {
                            Boolean.FALSE,
                            Boolean.TRUE,
                            Boolean.FALSE })
                            .booleanValue(),
                "False result for (false, true, false)");

        assertTrue(
                BooleanUtils
                    .or(
                        new Boolean[] {
                            Boolean.TRUE,
                            Boolean.FALSE,
                            Boolean.FALSE })
                            .booleanValue(),
                "False result for (true, false, false)");

        assertTrue(
                BooleanUtils
                    .or(new Boolean[] { Boolean.TRUE, Boolean.TRUE, Boolean.TRUE })
                    .booleanValue(),
                "False result for (true, true, true)");

        assertFalse(
                BooleanUtils.or(
                        new Boolean[] {
                            Boolean.FALSE,
                            Boolean.FALSE,
                            Boolean.FALSE })
                            .booleanValue(),
                "True result for (false, false)");

        assertTrue(
                BooleanUtils.or(
                        new Boolean[] {
                            Boolean.TRUE,
                            Boolean.TRUE,
                            Boolean.FALSE })
                            .booleanValue(),
                "False result for (true, true, false)");

        assertTrue(
                BooleanUtils.or(
                        new Boolean[] {
                            Boolean.TRUE,
                            Boolean.FALSE,
                            Boolean.TRUE })
                            .booleanValue(),
                "False result for (true, false, true)");

        assertTrue(
                BooleanUtils.or(
                        new Boolean[] {
                            Boolean.FALSE,
                            Boolean.TRUE,
                            Boolean.TRUE })
                            .booleanValue(),
                "False result for (false, true, true)");
    }

    @Test
    public void testOr_primitive_emptyInput() {
        assertThrows(IllegalArgumentException.class, () -> BooleanUtils.or(new boolean[] {}));
    }

    @Test
    public void testOr_primitive_nullInput() {
        assertThrows(NullPointerException.class, () -> BooleanUtils.or((boolean[]) null));
    }

    @Test
    public void testOr_primitive_validInput_2items() {
        assertTrue(
                BooleanUtils.or(new boolean[] { true, true }),
                "False result for (true, true)");

        assertFalse(
                BooleanUtils.or(new boolean[] { false, false }),
                "True result for (false, false)");

        assertTrue(
                BooleanUtils.or(new boolean[] { true, false }),
                "False result for (true, false)");

        assertTrue(
                BooleanUtils.or(new boolean[] { false, true }),
                "False result for (false, true)");
    }

    @Test
    public void testOr_primitive_validInput_3items() {
        assertTrue(
                BooleanUtils.or(new boolean[] { false, false, true }),
                "False result for (false, false, true)");

        assertTrue(
                BooleanUtils.or(new boolean[] { false, true, false }),
                "False result for (false, true, false)");

        assertTrue(
                BooleanUtils.or(new boolean[] { true, false, false }),
                "False result for (true, false, false)");

        assertTrue(
                BooleanUtils.or(new boolean[] { true, true, true }),
                "False result for (true, true, true)");

        assertFalse(
                BooleanUtils.or(new boolean[] { false, false, false }),
                "True result for (false, false)");

        assertTrue(
                BooleanUtils.or(new boolean[] { true, true, false }),
                "False result for (true, true, false)");

        assertTrue(
                BooleanUtils.or(new boolean[] { true, false, true }),
                "False result for (true, false, true)");

        assertTrue(
                BooleanUtils.or(new boolean[] { false, true, true }),
                "False result for (false, true, true)");
    }

    @Test
    public void testXor_object_emptyInput() {
        assertThrows(IllegalArgumentException.class, () -> BooleanUtils.xor(new Boolean[] {}));
    }

    @Test
    public void testXor_object_nullElementInput() {
        assertEquals(Boolean.FALSE, BooleanUtils.xor(new Boolean[] {null}));
    }

    @Test
    public void testXor_object_nullInput() {
        assertThrows(NullPointerException.class, () -> BooleanUtils.xor((Boolean[]) null));
    }

    @Test
    public void testXor_object_validInput_1items() {
        assertEquals(
                true,
                BooleanUtils.xor(new Boolean[] { Boolean.TRUE }).booleanValue(),
                "true");

        assertEquals(
                false,
                BooleanUtils.xor(new Boolean[] { Boolean.FALSE }).booleanValue(),
                "false");
    }

    @Test
    public void testXor_object_validInput_2items() {
        assertEquals(
                false ^ false,
                BooleanUtils.xor(new Boolean[] { Boolean.FALSE, Boolean.FALSE }).booleanValue(),
                "false ^ false");

        assertEquals(
                false ^ true,
                BooleanUtils.xor(new Boolean[] { Boolean.FALSE, Boolean.TRUE }).booleanValue(),
                "false ^ true");

        assertEquals(
                true ^ false,
                BooleanUtils.xor(new Boolean[] { Boolean.TRUE, Boolean.FALSE }).booleanValue(),
                "true ^ false");

        assertEquals(
                true ^ true,
                BooleanUtils.xor(new Boolean[] { Boolean.TRUE, Boolean.TRUE }).booleanValue(),
                "true ^ true");
    }

    @Test
    public void testXor_object_validInput_3items() {
        assertEquals(
                false ^ false ^ false,
                BooleanUtils.xor(
                                new Boolean[] {
                                        Boolean.FALSE,
                                        Boolean.FALSE,
                                        Boolean.FALSE })
                                .booleanValue(),
                "false ^ false ^ false");

        assertEquals(
                false ^ false ^ true,
                BooleanUtils
                        .xor(
                            new Boolean[] {
                                Boolean.FALSE,
                                Boolean.FALSE,
                                Boolean.TRUE })
                        .booleanValue(),
                "false ^ false ^ true");

        assertEquals(
                false ^ true ^ false,
                BooleanUtils
                        .xor(
                            new Boolean[] {
                                Boolean.FALSE,
                                Boolean.TRUE,
                                Boolean.FALSE })
                        .booleanValue(),
                "false ^ true ^ false");

        assertEquals(
                true ^ false ^ false,
                BooleanUtils
                        .xor(
                            new Boolean[] {
                                Boolean.TRUE,
                                Boolean.FALSE,
                                Boolean.FALSE })
                        .booleanValue(),
                "true ^ false ^ false");

        assertEquals(
                true ^ false ^ true,
                BooleanUtils.xor(
                                new Boolean[] {
                                        Boolean.TRUE,
                                        Boolean.FALSE,
                                        Boolean.TRUE })
                                .booleanValue(),
                "true ^ false ^ true");

        assertEquals(
                true ^ true ^ false,
                BooleanUtils.xor(
                            new Boolean[] {
                                Boolean.TRUE,
                                Boolean.TRUE,
                                Boolean.FALSE })
                        .booleanValue(),
                "true ^ true ^ false");

        assertEquals(
                false ^ true ^ true,
                BooleanUtils.xor(
                            new Boolean[] {
                                Boolean.FALSE,
                                Boolean.TRUE,
                                Boolean.TRUE })
                        .booleanValue(),
                "false ^ true ^ true");

        assertEquals(
                true ^ true ^ true,
                BooleanUtils.xor(
                        new Boolean[] {
                                Boolean.TRUE,
                                Boolean.TRUE,
                                Boolean.TRUE })
                        .booleanValue(),
                "true ^ true ^ true");
    }

    @Test
    public void testXor_primitive_emptyInput() {
        assertThrows(IllegalArgumentException.class, () -> BooleanUtils.xor(new boolean[] {}));
    }

    @Test
    public void testXor_primitive_nullInput() {
        assertThrows(NullPointerException.class, () -> BooleanUtils.xor((boolean[]) null));
    }

    @Test
    public void testXor_primitive_validInput_1items() {
        assertEquals(
                true,
                BooleanUtils.xor(new boolean[] { true }),
                "true");

        assertEquals(
                false,
                BooleanUtils.xor(new boolean[] { false }),
                "false");
    }

    @Test
    public void testXor_primitive_validInput_2items() {
        assertEquals(
                true ^ true,
                BooleanUtils.xor(new boolean[] { true, true }),
                "true ^ true");

        assertEquals(
                false ^ false,
                BooleanUtils.xor(new boolean[] { false, false }),
                "false ^ false");

        assertEquals(
                true ^ false,
                BooleanUtils.xor(new boolean[] { true, false }),
                "true ^ false");

        assertEquals(
                false ^ true,
                BooleanUtils.xor(new boolean[] { false, true }),
                "false ^ true");
    }

    @Test
    public void testXor_primitive_validInput_3items() {
        assertEquals(
                false ^ false ^ false,
                BooleanUtils.xor(new boolean[] { false, false, false }),
                "false ^ false ^ false");

        assertEquals(
                false ^ false ^ true,
                BooleanUtils.xor(new boolean[] { false, false, true }),
                "false ^ false ^ true");

        assertEquals(
                false ^ true ^ false,
                BooleanUtils.xor(new boolean[] { false, true, false }),
                "false ^ true ^ false");

        assertEquals(
                false ^ true ^ true,
                BooleanUtils.xor(new boolean[] { false, true, true }),
                "false ^ true ^ true");

        assertEquals(
                true ^ false ^ false,
                BooleanUtils.xor(new boolean[] { true, false, false }),
                "true ^ false ^ false");

        assertEquals(
                true ^ false ^ true,
                BooleanUtils.xor(new boolean[] { true, false, true }),
                "true ^ false ^ true");

        assertEquals(
                true ^ true ^ false,
                BooleanUtils.xor(new boolean[] { true, true, false }),
                "true ^ true ^ false");

        assertEquals(
                true ^ true ^ true,
                BooleanUtils.xor(new boolean[] { true, true, true }),
                "true ^ true ^ true");
    }
}
