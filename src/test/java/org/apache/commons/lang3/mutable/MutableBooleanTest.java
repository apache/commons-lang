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

package org.apache.commons.lang3.mutable;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * JUnit tests.
 *
 * @since 2.2
 * @see MutableBoolean
 */
public class MutableBooleanTest extends AbstractLangTest {

    @Test
    public void testCompareTo() {
        final MutableBoolean mutBool = new MutableBoolean(false);

        assertEquals(0, mutBool.compareTo(new MutableBoolean(false)));
        assertEquals(-1, mutBool.compareTo(new MutableBoolean(true)));
        mutBool.setValue(true);
        assertEquals(+1, mutBool.compareTo(new MutableBoolean(false)));
        assertEquals(0, mutBool.compareTo(new MutableBoolean(true)));
    }

    @Test
    public void testCompareToNull() {
        final MutableBoolean mutBool = new MutableBoolean(false);
        assertThrows(NullPointerException.class, () -> mutBool.compareTo(null));
    }

    @Test
    public void testConstructorNull() {
        assertThrows(NullPointerException.class, () -> new MutableBoolean(null));
    }

    @Test
    public void testConstructors() {
        assertFalse(new MutableBoolean().booleanValue());

        assertTrue(new MutableBoolean(true).booleanValue());
        assertFalse(new MutableBoolean(false).booleanValue());

        assertTrue(new MutableBoolean(Boolean.TRUE).booleanValue());
        assertFalse(new MutableBoolean(Boolean.FALSE).booleanValue());

    }

    @Test
    public void testEquals() {
        final MutableBoolean mutBoolA = new MutableBoolean(false);
        final MutableBoolean mutBoolB = new MutableBoolean(false);
        final MutableBoolean mutBoolC = new MutableBoolean(true);

        assertEquals(mutBoolA, mutBoolA);
        assertEquals(mutBoolA, mutBoolB);
        assertEquals(mutBoolB, mutBoolA);
        assertEquals(mutBoolB, mutBoolB);
        assertNotEquals(mutBoolA, mutBoolC);
        assertNotEquals(mutBoolB, mutBoolC);
        assertEquals(mutBoolC, mutBoolC);
        assertNotEquals(null, mutBoolA);
        assertNotEquals(mutBoolA, Boolean.FALSE);
        assertNotEquals("false", mutBoolA);
    }

    @Test
    public void testGetSet() {
        assertFalse(new MutableBoolean().booleanValue());
        assertEquals(Boolean.FALSE, new MutableBoolean().getValue());

        final MutableBoolean mutBool = new MutableBoolean(false);
        assertEquals(Boolean.FALSE, mutBool.toBoolean());
        assertFalse(mutBool.booleanValue());
        assertTrue(mutBool.isFalse());
        assertFalse(mutBool.isTrue());

        mutBool.setValue(Boolean.TRUE);
        assertEquals(Boolean.TRUE, mutBool.toBoolean());
        assertTrue(mutBool.booleanValue());
        assertFalse(mutBool.isFalse());
        assertTrue(mutBool.isTrue());

        mutBool.setValue(false);
        assertFalse(mutBool.booleanValue());

        mutBool.setValue(true);
        assertTrue(mutBool.booleanValue());

        mutBool.setFalse();
        assertFalse(mutBool.booleanValue());

        mutBool.setTrue();
        assertTrue(mutBool.booleanValue());

    }

    @Test
    public void testHashCode() {
        final MutableBoolean mutBoolA = new MutableBoolean(false);
        final MutableBoolean mutBoolB = new MutableBoolean(false);
        final MutableBoolean mutBoolC = new MutableBoolean(true);

        assertEquals(mutBoolA.hashCode(), mutBoolA.hashCode());
        assertEquals(mutBoolA.hashCode(), mutBoolB.hashCode());
        assertNotEquals(mutBoolA.hashCode(), mutBoolC.hashCode());
        assertEquals(mutBoolA.hashCode(), Boolean.FALSE.hashCode());
        assertEquals(mutBoolC.hashCode(), Boolean.TRUE.hashCode());
    }

    @Test
    public void testSetNull() {
        final MutableBoolean mutBool = new MutableBoolean(false);
        assertThrows(NullPointerException.class, () -> mutBool.setValue(null));
    }

    @Test
    public void testToString() {
        assertEquals(Boolean.FALSE.toString(), new MutableBoolean(false).toString());
        assertEquals(Boolean.TRUE.toString(), new MutableBoolean(true).toString());
    }

}
