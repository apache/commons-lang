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
package org.apache.commons.lang3.tuple;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import org.junit.Test;

/**
 * Test the MutablePair class.
 * @version $Id$
 */
public class MutablePairTest {

    @Test
    public void testBasic() throws Exception {
        MutablePair<Integer, String> pair = new MutablePair<Integer, String>(0, "foo");
        assertEquals(0, pair.getLeft().intValue());
        assertEquals("foo", pair.getRight());
        MutablePair<Object, String> pair2 = new MutablePair<Object, String>(null, "bar");
        assertNull(pair2.getLeft());
        assertEquals("bar", pair2.getRight());
    }

    @Test
    public void testDefault() throws Exception {
        MutablePair<Integer, String> pair = new MutablePair<Integer, String>();
        assertNull(pair.getLeft());
        assertNull(pair.getRight());
    }
    
    @Test
    public void testMutate() throws Exception {
        MutablePair<Integer, String> pair = new MutablePair<Integer, String>(0, "foo");
        pair.setLeft(42);
        pair.setRight("bar");
        assertEquals(42, pair.getLeft().intValue());
        assertEquals("bar", pair.getRight());
    }

    @Test
    public void testPairOf() throws Exception {
        MutablePair<Integer, String> pair = MutablePair.of(0, "foo");
        assertEquals(0, pair.getLeft().intValue());
        assertEquals("foo", pair.getRight());
        MutablePair<Object, String> pair2 = MutablePair.of(null, "bar");
        assertNull(pair2.getLeft());
        assertEquals("bar", pair2.getRight());
    }

    @Test
    public void testEquals() throws Exception {
        assertEquals(MutablePair.of(null, "foo"), MutablePair.of(null, "foo"));
        assertFalse(MutablePair.of("foo", 0).equals(MutablePair.of("foo", null)));
        assertFalse(MutablePair.of("foo", "bar").equals(MutablePair.of("xyz", "bar")));

        MutablePair<String, String> p = MutablePair.of("foo", "bar");
        assertTrue(p.equals(p));
        assertFalse(p.equals(new Object()));
    }

    @Test
    public void testHashCode() throws Exception {
        assertEquals(MutablePair.of(null, "foo").hashCode(), MutablePair.of(null, "foo").hashCode());
    }

    @Test
    public void testToString() throws Exception {
        assertEquals("(null,null)", MutablePair.of(null, null).toString());
        assertEquals("(null,two)", MutablePair.of(null, "two").toString());
        assertEquals("(one,null)", MutablePair.of("one", null).toString());
        assertEquals("(one,two)", MutablePair.of("one", "two").toString());
    }

    @Test
    @SuppressWarnings("unchecked")
    public void testSerialization() throws Exception {
        MutablePair<Integer, String> origPair = MutablePair.of(0, "foo");
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ObjectOutputStream out = new ObjectOutputStream(baos);
        out.writeObject(origPair);
        MutablePair<Integer, String> deserializedPair = (MutablePair<Integer, String>) new ObjectInputStream(
                new ByteArrayInputStream(baos.toByteArray())).readObject();
        assertEquals(origPair, deserializedPair);
        assertEquals(origPair.hashCode(), deserializedPair.hashCode());
    }
}
