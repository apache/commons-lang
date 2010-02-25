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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import org.junit.Test;

/**
 * Test the Pair class.
 * @author Matt Benson
 */
public class PairTest {

    @Test
    public void testBasic() throws Exception {
        Pair<Integer, String> pair = new Pair<Integer, String>(0, "foo");
        assertEquals(0, pair.left.intValue());
        assertEquals("foo", pair.right);
        Pair<Object, String> pair2 = new Pair<Object, String>(null, "bar");
        assertNull(pair2.left);
        assertEquals("bar", pair2.right);
    }

    @Test
    public void testPairOf() throws Exception {
        Pair<Integer, String> pair = Pair.of(0, "foo");
        assertEquals(0, pair.left.intValue());
        assertEquals("foo", pair.right);
        Pair<Object, String> pair2 = Pair.of(null, "bar");
        assertNull(pair2.left);
        assertEquals("bar", pair2.right);
    }

    @Test
    public void testEquals() throws Exception {
        assertEquals(Pair.of(null, "foo"), Pair.of(null, "foo"));
        assertFalse(Pair.of("foo", 0).equals(Pair.of("foo", null)));
    }

    @Test
    public void testHashCode() throws Exception {
        assertEquals(Pair.of(null, "foo").hashCode(), Pair.of(null, "foo").hashCode());
    }

    @Test
    public void testToString() throws Exception {
        assertEquals("(null,null)", Pair.of(null, null).toString());
        assertEquals("(null,two)", Pair.of(null, "two").toString());
        assertEquals("(one,null)", Pair.of("one", null).toString());
        assertEquals("(one,two)", Pair.of("one", "two").toString());
    }

    @Test
    @SuppressWarnings("unchecked")
    public void testSerialization() throws Exception {
        Pair<Integer, String> origPair = Pair.of(0, "foo");
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ObjectOutputStream out = new ObjectOutputStream(baos);
        out.writeObject(origPair);
        Pair<Integer, String> deserializedPair = (Pair<Integer, String>) new ObjectInputStream(
                new ByteArrayInputStream(baos.toByteArray())).readObject();
        assertEquals(origPair, deserializedPair);
        assertEquals(origPair.hashCode(), deserializedPair.hashCode());
    }
}
