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
package org.apache.commons.lang3.reflect;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

public class TypeLiteralTest extends AbstractLangTest {

    @Test
    public void testBasic() {
        assertTrue(TypeUtils.equals(String.class, new TypeLiteral<String>() {}.value));
        assertTrue(TypeUtils.equals(TypeUtils.parameterize(List.class, String.class),
            new TypeLiteral<List<String>>() {}.value));
    }

    @Test
    public void testTyped() {
        final Typed<String> stringType = new TypeLiteral<String>() {};
        assertTrue(TypeUtils.equals(String.class, stringType.getType()));
        final Typed<List<String>> listOfStringType = new TypeLiteral<List<String>>() {};
        assertTrue(TypeUtils.equals(TypeUtils.parameterize(List.class, String.class), listOfStringType.getType()));
    }

    @Test
    public void testEquals() {
        assertEquals(new TypeLiteral<String>() {}, new TypeLiteral<String>() {});
        assertEquals(new TypeLiteral<List<String>>() {}, new TypeLiteral<List<String>>() {});
        assertNotEquals(new TypeLiteral<String>() {}, new TypeLiteral<List<String>>() {});
    }

    @SuppressWarnings("rawtypes")
    @Test
    public void testRaw() {
        assertThrows(NullPointerException.class, () -> new TypeLiteral() {});
    }
}
