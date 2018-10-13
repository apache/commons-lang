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
package org.apache.commons.lang3.builder;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;

import org.apache.commons.lang3.builder.ToStringStyleTest.Person;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * Unit tests {@link org.apache.commons.lang3.builder.JsonToStringStyleTest}.
 */
public class JsonToStringStyleTest {

    private final Integer base = Integer.valueOf(5);

    @BeforeEach
    public void setUp() {
        ToStringBuilder.setDefaultStyle(ToStringStyle.JSON_STYLE);
    }

    @AfterEach
    public void tearDown() {
        ToStringBuilder.setDefaultStyle(ToStringStyle.DEFAULT_STYLE);
    }

    // ----------------------------------------------------------------

    @Test
    public void testNull() {
        assertEquals("null", new ToStringBuilder(null).toString());
    }

    @Test
    public void testBlank() {
        assertEquals("{}", new ToStringBuilder(base).toString());
    }

    @Test
    public void testAppendSuper() {
        assertEquals(
                "{}",
                new ToStringBuilder(base).appendSuper(
                        "Integer@8888[" + System.lineSeparator() + "]")
                        .toString());
        assertEquals(
                "{}",
                new ToStringBuilder(base).appendSuper(
                        "Integer@8888[" + System.lineSeparator() + "  null"
                                + System.lineSeparator() + "]").toString());
        assertEquals(
                "{\"a\":\"hello\"}",
                new ToStringBuilder(base)
                        .appendSuper(
                                "Integer@8888[" + System.lineSeparator()
                                        + "]").append("a", "hello").toString());
        assertEquals(
                "{\"a\":\"hello\"}",
                new ToStringBuilder(base)
                        .appendSuper(
                                "Integer@8888[" + System.lineSeparator()
                                        + "  null" + System.lineSeparator()
                                        + "]").append("a", "hello").toString());
        assertEquals("{\"a\":\"hello\"}", new ToStringBuilder(base)
                .appendSuper(null).append("a", "hello").toString());

        assertEquals("{\"a\":\"hello\",\"b\":\"world\"}", new ToStringBuilder(base)
                .appendSuper("{\"a\":\"hello\"}").append("b", "world").toString());
    }

    @Test
    public void testChar() {
        assertThrows(UnsupportedOperationException.class, () -> new ToStringBuilder(base).append('A').toString());

        assertEquals("{\"a\":\"A\"}", new ToStringBuilder(base).append("a", 'A')
                .toString());
        assertEquals("{\"a\":\"A\",\"b\":\"B\"}", new ToStringBuilder(base).append("a", 'A').append("b", 'B')
                .toString());
    }

    @Test
    public void testDate() {
        final Date now = new Date();
        final Date afterNow = new Date(System.currentTimeMillis() + 1);

        assertThrows(UnsupportedOperationException.class, () -> new ToStringBuilder(base).append(now).toString());

        assertEquals("{\"now\":\"" + now.toString() +"\"}", new ToStringBuilder(base).append("now", now)
                .toString());
        assertEquals("{\"now\":\"" + now.toString() +"\",\"after\":\"" + afterNow.toString() + "\"}", new ToStringBuilder(base).append("now", now).append("after", afterNow)
                .toString());
    }

    @Test
    public void testObject() {

        final Integer i3 = Integer.valueOf(3);
        final Integer i4 = Integer.valueOf(4);

        assertThrows(
                UnsupportedOperationException.class, () -> new ToStringBuilder(base).append((Object) null).toString());

        assertThrows(UnsupportedOperationException.class, () -> new ToStringBuilder(base).append(i3).toString());

        assertEquals("{\"a\":null}",
                new ToStringBuilder(base).append("a", (Object) null).toString());
        assertEquals("{\"a\":3}", new ToStringBuilder(base).append("a", i3)
                .toString());
        assertEquals("{\"a\":3,\"b\":4}",
                new ToStringBuilder(base).append("a", i3).append("b", i4)
                        .toString());

        assertThrows(
                UnsupportedOperationException.class, () -> new ToStringBuilder(base).append("a", i3, false).toString());

        assertThrows(
                UnsupportedOperationException.class,
                () -> new ToStringBuilder(base).append("a", new ArrayList<>(), false).toString());

        assertEquals(
                "{\"a\":[]}",
                new ToStringBuilder(base).append("a", new ArrayList<>(),
                        true).toString());

        assertThrows(
                UnsupportedOperationException.class,
                () -> new ToStringBuilder(base).append("a", new HashMap<>(), false).toString());

        assertEquals(
                "{\"a\":{}}",
                new ToStringBuilder(base).append("a",
                        new HashMap<>(), true).toString());

        assertThrows(
                UnsupportedOperationException.class,
                () -> new ToStringBuilder(base).append("a", (Object) new String[0], false).toString());

        assertEquals(
                "{\"a\":[]}",
                new ToStringBuilder(base).append("a", (Object) new String[0],
                        true).toString());

        assertThrows(
                UnsupportedOperationException.class,
                () -> new ToStringBuilder(base).append("a", (Object) new int[]{1, 2, 3}, false).toString());

        assertEquals(
                "{\"a\":[1,2,3]}",
                new ToStringBuilder(base).append("a",
                        (Object) new int[]{1, 2, 3}, true).toString());

        assertThrows(
                UnsupportedOperationException.class,
                () -> new ToStringBuilder(base).append("a", (Object) new String[]{"v", "x", "y", "z"}, false).toString());

        assertEquals(
                "{\"a\":[\"v\",\"x\",\"y\",\"z\"]}",
                new ToStringBuilder(base).append("a",
                        (Object) new String[]{"v", "x", "y", "z"}, true)
                        .toString());
    }

    @Test
    public void testPerson() {
        final Person p = new Person();
        p.name = "Jane Doe";
        p.age = 25;
        p.smoker = true;

        assertEquals(
                "{\"name\":\"Jane Doe\",\"age\":25,\"smoker\":true}",
                new ToStringBuilder(p).append("name", p.name)
                        .append("age", p.age).append("smoker", p.smoker)
                        .toString());
    }

    @Test
    public void testNestingPerson() {
        final Person p = new Person(){
            @Override
            public String toString(){
                return new ToStringBuilder(this).append("name", this.name)
                    .append("age", this.age).append("smoker", this.smoker)
                    .toString();
            }
        };
        p.name = "Jane Doe";
        p.age = 25;
        p.smoker = true;

        final NestingPerson nestP = new NestingPerson();
        nestP.pid="#1@Jane";
        nestP.person = p;

        assertEquals(
                "{\"pid\":\"#1@Jane\",\"person\":{\"name\":\"Jane Doe\",\"age\":25,\"smoker\":true}}",
                new ToStringBuilder(nestP).append("pid", nestP.pid)
                        .append("person", nestP.person)
                        .toString());
    }

    @Test
    public void testLong() {
        assertThrows(UnsupportedOperationException.class, () -> new ToStringBuilder(base).append(3L).toString());

        assertEquals("{\"a\":3}", new ToStringBuilder(base).append("a", 3L)
                .toString());
        assertEquals("{\"a\":3,\"b\":4}",
                new ToStringBuilder(base).append("a", 3L).append("b", 4L)
                        .toString());
    }

    @Test
    public void testObjectArray() {
        Object[] array = new Object[]{null, base, new int[]{3, 6}};

        assertThrows(UnsupportedOperationException.class, () -> new ToStringBuilder(base).append(array).toString());

        assertThrows(
                UnsupportedOperationException.class, () -> new ToStringBuilder(base).append((Object) array).toString());

        assertThrows(
                UnsupportedOperationException.class, () -> new ToStringBuilder(base).append((Object[]) null).toString());

        assertThrows(
                UnsupportedOperationException.class, () -> new ToStringBuilder(base).append((Object) array).toString());
    }

    @Test
    public void testLongArray() {
        long[] array = new long[]{1, 2, -3, 4};

        assertThrows(UnsupportedOperationException.class, () -> new ToStringBuilder(base).append(array).toString());

        assertThrows(
                UnsupportedOperationException.class, () -> new ToStringBuilder(base).append((Object) array).toString());

        assertThrows(
                UnsupportedOperationException.class, () -> new ToStringBuilder(base).append((long[]) null).toString());

        assertThrows(
                UnsupportedOperationException.class, () -> new ToStringBuilder(base).append((Object) array).toString());
    }

    @Test
    public void testLongArrayArray() {
        long[][] array = new long[][]{{1, 2}, null, {5}};

        assertThrows(UnsupportedOperationException.class, () -> new ToStringBuilder(base).append(array).toString());

        assertThrows(
                UnsupportedOperationException.class, () -> new ToStringBuilder(base).append((Object) array).toString());

        assertThrows(
                UnsupportedOperationException.class, () -> new ToStringBuilder(base).append((long[][]) null).toString());

        assertThrows(
                UnsupportedOperationException.class, () -> new ToStringBuilder(base).append((Object) array).toString());
    }

    @Test
    public void testArray() {
        final Person p = new Person();
        p.name = "Jane Doe";
        p.age = 25;
        p.smoker = true;

        assertEquals(
                "{\"name\":\"Jane Doe\",\"age\":25,\"smoker\":true,\"groups\":['admin', 'manager', 'user']}",
                new ToStringBuilder(p).append("name", p.name)
                        .append("age", p.age).append("smoker", p.smoker)
                        .append("groups", new Object() {
                            @Override
                            public String toString() {
                                return "['admin', 'manager', 'user']";
                            }
                        })
                        .toString());
    }

    @Test
    public void testLANG1395() {
        assertEquals("{\"name\":\"value\"}",new ToStringBuilder(base).append("name","value").toString());
        assertEquals("{\"name\":\"\"}",new ToStringBuilder(base).append("name","").toString());
        assertEquals("{\"name\":\"\\\"\"}",new ToStringBuilder(base).append("name",'"').toString());
        assertEquals("{\"name\":\"\\\\\"}",new ToStringBuilder(base).append("name",'\\').toString());
        assertEquals("{\"name\":\"Let's \\\"quote\\\" this\"}",new ToStringBuilder(base).append("name","Let's \"quote\" this").toString());
    }

    @Test
    public void testLANG1396() {
        assertEquals("{\"Let's \\\"quote\\\" this\":\"value\"}",new ToStringBuilder(base).append("Let's \"quote\" this","value").toString());
    }

    /**
     * An object with nested object structures used to test {@link ToStringStyle.JsonToStringStyle}.
     *
     */
    static class NestingPerson {
        /**
         * Test String field.
         */
        String pid;

        /**
         * Test nested object field.
         */
        Person person;
    }
}
