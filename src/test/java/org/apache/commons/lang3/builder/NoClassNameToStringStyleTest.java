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

import org.apache.commons.lang3.builder.ToStringStyleTest.Person;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.HashMap;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Unit tests {@link ToStringStyle#NO_CLASS_NAME_STYLE}.
 */
public class NoClassNameToStringStyleTest {

    private final Integer base = Integer.valueOf(5);

    @BeforeEach
    public void setUp() {
        ToStringBuilder.setDefaultStyle(ToStringStyle.NO_CLASS_NAME_STYLE);
    }

    @AfterEach
    public void tearDown() {
        ToStringBuilder.setDefaultStyle(ToStringStyle.DEFAULT_STYLE);
    }

    //----------------------------------------------------------------

    @Test
    public void testBlank() {
        assertEquals("[]", new ToStringBuilder(base).toString());
    }

    @Test
    public void testAppendSuper() {
        assertEquals("[]", new ToStringBuilder(base).appendSuper("Integer@8888[]").toString());
        assertEquals("[<null>]", new ToStringBuilder(base).appendSuper("Integer@8888[<null>]").toString());

        assertEquals("[a=hello]", new ToStringBuilder(base).appendSuper("Integer@8888[]").append("a", "hello").toString());
        assertEquals("[<null>,a=hello]", new ToStringBuilder(base).appendSuper("Integer@8888[<null>]").append("a", "hello").toString());
        assertEquals("[a=hello]", new ToStringBuilder(base).appendSuper(null).append("a", "hello").toString());
    }

    @Test
    public void testObject() {
        final Integer i3 = Integer.valueOf(3);
        final Integer i4 = Integer.valueOf(4);
        assertEquals("[<null>]", new ToStringBuilder(base).append((Object) null).toString());
        assertEquals("[3]", new ToStringBuilder(base).append(i3).toString());
        assertEquals("[a=<null>]", new ToStringBuilder(base).append("a", (Object) null).toString());
        assertEquals("[a=3]", new ToStringBuilder(base).append("a", i3).toString());
        assertEquals("[a=3,b=4]", new ToStringBuilder(base).append("a", i3).append("b", i4).toString());
        assertEquals("[a=<Integer>]", new ToStringBuilder(base).append("a", i3, false).toString());
        assertEquals("[a=<size=0>]", new ToStringBuilder(base).append("a", new ArrayList<>(), false).toString());
        assertEquals("[a=[]]", new ToStringBuilder(base).append("a", new ArrayList<>(), true).toString());
        assertEquals("[a=<size=0>]", new ToStringBuilder(base).append("a", new HashMap<>(), false).toString());
        assertEquals("[a={}]", new ToStringBuilder(base).append("a", new HashMap<>(), true).toString());
        assertEquals("[a=<size=0>]", new ToStringBuilder(base).append("a", (Object) new String[0], false).toString());
        assertEquals("[a={}]", new ToStringBuilder(base).append("a", (Object) new String[0], true).toString());
    }

    @Test
    public void testPerson() {
        final Person p = new Person();
        p.name = "John Q. Public";
        p.age = 45;
        p.smoker = true;
        assertEquals("[name=John Q. Public,age=45,smoker=true]", new ToStringBuilder(p).append("name", p.name).append("age", p.age).append("smoker", p.smoker).toString());
    }

    @Test
    public void testLong() {
        assertEquals("[3]", new ToStringBuilder(base).append(3L).toString());
        assertEquals("[a=3]", new ToStringBuilder(base).append("a", 3L).toString());
        assertEquals("[a=3,b=4]", new ToStringBuilder(base).append("a", 3L).append("b", 4L).toString());
    }

    @Test
    public void testObjectArray() {
        Object[] array = new Object[] {null, base, new int[] {3, 6}};
        assertEquals("[{<null>,5,{3,6}}]", new ToStringBuilder(base).append(array).toString());
        assertEquals("[{<null>,5,{3,6}}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals("[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals("[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

    @Test
    public void testLongArray() {
        long[] array = new long[] {1, 2, -3, 4};
        assertEquals("[{1,2,-3,4}]", new ToStringBuilder(base).append(array).toString());
        assertEquals("[{1,2,-3,4}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals("[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals("[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

    @Test
    public void testLongArrayArray() {
        long[][] array = new long[][] {{1, 2}, null, {5}};
        assertEquals("[{{1,2},<null>,{5}}]", new ToStringBuilder(base).append(array).toString());
        assertEquals("[{{1,2},<null>,{5}}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals("[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals("[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

}
