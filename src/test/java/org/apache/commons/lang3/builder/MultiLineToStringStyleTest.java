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

import java.util.Arrays;
import java.util.Collections;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.builder.ToStringStyleTest.Person;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * Unit tests {@link org.apache.commons.lang3.builder.MultiLineToStringStyleTest}.
 */
public class MultiLineToStringStyleTest extends AbstractLangTest {

    private final Integer base = Integer.valueOf(5);
    private final String baseStr = base.getClass().getName() + "@" + Integer.toHexString(System.identityHashCode(base));

    @BeforeEach
    public void setUp() {
        ToStringBuilder.setDefaultStyle(ToStringStyle.MULTI_LINE_STYLE);
    }

    @AfterEach
    public void tearDown() {
        ToStringBuilder.setDefaultStyle(ToStringStyle.DEFAULT_STYLE);
    }

    @Test
    public void testBlank() {
        assertEquals(baseStr + "[" + System.lineSeparator() + "]", new ToStringBuilder(base).toString());
    }

    @Test
    public void testAppendSuper() {
        assertEquals(baseStr + "[" + System.lineSeparator() + "]", new ToStringBuilder(base).appendSuper("Integer@8888[" + System.lineSeparator() + "]").toString());
        assertEquals(baseStr + "[" + System.lineSeparator() + "  <null>" + System.lineSeparator() + "]", new ToStringBuilder(base).appendSuper("Integer@8888[" + System.lineSeparator() + "  <null>" + System.lineSeparator() + "]").toString());

        assertEquals(baseStr + "[" + System.lineSeparator() + "  a=hello" + System.lineSeparator() + "]", new ToStringBuilder(base).appendSuper("Integer@8888[" + System.lineSeparator() + "]").append("a", "hello").toString());
        assertEquals(baseStr + "[" + System.lineSeparator() + "  <null>" + System.lineSeparator() + "  a=hello" + System.lineSeparator() + "]", new ToStringBuilder(base).appendSuper("Integer@8888[" + System.lineSeparator() + "  <null>" + System.lineSeparator() + "]").append("a", "hello").toString());
        assertEquals(baseStr + "[" + System.lineSeparator() + "  a=hello" + System.lineSeparator() + "]", new ToStringBuilder(base).appendSuper(null).append("a", "hello").toString());
    }

    @Test
    public void testObject() {
        final Integer i3 = Integer.valueOf(3);
        final Integer i4 = Integer.valueOf(4);
        assertEquals(baseStr + "[" + System.lineSeparator() + "  <null>" + System.lineSeparator() + "]", new ToStringBuilder(base).append((Object) null).toString());
        assertEquals(baseStr + "[" + System.lineSeparator() + "  3" + System.lineSeparator() + "]", new ToStringBuilder(base).append(i3).toString());
        assertEquals(baseStr + "[" + System.lineSeparator() + "  a=<null>" + System.lineSeparator() + "]", new ToStringBuilder(base).append("a", (Object) null).toString());
        assertEquals(baseStr + "[" + System.lineSeparator() + "  a=3" + System.lineSeparator() + "]", new ToStringBuilder(base).append("a", i3).toString());
        assertEquals(baseStr + "[" + System.lineSeparator() + "  a=3" + System.lineSeparator() + "  b=4" + System.lineSeparator() + "]", new ToStringBuilder(base).append("a", i3).append("b", i4).toString());
        assertEquals(baseStr + "[" + System.lineSeparator() + "  a=<Integer>" + System.lineSeparator() + "]", new ToStringBuilder(base).append("a", i3, false).toString());
    }

    @Test
    public void testCollection() {
        final Integer i3 = Integer.valueOf(3);
        final Integer i4 = Integer.valueOf(4);
        assertEquals(baseStr + "[" + System.lineSeparator() + "  a=<size=0>" + System.lineSeparator() + "]", new ToStringBuilder(base).append("a", Collections.emptyList(), false).toString());
        assertEquals(baseStr + "[" + System.lineSeparator() + "  a=[]" + System.lineSeparator() + "]", new ToStringBuilder(base).append("a", Collections.emptyList(), true).toString());
        assertEquals(baseStr + "[" + System.lineSeparator() + "  a=<size=1>" + System.lineSeparator() + "]", new ToStringBuilder(base).append("a", Collections.singletonList(i3), false).toString());
        assertEquals(baseStr + "[" + System.lineSeparator() + "  a=[3]" + System.lineSeparator() + "]", new ToStringBuilder(base).append("a", Collections.singletonList(i3), true).toString());
        assertEquals(baseStr + "[" + System.lineSeparator() + "  a=<size=2>" + System.lineSeparator() + "]", new ToStringBuilder(base).append("a", Arrays.asList(i3, i4), false).toString());
        assertEquals(baseStr + "[" + System.lineSeparator() + "  a=[3, 4]" + System.lineSeparator() + "]", new ToStringBuilder(base).append("a", Arrays.asList(i3, i4), true).toString());
    }

    @Test
    public void testMap() {
        assertEquals(baseStr + "[" + System.lineSeparator() + "  a=<size=0>" + System.lineSeparator() + "]", new ToStringBuilder(base).append("a", Collections.emptyMap(), false).toString());
        assertEquals(baseStr + "[" + System.lineSeparator() + "  a={}" + System.lineSeparator() + "]", new ToStringBuilder(base).append("a", Collections.emptyMap(), true).toString());
        assertEquals(baseStr + "[" + System.lineSeparator() + "  a=<size=1>" + System.lineSeparator() + "]", new ToStringBuilder(base).append("a", Collections.singletonMap("k", "v"), false).toString());
        assertEquals(baseStr + "[" + System.lineSeparator() + "  a={k=v}" + System.lineSeparator() + "]", new ToStringBuilder(base).append("a", Collections.singletonMap("k", "v"), true).toString());
    }

    @Test
    public void testArray() {
        final Integer i3 = Integer.valueOf(3);
        final Integer i4 = Integer.valueOf(4);
        assertEquals(baseStr + "[" + System.lineSeparator() + "  a=<size=0>" + System.lineSeparator() + "]", new ToStringBuilder(base).append("a", (Object) new Integer[0], false).toString());
        assertEquals(baseStr + "[" + System.lineSeparator() + "  a={}" + System.lineSeparator() + "]", new ToStringBuilder(base).append("a", (Object) new Integer[0], true).toString());
        assertEquals(baseStr + "[" + System.lineSeparator() + "  a=<size=1>" + System.lineSeparator() + "]", new ToStringBuilder(base).append("a", (Object) new Integer[] {i3}, false).toString());
        assertEquals(baseStr + "[" + System.lineSeparator() + "  a={3}" + System.lineSeparator() + "]", new ToStringBuilder(base).append("a", (Object) new Integer[] {i3}, true).toString());
        assertEquals(baseStr + "[" + System.lineSeparator() + "  a=<size=2>" + System.lineSeparator() + "]", new ToStringBuilder(base).append("a", (Object) new Integer[] {i3, i4}, false).toString());
        assertEquals(baseStr + "[" + System.lineSeparator() + "  a={3,4}" + System.lineSeparator() + "]", new ToStringBuilder(base).append("a", (Object) new Integer[] {i3, i4}, true).toString());
    }

    @Test
    public void testPerson() {
        final Person p = new Person();
        p.name = "Jane Doe";
        p.age = 25;
        p.smoker = true;
        final String pBaseStr = p.getClass().getName() + "@" + Integer.toHexString(System.identityHashCode(p));
        assertEquals(pBaseStr + "[" + System.lineSeparator() + "  name=Jane Doe" + System.lineSeparator() + "  age=25" + System.lineSeparator() + "  smoker=true" + System.lineSeparator() + "]", new ToStringBuilder(p).append("name", p.name).append("age", p.age).append("smoker", p.smoker).toString());
    }

    @Test
    public void testLong() {
        assertEquals(baseStr + "[" + System.lineSeparator() + "  3" + System.lineSeparator() + "]", new ToStringBuilder(base).append(3L).toString());
        assertEquals(baseStr + "[" + System.lineSeparator() + "  a=3" + System.lineSeparator() + "]", new ToStringBuilder(base).append("a", 3L).toString());
        assertEquals(baseStr + "[" + System.lineSeparator() + "  a=3" + System.lineSeparator() + "  b=4" + System.lineSeparator() + "]", new ToStringBuilder(base).append("a", 3L).append("b", 4L).toString());
    }

    @Test
    public void testObjectArray() {
        Object[] array = {null, base, new int[] {3, 6}};
        assertEquals(baseStr + "[" + System.lineSeparator() + "  {<null>,5,{3,6}}" + System.lineSeparator() + "]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[" + System.lineSeparator() + "  {<null>,5,{3,6}}" + System.lineSeparator() + "]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[" + System.lineSeparator() + "  <null>" + System.lineSeparator() + "]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[" + System.lineSeparator() + "  <null>" + System.lineSeparator() + "]", new ToStringBuilder(base).append((Object) array).toString());
    }

    @Test
    public void testLongArray() {
        long[] array = {1, 2, -3, 4};
        assertEquals(baseStr + "[" + System.lineSeparator() + "  {1,2,-3,4}" + System.lineSeparator() + "]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[" + System.lineSeparator() + "  {1,2,-3,4}" + System.lineSeparator() + "]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[" + System.lineSeparator() + "  <null>" + System.lineSeparator() + "]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[" + System.lineSeparator() + "  <null>" + System.lineSeparator() + "]", new ToStringBuilder(base).append((Object) array).toString());
    }

    @Test
    public void testLongArrayArray() {
        long[][] array = {{1, 2}, null, {5}};
        assertEquals(baseStr + "[" + System.lineSeparator() + "  {{1,2},<null>,{5}}" + System.lineSeparator() + "]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[" + System.lineSeparator() + "  {{1,2},<null>,{5}}" + System.lineSeparator() + "]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[" + System.lineSeparator() + "  <null>" + System.lineSeparator() + "]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[" + System.lineSeparator() + "  <null>" + System.lineSeparator() + "]", new ToStringBuilder(base).append((Object) array).toString());
    }

}
