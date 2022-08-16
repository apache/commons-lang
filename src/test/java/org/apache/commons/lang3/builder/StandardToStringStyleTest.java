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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Arrays;
import java.util.Collections;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.builder.ToStringStyleTest.Person;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * Unit tests {@link org.apache.commons.lang3.builder.ToStringStyle}.
 */
public class StandardToStringStyleTest extends AbstractLangTest {

    private final Integer base = Integer.valueOf(5);
    private final String baseStr = "Integer";

    private static final StandardToStringStyle STYLE = new StandardToStringStyle();

    static {
        STYLE.setUseShortClassName(true);
        STYLE.setUseIdentityHashCode(false);
        STYLE.setArrayStart("[");
        STYLE.setArraySeparator(", ");
        STYLE.setArrayEnd("]");
        STYLE.setNullText("%NULL%");
        STYLE.setSizeStartText("%SIZE=");
        STYLE.setSizeEndText("%");
        STYLE.setSummaryObjectStartText("%");
        STYLE.setSummaryObjectEndText("%");
        STYLE.setUseClassName(true);
        STYLE.setUseFieldNames(true);
        STYLE.setUseClassName(true);
        STYLE.setUseFieldNames(true);
        STYLE.setDefaultFullDetail(true);
        STYLE.setArrayContentDetail(true);
        STYLE.setFieldNameValueSeparator("=");
    }

    @BeforeEach
    public void setUp() {
        ToStringBuilder.setDefaultStyle(STYLE);
    }

    @AfterEach
    public void tearDown() {
        ToStringBuilder.setDefaultStyle(ToStringStyle.DEFAULT_STYLE);
    }

    @Test
    public void testBlank() {
        assertEquals(baseStr + "[]", new ToStringBuilder(base).toString());
    }

    @Test
    public void testAppendSuper() {
        assertEquals(baseStr + "[]", new ToStringBuilder(base).appendSuper("Integer@8888[]").toString());
        assertEquals(baseStr + "[%NULL%]", new ToStringBuilder(base).appendSuper("Integer@8888[%NULL%]").toString());

        assertEquals(baseStr + "[a=hello]", new ToStringBuilder(base).appendSuper("Integer@8888[]").append("a", "hello").toString());
        assertEquals(baseStr + "[%NULL%,a=hello]", new ToStringBuilder(base).appendSuper("Integer@8888[%NULL%]").append("a", "hello").toString());
        assertEquals(baseStr + "[a=hello]", new ToStringBuilder(base).appendSuper(null).append("a", "hello").toString());
    }

    @Test
    public void testObject() {
        final Integer i3 = Integer.valueOf(3);
        final Integer i4 = Integer.valueOf(4);
        assertEquals(baseStr + "[%NULL%]", new ToStringBuilder(base).append((Object) null).toString());
        assertEquals(baseStr + "[3]", new ToStringBuilder(base).append(i3).toString());
        assertEquals(baseStr + "[a=%NULL%]", new ToStringBuilder(base).append("a", (Object) null).toString());
        assertEquals(baseStr + "[a=3]", new ToStringBuilder(base).append("a", i3).toString());
        assertEquals(baseStr + "[a=3,b=4]", new ToStringBuilder(base).append("a", i3).append("b", i4).toString());
        assertEquals(baseStr + "[a=%Integer%]", new ToStringBuilder(base).append("a", i3, false).toString());
    }

    @Test
    public void testCollection() {
        final Integer i3 = Integer.valueOf(3);
        final Integer i4 = Integer.valueOf(4);
        assertEquals(baseStr + "[a=%SIZE=0%]", new ToStringBuilder(base).append("a", Collections.emptyList(), false).toString());
        assertEquals(baseStr + "[a=[]]", new ToStringBuilder(base).append("a", Collections.emptyList(), true).toString());
        assertEquals(baseStr + "[a=%SIZE=1%]", new ToStringBuilder(base).append("a", Collections.singletonList(i3), false).toString());
        assertEquals(baseStr + "[a=[3]]", new ToStringBuilder(base).append("a", Collections.singletonList(i3), true).toString());
        assertEquals(baseStr + "[a=%SIZE=2%]", new ToStringBuilder(base).append("a", Arrays.asList(i3, i4), false).toString());
        assertEquals(baseStr + "[a=[3, 4]]", new ToStringBuilder(base).append("a", Arrays.asList(i3, i4), true).toString());
    }

    @Test
    public void testMap() {
        assertEquals(baseStr + "[a=%SIZE=0%]", new ToStringBuilder(base).append("a", Collections.emptyMap(), false).toString());
        assertEquals(baseStr + "[a={}]", new ToStringBuilder(base).append("a", Collections.emptyMap(), true).toString());
        assertEquals(baseStr + "[a=%SIZE=1%]", new ToStringBuilder(base).append("a", Collections.singletonMap("k", "v"), false).toString());
        assertEquals(baseStr + "[a={k=v}]", new ToStringBuilder(base).append("a", Collections.singletonMap("k", "v"), true).toString());
    }

    @Test
    public void testArray() {
        final Integer i3 = Integer.valueOf(3);
        final Integer i4 = Integer.valueOf(4);
        assertEquals(baseStr + "[a=%SIZE=0%]", new ToStringBuilder(base).append("a", (Object) new Integer[0], false).toString());
        assertEquals(baseStr + "[a=[]]", new ToStringBuilder(base).append("a", (Object) new Integer[0], true).toString());
        assertEquals(baseStr + "[a=%SIZE=1%]", new ToStringBuilder(base).append("a", (Object) new Integer[] {i3}, false).toString());
        assertEquals(baseStr + "[a=[3]]", new ToStringBuilder(base).append("a", (Object) new Integer[] {i3}, true).toString());
        assertEquals(baseStr + "[a=%SIZE=2%]", new ToStringBuilder(base).append("a", (Object) new Integer[] {i3, i4}, false).toString());
        assertEquals(baseStr + "[a=[3, 4]]", new ToStringBuilder(base).append("a", (Object) new Integer[] {i3, i4}, true).toString());
    }

    @Test
    public void testPerson() {
        final Person p = new Person();
        p.name = "Suzy Queue";
        p.age = 19;
        p.smoker = false;
        final String pBaseStr = "ToStringStyleTest.Person";
        assertEquals(pBaseStr + "[name=Suzy Queue,age=19,smoker=false]", new ToStringBuilder(p).append("name", p.name).append("age", p.age).append("smoker", p.smoker).toString());
    }

    @Test
    public void testLong() {
        assertEquals(baseStr + "[3]", new ToStringBuilder(base).append(3L).toString());
        assertEquals(baseStr + "[a=3]", new ToStringBuilder(base).append("a", 3L).toString());
        assertEquals(baseStr + "[a=3,b=4]", new ToStringBuilder(base).append("a", 3L).append("b", 4L).toString());
    }

    @Test
    public void testObjectArray() {
        Object[] array = {null, base, new int[] {3, 6}};
        assertEquals(baseStr + "[[%NULL%, 5, [3, 6]]]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[[%NULL%, 5, [3, 6]]]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[%NULL%]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[%NULL%]", new ToStringBuilder(base).append((Object) array).toString());
    }

    @Test
    public void testLongArray() {
        long[] array = {1, 2, -3, 4};
        assertEquals(baseStr + "[[1, 2, -3, 4]]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[[1, 2, -3, 4]]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[%NULL%]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[%NULL%]", new ToStringBuilder(base).append((Object) array).toString());
    }

    @Test
    public void testLongArrayArray() {
        long[][] array = {{1, 2}, null, {5}};
        assertEquals(baseStr + "[[[1, 2], %NULL%, [5]]]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[[[1, 2], %NULL%, [5]]]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[%NULL%]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[%NULL%]", new ToStringBuilder(base).append((Object) array).toString());
    }

    @Test
    public void testDefaultValueOfUseClassName() {
        assertTrue((new StandardToStringStyle()).isUseClassName());
    }

    @Test
    public void testDefaultValueOfUseFieldNames() {
        assertTrue((new StandardToStringStyle()).isUseFieldNames());
    }

    @Test
    public void testDefaultValueOfUseShortClassName() {
        assertFalse((new StandardToStringStyle()).isUseShortClassName());
    }

    @Test
    public void testDefaultValueOfUseIdentityHashCode() {
        assertTrue((new StandardToStringStyle()).isUseIdentityHashCode());
    }

    @Test
    public void testDefaultValueOfFullDetail() {
        assertTrue((new StandardToStringStyle()).isDefaultFullDetail());
    }

    @Test
    public void testDefaultIsArrayContentDetail() {
        assertTrue((new StandardToStringStyle()).isArrayContentDetail());
    }

    @Test
    public void testDefaultIsFieldSeparatorAtStart() {
        assertFalse((new StandardToStringStyle()).isFieldSeparatorAtStart());
    }

    @Test
    public void testDefaultIsFieldSeparatorAtEnd() {
        assertFalse((new StandardToStringStyle()).isFieldSeparatorAtEnd());
    }

    @Test
    public void testDefaultGetter() {
        assertEquals("[", STYLE.getContentStart());
        assertEquals("]", STYLE.getContentEnd());
        assertEquals("=", STYLE.getFieldNameValueSeparator());
        assertEquals(",", STYLE.getFieldSeparator());
        assertEquals("%NULL%", STYLE.getNullText());
        assertEquals("%SIZE=", STYLE.getSizeStartText());
        assertEquals("%", STYLE.getSizeEndText());
        assertEquals("%", STYLE.getSummaryObjectStartText());
        assertEquals("%", STYLE.getSummaryObjectEndText());
    }
}
