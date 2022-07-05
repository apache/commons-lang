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
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.builder.ToStringStyleTest.Person;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * Unit tests {@link org.apache.commons.lang3.builder.JsonToStringStyleTest}.
 */
public class JsonToStringStyleTest extends AbstractLangTest {

    private final Integer base = Integer.valueOf(5);

    @BeforeEach
    public void setUp() {
        ToStringBuilder.setDefaultStyle(ToStringStyle.JSON_STYLE);
    }

    @AfterEach
    public void tearDown() {
        ToStringBuilder.setDefaultStyle(ToStringStyle.DEFAULT_STYLE);
    }

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
    public void testList() {
        final Student student = new Student();
        final ArrayList<Hobby> objects = new ArrayList<>();

        objects.add(Hobby.BOOK);
        objects.add(Hobby.SPORT);
        objects.add(Hobby.MUSIC);

        student.setHobbies(objects);

        assertEquals(student.toString(), "{\"hobbies\":[\"BOOK\",\"SPORT\",\"MUSIC\"]}");
        student.setHobbies(new ArrayList<>());
        assertEquals(student.toString(), "{\"hobbies\":[]}");
        student.setHobbies(null);
        assertEquals(student.toString(), "{\"hobbies\":null}");
    }

    @Test
    public void testArrayEnum() {
        final Teacher teacher = new Teacher();
        final Hobby[] hobbies = new Hobby[3];
        hobbies[0] = Hobby.BOOK;
        hobbies[1] = Hobby.SPORT;
        hobbies[2] = Hobby.MUSIC;

        teacher.setHobbies(hobbies);

        assertEquals(teacher.toString(), "{\"hobbies\":[\"BOOK\",\"SPORT\",\"MUSIC\"]}");
        teacher.setHobbies(new Hobby[0]);
        assertEquals(teacher.toString(), "{\"hobbies\":[]}");
        teacher.setHobbies(null);
        assertEquals(teacher.toString(), "{\"hobbies\":null}");
    }

    @Test
    public void testCombineListAndEnum() {
        final Teacher teacher = new Teacher();

        final Hobby[] teacherHobbies = new Hobby[3];
        teacherHobbies[0] = Hobby.BOOK;
        teacherHobbies[1] = Hobby.SPORT;
        teacherHobbies[2] = Hobby.MUSIC;

        teacher.setHobbies(teacherHobbies);

        final Student john = new Student();
        john.setHobbies(Arrays.asList(Hobby.BOOK, Hobby.MUSIC));

        final Student alice = new Student();
        alice.setHobbies(new ArrayList<>());

        final Student bob = new Student();
        bob.setHobbies(Collections.singletonList(Hobby.BOOK));

        final ArrayList<Student> students = new ArrayList<>();
        students.add(john);
        students.add(alice);
        students.add(bob);

        final AcademyClass academyClass = new AcademyClass();
        academyClass.setStudents(students);
        academyClass.setTeacher(teacher);

        assertEquals(academyClass.toString(), "{\"students\":[{\"hobbies\":[\"BOOK\",\"MUSIC\"]},{\"hobbies\":[]},{\"hobbies\":[\"BOOK\"]}],\"teacher\":{\"hobbies\":[\"BOOK\",\"SPORT\",\"MUSIC\"]}}");
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
        final Person p = new Person() {
            @Override
            public String toString() {
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
        final Object[] array = {null, base, new int[]{3, 6}};

        final ToStringBuilder toStringBuilder = new ToStringBuilder(base);
        assertThrows(UnsupportedOperationException.class, () -> toStringBuilder.append(array).toString());

        assertEquals("{\"objectArray\":[null,5,[3,6]]}", toStringBuilder.append("objectArray", array)
                .toString());

        assertThrows(
                UnsupportedOperationException.class, () -> toStringBuilder.append((Object) array).toString());

        assertThrows(
                UnsupportedOperationException.class, () -> toStringBuilder.append((Object[]) null).toString());

        assertThrows(
                UnsupportedOperationException.class, () -> toStringBuilder.append((Object) array).toString());
    }

    @Test
    public void testLongArray() {
        final long[] array = {1, 2, -3, 4};

        final ToStringBuilder toStringBuilder = new ToStringBuilder(base);
        assertThrows(UnsupportedOperationException.class, () -> toStringBuilder.append(array).toString());

        assertEquals("{\"longArray\":[1,2,-3,4]}", toStringBuilder.append("longArray", array)
                .toString());

        assertThrows(
                UnsupportedOperationException.class, () -> toStringBuilder.append((Object) array).toString());

        assertThrows(
                UnsupportedOperationException.class, () -> toStringBuilder.append((long[]) null).toString());

        assertThrows(
                UnsupportedOperationException.class, () -> toStringBuilder.append((Object) array).toString());
    }

    @Test
    public void testIntArray() {
        final int[] array = {1, 2, -3, 4};

        final ToStringBuilder toStringBuilder = new ToStringBuilder(base);
        assertThrows(UnsupportedOperationException.class, () -> toStringBuilder.append(array).toString());

        assertEquals("{\"intArray\":[1,2,-3,4]}", toStringBuilder.append("intArray", array)
                .toString());

        assertThrows(
                UnsupportedOperationException.class, () -> toStringBuilder.append((Object) array).toString());

        assertThrows(
                UnsupportedOperationException.class, () -> toStringBuilder.append((long[]) null).toString());

        assertThrows(
                UnsupportedOperationException.class, () -> toStringBuilder.append((Object) array).toString());
    }

    @Test
    public void testByteArray() {
        final byte[] array = {1, 2, -3, 4};

        final ToStringBuilder toStringBuilder = new ToStringBuilder(base);
        assertThrows(UnsupportedOperationException.class, () -> toStringBuilder.append(array).toString());

        assertEquals("{\"byteArray\":[1,2,-3,4]}", toStringBuilder.append("byteArray", array)
                .toString());

        assertThrows(
                UnsupportedOperationException.class, () -> toStringBuilder.append((Object) array).toString());

        assertThrows(
                UnsupportedOperationException.class, () -> toStringBuilder.append((long[]) null).toString());

        assertThrows(
                UnsupportedOperationException.class, () -> toStringBuilder.append((Object) array).toString());
    }

    @Test
    public void testShortArray() {
        final short[] array = {1, 2, -3, 4};

        final ToStringBuilder toStringBuilder = new ToStringBuilder(base);
        assertThrows(UnsupportedOperationException.class, () -> toStringBuilder.append(array).toString());

        assertEquals("{\"shortArray\":[1,2,-3,4]}", toStringBuilder.append("shortArray", array)
                .toString());

        assertThrows(
                UnsupportedOperationException.class, () -> toStringBuilder.append((Object) array).toString());

        assertThrows(
                UnsupportedOperationException.class, () -> toStringBuilder.append((long[]) null).toString());

        assertThrows(
                UnsupportedOperationException.class, () -> toStringBuilder.append((Object) array).toString());
    }

    @Test
    public void testDoubleArray() {
        final double[] array = {1, 2, -3, 4};

        final ToStringBuilder toStringBuilder = new ToStringBuilder(base);
        assertThrows(UnsupportedOperationException.class, () -> toStringBuilder.append(array).toString());

        assertEquals("{\"doubleArray\":[1.0,2.0,-3.0,4.0]}", toStringBuilder.append("doubleArray", array)
                .toString());

        assertThrows(
                UnsupportedOperationException.class, () -> toStringBuilder.append((Object) array).toString());

        assertThrows(
                UnsupportedOperationException.class, () -> toStringBuilder.append((long[]) null).toString());

        assertThrows(
                UnsupportedOperationException.class, () -> toStringBuilder.append((Object) array).toString());
    }

    @Test
    public void testFloatArray() {
        final float[] array = {1, 2, -3, 4};

        final ToStringBuilder toStringBuilder = new ToStringBuilder(base);
        assertThrows(UnsupportedOperationException.class, () -> toStringBuilder.append(array).toString());

        assertEquals("{\"floatArray\":[1.0,2.0,-3.0,4.0]}", toStringBuilder.append("floatArray", array)
                .toString());

        assertThrows(
                UnsupportedOperationException.class, () -> toStringBuilder.append((Object) array).toString());

        assertThrows(
                UnsupportedOperationException.class, () -> toStringBuilder.append((long[]) null).toString());

        assertThrows(
                UnsupportedOperationException.class, () -> toStringBuilder.append((Object) array).toString());
    }

    @Test
    public void testCharArray() {
        final char[] array = {'1', '2', '3', '4'};

        final ToStringBuilder toStringBuilder = new ToStringBuilder(base);
        assertThrows(UnsupportedOperationException.class, () -> toStringBuilder.append(array).toString());

        assertEquals("{\"charArray\":[\"1\",\"2\",\"3\",\"4\"]}", toStringBuilder.append("charArray", array)
                .toString());

        assertThrows(
                UnsupportedOperationException.class, () -> toStringBuilder.append((Object) array).toString());

        assertThrows(
                UnsupportedOperationException.class, () -> toStringBuilder.append((long[]) null).toString());

        assertThrows(
                UnsupportedOperationException.class, () -> toStringBuilder.append((Object) array).toString());
    }

    @Test
    public void testBooleanArray() {
        final boolean[] array = {true, false};

        final ToStringBuilder toStringBuilder = new ToStringBuilder(base);
        assertThrows(UnsupportedOperationException.class, () -> toStringBuilder.append(array).toString());

        assertEquals("{\"booleanArray\":[true,false]}", toStringBuilder.append("booleanArray", array)
                .toString());

        assertThrows(
                UnsupportedOperationException.class, () -> toStringBuilder.append((Object) array).toString());

        assertThrows(
                UnsupportedOperationException.class, () -> toStringBuilder.append((long[]) null).toString());

        assertThrows(
                UnsupportedOperationException.class, () -> toStringBuilder.append((Object) array).toString());
    }

    @Test
    public void testLongArrayArray() {
        final long[][] array = {{1, 2}, null, {5}};

        final ToStringBuilder toStringBuilder = new ToStringBuilder(base);
        assertThrows(UnsupportedOperationException.class, () -> toStringBuilder.append(array).toString());

        assertThrows(
                UnsupportedOperationException.class, () -> toStringBuilder.append((Object) array).toString());

        assertThrows(
                UnsupportedOperationException.class, () -> toStringBuilder.append((long[][]) null).toString());

        assertThrows(
                UnsupportedOperationException.class, () -> toStringBuilder.append((Object) array).toString());
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
        assertEquals("{\"name\":\"value\"}", new ToStringBuilder(base).append("name", "value").toString());
        assertEquals("{\"name\":\"\"}", new ToStringBuilder(base).append("name", "").toString());
        assertEquals("{\"name\":\"\\\"\"}", new ToStringBuilder(base).append("name", '"').toString());
        assertEquals("{\"name\":\"\\\\\"}", new ToStringBuilder(base).append("name", '\\').toString());
        assertEquals("{\"name\":\"Let's \\\"quote\\\" this\"}", new ToStringBuilder(base).append("name", "Let's \"quote\" this").toString());
    }

    @Test
    public void testLANG1396() {
        assertEquals("{\"Let's \\\"quote\\\" this\":\"value\"}", new ToStringBuilder(base).append("Let's \"quote\" this", "value").toString());
    }

    @Test
    public void testRootMap() {
        final Map<String, Object> map = new LinkedHashMap<>();
        map.put("k1", "v1");
        map.put("k2", 2);

        assertEquals("{\"map\":{\"k1\":\"v1\",\"k2\":2}}",
                new ToStringBuilder(base).append("map", map).toString());
    }

    @Test
    public void testObjectWithInnerMap() {
        final Map<String, Object> map = new LinkedHashMap<>();
        map.put("k1", "value1");
        map.put("k2", 2);

        final InnerMapObject object = new InnerMapObject(){
            @Override
            public String toString() {
                return new ToStringBuilder(this).append("pid", this.pid)
                        .append("map", this.map).toString();
            }
        };
        object.pid = "dummy-text";
        object.map = map;

        assertEquals("{\"object\":{\"pid\":\"dummy-text\",\"map\":{\"k1\":\"value1\",\"k2\":2}}}",
                new ToStringBuilder(base).append("object", object).toString());
    }

    @Test
    public void testNestedMaps() {
        final Map<String, Object> innerMap = new LinkedHashMap<>();
        innerMap.put("k2.1", "v2.1");
        innerMap.put("k2.2", "v2.2");
        final Map<String, Object> baseMap = new LinkedHashMap<>();
        baseMap.put("k1", "v1");
        baseMap.put("k2", innerMap);

        final InnerMapObject object = new InnerMapObject(){
            @Override
            public String toString() {
                return new ToStringBuilder(this).append("pid", this.pid)
                        .append("map", this.map).toString();
            }
        };
        object.pid = "dummy-text";
        object.map = baseMap;

        assertEquals("{\"object\":{\"pid\":\"dummy-text\",\"map\":{\"k1\":\"v1\"," +
                        "\"k2\":{\"k2.1\":\"v2.1\",\"k2.2\":\"v2.2\"}}}}",
                new ToStringBuilder(base).append("object", object).toString());
    }

    @Test
    public void testMapSkipNullKey() {
        final Map<String, Object> map = new LinkedHashMap<>();
        map.put("k1", "v1");
        map.put(null, "v2");

        assertEquals("{\"map\":{\"k1\":\"v1\"}}",
                new ToStringBuilder(base).append("map", map).toString());
    }

    /**
     * An object with nested object structures used to test {@code ToStringStyle.JsonToStringStyle}.
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

    enum Hobby {
        SPORT,
        BOOK,
        MUSIC
    }

    enum EmptyEnum {
    }

    static class Student {
        List<Hobby> hobbies;

        public List<Hobby> getHobbies() {
            return hobbies;
        }

        public void setHobbies(final List<Hobby> hobbies) {
            this.hobbies = hobbies;
        }

        @Override
        public String toString() {
            return ToStringBuilder.reflectionToString(this);
        }
    }

    static class Teacher {
        Hobby[] hobbies;

        public Hobby[] getHobbies() {
            return hobbies;
        }

        public void setHobbies(final Hobby[] hobbies) {
            this.hobbies = hobbies;
        }

        @Override
        public String toString() {
            return ToStringBuilder.reflectionToString(this);
        }
    }

    static class AcademyClass {
        Teacher teacher;
        List<Student> students;

        public void setTeacher(final Teacher teacher) {
            this.teacher = teacher;
        }

        public void setStudents(final List<Student> students) {
            this.students = students;
        }

        public Teacher getTeacher() {
            return teacher;
        }

        public List<Student> getStudents() {
            return students;
        }

        @Override
        public String toString() {
            return ToStringBuilder.reflectionToString(this);
        }
    }

    /**
     * An object with a Map field used to test {@code ToStringStyle.JsonToStringStyle}.
     */
    static class InnerMapObject {
        /**
         * Test String field.
         */
        String pid;

        /**
         * Test inner map field.
         */
        Map<String, Object> map;
    }
}
