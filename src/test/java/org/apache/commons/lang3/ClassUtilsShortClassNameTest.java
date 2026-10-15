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

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.lang.reflect.Array;
import java.util.function.Supplier;

import org.junit.jupiter.api.Test;

class $trange {
}

/**
 * Tests {@link ClassUtils#getShortClassName(Class)} for <a href="https://issues.apache.org/jira/browse/LANG-1818">LANG-1818</a>
 */
public class ClassUtilsShortClassNameTest extends AbstractLangTest {

    class $Inner {
    }

    class Inner {

        class Ne$ted {
        }
    }

    @Test
    void testDeeplyNestedMemberOfAnonymousClass() {
        final Supplier<Class<?>> anonymous = new Supplier<Class<?>>() {

            class Inner$Name {
                class Deep$Name {
                }
            }

            @Override
            public Class<?> get() {
                return Inner$Name.Deep$Name.class;
            }
        };
        final Class<?> deep = anonymous.get();
        final String expected = ClassUtils.getShortClassName(anonymous.getClass()) + ".Inner$Name.Deep$Name";
        assertEquals(expected, ClassUtils.getShortClassName(deep));
        assertEquals(expected + "[][]", ClassUtils.getShortClassName(Array.newInstance(deep, 0, 0).getClass()));
    }

    @Test
    void testDeeplyNestedMemberOfLocalClass() {
        class Local {
            class Inner$Name {
                class Deep$Name {
                }
            }
        }
        final String expected = ClassUtils.getShortClassName(Local.class) + ".Inner$Name.Deep$Name";
        assertEquals(expected, ClassUtils.getShortClassName(Local.Inner$Name.Deep$Name.class));
        assertEquals(expected + "[][]", ClassUtils.getShortClassName(Local.Inner$Name.Deep$Name[][].class));
    }

    @Test
    void testDollarSignImmediatelyAfterPackage() {
        assertEquals("$trange", ClassUtils.getShortClassName($trange.class));
    }

    @Test
    void testDollarSignWithinMemberOfAnonymousClass() {
        final Supplier<Class<?>> anonymous = new Supplier<Class<?>>() {

            class Inner$Name {
            }

            @Override
            public Class<?> get() {
                return Inner$Name.class;
            }
        };
        final String shortName = ClassUtils.getShortClassName(anonymous.get());
        assertEquals(ClassUtils.getShortClassName(anonymous.getClass()) + ".Inner$Name", shortName);
    }

    @Test
    void testDollarSignWithinMemberOfLocalClass() {
        class Local {
            class Inner$Name {
            }
        }
        final String shortName = ClassUtils.getShortClassName(Local.Inner$Name.class);
        assertEquals(ClassUtils.getShortClassName(Local.class) + ".Inner$Name", shortName);
    }

    @Test
    void testDollarSignWithinName() {
        assertEquals("Pa$$word", ClassUtils.getShortClassName(Pa$$word.class));
    }

    @Test
    void testInnerClassName() {
        assertEquals(getClass().getSimpleName() + ".Inner", ClassUtils.getShortClassName(Inner.class));
    }

    @Test
    void testMemberOfAnonymousClass() {
        final Supplier<Class<?>> anonymous = new Supplier<Class<?>>() {

            class Inner {
            }

            @Override
            public Class<?> get() {
                return Inner.class;
            }
        };
        final Class<?> inner = anonymous.get();
        // The enclosing class and compiler-generated ordinal must be kept, as in the name-based form.
        assertEquals(ClassUtils.getShortClassName(inner.getName()), ClassUtils.getShortClassName(inner));
        assertEquals(ClassUtils.getShortClassName(inner.getName()) + "[]", ClassUtils.getShortClassName(Array.newInstance(inner, 0).getClass()));
    }

    @Test
    void testMemberOfLocalClass() {
        class Local {
            class Inner {
            }
        }
        // The enclosing class and compiler-generated ordinal must be kept, as in the name-based form.
        assertEquals(ClassUtils.getShortClassName(Local.Inner.class.getName()), ClassUtils.getShortClassName(Local.Inner.class));
        assertEquals(ClassUtils.getShortClassName(Local.Inner.class.getName()) + "[]", ClassUtils.getShortClassName(Local.Inner[].class));
    }

    @Test
    void testMultipleDollarSigns() {
        assertEquals(getClass().getSimpleName() + ".$Inner", ClassUtils.getShortClassName($Inner.class));
    }

    @Test
    void testNe$tedClassName() {
        assertEquals(getClass().getSimpleName() + ".Inner.Ne$ted", ClassUtils.getShortClassName(Inner.Ne$ted.class));
    }
}

class Pa$$word {
}
