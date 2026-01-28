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

import org.junit.jupiter.api.Test;

class $trange {
}

/**
 * Tests {@link ClassUtils#getShortClassName(Class)} for <a href="https://issues.apache.org/jira/browse/LANG-1818">LANG-1818</a>
 */
public class ClassUtilsShortClassNameTest {

    class $Inner {
    }

    class Inner {

        class Ne$ted {
        }
    }

    @Test
    void testDollarSignImmediatelyAfterPackage() {
        assertEquals("$trange", ClassUtils.getShortClassName($trange.class));
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
