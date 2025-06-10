/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.commons.lang3;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link ClassLoaderUtils}.
 */
class ClassLoaderUtilsTest extends AbstractLangTest {

    @Test
    void testGetSystemURLs() {
        // TODO How to better test considering this test may be called from an IDE and Maven?
        assertNotNull(ClassLoaderUtils.getSystemURLs());
    }

    @Test
    void testGetThreadURLs() {
        // TODO How to better test considering this test may be called from an IDE and Maven?
        assertNotNull(ClassLoaderUtils.getThreadURLs());
    }

    @Test
    void testToString_ClassLoader() throws IOException {
        final URL url = new URL("http://localhost");
        try (URLClassLoader urlClassLoader = new URLClassLoader(new URL[] { url })) {
            @SuppressWarnings("resource")
            final ClassLoader classLoader = urlClassLoader;
            Assertions.assertEquals(String.format("%s[%s]", classLoader, url), ClassLoaderUtils.toString(classLoader));
        }
        assertEquals("null", ClassLoaderUtils.toString((ClassLoader) null));
    }

    @Test
    void testToString_URLClassLoader() throws IOException {
        final URL url = new URL("http://localhost");
        try (URLClassLoader urlClassLoader = new URLClassLoader(new URL[] { url })) {
            Assertions.assertEquals(String.format("%s[%s]", urlClassLoader, url), ClassLoaderUtils.toString(urlClassLoader));
        }
        assertEquals("null", ClassLoaderUtils.toString((URLClassLoader) null));
    }
}
