/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.commons.lang3.junit;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Tests {@link ContextClassLoaderExtension}.
 */
class ContextClassLoaderExtensionTest {

    private ExtensionContext newContext() {
        final ExtensionContext context = mock(ExtensionContext.class);
        final ExtensionContext.Store store = mock(ExtensionContext.Store.class);
        final Map<Object, Object> values = new HashMap<>();
        when(context.getStore(any(ExtensionContext.Namespace.class))).thenReturn(store);
        doAnswer(invocation -> {
            values.put(invocation.getArgument(0), invocation.getArgument(1));
            return null;
        }).when(store).put(any(), any());
        when(store.remove(any(), any())).thenAnswer(invocation -> values.remove(invocation.getArgument(0)));
        return context;
    }

    @ParameterizedTest
    @ValueSource(booleans = { false, true })
    void testConfiguredLoader(final boolean nullOriginal) {
        final Thread thread = Thread.currentThread();
        final ClassLoader saved = thread.getContextClassLoader();
        final ClassLoader original = nullOriginal ? null : getClass().getClassLoader();
        final ClassLoader custom = nullOriginal ? getClass().getClassLoader() : null;
        final ContextClassLoaderExtension extension = new ContextClassLoaderExtension(custom);
        final ExtensionContext context = newContext();
        try {
            thread.setContextClassLoader(original);
            extension.beforeEach(context);
            assertSame(custom, thread.getContextClassLoader());
            extension.afterEach(context);
            assertSame(original, thread.getContextClassLoader());
        } finally {
            thread.setContextClassLoader(saved);
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = { false, true })
    void testMethodScopedLoader(final boolean nullOriginal) {
        final Thread thread = Thread.currentThread();
        final ClassLoader saved = thread.getContextClassLoader();
        final ClassLoader original = nullOriginal ? null : getClass().getClassLoader();
        final ContextClassLoaderExtension extension = new ContextClassLoaderExtension();
        final ExtensionContext context = newContext();
        try {
            thread.setContextClassLoader(original);
            extension.beforeEach(context);
            assertSame(original, thread.getContextClassLoader());
            final ClassLoader custom = nullOriginal ? getClass().getClassLoader() : null;
            extension.setContextClassLoader(custom);
            assertSame(custom, thread.getContextClassLoader());
            extension.afterEach(context);
            assertSame(original, thread.getContextClassLoader());
        } finally {
            thread.setContextClassLoader(saved);
        }
    }
}
