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

import org.junit.jupiter.api.extension.AfterEachCallback;
import org.junit.jupiter.api.extension.BeforeEachCallback;
import org.junit.jupiter.api.extension.ExtensionContext;

/**
 * Restores the test thread's context class loader after each test, including a {@code null} loader. The loader may be configured at construction or changed
 * from a test method.
 */
public class ContextClassLoaderExtension implements BeforeEachCallback, AfterEachCallback {

    private static final ExtensionContext.Namespace NAMESPACE = ExtensionContext.Namespace.create(ContextClassLoaderExtension.class);

    private final ClassLoader customClassLoader;

    private final boolean setBeforeEach;

    /**
     * Constructs an extension that preserves the existing loader until the test changes it.
     */
    public ContextClassLoaderExtension() {
        customClassLoader = null;
        setBeforeEach = false;
    }

    /**
     * Constructs an extension that installs the given loader before each test.
     *
     * @param customClassLoader The context class loader to install, possibly {@code null}.
     */
    public ContextClassLoaderExtension(final ClassLoader customClassLoader) {
        this.customClassLoader = customClassLoader;
        setBeforeEach = true;
    }

    @Override
    public void afterEach(final ExtensionContext context) {
        final Runnable restore = context.getStore(NAMESPACE).remove(this, Runnable.class);
        if (restore != null) {
            restore.run();
        }
    }

    @Override
    public void beforeEach(final ExtensionContext context) {
        final Thread thread = Thread.currentThread();
        final ClassLoader original = thread.getContextClassLoader();
        context.getStore(NAMESPACE).put(this, (Runnable) () -> thread.setContextClassLoader(original));
        if (setBeforeEach) {
            setContextClassLoader(customClassLoader);
        }
    }

    /**
     * Sets the current thread's context class loader during a test.
     *
     * @param classLoader The context class loader to install, possibly {@code null}.
     */
    public void setContextClassLoader(final ClassLoader classLoader) {
        Thread.currentThread().setContextClassLoader(classLoader);
    }
}
