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
package org.apache.commons.lang3.vm;

import java.io.Serializable;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.function.Consumer;

import static java.util.Objects.requireNonNull;

public class JVMLaunchers
{
    private JVMLaunchers() {}

    public static class VmBuilder<T extends Serializable>
    {
        private VmCallable<T> callable;
        private Consumer<String> consoleHandler;
        private final List<URL> tmpJars = new ArrayList<>();

        public VmBuilder<T> setCallable(VmCallable<T> callable)
        {
            this.callable = requireNonNull(callable, "callable is null");
            return this;
        }

        public VmBuilder<T> setConsole(Consumer<String> consoleHandler)
        {
            this.consoleHandler = requireNonNull(consoleHandler, "consoleHandler is null");
            return this;
        }

        public VmBuilder<T> addUserURLClassLoader(URLClassLoader vmClassLoader)
        {
            ClassLoader classLoader = vmClassLoader;
            while (classLoader instanceof URLClassLoader) {
                Collections.addAll(tmpJars, ((URLClassLoader) classLoader).getURLs());
                classLoader = classLoader.getParent();
            }
            return this;
        }

        public VmBuilder<T> addUserjars(Collection<URL> jars)
        {
            tmpJars.addAll(jars);
            return this;
        }

        public JVMLauncher<T> build()
        {
            requireNonNull(consoleHandler, "setConsole(Consumer<String> consoleHandler) not setting");
            return new JVMLauncher<T>(callable, consoleHandler, tmpJars);
        }
    }

    public static <T extends Serializable> VmBuilder<T> newJvm()
    {
        return new VmBuilder<T>();
    }
}
