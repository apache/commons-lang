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

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.arch.Processor;
import org.apache.commons.lang3.stream.Streams;

/**
 * A utility class for the {@code os.arch} System Property. The class defines methods for
 * identifying the architecture of the current JVM.
 * <p>
 * Important: The {@code os.arch} System Property returns the architecture used by the JVM
 * not of the operating system.
 * </p>
 * @since 3.6
 */
public class ArchUtils {

    private static final Map<String, Processor> ARCH_TO_PROCESSOR;

    static {
        ARCH_TO_PROCESSOR = new HashMap<>();
        init();
    }

    private static void init() {
        initArch(Processor.Arch.BIT_64, Processor.Type.AARCH_64, "aarch64");
        initArch(Processor.Arch.BIT_32, Processor.Type.X86, "x86", "i386", "i486", "i586", "i686", "pentium");
        initArch(Processor.Arch.BIT_64, Processor.Type.X86, "x86_64", "amd64", "em64t", "universal");
        initArch(Processor.Arch.BIT_32, Processor.Type.IA_64, "ia64_32", "ia64n");
        initArch(Processor.Arch.BIT_64, Processor.Type.IA_64, "ia64", "ia64w");
        initArch(Processor.Arch.BIT_32, Processor.Type.PPC, "ppc", "power", "powerpc", "power_pc", "power_rs");
        initArch(Processor.Arch.BIT_64, Processor.Type.PPC, "ppc64", "power64", "powerpc64", "power_pc64", "power_rs64");
    }

    private static void initArch(Processor.Arch archType, Processor.Type type, String... arch) {
        final Processor processor = getProcessor(archType, type);
        addProcessors(processor, arch);
    }

    private static Processor getProcessor(Processor.Arch archType, Processor.Type type) {
        return new Processor(archType, type);
    }

    /**
     * Adds the given {@link Processor} with the given key {@link String} to the map.
     *
     * @param key The key as {@link String}.
     * @param processor The {@link Processor} to add.
     * @throws IllegalStateException If the key already exists.
     */
    private static void addProcessor(final String key, final Processor processor) {
        if (ARCH_TO_PROCESSOR.containsKey(key)) {
            throw new IllegalStateException("Key " + key + " already exists in processor map");
        }
        ARCH_TO_PROCESSOR.put(key, processor);
    }

    /**
     * Adds the given {@link Processor} with the given keys to the map.
     *
     * @param keys The keys.
     * @param processor The {@link Processor} to add.
     * @throws IllegalStateException If the key already exists.
     */
    private static void addProcessors(final Processor processor, final String... keys) {
        Streams.of(keys).forEach(e -> addProcessor(e, processor));
    }

    /**
     * Gets a {@link Processor} object of the current JVM.
     *
     * <p>
     * Important: The os.arch System Property returns the architecture used by the JVM
     * not of the operating system.
     * </p>
     *
     * @return A {@link Processor} when supported, else {@code null}.
     */
    public static Processor getProcessor() {
        return getProcessor(SystemProperties.getOsArch());
    }

    /**
     * Gets a {@link Processor} object the given value {@link String}. The {@link String} must be
     * like a value returned by the {@code os.arch} System Property.
     *
     * @param value A {@link String} like a value returned by the {@code os.arch} System Property.
     * @return A {@link Processor} when it exists, else {@code null}.
     */
    public static Processor getProcessor(final String value) {
        return ARCH_TO_PROCESSOR.get(value);
    }

}
