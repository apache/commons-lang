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

import org.apache.commons.lang3.arch.Processor;
import org.apache.commons.lang3.arch.ProcessorArch;
import org.apache.commons.lang3.arch.ProcessorType;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * An utility class for the os.arch System Property. The class defines methods for
 * identifying the architecture of the current JVM.
 * <p>
 * Important: The os.arch System Property returns the architecture used by the JVM
 * not of the operating system.
 * </p>
 */
public class ArchUtils {

    private static Map<String, Processor> map;

    static {
        map = new HashMap<>();
        init();
    }

    private static final void init() {
        init_X86_32Bit();
        init_X86_64Bit();
        init_IA64_32Bit();
        init_IA64_64Bit();
        init_PPC_32Bit();
        init_PPC_64Bit();
    }

    /**
     * Adding x86 32 bit {@link Processor}'s to the map
     */
    private static final void init_X86_32Bit() {
        Processor processor = new Processor(ProcessorArch.BIT_32, ProcessorType.X86);
        addProcessors(Arrays.asList("x86", "i386", "i486", "i586", "i686", "pentium"), processor);
    }

    /**
     * Adding x86 64 bit {@link Processor}'s to the map
     */
    private static final void init_X86_64Bit() {
        Processor processor = new Processor(ProcessorArch.BIT_64, ProcessorType.X86);
        addProcessors(Arrays.asList("x86_64", "amd64", "em64t", "universal"), processor);
    }

    /**
     * Adding ia64 32 bit {@link Processor}'s to the map
     */
    private static final void init_IA64_32Bit() {
        Processor processor = new Processor(ProcessorArch.BIT_32, ProcessorType.IA_64);
        addProcessors(Arrays.asList("ia64_32", "ia64n"), processor);
    }

    /**
     * Adding ia64 64 bit {@link Processor}'s to the map
     */
    private static final void init_IA64_64Bit() {
        Processor processor = new Processor(ProcessorArch.BIT_64, ProcessorType.IA_64);
        addProcessors(Arrays.asList("ia64", "ia64w"), processor);
    }

    /**
     * Adding PPC 32 bit {@link Processor}'s to the map
     */
    private static final void init_PPC_32Bit() {
        Processor processor = new Processor(ProcessorArch.BIT_32, ProcessorType.PPC);
        addProcessors(Arrays.asList("ppc", "power", "powerpc", "power_pc", "power_rs"), processor);
    }

    /**
     * Adding PPC 64 bit {@link Processor}'s to the map
     */
    private static final void init_PPC_64Bit() {
        Processor processor = new Processor(ProcessorArch.BIT_64, ProcessorType.PPC);
        addProcessors(Arrays.asList("ppc64", "power64", "powerpc64", "power_pc64", "power_rs64"), processor);
    }

    /**
     * Adds the given {@link Processor} whith the given key {@link String} to the map.
     *
     * @param key The key as {@link String}.
     * @param processor The {@link Processor} to add.
     * @throws UnsupportedOperationException When key already exists in map.
     */
    private static final void addProcessor(String key, Processor processor) throws UnsupportedOperationException {
        if (!map.containsKey(key)) {
            map.put(key, processor);
        } else {
            String msg = "Key " + key + " already exists in processor map";
            throw new UnsupportedOperationException(msg);
        }
    }

    /**
     * Adds the given {@link Processor} with the given keys to the map.
     *
     * @param keys A {@link List} of the key.
     * @param processor The {@link Processor} to add.
     * @throws UnsupportedOperationException When key already exists in map.
     */
    private static final void addProcessors(List<String> keys, Processor processor) {
        for (String key : keys) {
            addProcessor(key, processor);
        }
    }

    /**
     * Checks if the current running JVM is a 32 bit JVM using the os.arch System Property.
     * <p>
     * Important: The os.arch System Property returns the architecture used by the JVM
     * not of the operating system.
     * </p>
     *
     * @return <code>True</code>, if the current JVM is 32 bit, else <code>false</code>.
     */
    public static final boolean is32BitJVM() {
        return is32BitJVM(SystemUtils.OS_ARCH);
    }

    /**
     * Checks if the given {@link String} represents a 32 bit JVM. The {@link String} must be
     * like a value returned by the os.arch System Property.
     *
     * @param value The value to check.
     * @return <code>True</code>, if the {@link String} represents a 32 bit value, else <code>false</code>.
     */
    public static final boolean is32BitJVM(String value) {
        if (isSupported(value)) {
            Processor processor = map.get(value);
            return ProcessorArch.BIT_32.equals(processor.getProcessorArch());
        } else {
            return false;
        }
    }

    /**
     * Checks if the current running JVM is a 64 bit JVM using the os.arch System Property.
     * <p>
     * Important: The os.arch System Property returns the architecture used by the JVM
     * not of the operating system.
     * </p>
     *
     * @return <code>True</code>, if the current JVM is 64 bit, else <code>false</code>.
     */
    public static final boolean is64BitJVM() {
        return is64BitJVM(SystemUtils.OS_ARCH);
    }

    /**
     * Checks if the given {@link String} represents a 64 bit JVM. The {@link String} must be
     * like a value returned by the os.arch System Property.
     *
     * @param value The value to check.
     * @return <code>True</code>, if the {@link String} represents a 64 bit value, else <code>false</code>.
     */
    public static final boolean is64BitJVM(String value) {
        if (isSupported(value)) {
            Processor processor = map.get(value);
            return ProcessorArch.BIT_64.equals(processor.getProcessorArch());
        } else {
            return false;
        }
    }

    /**
     * Checks if the os.arch System Property {@link String} of the current JVM is supported.
     * <p>
     * Important: The os.arch System Property returns the architecture used by the JVM
     * not of the operating system.
     * </p>
     *
     * @return <code>True</code>, if supported, else <code>false</code>.
     */
    public static final boolean isSupported() {
        return isSupported(SystemUtils.OS_ARCH);
    }

    /**
     * Checks if the given os.arch {@link String} is supported. The given {@link String} must be
     * like a value returned by the os.arch System Property.
     *
     * @param value The value to check.
     * @return <code>True</code>, if supported, else <code>false</code>.
     */
    public static final boolean isSupported(String value) {
        return map.containsKey(value);
    }

    /**
     * Returns a {@link Processor} object of the current JVM.
     *
     * <p>
     * Important: The os.arch System Property returns the architecture used by the JVM
     * not of the operating system.
     * </p>
     *
     * @return A {@link Processor} when supported, else <code>null</code>.
     */
    public static final Processor getProcessor() {
        return getProcessor(SystemUtils.OS_ARCH);
    }

    /**
     * Returns a {@link Processor} object the given value {@link String}. The {@link String} must be
     * like a value returned by the os.arch System Property.
     *
     * @param value A {@link String} like a value returned by the os.arch System Property.
     * @return A {@link Processor} when exists, else <code>null</code>.
     */
    public static final Processor getProcessor(String value) {
        return map.get(value);
    }

}
