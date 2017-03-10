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

import java.util.HashMap;
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
        Processor x86 = new Processor("x86", ProcessorArch.BIT_32, ProcessorType.X86);
        Processor i386 = new Processor("i386", ProcessorArch.BIT_32, ProcessorType.X86);
        Processor i486 = new Processor("i486", ProcessorArch.BIT_32, ProcessorType.X86);
        Processor i586 = new Processor("i586", ProcessorArch.BIT_32, ProcessorType.X86);
        Processor i686 = new Processor("i686", ProcessorArch.BIT_32, ProcessorType.X86);
        Processor pentium = new Processor("pentium", ProcessorArch.BIT_32, ProcessorType.X86);
        addProcessors(x86, i386, i486, i586, i686, pentium);
    }

    /**
     * Adding x86 64 bit {@link Processor}'s to the map
     */
    private static final void init_X86_64Bit() {
        Processor x86_64 = new Processor("x86_64", ProcessorArch.BIT_64, ProcessorType.X86);
        Processor amd64 = new Processor("amd64", ProcessorArch.BIT_64, ProcessorType.X86);
        Processor em64t = new Processor("em64t", ProcessorArch.BIT_64, ProcessorType.X86);
        Processor universal = new Processor("universal", ProcessorArch.BIT_64, ProcessorType.X86);
        addProcessors(x86_64, amd64, em64t, universal);
    }

    /**
     * Adding ia64 32 bit {@link Processor}'s to the map
     */
    private static final void init_IA64_32Bit() {
        Processor ia64_32 = new Processor("ia64_32", ProcessorArch.BIT_32, ProcessorType.IA_64);
        Processor ia64n = new Processor("ia64n", ProcessorArch.BIT_32, ProcessorType.IA_64);
        addProcessors(ia64_32, ia64n);
    }

    /**
     * Adding ia64 64 bit {@link Processor}'s to the map
     */
    private static final void init_IA64_64Bit() {
        Processor ia64 = new Processor("ia64", ProcessorArch.BIT_64, ProcessorType.IA_64);
        Processor ia64w = new Processor("ia64w", ProcessorArch.BIT_64, ProcessorType.IA_64);
        addProcessors(ia64, ia64w);
    }

    /**
     * Adding PPC 32 bit {@link Processor}'s to the map
     */
    private static final void init_PPC_32Bit() {
        Processor ppc = new Processor("ppc", ProcessorArch.BIT_32, ProcessorType.PPC);
        Processor power = new Processor("power", ProcessorArch.BIT_32, ProcessorType.PPC);
        Processor powerpc = new Processor("powerpc", ProcessorArch.BIT_32, ProcessorType.PPC);
        Processor power_pc = new Processor("power_pc", ProcessorArch.BIT_32, ProcessorType.PPC);
        Processor power_rs = new Processor("power_rs", ProcessorArch.BIT_32, ProcessorType.PPC);
        addProcessors(ppc, power, powerpc, power_pc, power_rs);
    }

    /**
     * Adding PPC 64 bit {@link Processor}'s to the map
     */
    private static final void init_PPC_64Bit() {
        Processor ppc64 = new Processor("ppc64", ProcessorArch.BIT_64, ProcessorType.PPC);
        Processor power64 = new Processor("power64", ProcessorArch.BIT_64, ProcessorType.PPC);
        Processor powerpc64 = new Processor("powerpc64", ProcessorArch.BIT_64, ProcessorType.PPC);
        Processor power_pc64 = new Processor("power_pc64", ProcessorArch.BIT_64, ProcessorType.PPC);
        Processor power_rs64 = new Processor("power_rs64", ProcessorArch.BIT_64, ProcessorType.PPC);
        addProcessors(ppc64, power64, powerpc64, power_pc64, power_rs64);
    }

    /**
     * Adds the given {@link Processor} to the map. The {@link Processor#getName()} is used as key
     * and the {@link Processor} object as value.
     *
     * @param processor The {@link Processor} to add.
     * @throws UnsupportedOperationException When key already exists in map.
     */
    private static final void addProcessor(Processor processor) throws UnsupportedOperationException {
        if (!map.containsKey(processor.getName())) {
            map.put(processor.getName(), processor);
        } else {
            String msg = "Key " + processor.getName() + " already exists in processor map";
            throw new UnsupportedOperationException(msg);
        }
    }

    /**
     * Adds the given {@link Processor}' to the map. The {@link Processor#getName()} is used as key
     * and the {@link Processor} object as value.
     *
     * @param processors The {@link Processor}'s to add.
     * @throws UnsupportedOperationException When key already exists in map.
     */
    private static final void addProcessors(Processor... processors) {
        for (Processor processor : processors) {
            addProcessor(processor);
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
