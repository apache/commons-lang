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
package org.apache.commons.lang3.arch;

/**
 * The {@link Processor} represents a microprocessor and defines
 * some properties like architecture and type of the microprocessor.
 * @since 3.6
 */
public class Processor {

    /**
     * The {@link Arch} enum defines the architecture of
     * a microprocessor. The architecture represents the bit value
     * of the microprocessor.
     * The following architectures are defined:
     * <ul>
     *     <li>32 bit</li>
     *     <li>64 bit</li>
     *     <li>unknown</li>
     * </ul>
     */
    public enum Arch {

        /**
         * A 32-bit processor architecture.
         */
        BIT_32,

        /**
         * A 64-bit processor architecture.
         */
        BIT_64,

        /**
         * An unknown-bit processor architecture.
         */
        UNKNOWN
    }

    /**
     * The {@link Type} enum defines types of a microprocessor.
     * The following types are defined:
     * <ul>
     *     <li>x86</li>
     *     <li>ia64</li>
     *     <li>ppc</li>
     *     <li>unknown</li>
     * </ul>
     */
    public enum Type {

        /**
         * Intel x86 series of instruction set architectures.
         */
        X86,

        /**
         * Intel Itanium  64-bit architecture.
         */
        IA_64,

        /**
         * Apple–IBM–Motorola PowerPC architecture.
         */
        PPC,

        /**
         * Unknown architecture.
         */
        UNKNOWN
    }

    private final Arch arch;
    private final Type type;

    /**
     * Constructs a {@link Processor} object with the given
     * parameters.
     *
     * @param arch The processor architecture.
     * @param type The processor type.
     */
    public Processor(Arch arch, Type type) {
        this.arch = arch;
        this.type = type;
    }

    /**
     * Returns the processor architecture as an {@link Arch} enum.
     * The processor architecture defines, if the processor has
     * a 32 or 64 bit architecture.
     *
     * @return A {@link Arch} enum.
     */
    public Arch getArch() {
        return arch;
    }

    /**
     * Returns the processor type as {@link Type} enum.
     * The processor type defines, if the processor is for example
     * a x86 or PPA.
     *
     * @return A {@link Type} enum.
     */
    public Type getType() {
        return type;
    }

    /**
     * Checks if {@link Processor} is 32 bit.
     *
     * @return <code>true</code>, if {@link Processor} is {@link Arch#BIT_32}, else <code>false</code>.
     */
    public boolean is32Bit() {
        return Arch.BIT_32.equals(arch);
    }

    /**
     * Checks if {@link Processor} is 64 bit.
     *
     * @return <code>true</code>, if {@link Processor} is {@link Arch#BIT_64}, else <code>false</code>.
     */
    public boolean is64Bit() {
        return Arch.BIT_64.equals(arch);
    }

    /**
     * Checks if {@link Processor} is type of x86.
     *
     * @return <code>true</code>, if {@link Processor} is {@link Type#X86}, else <code>false</code>.
     */
    public boolean isX86() {
        return Type.X86.equals(type);
    }

    /**
     * Checks if {@link Processor} is type of Intel Itanium.
     *
     * @return <code>true</code>. if {@link Processor} is {@link Type#IA_64}, else <code>false</code>.
     */
    public boolean isIA64() {
        return Type.IA_64.equals(type);
    }

    /**
     * Checks if {@link Processor} is type of Power PC.
     *
     * @return <code>true</code>. if {@link Processor} is {@link Type#PPC}, else <code>false</code>.
     */
    public boolean isPPC() {
        return Type.PPC.equals(type);
    }

}
