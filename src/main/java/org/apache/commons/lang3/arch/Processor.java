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
        BIT_32, BIT_64, UNKNOWN
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
        X86, IA_64, PPC, UNKNOWN
    }

    private final Arch arch;
    private final Type type;

    /**
     * Constructs a {@link Processor}object with the given
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
     * Returns the processor architecture as {@link Arch} enum.
     * The processor architecture defines, if the processor is for example
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

}
