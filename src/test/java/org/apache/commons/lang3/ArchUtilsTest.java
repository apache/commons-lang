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
package org.apache.commons.lang3;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.apache.commons.lang3.arch.Processor;
import org.apache.commons.lang3.arch.Processor.Arch;
import org.apache.commons.lang3.arch.Processor.Type;
import org.junit.jupiter.api.Test;

/**
 * Test class for {@link ArchUtils}.
 */
class ArchUtilsTest extends AbstractLangTest {

    private static final String IA64 = "ia64";
    private static final String IA64_32 = "ia64_32";
    private static final String PPC = "ppc";
    private static final String PPC64 = "ppc64";
    private static final String X86 = "x86";
    private static final String X86_64 = "x86_64";
    private static final String AARCH_64 = "aarch64";
    private static final String RISCV_64 = "riscv64";
    private static final String RISCV_32 = "riscv32";

    private void assertEqualsArchNotNull(final Processor.Arch arch, final Processor processor) {
        assertNotNull(arch);
        assertNotNull(processor);
        assertEquals(arch, processor.getArch());
    }

    private void assertEqualsTypeNotNull(final Processor.Type type, final Processor processor) {
        assertNotNull(type);
        assertNotNull(processor);
        assertEquals(type, processor.getType());
    }

    private void assertNotEqualsArchNotNull(final Processor.Arch arch, final Processor processor) {
        assertNotNull(arch);
        assertNotNull(processor);
        assertNotEquals(arch, processor.getArch());
    }

    private void assertNotEqualsTypeNotNull(final Processor.Type type, final Processor processor) {
        assertNotNull(type);
        assertNotNull(processor);
        assertNotEquals(type, processor.getType());
    }

    @Test
    void testArch() {
        // X86
        Processor processor = ArchUtils.getProcessor(X86);
        assertEqualsTypeNotNull(Processor.Type.X86, processor);
        assertTrue(processor.isX86());
        assertNotEqualsTypeNotNull(Processor.Type.PPC, processor);
        assertFalse(processor.isPPC());
        // X86_64
        processor = ArchUtils.getProcessor(X86_64);
        assertEqualsTypeNotNull(Processor.Type.X86, processor);
        assertTrue(processor.isX86());
        // IA64_32
        processor = ArchUtils.getProcessor(IA64_32);
        assertEqualsTypeNotNull(Processor.Type.IA_64, processor);
        assertTrue(processor.isIA64());
        // IA64
        processor = ArchUtils.getProcessor(IA64);
        assertEqualsTypeNotNull(Processor.Type.IA_64, processor);
        assertTrue(processor.isIA64());
        assertNotEqualsTypeNotNull(Processor.Type.X86, processor);
        assertFalse(processor.isX86());
        // PPC
        processor = ArchUtils.getProcessor(PPC);
        assertEqualsTypeNotNull(Processor.Type.PPC, processor);
        assertTrue(processor.isPPC());
        assertNotEqualsTypeNotNull(Processor.Type.IA_64, processor);
        assertFalse(processor.isIA64());
        // PPC64
        processor = ArchUtils.getProcessor(PPC64);
        assertEqualsTypeNotNull(Processor.Type.PPC, processor);
        assertTrue(processor.isPPC());
        // AARCH_64
        processor = ArchUtils.getProcessor(AARCH_64);
        assertEqualsTypeNotNull(Processor.Type.AARCH_64, processor);
        assertTrue(processor.isAarch64());
        // RISCV_32
        processor = ArchUtils.getProcessor(RISCV_32);
        assertEqualsTypeNotNull(Processor.Type.RISC_V, processor);
        assertTrue(processor.isRISCV());
        // RISCV_64
        processor = ArchUtils.getProcessor(RISCV_64);
        assertEqualsTypeNotNull(Processor.Type.RISC_V, processor);
        assertTrue(processor.isRISCV());
    }

    @Test
    void testArchLabels() {
        for (final Arch arch : Arch.values()) {
            // Only test label presence.
            assertFalse(arch.getLabel().isEmpty());
        }
    }

    @Test
    void testGetProcessor() {
        assertNotNull(ArchUtils.getProcessor(X86));
        assertNull(ArchUtils.getProcessor("NA"));
        assertNull(ArchUtils.getProcessor(null));

        final Processor processor = ArchUtils.getProcessor();
        assertNotEquals(ObjectUtils.identityToString(processor), processor.toString());
    }

    @Test
    void testIs32BitJVM() {
        // X86
        Processor processor = ArchUtils.getProcessor(X86);
        assertEqualsArchNotNull(Processor.Arch.BIT_32, processor);
        assertTrue(processor.is32Bit());
        // IA64_32
        processor = ArchUtils.getProcessor(IA64_32);
        assertEqualsArchNotNull(Processor.Arch.BIT_32, processor);
        assertTrue(processor.is32Bit());
        // PPC
        processor = ArchUtils.getProcessor(PPC);
        assertEqualsArchNotNull(Processor.Arch.BIT_32, processor);
        processor.is32Bit();
        // RISCV_32
        processor = ArchUtils.getProcessor(RISCV_32);
        assertEqualsArchNotNull(Processor.Arch.BIT_32, processor);
        assertTrue(processor.is32Bit());
        // X86_64
        processor = ArchUtils.getProcessor(X86_64);
        assertNotEqualsArchNotNull(Processor.Arch.BIT_32, processor);
        assertFalse(processor.is32Bit());
        // PPC64
        processor = ArchUtils.getProcessor(PPC64);
        assertNotEqualsArchNotNull(Processor.Arch.BIT_32, processor);
        assertFalse(processor.is32Bit());
        // IA64
        processor = ArchUtils.getProcessor(IA64);
        assertNotEqualsArchNotNull(Processor.Arch.BIT_32, processor);
        assertFalse(processor.is32Bit());
        // RISCV_64
        processor = ArchUtils.getProcessor(RISCV_64);
        assertNotEqualsArchNotNull(Processor.Arch.BIT_32, processor);
        assertFalse(processor.is32Bit());
    }

    @Test
    void testIs64BitJVM() {
        // X86_64
        Processor processor = ArchUtils.getProcessor(X86_64);
        assertEqualsArchNotNull(Processor.Arch.BIT_64, processor);
        assertTrue(processor.is64Bit());
        // PPC64
        processor = ArchUtils.getProcessor(PPC64);
        assertEqualsArchNotNull(Processor.Arch.BIT_64, processor);
        assertTrue(processor.is64Bit());
        // IA64
        processor = ArchUtils.getProcessor(IA64);
        assertEqualsArchNotNull(Processor.Arch.BIT_64, processor);
        assertTrue(processor.is64Bit());
        // RISCV_64
        processor = ArchUtils.getProcessor(RISCV_64);
        assertEqualsArchNotNull(Processor.Arch.BIT_64, processor);
        assertTrue(processor.is64Bit());
        // X86
        processor = ArchUtils.getProcessor(X86);
        assertNotEqualsArchNotNull(Processor.Arch.BIT_64, processor);
        assertFalse(processor.is64Bit());
        // PPC
        processor = ArchUtils.getProcessor(PPC);
        assertNotEqualsArchNotNull(Processor.Arch.BIT_64, processor);
        assertFalse(processor.is64Bit());
        // IA64_32
        processor = ArchUtils.getProcessor(IA64_32);
        assertNotEqualsArchNotNull(Processor.Arch.BIT_64, processor);
        assertFalse(processor.is64Bit());
        // RISCV_32
        processor = ArchUtils.getProcessor(RISCV_32);
        assertNotEqualsArchNotNull(Processor.Arch.BIT_64, processor);
        assertFalse(processor.is64Bit());
        // AARCH_64
        processor = ArchUtils.getProcessor(AARCH_64);
        assertEqualsArchNotNull(Processor.Arch.BIT_64, processor);
        assertNotEqualsArchNotNull(Processor.Arch.BIT_32, processor);
        assertTrue(processor.is64Bit());
        assertFalse(processor.is32Bit());
    }

    @Test
    void testTypeLabels() {
        for (final Type type : Type.values()) {
            // Only test label presence.
            assertFalse(type.getLabel().isEmpty());
        }
    }

}
