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

package org.apache.commons.lang3.arch;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;

class ProcessorTV04Test {
    @Test void identifies32BitX86() {
        Processor p = new Processor(Processor.Arch.BIT_32, Processor.Type.X86);
        assertAll(() -> assertTrue(p.is32Bit()), () -> assertFalse(p.is64Bit()),
                () -> assertTrue(p.isX86()), () -> assertFalse(p.isAarch64()),
                () -> assertEquals("x86 32-bit", p.toString()));
    }
    @Test void identifies64BitArchitectures() {
        Processor a = new Processor(Processor.Arch.BIT_64, Processor.Type.AARCH_64);
        Processor r = new Processor(Processor.Arch.BIT_64, Processor.Type.RISC_V);
        assertAll(() -> assertTrue(a.is64Bit()), () -> assertTrue(a.isAarch64()),
                () -> assertTrue(r.isRISCV()), () -> assertFalse(r.isPPC()),
                () -> assertEquals("RISC-V 64-bit", r.toString()));
    }
    @Test void unknownValuesDoNotMatchKnownPredicates() {
        Processor p = new Processor(Processor.Arch.UNKNOWN, Processor.Type.UNKNOWN);
        assertAll(() -> assertFalse(p.is32Bit()), () -> assertFalse(p.is64Bit()),
                () -> assertFalse(p.isX86()), () -> assertFalse(p.isIA64()),
                () -> assertFalse(p.isPPC()), () -> assertFalse(p.isRISCV()));
    }
    @Test void exposesEnumLabels() {
        assertEquals("Unknown", Processor.Arch.UNKNOWN.getLabel());
        assertEquals("IA-64", Processor.Type.IA_64.getLabel());
    }
}
