/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.commons.lang3.arch;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.apache.commons.lang3.arch.Processor.Arch;
import org.apache.commons.lang3.arch.Processor.Type;
import org.junit.jupiter.api.Test;

public class ProcessorTest {

    @Test
    public void testIs32Bit() {
        Processor processor = new Processor(Arch.BIT_32, Type.X86);
        assertTrue(processor.is32Bit());
        processor = new Processor(Arch.BIT_64, Type.X86);
        assertFalse(processor.is32Bit());
    }

    @Test
    public void testIs64Bit() {
        Processor processor = new Processor(Arch.BIT_64, Type.X86);
        assertTrue(processor.is64Bit());
        processor = new Processor(Arch.BIT_32, Type.X86);
        assertFalse(processor.is64Bit());
    }

    @Test
    public void testIsAarch64() {
        Processor processor = new Processor(Arch.BIT_64, Type.AARCH_64);
        assertTrue(processor.isAarch64());
        processor = new Processor(Arch.BIT_64, Type.X86);
        assertFalse(processor.isAarch64());
    }

    @Test
    public void testIsIA64() {
        Processor processor = new Processor(Arch.BIT_64, Type.IA_64);
        assertTrue(processor.isIA64());
        processor = new Processor(Arch.BIT_64, Type.X86);
        assertFalse(processor.isIA64());
    }

    @Test
    public void testIsPPC() {
        Processor processor = new Processor(Arch.BIT_64, Type.PPC);
        assertTrue(processor.isPPC());
        processor = new Processor(Arch.BIT_64, Type.X86);
        assertFalse(processor.isPPC());
    }

    @Test
    public void testIsRISCV() {
        Processor processor = new Processor(Arch.BIT_64, Type.RISC_V);
        assertTrue(processor.isRISCV());
        processor = new Processor(Arch.BIT_64, Type.X86);
        assertFalse(processor.isRISCV());
    }

    @Test
    public void testIsX86() {
        Processor processor = new Processor(Arch.BIT_32, Type.X86);
        assertTrue(processor.isX86());
        processor = new Processor(Arch.BIT_64, Type.AARCH_64);
        assertFalse(processor.isX86());
    }
}
