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
import org.junit.Test;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;

/**
 * Test class for {@link ArchUtils}.
 *
 * @author Tomschi
 */
public class ArchUtilsTest {

    // x86
    private static final String X86 = "x86";
    private static final String X86_I386 = "i386";
    private static final String X86_I486 = "i486";
    private static final String X86_I586 = "i586";
    private static final String X86_I686 = "i686";
    private static final String X86_PENTIUM = "pentium";

    // x86_64
    private static final String X86_64 = "x86_64";
    private static final String X86_64_AMD64 = "amd64";
    private static final String X86_64_EM64T = "em64t";
    private static final String X86_64_UNIVERSAL = "universal";

    // IA64
    private static final String IA64 = "ia64";
    private static final String IA64W = "ia64w";

    // IA64_32
    private static final String IA64_32 = "ia64_32";
    private static final String IA64N = "ia64n";

    // PPC
    private static final String PPC = "ppc";
    private static final String POWER = "power";
    private static final String POWERPC = "powerpc";
    private static final String POWER_PC = "power_pc";
    private static final String POWER_RS = "power_rs";

    // PPC 64
    private static final String PPC64 = "ppc64";
    private static final String POWER64 = "power64";
    private static final String POWERPC64 = "powerpc64";
    private static final String POWER_PC64 = "power_pc64";
    private static final String POWER_RS64 = "power_rs64";

    private void assertEqualsArchNotNull(Processor.Arch arch, Processor processor) {
        assertNotNull(arch);
        assertNotNull(processor);
        assertEquals(arch, processor.getArch());
    }

    private void assertNotEqualsArchNotNull(Processor.Arch arch, Processor processor) {
        assertNotNull(arch);
        assertNotNull(processor);
        assertNotEquals(arch, processor.getArch());
    }

    private void assertEqualsTypeNotNull(Processor.Type type, Processor processor) {
        assertNotNull(type);
        assertNotNull(processor);
        assertEquals(type, processor.getType());
    }

    private void assertNotEqualsTypeNotNull(Processor.Type type, Processor processor) {
        assertNotNull(type);
        assertNotNull(processor);
        assertNotEquals(type, processor.getType());
    }

    @Test
    public void testIs32BitJVM() {
        assertEqualsArchNotNull(Processor.Arch.BIT_32, ArchUtils.getProcessor(X86));
        assertEqualsArchNotNull(Processor.Arch.BIT_32, ArchUtils.getProcessor(IA64_32));
        assertEqualsArchNotNull(Processor.Arch.BIT_32, ArchUtils.getProcessor(PPC));

        assertNotEqualsArchNotNull(Processor.Arch.BIT_32, ArchUtils.getProcessor(X86_64));
        assertNotEqualsArchNotNull(Processor.Arch.BIT_32, ArchUtils.getProcessor(PPC64));
        assertNotEqualsArchNotNull(Processor.Arch.BIT_32, ArchUtils.getProcessor(IA64));
    }

    @Test
    public void testIs64BitJVM() {
        assertEqualsArchNotNull(Processor.Arch.BIT_64, ArchUtils.getProcessor(X86_64));
        assertEqualsArchNotNull(Processor.Arch.BIT_64, ArchUtils.getProcessor(PPC64));
        assertEqualsArchNotNull(Processor.Arch.BIT_64, ArchUtils.getProcessor(IA64));

        assertNotEqualsArchNotNull(Processor.Arch.BIT_64, ArchUtils.getProcessor(X86));
        assertNotEqualsArchNotNull(Processor.Arch.BIT_64, ArchUtils.getProcessor(PPC));
        assertNotEqualsArchNotNull(Processor.Arch.BIT_64, ArchUtils.getProcessor(IA64_32));
    }

    @Test
    public void testArch() {
        assertEqualsTypeNotNull(Processor.Type.X86, ArchUtils.getProcessor(X86));
        assertEqualsTypeNotNull(Processor.Type.X86, ArchUtils.getProcessor(X86_64));
        assertEqualsTypeNotNull(Processor.Type.IA_64, ArchUtils.getProcessor(IA64_32));
        assertEqualsTypeNotNull(Processor.Type.IA_64, ArchUtils.getProcessor(IA64));
        assertEqualsTypeNotNull(Processor.Type.PPC, ArchUtils.getProcessor(PPC));
        assertEqualsTypeNotNull(Processor.Type.PPC, ArchUtils.getProcessor(PPC64));

        assertNotEqualsTypeNotNull(Processor.Type.X86, ArchUtils.getProcessor(IA64));
        assertNotEqualsTypeNotNull(Processor.Type.PPC, ArchUtils.getProcessor(X86));
        assertNotEqualsTypeNotNull(Processor.Type.IA_64, ArchUtils.getProcessor(PPC));
    }

    @Test
    public void testGetProcessor() {
        assertNotNull(ArchUtils.getProcessor(X86));
        assertNull(ArchUtils.getProcessor("NA"));
    }

}
