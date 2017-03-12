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
import org.junit.Test;

import static org.junit.Assert.*;

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

    @Test
    public void testIs32BitJVM() {
        Processor processor = ArchUtils.getProcessor(X86);
        assertNotNull(processor);
        assertTrue(ProcessorArch.BIT_32.equals(processor.getProcessorArch()));

        processor = ArchUtils.getProcessor(IA64_32);
        assertNotNull(processor);
        assertTrue(ProcessorArch.BIT_32.equals(processor.getProcessorArch()));

        processor = ArchUtils.getProcessor(PPC);
        assertNotNull(processor);
        assertTrue(ProcessorArch.BIT_32.equals(processor.getProcessorArch()));

        processor = ArchUtils.getProcessor(X86_64);
        assertNotNull(processor);
        assertFalse(ProcessorArch.BIT_32.equals(processor.getProcessorArch()));

        processor = ArchUtils.getProcessor(PPC64);
        assertNotNull(processor);
        assertFalse(ProcessorArch.BIT_32.equals(processor.getProcessorArch()));

        processor = ArchUtils.getProcessor(IA64);
        assertNotNull(processor);
        assertFalse(ProcessorArch.BIT_32.equals(processor.getProcessorArch()));
    }

    @Test
    public void testIs64BitJVM() {
        Processor processor = ArchUtils.getProcessor(X86_64);
        assertNotNull(processor);
        assertTrue(ProcessorArch.BIT_64.equals(processor.getProcessorArch()));

        processor = ArchUtils.getProcessor(PPC64);
        assertNotNull(processor);
        assertTrue(ProcessorArch.BIT_64.equals(processor.getProcessorArch()));

        processor = ArchUtils.getProcessor(IA64);
        assertNotNull(processor);
        assertTrue(ProcessorArch.BIT_64.equals(processor.getProcessorArch()));

        processor = ArchUtils.getProcessor(X86);
        assertNotNull(processor);
        assertFalse(ProcessorArch.BIT_64.equals(processor.getProcessorArch()));

        processor = ArchUtils.getProcessor(PPC);
        assertNotNull(processor);
        assertFalse(ProcessorArch.BIT_64.equals(processor.getProcessorArch()));

        processor = ArchUtils.getProcessor(IA64_32);
        assertNotNull(processor);
        assertFalse(ProcessorArch.BIT_64.equals(processor.getProcessorArch()));
    }

    @Test
    public void testGetProcessor() {
        assertNotNull(ArchUtils.getProcessor(X86));
        assertNull(ArchUtils.getProcessor("NA"));
    }

}
