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

import org.junit.Test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

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
    public void testIsX86JVM() {
        assertTrue(ArchUtils.isX86JVM(X86));
        assertTrue(ArchUtils.isX86JVM(X86_I386));
        assertTrue(ArchUtils.isX86JVM(X86_I486));
        assertTrue(ArchUtils.isX86JVM(X86_I586));
        assertTrue(ArchUtils.isX86JVM(X86_I686));
        assertTrue(ArchUtils.isX86JVM(X86_PENTIUM));

        assertFalse(ArchUtils.isX86JVM(X86_64));
        assertFalse(ArchUtils.isX86JVM(IA64));
        assertFalse(ArchUtils.isX86JVM(IA64_32));
        assertFalse(ArchUtils.isX86JVM(PPC));
        assertFalse(ArchUtils.isX86JVM(PPC64));
    }

    @Test
    public void testIsX86_64JVM() {
        assertTrue(ArchUtils.isX86_64JVM(X86_64));
        assertTrue(ArchUtils.isX86_64JVM(X86_64_AMD64));
        assertTrue(ArchUtils.isX86_64JVM(X86_64_EM64T));
        assertTrue(ArchUtils.isX86_64JVM(X86_64_UNIVERSAL));

        assertFalse(ArchUtils.isX86_64JVM(X86));
        assertFalse(ArchUtils.isX86_64JVM(IA64));
        assertFalse(ArchUtils.isX86_64JVM(IA64_32));
        assertFalse(ArchUtils.isX86_64JVM(PPC));
        assertFalse(ArchUtils.isX86_64JVM(PPC64));
    }

    @Test
    public void testIsIA64JVM() {
        assertTrue(ArchUtils.isIA64JVM(IA64));
        assertTrue(ArchUtils.isIA64JVM(IA64W));

        assertFalse(ArchUtils.isIA64JVM(X86));
        assertFalse(ArchUtils.isIA64JVM(X86_64));
        assertFalse(ArchUtils.isIA64JVM(IA64_32));
        assertFalse(ArchUtils.isIA64JVM(PPC));
        assertFalse(ArchUtils.isIA64JVM(PPC64));
    }

    @Test
    public void testIsIA64_32JVM() {
        assertTrue(ArchUtils.isIA64_32JVM(IA64_32));
        assertTrue(ArchUtils.isIA64_32JVM(IA64N));

        assertFalse(ArchUtils.isIA64_32JVM(X86));
        assertFalse(ArchUtils.isIA64_32JVM(X86_64));
        assertFalse(ArchUtils.isIA64_32JVM(IA64));
        assertFalse(ArchUtils.isIA64_32JVM(PPC));
        assertFalse(ArchUtils.isIA64_32JVM(PPC64));
    }

    @Test
    public void testIsPPCJVM() {
        assertTrue(ArchUtils.isPPCJVM(PPC));
        assertTrue(ArchUtils.isPPCJVM(POWER));
        assertTrue(ArchUtils.isPPCJVM(POWERPC));
        assertTrue(ArchUtils.isPPCJVM(POWER_PC));
        assertTrue(ArchUtils.isPPCJVM(POWER_RS));

        assertFalse(ArchUtils.isPPCJVM(X86));
        assertFalse(ArchUtils.isPPCJVM(X86_64));
        assertFalse(ArchUtils.isPPCJVM(IA64));
        assertFalse(ArchUtils.isPPCJVM(IA64_32));
        assertFalse(ArchUtils.isPPCJVM(PPC64));
    }

    @Test
    public void testIsPPC64JVM() {
        assertTrue(ArchUtils.isPPC64JVM(PPC64));
        assertTrue(ArchUtils.isPPC64JVM(POWER64));
        assertTrue(ArchUtils.isPPC64JVM(POWERPC64));
        assertTrue(ArchUtils.isPPC64JVM(POWER_PC64));
        assertTrue(ArchUtils.isPPC64JVM(POWER_RS64));

        assertFalse(ArchUtils.isPPC64JVM(X86));
        assertFalse(ArchUtils.isPPC64JVM(X86_64));
        assertFalse(ArchUtils.isPPC64JVM(IA64));
        assertFalse(ArchUtils.isPPC64JVM(IA64_32));
        assertFalse(ArchUtils.isPPC64JVM(PPC));
    }

    @Test
    public void testIs32BitJVM() {
        assertTrue(ArchUtils.is32BitJVM(X86));
        assertTrue(ArchUtils.is32BitJVM(IA64_32));
        assertTrue(ArchUtils.is32BitJVM(PPC));

        assertFalse(ArchUtils.is32BitJVM(X86_64));
        assertFalse(ArchUtils.is32BitJVM(PPC64));
        assertFalse(ArchUtils.is32BitJVM(IA64));
    }

    @Test
    public void testIs64BitJVM() {
        assertTrue(ArchUtils.is64BitJVM(X86_64));
        assertTrue(ArchUtils.is64BitJVM(PPC64));
        assertTrue(ArchUtils.is64BitJVM(IA64));

        assertFalse(ArchUtils.is64BitJVM(X86));
        assertFalse(ArchUtils.is64BitJVM(PPC));
        assertFalse(ArchUtils.is64BitJVM(IA64_32));
    }

    @Test
    public void testIsSupported() {
        assertTrue(ArchUtils.isSupported(X86));
        assertFalse(ArchUtils.isSupported("NA"));
    }

    @Test
    public void testAddingCustomArch() {
        String customX86 = "custom_x86";
        ArchUtils.addX86(customX86);
        assertTrue(ArchUtils.isX86JVM(customX86));
        assertTrue(ArchUtils.is32BitJVM(customX86));
        assertTrue(ArchUtils.isSupported(customX86));
        assertFalse(ArchUtils.is64BitJVM(customX86));

        String customX86_64 = "custom_x86_64";
        ArchUtils.addX86_64(customX86_64);
        assertTrue(ArchUtils.isX86_64JVM(customX86_64));
        assertTrue(ArchUtils.is64BitJVM(customX86_64));
        assertTrue(ArchUtils.isSupported(customX86_64));
        assertFalse(ArchUtils.is32BitJVM(customX86_64));

        String customIA64_32 = "custom_ia_64_32";
        ArchUtils.addIA64_32(customIA64_32);
        assertTrue(ArchUtils.isIA64_32JVM(customIA64_32));
        assertTrue(ArchUtils.is32BitJVM(customIA64_32));
        assertTrue(ArchUtils.isSupported(customIA64_32));
        assertFalse(ArchUtils.is64BitJVM(customIA64_32));

        String customIA64 = "custom_ia64";
        ArchUtils.addIA64(customIA64);
        assertTrue(ArchUtils.isIA64JVM(customIA64));
        assertTrue(ArchUtils.is64BitJVM(customIA64));
        assertTrue(ArchUtils.isSupported(customIA64));
        assertFalse(ArchUtils.is32BitJVM(customIA64));

        String customPPA = "custom_ppa";
        ArchUtils.addPPC(customPPA);
        assertTrue(ArchUtils.isPPCJVM(customPPA));
        assertTrue(ArchUtils.is32BitJVM(customPPA));
        assertTrue(ArchUtils.isSupported(customPPA));
        assertFalse(ArchUtils.is64BitJVM(customPPA));

        String customPPA64 = "custom_ppa_64";
        ArchUtils.addPPC64(customPPA64);
        assertTrue(ArchUtils.isPPC64JVM(customPPA64));
        assertTrue(ArchUtils.is64BitJVM(customPPA64));
        assertTrue(ArchUtils.isSupported(customPPA64));
        assertFalse(ArchUtils.is32BitJVM(customPPA64));
    }

}
