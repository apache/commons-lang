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

import org.apache.commons.lang3.tuple.ArchUtilsImproved;
import org.junit.Test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

/**
 * Test class for {@link ArchUtilsImproved}.
 *
 * @author Tomschi
 */
public class ArchUtilsImprovedTest {

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
        assertTrue(ArchUtilsImproved.isX86JVM(X86));
        assertTrue(ArchUtilsImproved.isX86JVM(X86_I386));
        assertTrue(ArchUtilsImproved.isX86JVM(X86_I486));
        assertTrue(ArchUtilsImproved.isX86JVM(X86_I586));
        assertTrue(ArchUtilsImproved.isX86JVM(X86_I686));
        assertTrue(ArchUtilsImproved.isX86JVM(X86_PENTIUM));

        assertFalse(ArchUtilsImproved.isX86JVM(X86_64));
        assertFalse(ArchUtilsImproved.isX86JVM(IA64));
        assertFalse(ArchUtilsImproved.isX86JVM(IA64_32));
        assertFalse(ArchUtilsImproved.isX86JVM(PPC));
        assertFalse(ArchUtilsImproved.isX86JVM(PPC64));
    }

    @Test
    public void testIsX86_64JVM() {
        assertTrue(ArchUtilsImproved.isX86_64JVM(X86_64));
        assertTrue(ArchUtilsImproved.isX86_64JVM(X86_64_AMD64));
        assertTrue(ArchUtilsImproved.isX86_64JVM(X86_64_EM64T));
        assertTrue(ArchUtilsImproved.isX86_64JVM(X86_64_UNIVERSAL));

        assertFalse(ArchUtilsImproved.isX86_64JVM(X86));
        assertFalse(ArchUtilsImproved.isX86_64JVM(IA64));
        assertFalse(ArchUtilsImproved.isX86_64JVM(IA64_32));
        assertFalse(ArchUtilsImproved.isX86_64JVM(PPC));
        assertFalse(ArchUtilsImproved.isX86_64JVM(PPC64));
    }

    @Test
    public void testIsIA64JVM() {
        assertTrue(ArchUtilsImproved.isIA64JVM(IA64));
        assertTrue(ArchUtilsImproved.isIA64JVM(IA64W));

        assertFalse(ArchUtilsImproved.isIA64JVM(X86));
        assertFalse(ArchUtilsImproved.isIA64JVM(X86_64));
        assertFalse(ArchUtilsImproved.isIA64JVM(IA64_32));
        assertFalse(ArchUtilsImproved.isIA64JVM(PPC));
        assertFalse(ArchUtilsImproved.isIA64JVM(PPC64));
    }

    @Test
    public void testIsIA64_32JVM() {
        assertTrue(ArchUtilsImproved.isIA64_32JVM(IA64_32));
        assertTrue(ArchUtilsImproved.isIA64_32JVM(IA64N));

        assertFalse(ArchUtilsImproved.isIA64_32JVM(X86));
        assertFalse(ArchUtilsImproved.isIA64_32JVM(X86_64));
        assertFalse(ArchUtilsImproved.isIA64_32JVM(IA64));
        assertFalse(ArchUtilsImproved.isIA64_32JVM(PPC));
        assertFalse(ArchUtilsImproved.isIA64_32JVM(PPC64));
    }

    @Test
    public void testIsPPCJVM() {
        assertTrue(ArchUtilsImproved.isPPCJVM(PPC));
        assertTrue(ArchUtilsImproved.isPPCJVM(POWER));
        assertTrue(ArchUtilsImproved.isPPCJVM(POWERPC));
        assertTrue(ArchUtilsImproved.isPPCJVM(POWER_PC));
        assertTrue(ArchUtilsImproved.isPPCJVM(POWER_RS));

        assertFalse(ArchUtilsImproved.isPPCJVM(X86));
        assertFalse(ArchUtilsImproved.isPPCJVM(X86_64));
        assertFalse(ArchUtilsImproved.isPPCJVM(IA64));
        assertFalse(ArchUtilsImproved.isPPCJVM(IA64_32));
        assertFalse(ArchUtilsImproved.isPPCJVM(PPC64));
    }

    @Test
    public void testIsPPC64JVM() {
        assertTrue(ArchUtilsImproved.isPPC64JVM(PPC64));
        assertTrue(ArchUtilsImproved.isPPC64JVM(POWER64));
        assertTrue(ArchUtilsImproved.isPPC64JVM(POWERPC64));
        assertTrue(ArchUtilsImproved.isPPC64JVM(POWER_PC64));
        assertTrue(ArchUtilsImproved.isPPC64JVM(POWER_RS64));

        assertFalse(ArchUtilsImproved.isPPC64JVM(X86));
        assertFalse(ArchUtilsImproved.isPPC64JVM(X86_64));
        assertFalse(ArchUtilsImproved.isPPC64JVM(IA64));
        assertFalse(ArchUtilsImproved.isPPC64JVM(IA64_32));
        assertFalse(ArchUtilsImproved.isPPC64JVM(PPC));
    }

    @Test
    public void testIs32BitJVM() {
        assertTrue(ArchUtilsImproved.is32BitJVM(X86));
        assertTrue(ArchUtilsImproved.is32BitJVM(IA64_32));
        assertTrue(ArchUtilsImproved.is32BitJVM(PPC));

        assertFalse(ArchUtilsImproved.is32BitJVM(X86_64));
        assertFalse(ArchUtilsImproved.is32BitJVM(PPC64));
        assertFalse(ArchUtilsImproved.is32BitJVM(IA64));
    }

    @Test
    public void testIs64BitJVM() {
        assertTrue(ArchUtilsImproved.is64BitJVM(X86_64));
        assertTrue(ArchUtilsImproved.is64BitJVM(PPC64));
        assertTrue(ArchUtilsImproved.is64BitJVM(IA64));

        assertFalse(ArchUtilsImproved.is64BitJVM(X86));
        assertFalse(ArchUtilsImproved.is64BitJVM(PPC));
        assertFalse(ArchUtilsImproved.is64BitJVM(IA64_32));
    }

    @Test
    public void testIsSupported() {
        assertTrue(ArchUtilsImproved.isSupported(X86));
        assertFalse(ArchUtilsImproved.isSupported("NA"));
    }

}
