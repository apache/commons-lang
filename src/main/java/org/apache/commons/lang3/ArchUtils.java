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

import java.util.HashMap;
import java.util.Map;

/**
 * An utility class for the os.arch System Property. The class defines methods for
 * identifying the architecture of the current JVM.
 *
 * <p>
 * Important: The os.arch System Property returns the architecture used by the JVM
 * not of the operating system.
 * </p>
 *
 * @author Tomschi
 */
public class ArchUtils {

	/**
	 * This {@link Map} contains the the possible os.arch property {@link String}'s.
	 */
	private static final Map<String, String> map;

	/**
	 * The value for x86 architecture.
	 */
	private static final String X86 = "x86";

	/**
	 * The value for x86_64 architecture.
	 */
	private static final String X86_64 = "x86_64";

	/**
	 * The value for ia64_32 architecture.
	 */
	private static final String IA64_32 = "ia64_32";

	/**
	 * The value for ia64 architecture.
	 */
	private static final String IA64 = "ia64";

	/**
	 * The value for ppc architecture.
	 */
	private static final String PPC = "ppc";

	/**
	 * The value for ppc64 architecture.
	 */
	private static final String PPC64 = "ppc64";

	static {
		map = new HashMap<>();

		// x86 mappings
		addX86(X86);
		addX86("i386");
		addX86("i486");
		addX86("i586");
		addX86("i686");
		addX86("pentium");

		// x86_64 mappings
		addX86_64(X86_64);
		addX86_64("amd64");
		addX86_64("em64t");
		addX86_64("universal"); // Needed for openjdk7 in Mac

		// Itenium 64-bit mappings
		addIA64(IA64);
		addIA64("ia64w");

		// Itenium 32-bit mappings, usually an HP-UX construct
		addIA64_32(IA64_32);
		addIA64_32("ia64n");

		// PowerPC mappings
		addPPC(PPC);
		addPPC("power");
		addPPC("powerpc");
		addPPC("power_pc");
		addPPC("power_rs");

		// PowerPC 64bit mappings
		addPPC64(PPC64);
		addPPC64("power64");
		addPPC64("powerpc64");
		addPPC64("power_pc64");
		addPPC64("power_rs64");
	}

	/**
	 * Possibility to add {@link String}, representing the <code>System.getProperty("os.arch")</code>
	 * to x86 architecture.
	 *
	 * @param value The {@link String} to add.
	 */
	public static final void addX86(String value) {
		map.put(value, X86);
	}

	/**
	 * Checks if the current running JVM is a JVM for x86 architecture.
	 * It returns <code>true</code>, if the os.arch System Property matches the following {@link String}'s:
	 * <ul>
	 *     <li>x86</li>
	 *     <li>i386</li>
	 *     <li>i486</li>
	 *     <li>i586</li>
	 *     <li>i686</li>
	 *     <li>pentium</li>
	 * </ul>
	 *
	 * It is possible to extend the {@link String}'s above using method {@link ArchUtils#addX86(String)}.
	 *
	 * <p>
	 * Important: The os.arch System Property returns the architecture used by the JVM
	 * not of the operating system.
	 * </p>
	 *
	 * @return <code>True</code> if the current JVM is a x86 JVM, else <code>false</code>.
	 *
	 */
	public static final boolean isX86JVM() {
		return isX86JVM(SystemUtils.OS_ARCH);
	}

	/**
	 * Checks if the given {@link String} represents a x86 JVM. The {@link String} must be
	 * like a value returned by the os.arch System Property.
	 *
	 * @param value The value to check.
	 *
	 * @return <code>True</code>, if the {@link String} represents a x86 value, else <code>false</code>.
	 */
	public static final boolean isX86JVM(String value) {
		return StringUtils.equals(X86, map.get(value));
	}

	/**
	 * Possibility to add {@link String}, representing the <code>System.getProperty("os.arch")</code>
	 * to x86_64 architecture.
	 *
	 * @param value The {@link String} to add.
	 */
	public static final void addX86_64(String value) {
		map.put(value, X86_64);
	}

	/**
	 * Checks if the current running JVM is a JVM for x86_64 architecture.
	 * It returns <code>true</code>, if the os.arch System Property matches the following {@link String}'s:
	 * <ul>
	 *     <li>x86_64</li>
	 *     <li>amd64</li>
	 *     <li>em64t</li>
	 *     <li>universal</li>
	 * </ul>
	 *
	 * It is possible to extend the {@link String}'s above using method {@link ArchUtils#addX86_64(String)}.
	 *
	 * <p>
	 * Important: The os.arch System Property returns the architecture used by the JVM
	 * not of the operating system.
	 * </p>
	 *
	 * @return <code>True</code> if the current JVM is a x86_64 JVM, else <code>false</code>.
	 *
	 */
	public static final boolean isX86_64JVM() {
		return isX86_64JVM(SystemUtils.OS_ARCH);
	}

	/**
	 * Checks if the given {@link String} represents a x86_64 JVM. The {@link String} must be
	 * like a value returned by the os.arch System Property.
	 *
	 * @param value The value to check.
	 *
	 * @return <code>True</code>, if the {@link String} represents a x86_64 value, else <code>false</code>.
	 */
	public static final boolean isX86_64JVM(String value) {
		return StringUtils.equals(X86_64, map.get(value));
	}

	/**
	 * Possibility to add {@link String}, representing the <code>System.getProperty("os.arch")</code>
	 * to IA32_64 architecture.
	 *
	 * @param value The {@link String} to add.
	 */
	public static final void addIA64_32(String value) {
		map.put(value, IA64_32);
	}

	/**
	 * Checks if the current running JVM is a JVM for IA64 32 bit architecture.
	 * It returns <code>true</code>, if the os.arch System Property matches the following {@link String}'s:
	 * <ul>
	 *     <li>ia64_32</li>
	 *     <li>ia64n</li>
	 * </ul>
	 *
	 * It is possible to extend the {@link String}'s above using method {@link ArchUtils#addIA64_32(String)}.
	 *
	 * <p>
	 * Important: The os.arch System Property returns the architecture used by the JVM
	 * not of the operating system.
	 * </p>
	 *
	 * @return <code>True</code> if the current JVM is a IA64 32 bit JVM, else <code>false</code>.
	 *
	 */
	public static final boolean isIA64_32JVM() {
		return isIA64_32JVM(SystemUtils.OS_ARCH);
	}

	/**
	 * Checks if the given {@link String} represents a IA64_32 JVM. The {@link String} must be
	 * like a value returned by the os.arch System Property.
	 *
	 * @param value The value to check.
	 *
	 * @return <code>True</code>, if the {@link String} represents a IA64_32 value, else <code>false</code>.
	 */
	public static final boolean isIA64_32JVM(String value) {
		return StringUtils.equals(IA64_32, map.get(value));
	}

	/**
	 * Possibility to add {@link String}, representing the <code>System.getProperty("os.arch")</code>
	 * to IA64 architecture.
	 *
	 * @param value The {@link String} to add.
	 */
	public static final void addIA64(String value) {
		map.put(value, IA64);
	}

	/**
	 * Checks if the current running JVM is a JVM for IA64 architecture.
	 * It returns <code>true</code>, if the os.arch System Property matches the following {@link String}'s:
	 * <ul>
	 *     <li>ia64</li>
	 *     <li>ia64w</li>
	 * </ul>
	 *
	 * It is possible to extend the {@link String}'s above using method {@link ArchUtils#addIA64(String)}.
	 *
	 * <p>
	 * Important: The os.arch System Property returns the architecture used by the JVM
	 * not of the operating system.
	 * </p>
	 *
	 * @return <code>True</code> if the current JVM is a IA64 JVM, else <code>false</code>.
	 *
	 */
	public static final boolean isIA64JVM() {
		return isIA64JVM(SystemUtils.OS_ARCH);
	}

	/**
	 * Checks if the given {@link String} represents a IA64 JVM. The {@link String} must be
	 * like a value returned by the os.arch System Property.
	 *
	 * @param value The value to check.
	 *
	 * @return <code>True</code>, if the {@link String} represents a IA64 value, else <code>false</code>.
	 */
	public static final boolean isIA64JVM(String value) {
		return StringUtils.equals(IA64, map.get(value));
	}

	/**
	 * Possibility to add {@link String}, representing the <code>System.getProperty("os.arch")</code>
	 * to PPC architecture.
	 *
	 * @param value The {@link String} to add.
	 */
	public static final void addPPC(String value) {
		map.put(value, PPC);
	}

	/**
	 * Checks if the current running JVM is a JVM for PPC architecture.
	 * It returns <code>true</code>, if the os.arch System Property matches the following {@link String}'s:
	 * <ul>
	 *     <li>ppc</li>
	 *     <li>powerpc</li>
	 *     <li>power_pc</li>
	 *     <li>power_rs</li>
	 * </ul>
	 *
	 * It is possible to extend the {@link String}'s above using method {@link ArchUtils#addPPC(String)}.
	 *
	 * <p>
	 * Important: The os.arch System Property returns the architecture used by the JVM
	 * not of the operating system.
	 * </p>
	 *
	 * @return <code>True</code> if the current JVM is a PPC JVM, else <code>false</code>.
	 *
	 */
	public static final boolean isPPCJVM() {
		return isPPCJVM(SystemUtils.OS_ARCH);
	}

	/**
	 * Checks if the given {@link String} represents a PPC JVM. The {@link String} must be
	 * like a value returned by the os.arch System Property.
	 *
	 * @param value The value to check.
	 *
	 * @return <code>True</code>, if the {@link String} represents a PPC value, else <code>false</code>.
	 */
	public static final boolean isPPCJVM(String value) {
		return StringUtils.equals(PPC, map.get(value));
	}

	/**
	 * Possibility to add {@link String}, representing the <code>System.getProperty("os.arch")</code>
	 * to PPC_64 architecture.
	 *
	 * @param value The {@link String} to add.
	 */
	public static final void addPPC64(String value) {
		map.put(value, PPC64);
	}

	/**
	 * Checks if the current running JVM is a JVM for PPC 64 bit architecture.
	 * It returns <code>true</code>, if the os.arch System Property matches the following {@link String}'s:
	 * <ul>
	 *     <li>ppc64</li>
	 *     <li>powerpc64</li>
	 *     <li>power_pc64</li>
	 *     <li>power_rs64</li>
	 * </ul>
	 *
	 * It is possible to extend the {@link String}'s above using method {@link ArchUtils#addPPC64(String)}.
	 *
	 * <p>
	 * Important: The os.arch System Property returns the architecture used by the JVM
	 * not of the operating system.
	 * </p>
	 *
	 * @return <code>True</code> if the current JVM is a PPC 64 bit JVM, else <code>false</code>.
	 *
	 */
	public static final boolean isPPC64JVM() {
		return isPPC64JVM(SystemUtils.OS_ARCH);
	}

	/**
	 * Checks if the given {@link String} represents a PPC 64 JVM. The {@link String} must be
	 * like a value returned by the os.arch System Property.
	 *
	 * @param value The value to check.
	 *
	 * @return <code>True</code>, if the {@link String} represents a PPC 64 value, else <code>false</code>.
	 */
	public static final boolean isPPC64JVM(String value) {
		return StringUtils.equals(PPC64, map.get(value));
	}

	/**
	 * Checks if the current running JVM is a 32 bit JVM using the os.arch System Property.
	 *
	 * <p>
	 * Important: The os.arch System Property returns the architecture used by the JVM
	 * not of the operating system.
	 * </p>
	 *
	 * @return <code>True</code>, if the current JVM is 32 bit, else <code>false</code>.
	 *
	 */
	public static final boolean is32BitJVM() {
		return isX86JVM() || isIA64_32JVM() || isPPCJVM();
	}

	/**
	 * Checks if the given {@link String} represents a 32 bit JVM. The {@link String} must be
	 * like a value returned by the os.arch System Property.
	 *
	 * @param value The value to check.
	 *
	 * @return <code>True</code>, if the {@link String} represents a 32 bit value, else <code>false</code>.
	 */
	public static final boolean is32BitJVM(String value) {
		return isX86JVM(value) || isIA64_32JVM(value) || isPPCJVM(value);
	}

	/**
	 * Checks if the current running JVM is a 64 bit JVM using the os.arch System Property.
	 *
	 * <p>
	 * Important: The os.arch System Property returns the architecture used by the JVM
	 * not of the operating system.
	 * </p>
	 *
	 * @return <code>True</code>, if the current JVM is 64 bit, else <code>false</code>.
	 *
	 */
	public static final boolean is64BitJVM() {
		return isX86_64JVM() || isIA64JVM() || isPPC64JVM();
	}

	/**
	 * Checks if the given {@link String} represents a 64 bit JVM. The {@link String} must be
	 * like a value returned by the os.arch System Property.
	 *
	 * @param value The value to check.
	 *
	 * @return <code>True</code>, if the {@link String} represents a 64 bit value, else <code>false</code>.
	 */
	public static final boolean is64BitJVM(String value) {
		return isX86_64JVM(value) || isIA64JVM(value) || isPPC64JVM(value);
	}

	/**
	 * Checks if the os.arch System Property {@link String} of the current JVM is supported.
	 *
	 * <p>
	 * Important: The os.arch System Property returns the architecture used by the JVM
	 * not of the operating system.
	 * </p>
	 *
	 * @return <code>True</code>, if supported, else <code>false</code>.
	 */
	public static final boolean isSupported() {
		return is32BitJVM() || is64BitJVM();
	}

	/**
	 * Checks if the given os.arch {@link String} is supported. The given {@link String} must be
	 * like a value returned by the os.arch System Property.
	 *
	 * @return <code>True</code>, if supported, else <code>false</code>.
	 */
	public static final boolean isSupported(String value) {
		return is32BitJVM(value) || is64BitJVM(value);
	}

}
