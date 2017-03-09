package org.apache.commons.lang3.tuple;

import org.apache.commons.lang3.ArchUtils;
import org.apache.commons.lang3.SystemUtils;
import org.apache.commons.lang3.arch.Processor;
import org.apache.commons.lang3.arch.ProcessorArch;
import org.apache.commons.lang3.arch.ProcessorType;

import java.util.HashMap;
import java.util.Map;

/**
 *
 */
public class ArchUtilsImproved {

    private static Map<String, Processor> map;

    static {
        map = new HashMap<>();
        init();
    }

    private static final void init() {
        init_X86_32Bit();
        init_X86_64Bit();
        init_IA64_32Bit();
        init_IA64_64Bit();
        init_PPA_32Bit();
        init_PPA_64Bit();
    }

    private static final void init_X86_32Bit() {
        Processor x86 = new Processor("x86", ProcessorArch.BIT_32, ProcessorType.X86);
        map.put(x86.getName(), x86);

        Processor i386 = new Processor("i386", ProcessorArch.BIT_32, ProcessorType.X86);
        map.put(i386.getName(), i386);

        Processor i486 = new Processor("i486", ProcessorArch.BIT_32, ProcessorType.X86);
        map.put(i486.getName(), i486);

        Processor i586 = new Processor("i586", ProcessorArch.BIT_32, ProcessorType.X86);
        map.put(i586.getName(), i586);

        Processor i686 = new Processor("i686", ProcessorArch.BIT_32, ProcessorType.X86);
        map.put(i686.getName(), i686);

        Processor pentium = new Processor("pentium", ProcessorArch.BIT_32, ProcessorType.X86);
        map.put(pentium.getName(), pentium);
    }

    private static final void init_X86_64Bit() {
        Processor x86_64 = new Processor("x86_64", ProcessorArch.BIT_64, ProcessorType.X86);
        map.put(x86_64.getName(), x86_64);

        Processor amd64 = new Processor("amd64", ProcessorArch.BIT_64, ProcessorType.X86);
        map.put(amd64.getName(), amd64);

        Processor em64t = new Processor("em64t", ProcessorArch.BIT_64, ProcessorType.X86);
        map.put(em64t.getName(), em64t);

        Processor universal = new Processor("universal", ProcessorArch.BIT_64, ProcessorType.X86);
        map.put(universal.getName(), universal);
    }

    private static final void init_IA64_32Bit() {
        Processor ia64_32 = new Processor("ia64_32", ProcessorArch.BIT_32, ProcessorType.IA_64);
        map.put(ia64_32.getName(), ia64_32);

        Processor ia64n = new Processor("ia64n", ProcessorArch.BIT_32, ProcessorType.IA_64);
        map.put(ia64n.getName(), ia64n);
    }

    private static final void init_IA64_64Bit() {
        Processor ia64 = new Processor("ia64", ProcessorArch.BIT_64, ProcessorType.IA_64);
        map.put(ia64.getName(), ia64);

        Processor ia64w = new Processor("ia64w", ProcessorArch.BIT_64, ProcessorType.IA_64);
        map.put(ia64w.getName(), ia64w);
    }

    private static final void init_PPA_32Bit() {
        Processor ppc = new Processor("ppc", ProcessorArch.BIT_32, ProcessorType.PPC);
        map.put(ppc.getName(), ppc);

        Processor power = new Processor("power", ProcessorArch.BIT_32, ProcessorType.PPC);
        map.put(power.getName(), power);

        Processor powerpc = new Processor("powerpc", ProcessorArch.BIT_32, ProcessorType.PPC);
        map.put(powerpc.getName(), powerpc);

        Processor power_pc = new Processor("power_pc", ProcessorArch.BIT_32, ProcessorType.PPC);
        map.put(power_pc.getName(), power_pc);

        Processor power_rs = new Processor("power_rs", ProcessorArch.BIT_32, ProcessorType.PPC);
        map.put(power_rs.getName(), power_rs);
    }

    private static final void init_PPA_64Bit() {
        Processor ppc64 = new Processor("ppc64", ProcessorArch.BIT_64, ProcessorType.PPC);
        map.put(ppc64.getName(), ppc64);

        Processor power64 = new Processor("power64", ProcessorArch.BIT_64, ProcessorType.PPC);
        map.put(power64.getName(), power64);

        Processor powerpc64 = new Processor("powerpc64", ProcessorArch.BIT_64, ProcessorType.PPC);
        map.put(powerpc64.getName(), powerpc64);

        Processor power_pc64 = new Processor("power_pc64", ProcessorArch.BIT_64, ProcessorType.PPC);
        map.put(power_pc64.getName(), power_pc64);

        Processor power_rs64 = new Processor("power_rs64", ProcessorArch.BIT_64, ProcessorType.PPC);
        map.put(power_rs64.getName(), power_rs64);
    }

    /**
     * Checks if the current running JVM is a JVM for x86 architecture.
     * It returns <code>true</code>, if the os.arch System Property matches the following {@link String}'s:
     * <ul>
     * <li>x86</li>
     * <li>i386</li>
     * <li>i486</li>
     * <li>i586</li>
     * <li>i686</li>
     * <li>pentium</li>
     * </ul>
     * <p>
     * It is possible to extend the {@link String}'s above using method {@link ArchUtils#addX86(String)}.
     * </p>
     * <p>
     * Important: The os.arch System Property returns the architecture used by the JVM
     * not of the operating system.
     * </p>
     *
     * @return <code>True</code> if the current JVM is a x86 JVM, else <code>false</code>.
     */
    public static final boolean isX86JVM() {
        return isX86JVM(SystemUtils.OS_ARCH);
    }

    /**
     * Checks if the given {@link String} represents a x86 JVM. The {@link String} must be
     * like a value returned by the os.arch System Property.
     *
     * @param value The value to check.
     * @return <code>True</code>, if the {@link String} represents a x86 value, else <code>false</code>.
     */
    public static final boolean isX86JVM(String value) {
        return is32BitJVM(value) && ProcessorType.X86.equals(map.get(value).getProcessorType());
    }

    /**
     * Checks if the current running JVM is a JVM for x86_64 architecture.
     * It returns <code>true</code>, if the os.arch System Property matches the following {@link String}'s:
     * <ul>
     * <li>x86_64</li>
     * <li>amd64</li>
     * <li>em64t</li>
     * <li>universal</li>
     * </ul>
     * <p>
     * It is possible to extend the {@link String}'s above using method {@link ArchUtils#addX86_64(String)}.
     * </p>
     * <p>
     * Important: The os.arch System Property returns the architecture used by the JVM
     * not of the operating system.
     * </p>
     *
     * @return <code>True</code> if the current JVM is a x86_64 JVM, else <code>false</code>.
     */
    public static final boolean isX86_64JVM() {
        return isX86_64JVM(SystemUtils.OS_ARCH);
    }

    /**
     * Checks if the given {@link String} represents a x86_64 JVM. The {@link String} must be
     * like a value returned by the os.arch System Property.
     *
     * @param value The value to check.
     * @return <code>True</code>, if the {@link String} represents a x86_64 value, else <code>false</code>.
     */
    public static final boolean isX86_64JVM(String value) {
        return is64BitJVM(value) && ProcessorType.X86.equals(map.get(value).getProcessorType());
    }

    /**
     * Checks if the current running JVM is a JVM for IA64 32 bit architecture.
     * It returns <code>true</code>, if the os.arch System Property matches the following {@link String}'s:
     * <ul>
     * <li>ia64_32</li>
     * <li>ia64n</li>
     * </ul>
     * <p>
     * It is possible to extend the {@link String}'s above using method {@link ArchUtils#addIA64_32(String)}.
     * </p>
     * <p>
     * Important: The os.arch System Property returns the architecture used by the JVM
     * not of the operating system.
     * </p>
     *
     * @return <code>True</code> if the current JVM is a IA64 32 bit JVM, else <code>false</code>.
     */
    public static final boolean isIA64_32JVM() {
        return isIA64_32JVM(SystemUtils.OS_ARCH);
    }

    /**
     * Checks if the given {@link String} represents a IA64_32 JVM. The {@link String} must be
     * like a value returned by the os.arch System Property.
     *
     * @param value The value to check.
     * @return <code>True</code>, if the {@link String} represents a IA64_32 value, else <code>false</code>.
     */
    public static final boolean isIA64_32JVM(String value) {
        return is32BitJVM(value) && ProcessorType.IA_64.equals(map.get(value).getProcessorType());
    }

    /**
     * Checks if the current running JVM is a JVM for IA64 architecture.
     * It returns <code>true</code>, if the os.arch System Property matches the following {@link String}'s:
     * <ul>
     * <li>ia64</li>
     * <li>ia64w</li>
     * </ul>
     * <p>
     * It is possible to extend the {@link String}'s above using method {@link ArchUtils#addIA64(String)}.
     * </p>
     * <p>
     * Important: The os.arch System Property returns the architecture used by the JVM
     * not of the operating system.
     * </p>
     *
     * @return <code>True</code> if the current JVM is a IA64 JVM, else <code>false</code>.
     */
    public static final boolean isIA64JVM() {
        return isIA64JVM(SystemUtils.OS_ARCH);
    }

    /**
     * Possibility to add {@link String}, representing the <code>System.getProperty("os.arch")</code>
     * to IA64 architecture.
     *
     * @param value The {@link String} to add.
     */
    public static final boolean isIA64JVM(String value) {
        return is64BitJVM(value) && ProcessorType.IA_64.equals(map.get(value).getProcessorType());
    }

    /**
     * Checks if the current running JVM is a JVM for PPC architecture.
     * It returns <code>true</code>, if the os.arch System Property matches the following {@link String}'s:
     * <ul>
     * <li>ppc</li>
     * <li>powerpc</li>
     * <li>power_pc</li>
     * <li>power_rs</li>
     * </ul>
     * <p>
     * It is possible to extend the {@link String}'s above using method {@link ArchUtils#addPPC(String)}.
     * </p>
     * <p>
     * Important: The os.arch System Property returns the architecture used by the JVM
     * not of the operating system.
     * </p>
     *
     * @return <code>True</code> if the current JVM is a PPC JVM, else <code>false</code>.
     */
    public static final boolean isPPCJVM() {
        return isPPCJVM(SystemUtils.OS_ARCH);
    }

    /**
     * Checks if the given {@link String} represents a PPC JVM. The {@link String} must be
     * like a value returned by the os.arch System Property.
     *
     * @param value The value to check.
     * @return <code>True</code>, if the {@link String} represents a PPC value, else <code>false</code>.
     */
    public static final boolean isPPCJVM(String value) {
        return is32BitJVM(value) && ProcessorType.PPC.equals(map.get(value).getProcessorType());
    }

    /**
     * Checks if the current running JVM is a JVM for PPC 64 bit architecture.
     * It returns <code>true</code>, if the os.arch System Property matches the following {@link String}'s:
     * <ul>
     * <li>ppc64</li>
     * <li>powerpc64</li>
     * <li>power_pc64</li>
     * <li>power_rs64</li>
     * </ul>
     * <p>
     * It is possible to extend the {@link String}'s above using method {@link ArchUtils#addPPC64(String)}.
     * </p>
     * <p>
     * Important: The os.arch System Property returns the architecture used by the JVM
     * not of the operating system.
     * </p>
     *
     * @return <code>True</code> if the current JVM is a PPC 64 bit JVM, else <code>false</code>.
     */
    public static final boolean isPPC64JVM() {
        return isPPC64JVM(SystemUtils.OS_ARCH);
    }

    /**
     * Checks if the given {@link String} represents a PPC 64 JVM. The {@link String} must be
     * like a value returned by the os.arch System Property.
     *
     * @param value The value to check.
     * @return <code>True</code>, if the {@link String} represents a PPC 64 value, else <code>false</code>.
     */
    public static final boolean isPPC64JVM(String value) {
        return is64BitJVM(value) && ProcessorType.PPC.equals(map.get(value).getProcessorType());
    }

    /**
     * Checks if the current running JVM is a 32 bit JVM using the os.arch System Property.
     * <p>
     * Important: The os.arch System Property returns the architecture used by the JVM
     * not of the operating system.
     * </p>
     *
     * @return <code>True</code>, if the current JVM is 32 bit, else <code>false</code>.
     */
    public static final boolean is32BitJVM() {
        return is32BitJVM(SystemUtils.OS_ARCH);
    }

    /**
     * Checks if the given {@link String} represents a 32 bit JVM. The {@link String} must be
     * like a value returned by the os.arch System Property.
     *
     * @param value The value to check.
     * @return <code>True</code>, if the {@link String} represents a 32 bit value, else <code>false</code>.
     */
    public static final boolean is32BitJVM(String value) {
        if (isSupported(value)) {
            Processor processor = map.get(value);
            return ProcessorArch.BIT_32.equals(processor.getProcessorArch());
        } else {
            return false;
        }
    }

    /**
     * Checks if the current running JVM is a 64 bit JVM using the os.arch System Property.
     * <p>
     * Important: The os.arch System Property returns the architecture used by the JVM
     * not of the operating system.
     * </p>
     *
     * @return <code>True</code>, if the current JVM is 64 bit, else <code>false</code>.
     */
    public static final boolean is64BitJVM() {
        return is64BitJVM(SystemUtils.OS_ARCH);
    }

    /**
     * Checks if the given {@link String} represents a 64 bit JVM. The {@link String} must be
     * like a value returned by the os.arch System Property.
     *
     * @param value The value to check.
     * @return <code>True</code>, if the {@link String} represents a 64 bit value, else <code>false</code>.
     */
    public static final boolean is64BitJVM(String value) {
        if (isSupported(value)) {
            Processor processor = map.get(value);
            return ProcessorArch.BIT_64.equals(processor.getProcessorArch());
        } else {
            return false;
        }
    }

    /**
     * Checks if the os.arch System Property {@link String} of the current JVM is supported.
     * <p>
     * Important: The os.arch System Property returns the architecture used by the JVM
     * not of the operating system.
     * </p>
     *
     * @return <code>True</code>, if supported, else <code>false</code>.
     */
    public static final boolean isSupported() {
        return isSupported(SystemUtils.OS_ARCH);
    }

    /**
     * Checks if the given os.arch {@link String} is supported. The given {@link String} must be
     * like a value returned by the os.arch System Property.
     *
     * @param value The value to check.
     * @return <code>True</code>, if supported, else <code>false</code>.
     */
    public static final boolean isSupported(String value) {
        return map.containsKey(value);
    }

}
