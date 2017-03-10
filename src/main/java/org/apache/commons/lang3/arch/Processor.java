package org.apache.commons.lang3.arch;

/**
 * The {@link Processor} represents a microprocessor and defines
 * some properties like architecture and type of the microprocessor.
 */
public class Processor {

    private final String name;
    private final ProcessorArch processorArch;
    private final ProcessorType processorType;

    public Processor(String name, ProcessorArch processorArch, ProcessorType processorType) {
        this.name = name;
        this.processorArch = processorArch;
        this.processorType = processorType;
    }

    /**
     * Returns the name of the processor. The name is the property value of
     * the os.arch system property.
     *
     * @return The name of the processor.
     */
    public String getName() {
        return name;
    }

    /**
     * Returns the processor architecture as {@link ProcessorArch} enum.
     * The processor architecture defines, if the processor is for example
     * a 32 or 64 bit architecture.
     *
     * @return A {@link ProcessorArch} enum.
     */
    public ProcessorArch getProcessorArch() {
        return processorArch;
    }

    /**
     * Returns the processor type as {@link ProcessorType} enum.
     * The processor type defines, if the processor is for example
     * a x86 or PPA.
     *
     * @return A {@link ProcessorType} enum.
     */
    public ProcessorType getProcessorType() {
        return processorType;
    }

}
