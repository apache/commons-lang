package org.apache.commons.lang3.arch;

/**
 * The {@link Processor} represents a microprocessor and defines
 * some properties like architecture and type of the microprocessor.
 */
public class Processor {

    private final ProcessorArch processorArch;
    private final ProcessorType processorType;

    public Processor(ProcessorArch processorArch, ProcessorType processorType) {
        this.processorArch = processorArch;
        this.processorType = processorType;
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
