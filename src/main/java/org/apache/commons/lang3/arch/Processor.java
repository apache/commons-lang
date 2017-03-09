package org.apache.commons.lang3.arch;

/**
 *
 */
public class Processor {

    private String name;
    private ProcessorArch processorArch;
    private ProcessorType processorType;

    public Processor(String name, ProcessorArch processorArch, ProcessorType processorType) {
        this.name = name;
        this.processorArch = processorArch;
        this.processorType = processorType;
    }

    public String getName() {
        return name;
    }

    public ProcessorArch getProcessorArch() {
        return processorArch;
    }

    public ProcessorType getProcessorType() {
        return processorType;
    }

}
