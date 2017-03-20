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
package org.apache.commons.lang3.arch;

/**
 * The {@link Processor} represents a microprocessor and defines
 * some properties like architecture and type of the microprocessor.
 */
public class Processor {

    private final ProcessorArch processorArch;
    private final ProcessorType processorType;

    /**
     * Constructs a {@link Processor}object with the given
     * parameters.
     *
     * @param processorArch The processor architecture.
     * @param processorType The processor type.
     */
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
