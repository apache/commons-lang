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
package org.apache.commons.lang3.stream;

import java.util.OptionalDouble;
import java.util.stream.LongStream;

/**
 * A reduced and simplified version of a {@link LongStream} with
 * failable method signatures.
 *
 * @see LongStream
 * @since 3.10
 */
public class FailableLongStream extends FailableBaseStream {
    private LongStream longStream;

    public FailableLongStream(LongStream longStream) {
        this.longStream = longStream;
    }

    /**
     * Returns the average of elements in this stream.
     *
     * <p>This is a terminal operation.
     *
     * @return the average of elements in this stream
     */
    public OptionalDouble average() {
        makeTerminated();
        return longStream.average();
    }

    /**
     * Returns a {@code FailableStream} consisting of the elements of this stream, each boxed to a Long.
     *
     * @return a FailableStream consisting of the elements of this stream, each boxed to a Long
     */
    public FailableStream<Long> boxed() {
        return new FailableStream<>(longStream.boxed());
    }

    /**
     * Returns the sum of elements in this stream.
     *
     * This is a special case of a reduction and is equivalent to:
     * <pre>{@code
     *     return reduce(0, Long::sum);
     * }</pre>
     *
     * <p>This is a terminal operation.
     *
     * @return the sum of elements in this stream
     */
    public long sum() {
        makeTerminated();
        return longStream.sum();
    }
}
