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

import java.util.Iterator;
import java.util.OptionalDouble;
import java.util.Spliterator;
import java.util.stream.IntStream;

/**
 * A reduced and simplified version of an {@link IntStream} with
 * failable method signatures.
 *
 * @see IntStream
 * @since 3.10
 */
public class FailableIntStream extends FailableBaseStream<Integer, FailableIntStream> {
    private IntStream intStream;

    public FailableIntStream(IntStream intStream) {
        this.intStream = intStream;
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
        return intStream.average();
    }

    /**
     * Returns a {@code FailableStream} consisting of the elements of this stream, each boxed to an Integer.
     *
     * @return a FailableStream consisting of the elements of this stream, each boxed to an Integer
     */
    public FailableStream<Integer> boxed() {
        return new FailableStream<>(intStream.boxed());
    }

    /**
     * Returns the sum of elements in this stream.
     *
     * This is a special case of a reduction and is equivalent to:
     * <pre>{@code
     *     return reduce(0, Integer::sum);
     * }</pre>
     *
     * <p>This is a terminal operation.
     *
     * @return the sum of elements in this stream
     */
    public int sum() {
        makeTerminated();
        return intStream.sum();
    }

    @Override
    public long count() {
        return 0;
    }

    @Override
    public Iterator<Integer> iterator() {
        return null;
    }

    @Override
    public Spliterator<Integer> spliterator() {
        return null;
    }

    @Override
    public FailableIntStream distinct() {
        return null;
    }

    @Override
    public FailableIntStream sequential() {
        return null;
    }

    @Override
    public FailableIntStream parallel() {
        return null;
    }
}
