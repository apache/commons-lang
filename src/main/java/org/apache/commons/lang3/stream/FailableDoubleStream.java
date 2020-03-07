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
import java.util.stream.DoubleStream;

/**
 * A reduced and simplified version of a {@link DoubleStream} with
 * failable method signatures.
 *
 * @see DoubleStream
 * @since 3.10
 */
public class FailableDoubleStream extends FailableBaseStream<Double, FailableDoubleStream> {
    private DoubleStream doubleStream;

    public FailableDoubleStream(DoubleStream doubleStream) {
        this.doubleStream = doubleStream;
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
        return doubleStream.average();
    }

    /**
     * Returns a {@code FailableStream} consisting of the elements of this stream, each boxed to a Double.
     *
     * @return a FailableStream consisting of the elements of this stream, each boxed to a Double
     */
    public FailableStream<Double> boxed() {
        return new FailableStream<>(doubleStream.boxed());
    }

    /**
     * Returns the sum of elements in this stream.
     * <p>
     * Summation is a special case of a reduction. If
     * floating-point summation were exact, this method would be
     * equivalent to:
     *
     * <pre>{@code
     *     return reduce(0, Double::sum);
     * }</pre>
     * <p>
     * However, since floating-point summation is not exact, the above
     * code is not necessarily equivalent to the summation computation
     * done by this method.
     *
     * <p>The value of a floating-point sum is a function both
     * of the input values as well as the order of addition
     * operations. The order of addition operations of this method is
     * intentionally not defined to allow for implementation
     * flexibility to improve the speed and accuracy of the computed
     * result.
     * <p>
     * In particular, this method may be implemented using compensated
     * summation or other technique to reduce the error bound in the
     * numerical sum compared to a simple summation of {@code double}
     * values.
     * <p>
     * Because of the unspecified order of operations and the
     * possibility of using differing summation schemes, the output of
     * this method may vary on the same input elements.
     *
     * <p>Various conditions can result in a non-finite sum being
     * computed. This can occur even if the all the elements
     * being summed are finite. If any element is non-finite,
     * the sum will be non-finite:
     *
     * <ul>
     *
     * <li>If any element is a NaN, then the final sum will be
     * NaN.
     *
     * <li>If the elements contain one or more infinities, the
     * sum will be infinite or NaN.
     *
     * <ul>
     *
     * <li>If the elements contain infinities of opposite sign,
     * the sum will be NaN.
     *
     * <li>If the elements contain infinities of one sign and
     * an intermediate sum overflows to an infinity of the opposite
     * sign, the sum may be NaN.
     *
     * </ul>
     *
     * </ul>
     * <p>
     * It is possible for intermediate sums of finite values to
     * overflow into opposite-signed infinities; if that occurs, the
     * final sum will be NaN even if the elements are all
     * finite.
     * <p>
     * If all the elements are zero, the sign of zero is
     * <em>not</em> guaranteed to be preserved in the final sum.
     *
     * <p>This is a terminal
     * operation.
     *
     * @return the sum of elements in this stream
     * @apiNote Elements sorted by increasing absolute magnitude tend
     * to yield more accurate results.
     */
    public double sum() {
        makeTerminated();
        return doubleStream.sum();
    }

    @Override
    public long count() {
        return 0;
    }

    @Override
    public Iterator<Double> iterator() {
        return null;
    }

    @Override
    public Spliterator<Double> spliterator() {
        return null;
    }

    @Override
    public FailableDoubleStream distinct() {
        return null;
    }

    @Override
    public FailableDoubleStream sequential() {
        return null;
    }

    @Override
    public FailableDoubleStream parallel() {
        return null;
    }
}
