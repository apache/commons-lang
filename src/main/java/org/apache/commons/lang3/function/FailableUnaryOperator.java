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
package org.apache.commons.lang3.function;

import org.apache.commons.lang3.Functions;

import java.util.function.UnaryOperator;

/**
 * Represents an operation on a single operand that produces a result of the
 * same type as its operand.  This is a specialization of {@code FailableFunction}
 * for the case where the operand and result are of the same type.
 *
 * <p>This is a functional interface whose functional method is {@link #apply(Object)}.
 *
 * @param <I> the type of the operand and the result of the operator
 * @param <T> the type of exception to be thrown
 *
 * @see UnaryOperator
 * @see Functions.FailableFunction
 * @since 3.10
 */
public interface FailableUnaryOperator<I, T extends Throwable> extends Functions.FailableFunction<I, I, T> {
}
