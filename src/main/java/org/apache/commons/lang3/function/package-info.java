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
/**
 * Provides functional interfaces to complement those in {@code java.lang.function} and utilities for working with Java
 * 8 lambdas.
 *
 * <p>
 * Contains failable functional interfaces that address the fact that lambdas are supposed not to throw Exceptions, at
 * least not checked Exceptions, A.K.A. instances of {@link java.lang.Exception}. A failable functional interface
 * declares a type of Exception that may be raised if the function fails.
 * </p>
 *
 * @since 3.11
 */
package org.apache.commons.lang3.function;
