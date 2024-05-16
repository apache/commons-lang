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
 * Provides annotations, that are designed to aim in static code analysis,
 * and other areas of self-describing code. As of this writing, the following
 * annotations are available:
 * <dl>
 *   <dt>{@link Insecure}</dt>
 *   <dd>Indicates, that a constructor, method, or parameter should only
 *     take input, that can be considered as <em>safe</em>.
 *     The API user (the downstream developer) is supposed to ensure, by
 *     whatever means, that the input is safe, and doesn't trigger any
 *     security related issues.</dd>
 *   <dt>{@link Safe}</dt>
 *   <dd>By annotating a variable with {@code @Safe}, the API user
 *     declares, that the variable contains trusted input, that can be
 *     used as a parameter in an invocation of a constructor, or method,
 *     that is annotated with {@code @Insecure}.</dd>
 * </dl>
 * @since 3.15
 */
package org.apache.commons.lang3.annotations;
