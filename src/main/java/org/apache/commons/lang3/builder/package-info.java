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
 * Assists in creating consistent {@code equals(Object)}, {@code toString()}, {@code hashCode()}, and {@code compareTo(Object)} methods.
 * These classes are not thread-safe.
 *
 * <p>When you write a {@link java.lang.Object#hashCode() hashCode()}, do you check Bloch's Effective Java? No?
 * You just hack in a quick number?
 * Well {@link org.apache.commons.lang3.builder.HashCodeBuilder} will save your day.
 * It, and its buddies ({@link org.apache.commons.lang3.builder.EqualsBuilder}, {@link org.apache.commons.lang3.builder.CompareToBuilder}, {@link org.apache.commons.lang3.builder.ToStringBuilder}), take care of the nasty bits while you focus on the important bits, like which fields will go into making up the hashcode.</p>
 *
 * @see Object#equals(Object)
 * @see Object#toString()
 * @see Object#hashCode()
 * @see Comparable#compareTo(Object)
 *
 * @since 1.0
 */
package org.apache.commons.lang3.builder;
