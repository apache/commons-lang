/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang3.builder;

import java.util.Collection;
import java.util.concurrent.atomic.AtomicBoolean;

import org.apache.commons.lang3.ClassUtils;
import org.apache.commons.lang3.mutable.MutableBoolean;

/**
 * Works with {@link ToStringBuilder} to create a "deep" {@code toString}.
 *
 * <p>To use this class write code as follows:</p>
 *
 * <pre>
 * public class Job {
 *   String title;
 *   ...
 * }
 *
 * public class Person {
 *   String name;
 *   int age;
 *   boolean smoker;
 *   Job job;
 *
 *   ...
 *
 *   public String toString() {
 *     return new ReflectionToStringBuilder(this, new RecursiveToStringStyle()).toString();
 *   }
 * }
 * </pre>
 *
 * <p>This will produce a toString of the format:
 * {@code Person@7f54[name=Stephen,age=29,smoker=false,job=Job@43cd2[title=Manager]]}</p>
 *
 * @since 3.2
 */
public class RecursiveToStringStyle extends ToStringStyle {

    /**
     * Required for serialization support.
     *
     * @see java.io.Serializable
     */
    private static final long serialVersionUID = 1L;

    /**
     * Constructs a new instance.
     */
    public RecursiveToStringStyle() {
    }

    /**
     * Tests whether or not to recursively format the given {@link Class}.
     * <p>
     * By default, this method always filters out the following:
     * </p>
     * <ul>
     * <li><a href="https://docs.oracle.com/javase/specs/jls/se25/html/jls-5.html#jls-5.1.7">Boxed primitives</a>, see {@link ClassUtils#isPrimitiveWrapper(Class)}
     * <li>{@link String}</li>
     * <li>{@link Number} subclasses</li>
     * <li>{@link AtomicBoolean}</li>
     * <li>{@link MutableBoolean}</li>
     * </ul>
     *
     * @param clazz The class to test.
     * @return Whether or not to recursively format instances of the given {@link Class}.
     */
    protected boolean accept(final Class<?> clazz) {
        // @formatter:off
        return !ClassUtils.isPrimitiveWrapper(clazz) &&
               !String.class.equals(clazz) &&
               !Number.class.isAssignableFrom(clazz) &&
               !AtomicBoolean.class.equals(clazz) &&
               !MutableBoolean.class.equals(clazz);
        // @formatter:on
    }

    @Override
    protected void appendDetail(final StringBuffer buffer, final String fieldName, final Collection<?> coll) {
        appendClassName(buffer, coll);
        appendIdentityHashCode(buffer, coll);
        appendDetail(buffer, fieldName, coll.toArray());
    }

    @Override
    public void appendDetail(final StringBuffer buffer, final String fieldName, final Object value) {
        if (value != null && accept(value.getClass())) {
            buffer.append(ReflectionToStringBuilder.toString(value, this));
        } else {
            super.appendDetail(buffer, fieldName, value);
        }
    }
}
