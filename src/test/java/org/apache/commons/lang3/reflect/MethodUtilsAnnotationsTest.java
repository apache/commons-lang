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

package org.apache.commons.lang3.reflect;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.lang.reflect.Method;

import org.junit.jupiter.api.Test;

/**
 * Tests {@link MethodUtils#getAnnotation(Method, Class, boolean, boolean)}.
 * <p>
 * getMatchingMethod allows assignable params, potentially finding annotations on unrelated overloads.
 * </p>
 */
public class MethodUtilsAnnotationsTest {

    /** Interface with a method taking Number, annotated @Deprecated */
    public interface Processor {

        @SuppressWarnings("javadoc")
        @Deprecated
        void process(Number n);
    }

    /** Implementation that does NOT annotate process(Integer) */
    public static class ProcessorImpl implements Processor {

        // Overload with Integer — NOT annotated
        @SuppressWarnings("javadoc")
        public void process(final Integer i) {
            // intentionally no @Deprecated
        }

        @SuppressWarnings("deprecation")
        @Override
        public void process(final Number n) {
            // inherited, annotated on interface
        }
    }

    /**
     * getAnnotation() for process(Integer) should return null because the Integer overload is NOT an override of process(Number).
     * <ul>
     * <li>Pre-patch: getMatchingMethod finds process(Number) (since Integer is assignable to Number) and returns the {@code @Deprecated} annotation
     * incorrectly.</li>
     * <li>Post-patch: uses getDeclaredMethod with exact types, finds nothing, returns null.</li>
     * </ul>
     */
    @SuppressWarnings("javadoc")
    @Test
    public void testAnnotationLookupDoesNotMatchAssignableOverload() throws NoSuchMethodException {
        final Method integerMethod = ProcessorImpl.class.getDeclaredMethod("process", Integer.class);
        final Deprecated ann = MethodUtils.getAnnotation(integerMethod, Deprecated.class, true, true);
        assertNull(ann, "process(Integer) is NOT an override of process(Number); its annotation lookup must return null");
        final Method numberMethod = ProcessorImpl.class.getDeclaredMethod("process", Number.class);
        assertNotNull(MethodUtils.getAnnotation(numberMethod, Deprecated.class, true, true));
    }
}
