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
package org.apache.commons.lang3.external;

import org.apache.commons.lang3.AnnotationUtils;
import org.junit.jupiter.api.Test;

import java.lang.annotation.Annotation;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.reflect.InvocationTargetException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Regression test for <a href="https://issues.apache.org/jira/browse/LANG-1815">LANG-1815</a>.
 * <p>
 * Verifies that {@code AnnotationUtils.equals(Annotation, Annotation)} treats two equal
 * package-private annotations as equal, and also wraps a possible ReflectiveOperationException.
 * </p>
 *
 * <h2>Important</h2>
 * <p>
 * This test relies on reflective access rules that differ depending on the caller's package.
 * To reproduce the original bug, this class <strong>must remain outside</strong> the
 * {@code org.apache.commons.lang3} package.
 * </p>
 * <p>
 * Do <strong>not</strong> move this class into {@code org.apache.commons.lang3},
 * otherwise the test may no longer exercise the failing scenario from LANG-1815.
 * </p>
 */
public class AnnotationEqualsTest {
    @Retention(RetentionPolicy.RUNTIME)
    @interface Tag {
        String value();
    }

    static class ThrowingTag implements Tag {
        @Override
        public String value() {
            throw new IllegalArgumentException("boom");
        }

        @Override
        public Class<? extends Annotation> annotationType() {
            return Tag.class;
        }
    }

    @Tag("value")
    private final Object a = new Object();
    @Tag("value")
    private final Object b = new Object();

    @Test
    void equalsWorksOnPackagePrivateAnnotations() throws Exception {
        Tag tagA = getClass().getDeclaredField("a").getAnnotation(Tag.class);
        Tag tagB = getClass().getDeclaredField("b").getAnnotation(Tag.class);
        assertTrue(AnnotationUtils.equals(tagA, tagB));
    }

    @Test
    void equalsWrapsReflectiveOperationException() throws Exception {
        // Proxy annotation instances: calling Tag#value() will throw at runtime
        final Tag tagA = new ThrowingTag();
        final Tag tagB = getClass().getDeclaredField("b").getAnnotation(Tag.class);

        final IllegalStateException ex =
                assertThrows(IllegalStateException.class, () -> AnnotationUtils.equals(tagA, tagB));
        assertInstanceOf(InvocationTargetException.class, ex.getCause());
        assertEquals("boom", ((InvocationTargetException) ex.getCause()).getTargetException().getMessage());
    }

}
