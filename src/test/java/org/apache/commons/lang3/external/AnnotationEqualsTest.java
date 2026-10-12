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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.annotation.Annotation;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.reflect.InvocationTargetException;

import org.apache.commons.lang3.AnnotationUtils;
import org.apache.commons.lang3.exception.UncheckedException;
import org.junit.jupiter.api.Test;
import org.junitpioneer.jupiter.SetSystemProperty;

/**
 * Regression test for <a href="https://issues.apache.org/jira/browse/LANG-1815">LANG-1815</a>.
 * <p>
 * Verifies that {@link AnnotationUtils} can reflectively read package-private annotation members
 * for {@code equals}, {@code hashCode}, and {@code toString}, and that reflective invocation failures
 * are not treated as inequality.
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
    /**
     * Declares an annotation accessible without suppressing reflective access checks.
     */
    @Retention(RetentionPolicy.RUNTIME)
    public @interface PublicTag {
        /**
         * Gets the tag value.
         *
         * @return the tag value
         */
        String value();
    }

    @Retention(RetentionPolicy.RUNTIME)
    @interface Tag {
        String value();
    }

    static class ThrowingTag implements Tag {
        @Override
        public Class<? extends Annotation> annotationType() {
            return Tag.class;
        }

        @Override
        public String value() {
            throw new IllegalArgumentException("boom");
        }
    }

    @PublicTag("value")
    @Tag("value")
    private final Object a = new Object();

    @PublicTag("value")
    @Tag("value")
    private final Object b = new Object();

    @Test
    @SetSystemProperty(key = "AbstractReflection.forceAccessible", value = "false")
    void equalsRejectsPackagePrivateAnnotationsWithoutForcedAccess() throws Exception {
        final Tag tagA = getClass().getDeclaredField("a").getAnnotation(Tag.class);
        final Tag tagB = getClass().getDeclaredField("b").getAnnotation(Tag.class);
        final IllegalStateException ex = assertThrows(IllegalStateException.class, () -> AnnotationUtils.equals(tagA, tagB));
        assertInstanceOf(IllegalAccessException.class, ex.getCause());
    }

    @Test
    void equalsWorksOnPackagePrivateAnnotations() throws Exception {
        final Tag tagA = getClass().getDeclaredField("a").getAnnotation(Tag.class);
        final Tag tagB = getClass().getDeclaredField("b").getAnnotation(Tag.class);
        assertTrue(AnnotationUtils.equals(tagA, tagB));
    }

    @Test
    @SetSystemProperty(key = "AbstractReflection.forceAccessible", value = "false")
    void equalsWorksOnPublicAnnotationsWithoutForcedAccess() throws Exception {
        final PublicTag tagA = getClass().getDeclaredField("a").getAnnotation(PublicTag.class);
        final PublicTag tagB = getClass().getDeclaredField("b").getAnnotation(PublicTag.class);
        assertTrue(AnnotationUtils.equals(tagA, tagB));
    }

    @Test
    void equalsWrapsReflectiveOperationException() throws Exception {
        final Tag tagA = new ThrowingTag();
        final Tag tagB = getClass().getDeclaredField("b").getAnnotation(Tag.class);

        final IllegalStateException ex =
                assertThrows(IllegalStateException.class, () -> AnnotationUtils.equals(tagA, tagB));
        assertInstanceOf(InvocationTargetException.class, ex.getCause());
        assertEquals("boom", ((InvocationTargetException) ex.getCause()).getTargetException().getMessage());
    }

    @Test
    @SetSystemProperty(key = "AbstractReflection.forceAccessible", value = "false")
    void hashCodeRejectsPackagePrivateAnnotationsWithoutForcedAccess() throws Exception {
        final Tag tag = getClass().getDeclaredField("a").getAnnotation(Tag.class);
        final UncheckedException ex = assertThrows(UncheckedException.class, () -> AnnotationUtils.hashCode(tag));
        assertInstanceOf(IllegalAccessException.class, ex.getCause());
    }

    @Test
    void hashCodeWorksOnPackagePrivateAnnotations() throws Exception {
        final Tag tag = getClass().getDeclaredField("a").getAnnotation(Tag.class);
        assertEquals(tag.hashCode(), AnnotationUtils.hashCode(tag));
    }

    @Test
    @SetSystemProperty(key = "AbstractReflection.forceAccessible", value = "false")
    void hashCodeWorksOnPublicAnnotationsWithoutForcedAccess() throws Exception {
        final PublicTag tag = getClass().getDeclaredField("a").getAnnotation(PublicTag.class);
        assertEquals(tag.hashCode(), AnnotationUtils.hashCode(tag));
    }

    @Test
    @SetSystemProperty(key = "AbstractReflection.forceAccessible", value = "false")
    void toStringRejectsPackagePrivateAnnotationsWithoutForcedAccess() throws Exception {
        final Tag tag = getClass().getDeclaredField("a").getAnnotation(Tag.class);
        final UncheckedException ex = assertThrows(UncheckedException.class, () -> AnnotationUtils.toString(tag));
        assertInstanceOf(IllegalAccessException.class, ex.getCause());
    }

    @Test
    void toStringWorksOnPackagePrivateAnnotations() throws Exception {
        final Tag tag = getClass().getDeclaredField("a").getAnnotation(Tag.class);
        final String text = AnnotationUtils.toString(tag);
        assertTrue(text.contains("value=value"), text);
    }

    @Test
    @SetSystemProperty(key = "AbstractReflection.forceAccessible", value = "false")
    void toStringWorksOnPublicAnnotationsWithoutForcedAccess() throws Exception {
        final PublicTag tag = getClass().getDeclaredField("a").getAnnotation(PublicTag.class);
        assertEquals("@" + PublicTag.class.getName() + "(value=value)", AnnotationUtils.toString(tag));
    }

}
