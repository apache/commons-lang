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
package org.apache.commons.lang3.concurrent.annotation;

import org.apache.bcel.Repository;
import org.apache.bcel.classfile.AnnotationEntry;
import org.apache.bcel.classfile.ElementValue;
import org.apache.bcel.classfile.ElementValuePair;
import org.apache.bcel.classfile.JavaClass;
import org.junit.Assert;
import org.junit.Test;

/**
 * Tests {@link GuardedBy}.
 */
public class GuardedByTest {

    private org.apache.bcel.classfile.Method getBcelMethod(final JavaClass clazz, final String name) {
        for (final org.apache.bcel.classfile.Method method : clazz.getMethods()) {
            if (method.getName().equals(name)) {
                return method;
            }
        }
        return null;
    }

    @Test
    public void testMethodAnnotationInClassFile() throws Exception {
        final JavaClass clazz = Repository.lookupClass("org.apache.commons.lang3.concurrent.annotation.GuardedByTestFixture");
        final org.apache.bcel.classfile.Method method = getBcelMethod(clazz, "foo");
        Assert.assertNotNull(method);
        final AnnotationEntry[] annotationEntries = method.getAnnotationEntries();
        Assert.assertNotNull(annotationEntries);
        Assert.assertEquals(1, annotationEntries.length);
        final AnnotationEntry annotationEntry = annotationEntries[0];
        Assert.assertEquals("Lorg/apache/commons/lang3/concurrent/annotation/GuardedBy;", annotationEntry .getAnnotationType());
        final ElementValuePair[] elementValuePairs = annotationEntry.getElementValuePairs();
        Assert.assertNotNull(elementValuePairs);
        Assert.assertEquals(1, elementValuePairs.length);
        final ElementValuePair elementValuePair = elementValuePairs[0];
        final ElementValue value = elementValuePair.getValue();
        Assert.assertNotNull(value);
        Assert.assertEquals("bar", value.toString());
    }

    @Test
    public void testMethodAnnotationNotRetainedAtRuntime() {
        AnnotationTestUtils.testMethodAnnotationNotRetainedAtRuntime(GuardedByTestFixture.class, GuardedBy.class);
    }

}
