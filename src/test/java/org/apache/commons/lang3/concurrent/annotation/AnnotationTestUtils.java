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

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;

import org.apache.bcel.Repository;
import org.apache.bcel.classfile.AnnotationEntry;
import org.apache.bcel.classfile.ElementValuePair;
import org.apache.bcel.classfile.JavaClass;
import org.apache.commons.lang3.reflect.MethodUtils;
import org.junit.Assert;

class AnnotationTestUtils {

    public static void testClassAnnotationInClassFile(final String className, final String annotationType) throws Exception {
        final JavaClass clazz = Repository.lookupClass(className);
        final AnnotationEntry[] annotationEntries = clazz.getAnnotationEntries();
        Assert.assertNotNull(annotationEntries);
        Assert.assertEquals(1, annotationEntries.length);
        final AnnotationEntry annotationEntry = annotationEntries[0];
        Assert.assertEquals(annotationType, annotationEntry .getAnnotationType());
        final ElementValuePair[] elementValuePairs = annotationEntry.getElementValuePairs();
        Assert.assertNotNull(elementValuePairs);
        Assert.assertEquals(0, elementValuePairs.length);
    }

    public static void testMethodAnnotationNotRetainedAtRuntime(final Class<?> cls,
            final Class<? extends Annotation> annotationCls) {
        final Method[] methods = MethodUtils.getMethodsWithAnnotation(cls, annotationCls);
        Assert.assertNotNull(methods);
        Assert.assertEquals(0, methods.length);
    }

}
