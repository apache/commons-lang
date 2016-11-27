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

import org.junit.Test;

/**
 * Tests {@link GuardedBy}.
 */
public class ImmutableTest {

    @Test
    public void testClassAnnotationInClassFile() throws Exception {
        AnnotationTestUtils.testClassAnnotationInClassFile(
                "org.apache.commons.lang3.concurrent.annotation.ImmutableTestFixture",
                "Lorg/apache/commons/lang3/concurrent/annotation/Immutable;");
    }

    @Test
    public void testMethodAnnotationNotRetainedAtRuntime() {
        AnnotationTestUtils.testMethodAnnotationNotRetainedAtRuntime(ImmutableTestFixture.class, Immutable.class);
    }

}
