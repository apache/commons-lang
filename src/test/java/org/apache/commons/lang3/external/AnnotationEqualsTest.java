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
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

// CAUTION: in order to reproduce https://issues.apache.org/jira/browse/LANG-1815,
// this test MUST be located OUTSIDE org.apache.commons.lang3 package.
// Do NOT move it to the org.apache.commons.lang3 package!
public class AnnotationEqualsTest {
    @Retention(RetentionPolicy.RUNTIME)
    @interface Tag {
        String value();
    }

    @Tag("value")
    private final Object a = new Object();
    @Tag("value")
    private final Object b = new Object();

    @Test
    void equalsWorksOnPackagePrivateAnnotations() throws Exception {
        Tag tagA = getClass().getDeclaredField("a").getAnnotation(Tag.class);
        Tag tagB = getClass().getDeclaredField("b").getAnnotation(Tag.class);
        Assertions.assertTrue(AnnotationUtils.equals(tagA, tagB));
    }
}
