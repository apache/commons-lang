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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link ReflectionToStringBuilder}.
 */
class ReflectionToStringBuilderTest extends AbstractLangTest {

    @Test
    void testConstructorWithNullObject() {
        assertEquals("<null>", new ReflectionToStringBuilder(null, ToStringStyle.DEFAULT_STYLE, new StringBuffer()).toString());
    }

    @Test
    void testFallsBackToCallingToStringMethodIfReflectiveAccessNotAllowed() {
      final String data = "string-value";
      final ReflectionToStringBuilder builder =  new ReflectionToStringBuilder(data, ToStringStyle.DEFAULT_STYLE, new StringBuffer());
      builder.reflectiveAccess = new ReflectiveAccessUtil() {
          @Override
          boolean isAllowed(Class<?> targetClass) {
              return false;
          }
      };
      assertEquals(String.format("java.lang.String@%s[%s]", Integer.toHexString(System.identityHashCode(data)), data), builder.toString());
    }

    @Test
    void testUsesReflectionToGetFieldsIfReflectiveAccessAllowed() {
        final String data = "string-value";
        final ReflectionToStringBuilder builder =  new ReflectionToStringBuilder(data, ToStringStyle.DEFAULT_STYLE, new StringBuffer());
        builder.reflectiveAccess = new ReflectiveAccessUtil() {
            @Override
            boolean isAllowed(Class<?> targetClass) {
                return true;
            }
        };
        final String result = builder.toString();
        assertTrue(result.startsWith(String.format("java.lang.String@%s[", Integer.toHexString(System.identityHashCode(data)))));
        assertTrue(result.contains(String.format("hash=%s", data.hashCode())));
        assertTrue(result.endsWith("]"));
    }

    @Test
    void testUsesReflectionToGetFieldsIfJpmsNotSupported() {
        final String data = "string-value";
        final ReflectionToStringBuilder builder =  new ReflectionToStringBuilder(data, ToStringStyle.DEFAULT_STYLE, new StringBuffer());
        builder.reflectiveAccess = new ReflectiveAccessUtil() {
            @Override
            boolean isJpmsSupported() {
                return false;
            }
        };
        final String result = builder.toString();
        assertTrue(result.startsWith(String.format("java.lang.String@%s[", Integer.toHexString(System.identityHashCode(data)))));
        assertTrue(result.contains(String.format("hash=%s", data.hashCode())));
        assertTrue(result.endsWith("]"));
    }
}
