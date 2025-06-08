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

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Test case for ToStringStyle.
 */
class ToStringStyleTest extends AbstractLangTest {

    /**
     * An object used to test {@link ToStringStyle}.
     */
    static class Person {
        /**
         * Test String field.
         */
        String name;

        /**
         * Test integer field.
         */
        int age;

        /**
         * Test boolean field.
         */
        boolean smoker;
    }

    private static final class ToStringStyleImpl extends ToStringStyle {
        private static final long serialVersionUID = 1L;

    }

    @Test
    void testSetArrayEnd() {
        final ToStringStyle style = new ToStringStyleImpl();
        style.setArrayEnd(null);
        assertEquals("", style.getArrayEnd());
    }

    @Test
    void testSetArraySeparator() {
        final ToStringStyle style = new ToStringStyleImpl();
        style.setArraySeparator(null);
        assertEquals("", style.getArraySeparator());
    }

    @Test
    void testSetArrayStart() {
        final ToStringStyle style = new ToStringStyleImpl();
        style.setArrayStart(null);
        assertEquals("", style.getArrayStart());
    }

    @Test
    void testSetContentEnd() {
        final ToStringStyle style = new ToStringStyleImpl();
        style.setContentEnd(null);
        assertEquals("", style.getContentEnd());
    }

    @Test
    void testSetContentStart() {
        final ToStringStyle style = new ToStringStyleImpl();
        style.setContentStart(null);
        assertEquals("", style.getContentStart());
    }

    @Test
    void testSetFieldNameValueSeparator() {
        final ToStringStyle style = new ToStringStyleImpl();
        style.setFieldNameValueSeparator(null);
        assertEquals("", style.getFieldNameValueSeparator());
    }

    @Test
    void testSetFieldSeparator() {
        final ToStringStyle style = new ToStringStyleImpl();
        style.setFieldSeparator(null);
        assertEquals("", style.getFieldSeparator());
    }

    @Test
    void testSetNullText() {
        final ToStringStyle style = new ToStringStyleImpl();
        style.setNullText(null);
        assertEquals("", style.getNullText());
    }

    @Test
    void testSetSizeEndText() {
        final ToStringStyle style = new ToStringStyleImpl();
        style.setSizeEndText(null);
        assertEquals("", style.getSizeEndText());
    }

    @Test
    void testSetSizeStartText() {
        final ToStringStyle style = new ToStringStyleImpl();
        style.setSizeStartText(null);
        assertEquals("", style.getSizeStartText());
    }

    @Test
    void testSetSummaryObjectEndText() {
        final ToStringStyle style = new ToStringStyleImpl();
        style.setSummaryObjectEndText(null);
        assertEquals("", style.getSummaryObjectEndText());
    }

    @Test
    void testSetSummaryObjectStartText() {
        final ToStringStyle style = new ToStringStyleImpl();
        style.setSummaryObjectStartText(null);
        assertEquals("", style.getSummaryObjectStartText());
    }
}
