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
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link Diff}.
 */
class DiffTest extends AbstractLangTest {

    private static final class BooleanDiff extends Diff<Boolean> {
        private static final long serialVersionUID = 1L;

        protected BooleanDiff(final String fieldName) {
            super(fieldName);
        }

        @Override
        public Boolean getLeft() {
            return Boolean.TRUE;
        }

        @Override
        public Boolean getRight() {
            return Boolean.FALSE;
        }
    }
    private static final String FIELD_NAME = "field";

    private static final Diff<Boolean> booleanDiff = new BooleanDiff(FIELD_NAME);

    @Test
    void testCannotModify() {
        assertThrows(UnsupportedOperationException.class, () -> booleanDiff.setValue(Boolean.FALSE));
    }

    @Test
    void testGetFieldName() {
        assertEquals(FIELD_NAME, booleanDiff.getFieldName());
    }

    @Test
    void testGetType() {
        assertEquals(Boolean.class, booleanDiff.getType());
    }

    @Test
    void testToString() {
        assertEquals(String.format("[%s: %s, %s]", FIELD_NAME, booleanDiff.getLeft(),
                booleanDiff.getRight()), booleanDiff.toString());
    }
}
