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

package org.apache.commons.lang3.text.translate;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Unit tests for {@link org.apache.commons.lang3.text.translate.UnicodeEscaper}.
 */
@Deprecated
public class UnicodeEscaperTest extends AbstractLangTest {

    @Test
    public void testBelow() {
        final UnicodeEscaper ue = UnicodeEscaper.below('F');

        final String input = "ADFGZ";
        final String result = ue.translate(input);
        assertEquals("\\u0041\\u0044FGZ", result, "Failed to escape Unicode characters via the below method");
    }

    @Test
    public void testBetween() {
        final UnicodeEscaper ue = UnicodeEscaper.between('F', 'L');

        final String input = "ADFGZ";
        final String result = ue.translate(input);
        assertEquals("AD\\u0046\\u0047Z", result, "Failed to escape Unicode characters via the between method");
    }

    @Test
    public void testAbove() {
        final UnicodeEscaper ue = UnicodeEscaper.above('F');

        final String input = "ADFGZ";
        final String result = ue.translate(input);
        assertEquals("ADF\\u0047\\u005A", result, "Failed to escape Unicode characters via the above method");
    }
}
