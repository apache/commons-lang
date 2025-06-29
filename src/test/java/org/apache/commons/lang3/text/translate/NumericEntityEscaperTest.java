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

package org.apache.commons.lang3.text.translate;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Tests for {@link org.apache.commons.lang3.text.translate.NumericEntityEscaper}.
 */
@Deprecated
class NumericEntityEscaperTest extends AbstractLangTest {

    @Test
    void testAbove() {
        final NumericEntityEscaper nee = NumericEntityEscaper.above('F');

        final String input = "ADFGZ";
        final String result = nee.translate(input);
        assertEquals("ADF&#71;&#90;", result, "Failed to escape numeric entities via the above method");
    }

    @Test
    void testBelow() {
        final NumericEntityEscaper nee = NumericEntityEscaper.below('F');

        final String input = "ADFGZ";
        final String result = nee.translate(input);
        assertEquals("&#65;&#68;FGZ", result, "Failed to escape numeric entities via the below method");
    }

    @Test
    void testBetween() {
        final NumericEntityEscaper nee = NumericEntityEscaper.between('F', 'L');

        final String input = "ADFGZ";
        final String result = nee.translate(input);
        assertEquals("AD&#70;&#71;Z", result, "Failed to escape numeric entities via the between method");
    }

    // See LANG-617
    @Test
    void testSupplementary() {
        final NumericEntityEscaper nee = new NumericEntityEscaper();
        final String input = "\uD803\uDC22";
        final String expected = "&#68642;";

        final String result = nee.translate(input);
        assertEquals(expected, result, "Failed to escape numeric entities supplementary characters");

    }

}
