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
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Unit tests for {@link org.apache.commons.lang3.text.translate.NumericEntityUnescaper}.
 */
@Deprecated
public class NumericEntityUnescaperTest extends AbstractLangTest {

    @Test
    public void testSupplementaryUnescaping() {
        final NumericEntityUnescaper neu = new NumericEntityUnescaper();
        final String input = "&#68642;";
        final String expected = "\uD803\uDC22";

        final String result = neu.translate(input);
        assertEquals(expected, result, "Failed to unescape numeric entities supplementary characters");
    }

    @Test
    public void testOutOfBounds() {
        final NumericEntityUnescaper neu = new NumericEntityUnescaper();

        assertEquals("Test &", neu.translate("Test &"), "Failed to ignore when last character is &");
        assertEquals("Test &#", neu.translate("Test &#"), "Failed to ignore when last character is &");
        assertEquals("Test &#x", neu.translate("Test &#x"), "Failed to ignore when last character is &");
        assertEquals("Test &#X", neu.translate("Test &#X"), "Failed to ignore when last character is &");
    }

    @Test
    public void testUnfinishedEntity() {
        // parse it
        NumericEntityUnescaper neu = new NumericEntityUnescaper(NumericEntityUnescaper.OPTION.semiColonOptional);
        String input = "Test &#x30 not test";
        String expected = "Test \u0030 not test";

        String result = neu.translate(input);
        assertEquals(expected, result, "Failed to support unfinished entities (i.e. missing semicolon)");

        // ignore it
        neu = new NumericEntityUnescaper();
        input = "Test &#x30 not test";
        expected = input;

        result = neu.translate(input);
        assertEquals(expected, result, "Failed to ignore unfinished entities (i.e. missing semicolon)");

        // fail it
        final NumericEntityUnescaper failingNeu =
                new NumericEntityUnescaper(NumericEntityUnescaper.OPTION.errorIfNoSemiColon);
        final String failingInput = "Test &#x30 not test";
        assertThrows(IllegalArgumentException.class, () -> failingNeu.translate(failingInput));
    }

}
