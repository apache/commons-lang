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

import static org.junit.Assert.assertEquals;

import org.junit.Test;

/**
 * Unit tests for {@link org.apache.commons.lang3.text.translate.UnicodeEscaper}.
 * @version $Id$
 */
public class UnicodeEscaperTest  {

    @Test
    public void testBelow() {
        UnicodeEscaper ue = UnicodeEscaper.below('F');

        String input = "ADFGZ";
        String result = ue.translate(input);
        assertEquals("Failed to escape Unicode characters via the below method", "\\u0041\\u0044FGZ", result);
    }

    @Test
    public void testBetween() {
        UnicodeEscaper ue = UnicodeEscaper.between('F', 'L');

        String input = "ADFGZ";
        String result = ue.translate(input);
        assertEquals("Failed to escape Unicode characters via the between method", "AD\\u0046\\u0047Z", result);
    }

    @Test
    public void testAbove() {
        UnicodeEscaper ue = UnicodeEscaper.above('F');

        String input = "ADFGZ";
        String result = ue.translate(input);
        assertEquals("Failed to escape Unicode characters via the above method", "ADF\\u0047\\u005A", result);
    }
}
