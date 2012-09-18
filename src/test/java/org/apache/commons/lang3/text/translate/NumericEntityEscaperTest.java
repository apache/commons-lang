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
 * Unit tests for {@link org.apache.commons.lang3.text.translate.NumericEntityEscaper}.
 * @version $Id$
 */
public class NumericEntityEscaperTest  {

    @Test
    public void testBelow() {
        NumericEntityEscaper nee = NumericEntityEscaper.below('F');

        String input = "ADFGZ";
        String result = nee.translate(input);
        assertEquals("Failed to escape numeric entities via the below method", "&#65;&#68;FGZ", result);
    }

    @Test
    public void testBetween() {
        NumericEntityEscaper nee = NumericEntityEscaper.between('F', 'L');

        String input = "ADFGZ";
        String result = nee.translate(input);
        assertEquals("Failed to escape numeric entities via the between method", "AD&#70;&#71;Z", result);
    }

    @Test
    public void testAbove() {
        NumericEntityEscaper nee = NumericEntityEscaper.above('F');

        String input = "ADFGZ";
        String result = nee.translate(input);
        assertEquals("Failed to escape numeric entities via the above method", "ADF&#71;&#90;", result);
    }

    // See LANG-617
    @Test
    public void testSupplementary() {
        NumericEntityEscaper nee = new NumericEntityEscaper();
        String input = "\uD803\uDC22";
        String expected = "&#68642;";

        String result = nee.translate(input);
        assertEquals("Failed to escape numeric entities supplementary characters", expected, result);

    }

}
