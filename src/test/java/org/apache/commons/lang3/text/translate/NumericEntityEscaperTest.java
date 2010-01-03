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

import junit.framework.TestCase;

/**
 * Unit tests for {@link org.apache.commons.lang3.text.translate.NumericEntityEscaper}.
 */
public class NumericEntityEscaperTest extends TestCase {

    public void testBelow() {
        NumericEntityEscaper nee = NumericEntityEscaper.below('F');

        String input = "ADFGZ";
        String result = nee.translate(input);
        assertEquals("Failed to escape numeric entities via the below method", "&#65;&#68;FGZ", result);
    }

    public void testBetween() {
        NumericEntityEscaper nee = NumericEntityEscaper.between('F', 'L');

        String input = "ADFGZ";
        String result = nee.translate(input);
        assertEquals("Failed to escape numeric entities via the between method", "AD&#70;&#71;Z", result);
    }

    public void testAbove() {
        NumericEntityEscaper nee = NumericEntityEscaper.above('F');

        String input = "ADFGZ";
        String result = nee.translate(input);
        assertEquals("Failed to escape numeric entities via the above method", "ADF&#71;&#90;", result);
    }
}
