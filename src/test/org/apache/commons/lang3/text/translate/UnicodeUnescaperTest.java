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
 * Unit tests for {@link org.apache.commons.lang3.text.translate.UnicodeEscaper}.
 */
public class UnicodeUnescaperTest extends TestCase {

    // Requested in LANG-507
    public void testUPlus() {
        UnicodeUnescaper uu = new UnicodeUnescaper();

        String input = "\\u+0047";
        try {
            uu.translate(input);
            fail("Default behaviour should not parse u+");
        } catch(IllegalArgumentException iae) {
            // expected
        }

        uu = new UnicodeUnescaper(UnicodeUnescaper.OPTION.escapePlus);
        assertEquals("Failed to unescape unicode characters with 'u+' notation", "G", uu.translate(input));
    }

    public void testUuuuu() {
        UnicodeUnescaper uu = new UnicodeUnescaper();

        String input = "\\uuuuuuuu0047";
        String result = uu.translate(input);
        assertEquals("Failed to unescape unicode characters with many 'u' characters", "G", result);
    }

    public void testLessThanFour() {
        UnicodeUnescaper uu = new UnicodeUnescaper();

        String input = "\\0047\\u006";
        try {
            uu.translate(input);
            fail("A lack of digits in a unicode escape sequence failed to throw an exception");
        } catch(IllegalArgumentException iae) {
            // expected
        }
    }
}
