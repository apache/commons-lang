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

import org.junit.Test;
import static org.junit.Assert.*;

/**
 * Unit tests for {@link org.apache.commons.lang3.text.translate.OctalUnescaper}.
 */
public class OctalUnescaperTest {

    @Test
    public void testBetween() {
        final OctalUnescaper oue = new OctalUnescaper();   //.between("1", "377");

        String input = "\\45";
        String result = oue.translate(input);
        assertEquals("Failed to unescape octal characters via the between method", "\45", result);

        input = "\\377";
        result = oue.translate(input);
        assertEquals("Failed to unescape octal characters via the between method", "\377", result);

        input = "\\377 and";
        result = oue.translate(input);
        assertEquals("Failed to unescape octal characters via the between method", "\377 and", result);

        input = "\\378 and";
        result = oue.translate(input);
        assertEquals("Failed to unescape octal characters via the between method", "\37" + "8 and", result);

        input = "\\378";
        result = oue.translate(input);
        assertEquals("Failed to unescape octal characters via the between method", "\37" + "8", result);

        input = "\\1";
        result = oue.translate(input);
        assertEquals("Failed to unescape octal characters via the between method", "\1", result);

        input = "\\036";
        result = oue.translate(input);
        assertEquals("Failed to unescape octal characters via the between method", "\036", result);

        input = "\\0365";
        result = oue.translate(input);
        assertEquals("Failed to unescape octal characters via the between method", "\036" + "5", result);

        input = "\\003";
        result = oue.translate(input);
        assertEquals("Failed to unescape octal characters via the between method", "\003", result);

        input = "\\0003";
        result = oue.translate(input);
        assertEquals("Failed to unescape octal characters via the between method", "\000" + "3", result);

        input = "\\279";
        result = oue.translate(input);
        assertEquals("Failed to unescape octal characters via the between method", "\279", result);

        input = "\\999";
        result = oue.translate(input);
        assertEquals("Failed to ignore an out of range octal character via the between method", "\\999", result);
    }

}
