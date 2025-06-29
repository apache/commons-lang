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

package org.apache.commons.lang3;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link Charsets}.
 */
class CharsetsTest extends AbstractLangTest {

    @Test
    void testToCharset_Charset() {
        Assertions.assertEquals(Charset.defaultCharset(), Charsets.toCharset((Charset) null));
        Assertions.assertEquals(Charset.defaultCharset(), Charsets.toCharset(Charset.defaultCharset()));
        Assertions.assertEquals(StandardCharsets.UTF_8, Charsets.toCharset(StandardCharsets.UTF_8));
    }

    @Test
    void testToCharset_String() {
        Assertions.assertEquals(Charset.defaultCharset(), Charsets.toCharset((String) null));
        Assertions.assertEquals(Charset.defaultCharset(), Charsets.toCharset(Charset.defaultCharset().name()));
        Assertions.assertEquals(StandardCharsets.UTF_8, Charsets.toCharset(StandardCharsets.UTF_8.name()));
    }

    @Test
    void testToCharsetName() {
        Assertions.assertEquals(Charset.defaultCharset().name(), Charsets.toCharsetName((String) null));
        Assertions.assertEquals("UTF-8", Charsets.toCharsetName(StandardCharsets.UTF_8.name()));
    }

}
