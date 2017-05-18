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

import org.junit.Before;
import org.junit.Test;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;

import static org.junit.Assert.assertEquals;

/**
 * Unit test for {@link SinglePassTranslator}
 */
public class SinglePassTranslatorTest {

    private final SinglePassTranslator dummyTranslator = new SinglePassTranslator() {
        @Override
        void translateWhole(final CharSequence input, final Writer out) throws IOException {
        }
    };

    private StringWriter out;

    @Before
    public void before() {
         out = new StringWriter();
    }

    @Test
    public void codePointsAreReturned() throws Exception {
        assertEquals(0, dummyTranslator.translate("", 0, out));
        assertEquals(3, dummyTranslator.translate("abc", 0, out));
        assertEquals(7, dummyTranslator.translate("abcdefg", 0, out));
    }

    @Test(expected = IllegalStateException.class)
    public void indexIsValidated() throws Exception {
        dummyTranslator.translate("abc", 1, out);
    }
}