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
package org.apache.commons.lang3;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

/**
 * Unit tests {@link org.apache.commons.lang3.NotImplementedException}.
 */
public class NotImplementedExceptionTest extends AbstractLangTest {

    @Test
    public void testConstructors() {
        final Throwable nested = new RuntimeException();
        final String message = "Not Implemented";
        final String code = "CODE";

        NotImplementedException nie = new NotImplementedException(message);
        assertCorrect("Issue in (String)", nie, message, null, null);
        nie = new NotImplementedException(nested);
        assertCorrect("Issue in (Throwable)", nie, nested.toString(), nested, null);
        nie = new NotImplementedException(message, nested);
        assertCorrect("Issue in (String, Throwable)", nie, message, nested, null);
        nie = new NotImplementedException(message, code);
        assertCorrect("Issue in (String, String)", nie, message, null, code);
        nie = new NotImplementedException(nested, code);
        assertCorrect("Issue in (Throwable, String)", nie, nested.toString(), nested, code);
        nie = new NotImplementedException(message, nested, code);
        assertCorrect("Issue in (String, Throwable, String)", nie, message, nested, code);

        assertNull(new NotImplementedException().getCode());
    }

    private void assertCorrect(final String assertMessage, final NotImplementedException nie, final String message, final Throwable nested, final String code) {
        assertNotNull(nie, assertMessage + ": target is null");
        assertEquals(message, nie.getMessage(), assertMessage + ": Message not equal");
        assertEquals(nested, nie.getCause(), assertMessage + ": Nested throwable not equal");
        assertEquals(code, nie.getCode(), assertMessage + ": Code not equal");
    }
}
