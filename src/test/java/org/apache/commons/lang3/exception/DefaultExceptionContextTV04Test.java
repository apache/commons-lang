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

package org.apache.commons.lang3.exception;

import static org.junit.jupiter.api.Assertions.*;
import java.util.List;
import java.util.Arrays;
import org.junit.jupiter.api.Test;

class DefaultExceptionContextTV04Test {
    @Test void storesMultipleValuesAndPreservesInsertionOrder() {
        DefaultExceptionContext c = new DefaultExceptionContext();
        c.addContextValue("id", 10).addContextValue("name", "Ada").addContextValue("id", 11);
        assertEquals(Arrays.asList(10, 11), c.getContextValues("id"));
        assertEquals(10, c.getFirstContextValue("id"));
        assertEquals(3, c.getContextEntries().size());
        assertEquals(2, c.getContextLabels().size());
    }
    @Test void setContextValueReplacesAllValuesForLabelAndAppendsReplacement() {
        DefaultExceptionContext c = new DefaultExceptionContext();
        c.addContextValue("x", 1).addContextValue("y", 2).addContextValue("x", 3);
        c.setContextValue("x", 9);
        assertEquals(Arrays.asList(9), c.getContextValues("x"));
        assertEquals("y", c.getContextEntries().get(0).getKey());
        assertEquals("x", c.getContextEntries().get(1).getKey());
    }
    @Test void absentLabelAndNullBaseMessageAreHandled() {
        DefaultExceptionContext c = new DefaultExceptionContext();
        assertTrue(c.getContextValues("missing").isEmpty());
        assertNull(c.getFirstContextValue("missing"));
        assertEquals("", c.getFormattedExceptionMessage(null));
        c.addContextValue("k", "v");
        String formatted = c.getFormattedExceptionMessage(null);
        assertTrue(formatted.contains("Exception Context:"));
        assertTrue(formatted.contains("[1:k=v]"));
    }
    @Test void formattedMessageIncludesBaseMessageAndContext() {
        DefaultExceptionContext c = new DefaultExceptionContext().addContextValue("user", "Ada");
        String result = c.getFormattedExceptionMessage("failed");
        assertTrue(result.startsWith("failed\n"));
        assertTrue(result.contains("user=Ada"));
        assertTrue(result.endsWith("---------------------------------"));
    }
}
