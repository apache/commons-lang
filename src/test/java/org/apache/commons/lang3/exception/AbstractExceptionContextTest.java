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
package org.apache.commons.lang3.exception;

import org.junit.Test;
import org.junit.Before;
import static org.junit.Assert.*;
import java.io.Serializable;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.SerializationUtils;
import org.apache.commons.lang3.tuple.Pair;


/**
 * Abstract test of an ExceptionContext implementation.
 */
public abstract class AbstractExceptionContextTest<T extends ExceptionContext & Serializable> {

    protected static final String TEST_MESSAGE_2 = "This is monotonous";
    protected static final String TEST_MESSAGE = "Test Message";
    protected T exceptionContext;

    protected static class ObjectWithFaultyToString {
        @Override
        public String toString() {
            throw new RuntimeException("Crap");
        }
    }


    @Before
    public void setUp() throws Exception {
        exceptionContext
            .addContextValue("test1", null)
            .addContextValue("test2", "some value")
            .addContextValue("test Date", new Date())
            .addContextValue("test Nbr", Integer.valueOf(5))
            .addContextValue("test Poorly written obj", new ObjectWithFaultyToString());
    }

    @Test
    public void testAddContextValue() {
        String message = exceptionContext.getFormattedExceptionMessage(TEST_MESSAGE);
        assertTrue(message.indexOf(TEST_MESSAGE) >= 0);
        assertTrue(message.indexOf("test1") >= 0);
        assertTrue(message.indexOf("test2") >= 0);
        assertTrue(message.indexOf("test Date") >= 0);
        assertTrue(message.indexOf("test Nbr") >= 0);
        assertTrue(message.indexOf("some value") >= 0);
        assertTrue(message.indexOf("5") >= 0);

        assertTrue(exceptionContext.getFirstContextValue("test1") == null);
        assertTrue(exceptionContext.getFirstContextValue("test2").equals("some value"));

        assertEquals(5, exceptionContext.getContextLabels().size());
        assertTrue(exceptionContext.getContextLabels().contains("test1"));
        assertTrue(exceptionContext.getContextLabels().contains("test2"));
        assertTrue(exceptionContext.getContextLabels().contains("test Date"));
        assertTrue(exceptionContext.getContextLabels().contains("test Nbr"));

        exceptionContext.addContextValue("test2", "different value");
        assertEquals(5, exceptionContext.getContextLabels().size());
        assertTrue(exceptionContext.getContextLabels().contains("test2"));

        String contextMessage = exceptionContext.getFormattedExceptionMessage(null);
        assertTrue(contextMessage.indexOf(TEST_MESSAGE) == -1);
    }

    @Test
    public void testSetContextValue() {
        exceptionContext.addContextValue("test2", "different value");
        exceptionContext.setContextValue("test3", "3");

        String message = exceptionContext.getFormattedExceptionMessage(TEST_MESSAGE);
        assertTrue(message.indexOf(TEST_MESSAGE) >= 0);
        assertTrue(message.indexOf("test Poorly written obj") >= 0);
        assertTrue(message.indexOf("Crap") >= 0);

        assertTrue(exceptionContext.getFirstContextValue("crap") == null);
        assertTrue(exceptionContext.getFirstContextValue("test Poorly written obj") instanceof ObjectWithFaultyToString);

        assertEquals(7, exceptionContext.getContextEntries().size());
        assertEquals(6, exceptionContext.getContextLabels().size());

        assertTrue(exceptionContext.getContextLabels().contains("test Poorly written obj"));
        assertTrue(!exceptionContext.getContextLabels().contains("crap"));

        exceptionContext.setContextValue("test Poorly written obj", "replacement");

        assertEquals(7, exceptionContext.getContextEntries().size());
        assertEquals(6, exceptionContext.getContextLabels().size());

        exceptionContext.setContextValue("test2", "another");

        assertEquals(6, exceptionContext.getContextEntries().size());
        assertEquals(6, exceptionContext.getContextLabels().size());

        String contextMessage = exceptionContext.getFormattedExceptionMessage(null);
        assertTrue(contextMessage.indexOf(TEST_MESSAGE) == -1);
    }

    @Test
    public void testGetFirstContextValue() {
        exceptionContext.addContextValue("test2", "different value");

        assertTrue(exceptionContext.getFirstContextValue("test1") == null);
        assertTrue(exceptionContext.getFirstContextValue("test2").equals("some value"));
        assertTrue(exceptionContext.getFirstContextValue("crap") == null);

        exceptionContext.setContextValue("test2", "another");

        assertTrue(exceptionContext.getFirstContextValue("test2").equals("another"));
    }

    @Test
    public void testGetContextValues() {
        exceptionContext.addContextValue("test2", "different value");

        assertEquals(exceptionContext.getContextValues("test1"), Collections.singletonList(null));
        assertEquals(exceptionContext.getContextValues("test2"), Arrays.asList("some value", "different value"));

        exceptionContext.setContextValue("test2", "another");

        assertTrue(exceptionContext.getFirstContextValue("test2").equals("another"));
    }

    @Test
    public void testGetContextLabels() {
        assertEquals(5, exceptionContext.getContextEntries().size());
        
        exceptionContext.addContextValue("test2", "different value");

        Set<String> labels = exceptionContext.getContextLabels();
        assertEquals(6, exceptionContext.getContextEntries().size());
        assertEquals(5, labels.size());
        assertTrue(labels.contains("test1"));
        assertTrue(labels.contains("test2"));
        assertTrue(labels.contains("test Date"));
        assertTrue(labels.contains("test Nbr"));
    }

    @Test
    public void testGetContextEntries() {
        assertEquals(5, exceptionContext.getContextEntries().size());
        
        exceptionContext.addContextValue("test2", "different value");

        List<Pair<String, Object>> entries = exceptionContext.getContextEntries();
        assertEquals(6, entries.size());
        assertEquals("test1", entries.get(0).getKey());
        assertEquals("test2", entries.get(1).getKey());
        assertEquals("test Date", entries.get(2).getKey());
        assertEquals("test Nbr", entries.get(3).getKey());
        assertEquals("test Poorly written obj", entries.get(4).getKey());
        assertEquals("test2", entries.get(5).getKey());
    }
    
    @Test
    public void testJavaSerialization() {
        exceptionContext.setContextValue("test Poorly written obj", "serializable replacement");
        
        T clone = SerializationUtils.deserialize(SerializationUtils.serialize(exceptionContext));
        assertEquals(exceptionContext.getFormattedExceptionMessage(null), clone.getFormattedExceptionMessage(null));
    }
}
