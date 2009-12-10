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

import java.util.Date;

import junit.framework.TestCase;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ContextedExceptionTest.ObjectWithFaultyToString;

/**
 * JUnit tests for ContextedRuntimeException.
 * @author D. Ashmore
 *
 */
public class ContextedRuntimeExceptionTest extends TestCase {
    
    private static final String TEST_MESSAGE_2 = "This is monotonous";
    private static final String TEST_MESSAGE = "Test Message";
    private ContextedRuntimeException contextedRuntimeException;

    public ContextedRuntimeExceptionTest(String name) {
        super(name);
    }

    public void testContextedException() {
        contextedRuntimeException = new ContextedRuntimeException();
        String message = contextedRuntimeException.getMessage();
        String trace = ExceptionUtils.getStackTrace(contextedRuntimeException);
        assertTrue(trace.indexOf("ContextedException")>=0);
        assertTrue(StringUtils.isEmpty(message));
    }

    public void testContextedExceptionString() {
        contextedRuntimeException = new ContextedRuntimeException(TEST_MESSAGE);
        assertEquals(TEST_MESSAGE, contextedRuntimeException.getMessage());
        
        String trace = ExceptionUtils.getStackTrace(contextedRuntimeException);
        assertTrue(trace.indexOf(TEST_MESSAGE)>=0);
    }

    public void testContextedExceptionThrowable() {
        contextedRuntimeException = new ContextedRuntimeException(new Exception(TEST_MESSAGE));
        String message = contextedRuntimeException.getMessage();
        String trace = ExceptionUtils.getStackTrace(contextedRuntimeException);
        assertTrue(trace.indexOf("ContextedException")>=0);
        assertTrue(trace.indexOf(TEST_MESSAGE)>=0);
        assertTrue(message.indexOf(TEST_MESSAGE)>=0);
    }

    public void testContextedExceptionStringThrowable() {
        contextedRuntimeException = new ContextedRuntimeException(TEST_MESSAGE_2, new Exception(TEST_MESSAGE));
        String message = contextedRuntimeException.getMessage();
        String trace = ExceptionUtils.getStackTrace(contextedRuntimeException);
        assertTrue(trace.indexOf("ContextedException")>=0);
        assertTrue(trace.indexOf(TEST_MESSAGE)>=0);
        assertTrue(trace.indexOf(TEST_MESSAGE_2)>=0);
        assertTrue(message.indexOf(TEST_MESSAGE_2)>=0);
    }
    
    public void testContextedExceptionStringThrowableContext() {
        contextedRuntimeException = new ContextedRuntimeException(TEST_MESSAGE_2, new Exception(TEST_MESSAGE), new DefaultExceptionContext());
        String message = contextedRuntimeException.getMessage();
        String trace = ExceptionUtils.getStackTrace(contextedRuntimeException);
        assertTrue(trace.indexOf("ContextedException")>=0);
        assertTrue(trace.indexOf(TEST_MESSAGE)>=0);
        assertTrue(trace.indexOf(TEST_MESSAGE_2)>=0);
        assertTrue(message.indexOf(TEST_MESSAGE_2)>=0);
    }

    public void testAddValue() {
        contextedRuntimeException = new ContextedRuntimeException(new Exception(TEST_MESSAGE))
        .addValue("test1", null)
        .addValue("test2", "some value")
        .addValue("test Date", new Date())
        .addValue("test Nbr", new Integer(5))
        .addValue("test Poorly written obj", new ObjectWithFaultyToString());
        
        String message = contextedRuntimeException.getMessage();
        assertTrue(message.indexOf(TEST_MESSAGE)>=0);
        assertTrue(message.indexOf("test1")>=0);
        assertTrue(message.indexOf("test2")>=0);
        assertTrue(message.indexOf("test Date")>=0);
        assertTrue(message.indexOf("test Nbr")>=0);
        assertTrue(message.indexOf("test Poorly written obj")>=0);
        assertTrue(message.indexOf("some value")>=0);
        assertTrue(message.indexOf("5")>=0);
        assertTrue(message.indexOf("Crap")>=0);
        
        assertTrue(contextedRuntimeException.getValue("test1") == null);
        assertTrue(contextedRuntimeException.getValue("test2").equals("some value"));
        assertTrue(contextedRuntimeException.getValue("crap") == null);
        assertTrue(contextedRuntimeException.getValue("test Poorly written obj") instanceof ObjectWithFaultyToString);
        
        assertTrue(contextedRuntimeException.getLabelSet().size() == 5);
        assertTrue(contextedRuntimeException.getLabelSet().contains("test1"));
        assertTrue(contextedRuntimeException.getLabelSet().contains("test2"));
        assertTrue(contextedRuntimeException.getLabelSet().contains("test Date"));
        assertTrue(contextedRuntimeException.getLabelSet().contains("test Nbr"));
        assertTrue(contextedRuntimeException.getLabelSet().contains("test Poorly written obj"));
        
        assertTrue(!contextedRuntimeException.getLabelSet().contains("crap"));

        contextedRuntimeException.addValue("test Poorly written obj", "replacement");
        
        String contextMessage = contextedRuntimeException.getFormattedExceptionMessage(null);
        assertTrue(contextMessage.indexOf(TEST_MESSAGE) == -1);
        assertTrue(contextedRuntimeException.getMessage().endsWith(contextMessage));
    }
    
    public void testNullExceptionPassing() {
        contextedRuntimeException = new ContextedRuntimeException(TEST_MESSAGE_2, new Exception(TEST_MESSAGE), null)
        .addValue("test1", null)
        .addValue("test2", "some value")
        .addValue("test Date", new Date())
        .addValue("test Nbr", new Integer(5))
        .addValue("test Poorly written obj", new ObjectWithFaultyToString());
        
        String message = contextedRuntimeException.getMessage();
        assertTrue(message != null);
    }

    public void testGetMessage() {
        testAddValue();
    }
    

}
