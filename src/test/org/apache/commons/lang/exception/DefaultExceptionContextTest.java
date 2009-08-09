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
package org.apache.commons.lang.exception;

import java.util.Date;

import org.apache.commons.lang.exception.ContextedExceptionTest.ObjectWithFaultyToString;

import junit.framework.TestCase;

/**
 * JUnit tests for DefaultExceptionContext
 * @author D. Ashmore
 *
 */
public class DefaultExceptionContextTest extends TestCase {
    
    private ExceptionContext defaultExceptionContext;

    public DefaultExceptionContextTest(String name) {
        super(name);
    }
    
    public void setUp() {
        defaultExceptionContext = new DefaultExceptionContext()
        .addLabeledValue("test1", null)
        .addLabeledValue("test2", "some value")
        .addLabeledValue("test Date", new Date())
        .addLabeledValue("test Nbr", new Integer(5))
        .addLabeledValue("test Poorly written obj", new ObjectWithFaultyToString());
    }
    
    public void testAddLabeledValue() {
                
        String message = defaultExceptionContext.getFormattedExceptionMessage("This is an error");
        assertTrue(message.indexOf("This is an error")>=0);
        assertTrue(message.indexOf("test1")>=0);
        assertTrue(message.indexOf("test2")>=0);
        assertTrue(message.indexOf("test Date")>=0);
        assertTrue(message.indexOf("test Nbr")>=0);
        assertTrue(message.indexOf("test Poorly written obj")>=0);
        assertTrue(message.indexOf("some value")>=0);
        assertTrue(message.indexOf("5")>=0);
        assertTrue(message.indexOf("Crap")>=0);
        
        //contextedException.printStackTrace();
    }
    
    public void testFormattedExceptionMessageNull() {
        defaultExceptionContext = new DefaultExceptionContext();
        defaultExceptionContext.getFormattedExceptionMessage(null);
    }
    
    public void testGetLabeledValue() {
        assertTrue(defaultExceptionContext.getLabeledValue("test1") == null);
        assertTrue(defaultExceptionContext.getLabeledValue("test2").equals("some value"));
        assertTrue(defaultExceptionContext.getLabeledValue("crap") == null);
        assertTrue(defaultExceptionContext.getLabeledValue("test Poorly written obj") instanceof ObjectWithFaultyToString);
    }
    
    public void testGetLabelSet() {
        assertTrue(defaultExceptionContext.getLabelSet().size() == 5);
        assertTrue(defaultExceptionContext.getLabelSet().contains("test1"));
        assertTrue(defaultExceptionContext.getLabelSet().contains("test2"));
        assertTrue(defaultExceptionContext.getLabelSet().contains("test Date"));
        assertTrue(defaultExceptionContext.getLabelSet().contains("test Nbr"));
        assertTrue(defaultExceptionContext.getLabelSet().contains("test Poorly written obj"));
        
        assertTrue(!defaultExceptionContext.getLabelSet().contains("crap"));
    }

}
