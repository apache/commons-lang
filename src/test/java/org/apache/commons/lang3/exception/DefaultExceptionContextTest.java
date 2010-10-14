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

import org.apache.commons.lang3.exception.ContextedExceptionTest.ObjectWithFaultyToString;

/**
 * JUnit tests for DefaultExceptionContext.
 * @author D. Ashmore
 *
 */
public class DefaultExceptionContextTest extends TestCase {
    
    private ExceptionContext defaultExceptionContext;

    public DefaultExceptionContextTest(String name) {
        super(name);
    }
    
    @Override
    public void setUp() {
        defaultExceptionContext = new DefaultExceptionContext()
        .addValue("test1", null)
        .addValue("test2", "some value")
        .addValue("test Date", new Date())
        .addValue("test Nbr", new Integer(5))
        .addValue("test Poorly written obj", new ObjectWithFaultyToString());
    }
    
    public void testAddValue() {
        defaultExceptionContext.addValue("test2", "different value");
                
        String message = defaultExceptionContext.getFormattedExceptionMessage("This is an error");
        assertTrue(message.indexOf("This is an error")>=0);
        assertTrue(message.indexOf("test1")>=0);
        assertTrue(message.indexOf("test2")>=0);
        assertTrue(message.indexOf("test2[1]")>=0);
        assertTrue(message.indexOf("test Date")>=0);
        assertTrue(message.indexOf("test Nbr")>=0);
        assertTrue(message.indexOf("test Poorly written obj")>=0);
        assertTrue(message.indexOf("some value")>=0);
        assertTrue(message.indexOf("different value")>=0);
        assertTrue(message.indexOf("5")>=0);
        assertTrue(message.indexOf("Crap")>=0);
    }
    
    public void testReplaceValue() {
        defaultExceptionContext.replaceValue("test2", "different value");
        defaultExceptionContext.replaceValue("test3", "3");
                
        String message = defaultExceptionContext.getFormattedExceptionMessage("This is an error");
        assertTrue(message.indexOf("This is an error")>=0);
        assertTrue(message.indexOf("test1")>=0);
        assertTrue(message.indexOf("test2")>=0);
        assertTrue(message.indexOf("test3")>=0);
        assertTrue(message.indexOf("test Date")>=0);
        assertTrue(message.indexOf("test Nbr")>=0);
        assertTrue(message.indexOf("test Poorly written obj")>=0);
        assertTrue(message.indexOf("different value")>=0);
        assertTrue(message.indexOf("5")>=0);
        assertTrue(message.indexOf("Crap")>=0);

        assertTrue(message.indexOf("test2[1]")<0);
        assertTrue(message.indexOf("some value")<0);
}
    
    public void testFormattedExceptionMessageNull() {
        defaultExceptionContext = new DefaultExceptionContext();
        defaultExceptionContext.getFormattedExceptionMessage(null);
    }
    
    public void testGetValue() {
        assertTrue(defaultExceptionContext.getValue("test1") == null);
        assertTrue(defaultExceptionContext.getValue("test2").equals("some value"));
        assertTrue(defaultExceptionContext.getValue("crap") == null);
        assertTrue(defaultExceptionContext.getValue("test Poorly written obj") instanceof ObjectWithFaultyToString);
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
