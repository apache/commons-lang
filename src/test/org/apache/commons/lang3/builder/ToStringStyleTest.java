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
package org.apache.commons.lang.builder;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Test case for ToStringStyle.
 * 
 * @author Masato Tezuka
 * @version $Id$
 */
public class ToStringStyleTest extends TestCase {

    public ToStringStyleTest(String name) {
        super(name);
    }

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(ToStringStyleTest.class);
        suite.setName("ToStringStyle Tests");
        return suite;
    }

    @Override
    protected void setUp() throws Exception {
        super.setUp();
    }

    private static class ToStringStyleImpl extends ToStringStyle {
    }

    //-----------------------------------------------------------------------
    public void testSetArrayStart() {
        ToStringStyle style = new ToStringStyleImpl();
        style.setArrayStart(null);
        assertEquals("", style.getArrayStart());
    }

    public void testSetArrayEnd() {
        ToStringStyle style = new ToStringStyleImpl();
        style.setArrayEnd(null);
        assertEquals("", style.getArrayEnd());
    }

    public void testSetArraySeparator() {
        ToStringStyle style = new ToStringStyleImpl();
        style.setArraySeparator(null);
        assertEquals("", style.getArraySeparator());
    }

    public void testSetContentStart() {
        ToStringStyle style = new ToStringStyleImpl();
        style.setContentStart(null);
        assertEquals("", style.getContentStart());
    }

    public void testSetContentEnd() {
        ToStringStyle style = new ToStringStyleImpl();
        style.setContentEnd(null);
        assertEquals("", style.getContentEnd());
    }

    public void testSetFieldNameValueSeparator() {
        ToStringStyle style = new ToStringStyleImpl();
        style.setFieldNameValueSeparator(null);
        assertEquals("", style.getFieldNameValueSeparator());
    }

    public void testSetFieldSeparator() {
        ToStringStyle style = new ToStringStyleImpl();
        style.setFieldSeparator(null);
        assertEquals("", style.getFieldSeparator());
    }

    public void testSetNullText() {
        ToStringStyle style = new ToStringStyleImpl();
        style.setNullText(null);
        assertEquals("", style.getNullText());
    }

    public void testSetSizeStartText() {
        ToStringStyle style = new ToStringStyleImpl();
        style.setSizeStartText(null);
        assertEquals("", style.getSizeStartText());
    }

    public void testSetSizeEndText() {
        ToStringStyle style = new ToStringStyleImpl();
        style.setSizeEndText(null);
        assertEquals("", style.getSizeEndText());
    }

    public void testSetSummaryObjectStartText() {
        ToStringStyle style = new ToStringStyleImpl();
        style.setSummaryObjectStartText(null);
        assertEquals("", style.getSummaryObjectStartText());
    }

    public void testSetSummaryObjectEndText() {
        ToStringStyle style = new ToStringStyleImpl();
        style.setSummaryObjectEndText(null);
        assertEquals("", style.getSummaryObjectEndText());
    }

    /**
     * An object used to test {@link ToStringStyle}.
     * 
     * @author Scott Bassin
     */
    static class Person {
        /**
         * Test String field.
         */
        String name;

        /**
         * Test integer field.
         */
        int age;

        /**
         * Test boolean field.
         */
        boolean smoker;
    }
}
