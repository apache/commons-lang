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
package org.apache.commons.lang3.builder;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

/**
 * Tests {@link org.apache.commons.lang3.builder.HashCodeBuilder} and
 * {@link org.apache.commons.lang3.builder.EqualsBuilderTest} to insure that equal
 * objects must have equal hash codes.
 * 
 * @version $Id$
 */
public class HashCodeBuilderAndEqualsBuilderTest {

    //-----------------------------------------------------------------------

    private void testInteger(boolean testTransients) {
        Integer i1 = Integer.valueOf(12345);
        Integer i2 = Integer.valueOf(12345);
        assertEqualsAndHashCodeContract(i1, i2, testTransients);
    }

    @Test
    public void testInteger() {
        testInteger(false);
    }

    @Test
    public void testIntegerWithTransients() {
        testInteger(true);
    }

    @Test
    public void testFixture() {
        testFixture(false);
    }

    @Test
    public void testFixtureWithTransients() {
        testFixture(true);
    }

    private void testFixture(boolean testTransients) {
        assertEqualsAndHashCodeContract(new TestFixture(2, 'c', "Test", (short) 2), new TestFixture(2, 'c', "Test", (short) 2), testTransients);
        assertEqualsAndHashCodeContract(
            new AllTransientFixture(2, 'c', "Test", (short) 2),
            new AllTransientFixture(2, 'c', "Test", (short) 2),
            testTransients);
        assertEqualsAndHashCodeContract(
            new SubTestFixture(2, 'c', "Test", (short) 2, "Same"),
            new SubTestFixture(2, 'c', "Test", (short) 2, "Same"),
            testTransients);
        assertEqualsAndHashCodeContract(
            new SubAllTransientFixture(2, 'c', "Test", (short) 2, "Same"),
            new SubAllTransientFixture(2, 'c', "Test", (short) 2, "Same"),
            testTransients);
    }

    /**
     * Asserts that if <code>lhs</code> equals <code>rhs</code> 
     * then their hash codes MUST be identical.
     * 
     * @param lhs The Left-Hand-Side of the equals test
     * @param rhs The Right-Hand-Side of the equals test
     * @param testTransients wether to test transient fields
     */
    private void assertEqualsAndHashCodeContract(Object lhs, Object rhs, boolean testTransients) {
        if (EqualsBuilder.reflectionEquals(lhs, rhs, testTransients)) {
            // test a couple of times for consistency.
            assertEquals(HashCodeBuilder.reflectionHashCode(lhs, testTransients), HashCodeBuilder.reflectionHashCode(rhs, testTransients));
            assertEquals(HashCodeBuilder.reflectionHashCode(lhs, testTransients), HashCodeBuilder.reflectionHashCode(rhs, testTransients));
            assertEquals(HashCodeBuilder.reflectionHashCode(lhs, testTransients), HashCodeBuilder.reflectionHashCode(rhs, testTransients));
        }
    }

    static class TestFixture {
        int i;
        char c;
        String string;
        short s;

        TestFixture(int i, char c, String string, short s) {
            this.i = i;
            this.c = c;
            this.string = string;
            this.s = s;
        }
    }

    static class SubTestFixture extends TestFixture {
        transient String tString;

        SubTestFixture(int i, char c, String string, short s, String tString) {
            super(i, c, string, s);
            this.tString = tString;
        }
    }

    static class AllTransientFixture {
        transient int i;
        transient char c;
        transient String string;
        transient short s;

        AllTransientFixture(int i, char c, String string, short s) {
            this.i = i;
            this.c = c;
            this.string = string;
            this.s = s;
        }
    }

    static class SubAllTransientFixture extends AllTransientFixture {
        transient String tString;

        SubAllTransientFixture(int i, char c, String string, short s, String tString) {
            super(i, c, string, s);
            this.tString = tString;
        }
    }


}
