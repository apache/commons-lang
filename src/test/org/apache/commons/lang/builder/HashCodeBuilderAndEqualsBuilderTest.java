/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002-2003 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
 *
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 */
package org.apache.commons.lang.builder;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Tests HashCodeBuilder and EqualsBuilderTest to insure that equal 
 * objects must have equal hash codes.
 * 
 * @author Gary Gregory
 * @version $Id: HashCodeBuilderAndEqualsBuilderTest.java,v 1.2 2003/05/21 23:49:15 scolebourne Exp $
 */
public class HashCodeBuilderAndEqualsBuilderTest extends TestCase {

    /**
     * Constructor for HashCodeBuilderAndEqualsBuilderTest.
     * @param name
     */
    public HashCodeBuilderAndEqualsBuilderTest(String name) {
        super(name);
    }

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(HashCodeBuilderAndEqualsBuilderTest.class);
        suite.setName("HashCodeBuilderAndEqualsBuilder Tests");
        return suite;
    }

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    //-----------------------------------------------------------------------

    public void testInteger(boolean testTransients) {
        Integer i1 = new Integer(12345);
        Integer i2 = new Integer(12345);
        assertEqualsAndHashCodeContract(i1, i2, testTransients);
    }

    public void testInteger() {
        testInteger(false);
    }

    public void testIntegerWithTransients() {
        testInteger(true);
    }

    public void testFixture() {
        testFixture(false);
    }

    public void testFixtureWithTransients() {
        testFixture(true);
    }

    public void testFixture(boolean testTransients) {
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
    public void assertEqualsAndHashCodeContract(Object lhs, Object rhs, boolean testTransients) {
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
