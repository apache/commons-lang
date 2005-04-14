/*
 * Copyright 2002-2005 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
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
 * Test suite for the Lang Builder package.
 *
 * @author Stephen Colebourne
 * @version $Id$
 */
public class BuilderTestSuite extends TestCase {
    
    /**
     * Construct a new instance.
     */
    public BuilderTestSuite(String name) {
        super(name);
    }

    /**
     * Command-line interface.
     */
    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    /**
     * Get the suite of tests
     */
    public static Test suite() {
        TestSuite suite = new TestSuite();
        suite.setName("Commons-Lang-Builder Tests");
        suite.addTest(CompareToBuilderTest.suite());
        suite.addTest(EqualsBuilderTest.suite());
        suite.addTest(HashCodeBuilderTest.suite());
        suite.addTest(HashCodeBuilderAndEqualsBuilderTest.suite());
        suite.addTest(ToStringBuilderTest.suite());
        suite.addTest(DefaultToStringStyleTest.suite());
        suite.addTest(NoFieldNamesToStringStyleTest.suite());
        suite.addTest(MultiLineToStringStyleTest.suite());
        suite.addTest(SimpleToStringStyleTest.suite());
        suite.addTest(StandardToStringStyleTest.suite());
        suite.addTest(ToStringStyleTest.suite());
        return suite;
    }
}
