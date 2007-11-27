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
package org.apache.commons.lang.text;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Test suite for the Text package.
 *
 * @author Stephen Colebourne
 * @version $Id$
 */
public class TextTestSuite extends TestCase {
    
    /**
     * Construct a new instance.
     */
    public TextTestSuite(String name) {
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
        suite.setName("Commons-Lang-Text Tests");
        suite.addTest(CompositeFormatTest.suite());
        suite.addTest(StrBuilderTest.suite());
        suite.addTest(StrBuilderAppendInsertTest.suite());
        suite.addTest(StrLookupTest.suite());
        suite.addTest(StrMatcherTest.suite());
        suite.addTest(StrSubstitutorTest.suite());
        suite.addTest(StrTokenizerTest.suite());
        suite.addTestSuite(MultiFormatTest.class);
        suite.addTestSuite(MessageFormatTest.US.class);
        suite.addTestSuite(MessageFormatTest.UK.class);
        suite.addTestSuite(MessageFormatTest.DE.class);
        suite.addTestSuite(MessageFormatTest.IT.class);
        suite.addTestSuite(MessageFormatTest.JP.class);
        suite.addTestSuite(MessageFormatTest.CA.class);
        suite.addTestSuite(MessageFormatTest.CN.class);
        suite.addTestSuite(MessageFormatTest.FR.class);
        suite.addTestSuite(MessageFormatTest.KR.class);
        suite.addTestSuite(MessageFormatTest.TW.class);
        suite.addTestSuite(ExtendedMessageFormatBaselineTest.US.class);
        suite.addTestSuite(ExtendedMessageFormatBaselineTest.UK.class);
        suite.addTestSuite(ExtendedMessageFormatBaselineTest.DE.class);
        suite.addTestSuite(ExtendedMessageFormatBaselineTest.IT.class);
        suite.addTestSuite(ExtendedMessageFormatBaselineTest.JP.class);
        suite.addTestSuite(ExtendedMessageFormatBaselineTest.CA.class);
        suite.addTestSuite(ExtendedMessageFormatBaselineTest.CN.class);
        suite.addTestSuite(ExtendedMessageFormatBaselineTest.FR.class);
        suite.addTestSuite(ExtendedMessageFormatBaselineTest.KR.class);
        suite.addTestSuite(ExtendedMessageFormatBaselineTest.TW.class);
        suite.addTestSuite(MessageFormatExtensionTest.US.class);
        suite.addTestSuite(MessageFormatExtensionTest.UK.class);
        suite.addTestSuite(MessageFormatExtensionTest.DE.class);
        suite.addTestSuite(MessageFormatExtensionTest.IT.class);
        suite.addTestSuite(MessageFormatExtensionTest.JP.class);
        suite.addTestSuite(MessageFormatExtensionTest.CA.class);
        suite.addTestSuite(MessageFormatExtensionTest.CN.class);
        suite.addTestSuite(MessageFormatExtensionTest.FR.class);
        suite.addTestSuite(MessageFormatExtensionTest.KR.class);
        suite.addTestSuite(MessageFormatExtensionTest.TW.class);
        return suite;
    }

}
