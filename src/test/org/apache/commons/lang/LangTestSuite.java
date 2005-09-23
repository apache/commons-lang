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
package org.apache.commons.lang;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Test suite for the Lang package.
 *
 * @author Stephen Colebourne
 * @author <a href="mailto:ridesmet@users.sourceforge.net">Ringo De Smet</a>
 * @author Matthew Hawthorne
 * @version $Id$
 */
public class LangTestSuite extends TestCase {
    
    /**
     * Construct a new instance.
     */
    public LangTestSuite(String name) {
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
        suite.setName("Commons-Lang Tests");
        suite.addTest(ArrayUtilsTest.suite());
        suite.addTest(ArrayUtilsAddTest.suite());
        suite.addTest(ArrayUtilsRemoveTest.suite());
        suite.addTest(BitFieldTest.suite());
        suite.addTest(BooleanUtilsTest.suite());
        suite.addTest(CharEncodingTest.suite());
        suite.addTest(CharRangeTest.suite());
        suite.addTest(CharSetTest.suite());
        suite.addTest(CharSetUtilsTest.suite());
        suite.addTest(CharUtilsTest.suite());
        suite.addTest(ClassUtilsTest.suite());
        suite.addTest(EntitiesTest.suite());
        suite.addTest(IllegalClassExceptionTest.suite());
        suite.addTest(IncompleteArgumentExceptionTest.suite());
        suite.addTest(IntHashMapTest.suite());
        suite.addTest(LocaleUtilsTest.suite());
        suite.addTest(NotImplementedExceptionTest.suite());
        suite.addTest(NullArgumentExceptionTest.suite());
        suite.addTest(NumberRangeTest.suite());
        suite.addTest(NumberUtilsTest.suite());
        suite.addTest(ObjectUtilsTest.suite());
        suite.addTest(RandomStringUtilsTest.suite());
        suite.addTest(SerializationUtilsTest.suite());
        suite.addTest(StringUtilsTest.suite());
        suite.addTest(StringUtilsTrimEmptyTest.suite());
        suite.addTest(StringUtilsSubstringTest.suite());
        suite.addTest(StringUtilsEqualsIndexOfTest.suite());
        suite.addTest(StringUtilsIsTest.suite());
        suite.addTest(StringEscapeUtilsTest.suite());
        suite.addTest(SystemUtilsTest.suite());
        suite.addTest(UnhandledExceptionTest.suite());
        suite.addTest(ValidateTest.suite());
        suite.addTest(WordUtilsTest.suite());
        return suite;
    }
}
