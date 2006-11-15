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

package org.apache.commons.lang.mutable;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * JUnit tests.
 * 
 * @version $Id$
 */
public class MutableTestSuite extends TestCase {

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public MutableTestSuite(String name) {
        super(name);
    }

    public static Test suite() {
        final TestSuite suite = new TestSuite();

        suite.addTest(MutableBooleanTest.suite());
        suite.addTest(MutableByteTest.suite());
        suite.addTest(MutableShortTest.suite());
        suite.addTest(MutableIntTest.suite());
        suite.addTest(MutableLongTest.suite());
        suite.addTest(MutableFloatTest.suite());
        suite.addTest(MutableDoubleTest.suite());
        suite.addTest(MutableObjectTest.suite());

        return suite;
    }

}
