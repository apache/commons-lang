/*
 * Copyright 2002-2004 The Apache Software Foundation.
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

import java.util.Map;
import java.util.HashMap;

/**
 * Unit tests {@link org.apache.commons.lang.Interpolation}.
 *
 * @author Henri Yandell
 * @author Ken Fitzpatrick
 * @version $Id: InterpolationTest.java,v 1.1 2004/07/04 04:51:25 bayard Exp $
 */
public class InterpolationTest extends TestCase {

    private static final String INPUT_TEMPLATE     = "The ${animal} jumped over the ${target}.";
    private static final String EXPECTED_RESULTS_1 = "The quick brown fox jumped over the lazy dog.";
    private static final String EXPECTED_RESULTS_2 = "The cow jumped over the moon.";

    public InterpolationTest(String name) {
        super(name);
    }

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
    	TestSuite suite = new TestSuite(InterpolationTest.class);
    	suite.setName("Interpolation Tests");
        return suite;
    }

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    public void testSimpleVariableSubstitution() {

        // test case: "The quick brown fox jumped over the lazy dog."
        Map valuesMap = new HashMap();
        valuesMap.put( "animal", "quick brown fox" );
        valuesMap.put( "target", "lazy dog" );
        assertEquals( "Test case 1: simple variable substitution", EXPECTED_RESULTS_1,
            Interpolation.interpolate( INPUT_TEMPLATE, valuesMap) );

        // test case: "The cow jumped over the moon."
        valuesMap = new HashMap();
        valuesMap.put( "animal", "cow" );
        valuesMap.put( "target", "moon" );
        assertEquals( "Test case 2: template reuse, different results" ,EXPECTED_RESULTS_2,
            Interpolation.interpolate( INPUT_TEMPLATE, valuesMap) );
    }

    public void testNullMap() {
        // negative test case: Map == null
        Map valuesMap = null;
        assertEquals( "Test case 3: Map == null", INPUT_TEMPLATE,
            Interpolation.interpolate( INPUT_TEMPLATE, valuesMap) );
    }

    public void testEmptyMap() {
        // negative test case: Map.isEmpty()
        Map valuesMap = new HashMap();
        assertEquals( "Test case 4: Map.isEmpty()", INPUT_TEMPLATE,
            Interpolation.interpolate( INPUT_TEMPLATE, valuesMap) );
    }

    public void testNullTemplate() {
        // negative test case: INPUT_TEMPLATE == null
        Map valuesMap = new HashMap();
        valuesMap.put( "animal", "cow" );
        valuesMap.put( "target", "moon" );
        assertNull( "Test case 5: template == null",
            Interpolation.interpolate( null, valuesMap) );
    }

    public void testRecursive() {
        // test case: process repeatedly
        Map valuesMap = new HashMap();
        valuesMap.put( "animal", "${critter}" );
        valuesMap.put( "target", "${pet}" );
        valuesMap.put( "pet", "${petCharacteristic} dog" );
        valuesMap.put( "petCharacteristic", "lazy" );
        valuesMap.put( "critter", "${critterSpeed} ${critterColor} ${critterType}" );
        valuesMap.put( "critterSpeed", "quick" );
        valuesMap.put( "critterColor", "brown" );
        valuesMap.put( "critterType", "fox" );
        assertEquals( "Test case 6: interpolateRepeatedly", EXPECTED_RESULTS_1,
            Interpolation.interpolateRepeatedly( INPUT_TEMPLATE, valuesMap ) );

        // test case: process repeatedly
        valuesMap = new HashMap();
        valuesMap.put( "animal", "cow" );
        valuesMap.put( "target", "${celestialObject}" );
        valuesMap.put( "celestialObject", "moon" );
        assertEquals( "Test case 8: interpolateRepeatedly", EXPECTED_RESULTS_2,
            Interpolation.interpolateRepeatedly( INPUT_TEMPLATE, valuesMap ) );
    }

}
