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
package org.apache.commons.lang.mutable;

import junit.framework.Test;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * JUnit tests.
 * 
 * @version $Id: MutableFloatTest.java,v 1.1 2004/06/11 02:26:32 matth Exp $
 * @see MutableFloat
 */
public class MutableFloatTest extends MutableNumberTest {

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
        return new TestSuite(MutableFloatTest.class);
    }

    /**
     * @param testName
     */
    public MutableFloatTest(String testName) {
        super(testName);
    }

    public MutableNumber getMutableNumber(double value) {
        return new MutableFloat((float)value);
    }

    //  Converters
    // ----------------------------------------------------------------
    public byte byteValue(double value) {
        return (byte)(float)value;
    }

    public short shortValue(double value) {
        return (short)(float)value;
    }

    public int intValue(double value) {
        return (int)(float)value;
    }

    public long longValue(double value) {
        return (long)(float)value;
    }

    public float floatValue(double value) {
        return (float)value;
    }

    public double doubleValue(double value) {
        return (float)value;
    }

}
