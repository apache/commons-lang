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

import junit.framework.TestCase;

/**
 * JUnit tests.
 *
 * @version $Id: MutableNumberTest.java,v 1.1 2004/06/11 02:26:32 matth Exp $
 * @see MutableNumber
 */
public abstract class MutableNumberTest extends TestCase {

    public MutableNumberTest(String testName) {
        super(testName);
    }

    /**
     * Gets an instance to test.
     * @param value the value of the number.
     * @return a <code>MutableNumber</code>
     */
    public abstract MutableNumber getMutableNumber(double value);

    // ----------------------------------------------------------------
    // Converters
    // ----------------------------------------------------------------

    public abstract byte byteValue(double value);

    public abstract short shortValue(double value);

    public abstract int intValue(double value);

    public abstract long longValue(double value);

    public abstract float floatValue(double value);

    public abstract double doubleValue(double value);

    //  ----------------------------------------------------------------
    // Tests
    // ----------------------------------------------------------------

    public void testCompareTo() {
        final double num = 0;
        final MutableNumber mutNum = getMutableNumber(num);

        assertEquals("Equality", 0, mutNum.compareTo(new Double(num)));

        assertEquals(
            "Less than",
            -1,
            mutNum.compareTo(new Double(Double.POSITIVE_INFINITY)));

        assertEquals(
            "Greater than",
            1,
            mutNum.compareTo(new Double(Double.NEGATIVE_INFINITY)));
    }

    public void testPrimitiveAccessors() {
        testPrimitiveAccessors(0);
        testPrimitiveAccessors(Double.MAX_VALUE);
        testPrimitiveAccessors(-Double.MAX_VALUE);

        testPrimitiveAccessors(Float.MAX_VALUE);
        testPrimitiveAccessors(-Float.MAX_VALUE);

        testPrimitiveAccessors(Long.MAX_VALUE);
        testPrimitiveAccessors(Long.MIN_VALUE);

        testPrimitiveAccessors(Integer.MAX_VALUE);
        testPrimitiveAccessors(Integer.MIN_VALUE);

        testPrimitiveAccessors(Short.MAX_VALUE);
        testPrimitiveAccessors(Short.MIN_VALUE);

        testPrimitiveAccessors(Byte.MAX_VALUE);
        testPrimitiveAccessors(Byte.MIN_VALUE);
    }

    public void XtestObjectAccessors() {
        testObjectAccessors(0);
        testObjectAccessors(Double.MAX_VALUE);
        testObjectAccessors(-Double.MAX_VALUE);

        testObjectAccessors(Float.MAX_VALUE);
        testObjectAccessors(-Float.MAX_VALUE);

        testObjectAccessors(Long.MAX_VALUE);
        testObjectAccessors(Long.MIN_VALUE);

        testObjectAccessors(Integer.MAX_VALUE);
        testObjectAccessors(Integer.MIN_VALUE);

        testObjectAccessors(Short.MAX_VALUE);
        testObjectAccessors(Short.MIN_VALUE);

        testObjectAccessors(Byte.MAX_VALUE);
        testObjectAccessors(Byte.MIN_VALUE);
    }

    public void testSetValue() {
        setValueAndTestAccessors(Double.MAX_VALUE);
        setValueAndTestAccessors(-Double.MAX_VALUE);

        setValueAndTestAccessors(Float.MAX_VALUE);
        setValueAndTestAccessors(-Float.MAX_VALUE);

        setValueAndTestAccessors(Long.MAX_VALUE);
        setValueAndTestAccessors(Long.MIN_VALUE);

        setValueAndTestAccessors(Integer.MAX_VALUE);
        setValueAndTestAccessors(Integer.MIN_VALUE);

        setValueAndTestAccessors(Short.MAX_VALUE);
        setValueAndTestAccessors(Short.MIN_VALUE);

        setValueAndTestAccessors(Byte.MAX_VALUE);
        setValueAndTestAccessors(Byte.MIN_VALUE);
    }

    // ----------------------------------------------------------------
    // Private methods
    // ----------------------------------------------------------------

    private void setValueAndTestAccessors(double num) {
        final MutableNumber mutNum = getMutableNumber(0);
        mutNum.setValue(new Double(num));
        testPrimitiveAccessors(mutNum, num);
        //testObjectAccessors(mutNum, num);
    }

    private void testPrimitiveAccessors(double num) {
        testPrimitiveAccessors(getMutableNumber(num), num);
    }

    private void testPrimitiveAccessors(MutableNumber mutNum, double num) {
        assertEquals("byte comparison", byteValue(num), mutNum.byteValue());
        assertEquals("short comparison", shortValue(num), mutNum.shortValue());
        assertEquals("int comparison", intValue(num), mutNum.intValue());
        assertEquals("long comparison", longValue(num), mutNum.longValue());

        assertEquals(
            "float comparison",
            floatValue(num),
            mutNum.floatValue(),
            0);

        assertEquals(
            "double comparison",
            doubleValue(num),
            mutNum.doubleValue(),
            0);
    }

    private void testObjectAccessors(double num) {
        testObjectAccessors(getMutableNumber(num), num);
    }

    private void testObjectAccessors(MutableNumber mutNum, double num) {
        assertEquals(
            "byte comparison",
            new Byte(byteValue(num)),
            mutNum.getValue());

        assertEquals(
            "short comparison",
            new Short(shortValue(num)),
            mutNum.getValue());

        assertEquals(
            "int comparison",
            new Integer(intValue(num)),
            mutNum.getValue());

        assertEquals(
            "long comparison",
            new Long(longValue(num)),
            mutNum.getValue());

        assertEquals(
            "float comparison",
            new Float(floatValue(num)),
            mutNum.getValue());

        assertEquals(
            "double comparison",
            new Double(doubleValue(num)),
            mutNum.getValue());
    }

}
