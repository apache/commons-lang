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
package org.apache.commons.lang3.math;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.Test;

/**
 * Unit tests {@link org.apache.commons.lang3.math.IEEE754rUtils}.
 * 
 * @version $Id$
 */
public class IEEE754rUtilsTest {

    @Test
    public void testLang381() {
        assertEquals(1.2, IEEE754rUtils.min(1.2, 2.5, Double.NaN), 0.01);
        assertEquals(2.5, IEEE754rUtils.max(1.2, 2.5, Double.NaN), 0.01);
        assertTrue(Double.isNaN(IEEE754rUtils.max(Double.NaN, Double.NaN,
                Double.NaN)));
        assertEquals(1.2f, IEEE754rUtils.min(1.2f, 2.5f, Float.NaN), 0.01);
        assertEquals(2.5f, IEEE754rUtils.max(1.2f, 2.5f, Float.NaN), 0.01);
        assertTrue(Float.isNaN(IEEE754rUtils.max(Float.NaN, Float.NaN,
                Float.NaN)));

        final double[] a = new double[] { 1.2, Double.NaN, 3.7, 27.0, 42.0,
                Double.NaN };
        assertEquals(42.0, IEEE754rUtils.max(a), 0.01);
        assertEquals(1.2, IEEE754rUtils.min(a), 0.01);

        final double[] b = new double[] { Double.NaN, 1.2, Double.NaN, 3.7,
                27.0, 42.0, Double.NaN };
        assertEquals(42.0, IEEE754rUtils.max(b), 0.01);
        assertEquals(1.2, IEEE754rUtils.min(b), 0.01);

        final float[] aF = new float[] { 1.2f, Float.NaN, 3.7f, 27.0f, 42.0f,
                Float.NaN };
        assertEquals(1.2f, IEEE754rUtils.min(aF), 0.01);
        assertEquals(42.0f, IEEE754rUtils.max(aF), 0.01);

        final float[] bF = new float[] { Float.NaN, 1.2f, Float.NaN, 3.7f,
                27.0f, 42.0f, Float.NaN };
        assertEquals(1.2f, IEEE754rUtils.min(bF), 0.01);
        assertEquals(42.0f, IEEE754rUtils.max(bF), 0.01);
    }

    @Test
    public void testEnforceExceptions() {
        try {
            IEEE754rUtils.min((float[]) null);
            fail("IllegalArgumentException expected for null input");
        } catch (final IllegalArgumentException iae) { /* expected */
        }

        try {
            IEEE754rUtils.min(new float[0]);
            fail("IllegalArgumentException expected for empty input");
        } catch (final IllegalArgumentException iae) { /* expected */
        }

        try {
            IEEE754rUtils.max((float[]) null);
            fail("IllegalArgumentException expected for null input");
        } catch (final IllegalArgumentException iae) { /* expected */
        }

        try {
            IEEE754rUtils.max(new float[0]);
            fail("IllegalArgumentException expected for empty input");
        } catch (final IllegalArgumentException iae) { /* expected */
        }

        try {
            IEEE754rUtils.min((double[]) null);
            fail("IllegalArgumentException expected for null input");
        } catch (final IllegalArgumentException iae) { /* expected */
        }

        try {
            IEEE754rUtils.min(new double[0]);
            fail("IllegalArgumentException expected for empty input");
        } catch (final IllegalArgumentException iae) { /* expected */
        }

        try {
            IEEE754rUtils.max((double[]) null);
            fail("IllegalArgumentException expected for null input");
        } catch (final IllegalArgumentException iae) { /* expected */
        }

        try {
            IEEE754rUtils.max(new double[0]);
            fail("IllegalArgumentException expected for empty input");
        } catch (final IllegalArgumentException iae) { /* expected */
        }

    }

    @Test
    public void testConstructorExists() {
        new IEEE754rUtils();
    }

    // min/max tests
    // ----------------------------------------------------------------------

    @Test(expected = IllegalArgumentException.class)
    public void testMinVADouble_nullArray() {
        IEEE754rUtils.min((double[]) null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testMinVADouble_emptyArray() {
        IEEE754rUtils.min(new double[0]);
    }

    @Test
    public void testMinVADouble() {
        assertEquals("min(double...) failed for array length 1", 5.12,
                IEEE754rUtils.min(5.12), 0);

        assertEquals("min(double...) failed for array length 2", 6.23,
                IEEE754rUtils.min(6.23, 9.34), 0);

        assertEquals("min(double[]) failed for array length 5", -10.45,
                IEEE754rUtils.min(-10.45, -5.56, 0, 5.67, 10.78), 0);
        assertEquals((double) -10, IEEE754rUtils.min((double) -10, (double) -5,
                (double) 0, (double) 5, (double) 10), 0.0001);
        assertEquals((double) -10, IEEE754rUtils.min((double) -5, (double) 0,
                (double) -10, (double) 5, (double) 10), 0.0001);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testMinVAFloat_nullArray() {
        IEEE754rUtils.min((float[]) null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testMinVAFloat_emptyArray() {
        IEEE754rUtils.min(new float[0]);
    }

    @Test
    public void testMinVAFloat() {
        assertEquals("min(float...) failed for array length 1", 5.9f,
                IEEE754rUtils.min(5.9f), 0);

        assertEquals("min(float...) failed for array length 2", 6.8f,
                IEEE754rUtils.min(6.8f, 9.7f), 0);

        assertEquals("min(float...) failed for array length 5", -10.6f,
                IEEE754rUtils.min(-10.6f, -5.5f, 0, 5.4f, 10.3f), 0);
        assertEquals(-10, IEEE754rUtils.min((float) -10, (float) -5, (float) 0,
                (float) 5, (float) 10), 0.0001f);
        assertEquals(-10, IEEE754rUtils.min((float) -5, (float) 0, (float) -10,
                (float) 5, (float) 10), 0.0001f);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testMaxVA_noArgs() {
        IEEE754rUtils.max();
    }

    @Test(expected = IllegalArgumentException.class)
    public void testMaxVALong_nullArray() {
        IEEE754rUtils.max();
    }

    @Test(expected = IllegalArgumentException.class)
    public void testMaxVADouble_nullArray() {
        IEEE754rUtils.max((double[]) null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testMaxVADouble_emptyArray() {
        IEEE754rUtils.max(new double[0]);
    }

    @Test
    public void testMaxVADouble() {
        final double[] d = null;
        try {
            IEEE754rUtils.max(d);
            fail("No exception was thrown for null input.");
        } catch (final IllegalArgumentException ex) {
        }

        try {
            IEEE754rUtils.max(new double[0]);
            fail("No exception was thrown for empty input.");
        } catch (final IllegalArgumentException ex) {
        }

        assertEquals("max(double...) failed for array length 1", (double) 5.1f,
                IEEE754rUtils.max((double) 5.1f), 0);

        assertEquals("max(double...) failed for array length 2", (double) 9.2f,
                IEEE754rUtils.max((double) 6.3f, (double) 9.2f), 0);

        assertEquals("max(double...) failed for float length 5",
                (double) 10.4f, IEEE754rUtils.max((double) -10.5f,
                        (double) -5.6f, (double) 0, (double) 5.7f,
                        (double) 10.4f), 0);
        assertEquals((double) 10, IEEE754rUtils.max((double) -10, (double) -5,
                (double) 0, (double) 5, (double) 10), 0.0001);
        assertEquals((double) 10, IEEE754rUtils.max((double) -5, (double) 0,
                (double) 10, (double) 5, (double) -10), 0.0001);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testMaxVAFloat_nullArray() {
        IEEE754rUtils.max((float[]) null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testMaxVAFloat_emptyArray() {
        IEEE754rUtils.max(new float[0]);
    }

    @Test
    public void testMaxVAFloat() {
        assertEquals("max(float...) failed for array length 1", 5.1f,
                IEEE754rUtils.max(new float[] { 5.1f }), 0);

        assertEquals("max(float...) failed for array length 2", 9.2f,
                IEEE754rUtils.max(new float[] { 6.3f, 9.2f }), 0);

        assertEquals("max(float...) failed for float length 5", 10.4f,
                IEEE754rUtils.max(-10.5f, -5.6f, 0, 5.7f, 10.4f), 0);
        assertEquals(10, IEEE754rUtils.max((float) (float) -10, (float) -5,
                (float) 0, (float) 5, (float) 10), 0.0001f);
        assertEquals(10, IEEE754rUtils.max((float) -5, (float) 0, (float) 10,
                (float) 5, (float) -10), 0.0001f);
    }

}
