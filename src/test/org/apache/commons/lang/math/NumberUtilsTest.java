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
 *    any, must include the following acknowledgement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowledgement may appear in the software itself,
 *    if and wherever such third-party acknowledgements normally appear.
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
package org.apache.commons.lang.math;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.math.BigDecimal;
import java.math.BigInteger;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

import org.apache.commons.lang.SystemUtils;

/**
 * Unit tests {@link org.apache.commons.lang.math.NumberUtils}.
 *
 * @author <a href="mailto:rand_mcneely@yahoo.com">Rand McNeely</a>
 * @author <a href="mailto:ridesmet@users.sourceforge.net">Ringo De Smet</a>
 * @author Eric Pugh
 * @author Phil Steitz
 * @author Stephen Colebourne
 * @author Matthew Hawthorne
 * @author <a href="mailto:ggregory@seagullsw.com">Gary Gregory</a>
 * @version $Id: NumberUtilsTest.java,v 1.9 2003/09/05 15:55:09 psteitz Exp $
 */
public class NumberUtilsTest extends TestCase {

    public NumberUtilsTest(String name) {
        super(name);
    }

    public static void main(String[] args) {
        TestRunner.run(suite());
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(NumberUtilsTest.class);
        suite.setName("NumberUtils Tests");
        return suite;
    }

    //-----------------------------------------------------------------------
    public void testConstructor() {
        assertNotNull(new NumberUtils());
        Constructor[] cons = NumberUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertEquals(true, Modifier.isPublic(cons[0].getModifiers()));
        assertEquals(true, Modifier.isPublic(NumberUtils.class.getModifiers()));
        assertEquals(false, Modifier.isFinal(NumberUtils.class.getModifiers()));
    }
    
    //---------------------------------------------------------------------

    /**
     * Test for int stringToInt(String)
     */
    public void testStringToIntString() {
        assertTrue("stringToInt(String) 1 failed", NumberUtils.stringToInt("12345") == 12345);
        assertTrue("stringToInt(String) 2 failed", NumberUtils.stringToInt("abc") == 0);
        assertTrue("stringToInt(empty) failed", NumberUtils.stringToInt("") == 0);
        assertTrue("stringToInt(null) failed", NumberUtils.stringToInt(null) == 0);
    }

    /**
     * Test for int stringToInt(String, int)
     */
    public void testStringToIntStringI() {
        assertTrue("stringToInt(String,int) 1 failed", NumberUtils.stringToInt("12345", 5) == 12345);
        assertTrue("stringToInt(String,int) 2 failed", NumberUtils.stringToInt("1234.5", 5) == 5);
    }

    /**
     * Test for long stringToLong(String)
     */
    public void testStringToLongString() {
        assertTrue("stringToLong(String) 1 failed", NumberUtils.stringToLong("12345") == 12345l);
        assertTrue("stringToLong(String) 2 failed", NumberUtils.stringToLong("abc") == 0l);
        assertTrue("stringToLong(String) 3 failed", NumberUtils.stringToLong("1L") == 0l);
        assertTrue("stringToLong(String) 4 failed", NumberUtils.stringToLong("1l") == 0l);
        assertTrue("stringToLong(Long.MAX_VALUE) failed", NumberUtils.stringToLong(Long.MAX_VALUE+"") == Long.MAX_VALUE);
        assertTrue("stringToLong(Long.MIN_VALUE) failed", NumberUtils.stringToLong(Long.MIN_VALUE+"") == Long.MIN_VALUE);
        assertTrue("stringToLong(empty) failed", NumberUtils.stringToLong("") == 0l);
        assertTrue("stringToLong(null) failed", NumberUtils.stringToLong(null) == 0l);
    }

    /**
     * Test for long stringToLong(String, long)
     */
    public void testStringToLongStringL() {
        assertTrue("stringToLong(String,long) 1 failed", NumberUtils.stringToLong("12345", 5l) == 12345l);
        assertTrue("stringToLong(String,long) 2 failed", NumberUtils.stringToLong("1234.5", 5l) == 5l);
    }

    /**
     * Test for float stringToFloat(String)
     */
    public void testStringToFloatString() {
        assertTrue("stringToFloat(String) 1 failed", NumberUtils.stringToFloat("-1.2345") == -1.2345f);
        assertTrue("stringToFloat(String) 2 failed", NumberUtils.stringToFloat("1.2345") == 1.2345f);
        assertTrue("stringToFloat(String) 3 failed", NumberUtils.stringToFloat("abc") == 0.0f);
        assertTrue("stringToFloat(Float.MAX_VALUE) failed", NumberUtils.stringToFloat(Float.MAX_VALUE+"") ==  Float.MAX_VALUE);
        assertTrue("stringToFloat(Float.MIN_VALUE) failed", NumberUtils.stringToFloat(Float.MIN_VALUE+"") == Float.MIN_VALUE);
        assertTrue("stringToFloat(empty) failed", NumberUtils.stringToFloat("") == 0.0f);
        assertTrue("stringToFloat(null) failed", NumberUtils.stringToFloat(null) == 0.0f);
    }

    /**
     * Test for float stringToFloat(String, float)
     */
    public void testStringToFloatStringF() {
        assertTrue("stringToFloat(String,int) 1 failed", NumberUtils.stringToFloat("1.2345", 5.1f) == 1.2345f);
        assertTrue("stringToFloat(String,int) 2 failed", NumberUtils.stringToFloat("a", 5.0f) == 5.0f);
    }

    /**
     * Test for double stringToDouble(String)
     */
    public void testStringToDoubleString() {
        assertTrue("stringToDouble(String) 1 failed", NumberUtils.stringToDouble("-1.2345") == -1.2345d);
        assertTrue("stringToDouble(String) 2 failed", NumberUtils.stringToDouble("1.2345") == 1.2345d);
        assertTrue("stringToDouble(String) 3 failed", NumberUtils.stringToDouble("abc") == 0.0d);
        assertTrue("stringToDouble(Double.MAX_VALUE) failed", NumberUtils.stringToDouble(Double.MAX_VALUE+"") == Double.MAX_VALUE);
        assertTrue("stringToDouble(Double.MIN_VALUE) failed", NumberUtils.stringToDouble(Double.MIN_VALUE+"") == Double.MIN_VALUE);
        assertTrue("stringToDouble(empty) failed", NumberUtils.stringToDouble("") == 0.0d);
        assertTrue("stringToDouble(null) failed", NumberUtils.stringToDouble(null) == 0.0d);
    }

    /**
     * Test for double stringToFloat(String, float)
     */
    public void testStringToDoubleStringD() {
        assertTrue("stringToDouble(String,int) 1 failed", NumberUtils.stringToDouble("1.2345", 5.1d) == 1.2345d);
        assertTrue("stringToDouble(String,int) 2 failed", NumberUtils.stringToDouble("a", 5.0d) == 5.0d);
    }

    public void testCreateNumber() {
        //a lot of things can go wrong
        assertEquals("createNumber(String) 1 failed", new Float("1234.5"), NumberUtils.createNumber("1234.5"));
        assertEquals("createNumber(String) 2 failed", new Integer("12345"), NumberUtils.createNumber("12345"));
        assertEquals("createNumber(String) 3 failed", new Double("1234.5"), NumberUtils.createNumber("1234.5D"));
        assertEquals("createNumber(String) 4 failed", new Float("1234.5"), NumberUtils.createNumber("1234.5F"));
        assertEquals("createNumber(String) 5 failed", new Long(Integer.MAX_VALUE + 1L), NumberUtils.createNumber("" + (Integer.MAX_VALUE + 1L)));
        assertEquals("createNumber(String) 6 failed", new Long(12345), NumberUtils.createNumber("12345L"));
        assertEquals("createNumber(String) 7 failed", new Float("-1234.5"), NumberUtils.createNumber("-1234.5"));
        assertEquals("createNumber(String) 8 failed", new Integer("-12345"), NumberUtils.createNumber("-12345"));
        assertTrue("createNumber(String) 9 failed", 0xFADE == NumberUtils.createNumber("0xFADE").intValue());
        assertTrue("createNumber(String) 10 failed", -0xFADE == NumberUtils.createNumber("-0xFADE").intValue());
        assertEquals("createNumber(String) 11 failed", new Double("1.1E200"), NumberUtils.createNumber("1.1E200"));
        assertEquals("createNumber(String) 12 failed", new Float("1.1E20"), NumberUtils.createNumber("1.1E20"));
        assertEquals("createNumber(String) 13 failed", new Double("-1.1E200"), NumberUtils.createNumber("-1.1E200"));
        assertEquals("createNumber(String) 14 failed", new Double("1.1E-200"), NumberUtils.createNumber("1.1E-200"));
        assertEquals("createNumber(null) failed", null, NumberUtils.createNumber(null));
        
        // jdk 1.2 doesn't support this. unsure about jdk 1.2.2
        if(SystemUtils.isJavaVersionAtLeast(1.3f)) { 
            assertEquals("createNumber(String) 15 failed", new BigDecimal("1.1E-700"), NumberUtils.createNumber("1.1E-700F"));
        }
        assertEquals(
            "createNumber(String) 16 failed",
            new Long("10" + Integer.MAX_VALUE),
            NumberUtils.createNumber("10" + Integer.MAX_VALUE + "L"));
        assertEquals(
            "createNumber(String) 17 failed",
            new Long("10" + Integer.MAX_VALUE),
            NumberUtils.createNumber("10" + Integer.MAX_VALUE));
        assertEquals(
            "createNumber(String) 18 failed",
            new BigInteger("10" + Long.MAX_VALUE),
            NumberUtils.createNumber("10" + Long.MAX_VALUE));

    }

    public void testCreateFloat() {
        assertEquals("createFloat(String) failed", new Float("1234.5"), NumberUtils.createFloat("1234.5"));
        assertEquals("createFloat(null) failed", null, NumberUtils.createFloat(null));
        this.testCreateFloatFailure("");
        this.testCreateFloatFailure(" ");
        this.testCreateFloatFailure("\b\t\n\f\r");
        // Funky whitespaces
        this.testCreateFloatFailure("\u00A0\uFEFF\u000B\u000C\u001C\u001D\u001E\u001F");
    }

    protected void testCreateFloatFailure(String str) {
        try {
            Float value = NumberUtils.createFloat(str);
            fail("createFloat(blank) failed: " + value);
        } catch (NumberFormatException ex) {
            // empty
        }
    }

    public void testCreateDouble() {
        assertEquals("createDouble(String) failed", new Double("1234.5"), NumberUtils.createDouble("1234.5"));
        assertEquals("createDouble(null) failed", null, NumberUtils.createDouble(null));
        this.testCreateDoubleFailure("");
        this.testCreateDoubleFailure(" ");
        this.testCreateDoubleFailure("\b\t\n\f\r");
        // Funky whitespaces
        this.testCreateDoubleFailure("\u00A0\uFEFF\u000B\u000C\u001C\u001D\u001E\u001F");
    }

    protected void testCreateDoubleFailure(String str) {
        try {
            Double value = NumberUtils.createDouble(str);
            fail("createDouble(blank) failed: " + value);
        } catch (NumberFormatException ex) {
            // empty
        }
    }

    public void testCreateInteger() {
        assertEquals("createInteger(String) failed", new Integer("12345"), NumberUtils.createInteger("12345"));
        assertEquals("createInteger(null) failed", null, NumberUtils.createInteger(null));
        this.testCreateIntegerFailure("");
        this.testCreateIntegerFailure(" ");
        this.testCreateIntegerFailure("\b\t\n\f\r");
        // Funky whitespaces
        this.testCreateIntegerFailure("\u00A0\uFEFF\u000B\u000C\u001C\u001D\u001E\u001F");
    }

    protected void testCreateIntegerFailure(String str) {
        try {
            Integer value = NumberUtils.createInteger(str);
            fail("createInteger(blank) failed: " + value);
        } catch (NumberFormatException ex) {
            // empty
        }
    }

    public void testCreateLong() {
        assertEquals("createLong(String) failed", new Long("12345"), NumberUtils.createLong("12345"));
        assertEquals("createLong(null) failed", null, NumberUtils.createLong(null));
        this.testCreateLongFailure("");
        this.testCreateLongFailure(" ");
        this.testCreateLongFailure("\b\t\n\f\r");
        // Funky whitespaces
        this.testCreateLongFailure("\u00A0\uFEFF\u000B\u000C\u001C\u001D\u001E\u001F");
    }

    protected void testCreateLongFailure(String str) {
        try {
            Long value = NumberUtils.createLong(str);
            fail("createLong(blank) failed: " + value);
        } catch (NumberFormatException ex) {
            // empty
        }
    }

    public void testCreateBigInteger() {
        assertEquals("createBigInteger(String) failed", new BigInteger("12345"), NumberUtils.createBigInteger("12345"));
        assertEquals("createBigInteger(null) failed", null, NumberUtils.createBigInteger(null));
        this.testCreateBigIntegerFailure("");
        this.testCreateBigIntegerFailure(" ");
        this.testCreateBigIntegerFailure("\b\t\n\f\r");
        // Funky whitespaces
        this.testCreateBigIntegerFailure("\u00A0\uFEFF\u000B\u000C\u001C\u001D\u001E\u001F");
    }

    protected void testCreateBigIntegerFailure(String str) {
        try {
            BigInteger value = NumberUtils.createBigInteger(str);
            fail("createBigInteger(blank) failed: " + value);
        } catch (NumberFormatException ex) {
            // empty
        }
    }

    public void testCreateBigDecimal() {
        assertEquals("createBigDecimal(String) failed", new BigDecimal("1234.5"), NumberUtils.createBigDecimal("1234.5"));
        assertEquals("createBigDecimal(null) failed", null, NumberUtils.createBigDecimal(null));
        this.testCreateBigDecimalFailure("");
        this.testCreateBigDecimalFailure(" ");
        this.testCreateBigDecimalFailure("\b\t\n\f\r");
        // Funky whitespaces
        this.testCreateBigDecimalFailure("\u00A0\uFEFF\u000B\u000C\u001C\u001D\u001E\u001F");
    }

    protected void testCreateBigDecimalFailure(String str) {
        try {
            BigDecimal value = NumberUtils.createBigDecimal(str);
            fail("createBigDecimal(blank) failed: " + value);
        } catch (NumberFormatException ex) {
            // empty
        }
    }

    // min/max tests
    // ----------------------------------------------------------------------
    public void testMinLong() {
        final long[] l = null;
        try {
            NumberUtils.min(l);
            fail("No exception was thrown for null input.");
        } catch (IllegalArgumentException ex) {}

        try {
            NumberUtils.min(new long[0]);
            fail("No exception was thrown for empty input.");
        } catch (IllegalArgumentException ex) {}

        assertEquals(
            "min(long[]) failed for array length 1",
            5,
            NumberUtils.min(new long[] { 5 }));

        assertEquals(
            "min(long[]) failed for array length 2",
            6,
            NumberUtils.min(new long[] { 6, 9 }));

        assertEquals(-10, NumberUtils.min(new long[] { -10, -5, 0, 5, 10 }));
        assertEquals(-10, NumberUtils.min(new long[] { -5, 0, -10, 5, 10 }));
    }

    public void testMinInt() {
        final int[] i = null;
        try {
            NumberUtils.min(i);
            fail("No exception was thrown for null input.");
        } catch (IllegalArgumentException ex) {}

        try {
            NumberUtils.min(new int[0]);
            fail("No exception was thrown for empty input.");
        } catch (IllegalArgumentException ex) {}

        assertEquals(
            "min(int[]) failed for array length 1",
            5,
            NumberUtils.min(new int[] { 5 }));

        assertEquals(
            "min(int[]) failed for array length 2",
            6,
            NumberUtils.min(new int[] { 6, 9 }));

        assertEquals(-10, NumberUtils.min(new int[] { -10, -5, 0, 5, 10 }));
        assertEquals(-10, NumberUtils.min(new int[] { -5, 0, -10, 5, 10 }));
    }

    public void testMinShort() {
        final short[] s = null;
        try {
            NumberUtils.min(s);
            fail("No exception was thrown for null input.");
        } catch (IllegalArgumentException ex) {}

        try {
            NumberUtils.min(new short[0]);
            fail("No exception was thrown for empty input.");
        } catch (IllegalArgumentException ex) {}

        assertEquals(
            "min(short[]) failed for array length 1",
            5,
            NumberUtils.min(new short[] { 5 }));

        assertEquals(
            "min(short[]) failed for array length 2",
            6,
            NumberUtils.min(new short[] { 6, 9 }));

        assertEquals(-10, NumberUtils.min(new short[] { -10, -5, 0, 5, 10 }));
        assertEquals(-10, NumberUtils.min(new short[] { -5, 0, -10, 5, 10 }));
    }

    public void testMinDouble() {
        final double[] d = null;
        try {
            NumberUtils.min(d);
            fail("No exception was thrown for null input.");
        } catch (IllegalArgumentException ex) {}

        try {
            NumberUtils.min(new double[0]);
            fail("No exception was thrown for empty input.");
        } catch (IllegalArgumentException ex) {}

        assertEquals(
            "min(double[]) failed for array length 1",
            5.12,
            NumberUtils.min(new double[] { 5.12 }),
            0);

        assertEquals(
            "min(double[]) failed for array length 2",
            6.23,
            NumberUtils.min(new double[] { 6.23, 9.34 }),
            0);

        assertEquals(
            "min(double[]) failed for array length 5",
            -10.45,
            NumberUtils.min(new double[] { -10.45, -5.56, 0, 5.67, 10.78 }),
            0);
        assertEquals(-10, NumberUtils.min(new double[] { -10, -5, 0, 5, 10 }), 0.0001);
        assertEquals(-10, NumberUtils.min(new double[] { -5, 0, -10, 5, 10 }), 0.0001);
    }

    public void testMinFloat() {
        final float[] f = null;
        try {
            NumberUtils.min(f);
            fail("No exception was thrown for null input.");
        } catch (IllegalArgumentException ex) {}

        try {
            NumberUtils.min(new float[0]);
            fail("No exception was thrown for empty input.");
        } catch (IllegalArgumentException ex) {}

        assertEquals(
            "min(float[]) failed for array length 1",
            5.9f,
            NumberUtils.min(new float[] { 5.9f }),
            0);

        assertEquals(
            "min(float[]) failed for array length 2",
            6.8f,
            NumberUtils.min(new float[] { 6.8f, 9.7f }),
            0);

        assertEquals(
            "min(float[]) failed for array length 5",
            -10.6f,
            NumberUtils.min(new float[] { -10.6f, -5.5f, 0, 5.4f, 10.3f }),
            0);
        assertEquals(-10, NumberUtils.min(new float[] { -10, -5, 0, 5, 10 }), 0.0001f);
        assertEquals(-10, NumberUtils.min(new float[] { -5, 0, -10, 5, 10 }), 0.0001f);
    }

    public void testMaxLong() {
        final long[] l = null;
        try {
            NumberUtils.max(l);
            fail("No exception was thrown for null input.");
        } catch (IllegalArgumentException ex) {}

        try {
            NumberUtils.max(new long[0]);
            fail("No exception was thrown for empty input.");
        } catch (IllegalArgumentException ex) {}

        assertEquals(
            "max(long[]) failed for array length 1",
            5,
            NumberUtils.max(new long[] { 5 }));

        assertEquals(
            "max(long[]) failed for array length 2",
            9,
            NumberUtils.max(new long[] { 6, 9 }));

        assertEquals(
            "max(long[]) failed for array length 5",
            10,
            NumberUtils.max(new long[] { -10, -5, 0, 5, 10 }));
        assertEquals(10, NumberUtils.max(new long[] { -10, -5, 0, 5, 10 }));
        assertEquals(10, NumberUtils.max(new long[] { -5, 0, 10, 5, -10 }));
    }

    public void testMaxInt() {
        final int[] i = null;
        try {
            NumberUtils.max(i);
            fail("No exception was thrown for null input.");
        } catch (IllegalArgumentException ex) {}

        try {
            NumberUtils.max(new int[0]);
            fail("No exception was thrown for empty input.");
        } catch (IllegalArgumentException ex) {}

        assertEquals(
            "max(int[]) failed for array length 1",
            5,
            NumberUtils.max(new int[] { 5 }));

        assertEquals(
            "max(int[]) failed for array length 2",
            9,
            NumberUtils.max(new int[] { 6, 9 }));

        assertEquals(
            "max(int[]) failed for array length 5",
            10,
            NumberUtils.max(new int[] { -10, -5, 0, 5, 10 }));
        assertEquals(10, NumberUtils.max(new int[] { -10, -5, 0, 5, 10 }));
        assertEquals(10, NumberUtils.max(new int[] { -5, 0, 10, 5, -10 }));
    }

    public void testMaxShort() {
        final short[] s = null;
        try {
            NumberUtils.max(s);
            fail("No exception was thrown for null input.");
        } catch (IllegalArgumentException ex) {}

        try {
            NumberUtils.max(new short[0]);
            fail("No exception was thrown for empty input.");
        } catch (IllegalArgumentException ex) {}

        assertEquals(
            "max(short[]) failed for array length 1",
            5,
            NumberUtils.max(new short[] { 5 }));

        assertEquals(
            "max(short[]) failed for array length 2",
            9,
            NumberUtils.max(new short[] { 6, 9 }));

        assertEquals(
            "max(short[]) failed for array length 5",
            10,
            NumberUtils.max(new short[] { -10, -5, 0, 5, 10 }));
        assertEquals(10, NumberUtils.max(new short[] { -10, -5, 0, 5, 10 }));
        assertEquals(10, NumberUtils.max(new short[] { -5, 0, 10, 5, -10 }));
    }

    public void testMaxDouble() {
        final double[] d = null;
        try {
            NumberUtils.max(d);
            fail("No exception was thrown for null input.");
        } catch (IllegalArgumentException ex) {}

        try {
            NumberUtils.max(new double[0]);
            fail("No exception was thrown for empty input.");
        } catch (IllegalArgumentException ex) {}

        assertEquals(
            "max(double[]) failed for array length 1",
            5.1f,
            NumberUtils.max(new double[] { 5.1f }),
            0);

        assertEquals(
            "max(double[]) failed for array length 2",
            9.2f,
            NumberUtils.max(new double[] { 6.3f, 9.2f }),
            0);

        assertEquals(
            "max(double[]) failed for float length 5",
            10.4f,
            NumberUtils.max(new double[] { -10.5f, -5.6f, 0, 5.7f, 10.4f }),
            0);
        assertEquals(10, NumberUtils.max(new double[] { -10, -5, 0, 5, 10 }), 0.0001);
        assertEquals(10, NumberUtils.max(new double[] { -5, 0, 10, 5, -10 }), 0.0001);
    }
 
    public void testMaxFloat() {
        final float[] f = null;
        try {
            NumberUtils.max(f);
            fail("No exception was thrown for null input.");
        } catch (IllegalArgumentException ex) {}

        try {
            NumberUtils.max(new float[0]);
            fail("No exception was thrown for empty input.");
        } catch (IllegalArgumentException ex) {}

        assertEquals(
            "max(float[]) failed for array length 1",
            5.1f,
            NumberUtils.max(new float[] { 5.1f }),
            0);

        assertEquals(
            "max(float[]) failed for array length 2",
            9.2f,
            NumberUtils.max(new float[] { 6.3f, 9.2f }),
            0);

        assertEquals(
            "max(float[]) failed for float length 5",
            10.4f,
            NumberUtils.max(new float[] { -10.5f, -5.6f, 0, 5.7f, 10.4f }),
            0);
        assertEquals(10, NumberUtils.max(new float[] { -10, -5, 0, 5, 10 }), 0.0001f);
        assertEquals(10, NumberUtils.max(new float[] { -5, 0, 10, 5, -10 }), 0.0001f);
    }

    public void testMinimumLong() {
        assertEquals("minimum(long,long,long) 1 failed", 12345L, NumberUtils.min(12345L, 12345L + 1L, 12345L + 2L));
        assertEquals("minimum(long,long,long) 2 failed", 12345L, NumberUtils.min(12345L + 1L, 12345L, 12345 + 2L));
        assertEquals("minimum(long,long,long) 3 failed", 12345L, NumberUtils.min(12345L + 1L, 12345L + 2L, 12345L));
        assertEquals("minimum(long,long,long) 4 failed", 12345L, NumberUtils.min(12345L + 1L, 12345L, 12345L));
        assertEquals("minimum(long,long,long) 5 failed", 12345L, NumberUtils.min(12345L, 12345L, 12345L));
    }

    public void testMinimumInt() {
        assertEquals("minimum(int,int,int) 1 failed", 12345, NumberUtils.min(12345, 12345 + 1, 12345 + 2));
        assertEquals("minimum(int,int,int) 2 failed", 12345, NumberUtils.min(12345 + 1, 12345, 12345 + 2));
        assertEquals("minimum(int,int,int) 3 failed", 12345, NumberUtils.min(12345 + 1, 12345 + 2, 12345));
        assertEquals("minimum(int,int,int) 4 failed", 12345, NumberUtils.min(12345 + 1, 12345, 12345));
        assertEquals("minimum(int,int,int) 5 failed", 12345, NumberUtils.min(12345, 12345, 12345));
    }

    public void testMinimumShort() {
        short low = 1234;
        short mid = 1234 + 1;
        short high = 1234 + 2;
        assertEquals("minimum(short,short,short) 1 failed", low, NumberUtils.min(low, mid, high));
        assertEquals("minimum(short,short,short) 1 failed", low, NumberUtils.min(mid, low, high));
        assertEquals("minimum(short,short,short) 1 failed", low, NumberUtils.min(mid, high, low));
        assertEquals("minimum(short,short,short) 1 failed", low, NumberUtils.min(low, mid, low));
    }

    public void testMinimumByte() {
        byte low = 123;
        byte mid = 123 + 1;
        byte high = 123 + 2;
        assertEquals("minimum(byte,byte,byte) 1 failed", low, NumberUtils.min(low, mid, high));
        assertEquals("minimum(byte,byte,byte) 1 failed", low, NumberUtils.min(mid, low, high));
        assertEquals("minimum(byte,byte,byte) 1 failed", low, NumberUtils.min(mid, high, low));
        assertEquals("minimum(byte,byte,byte) 1 failed", low, NumberUtils.min(low, mid, low));
    }

    public void testMinimumDouble() {
        double low = 12.3;
        double mid = 12.3 + 1;
        double high = 12.3 + 2;
        assertEquals(low, NumberUtils.min(low, mid, high), 0.0001);
        assertEquals(low, NumberUtils.min(mid, low, high), 0.0001);
        assertEquals(low, NumberUtils.min(mid, high, low), 0.0001);
        assertEquals(low, NumberUtils.min(low, mid, low), 0.0001);
        assertEquals(mid, NumberUtils.min(high, mid, high), 0.0001);
    }

    public void testMinimumFloat() {
        float low = 12.3f;
        float mid = 12.3f + 1;
        float high = 12.3f + 2;
        assertEquals(low, NumberUtils.min(low, mid, high), 0.0001f);
        assertEquals(low, NumberUtils.min(mid, low, high), 0.0001f);
        assertEquals(low, NumberUtils.min(mid, high, low), 0.0001f);
        assertEquals(low, NumberUtils.min(low, mid, low), 0.0001f);
        assertEquals(mid, NumberUtils.min(high, mid, high), 0.0001f);
    }

    public void testMaximumLong() {
        assertEquals("maximum(long,long,long) 1 failed", 12345L, NumberUtils.max(12345L, 12345L - 1L, 12345L - 2L));
        assertEquals("maximum(long,long,long) 2 failed", 12345L, NumberUtils.max(12345L - 1L, 12345L, 12345L - 2L));
        assertEquals("maximum(long,long,long) 3 failed", 12345L, NumberUtils.max(12345L - 1L, 12345L - 2L, 12345L));
        assertEquals("maximum(long,long,long) 4 failed", 12345L, NumberUtils.max(12345L - 1L, 12345L, 12345L));
        assertEquals("maximum(long,long,long) 5 failed", 12345L, NumberUtils.max(12345L, 12345L, 12345L));
    }

    public void testMaximumInt() {
        assertEquals("maximum(int,int,int) 1 failed", 12345, NumberUtils.max(12345, 12345 - 1, 12345 - 2));
        assertEquals("maximum(int,int,int) 2 failed", 12345, NumberUtils.max(12345 - 1, 12345, 12345 - 2));
        assertEquals("maximum(int,int,int) 3 failed", 12345, NumberUtils.max(12345 - 1, 12345 - 2, 12345));
        assertEquals("maximum(int,int,int) 4 failed", 12345, NumberUtils.max(12345 - 1, 12345, 12345));
        assertEquals("maximum(int,int,int) 5 failed", 12345, NumberUtils.max(12345, 12345, 12345));
    }

    public void testMaximumShort() {
        short low = 1234;
        short mid = 1234 + 1;
        short high = 1234 + 2;
        assertEquals("maximum(short,short,short) 1 failed", high, NumberUtils.max(low, mid, high));
        assertEquals("maximum(short,short,short) 1 failed", high, NumberUtils.max(mid, low, high));
        assertEquals("maximum(short,short,short) 1 failed", high, NumberUtils.max(mid, high, low));
        assertEquals("maximum(short,short,short) 1 failed", high, NumberUtils.max(high, mid, high));
    }

    public void testMaximumByte() {
        byte low = 123;
        byte mid = 123 + 1;
        byte high = 123 + 2;
        assertEquals("maximum(byte,byte,byte) 1 failed", high, NumberUtils.max(low, mid, high));
        assertEquals("maximum(byte,byte,byte) 1 failed", high, NumberUtils.max(mid, low, high));
        assertEquals("maximum(byte,byte,byte) 1 failed", high, NumberUtils.max(mid, high, low));
        assertEquals("maximum(byte,byte,byte) 1 failed", high, NumberUtils.max(high, mid, high));
    }

    public void testMaximumDouble() {
        double low = 12.3;
        double mid = 12.3 + 1;
        double high = 12.3 + 2;
        assertEquals(high, NumberUtils.max(low, mid, high), 0.0001);
        assertEquals(high, NumberUtils.max(mid, low, high), 0.0001);
        assertEquals(high, NumberUtils.max(mid, high, low), 0.0001);
        assertEquals(mid, NumberUtils.max(low, mid, low), 0.0001);
        assertEquals(high, NumberUtils.max(high, mid, high), 0.0001);
    }

    public void testMaximumFloat() {
        float low = 12.3f;
        float mid = 12.3f + 1;
        float high = 12.3f + 2;
        assertEquals(high, NumberUtils.max(low, mid, high), 0.0001f);
        assertEquals(high, NumberUtils.max(mid, low, high), 0.0001f);
        assertEquals(high, NumberUtils.max(mid, high, low), 0.0001f);
        assertEquals(mid, NumberUtils.max(low, mid, low), 0.0001f);
        assertEquals(high, NumberUtils.max(high, mid, high), 0.0001f);
    }

    public void testCompareDouble() {
        assertTrue(NumberUtils.compare(Double.NaN, Double.NaN) == 0);
        assertTrue(NumberUtils.compare(Double.NaN, Double.POSITIVE_INFINITY) == +1);
        assertTrue(NumberUtils.compare(Double.NaN, Double.MAX_VALUE) == +1);
        assertTrue(NumberUtils.compare(Double.NaN, 1.2d) == +1);
        assertTrue(NumberUtils.compare(Double.NaN, 0.0d) == +1);
        assertTrue(NumberUtils.compare(Double.NaN, -0.0d) == +1);
        assertTrue(NumberUtils.compare(Double.NaN, -1.2d) == +1);
        assertTrue(NumberUtils.compare(Double.NaN, -Double.MAX_VALUE) == +1);
        assertTrue(NumberUtils.compare(Double.NaN, Double.NEGATIVE_INFINITY) == +1);
        
        assertTrue(NumberUtils.compare(Double.POSITIVE_INFINITY, Double.NaN) == -1);
        assertTrue(NumberUtils.compare(Double.POSITIVE_INFINITY, Double.POSITIVE_INFINITY) == 0);
        assertTrue(NumberUtils.compare(Double.POSITIVE_INFINITY, Double.MAX_VALUE) == +1);
        assertTrue(NumberUtils.compare(Double.POSITIVE_INFINITY, 1.2d) == +1);
        assertTrue(NumberUtils.compare(Double.POSITIVE_INFINITY, 0.0d) == +1);
        assertTrue(NumberUtils.compare(Double.POSITIVE_INFINITY, -0.0d) == +1);
        assertTrue(NumberUtils.compare(Double.POSITIVE_INFINITY, -1.2d) == +1);
        assertTrue(NumberUtils.compare(Double.POSITIVE_INFINITY, -Double.MAX_VALUE) == +1);
        assertTrue(NumberUtils.compare(Double.POSITIVE_INFINITY, Double.NEGATIVE_INFINITY) == +1);
        
        assertTrue(NumberUtils.compare(Double.MAX_VALUE, Double.NaN) == -1);
        assertTrue(NumberUtils.compare(Double.MAX_VALUE, Double.POSITIVE_INFINITY) == -1);
        assertTrue(NumberUtils.compare(Double.MAX_VALUE, Double.MAX_VALUE) == 0);
        assertTrue(NumberUtils.compare(Double.MAX_VALUE, 1.2d) == +1);
        assertTrue(NumberUtils.compare(Double.MAX_VALUE, 0.0d) == +1);
        assertTrue(NumberUtils.compare(Double.MAX_VALUE, -0.0d) == +1);
        assertTrue(NumberUtils.compare(Double.MAX_VALUE, -1.2d) == +1);
        assertTrue(NumberUtils.compare(Double.MAX_VALUE, -Double.MAX_VALUE) == +1);
        assertTrue(NumberUtils.compare(Double.MAX_VALUE, Double.NEGATIVE_INFINITY) == +1);
        
        assertTrue(NumberUtils.compare(1.2d, Double.NaN) == -1);
        assertTrue(NumberUtils.compare(1.2d, Double.POSITIVE_INFINITY) == -1);
        assertTrue(NumberUtils.compare(1.2d, Double.MAX_VALUE) == -1);
        assertTrue(NumberUtils.compare(1.2d, 1.2d) == 0);
        assertTrue(NumberUtils.compare(1.2d, 0.0d) == +1);
        assertTrue(NumberUtils.compare(1.2d, -0.0d) == +1);
        assertTrue(NumberUtils.compare(1.2d, -1.2d) == +1);
        assertTrue(NumberUtils.compare(1.2d, -Double.MAX_VALUE) == +1);
        assertTrue(NumberUtils.compare(1.2d, Double.NEGATIVE_INFINITY) == +1);
        
        assertTrue(NumberUtils.compare(0.0d, Double.NaN) == -1);
        assertTrue(NumberUtils.compare(0.0d, Double.POSITIVE_INFINITY) == -1);
        assertTrue(NumberUtils.compare(0.0d, Double.MAX_VALUE) == -1);
        assertTrue(NumberUtils.compare(0.0d, 1.2d) == -1);
        assertTrue(NumberUtils.compare(0.0d, 0.0d) == 0);
        assertTrue(NumberUtils.compare(0.0d, -0.0d) == +1);
        assertTrue(NumberUtils.compare(0.0d, -1.2d) == +1);
        assertTrue(NumberUtils.compare(0.0d, -Double.MAX_VALUE) == +1);
        assertTrue(NumberUtils.compare(0.0d, Double.NEGATIVE_INFINITY) == +1);
        
        assertTrue(NumberUtils.compare(-0.0d, Double.NaN) == -1);
        assertTrue(NumberUtils.compare(-0.0d, Double.POSITIVE_INFINITY) == -1);
        assertTrue(NumberUtils.compare(-0.0d, Double.MAX_VALUE) == -1);
        assertTrue(NumberUtils.compare(-0.0d, 1.2d) == -1);
        assertTrue(NumberUtils.compare(-0.0d, 0.0d) == -1);
        assertTrue(NumberUtils.compare(-0.0d, -0.0d) == 0);
        assertTrue(NumberUtils.compare(-0.0d, -1.2d) == +1);
        assertTrue(NumberUtils.compare(-0.0d, -Double.MAX_VALUE) == +1);
        assertTrue(NumberUtils.compare(-0.0d, Double.NEGATIVE_INFINITY) == +1);
        
        assertTrue(NumberUtils.compare(-1.2d, Double.NaN) == -1);
        assertTrue(NumberUtils.compare(-1.2d, Double.POSITIVE_INFINITY) == -1);
        assertTrue(NumberUtils.compare(-1.2d, Double.MAX_VALUE) == -1);
        assertTrue(NumberUtils.compare(-1.2d, 1.2d) == -1);
        assertTrue(NumberUtils.compare(-1.2d, 0.0d) == -1);
        assertTrue(NumberUtils.compare(-1.2d, -0.0d) == -1);
        assertTrue(NumberUtils.compare(-1.2d, -1.2d) == 0);
        assertTrue(NumberUtils.compare(-1.2d, -Double.MAX_VALUE) == +1);
        assertTrue(NumberUtils.compare(-1.2d, Double.NEGATIVE_INFINITY) == +1);
        
        assertTrue(NumberUtils.compare(-Double.MAX_VALUE, Double.NaN) == -1);
        assertTrue(NumberUtils.compare(-Double.MAX_VALUE, Double.POSITIVE_INFINITY) == -1);
        assertTrue(NumberUtils.compare(-Double.MAX_VALUE, Double.MAX_VALUE) == -1);
        assertTrue(NumberUtils.compare(-Double.MAX_VALUE, 1.2d) == -1);
        assertTrue(NumberUtils.compare(-Double.MAX_VALUE, 0.0d) == -1);
        assertTrue(NumberUtils.compare(-Double.MAX_VALUE, -0.0d) == -1);
        assertTrue(NumberUtils.compare(-Double.MAX_VALUE, -1.2d) == -1);
        assertTrue(NumberUtils.compare(-Double.MAX_VALUE, -Double.MAX_VALUE) == 0);
        assertTrue(NumberUtils.compare(-Double.MAX_VALUE, Double.NEGATIVE_INFINITY) == +1);
        
        assertTrue(NumberUtils.compare(Double.NEGATIVE_INFINITY, Double.NaN) == -1);
        assertTrue(NumberUtils.compare(Double.NEGATIVE_INFINITY, Double.POSITIVE_INFINITY) == -1);
        assertTrue(NumberUtils.compare(Double.NEGATIVE_INFINITY, Double.MAX_VALUE) == -1);
        assertTrue(NumberUtils.compare(Double.NEGATIVE_INFINITY, 1.2d) == -1);
        assertTrue(NumberUtils.compare(Double.NEGATIVE_INFINITY, 0.0d) == -1);
        assertTrue(NumberUtils.compare(Double.NEGATIVE_INFINITY, -0.0d) == -1);
        assertTrue(NumberUtils.compare(Double.NEGATIVE_INFINITY, -1.2d) == -1);
        assertTrue(NumberUtils.compare(Double.NEGATIVE_INFINITY, -Double.MAX_VALUE) == -1);
        assertTrue(NumberUtils.compare(Double.NEGATIVE_INFINITY, Double.NEGATIVE_INFINITY) == 0);
    }

    public void testCompareFloat() {
        assertTrue(NumberUtils.compare(Float.NaN, Float.NaN) == 0);
        assertTrue(NumberUtils.compare(Float.NaN, Float.POSITIVE_INFINITY) == +1);
        assertTrue(NumberUtils.compare(Float.NaN, Float.MAX_VALUE) == +1);
        assertTrue(NumberUtils.compare(Float.NaN, 1.2f) == +1);
        assertTrue(NumberUtils.compare(Float.NaN, 0.0f) == +1);
        assertTrue(NumberUtils.compare(Float.NaN, -0.0f) == +1);
        assertTrue(NumberUtils.compare(Float.NaN, -1.2f) == +1);
        assertTrue(NumberUtils.compare(Float.NaN, -Float.MAX_VALUE) == +1);
        assertTrue(NumberUtils.compare(Float.NaN, Float.NEGATIVE_INFINITY) == +1);
        
        assertTrue(NumberUtils.compare(Float.POSITIVE_INFINITY, Float.NaN) == -1);
        assertTrue(NumberUtils.compare(Float.POSITIVE_INFINITY, Float.POSITIVE_INFINITY) == 0);
        assertTrue(NumberUtils.compare(Float.POSITIVE_INFINITY, Float.MAX_VALUE) == +1);
        assertTrue(NumberUtils.compare(Float.POSITIVE_INFINITY, 1.2f) == +1);
        assertTrue(NumberUtils.compare(Float.POSITIVE_INFINITY, 0.0f) == +1);
        assertTrue(NumberUtils.compare(Float.POSITIVE_INFINITY, -0.0f) == +1);
        assertTrue(NumberUtils.compare(Float.POSITIVE_INFINITY, -1.2f) == +1);
        assertTrue(NumberUtils.compare(Float.POSITIVE_INFINITY, -Float.MAX_VALUE) == +1);
        assertTrue(NumberUtils.compare(Float.POSITIVE_INFINITY, Float.NEGATIVE_INFINITY) == +1);
        
        assertTrue(NumberUtils.compare(Float.MAX_VALUE, Float.NaN) == -1);
        assertTrue(NumberUtils.compare(Float.MAX_VALUE, Float.POSITIVE_INFINITY) == -1);
        assertTrue(NumberUtils.compare(Float.MAX_VALUE, Float.MAX_VALUE) == 0);
        assertTrue(NumberUtils.compare(Float.MAX_VALUE, 1.2f) == +1);
        assertTrue(NumberUtils.compare(Float.MAX_VALUE, 0.0f) == +1);
        assertTrue(NumberUtils.compare(Float.MAX_VALUE, -0.0f) == +1);
        assertTrue(NumberUtils.compare(Float.MAX_VALUE, -1.2f) == +1);
        assertTrue(NumberUtils.compare(Float.MAX_VALUE, -Float.MAX_VALUE) == +1);
        assertTrue(NumberUtils.compare(Float.MAX_VALUE, Float.NEGATIVE_INFINITY) == +1);
        
        assertTrue(NumberUtils.compare(1.2f, Float.NaN) == -1);
        assertTrue(NumberUtils.compare(1.2f, Float.POSITIVE_INFINITY) == -1);
        assertTrue(NumberUtils.compare(1.2f, Float.MAX_VALUE) == -1);
        assertTrue(NumberUtils.compare(1.2f, 1.2f) == 0);
        assertTrue(NumberUtils.compare(1.2f, 0.0f) == +1);
        assertTrue(NumberUtils.compare(1.2f, -0.0f) == +1);
        assertTrue(NumberUtils.compare(1.2f, -1.2f) == +1);
        assertTrue(NumberUtils.compare(1.2f, -Float.MAX_VALUE) == +1);
        assertTrue(NumberUtils.compare(1.2f, Float.NEGATIVE_INFINITY) == +1);
        
        assertTrue(NumberUtils.compare(0.0f, Float.NaN) == -1);
        assertTrue(NumberUtils.compare(0.0f, Float.POSITIVE_INFINITY) == -1);
        assertTrue(NumberUtils.compare(0.0f, Float.MAX_VALUE) == -1);
        assertTrue(NumberUtils.compare(0.0f, 1.2f) == -1);
        assertTrue(NumberUtils.compare(0.0f, 0.0f) == 0);
        assertTrue(NumberUtils.compare(0.0f, -0.0f) == +1);
        assertTrue(NumberUtils.compare(0.0f, -1.2f) == +1);
        assertTrue(NumberUtils.compare(0.0f, -Float.MAX_VALUE) == +1);
        assertTrue(NumberUtils.compare(0.0f, Float.NEGATIVE_INFINITY) == +1);
        
        assertTrue(NumberUtils.compare(-0.0f, Float.NaN) == -1);
        assertTrue(NumberUtils.compare(-0.0f, Float.POSITIVE_INFINITY) == -1);
        assertTrue(NumberUtils.compare(-0.0f, Float.MAX_VALUE) == -1);
        assertTrue(NumberUtils.compare(-0.0f, 1.2f) == -1);
        assertTrue(NumberUtils.compare(-0.0f, 0.0f) == -1);
        assertTrue(NumberUtils.compare(-0.0f, -0.0f) == 0);
        assertTrue(NumberUtils.compare(-0.0f, -1.2f) == +1);
        assertTrue(NumberUtils.compare(-0.0f, -Float.MAX_VALUE) == +1);
        assertTrue(NumberUtils.compare(-0.0f, Float.NEGATIVE_INFINITY) == +1);
        
        assertTrue(NumberUtils.compare(-1.2f, Float.NaN) == -1);
        assertTrue(NumberUtils.compare(-1.2f, Float.POSITIVE_INFINITY) == -1);
        assertTrue(NumberUtils.compare(-1.2f, Float.MAX_VALUE) == -1);
        assertTrue(NumberUtils.compare(-1.2f, 1.2f) == -1);
        assertTrue(NumberUtils.compare(-1.2f, 0.0f) == -1);
        assertTrue(NumberUtils.compare(-1.2f, -0.0f) == -1);
        assertTrue(NumberUtils.compare(-1.2f, -1.2f) == 0);
        assertTrue(NumberUtils.compare(-1.2f, -Float.MAX_VALUE) == +1);
        assertTrue(NumberUtils.compare(-1.2f, Float.NEGATIVE_INFINITY) == +1);
        
        assertTrue(NumberUtils.compare(-Float.MAX_VALUE, Float.NaN) == -1);
        assertTrue(NumberUtils.compare(-Float.MAX_VALUE, Float.POSITIVE_INFINITY) == -1);
        assertTrue(NumberUtils.compare(-Float.MAX_VALUE, Float.MAX_VALUE) == -1);
        assertTrue(NumberUtils.compare(-Float.MAX_VALUE, 1.2f) == -1);
        assertTrue(NumberUtils.compare(-Float.MAX_VALUE, 0.0f) == -1);
        assertTrue(NumberUtils.compare(-Float.MAX_VALUE, -0.0f) == -1);
        assertTrue(NumberUtils.compare(-Float.MAX_VALUE, -1.2f) == -1);
        assertTrue(NumberUtils.compare(-Float.MAX_VALUE, -Float.MAX_VALUE) == 0);
        assertTrue(NumberUtils.compare(-Float.MAX_VALUE, Float.NEGATIVE_INFINITY) == +1);
        
        assertTrue(NumberUtils.compare(Float.NEGATIVE_INFINITY, Float.NaN) == -1);
        assertTrue(NumberUtils.compare(Float.NEGATIVE_INFINITY, Float.POSITIVE_INFINITY) == -1);
        assertTrue(NumberUtils.compare(Float.NEGATIVE_INFINITY, Float.MAX_VALUE) == -1);
        assertTrue(NumberUtils.compare(Float.NEGATIVE_INFINITY, 1.2f) == -1);
        assertTrue(NumberUtils.compare(Float.NEGATIVE_INFINITY, 0.0f) == -1);
        assertTrue(NumberUtils.compare(Float.NEGATIVE_INFINITY, -0.0f) == -1);
        assertTrue(NumberUtils.compare(Float.NEGATIVE_INFINITY, -1.2f) == -1);
        assertTrue(NumberUtils.compare(Float.NEGATIVE_INFINITY, -Float.MAX_VALUE) == -1);
        assertTrue(NumberUtils.compare(Float.NEGATIVE_INFINITY, Float.NEGATIVE_INFINITY) == 0);
    }

    public void testIsDigits() {
        assertEquals("isDigits(null) failed", false, NumberUtils.isDigits(null));
        assertEquals("isDigits('') failed", false, NumberUtils.isDigits(""));
        assertEquals("isDigits(String) failed", true, NumberUtils.isDigits("12345"));
        assertEquals("isDigits(String) neg 1 failed", false, NumberUtils.isDigits("1234.5"));
        assertEquals("isDigits(String) neg 3 failed", false, NumberUtils.isDigits("1ab"));
        assertEquals("isDigits(String) neg 4 failed", false, NumberUtils.isDigits("abc"));
    }
    
    /**
     * Tests isNumber(String) and tests that createNumber(String) returns
     * a valid number iff isNumber(String) returns false.
     */
    public void testIsNumber() {
        String val = "12345";
        assertTrue("isNumber(String) 1 failed", NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 1 failed", checkCreateNumber(val));
        val = "1234.5";
        assertTrue("isNumber(String) 2 failed", NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 2 failed", checkCreateNumber(val));
        val = ".12345";
        assertTrue("isNumber(String) 3 failed", NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 3 failed", checkCreateNumber(val));
        val = "1234E5";
        assertTrue("isNumber(String) 4 failed", NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 4 failed", checkCreateNumber(val));
        val = "1234E+5";
        assertTrue("isNumber(String) 5 failed", NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 5 failed", checkCreateNumber(val));
        val = "1234E-5";
        assertTrue("isNumber(String) 6 failed", NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 6 failed", checkCreateNumber(val));
        val = "123.4E5";
        assertTrue("isNumber(String) 7 failed", NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 7 failed", checkCreateNumber(val));
        val = "-1234";
        assertTrue("isNumber(String) 8 failed", NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 8 failed", checkCreateNumber(val));
        val = "-1234.5";
        assertTrue("isNumber(String) 9 failed", NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 9 failed", checkCreateNumber(val));
        val = "-.12345";
        assertTrue("isNumber(String) 10 failed", NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 10 failed", checkCreateNumber(val));
        val = "-1234E5";
        assertTrue("isNumber(String) 11 failed", NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 11 failed", checkCreateNumber(val));
        val = "0";
        assertTrue("isNumber(String) 12 failed", NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 12 failed", checkCreateNumber(val));
        val = "-0";
        assertTrue("isNumber(String) 13 failed", NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 13 failed", checkCreateNumber(val));
        val = "01234";
        assertTrue("isNumber(String) 14 failed", NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 14 failed", checkCreateNumber(val));
        val = "-01234";
        assertTrue("isNumber(String) 15 failed", NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 15 failed", checkCreateNumber(val));
        val = "0xABC123";
        assertTrue("isNumber(String) 16 failed", NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 16 failed", checkCreateNumber(val));
        val = "0x0";
        assertTrue("isNumber(String) 17 failed", NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 17 failed", checkCreateNumber(val));
        val = "123.4E21D";
        assertTrue("isNumber(String) 19 failed", NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 19 failed", checkCreateNumber(val));
        val = "-221.23F";
        assertTrue("isNumber(String) 20 failed", NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 20 failed", checkCreateNumber(val));
        val = "22338L";
        assertTrue("isNumber(String) 21 failed", NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 21 failed", checkCreateNumber(val));
        val = null;
        assertTrue("isNumber(String) 1 Neg failed", !NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 1 Neg failed", !checkCreateNumber(val));
        val = "";
        assertTrue("isNumber(String) 2 Neg failed", !NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 2 Neg failed", !checkCreateNumber(val));
        val = "--2.3";
        assertTrue("isNumber(String) 3 Neg failed", !NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 3 Neg failed", !checkCreateNumber(val));
        val = ".12.3";
        assertTrue("isNumber(String) 4 Neg failed", !NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 4 Neg failed", !checkCreateNumber(val));
        val = "-123E";
        assertTrue("isNumber(String) 5 Neg failed", !NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 5 Neg failed", !checkCreateNumber(val));
        val = "-123E+-212";
        assertTrue("isNumber(String) 6 Neg failed", !NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 6 Neg failed", !checkCreateNumber(val));
        val = "-123E2.12";
        assertTrue("isNumber(String) 7 Neg failed", !NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 7 Neg failed", !checkCreateNumber(val));
        val = "0xGF";
        assertTrue("isNumber(String) 8 Neg failed", !NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 8 Neg failed", !checkCreateNumber(val));
        val = "0xFAE-1";
        assertTrue("isNumber(String) 9 Neg failed", !NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 9 Neg failed", !checkCreateNumber(val));
        val = ".";
        assertTrue("isNumber(String) 10 Neg failed", !NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 10 Neg failed", !checkCreateNumber(val));
        val = "-0ABC123";
        assertTrue("isNumber(String) 11 Neg failed", !NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 11 Neg failed", !checkCreateNumber(val));
        val = "123.4E-D";
        assertTrue("isNumber(String) 12 Neg failed", !NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 12 Neg failed", !checkCreateNumber(val));
        val = "123.4ED";
        assertTrue("isNumber(String) 13 Neg failed", !NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 13 Neg failed", !checkCreateNumber(val));
        val = "1234E5l";
        assertTrue("isNumber(String) 14 Neg failed", !NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 14 Neg failed", !checkCreateNumber(val));
        val = "11a";
        assertTrue("isNumber(String) 15 Neg failed", !NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 15 Neg failed", !checkCreateNumber(val)); 
        val = "1a";
        assertTrue("isNumber(String) 16 Neg failed", !NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 16 Neg failed", !checkCreateNumber(val)); 
        val = "a";
        assertTrue("isNumber(String) 17 Neg failed", !NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 17 Neg failed", !checkCreateNumber(val)); 
        val = "11g";
        assertTrue("isNumber(String) 18 Neg failed", !NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 18 Neg failed", !checkCreateNumber(val)); 
        val = "11z";
        assertTrue("isNumber(String) 19 Neg failed", !NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 19 Neg failed", !checkCreateNumber(val)); 
        val = "11def";
        assertTrue("isNumber(String) 20 Neg failed", !NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 20 Neg failed", !checkCreateNumber(val)); 
        val = "11d11";
        assertTrue("isNumber(String) 21 Neg failed", !NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 21 Neg failed", !checkCreateNumber(val)); 
        val = "11 11";
        assertTrue("isNumber(String) 22 Neg failed", !NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 22 Neg failed", !checkCreateNumber(val));
        val = " 1111";
        assertTrue("isNumber(String) 23 Neg failed", !NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 23 Neg failed", !checkCreateNumber(val));
        val = "1111 ";
        assertTrue("isNumber(String) 24 Neg failed", !NumberUtils.isNumber(val));
        assertTrue("isNumber(String)/createNumber(String) 24 Neg failed", !checkCreateNumber(val));

    }

    private boolean checkCreateNumber(String val) {
        try {
            Object obj = NumberUtils.createNumber(val);
            if (obj == null) {
                return false;
            }
            return true;
        } catch (NumberFormatException e) {
            return false;
       }
    }

    public void testConstants() {
        assertTrue(NumberUtils.LONG_ZERO instanceof Long);
        assertTrue(NumberUtils.LONG_ONE instanceof Long);
        assertTrue(NumberUtils.LONG_MINUS_ONE instanceof Long);
        assertTrue(NumberUtils.INTEGER_ZERO instanceof Integer);
        assertTrue(NumberUtils.INTEGER_ONE instanceof Integer);
        assertTrue(NumberUtils.INTEGER_MINUS_ONE instanceof Integer);
        assertTrue(NumberUtils.SHORT_ZERO instanceof Short);
        assertTrue(NumberUtils.SHORT_ONE instanceof Short);
        assertTrue(NumberUtils.SHORT_MINUS_ONE instanceof Short);
        assertTrue(NumberUtils.BYTE_ZERO instanceof Byte);
        assertTrue(NumberUtils.BYTE_ONE instanceof Byte);
        assertTrue(NumberUtils.BYTE_MINUS_ONE instanceof Byte);
        assertTrue(NumberUtils.DOUBLE_ZERO instanceof Double);
        assertTrue(NumberUtils.DOUBLE_ONE instanceof Double);
        assertTrue(NumberUtils.DOUBLE_MINUS_ONE instanceof Double);
        assertTrue(NumberUtils.FLOAT_ZERO instanceof Float);
        assertTrue(NumberUtils.FLOAT_ONE instanceof Float);
        assertTrue(NumberUtils.FLOAT_MINUS_ONE instanceof Float);
        
        assertTrue(NumberUtils.LONG_ZERO.longValue() == 0);
        assertTrue(NumberUtils.LONG_ONE.longValue() == 1);
        assertTrue(NumberUtils.LONG_MINUS_ONE.longValue() == -1);
        assertTrue(NumberUtils.INTEGER_ZERO.intValue() == 0);
        assertTrue(NumberUtils.INTEGER_ONE.intValue() == 1);
        assertTrue(NumberUtils.INTEGER_MINUS_ONE.intValue() == -1);
        assertTrue(NumberUtils.SHORT_ZERO.shortValue() == 0);
        assertTrue(NumberUtils.SHORT_ONE.shortValue() == 1);
        assertTrue(NumberUtils.SHORT_MINUS_ONE.shortValue() == -1);
        assertTrue(NumberUtils.BYTE_ZERO.byteValue() == 0);
        assertTrue(NumberUtils.BYTE_ONE.byteValue() == 1);
        assertTrue(NumberUtils.BYTE_MINUS_ONE.byteValue() == -1);
        assertTrue(NumberUtils.DOUBLE_ZERO.doubleValue() == 0.0d);
        assertTrue(NumberUtils.DOUBLE_ONE.doubleValue() == 1.0d);
        assertTrue(NumberUtils.DOUBLE_MINUS_ONE.doubleValue() == -1.0d);
        assertTrue(NumberUtils.FLOAT_ZERO.floatValue() == 0.0f);
        assertTrue(NumberUtils.FLOAT_ONE.floatValue() == 1.0f);
        assertTrue(NumberUtils.FLOAT_MINUS_ONE.floatValue() == -1.0f);
    }
    
}
