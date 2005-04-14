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

import java.math.BigDecimal;
import java.math.BigInteger;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * Unit tests {@link org.apache.commons.lang.NumberUtils}.
 *
 * @author <a href="mailto:rand_mcneely@yahoo.com">Rand McNeely</a>
 * @author <a href="mailto:ridesmet@users.sourceforge.net">Ringo De Smet</a>
 * @author Eric Pugh
 * @author Phil Steitz
 * @author Stephen Colebourne
 * @version $Id$
 */
public class NumberUtilsTest extends TestCase {

    public NumberUtilsTest(String name) {
        super(name);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(NumberUtilsTest.class);
        suite.setName("NumberUtils Tests");
        return suite;
    }

    //---------------------------------------------------------------------

    /**
     * Test for int stringToInt(String)
     */
    public void testStringToIntString() {
        assertTrue("stringToInt(String) 1 failed", NumberUtils.stringToInt("12345") == 12345);
        assertTrue("stringToInt(String) 2 failed", NumberUtils.stringToInt("abc") == 0);
    }

    /**
     * Test for int stringToInt(String, int)
     */
    public void testStringToIntStringI() {
        assertTrue("stringToInt(String,int) 1 failed", NumberUtils.stringToInt("12345", 5) == 12345);
        assertTrue("stringToInt(String,int) 2 failed", NumberUtils.stringToInt("1234.5", 5) == 5);
    }

    public void testCreateNumber() {
        //a lot of things can go wrong
        assertEquals("createNumber(String) 1 failed", new Float("1234.5"), NumberUtils.createNumber("1234.5"));
        assertEquals("createNumber(String) 2 failed", new Integer("12345"), NumberUtils.createNumber("12345"));
        assertEquals("createNumber(String) 3 failed", new Double("1234.5"), NumberUtils.createNumber("1234.5D"));
        assertEquals("createNumber(String) 4 failed", new Float("1234.5"), NumberUtils.createNumber("1234.5F"));
        assertEquals("createNumber(String) 5 failed", new Long(Integer.MAX_VALUE + 1L), NumberUtils.createNumber("" + (Integer.MAX_VALUE + 1L)));
        assertEquals("createNumber(String) 6 failed", new BigInteger(Long.MAX_VALUE + "0"), NumberUtils.createNumber(Long.MAX_VALUE + "0L"));
        assertEquals("createNumber(String) 7 failed", new Long(12345), NumberUtils.createNumber("12345L"));
        assertEquals("createNumber(String) 8 failed", new Float("-1234.5"), NumberUtils.createNumber("-1234.5"));
        assertEquals("createNumber(String) 9 failed", new Integer("-12345"), NumberUtils.createNumber("-12345"));
        assertTrue("createNumber(String) 10 failed", 0xFADE == NumberUtils.createNumber("0xFADE").intValue());
        assertTrue("createNumber(String) 11 failed", -0xFADE == NumberUtils.createNumber("-0xFADE").intValue());
        assertEquals("createNumber(String) 12 failed", new Double("1.1E200"), NumberUtils.createNumber("1.1E200"));
        assertEquals("createNumber(String) 13 failed", new Float("1.1E20"), NumberUtils.createNumber("1.1E20"));
        assertEquals("createNumber(String) 14 failed", new Double("-1.1E200"), NumberUtils.createNumber("-1.1E200"));
        assertEquals("createNumber(String) 15 failed", new Double("1.1E-200"), NumberUtils.createNumber("1.1E-200"));
        assertEquals("createNumber(String) 16 failed", new Double("1.1E-200"), NumberUtils.createNumber("1.1E-200"));

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
    }

    public void testCreateDouble() {
        assertEquals("createDouble(String) failed", new Double("1234.5"), NumberUtils.createDouble("1234.5"));
    }

    public void testCreateInteger() {
        assertEquals("createInteger(String) failed", new Integer("12345"), NumberUtils.createInteger("12345"));
    }

    public void testCreateLong() {
        assertEquals("createInteger(String) failed", new Long("12345"), NumberUtils.createLong("12345"));
    }

    public void testCreateBigInteger() {
        assertEquals("createBigInteger(String) failed", new BigInteger("12345"), NumberUtils.createBigInteger("12345"));
    }

    public void testCreateBigDecimal() {
        assertEquals("createBigDecimal(String) failed", new BigDecimal("1234.5"), NumberUtils.createBigDecimal("1234.5"));
    }

    public void testMinimumLong() {
        assertEquals("minimum(long,long,long) 1 failed", 12345L, NumberUtils.minimum(12345L, 12345L + 1L, 12345L + 2L));
        assertEquals("minimum(long,long,long) 2 failed", 12345L, NumberUtils.minimum(12345L + 1L, 12345L, 12345 + 2L));
        assertEquals("minimum(long,long,long) 3 failed", 12345L, NumberUtils.minimum(12345L + 1L, 12345L + 2L, 12345L));
        assertEquals("minimum(long,long,long) 4 failed", 12345L, NumberUtils.minimum(12345L + 1L, 12345L, 12345L));
        assertEquals("minimum(long,long,long) 5 failed", 12345L, NumberUtils.minimum(12345L, 12345L, 12345L));

    }

    public void testMinimumInt() {
        assertEquals("minimum(int,int,int) 1 failed", 12345, NumberUtils.minimum(12345, 12345 + 1, 12345 + 2));
        assertEquals("minimum(int,int,int) 2 failed", 12345, NumberUtils.minimum(12345 + 1, 12345, 12345 + 2));
        assertEquals("minimum(int,int,int) 3 failed", 12345, NumberUtils.minimum(12345 + 1, 12345 + 2, 12345));
        assertEquals("minimum(int,int,int) 4 failed", 12345, NumberUtils.minimum(12345 + 1, 12345, 12345));
        assertEquals("minimum(int,int,int) 5 failed", 12345, NumberUtils.minimum(12345, 12345, 12345));

    }

    public void testMaximumLong() {
        assertEquals("maximum(long,long,long) 1 failed", 12345L, NumberUtils.maximum(12345L, 12345L - 1L, 12345L - 2L));
        assertEquals("maximum(long,long,long) 2 failed", 12345L, NumberUtils.maximum(12345L - 1L, 12345L, 12345L - 2L));
        assertEquals("maximum(long,long,long) 3 failed", 12345L, NumberUtils.maximum(12345L - 1L, 12345L - 2L, 12345L));
        assertEquals("maximum(long,long,long) 4 failed", 12345L, NumberUtils.maximum(12345L - 1L, 12345L, 12345L));
        assertEquals("maximum(long,long,long) 5 failed", 12345L, NumberUtils.maximum(12345L, 12345L, 12345L));

    }

    public void testMaximumInt() {
        assertEquals("maximum(int,int,int) 1 failed", 12345, NumberUtils.maximum(12345, 12345 - 1, 12345 - 2));
        assertEquals("maximum(int,int,int) 2 failed", 12345, NumberUtils.maximum(12345 - 1, 12345, 12345 - 2));
        assertEquals("maximum(int,int,int) 3 failed", 12345, NumberUtils.maximum(12345 - 1, 12345 - 2, 12345));
        assertEquals("maximum(int,int,int) 4 failed", 12345, NumberUtils.maximum(12345 - 1, 12345, 12345));
        assertEquals("maximum(int,int,int) 5 failed", 12345, NumberUtils.maximum(12345, 12345, 12345));

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

    }
    
    public void testIsNumberInvalidInput() {
        String val = "0x";
        assertEquals("isNumber() with 0x wasn't false",  false, NumberUtils.isNumber(val));
        val = "0x3x3";
        assertEquals("isNumber() with 0x3x3 wasn't false",  false, NumberUtils.isNumber(val));
        val = "20EE-3";
        assertEquals("isNumber() with 20EE-3 wasn't false",  false, NumberUtils.isNumber(val));
        val = "2435q";
        assertEquals("isNumber() with 2435q wasn't false",  false, NumberUtils.isNumber(val));
        val = ".";
        assertEquals("isNumber() with . wasn't false",  false, NumberUtils.isNumber(val));

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
        } catch (NullPointerException e) {
            return false;
        }
    }

    public void testPublicNoArgConstructor() {
        try {
            NumberUtils nu = new NumberUtils();
        } catch( Exception e ) {
            fail( "Error calling public no-arg constructor" );
        }
    }
    
}
