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

package org.apache.commons.lang;

import java.util.Arrays;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Tests ArrayUtils add methods.
 * 
 * @author Gary D. Gregory
 * @version $Id: ArrayUtilsAddTest.java,v 1.1 2004/02/03 22:14:24 ggregory Exp $
 */
public class ArrayUtilsAddTest extends TestCase {
    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(ArrayUtilsAddTest.class);
        suite.setName("ArrayUtils add Tests");
        return suite;
    }

    public void testAddObjectArrayBoolean() {
        boolean[] newArray;
        newArray = ArrayUtils.add((boolean[])null, false);
        assertTrue(Arrays.equals(new boolean[]{false}, newArray));
        assertEquals(Boolean.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add((boolean[])null, true);
        assertTrue(Arrays.equals(new boolean[]{true}, newArray));
        assertEquals(Boolean.TYPE, newArray.getClass().getComponentType());
        boolean[] array1 = new boolean[]{true, false, true};
        newArray = ArrayUtils.add(array1, false);
        assertTrue(Arrays.equals(new boolean[]{true, false, true, false}, newArray));
        assertEquals(Boolean.TYPE, newArray.getClass().getComponentType());
    }
    
    public void testAddObjectArrayByte() {
        byte[] newArray;
        newArray = ArrayUtils.add((byte[])null, (byte)0);
        assertTrue(Arrays.equals(new byte[]{0}, newArray));
        assertEquals(Byte.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add((byte[])null, (byte)1);
        assertTrue(Arrays.equals(new byte[]{1}, newArray));
        assertEquals(Byte.TYPE, newArray.getClass().getComponentType());
        byte[] array1 = new byte[]{1, 2, 3};
        newArray = ArrayUtils.add(array1, (byte)0);
        assertTrue(Arrays.equals(new byte[]{1, 2, 3, 0}, newArray));
        assertEquals(Byte.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add(array1, (byte)4);
        assertTrue(Arrays.equals(new byte[]{1, 2, 3, 4}, newArray));
        assertEquals(Byte.TYPE, newArray.getClass().getComponentType());        
    }
    
    public void testAddObjectArrayChar() {
        char[] newArray;
        newArray = ArrayUtils.add((char[])null, (char)0);
        assertTrue(Arrays.equals(new char[]{0}, newArray));
        assertEquals(Character.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add((char[])null, (char)1);
        assertTrue(Arrays.equals(new char[]{1}, newArray));
        assertEquals(Character.TYPE, newArray.getClass().getComponentType());
        char[] array1 = new char[]{1, 2, 3};
        newArray = ArrayUtils.add(array1, (char)0);
        assertTrue(Arrays.equals(new char[]{1, 2, 3, 0}, newArray));
        assertEquals(Character.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add(array1, (char)4);
        assertTrue(Arrays.equals(new char[]{1, 2, 3, 4}, newArray));
        assertEquals(Character.TYPE, newArray.getClass().getComponentType());        
    }
    
    public void testAddObjectArrayDouble() {
        double[] newArray;
        newArray = ArrayUtils.add((double[])null, 0);
        assertTrue(Arrays.equals(new double[]{0}, newArray));
        assertEquals(Double.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add((double[])null, 1);
        assertTrue(Arrays.equals(new double[]{1}, newArray));
        assertEquals(Double.TYPE, newArray.getClass().getComponentType());
        double[] array1 = new double[]{1, 2, 3};
        newArray = ArrayUtils.add(array1, 0);
        assertTrue(Arrays.equals(new double[]{1, 2, 3, 0}, newArray));
        assertEquals(Double.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add(array1, 4);
        assertTrue(Arrays.equals(new double[]{1, 2, 3, 4}, newArray));
        assertEquals(Double.TYPE, newArray.getClass().getComponentType());        
    }
    
    public void testAddObjectArrayFloat() {
        float[] newArray;
        newArray = ArrayUtils.add((float[])null, 0);
        assertTrue(Arrays.equals(new float[]{0}, newArray));
        assertEquals(Float.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add((float[])null, 1);
        assertTrue(Arrays.equals(new float[]{1}, newArray));
        assertEquals(Float.TYPE, newArray.getClass().getComponentType());
        float[] array1 = new float[]{1, 2, 3};
        newArray = ArrayUtils.add(array1, 0);
        assertTrue(Arrays.equals(new float[]{1, 2, 3, 0}, newArray));
        assertEquals(Float.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add(array1, 4);
        assertTrue(Arrays.equals(new float[]{1, 2, 3, 4}, newArray));
        assertEquals(Float.TYPE, newArray.getClass().getComponentType());        
    }
    
    public void testAddObjectArrayInt() {
        int[] newArray;
        newArray = ArrayUtils.add((int[])null, 0);
        assertTrue(Arrays.equals(new int[]{0}, newArray));
        assertEquals(Integer.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add((int[])null, 1);
        assertTrue(Arrays.equals(new int[]{1}, newArray));
        assertEquals(Integer.TYPE, newArray.getClass().getComponentType());
        int[] array1 = new int[]{1, 2, 3};
        newArray = ArrayUtils.add(array1, 0);
        assertTrue(Arrays.equals(new int[]{1, 2, 3, 0}, newArray));
        assertEquals(Integer.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add(array1, 4);
        assertTrue(Arrays.equals(new int[]{1, 2, 3, 4}, newArray));
        assertEquals(Integer.TYPE, newArray.getClass().getComponentType());        
    }
    
    public void testAddObjectArrayLong() {
        long[] newArray;
        newArray = ArrayUtils.add((long[])null, 0);
        assertTrue(Arrays.equals(new long[]{0}, newArray));
        assertEquals(Long.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add((long[])null, 1);
        assertTrue(Arrays.equals(new long[]{1}, newArray));
        assertEquals(Long.TYPE, newArray.getClass().getComponentType());
        long[] array1 = new long[]{1, 2, 3};
        newArray = ArrayUtils.add(array1, 0);
        assertTrue(Arrays.equals(new long[]{1, 2, 3, 0}, newArray));
        assertEquals(Long.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add(array1, 4);
        assertTrue(Arrays.equals(new long[]{1, 2, 3, 4}, newArray));
        assertEquals(Long.TYPE, newArray.getClass().getComponentType());        
    }
    
    public void testAddObjectArrayShort() {
        short[] newArray;
        newArray = ArrayUtils.add((short[])null, (short)0);
        assertTrue(Arrays.equals(new short[]{0}, newArray));
        assertEquals(Short.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add((short[])null, (short)1);
        assertTrue(Arrays.equals(new short[]{1}, newArray));
        assertEquals(Short.TYPE, newArray.getClass().getComponentType());
        short[] array1 = new short[]{1, 2, 3};
        newArray = ArrayUtils.add(array1, (short)0);
        assertTrue(Arrays.equals(new short[]{1, 2, 3, 0}, newArray));
        assertEquals(Short.TYPE, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add(array1, (short)4);
        assertTrue(Arrays.equals(new short[]{1, 2, 3, 4}, newArray));
        assertEquals(Short.TYPE, newArray.getClass().getComponentType());        
    }
    
    public void testAddObjectArrayObject() {
        Object[] newArray;
        newArray = ArrayUtils.add((Object[])null, null);
        assertTrue(Arrays.equals((new Object[]{null}), newArray));
        assertEquals(Object.class, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add((Object[])null, "a");
        assertTrue(Arrays.equals((new String[]{"a"}), newArray));
        assertTrue(Arrays.equals((new Object[]{"a"}), newArray));
        assertEquals(String.class, newArray.getClass().getComponentType());
        String[] stringArray1 = new String[]{"a", "b", "c"};
        newArray = ArrayUtils.add(stringArray1, null);
        assertTrue(Arrays.equals((new String[]{"a", "b", "c", null}), newArray));
        assertEquals(String.class, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add(stringArray1, "d");
        assertTrue(Arrays.equals((new String[]{"a", "b", "c", "d"}), newArray));
        assertEquals(String.class, newArray.getClass().getComponentType());
    }
    
    public void testAddObjectArrayToObjectArray() {
        assertNull(ArrayUtils.addAll(null, null));
        Object[] newArray;
        String[] stringArray1 = new String[]{"a", "b", "c"};
        String[] stringArray2 = new String[]{"1", "2", "3"};
        newArray = ArrayUtils.addAll(stringArray1, null);
        assertTrue(Arrays.equals(stringArray1, newArray));
        assertTrue(Arrays.equals((new String[]{"a", "b", "c"}), newArray));
        assertEquals(String.class, newArray.getClass().getComponentType());
        newArray = ArrayUtils.addAll(null, stringArray2);
        assertTrue(Arrays.equals(stringArray2, newArray));
        assertTrue(Arrays.equals((new String[]{"1", "2", "3"}), newArray));
        assertEquals(String.class, newArray.getClass().getComponentType());
        newArray = ArrayUtils.addAll(stringArray1, stringArray2);
        assertTrue(Arrays.equals((new String[]{"a", "b", "c", "1", "2", "3"}), newArray));
        assertEquals(String.class, newArray.getClass().getComponentType());
        newArray = ArrayUtils.addAll(ArrayUtils.EMPTY_STRING_ARRAY, null);
        assertTrue(Arrays.equals(ArrayUtils.EMPTY_STRING_ARRAY, newArray));
        assertTrue(Arrays.equals((new String[]{}), newArray));
        assertEquals(String.class, newArray.getClass().getComponentType());
        newArray = ArrayUtils.addAll(null, ArrayUtils.EMPTY_STRING_ARRAY);
        assertTrue(Arrays.equals(ArrayUtils.EMPTY_STRING_ARRAY, newArray));
        assertTrue(Arrays.equals((new String[]{}), newArray));
        assertEquals(String.class, newArray.getClass().getComponentType());
        newArray = ArrayUtils.addAll(ArrayUtils.EMPTY_STRING_ARRAY, ArrayUtils.EMPTY_STRING_ARRAY);
        assertTrue(Arrays.equals(ArrayUtils.EMPTY_STRING_ARRAY, newArray));
        assertTrue(Arrays.equals((new String[]{}), newArray));
        assertEquals(String.class, newArray.getClass().getComponentType());
        String[] stringArrayNull = new String []{null};
        newArray = ArrayUtils.addAll(stringArrayNull, stringArrayNull);        
        assertTrue(Arrays.equals((new String[]{null, null}), newArray));
        assertEquals(String.class, newArray.getClass().getComponentType());
    }    
    
    public void testAddObjectAtIndex() {
        Object[] newArray;
        newArray = ArrayUtils.add((Object[])null, 0, null);
        assertTrue(Arrays.equals((new Object[]{null}), newArray));
        assertEquals(Object.class, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add((Object[])null, 0, "a");
        assertTrue(Arrays.equals((new String[]{"a"}), newArray));
        assertTrue(Arrays.equals((new Object[]{"a"}), newArray));
        assertEquals(String.class, newArray.getClass().getComponentType());
        String[] stringArray1 = new String[]{"a", "b", "c"};
        newArray = ArrayUtils.add(stringArray1, 0, null);
        assertTrue(Arrays.equals((new String[]{null, "a", "b", "c"}), newArray));
        assertEquals(String.class, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add(stringArray1, 1, null);
        assertTrue(Arrays.equals((new String[]{"a", null, "b", "c"}), newArray));
        assertEquals(String.class, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add(stringArray1, 3, null);
        assertTrue(Arrays.equals((new String[]{"a", "b", "c", null}), newArray));
        assertEquals(String.class, newArray.getClass().getComponentType());
        newArray = ArrayUtils.add(stringArray1, 3, "d");
        assertTrue(Arrays.equals((new String[]{"a", "b", "c", "d"}), newArray));
        assertEquals(String.class, newArray.getClass().getComponentType());        
        assertEquals(String.class, newArray.getClass().getComponentType());

        Object[] o = new Object[] {"1", "2", "4"};
        Object[] result = ArrayUtils.add(o, 2, "3");
        Object[] result2 = ArrayUtils.add(o, 3, "5");
        
        assertNotNull(result);
        assertEquals(4, result.length);
        assertEquals("1", result[0]);
        assertEquals("2", result[1]);
        assertEquals("3", result[2]);
        assertEquals("4", result[3]);
        assertNotNull(result2);
        assertEquals(4, result2.length);
        assertEquals("1", result2[0]);
        assertEquals("2", result2[1]);
        assertEquals("4", result2[2]);
        assertEquals("5", result2[3]);
    }
    
}
