package org.apache.commons.lang;

/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002 The Apache Software Foundation.  All rights
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
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
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

/**
 * <code>HashCode</code> generation routines.
 * <p>
 * This class enables a good hashcode to be built for any class. It follows
 * the rules laid out in the book Effective Java, by Joshua Bloch. Writing a 
 * good hashCode is actually quite difficult. This class aims to simplify the 
 * process.
 * <p>
 * All relevant fields from the object should be included in the hashCode. Derived
 * fields may be excluded. In general, any field used in the equals method must be
 * used in the hashCode method. 
 * <p>
 * To use this class write code as follows:
 * <code>
 * public class Person {
 *   String name;
 *   int age;
 *   boolean isSmoker;
 * 
 *   ...
 * 
 *   public int hashCode() {
 *     int total = 17;  // you pick a random, non-zero, odd number
 *     total = HashCodeUtils.buildHashCode(total, name);
 *     total = HashCodeUtils.buildHashCode(total, age);
 *     total = HashCodeUtils.buildHashCode(total, isSmoker);
 *     return total;
 *   }
 * }
 * </code>
 *
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @version $Id: HashCodeUtils.java,v 1.1 2002/08/10 12:12:49 scolebourne Exp $
 */
public class HashCodeUtils {
    
    // According to Bloch, its a random odd prime
    private static final int CONSTANT = 37;
    
    /**
     * Prevent construction of HashCodeUtils instances
     */
    private HashCodeUtils() {
    }

    /**
     * Build a hashCode for an Object.
     *
     * @param totalSoFar  the hashCode total so far
     * @param object  the object to add to the hashCode
     * @return updated totalSoFar
     */
    public static int buildHashCode(int totalSoFar, Object object) {
        if (object == null) {
            return totalSoFar * CONSTANT;
        } else {
            return totalSoFar * CONSTANT + object.hashCode();
        }
    }

    /**
     * Build a hashCode for a long.
     *
     * @param totalSoFar  the hashCode total so far
     * @param value  the long to add to the hashCode
     * @return updated totalSoFar
     */
    public static int buildHashCode(int totalSoFar, long value) {
        return totalSoFar * CONSTANT + ((int) (value ^ (value >> 32)));
    }

    /**
     * Build a hashCode for an int.
     *
     * @param totalSoFar  the hashCode total so far
     * @param value  the int to add to the hashCode
     * @return updated totalSoFar
     */
    public static int buildHashCode(int totalSoFar, int value) {
        return totalSoFar * CONSTANT + value;
    }

    /**
     * Build a hashCode for a short.
     *
     * @param totalSoFar  the hashCode total so far
     * @param value  the short to add to the hashCode
     * @return updated totalSoFar
     */
    public static int buildHashCode(int totalSoFar, short value) {
        return totalSoFar * CONSTANT + (int) value;
    }

    /**
     * Build a hashCode for a char.
     *
     * @param totalSoFar  the hashCode total so far
     * @param value  the char to add to the hashCode
     * @return updated totalSoFar
     */
    public static int buildHashCode(int totalSoFar, char value) {
        return totalSoFar * CONSTANT + (int) value;
    }

    /**
     * Build a hashCode for a byte.
     *
     * @param totalSoFar  the hashCode total so far
     * @param value  the byte to add to the hashCode
     * @return updated totalSoFar
     */
    public static int buildHashCode(int totalSoFar, byte value) {
        return totalSoFar * CONSTANT + (int) value;
    }

    /**
     * Build a hashCode for a double.
     *
     * @param totalSoFar  the hashCode total so far
     * @param value  the double to add to the hashCode
     * @return updated totalSoFar
     */
    public static int buildHashCode(int totalSoFar, double value) {
        return buildHashCode(totalSoFar, Double.doubleToLongBits(value));
    }

    /**
     * Build a hashCode for a float.
     *
     * @param totalSoFar  the hashCode total so far
     * @param value  the float to add to the hashCode
     * @return updated totalSoFar
     */
    public static int buildHashCode(int totalSoFar, float value) {
        return totalSoFar * CONSTANT + Float.floatToIntBits(value);
    }

    /**
     * Build a hashCode for a long.
     *
     * @param totalSoFar  the hashCode total so far
     * @param value  the long to add to the hashCode
     * @return updated totalSoFar
     */
    public static int buildHashCode(int totalSoFar, boolean value) {
        return totalSoFar * CONSTANT + (value ? 0 : 1);
    }

    /**
     * Build a hashCode for an Object array.
     *
     * @param totalSoFar  the hashCode total so far
     * @param array  the array to add to the hashCode
     * @return updated totalSoFar
     */
    public static int buildHashCode(int totalSoFar, Object[] array) {
        if (array == null) {
            return totalSoFar * CONSTANT;
        } else {
            for (int i = 0; i < array.length; i++) {
                totalSoFar = buildHashCode(totalSoFar, array[i]);
            }
            return totalSoFar;
        }
    }

    /**
     * Build a hashCode for a long array.
     *
     * @param totalSoFar  the hashCode total so far
     * @param array  the array to add to the hashCode
     * @return updated totalSoFar
     */
    public static int buildHashCode(int totalSoFar, long[] array) {
        if (array == null) {
            return totalSoFar * CONSTANT;
        } else {
            for (int i = 0; i < array.length; i++) {
                totalSoFar = buildHashCode(totalSoFar, array[i]);
            }
            return totalSoFar;
        }
    }

    /**
     * Build a hashCode for an int array.
     *
     * @param totalSoFar  the hashCode total so far
     * @param array  the array to add to the hashCode
     * @return updated totalSoFar
     */
    public static int buildHashCode(int totalSoFar, int[] array) {
        if (array == null) {
            return totalSoFar * CONSTANT;
        } else {
            for (int i = 0; i < array.length; i++) {
                totalSoFar = buildHashCode(totalSoFar, array[i]);
            }
            return totalSoFar;
        }
    }

    /**
     * Build a hashCode for a short array.
     *
     * @param totalSoFar  the hashCode total so far
     * @param array  the array to add to the hashCode
     * @return updated totalSoFar
     */
    public static int buildHashCode(int totalSoFar, short[] array) {
        if (array == null) {
            return totalSoFar * CONSTANT;
        } else {
            for (int i = 0; i < array.length; i++) {
                totalSoFar = buildHashCode(totalSoFar, array[i]);
            }
            return totalSoFar;
        }
    }

    /**
     * Build a hashCode for a char array.
     *
     * @param totalSoFar  the hashCode total so far
     * @param array  the array to add to the hashCode
     * @return updated totalSoFar
     */
    public static int buildHashCode(int totalSoFar, char[] array) {
        if (array == null) {
            return totalSoFar * CONSTANT;
        } else {
            for (int i = 0; i < array.length; i++) {
                totalSoFar = buildHashCode(totalSoFar, array[i]);
            }
            return totalSoFar;
        }
    }

    /**
     * Build a hashCode for a byte array.
     *
     * @param totalSoFar  the hashCode total so far
     * @param array  the array to add to the hashCode
     * @return updated totalSoFar
     */
    public static int buildHashCode(int totalSoFar, byte[] array) {
        if (array == null) {
            return totalSoFar * CONSTANT;
        } else {
            for (int i = 0; i < array.length; i++) {
                totalSoFar = buildHashCode(totalSoFar, array[i]);
            }
            return totalSoFar;
        }
    }

    /**
     * Build a hashCode for a double array.
     *
     * @param totalSoFar  the hashCode total so far
     * @param array  the array to add to the hashCode
     * @return updated totalSoFar
     */
    public static int buildHashCode(int totalSoFar, double[] array) {
        if (array == null) {
            return totalSoFar * CONSTANT;
        } else {
            for (int i = 0; i < array.length; i++) {
                totalSoFar = buildHashCode(totalSoFar, array[i]);
            }
            return totalSoFar;
        }
    }

    /**
     * Build a hashCode for a float array.
     *
     * @param totalSoFar  the hashCode total so far
     * @param array  the array to add to the hashCode
     * @return updated totalSoFar
     */
    public static int buildHashCode(int totalSoFar, float[] array) {
        if (array == null) {
            return totalSoFar * CONSTANT;
        } else {
            for (int i = 0; i < array.length; i++) {
                totalSoFar = buildHashCode(totalSoFar, array[i]);
            }
            return totalSoFar;
        }
    }

    /**
     * Build a hashCode for a boolean array.
     *
     * @param totalSoFar  the hashCode total so far
     * @param array  the array to add to the hashCode
     * @return updated totalSoFar
     */
    public static int buildHashCode(int totalSoFar, boolean[] array) {
        if (array == null) {
            return totalSoFar * CONSTANT;
        } else {
            for (int i = 0; i < array.length; i++) {
                totalSoFar = buildHashCode(totalSoFar, array[i]);
            }
            return totalSoFar;
        }
    }

}
