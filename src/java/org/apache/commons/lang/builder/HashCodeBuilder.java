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
package org.apache.commons.lang.builder;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
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
 *   ...
 * 
 *   public int hashCode() {
 *     // you pick a hard-coded, randomly chosen, non-zero, odd number
 *     // ideally different for each class
 *     return new HashCodeBuilder(17, 37).   
 *       append(name).
 *       append(age).
 *       append(smoker).
 *       toHashCode();
 *   }
 * }
 * </code>
 * <p>
 * Alternatively, there is a method that uses reflection to determine
 * the fields to test. Because these fields are usually private, the method, 
 * <code>reflectionTest</code>, uses <code>Field.setAccessible</code> to change
 * the visibility of the fields. This will fail under a security manager, 
 * unless the appropriate permissions are set. It is also slower than testing 
 * explicitly.
 * <p>
 * A typical invocation for this method would look like:
 * <code>
 * public boolean hashCode(Object o) {
 *   return HashCodeBuilder.reflectionHashCode(this);
 * }
 * </code>
 * 
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @version $Id: HashCodeBuilder.java,v 1.1 2002/09/12 21:59:01 scolebourne Exp $
 */
public class HashCodeBuilder {
    
    /**
     * Constant to use in building the hashCode
     */
    private final int iConstant;
    /**
     * Running total of the hashCode
     */
    private int iTotal = 0;
    
    /**
     * Constructor for HashCodeBuilder.
     * This constructor uses two hard coded choices for the constants needed
     * to build a hashCode.
     */
    public HashCodeBuilder() {
        super();
        iConstant = 37;
        iTotal = 17;
    }
    
    /**
     * Constructor for HashCodeBuilder.
     * Two randomly chosen, non-zero, odd numbers must be passed in. Ideally
     * these should be different for each class, however this is not vital.
     * Prime numbers are preferred, especially for the multiplier.
     * 
     * @param initialNonZeroOddNumber
     * @param multiplierNonZeroOddNumber
     * @throws IllegalArgumentException if the number is zero or even
     */
    public HashCodeBuilder(int initialNonZeroOddNumber, int multiplierNonZeroOddNumber) {
        super();
        if (initialNonZeroOddNumber == 0) {
            throw new IllegalArgumentException("HashCodeBuilder requires a non zero initial value");
        }
        if (initialNonZeroOddNumber % 2 == 0) {
            throw new IllegalArgumentException("HashCodeBuilder requires an odd initial value");
        }
        if (multiplierNonZeroOddNumber == 0) {
            throw new IllegalArgumentException("HashCodeBuilder requires a non zero multiplier");
        }
        if (multiplierNonZeroOddNumber % 2 == 0) {
            throw new IllegalArgumentException("HashCodeBuilder requires an odd multiplier");
        }
        iConstant = multiplierNonZeroOddNumber;
        iTotal = initialNonZeroOddNumber;
    }

    //-------------------------------------------------------------------------
    
    /**
     * This method uses reflection to build a valid hash code. 
     * <p>
     * It uses Field.setAccessible to gain access to private fields. This means
     * that it will throw a security exception if run under a security manger, if
     * the permissions are not set up.
     * It is also not as efficient as testing explicitly. 
     * Transient members will be not be used, as they are likely derived 
     * fields, and not part of the value of the object. 
     * This constructor uses two hard coded choices for the constants needed
     * to build a hash code.
     * 
     * @param object  the object to create a hash code for
     * @return int hash code
     * @throws IllegalArgumentException if the object is null
     */
    public static int reflectionHashCode(Object object) {
        return reflectionHashCode(object, false);
    }

    /**
     * This method uses reflection to build a valid hash code. 
     * <p>
     * It uses Field.setAccessible to gain access to private fields. This means
     * that it will throw a security exception if run under a security manger, if
     * the permissions are not set up.
     * It is also not as efficient as testing explicitly. 
     * Transient members will be not be used, as they are likely derived 
     * fields, and not part of the value of the object. 
     * <p>
     * Two randomly chosen, non-zero, odd numbers must be passed in. Ideally
     * these should be different for each class, however this is not vital.
     * Prime numbers are preferred, especially for the multiplier.
     * 
     * @param initialNonZeroOddNumber
     * @param multiplierNonZeroOddNumber
     * @param object  the object to create a hash code for
     * @return int hash code
     * @throws IllegalArgumentException if the object is null
     * @throws IllegalArgumentException if the number is zero or even
     */
    public static int reflectionHashCode(
            int initialNonZeroOddNumber, int multiplierNonZeroOddNumber,
            Object object) {
        return reflectionHashCode(initialNonZeroOddNumber, multiplierNonZeroOddNumber, object);
    }
    
    /**
     * This method uses reflection to build a valid hash code. 
     * <p>
     * It uses Field.setAccessible to gain access to private fields. This means
     * that it will throw a security exception if run under a security manger, if
     * the permissions are not set up.
     * It is also not as efficient as testing explicitly. 
     * If the TestTransients parameter is set to true, transient members will be
     * tested, otherwise they are ignored, as they are likely derived fields, and
     * not part of the value of the object. 
     * This constructor uses two hard coded choices for the constants needed
     * to build a hash code.
     * 
     * @param object  the object to create a hash code for
     * @param testTransients  whether to include transient fields
     * @return int hash code
     * @throws IllegalArgumentException if the object is null
     */
    public static int reflectionHashCode(Object object, boolean testTransients) {
        return reflectionHashCode(17, 37, object, testTransients);
    }
        
    /**
     * This method uses reflection to build a valid hash code. 
     * <p>
     * It uses Field.setAccessible to gain access to private fields. This means
     * that it will throw a security exception if run under a security manger, if
     * the permissions are not set up.
     * It is also not as efficient as testing explicitly. 
     * If the TestTransients parameter is set to true, transient members will be
     * tested, otherwise they are ignored, as they are likely derived fields, and
     * not part of the value of the object. 
     * <p>
     * Two randomly chosen, non-zero, odd numbers must be passed in. Ideally
     * these should be different for each class, however this is not vital.
     * Prime numbers are preferred, especially for the multiplier.
     * 
     * @param initialNonZeroOddNumber
     * @param multiplierNonZeroOddNumber
     * @param object  the object to create a hash code for
     * @param testTransients  whether to include transient fields
     * @return int hash code
     * @throws IllegalArgumentException if the object is null
     * @throws IllegalArgumentException if the number is zero or even
     */
    public static int reflectionHashCode(
            int initialNonZeroOddNumber, int multiplierNonZeroOddNumber,
            Object object, boolean testTransients) {
                
        if (object == null) {
            throw new IllegalArgumentException("The object to build a hash code for must not be null");
        }
        HashCodeBuilder hashCodeBuilder = new HashCodeBuilder(initialNonZeroOddNumber, multiplierNonZeroOddNumber);
        Field[] fields = object.getClass().getDeclaredFields();
        Field.setAccessible(fields, true);
        for (int i = 0; i < fields.length; ++i) {
            Field f = fields[i];
            if (testTransients || !Modifier.isTransient(f.getModifiers())) {
                try {
                    hashCodeBuilder.append(f.get(object));
                } catch (IllegalAccessException e) {
                    //this can't happen. Would get a Security exception instead
                    //throw a runtime exception in case the impossible happens.
                    throw new InternalError("Unexpected IllegalAccessException");
                }
            }
        }
        return hashCodeBuilder.toHashCode();
    }

    //-------------------------------------------------------------------------
    
    /**
     * Append a hashCode for an Object.
     *
     * @param object  the object to add to the hashCode
     * @return this
     */
    public HashCodeBuilder append(Object object) {
        if (object == null) {
            iTotal = iTotal * iConstant;
            
        } else {
            if (object.getClass().isArray() == false) {
                //the simple case, not an array, just the element 
                iTotal = iTotal * iConstant + object.hashCode();
                
            } else {
                //'Switch' on type of array, to dispatch to the correct handler
                // This handles multi dimensional arrays
                if (object instanceof long[]) {
                    append((long[]) object);
                } else if (object instanceof int[]) {
                    append((int[]) object);
                } else if (object instanceof short[]) {
                    append((short[]) object);
                } else if (object instanceof char[]) {
                    append((char[]) object);
                } else if (object instanceof byte[]) {
                    append((byte[]) object);
                } else if (object instanceof double[]) {
                    append((double[]) object);
                } else if (object instanceof float[]) {
                    append((float[]) object);
                } else if (object instanceof boolean[]) {
                    append((boolean[]) object);
                } else { 
                    // Not an array of primitives
                    append((Object[]) object);
                }
            }
        }
        return this;
    }

    /**
     * Append a hashCode for a long.
     *
     * @param value  the long to add to the hashCode
     * @return this
     */
    public HashCodeBuilder append(long value) {
        iTotal = iTotal * iConstant + ((int) (value ^ (value >> 32)));
        return this;
    }

    /**
     * Append a hashCode for an int.
     *
     * @param value  the int to add to the hashCode
     * @return this
     */
    public HashCodeBuilder append(int value) {
        iTotal = iTotal * iConstant + value;
        return this;
    }

    /**
     * Append a hashCode for a short.
     *
     * @param value  the short to add to the hashCode
     * @return this
     */
    public HashCodeBuilder append(short value) {
        iTotal = iTotal * iConstant + (int) value;
        return this;
    }

    /**
     * Append a hashCode for a char.
     *
     * @param value  the char to add to the hashCode
     * @return this
     */
    public HashCodeBuilder append(char value) {
        iTotal = iTotal * iConstant + (int) value;
        return this;
    }

    /**
     * Append a hashCode for a byte.
     *
     * @param value  the byte to add to the hashCode
     * @return this
     */
    public HashCodeBuilder append(byte value) {
        iTotal = iTotal * iConstant + (int) value;
        return this;
    }

    /**
     * Append a hashCode for a double.
     *
     * @param value  the double to add to the hashCode
     * @return this
     */
    public HashCodeBuilder append(double value) {
        return append(Double.doubleToLongBits(value));
    }

    /**
     * Append a hashCode for a float.
     *
     * @param value  the float to add to the hashCode
     * @return this
     */
    public HashCodeBuilder append(float value) {
        iTotal = iTotal * iConstant + Float.floatToIntBits(value);
        return this;
    }

    /**
     * Append a hashCode for a long.
     *
     * @param value  the long to add to the hashCode
     * @return this
     */
    public HashCodeBuilder append(boolean value) {
        iTotal = iTotal * iConstant + (value ? 0 : 1);
        return this;
    }

    /**
     * Append a hashCode for an Object array.
     *
     * @param array  the array to add to the hashCode
     * @return this
     */
    public HashCodeBuilder append(Object[] array) {
        if (array == null) {
            iTotal = iTotal * iConstant;
        } else {
            for (int i = 0; i < array.length; i++) {
                append(array[i]);
            }
        }
        return this;
    }

    /**
     * Append a hashCode for a long array.
     *
     * @param array  the array to add to the hashCode
     * @return this
     */
    public HashCodeBuilder append(long[] array) {
        if (array == null) {
            iTotal = iTotal * iConstant;
        } else {
            for (int i = 0; i < array.length; i++) {
                append(array[i]);
            }
        }
        return this;
    }

    /**
     * Append a hashCode for an int array.
     *
     * @param array  the array to add to the hashCode
     * @return this
     */
    public HashCodeBuilder append(int[] array) {
        if (array == null) {
            iTotal = iTotal * iConstant;
        } else {
            for (int i = 0; i < array.length; i++) {
                append(array[i]);
            }
        }
        return this;
    }

    /**
     * Append a hashCode for a short array.
     *
     * @param array  the array to add to the hashCode
     * @return this
     */
    public HashCodeBuilder append(short[] array) {
        if (array == null) {
            iTotal = iTotal * iConstant;
        } else {
            for (int i = 0; i < array.length; i++) {
                append(array[i]);
            }
        }
        return this;
    }

    /**
     * Append a hashCode for a char array.
     *
     * @param array  the array to add to the hashCode
     * @return this
     */
    public HashCodeBuilder append(char[] array) {
        if (array == null) {
            iTotal = iTotal * iConstant;
        } else {
            for (int i = 0; i < array.length; i++) {
                append(array[i]);
            }
        }
        return this;
    }

    /**
     * Append a hashCode for a byte array.
     *
     * @param array  the array to add to the hashCode
     * @return this
     */
    public HashCodeBuilder append(byte[] array) {
        if (array == null) {
            iTotal = iTotal * iConstant;
        } else {
            for (int i = 0; i < array.length; i++) {
                append(array[i]);
            }
        }
        return this;
    }

    /**
     * Append a hashCode for a double array.
     *
     * @param array  the array to add to the hashCode
     * @return this
     */
    public HashCodeBuilder append(double[] array) {
        if (array == null) {
            iTotal = iTotal * iConstant;
        } else {
            for (int i = 0; i < array.length; i++) {
                append(array[i]);
            }
        }
        return this;
    }

    /**
     * Append a hashCode for a float array.
     *
     * @param array  the array to add to the hashCode
     * @return this
     */
    public HashCodeBuilder append(float[] array) {
        if (array == null) {
            iTotal = iTotal * iConstant;
        } else {
            for (int i = 0; i < array.length; i++) {
                append(array[i]);
            }
        }
        return this;
    }

    /**
     * Append a hashCode for a boolean array.
     *
     * @param array  the array to add to the hashCode
     * @return this
     */
    public HashCodeBuilder append(boolean[] array) {
        if (array == null) {
            iTotal = iTotal * iConstant;
        } else {
            for (int i = 0; i < array.length; i++) {
                append(array[i]);
            }
        }
        return this;
    }

    /**
     * Return the computed hashCode
     * 
     * @return int hashCode based on the fields appended
     */    
    public int toHashCode() {
        return iTotal;
    }

}
