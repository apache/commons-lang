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
package org.apache.commons.lang.math;

import java.util.Random;

/**
 * <p><code>JVMRandom</code> is a wrapper that supports all possible 
 * Random methods via the java.lang.Math.random() method and its system-wide 
 * Random object.
 * 
 * @author Henri Yandell
 * @since 2.0
 * @version $Id: JVMRandom.java,v 1.6 2003/06/16 02:26:41 bayard Exp $
 */
public final class JVMRandom extends Random {

    /** ensures that only the constructor can call reseed */
    private boolean constructed = false;

    public JVMRandom() {
        this.constructed = true;
    }
    
    /** Unsupported in 2.0 */
    public synchronized void setSeed(long seed) {
        if (this.constructed) {
            throw new UnsupportedOperationException();
        }
    }

    /** Unsupported in 2.0 */
    public synchronized double nextGaussian() {
        throw new UnsupportedOperationException();
    }

    /** Unsupported in 2.0 */
    public void nextBytes(byte[] byteArray) {
        throw new UnsupportedOperationException();
    }

    /**
     * Returns the next pseudorandom, uniformly distributed int value 
     * from the Math.random() sequence. 
     *
     * @return the random int
     */
    public int nextInt() {
        return nextInt(Integer.MAX_VALUE);
    }
    /**
     * Returns a pseudorandom, uniformly distributed int value between 0 
     * (inclusive) and the specified value (exclusive), from the 
     * Math.random() sequence. 
     *
     * @param n  the specified exclusive max-value
     * @throws IllegalArgumentException when n <= 0
     *
     * @return the random int
     */
    public int nextInt(int n) {
        if (n <= 0) {
            throw new IllegalArgumentException(
                "Upper bound for nextInt must be positive"
            );
        }
        // TODO: check this cannot return 'n'
        return (int)(Math.random() * n);
    }
    /**
     * Returns the next pseudorandom, uniformly distributed long value 
     * from the Math.random() sequence.
     *
     * @return the random long
     */
    public long nextLong() {
        // possible loss of precision?
        return nextLong(Long.MAX_VALUE);
    }


    /**
     * Returns a pseudorandom, uniformly distributed long value between 0 
     * (inclusive) and the specified value (exclusive), from the 
     * Math.random() sequence.
     *
     * @param n  the specified exclusive max-value
     * @throws IllegalArgumentException when n <= 0
     *
     * @return the random long
     */
    public static long nextLong(long n) {
        if (n <= 0) {
            throw new IllegalArgumentException(
                "Upper bound for nextInt must be positive"
            );
        }
        // TODO: check this cannot return 'n'
        return (long)(Math.random() * n);
     }

    /**
     * Returns the next pseudorandom, uniformly distributed boolean value 
     * from the Math.random() sequence.
     *
     * @return the random boolean
     */
    public boolean nextBoolean() {
        return (Math.random() > 0.5);
    }
    /**
     * Returns the next pseudorandom, uniformly distributed float value 
     * between 0.0 and 1.0 from the Math.random() sequence.
     *
     * @return the random float
     */
    public float nextFloat() {
        return (float)Math.random();
    }
    /**
     * Synonymous to the Math.random() call.
     *
     * @return the random double
     */
    public double nextDouble() {
        return Math.random();
    }
    
}
