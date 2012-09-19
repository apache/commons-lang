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
package org.apache.commons.lang3;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;

import junit.framework.Assert;

import org.junit.Test;

/**
 * Tests CharSequenceUtils
 *
 * @version $Id: CharSequenceUtilsTest.java 1066341 2011-02-02 06:21:53Z bayard $
 */
public class CharSequenceUtilsTest {

    //-----------------------------------------------------------------------
    @Test
    public void testConstructor() {
        assertNotNull(new CharSequenceUtils());
        Constructor<?>[] cons = CharSequenceUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertTrue(Modifier.isPublic(cons[0].getModifiers()));
        assertTrue(Modifier.isPublic(CharSequenceUtils.class.getModifiers()));
        assertFalse(Modifier.isFinal(CharSequenceUtils.class.getModifiers()));
    }
    
    //-----------------------------------------------------------------------
    @Test
    public void testSubSequence() {
        //
        // null input
        //
        Assert.assertEquals(null, CharSequenceUtils.subSequence(null, -1));
        Assert.assertEquals(null, CharSequenceUtils.subSequence(null, 0));
        Assert.assertEquals(null, CharSequenceUtils.subSequence(null, 1));
        //
        // non-null input
        //
        Assert.assertEquals(StringUtils.EMPTY, CharSequenceUtils.subSequence(StringUtils.EMPTY, 0));
        Assert.assertEquals("012", CharSequenceUtils.subSequence("012", 0));
        Assert.assertEquals("12", CharSequenceUtils.subSequence("012", 1));
        Assert.assertEquals("2", CharSequenceUtils.subSequence("012", 2));
        Assert.assertEquals(StringUtils.EMPTY, CharSequenceUtils.subSequence("012", 3));
        //
        // Exception expected
        //
        try {
            Assert.assertEquals(null, CharSequenceUtils.subSequence(StringUtils.EMPTY, -1));
            Assert.fail("Expected " + IndexOutOfBoundsException.class.getName());
        } catch (IndexOutOfBoundsException e) {
            // Expected
        }
        try {
            Assert.assertEquals(null, CharSequenceUtils.subSequence(StringUtils.EMPTY, 1));
            Assert.fail("Expected " + IndexOutOfBoundsException.class.getName());
        } catch (IndexOutOfBoundsException e) {
            // Expected
        }
    }

}
