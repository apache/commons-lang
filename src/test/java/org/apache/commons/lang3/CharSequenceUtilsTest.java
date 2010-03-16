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

import java.nio.CharBuffer;

import junit.framework.Assert;
import junit.framework.TestCase;

/**
 * Tests CharSequenceUtils
 *
 * @author Gary Gregory
 */
public class CharSequenceUtilsTest extends TestCase {

    public void testLength_CharBuffer() {
        Assert.assertEquals(0, CharSequenceUtils.length(CharBuffer.wrap("")));
        Assert.assertEquals(1, CharSequenceUtils.length(CharBuffer.wrap("A")));
        Assert.assertEquals(1, CharSequenceUtils.length(CharBuffer.wrap(" ")));
        Assert.assertEquals(8, CharSequenceUtils.length(CharBuffer.wrap("ABCDEFGH")));
    }

    public void testLength_String() {
        Assert.assertEquals(0, CharSequenceUtils.length(null));
        Assert.assertEquals(0, CharSequenceUtils.length(""));
        Assert.assertEquals(1, CharSequenceUtils.length("A"));
        Assert.assertEquals(1, CharSequenceUtils.length(" "));
        Assert.assertEquals(8, CharSequenceUtils.length("ABCDEFGH"));
    }

    public void testLength_StringBuffer() {
        Assert.assertEquals(0, CharSequenceUtils.length(new StringBuffer("")));
        Assert.assertEquals(1, CharSequenceUtils.length(new StringBuffer("A")));
        Assert.assertEquals(1, CharSequenceUtils.length(new StringBuffer(" ")));
        Assert.assertEquals(8, CharSequenceUtils.length(new StringBuffer("ABCDEFGH")));
    }

    public void testLength_StringBuilder() {
        Assert.assertEquals(0, CharSequenceUtils.length(new StringBuilder("")));
        Assert.assertEquals(1, CharSequenceUtils.length(new StringBuilder("A")));
        Assert.assertEquals(1, CharSequenceUtils.length(new StringBuilder(" ")));
        Assert.assertEquals(8, CharSequenceUtils.length(new StringBuilder("ABCDEFGH")));
    }

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
