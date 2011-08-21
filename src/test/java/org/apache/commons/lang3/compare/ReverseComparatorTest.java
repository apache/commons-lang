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

package org.apache.commons.lang3.compare;

import static org.junit.Assert.*;

import java.util.Comparator;

import org.junit.Before;
import org.junit.Test;

/**
 * <p>
 * Tests the methods in the {@link org.apache.commons.lang3.compare.ReverseComparator} class.
 * </p>
 * 
 * @version $Id: RangeTest.java 1147537 2011-07-17 06:10:37Z mbenson $
 */
public class ReverseComparatorTest {

    @Before
    public void setUp() {
    }

    //-----------------------------------------------------------------------
    @Test
    public void testUse() {
        ReverseComparator rc = new ReverseComparator();

        // back to front tests
        assertTrue("Comparison wasn't reversed", rc.compare( 2, 1 ) < 0 );
        assertTrue("Comparison wasn't reversed", rc.compare( 1, 2 ) > 0 );
        assertTrue("Comparison wasn't reversed", rc.compare( "baa", "aardvark" ) < 0 );
        assertTrue("Comparison wasn't reversed", rc.compare( "aardvark", "baa" ) > 0 );
    }

    @Test
    public void testTwoCallsCancel() {
        ReverseComparator rc = new ReverseComparator(new ReverseComparator());

        // back to front tests
        assertTrue("Reversal wasn't cancelled out", rc.compare( 1, 2 ) < 0 );
        assertTrue("Reversal wasn't cancelled out", rc.compare( 2, 1 ) > 0 );
        assertTrue("Reversal wasn't cancelled out", rc.compare( "aardvark", "baa" ) < 0 );
        assertTrue("Reversal wasn't cancelled out", rc.compare( "baa", "aardvark" ) > 0 );
    }

    @Test
    public void testEquality() {
        ReverseComparator rc1 = new ReverseComparator();
        ReverseComparator rc2 = new ReverseComparator(rc1);
        ReverseComparator rc3 = new ReverseComparator(rc1);

        // test same instance
        assertTrue("Same instance wasn't equal", rc1.equals(rc1));
        assertEquals("Equal instance has different hash code", rc1.hashCode(), rc1.hashCode());

        // test null not equal
        assertFalse("Null was equal", rc1.equals(null));

        // test diff subcomparator not equal
        assertFalse("Was equal despite different nested comparators", rc1.equals(rc2));

        // test same subcomparator equal
        assertTrue("Wasn't equal despite same nested comparator", rc2.equals(rc3));
        assertEquals("Same subcomparator had different hash code", rc2.hashCode(), rc3.hashCode());

        // test different type not equal
        assertFalse("Was equal despite not being same class", rc1.equals(this));
    }

}
