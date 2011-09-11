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
import java.util.Iterator;

import org.junit.Before;
import org.junit.Test;

/**
 * <p>
 * Tests the methods in the {@link org.apache.commons.lang3.compare.ComparatorChain} class.
 * </p>
 * 
 * @version $Id: RangeTest.java 1147537 2011-07-17 06:10:37Z mbenson $
 */
public class ComparatorChainTest {

    @Test
    public void testUse() {
        // Sorts ABC-123 by numbers and then letters
        ComparatorChain cc = new ComparatorChain(
            new ComparableComparator<String>() {
                public int compare(String o1, String o2) {
                    return super.compare(o1.substring(4), o2.substring(4));
                }
            },
            ComparableComparator.INSTANCE
        );

        assertTrue("Comparison failed", cc.compare( "ABC-123", "ABC-124" ) < 0 );
        assertTrue("Comparison failed", cc.compare( "ZZZ-123", "AAA-124" ) < 0 );
        assertTrue("Comparison failed", cc.compare( "ABC-123", "ABD-123" ) < 0 );
        assertTrue("Comparison failed", cc.compare( "ABC-123", "ABC-123" ) == 0 );
    }

    @Test
    public void testIterate() {
        Comparator c1 = ComparableComparator.INSTANCE;
        Comparator c2 = new ComparableComparator();
        Comparator c3 = new NullComparator();
        Comparator c4 = new ReverseComparator();
        Iterable cc = new ComparatorChain(c1, c2, c3, c4);

        Iterator itr = cc.iterator();
        assertEquals( "Iteration failed", c1, itr.next() );
        assertEquals( "Iteration failed", c2, itr.next() );
        assertEquals( "Iteration failed", c3, itr.next() );
        assertEquals( "Iteration failed", c4, itr.next() );
        assertFalse( "Iteration failed", itr.hasNext() );
    }

}
