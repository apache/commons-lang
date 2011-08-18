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
import java.util.Arrays;

import org.junit.Before;
import org.junit.Test;

/**
 * <p>
 * Tests the methods in the {@link org.apache.commons.lang3.compare.FixedOrderComparator} class.
 * </p>
 * 
 * @version $Id: RangeTest.java 1147537 2011-07-17 06:10:37Z mbenson $
 */
public class FixedOrderComparatorTest {

    @Before
    public void setUp() {
    }

    //-----------------------------------------------------------------------
    @Test
    public void testJavadocExample() {
        String[] planets = {"Mercury", "Venus", "Earth", "Mars"};
        String[] modified = {"Mercury", "Venus", "Earth", "Mars"};
        Comparator distanceFromSun = new FixedOrderComparator(planets);
        Arrays.sort(modified);                     // Sort to alphabetical order
        Arrays.sort(modified, distanceFromSun);    // Back to original order
        assertArrayEquals("Did not sort to fixed order", planets, modified);
    }

}
