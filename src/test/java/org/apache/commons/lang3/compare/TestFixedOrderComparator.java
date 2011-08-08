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
package org.apache.commons.collections.comparators;

import java.util.Arrays;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;

import junit.framework.TestCase;

/**
 * Test class for FixedOrderComparator.
 *
 * @version $Revision$ $Date$
 *
 * @author David Leppik
 * @author Stephen Colebourne
 */
public class TestFixedOrderComparator extends TestCase {

    /**
     * Top cities of the world, by population including metro areas.
     */
    private static final String topCities[] = new String[] {
        "Tokyo",
        "Mexico City",
        "Mumbai",
        "Sao Paulo",
        "New York",
        "Shanghai",
        "Lagos",
        "Los Angeles",
        "Calcutta",
        "Buenos Aires"
    };

    //
    // Initialization and busywork
    //

    public TestFixedOrderComparator(String name) {
        super(name);
    }

    //
    // Set up and tear down
    //



    //
    // The tests
    //

    /**
     * Tests that the constructor plus add method compares items properly.
     */
    public void testConstructorPlusAdd() {
        FixedOrderComparator<String> comparator = new FixedOrderComparator<String>();
        for (int i = 0; i < topCities.length; i++) {
            comparator.add(topCities[i]);
        }
        String[] keys = topCities.clone();
        assertComparatorYieldsOrder(keys, comparator);
    }

    /**
     * Tests that the array constructor compares items properly.
     */
    public void testArrayConstructor() {
        String[] keys = topCities.clone();
        String[] topCitiesForTest = topCities.clone();
        FixedOrderComparator<String> comparator = new FixedOrderComparator<String>(topCitiesForTest);
        assertComparatorYieldsOrder(keys, comparator);
        // test that changing input after constructor has no effect
        topCitiesForTest[0] = "Brighton";
        assertComparatorYieldsOrder(keys, comparator);
    }

    /**
     * Tests the list constructor.
     */
    public void testListConstructor() {
        String[] keys = topCities.clone();
        List<String> topCitiesForTest = new LinkedList<String>(Arrays.asList(topCities));
        FixedOrderComparator<String> comparator = new FixedOrderComparator<String>(topCitiesForTest);
        assertComparatorYieldsOrder(keys, comparator);
        // test that changing input after constructor has no effect
        topCitiesForTest.set(0, "Brighton");
        assertComparatorYieldsOrder(keys, comparator);
    }

    /**
     * Tests addAsEqual method.
     */
    public void testAddAsEqual() {
        FixedOrderComparator<String> comparator = new FixedOrderComparator<String>(topCities);
        comparator.addAsEqual("New York", "Minneapolis");
        assertEquals(0, comparator.compare("New York", "Minneapolis"));
        assertEquals(-1, comparator.compare("Tokyo", "Minneapolis"));
        assertEquals(1, comparator.compare("Shanghai", "Minneapolis"));
    }

    /**
     * Tests whether or not updates are disabled after a comparison is made.
     */
    public void testLock() {
        FixedOrderComparator<String> comparator = new FixedOrderComparator<String>(topCities);
        assertEquals(false, comparator.isLocked());
        comparator.compare("New York", "Tokyo");
        assertEquals(true, comparator.isLocked());
        try {
            comparator.add("Minneapolis");
            fail("Should have thrown an UnsupportedOperationException");
        } catch (UnsupportedOperationException e) {
            // success -- ignore
        }

        try {
            comparator.addAsEqual("New York", "Minneapolis");
            fail("Should have thrown an UnsupportedOperationException");
        } catch (UnsupportedOperationException e) {
            // success -- ignore
        }
    }

    public void testUnknownObjectBehavior() {
        FixedOrderComparator<String> comparator = new FixedOrderComparator<String>(topCities);
        try {
            comparator.compare("New York", "Minneapolis");
            fail("Should have thrown a IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // success-- ignore
        }
        try {
            comparator.compare("Minneapolis", "New York");
            fail("Should have thrown a IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // success-- ignore
        }
        assertEquals(FixedOrderComparator.UnknownObjectBehavior.EXCEPTION, comparator.getUnknownObjectBehavior());

        comparator = new FixedOrderComparator<String>(topCities);
        comparator.setUnknownObjectBehavior(FixedOrderComparator.UnknownObjectBehavior.BEFORE);
        assertEquals(FixedOrderComparator.UnknownObjectBehavior.BEFORE, comparator.getUnknownObjectBehavior());
        LinkedList<String> keys = new LinkedList<String>(Arrays.asList(topCities));
        keys.addFirst("Minneapolis");
        assertComparatorYieldsOrder(keys.toArray(new String[0]), comparator);

        assertEquals(-1, comparator.compare("Minneapolis", "New York"));
        assertEquals( 1, comparator.compare("New York", "Minneapolis"));
        assertEquals( 0, comparator.compare("Minneapolis", "St Paul"));

        comparator = new FixedOrderComparator<String>(topCities);
        comparator.setUnknownObjectBehavior(FixedOrderComparator.UnknownObjectBehavior.AFTER);
        keys = new LinkedList<String>(Arrays.asList(topCities));
        keys.add("Minneapolis");
        assertComparatorYieldsOrder(keys.toArray(new String[0]), comparator);

        assertEquals( 1, comparator.compare("Minneapolis", "New York"));
        assertEquals(-1, comparator.compare("New York", "Minneapolis"));
        assertEquals( 0, comparator.compare("Minneapolis", "St Paul"));

    }

    //
    // Helper methods
    //

    /** Shuffles the keys and asserts that the comparator sorts them back to
     * their original order.
     */
    @SuppressWarnings("unused")
    private void assertComparatorYieldsOrder(String[] orderedObjects,
                                             Comparator<String> comparator) {
        String[] keys = orderedObjects.clone();

        // shuffle until the order changes.  It's extremely rare that
        // this requires more than one shuffle.

        boolean isInNewOrder = false;
        Random rand = new Random();
        while (keys.length > 1 && isInNewOrder == false) {
            // shuffle:
            for (int i = keys.length-1; i > 0; i--) {
                String swap = keys[i];
                int j = rand.nextInt(i+1);
                keys[i] = keys[j];
                keys[j] = swap;
            }

            // testShuffle
            for (int i = 0; i < keys.length && !isInNewOrder; i++) {
                if( !orderedObjects[i].equals(keys[i])) {
                    isInNewOrder = true;
                }
            }
        }

        // The real test:  sort and make sure they come out right.

        Arrays.sort(keys, comparator);

        for (int i = 0; i < orderedObjects.length; i++) {
            assertEquals(orderedObjects[i], keys[i]);
        }
    }

}
