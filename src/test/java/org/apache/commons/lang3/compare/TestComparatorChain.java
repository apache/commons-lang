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

import java.io.Serializable;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

/**
 * Tests for ComparatorChain.
 *
 * @version $Revision$ $Date$
 *
 * @author Unknown
 */
public class TestComparatorChain extends AbstractTestComparator<TestComparatorChain.PseudoRow> {

    public TestComparatorChain(String testName) {
        super(testName);
    }

    @Override
    public Comparator<PseudoRow> makeObject() {
        ComparatorChain<PseudoRow> chain = new ComparatorChain<PseudoRow>(new ColumnComparator(0));
        chain.addComparator(new ColumnComparator(1), true); // reverse the second column
        chain.addComparator(new ColumnComparator(2), false);
        return chain;
    }

    public void testNoopComparatorChain() {
        ComparatorChain<Integer> chain = new ComparatorChain<Integer>();
        Integer i1 = new Integer(4);
        Integer i2 = new Integer(6);
        chain.addComparator(new ComparableComparator<Integer>());

        int correctValue = i1.compareTo(i2);
        assertTrue("Comparison returns the right order", chain.compare(i1, i2) == correctValue);
    }

    public void testBadNoopComparatorChain() {
        ComparatorChain<Integer> chain = new ComparatorChain<Integer>();
        Integer i1 = new Integer(4);
        Integer i2 = new Integer(6);
        try {
            chain.compare(i1,i2);
            fail("An exception should be thrown when a chain contains zero comparators.");
        } catch (UnsupportedOperationException e) {
        }
    }

    public void testListComparatorChain() {
        List<Comparator<Integer>> list = new LinkedList<Comparator<Integer>>();
        list.add(new ComparableComparator<Integer>());
        ComparatorChain<Integer> chain = new ComparatorChain<Integer>(list);
        Integer i1 = new Integer(4);
        Integer i2 = new Integer(6);

        int correctValue = i1.compareTo(i2);
        assertTrue("Comparison returns the right order", chain.compare(i1, i2) == correctValue);
    }

    public void testBadListComparatorChain() {
        List<Comparator<Integer>> list = new LinkedList<Comparator<Integer>>();
        ComparatorChain<Integer> chain = new ComparatorChain<Integer>(list);
        Integer i1 = new Integer(4);
        Integer i2 = new Integer(6);
        try {
            chain.compare(i1, i2);
            fail("An exception should be thrown when a chain contains zero comparators.");
        } catch (UnsupportedOperationException e) {
        }
    }

    public void testComparatorChainOnMinvaluedCompatator() {
        // -1 * Integer.MIN_VALUE is less than 0,
        // test that ComparatorChain handles this edge case correctly
        ComparatorChain<Integer> chain = new ComparatorChain<Integer>();
        chain.addComparator(new Comparator<Integer>() {
            public int compare(Integer a, Integer b) {
                int result = a.compareTo(b);
                if (result < 0) {
                    return Integer.MIN_VALUE;
                }
                if (result > 0) {
                    return Integer.MAX_VALUE;
                }
                return 0;
            }
        }, true);

        assertTrue(chain.compare(new Integer(4), new Integer(5)) > 0);
        assertTrue(chain.compare(new Integer(5), new Integer(4)) < 0);
        assertTrue(chain.compare(new Integer(4), new Integer(4)) == 0);
    }

    @Override
    public List<PseudoRow> getComparableObjectsOrdered() {
        List<PseudoRow> list = new LinkedList<PseudoRow>();
        // this is the correct order assuming a
        // "0th forward, 1st reverse, 2nd forward" sort
        list.add(new PseudoRow(1, 2, 3));
        list.add(new PseudoRow(2, 3, 5));
        list.add(new PseudoRow(2, 2, 4));
        list.add(new PseudoRow(2, 2, 8));
        list.add(new PseudoRow(3, 1, 0));
        list.add(new PseudoRow(4, 4, 4));
        list.add(new PseudoRow(4, 4, 7));
        return list;
    }

    @SuppressWarnings("serial")
    public static class PseudoRow implements Serializable {

        public int cols[] = new int[3];

        public PseudoRow(int col1, int col2, int col3) {
            cols[0] = col1;
            cols[1] = col2;
            cols[2] = col3;
        }

        public int getColumn(int colIndex) {
            return cols[colIndex];
        }

        @Override
        public String toString() {
            StringBuilder buf = new StringBuilder();
            buf.append("[");
            buf.append(cols[0]);
            buf.append(",");
            buf.append(cols[1]);
            buf.append(",");
            buf.append(cols[2]);
            buf.append("]");
            return buf.toString();
        }

        @Override
        public boolean equals(Object o) {
            if (!(o instanceof PseudoRow)) {
                return false;
            }

            PseudoRow row = (PseudoRow) o;
            if (getColumn(0) != row.getColumn(0)) {
                return false;
            }

            if (getColumn(1) != row.getColumn(1)) {
                return false;
            }

            if (getColumn(2) != row.getColumn(2)) {
                return false;
            }

            return true;
        }

    }

    public static class ColumnComparator implements Comparator<PseudoRow>, Serializable {
        private static final long serialVersionUID = -2284880866328872105L;

        protected int colIndex = 0;

        public ColumnComparator(int colIndex) {
            this.colIndex = colIndex;
        }

        public int compare(PseudoRow o1, PseudoRow o2) {

            int col1 = o1.getColumn(colIndex);
            int col2 = o2.getColumn(colIndex);

            if (col1 > col2) {
                return 1;
            }
            if (col1 < col2) {
                return -1;
            }
            return 0;
        }

        @Override
        public int hashCode() {
            return colIndex;
        }

        @Override
        public boolean equals(Object that) {
            return that instanceof ColumnComparator && colIndex == ((ColumnComparator) that).colIndex;
        }
    }
}
