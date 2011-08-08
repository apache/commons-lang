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

import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * Test the NullComparator.
 *
 * @version $Revision$ $Date$
 *
 * @author Michael A. Smith
 */
public abstract class AbstractTestNullComparator extends AbstractTestComparator<Integer> {

    public AbstractTestNullComparator(String testName) {
        super(testName);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(AbstractTestNullComparator.class.getName());
        suite.addTest(new TestSuite(TestNullComparator1.class));
        suite.addTest(new TestSuite(TestNullComparator2.class));
        return suite;
    }

    /**
     *  Test the NullComparator with nulls high, using comparable comparator
     **/
    public static class TestNullComparator1 extends AbstractTestNullComparator {

        public TestNullComparator1(String testName) {
            super(testName);
        }

        @Override
        public Comparator<Integer> makeObject() {
            return new NullComparator<Integer>();
        }

        @Override
        public List<Integer> getComparableObjectsOrdered() {
            List<Integer> list = new LinkedList<Integer>();
            list.add(new Integer(1));
            list.add(new Integer(2));
            list.add(new Integer(3));
            list.add(new Integer(4));
            list.add(new Integer(5));
            list.add(null);
            return list;
        }

        @Override
        public String getCanonicalComparatorName(Object object) {
            return super.getCanonicalComparatorName(object) + "1";
        }
    }

    /**
     *  Test the NullComparator with nulls low using the comparable comparator
     **/
    public static class TestNullComparator2 extends AbstractTestNullComparator {

        public TestNullComparator2(String testName) {
            super(testName);
        }

        @Override
        public Comparator<Integer> makeObject() {
            return new NullComparator<Integer>(false);
        }

        @Override
        public List<Integer> getComparableObjectsOrdered() {
            List<Integer> list = new LinkedList<Integer>();
            list.add(null);
            list.add(new Integer(1));
            list.add(new Integer(2));
            list.add(new Integer(3));
            list.add(new Integer(4));
            list.add(new Integer(5));
            return list;
        }

        @Override
        public String getCanonicalComparatorName(Object object) {
            return super.getCanonicalComparatorName(object) + "2";
        }
    }
}
