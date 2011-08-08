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

/**
 * Tests for ComparableComparator.
 *
 * @version $Revision$ $Date$
 *
 */
@SuppressWarnings("boxing")
public class TestComparableComparator extends AbstractTestComparator<Integer> {

    public TestComparableComparator(String testName) {
        super(testName);
    }

    @Override
    public Comparator<Integer> makeObject() {
        return new ComparableComparator<Integer>();
    }

    @Override
    public List<Integer> getComparableObjectsOrdered() {
        List<Integer> list = new LinkedList<Integer>();
        list.add(1);
        list.add(2);
        list.add(3);
        list.add(4);
        list.add(5);
        return list;
    }

}
