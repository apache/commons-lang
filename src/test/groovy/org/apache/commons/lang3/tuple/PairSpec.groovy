/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.commons.lang3.tuple

import org.apache.commons.lang3.AbstractLangSpec
import spock.lang.Title

import static org.junit.jupiter.api.Assertions.*

@Title("org.apache.commons.lang3.tuple.PairTest")
class PairSpec extends AbstractLangSpec {

    def "testComparable1"() {
        given:
        Pair<String, String> pair1 = Pair.of("A", "D")
        Pair<String, String> pair2 = Pair.of("B", "C")
        expect:
        0 == (pair1 <=> pair1)
        pair1 < pair2
        0 == (pair2 <=> pair2)
        pair2 > pair1
    }

    def "testComparable2"() {
        given:
        Pair<String, String> pair1 = Pair.of("A", "C")
        Pair<String, String> pair2 = Pair.of("A", "D")
        expect:
        0 == (pair1 <=> pair1)
        pair1 < pair2
        0 == (pair2 <=> pair2)
        pair2 > pair1
    }

    def "testCompatibilityBetweenPairs"() {
        given:
        Pair<Integer, String> pair = ImmutablePair.of(0, "foo")
        Pair<Integer, String> pair2 = MutablePair.of(0, "foo")
        HashSet<Pair<Integer, String>> set = new HashSet<>()
        set.add(pair)

        expect:
        pair == pair2
        pair.hashCode() == pair2.hashCode()
        set.contains(pair2)

        when:
        pair2.setValue("bar")

        then:
        pair != pair2
        pair.hashCode() != pair2.hashCode()
    }

    def "testEmptyArrayGenerics"() {
        given:
        Pair<Integer, String>[] empty = Pair.emptyArray()
        expect:
        0 == empty.length
    }

    def "testEmptyArrayLength"() {
        given:
        @SuppressWarnings("unchecked")
        Pair<Integer, String>[] empty = (Pair<Integer, String>[]) Pair.EMPTY_ARRAY
        expect:
        0 == empty.length
    }

    def "testFormattable_padded"() {
        given:
        Pair<String, String> pair = Pair.of("Key", "Value")
        expect:
         "         (Key,Value)" == String.format('%1$20s', pair)
    }

    def "testFormattable_simple"() {
        given:
        Pair<String, String> pair = Pair.of("Key", "Value")
        expect:
        "(Key,Value)" == String.format('%1$s', pair)
    }

    def "testMapEntry"() {
        given:
        Pair<Integer, String> pair = ImmutablePair.of(0, "foo")
        HashMap<Integer, String> map = new HashMap<>()
        map.put(0, "foo")
        Map.Entry<Integer, String> entry = map.entrySet().iterator().next()
        expect:
        pair.equals(entry) // pair is Map.Entry but == or as Map.Entry didn't work
        pair.hashCode() == entry.hashCode()
    }

    def "testOfNonNull_1"() {
        when:
        Pair.ofNonNull(null, null)
        then:
        thrown(NullPointerException)
    }

    def "testOfNonNull_2"() {
        when:
        Pair.ofNonNull(null, "x")
        then:
        thrown(NullPointerException)
    }

    def "testOfNonNull_3"() {
        when:
        Pair.ofNonNull("x", null)
        then:
        thrown(NullPointerException)
    }

    def "testOfNonNull_4"() {
        given:
        Pair<String, String> pair = Pair.ofNonNull("x", "y")
        expect:
        "x" == pair.getLeft()
        "y" == pair.getRight()
    }

    def "testPairOfMapEntry"() {
        given:
        HashMap<Integer, String> map = new HashMap<>()
        map.put(0, "foo")
        Map.Entry<Integer, String> entry = map.entrySet().iterator().next()
        Pair<Integer, String> pair = Pair.of(entry)
        expect:
        entry.getKey() == pair.getLeft()
        entry.getValue() == pair.getRight()
    }

    def "testPairOfObjects"() {
        given:
        Pair<Integer, String> pair = Pair.of(0, "foo")
        Pair<Object, String> pair2 = Pair.of(null, "bar")
        Pair<?, ?> pair3 = Pair.of(null, null)

        expect:
        pair instanceof ImmutablePair<?, ?>
        0 == ((ImmutablePair<Integer, String>) pair).left.intValue()
        "foo" == ((ImmutablePair<Integer, String>) pair).right

        pair2 instanceof ImmutablePair<?, ?>
        !((ImmutablePair<Object, String>) pair2).left
        "bar" == ((ImmutablePair<Object, String>) pair2).right

        !pair3.getLeft()
        !pair3.getRight()
    }

    def "testToString"() {
        given:
        Pair<String, String> pair = Pair.of("Key", "Value")
        expect:
        assertEquals("(Key,Value)", pair.toString())
    }

    def "testToStringCustom"() {
        given:
        Calendar date = Calendar.getInstance();
        date.set(2011, Calendar.APRIL, 25);
        Pair<String, Calendar> pair = Pair.of("DOB", date);

        expect:
        "Test created on " + "04-25-2011" == pair.toString('Test created on %2$tm-%2$td-%2$tY')
    }

}
