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

@Title("org.apache.commons.lang3.tuple.TripleTest")
class TripleSpec extends AbstractLangSpec {

    def "testOfNonNull"() {
        given:
        Triple<String, String, String> pair = Triple.ofNonNull("x", "y", "z")

        when:
        Triple.ofNonNull(null, null, null)
        then:
        thrown(NullPointerException)

        when:
        Triple.ofNonNull(null, null, "z")
        then:
        thrown(NullPointerException)

        when:
        Triple.ofNonNull(null, "y", "z")
        then:
        thrown(NullPointerException)

        when:
        Triple.ofNonNull("x", null, null)
        then:
        thrown(NullPointerException)

        when:
        Triple.ofNonNull("x", "y", null)
        then:
        thrown(NullPointerException)

        "x" == pair.getLeft()
        "y" == pair.getMiddle()
        "z" == pair.getRight()
    }

    def "testComparable1"() {
        given:
        Triple<String, String, String> triple1 = Triple.of("A", "D", "A")
        Triple<String, String, String> triple2 = Triple.of("B", "C", "A")
        expect:
        0 == (triple1 <=> triple1)
        0 == (triple2 <=> triple2)
        triple1 < triple2
        triple2 > triple1
    }

    def "testComparable2"() {
        given:
        Triple<String, String, String> triple1 = Triple.of("A", "C", "B")
        Triple<String, String, String> triple2 = Triple.of("A", "D", "B")
        expect:
        0 == (triple1 <=> triple1)
        0 == (triple2 <=> triple2)
        triple1 < triple2
        triple2 > triple1
    }

    def "testComparable3"() {
        given:
        Triple<String, String, String> triple1 = Triple.of("A", "A", "D")
        Triple<String, String, String> triple2 = Triple.of("A", "B", "C")
        expect:
        0 == (triple1 <=> triple1)
        0 == (triple2 <=> triple2)
        triple1 < triple2
        triple2 > triple1
    }

    def "testComparable4"() {
        given:
        Triple<String, String, String> triple1 = Triple.of("B", "A", "C")
        Triple<String, String, String> triple2 = Triple.of("B", "A", "D")
        expect:
        0 == (triple1 <=> triple1)
        0 == (triple2 <=> triple2)
        triple1 < triple2
        triple2 > triple1
    }

    def "testCompatibilityBetweenTriples"() {
        given:
        Triple<Integer, String, Boolean> triple = ImmutableTriple.of(0, "foo", Boolean.TRUE)
        Triple<Integer, String, Boolean> triple2 = MutableTriple.of(0, "foo", Boolean.TRUE)
        HashSet<Triple<Integer, String, Boolean>> set = new HashSet<>()
        set.add(triple)
        expect:
        triple == triple2
        triple.hashCode() == triple2.hashCode()
        set.contains(triple2)
    }

    def "testEmptyArrayGenerics"() {
        given:
        Triple<Integer, String, Boolean>[] empty = Triple.emptyArray()
        expect:
        0 == empty.length
    }

    def "testEmptyArrayLength"() {
        given:
        @SuppressWarnings("unchecked")
        Triple<Integer, String, Boolean>[] empty = (Triple<Integer, String, Boolean>[]) Triple.EMPTY_ARRAY
        expect:
        0 == empty.length
    }

    def "testFormattable_padded"() {
        given:
        Triple<String, String, String> triple = Triple.of("Key", "Something", "Value")
        expect:
        "         (Key,Something,Value)" == String.format('%1$30s', triple)
    }

    def "testFormattable_simple"() {
        given:
        Triple<String, String, String> triple = Triple.of("Key", "Something", "Value")
        expect:
        "(Key,Something,Value)" == String.format('%1$s', triple)
    }

    def "testToString"() {
        given:
        Triple<String, String, String> triple = Triple.of("Key", "Something", "Value")
        expect:
        "(Key,Something,Value)" == triple.toString()
    }

    def "testToStringCustom"() {
        given:
        Calendar date = Calendar.getInstance()
        when:
        date.set(2011, Calendar.APRIL, 25)
        Triple<String, String, Calendar> triple = Triple.of("DOB", "string", date)
        then:
        "Test created on " + "04-25-2011" == triple.toString('Test created on %3$tm-%3$td-%3$tY')
    }

    def "testTripleOf"() {
        given:
        Triple<Integer, String, Boolean> triple = Triple.of(0, "foo", Boolean.TRUE)
        Triple<Object, String, Long> triple2 = Triple.of(null, "bar", Long.valueOf(200L))
        expect:
        triple instanceof ImmutableTriple<?, ?, ?>
        0 == ((ImmutableTriple<Integer, String, Boolean>) triple).left.intValue()
        "foo" == ((ImmutableTriple<Integer, String, Boolean>) triple).middle
        Boolean.TRUE == ((ImmutableTriple<Integer, String, Boolean>) triple).right
        triple2 instanceof ImmutableTriple<?, ?, ?>
        !((ImmutableTriple<Object, String, Long>) triple2).left
        "bar" == ((ImmutableTriple<Object, String, Long>) triple2).middle
        Long.valueOf(200L) == ((ImmutableTriple<Object, String, Long>) triple2).right
    }
}
