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

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;

import java.util.concurrent.TimeUnit;

/**
 * Test to show whether using BitSet for removeAll() methods is faster than using HashSet.
 */
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
public class CharSequenceUtilsRegionMatchesTest {
    static class TestData {
        final String source;
        final boolean ignoreCase;
        final int toffset;
        final String other;
        final int ooffset;
        final int len;
        final boolean expected;
        final Class<? extends Throwable> throwable;

        TestData(final String source, final boolean ignoreCase, final int toffset,
                 final String other, final int ooffset, final int len, final boolean expected) {
            this.source = source;
            this.ignoreCase = ignoreCase;
            this.toffset = toffset;
            this.other = other;
            this.ooffset = ooffset;
            this.len = len;
            this.expected = expected;
            this.throwable = null;
        }

        TestData(final String source, final boolean ignoreCase, final int toffset,
                 final String other, final int ooffset, final int len, final Class<? extends Throwable> throwable) {
            this.source = source;
            this.ignoreCase = ignoreCase;
            this.toffset = toffset;
            this.other = other;
            this.ooffset = ooffset;
            this.len = len;
            this.expected = false;
            this.throwable = throwable;
        }

        @Override
        public String toString() {
            final StringBuilder sb = new StringBuilder();
            sb.append(source).append("[").append(toffset).append("]");
            sb.append(ignoreCase ? " caseblind " : " samecase ");
            sb.append(other).append("[").append(ooffset).append("]");
            sb.append(" ").append(len).append(" => ");
            if (throwable != null) {
                sb.append(throwable);
            } else {
                sb.append(expected);
            }
            return sb.toString();
        }
    }

    TestData[] TEST_DATA;


    @Setup
    public void prepare() {
        final String STRING0 = String0(20);
        final String STRING1 = String1(20);
        TEST_DATA = new TestData[]{
                new TestData(STRING0, false, 0, STRING0, 0, STRING0.length(), true),
                new TestData(STRING0, true, 0, STRING0, 0, STRING0.length(), true),
                new TestData(STRING1, true, 0, STRING0, 0, STRING0.length(), true),
        };
    }


    @Benchmark
    public void test0New() {
        TestData data = TEST_DATA[0];
        regionMatchesNew(
                new CharSequenceUtilsTest.WrapperString(data.source),
                data.ignoreCase,
                data.toffset,
                new CharSequenceUtilsTest.WrapperString(data.other),
                data.ooffset,
                data.len
        );
    }

    @Benchmark
    public void test1New() {
        TestData data = TEST_DATA[1];
        regionMatchesNew(
                new CharSequenceUtilsTest.WrapperString(data.source),
                data.ignoreCase,
                data.toffset,
                new CharSequenceUtilsTest.WrapperString(data.other),
                data.ooffset,
                data.len
        );
    }

    @Benchmark
    public void test2New() {
        TestData data = TEST_DATA[2];
        regionMatchesNew(
                new CharSequenceUtilsTest.WrapperString(data.source),
                data.ignoreCase,
                data.toffset,
                new CharSequenceUtilsTest.WrapperString(data.other),
                data.ooffset,
                data.len
        );
    }

    @Benchmark
    public void test0NewInline() {
        TestData data = TEST_DATA[0];
        regionMatchesNewInline(
                new CharSequenceUtilsTest.WrapperString(data.source),
                data.ignoreCase,
                data.toffset,
                new CharSequenceUtilsTest.WrapperString(data.other),
                data.ooffset,
                data.len
        );
    }

    @Benchmark
    public void test1NewInline() {
        TestData data = TEST_DATA[1];
        regionMatchesNewInline(
                new CharSequenceUtilsTest.WrapperString(data.source),
                data.ignoreCase,
                data.toffset,
                new CharSequenceUtilsTest.WrapperString(data.other),
                data.ooffset,
                data.len
        );
    }

    @Benchmark
    public void test2NewInline() {
        TestData data = TEST_DATA[2];
        regionMatchesNewInline(
                new CharSequenceUtilsTest.WrapperString(data.source),
                data.ignoreCase,
                data.toffset,
                new CharSequenceUtilsTest.WrapperString(data.other),
                data.ooffset,
                data.len
        );
    }

    @Benchmark
    public void test0Old() {
        TestData data = TEST_DATA[0];
        regionMatchesOld(
                new CharSequenceUtilsTest.WrapperString(data.source),
                data.ignoreCase,
                data.toffset,
                new CharSequenceUtilsTest.WrapperString(data.other),
                data.ooffset,
                data.len
        );
    }

    @Benchmark
    public void test1Old() {
        TestData data = TEST_DATA[1];
        regionMatchesOld(
                new CharSequenceUtilsTest.WrapperString(data.source),
                data.ignoreCase,
                data.toffset,
                new CharSequenceUtilsTest.WrapperString(data.other),
                data.ooffset,
                data.len
        );
    }

    @Benchmark
    public void test2Old() {
        TestData data = TEST_DATA[2];
        regionMatchesOld(
                new CharSequenceUtilsTest.WrapperString(data.source),
                data.ignoreCase,
                data.toffset,
                new CharSequenceUtilsTest.WrapperString(data.other),
                data.ooffset,
                data.len
        );
    }


    public static String String0(int loop) {
        StringBuilder stringBuilder = new StringBuilder("ab");
        for (int i = 0; i < loop; i++) {
            stringBuilder.append(stringBuilder);
        }
        return stringBuilder.toString();
    }

    public static String String1(int loop) {
        StringBuilder stringBuilder = new StringBuilder("AB");
        for (int i = 0; i < loop; i++) {
            stringBuilder.append(stringBuilder);
        }
        return stringBuilder.toString();
    }

    //----------

    /**
     * Old version of CharSequenceUtils.regionMatches
     *
     * Green implementation of regionMatches.
     *
     * @param cs the {@code CharSequence} to be processed
     * @param ignoreCase whether or not to be case insensitive
     * @param thisStart the index to start on the {@code cs} CharSequence
     * @param substring the {@code CharSequence} to be looked for
     * @param start the index to start on the {@code substring} CharSequence
     * @param length character length of the region
     * @return whether the region matched
     */
    static boolean regionMatchesOld(final CharSequence cs, final boolean ignoreCase, final int thisStart,
                                    final CharSequence substring, final int start, final int length) {
        if (cs instanceof String && substring instanceof String) {
            return ((String) cs).regionMatches(ignoreCase, thisStart, (String) substring, start, length);
        }
        int index1 = thisStart;
        int index2 = start;
        int tmpLen = length;

        // Extract these first so we detect NPEs the same as the java.lang.String version
        final int srcLen = cs.length() - thisStart;
        final int otherLen = substring.length() - start;

        // Check for invalid parameters
        if (thisStart < 0 || start < 0 || length < 0) {
            return false;
        }

        // Check that the regions are long enough
        if (srcLen < length || otherLen < length) {
            return false;
        }

        while (tmpLen-- > 0) {
            final char c1 = cs.charAt(index1++);
            final char c2 = substring.charAt(index2++);

            if (c1 == c2) {
                continue;
            }

            if (!ignoreCase) {
                return false;
            }

            // The real same check as in String.regionMatches():
            char u1 = Character.toUpperCase(c1);
            char u2 = Character.toUpperCase(c2);
            if (u1 != u2 && Character.toLowerCase(u1) != Character.toLowerCase(u2)) {
                return false;
            }
        }

        return true;
    }

    //----------

    /**
     * Green implementation of regionMatches.
     *
     * @param cs the {@code CharSequence} to be processed
     * @param ignoreCase whether or not to be case insensitive
     * @param thisStart the index to start on the {@code cs} CharSequence
     * @param substring the {@code CharSequence} to be looked for
     * @param start the index to start on the {@code substring} CharSequence
     * @param length character length of the region
     * @return whether the region matched
     */
    static boolean regionMatchesNew(final CharSequence cs, final boolean ignoreCase, final int thisStart,
                                    final CharSequence substring, final int start, final int length) {
        if (!ignoreCase) {
            return regionMatches(cs, thisStart, substring, start, length);
        } else {
            return regionMatchesIgnoreCase(cs, thisStart, substring, start, length);
        }
    }

    /**
     * Green implementation of regionMatches.
     *
     * @param cs the {@code CharSequence} to be processed
     * @param thisStart the index to start on the {@code cs} CharSequence
     * @param substring the {@code CharSequence} to be looked for
     * @param start the index to start on the {@code substring} CharSequence
     * @param length character length of the region
     * @return whether the region matched
     */
    static boolean regionMatches(final CharSequence cs, final int thisStart,
                                 final CharSequence substring, final int start, final int length) {
        if (cs instanceof String && substring instanceof String) {
            return ((String) cs).regionMatches(thisStart, (String) substring, start, length);
        }
        int index1 = thisStart;
        int index2 = start;
        int tmpLen = length;

        // Extract these first so we detect NPEs the same as the java.lang.String version
        final int srcLen = cs.length() - thisStart;
        final int otherLen = substring.length() - start;

        // Check for invalid parameters
        if (thisStart < 0 || start < 0 || length < 0) {
            return false;
        }

        // Check that the regions are long enough
        if (srcLen < length || otherLen < length) {
            return false;
        }

        while (tmpLen-- > 0) {
            if (cs.charAt(index1++) != substring.charAt(index2++)) {
                return false;
            }
        }

        return true;
    }

    /**
     * Green implementation of regionMatches.
     *
     * @param cs the {@code CharSequence} to be processed
     * @param thisStart the index to start on the {@code cs} CharSequence
     * @param substring the {@code CharSequence} to be looked for
     * @param start the index to start on the {@code substring} CharSequence
     * @param length character length of the region
     * @return whether the region matched
     */
    static boolean regionMatchesIgnoreCase(final CharSequence cs, final int thisStart,
                                           final CharSequence substring, final int start, final int length) {
        if (cs instanceof String && substring instanceof String) {
            return ((String) cs).regionMatches(true, thisStart, (String) substring, start, length);
        }
        int index1 = thisStart;
        int index2 = start;
        int tmpLen = length;

        // Extract these first so we detect NPEs the same as the java.lang.String version
        final int srcLen = cs.length() - thisStart;
        final int otherLen = substring.length() - start;

        // Check for invalid parameters
        if (thisStart < 0 || start < 0 || length < 0) {
            return false;
        }

        // Check that the regions are long enough
        if (srcLen < length || otherLen < length) {
            return false;
        }

        while (tmpLen-- > 0) {
            final char c1 = cs.charAt(index1++);
            final char c2 = substring.charAt(index2++);

            if (c1 == c2) {
                continue;
            }

            // The real same check as in String.regionMatches():
            char u1 = Character.toUpperCase(c1);
            char u2 = Character.toUpperCase(c2);
            if (u1 != u2 && Character.toLowerCase(u1) != Character.toLowerCase(u2)) {
                return false;
            }
        }

        return true;
    }

    //----------

    /**
     * Green implementation of regionMatches.
     *
     * @param cs the {@code CharSequence} to be processed
     * @param ignoreCase whether or not to be case insensitive
     * @param thisStart the index to start on the {@code cs} CharSequence
     * @param substring the {@code CharSequence} to be looked for
     * @param start the index to start on the {@code substring} CharSequence
     * @param length character length of the region
     * @return whether the region matched
     */
    static boolean regionMatchesNewInline(final CharSequence cs, final boolean ignoreCase, final int thisStart,
                                          final CharSequence substring, final int start, final int length) {
        if (cs instanceof String && substring instanceof String) {
            return ((String) cs).regionMatches(true, thisStart, (String) substring, start, length);
        }
        int index1 = thisStart;
        int index2 = start;
        int tmpLen = length;

        // Extract these first so we detect NPEs the same as the java.lang.String version
        final int srcLen = cs.length() - thisStart;
        final int otherLen = substring.length() - start;

        // Check for invalid parameters
        if (thisStart < 0 || start < 0 || length < 0) {
            return false;
        }

        // Check that the regions are long enough
        if (srcLen < length || otherLen < length) {
            return false;
        }
        if (!ignoreCase) {
            while (tmpLen-- > 0) {
                if (cs.charAt(index1++) != substring.charAt(index2++)) {
                    return false;
                }
            }

            return true;
        } else {
            while (tmpLen-- > 0) {
                final char c1 = cs.charAt(index1++);
                final char c2 = substring.charAt(index2++);

                if (c1 == c2) {
                    continue;
                }

                // The real same check as in String.regionMatches():
                char u1 = Character.toUpperCase(c1);
                char u2 = Character.toUpperCase(c2);
                if (u1 != u2 && Character.toLowerCase(u1) != Character.toLowerCase(u2)) {
                    return false;
                }
            }

            return true;
        }
    }
}
