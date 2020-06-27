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

import java.nio.CharBuffer;
import java.util.concurrent.TimeUnit;

/**
 * Test to show whether using BitSet for removeAll() methods is faster than using HashSet.
 */
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
public class CharSequenceUtilsToCharArrayTest {
    static class TestData {
        final String source;

        TestData(final String source) {
            this.source = source;
        }

        @Override
        public String toString() {
            return "test-source:[" + source + "]";
        }
    }

    TestData[] TEST_DATA;


    @Setup
    public void prepare() {
        final String STRING0 = String0(20);
        final String STRING1 = String1(20);
        TEST_DATA = new TestData[]{
                new TestData(STRING0),
                new TestData(STRING1),

                //len 1
                new TestData("a"),

                //len 0
                new TestData(""),

                //len 256
                new TestData(
                        "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                ),

                //len 8
                new TestData(
                        "aaaabbbb"
                ),

                //len 16
                new TestData(
                        "aaaabbbbccccdddd"
                ),

                //len 32
                new TestData(
                        "aaaabbbbccccddddaaaabbbbccccdddd"
                ),
        };
    }

    //0 Old

    @Benchmark
    public void test0StringBufferOld() {
        toCharArrayOld(new StringBuffer(TEST_DATA[0].source));
    }

    @Benchmark
    public void test0StringBuilderOld() {
        toCharArrayOld(new StringBuilder(TEST_DATA[0].source));
    }

    @Benchmark
    public void test0CharBufferOld() {
        toCharArrayOld(CharBuffer.wrap(TEST_DATA[0].source));
    }

    @Benchmark
    public void test0WrapperStringOld() {
        toCharArrayOld(new CharSequenceUtilsTest.WrapperString(TEST_DATA[0].source));
    }

    //1 Old

    @Benchmark
    public void test1StringBufferOld() {
        toCharArrayOld(new StringBuffer(TEST_DATA[1].source));
    }

    @Benchmark
    public void test1StringBuilderOld() {
        toCharArrayOld(new StringBuilder(TEST_DATA[1].source));
    }

    @Benchmark
    public void test1CharBufferOld() {
        toCharArrayOld(CharBuffer.wrap(TEST_DATA[1].source));
    }

    @Benchmark
    public void test1WrapperStringOld() {
        toCharArrayOld(new CharSequenceUtilsTest.WrapperString(TEST_DATA[1].source));
    }

    //2 Old

    @Benchmark
    public void test2StringBufferOld() {
        toCharArrayOld(new StringBuffer(TEST_DATA[2].source));
    }

    @Benchmark
    public void test2StringBuilderOld() {
        toCharArrayOld(new StringBuilder(TEST_DATA[2].source));
    }

    @Benchmark
    public void test2CharBufferOld() {
        toCharArrayOld(CharBuffer.wrap(TEST_DATA[2].source));
    }

    @Benchmark
    public void test2WrapperStringOld() {
        toCharArrayOld(new CharSequenceUtilsTest.WrapperString(TEST_DATA[2].source));
    }

    //3 Old

    @Benchmark
    public void test3StringBufferOld() {
        toCharArrayOld(new StringBuffer(TEST_DATA[3].source));
    }

    @Benchmark
    public void test3StringBuilderOld() {
        toCharArrayOld(new StringBuilder(TEST_DATA[3].source));
    }

    @Benchmark
    public void test3CharBufferOld() {
        toCharArrayOld(CharBuffer.wrap(TEST_DATA[3].source));
    }

    @Benchmark
    public void test3WrapperStringOld() {
        toCharArrayOld(new CharSequenceUtilsTest.WrapperString(TEST_DATA[3].source));
    }

    //4 Old

    @Benchmark
    public void test4StringBufferOld() {
        toCharArrayOld(new StringBuffer(TEST_DATA[4].source));
    }

    @Benchmark
    public void test4StringBuilderOld() {
        toCharArrayOld(new StringBuilder(TEST_DATA[4].source));
    }

    @Benchmark
    public void test4CharBufferOld() {
        toCharArrayOld(CharBuffer.wrap(TEST_DATA[4].source));
    }

    @Benchmark
    public void test4WrapperStringOld() {
        toCharArrayOld(new CharSequenceUtilsTest.WrapperString(TEST_DATA[4].source));
    }

    //5 Old

    @Benchmark
    public void test5StringBufferOld() {
        toCharArrayOld(new StringBuffer(TEST_DATA[5].source));
    }

    @Benchmark
    public void test5StringBuilderOld() {
        toCharArrayOld(new StringBuilder(TEST_DATA[5].source));
    }

    @Benchmark
    public void test5CharBufferOld() {
        toCharArrayOld(CharBuffer.wrap(TEST_DATA[5].source));
    }

    @Benchmark
    public void test5WrapperStringOld() {
        toCharArrayOld(new CharSequenceUtilsTest.WrapperString(TEST_DATA[5].source));
    }

    //6 Old

    @Benchmark
    public void test6StringBufferOld() {
        toCharArrayOld(new StringBuffer(TEST_DATA[6].source));
    }

    @Benchmark
    public void test6StringBuilderOld() {
        toCharArrayOld(new StringBuilder(TEST_DATA[6].source));
    }

    @Benchmark
    public void test6CharBufferOld() {
        toCharArrayOld(CharBuffer.wrap(TEST_DATA[6].source));
    }

    @Benchmark
    public void test6WrapperStringOld() {
        toCharArrayOld(new CharSequenceUtilsTest.WrapperString(TEST_DATA[6].source));
    }

    //7 Old

    @Benchmark
    public void test7StringBufferOld() {
        toCharArrayOld(new StringBuffer(TEST_DATA[7].source));
    }

    @Benchmark
    public void test7StringBuilderOld() {
        toCharArrayOld(new StringBuilder(TEST_DATA[7].source));
    }

    @Benchmark
    public void test7CharBufferOld() {
        toCharArrayOld(CharBuffer.wrap(TEST_DATA[7].source));
    }

    @Benchmark
    public void test7WrapperStringOld() {
        toCharArrayOld(new CharSequenceUtilsTest.WrapperString(TEST_DATA[7].source));
    }

    //0 New

    @Benchmark
    public void test0StringBufferNew() {
        toCharArrayNew(new StringBuffer(TEST_DATA[0].source));
    }

    @Benchmark
    public void test0StringBuilderNew() {
        toCharArrayNew(new StringBuilder(TEST_DATA[0].source));
    }

    @Benchmark
    public void test0CharBufferNew() {
        toCharArrayNew(CharBuffer.wrap(TEST_DATA[0].source));
    }

    @Benchmark
    public void test0WrapperStringNew() {
        toCharArrayNew(new CharSequenceUtilsTest.WrapperString(TEST_DATA[0].source));
    }

    //1 New

    @Benchmark
    public void test1StringBufferNew() {
        toCharArrayNew(new StringBuffer(TEST_DATA[1].source));
    }

    @Benchmark
    public void test1StringBuilderNew() {
        toCharArrayNew(new StringBuilder(TEST_DATA[1].source));
    }

    @Benchmark
    public void test1CharBufferNew() {
        toCharArrayNew(CharBuffer.wrap(TEST_DATA[1].source));
    }

    @Benchmark
    public void test1WrapperStringNew() {
        toCharArrayNew(new CharSequenceUtilsTest.WrapperString(TEST_DATA[1].source));
    }

    //2 New

    @Benchmark
    public void test2StringBufferNew() {
        toCharArrayNew(new StringBuffer(TEST_DATA[2].source));
    }

    @Benchmark
    public void test2StringBuilderNew() {
        toCharArrayNew(new StringBuilder(TEST_DATA[2].source));
    }

    @Benchmark
    public void test2CharBufferNew() {
        toCharArrayNew(CharBuffer.wrap(TEST_DATA[2].source));
    }

    @Benchmark
    public void test2WrapperStringNew() {
        toCharArrayNew(new CharSequenceUtilsTest.WrapperString(TEST_DATA[2].source));
    }

    //3 New

    @Benchmark
    public void test3StringBufferNew() {
        toCharArrayNew(new StringBuffer(TEST_DATA[3].source));
    }

    @Benchmark
    public void test3StringBuilderNew() {
        toCharArrayNew(new StringBuilder(TEST_DATA[3].source));
    }

    @Benchmark
    public void test3CharBufferNew() {
        toCharArrayNew(CharBuffer.wrap(TEST_DATA[3].source));
    }

    @Benchmark
    public void test3WrapperStringNew() {
        toCharArrayNew(new CharSequenceUtilsTest.WrapperString(TEST_DATA[3].source));
    }

    //4 New

    @Benchmark
    public void test4StringBufferNew() {
        toCharArrayNew(new StringBuffer(TEST_DATA[4].source));
    }

    @Benchmark
    public void test4StringBuilderNew() {
        toCharArrayNew(new StringBuilder(TEST_DATA[4].source));
    }

    @Benchmark
    public void test4CharBufferNew() {
        toCharArrayNew(CharBuffer.wrap(TEST_DATA[4].source));
    }

    @Benchmark
    public void test4WrapperStringNew() {
        toCharArrayNew(new CharSequenceUtilsTest.WrapperString(TEST_DATA[4].source));
    }

    //5 New

    @Benchmark
    public void test5StringBufferNew() {
        toCharArrayNew(new StringBuffer(TEST_DATA[5].source));
    }

    @Benchmark
    public void test5StringBuilderNew() {
        toCharArrayNew(new StringBuilder(TEST_DATA[5].source));
    }

    @Benchmark
    public void test5CharBufferNew() {
        toCharArrayNew(CharBuffer.wrap(TEST_DATA[5].source));
    }

    @Benchmark
    public void test5WrapperStringNew() {
        toCharArrayNew(new CharSequenceUtilsTest.WrapperString(TEST_DATA[5].source));
    }

    //6 New

    @Benchmark
    public void test6StringBufferNew() {
        toCharArrayNew(new StringBuffer(TEST_DATA[6].source));
    }

    @Benchmark
    public void test6StringBuilderNew() {
        toCharArrayNew(new StringBuilder(TEST_DATA[6].source));
    }

    @Benchmark
    public void test6CharBufferNew() {
        toCharArrayNew(CharBuffer.wrap(TEST_DATA[6].source));
    }

    @Benchmark
    public void test6WrapperStringNew() {
        toCharArrayNew(new CharSequenceUtilsTest.WrapperString(TEST_DATA[6].source));
    }

    //7 New

    @Benchmark
    public void test7StringBufferNew() {
        toCharArrayNew(new StringBuffer(TEST_DATA[7].source));
    }

    @Benchmark
    public void test7StringBuilderNew() {
        toCharArrayNew(new StringBuilder(TEST_DATA[7].source));
    }

    @Benchmark
    public void test7CharBufferNew() {
        toCharArrayNew(CharBuffer.wrap(TEST_DATA[7].source));
    }

    @Benchmark
    public void test7WrapperStringNew() {
        toCharArrayNew(new CharSequenceUtilsTest.WrapperString(TEST_DATA[7].source));
    }

    //----------

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
     * Green implementation of toCharArray.
     *
     * @param cs the {@code CharSequence} to be processed
     * @return the resulting char array
     * @since 3.11
     */
    public static char[] toCharArrayOld(final CharSequence cs) {
        if (cs instanceof String) {
            return ((String) cs).toCharArray();
        }
        final int sz = cs.length();
        final char[] array = new char[cs.length()];
        for (int i = 0; i < sz; i++) {
            array[i] = cs.charAt(i);
        }
        return array;
    }

    //----------

    /**
     * Green implementation of toCharArray.
     *
     * @param cs the {@code CharSequence} to be processed
     * @return the resulting char array
     * @since 3.11
     */
    public static char[] toCharArrayNew(final CharSequence cs) {
        return cs.toString().toCharArray();
    }
}
