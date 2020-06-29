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
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.Warmup;

import java.util.concurrent.TimeUnit;

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.SPACE;
import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.apache.commons.lang3.StringUtils.length;

@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgs = {"-server", "-Xms512M", "-Xmx512M"})
public class StringUtilsNormalizeSpaceTest {

    private static final String string0 = "  ad assd qawe e sdafg dwsa fwef wef wewef f weffwea wefwewe fw fwe we we " +
            "  fwewe wa wea ffewaf weaf wef    wefad asdas ad asd asd as asd qw dwqasdas qw qw dqaw qdw qwd qw wqd " +
            "qwdwqd qwd q  w dqwq   w aadwqd qwd qwd qw dwq dw  qd qw dqw dw qdw qd qw dw qd qw dwq ddwaaw ad wa dwa " +
            "dw ad wa fwa faw w  a   dqawv  daw dawdawd aw dw af aw      fw a w3e tr tgyer g e h e he t we 32  gr wt " +
            "4 t5g 23t";

    @Benchmark
    public String testOld() {
        return normalizeSpaceOld(string0);
    }

    @Benchmark
    public String testNew() {
        return normalizeSpaceNew(string0);
    }

    @Benchmark
    public String testNew2() {
        return normalizeSpaceNew2(string0);
    }

    //----------

    public static String normalizeSpaceOld(final String str) {
        // LANG-1020: Improved performance significantly by normalizing manually instead of using regex
        // See https://github.com/librucha/commons-lang-normalizespaces-benchmark for performance test
        final int size = length(str);
        if (size == 0) {
            return str;
        }
        final char[] newChars = new char[size];
        int count = 0;
        int whitespacesCount = 0;
        boolean startWhitespaces = true;
        for (int i = 0; i < size; i++) {
            final char actualChar = str.charAt(i);
            final boolean isWhitespace = Character.isWhitespace(actualChar);
            if (isWhitespace) {
                if (whitespacesCount == 0 && !startWhitespaces) {
                    newChars[count++] = SPACE.charAt(0);
                }
                whitespacesCount++;
            } else {
                startWhitespaces = false;
                newChars[count++] = (actualChar == 160 ? 32 : actualChar);
                whitespacesCount = 0;
            }
        }
        if (startWhitespaces) {
            return EMPTY;
        }
        return new String(newChars, 0, count - (whitespacesCount > 0 ? 1 : 0)).trim();
    }

    public static String normalizeSpaceNew(final String str) {
        // LANG-1020: Improved performance significantly by normalizing manually instead of using regex
        // See https://github.com/librucha/commons-lang-normalizespaces-benchmark for performance test
        if (isEmpty(str)) {
            return str;
        }
        final int size = str.length();
        StringBuilder stringBuilder = new StringBuilder(size);
        boolean lastIsWhiteSpace = true;
        for (int i = 0; i < size; i++) {
            final char nowChar = str.charAt(i);
            if (Character.isWhitespace(nowChar)) {
                if (!lastIsWhiteSpace) {
                    stringBuilder.append(' ');
                    lastIsWhiteSpace = true;
                }
            } else {
                stringBuilder.append(nowChar == 160 ? (char) 32 : nowChar);
                lastIsWhiteSpace = false;
            }
        }
        final int stringBuilderLength = stringBuilder.length();
        if (stringBuilderLength == 0) {
            return EMPTY;
        }
        final int lastIndex = stringBuilderLength - 1;
        if (stringBuilder.charAt(lastIndex) == ' ') {
            if (lastIndex == 0) {
                return EMPTY;
            }
            stringBuilder.setLength(lastIndex);
        }
        return stringBuilder.toString();
    }

    public static String normalizeSpaceNew2(final String str) {
        // LANG-1020: Improved performance significantly by normalizing manually instead of using regex
        // See https://github.com/librucha/commons-lang-normalizespaces-benchmark for performance test
        final int size = length(str);
        if (size == 0) {
            return str;
        }
        final char[] newChars = new char[size];
        int count = 0;
        boolean lastIsWhiteSpace = true;
        int i = 0;
        for (; i < size; i++) {
            final char nowChar = str.charAt(i);
            if (!(nowChar <= ' ' || Character.isWhitespace(nowChar))) {
                break;
            }
        }
        for (; i < size; i++) {
            final char nowChar = str.charAt(i);
            if (Character.isWhitespace(nowChar)) {
                if (!lastIsWhiteSpace) {
                    newChars[count++] = ' ';
                    lastIsWhiteSpace = true;
                }
            } else {
                newChars[count++] = (nowChar == 160 ? (char) 32 : nowChar);
                lastIsWhiteSpace = false;
            }
        }
        while (count > 0 && newChars[count - 1] <= ' ') {
            --count;
        }
        if (count == 0) {
            return EMPTY;
        }
        return new String(newChars, 0, count);
    }
}
