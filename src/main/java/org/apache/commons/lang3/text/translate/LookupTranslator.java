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
package org.apache.commons.lang3.text.translate;

import java.io.IOException;
import java.io.Writer;
import java.util.HashMap;
import java.util.HashSet;

import org.checkerframework.common.value.qual.MinLen;
import org.checkerframework.common.value.qual.ArrayLen;
import org.checkerframework.checker.index.qual.IndexFor;
import org.checkerframework.checker.index.qual.NonNegative;
import org.checkerframework.checker.index.qual.LTEqLengthOf;
import org.checkerframework.checker.index.qual.LTLengthOf;

/**
 * Translates a value using a lookup table.
 *
 * @since 3.0
 * @deprecated as of 3.6, use commons-text
 * <a href="https://commons.apache.org/proper/commons-text/javadocs/api-release/org/apache/commons/text/translate/LookupTranslator.html">
 * LookupTranslator</a> instead
 */
@Deprecated
public class LookupTranslator extends CharSequenceTranslator {

    private final HashMap<String, String> lookupMap;
    private final HashSet<Character> prefixSet;
    private final @NonNegative int shortest;
    private final @NonNegative int longest;

    /**
     * Define the lookup table to be used in translation
     *
     * Note that, as of Lang 3.1, the key to the lookup table is converted to a
     * java.lang.String. This is because we need the key to support hashCode and
     * equals(Object), allowing it to be the key for a HashMap. See LANG-882.
     *
     * @param lookup CharSequence[][] table of size [*][2]
     */
    @SuppressWarnings({"value:enhancedfor.type.incompatible","index:argument.type.incompatible"}) /*
    #1: CharSequence is of the type [*][2] (according to the documentation), hence Charsequence has length 2
    #2: Minimum 1 row ensures seq[0].charAt(0) to be valid
    */
    public LookupTranslator(final CharSequence @MinLen(1) []... lookup) {
        lookupMap = new HashMap<>();
        prefixSet = new HashSet<>();
        int _shortest = Integer.MAX_VALUE;
        int _longest = 0;
        if (lookup != null) {
            for (final CharSequence @ArrayLen(2) [] seq : lookup) { // #1
                this.lookupMap.put(seq[0].toString(), seq[1].toString());
                this.prefixSet.add(seq[0].charAt(0)); // #2
                final int sz = seq[0].length();
                if (sz < _shortest) {
                    _shortest = sz;
                }
                if (sz > _longest) {
                    _longest = sz;
                }
            }
        }
        shortest = _shortest;
        longest = _longest;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int translate(final CharSequence input, final @IndexFor("#1") int index, final Writer out) throws IOException {
        // check if translation exists for the input at position index
        if (prefixSet.contains(input.charAt(index))) {
            int max = longest;
            if (index + longest > input.length()) {
                max = input.length() - index;
            }
            // implement greedy algorithm by trying maximum match first
            for (@SuppressWarnings("index:assignment.type.incompatible") @LTLengthOf(value = {"input"}, offset = {"index-1"}) int i = max; i >= shortest; i--) { // if index + longest > input.length(), max = input.length() - index and i goes from max till shortest. Else max < input.length() - index and the annotation meaning is retained
                final CharSequence subSeq = input.subSequence(index, index + i);
                final String result = lookupMap.get(subSeq.toString());

                if (result != null) {
                    out.write(result);
                    return i;
                }
            }
        }
        return 0;
    }
}
