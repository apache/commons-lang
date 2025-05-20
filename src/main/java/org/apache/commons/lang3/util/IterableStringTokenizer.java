/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.commons.lang3.util;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.StringTokenizer;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import org.apache.commons.lang3.ArrayUtils;

/**
 * A {@link StringTokenizer} that implements {@link Iterable}{@code <String>} and conversion methods {@link #toList()} and {@link #toStream()}.
 *
 * @since 3.18.0
 */
public class IterableStringTokenizer extends StringTokenizer implements Iterable<String> {

    /**
     * Constructs a new instance like {@link StringTokenizer#StringTokenizer(String, String, boolean)}.
     *
     * @param str a string to be parsed.
     * @exception NullPointerException if str is {@code null}.
     */
    public IterableStringTokenizer(final String str) {
        super(str);
    }

    /**
     * Constructs a new instance like {@link StringTokenizer#StringTokenizer(String, String, boolean)}.
     *
     * @param str   a string to be parsed.
     * @param delim the delimiters.
     * @exception NullPointerException if str is {@code null}.
     */
    public IterableStringTokenizer(final String str, final String delim) {
        super(str, delim);
    }

    /**
     * Constructs a new instance like {@link StringTokenizer#StringTokenizer(String, String, boolean)}.
     *
     * @param str          a string to be parsed.
     * @param delim        the delimiters.
     * @param returnDelims flag indicating whether to return the delimiters as tokens.
     * @exception NullPointerException if str is {@code null}.
     */
    public IterableStringTokenizer(final String str, final String delim, final boolean returnDelims) {
        super(str, delim, returnDelims);
    }

    @Override
    public Iterator<String> iterator() {
        return new Iterator<String>() {

            @Override
            public boolean hasNext() {
                return hasMoreElements();
            }

            @Override
            public String next() {
                return Objects.toString(nextElement(), null);
            }
        };
    }

    /**
     * Returns a new {@code String[]} containing the tokenizer elements.
     *
     * @return a new {@code String[]}.
     */
    public String[] toArray() {
        return toList().toArray(ArrayUtils.EMPTY_STRING_ARRAY);
    }

    /**
     * Returns a new {@link List} containing the tokenizer elements.
     *
     * @return a new {@link List}.
     */
    public List<String> toList() {
        final List<String> list = new ArrayList<>();
        forEach(list::add);
        return list;
    }

    /**
     * Returns a sequential stream on this Iterable instance.
     *
     * @return a sequential stream on this Iterable instance.
     */
    public Stream<String> toStream() {
        return StreamSupport.stream(spliterator(), false);
    }
}
