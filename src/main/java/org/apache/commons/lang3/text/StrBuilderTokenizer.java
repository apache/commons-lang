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
package org.apache.commons.lang3.text;

import java.util.List;

/**
 * Tokenizer that uses a {@link StrBuilder} as its character source.
 * <p>
 * This class was extracted from StrBuilder to improve maintainability
 * and follow the Single Responsibility Principle.
 * </p>
 *
 * @since 3.0
 */
final class StrBuilderTokenizer extends StrTokenizer {

    /** The parent StrBuilder providing character data. */
    private final StrBuilder builder;

    /**
     * Constructs a tokenizer for the given StrBuilder.
     *
     * @param builder  the StrBuilder to tokenize, must not be null
     */
    StrBuilderTokenizer(final StrBuilder builder) {
        this.builder = builder;
    }

    /** {@inheritDoc} */
    @Override
    public String getContent() {
        final String str = super.getContent();
        if (str == null) {
            return builder.toString();
        }
        return str;
    }

    /** {@inheritDoc} */
    @Override
    protected List<String> tokenize(final char[] chars, final int offset, final int count) {
        if (chars == null) {
            return super.tokenize(builder.buffer, 0, builder.size());
        }
        return super.tokenize(chars, offset, count);
    }
}
