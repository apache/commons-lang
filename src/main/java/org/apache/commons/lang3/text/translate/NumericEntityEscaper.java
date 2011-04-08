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

import org.apache.commons.lang3.Range;

/**
 * Translates codepoints to their XML numeric entity escaped value.
 *
 * @since 3.0
 * @version $Id$
 */
public class NumericEntityEscaper extends CodePointTranslator {

    private Range<Integer> range;

    /**
     * <p>Constructs a <code>NumericEntityEscaper</code> for the specified range. This is
     * the underlying method for the other constructors/builders. </p>
     *
     * @param range range within which to escape entities
     */
    public NumericEntityEscaper(Range<Integer> range) {
        this.range = range;
    }

    /**
     * <p>Constructs a <code>NumericEntityEscaper</code> for all characters. </p>
     */
    public NumericEntityEscaper() {
        this.range = Range.between(0, Integer.MAX_VALUE);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean translate(int codepoint, Writer out) throws IOException {
        if(!range.contains(codepoint)) {
            return false;
        }

        out.write("&#");
        out.write(Integer.toString(codepoint, 10));
        out.write(';');
        return true;
    }
}
