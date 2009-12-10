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

/**
 * Translates codepoints to their XML numeric entity escaped value.
 * 
 * @author Apache Software Foundation
 * @since 3.0
 */
public class NumericEntityEscaper extends CodePointTranslator {

    private final int below;
    private final int above;
    private final boolean between;

    private NumericEntityEscaper(int below, int above, boolean between) {
        this.below = below;
        this.above = above;
        this.between = between;
    }

    public NumericEntityEscaper() { 
        this(0, Integer.MAX_VALUE, true);
    }

    public static NumericEntityEscaper below(int codepoint) {
        return outsideOf(codepoint, Integer.MAX_VALUE);
    }

    public static NumericEntityEscaper above(int codepoint) {
        return outsideOf(0, codepoint);
    }

    public static NumericEntityEscaper between(int codepointLow, int codepointHigh) {
        return new NumericEntityEscaper(codepointLow, codepointHigh, true);
    }

    public static NumericEntityEscaper outsideOf(int codepointLow, int codepointHigh) {
        return new NumericEntityEscaper(codepointLow, codepointHigh, false);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean translate(int codepoint, Writer out) throws IOException {
        if(between) {
            if (codepoint < below || codepoint > above) {
                return false;
            }
        } else {
            if (codepoint >= below && codepoint <= above) {
                return false;
            }
        }

        // TODO: if (codepoint > 0xffff) ?
        out.write("&#");
        out.write(Integer.toString(codepoint, 10));
        out.write(';');
        return true;
    }
}
