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
 * Abstract translator for processing whole input in single pass.
 * Handles initial index checking and counting of returned code points.
 */
abstract class SinglePassTranslator extends CharSequenceTranslator {

    @Override
    public int translate(final CharSequence input, final int index, final Writer out) throws IOException {
        if (index != 0) {
            throw new IllegalStateException(getClassName() + " should never reach index different than 0");
        }

        translateWhole(input, out);

        return Character.codePointCount(input, 0, input.length());
    }

    private String getClassName() {
        final Class clazz = this.getClass();
        return clazz.isAnonymousClass() ?  clazz.getName() : clazz.getSimpleName();
    }

    /**
     * Translate whole set of code points passed in input.
     *
     * @param input CharSequence that is being translated
     * @param out Writer to translate the text to
     * @return total count of codepoints in input
     * @throws IOException if and only if the Writer produces an IOException
     */
    abstract void translateWhole(final CharSequence input, final Writer out) throws IOException;
}
