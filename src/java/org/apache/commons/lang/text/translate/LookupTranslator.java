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
package org.apache.commons.lang.text.translate;

import java.io.IOException;
import java.io.Writer;

/**
 * Translates a value using a lookup table. 
 * @since 3.0
 */
public class LookupTranslator extends CharSequenceTranslator {

    protected CharSequence[][] lookup;

    /**
     * Define the lookup table to be used in translation
     *
     * @param CharSequence[][] Lookup table of size [*][2]
     */
    public LookupTranslator(CharSequence[][] lookup) {
        this.lookup = lookup;
    }

    /**
     * {@inheritDoc}
     */
    public int translate(CharSequence input, int index, Writer out) throws IOException {
        CharSequence subsequence = input.subSequence(index, input.length());
        for(CharSequence[] seq : lookup) {
            if( subsequence.toString().startsWith(seq[0].toString()) ) {
                out.write(seq[1].toString());
                return seq[0].length();
            }
        }
        return 0;
    }
}
