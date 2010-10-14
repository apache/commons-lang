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
import java.util.Arrays;
import java.util.EnumSet;

/**
 * Translates escaped unicode values of the form \\u+\d\d\d\d back to 
 * unicode.
 * 
 * @author Apache Software Foundation
 * @since 3.0
 * @version $Id$
 */
public class UnicodeUnescaper extends CharSequenceTranslator {

    public static enum OPTION { escapePlus }

    // TODO: Create an OptionsSet class to hide some of the conditional logic below
    private final EnumSet<OPTION> options;

    public UnicodeUnescaper(OPTION... options) {
        if(options.length > 0) {
            this.options = EnumSet.copyOf(Arrays.asList(options));
        } else {
            this.options = null;
        }
    }

    public boolean isSet(OPTION opt) { 
        return (options == null) ? false : options.contains(opt);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int translate(CharSequence input, int index, Writer out) throws IOException {
        if(input.charAt(index) == '\\') {
            if( (index + 1 < input.length()) && input.charAt(index + 1) == 'u') {
                // consume optional additional 'u' chars
                int i=2;
                while( (index + i < input.length()) && input.charAt(index + i) == 'u') {
                    i++;
                }

                // consume + symbol in \\u+0045
                if(isSet(OPTION.escapePlus)) {
                    if( (index + i < input.length()) && (input.charAt(index + i) == '+') ) {
                        i++;
                    }
                }

                if( (index + i + 4 <= input.length()) ) {
                    // Get 4 hex digits
                    CharSequence unicode = input.subSequence(index + i, index + i + 4);

                    try {
                        int value = Integer.parseInt(unicode.toString(), 16);
                        out.write((char) value);
                    } catch (NumberFormatException nfe) {
                        throw new IllegalArgumentException("Unable to parse unicode value: " + unicode, nfe);
                    }
                    return i + 4;
                } else {
                    throw new IllegalArgumentException("Less than 4 hex digits in unicode value: '" + 
                                                       input.subSequence(index, input.length()) +
                                                       "' due to end of CharSequence");
                }
            }
        }
        return 0;
    }
}
