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
import java.io.StringWriter;
import java.io.Writer;
import java.util.Locale;

/**
 * An API for translating text. 
 * Its core use is to escape and unescape text. Because escaping and unescaping 
 * is completely contextual, the API does not present two separate signatures.
 * 
 * @since 3.0
 * @version $Id$
 */
public abstract class CharSequenceTranslator {

    /**
     * Translate a set of codepoints, represented by an int index into a CharSequence, 
     * into another set of codepoints. The number of codepoints consumed must be returned, 
     * and the only IOExceptions thrown must be from interacting with the Writer so that 
     * the top level API may reliable ignore StringWriter IOExceptions. 
     *
     * @param input CharSequence that is being translated
     * @param index int representing the current point of translation
     * @param out Writer to translate the text to
     * @return int count of codepoints consumed
     * @throws IOException if and only if the Writer produces an IOException
     */
    public abstract int translate(CharSequence input, int index, Writer out) throws IOException;

    /**
     * Helper for non-Writer usage. 
     * @param input CharSequence to be translated
     * @return String output of translation
     */
    public final String translate(CharSequence input) {
        if (input == null) {
            return null;
        }
        try {
            StringWriter writer = new StringWriter(input.length() * 2);
            translate(input, writer);
            return writer.toString();
        } catch (IOException ioe) {
            // this should never ever happen while writing to a StringWriter
            throw new RuntimeException(ioe);
        }
    }

    /**
     * Translate an input onto a Writer. This is intentionally final as its algorithm is 
     * tightly coupled with the abstract method of this class. 
     *
     * @param input CharSequence that is being translated
     * @param out Writer to translate the text to
     * @throws IOException if and only if the Writer produces an IOException
     */
    public final void translate(CharSequence input, Writer out) throws IOException {
        if (out == null) {
            throw new IllegalArgumentException("The Writer must not be null");
        }
        if (input == null) {
            return;
        }
        int sz = Character.codePointCount(input, 0, input.length());
        for (int i = 0; i < sz; i++) {

            // consumed is the number of codepoints consumed
            int consumed = translate(input, i, out);

            if (consumed == 0) {
                out.write(Character.toChars(Character.codePointAt(input, i)));
            } else {
                // contract with translators is that they have to understand codepoints 
                // and they just took care of a surrogate pair
                for (int j = 0; j < consumed; j++) {
                    if (i < sz - 2) {
                        i += Character.charCount(Character.codePointAt(input, i));
                    } else {
                        // If the String ends with a high surrogate, just add the 1 and don't worry about such things
                        i++;
                    }
                }
                // for loop will increment 1 anyway, so remove 1 to account for that
                i--;
            }
        }
    }

    /**
     * Helper method to create a merger of this translator with another set of 
     * translators. Useful in customizing the standard functionality.
     *
     * @param translators CharSequenceTranslator array of translators to merge with this one
     * @return CharSequenceTranslator merging this translator with the others
     */
    public final CharSequenceTranslator with(CharSequenceTranslator... translators) {
        CharSequenceTranslator[] newArray = new CharSequenceTranslator[translators.length + 1];
        newArray[0] = this;
        System.arraycopy(translators, 0, newArray, 1, translators.length);
        return new AggregateTranslator(newArray);
    }

    /**
     * <p>Returns an upper case hexadecimal <code>String</code> for the given
     * character.</p>
     *
     * @param codepoint The codepoint to convert.
     * @return An upper case hexadecimal <code>String</code>
     */
    public static String hex(int codepoint) {
        return Integer.toHexString(codepoint).toUpperCase(Locale.ENGLISH);
    }

}
