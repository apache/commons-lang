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
import java.util.ArrayList;
import java.util.HashMap;

/**
 * Translates a value using a lookup table.
 *
 * @since 3.0
 * @version $Id$
 */
public class LookupTranslator extends CharSequenceTranslator {

    private static class Translation {
        String match;
        String replacement;

        public Translation(String match, String replacement) {
          this.match = match;
          this.replacement = replacement;
        }
    }

    private final HashMap<Character, HashMap<Integer, ArrayList<Translation>>> byFirstChar;
    private final int shortest;
    private final int longest;

    /**
     * Define the lookup table to be used in translation
     *
     * @param lookup CharSequence[][] table of size [*][2]
     */
    public LookupTranslator(final CharSequence[]... lookup) {
        byFirstChar = new HashMap<Character, HashMap<Integer, ArrayList<Translation>>>();
        int _shortest = Integer.MAX_VALUE;
        int _longest = 0;
        if (lookup != null) {
            for (final CharSequence[] seq : lookup) {
                String match = seq[0].toString();
                String replacement = seq[1].toString();
                int len = match.length();
                if (len < _shortest) {
                    _shortest = len;
                }
                if (len > _longest) {
                    _longest = len;
                }
                Character firstChar = match.charAt(0);
                HashMap<Integer, ArrayList<Translation>> byLen = byFirstChar.get(firstChar);
                if (byLen == null) {
                    byLen = new HashMap<Integer, ArrayList<Translation>>();
                    byFirstChar.put(firstChar, byLen);
                }
                ArrayList<Translation> translations = byLen.get(len);
                if (translations == null) {
                    translations = new ArrayList<Translation>();
                    byLen.put(len, translations);
                }

                translations.add(new Translation(match, replacement));
            }
        }
        shortest = _shortest;
        longest = _longest;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int translate(final CharSequence input, final int index, final Writer out) throws IOException {
        int max = longest;
        if (index + longest > input.length()) {
            max = input.length() - index;
        }

        HashMap<Integer, ArrayList<Translation>> byLen = byFirstChar.get(input.charAt(index));
        if (byLen != null) {

            // Implement greedy algorithm by trying descending length
            for (int len = max; len >= shortest; len--) {
                ArrayList<Translation> translations = byLen.get(len);
                if (translations != null) {
                    // iterate over all replacements check if they match
                    for (Translation translation : translations) {
                        if (translationMatches(translation, input, index)) {
                          out.write(translation.replacement);
                          return len;
                        }
                    }
                }
            }
        }
        return 0;
    }

    private boolean translationMatches(Translation translation, CharSequence input, int index) {
        String match = translation.match;
        // translation match is checked against input. Doing this char by char is more efficient
        // than getting a substring and hashing it (which would always need to allocate substrings).
        if (input instanceof String) {
            // for Strings startsWith is slightly faster when match is longer than 1
            return ((String) input).startsWith(match, index);
        }
        for (int i = 0; i < match.length(); i++) {
            if (input.charAt(index + i) != match.charAt(i)) {
              return false;
            }
        }
        return true;
    }
}
