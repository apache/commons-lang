package org.apache.commons.lang3.text.translate;

import java.io.IOException;
import java.io.Writer;
import java.util.HashMap;
import java.util.HashSet;

/**
 * Translates a value using a lookup table.
 * But doesn't translate if that value is already translated.
 *
 * @since 3.0
 */
public class SingleLookupTranslator extends CharSequenceTranslator {

    private final HashMap<String, String> lookupMap;
    private final HashSet<Character>      prefixSet;
    private final int                     shortest;
    private final int                     longest;
    private final int                     shortestValue;
    private final int                     longestValue;

    /**
     * Define the look tables to be used in translation.
     *
     * Note that, as of Lang 3.1, the key to the lookup table is converted to a
     * java.lang.String. This is because we need the key to support hashCode and
     * equals(Object), allowing it to be the key for a HashMap. See LANG-882.
     *
     * Also note that, multiple lookup tables should be passed to this translator
     * instead of passing multiple instances of this translator to the
     * AggregateTranslator. Because, this translator only checks the values of the
     * lookup table passed to this instance while deciding whether a value is
     * already translated or not.
     *
     * @param inputArrays
     */
    public SingleLookupTranslator(final String[][]... inputArrays) {
        String[][] lookup = new String[0][];
        for (String[][] input : inputArrays) {
            lookup = append(lookup, input);
        }
        lookupMap = new HashMap<String, String>();
        prefixSet = new HashSet<Character>();
        int _shortest = Integer.MAX_VALUE;
        int _longest = 0;
        int _shortestValue = Integer.MAX_VALUE;
        int _longestValue = 0;
        if (lookup != null) {
            for (final CharSequence[] seq : lookup) {
                this.lookupMap.put(seq[0].toString(), seq[1].toString());
                this.prefixSet.add(seq[0].charAt(0));
                final int sz = seq[0].length();
                if (sz < _shortest) {
                    _shortest = sz;
                }
                if (sz > _longest) {
                    _longest = sz;
                }
                final int sizeOfValue = seq[1].length();
                if (sizeOfValue < _shortestValue) {
                    _shortestValue = sizeOfValue;
                }
                if (sizeOfValue > _longestValue) {
                    _longestValue = sizeOfValue;
                }
            }
        }
        shortest = _shortest;
        longest = _longest;
        shortestValue = _shortestValue;
        longestValue = _longestValue;
    }

    private static String[][] append(String[][] a, String[][] b) {
        String[][] result = new String[a.length + b.length][];
        System.arraycopy(a, 0, result, 0, a.length);
        System.arraycopy(b, 0, result, a.length, b.length);
        return result;
    }

    /**
     * Translate a set of codepoints, represented by an int index into a CharSequence,
     * into another set of codepoints. The number of codepoints consumed must be returned,
     * and the only IOExceptions thrown must be from interacting with the Writer so that
     * the top level API may reliably ignore StringWriter IOExceptions.
     *
     * @param input CharSequence that is being translated
     * @param index int representing the current point of translation
     * @param out   Writer to translate the text to
     * @return int count of codepoints consumed
     * @throws IOException if and only if the Writer produces an IOException
     */
    @Override
    public int translate(CharSequence input, int index, Writer out) throws IOException {
        // check if already translated
        int maxValue = longestValue;
        if (index + maxValue > input.length()) {
            maxValue = input.length() - index;
        }
        // implement greedy algorithm to check all the possible 'value' matches for which we need to skip translation.
        for (int i = maxValue; i >= shortestValue; i--) {
            final CharSequence subSeq = input.subSequence(index, index + i);
            // If the sub-string is already translated, return without translating.
            if (lookupMap.containsValue(subSeq.toString())) {
                return 0;
            }
        }

        // check if translation exists for the input at position index
        if (prefixSet.contains(input.charAt(index))) {
            int max = longest;
            if (index + longest > input.length()) {
                max = input.length() - index;
            }
            // implement greedy algorithm by trying maximum match first
            for (int i = max; i >= shortest; i--) {
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
