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
