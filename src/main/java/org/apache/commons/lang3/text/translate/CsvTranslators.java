package org.apache.commons.lang3.text.translate;

import org.apache.commons.lang3.CharUtils;
import org.apache.commons.lang3.StringUtils;

import java.io.IOException;
import java.io.Writer;

/**
 * This class holds inner classes for escaping/unescaping Comma Separated Values.
 */
public class CsvTranslators {

    private static final char CSV_DELIMITER = ',';
    private static final char CSV_QUOTE = '"';
    private static final String CSV_QUOTE_STR = String.valueOf(CSV_QUOTE);
    private static final char[] CSV_SEARCH_CHARS =
            new char[] {CSV_DELIMITER, CSV_QUOTE, CharUtils.CR, CharUtils.LF};

    private CsvTranslators() {}

    /**
     * Translator for escaping Comma Separated Values.
     */
    public static class CsvEscaper extends SinglePassTranslator {

        @Override
        void translateWhole(final CharSequence input, final Writer out) throws IOException {
            if (StringUtils.containsNone(input.toString(), CSV_SEARCH_CHARS)) {
                out.write(input.toString());
            } else {
                out.write(CSV_QUOTE);
                out.write(StringUtils.replace(input.toString(), CSV_QUOTE_STR, CSV_QUOTE_STR + CSV_QUOTE_STR));
                out.write(CSV_QUOTE);
            }
        }
    }

    /**
     * Translator for unescaping escaped Comma Separated Value entries.
     */
    public static class CsvUnescaper extends SinglePassTranslator {

        @Override
        void translateWhole(final CharSequence input, final Writer out) throws IOException {
            if (input.charAt(0) != CSV_QUOTE || input.charAt(input.length() - 1) != CSV_QUOTE) {
                out.write(input.toString());
                return;
            }

            // strip quotes
            final String quoteless = input.subSequence(1, input.length() - 1).toString();

            if (StringUtils.containsAny(quoteless, CSV_SEARCH_CHARS)) {
                // deal with escaped quotes; ie) ""
                out.write(StringUtils.replace(quoteless, CSV_QUOTE_STR + CSV_QUOTE_STR, CSV_QUOTE_STR));
            } else {
                out.write(input.toString());
            }
        }
    }
}