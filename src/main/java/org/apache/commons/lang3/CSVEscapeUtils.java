package org.apache.commons.lang3;

import org.apache.commons.lang3.text.translate.CharSequenceTranslator;

import java.io.IOException;
import java.io.Writer;

public class CSVEscapeUtils extends CharSequenceTranslator {

    public static final CharSequenceTranslator ESCAPE_CSV = new CSVEscapeUtils();
    public static final CharSequenceTranslator UNESCAPE_CSV = new CSVEscapeUtils();
//    @Override
//    public int translate(CharSequence input, int index, Writer out) throws IOException {
//        return 0;
//    }

    private static final char CSV_DELIMITER = ',';
    private static final char CSV_QUOTE = '"';
    private static final String CSV_QUOTE_STR = String.valueOf(CSV_QUOTE);
    private static final char[] CSV_SEARCH_CHARS = { CSV_DELIMITER, CSV_QUOTE, CharUtils.CR, CharUtils.LF };

    @Override
    public int translate(final CharSequence input, final int index, final Writer out) throws IOException {

        if (index != 0) {
            throw new IllegalStateException("CsvEscaper should never reach the [1] index");
        }

        if (StringUtils.containsNone(input.toString(), CSV_SEARCH_CHARS)) {
            out.write(input.toString());
        } else {
            out.write(CSV_QUOTE);
            out.write(StringUtils.replace(input.toString(), CSV_QUOTE_STR, CSV_QUOTE_STR + CSV_QUOTE_STR));
            out.write(CSV_QUOTE);
        }
        return Character.codePointCount(input, 0, input.length());
    }

    public static final String escapeCsv(final String input) {
        return ESCAPE_CSV.translate(input);
    }

    /**
     * <p>Returns a {@code String} value for an unescaped CSV column. </p>
     *
     * <p>If the value is enclosed in double quotes, and contains a comma, newline
     *    or double quote, then quotes are removed.
     * </p>
     *
     * <p>Any double quote escaped characters (a pair of double quotes) are unescaped
     *    to just one double quote. </p>
     *
     * <p>If the value is not enclosed in double quotes, or is and does not contain a
     *    comma, newline or double quote, then the String value is returned unchanged.</p>
     *
     * see <a href="http://en.wikipedia.org/wiki/Comma-separated_values">Wikipedia</a> and
     * <a href="http://tools.ietf.org/html/rfc4180">RFC 4180</a>.
     *
     * @param input the input CSV column String, may be null
     * @return the input String, with enclosing double quotes removed and embedded double
     * quotes unescaped, {@code null} if null string input
     * @since 2.4
     */
    public static final String unescapeCsv(final String input) {
        return UNESCAPE_CSV.translate(input);
    }
}
