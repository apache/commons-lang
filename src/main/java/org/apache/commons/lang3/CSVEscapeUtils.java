package org.apache.commons.lang3;

import org.apache.commons.lang3.text.translate.CharSequenceTranslator;

import java.io.IOException;
import java.io.Writer;

public class CSVEscapeUtils extends CharSequenceTranslator {

    public static final CharSequenceTranslator ESCAPE_CSV = new CSVEscapeUtils();
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
}
