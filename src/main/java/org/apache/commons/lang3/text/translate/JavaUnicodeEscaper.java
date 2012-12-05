package org.apache.commons.lang3.text.translate;

/**
 * Translates codepoints to their Unicode escaped value suitable for Java source.
 * 
 * @since 3.2
 * @version $Id$
 */
public class JavaUnicodeEscaper extends UnicodeEscaper {

    /**
     * <p>
     * Constructs a <code>JavaUnicodeEscaper</code> above the specified value (exclusive).
     * </p>
     * 
     * @param codepoint
     *            above which to escape
     * @return the newly created {@code UnicodeEscaper} instance
     */
    public static JavaUnicodeEscaper above(int codepoint) {
        return outsideOf(0, codepoint);
    }

    /**
     * <p>
     * Constructs a <code>JavaUnicodeEscaper</code> below the specified value (exclusive).
     * </p>
     * 
     * @param codepoint
     *            below which to escape
     * @return the newly created {@code UnicodeEscaper} instance
     */
    public static JavaUnicodeEscaper below(int codepoint) {
        return outsideOf(codepoint, Integer.MAX_VALUE);
    }

    /**
     * <p>
     * Constructs a <code>JavaUnicodeEscaper</code> between the specified values (inclusive).
     * </p>
     * 
     * @param codepointLow
     *            above which to escape
     * @param codepointHigh
     *            below which to escape
     * @return the newly created {@code UnicodeEscaper} instance
     */
    public static JavaUnicodeEscaper between(int codepointLow, int codepointHigh) {
        return new JavaUnicodeEscaper(codepointLow, codepointHigh, true);
    }

    /**
     * <p>
     * Constructs a <code>JavaUnicodeEscaper</code> outside of the specified values (exclusive).
     * </p>
     * 
     * @param codepointLow
     *            below which to escape
     * @param codepointHigh
     *            above which to escape
     * @return the newly created {@code UnicodeEscaper} instance
     */
    public static JavaUnicodeEscaper outsideOf(int codepointLow, int codepointHigh) {
        return new JavaUnicodeEscaper(codepointLow, codepointHigh, false);
    }

    /**
     * <p>
     * Constructs a <code>JavaUnicodeEscaper</code> for the specified range. This is the underlying method for the
     * other constructors/builders. The <code>below</code> and <code>above</code> boundaries are inclusive when
     * <code>between</code> is <code>true</code> and exclusive when it is <code>false</code>.
     * </p>
     * 
     * @param below
     *            int value representing the lowest codepoint boundary
     * @param above
     *            int value representing the highest codepoint boundary
     * @param between
     *            whether to escape between the boundaries or outside them
     */
    public JavaUnicodeEscaper(int below, int above, boolean between) {
        super(below, above, between);
    }

    /**
     * Converts the given codepoint to a hex string of the form {@code "\\uXXXX\\uXXXX"}
     * 
     * @param codePoint
     *            a Unicode code point
     */
    @Override
    protected String toUtf16Escape(int codepoint) {
        char[] surrogatePair = Character.toChars(codepoint);
        return "\\u" + hex(surrogatePair[0]) + "\\u" + hex(surrogatePair[1]);
    }

}
