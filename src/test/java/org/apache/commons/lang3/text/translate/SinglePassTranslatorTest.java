package org.apache.commons.lang3.text.translate;

import org.junit.Before;
import org.junit.Test;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;

import static org.junit.Assert.assertEquals;

/**
 * Unit test for {@link SinglePassTranslator}
 */
public class SinglePassTranslatorTest {

    private final SinglePassTranslator dummyTranslator = new SinglePassTranslator() {
        @Override
        void translateWhole(final CharSequence input, final Writer out) throws IOException {
        }
    };

    private StringWriter out;

    @Before
    public void before() {
         out = new StringWriter();
    }

    @Test
    public void codePointsAreReturned() throws Exception {
        assertEquals(0, dummyTranslator.translate("", 0, out));
        assertEquals(3, dummyTranslator.translate("abc", 0, out));
        assertEquals(7, dummyTranslator.translate("abcdefg", 0, out));
    }

    @Test(expected = IllegalStateException.class)
    public void indexIsValidated() throws Exception {
        dummyTranslator.translate("abc", 1, out);
    }
}