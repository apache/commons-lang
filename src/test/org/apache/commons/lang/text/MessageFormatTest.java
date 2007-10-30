package org.apache.commons.lang.text;

import java.text.MessageFormat;
import java.util.Locale;

/**
 * Baseline tests for java.text.MessageFormat.
 * 
 * @author Matt Benson
 * @since 2.4
 * @version $Id$
 */
public class MessageFormatTest extends AbstractMessageFormatTest {
    /*
     * (non-Javadoc)
     * 
     * @see org.apache.commons.lang.text.AbstractMessageFormatTest#createMessageFormat(java.lang.String)
     */
    protected MessageFormat createMessageFormat(String pattern) {
        return new MessageFormat(pattern, Locale.US);
    }
}
