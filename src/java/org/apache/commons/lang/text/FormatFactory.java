package org.apache.commons.lang.text;

import java.text.Format;
import java.util.Locale;

/**
 * Format factory.
 * @since 2.4
 * @author Niall Pemberton
 * @version $Id$
 */
public interface FormatFactory {

    /**
     * Create or retrieve a format instance.
     *
     * @param name The format type name
     * @param arguments Arguments used to create the format instance. This allows the
     *                  <code>FormatFactory</code> to implement the "format style"
     *                  concept from <code>java.text.MessageFormat</code>.
     * @param locale The locale, may be null
     * @return The format instance
     */
    Format getFormat(String name, String arguments, Locale locale);

}
