package org.apache.commons.lang3.time;

import java.util.TimeZone;

/**
 * An immutable {@link TimeZone} implementation which delegates all read only
 * methods to the provided delegate and throws exception for all state modifying
 * method calls.
 */
public class ImmutableTimeZone extends DelegatingTimeZone {
    private static final long serialVersionUID = 2425068836232712021L;

    /**
     * @param delegate
     *            the not-null wrapped timezone
     */
    public ImmutableTimeZone(TimeZone delegate) {
        super(delegate);
    }

    @Override
    public void setID(String ID) {
        throw fail();
    }

    @Override
    public void setRawOffset(int offsetMillis) {
        throw fail();
    }

    private final RuntimeException fail() {
        return new UnsupportedOperationException(
                "This is an immutable TimeZone. State modifying methods are not supported.");
    }
}
