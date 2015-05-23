package org.apache.commons.lang3.builder;

import org.junit.Test;

public class ReflectionToStringBuilderTest {

    @Test(expected=IllegalArgumentException.class)
    public void testConstructorWithNullObject() {
        new ReflectionToStringBuilder(null, ToStringStyle.DEFAULT_STYLE, new StringBuilder());
    }

}
