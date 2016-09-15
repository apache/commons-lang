package org.apache.commons.lang3.builder;

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.not;
import static org.junit.Assert.assertThat;

import org.junit.Test;

public class ReflectionToStringBuilderExcludeNullValueWithAnnotationTest {

    class TestFixture {
    	@ToStringExclude
        private final String excludedField = null;

        @SuppressWarnings("unused")
        private final String includedField = INCLUDED_FIELD_VALUE;
    }
	
    private static final String INCLUDED_FIELD_NAME = "includedField";

    private static final String INCLUDED_FIELD_VALUE = "Hello World!";

    private static final String EXCLUDED_FIELD_NAME = "excludedField";
    
    private static final String VALUE_WHEN_IS_NULL = "null";
    
    @Test
    public void test_toStringExclude() {
        final String toString = ReflectionToStringBuilder.toString(new TestFixture());

        assertThat(toString, not(containsString(EXCLUDED_FIELD_NAME)));
        assertThat(toString, not(containsString(VALUE_WHEN_IS_NULL)));
        assertThat(toString, containsString(INCLUDED_FIELD_NAME));
        assertThat(toString, containsString(INCLUDED_FIELD_VALUE));
    }
    
}
