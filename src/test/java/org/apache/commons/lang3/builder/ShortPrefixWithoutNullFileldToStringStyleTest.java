package org.apache.commons.lang3.builder;

import junit.framework.TestCase;

import org.apache.commons.lang3.builder.ToStringStyleTest.Person;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class ShortPrefixWithoutNullFileldToStringStyleTest extends TestCase {

    @Before
    protected void setUp() throws Exception {
	super.setUp();
	ToStringBuilder
		.setDefaultStyle(ToStringStyle.SHORT_PREFIX_WITHOUT_NULL_FIELD_STYLE);
    }

    @After
    protected void tearDown() throws Exception {
	ToStringBuilder.setDefaultStyle(ToStringStyle.DEFAULT_STYLE);
	super.tearDown();

    }

    @Test
    public void testInteger() {
	Integer i1 = new Integer(1);

	String baseString = "Integer";
	assertEquals(baseString + "[value=1]",
		ToStringBuilder.reflectionToString(i1,
			ToStringStyle.SHORT_PREFIX_WITHOUT_NULL_FIELD_STYLE));
    }

    @Test
    public void testPerson() {
	Person p = new Person();

	String baseString = "ToStringStyleTest.Person";
	assertEquals(baseString + "[age=0,smoker=false]",
		ToStringBuilder.reflectionToString(p,
			ToStringStyle.SHORT_PREFIX_WITHOUT_NULL_FIELD_STYLE));

    }

}
