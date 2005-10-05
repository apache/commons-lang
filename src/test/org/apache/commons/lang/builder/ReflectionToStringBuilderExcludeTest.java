/*
 * Copyright 2005 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.commons.lang.builder;

import junit.framework.Assert;
import junit.framework.TestCase;

/**
 * @author <a href="mailto:ggregory@seagullsw.com">ggregory</a>
 * @version $Id$
 */
public class ReflectionToStringBuilderExcludeTest extends TestCase {

    class TestFixture {
        private String secretField = SECRET_VALUE;

        private String showField = NOT_SECRET_VALUE;
    }

    private static final int INDEX_NOT_FOUND = -1;

    private static final String NOT_SECRET_FIELD = "showField";

    private static final String NOT_SECRET_VALUE = "Hello World!";

    private static final String SECRET_FIELD = "secretField";

    private static final String SECRET_VALUE = "secret value";

    public void test_toStringExcluding() {
        String toString = ReflectionToStringBuilder.toStringExclude(new TestFixture(), SECRET_FIELD);
        this.validateToStringValue(toString);
    }

    public void test_toStringExcludingArray() {
        String toString = ReflectionToStringBuilder.toStringExclude(new TestFixture(), new String[]{SECRET_FIELD});
        this.validateToStringValue(toString);
    }

    void validateToStringValue(String toString) {
        Assert.assertEquals(INDEX_NOT_FOUND, toString.indexOf(SECRET_VALUE));
        Assert.assertTrue(toString.indexOf(NOT_SECRET_FIELD) > INDEX_NOT_FOUND);
        Assert.assertTrue(toString.indexOf(NOT_SECRET_VALUE) > INDEX_NOT_FOUND);
    }
}
