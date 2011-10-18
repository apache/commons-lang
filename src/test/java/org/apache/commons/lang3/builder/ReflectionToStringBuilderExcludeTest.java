/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.commons.lang3.builder;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import junit.framework.Assert;

import org.apache.commons.lang3.ArrayUtils;
import org.junit.Test;

/**
 * @version $Id$
 */
public class ReflectionToStringBuilderExcludeTest {

    class TestFixture {
        @SuppressWarnings("unused")
        private String secretField = SECRET_VALUE;

        @SuppressWarnings("unused")
        private String showField = NOT_SECRET_VALUE;
    }

    private static final String NOT_SECRET_FIELD = "showField";

    private static final String NOT_SECRET_VALUE = "Hello World!";

    private static final String SECRET_FIELD = "secretField";

    private static final String SECRET_VALUE = "secret value";

    @Test
    public void test_toStringExclude() {
        String toString = ReflectionToStringBuilder.toStringExclude(new TestFixture(), SECRET_FIELD);
        this.validateSecretFieldAbsent(toString);
    }

    @Test
    public void test_toStringExcludeArray() {
        String toString = ReflectionToStringBuilder.toStringExclude(new TestFixture(), new String[]{SECRET_FIELD});
        this.validateSecretFieldAbsent(toString);
    }

    @Test
    public void test_toStringExcludeArrayWithNull() {
        String toString = ReflectionToStringBuilder.toStringExclude(new TestFixture(), new String[]{null});
        this.validateSecretFieldPresent(toString);
    }

    @Test
    public void test_toStringExcludeArrayWithNulls() {
        String toString = ReflectionToStringBuilder.toStringExclude(new TestFixture(), new String[]{null, null});
        this.validateSecretFieldPresent(toString);
    }

    @Test
    public void test_toStringExcludeCollection() {
        List<String> excludeList = new ArrayList<String>();
        excludeList.add(SECRET_FIELD);
        String toString = ReflectionToStringBuilder.toStringExclude(new TestFixture(), excludeList);
        this.validateSecretFieldAbsent(toString);
    }

    @Test
    public void test_toStringExcludeCollectionWithNull() {
        List<String> excludeList = new ArrayList<String>();
        excludeList.add(null);
        String toString = ReflectionToStringBuilder.toStringExclude(new TestFixture(), excludeList);
        this.validateSecretFieldPresent(toString);
    }

    @Test
    public void test_toStringExcludeCollectionWithNulls() {
        List<String> excludeList = new ArrayList<String>();
        excludeList.add(null);
        excludeList.add(null);
        String toString = ReflectionToStringBuilder.toStringExclude(new TestFixture(), excludeList);
        this.validateSecretFieldPresent(toString);
    }

    @Test
    public void test_toStringExcludeEmptyArray() {
        String toString = ReflectionToStringBuilder.toStringExclude(new TestFixture(), ArrayUtils.EMPTY_STRING_ARRAY);
        this.validateSecretFieldPresent(toString);
    }

    @Test
    public void test_toStringExcludeEmptyCollection() {
        String toString = ReflectionToStringBuilder.toStringExclude(new TestFixture(), new ArrayList<String>());
        this.validateSecretFieldPresent(toString);
    }

    @Test
    public void test_toStringExcludeNullArray() {
        String toString = ReflectionToStringBuilder.toStringExclude(new TestFixture(), (String[]) null);
        this.validateSecretFieldPresent(toString);
    }

    @Test
    public void test_toStringExcludeNullCollection() {
        String toString = ReflectionToStringBuilder.toStringExclude(new TestFixture(), (Collection<String>) null);
        this.validateSecretFieldPresent(toString);
    }

    private void validateNonSecretField(String toString) {
        Assert.assertTrue(toString.indexOf(NOT_SECRET_FIELD) > ArrayUtils.INDEX_NOT_FOUND);
        Assert.assertTrue(toString.indexOf(NOT_SECRET_VALUE) > ArrayUtils.INDEX_NOT_FOUND);
    }

    private void validateSecretFieldAbsent(String toString) {
        Assert.assertEquals(ArrayUtils.INDEX_NOT_FOUND, toString.indexOf(SECRET_VALUE));
        this.validateNonSecretField(toString);
    }

    private void validateSecretFieldPresent(String toString) {
        Assert.assertTrue(toString.indexOf(SECRET_VALUE) > 0);
        this.validateNonSecretField(toString);
    }
}
