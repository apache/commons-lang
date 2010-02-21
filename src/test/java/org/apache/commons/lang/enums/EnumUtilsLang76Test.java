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
package org.apache.commons.lang.enums;

import junit.framework.TestCase;

/**
 * Test cases for the LANG-76 issue with {@link EnumUtils}.
 *
 * NOTE: this needs to be compiled with source/target versions set to 1.5
 *       in order to replicate/test the issue properly
 */
public class EnumUtilsLang76Test extends TestCase {

    public EnumUtilsLang76Test(String name) {
        super(name);
    }

    /**
     * Test LANG-76
     */
    public void test_EnumUtils_getEnum_LANG76() {
        Object obj = EnumUtils.getEnum(TestEnum.class, "1");
        assertNotNull("Enum is not intialized", obj);
        assertEquals("EnumUtilsLang76Test.TestEnum[1]", obj.toString());
    }

    /** Test Enum for LANG-76 **/
    public static final class TestEnum  extends Enum {
        private static final long serialVersionUID = 1L;
        public static final TestEnum ONE     = new TestEnum("1");
        public static final TestEnum TWO     = new TestEnum("2");
        public static final TestEnum THREE   = new TestEnum("3");

        private TestEnum(String value) {
            super(value);
        }
    }
}
