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

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

public class ReflectionToStringBuilderSummaryTest extends AbstractLangTest {

    @SuppressWarnings("unused")
    private final String stringField = "string";

    @ToStringSummary
    private final String summaryString = "summary";

    @Test
    public void testSummary() {
        assertEquals("[stringField=string,summaryString=<String>]",
                new ReflectionToStringBuilder(this, ToStringStyle.NO_CLASS_NAME_STYLE).build());
    }

}
