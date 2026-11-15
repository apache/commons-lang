/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.commons.lang3.builder;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;
class ReflectionToStringBuilderTV04Test {
 static class Item { private final String name="Ada"; private final String secret="hidden"; @ToStringExclude private final String omitted="omit"; @ToStringSummary private final int[] ids={1,2}; }
 @Test void rendersFieldsAndHonorsExclusion() { String value=ReflectionToStringBuilder.toString(new Item()); assertTrue(value.contains("name=Ada")); assertFalse(value.contains("omitted")); assertTrue(value.contains("secret=hidden")); }
 @Test void includeAndExcludeFieldListsAreApplied() { Item item=new Item(); String onlyName=ReflectionToStringBuilder.toStringInclude(item,"name"); assertTrue(onlyName.contains("name")); assertFalse(onlyName.contains("secret")); String noSecret=ReflectionToStringBuilder.toStringExclude(item,"secret"); assertFalse(noSecret.contains("secret")); }
}
