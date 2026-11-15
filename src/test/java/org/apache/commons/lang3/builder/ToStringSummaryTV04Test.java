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
import java.lang.reflect.Field;
import org.junit.jupiter.api.Test;
class ToStringSummaryTV04Test {
 static class Item { @ToStringSummary private int[] values={1,2,3}; }
 @Test void annotationIsRuntimeVisibleAndSummarizesArray() throws Exception { Field f=Item.class.getDeclaredField("values"); assertTrue(f.isAnnotationPresent(ToStringSummary.class)); String text=ReflectionToStringBuilder.toString(new Item()); assertTrue(text.contains("values")); assertTrue(text.contains("3")); }
}
