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
class ReflectionDiffBuilderTV04Test {
 static class Item { private int count; @DiffExclude private String token; Item(int c,String t){count=c;token=t;} }
 @Test void buildsDiffsFromFieldsAndSupportsExclusion() {
  Item a=new Item(1,"a"), b=new Item(2,"b");
  DiffBuilder<Item> base=new DiffBuilder<>(a,b,ToStringStyle.SIMPLE_STYLE);
  DiffResult<Item> r=ReflectionDiffBuilder.<Item>builder().setDiffBuilder(base).setExcludeFieldNames("token").build().build();
  assertEquals(1,r.getNumberOfDiffs()); assertEquals("count",r.getDiffs().get(0).getFieldName());
 }
}
