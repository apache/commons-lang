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
class DiffBuilderTV04Test {
 @Test void recordsOnlyDifferentFieldsAndExposesResult() {
  DiffResult<String> r=DiffBuilder.<String>builder().setLeft("left").setRight("right").setStyle(ToStringStyle.SIMPLE_STYLE).build()
    .append("age",1,2).append("name","same","same").append("active",true,false).build();
  assertEquals(2,r.getNumberOfDiffs()); assertEquals("left",r.getLeft()); assertEquals("right",r.getRight());
  assertEquals("age",r.getDiffs().get(0).getFieldName()); assertTrue(r.toString().contains("age"));
 }
 @Test void equalObjectsProduceNoDiffsByDefault() { String s=new String("same"); DiffResult<String> r=new DiffBuilder<>(s,new String("same"),ToStringStyle.DEFAULT_STYLE).append("x",1,2).build(); assertEquals(0,r.getNumberOfDiffs()); }
}
