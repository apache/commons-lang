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
class DiffableTV04Test {
 static class Person implements Diffable<Person> { int age; Person(int age){this.age=age;} public DiffResult<Person> diff(Person other){ return new DiffBuilder<>(this,other,ToStringStyle.SIMPLE_STYLE).append("age",age,other.age).build(); } }
 @Test void implementationUsesDiffContract() { assertEquals(0,new Person(20).diff(new Person(20)).getNumberOfDiffs()); assertEquals(1,new Person(20).diff(new Person(21)).getNumberOfDiffs()); }
}
