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

package org.apache.commons.lang3.function;
import static org.junit.jupiter.api.Assertions.*;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.Test;
class FailableConsumerTV04Test {
 @Test void acceptStaticMethodIgnoresNullAndDelegatesOtherwise() throws Exception {
  AtomicReference<String> ref=new AtomicReference<>(); FailableConsumer<String,RuntimeException> c=ref::set;
  FailableConsumer.accept(c,"value"); assertEquals("value",ref.get()); FailableConsumer.accept(null,"ignored"); assertEquals("value",ref.get());
 }
 @Test void andThenRunsBothActionsInOrder() throws Exception {
  StringBuilder b=new StringBuilder(); FailableConsumer<String,RuntimeException> c=s->b.append(s);
  c.andThen(s->b.append("!")).accept("ok"); assertEquals("ok!",b.toString());
 }
}