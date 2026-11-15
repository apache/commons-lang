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
import org.junit.jupiter.api.Test;
class FailableByteConsumerTV04Test {
 @Test void consumesByteAndSupportsAndThen() throws Exception {
  int[] value={0};
  FailableByteConsumer<RuntimeException> c = b -> value[0]=b;
  c.andThen(b -> value[0]+=b).accept((byte)4);
  assertEquals(8,value[0]);
  FailableByteConsumer.<RuntimeException>nop().accept((byte)1);
 }
}
