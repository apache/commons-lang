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
class TriConsumerTV04Test {
 @Test void acceptsThreeInputsAndChains() { StringBuilder b=new StringBuilder(); TriConsumer<String,String,String> c=(a,x,z)->b.append(a).append(x).append(z); c.andThen((a,x,z)->b.append("!")).accept("a","b","c"); assertEquals("abc!",b.toString()); }
}
