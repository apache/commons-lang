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

package org.apache.commons.lang3.reflect;

import org.junit.jupiter.api.Test;

/**
 * A top-level class is parameterised using itself.
 *
 * @param <U> itself
 */
public class Lang1703Test<U extends Lang1703Test<U>> {

    @Test
    void testStackOverflowError() {
        // causes a java.lang.StackOverflowError
        TypeUtils.toString(Lang1703Test.class);
    }

}
