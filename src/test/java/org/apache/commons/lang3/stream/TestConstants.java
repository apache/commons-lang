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
package org.apache.commons.lang3.stream;

import java.util.Arrays;
import java.util.List;

/**
 * This class holds constant values used for testing the classes in
 * {@link org.apache.commons.lang3.stream}.
 */
public class TestConstants {
    static final String EXPECTED_NFE_MESSAGE_INT = "For input string: \"4 \"";
    static final String EXPECTED_NFE_MESSAGE_DOUBLE = "For input string: \"4.5x\"";
    static final String EXPECTED_NFE_MESSAGE_LONG = "For input string: \"4000000000 \"";

    static final String EXPECTED_EXCEPTION = "Expected Exception";

    private static final List<String> EXTRA_INTS = Arrays.asList("7", "8");
    static final List<String> INPUT_INT = Arrays.asList("1", "2", "3", "4", "5", "6");
    static final List<String> FAILING_INPUT_INT = Arrays.asList("1", "2", "3", "4 ", "5", "6");
    static final List<List<String>> FLAT_MAP_INPUT_INT = Arrays.asList(INPUT_INT, EXTRA_INTS);
    static final List<List<String>> FAILING_FLAT_MAP_INPUT_INT = Arrays.asList(FAILING_INPUT_INT,
            EXTRA_INTS);

    static final List<String> INPUT_LONG = Arrays.asList("1000000000", "2000000000", "3000000000",
            "4000000000", "5000000000", "6000000000");
    static final List<List<String>> FLAT_MAP_INPUT_LONG = Arrays.asList(INPUT_LONG,
                    Arrays.asList("7000000000", "8000000000"));

    static final List<String> INPUT_DOUBLE = Arrays.asList("1.5", "2.5", "3.5", "4.5", "5.5", "6.5");
    static final List<List<String>> FLAT_MAP_INPUT_DOUBLE = Arrays.asList(INPUT_DOUBLE,
            Arrays.asList("7.5", "8.5"));

    private TestConstants() {}
}
