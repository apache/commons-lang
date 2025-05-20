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

package org.apache.commons.lang3;

/**
 * Supplementary character test fixtures.
 *
 * See https://www.oracle.com/technical-resources/articles/javase/supplementary.html
 */
public class Supplementary {

    /**
     * Supplementary character U+20000 See https://www.oracle.com/technical-resources/articles/javase/supplementary.html
     * <p>
     * The UTF-16 character encoding scheme is {@code [high-surrogates][low-surrogates]}.
     * </p>
     * <p>
     * Supplementary characters are encoded in two code units, the first from the high-surrogates range (U+D800 to U+DBFF), the second from the low-surrogates
     * range (U+DC00 to U+DFFF).
     * </p>
     */
    static final String CharU20000 = "\uD840\uDC00";

    /**
     * Supplementary character U+20001 See https://www.oracle.com/technical-resources/articles/javase/supplementary.html
     * <p>
     * The UTF-16 character encoding scheme is {@code [high-surrogates][low-surrogates]}.
     * </p>
     * <p>
     * Supplementary characters are encoded in two code units, the first from the high-surrogates range (U+D800 to U+DBFF), the second from the low-surrogates
     * range (U+DC00 to U+DFFF).
     * </p>
     */
    static final String CharU20001 = "\uD840\uDC01";

    /**
     * Incomplete supplementary character U+20000, <em>high surrogate only</em>.
     * <p>
     * See https://www.oracle.com/technical-resources/articles/javase/supplementary.html
     * </p>
     */
    static final String CharUSuppCharHigh = "\uD840";

    /**
     * Incomplete supplementary character U+20000, <em>low surrogate only</em>.
     * <p>
     * See https://www.oracle.com/technical-resources/articles/javase/supplementary.html
     * </p>
     */
    static final String CharUSuppCharLow = "\uDC00";

}
