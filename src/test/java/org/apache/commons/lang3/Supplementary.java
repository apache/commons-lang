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

package org.apache.commons.lang3;

/**
 * Supplementary character test fixtures.
 *
 * See https://www.oracle.com/technical-resources/articles/javase/supplementary.html
 */
public class Supplementary {

    /**
     * Supplementary character U+20000 See https://www.oracle.com/technical-resources/articles/javase/supplementary.html
     */
    static final String CharU20000 = "\uD840\uDC00";

    /**
     * Supplementary character U+20001 See https://www.oracle.com/technical-resources/articles/javase/supplementary.html
     */
    static final String CharU20001 = "\uD840\uDC01";

    /**
     * Incomplete supplementary character U+20000, high surrogate only. See
     * https://www.oracle.com/technical-resources/articles/javase/supplementary.html
     */
    static final String CharUSuppCharHigh = "\uDC00";

    /**
     * Incomplete supplementary character U+20000, low surrogate only. See
     * https://www.oracle.com/technical-resources/articles/javase/supplementary.html
     */
    static final String CharUSuppCharLow = "\uD840";

}
