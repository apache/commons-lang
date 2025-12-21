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

import static org.junit.jupiter.api.Assumptions.assumeTrue;

import java.text.ParseException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Offers tests a way to skip problematic Locales with a JUnit assumption.
 */
public class LocaleProblems {

    // needs a better name.
    private static final Map<JavaVersion, List<String>> UNSUPPORTED_CAT_A;
    static {
        UNSUPPORTED_CAT_A = new HashMap<>();
        // "cv_RU" is a problem on Java version: 26-beta, vendor: Eclipse Adoptium, runtime: /opt/hostedtoolcache/Java_Temurin-Hotspot_jdk/26.0.0-ea.27.0.ea/x64
        // Same for "cv"
        UNSUPPORTED_CAT_A.put(JavaVersion.JAVA_26, Arrays.asList("cv", "cv_RU"));
    }

    // needs a better name.
    private static final Map<JavaVersion, List<String>> UNSUPPORTED_CAT_B;
    static {
        UNSUPPORTED_CAT_B = new HashMap<>();
        // "cv_RU_#Cyrl" is a problem on Java version: 26-beta, vendor: Eclipse Adoptium, runtime: /opt/hostedtoolcache/Java_Temurin-Hotspot_jdk/26.0.0-ea.27.0.ea/x64
        UNSUPPORTED_CAT_B.put(JavaVersion.JAVA_26, Arrays.asList("cv", "cv_RU", "cv_RU_#Cyrl"));
    }

    private static void assumeLocaleSupported(final Locale locale, final Map<JavaVersion, List<String>> unsupportedCatA, final ParseException e) {
        final boolean supported = isSupported(locale, unsupportedCatA);
        if (!supported) {
            System.out.printf("Failing test assumption for locale '%s' on Java version %s.%n", locale, SystemUtils.JAVA_SPECIFICATION_VERSION_ENUM);
            if (e != null) {
                // Not the whole stack trace, just the exception message.
                System.out.printf("\t%s%n", e);
            }
        }
        assumeTrue(supported);
    }

    public static void assumeLocaleSupportedA(final Locale locale) {
        assumeLocaleSupported(locale, UNSUPPORTED_CAT_A, null);
    }

    public static void assumeLocaleSupportedB(final Locale locale, final ParseException e) {
        assumeLocaleSupported(locale, UNSUPPORTED_CAT_B, e);
    }

    private static boolean isSupported(final Locale locale, final Map<JavaVersion, List<String>> unsupported) {
        final List<String> list = unsupported.get(SystemUtils.JAVA_SPECIFICATION_VERSION_ENUM);
        return list == null || !list.contains(locale.toString());
    }
}
