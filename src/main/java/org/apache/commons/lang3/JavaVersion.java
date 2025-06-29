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

import org.apache.commons.lang3.math.NumberUtils;

/**
 * An enum representing all the versions of the Java specification.
 * This is intended to mirror available values from the
 * <em>java.specification.version</em> System property.
 *
 * @since 3.0
 */
public enum JavaVersion {

    /**
     * The Java version reported by Android. This is not an official Java version number.
     */
    JAVA_0_9(1.5f, "0.9"),

    /**
     * Java 1.1.
     */
    JAVA_1_1(1.1f, "1.1"),

    /**
     * Java 1.2.
     */
    JAVA_1_2(1.2f, "1.2"),

    /**
     * Java 1.3.
     */
    JAVA_1_3(1.3f, "1.3"),

    /**
     * Java 1.4.
     */
    JAVA_1_4(1.4f, "1.4"),

    /**
     * Java 1.5.
     */
    JAVA_1_5(1.5f, "1.5"),

    /**
     * Java 1.6.
     */
    JAVA_1_6(1.6f, "1.6"),

    /**
     * Java 1.7.
     */
    JAVA_1_7(1.7f, "1.7"),

    /**
     * Java 1.8.
     */
    JAVA_1_8(1.8f, "1.8"),

    /**
     * Java 1.9.
     *
     * @deprecated As of release 3.5, replaced by {@link #JAVA_9}
     */
    @Deprecated
    JAVA_1_9(9.0f, "9"),

    /**
     * Java 9.
     *
     * @since 3.5
     */
    JAVA_9(9.0f, "9"),

    /**
     * Java 10.
     *
     * @since 3.7
     */
    JAVA_10(10.0f, "10"),

    /**
     * Java 11.
     *
     * @since 3.8
     */
    JAVA_11(11.0f, "11"),

    /**
     * Java 12.
     *
     * @since 3.9
     */
    JAVA_12(12.0f, "12"),

    /**
     * Java 13.
     *
     * @since 3.9
     */
    JAVA_13(13.0f, "13"),

    /**
     * Java 14.
     *
     * @since 3.11
     */
    JAVA_14(14.0f, "14"),

    /**
     * Java 15.
     *
     * @since 3.11
     */
    JAVA_15(15.0f, "15"),

    /**
     * Java 16.
     *
     * @since 3.11
     */
    JAVA_16(16.0f, "16"),

    /**
     * Java 17.
     *
     * @since 3.12.0
     */
    JAVA_17(17.0f, "17"),

    /**
     * Java 18.
     *
     * @since 3.13.0
     */
    JAVA_18(18.0f, "18"),

    /**
     * Java 19.
     *
     * @since 3.13.0
     */
    JAVA_19(19.0f, "19"),

    /**
     * Java 20.
     *
     * @since 3.13.0
     */
    JAVA_20(20, "20"),

    /**
     * Java 21.
     *
     * @since 3.13.0
     */
    JAVA_21(21, "21"),

    /**
     * Java 22.
     *
     * @since 3.15.0
     */
    JAVA_22(22, "22"),

    /**
     * Java 23.
     *
     * @since 3.18.0
     */
    JAVA_23(23, "23"),

    /**
     * Java 24.
     *
     * @since 3.18.0
     */
    JAVA_24(24, "24"),

    /**
     * The most recent Java version. Mainly introduced to avoid to break when a new version of Java is used.
     */
    JAVA_RECENT(maxVersion(), Float.toString(maxVersion()));

    /**
     * The regex to split version strings.
     */
    private static final String VERSION_SPLIT_REGEX = "\\.";

    /**
     * Transforms the given string with a Java version number to the
     * corresponding constant of this enumeration class. This method is used
     * internally.
     *
     * @param versionStr the Java version as string
     * @return the corresponding enumeration constant or <strong>null</strong> if the
     * version is unknown
     */
    static JavaVersion get(final String versionStr) {
        if (versionStr == null) {
            return null;
        }
        switch (versionStr) {
        case "0.9":
            return JAVA_0_9;
        case "1.1":
            return JAVA_1_1;
        case "1.2":
            return JAVA_1_2;
        case "1.3":
            return JAVA_1_3;
        case "1.4":
            return JAVA_1_4;
        case "1.5":
            return JAVA_1_5;
        case "1.6":
            return JAVA_1_6;
        case "1.7":
            return JAVA_1_7;
        case "1.8":
            return JAVA_1_8;
        case "9":
            return JAVA_9;
        case "10":
            return JAVA_10;
        case "11":
            return JAVA_11;
        case "12":
            return JAVA_12;
        case "13":
            return JAVA_13;
        case "14":
            return JAVA_14;
        case "15":
            return JAVA_15;
        case "16":
            return JAVA_16;
        case "17":
            return JAVA_17;
        case "18":
            return JAVA_18;
        case "19":
            return JAVA_19;
        case "20":
            return JAVA_20;
        case "21":
            return JAVA_21;
        case "22":
            return JAVA_22;
        case "23":
            return JAVA_23;
        case "24":
            return JAVA_24;
        default:
            final float v = toFloatVersion(versionStr);
            if (v - 1. < 1.) { // then we need to check decimals > .9
                final int firstComma = Math.max(versionStr.indexOf('.'), versionStr.indexOf(','));
                final int end = Math.max(versionStr.length(), versionStr.indexOf(',', firstComma));
                if (Float.parseFloat(versionStr.substring(firstComma + 1, end)) > .9f) {
                    return JAVA_RECENT;
                }
            } else if (v > 10) {
                return JAVA_RECENT;
            }
            return null;
        }
    }

    /**
     * Transforms the given string with a Java version number to the
     * corresponding constant of this enumeration class. This method is used
     * internally.
     *
     * @param versionStr the Java version as string
     * @return the corresponding enumeration constant or <strong>null</strong> if the
     * version is unknown
     */
    static JavaVersion getJavaVersion(final String versionStr) {
        return get(versionStr);
    }

    /**
     * Gets the Java Version from the system or 99.0 if the {@code java.specification.version} system property is not set.
     *
     * @return the value of {@code java.specification.version} system property or 99.0 if it is not set.
     */
    private static float maxVersion() {
        final float v = toFloatVersion(SystemProperties.getJavaSpecificationVersion("99.0"));
        return v > 0 ? v : 99f;
    }

    static String[] split(final String value) {
        return value.split(VERSION_SPLIT_REGEX);
    }

    /**
     * Parses a float value from a String.
     *
     * @param value the String to parse.
     * @return the float value represented by the string or -1 if the given String cannot be parsed.
     */
    private static float toFloatVersion(final String value) {
        final int defaultReturnValue = -1;
        if (!value.contains(".")) {
            return NumberUtils.toFloat(value, defaultReturnValue);
        }
        final String[] toParse = split(value);
        if (toParse.length >= 2) {
            return NumberUtils.toFloat(toParse[0] + '.' + toParse[1], defaultReturnValue);
        }
        return defaultReturnValue;
    }

    /**
     * The float value.
     */
    private final float value;

    /**
     * The standard name.
     */
    private final String name;

    /**
     * Constructs a new instance.
     *
     * @param value  the float value
     * @param name  the standard name, not null
     */
    JavaVersion(final float value, final String name) {
        this.value = value;
        this.name = name;
    }

    /**
     * Tests whether this version of Java is at least the version of Java passed in.
     *
     * <p>For example:<br>
     *  {@code myVersion.atLeast(JavaVersion.JAVA_1_4)}</p>
     *
     * @param requiredVersion  the version to check against, not null
     * @return true if this version is equal to or greater than the specified version
     */
    public boolean atLeast(final JavaVersion requiredVersion) {
        return this.value >= requiredVersion.value;
    }

    /**
     * Tests whether this version of Java is at most the version of Java passed in.
     *
     * <p>For example:<br>
     *  {@code myVersion.atMost(JavaVersion.JAVA_1_4)}</p>
     *
     * @param requiredVersion  the version to check against, not null
     * @return true if this version is equal to or greater than the specified version
     * @since 3.9
     */
    public boolean atMost(final JavaVersion requiredVersion) {
        return this.value <= requiredVersion.value;
    }

    /**
     * The string value is overridden to return the standard name.
     *
     * <p>For example, {@code "1.5"}.</p>
     *
     * @return the name, not null
     */
    @Override
    public String toString() {
        return name;
    }
}
