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
 * <p>An enum representing all the versions of the Java specification.
 * This is intended to mirror available values from the 
 * <em>java.specification.version</em> System property. </p>
 *
 * @author Apache Software Foundation
 * @since 3.0
 * @version $Id: $
 */
public enum JavaVersion {
    JAVA_0_9(1.5f, "0.9"),    // Android 
    JAVA_1_1(1.1f, "1.1"), 
    JAVA_1_2(1.2f, "1.2"), 
    JAVA_1_3(1.3f, "1.3"), 
    JAVA_1_4(1.4f, "1.4"), 
    JAVA_1_5(1.5f, "1.5"), 
    JAVA_1_6(1.6f, "1.6"), 
    JAVA_1_7(1.7f, "1.7"),
    JAVA_1_8(1.8f, "1.8");

    private float value;
    private String name;

    JavaVersion(final float value, final String name) {
        this.value = value;
        this.name = name;
    }

    /**
     * Whether this version of Java is at least the version 
     * of Java passed in.
     *
     * For example:
     *  myVersion.atLeast(JavaVersion.JAVA_1_4)
     *
     * @param requiredVersion to check this version is at least equivalent to
     *
     * @return Whether this version is at least the passed in version
     */
    public boolean atLeast(JavaVersion requiredVersion) {
        return this.value >= requiredVersion.value;
    }

    // helper for static importing
    static JavaVersion getJavaVersion(final String nom) {
        return get(nom);
    }
    static JavaVersion get(final String nom) {
        if("0.9".equals(nom)) {
            return JAVA_0_9;
        } else
        if("1.1".equals(nom)) {
            return JAVA_1_1;
        } else
        if("1.2".equals(nom)) {
            return JAVA_1_2;
        } else
        if("1.3".equals(nom)) {
            return JAVA_1_3;
        } else
        if("1.4".equals(nom)) {
            return JAVA_1_4;
        } else
        if("1.5".equals(nom)) {
            return JAVA_1_5;
        } else
        if("1.6".equals(nom)) {
            return JAVA_1_6;
        } else
        if("1.7".equals(nom)) {
            return JAVA_1_7;
        } else
        if("1.8".equals(nom)) {
            return JAVA_1_8;
        } else {
            return null;
        }
    }
}
