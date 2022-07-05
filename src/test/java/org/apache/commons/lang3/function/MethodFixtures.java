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

package org.apache.commons.lang3.function;

import java.io.IOException;
import java.lang.reflect.Method;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.exception.CustomCheckedException;
import org.apache.commons.lang3.exception.CustomUncheckedException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;

class MethodFixtures extends AbstractLangTest {

    static MethodFixtures INSTANCE = new MethodFixtures();

    static Method getDeclaredMethod(final String name, final Class<?>... parameterTypes) throws NoSuchMethodException, SecurityException {
        return MethodFixtures.class.getDeclaredMethod(name, parameterTypes);
    }

    static Method getMethodForGetString() throws NoSuchMethodException, SecurityException {
        return getDeclaredMethod("getString");
    }

    static Method getMethodForGetString1Arg() throws NoSuchMethodException, SecurityException {
        return getDeclaredMethod("getString1Arg", String.class);
    }

    static Method getMethodForGetString1ArgChecked() throws NoSuchMethodException, SecurityException {
        return getDeclaredMethod("getString1ArgChecked", String.class);
    }

    static Method getMethodForGetString1ArgThrowsChecked() throws NoSuchMethodException, SecurityException {
        return getDeclaredMethod("getString1ArgThrowsChecked", String.class);
    }

    static Method getMethodForGetString1ArgThrowsUnchecked() throws NoSuchMethodException, SecurityException {
        return getDeclaredMethod("getString1ArgThrowsUnchecked", String.class);
    }

    static Method getMethodForGetString2Arg() throws NoSuchMethodException, SecurityException {
        return getDeclaredMethod("getString2Args", String.class, String.class);
    }

    static Method getMethodForGetStringChecked() throws NoSuchMethodException, SecurityException {
        return getDeclaredMethod("getStringChecked");
    }

    static Method getMethodForGetStringsVarArg() throws NoSuchMethodException, SecurityException {
        return getDeclaredMethod("getStringArrayVarStringArgs", String[].class);
    }

    static Method getMethodForGetStringThrowsChecked() throws NoSuchMethodException, SecurityException {
        return getDeclaredMethod("getStringThrowsChecked");
    }

    static Method getMethodForGetStringThrowsUnchecked() throws NoSuchMethodException, SecurityException {
        return getDeclaredMethod("getStringThrowsUnchecked");
    }

    static Method getMethodForGetStringVarStringArgs() throws NoSuchMethodException, SecurityException {
        return getDeclaredMethod("geStringtVarStringArgs", String[].class);
    }

    static Method getMethodForSetString1Arg() throws NoSuchMethodException, SecurityException {
        return getDeclaredMethod("setValue1", String.class);
    }

    static Method getMethodForSetString1ArgThrows() throws NoSuchMethodException, SecurityException {
        return getDeclaredMethod("setValue1Throws", String.class);
    }

    static Method getMethodForSetString1ArgThrowsChecked() throws NoSuchMethodException, SecurityException {
        return getDeclaredMethod("setValue1ThrowsChecked", String.class);
    }

    static Method getMethodForSetString1ArgThrowsUnchecked() throws NoSuchMethodException, SecurityException {
        return getDeclaredMethod("setValue1ThrowsUnchecked", String.class);
    }

    static Method getMethodForSetString2Args() throws NoSuchMethodException, SecurityException {
        return getDeclaredMethod("setValue1And2", String.class, String.class);
    }

    static Method getMethodForSetStringsVarArg() throws NoSuchMethodException, SecurityException {
        return getDeclaredMethod("setValueArray", String[].class);
    }

    static Method getMethodForStaticGetString() throws NoSuchMethodException, SecurityException {
        return getDeclaredMethod("staticGetString");
    }

    static Method getMethodForVoidMethod() throws NoSuchMethodException, SecurityException {
        return getDeclaredMethod("voidMethod");
    }

    public static String staticGetString() {
        return "Static.ABC";
    }

    private String value1;

    private String value2;

    private String[] valueArray;

    @BeforeEach
    @AfterEach
    public void clear() {
        value1 = null;
        value1 = null;
        valueArray = null;
    }

    public String geStringtVarStringArgs(final String... strings) {
        return "XYZ";
    }

    @AnnotationTestFixture
    public String getString() {
        return "ABC";
    }

    public String getString1Arg(final String value) {
        return value;
    }

    @SuppressWarnings("unused") // IOException is declared but never thrown.
    public String getString1ArgChecked(final String value) throws IOException {
        return value;
    }

    public String getString1ArgThrowsChecked(final String value) throws CustomCheckedException {
        throw new CustomCheckedException("getString1ArgThrowsChecked");
    }

    public String getString1ArgThrowsUnchecked(final String value) {
        throw new CustomUncheckedException("getString1ArgThrowsUnchecked");
    }

    @AnnotationTestFixture
    public String getString2() {
        return "EFG";
    }

    public String getString2Args(final String value1, final String value2) {
        return value1 + value2;
    }

    public String[] getStringArrayVarStringArgs(final String... strings) {
        return strings;
    }

    public String getStringChecked() throws Exception {
        return "ABC";
    }

    public String getStringThrowsChecked() throws CustomCheckedException {
        throw new CustomCheckedException("getStringThrowsChecked");
    }

    public String getStringThrowsUnchecked() {
        throw new CustomUncheckedException("getStringThrowsUnchecked");
    }

    String getValue1() {
        return value1;
    }

    String getValue2() {
        return value2;
    }

    String[] getValueArray() {
        return valueArray;
    }

    void setValue1(final String value1) throws Exception {
        this.value1 = value1;
    }

    void setValue1And2(final String value1, final String value2) throws Exception {
        this.value1 = value1;
        this.value2 = value2;
    }

    void setValue1ThrowsChecked(final String value1) throws CustomCheckedException {
        throw new CustomCheckedException("setValue1ThrowsChecked");
    }

    void setValue1ThrowsUnchecked(final String value1) {
        throw new CustomUncheckedException("setValue1ThrowsUnchecked");
    }

    void setValue2(final String value2) {
        this.value2 = value2;
    }

    void setValueArray(final String... values) throws Exception {
        this.valueArray = values;
    }

    public void voidMethod() {
        // noop
    }

}
