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

package org.apache.commons.lang3.reflect;

import org.junit.jupiter.api.Test;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import static org.junit.jupiter.api.Assertions.*;

class MemberUtilsTest {

    static class TestClass {
        public TestClass() {
        }

        public TestClass(int a) {
        }

        public void methodA() {
        }

        public void methodA(int a) {
        }

        public void methodA(double a) {
        }
    }

    @Test
    void testCompareMethodFit() throws Exception {
        Method method1 = TestClass.class.getMethod("methodA", int.class);
        Method method2 = TestClass.class.getMethod("methodA", double.class);
        Class<?>[] actualParams = { int.class };

        int comparison = MemberUtils.compareMethodFit(method1, method2, actualParams);
        assertTrue(comparison < 0, "Method with int parameter should be a better fit than double");
    }

    @Test
    void testCompareConstructorFit() throws Exception {
        Constructor<?> ctor1 = TestClass.class.getConstructor();
        Constructor<?> ctor2 = TestClass.class.getConstructor(int.class);
        Class<?>[] actualParams = { int.class };

        int comparison = MemberUtils.compareConstructorFit(ctor1, ctor2, actualParams);

        assertTrue(comparison >= 0,
                "Expected the constructor with an int parameter to be a better fit, but got: " + comparison);
    }

    @Test
    void testIsAccessible() throws Exception {
        Method method = TestClass.class.getMethod("methodA");
        assertTrue(MemberUtils.isAccessible(method));
    }

    @Test
    void testIsPublic() throws Exception {
        Method method = TestClass.class.getMethod("methodA");
        assertTrue(MemberUtils.isPublic(method));
    }

    @Test
    void testIsStatic() throws Exception {
        Method method = TestClass.class.getMethod("methodA");
        assertFalse(MemberUtils.isStatic(method));
    }
}
