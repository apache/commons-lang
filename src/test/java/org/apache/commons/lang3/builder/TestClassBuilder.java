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

package org.apache.commons.lang3.builder;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * Builds classes dynamically for tests.
 */
public class TestClassBuilder {

    /**
     * Extends {@link ClassLoader} to make {@link ClassLoader#defineClass(String, byte[])} public.
     */
    static final class DynamicClassLoader extends ClassLoader {
        public Class<?> defineClass(final String name, final byte[] b) {
            return defineClass(name, b, 0, b.length);
        }
    }

    /**
     * Defines the simplest possible class.
     *
     * @param name The class name.
     * @return The new class.
     */
    static Class<?> defineSimpleClass(final String name) {
        final ClassWriter classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS);
        classWriter.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, name, null, "java/lang/Object", new String[] {});
        final MethodVisitor ctor = classWriter.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", null, null);
        ctor.visitCode();
        ctor.visitVarInsn(Opcodes.ALOAD, 0);
        ctor.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false);
        ctor.visitInsn(Opcodes.RETURN);
        ctor.visitMaxs(1, 1);
        return new DynamicClassLoader().defineClass(name.replace('/', '.'), classWriter.toByteArray());
    }

    static Class<?> defineSimpleClass(final String packageName, final int i) {
        return defineSimpleClass(packageName.replace('.', '/') + "/C" + i);
    }

}
