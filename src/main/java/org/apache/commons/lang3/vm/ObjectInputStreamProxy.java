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
package org.apache.commons.lang3.vm;

import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectStreamClass;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Map;

public class ObjectInputStreamProxy
        extends java.io.ObjectInputStream
{
    private ClassLoader classLoader;

    public ObjectInputStreamProxy(InputStream in)
            throws IOException
    {
        super(in);
    }

    /**
     * ObjectInputStreamProxy used by user classLoader
     * <p>
     *
     * @param classLoader used by loadObject
     */
    public ObjectInputStreamProxy(InputStream in, ClassLoader classLoader)
            throws IOException
    {
        super(in);
        this.classLoader = classLoader;
    }

    /**
     * get Method LatestUserDefinedLoader with java.io.ObjectInputStreamProxy
     * with jdk.internal.misc.VM.latestUserDefinedLoader()
     */
    public static ClassLoader getLatestUserDefinedLoader()
    {
        //super.latestUserDefinedLoader();
        Class<?> class1 = java.io.ObjectInputStream.class;
        try {
            Method method = class1.getDeclaredMethod("latestUserDefinedLoader");
            method.setAccessible(true);
            return (ClassLoader) method.invoke(null);
        }
        catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
            throw new RuntimeException("Not compatible with java version");
        }
    }

    /**
     * get field primClasses with java.io.ObjectInputStreamProxy
     */
    private static Map<String, Class<?>> getPrimClasses()
    {
        Class<?> class1 = java.io.ObjectInputStream.class;
        Map<String, Class<?>> primClasses = null;
        try {
            Field field = class1.getDeclaredField("primClasses");
            field.setAccessible(true);
            primClasses = (Map<String, Class<?>>) field.get(class1);
            return primClasses;
        }
        catch (NoSuchFieldException | IllegalAccessException e) {
            throw new RuntimeException("Not compatible with java version");
        }
    }

    @Override
    protected Class<?> resolveClass(ObjectStreamClass desc)
            throws IOException, ClassNotFoundException
    {
        if (classLoader == null) {
            return super.resolveClass(desc);
        }

        //return super.resolveClass(desc);
        String name = desc.getName();
        try {
            return Class.forName(name, false, classLoader);
        }
        catch (ClassNotFoundException ex) {
            Class<?> cl = getPrimClasses().get(name);
            if (cl != null) {
                return cl;
            }
            else {
                throw ex;
            }
        }
    }
}
