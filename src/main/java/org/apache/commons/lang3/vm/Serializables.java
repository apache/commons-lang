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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.Serializable;

public class Serializables
{
    private Serializables() {}

    public static byte[] serialize(Serializable serializable)
            throws IOException
    {
        try (ByteArrayOutputStream bos = new ByteArrayOutputStream();
                ObjectOutputStream os = new ObjectOutputStream(bos)
        ) {
            os.writeObject(serializable);
            return bos.toByteArray();
        }
    }

    public static Object byteToObject(byte[] bytes)
            throws IOException, ClassNotFoundException
    {
        return byteToObject(bytes, null);
    }

    public static Object byteToObject(byte[] bytes, ClassLoader classLoader)
            throws IOException, ClassNotFoundException
    {
        try (ByteArrayInputStream bi = new ByteArrayInputStream(bytes);
                ObjectInputStreamProxy oi = new ObjectInputStreamProxy(bi, classLoader)
        ) {
            return oi.readObject();
        }
    }
}
