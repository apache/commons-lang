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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.util.HashMap;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * Unit tests {@link org.apache.commons.lang3.SerializationUtils}.
 *
 * @version $Id$
 */
public class SerializationUtilsTest {

  static final String CLASS_NOT_FOUND_MESSAGE = "ClassNotFoundSerialization.readObject fake exception";
    protected static final String SERIALIZE_IO_EXCEPTION_MESSAGE = "Anonymous OutputStream I/O exception";
  
    private String iString;
    private Integer iInteger;
    private HashMap<Object, Object> iMap;

    @Before
    public void setUp() {
        iString = "foo";
        iInteger = Integer.valueOf(7);
        iMap = new HashMap<Object, Object>();
        iMap.put("FOO", iString);
        iMap.put("BAR", iInteger);
    }

    //-----------------------------------------------------------------------

    @Test
    public void testConstructor() {
        assertNotNull(new SerializationUtils());
        Constructor<?>[] cons = SerializationUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertTrue(Modifier.isPublic(cons[0].getModifiers()));
        assertTrue(Modifier.isPublic(SerializationUtils.class.getModifiers()));
        assertFalse(Modifier.isFinal(SerializationUtils.class.getModifiers()));
    }
    
    @Test
    public void testException() {
        SerializationException serEx;
        Exception ex = new Exception();
        
        serEx = new SerializationException();
        assertSame(null, serEx.getMessage());
        assertSame(null, serEx.getCause());
        
        serEx = new SerializationException("Message");
        assertSame("Message", serEx.getMessage());
        assertSame(null, serEx.getCause());
        
        serEx = new SerializationException(ex);
        assertEquals("java.lang.Exception", serEx.getMessage());
        assertSame(ex, serEx.getCause());
        
        serEx = new SerializationException("Message", ex);
        assertSame("Message", serEx.getMessage());
        assertSame(ex, serEx.getCause());
    }
    
    //-----------------------------------------------------------------------

    @Test
    public void testSerializeStream() throws Exception {
        ByteArrayOutputStream streamTest = new ByteArrayOutputStream();
        SerializationUtils.serialize(iMap, streamTest);

        ByteArrayOutputStream streamReal = new ByteArrayOutputStream();
        ObjectOutputStream oos = new ObjectOutputStream(streamReal);
        oos.writeObject(iMap);
        oos.flush();
        oos.close();

        byte[] testBytes = streamTest.toByteArray();
        byte[] realBytes = streamReal.toByteArray();
        assertEquals(testBytes.length, realBytes.length);
        for (int i = 0; i < realBytes.length; i++) {
            assertEquals(realBytes[i], testBytes[i]);
        }
    }

    @Test
    public void testSerializeStreamUnserializable() throws Exception {
        ByteArrayOutputStream streamTest = new ByteArrayOutputStream();
        try {
            iMap.put(new Object(), new Object());
            SerializationUtils.serialize(iMap, streamTest);
        } catch (SerializationException ex) {
            return;
        }
        fail();
    }

    @Test
    public void testSerializeStreamNullObj() throws Exception {
        ByteArrayOutputStream streamTest = new ByteArrayOutputStream();
        SerializationUtils.serialize(null, streamTest);

        ByteArrayOutputStream streamReal = new ByteArrayOutputStream();
        ObjectOutputStream oos = new ObjectOutputStream(streamReal);
        oos.writeObject(null);
        oos.flush();
        oos.close();

        byte[] testBytes = streamTest.toByteArray();
        byte[] realBytes = streamReal.toByteArray();
        assertEquals(testBytes.length, realBytes.length);
        for (int i = 0; i < realBytes.length; i++) {
            assertEquals(realBytes[i], testBytes[i]);
        }
    }

    @Test
    public void testSerializeStreamObjNull() throws Exception {
        try {
            SerializationUtils.serialize(iMap, null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }

    @Test
    public void testSerializeStreamNullNull() throws Exception {
        try {
            SerializationUtils.serialize(null, null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }
    
    @Test
    public void testSerializeIOException() throws Exception {
        // forces an IOException when the ObjectOutputStream is created, to test not closing the stream
        // in the finally block
        OutputStream streamTest = new OutputStream() {
            @Override
            public void write(int arg0) throws IOException {
                throw new IOException(SERIALIZE_IO_EXCEPTION_MESSAGE);
            }
        };
        try {
            SerializationUtils.serialize(iMap, streamTest);
        }
        catch(SerializationException e) {
            assertEquals("java.io.IOException: " + SERIALIZE_IO_EXCEPTION_MESSAGE, e.getMessage());
        }
    }

    //-----------------------------------------------------------------------

    @Test
    public void testDeserializeStream() throws Exception {
        ByteArrayOutputStream streamReal = new ByteArrayOutputStream();
        ObjectOutputStream oos = new ObjectOutputStream(streamReal);
        oos.writeObject(iMap);
        oos.flush();
        oos.close();

        ByteArrayInputStream inTest = new ByteArrayInputStream(streamReal.toByteArray());
        Object test = SerializationUtils.deserialize(inTest);
        assertNotNull(test);
        assertTrue(test instanceof HashMap<?, ?>);
        assertTrue(test != iMap);
        HashMap<?, ?> testMap = (HashMap<?, ?>) test;
        assertEquals(iString, testMap.get("FOO"));
        assertTrue(iString != testMap.get("FOO"));
        assertEquals(iInteger, testMap.get("BAR"));
        assertTrue(iInteger != testMap.get("BAR"));
        assertEquals(iMap, testMap);
    }

    @Test(expected=ClassCastException.class)
    public void testDeserializeClassCastException() {
        final String value = "Hello";
        byte[] serialized = SerializationUtils.serialize(value);
        Assert.assertEquals(value, SerializationUtils.deserialize(serialized));
        // Causes ClassCastException in call site, not in SerializationUtils.deserialize 
        Integer i = SerializationUtils.deserialize(serialized);
    }

    @Test
    public void testDeserializeStreamOfNull() throws Exception {
        ByteArrayOutputStream streamReal = new ByteArrayOutputStream();
        ObjectOutputStream oos = new ObjectOutputStream(streamReal);
        oos.writeObject(null);
        oos.flush();
        oos.close();

        ByteArrayInputStream inTest = new ByteArrayInputStream(streamReal.toByteArray());
        Object test = SerializationUtils.deserialize(inTest);
        assertNull(test);
    }

    @Test
    public void testDeserializeStreamNull() throws Exception {
        try {
            SerializationUtils.deserialize((InputStream) null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }

    @Test
    public void testDeserializeStreamBadStream() throws Exception {
        try {
            SerializationUtils.deserialize(new ByteArrayInputStream(new byte[0]));
        } catch (SerializationException ex) {
            return;
        }
        fail();
    }

    @Test
    public void testDeserializeStreamClassNotFound() throws Exception {
        ByteArrayOutputStream streamReal = new ByteArrayOutputStream();
        ObjectOutputStream oos = new ObjectOutputStream(streamReal);
        oos.writeObject(new ClassNotFoundSerialization());
        oos.flush();
        oos.close();

        ByteArrayInputStream inTest = new ByteArrayInputStream(streamReal.toByteArray());
        try {
            @SuppressWarnings("unused")
            Object test = SerializationUtils.deserialize(inTest);
        } catch(SerializationException se) {
            assertEquals("java.lang.ClassNotFoundException: " + CLASS_NOT_FOUND_MESSAGE, se.getMessage());
        }
    }
    
    //-----------------------------------------------------------------------

    @Test
    public void testSerializeBytes() throws Exception {
        byte[] testBytes = SerializationUtils.serialize(iMap);

        ByteArrayOutputStream streamReal = new ByteArrayOutputStream();
        ObjectOutputStream oos = new ObjectOutputStream(streamReal);
        oos.writeObject(iMap);
        oos.flush();
        oos.close();

        byte[] realBytes = streamReal.toByteArray();
        assertEquals(testBytes.length, realBytes.length);
        for (int i = 0; i < realBytes.length; i++) {
            assertEquals(realBytes[i], testBytes[i]);
        }
    }

    @Test
    public void testSerializeBytesUnserializable() throws Exception {
        try {
            iMap.put(new Object(), new Object());
            SerializationUtils.serialize(iMap);
        } catch (SerializationException ex) {
            return;
        }
        fail();
    }

    @Test
    public void testSerializeBytesNull() throws Exception {
        byte[] testBytes = SerializationUtils.serialize(null);

        ByteArrayOutputStream streamReal = new ByteArrayOutputStream();
        ObjectOutputStream oos = new ObjectOutputStream(streamReal);
        oos.writeObject(null);
        oos.flush();
        oos.close();

        byte[] realBytes = streamReal.toByteArray();
        assertEquals(testBytes.length, realBytes.length);
        for (int i = 0; i < realBytes.length; i++) {
            assertEquals(realBytes[i], testBytes[i]);
        }
    }

    //-----------------------------------------------------------------------

    @Test
    public void testDeserializeBytes() throws Exception {
        ByteArrayOutputStream streamReal = new ByteArrayOutputStream();
        ObjectOutputStream oos = new ObjectOutputStream(streamReal);
        oos.writeObject(iMap);
        oos.flush();
        oos.close();

        Object test = SerializationUtils.deserialize(streamReal.toByteArray());
        assertNotNull(test);
        assertTrue(test instanceof HashMap<?, ?>);
        assertTrue(test != iMap);
        HashMap<?, ?> testMap = (HashMap<?, ?>) test;
        assertEquals(iString, testMap.get("FOO"));
        assertTrue(iString != testMap.get("FOO"));
        assertEquals(iInteger, testMap.get("BAR"));
        assertTrue(iInteger != testMap.get("BAR"));
        assertEquals(iMap, testMap);
    }

    @Test
    public void testDeserializeBytesOfNull() throws Exception {
        ByteArrayOutputStream streamReal = new ByteArrayOutputStream();
        ObjectOutputStream oos = new ObjectOutputStream(streamReal);
        oos.writeObject(null);
        oos.flush();
        oos.close();

        Object test = SerializationUtils.deserialize(streamReal.toByteArray());
        assertNull(test);
    }

    @Test
    public void testDeserializeBytesNull() throws Exception {
        try {
            SerializationUtils.deserialize((byte[]) null);
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }

    @Test
    public void testDeserializeBytesBadStream() throws Exception {
        try {
            SerializationUtils.deserialize(new byte[0]);
        } catch (SerializationException ex) {
            return;
        }
        fail();
    }

    //-----------------------------------------------------------------------

    @Test
    public void testClone() throws Exception {
        Object test = SerializationUtils.clone(iMap);
        assertNotNull(test);
        assertTrue(test instanceof HashMap<?,?>);
        assertTrue(test != iMap);
        HashMap<?, ?> testMap = (HashMap<?, ?>) test;
        assertEquals(iString, testMap.get("FOO"));
        assertTrue(iString != testMap.get("FOO"));
        assertEquals(iInteger, testMap.get("BAR"));
        assertTrue(iInteger != testMap.get("BAR"));
        assertEquals(iMap, testMap);
    }

    @Test
    public void testCloneNull() throws Exception {
        Object test = SerializationUtils.clone(null);
        assertNull(test);
    }

    @Test
    public void testCloneUnserializable() throws Exception {
        try {
            iMap.put(new Object(), new Object());
            SerializationUtils.clone(iMap);
        } catch (SerializationException ex) {
            return;
        }
        fail();
    }
    
    @Test
    public void testPrimitiveTypeClassSerialization() {
        Class<?>[] primitiveTypes = { byte.class, short.class, int.class, long.class, float.class, double.class,
                boolean.class, char.class, void.class };

        for (Class<?> primitiveType : primitiveTypes) {
            Class<?> clone = SerializationUtils.clone(primitiveType);
            assertEquals(primitiveType, clone);
        }
    }

}

@SuppressWarnings("serial")
class ClassNotFoundSerialization implements Serializable
{

    private void readObject(ObjectInputStream in) throws ClassNotFoundException    {
        throw new ClassNotFoundException(SerializationUtilsTest.CLASS_NOT_FOUND_MESSAGE);
    }
}
