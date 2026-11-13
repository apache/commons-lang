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

import static org.apache.commons.lang3.LangAssertions.assertNullPointerException;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Proxy;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.function.Supplier;

import org.apache.commons.lang3.reflect.testbed.Bar;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

final class ClassNotFoundSerialization implements Serializable {

    private static final long serialVersionUID = 1L;

    private void readObject(final ObjectInputStream in) throws ClassNotFoundException    {
        throw new ClassNotFoundException(SerializationUtilsTest.CLASS_NOT_FOUND_MESSAGE);
    }
}

interface SerializableSupplier<T> extends Supplier<T>, Serializable {
    // empty
}

/**
 * Tests {@link SerializationUtils}.
 */
public class SerializationUtilsTest extends AbstractLangTest {

    public static final class ProxyHolder implements Serializable {

        private static final long serialVersionUID = 1L;

        public final Serializable value;

        public ProxyHolder(final Serializable value) {
            this.value = value;
        }
    }

    private static final class SerializableInvocationHandler implements InvocationHandler, Serializable {

        private static final long serialVersionUID = 1L;

        @Override
        public Object invoke(final Object proxy, final Method method, final Object[] args) {
            return "test";
        }
    }

    static final String CLASS_NOT_FOUND_MESSAGE = "ClassNotFoundSerialization.readObject fake exception";

    protected static final String SERIALIZE_IO_EXCEPTION_MESSAGE = "Anonymous OutputStream I/O exception";

    public static byte[] intToBytes(final int v) {
        return new byte[] { (byte) (v >>> 24), (byte) (v >>> 16), (byte) (v >>> 8), (byte) v };
    }

    public static byte[] replaceLastInt(final byte[] src, final int from, final int to) {
        final byte[] fromB = intToBytes(from);
        final byte[] toB = intToBytes(to);
        final byte[] out = src.clone();
        for (int i = out.length - 4; i >= 0; i--) {
            if (out[i] == fromB[0] && out[i + 1] == fromB[1] && out[i + 2] == fromB[2] && out[i + 3] == fromB[3]) {
                out[i] = toB[0];
                out[i + 1] = toB[1];
                out[i + 2] = toB[2];
                out[i + 3] = toB[3];
                return out;
            }
        }
        fail("No legitimate int in stream, serialization must keep hashCode in default field set");
        return null;
    }

    private String iString;

    private Integer iInteger;

    private HashMap<Object, Object> iMap;

    @BeforeEach
    public void setUp() {
        iString = "foo";
        iInteger = Integer.valueOf(7);
        iMap = new HashMap<>();
        iMap.put("FOO", iString);
        iMap.put("BAR", iInteger);
    }

    @Test
    void testClone() {
        final Object test = SerializationUtils.clone(iMap);
        assertNotNull(test);
        assertInstanceOf(HashMap.class, test);
        assertNotSame(test, iMap);
        final HashMap<?, ?> testMap = (HashMap<?, ?>) test;
        assertEquals(iString, testMap.get("FOO"));
        assertNotSame(iString, testMap.get("FOO"));
        assertEquals(iInteger, testMap.get("BAR"));
        assertNotSame(iInteger, testMap.get("BAR"));
        assertEquals(iMap, testMap);
    }

    @Test
    void testCloneClassNotFound() {
        final SerializationException ex = assertThrows(SerializationException.class, () -> SerializationUtils.clone(new ClassNotFoundSerialization()));
        assertInstanceOf(ClassNotFoundException.class, ex.getCause());
        assertEquals(CLASS_NOT_FOUND_MESSAGE, ex.getCause().getMessage());
    }

    @Test
    void testCloneNull() {
        final Object test = SerializationUtils.clone(null);
        assertNull(test);
    }

    @ParameterizedTest
    @ValueSource(strings = { "org.apache.commons.lang3.reflect.testbed.Bar", "org.apache.commons.lang3.SerializableSupplier" })
    void testCloneProxyWithCustomClassLoader(final String interfaceName) throws Exception {
        try (URLClassLoader loader = new URLClassLoader(new URL[] { getClass().getProtectionDomain().getCodeSource().getLocation() }, null)) {
            final Class<?> proxyInterface = Class.forName(interfaceName, false, loader);
            final Serializable proxy = (Serializable) Proxy.newProxyInstance(loader, new Class<?>[] { proxyInterface }, new SerializableInvocationHandler());
            final Serializable clone = SerializationUtils.clone(proxy);
            assertNotSame(proxy, clone);
            assertSame(proxy.getClass(), clone.getClass());
            assertTrue(proxyInterface.isInstance(clone));
            assertNotSame(Proxy.getInvocationHandler(proxy), Proxy.getInvocationHandler(clone));
            assertEquals("test", clone.toString());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = { "org.apache.commons.lang3.reflect.testbed.Bar", "org.apache.commons.lang3.SerializableSupplier" })
    void testCloneProxyWithContextClassLoader(final String interfaceName) throws Exception {
        final Thread thread = Thread.currentThread();
        final ClassLoader originalLoader = thread.getContextClassLoader();
        try (URLClassLoader loader = new URLClassLoader(new URL[] { getClass().getProtectionDomain().getCodeSource().getLocation() }, null)) {
            final Class<?> proxyInterface = Class.forName(interfaceName, false, loader);
            final Serializable proxy = (Serializable) Proxy.newProxyInstance(loader, new Class<?>[] { proxyInterface }, new SerializableInvocationHandler());
            final ArrayList<Serializable> holder = new ArrayList<>();
            holder.add(proxy);
            thread.setContextClassLoader(loader);
            final Serializable clone = SerializationUtils.clone(holder).get(0);
            assertNotSame(proxy, clone);
            assertSame(proxy.getClass(), clone.getClass());
            assertEquals("test", clone.toString());
        } finally {
            thread.setContextClassLoader(originalLoader);
        }
    }

    @Test
    void testCloneProxyWithDefaultClassLoader() {
        final Serializable proxy = (Serializable) Proxy.newProxyInstance(getClass().getClassLoader(), new Class<?>[] { SerializableSupplier.class },
                new SerializableInvocationHandler());
        final SerializableSupplier<?> clone = (SerializableSupplier<?>) SerializationUtils.clone(proxy);
        assertNotSame(proxy, clone);
        assertEquals("test", clone.get());
    }

    @ParameterizedTest
    @ValueSource(strings = { "org.apache.commons.lang3.reflect.testbed.Bar", "org.apache.commons.lang3.SerializableSupplier" })
    void testCloneProxiesFromDifferentClassLoaders(final String interfaceName) throws Exception {
        final URL[] urls = { getClass().getProtectionDomain().getCodeSource().getLocation() };
        try (URLClassLoader first = new URLClassLoader(urls, null);
                URLClassLoader second = new URLClassLoader(urls, null)) {
            final Serializable firstProxy = (Serializable) Proxy.newProxyInstance(first,
                    new Class<?>[] { Class.forName(interfaceName, false, first) }, new SerializableInvocationHandler());
            final Serializable secondProxy = (Serializable) Proxy.newProxyInstance(second,
                    new Class<?>[] { Class.forName(interfaceName, false, second) }, new SerializableInvocationHandler());
            final ArrayList<Serializable> values = new ArrayList<>();
            values.add(firstProxy.getClass());
            values.add(firstProxy);
            values.add(secondProxy);
            values.add(firstProxy);
            values.add(secondProxy.getClass());
            final ArrayList<Serializable> clone = SerializationUtils.clone(values);
            assertSame(firstProxy.getClass(), clone.get(0));
            assertNotSame(firstProxy, clone.get(1));
            assertNotSame(secondProxy, clone.get(2));
            assertSame(firstProxy.getClass(), clone.get(1).getClass());
            assertSame(secondProxy.getClass(), clone.get(2).getClass());
            assertSame(clone.get(1), clone.get(3));
            assertSame(secondProxy.getClass(), clone.get(4));
            assertEquals("test", clone.get(1).toString());
            assertEquals("test", clone.get(2).toString());
        }
    }

    @Test
    void testCloneProxyDoesNotLookUpInterfaces() throws Exception {
        final int[] interfaceLookups = { 0 };
        final URL[] urls = { getClass().getProtectionDomain().getCodeSource().getLocation() };
        final ClassLoader applicationLoader = getClass().getClassLoader();
        final Thread thread = Thread.currentThread();
        final ClassLoader originalLoader = thread.getContextClassLoader();
        try (URLClassLoader loader = new URLClassLoader(urls, applicationLoader) {
            @Override
            protected Class<?> loadClass(final String name, final boolean resolve) throws ClassNotFoundException {
                if (name.equals(Bar.class.getName())) {
                    interfaceLookups[0]++;
                    throw new ClassNotFoundException(name);
                }
                if (name.equals(ProxyHolder.class.getName())) {
                    Class<?> cls = findLoadedClass(name);
                    if (cls == null) {
                        cls = findClass(name);
                    }
                    if (resolve) {
                        resolveClass(cls);
                    }
                    return cls;
                }
                return super.loadClass(name, resolve);
            }
        }) {
            final Serializable proxy = (Serializable) Proxy.newProxyInstance(applicationLoader, new Class<?>[] { Bar.class },
                    new SerializableInvocationHandler());
            final Class<?> holderClass = Class.forName(ProxyHolder.class.getName(), true, loader);
            final Serializable holder = (Serializable) holderClass.getConstructor(Serializable.class).newInstance(proxy);
            thread.setContextClassLoader(applicationLoader);
            final Serializable clone = SerializationUtils.clone(holder);
            final Object clonedProxy = holderClass.getField("value").get(clone);
            assertSame(proxy.getClass(), clonedProxy.getClass());
            assertNotSame(proxy, clonedProxy);
            assertEquals("test", clonedProxy.toString());
            assertEquals(0, interfaceLookups[0]);
        } finally {
            thread.setContextClassLoader(originalLoader);
        }
    }

    @Test
    void testCloneProxyWithPublicInterfaces() {
        final Serializable proxy = (Serializable) Proxy.newProxyInstance(getClass().getClassLoader(),
                new Class<?>[] { Supplier.class, Serializable.class }, new SerializableInvocationHandler());
        final Supplier<?> clone = (Supplier<?>) SerializationUtils.clone(proxy);
        assertNotSame(proxy, clone);
        assertSame(proxy.getClass(), clone.getClass());
        assertEquals("test", clone.get());
    }

    @Test
    void testCloneProxyWithNoInterfaces() {
        final Serializable proxy = (Serializable) Proxy.newProxyInstance(getClass().getClassLoader(), new Class<?>[0], new SerializableInvocationHandler());
        final Serializable clone = SerializationUtils.clone(proxy);
        assertNotSame(proxy, clone);
        assertSame(proxy.getClass(), clone.getClass());
        assertNotSame(Proxy.getInvocationHandler(proxy), Proxy.getInvocationHandler(clone));
        assertEquals("test", clone.toString());
    }

    @Test
    void testCloneSerializableSupplier() {
        final SerializableSupplier<String> supplier = () -> "test";
        assertEquals("test", supplier.get());
        final SerializableSupplier<String> clone = SerializationUtils.clone(supplier);
        assertEquals("test", clone.get());
    }

    @Test
    void testCloneUnserializable() {
        iMap.put(new Object(), new Object());
        assertThrows(SerializationException.class, () -> SerializationUtils.clone(iMap));
    }

    @SuppressWarnings("deprecation")
    @Test
    void testConstructor() {
        assertNotNull(new SerializationUtils());
        final Constructor<?>[] cons = SerializationUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertTrue(Modifier.isPublic(cons[0].getModifiers()));
        assertTrue(Modifier.isPublic(SerializationUtils.class.getModifiers()));
        assertFalse(Modifier.isFinal(SerializationUtils.class.getModifiers()));
    }

    @Test
    void testDeserializeBytes() throws Exception {
        final ByteArrayOutputStream streamReal = new ByteArrayOutputStream();
        try (ObjectOutputStream oos = new ObjectOutputStream(streamReal)) {
            oos.writeObject(iMap);
            oos.flush();
        }

        final Object test = SerializationUtils.deserialize(streamReal.toByteArray());
        assertNotNull(test);
        assertInstanceOf(HashMap.class, test);
        assertNotSame(test, iMap);
        final HashMap<?, ?> testMap = (HashMap<?, ?>) test;
        assertEquals(iString, testMap.get("FOO"));
        assertNotSame(iString, testMap.get("FOO"));
        assertEquals(iInteger, testMap.get("BAR"));
        assertNotSame(iInteger, testMap.get("BAR"));
        assertEquals(iMap, testMap);
    }

    @Test
    void testDeserializeBytesBadStream() {
        assertThrows(SerializationException.class, () -> SerializationUtils.deserialize(new byte[0]));
    }

    @Test
    void testDeserializeBytesNull() {
        assertNullPointerException(() -> SerializationUtils.deserialize((byte[]) null));
    }

    @Test
    void testDeserializeBytesOfNull() throws Exception {
        final ByteArrayOutputStream streamReal = new ByteArrayOutputStream();
        try (ObjectOutputStream oos = new ObjectOutputStream(streamReal)) {
            oos.writeObject(null);
            oos.flush();
        }

        final Object test = SerializationUtils.deserialize(streamReal.toByteArray());
        assertNull(test);
    }

    @Test
    void testDeserializeClassCastException() {
        final String value = "Hello";
        final byte[] serialized = SerializationUtils.serialize(value);
        assertEquals(value, SerializationUtils.deserialize(serialized));
        assertThrows(ClassCastException.class, () -> {
            // Causes ClassCastException in call site, not in SerializationUtils.deserialize
            @SuppressWarnings("unused") // needed to cause Exception
            final Integer i = SerializationUtils.deserialize(serialized);
        });
    }

    @Test
    void testDeserializeStream() throws Exception {
        final ByteArrayOutputStream streamReal = new ByteArrayOutputStream();
        try (ObjectOutputStream oos = new ObjectOutputStream(streamReal)) {
            oos.writeObject(iMap);
            oos.flush();
        }

        final ByteArrayInputStream inTest = new ByteArrayInputStream(streamReal.toByteArray());
        final Object test = SerializationUtils.deserialize(inTest);
        assertNotNull(test);
        assertInstanceOf(HashMap.class, test);
        assertNotSame(test, iMap);
        final HashMap<?, ?> testMap = (HashMap<?, ?>) test;
        assertEquals(iString, testMap.get("FOO"));
        assertNotSame(iString, testMap.get("FOO"));
        assertEquals(iInteger, testMap.get("BAR"));
        assertNotSame(iInteger, testMap.get("BAR"));
        assertEquals(iMap, testMap);
    }

    @Test
    void testDeserializeStreamBadStream() {
        assertThrows(SerializationException.class, () -> SerializationUtils.deserialize(new ByteArrayInputStream(new byte[0])));
    }

    @Test
    void testDeserializeStreamClassNotFound() throws Exception {
        final ByteArrayOutputStream streamReal = new ByteArrayOutputStream();
        try (ObjectOutputStream oos = new ObjectOutputStream(streamReal)) {
            oos.writeObject(new ClassNotFoundSerialization());
            oos.flush();
        }

        final ByteArrayInputStream inTest = new ByteArrayInputStream(streamReal.toByteArray());
        final SerializationException se = assertThrows(SerializationException.class, () -> SerializationUtils.deserialize(inTest));
        assertEquals("java.lang.ClassNotFoundException: " + CLASS_NOT_FOUND_MESSAGE, se.getMessage());
    }

    @Test
    void testDeserializeStreamNull() {
        assertNullPointerException(() -> SerializationUtils.deserialize((InputStream) null));
    }

    @Test
    void testDeserializeStreamOfNull() throws Exception {
        final ByteArrayOutputStream streamReal = new ByteArrayOutputStream();
        try (ObjectOutputStream oos = new ObjectOutputStream(streamReal)) {
            oos.writeObject(null);
            oos.flush();
        }

        final ByteArrayInputStream inTest = new ByteArrayInputStream(streamReal.toByteArray());
        final Object test = SerializationUtils.deserialize(inTest);
        assertNull(test);
    }

    @Test
    void testException() {
        SerializationException serEx;
        final Exception ex = new Exception();

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

    @Test
    void testNegativeByteArray() {
        final byte[] byteArray = {
            (byte) -84, (byte) -19, (byte) 0, (byte) 5, (byte) 125, (byte) -19, (byte) 0,
            (byte) 5, (byte) 115, (byte) 114, (byte) -1, (byte) 97, (byte) 122, (byte) -48, (byte) -65
        };

        assertThrows(SerializationException.class, () -> SerializationUtils.deserialize(new ByteArrayInputStream(byteArray)));
    }

    @Test
    void testPrimitiveTypeClassSerialization() {
        final Class<?>[] primitiveTypes = { byte.class, short.class, int.class, long.class, float.class, double.class,
                boolean.class, char.class, void.class };

        for (final Class<?> primitiveType : primitiveTypes) {
            final Class<?> clone = SerializationUtils.clone(primitiveType);
            assertEquals(primitiveType, clone);
        }
    }

    @Test
    void testRoundtrip() {
        final HashMap<Object, Object> newMap = SerializationUtils.roundtrip(iMap);
        assertEquals(iMap, newMap);
    }

    @Test
    void testSerializeBytes() throws Exception {
        final byte[] testBytes = SerializationUtils.serialize(iMap);

        final ByteArrayOutputStream streamReal = new ByteArrayOutputStream();
        try (ObjectOutputStream oos = new ObjectOutputStream(streamReal)) {
            oos.writeObject(iMap);
            oos.flush();
        }

        final byte[] realBytes = streamReal.toByteArray();
        assertEquals(testBytes.length, realBytes.length);
        assertArrayEquals(realBytes, testBytes);
    }

    @Test
    void testSerializeBytesNull() throws Exception {
        final byte[] testBytes = SerializationUtils.serialize(null);

        final ByteArrayOutputStream streamReal = new ByteArrayOutputStream();
        try (ObjectOutputStream oos = new ObjectOutputStream(streamReal)) {
            oos.writeObject(null);
            oos.flush();
        }

        final byte[] realBytes = streamReal.toByteArray();
        assertEquals(testBytes.length, realBytes.length);
        assertArrayEquals(realBytes, testBytes);
    }

    @Test
    void testSerializeBytesUnserializable() {
        iMap.put(new Object(), new Object());
        assertThrows(SerializationException.class, () -> SerializationUtils.serialize(iMap));
    }

    @Test
    void testSerializeIOException() {
        // forces an IOException when the ObjectOutputStream is created, to test not closing the stream
        // in the finally block
        final OutputStream streamTest = new OutputStream() {
            @Override
            public void write(final int arg0) throws IOException {
                throw new IOException(SERIALIZE_IO_EXCEPTION_MESSAGE);
            }
        };
        final SerializationException e =
                assertThrows(SerializationException.class, () -> SerializationUtils.serialize(iMap, streamTest));
        assertEquals("java.io.IOException: " + SERIALIZE_IO_EXCEPTION_MESSAGE, e.getMessage());
    }

    @Test
    void testSerializeStream() throws Exception {
        final ByteArrayOutputStream streamTest = new ByteArrayOutputStream();
        SerializationUtils.serialize(iMap, streamTest);

        final ByteArrayOutputStream streamReal = new ByteArrayOutputStream();
        try (ObjectOutputStream oos = new ObjectOutputStream(streamReal)) {
            oos.writeObject(iMap);
            oos.flush();
        }

        final byte[] testBytes = streamTest.toByteArray();
        final byte[] realBytes = streamReal.toByteArray();
        assertEquals(testBytes.length, realBytes.length);
        assertArrayEquals(realBytes, testBytes);
    }

    @Test
    void testSerializeStreamNullNull() {
        assertNullPointerException(() -> SerializationUtils.serialize(null, null));
    }

    @Test
    void testSerializeStreamNullObj() throws Exception {
        final ByteArrayOutputStream streamTest = new ByteArrayOutputStream();
        SerializationUtils.serialize(null, streamTest);

        final ByteArrayOutputStream streamReal = new ByteArrayOutputStream();
        try (ObjectOutputStream oos = new ObjectOutputStream(streamReal)) {
            oos.writeObject(null);
            oos.flush();
        }

        final byte[] testBytes = streamTest.toByteArray();
        final byte[] realBytes = streamReal.toByteArray();
        assertEquals(testBytes.length, realBytes.length);
        assertArrayEquals(realBytes, testBytes);
    }

    @Test
    void testSerializeStreamObjNull() {
        assertNullPointerException(() -> SerializationUtils.serialize(iMap, null));
    }

    @Test
    void testSerializeStreamUnserializable() {
        final ByteArrayOutputStream streamTest = new ByteArrayOutputStream();
        iMap.put(new Object(), new Object());
        assertThrows(SerializationException.class, () -> SerializationUtils.serialize(iMap, streamTest));
    }
}
