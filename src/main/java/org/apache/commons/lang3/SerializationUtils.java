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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.ObjectStreamClass;
import java.io.OutputStream;
import java.io.Serializable;
import java.util.Objects;

/**
 * Assists with the serialization process and performs additional functionality based
 * on serialization.
 *
 * <ul>
 * <li>Deep clone using serialization
 * <li>Serialize managing finally and IOException
 * <li>Deserialize managing finally and IOException
 * </ul>
 *
 * <p>This class throws exceptions for invalid {@code null} inputs.
 * Each method documents its behavior in more detail.</p>
 *
 * <p>#ThreadSafe#</p>
 * @since 1.0
 */
public class SerializationUtils {

    /**
     * Custom specialization of the standard JDK {@link ObjectInputStream}
     * that uses a custom  {@link ClassLoader} to resolve a class.
     * If the specified {@link ClassLoader} is not able to resolve the class,
     * the context classloader of the current thread will be used.
     * This way, the standard deserialization work also in web-application
     * containers and application servers, no matter in which of the
     * {@link ClassLoader} the particular class that encapsulates
     * serialization/deserialization lives.
     *
     * <p>For more in-depth information about the problem for which this
     * class here is a workaround, see the JIRA issue LANG-626.</p>
     */
     static final class ClassLoaderAwareObjectInputStream extends ObjectInputStream {

        private final ClassLoader classLoader;

        /**
         * Constructs a new instance.
         * @param in The {@link InputStream}.
         * @param classLoader classloader to use
         * @throws IOException if an I/O error occurs while reading stream header.
         * @see java.io.ObjectInputStream
         */
        ClassLoaderAwareObjectInputStream(final InputStream in, final ClassLoader classLoader) throws IOException {
            super(in);
            this.classLoader = classLoader;
        }

        /**
         * Overridden version that uses the parameterized {@link ClassLoader} or the {@link ClassLoader}
         * of the current {@link Thread} to resolve the class.
         * @param desc An instance of class {@link ObjectStreamClass}.
         * @return A {@link Class} object corresponding to {@code desc}.
         * @throws IOException Any of the usual Input/Output exceptions.
         * @throws ClassNotFoundException If class of a serialized object cannot be found.
         */
        @Override
        protected Class<?> resolveClass(final ObjectStreamClass desc) throws IOException, ClassNotFoundException {
            final String name = desc.getName();
            try {
                return Class.forName(name, false, classLoader);
            } catch (final ClassNotFoundException ex) {
                try {
                    return Class.forName(name, false, Thread.currentThread().getContextClassLoader());
                } catch (final ClassNotFoundException cnfe) {
                    final Class<?> cls = ClassUtils.getPrimitiveClass(name);
                    if (cls != null) {
                        return cls;
                    }
                    throw cnfe;
                }
            }
        }

    }

    /**
     * Deep clones an {@link Object} using serialization.
     *
     * <p>This is many times slower than writing clone methods by hand
     * on all objects in your object graph. However, for complex object
     * graphs, or for those that don't support deep cloning this can
     * be a simple alternative implementation. Of course all the objects
     * must be {@link Serializable}.</p>
     *
     * @param <T> the type of the object involved.
     * @param object  the {@link Serializable} object to clone.
     * @return the cloned object.
     * @throws SerializationException (runtime) if the serialization fails.
     */
    public static <T extends Serializable> T clone(final T object) {
        if (object == null) {
            return null;
        }
        final ByteArrayInputStream bais = new ByteArrayInputStream(serialize(object));
        final Class<T> cls = ObjectUtils.getClass(object);
        try (ClassLoaderAwareObjectInputStream in = new ClassLoaderAwareObjectInputStream(bais, cls.getClassLoader())) {
            // When we serialize and deserialize an object, it is reasonable to assume the deserialized object is of the
            // same type as the original serialized object
            return (T) in.readObject();

        } catch (final ClassNotFoundException | IOException ex) {
            throw new SerializationException(String.format("%s while reading cloned object data", ex.getClass().getSimpleName()), ex);
        }
    }

    /**
     * Deserializes a single {@link Object} from an array of bytes.
     *
     * <p>
     * If the call site incorrectly types the return value, a {@link ClassCastException} is thrown from the call site.
     * Without Generics in this declaration, the call site must type cast and can cause the same ClassCastException.
     * Note that in both cases, the ClassCastException is in the call site, not in this method.
     * </p>
     *
     * @param <T>  the object type to be deserialized.
     * @param objectData
     *            the serialized object, must not be null.
     * @return the deserialized object.
     * @throws NullPointerException if {@code objectData} is {@code null}.
     * @throws SerializationException (runtime) if the serialization fails.
     */
    public static <T> T deserialize(final byte[] objectData) {
        Objects.requireNonNull(objectData, "objectData");
        return deserialize(new ByteArrayInputStream(objectData));
    }

    /**
     * Deserializes an {@link Object} from the specified stream.
     *
     * <p>
     * The stream will be closed once the object is written. This avoids the need for a finally clause, and maybe also
     * exception handling, in the application code.
     * </p>
     *
     * <p>
     * The stream passed in is not buffered internally within this method. This is the responsibility of your
     * application if desired.
     * </p>
     *
     * <p>
     * If the call site incorrectly types the return value, a {@link ClassCastException} is thrown from the call site.
     * Without Generics in this declaration, the call site must type cast and can cause the same ClassCastException.
     * Note that in both cases, the ClassCastException is in the call site, not in this method.
     * </p>
     *
     * @param <T>  the object type to be deserialized.
     * @param inputStream
     *            the serialized object input stream, must not be null.
     * @return the deserialized object.
     * @throws NullPointerException if {@code inputStream} is {@code null}.
     * @throws SerializationException (runtime) if the serialization fails.
     */
    @SuppressWarnings("resource") // inputStream is managed by the caller
    public static <T> T deserialize(final InputStream inputStream) {
        Objects.requireNonNull(inputStream, "inputStream");
        try (ObjectInputStream in = new ObjectInputStream(inputStream)) {
            @SuppressWarnings("unchecked")
            final T obj = (T) in.readObject();
            return obj;
        } catch (final ClassNotFoundException | IOException | NegativeArraySizeException ex) {
            throw new SerializationException(ex);
        }
    }

    /**
     * Performs a serialization roundtrip. Serializes and deserializes the given object, great for testing objects that
     * implement {@link Serializable}.
     *
     * @param <T>
     *           the type of the object involved.
     * @param obj
     *            the object to roundtrip.
     * @return the serialized and deserialized object.
     * @since 3.3
     */
    @SuppressWarnings("unchecked") // OK, because we serialized a type `T`
    public static <T extends Serializable> T roundtrip(final T obj) {
        return (T) deserialize(serialize(obj));
    }

    /**
     * Serializes an {@link Object} to a byte array for
     * storage/serialization.
     *
     * @param obj  the object to serialize to bytes.
     * @return a byte[] with the converted Serializable.
     * @throws SerializationException (runtime) if the serialization fails.
     */
    public static byte[] serialize(final Serializable obj) {
        final ByteArrayOutputStream baos = new ByteArrayOutputStream(512);
        serialize(obj, baos);
        return baos.toByteArray();
    }

    /**
     * Serializes an {@link Object} to the specified stream.
     *
     * <p>The stream will be closed once the object is written.
     * This avoids the need for a finally clause, and maybe also exception
     * handling, in the application code.</p>
     *
     * <p>The stream passed in is not buffered internally within this method.
     * This is the responsibility of your application if desired.</p>
     *
     * @param obj  the object to serialize to bytes, may be null.
     * @param outputStream  the stream to write to, must not be null.
     * @throws NullPointerException if {@code outputStream} is {@code null}.
     * @throws SerializationException (runtime) if the serialization fails.
     */
    @SuppressWarnings("resource") // outputStream is managed by the caller
    public static void serialize(final Serializable obj, final OutputStream outputStream) {
        Objects.requireNonNull(outputStream, "outputStream");
        try (ObjectOutputStream out = new ObjectOutputStream(outputStream)) {
            out.writeObject(obj);
        } catch (final IOException ex) {
            throw new SerializationException(ex);
        }
    }

    /**
     * SerializationUtils instances should NOT be constructed in standard programming.
     * Instead, the class should be used as {@code SerializationUtils.clone(object)}.
     *
     * <p>This constructor is public to permit tools that require a JavaBean instance
     * to operate.</p>
     * @since 2.0
     * @deprecated TODO Make private in 4.0.
     */
    @Deprecated
    public SerializationUtils() {
        // empty
    }

}
