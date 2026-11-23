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
import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.ObjectStreamClass;
import java.io.OutputStream;
import java.io.Serializable;
import java.lang.reflect.Modifier;
import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Performs additional functionality for serialization.
 *
 * <ul>
 * <li>Deep clone using serialization</li>
 * <li>Serialize managing finally and IOException</li>
 * <li>Deserialize managing finally and IOException</li>
 * </ul>
 *
 * <p>
 * This class throws exceptions for invalid {@code null} inputs. Each method documents its behavior in more detail.
 * </p>
 * <p>
 * If you want to secure deserialization with a whitelist or blacklist, please use Apache Commons IO's
 * {@link org.apache.commons.io.serialization.ValidatingObjectInputStream ValidatingObjectInputStream}.
 * </p>
 * <p>
 * #ThreadSafe#
 * </p>
 *
 * @see org.apache.commons.io.serialization.ValidatingObjectInputStream
 * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/specs/serialization/">Java Object Serialization Specification</a>
 * @since 1.0
 */
public class SerializationUtils {

    /**
     * Custom specialization of the standard JDK {@link ObjectInputStream} that resolves classes to their original definitions.
     *
     * <p>
     * Classes, including proxy classes, are recorded during serialization and reused directly to preserve their defining class loaders. Each class descriptor
     * contains an index into the recorded classes, distinguishing classes with identical names from different loaders in the same object graph.
     * </p>
     *
     * <p>
     * For more in-depth information about the problem for which this class here is a workaround, see the JIRA issue LANG-626.
     * </p>
     */
    static final class ClassLoaderAwareObjectInputStream extends ObjectInputStream {

        private final List<Class<?>> classes;

        /**
         * Constructs a new instance.
         *
         * @param in      The {@link InputStream}.
         * @param classes The classes recorded while serializing the original object.
         * @throws IOException Thrown if an I/O error occurs while reading stream header.
         * @see java.io.ObjectInputStream
         */
        ClassLoaderAwareObjectInputStream(final InputStream in, final List<Class<?>> classes) throws IOException {
            super(in);
            this.classes = classes;
        }

        /**
         * Resolves a class using its recorded identity.
         *
         * @param desc An instance of class {@link ObjectStreamClass}.
         * @return A {@link Class} object corresponding to {@code desc}.
         * @throws IOException Thrown if an I/O error occurs.
         */
        @Override
        protected Class<?> resolveClass(final ObjectStreamClass desc) throws IOException {
            return classes.get(readInt());
        }

        @Override
        protected Class<?> resolveProxyClass(final String[] interfaces) throws IOException {
            return classes.get(readInt());
        }
    }

    /**
     * Resolves serialized classes using a caller-supplied class loader.
     */
    private static final class ClassLoaderObjectInputStream extends ObjectInputStream {

        private final ClassLoader classLoader;

        private ClassLoaderObjectInputStream(final InputStream inputStream, final ClassLoader classLoader) throws IOException {
            super(inputStream);
            this.classLoader = classLoader;
        }

        @Override
        protected Class<?> resolveClass(final ObjectStreamClass descriptor) throws ClassNotFoundException {
            final Class<?> primitive = ClassUtils.getPrimitiveClass(descriptor.getName());
            return primitive != null ? primitive : Class.forName(descriptor.getName(), false, classLoader);
        }

        @Override
        protected Class<?> resolveProxyClass(final String[] interfaces) throws ClassNotFoundException {
            final Class<?>[] interfaceClasses = new Class<?>[interfaces.length];
            ClassLoader proxyLoader = classLoader;
            boolean nonPublic = false;
            for (int i = 0; i < interfaces.length; i++) {
                final Class<?> type = Class.forName(interfaces[i], false, classLoader);
                if (!Modifier.isPublic(type.getModifiers())) {
                    if (nonPublic && proxyLoader != type.getClassLoader()) {
                        throw new ClassNotFoundException("Non-public proxy interfaces have different class loaders");
                    }
                    proxyLoader = type.getClassLoader();
                    nonPublic = true;
                }
                interfaceClasses[i] = type;
            }
            try {
                // Deserialization restores the real invocation handler; only the proxy class is needed here.
                return Proxy.newProxyInstance(proxyLoader, interfaceClasses, (proxy, method, args) -> null).getClass();
            } catch (final IllegalArgumentException ex) {
                throw new ClassNotFoundException("Cannot resolve proxy class", ex);
            }
        }
    }

    /**
     * Deep clones an {@link Object} using serialization.
     *
     * <p>
     * This is many times slower than writing clone methods by hand on all objects in your object graph. However, for complex object graphs, or for those that
     * don't support deep cloning this can be a simple alternative implementation. Of course all the objects must be {@link Serializable}.
     * </p>
     *
     * <p>
     * Resolves serialized classes and proxy classes to their original definitions, preserving their defining class loaders.
     * </p>
     *
     * @param <T>    the type of the object involved.
     * @param object The {@link Serializable} object to clone.
     * @return The cloned object.
     * @throws SerializationException Thrown if the serialization fails.
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/specs/serialization/">Java Object Serialization Specification</a>
     */
    public static <T extends Serializable> T clone(final T object) {
        if (object == null) {
            return null;
        }
        final ByteArrayOutputStream baos = new ByteArrayOutputStream(512);
        final List<Class<?>> classes = new ArrayList<>();
        try (ObjectOutputStream out = new ObjectOutputStream(baos) {
            @Override
            protected void annotateClass(final Class<?> cls) throws IOException {
                // Pair each descriptor with its original class, even when another loader defines the same name.
                writeInt(classes.size());
                classes.add(cls);
            }

            @Override
            protected void annotateProxyClass(final Class<?> proxyClass) throws IOException {
                annotateClass(proxyClass);
            }
        }) {
            out.writeObject(object);
        } catch (final IOException ex) {
            throw new SerializationException(ex);
        }
        final ByteArrayInputStream bais = new ByteArrayInputStream(baos.toByteArray());
        try (ClassLoaderAwareObjectInputStream in = new ClassLoaderAwareObjectInputStream(bais, classes)) {
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
     * If the call site incorrectly types the return value, a {@link ClassCastException} is thrown from the call site. Without Generics in this declaration, the
     * call site must type cast and can cause the same ClassCastException. Note that in both cases, the ClassCastException is in the call site, not in this
     * method.
     * </p>
     * <p>
     * If you want to secure deserialization with a whitelist or blacklist, please use Apache Commons IO's
     * {@link org.apache.commons.io.serialization.ValidatingObjectInputStream ValidatingObjectInputStream}.
     * </p>
     *
     * @param <T>        the object type to be deserialized.
     * @param objectData The serialized object, must not be null.
     * @return The deserialized object.
     * @throws NullPointerException   Thrown if {@code objectData} is {@code null}.
     * @throws SerializationException Thrown if the serialization fails.
     * @see org.apache.commons.io.serialization.ValidatingObjectInputStream
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/specs/serialization/">Java Object Serialization Specification</a>
     */
    public static <T> T deserialize(final byte[] objectData) {
        Objects.requireNonNull(objectData, "objectData");
        return deserialize(new ByteArrayInputStream(objectData));
    }

    /**
     * Deserializes an object from an array of bytes using the specified class loader.
     *
     * <p>
     * This is useful in containers where the serialized classes are not visible to the class loader that loaded this library.
     * Class and proxy-interface names are resolved using {@code classLoader}, without falling back to another loader.
     * A {@code null} class loader uses the standard {@link ObjectInputStream} resolution behavior instead.
     * </p>
     *
     * @param <T>         The object type to be deserialized.
     * @param objectData  The serialized object, must not be null.
     * @param classLoader The class loader to use, or null for standard resolution.
     * @return The deserialized object.
     * @throws NullPointerException   Thrown if {@code objectData} is {@code null}.
     * @throws SerializationException Thrown if the serialization fails.
     * @since 3.22.1
     * @see #deserialize(InputStream, ClassLoader)
     */
    public static <T> T deserialize(final byte[] objectData, final ClassLoader classLoader) {
        Objects.requireNonNull(objectData, "objectData");
        return deserialize(new ByteArrayInputStream(objectData), classLoader);
    }

    /**
     * Deserializes an {@link Object} from the specified stream.
     *
     * <p>
     * The stream will be closed once the object is written. This avoids the need for a finally clause, and maybe also exception handling, in the application
     * code.
     * </p>
     *
     * <p>
     * The stream passed in is not buffered internally within this method. This is the responsibility of your application if desired.
     * </p>
     *
     * <p>
     * If the call site incorrectly types the return value, a {@link ClassCastException} is thrown from the call site. Without Generics in this declaration, the
     * call site must type cast and can cause the same ClassCastException. Note that in both cases, the ClassCastException is in the call site, not in this
     * method.
     * </p>
     *
     * <p>
     * If you want to secure deserialization with a whitelist or blacklist, please use Apache Commons IO's
     * {@link org.apache.commons.io.serialization.ValidatingObjectInputStream ValidatingObjectInputStream}.
     * </p>
     *
     * @param <T>         the object type to be deserialized.
     * @param inputStream The serialized object input stream, must not be null.
     * @return The deserialized object.
     * @throws NullPointerException   Thrown if {@code inputStream} is {@code null}.
     * @throws SerializationException Thrown if the serialization fails.
     * @see org.apache.commons.io.serialization.ValidatingObjectInputStream
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/specs/serialization/">Java Object Serialization Specification</a>
     */
    public static <T> T deserialize(final InputStream inputStream) {
        return deserialize(inputStream, null);
    }

    /**
     * Deserializes an object from a stream using the specified class loader.
     *
     * <p>
     * Class and proxy-interface names are resolved using {@code classLoader}, without falling back to another loader.
     * A {@code null} class loader uses the standard {@link ObjectInputStream} resolution behavior instead.
     * Non-public proxy interfaces must share a defining class loader, which is used to define their proxy class.
     * </p>
     *
     * <p>
     * The stream is closed after the object is read and is not buffered internally.
     * </p>
     *
     * @param <T>         The object type to be deserialized.
     * @param inputStream The serialized object input stream, must not be null.
     * @param classLoader The class loader to use, or null for standard resolution.
     * @return The deserialized object.
     * @throws NullPointerException   Thrown if {@code inputStream} is {@code null}.
     * @throws SerializationException Thrown if the serialization fails.
     * @since 3.22.1
     * @see #deserialize(InputStream)
     */
    @SuppressWarnings("resource") // inputStream is managed by the caller
    public static <T> T deserialize(final InputStream inputStream, final ClassLoader classLoader) {
        Objects.requireNonNull(inputStream, "inputStream");
        try (ObjectInputStream in = classLoader == null ? new ObjectInputStream(inputStream) : new ClassLoaderObjectInputStream(inputStream, classLoader)) {
            @SuppressWarnings("unchecked")
            final T obj = (T) in.readObject();
            return obj;
        } catch (final ClassNotFoundException | IOException | NegativeArraySizeException ex) {
            throw new SerializationException(ex);
        }
    }

    /**
     * Checks that the specified object reference is not {@code null} and throws a customized {@link InvalidObjectException} if it is. This method is designed
     * primarily for doing state validation in {@link Serializable} class's {@code readObject(ObjectInputStream)} methods.
     *
     * @param obj     The object reference to check for nullity.
     * @param message detail message to be used in the event that a {@link InvalidObjectException} is thrown.
     * @param <T>     the type of the reference.
     * @return {@code obj} if not {@code null}.
     * @throws InvalidObjectException Thrown if {@code obj} is {@code null}.
     * @see Serializable
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/specs/serialization/">Java Object Serialization Specification</a>
     * @since 3.21.0
     */
    public static <T> T requireNonNull(final T obj, final String message) throws InvalidObjectException {
        if (obj == null) {
            throw new InvalidObjectException(message);
        }
        return obj;
    }

    /**
     * Performs a serialization roundtrip. Serializes and deserializes the given object, great for testing objects that implement {@link Serializable}.
     *
     * <p>
     * Uses {@link #clone(Serializable)} to preserve the defining class loaders of serialized classes and proxy classes.
     * </p>
     *
     * @param <T> The type of the object involved.
     * @param obj The object to roundtrip, may be {@code null}.
     * @return The serialized and deserialized object, or {@code null} if the input is {@code null}.
     * @throws SerializationException Thrown if serialization or deserialization fails.
     * @since 3.3
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/specs/serialization/">Java Object Serialization Specification</a>
     */
    public static <T extends Serializable> T roundtrip(final T obj) {
        return clone(obj);
    }

    /**
     * Serializes an {@link Object} to a byte array for storage/serialization.
     *
     * @param obj The object to serialize to bytes.
     * @return A byte[] with the converted Serializable.
     * @throws SerializationException Thrown if the serialization fails.
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/specs/serialization/">Java Object Serialization Specification</a>
     */
    public static byte[] serialize(final Serializable obj) {
        final ByteArrayOutputStream baos = new ByteArrayOutputStream(512);
        serialize(obj, baos);
        return baos.toByteArray();
    }

    /**
     * Serializes an {@link Object} to the specified stream.
     *
     * <p>
     * The stream will be closed once the object is written. This avoids the need for a finally clause, and maybe also exception handling, in the application
     * code.
     * </p>
     *
     * <p>
     * The stream passed in is not buffered internally within this method. This is the responsibility of your application if desired.
     * </p>
     *
     * @param obj          The object to serialize to bytes, may be null.
     * @param outputStream The stream to write to, must not be null.
     * @throws NullPointerException   Thrown if {@code outputStream} is {@code null}.
     * @throws SerializationException Thrown if the serialization fails.
     * @see <a href="https://docs.oracle.com/en/java/javase/25/docs/specs/serialization/">Java Object Serialization Specification</a>
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
     * SerializationUtils instances should NOT be constructed in standard programming. Instead, the class should be used as
     * {@code SerializationUtils.clone(object)}.
     *
     * <p>
     * This constructor is public to permit tools that require a JavaBean instance to operate.
     * </p>
     *
     * @since 2.0
     * @deprecated TODO Make private in 4.0.
     */
    @Deprecated
    public SerializationUtils() {
        // empty
    }
}
