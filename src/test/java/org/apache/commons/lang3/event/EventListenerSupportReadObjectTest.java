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

package org.apache.commons.lang3.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.ObjectStreamClass;
import java.io.Serializable;
import java.util.Date;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.commons.lang3.AbstractLangTest;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link EventListenerSupport#readObject(ObjectInputStream)}.
 *
 * <p>
 * {@code EventListenerSupport} uses a completely custom wire format: {@link EventListenerSupport#writeObject(ObjectOutputStream)} serializes the listeners as a
 * single {@code L[]} object, and {@link EventListenerSupport#readObject(ObjectInputStream)} reads it back. The tests below cover:
 * </p>
 * <ul>
 * <li>Happy-path round-trip with no listeners.</li>
 * <li>Happy-path round-trip with serializable listeners – listeners survive.</li>
 * <li>Non-serializable listeners are silently dropped during serialization, so {@code readObject} sees a shorter array.</li>
 * <li>After deserialization the transient {@code proxy} field is rebuilt and {@code fire()} dispatches events.</li>
 * <li>A forged stream that supplies {@code null} instead of the listener array is rejected by
 * {@link org.apache.commons.lang3.SerializationUtils#requireNonNull} with {@link InvalidObjectException}.</li>
 * </ul>
 */
class EventListenerSupportReadObjectTest extends AbstractLangTest {

    /**
     * A simple {@link Serializable} {@link VetoableChangeListener} that counts how many times it has been called.
     */
    private static final class CountingListener implements VetoableChangeListener, Serializable {

        private static final long serialVersionUID = 1L;
        final AtomicInteger callCount = new AtomicInteger();

        @Override
        public void vetoableChange(final PropertyChangeEvent evt) throws PropertyVetoException {
            callCount.incrementAndGet();
        }
    }

    /**
     * A forge helper whose {@code writeObject} emits {@code null} instead of a listener array. The {@code serialVersionUID} matches
     * {@link EventListenerSupport#serialVersionUID} and the class descriptor is replaced in the stream by a custom {@link ObjectOutputStream} so that
     * {@link ObjectInputStream} interprets the bytes as an {@link EventListenerSupport}.
     */
    private static final class EventListenerSupportForge implements Serializable {

        /** Must match {@link EventListenerSupport}'s {@code serialVersionUID}. */
        private static final long serialVersionUID = 3593265990380473632L;

        /**
         * Writes {@code null} in place of the listener array, simulating a malicious or corrupt stream.
         */
        private void writeObject(final ObjectOutputStream oos) throws IOException {
            oos.writeObject(null);
        }
    }

    /**
     * A {@link VetoableChangeListener} that is intentionally <em>not</em> {@link Serializable}.
     */
    private static final class NonSerializableListener implements VetoableChangeListener {

        @Override
        public void vetoableChange(final PropertyChangeEvent evt) throws PropertyVetoException {
            // no-op
        }
    }

    /**
     * Deserializes the first object from {@code bytes}.
     */
    @SuppressWarnings("unchecked")
    private static <T> T deserialize(final byte[] bytes) throws IOException, ClassNotFoundException {
        try (ObjectInputStream ois = new ObjectInputStream(new ByteArrayInputStream(bytes))) {
            return (T) ois.readObject();
        }
    }

    /**
     * Serializes a {@link EventListenerSupportForge} but rewrites its class descriptor so that the resulting stream is treated as an
     * {@link EventListenerSupport} during deserialization.
     */
    private static byte[] forgeNullListenerArrayStream() throws IOException {
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try (ObjectOutputStream oos = new ObjectOutputStream(baos) {

            @Override
            protected void writeClassDescriptor(final ObjectStreamClass desc) throws IOException {
                if (desc.getName().equals(EventListenerSupportForge.class.getName())) {
                    // Replace the forge descriptor with the real EventListenerSupport descriptor.
                    super.writeClassDescriptor(ObjectStreamClass.lookup(EventListenerSupport.class));
                } else {
                    super.writeClassDescriptor(desc);
                }
            }
        }) {
            oos.writeObject(new EventListenerSupportForge());
        }
        return baos.toByteArray();
    }

    /**
     * Serializes {@code support} to a byte array using normal Java serialization.
     */
    private static byte[] serialize(final EventListenerSupport<?> support) throws IOException {
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try (ObjectOutputStream oos = new ObjectOutputStream(baos)) {
            oos.writeObject(support);
        }
        return baos.toByteArray();
    }

    /**
     * Non-{@link Serializable} listeners are silently dropped by {@link EventListenerSupport#writeObject(ObjectOutputStream)}; after deserialization only the
     * serializable subset is available.
     */
    @Test
    void testReadObjectDropsNonSerializableListeners() throws IOException, ClassNotFoundException {
        final EventListenerSupport<VetoableChangeListener> original = EventListenerSupport.create(VetoableChangeListener.class);
        final CountingListener serializable = new CountingListener();
        original.addListener(serializable);
        original.addListener(new NonSerializableListener()); // must be silently dropped
        final EventListenerSupport<VetoableChangeListener> restored = deserialize(serialize(original));
        assertEquals(1, restored.getListenerCount(), "Only the serializable listener must survive; the non-serializable one must be dropped");
        assertEquals(CountingListener.class, restored.getListeners()[0].getClass());
    }

    /**
     * An empty {@link EventListenerSupport} (no registered listeners) survives a serialization round-trip; after deserialization the support object is
     * non-null, reports zero listeners, and {@code fire()} returns a functional proxy.
     */
    @Test
    void testReadObjectEmptyListeners() throws IOException, ClassNotFoundException, PropertyVetoException {
        final EventListenerSupport<VetoableChangeListener> original = EventListenerSupport.create(VetoableChangeListener.class);
        final EventListenerSupport<VetoableChangeListener> restored = deserialize(serialize(original));
        assertNotNull(restored);
        assertEquals(0, restored.getListenerCount());
        // The transient proxy must have been rebuilt; calling fire() on an empty support must not throw.
        assertNotNull(restored.fire());
        restored.fire().vetoableChange(new PropertyChangeEvent(new Date(), "Day", 0, 1));
    }

    /**
     * After deserialization the transient {@code proxy} returned by {@code fire()} dispatches events to all restored listeners.
     */
    @Test
    void testReadObjectFireDispatchesAfterDeserializing() throws IOException, ClassNotFoundException, PropertyVetoException {
        final EventListenerSupport<VetoableChangeListener> original = EventListenerSupport.create(VetoableChangeListener.class);
        original.addListener(new CountingListener());
        original.addListener(new CountingListener());
        final EventListenerSupport<VetoableChangeListener> restored = deserialize(serialize(original));
        final PropertyChangeEvent evt = new PropertyChangeEvent(new Date(), "Prop", "old", "new");
        restored.fire().vetoableChange(evt);
        for (final VetoableChangeListener l : restored.getListeners()) {
            assertEquals(1, ((CountingListener) l).callCount.get());
        }
    }

    /**
     * A forged stream that provides {@code null} as the listener array is rejected by
     * {@link org.apache.commons.lang3.SerializationUtils#requireNonNull(Object, String)} inside {@code readObject}, which throws
     * {@link InvalidObjectException}.
     */
    @Test
    void testReadObjectNullListenerArrayRejected() throws IOException {
        final byte[] forgedBytes = forgeNullListenerArrayStream();
        assertThrows(InvalidObjectException.class, () -> deserialize(forgedBytes));
    }

    /**
     * {@code readObject} restores the full list of listeners that were {@link Serializable} at serialization time.
     */
    @Test
    void testReadObjectPreservesSerializableListeners() throws IOException, ClassNotFoundException, PropertyVetoException {
        final EventListenerSupport<VetoableChangeListener> original = EventListenerSupport.create(VetoableChangeListener.class);
        final CountingListener listener1 = new CountingListener();
        final CountingListener listener2 = new CountingListener();
        original.addListener(listener1);
        original.addListener(listener2);
        final EventListenerSupport<VetoableChangeListener> restored = deserialize(serialize(original));
        assertEquals(2, restored.getListenerCount());
        // Exercise the restored proxy to confirm event dispatching works.
        restored.fire().vetoableChange(new PropertyChangeEvent(new Date(), "Day", 0, 1));
        // Each restored listener should have been called exactly once.
        final VetoableChangeListener[] listeners = restored.getListeners();
        assertEquals(VetoableChangeListener.class, listeners.getClass().getComponentType());
        for (final VetoableChangeListener l : listeners) {
            assertEquals(1, ((CountingListener) l).callCount.get());
        }
    }

    /**
     * A new listener added after deserialization receives subsequent events, confirming that {@code readObject} fully initialized the internal state.
     */
    @Test
    void testReadObjectSupportsAddListenerAfterDeserializing() throws IOException, ClassNotFoundException, PropertyVetoException {
        final EventListenerSupport<VetoableChangeListener> original = EventListenerSupport.create(VetoableChangeListener.class);
        final EventListenerSupport<VetoableChangeListener> restored = deserialize(serialize(original));
        final CountingListener newListener = new CountingListener();
        restored.addListener(newListener);
        assertEquals(1, restored.getListenerCount());
        restored.fire().vetoableChange(new PropertyChangeEvent(new Date(), "Prop", "old", "new"));
        assertEquals(1, newListener.callCount.get());
    }
}
