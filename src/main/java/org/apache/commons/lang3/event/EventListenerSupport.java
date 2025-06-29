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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.Validate;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.commons.lang3.function.FailableConsumer;

/**
 * Manages a list of event listeners of a given generic type. This class provides {@link #addListener(Object)} and {@link #removeListener(Object)} methods for
 * managing listeners, as well as a {@link #fire()} method for firing events to the listeners.
 *
 * <p>
 * For example, to support ActionEvents:
 * </p>
 *
 * <pre>{@code
 * public class MyActionEventSource {
 *
 *     private EventListenerSupport<ActionListener> actionListeners = EventListenerSupport.create(ActionListener.class);
 *
 *     public void someMethodThatFiresAction() {
 *         ActionEvent e = new ActionEvent(this, ActionEvent.ACTION_PERFORMED, "something");
 *         actionListeners.fire().actionPerformed(e);
 *     }
 * }
 * }</pre>
 * <p>
 * Events are fired
 * <p>
 * Serializing an {@link EventListenerSupport} instance will result in any non-{@link Serializable} listeners being silently dropped.
 * </p>
 *
 * @param <L> the type of event listener that is supported by this proxy.
 * @since 3.0
 */
public class EventListenerSupport<L> implements Serializable {

    /**
     * Invokes listeners through {@link #invoke(Object, Method, Object[])} in the order added to the underlying {@link List}.
     */
    protected class ProxyInvocationHandler implements InvocationHandler {

        private final FailableConsumer<Throwable, IllegalAccessException> handler;

        /**
         * Constructs a new instance.
         */
        public ProxyInvocationHandler() {
            this(ExceptionUtils::rethrow);
        }

        /**
         * Constructs a new instance.
         *
         * @param handler Handles Throwables.
         * @since 3.15.0
         */
        public ProxyInvocationHandler(final FailableConsumer<Throwable, IllegalAccessException> handler) {
            this.handler = Objects.requireNonNull(handler);
        }

        /**
         * Handles an exception thrown by a listener. By default rethrows the given Throwable.
         *
         * @param t The Throwable
         * @throws IllegalAccessException thrown by the listener.
         * @throws IllegalArgumentException thrown by the listener.
         * @throws InvocationTargetException thrown by the listener.
         * @since 3.15.0
         */
        protected void handle(final Throwable t) throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
            handler.accept(t);
        }

        /**
         * Propagates the method call to all registered listeners in place of the proxy listener object.
         * <p>
         * Calls listeners in the order added to the underlying {@link List}.
         * </p>
         *
         * @param unusedProxy the proxy object representing a listener on which the invocation was called; not used
         * @param method the listener method that will be called on all of the listeners.
         * @param args event arguments to propagate to the listeners.
         * @return the result of the method call
         * @throws InvocationTargetException if an error occurs
         * @throws IllegalArgumentException if an error occurs
         * @throws IllegalAccessException if an error occurs
         */
        @Override
        public Object invoke(final Object unusedProxy, final Method method, final Object[] args)
                throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
            for (final L listener : listeners) {
                try {
                    method.invoke(listener, args);
                } catch (final Throwable t) {
                    handle(t);
                }
            }
            return null;
        }
    }

    /** Serialization version */
    private static final long serialVersionUID = 3593265990380473632L;

    /**
     * Creates an EventListenerSupport object which supports the specified
     * listener type.
     *
     * @param <T> the type of the listener interface
     * @param listenerInterface the type of listener interface that will receive
     *        events posted using this class.
     *
     * @return an EventListenerSupport object which supports the specified
     *         listener type.
     *
     * @throws NullPointerException if {@code listenerInterface} is
     *         {@code null}.
     * @throws IllegalArgumentException if {@code listenerInterface} is
     *         not an interface.
     */
    public static <T> EventListenerSupport<T> create(final Class<T> listenerInterface) {
        return new EventListenerSupport<>(listenerInterface);
    }

    /**
     * Hold the registered listeners. This list is intentionally a thread-safe copy-on-write-array so that traversals over the list of listeners will be atomic.
     */
    private List<L> listeners = new CopyOnWriteArrayList<>();

    /**
     * The proxy representing the collection of listeners. Calls to this proxy object will be sent to all registered listeners.
     */
    private transient L proxy;
    /**
     * Empty typed array for #getListeners().
     */
    private transient L[] prototypeArray;

    /**
     * Constructs a new EventListenerSupport instance.
     * <p>
     * This constructor is needed for serialization.
     * </p>
     */
    private EventListenerSupport() {
    }

    /**
     * Constructs an EventListenerSupport object which supports the provided
     * listener interface.
     *
     * @param listenerInterface the type of listener interface that will receive
     *        events posted using this class.
     *
     * @throws NullPointerException if {@code listenerInterface} is
     *         {@code null}.
     * @throws IllegalArgumentException if {@code listenerInterface} is
     *         not an interface.
     */
    public EventListenerSupport(final Class<L> listenerInterface) {
        this(listenerInterface, Thread.currentThread().getContextClassLoader());
    }

    /**
     * Constructs an EventListenerSupport object which supports the provided
     * listener interface using the specified class loader to create the JDK
     * dynamic proxy.
     *
     * @param listenerInterface the listener interface.
     * @param classLoader       the class loader.
     * @throws NullPointerException if {@code listenerInterface} or
     *         {@code classLoader} is {@code null}.
     * @throws IllegalArgumentException if {@code listenerInterface} is
     *         not an interface.
     */
    public EventListenerSupport(final Class<L> listenerInterface, final ClassLoader classLoader) {
        this();
        Objects.requireNonNull(listenerInterface, "listenerInterface");
        Objects.requireNonNull(classLoader, "classLoader");
        Validate.isTrue(listenerInterface.isInterface(), "Class %s is not an interface", listenerInterface.getName());
        initializeTransientFields(listenerInterface, classLoader);
    }

    /**
     * Adds an event listener.
     * <p>
     * Listeners are called in the order added.
     * </p>
     *
     * @param listener the event listener (may not be {@code null}).
     * @throws NullPointerException if {@code listener} is {@code null}.
     */
    public void addListener(final L listener) {
        addListener(listener, true);
    }

    /**
     * Adds an event listener. Will not add a pre-existing listener object to the list if {@code allowDuplicate} is false.
     * <p>
     * Listeners are called in the order added.
     * </p>
     *
     * @param listener       the event listener (may not be {@code null}).
     * @param allowDuplicate the flag for determining if duplicate listener objects are allowed to be registered.
     *
     * @throws NullPointerException if {@code listener} is {@code null}.
     * @since 3.5
     */
    public void addListener(final L listener, final boolean allowDuplicate) {
        Objects.requireNonNull(listener, "listener");
        if (allowDuplicate || !listeners.contains(listener)) {
            listeners.add(listener);
        }
    }

    /**
     * Creates the {@link InvocationHandler} responsible for calling
     * to the managed listeners. Subclasses can override to provide custom behavior.
     *
     * @return ProxyInvocationHandler
     */
    protected InvocationHandler createInvocationHandler() {
        return new ProxyInvocationHandler();
    }

    /**
     * Creates the proxy object.
     *
     * @param listenerInterface the class of the listener interface
     * @param classLoader the class loader to be used
     */
    private void createProxy(final Class<L> listenerInterface, final ClassLoader classLoader) {
        proxy = listenerInterface.cast(Proxy.newProxyInstance(classLoader, new Class[] { listenerInterface }, createInvocationHandler()));
    }

    /**
     * Returns a proxy object which can be used to call listener methods on all
     * of the registered event listeners. All calls made to this proxy will be
     * forwarded to all registered listeners.
     *
     * @return a proxy object which can be used to call listener methods on all
     * of the registered event listeners
     */
    public L fire() {
        return proxy;
    }

    /**
     * Gets the number of registered listeners.
     *
     * @return the number of registered listeners.
     */
    int getListenerCount() {
        return listeners.size();
    }

    /**
     * Gets an array containing the currently registered listeners.
     * Modification to this array's elements will have no effect on the
     * {@link EventListenerSupport} instance.
     * @return L[]
     */
    public L[] getListeners() {
        return listeners.toArray(prototypeArray);
    }

    /**
     * Initializes transient fields.
     *
     * @param listenerInterface the class of the listener interface
     * @param classLoader the class loader to be used
     */
    private void initializeTransientFields(final Class<L> listenerInterface, final ClassLoader classLoader) {
        // Will throw CCE here if not correct
        this.prototypeArray = ArrayUtils.newInstance(listenerInterface, 0);
        createProxy(listenerInterface, classLoader);
    }

    /**
     * Deserializes the next object into this instance.
     *
     * @param objectInputStream the input stream
     * @throws IOException if an IO error occurs
     * @throws ClassNotFoundException if the class cannot be resolved
     */
    private void readObject(final ObjectInputStream objectInputStream) throws IOException, ClassNotFoundException {
        @SuppressWarnings("unchecked") // Will throw CCE here if not correct
        final L[] srcListeners = (L[]) objectInputStream.readObject();
        this.listeners = new CopyOnWriteArrayList<>(srcListeners);
        final Class<L> listenerInterface = ArrayUtils.getComponentType(srcListeners);
        initializeTransientFields(listenerInterface, Thread.currentThread().getContextClassLoader());
    }

    /**
     * Removes an event listener.
     *
     * @param listener the event listener (may not be {@code null}).
     * @throws NullPointerException if {@code listener} is
     *         {@code null}.
     */
    public void removeListener(final L listener) {
        listeners.remove(Objects.requireNonNull(listener, "listener"));
    }

    /**
     * Serializes this instance onto the given ObjectOutputStream.
     *
     * @param objectOutputStream the output stream
     * @throws IOException if an IO error occurs
     */
    private void writeObject(final ObjectOutputStream objectOutputStream) throws IOException {
        final ArrayList<L> serializableListeners = new ArrayList<>();
        // Don't just rely on instanceof Serializable:
        ObjectOutputStream testObjectOutputStream = new ObjectOutputStream(new ByteArrayOutputStream());
        for (final L listener : listeners) {
            try {
                testObjectOutputStream.writeObject(listener);
                serializableListeners.add(listener);
            } catch (final IOException exception) {
                //recreate test stream in case of indeterminate state
                testObjectOutputStream = new ObjectOutputStream(new ByteArrayOutputStream());
            }
        }
        // We can reconstitute everything we need from an array of our listeners,
        // which has the additional advantage of typically requiring less storage than a list:
        objectOutputStream.writeObject(serializableListeners.toArray(prototypeArray));
    }
}
