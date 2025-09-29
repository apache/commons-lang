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

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.apache.commons.lang3.reflect.MethodUtils;

/**
 * Provides some useful event-based utility methods.
 *
 * @since 3.0
 */
public class EventUtils {

    private static final class EventBindingInvocationHandler implements InvocationHandler {
        private final Object target;
        private final String methodName;
        private final Set<String> eventTypes;

        /**
         * Creates a new instance of {@link EventBindingInvocationHandler}.
         *
         * @param target the target object for method invocations.
         * @param methodName the name of the method to be invoked.
         * @param eventTypes the names of the supported event types.
         */
        EventBindingInvocationHandler(final Object target, final String methodName, final String[] eventTypes) {
            this.target = target;
            this.methodName = methodName;
            this.eventTypes = new HashSet<>(Arrays.asList(eventTypes));
        }

        /**
         * Checks whether a method for the passed in parameters can be found.
         *
         * @param method the listener method invoked.
         * @return a flag whether the parameters could be matched.
         */
        private boolean hasMatchingParametersMethod(final Method method) {
            return MethodUtils.getAccessibleMethod(target.getClass(), methodName, method.getParameterTypes()) != null;
        }

        /**
         * Handles a method invocation on the proxy object.
         *
         * @param proxy the proxy instance.
         * @param method the method to be invoked.
         * @param parameters the parameters for the method invocation.
         * @return the result of the method call.
         * @throws SecurityException if an underlying accessible object's method denies the request.
         * @see SecurityManager#checkPermission
         * @throws Throwable if an error occurs
         */
        @Override
        public Object invoke(final Object proxy, final Method method, final Object[] parameters) throws Throwable {
            if (eventTypes.isEmpty() || eventTypes.contains(method.getName())) {
                if (hasMatchingParametersMethod(method)) {
                    return MethodUtils.invokeMethod(target, methodName, parameters);
                }
                return MethodUtils.invokeMethod(target, methodName);
            }
            return null;
        }
    }

    /**
     * Adds an event listener to the specified source.  This looks for an "add" method corresponding to the event
     * type (addActionListener, for example).
     *
     * @param eventSource   the event source.
     * @param listenerType  the event listener type.
     * @param listener      the listener.
     * @param <L>           the event listener type.
     * @throws IllegalArgumentException if the object doesn't support the listener type.
     */
    public static <L> void addEventListener(final Object eventSource, final Class<L> listenerType, final L listener) {
        try {
            MethodUtils.invokeMethod(eventSource, "add" + listenerType.getSimpleName(), listener);
        } catch (final ReflectiveOperationException e) {
            throw new IllegalArgumentException("Unable to add listener for class " + eventSource.getClass().getName()
                    + " and public add" + listenerType.getSimpleName()
                    + " method which takes a parameter of type " + listenerType.getName() + ".");
        }
    }

    /**
     * Binds an event listener to a specific method on a specific object.
     *
     * @param <L>          the event listener type.
     * @param target       the target object.
     * @param methodName   the name of the method to be called.
     * @param eventSource  the object which is generating events (JButton, JList, etc.).
     * @param listenerType the listener interface (ActionListener.class, SelectionListener.class, etc.).
     * @param eventTypes   the event types (method names) from the listener interface (if none specified, all will be
     *                     supported).
     */
    public static <L> void bindEventsToMethod(final Object target, final String methodName, final Object eventSource,
            final Class<L> listenerType, final String... eventTypes) {
        final L listener = listenerType.cast(Proxy.newProxyInstance(target.getClass().getClassLoader(),
                new Class[] { listenerType }, new EventBindingInvocationHandler(target, methodName, eventTypes)));
        addEventListener(eventSource, listenerType, listener);
    }

    /**
     * Make private in 4.0.
     *
     * @deprecated TODO Make private in 4.0.
     */
    @Deprecated
    public EventUtils() {
        // empty
    }
}
