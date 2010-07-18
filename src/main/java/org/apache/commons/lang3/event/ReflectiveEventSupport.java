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

package org.apache.commons.lang3.event;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.EventListener;
import java.util.EventObject;

import org.apache.commons.lang3.Validate;
import org.apache.commons.lang3.reflect.MethodUtils;

/**
 * <p>
 * The ReflectiveEventSupport class provides a means of posting 
 * {@link EventObject}s to registered listeners. The class uses reflection to
 * call specified methods on the listeners, either by {@link Method} or method 
 * name.
 * </p>
 * 
 * <p>
 * <em>NOTE: The methods on the listeners must be accessible in order to be
 * called.</em>
 * </p>
 * 
 * <p>
 * Example:
 * <code><pre>
 * ReflectiveEventSupport<ChangeListener> reflectiveEventSupport = 
 *    new ReflectiveEventSupport<ChangeListener>(this);
 * 
 * ...
 *    
 * reflectiveEventSupport.addListener(listener);
 * 
 * ...
 * 
 * reflectiveEventSupport.fireEvent("stateChanged",
 *     new ChangeEvent(reflectiveEventSupport.getSource());
 * </pre></code>
 * </p>
 * 
 * @author <a href="mailto:mwooten.dev@gmail.com">Michael Wooten</a>
 *
 * @param <L> the subclass of {@link EventListener} that this event support
 *        class can register.
 * 
 * @since 3.0
 */
public class ReflectiveEventSupport<L extends EventListener> 
    extends AbstractEventSupport<L> {

    /**
     * The serialization unique version identifier.
     */
    private static final long serialVersionUID = 20100310L;
    
    /**
     * Constructs a new ReflectiveEventSupport object and associates it with the
     * object that can be used as the source of all events sent to the 
     * listeners.
     * 
     * @param source the object that can be used as the source of all events
     *        posted to the listeners.
     *        
     * @throws NullPointerException if <code>source</code> is 
     *         <code>null</code>.
     */
    public ReflectiveEventSupport(Object source) {
        super(source);
    }

    /**
     * Fires the provided event object to the named method specified on each of 
     * the listeners registered with this event support class.
     * 
     * @param <E>
     *            the {@link EventObject} type that will be posted to the
     *            listeners.
     * 
     * @param methodName
     *            the name of the method that should be called on each of the
     *            listeners.
     * @param eventObject
     *            the event object that will be passed to the listener's method.
     * 
     * @throws NullPointerException 
     *             if <code>methodName</code> is <code>null</code>.
     * @throws NoSuchMethodException
     *             if there is no such accessible method
     * @throws InvocationTargetException
     *             wraps an exception thrown by the method invoked
     * @throws IllegalAccessException
     *             if the requested method is not accessible via reflection
     */
    public <E extends EventObject> void fireEvent(String methodName, E eventObject) 
        throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
        Validate.notNull(methodName, "methodName cannot be null");
        for (L listener : this) {
            MethodUtils.invokeMethod(listener, methodName, eventObject);
        }
    }
    
    /**
     * Fires the provided event object to the method specified on each of the
     * listeners registered with this event support class.
     * 
     * @param <E>
     *            the {@link EventObject} type that will be posted to the
     *            listeners.
     * @param method
     *            the method that should be called on each of the listeners.
     * @param eventObject
     *            the event object that will be passed to the listener's method.
     * 
     * @throws NullPointerException 
     *             if <code>method</code> is <code>null</code>.
     * @throws NoSuchMethodException 
     *            if there is no such accessible method
     * @throws InvocationTargetException 
     *            wraps an exception thrown by the method invoked
     * @throws IllegalAccessException 
     *            if the requested method is not accessible via reflection
     */
    public <E extends EventObject> void fireEvent(Method method, E eventObject) 
        throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
        Validate.notNull(method, "method cannot be null");
        Method accessibleMethod = MethodUtils.getAccessibleMethod(method);
        for (L listener : this) {
            accessibleMethod.invoke(listener, eventObject);
        }
    }
}
