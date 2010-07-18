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

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import junit.framework.TestCase;

/**
 * <p>
 * The ReflectiveEventSupportTest class provides a set of unit tests for the 
 * {@link ReflectiveEventSupport} class.
 * </p>
 * 
 * @author <a href="mailto:mwooten.dev@gmail.com">Michael Wooten</a>
 * 
 * @since 3.0
 */
public class ReflectiveEventSupportTest extends TestCase {

    /**
     * The event support mock object that will be used for testing.
     */
    private ReflectiveEventSupport<ChangeListener> eventSupport;
    
    /**
     * The first listener that will be registered for change events.
     */
    private ChangeDetectedChangeListener firstChangeListener;
    
    /**
     * The second listener that will be registered for change events.
     */
    private ChangeDetectedChangeListener secondChangeListener;
    
    /**
     * Creates the {@link ReflectiveEventSupport} instance under test and 
     * registers a couple of {@link ChangeDetectedChangeListener}s with the
     * event support.
     */
    protected void setUp() throws Exception {
        eventSupport = new ReflectiveEventSupport<ChangeListener>(this);
        firstChangeListener = new ChangeDetectedChangeListener();
        secondChangeListener = new ChangeDetectedChangeListener();
        eventSupport.addListener(firstChangeListener);
        eventSupport.addListener(secondChangeListener);
    }

    /**
     * Tests the {@link ReflectiveEventSupport#ReflectiveEventSupport(Object)}
     * constructor.
     */
    public void testReflectiveEventSupport() {
        try 
        {
            new ReflectiveEventSupport<ChangeListener>(null);
            fail("ReflectiveEventSupport(null) did not throw an IllegalArgumentException");
        }
        catch (NullPointerException iae)
        {
            // Success, the exception was properly thrown
        }
    }

    /**
     * Tests the 
     * {@link ReflectiveEventSupport#fireEvent(String, java.util.EventObject)}
     * method to ensure that events will be propagated to accessible methods
     * with the provided name.
     * 
     * @throws NoSuchMethodException
     * @throws IllegalAccessException
     * @throws InvocationTargetException
     */
    public void testFireEventByMethodName() throws NoSuchMethodException, 
        IllegalAccessException, InvocationTargetException {
        ChangeEvent changeEvent = new ChangeEvent(eventSupport.getSource());
        eventSupport.fireEvent("stateChanged", changeEvent);
        assertTrue("ChangeEvent not propogated to first change lisetener", 
                firstChangeListener.isChanged());
        assertTrue("ChangeEvent not propogated to second change lisetener", 
                secondChangeListener.isChanged());
    }
    
    /**
     * Tests the 
     * {@link ReflectiveEventSupport#fireEvent(String, java.util.EventObject)}
     * method to ensure that a {@link NoSuchMethodException} is thrown if the
     * method is not accessible.
     * 
     * @throws IllegalAccessException
     * @throws InvocationTargetException
     */
    public void testFireEventByMethodNameToInaccessibleMethods() 
        throws IllegalAccessException, InvocationTargetException {
        
        ChangeEvent changeEvent = new ChangeEvent(eventSupport.getSource());
        
        try
        {
            eventSupport.fireEvent("privateMethod", changeEvent);
            fail("eventSupport.fireEvent() did not throw an exception " +
                "for a private method");
        } 
        catch (NoSuchMethodException nsme)
        {
            // Success
        }
        
        try
        {
            eventSupport.fireEvent("protectedMethod", changeEvent);
            fail("eventSupport.fireEvent() did not throw an exception " +
                "for a protected method");
        } 
        catch (NoSuchMethodException nsme)
        {
            // Success
        }
        
        try
        {
            eventSupport.fireEvent("defaultMethod", changeEvent);
            fail("eventSupport.fireEvent() did not throw an exception " +
                "for a default method");
        } 
        catch (NoSuchMethodException nsme)
        {
            // Success
        }
    }
    
    /**
     * Tests the 
     * {@link ReflectiveEventSupport#fireEvent(String, java.util.EventObject)}
     * method to ensure that a {@link NullPointerException} is thrown if a 
     * <code>null</code> value is provided for the method name.
     * 
     * @throws NoSuchMethodException
     * @throws IllegalAccessException
     * @throws InvocationTargetException
     */
    public void testFireEventNullMethodName() throws NoSuchMethodException, 
        IllegalAccessException, 
        InvocationTargetException {
        try
        {
            ChangeEvent changeEvent = new ChangeEvent(eventSupport.getSource());
            eventSupport.fireEvent((String) null, changeEvent);
            fail("eventSupport.fireEvent() did not throw an exception for a " +
                "null method name.");
        }
        catch (NullPointerException npe)
        {
            // Success
        }
    }

    /**
     * Tests the 
     * {@link ReflectiveEventSupport#fireEvent(Method, java.util.EventObject)}
     * method to ensure that events will be propagated to the accessible method
     * provided.
     * 
     * @throws NoSuchMethodException
     * @throws IllegalAccessException
     * @throws InvocationTargetException
     */
    public void testFireEventByMethod() throws NoSuchMethodException, 
        IllegalAccessException, InvocationTargetException {
        ChangeEvent changeEvent = new ChangeEvent(eventSupport.getSource());
        Method stateChangedMethod = 
            ChangeListener.class.getMethod("stateChanged", ChangeEvent.class);
        eventSupport.fireEvent(stateChangedMethod, changeEvent);
        assertTrue("ChangeEvent not propogated to first change lisetener", 
                firstChangeListener.isChanged());
        assertTrue("ChangeEvent not propogated to second change lisetener", 
                secondChangeListener.isChanged());
    }
    
    /**
     * Tests the 
     * {@link ReflectiveEventSupport#fireEvent(Method, java.util.EventObject)}
     * method to ensure that a {@link NoSuchMethodException} is thrown if the
     * method is not accessible.
     * 
     * @throws IllegalAccessException
     * @throws InvocationTargetException
     */
    public void testFireEventByMethodToInaccessibleMethods() 
        throws IllegalAccessException, InvocationTargetException {
        
        ChangeEvent changeEvent = new ChangeEvent(eventSupport.getSource());
        
        try
        {
            Method privateMethod = 
                ChangeListener.class.getMethod("privateMethod", 
                    ChangeEvent.class);
            eventSupport.fireEvent(privateMethod, changeEvent);
            fail("eventSupport.fireEvent() did not throw an exception " +
                "for a private method");
        } 
        catch (NoSuchMethodException nsme)
        {
            // Success
        }
        
        try
        {
            Method protectedMethod = 
                ChangeListener.class.getMethod("protectedMethod", 
                    ChangeEvent.class);
            eventSupport.fireEvent(protectedMethod, changeEvent);
            fail("eventSupport.fireEvent() did not throw an exception " +
                "for a protected method");
        } 
        catch (NoSuchMethodException nsme)
        {
            // Success
        }
        
        try
        {
            Method defaultMethod = 
                ChangeListener.class.getMethod("defaultMethod", 
                    ChangeEvent.class);
            eventSupport.fireEvent(defaultMethod, changeEvent);
            fail("eventSupport.fireEvent() did not throw an exception " +
                "for a default method");
        } 
        catch (NoSuchMethodException nsme)
        {
            // Success
        }
    }

    /**
     * Tests the 
     * {@link ReflectiveEventSupport#fireEvent(Method, java.util.EventObject)}
     * method to ensure that a {@link NullPointerException} is thrown if a 
     * <code>null</code> value is provided for the method.
     * 
     * @throws NoSuchMethodException
     * @throws IllegalAccessException
     * @throws InvocationTargetException
     */
    public void testFireEventNullMethod() throws NoSuchMethodException, 
        IllegalAccessException, InvocationTargetException {
        try
        {
            ChangeEvent changeEvent = new ChangeEvent(eventSupport.getSource());
            eventSupport.fireEvent((Method) null, changeEvent);
            fail("eventSupport.fireEvent() did not throw an exception for a " +
                "null method.");
        }
        catch (NullPointerException npe)
        {
            // Success
        }
    }
    
    /**
     * Tests the {@link ReflectiveEventSupport#getSource()} method to ensure it
     * returns the source object it was originally provided.
     */
    public void testGetSource() {
        assertEquals(this, eventSupport.getSource());
    }

    /**
     * <p>
     * The ChangeDetectedChangeListener class provides a version of the
     * {@link ChangeListener} interface that detects when the listener has 
     * been called. The class provides an {@link #isChanged()} method that
     * will indicate whether or not the listener has been called.
     * </p>
     */
    public class ChangeDetectedChangeListener implements ChangeListener {
        
        /**
         * Represents whether or not the listener has detected a change.
         */
        private boolean changed = false;
        
        /**
         * Called whenever a change is detected.
         * 
         * @param changeEvent the change event indicating a state change.
         */
        public void stateChanged(ChangeEvent changeEvent) {
            this.changed = true;
        }
        
        /**
         * Returns whether or not the listener has detected a change event.
         * 
         * @return <code>true</code> if the listener has detected a change
         *         event, <code>false</code> otherwise.
         */
        public boolean isChanged() {
            return this.changed;
        }
        
        /**
         * A default (package private) method used to test 
         * ReflectiveEventSupport calls to inaccessible methods.
         * 
         * @param changeEvent not used.
         */
        void defaultMethod(ChangeEvent changeEvent) {
            this.changed = true;
        }
        
        /**
         * A protected method used to test ReflectiveEventSupport calls to 
         * inaccessible methods.
         * 
         * @param changeEvent not used.
         */
        protected void protectedMethod(ChangeEvent changeEvent) {
            this.changed = true;
        }
        
        /**
         * A private method used to test ReflectiveEventSupport calls to 
         * inaccessible methods.
         * 
         * @param changeEvent not used.
         */
        @SuppressWarnings("unused")
        private void privateMethod(ChangeEvent changeEvent) {
            this.changed = true;
        }
    }
}
