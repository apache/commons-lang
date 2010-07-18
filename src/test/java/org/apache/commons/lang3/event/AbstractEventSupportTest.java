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

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import junit.framework.TestCase;

/**
 * <p>
 * The {@link AbstractEventSupportTestCase} class provides test cases for 
 * testing the {@link AbstractEventSupport} class.
 * </p>
 * 
 * @author <a href="mailto:mwooten.dev@gmail.com">Michael Wooten</a>
 *
 * @since 3.0
 */
public class AbstractEventSupportTest extends TestCase {

    /**
     * The event support mock object that will be used for testing.
     */
    private AbstractEventSupport<ChangeListener> eventSupport;
    
    /**
     * Creates the implementation of {@link AbstractEventSupport} that will be
     * used for testing.
     */
    public void setUp() throws Exception {
        eventSupport = new AbstractEventSupportMock(this);
    }
    
    /**
     * Tests that the 
     * {@link AbstractEventSupport#addListener(java.util.EventListener)}
     * properly registers a listener.
     */
    public void testAddListener() {
        ChangeListener changeListener = new ChangeListenerMock();
        eventSupport.addListener(changeListener);
        assertTrue(eventSupport.iterator().hasNext());
        assertEquals(changeListener, eventSupport.iterator().next());
    }
    
    /**
     * Tests that the 
     * {@link AbstractEventSupport#addListener(java.util.EventListener)}
     * method performs no operation when provided the value of 
     * <code>null</code>.
     */
    public void testAddNullListener() {
        eventSupport.addListener(null);
        assertFalse(eventSupport.iterator().hasNext());
    }

    /**
     * Tests that the 
     * {@link AbstractEventSupport#removeListener(java.util.EventListener)}
     * properly removes a previously registered listener.
     */
    public void testRemoveListener() {
        ChangeListener changeListener = new ChangeListenerMock();
        eventSupport.addListener(changeListener);
        assertTrue(eventSupport.iterator().hasNext());
        eventSupport.removeListener(changeListener);
        assertFalse(eventSupport.iterator().hasNext());
    }
    
    /**
     * Tests that the 
     * {@link AbstractEventSupport#removeListener(java.util.EventListener)}
     * method performs no operation when provided the value of 
     * <code>null</code>.
     */
    public void testRemoveNullListener() {
        ChangeListener changeListener = new ChangeListenerMock();
        eventSupport.addListener(changeListener);
        assertTrue(eventSupport.iterator().hasNext());
        eventSupport.removeListener(null);
        assertTrue(eventSupport.iterator().hasNext());
    }

    /**
     * Tests that the source registered with the event support is the one 
     * provided by {@link AbstractEventSupport#getSource()}.
     */
    public void testGetSource() {
        assertEquals(this, eventSupport.getSource());
    }

    /**
     * Tests that the event support object returns an iterator of the registered
     * listeners.
     */
    public void testIterator() {
        ChangeListener changeListener = new ChangeListenerMock();
        eventSupport.addListener(changeListener);
        assertNotNull(eventSupport.iterator());
        assertTrue(eventSupport.iterator().hasNext());
    }
    
    /**
     * <p>
     * The AbstractEventSupportMock class provides a mock version of the
     * {@link AbstractEventSupport} class that can be used for testing since
     * AbstractEventSupport is an abstract class.
     * </p>
     */
    @SuppressWarnings("serial")
    private class AbstractEventSupportMock extends AbstractEventSupport<ChangeListener> {

        /**
         * Constructs a new AbstractEventSupportMock and associates it with the
         * source of the events.
         * 
         * @param source the source of the events.
         */
        public AbstractEventSupportMock(Object source) {
            super(source);
        }
    }
    
    /**
     * <p>
     * The ChangeListenerMock class provides a mock version of the
     * {@link ChangeListener} interface that can be used for testing.
     * </p>
     */
    private class ChangeListenerMock implements ChangeListener {
        public void stateChanged(ChangeEvent changeEvent) {
        }
    }
}
