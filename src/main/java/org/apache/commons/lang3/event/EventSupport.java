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

import java.util.EventListener;

/**
 * <p>
 * The EventSupport interface identifies as class as being able to register
 * listeners for events. 
 * </p>
 * 
 * @author <a href="mailto:mwooten.dev@gmail.com">Michael Wooten</a>
 *
 * @param <L> the subclass of {@link EventListener} that this event support
 *        class can register.
 *        
 * @since 3.0
 */
public interface EventSupport<L extends EventListener> {

    /**
     * Registers a listener to receive events posted the by the supported class.
     * 
     * @param listener the listener to register for posted events. Values of 
     *        <code>null</code> will be ignored.
     */
    public void addListener(L listener);
    
    /**
     * Unregisters a listener from receiving events posted by the supported 
     * class.
     * 
     * @param listener the listener to stop posting events to. Values of 
     *        <code>null</code> will be ignored.
     */
    public void removeListener(L listener);
}
