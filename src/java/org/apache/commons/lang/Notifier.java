/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
 *
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 */
package org.apache.commons.lang;

import java.util.ArrayList;
import java.util.Collections;
import java.util.EventObject;
import java.util.Iterator;
import java.util.List;

import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;

/**
 * <p>A utility which takes much of the pain out of the Event/Listener
 * system. It handles the collection, and the loop-notification.
 * Reflection is used for the actual notification call.</p>
 *
 * <p>Alternate strategies are usable. For example this class currently
 * does not enforce a particular interface, which means it cannot 
 * cache that method. Doing this probably makes a lot of sense.</p>
 */
public class Notifier {

    private ArrayList listeners = new ArrayList();
    private Method listenerMethod;
    private String methodName;
    private Class clss;

    public Notifier(Class listener) {
        if(listener == null) {
            throw new IllegalArgumentException("Illegal to have a null listener Class. ");
        }
        
        this.clss = clss;
        // now we check methods, if only one of them, then 
        // let's set it
        Method[] meths = clss.getDeclaredMethods();
        if(meths.length == 0) {
            this.listenerMethod = meths[0];
        }
    }

    /**
     * <p>Construct with the class and the name of the method to
     * call upon the listeners.</p>
     */
    public Notifier(Class clss, String name) {
        if(clss == null) {
            throw new IllegalArgumentException("Illegal to have a null Listener Class. ");
        }
        if(name == null) {
            throw new IllegalArgumentException("Illegal to have a null method name. ");
        }
        this.clss = clss;
        this.methodName = name;
        try {
            // then we get the Method object
            this.listenerMethod = this.clss.getDeclaredMethod(name, new Class[] { EventObject.class} );
        } catch(NoSuchMethodException nsme) {
//            nsme.printStackTrace();
        throw new IllegalArgumentException("Method not on Class. ");
        }
    }

    public void addListener(Object not) {
        this.listeners.add(not);
    }

    public void removeListener(Object not) {
        this.listeners.remove(not);
    }

    public List getListeners() {
        ArrayList cloned = new ArrayList(listeners);
        return Collections.unmodifiableList(cloned);
    }

    /**
     * <p>Convenience method for when a listener has a single method.</p>
     *
     * <p>Currently this method needs to be called, but it's possible
     * that by providing the interface class, it can be assumed as 
     * to what the listening method is.</p>
     */
    public void notify(EventObject event) throws NotifierException {
        if(this.clss == null) {
            notify(this.methodName, event);
        } else {
            notify(this.listenerMethod, event);
        }
    }

    /**
     * <p>Notify the listeners of a certain event, to a certain method.</p>
     *
     * <p>This is usable when a Listener has more than one method and
     * a single <code>Notifier</code> wants to be shared.</p>
     */
    private void notify(Method listenerMethod, EventObject event) throws NotifierException {
        Iterator itr = getListeners().iterator();
        while(itr.hasNext()) {
            try {
                Object listener = itr.next();
                listenerMethod.invoke( listener, new Object[] { event } );
            } catch(SecurityException se) {
                throw new NotifierException(se);
            } catch(IllegalAccessException iae) {
                throw new NotifierException(iae);
            } catch(IllegalArgumentException iae) {
                throw new NotifierException(iae);
            } catch(InvocationTargetException ite) {
                throw new NotifierException(ite);
            }
        }
    }

    /**
     * <p>Notify the listeners of a certain event, to a certain method.</p>
     *
     * <p>This is usable when a Listener has more than one method and
     * a single Notifier wants to be shared.</p>
     */
    public void notify(String methodName, EventObject event) throws NotifierException {
        Iterator itr = getListeners().iterator();
        while(itr.hasNext()) {
            try {
                Object listener = itr.next();
                Class clss = listener.getClass();
                Method method = clss.getMethod(methodName, new Class[] { event.getClass() } );
                method.invoke( listener, new Object[] { event } );
            } catch(SecurityException se) {
                throw new NotifierException(se);
            } catch(NoSuchMethodException nsme) {
                throw new NotifierException(nsme);
            } catch(IllegalAccessException iae) {
                throw new NotifierException(iae);
            } catch(IllegalArgumentException iae) {
                throw new NotifierException(iae);
            } catch(InvocationTargetException ite) {
                throw new NotifierException(ite);
            }
        }
    }

}
