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
package org.apache.commons.lang.reflect;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Member;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
/**
 * <code>MethodUtils</code> contains utility methods for working for
 * methods by reflection.
 * <p>
 * The ability is provided to break the scoping restrictions coded by the
 * programmer. This can break an implementation if used incorrectly. This
 * facility should be used with care.
 *
 * @author <a href="mailto:scolebourne@apache.org">Stephen Colebourne</a>
 * @author Based on code from <code>BeanUtils</code> by: Craig R. McClanahan
 * @author Ralph Schaer
 * @author Chris Audley
 * @author Rey François
 * @author Gregor Raýman
 * @author Jan Sorensen
 * @author Robert Burrell Donkin
 * @version $Id: MethodUtils.java,v 1.5 2002/11/20 21:45:47 rdonkin Exp $
 */
public class MethodUtils {
    
    public static final boolean debug = false;
    
    /** An empty method array */
    public static final Method[] EMPTY_METHOD_ARRAY = new Method[0];
    
    /**
     * MethodUtils instances should NOT be constructed in standard programming.
     * Instead, the class should be used as <code>MethodUtils.getMethod(cls, name)</code>.
     * This constructor is public to permit tools that require a JavaBean instance
     * to operate.
     */
    public MethodUtils() {
    }

    // -------------------------------------------------------------------------
    
    /**
     * Gets a Method by name. The method must be public and take no parameters.
     * Superclasses will be considered.
     *
     * @param cls  the class to reflect, must not be null
     * @param methodName  the field name to obtain
     * @return the Method object
     * @throws IllegalArgumentException if the class or method name is null
     * @throws ReflectionException if an error occurs during reflection
     */
    public static Method getMethod(Class cls, String methodName) {
        return getMethod(cls, methodName, ArrayUtils.EMPTY_CLASS_ARRAY, false);
    }
    
    /**
     * Gets a Method by name. The method must be public.
     * Superclasses will be considered.
     *
     * @param cls  the class to reflect, must not be null
     * @param methodName  the field name to obtain
     * @return the Method object
     * @throws IllegalArgumentException if the class or method name is null
     * @throws ReflectionException if an error occurs during reflection
     */
    public static Method getMethod(Class cls, String methodName, Class[] paramTypes) {
        return getMethod(cls, methodName, paramTypes, false);
    }
    
    /**
     * Gets a Method by name.
     * Superclasses will be considered.
     *
     * @param cls  the class to reflect, must not be null
     * @param methodName  the method name to obtain
     * @param breakScope  whether to break scope restrictions using the
     *  <code>setAccessible</code> method. False will only match public fields.
     * @return the Method object
     * @throws IllegalArgumentException if the class or field name is null
     * @throws ReflectionException if an error occurs during reflection
     */
    public static Method getMethod(Class cls, String methodName, Class[] paramTypes, boolean breakScope) {
        if (cls == null) {
            throw new IllegalArgumentException("The class must not be null");
        }
        if (methodName == null) {
            throw new IllegalArgumentException("The method name must not be null");
        }
        try {
            if (breakScope) {
                try {
                    // most common case, always do this for speed
                    return cls.getMethod(methodName, paramTypes);  // must be public
                } catch (NoSuchMethodException ex) {
                    // ignore
                }
                Class acls = cls;
                while (acls != null) {
                    Method[] methods = acls.getDeclaredMethods();
                    for (int i = 0; i < methods.length; i++) {
                        if (methods[i].getName().equals(methodName) &&
                            ReflectionUtils.isCompatable(paramTypes, methods[i].getParameterTypes())) {
                            if (Modifier.isPublic(methods[i].getModifiers())) {
                                methods[i].setAccessible(true);
                            }
                            return methods[i];
                        }
                    }
                    acls = acls.getSuperclass();  // TODO interfaces
                }
                throw new NoSuchMethodException("The method '" + methodName + "' could not be found");
            } else {
                return cls.getMethod(methodName, paramTypes);
            }
    
        } catch (ReflectionException ex) {
            throw ex;
        } catch (LinkageError ex) {
            throw new ReflectionException(ReflectionUtils.getThrowableText(
                ex, "getting method", cls.getName(), null, methodName), ex);
        } catch (Exception ex) {
            throw new ReflectionException(ReflectionUtils.getThrowableText(
                ex, "getting method", cls.getName(), null, methodName), ex);
        }
    }
    
    // -------------------------------------------------------------------------
    
    /**
     * <p>Invoke a named method whose parameter type matches the object type.</p>
     *
     * <p>This method supports calls to methods taking primitive parameters 
     * via passing in wrapping classes. So, for example, a <code>Boolean</code> class
     * would match a <code>boolean</code> primitive.</p>
     *
     * <p> This is a convenient wrapper for
     * {@link #invokeMethod(Object object,String methodName,Object [] args)}.
     * </p>
     *
     * @param objectToInvoke  invoke method on this object, must not be null
     * @param methodName  get method with this name, must not be null
     * @param arg  use this argument, must not be null
     *
     * @throws NoSuchMethodException if there is no such accessible method
     * @throws InvocationTargetException wraps an exception thrown by the
     *  method invoked
     * @throws IllegalAccessException if the requested method is not accessible
     *  via reflection
     * @throws IllegalArgumentException if any parameter is null
     */
    public static Object invokeMethod(
            Object objectToInvoke,
            String methodName,
            Object arg)
                throws
                    NoSuchMethodException,
                    IllegalAccessException,
                    InvocationTargetException {

        if (objectToInvoke == null) {
            throw new IllegalArgumentException("The object to invoke must not be null");
        }
        if (methodName == null) {
            throw new IllegalArgumentException("The method name must not be null");
        }
        if (arg == null) {
            throw new IllegalArgumentException("The argument must not be null");
        }
        Object[] args = {arg};
        return invokeMethod(objectToInvoke, methodName, args);
    }

    /**
     * <p>Invoke a named method whose parameter type matches the object type.</p>
     *
     * <p>This method supports calls to methods taking primitive parameters 
     * via passing in wrapping classes. So, for example, a <code>Boolean</code> class
     * would match a <code>boolean</code> primitive.</p>
     *
     * <p> This is a convenient wrapper for
     * {@link #invokeMethod(Object object,String methodName,Object [] args,Class[] parameterTypes)}.
     * </p>
     *
     * @param objectToInvoke  invoke method on this object, must not be null
     * @param methodName  get method with this name, must not be null
     * @param args  use these arguments - treat null as empty array
     *
     * @throws NoSuchMethodException if there is no such accessible method
     * @throws InvocationTargetException wraps an exception thrown by the
     *  method invoked
     * @throws IllegalAccessException if the requested method is not accessible
     *  via reflection
     * @throws IllegalArgumentException if the objectToInvoke, methodName or any argument is null
     */
    public static Object invokeMethod(
            Object objectToInvoke,
            String methodName,
            Object[] args)
                throws
                    NoSuchMethodException,
                    IllegalAccessException,
                    InvocationTargetException {
        
        if (objectToInvoke == null) {
            throw new IllegalArgumentException("The object to invoke must not be null");
        }
        if (methodName == null) {
            throw new IllegalArgumentException("The method name must not be null");
        }
        if (args == null) {
            return invokeMethod(objectToInvoke, methodName, null, null);
        } else {
            int arguments = args.length;
            Class parameterTypes [] = new Class[arguments];
            for (int i = 0; i < arguments; i++) {
                if (args[i] == null) {
                    throw new IllegalArgumentException("The arguments must not be null. Index " + i + " was null.");
                }
                parameterTypes[i] = args[i].getClass();
            }
            return invokeMethod(objectToInvoke, methodName, args, parameterTypes);
        }
    }

    /**
     * <p>Invoke a named method whose parameter type matches the object type.</p>
     *
     * <p>This method supports calls to methods taking primitive parameters 
     * via passing in wrapping classes. So, for example, a <code>Boolean</code> class
     * would match a <code>boolean</code> primitive.</p>
     *
     *
     * @param object  invoke method on this object
     * @param methodName  get method with this name
     * @param args  use these arguments - treat null as empty array
     * @param parameterTypes  match these parameters - treat null as empty array
     *
     * @throws NoSuchMethodException if there is no such accessible method
     * @throws InvocationTargetException wraps an exception thrown by the
     *  method invoked
     * @throws IllegalAccessException if the requested method is not accessible
     *  via reflection
     */
    public static Object invokeMethod(
            Object object,
            String methodName,
            Object[] args,
            Class[] parameterTypes)
                throws
                    NoSuchMethodException,
                    IllegalAccessException,
                    InvocationTargetException {
                    
        if (parameterTypes == null) {
            parameterTypes = ArrayUtils.EMPTY_CLASS_ARRAY;
        }        
        if (args == null) {
            args = ArrayUtils.EMPTY_OBJECT_ARRAY;
        }  

        Method method = getMatchingAccessibleMethod(
                object.getClass(),
                methodName,
                parameterTypes);
        if (method == null)
            throw new NoSuchMethodException("No such accessible method: " +
                    methodName + "() on object: " + object.getClass().getName());
        return method.invoke(object, args);
    }

    /**
     * <p>Return an accessible method (that is, one that can be invoked via
     * reflection) with given name and a single parameter.  If no such method
     * can be found, return <code>null</code>.
     * Basically, a convenience wrapper that constructs a <code>Class</code>
     * array for you.</p>
     *
     * @param clazz get method from this class
     * @param methodName get method with this name
     * @param parameterType taking this type of parameter
     */
    public static Method getAccessibleMethod(
            Class clazz,
            String methodName,
            Class parameterType) {

        Class[] parameterTypes = {parameterType};
        return getAccessibleMethod(clazz, methodName, parameterTypes);

    }


    /**
     * <p>Return an accessible method (that is, one that can be invoked via
     * reflection) with given name and parameters.  If no such method
     * can be found, return <code>null</code>.
     * This is just a convenient wrapper for
     * {@link #getAccessibleMethod(Method method)}.</p>
     *
     * @param clazz get method from this class
     * @param methodName get method with this name
     * @param parameterTypes with these parameters types
     */
    public static Method getAccessibleMethod(
            Class clazz,
            String methodName,
            Class[] parameterTypes) {

        try {
            return getAccessibleMethod
                    (clazz.getMethod(methodName, parameterTypes));
        } catch (NoSuchMethodException e) {
            return (null);
        }

    }


    /**
     * <p>Return an accessible method (that is, one that can be invoked via
     * reflection) that implements the specified Method.  If no such method
     * can be found, return <code>null</code>.</p>
     *
     * @param method The method that we wish to call
     */
    public static Method getAccessibleMethod(Method method) {

        // Make sure we have a method to check
        if (method == null) {
            return (null);
        }

        // If the requested method is not public we cannot call it
        if (!Modifier.isPublic(method.getModifiers())) {
            return (null);
        }

        // If the declaring class is public, we are done
        Class clazz = method.getDeclaringClass();
        if (Modifier.isPublic(clazz.getModifiers())) {
            return (method);
        }

        // Check the implemented interfaces and subinterfaces
        String methodName = method.getName();
        Class[] parameterTypes = method.getParameterTypes();
        method =
                getAccessibleMethodFromInterfaceNest(clazz,
                        method.getName(),
                        method.getParameterTypes());
        return (method);

    }


    // -------------------------------------------------------- Private Methods

    /**
     * <p>Return an accessible method (that is, one that can be invoked via
     * reflection) that implements the specified method, by scanning through
     * all implemented interfaces and subinterfaces.  If no such method
     * can be found, return <code>null</code>.</p>
     *
     * <p> There isn't any good reason why this method must be private.
     * It is because there doesn't seem any reason why other classes should
     * call this rather than the higher level methods.</p>
     *
     * @param clazz Parent class for the interfaces to be checked
     * @param methodName Method name of the method we wish to call
     * @param parameterTypes The parameter type signatures
     */
    private static Method getAccessibleMethodFromInterfaceNest
            (Class clazz, String methodName, Class parameterTypes[]) {

        Method method = null;

        // Search up the superclass chain
        for (; clazz != null; clazz = clazz.getSuperclass()) {

            // Check the implemented interfaces of the parent class
            Class interfaces[] = clazz.getInterfaces();
            for (int i = 0; i < interfaces.length; i++) {

                // Is this interface public?
                if (!Modifier.isPublic(interfaces[i].getModifiers()))
                    continue;

                // Does the method exist on this interface?
                try {
                    method = interfaces[i].getDeclaredMethod(methodName,
                            parameterTypes);
                } catch (NoSuchMethodException e) {
                    ;
                }
                if (method != null)
                    break;

                // Recursively check our parent interfaces
                method =
                        getAccessibleMethodFromInterfaceNest(interfaces[i],
                                methodName,
                                parameterTypes);
                if (method != null)
                    break;

            }

        }

        // If we found a method return it
        if (method != null)
            return (method);

        // We did not find anything
        return (null);

    }


    /**
     * <p>Find an accessible method that matches the given name and has compatible parameters.
     * Compatible parameters mean that every method parameter is assignable from 
     * the given parameters.
     * In other words, it finds a method with the given name 
     * that will take the parameters given.<p>
     *
     * <p>This method is slightly undeterminstic since it loops 
     * through methods names and return the first matching method.</p>
     * 
     * <p>This method is used by 
     * {@link 
     * #invokeMethod(Object object,String methodName,Object [] args,Class[] parameterTypes)}.
     *
     * <p>This method can match primitive parameter by passing in wrapper classes.
     * For example, a <code>Boolean</code> will match a primitive <code>boolean</code>
     * parameter.
     *
     * @param clazz find method in this class
     * @param methodName find method with this name
     * @param parameterTypes find method with compatible parameters 
     */
    private static Method getMatchingAccessibleMethod(
                                                Class clazz,
                                                String methodName,
                                                Class[] parameterTypes) {
        // trace logging
        if (debug) {
            log("Matching name=" + methodName + " on " + clazz);
        }
        
        // see if we can find the method directly
        // most of the time this works and it's much faster
        try {
            Method method = clazz.getMethod(methodName, parameterTypes);
            if (debug) {
                log("Found straight match: " + method);
                log("isPublic:" + Modifier.isPublic(method.getModifiers()));
            }
            
            try {
                //
                // XXX Default access superclass workaround
                //
                // When a public class has a default access superclass
                // with public methods, these methods are accessible.
                // Calling them from compiled code works fine.
                //
                // Unfortunately, using reflection to invoke these methods
                // seems to (wrongly) to prevent access even when the method
                // modifer is public.
                //
                // The following workaround solves the problem but will only
                // work from sufficiently privilages code. 
                //
                // Better workarounds would be greatfully accepted.
                //
                method.setAccessible(true);
                
            } catch (SecurityException se) {
                // log but continue just in case the method.invoke works anyway
                log(
                "Cannot setAccessible on method. Therefore cannot use jvm access bug workaround.", 
                se);
            }
            return method;
            
        } catch (NoSuchMethodException e) { /* SWALLOW */ }
        
        // search through all methods 
        int paramSize = parameterTypes.length;
        Method[] methods = clazz.getMethods();
        for (int i = 0, size = methods.length; i < size ; i++) {
            if (methods[i].getName().equals(methodName)) {	
                // log some trace information
                if (debug) {
                    log("Found matching name:");
                    log(methods[i]);
                }                
                
                // compare parameters
                Class[] methodsParams = methods[i].getParameterTypes();
                int methodParamSize = methodsParams.length;
                if (methodParamSize == paramSize) {          
                    boolean match = true;
                    for (int n = 0 ; n < methodParamSize; n++) {
                        if (debug) {
                            log("Param=" + parameterTypes[n].getName());
                            log("Method=" + methodsParams[n].getName());
                        }
                        if (!ReflectionUtils.isCompatable(parameterTypes[n], methodsParams[n])) {
                            if (debug) {
                                log(methodsParams[n] + " is not assignable from " 
                                            + parameterTypes[n]);
                            }    
                            match = false;
                            break;
                        }
                    }
                    
                    if (match) {
                        // get accessible version of method
                        Method method = getAccessibleMethod(methods[i]);
                        if (method != null) {
                            if (debug) {
                                log(method + " accessible version of " 
                                            + methods[i]);
                            }
                            try {
                                //
                                // XXX Default access superclass workaround
                                // (See above for more details.)
                                //
                                method.setAccessible(true);
                                
                            } catch (SecurityException se) {
                                // log but continue just in case the method.invoke works anyway
                                log(
                                "Cannot setAccessible on method. Therefore cannot use jvm access bug workaround.", 
                                se);
                            }
                            return method;
                        }
                        
                        log("Couldn't find accessible method.");
                    }
                }
            }
        }
        
        // didn't find a match
        log("No match found.");
        return null;                                        
    }    
    
    private static void log(Object o) {
        if (debug) {
            System.err.println(o);
        }
    }
    
    private static void log(Object o, Throwable t) {
        if (debug) {
            System.err.println(o);
            System.err.println(t);
            
        }
    }
}
