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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;

import org.apache.commons.lang.ArrayUtils;
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
 * @author Gary Gregory
 * @version $Id: MethodUtils.java,v 1.11 2003/01/25 13:01:38 scolebourne Exp $
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
    public static Method getMethod(Class cls, String methodName, Class paramType) {
        Class[] paramTypes = {paramType};
        return getMethod(cls, methodName, paramTypes);
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
                            ReflectionUtils.isCompatible(paramTypes, methods[i].getParameterTypes())) {
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
                // apply workarounds
                Method method = null;
                try {
                
                    method = cls.getMethod(methodName, paramTypes);
                    
                } catch(NoSuchMethodException e) {
                    // swallow
                }
                
                if (method == null) {
                    // use the same as beanutils for the moment
                    Method[] compatibles = getCompatibleMethods(cls, methodName, paramTypes);
                    if (compatibles.length > 0) {
                        method = compatibles[0];
                    }
                }
                return getMethod(method);
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

    /**
     * <p>Return an accessible method (that is, one that can be invoked via
     * reflection) that implements the specified Method.  If no such method
     * can be found, return <code>null</code>.</p>
     *
     * @param method The method that we wish to call
     */
    public static Method getMethod(Method method) {
        
        Method accessibleMethod = getAccessibleMethod(method);
        if (accessibleMethod == null) {
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
                if (ReflectionUtils.isPublicScope(method)) {
                    method.setAccessible(true);
                    accessibleMethod = method;
                }
                
            } catch (SecurityException se) {
                // log but continue just in case the method.invoke works anyway
                log(
                "Cannot setAccessible on method. Therefore cannot use jvm access bug workaround.", 
                se);
            }
        }
        return (accessibleMethod);

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
     * @throws ReflectionException if an error occurs during reflection
     * @throws IllegalArgumentException if any parameter is null
     */
    public static Object invokeMethod(
            Object objectToInvoke,
            String methodName,
            Object arg)
                throws
                    ReflectionException {

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
     * @throws ReflectionException if an error occurs during reflection
     * @throws IllegalArgumentException if the objectToInvoke, methodName or any argument is null
     */
    public static Object invokeMethod(
            Object objectToInvoke,
            String methodName,
            Object[] args)
                throws
                    ReflectionException {
        
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
     * @throws ReflectionException if an error occurs during reflection
     */
    public static Object invokeMethod(
            Object object,
            String methodName,
            Object[] args,
            Class[] parameterTypes)
                throws
                    ReflectionException {
                    
        if (parameterTypes == null) {
            parameterTypes = ArrayUtils.EMPTY_CLASS_ARRAY;
        }        
        if (args == null) {
            args = ArrayUtils.EMPTY_OBJECT_ARRAY;
        }  

        Method method = getMethod(
                object.getClass(),
                methodName,
                parameterTypes);
        if (method == null)
            throw new ReflectionException("No such accessible method: " +
                    methodName + "() on object: " + object.getClass().getName());
        
        try {
        
            return method.invoke(object, args);
            
        } catch (IllegalAccessException ex) {
            throw new ReflectionException(
                ReflectionUtils.getThrowableText(
                    ex, "invoking method", object.getClass().getName(), parameterTypes, methodName)
                , ex);
        
        } catch (InvocationTargetException ex) {
            throw new ReflectionException(
                ReflectionUtils.getThrowableText(
                    ex, "invoking method", object.getClass().getName(), parameterTypes, methodName)
                , ex);
        
        }
    }



    // -------------------------------------------------------- Private Methods

    private static Method getAccessibleMethod(Method method) {

        // Make sure we have a method to check
        if (method == null) {
            return (null);
        }

        // If the requested method is not public we cannot call it
        if (!Modifier.isPublic(method.getModifiers())) {
            log("Method is not public");
            return (null);
        }

        // If the declaring class is public, we are done
        Class clazz = method.getDeclaringClass();
        if (Modifier.isPublic(clazz.getModifiers())) {
            log("Class is public");
            return (method);
        }
        
        if (debug) {
            log("Method is in non-public class " + clazz);
        }

        // Check the implemented interfaces and subinterfaces
        method =
                getAccessibleMethodFromInterfaceNest(clazz,
                        method.getName(),
                        method.getParameterTypes());

        return (method);

    }


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
        if (debug) {
            log("Finding accessible method " + methodName + " from interface nest");
        }
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
        if (method != null) {
            if (debug) {
                log("Found method in class " + method.getDeclaringClass());
            }
            return (method);
        }
        // We did not find anything
        return (null);

    } 
    
    private static Method[] getCompatibleMethods(
                                                Class clazz,
                                                String methodName,
                                                Class[] parameterTypes) {
        // trace logging
        if (debug) {
            log("Matching name=" + methodName + " on " + clazz);
        }
        
        // search through all methods 
        Method[] methods = clazz.getMethods();
        ArrayList compatibles = new ArrayList(methods.length);
        for (int i = 0, size = methods.length; i < size ; i++) {
            if (debug) {
                log("Checking: " + methods[i]);
            }     
            if (methods[i].getName().equals(methodName)) {	
                // log some trace information
                if (debug) {
                    log("Found matching name:" + methods[i]);
                }                
                
                // compare parameters
                Class[] methodsParams = methods[i].getParameterTypes();
                if (ReflectionUtils.isCompatible(parameterTypes, methodsParams)) {
                    // get accessible version of method
                    Method method = getMethod(methods[i]);
                    if (method != null) {
                        compatibles.add(method);
                    } else {
                        log("Couldn't find accessible method for: " + methods[i]);
                    }
                }
            }
        }
        
        return (Method[]) compatibles.toArray(new Method[compatibles.size()]); 
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
