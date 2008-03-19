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
package org.apache.commons.lang;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import java.util.Arrays;
import java.util.WeakHashMap;

/**
 * <p> Utility reflection methods focused on methods, originally from Commons BeanUtils.
 * Differences from the BeanUtils version may be noted, especially where similar functionality
 * already existed within Lang.
 * </p>
 *
 * <h3>Known Limitations</h3>
 * <h4>Accessing Public Methods In A Default Access Superclass</h4>
 * <p>There is an issue when invoking public methods contained in a default access superclass on JREs prior to 1.4.
 * Reflection locates these methods fine and correctly assigns them as public.
 * However, an <code>IllegalAccessException</code> is thrown if the method is invoked.</p>
 *
 * <p><code>MethodUtils</code> contains a workaround for this situation. 
 * It will attempt to call <code>setAccessible</code> on this method.
 * If this call succeeds, then the method can be invoked as normal.
 * This call will only succeed when the application has sufficient security privileges. 
 * If this call fails then the method may fail.</p>
 *
 * @author Craig R. McClanahan
 * @author Ralph Schaer
 * @author Chris Audley
 * @author Rey Fran&#231;ois
 * @author Gregor Ra&#253;man
 * @author Jan Sorensen
 * @author Robert Burrell Donkin
 * @author Niall Pemberton
 * @author Matt Benson
 * @since 2.5
 * @version $Id$
 */
public class MethodUtils {

    /**
     * Stores a cache of MethodDescriptor -> Method in a WeakHashMap.
     * <p>
     * The keys into this map only ever exist as temporary variables within
     * methods of this class, and are never exposed to users of this class.
     * This means that the WeakHashMap is used only as a mechanism for 
     * limiting the size of the cache, ie a way to tell the garbage collector
     * that the contents of the cache can be completely garbage-collected 
     * whenever it needs the memory. Whether this is a good approach to
     * this problem is doubtful; something like the commons-collections
     * LRUMap may be more appropriate (though of course selecting an
     * appropriate size is an issue).
     * <p>
     * This static variable is safe even when this code is deployed via a
     * shared classloader because it is keyed via a MethodDescriptor object
     * which has a Class as one of its members and that member is used in
     * the MethodDescriptor.equals method. So two components that load the same
     * class via different classloaders will generate non-equal MethodDescriptor
     * objects and hence end up with different entries in the map.
     */
    private static final WeakHashMap/* <MethodDescriptor, Method> */cache = new WeakHashMap();

    /**
     * Indicates whether methods should be cached for improved performance.
     * <p>
     * Note that when this class is deployed via a shared classloader in
     * a container, this will affect all webapps. However making this
     * configurable per webapp would mean having a map keyed by context classloader
     * which may introduce memory-leak problems.
     */
    private static boolean cacheMethods = true;

    /**
     * <p>MethodUtils instances should NOT be constructed in standard programming.
     * Instead, the class should be used as
     * <code>MethodUtils.getAccessibleMethod(method)</code>.</p>
     *
     * <p>This constructor is public to permit tools that require a JavaBean
     * instance to operate.</p>
     */
    public MethodUtils() {
        super();
    }

    /**
     * Set whether methods should be cached for greater performance or not,
     * default is <code>true</code>.
     *
     * @param cacheMethods <code>true</code> if methods should be
     * cached for greater performance, otherwise <code>false</code>
     */
    public static synchronized void setCacheMethods(boolean cacheMethods) {
        MethodUtils.cacheMethods = cacheMethods;
        if (!MethodUtils.cacheMethods) {
            clearCache();
        }
    }

    /**
     * Clear the method cache.
     * @return the number of cached methods cleared
     */
    public static synchronized int clearCache() {
        int size = cache.size();
        cache.clear();
        return size;
    }

    /**
     * <p>Invoke a named method whose parameter type matches the object type.</p>
     *
     * <p>This method delegates the method search to {@link #getMatchingAccessibleMethod(Class, String, Class[])}.</p>
     *
     * <p>This method supports calls to methods taking primitive parameters 
     * via passing in wrapping classes. So, for example, a <code>Boolean</code> object
     * would match a <code>boolean</code> primitive.</p>
     *
     * <p> This is a convenient wrapper for
     * {@link #invokeMethod(Object object, String methodName, Object[] args)}.
     * </p>
     *
     * @param object invoke method on this object
     * @param methodName get method with this name
     * @param arg use this argument
     * @return The value returned by the invoked method
     *
     * @throws NoSuchMethodException if there is no such accessible method
     * @throws InvocationTargetException wraps an exception thrown by the method invoked
     * @throws IllegalAccessException if the requested method is not accessible via reflection
     */
    public static Object invokeMethod(Object object, String methodName,
            Object arg) throws NoSuchMethodException, IllegalAccessException,
            InvocationTargetException {
        return invokeMethod(object, methodName, new Object[] { arg });
    }

    /**
     * <p>Invoke a named method whose parameter type matches the object type.</p>
     *
     * <p>This method delegates the method search to {@link #getMatchingAccessibleMethod(Class, String, Class[])}.</p>
     *
     * <p>This method supports calls to methods taking primitive parameters 
     * via passing in wrapping classes. So, for example, a <code>Boolean</code> object
     * would match a <code>boolean</code> primitive.</p>
     *
     * <p> This is a convenient wrapper for
     * {@link #invokeMethod(Object object,String methodName, Object[] args, Class[] parameterTypes)}.
     * </p>
     *
     * @param object invoke method on this object
     * @param methodName get method with this name
     * @param args use these arguments - treat null as empty array
     * @return The value returned by the invoked method
     *
     * @throws NoSuchMethodException if there is no such accessible method
     * @throws InvocationTargetException wraps an exception thrown by the method invoked
     * @throws IllegalAccessException if the requested method is not accessible via reflection
     */
    public static Object invokeMethod(Object object, String methodName,
            Object[] args) throws NoSuchMethodException,
            IllegalAccessException, InvocationTargetException {
        if (args == null) {
            args = ArrayUtils.EMPTY_OBJECT_ARRAY;
        }
        int arguments = args.length;
        Class[] parameterTypes = new Class[arguments];
        for (int i = 0; i < arguments; i++) {
            parameterTypes[i] = args[i].getClass();
        }
        return invokeMethod(object, methodName, args, parameterTypes);
    }

    /**
     * <p>Invoke a named method whose parameter type matches the object type.</p>
     *
     * <p>This method delegates the method search to {@link #getMatchingAccessibleMethod(Class, String, Class[])}.</p>
     *
     * <p>This method supports calls to methods taking primitive parameters 
     * via passing in wrapping classes. So, for example, a <code>Boolean</code> object
     * would match a <code>boolean</code> primitive.</p>
     *
     * @param object invoke method on this object
     * @param methodName get method with this name
     * @param args use these arguments - treat null as empty array
     * @param parameterTypes match these parameters - treat null as empty array
     * @return The value returned by the invoked method
     *
     * @throws NoSuchMethodException if there is no such accessible method
     * @throws InvocationTargetException wraps an exception thrown by the method invoked
     * @throws IllegalAccessException if the requested method is not accessible via reflection
     */
    public static Object invokeMethod(Object object, String methodName,
            Object[] args, Class[] parameterTypes)
            throws NoSuchMethodException, IllegalAccessException,
            InvocationTargetException {
        if (parameterTypes == null) {
            parameterTypes = ArrayUtils.EMPTY_CLASS_ARRAY;
        }
        if (args == null) {
            args = ArrayUtils.EMPTY_OBJECT_ARRAY;
        }
        Method method = getMatchingAccessibleMethod(object.getClass(),
                methodName, parameterTypes);
        if (method == null) {
            throw new NoSuchMethodException("No such accessible method: "
                    + methodName + "() on object: "
                    + object.getClass().getName());
        }
        return method.invoke(object, args);
    }

    /**
     * <p>Invoke a method whose parameter type matches exactly the object
     * type.</p>
     *
     * <p> This is a convenient wrapper for
     * {@link #invokeExactMethod(Object object,String methodName,Object [] args)}.
     * </p>
     *
     * @param object invoke method on this object
     * @param methodName get method with this name
     * @param arg use this argument
     * @return The value returned by the invoked method
     *
     * @throws NoSuchMethodException if there is no such accessible method
     * @throws InvocationTargetException wraps an exception thrown by the
     *  method invoked
     * @throws IllegalAccessException if the requested method is not accessible
     *  via reflection
     */
    public static Object invokeExactMethod(Object object, String methodName,
            Object arg) throws NoSuchMethodException, IllegalAccessException,
            InvocationTargetException {
        return invokeExactMethod(object, methodName, new Object[] { arg });
    }

    /**
     * <p>Invoke a method whose parameter types match exactly the object
     * types.</p>
     *
     * <p> This uses reflection to invoke the method obtained from a call to
     * <code>getAccessibleMethod()</code>.</p>
     *
     * @param object invoke method on this object
     * @param methodName get method with this name
     * @param args use these arguments - treat null as empty array
     * @return The value returned by the invoked method
     *
     * @throws NoSuchMethodException if there is no such accessible method
     * @throws InvocationTargetException wraps an exception thrown by the
     *  method invoked
     * @throws IllegalAccessException if the requested method is not accessible
     *  via reflection
     */
    public static Object invokeExactMethod(Object object, String methodName,
            Object[] args) throws NoSuchMethodException,
            IllegalAccessException, InvocationTargetException {
        if (args == null) {
            args = ArrayUtils.EMPTY_OBJECT_ARRAY;
        }
        int arguments = args.length;
        Class[] parameterTypes = new Class[arguments];
        for (int i = 0; i < arguments; i++) {
            parameterTypes[i] = args[i].getClass();
        }
        return invokeExactMethod(object, methodName, args, parameterTypes);
    }

    /**
     * <p>Invoke a method whose parameter types match exactly the parameter
     * types given.</p>
     *
     * <p>This uses reflection to invoke the method obtained from a call to
     * <code>getAccessibleMethod()</code>.</p>
     *
     * @param object invoke method on this object
     * @param methodName get method with this name
     * @param args use these arguments - treat null as empty array
     * @param parameterTypes match these parameters - treat null as empty array
     * @return The value returned by the invoked method
     *
     * @throws NoSuchMethodException if there is no such accessible method
     * @throws InvocationTargetException wraps an exception thrown by the
     *  method invoked
     * @throws IllegalAccessException if the requested method is not accessible
     *  via reflection
     */
    public static Object invokeExactMethod(Object object, String methodName,
            Object[] args, Class[] parameterTypes)
            throws NoSuchMethodException, IllegalAccessException,
            InvocationTargetException {
        if (args == null) {
            args = ArrayUtils.EMPTY_OBJECT_ARRAY;
        }
        if (parameterTypes == null) {
            parameterTypes = ArrayUtils.EMPTY_CLASS_ARRAY;
        }
        Method method = getAccessibleMethod(object.getClass(), methodName,
                parameterTypes);
        if (method == null) {
            throw new NoSuchMethodException("No such accessible method: "
                    + methodName + "() on object: "
                    + object.getClass().getName());
        }
        return method.invoke(object, args);
    }

    /**
     * <p>Invoke a static method whose parameter types match exactly the parameter
     * types given.</p>
     *
     * <p>This uses reflection to invoke the method obtained from a call to
     * {@link #getAccessibleMethod(Class, String, Class[])}.</p>
     *
     * @param cls invoke static method on this class
     * @param methodName get method with this name
     * @param args use these arguments - treat null as empty array
     * @param parameterTypes match these parameters - treat null as empty array
     * @return The value returned by the invoked method
     *
     * @throws NoSuchMethodException if there is no such accessible method
     * @throws InvocationTargetException wraps an exception thrown by the
     *  method invoked
     * @throws IllegalAccessException if the requested method is not accessible
     *  via reflection
     */
    public static Object invokeExactStaticMethod(Class cls, String methodName,
            Object[] args, Class[] parameterTypes)
            throws NoSuchMethodException, IllegalAccessException,
            InvocationTargetException {
        if (args == null) {
            args = ArrayUtils.EMPTY_OBJECT_ARRAY;
        }
        if (parameterTypes == null) {
            parameterTypes = ArrayUtils.EMPTY_CLASS_ARRAY;
        }
        Method method = getAccessibleMethod(cls, methodName, parameterTypes);
        if (method == null) {
            throw new NoSuchMethodException("No such accessible method: "
                    + methodName + "() on class: " + cls.getName());
        }
        return method.invoke(null, args);
    }

    /**
     * <p>Invoke a named static method whose parameter type matches the object type.</p>
     *
     * <p>This method delegates the method search to {@link #getMatchingAccessibleMethod(Class, String, Class[])}.</p>
     *
     * <p>This method supports calls to methods taking primitive parameters 
     * via passing in wrapping classes. So, for example, a <code>Boolean</code> class
     * would match a <code>boolean</code> primitive.</p>
     *
     * <p> This is a convenient wrapper for
     * {@link #invokeStaticMethod(Class objectClass,String methodName,Object [] args)}.
     * </p>
     *
     * @param cls invoke static method on this class
     * @param methodName get method with this name
     * @param arg use this argument
     * @return The value returned by the invoked method
     *
     * @throws NoSuchMethodException if there is no such accessible method
     * @throws InvocationTargetException wraps an exception thrown by the
     *  method invoked
     * @throws IllegalAccessException if the requested method is not accessible
     *  via reflection
     */
    public static Object invokeStaticMethod(Class cls, String methodName,
            Object arg) throws NoSuchMethodException, IllegalAccessException,
            InvocationTargetException {
        return invokeStaticMethod(cls, methodName, new Object[] { arg });
    }

    /**
     * <p>Invoke a named static method whose parameter type matches the object type.</p>
     *
     * <p>This method delegates the method search to {@link #getMatchingAccessibleMethod(Class, String, Class[])}.</p>
     *
     * <p>This method supports calls to methods taking primitive parameters 
     * via passing in wrapping classes. So, for example, a <code>Boolean</code> class
     * would match a <code>boolean</code> primitive.</p>
     *
     * <p> This is a convenient wrapper for
     * {@link #invokeStaticMethod(Class objectClass,String methodName,Object [] args,Class[] parameterTypes)}.
     * </p>
     *
     * @param cls invoke static method on this class
     * @param methodName get method with this name
     * @param args use these arguments - treat null as empty array
     * @return The value returned by the invoked method
     *
     * @throws NoSuchMethodException if there is no such accessible method
     * @throws InvocationTargetException wraps an exception thrown by the
     *  method invoked
     * @throws IllegalAccessException if the requested method is not accessible
     *  via reflection
     */
    public static Object invokeStaticMethod(Class cls, String methodName,
            Object[] args) throws NoSuchMethodException,
            IllegalAccessException, InvocationTargetException {
        if (args == null) {
            args = ArrayUtils.EMPTY_OBJECT_ARRAY;
        }
        int arguments = args.length;
        Class[] parameterTypes = new Class[arguments];
        for (int i = 0; i < arguments; i++) {
            parameterTypes[i] = args[i].getClass();
        }
        return invokeStaticMethod(cls, methodName, args, parameterTypes);
    }

    /**
     * <p>Invoke a named static method whose parameter type matches the object type.</p>
     *
     * <p>This method delegates the method search to {@link #getMatchingAccessibleMethod(Class, String, Class[])}.</p>
     *
     * <p>This method supports calls to methods taking primitive parameters 
     * via passing in wrapping classes. So, for example, a <code>Boolean</code> class
     * would match a <code>boolean</code> primitive.</p>
     *
     *
     * @param cls invoke static method on this class
     * @param methodName get method with this name
     * @param args use these arguments - treat null as empty array
     * @param parameterTypes match these parameters - treat null as empty array
     * @return The value returned by the invoked method
     *
     * @throws NoSuchMethodException if there is no such accessible method
     * @throws InvocationTargetException wraps an exception thrown by the
     *  method invoked
     * @throws IllegalAccessException if the requested method is not accessible
     *  via reflection
     */
    public static Object invokeStaticMethod(Class cls, String methodName,
            Object[] args, Class[] parameterTypes)
            throws NoSuchMethodException, IllegalAccessException,
            InvocationTargetException {
        if (parameterTypes == null) {
            parameterTypes = ArrayUtils.EMPTY_CLASS_ARRAY;
        }
        if (args == null) {
            args = ArrayUtils.EMPTY_OBJECT_ARRAY;
        }
        Method method = getMatchingAccessibleMethod(cls, methodName,
                parameterTypes);
        if (method == null) {
            throw new NoSuchMethodException("No such accessible method: "
                    + methodName + "() on class: " + cls.getName());
        }
        return method.invoke(null, args);
    }

    /**
     * <p>Invoke a static method whose parameter type matches exactly the object
     * type.</p>
     *
     * <p> This is a convenient wrapper for
     * {@link #invokeExactStaticMethod(Class objectClass,String methodName,Object [] args)}.
     * </p>
     *
     * @param cls invoke static method on this class
     * @param methodName get method with this name
     * @param arg use this argument
     * @return The value returned by the invoked method
     *
     * @throws NoSuchMethodException if there is no such accessible method
     * @throws InvocationTargetException wraps an exception thrown by the
     *  method invoked
     * @throws IllegalAccessException if the requested method is not accessible
     *  via reflection
     */
    public static Object invokeExactStaticMethod(Class cls, String methodName,
            Object arg) throws NoSuchMethodException, IllegalAccessException,
            InvocationTargetException {
        return invokeExactStaticMethod(cls, methodName, new Object[] { arg });
    }

    /**
     * <p>Invoke a static method whose parameter types match exactly the object
     * types.</p>
     *
     * <p> This uses reflection to invoke the method obtained from a call to
     * {@link #getAccessibleMethod(Class, String, Class[])}.</p>
     *
     * @param cls invoke static method on this class
     * @param methodName get method with this name
     * @param args use these arguments - treat null as empty array
     * @return The value returned by the invoked method
     *
     * @throws NoSuchMethodException if there is no such accessible method
     * @throws InvocationTargetException wraps an exception thrown by the
     *  method invoked
     * @throws IllegalAccessException if the requested method is not accessible
     *  via reflection
     */
    public static Object invokeExactStaticMethod(Class cls, String methodName,
            Object[] args) throws NoSuchMethodException,
            IllegalAccessException, InvocationTargetException {
        if (args == null) {
            args = ArrayUtils.EMPTY_OBJECT_ARRAY;
        }
        int arguments = args.length;
        Class[] parameterTypes = new Class[arguments];
        for (int i = 0; i < arguments; i++) {
            parameterTypes[i] = args[i].getClass();
        }
        return invokeExactStaticMethod(cls, methodName, args, parameterTypes);
    }

    /**
     * <p>Return an accessible method (that is, one that can be invoked via
     * reflection) with given name and a single parameter.  If no such method
     * can be found, return <code>null</code>.
     * Basically, a convenience wrapper that constructs a <code>Class</code>
     * array for you.</p>
     *
     * @param cls get method from this class
     * @param methodName get method with this name
     * @param parameterType taking this type of parameter
     * @return The accessible method
     */
    public static Method getAccessibleMethod(Class cls, String methodName,
            Class parameterType) {
        return getAccessibleMethod(cls, methodName,
                new Class[] { parameterType });
    }

    /**
     * <p>Return an accessible method (that is, one that can be invoked via
     * reflection) with given name and parameters.  If no such method
     * can be found, return <code>null</code>.
     * This is just a convenient wrapper for
     * {@link #getAccessibleMethod(Method method)}.</p>
     *
     * @param cls get method from this class
     * @param methodName get method with this name
     * @param parameterTypes with these parameters types
     * @return The accessible method
     */
    public static Method getAccessibleMethod(Class cls, String methodName,
            Class[] parameterTypes) {
        try {
            MethodDescriptor md = new MethodDescriptor(cls, methodName,
                    parameterTypes, true);
            // Check the cache first
            Method method = getCachedMethod(md);
            if (method != null) {
                return method;
            }
            method = getAccessibleMethod(cls.getMethod(methodName,
                    parameterTypes));
            cacheMethod(md, method);
            return method;
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
     * @return The accessible method
     */
    public static Method getAccessibleMethod(Method method) {
        if (!MemberUtils.isAccessible(method)) {
            return null;
        }
        // If the declaring class is public, we are done
        Class cls = method.getDeclaringClass();
        if (Modifier.isPublic(cls.getModifiers())) {
            return method;
        }
        String methodName = method.getName();
        Class[] parameterTypes = method.getParameterTypes();

        // Check the implemented interfaces and subinterfaces
        method = getAccessibleMethodFromInterfaceNest(cls, methodName,
                parameterTypes);

        // Check the superclass chain
        if (method == null) {
            method = getAccessibleMethodFromSuperclass(cls, methodName,
                    parameterTypes);
        }
        return method;
    }

    /**
     * <p>Return an accessible method (that is, one that can be invoked via
     * reflection) by scanning through the superclasses. If no such method
     * can be found, return <code>null</code>.</p>
     *
     * @param cls Class to be checked
     * @param methodName Method name of the method we wish to call
     * @param parameterTypes The parameter type signatures
     */
    private static Method getAccessibleMethodFromSuperclass(Class cls,
            String methodName, Class[] parameterTypes) {
        Class parentClass = cls.getSuperclass();
        while (parentClass != null) {
            if (Modifier.isPublic(parentClass.getModifiers())) {
                try {
                    return parentClass.getMethod(methodName, parameterTypes);
                } catch (NoSuchMethodException e) {
                    return null;
                }
            }
            parentClass = parentClass.getSuperclass();
        }
        return null;
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
     * @param cls Parent class for the interfaces to be checked
     * @param methodName Method name of the method we wish to call
     * @param parameterTypes The parameter type signatures
     */
    private static Method getAccessibleMethodFromInterfaceNest(Class cls,
            String methodName, Class[] parameterTypes) {
        Method method = null;

        // Search up the superclass chain
        for (; cls != null; cls = cls.getSuperclass()) {

            // Check the implemented interfaces of the parent class
            Class[] interfaces = cls.getInterfaces();
            for (int i = 0; i < interfaces.length; i++) {
                // Is this interface public?
                if (!Modifier.isPublic(interfaces[i].getModifiers())) {
                    continue;
                }
                // Does the method exist on this interface?
                try {
                    method = interfaces[i].getDeclaredMethod(methodName,
                            parameterTypes);
                } catch (NoSuchMethodException e) {
                    /*
                     * Swallow, if no method is found after the loop then this
                     * method returns null.
                     */
                }
                if (method != null) {
                    break;
                }
                // Recursively check our parent interfaces
                method = getAccessibleMethodFromInterfaceNest(interfaces[i],
                        methodName, parameterTypes);
                if (method != null) {
                    break;
                }
            }
        }
        return method;
    }

    /**
     * <p>Find an accessible method that matches the given name and has compatible parameters.
     * Compatible parameters mean that every method parameter is assignable from 
     * the given parameters.
     * In other words, it finds a method with the given name 
     * that will take the parameters given.<p>
     *
     * <p>This method is used by 
     * {@link 
     * #invokeMethod(Object object, String methodName, Object[] args, Class[] parameterTypes)}.
     *
     * <p>This method can match primitive parameter by passing in wrapper classes.
     * For example, a <code>Boolean</code> will match a primitive <code>boolean</code>
     * parameter.
     *
     * @param cls find method in this class
     * @param methodName find method with this name
     * @param parameterTypes find method with most compatible parameters 
     * @return The accessible method
     */
    public static Method getMatchingAccessibleMethod(Class cls,
            String methodName, Class[] parameterTypes) {
        MethodDescriptor md = new MethodDescriptor(cls, methodName,
                parameterTypes, false);
        // Check the cache first
        Method method = getCachedMethod(md);
        if (method != null) {
            return method;
        }
        // see if we can find the method directly
        // most of the time this works and it's much faster
        try {
            method = cls.getMethod(methodName, parameterTypes);
            MemberUtils.setAccessibleWorkaround(method);
            cacheMethod(md, method);
            return method;
        } catch (NoSuchMethodException e) { /* SWALLOW */
        }
        // search through all methods
        Method bestMatch = null;
        Method[] methods = cls.getMethods();
        for (int i = 0, size = methods.length; i < size; i++) {
            if (methods[i].getName().equals(methodName)) {
                // compare parameters
                if (ClassUtils.isAssignable(parameterTypes, methods[i]
                        .getParameterTypes(), true)) {
                    // get accessible version of method
                    Method accessibleMethod = getAccessibleMethod(methods[i]);
                    if (accessibleMethod != null) {
                        if (bestMatch == null
                                || MemberUtils.compareParameterTypes(
                                        accessibleMethod.getParameterTypes(),
                                        bestMatch.getParameterTypes(),
                                        parameterTypes) < 0) {
                            bestMatch = accessibleMethod;
                        }
                    }
                }
            }
        }
        if (bestMatch != null) {
            MemberUtils.setAccessibleWorkaround(bestMatch);
            cacheMethod(md, bestMatch);
        }
        return bestMatch;
    }

    /**
     * Return the method from the cache, if present.
     *
     * @param md The method descriptor
     * @return The cached method
     */
    private static Method getCachedMethod(MethodDescriptor md) {
        if (cacheMethods) {
            return (Method) cache.get(md);
        }
        return null;
    }

    /**
     * Add a method to the cache.
     *
     * @param md The method descriptor
     * @param method The method to cache
     */
    private static void cacheMethod(MethodDescriptor md, Method method) {
        if (cacheMethods) {
            if (method != null) {
                cache.put(md, method);
            }
        }
    }

    /**
     * Represents the key to looking up a Method by reflection.
     */
    private static class MethodDescriptor {
        private Class cls;
        private String methodName;
        private Class[] paramTypes;
        private boolean exact;
        private int hashCode;

        /**
         * The sole constructor.
         *
         * @param cls  the class to reflect, must not be null
         * @param methodName  the method name to obtain
         * @param paramTypes the array of classes representing the paramater types
         * @param exact whether the match has to be exact.
         */
        public MethodDescriptor(Class cls, String methodName,
                Class[] paramTypes, boolean exact) {
            if (cls == null) {
                throw new IllegalArgumentException("Class cannot be null");
            }
            if (methodName == null) {
                throw new IllegalArgumentException("Method Name cannot be null");
            }
            if (paramTypes == null) {
                paramTypes = ArrayUtils.EMPTY_CLASS_ARRAY;
            }
            this.cls = cls;
            this.methodName = methodName;
            this.paramTypes = paramTypes;
            this.exact = exact;
            // is this adequate? :/
            this.hashCode = methodName.length();
        }

        /**
         * Checks for equality.
         * @param obj object to be tested for equality
         * @return true, if the object describes the same Method.
         */
        public boolean equals(Object obj) {
            if (!(obj instanceof MethodDescriptor)) {
                return false;
            }
            MethodDescriptor md = (MethodDescriptor) obj;

            return exact == md.exact && methodName.equals(md.methodName)
                    && cls.equals(md.cls)
                    && Arrays.equals(paramTypes, md.paramTypes);
        }

        /**
         * Returns the string length of method name. I.e. if the
         * hashcodes are different, the objects are different. If the
         * hashcodes are the same, need to use the equals method to
         * determine equality.
         * @return the string length of method name.
         */
        public int hashCode() {
            return hashCode;
        }
    }
}
