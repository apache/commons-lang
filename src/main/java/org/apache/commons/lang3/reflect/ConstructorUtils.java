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
package org.apache.commons.lang3.reflect;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.ClassUtils;

/**
 * <p> Utility reflection methods focussed on constructors, modelled after {@link MethodUtils}. </p>
 *
 * <h3>Known Limitations</h3>
 * <h4>Accessing Public Constructors In A Default Access Superclass</h4>
 * <p>There is an issue when invoking public constructors contained in a default access superclass.
 * Reflection locates these constructors fine and correctly assigns them as public.
 * However, an <code>IllegalAccessException</code> is thrown if the constructors is invoked.</p>
 *
 * <p><code>ConstructorUtils</code> contains a workaround for this situation.
 * It will attempt to call <code>setAccessible</code> on this constructor.
 * If this call succeeds, then the method can be invoked as normal.
 * This call will only succeed when the application has sufficient security privilages.
 * If this call fails then a warning will be logged and the method may fail.</p>
 *
 * @author Apache Software Foundation
 * @author Craig R. McClanahan
 * @author Ralph Schaer
 * @author Chris Audley
 * @author Rey Francois
 * @author Gregor Rayman
 * @author Jan Sorensen
 * @author Robert Burrell Donkin
 * @author Rodney Waldhoff
 * @since 2.5
 * @version $Id$
 */
public class ConstructorUtils {

    /**
     * <p>ConstructorUtils instances should NOT be constructed in standard programming.
     * Instead, the class should be used as
     * <code>ConstructorUtils.invokeConstructor(cls, args)</code>.</p>
     *
     * <p>This constructor is public to permit tools that require a JavaBean
     * instance to operate.</p>
     */
    public ConstructorUtils() {
        super();
    }

    /**
     * <p>Returns new instance of <code>klazz</code> created using the actual arguments <code>args</code>.
     * The formal parameter types are inferred from the actual values of <code>args</code>.
     * See {@link #invokeExactConstructor(Class, Object[], Class[])} for more details.</p>
     *
     * <p>The signatures should be assignment compatible.</p>
     *
     * @param cls the class to be constructed.
     * @param args actual argument array
     * @return new instance of <code>klazz</code>
     *
     * @throws NoSuchMethodException If the constructor cannot be found
     * @throws IllegalAccessException If an error occurs accessing the constructor
     * @throws InvocationTargetException If an error occurs invoking the constructor
     * @throws InstantiationException If an error occurs instantiating the class
     *
     * @see #invokeConstructor(java.lang.Class, java.lang.Object[], java.lang.Class[])
     */
    public static <T> T invokeConstructor(Class<T> cls, Object... args)
            throws NoSuchMethodException, IllegalAccessException,
            InvocationTargetException, InstantiationException {
        if (null == args) {
            args = ArrayUtils.EMPTY_OBJECT_ARRAY;
        }
        Class<?> parameterTypes[] = new Class[args.length];
        for (int i = 0; i < args.length; i++) {
            parameterTypes[i] = args[i].getClass();
        }
        return invokeConstructor(cls, args, parameterTypes);
    }

    /**
     * <p>Returns new instance of <code>klazz</code> created using constructor
     * with signature <code>parameterTypes</code> and actual arguments <code>args</code>.</p>
     *
     * <p>The signatures should be assignment compatible.</p>
     *
     * @param cls the class to be constructed.
     * @param args actual argument array
     * @param parameterTypes parameter types array
     * @return new instance of <code>klazz</code>
     *
     * @throws NoSuchMethodException if matching constructor cannot be found
     * @throws IllegalAccessException thrown on the constructor's invocation
     * @throws InvocationTargetException thrown on the constructor's invocation
     * @throws InstantiationException thrown on the constructor's invocation
     * @see Constructor#newInstance
     */
    public static <T> T invokeConstructor(Class<T> cls, Object[] args,
            Class<?>[] parameterTypes) throws NoSuchMethodException,
            IllegalAccessException, InvocationTargetException,
            InstantiationException {
        if (parameterTypes == null) {
            parameterTypes = ArrayUtils.EMPTY_CLASS_ARRAY;
        }
        if (args == null) {
            args = ArrayUtils.EMPTY_OBJECT_ARRAY;
        }
        Constructor<T> ctor = getMatchingAccessibleConstructor(cls, parameterTypes);
        if (null == ctor) {
            throw new NoSuchMethodException(
                    "No such accessible constructor on object: "
                            + cls.getName());
        }
        return ctor.newInstance(args);
    }

    /**
     * <p>Returns new instance of <code>klazz</code> created using the actual arguments <code>args</code>.
     * The formal parameter types are inferred from the actual values of <code>args</code>.
     * See {@link #invokeExactConstructor(Class, Object[], Class[])} for more details.</p>
     *
     * <p>The signatures should match exactly.</p>
     *
     * @param cls the class to be constructed.
     * @param args actual argument array
     * @return new instance of <code>klazz</code>
     *
     * @throws NoSuchMethodException If the constructor cannot be found
     * @throws IllegalAccessException If an error occurs accessing the constructor
     * @throws InvocationTargetException If an error occurs invoking the constructor
     * @throws InstantiationException If an error occurs instantiating the class
     *
     * @see #invokeExactConstructor(java.lang.Class, java.lang.Object[], java.lang.Class[])
     */
    public static <T> T invokeExactConstructor(Class<T> cls, Object... args)
            throws NoSuchMethodException, IllegalAccessException,
            InvocationTargetException, InstantiationException {
        if (null == args) {
            args = ArrayUtils.EMPTY_OBJECT_ARRAY;
        }
        int arguments = args.length;
        Class<?> parameterTypes[] = new Class[arguments];
        for (int i = 0; i < arguments; i++) {
            parameterTypes[i] = args[i].getClass();
        }
        return invokeExactConstructor(cls, args, parameterTypes);
    }

    /**
     * <p>Returns new instance of <code>klazz</code> created using constructor
     * with signature <code>parameterTypes</code> and actual arguments
     * <code>args</code>.</p>
     *
     * <p>The signatures should match exactly.</p>
     *
     * @param cls the class to be constructed.
     * @param args actual argument array
     * @param parameterTypes parameter types array
     * @return new instance of <code>klazz</code>
     *
     * @throws NoSuchMethodException if matching constructor cannot be found
     * @throws IllegalAccessException thrown on the constructor's invocation
     * @throws InvocationTargetException thrown on the constructor's invocation
     * @throws InstantiationException thrown on the constructor's invocation
     * @see Constructor#newInstance
     */
    public static <T> T invokeExactConstructor(Class<T> cls, Object[] args,
            Class<?>[] parameterTypes) throws NoSuchMethodException,
            IllegalAccessException, InvocationTargetException,
            InstantiationException {
        if (args == null) {
            args = ArrayUtils.EMPTY_OBJECT_ARRAY;
        }
        if (parameterTypes == null) {
            parameterTypes = ArrayUtils.EMPTY_CLASS_ARRAY;
        }
        Constructor<T> ctor = getAccessibleConstructor(cls, parameterTypes);
        if (null == ctor) {
            throw new NoSuchMethodException(
                    "No such accessible constructor on object: "
                            + cls.getName());
        }
        return ctor.newInstance(args);
    }

    /**
     * Returns a constructor given a class and signature.
     * @param cls the class to be constructed
     * @param parameterTypes the parameter array
     * @return null if matching accessible constructor can not be found
     * @see Class#getConstructor
     * @see #getAccessibleConstructor(java.lang.reflect.Constructor)
     */
    public static <T> Constructor<T> getAccessibleConstructor(Class<T> cls,
            Class<?>... parameterTypes) {
        try {
            return getAccessibleConstructor(cls.getConstructor(parameterTypes));
        } catch (NoSuchMethodException e) {
            return (null);
        }
    }

    /**
     * Returns accessible version of the given constructor.
     * @param ctor prototype constructor object.
     * @return <code>null</code> if accessible constructor can not be found.
     * @see java.lang.SecurityManager
     */
    public static <T> Constructor<T> getAccessibleConstructor(Constructor<T> ctor) {
        return MemberUtils.isAccessible(ctor)
                && Modifier.isPublic(ctor.getDeclaringClass().getModifiers()) ? ctor
                : null;
    }

    /**
     * <p>Find an accessible constructor with compatible parameters.
     * Compatible parameters mean that every method parameter is assignable from
     * the given parameters. In other words, it finds constructor that will take
     * the parameters given.</p>
     *
     * <p>First it checks if there is constructor matching the exact signature.
     * If no such, all the constructors of the class are tested if their signatures
     * are assignment compatible with the parameter types.
     * The first matching constructor is returned.</p>
     *
     * @param cls find constructor for this class
     * @param parameterTypes find method with compatible parameters
     * @return a valid Constructor object. If there's no matching constructor, returns <code>null</code>.
     */
    @SuppressWarnings("unchecked")
    public static <T> Constructor<T> getMatchingAccessibleConstructor(Class<T> cls,
            Class<?>... parameterTypes) {
        // see if we can find the constructor directly
        // most of the time this works and it's much faster
        try {
            Constructor<T> ctor = cls.getConstructor(parameterTypes);
            MemberUtils.setAccessibleWorkaround(ctor);
            return ctor;
        } catch (NoSuchMethodException e) { /* SWALLOW */
        }
        Constructor<T> result = null;
        // search through all constructors
        Constructor<?>[] ctors = cls.getConstructors();
        for (int i = 0; i < ctors.length; i++) {
            // compare parameters
            if (ClassUtils.isAssignable(parameterTypes, ctors[i]
                    .getParameterTypes(), true)) {
                // get accessible version of method
                Constructor<T> ctor = getAccessibleConstructor((Constructor<T>) ctors[i]);
                if (ctor != null) {
                    MemberUtils.setAccessibleWorkaround(ctor);
                    if (result == null
                            || MemberUtils.compareParameterTypes(ctor
                                    .getParameterTypes(), result
                                    .getParameterTypes(), parameterTypes) < 0) {
                        result = ctor;
                    }
                }
            }
        }
        return result;
    }

}
